-module(beamai_chat_completion).

%% @doc Chat Completion Service
%%
%% Provides LLM chat completion with:
%% - Multi-provider routing (openai, anthropic, zhipu, ollama, deepseek, bailian)
%% - Request building (messages, tools, tool_choice, stream)
%% - Retry logic with exponential backoff
%% - Streaming support with token callbacks

-behaviour(beamai_chat_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Config API
-export([create/2]).

%% Chat API
-export([chat/2, chat/3]).
-export([stream_chat/3, stream_chat/4]).

%% Types
-export_type([config/0, provider/0]).

-ifdef(TEST).
-export([is_retryable/1, compute_delay/3]).
-endif.

-type provider() :: openai | anthropic | ollama | zhipu | bailian | deepseek | mock | {custom, module()}.
-type config() :: #{
    provider := provider(),
    module := module(),
    '__llm_config__' := true,
    atom() => term()
}.

%%====================================================================
%% Config API
%%====================================================================

%% @doc Create a chat completion config for a given provider.
%%
%% Example:
%%   Config = beamai_chat_completion:create(anthropic, #{
%%       model => <<"claude-sonnet-4-20250514">>,
%%       api_key => <<"sk-...">>
%%   })
-spec create(provider(), map()) -> config().
create(Provider, Opts) ->
    Module = provider_module(Provider),
    DefaultConfig = Module:default_config(),
    BaseConfig = #{
        provider => Provider,
        module => ?MODULE,
        '__llm_config__' => true
    },
    maps:merge(maps:merge(DefaultConfig, BaseConfig), Opts).

%%====================================================================
%% Chat API
%%====================================================================

%% @doc Send chat completion request
-spec chat(config(), [map()]) -> {ok, map()} | {error, term()}.
chat(Config, Messages) ->
    chat(Config, Messages, #{}).

%% @doc Send chat completion request with options
%%
%% Options:
%%   tools => [tool_spec()]     - tool definitions
%%   tool_choice => auto | none | required
%%   max_retries => integer()   - retry count (default 3)
%%   retry_delay => integer()   - base retry delay ms (default 1000)
%%   on_retry => fun(RetryState) - retry callback
-spec chat(config(), [map()], map()) -> {ok, map()} | {error, term()}.
chat(Config, Messages, Opts) ->
    Request = build_request(Messages, Opts),
    RetryOpts = get_retry_opts(Opts),
    do_chat(Config, Request, RetryOpts).

%% @doc Send streaming chat request
-spec stream_chat(config(), [map()], fun((term()) -> ok)) ->
    {ok, map()} | {error, term()}.
stream_chat(Config, Messages, Callback) ->
    stream_chat(Config, Messages, Callback, #{}).

%% @doc Send streaming chat request with options
-spec stream_chat(config(), [map()], fun((term()) -> ok), map()) ->
    {ok, map()} | {error, term()}.
stream_chat(Config, Messages, Callback, Opts) ->
    Module = provider_module(maps:get(provider, Config)),
    Request = build_request(Messages, Opts#{stream => true}),
    WrappedCallback = wrap_stream_callback(Callback, Opts),
    Module:stream_chat(Config, Request, WrappedCallback).

%%====================================================================
%% Internal - Provider Routing
%%====================================================================

provider_module(openai) -> beamai_llm_provider_openai;
provider_module(anthropic) -> beamai_llm_provider_anthropic;
provider_module(ollama) -> beamai_llm_provider_ollama;
provider_module(zhipu) -> beamai_llm_provider_zhipu;
provider_module(bailian) -> beamai_llm_provider_bailian;
provider_module(deepseek) -> beamai_llm_provider_deepseek;
provider_module(mock) -> beamai_llm_provider_mock;
provider_module({custom, Module}) -> Module.

%%====================================================================
%% Internal - Request Building
%%====================================================================

build_request(Messages, Opts) ->
    Base = #{messages => Messages},
    Fields = [tools, tool_choice, stream],
    lists:foldl(fun(F, Acc) ->
        case maps:get(F, Opts, undefined) of
            undefined -> Acc;
            Value -> Acc#{F => Value}
        end
    end, Base, Fields).

%%====================================================================
%% Internal - Retry Logic
%%====================================================================

get_retry_opts(Opts) ->
    #{
        max_retries => maps:get(max_retries, Opts, ?DEFAULT_MAX_RETRIES),
        retry_delay => maps:get(retry_delay, Opts, ?DEFAULT_RETRY_DELAY),
        on_retry => maps:get(on_retry, Opts, undefined)
    }.

do_chat(Config, Request, RetryOpts) ->
    Module = provider_module(maps:get(provider, Config)),
    do_chat_with_retry(Module, Config, Request, RetryOpts, 0).

do_chat_with_retry(Module, Config, Request, #{max_retries := Max}, Attempt) when Attempt >= Max ->
    Module:chat(Config, Request);
do_chat_with_retry(Module, Config, Request, RetryOpts, Attempt) ->
    case Module:chat(Config, Request) of
        {ok, _} = Success ->
            Success;
        {error, Reason} = Error ->
            case is_retryable(Reason) of
                true ->
                    Delay = compute_delay(RetryOpts, Attempt, Reason),
                    invoke_retry_callback(RetryOpts, #{
                        attempt => Attempt + 1,
                        max_retries => maps:get(max_retries, RetryOpts),
                        error => Reason,
                        delay => Delay
                    }),
                    timer:sleep(Delay),
                    do_chat_with_retry(Module, Config, Request, RetryOpts, Attempt + 1);
                false ->
                    Error
            end
    end.

%% 重试上限退避（避免 Retry-After 过大时长时间阻塞）
-define(MAX_RETRY_DELAY, 60000).

%% @private 是否可重试（统一委托给 beamai_llm_error 分类，单一事实源）
is_retryable(Reason) ->
    beamai_llm_error:retryable(beamai_llm_error:from_reason(Reason)).

%% @private 计算退避时长
%% 错误携带 Retry-After（服务端建议）时按其退避（上限 ?MAX_RETRY_DELAY），
%% 否则使用指数退避 retry_delay * (Attempt + 1)。
compute_delay(RetryOpts, Attempt, Reason) ->
    case beamai_llm_error:retry_after_ms(beamai_llm_error:from_reason(Reason)) of
        Ms when is_integer(Ms), Ms > 0 ->
            min(Ms, ?MAX_RETRY_DELAY);
        _ ->
            maps:get(retry_delay, RetryOpts) * (Attempt + 1)
    end.

invoke_retry_callback(#{on_retry := undefined}, _) -> ok;
invoke_retry_callback(#{on_retry := Callback}, RetryState) when is_function(Callback) ->
    try Callback(RetryState)
    catch _:_ -> ok
    end;
invoke_retry_callback(_, _) -> ok.

%%====================================================================
%% Internal - Streaming
%%====================================================================

wrap_stream_callback(Callback, Opts) ->
    OnNewToken = maps:get(on_llm_new_token, Opts, undefined),
    Meta = maps:get(callback_meta, Opts, #{}),
    fun(Event) ->
        invoke_new_token_callback(Event, OnNewToken, Meta),
        Callback(Event)
    end.

invoke_new_token_callback(_Event, undefined, _Meta) -> ok;
invoke_new_token_callback(Event, Callback, Meta) when is_function(Callback) ->
    case extract_token_from_event(Event) of
        <<>> -> ok;
        Token ->
            try Callback(Token, Meta)
            catch _:_ -> ok
            end
    end;
invoke_new_token_callback(_, _, _) -> ok.

extract_token_from_event(#{<<"choices">> := [#{<<"delta">> := Delta} | _]}) ->
    maps:get(<<"content">>, Delta, <<>>);
extract_token_from_event(#{<<"delta">> := #{<<"text">> := Text}}) ->
    Text;
extract_token_from_event(#{<<"response">> := Response}) when is_binary(Response) ->
    Response;
extract_token_from_event(#{<<"message">> := #{<<"content">> := Content}}) ->
    Content;
extract_token_from_event(_) ->
    <<>>.
