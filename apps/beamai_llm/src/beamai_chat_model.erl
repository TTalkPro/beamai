-module(beamai_chat_model).

%% @doc Chat Completion Service
%%
%% Provides LLM chat completion with:
%% - Multi-provider routing (openai, anthropic, zhipu, ollama, deepseek, dashscope,
%%   xai, moonshot/kimi, openrouter, siliconflow)
%% - Request building (messages, tools, tool_choice, stream)
%% - Retry with backoff (see 重试 below)
%% - Streaming support with token callbacks
%%
%% 重试：`chat/3` 内建按错误分类的退避重试（与 beamai_embedding / beamai_rerank
%% 同一套 beamai_llm_retry）。它位于**整个 filter 栈之下**——filter 看到的是「一次
%% 逻辑调用」，重试重入碰不到任何 filter，`around_chat` 上的记忆/记账因此每轮只跑
%% 一次。参数三级取值：单次 Opts > provider Config > 框架默认，`max_retries => 0`
%% 即关闭。
%%
%% 流式（`stream_chat/4`）**不重试**：token 已经投递给 sink，重跑会让下游看到重复
%% 内容；需要容错请在 turn 层重跑整轮。

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

-type provider() :: openai | anthropic | ollama | zhipu | dashscope | deepseek |
                    xai | moonshot | kimi | openrouter | siliconflow |
                    mock | {custom, module()}.
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
%%   Config = beamai_chat_model:create(anthropic, #{
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
%%   tools => [tool_spec()]      - tool definitions
%%   tool_choice => auto | none | required
%%   max_retries => integer()    - 重试次数（默认 3；0 关闭）
%%   retry_delay => integer()    - 基础退避 ms（默认 1000；服务端给 Retry-After 时按其建议）
%%   on_retry => fun(RetryState) - 每次重试前的回调
%%
%% 后三项也可写在 provider Config 里作为该 provider 的默认值，单次 Opts 优先。
-spec chat(config(), [map()], map()) -> {ok, map()} | {error, term()}.
chat(Config, Messages, Opts) ->
    Module = provider_module(maps:get(provider, Config)),
    Request = build_request(Messages, Opts),
    RetryOpts = beamai_llm_retry:opts(Config, Opts),
    beamai_llm_retry:run(fun() -> Module:chat(Config, Request) end, RetryOpts).

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
provider_module(dashscope) -> beamai_llm_provider_dashscope;
provider_module(deepseek) -> beamai_llm_provider_deepseek;
provider_module(xai) -> beamai_llm_provider_xai;
provider_module(moonshot) -> beamai_llm_provider_moonshot;
provider_module(kimi) -> beamai_llm_provider_moonshot;  %% Kimi 为 Moonshot 的产品名
provider_module(openrouter) -> beamai_llm_provider_openrouter;
provider_module(siliconflow) -> beamai_llm_provider_siliconflow;
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

-ifdef(TEST).
%% @private 重试判定与退避计算下沉在 beamai_llm_retry（与 embedding/rerank 共用），
%% 此处保留薄封装供既有测试用例调用。
is_retryable(Reason) ->
    beamai_llm_retry:is_retryable(Reason).

compute_delay(RetryOpts, Attempt, Reason) ->
    beamai_llm_retry:compute_delay(RetryOpts, Attempt, Reason).
-endif.

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
