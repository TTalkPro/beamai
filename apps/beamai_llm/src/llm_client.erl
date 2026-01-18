%%%-------------------------------------------------------------------
%%% @doc LLM 客户端统一入口
%%%
%%% 提供与多种 LLM Provider 交互的统一接口。
%%% 根据配置自动路由到对应的 Provider 实现。
%%%
%%% 支持的 Provider：
%%%   - openai: OpenAI API (GPT-4, GPT-3.5)
%%%   - anthropic: Anthropic Claude API
%%%   - ollama: Ollama 本地模型
%%%   - zhipu: 智谱 AI (GLM-4 系列)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_client).

%% 引入公共常量定义
-include_lib("beamai_core/include/beamai_common.hrl").

%% 配置 API
-export([config/2, config/1]).
-export([create/2]).  %% config/2 的语义化别名
-export([merge_config/2]).

%% 聊天 API
-export([chat/2, chat/3]).
-export([stream_chat/3, stream_chat/4]).

%% 便捷函数
-export([simple_chat/2, simple_chat/3]).
-export([with_tools/3, with_tools/4]).

%% Provider 信息
-export([list_providers/0, provider_info/1]).

%% 类型导出
-export_type([provider/0, config/0]).

%% 类型定义
-type provider() :: openai | anthropic | ollama | zhipu | {custom, module()}.
-type config() :: llm_provider_behaviour:config().
-type message() :: llm_provider_behaviour:message().
-type tool() :: llm_provider_behaviour:tool().
-type chat_response() :: llm_provider_behaviour:chat_response().

%% 使用 agent_common.hrl 中的常量:
%% - DEFAULT_MAX_RETRIES
%% - DEFAULT_RETRY_DELAY

%%====================================================================
%% 配置管理
%%====================================================================

%% @doc 创建 Provider 配置
-spec config(provider(), map()) -> config().
config(Provider, Opts) ->
    Module = provider_module(Provider),
    DefaultConfig = Module:default_config(),
    BaseConfig = #{provider => Provider},
    maps:merge(maps:merge(DefaultConfig, BaseConfig), Opts).

%% @doc 创建 LLM 配置（config/2 的语义化别名）
%%
%% 推荐用法：先创建配置，然后在多个 Agent 间复用
%% ```
%% LLM = llm_client:create(anthropic, #{model => <<"glm-4.7">>, ...}),
%% {ok, Agent1} = beamai_agent:start_link(<<"a1">>, #{llm => LLM, ...}),
%% {ok, Agent2} = beamai_agent:start_link(<<"a2">>, #{llm => LLM, ...}).
%% ```
-spec create(provider(), map()) -> config().
create(Provider, Opts) ->
    config(Provider, Opts).

%% @doc 从环境变量创建配置
-spec config(map()) -> {ok, config()} | {error, term()}.
config(Opts) ->
    Provider = detect_provider(Opts),
    Config = config(Provider, Opts),
    Module = provider_module(Provider),
    case Module:validate_config(Config) of
        ok -> {ok, Config};
        Error -> Error
    end.

%% @doc 合并配置
-spec merge_config(config(), map()) -> config().
merge_config(Config, Opts) ->
    maps:merge(Config, Opts).

%% @doc 检测 Provider 类型
detect_provider(#{provider := P}) -> P;
detect_provider(#{api_key := Key}) when is_binary(Key) ->
    case Key of
        <<"sk-ant-", _/binary>> -> anthropic;
        <<"sk-", _/binary>> -> openai;
        _ -> openai
    end;
detect_provider(_) -> ollama.

%%====================================================================
%% 聊天 API
%%====================================================================

%% @doc 发送聊天请求
-spec chat(config(), [message()]) -> {ok, chat_response()} | {error, term()}.
chat(Config, Messages) ->
    chat(Config, Messages, #{}).

%% @doc 发送聊天请求（带选项）
-spec chat(config(), [message()], map()) -> {ok, chat_response()} | {error, term()}.
chat(Config, Messages, Opts) ->
    Request = build_request(Messages, Opts),
    do_chat(Config, Request, get_retry_opts(Opts)).

%% @doc 发送流式聊天请求
-spec stream_chat(config(), [message()], fun((term()) -> ok)) ->
    {ok, chat_response()} | {error, term()}.
stream_chat(Config, Messages, Callback) ->
    stream_chat(Config, Messages, Callback, #{}).

%% @doc 发送流式聊天请求（带选项）
%%
%% 选项：
%% - on_llm_new_token: fun(Token, Meta) -> ok  流式 Token 回调
%% - callback_meta: map()  回调元数据
-spec stream_chat(config(), [message()], fun((term()) -> ok), map()) ->
    {ok, chat_response()} | {error, term()}.
stream_chat(Config, Messages, Callback, Opts) ->
    Module = provider_module(maps:get(provider, Config)),
    Request = build_request(Messages, Opts#{stream => true}),
    %% 包装回调以支持 on_llm_new_token
    WrappedCallback = wrap_stream_callback(Callback, Opts),
    Module:stream_chat(Config, Request, WrappedCallback).

%%====================================================================
%% 便捷函数
%%====================================================================

%% @doc 简单聊天（单轮对话）
-spec simple_chat(config(), binary()) -> {ok, binary()} | {error, term()}.
simple_chat(Config, Prompt) ->
    simple_chat(Config, Prompt, #{}).

%% @doc 简单聊天（带系统提示词）
-spec simple_chat(config(), binary(), map()) -> {ok, binary()} | {error, term()}.
simple_chat(Config, Prompt, Opts) ->
    SystemPrompt = maps:get(system_prompt, Opts, undefined),
    Messages = build_simple_messages(SystemPrompt, Prompt),
    case chat(Config, Messages, Opts) of
        {ok, #{content := Content}} -> {ok, Content};
        Error -> Error
    end.

%% @doc 带工具的聊天
-spec with_tools(config(), [message()], [tool()]) ->
    {ok, chat_response()} | {error, term()}.
with_tools(Config, Messages, Tools) ->
    with_tools(Config, Messages, Tools, #{}).

%% @doc 带工具的聊天（带选项）
-spec with_tools(config(), [message()], [tool()], map()) ->
    {ok, chat_response()} | {error, term()}.
with_tools(Config, Messages, Tools, Opts) ->
    chat(Config, Messages, Opts#{tools => Tools}).

%%====================================================================
%% Provider 信息
%%====================================================================

%% @doc 列出所有支持的 Provider
-spec list_providers() -> [provider()].
list_providers() ->
    [openai, anthropic, ollama, zhipu].

%% @doc 获取 Provider 信息
-spec provider_info(provider()) -> map().
provider_info(Provider) ->
    Module = provider_module(Provider),
    #{
        name => Module:name(),
        supports_tools => Module:supports_tools(),
        supports_streaming => Module:supports_streaming(),
        default_config => Module:default_config()
    }.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 获取 Provider 模块
provider_module(openai) -> llm_provider_openai;
provider_module(anthropic) -> llm_provider_anthropic;
provider_module(ollama) -> llm_provider_ollama;
provider_module(zhipu) -> llm_provider_zhipu;
provider_module({custom, Module}) -> Module.

%% @doc 构建请求
build_request(Messages, Opts) ->
    Base = #{messages => Messages},
    Fields = [tools, tool_choice, stream],
    lists:foldl(fun(F, Acc) -> maybe_add(Acc, F, Opts) end, Base, Fields).

%% @doc 条件添加字段
maybe_add(Map, Key, Opts) ->
    case maps:get(Key, Opts, undefined) of
        undefined -> Map;
        Value -> Map#{Key => Value}
    end.

%% @doc 构建简单消息列表
build_simple_messages(undefined, Prompt) ->
    [#{role => user, content => Prompt}];
build_simple_messages(SystemPrompt, Prompt) ->
    [
        #{role => system, content => SystemPrompt},
        #{role => user, content => Prompt}
    ].

%% @doc 获取重试选项
get_retry_opts(Opts) ->
    #{
        max_retries => maps:get(max_retries, Opts, ?DEFAULT_MAX_RETRIES),
        retry_delay => maps:get(retry_delay, Opts, ?DEFAULT_RETRY_DELAY),
        on_retry => maps:get(on_retry, Opts, undefined)
    }.

%% @doc 执行聊天（带重试）
do_chat(Config, Request, RetryOpts) ->
    Module = provider_module(maps:get(provider, Config)),
    do_chat_with_retry(Module, Config, Request, RetryOpts, 0).

%% @doc 带重试的聊天执行
do_chat_with_retry(Module, Config, Request, #{max_retries := Max}, Attempt) when Attempt >= Max ->
    Module:chat(Config, Request);
do_chat_with_retry(Module, Config, Request, RetryOpts, Attempt) ->
    case Module:chat(Config, Request) of
        {ok, _} = Success ->
            Success;
        {error, Reason} = Error ->
            case is_retryable(Reason) of
                true ->
                    Delay = maps:get(retry_delay, RetryOpts) * (Attempt + 1),
                    MaxRetries = maps:get(max_retries, RetryOpts),
                    %% 调用 on_retry 回调
                    invoke_retry_callback(RetryOpts, #{
                        attempt => Attempt + 1,
                        max_retries => MaxRetries,
                        error => Reason,
                        delay => Delay
                    }),
                    timer:sleep(Delay),
                    do_chat_with_retry(Module, Config, Request, RetryOpts, Attempt + 1);
                false ->
                    Error
            end
    end.

%% @private 调用重试回调
invoke_retry_callback(#{on_retry := undefined}, _RetryState) ->
    ok;
invoke_retry_callback(#{on_retry := Callback}, RetryState) when is_function(Callback) ->
    try
        Callback(RetryState)
    catch
        _:_ -> ok
    end;
invoke_retry_callback(_, _) ->
    ok.

%% @doc 判断错误是否可重试
is_retryable({http_error, Code, _}) when Code >= 500 -> true;
is_retryable({http_error, 429, _}) -> true;
is_retryable({request_failed, timeout}) -> true;
is_retryable({request_failed, {closed, _}}) -> true;
is_retryable(_) -> false.

%%====================================================================
%% 流式回调支持
%%====================================================================

%% @private 包装流式回调以支持 on_llm_new_token
-spec wrap_stream_callback(fun((term()) -> ok), map()) -> fun((term()) -> ok).
wrap_stream_callback(Callback, Opts) ->
    OnNewToken = maps:get(on_llm_new_token, Opts, undefined),
    Meta = maps:get(callback_meta, Opts, #{}),
    fun(Event) ->
        %% 提取并回调 token
        invoke_new_token_callback(Event, OnNewToken, Meta),
        %% 调用原始回调
        Callback(Event)
    end.

%% @private 调用 on_llm_new_token 回调
-spec invoke_new_token_callback(map(), function() | undefined, map()) -> ok.
invoke_new_token_callback(_Event, undefined, _Meta) ->
    ok;
invoke_new_token_callback(Event, Callback, Meta) when is_function(Callback) ->
    case extract_token_from_event(Event) of
        <<>> -> ok;
        Token ->
            try
                Callback(Token, Meta)
            catch
                _:_ -> ok
            end
    end;
invoke_new_token_callback(_, _, _) ->
    ok.

%% @private 从流式事件中提取 token
%% 支持 OpenAI/Anthropic/Ollama 等格式
-spec extract_token_from_event(map()) -> binary().
extract_token_from_event(#{<<"choices">> := [#{<<"delta">> := Delta} | _]}) ->
    %% OpenAI 格式
    maps:get(<<"content">>, Delta, <<>>);
extract_token_from_event(#{<<"delta">> := #{<<"text">> := Text}}) ->
    %% Anthropic 格式
    Text;
extract_token_from_event(#{<<"response">> := Response}) when is_binary(Response) ->
    %% Ollama 格式
    Response;
extract_token_from_event(#{<<"message">> := #{<<"content">> := Content}}) ->
    %% 其他格式
    Content;
extract_token_from_event(_) ->
    <<>>.
