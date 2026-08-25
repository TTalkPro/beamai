%%%-------------------------------------------------------------------
%%% @doc 智谱 AI (Zhipu/BigModel) LLM Provider 实现
%%%
%%% 支持智谱 AI 的对话补全 API，包括 OpenAI 兼容和 Anthropic 兼容两种模式。
%%% 使用 beamai_llm_http_client 处理公共 HTTP 逻辑。
%%%
%%% API 文档: https://docs.bigmodel.cn/api-reference/
%%%
%%% == API 模式 ==
%%%
%%% 通过 `api_mode` 配置项选择 API 兼容模式：
%%%
%%% - `openai`（默认）: OpenAI 兼容 API
%%%   - Base URL: https://open.bigmodel.cn
%%%   - Endpoint: /api/paas/v4/chat/completions
%%%   - 或使用 Coding API: /api/coding/paas/v4/chat/completions
%%%
%%% - `anthropic`: Anthropic 兼容 API
%%%   - Base URL: https://open.bigmodel.cn/api/anthropic
%%%   - Endpoint: /v1/messages
%%%
%%% 支持的模型:
%%%   - GLM-4.7 系列（最新旗舰）
%%%   - GLM-4.6 系列
%%%   - GLM-4.5 系列
%%%   - GLM-4 系列
%%%
%%% 特性:
%%%   - 同步对话补全
%%%   - 流式输出 (SSE)
%%%   - 工具调用 (Function Calling)
%%%   - 异步对话补全（仅 OpenAI 模式）
%%%   - reasoning_content 支持（GLM-4.6+，仅 OpenAI 模式）
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% OpenAI 兼容模式（默认）
%%% Config = #{
%%%     api_key => <<"your-api-key">>,
%%%     model => <<"glm-4.7">>,
%%%     api_mode => openai
%%% },
%%%
%%% %% Anthropic 兼容模式
%%% Config = #{
%%%     api_key => <<"your-api-key">>,
%%%     model => <<"glm-4.7">>,
%%%     api_mode => anthropic
%%% },
%%%
%%% %% 使用 Coding API（代码相关任务）
%%% Config = #{
%%%     api_key => <<"your-api-key">>,
%%%     model => <<"glm-4.7">>,
%%%     api_mode => openai,
%%%     use_coding_api => true
%%% }.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_zhipu).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([base_url/1, endpoint/2, headers/2, body/2, parser/1,
         stream_accumulator/1, stream_finalizer/1]).
-export([supports_tools/0, supports_streaming/0]).

%% 扩展 API - 异步调用
-export([async_chat/2, get_async_result/2]).

%% 默认值
-define(ZHIPU_BASE_URL, <<"https://open.bigmodel.cn">>).

%% OpenAI 兼容模式端点
-define(ZHIPU_OPENAI_ENDPOINT, <<"/api/paas/v4/chat/completions">>).
-define(ZHIPU_CODING_ENDPOINT, <<"/api/coding/paas/v4/chat/completions">>).
-define(ZHIPU_ASYNC_ENDPOINT, <<"/api/paas/v4/async/chat/completions">>).
-define(ZHIPU_ASYNC_RESULT_PREFIX, <<"/api/paas/v4/async-result/">>).

%% Anthropic 兼容模式端点
-define(ZHIPU_ANTHROPIC_BASE_URL, <<"https://open.bigmodel.cn/api/anthropic">>).
-define(ZHIPU_ANTHROPIC_ENDPOINT, <<"/v1/messages">>).

%% 通用默认值
-define(ZHIPU_MODEL, <<"glm-4.7">>).
-define(ZHIPU_CONNECT_TIMEOUT, 10000).
-define(ZHIPU_MAX_TOKENS, 4096).
-define(ZHIPU_TEMPERATURE, 0.7).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"Zhipu AI">>.

default_config() ->
    #{
        base_url => ?ZHIPU_BASE_URL,
        model => ?ZHIPU_MODEL,
        timeout => beamai_llm_provider_common:default_timeout(zhipu),
        max_tokens => ?ZHIPU_MAX_TOKENS,
        temperature => ?ZHIPU_TEMPERATURE
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

supports_tools() -> true.
supports_streaming() -> true.

%%====================================================================
%% 聊天 API
%%====================================================================

%% @doc 发送聊天请求
%% 根据 api_mode 配置选择 OpenAI 或 Anthropic 兼容模式
chat(Config, Request) ->
    beamai_llm_http_provider:chat(?MODULE, Config, Request).

%% @doc 发送流式聊天请求
stream_chat(Config, Request, Callback) ->
    beamai_llm_http_provider:stream_chat(?MODULE, Config, Request, Callback).

%%====================================================================
%% 声明式回调：底层信息（怎么发由 beamai_llm_http_provider 统一负责）
%%====================================================================
%%
%% 智谱有两套兼容协议，`api_mode` 决定走哪套——端点/头/体/解析/累加器成套切换，
%% 这正是这些回调都带 Config 的原因。

base_url(Config) ->
    case api_mode(Config) of
        anthropic -> ?ZHIPU_ANTHROPIC_BASE_URL;
        _ -> ?ZHIPU_BASE_URL
    end.

endpoint(Config, _Request) ->
    case api_mode(Config) of
        anthropic -> ?ZHIPU_ANTHROPIC_ENDPOINT;
        _ -> openai_endpoint(Config)
    end.

headers(Config, _Request) ->
    case api_mode(Config) of
        anthropic -> build_anthropic_headers(Config);
        _ -> build_headers(Config)
    end.

body(Config, Request) ->
    case api_mode(Config) of
        anthropic -> build_anthropic_request_body(Config, Request);
        _ -> build_openai_request_body(Config, Request)
    end.

parser(Config) ->
    case api_mode(Config) of
        anthropic -> beamai_llm_response_parser:parser_anthropic();
        _ -> beamai_llm_response_parser:parser_zhipu()
    end.

stream_accumulator(Config) ->
    case api_mode(Config) of
        anthropic -> fun beamai_llm_provider_common:accumulate_anthropic_event/2;
        _ -> fun beamai_llm_provider_common:accumulate_openai_event/2
    end.

stream_finalizer(Config) ->
    case api_mode(Config) of
        anthropic -> fun beamai_llm_provider_common:finalize_anthropic_stream/1;
        _ -> fun(Acc) -> beamai_llm_provider_common:finalize_openai_stream(Acc, zhipu) end
    end.

%% @private 兼容协议：openai（默认）| anthropic
api_mode(Config) -> maps:get(api_mode, Config, openai).

%%====================================================================
%% 扩展 API - 异步调用
%%====================================================================

%% @doc 发送异步聊天请求（仅 OpenAI 兼容模式）
%% 返回任务 ID，可用于后续查询结果
-spec async_chat(map(), map()) -> {ok, binary()} | {error, term()}.
async_chat(Config, Request) ->
    Url = build_url(Config, ?ZHIPU_ASYNC_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_openai_request_body(Config, Request),
    Opts = build_request_opts(Config),
    case beamai_llm_http_client:request(Url, Headers, Body, Opts) of
        {ok, #{<<"id">> := TaskId}} -> {ok, TaskId};
        {ok, Response} -> {error, {unexpected_response, Response}};
        Error -> Error
    end.

%% @doc 获取异步任务结果
-spec get_async_result(map(), binary()) -> {ok, map()} | {pending, map()} | {error, term()}.
get_async_result(Config, TaskId) ->
    Url = build_url(Config, <<?ZHIPU_ASYNC_RESULT_PREFIX/binary, TaskId/binary>>),
    Headers = build_headers(Config),
    Opts = build_request_opts(Config),
    case do_get_request(Url, Headers, Opts) of
        {ok, Response} -> handle_async_response(Response);
        Error -> Error
    end.

%% @private 处理异步响应状态
handle_async_response(#{<<"task_status">> := <<"SUCCESS">>} = Resp) ->
    beamai_llm_response_parser:from_zhipu(Resp);
handle_async_response(#{<<"task_status">> := <<"PROCESSING">>} = Resp) ->
    {pending, Resp};
handle_async_response(#{<<"task_status">> := <<"FAIL">>} = Resp) ->
    {error, {task_failed, Resp}};
handle_async_response(Resp) ->
    beamai_llm_response_parser:from_zhipu(Resp).

%% @private 执行 GET 请求（用于异步结果查询）
%% 使用 beamai_http 作为底层 HTTP 客户端。
%% 异步任务轮询可能持连数分钟，默认路由到 http_pool_longpoll，
%% 不与同步 chat 流量争抢 http_pool_short 的连接预算
%% （经 maybe_inject_pool 门控：Opts 里显式指定的 pool 优先，
%% 否则仅 Gun 后端注入默认池）。
do_get_request(Url, Headers, Opts) ->
    HttpOpts = beamai_llm_http_client:maybe_inject_pool(async_poll, Opts, #{
        timeout => beamai_llm_provider_common:request_timeout(Opts, zhipu),
        connect_timeout => maps:get(connect_timeout, Opts, ?ZHIPU_CONNECT_TIMEOUT),
        headers => Headers
    }),
    case beamai_http:get(Url, #{}, HttpOpts) of
        {ok, Response} when is_map(Response) ->
            {ok, Response};
        {ok, Response} when is_binary(Response) ->
            {ok, json:decode(Response)};
        {error, {http_error, Code, RespBody}} ->
            {error, {http_error, Code, RespBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%%====================================================================
%% 请求构建
%%====================================================================

%% @private OpenAI 兼容模式端点（coding API 走另一条路径）
openai_endpoint(Config) ->
    case maps:get(use_coding_api, Config, false) of
        true -> ?ZHIPU_CODING_ENDPOINT;
        false -> ?ZHIPU_OPENAI_ENDPOINT
    end.

%% @private 构建通用 URL（用于异步 API）
build_url(Config, DefaultEndpoint) ->
    beamai_llm_provider_common:build_url(Config, DefaultEndpoint, ?ZHIPU_BASE_URL).

%% @private 构建 OpenAI 兼容模式请求头
build_headers(Config) ->
    beamai_llm_provider_common:build_bearer_auth_headers(Config).

%% @private 构建 Anthropic 兼容模式请求头
build_anthropic_headers(#{api_key := ApiKey}) ->
    [
        {<<"x-api-key">>, ApiKey},
        {<<"anthropic-version">>, <<"2023-06-01">>},
        {<<"Content-Type">>, <<"application/json">>}
    ].

%% @private 构建请求选项（Config 的 pool 可按 provider 覆盖连接池路由）
build_request_opts(Config) ->
    beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, zhipu),
        connect_timeout => maps:get(connect_timeout, Config, ?ZHIPU_CONNECT_TIMEOUT)
    }, Config).

%% @private 构建 OpenAI 兼容模式请求体
build_openai_request_body(Config, Request) ->
    Messages = beamai_chat_request:messages(Request),
    Base = #{
        <<"model">> => maps:get(model, Config, ?ZHIPU_MODEL),
        <<"messages">> => beamai_llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?ZHIPU_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?ZHIPU_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, beamai_chat_request:options(Request)) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tools(B, beamai_chat_request:options(Request)) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_top_p(B, Config) end
    ]).

%% @private 构建 Anthropic 兼容模式请求体
build_anthropic_request_body(Config, Request) ->
    Messages = beamai_chat_request:messages(Request),
    {SystemPrompt, UserMessages} = beamai_llm_message_adapter:extract_system_prompt(Messages),
    Base = #{
        <<"model">> => maps:get(model, Config, ?ZHIPU_MODEL),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?ZHIPU_MAX_TOKENS),
        <<"messages">> => beamai_llm_message_adapter:to_anthropic(UserMessages)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> maybe_add_system(B, SystemPrompt) end,
        fun(B) -> maybe_add_anthropic_tools(B, beamai_chat_request:options(Request)) end,
        fun(B) -> maybe_add_anthropic_stream(B, beamai_chat_request:options(Request)) end
    ]).

%% @private 添加系统提示
maybe_add_system(Body, undefined) -> Body;
maybe_add_system(Body, SystemPrompt) -> Body#{<<"system">> => SystemPrompt}.

%% @private 添加工具定义（Anthropic 格式）
maybe_add_anthropic_tools(Body, #{tools := Tools}) when Tools =/= [] ->
    Body#{<<"tools">> => beamai_llm_tool_adapter:to_anthropic(Tools)};
maybe_add_anthropic_tools(Body, _) ->
    Body.

%% @private 添加流式标志
maybe_add_anthropic_stream(Body, #{stream := true}) -> Body#{<<"stream">> => true};
maybe_add_anthropic_stream(Body, _) -> Body.
