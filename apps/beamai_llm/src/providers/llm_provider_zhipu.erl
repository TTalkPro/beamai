%%%-------------------------------------------------------------------
%%% @doc 智谱 AI (Zhipu/BigModel) LLM Provider 实现
%%%
%%% 支持智谱 AI 的对话补全 API。
%%% 使用 llm_http_client 处理公共 HTTP 逻辑。
%%%
%%% API 文档: https://docs.bigmodel.cn/api-reference/
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
%%%   - 异步对话补全
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_provider_zhipu).
-behaviour(llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

%% 扩展 API - 异步调用
-export([async_chat/2, get_async_result/2]).

%% 默认值
-define(ZHIPU_BASE_URL, <<"https://open.bigmodel.cn">>).
-define(ZHIPU_ENDPOINT, <<"/api/paas/v4/chat/completions">>).
-define(ZHIPU_ASYNC_ENDPOINT, <<"/api/paas/v4/async/chat/completions">>).
-define(ZHIPU_ASYNC_RESULT_PREFIX, <<"/api/paas/v4/async-result/">>).
-define(ZHIPU_MODEL, <<"glm-4.7">>).
-define(ZHIPU_TIMEOUT, 300000).
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
        timeout => ?ZHIPU_TIMEOUT,
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
chat(Config, Request) ->
    Url = build_url(Config, ?ZHIPU_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = build_request_opts(Config),
    llm_http_client:request(Url, Headers, Body, Opts, fun parse_response/1).

%% @doc 发送流式聊天请求
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, ?ZHIPU_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = build_request_opts(Config),
    llm_http_client:stream_request(Url, Headers, Body, Opts, Callback, fun accumulate_event/2).

%%====================================================================
%% 扩展 API - 异步调用
%%====================================================================

%% @doc 发送异步聊天请求
%% 返回任务 ID，可用于后续查询结果
-spec async_chat(map(), map()) -> {ok, binary()} | {error, term()}.
async_chat(Config, Request) ->
    Url = build_url(Config, ?ZHIPU_ASYNC_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = build_request_opts(Config),
    case llm_http_client:request(Url, Headers, Body, Opts) of
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
    parse_response(Resp);
handle_async_response(#{<<"task_status">> := <<"PROCESSING">>} = Resp) ->
    {pending, Resp};
handle_async_response(#{<<"task_status">> := <<"FAIL">>} = Resp) ->
    {error, {task_failed, Resp}};
handle_async_response(Resp) ->
    parse_response(Resp).

%% @private 执行 GET 请求（用于异步结果查询）
%% 使用 beamai_http 作为底层 HTTP 客户端
do_get_request(Url, Headers, Opts) ->
    HttpOpts = #{
        timeout => maps:get(timeout, Opts, ?ZHIPU_TIMEOUT),
        connect_timeout => maps:get(connect_timeout, Opts, ?ZHIPU_CONNECT_TIMEOUT),
        headers => Headers
    },
    case beamai_http:get(Url, #{}, HttpOpts) of
        {ok, Response} when is_map(Response) ->
            {ok, Response};
        {ok, Response} when is_binary(Response) ->
            {ok, jsx:decode(Response, [return_maps])};
        {error, {http_error, Code, RespBody}} ->
            {error, {http_error, Code, RespBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%%====================================================================
%% 请求构建（使用公共模块）
%%====================================================================

%% @private 构建请求 URL
build_url(Config, DefaultEndpoint) ->
    llm_provider_common:build_url(Config, DefaultEndpoint, ?ZHIPU_BASE_URL).

%% @private 构建请求头
build_headers(Config) ->
    llm_provider_common:build_bearer_auth_headers(Config).

%% @private 构建请求选项
build_request_opts(Config) ->
    #{
        timeout => maps:get(timeout, Config, ?ZHIPU_TIMEOUT),
        connect_timeout => maps:get(connect_timeout, Config, ?ZHIPU_CONNECT_TIMEOUT)
    }.

%% @private 构建请求体
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    Base = #{
        <<"model">> => maps:get(model, Config, ?ZHIPU_MODEL),
        <<"messages">> => llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?ZHIPU_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?ZHIPU_TEMPERATURE)
    },
    build_body_pipeline(Base, Config, Request).

%% @private 请求体构建管道（使用公共模块）
build_body_pipeline(Body, Config, Request) ->
    ?BUILD_BODY_PIPELINE(Body, [
        fun(B) -> llm_provider_common:maybe_add_stream(B, Request) end,
        fun(B) -> llm_provider_common:maybe_add_tools(B, Request) end,
        fun(B) -> llm_provider_common:maybe_add_top_p(B, Config) end
    ]).

%%====================================================================
%% 响应解析
%%====================================================================

%% @private 解析响应
parse_response(#{<<"choices">> := [Choice | _]} = Resp) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    {ok, #{
        id => maps:get(<<"id">>, Resp, <<>>),
        model => maps:get(<<"model">>, Resp, <<>>),
        content => extract_content(Message),
        reasoning_content => maps:get(<<"reasoning_content">>, Message, null),
        tool_calls => parse_tool_calls(Message),
        finish_reason => maps:get(<<"finish_reason">>, Choice, <<>>),
        usage => parse_usage(maps:get(<<"usage">>, Resp, #{}))
    }};

parse_response(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
parse_response(_) ->
    {error, invalid_response}.

%% @private 提取内容（处理 GLM-4.6+ 的 reasoning_content 格式）
%% GLM-4.6 等模型可能返回空 content 但有 reasoning_content（思维链）
extract_content(Message) ->
    case maps:get(<<"content">>, Message, null) of
        Content when Content =/= null, Content =/= <<>> ->
            Content;
        _ ->
            %% 如果 content 为空，检查是否有 reasoning_content
            case maps:get(<<"reasoning_content">>, Message, null) of
                null -> null;
                <<>> -> null;
                ReasoningContent -> ReasoningContent
            end
    end.

%% @private 解析工具调用（使用公共模块）
parse_tool_calls(Message) ->
    llm_provider_common:parse_tool_calls(Message).

%% @private 解析使用统计（使用公共模块）
parse_usage(Usage) ->
    llm_provider_common:parse_usage(Usage).

%%====================================================================
%% 流式事件累加（OpenAI 兼容格式）
%%====================================================================

%% @private 智谱 AI 事件累加器（使用 OpenAI 兼容格式）
%% 支持 GLM-4.6+ 的 reasoning_content 字段
accumulate_event(#{<<"choices">> := [#{<<"delta">> := Delta} | _]} = Event, Acc) ->
    Content = maps:get(<<"content">>, Delta, <<>>),
    ReasoningContent = maps:get(<<"reasoning_content">>, Delta, <<>>),
    FinishReason = extract_finish_reason(Event, Acc),

    %% 累加 content 和 reasoning_content
    AccContent = maps:get(content, Acc, <<>>),
    AccReasoning = maps:get(reasoning_content, Acc, <<>>),

    NewContent = <<AccContent/binary, (beamai_utils:ensure_binary(Content))/binary>>,
    NewReasoning = <<AccReasoning/binary, (beamai_utils:ensure_binary(ReasoningContent))/binary>>,

    %% 如果 content 为空但有 reasoning_content，使用 reasoning_content 作为 content
    FinalContent = case NewContent of
        <<>> -> NewReasoning;
        _ -> NewContent
    end,

    Acc#{
        id => maps:get(<<"id">>, Event, maps:get(id, Acc)),
        model => maps:get(<<"model">>, Event, maps:get(model, Acc)),
        content => FinalContent,
        reasoning_content => NewReasoning,
        finish_reason => beamai_utils:ensure_binary(FinishReason)
    };
accumulate_event(_, Acc) ->
    Acc.

%% @private 提取完成原因
extract_finish_reason(#{<<"choices">> := [Choice | _]}, Acc) ->
    maps:get(<<"finish_reason">>, Choice, maps:get(finish_reason, Acc));
extract_finish_reason(_, Acc) ->
    maps:get(finish_reason, Acc).
