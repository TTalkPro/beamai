%%%-------------------------------------------------------------------
%%% @doc 阿里云百炼 (Bailian) LLM Provider 实现
%%%
%%% 支持阿里云百炼平台的 OpenAI 兼容 API。
%%% 使用 llm_http_client 处理公共 HTTP 逻辑。
%%%
%%% API 文档: https://help.aliyun.com/zh/model-studio/qwen-api-reference
%%%
%%% 支持的模型:
%%%   - qwen3-max (旗舰模型)
%%%   - qwen3-plus
%%%   - qwen-turbo
%%%   - 其他通义千问系列模型
%%%
%%% 特性:
%%%   - 同步对话补全
%%%   - 流式输出 (SSE)
%%%   - 工具调用 (Function Calling)
%%%
%%% 使用方式:
%%% ```
%%% ApiKey = os:getenv("BAILIAN_API_KEY"),
%%% Config = llm_client:create(bailian, #{
%%%     api_key => list_to_binary(ApiKey),
%%%     model => <<"qwen3-max">>
%%% }),
%%% {ok, Response} = llm_client:simple_chat(Config, <<"你好">>).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_provider_bailian).
-behaviour(llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

%% 默认值
-define(BAILIAN_BASE_URL, <<"https://dashscope.aliyuncs.com">>).
-define(BAILIAN_ENDPOINT, <<"/compatible-mode/v1/chat/completions">>).
-define(BAILIAN_MODEL, <<"qwen3-max">>).
-define(BAILIAN_TIMEOUT, 300000).
-define(BAILIAN_CONNECT_TIMEOUT, 10000).
-define(BAILIAN_MAX_TOKENS, 4096).
-define(BAILIAN_TEMPERATURE, 0.7).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"Bailian">>.

default_config() ->
    #{
        base_url => ?BAILIAN_BASE_URL,
        model => ?BAILIAN_MODEL,
        timeout => ?BAILIAN_TIMEOUT,
        max_tokens => ?BAILIAN_MAX_TOKENS,
        temperature => ?BAILIAN_TEMPERATURE
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
    Url = build_url(Config, ?BAILIAN_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = build_request_opts(Config),
    llm_http_client:request(Url, Headers, Body, Opts, fun parse_response/1).

%% @doc 发送流式聊天请求
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, ?BAILIAN_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = build_request_opts(Config),
    llm_http_client:stream_request(Url, Headers, Body, Opts, Callback, fun accumulate_event/2).

%%====================================================================
%% 请求构建（Provider 特定）
%%====================================================================

%% @private 构建请求 URL
build_url(Config, Endpoint) ->
    BaseUrl = maps:get(base_url, Config, ?BAILIAN_BASE_URL),
    <<BaseUrl/binary, Endpoint/binary>>.

%% @private 构建请求头
build_headers(#{api_key := ApiKey}) ->
    [
        {<<"Authorization">>, <<"Bearer ", ApiKey/binary>>},
        {<<"Content-Type">>, <<"application/json">>}
    ].

%% @private 构建请求选项
build_request_opts(Config) ->
    #{
        timeout => maps:get(timeout, Config, ?BAILIAN_TIMEOUT),
        connect_timeout => maps:get(connect_timeout, Config, ?BAILIAN_CONNECT_TIMEOUT)
    }.

%% @private 构建请求体
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    Base = #{
        <<"model">> => maps:get(model, Config, ?BAILIAN_MODEL),
        <<"messages">> => llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?BAILIAN_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?BAILIAN_TEMPERATURE)
    },
    build_body_pipeline(Base, Config, Request).

%% @private 请求体构建管道（使用宏）
build_body_pipeline(Body, Config, Request) ->
    ?BUILD_BODY_PIPELINE(Body, [
        fun(B) -> maybe_add_stream(B, Request) end,
        fun(B) -> maybe_add_tools(B, Request) end,
        fun(B) -> maybe_add_top_p(B, Config) end,
        fun(B) -> maybe_add_enable_search(B, Config) end
    ]).

%% @private 添加流式标志
maybe_add_stream(Body, #{stream := true}) -> Body#{<<"stream">> => true};
maybe_add_stream(Body, _) -> Body.

%% @private 添加工具定义
maybe_add_tools(Body, #{tools := Tools}) when Tools =/= [] ->
    FormattedTools = llm_tool_adapter:to_openai(Tools),
    ToolChoice = maps:get(tool_choice, Body, <<"auto">>),
    Body#{<<"tools">> => FormattedTools, <<"tool_choice">> => ToolChoice};
maybe_add_tools(Body, _) ->
    Body.

%% @private 添加 top_p 参数
maybe_add_top_p(Body, #{top_p := TopP}) -> Body#{<<"top_p">> => TopP};
maybe_add_top_p(Body, _) -> Body.

%% @private 添加联网搜索参数
maybe_add_enable_search(Body, #{enable_search := true}) ->
    Body#{<<"enable_search">> => true};
maybe_add_enable_search(Body, _) -> Body.

%%====================================================================
%% 响应解析
%%====================================================================

%% @private 解析响应
parse_response(#{<<"choices">> := [Choice | _]} = Resp) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    {ok, #{
        id => maps:get(<<"id">>, Resp, <<>>),
        model => maps:get(<<"model">>, Resp, <<>>),
        content => maps:get(<<"content">>, Message, null),
        tool_calls => parse_tool_calls(Message),
        finish_reason => maps:get(<<"finish_reason">>, Choice, <<>>),
        usage => parse_usage(maps:get(<<"usage">>, Resp, #{}))
    }};

parse_response(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
parse_response(_) ->
    {error, invalid_response}.

%% @private 解析工具调用
parse_tool_calls(#{<<"tool_calls">> := Calls}) when is_list(Calls) ->
    [parse_single_tool_call(C) || C <- Calls];
parse_tool_calls(_) ->
    [].

%% @private 解析单个工具调用
parse_single_tool_call(#{<<"id">> := Id, <<"function">> := Func}) ->
    #{
        id => Id,
        name => maps:get(<<"name">>, Func, <<>>),
        arguments => maps:get(<<"arguments">>, Func, <<>>)
    };
parse_single_tool_call(_) ->
    #{id => <<>>, name => <<>>, arguments => <<>>}.

%% @private 解析使用统计
parse_usage(Usage) ->
    #{
        prompt_tokens => maps:get(<<"prompt_tokens">>, Usage, 0),
        completion_tokens => maps:get(<<"completion_tokens">>, Usage, 0),
        total_tokens => maps:get(<<"total_tokens">>, Usage, 0)
    }.

%%====================================================================
%% 流式事件累加（OpenAI 兼容格式）
%%====================================================================

%% @private 百炼事件累加器（使用 OpenAI 兼容格式）
accumulate_event(#{<<"choices">> := [#{<<"delta">> := Delta} | _]} = Event, Acc) ->
    Content = maps:get(<<"content">>, Delta, <<>>),
    FinishReason = extract_finish_reason(Event, Acc),

    %% 累加 content
    AccContent = maps:get(content, Acc, <<>>),
    NewContent = <<AccContent/binary, (beamai_utils:ensure_binary(Content))/binary>>,

    Acc#{
        id => maps:get(<<"id">>, Event, maps:get(id, Acc)),
        model => maps:get(<<"model">>, Event, maps:get(model, Acc)),
        content => NewContent,
        finish_reason => beamai_utils:ensure_binary(FinishReason)
    };
accumulate_event(_, Acc) ->
    Acc.

%% @private 提取完成原因
extract_finish_reason(#{<<"choices">> := [Choice | _]}, Acc) ->
    maps:get(<<"finish_reason">>, Choice, maps:get(finish_reason, Acc));
extract_finish_reason(_, Acc) ->
    maps:get(finish_reason, Acc).
