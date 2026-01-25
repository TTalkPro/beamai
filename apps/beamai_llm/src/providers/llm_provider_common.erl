%%%-------------------------------------------------------------------
%%% @doc LLM Provider 公共函数模块
%%%
%%% 抽取多个 Provider 共用的函数，减少代码重复。
%%% 遵循 DRY 原则，提供以下公共功能：
%%%
%%% == URL 构建 ==
%%% - build_url/3: 构建请求 URL（base_url + endpoint）
%%%
%%% == 请求头构建 ==
%%% - build_bearer_auth_headers/1: 构建 Bearer Token 认证头
%%%
%%% == 请求体构建辅助 ==
%%% - maybe_add_stream/2: 添加流式标志
%%% - maybe_add_tools/2: 添加 OpenAI 格式工具定义
%%% - maybe_add_top_p/2: 添加 top_p 参数
%%%
%%% == 流式响应累加 ==
%%% - accumulate_openai_event/2: OpenAI 格式事件累加器
%%%
%%% == 响应解析 ==
%%% - parse_tool_calls/1: 解析 OpenAI 格式工具调用
%%% - parse_single_tool_call/1: 解析单个工具调用
%%% - parse_usage/1: 解析 OpenAI 格式使用统计
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 在 Provider 模块中使用
%%% -module(llm_provider_xxx).
%%%
%%% build_url(Config, DefaultEndpoint) ->
%%%     llm_provider_common:build_url(Config, DefaultEndpoint, ?XXX_BASE_URL).
%%%
%%% build_headers(Config) ->
%%%     llm_provider_common:build_bearer_auth_headers(Config).
%%%
%%% build_request_body(Config, Request) ->
%%%     Base = #{...},
%%%     ?BUILD_BODY_PIPELINE(Base, [
%%%         fun(B) -> llm_provider_common:maybe_add_stream(B, Request) end,
%%%         fun(B) -> llm_provider_common:maybe_add_tools(B, Request) end
%%%     ]).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_provider_common).

%% API 导出
-export([
    %% URL 构建
    build_url/3,

    %% 请求头构建
    build_bearer_auth_headers/1,

    %% 请求体辅助函数
    maybe_add_stream/2,
    maybe_add_tools/2,
    maybe_add_top_p/2,

    %% 流式响应累加
    accumulate_openai_event/2,

    %% 响应解析
    parse_tool_calls/1,
    parse_single_tool_call/1,
    parse_usage/1
]).

%%====================================================================
%% URL 构建
%%====================================================================

%% @doc 构建请求 URL
%%
%% 将 base_url 和 endpoint 拼接成完整 URL。
%% 支持通过 Config 覆盖默认值，便于第三方 API 代理兼容
%% （如 one-api、new-api 等）。
%%
%% @param Config Provider 配置 map，可包含 base_url 和 endpoint
%% @param DefaultEndpoint 默认端点路径
%% @param DefaultBaseUrl 默认基础 URL
%% @returns 完整请求 URL（binary）
-spec build_url(map(), binary(), binary()) -> binary().
build_url(Config, DefaultEndpoint, DefaultBaseUrl) ->
    BaseUrl = maps:get(base_url, Config, DefaultBaseUrl),
    Endpoint = maps:get(endpoint, Config, DefaultEndpoint),
    <<BaseUrl/binary, Endpoint/binary>>.

%%====================================================================
%% 请求头构建
%%====================================================================

%% @doc 构建 Bearer Token 认证请求头
%%
%% 生成标准的 Bearer Token 认证头，用于 OpenAI 兼容 API。
%% 包含 Authorization 和 Content-Type 头。
%%
%% @param Config Provider 配置 map，必须包含 api_key
%% @returns 请求头列表 [{Name, Value}]
-spec build_bearer_auth_headers(map()) -> [{binary(), binary()}].
build_bearer_auth_headers(#{api_key := ApiKey}) ->
    [
        {<<"Authorization">>, <<"Bearer ", ApiKey/binary>>},
        {<<"Content-Type">>, <<"application/json">>}
    ].

%%====================================================================
%% 请求体辅助函数
%%====================================================================

%% @doc 根据请求参数添加流式标志
%%
%% 如果请求中包含 stream => true，则在请求体中添加流式标志。
%%
%% @param Body 当前请求体 map
%% @param Request 原始请求参数 map
%% @returns 更新后的请求体 map
-spec maybe_add_stream(map(), map()) -> map().
maybe_add_stream(Body, #{stream := true}) -> Body#{<<"stream">> => true};
maybe_add_stream(Body, _) -> Body.

%% @doc 根据请求参数添加工具定义
%%
%% 如果请求中包含非空的 tools 列表，则添加 OpenAI 格式的工具定义。
%% 同时设置 tool_choice 为 "auto"（如果未指定）。
%%
%% @param Body 当前请求体 map
%% @param Request 原始请求参数 map
%% @returns 更新后的请求体 map
-spec maybe_add_tools(map(), map()) -> map().
maybe_add_tools(Body, #{tools := Tools}) when Tools =/= [] ->
    FormattedTools = llm_tool_adapter:to_openai(Tools),
    ToolChoice = maps:get(tool_choice, Body, <<"auto">>),
    Body#{<<"tools">> => FormattedTools, <<"tool_choice">> => ToolChoice};
maybe_add_tools(Body, _) ->
    Body.

%% @doc 根据配置添加 top_p 参数
%%
%% 如果配置中包含 top_p 且为有效数值，则添加到请求体。
%%
%% @param Body 当前请求体 map
%% @param Config Provider 配置 map
%% @returns 更新后的请求体 map
-spec maybe_add_top_p(map(), map()) -> map().
maybe_add_top_p(Body, #{top_p := TopP}) when is_number(TopP) ->
    Body#{<<"top_p">> => TopP};
maybe_add_top_p(Body, _) ->
    Body.

%%====================================================================
%% 流式响应累加
%%====================================================================

%% @doc OpenAI 格式流式事件累加器
%%
%% 累加 OpenAI 兼容 API 的 SSE 事件。
%% 从 delta 中提取 content 并追加到累加器。
%% 同时提取 id、model 和 finish_reason。
%%
%% 此函数作为 llm_http_client:stream_request 的 accumulator 参数使用。
%%
%% @param Event 解析后的 SSE 事件 map
%% @param Acc 当前累加器 map（需包含 content 字段）
%% @returns 更新后的累加器 map
-spec accumulate_openai_event(map(), map()) -> map().
accumulate_openai_event(#{<<"choices">> := [#{<<"delta">> := Delta} | _]} = Event, Acc) ->
    Content = maps:get(<<"content">>, Delta, <<>>),
    ContentBin = beamai_utils:ensure_binary(Content),
    FinishReason = extract_finish_reason(Event, Acc),
    Acc#{
        id => maps:get(<<"id">>, Event, maps:get(id, Acc, <<>>)),
        model => maps:get(<<"model">>, Event, maps:get(model, Acc, <<>>)),
        content => <<(maps:get(content, Acc, <<>>))/binary, ContentBin/binary>>,
        finish_reason => beamai_utils:ensure_binary(FinishReason)
    };
accumulate_openai_event(_, Acc) ->
    Acc.

%% @private 从事件中提取完成原因
-spec extract_finish_reason(map(), map()) -> binary() | undefined.
extract_finish_reason(#{<<"choices">> := [Choice | _]}, Acc) ->
    maps:get(<<"finish_reason">>, Choice, maps:get(finish_reason, Acc, undefined));
extract_finish_reason(_, Acc) ->
    maps:get(finish_reason, Acc, undefined).

%%====================================================================
%% 响应解析
%%====================================================================

%% @doc 解析 OpenAI 格式的工具调用列表
%%
%% 从消息 map 中提取 tool_calls 字段并解析为统一格式。
%%
%% @param Message 消息 map，可能包含 tool_calls 字段
%% @returns 工具调用列表 [#{id, name, arguments}]
-spec parse_tool_calls(map()) -> [map()].
parse_tool_calls(#{<<"tool_calls">> := Calls}) when is_list(Calls) ->
    [parse_single_tool_call(C) || C <- Calls];
parse_tool_calls(_) ->
    [].

%% @doc 解析单个 OpenAI 格式的工具调用
%%
%% @param Call 工具调用 map
%% @returns 标准化的工具调用 map #{id, name, arguments}
-spec parse_single_tool_call(map()) -> map().
parse_single_tool_call(#{<<"id">> := Id, <<"function">> := Func}) ->
    #{
        id => Id,
        name => maps:get(<<"name">>, Func, <<>>),
        arguments => maps:get(<<"arguments">>, Func, <<>>)
    };
parse_single_tool_call(#{<<"function">> := Func}) ->
    %% 某些 Provider 可能不返回 id
    #{
        id => generate_tool_call_id(),
        name => maps:get(<<"name">>, Func, <<>>),
        arguments => maps:get(<<"arguments">>, Func, <<>>)
    };
parse_single_tool_call(_) ->
    #{id => <<>>, name => <<>>, arguments => <<>>}.

%% @doc 解析 OpenAI 格式的使用统计
%%
%% @param Usage 使用统计 map
%% @returns 标准化的使用统计 map #{prompt_tokens, completion_tokens, total_tokens}
-spec parse_usage(map()) -> map().
parse_usage(Usage) ->
    #{
        prompt_tokens => maps:get(<<"prompt_tokens">>, Usage, 0),
        completion_tokens => maps:get(<<"completion_tokens">>, Usage, 0),
        total_tokens => maps:get(<<"total_tokens">>, Usage, 0)
    }.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 生成工具调用 ID
-spec generate_tool_call_id() -> binary().
generate_tool_call_id() ->
    Rand = integer_to_binary(rand:uniform(1000000000)),
    <<"call_", Rand/binary>>.
