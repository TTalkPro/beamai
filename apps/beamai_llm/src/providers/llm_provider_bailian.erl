%%%-------------------------------------------------------------------
%%% @doc 阿里云百炼 (Bailian/DashScope) LLM Provider 实现
%%%
%%% 支持阿里云百炼平台的 DashScope 原生 API。
%%% 使用 llm_http_client 处理公共 HTTP 逻辑。
%%%
%%% API 文档: https://help.aliyun.com/zh/model-studio/text-generation
%%%
%%% API 端点:
%%%   - 文本生成: POST https://dashscope.aliyuncs.com/api/v1/services/aigc/text-generation/generation
%%%   - 多模态生成: POST https://dashscope.aliyuncs.com/api/v1/services/aigc/multimodal-generation/generation
%%%
%%% 支持的模型:
%%%   - qwen-max (旗舰模型)
%%%   - qwen-plus (均衡推荐)
%%%   - qwen-turbo (快速低成本)
%%%   - qwen-vl-plus (视觉语言)
%%%   - 其他通义千问系列模型
%%%
%%% 特性:
%%%   - 同步对话补全
%%%   - 流式输出 (SSE)
%%%   - 工具调用 (Function Calling)
%%%   - 联网搜索
%%%
%%% 使用方式:
%%% ```
%%% ApiKey = os:getenv("BAILIAN_API_KEY"),
%%% Config = beamai_chat_completion:create(bailian, #{
%%%     api_key => list_to_binary(ApiKey),
%%%     model => <<"qwen-plus">>
%%% }),
%%% {ok, Response} = beamai_chat_completion:chat(Config, [#{role => user, content => <<"你好">>}]).
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

%% 默认值 - DashScope 原生 API
-define(DASHSCOPE_BASE_URL, <<"https://dashscope.aliyuncs.com">>).
-define(DASHSCOPE_TEXT_ENDPOINT, <<"/api/v1/services/aigc/text-generation/generation">>).
-define(DASHSCOPE_MULTIMODAL_ENDPOINT, <<"/api/v1/services/aigc/multimodal-generation/generation">>).
-define(DASHSCOPE_MODEL, <<"qwen-plus">>).
-define(DASHSCOPE_TIMEOUT, 300000).
-define(DASHSCOPE_CONNECT_TIMEOUT, 10000).
-define(DASHSCOPE_MAX_TOKENS, 4096).
-define(DASHSCOPE_TEMPERATURE, 0.7).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"Bailian (DashScope)">>.

default_config() ->
    #{
        base_url => ?DASHSCOPE_BASE_URL,
        model => ?DASHSCOPE_MODEL,
        timeout => ?DASHSCOPE_TIMEOUT,
        max_tokens => ?DASHSCOPE_MAX_TOKENS,
        temperature => ?DASHSCOPE_TEMPERATURE
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
    Url = build_url(Config, get_endpoint(Config)),
    Headers = build_headers(Config, false),
    Body = build_request_body(Config, Request),
    Opts = build_request_opts(Config),
    llm_http_client:request(Url, Headers, Body, Opts, fun parse_response/1).

%% @doc 发送流式聊天请求
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, get_endpoint(Config)),
    Headers = build_headers(Config, true),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = build_request_opts(Config),
    llm_http_client:stream_request(Url, Headers, Body, Opts, Callback, fun accumulate_event/2).

%%====================================================================
%% 请求构建（DashScope 原生格式）
%%====================================================================

%% @private 根据模型选择端点
%% VL 模型使用多模态端点
get_endpoint(#{model := Model}) ->
    case is_multimodal_model(Model) of
        true -> ?DASHSCOPE_MULTIMODAL_ENDPOINT;
        false -> ?DASHSCOPE_TEXT_ENDPOINT
    end;
get_endpoint(_) ->
    ?DASHSCOPE_TEXT_ENDPOINT.

%% @private 判断是否为多模态模型
is_multimodal_model(Model) when is_binary(Model) ->
    case binary:match(Model, [<<"-vl">>, <<"-audio">>, <<"-omni">>]) of
        nomatch -> false;
        _ -> true
    end;
is_multimodal_model(_) ->
    false.

%% @private 构建请求 URL（使用公共模块）
build_url(Config, DefaultEndpoint) ->
    llm_provider_common:build_url(Config, DefaultEndpoint, ?DASHSCOPE_BASE_URL).

%% @private 构建请求头
%% DashScope 原生 API 流式输出需要 X-DashScope-SSE 头
build_headers(#{api_key := ApiKey}, IsStream) ->
    BaseHeaders = [
        {<<"Authorization">>, <<"Bearer ", ApiKey/binary>>},
        {<<"Content-Type">>, <<"application/json">>}
    ],
    case IsStream of
        true -> [{<<"X-DashScope-SSE">>, <<"enable">>} | BaseHeaders];
        false -> BaseHeaders
    end.

%% @private 构建请求选项
build_request_opts(Config) ->
    #{
        timeout => maps:get(timeout, Config, ?DASHSCOPE_TIMEOUT),
        connect_timeout => maps:get(connect_timeout, Config, ?DASHSCOPE_CONNECT_TIMEOUT)
    }.

%% @private 构建请求体 - DashScope 原生格式
%% 格式: {model, input: {messages}, parameters: {...}}
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),

    %% 构建 input 对象
    Input = #{
        <<"messages">> => llm_message_adapter:to_openai(Messages)
    },

    %% 构建 parameters 对象
    Parameters = build_parameters(Config, Request),

    #{
        <<"model">> => maps:get(model, Config, ?DASHSCOPE_MODEL),
        <<"input">> => Input,
        <<"parameters">> => Parameters
    }.

%% @private 构建 parameters 对象
build_parameters(Config, Request) ->
    Base = #{
        <<"result_format">> => <<"message">>,
        <<"max_tokens">> => maps:get(max_tokens, Config, ?DASHSCOPE_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?DASHSCOPE_TEMPERATURE)
    },
    build_parameters_pipeline(Base, Config, Request).

%% @private parameters 构建管道
build_parameters_pipeline(Params, Config, Request) ->
    ?BUILD_BODY_PIPELINE(Params, [
        fun(P) -> maybe_add_stream_param(P, Request) end,
        fun(P) -> maybe_add_tools_param(P, Request) end,
        fun(P) -> maybe_add_top_p_param(P, Config) end,
        fun(P) -> maybe_add_enable_search_param(P, Config) end,
        fun(P) -> maybe_add_tool_choice_param(P, Request) end
    ]).

%% @private 添加流式参数
maybe_add_stream_param(Params, #{stream := true}) ->
    Params#{<<"incremental_output">> => true};
maybe_add_stream_param(Params, _) ->
    Params.

%% @private 添加工具定义
maybe_add_tools_param(Params, #{tools := Tools}) when Tools =/= [] ->
    FormattedTools = llm_tool_adapter:to_openai(Tools),
    Params#{<<"tools">> => FormattedTools};
maybe_add_tools_param(Params, _) ->
    Params.

%% @private 添加 top_p 参数
maybe_add_top_p_param(Params, #{top_p := TopP}) ->
    Params#{<<"top_p">> => TopP};
maybe_add_top_p_param(Params, _) ->
    Params.

%% @private 添加联网搜索参数
maybe_add_enable_search_param(Params, #{enable_search := true}) ->
    Params#{<<"enable_search">> => true};
maybe_add_enable_search_param(Params, _) ->
    Params.

%% @private 添加 tool_choice 参数
maybe_add_tool_choice_param(Params, #{tool_choice := ToolChoice}) ->
    Params#{<<"tool_choice">> => ToolChoice};
maybe_add_tool_choice_param(Params, _) ->
    Params.

%%====================================================================
%% 响应解析（DashScope 原生格式）
%%====================================================================

%% @private 解析响应 - DashScope 原生格式
%% 响应格式: {output: {choices: [...]}, usage: {...}, request_id: "..."}
parse_response(#{<<"output">> := Output} = Resp) ->
    parse_output(Output, Resp);

parse_response(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};

parse_response(#{<<"code">> := Code, <<"message">> := Message}) ->
    %% DashScope 错误格式
    {error, {api_error, #{code => Code, message => Message}}};

parse_response(Response) ->
    {error, {invalid_response, Response}}.

%% @private 解析 output 对象
parse_output(#{<<"choices">> := [Choice | _]}, Resp) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    {ok, #{
        id => maps:get(<<"request_id">>, Resp, <<>>),
        model => <<>>,  %% DashScope 响应不包含 model
        content => maps:get(<<"content">>, Message, null),
        tool_calls => parse_tool_calls(Message),
        finish_reason => maps:get(<<"finish_reason">>, Choice, <<>>),
        usage => parse_usage(maps:get(<<"usage">>, Resp, #{}))
    }};

%% 兼容旧格式（text + finish_reason 直接在 output 下）
parse_output(#{<<"text">> := Text, <<"finish_reason">> := FinishReason}, Resp) ->
    {ok, #{
        id => maps:get(<<"request_id">>, Resp, <<>>),
        model => <<>>,
        content => Text,
        tool_calls => [],
        finish_reason => FinishReason,
        usage => parse_usage(maps:get(<<"usage">>, Resp, #{}))
    }};

parse_output(Output, _Resp) ->
    {error, {invalid_output, Output}}.

%% @private 解析工具调用（使用公共模块）
parse_tool_calls(Message) ->
    llm_provider_common:parse_tool_calls(Message).

%% @private 解析使用统计 - DashScope 使用 input_tokens/output_tokens
parse_usage(Usage) ->
    #{
        prompt_tokens => maps:get(<<"input_tokens">>, Usage, 0),
        completion_tokens => maps:get(<<"output_tokens">>, Usage, 0),
        total_tokens => maps:get(<<"total_tokens">>, Usage, 0)
    }.

%%====================================================================
%% 流式事件累加（DashScope 原生格式）
%%====================================================================

%% @private DashScope 事件累加器
%% DashScope SSE 格式与 OpenAI 略有不同，数据在 output.choices 下
accumulate_event(#{<<"output">> := Output} = Event, Acc) ->
    accumulate_output_event(Output, Event, Acc);

%% 兼容增量输出格式（output.text 直接累加）
accumulate_event(#{<<"choices">> := [#{<<"delta">> := Delta} | _]} = Event, Acc) ->
    %% 兼容 OpenAI 风格的流式格式
    Content = maps:get(<<"content">>, Delta, <<>>),
    FinishReason = extract_finish_reason_openai(Event, Acc),
    accumulate_content(Content, FinishReason, Event, Acc);

accumulate_event(_, Acc) ->
    Acc.

%% @private 累加 output 格式的事件
accumulate_output_event(#{<<"choices">> := [Choice | _]}, Event, Acc) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    Content = maps:get(<<"content">>, Message, <<>>),
    FinishReason = maps:get(<<"finish_reason">>, Choice, maps:get(finish_reason, Acc)),

    %% 累加工具调用
    NewAcc = accumulate_tool_calls(Message, Acc),
    accumulate_content(Content, FinishReason, Event, NewAcc);

%% 旧格式：text 直接在 output 下
accumulate_output_event(#{<<"text">> := Text} = Output, Event, Acc) ->
    FinishReason = maps:get(<<"finish_reason">>, Output, maps:get(finish_reason, Acc)),
    accumulate_content(Text, FinishReason, Event, Acc);

accumulate_output_event(_, _, Acc) ->
    Acc.

%% @private 累加内容
accumulate_content(Content, FinishReason, Event, Acc) ->
    AccContent = maps:get(content, Acc, <<>>),
    NewContent = <<AccContent/binary, (beamai_utils:ensure_binary(Content))/binary>>,

    Acc#{
        id => maps:get(<<"request_id">>, Event, maps:get(id, Acc, <<>>)),
        content => NewContent,
        finish_reason => beamai_utils:ensure_binary(FinishReason),
        usage => extract_usage(Event, Acc)
    }.

%% @private 累加工具调用（处理流式工具调用）
accumulate_tool_calls(#{<<"tool_calls">> := Calls}, Acc) when is_list(Calls) ->
    ExistingCalls = maps:get(tool_calls, Acc, []),
    %% 合并工具调用（按 index 或 id 累加 arguments）
    NewCalls = merge_tool_calls(ExistingCalls, Calls),
    Acc#{tool_calls => NewCalls};
accumulate_tool_calls(_, Acc) ->
    Acc.

%% @private 合并工具调用
merge_tool_calls(Existing, New) ->
    lists:foldl(fun(Call, Acc) ->
        Index = maps:get(<<"index">>, Call, 0),
        merge_single_tool_call(Acc, Index, Call)
    end, Existing, New).

%% @private 合并单个工具调用
%% 优化：将嵌套逻辑拆分为独立函数
merge_single_tool_call(Calls, Index, NewCall) ->
    case Index < length(Calls) of
        true ->
            update_call_at_index(Calls, Index, NewCall);
        false ->
            Calls ++ [llm_provider_common:parse_single_tool_call(NewCall)]
    end.

%% @private 更新指定索引位置的工具调用
%% 将 lists:map + lists:zip 的复杂逻辑提取为独立函数
-spec update_call_at_index([map()], non_neg_integer(), map()) -> [map()].
update_call_at_index(Calls, TargetIndex, NewCall) ->
    {Updated, _} = lists:mapfoldl(
        fun(Call, CurrentIndex) ->
            NewValue = case CurrentIndex of
                TargetIndex -> merge_call_data(Call, NewCall);
                _ -> Call
            end,
            {NewValue, CurrentIndex + 1}
        end,
        0,
        Calls
    ),
    Updated.

%% @private 合并调用数据
merge_call_data(Existing, New) ->
    Func = maps:get(<<"function">>, New, #{}),
    NewArgs = maps:get(<<"arguments">>, Func, <<>>),
    ExistingArgs = maps:get(arguments, Existing, <<>>),
    Existing#{
        arguments => <<ExistingArgs/binary, NewArgs/binary>>
    }.

%% @private 提取 OpenAI 格式的完成原因
extract_finish_reason_openai(#{<<"choices">> := [Choice | _]}, Acc) ->
    maps:get(<<"finish_reason">>, Choice, maps:get(finish_reason, Acc));
extract_finish_reason_openai(_, Acc) ->
    maps:get(finish_reason, Acc).

%% @private 提取 usage
extract_usage(#{<<"usage">> := Usage}, _Acc) ->
    parse_usage(Usage);
extract_usage(_, Acc) ->
    maps:get(usage, Acc, #{}).
