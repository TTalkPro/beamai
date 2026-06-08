%%%-------------------------------------------------------------------
%%% @doc 阿里云百炼 (Bailian/DashScope) LLM Provider 实现
%%%
%%% 支持阿里云百炼平台的 DashScope 原生 API。
%%% 使用 beamai_llm_http_client 处理公共 HTTP 逻辑。
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
-module(beamai_llm_provider_bailian).
-behaviour(beamai_llm_provider_behaviour).

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
    Opts = (build_request_opts(Config))#{
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    },
    beamai_llm_http_client:request(Url, Headers, Body, Opts, beamai_llm_response_parser:parser_dashscope()).

%% @doc 发送流式聊天请求
%% 流式累加结果经 finalize_dashscope_stream 重建为 DashScope 原始格式后解析，
%% 使流式响应与同步模式一致（统一 beamai_llm_response，含工具调用与 usage）。
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, get_endpoint(Config)),
    Headers = build_headers(Config, true),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = (build_request_opts(Config))#{
        finalizer => fun finalize_dashscope_stream/1,
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    },
    beamai_llm_http_client:stream_request(Url, Headers, Body, Opts, Callback, fun accumulate_event/2).

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
    beamai_llm_provider_common:build_url(Config, DefaultEndpoint, ?DASHSCOPE_BASE_URL).

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
        <<"messages">> => beamai_llm_message_adapter:to_openai(Messages)
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
    FormattedTools = beamai_llm_tool_adapter:to_openai(Tools),
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
            Calls ++ [beamai_llm_provider_common:parse_single_tool_call(NewCall)]
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
    %% DashScope 使用 input_tokens/output_tokens
    #{
        prompt_tokens => maps:get(<<"input_tokens">>, Usage, 0),
        completion_tokens => maps:get(<<"output_tokens">>, Usage, 0),
        total_tokens => maps:get(<<"total_tokens">>, Usage, 0)
    };
extract_usage(_, Acc) ->
    maps:get(usage, Acc, #{}).

%%====================================================================
%% 流式终结（重建 DashScope 原始格式 → 统一响应）
%%====================================================================

%% @private 将流式累加结果转换为统一响应
%% 把累加的 content / tool_calls / finish_reason / usage 重建为 DashScope
%% 原始响应结构，再交给 from_dashscope 解析，使流式与同步结构完全一致。
-spec finalize_dashscope_stream(map()) -> {ok, map()} | {error, term()}.
finalize_dashscope_stream(Acc) ->
    Message0 = #{
        <<"role">> => <<"assistant">>,
        <<"content">> => maps:get(content, Acc, <<>>)
    },
    Message = case maps:get(tool_calls, Acc, []) of
        [] -> Message0;
        Calls -> Message0#{<<"tool_calls">> => [rebuild_raw_tool_call(C) || C <- Calls]}
    end,
    FinishReason = case maps:get(finish_reason, Acc, <<>>) of
        <<>> -> <<"stop">>;
        FR when is_binary(FR) -> FR;
        _ -> <<"stop">>
    end,
    AccUsage = maps:get(usage, Acc, #{}),
    Raw = #{
        <<"request_id">> => maps:get(id, Acc, <<>>),
        <<"output">> => #{
            <<"choices">> => [#{
                <<"message">> => Message,
                <<"finish_reason">> => FinishReason
            }]
        },
        <<"usage">> => #{
            <<"input_tokens">> => maps:get(prompt_tokens, AccUsage, 0),
            <<"output_tokens">> => maps:get(completion_tokens, AccUsage, 0),
            <<"total_tokens">> => maps:get(total_tokens, AccUsage, 0)
        }
    },
    beamai_llm_response_parser:from_dashscope(Raw).

%% @private 将累加器中的工具调用（#{id, name, arguments}）重建为 OpenAI 原始格式
rebuild_raw_tool_call(#{id := Id, name := Name, arguments := Args}) ->
    #{
        <<"id">> => Id,
        <<"type">> => <<"function">>,
        <<"function">> => #{<<"name">> => Name, <<"arguments">> => Args}
    };
rebuild_raw_tool_call(Call) when is_map(Call) ->
    %% 兼容已是原始格式的情况
    Call.
