%%%-------------------------------------------------------------------
%%% @doc xAI (Grok) LLM Provider 实现
%%%
%%% 支持 xAI 的 Chat Completions API（OpenAI 兼容），模型为 Grok 系列。
%%% API 文档: https://docs.x.ai/docs/api-reference
%%%
%%% 支持的功能：
%%%   - 基本对话 (chat/stream_chat)
%%%   - 多模态输入（图片，经 message_adapter 转为 image_url）
%%%   - 工具调用 (tools + tool_choice)，流式分片工具调用累加
%%%   - 推理强度 (reasoning_effort: none / low / medium / high)
%%%   - 思维链内容 (reasoning_content，推理模型返回)
%%%   - 结构化输出 (response_format: json_object / json_schema)
%%%   - 对数概率 (logprobs, top_logprobs) / 并行工具调用开关
%%%   - 流式 usage 统计 (stream_options.include_usage)
%%%
%%% 注意（对齐 Vercel AI SDK 的 xai provider）：
%%%   - xAI 不支持 frequency_penalty / presence_penalty / stop / top_k，
%%%     即使配置了也不会发送，避免 400。
%%%   - grok-4.20 的 reasoning / non-reasoning 变体拒绝 reasoning_effort
%%%     （含 none），构建请求时自动剔除。
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% Config = beamai_chat_model:create(xai, #{
%%%     api_key => <<"xai-...">>,
%%%     model => <<"grok-4.5">>,
%%%     reasoning_effort => <<"high">>
%%% }),
%%% {ok, Resp} = beamai_chat_model:chat(Config, Messages).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_xai).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

-ifdef(TEST).
-export([build_request_body/2, supports_reasoning_effort/1]).
-endif.

%% 默认值
-define(XAI_BASE_URL, <<"https://api.x.ai">>).
-define(XAI_ENDPOINT, <<"/v1/chat/completions">>).
-define(XAI_MODEL, <<"grok-4.5">>).
-define(XAI_MAX_TOKENS, 4096).
-define(XAI_TEMPERATURE, 0.7).

%% Config 中可选参数与 xAI API 字段的映射
%% （xAI 不支持 frequency_penalty / presence_penalty / stop / top_k，故不在表内）
-define(OPTIONAL_PARAMS, [
    {n, <<"n">>},
    {seed, <<"seed">>},
    {user, <<"user">>},
    {logprobs, <<"logprobs">>},
    {top_logprobs, <<"top_logprobs">>},
    {parallel_function_calling, <<"parallel_function_calling">>},
    {response_format, <<"response_format">>}
]).

%% grok-4.20 的 reasoning / non-reasoning 变体拒绝 reasoning_effort
-define(NO_REASONING_EFFORT_RE, "^grok-4\\.20(-[0-9]{4})?-(non-)?reasoning$").

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"xAI">>.

default_config() ->
    #{
        base_url => ?XAI_BASE_URL,
        model => ?XAI_MODEL,
        timeout => beamai_llm_provider_common:default_timeout(xai),
        max_tokens => ?XAI_MAX_TOKENS,
        temperature => ?XAI_TEMPERATURE
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
    Url = build_url(Config),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, xai),
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    }, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_llm_response_parser:parser_xai()).

%% @doc 发送流式聊天请求
%% 流式累加结果经 finalize_openai_stream 转换为与同步一致的统一响应
%% （含分片工具调用拼接和 reasoning_content 累加）。
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, xai),
        finalizer => fun(Acc) ->
            beamai_llm_provider_common:finalize_openai_stream(Acc, xai)
        end,
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    }, Config),
    beamai_llm_http_client:stream_request(Url, Headers, Body, Opts, Callback,
                                          fun beamai_llm_provider_common:accumulate_openai_event/2).

%%====================================================================
%% 请求构建
%%====================================================================

%% @private 构建请求 URL
build_url(Config) ->
    beamai_llm_provider_common:build_url(Config, ?XAI_ENDPOINT, ?XAI_BASE_URL).

%% @private 构建请求头
build_headers(Config) ->
    beamai_llm_provider_common:build_bearer_auth_headers(Config).

%% @private 构建请求体（使用管道模式）
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    Base = #{
        <<"model">> => maps:get(model, Config, ?XAI_MODEL),
        <<"messages">> => beamai_llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?XAI_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?XAI_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> beamai_llm_provider_common:maybe_add_top_p(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Config, ?OPTIONAL_PARAMS) end,
        fun(B) -> maybe_add_reasoning_effort(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tools(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tool_choice(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Request) end,
        fun(B) -> maybe_add_stream_options(B, Config, Request) end
    ]).

%% @private 添加推理强度（模型不支持时静默剔除）
maybe_add_reasoning_effort(#{<<"model">> := Model} = Body, Config) ->
    case maps:get(reasoning_effort, Config, undefined) of
        undefined ->
            Body;
        Effort ->
            case supports_reasoning_effort(Model) of
                true -> Body#{<<"reasoning_effort">> => Effort};
                false -> Body
            end
    end;
maybe_add_reasoning_effort(Body, _Config) ->
    Body.

%% @doc 模型是否接受 reasoning_effort 参数
%% grok-4.20 的 reasoning / non-reasoning 变体（含日期版本）对任何取值都报错。
-spec supports_reasoning_effort(binary()) -> boolean().
supports_reasoning_effort(Model) when is_binary(Model) ->
    re:run(Model, ?NO_REASONING_EFFORT_RE, [{capture, none}]) =:= nomatch;
supports_reasoning_effort(_) ->
    true.

%% @private 流式模式下添加 stream_options（默认开启 include_usage）
maybe_add_stream_options(Body, Config, #{stream := true}) ->
    case maps:get(stream_include_usage, Config, true) of
        true -> Body#{<<"stream_options">> => #{<<"include_usage">> => true}};
        false -> Body
    end;
maybe_add_stream_options(Body, _Config, _Request) ->
    Body.
