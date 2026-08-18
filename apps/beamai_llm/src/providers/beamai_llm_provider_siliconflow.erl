%%%-------------------------------------------------------------------
%%% @doc SiliconFlow（硅基流动）LLM Provider 实现
%%%
%%% 硅基流动是国内的多模型托管平台，接口与 OpenAI Chat Completions 兼容，
%%% 模型名带厂商前缀（如 `deepseek-ai/DeepSeek-V3'、`Qwen/Qwen3-32B'）。
%%% API 文档: https://docs.siliconflow.cn/api-reference
%%%
%%% == 站点区域 ==
%%%
%%%   - `cn'（默认）: https://api.siliconflow.cn
%%%   - `global'    : https://api.siliconflow.com
%%%
%%% 支持的功能：
%%%   - 基本对话 (chat/stream_chat)
%%%   - 多模态输入（图片 / 视频，经 message_adapter 转换）
%%%   - 工具调用 (tools + tool_choice)，流式分片工具调用累加
%%%   - 混合推理开关 (enable_thinking + thinking_budget，Qwen3 / GLM 等)
%%%   - 思维链内容 (reasoning_content，同步与流式均支持)
%%%   - 采样参数 (temperature / top_p / top_k / min_p /
%%%     frequency_penalty / presence_penalty / n / stop)
%%%   - 结构化输出 (response_format: json_object / json_schema)
%%%   - 流式 usage 统计 (stream_options.include_usage)
%%%
%%% 文本向量化见 beamai_embedding（provider = siliconflow）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_siliconflow).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

-ifdef(TEST).
-export([build_request_body/2, build_url/1]).
-endif.

%% 默认值
-define(SILICONFLOW_BASE_URL_CN, <<"https://api.siliconflow.cn">>).
-define(SILICONFLOW_BASE_URL_GLOBAL, <<"https://api.siliconflow.com">>).
-define(SILICONFLOW_ENDPOINT, <<"/v1/chat/completions">>).
-define(SILICONFLOW_MODEL, <<"deepseek-ai/DeepSeek-V3">>).
-define(SILICONFLOW_MAX_TOKENS, 4096).
-define(SILICONFLOW_TEMPERATURE, 0.7).

%% Config 中可选参数与 SiliconFlow API 字段的映射
-define(OPTIONAL_PARAMS, [
    {frequency_penalty, <<"frequency_penalty">>},
    {presence_penalty, <<"presence_penalty">>},
    {top_k, <<"top_k">>},
    {min_p, <<"min_p">>},
    {n, <<"n">>},
    {stop, <<"stop">>},
    {user, <<"user">>},
    {response_format, <<"response_format">>},
    %% 混合推理模型（Qwen3 / GLM 等）的思考开关与预算
    {enable_thinking, <<"enable_thinking">>},
    {thinking_budget, <<"thinking_budget">>}
]).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"SiliconFlow">>.

default_config() ->
    #{
        region => cn,
        model => ?SILICONFLOW_MODEL,
        timeout => beamai_llm_provider_common:default_timeout(siliconflow),
        max_tokens => ?SILICONFLOW_MAX_TOKENS,
        temperature => ?SILICONFLOW_TEMPERATURE
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
        timeout => beamai_llm_provider_common:request_timeout(Config, siliconflow),
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    }, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_llm_response_parser:parser_siliconflow()).

%% @doc 发送流式聊天请求
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, siliconflow),
        finalizer => fun(Acc) ->
            beamai_llm_provider_common:finalize_openai_stream(Acc, siliconflow)
        end,
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    }, Config),
    beamai_llm_http_client:stream_request(Url, Headers, Body, Opts, Callback,
                                          fun beamai_llm_provider_common:accumulate_openai_event/2).

%%====================================================================
%% 请求构建
%%====================================================================

%% @private 构建请求 URL（base_url 优先，其次按 region 选择站点）
build_url(Config) ->
    Default = region_base_url(maps:get(region, Config, cn)),
    beamai_llm_provider_common:build_url(Config, ?SILICONFLOW_ENDPOINT, Default).

%% @private 区域对应的站点
region_base_url(global) -> ?SILICONFLOW_BASE_URL_GLOBAL;
region_base_url(_) -> ?SILICONFLOW_BASE_URL_CN.

%% @private 构建请求头
build_headers(Config) ->
    beamai_llm_provider_common:build_bearer_auth_headers(Config).

%% @private 构建请求体（使用管道模式）
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    Base = #{
        <<"model">> => maps:get(model, Config, ?SILICONFLOW_MODEL),
        <<"messages">> => beamai_llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?SILICONFLOW_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?SILICONFLOW_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> beamai_llm_provider_common:maybe_add_top_p(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Config, ?OPTIONAL_PARAMS) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tools(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tool_choice(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Request) end,
        fun(B) -> maybe_add_stream_options(B, Config, Request) end
    ]).

%% @private 流式模式下添加 stream_options（默认开启 include_usage）
maybe_add_stream_options(Body, Config, #{stream := true}) ->
    case maps:get(stream_include_usage, Config, true) of
        true -> Body#{<<"stream_options">> => #{<<"include_usage">> => true}};
        false -> Body
    end;
maybe_add_stream_options(Body, _Config, _Request) ->
    Body.
