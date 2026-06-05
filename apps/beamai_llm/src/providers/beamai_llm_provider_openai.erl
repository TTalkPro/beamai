%%%-------------------------------------------------------------------
%%% @doc OpenAI LLM Provider 实现
%%%
%%% 支持 OpenAI API 及兼容接口（如 Azure OpenAI、vLLM）。
%%% 使用 beamai_llm_http_client 处理公共 HTTP 逻辑。
%%%
%%% 支持的功能：
%%%   - 基本对话 (chat/stream_chat)
%%%   - 工具调用 (tools + tool_choice)，流式分片工具调用累加
%%%   - 采样参数 (temperature, top_p, frequency_penalty, presence_penalty)
%%%   - 确定性输出 (seed) / 多候选 (n) / 停止序列 (stop)
%%%   - 对数概率 (logprobs, top_logprobs) / token 偏置 (logit_bias)
%%%   - 结构化输出 (response_format: text / json_object / json_schema)
%%%   - 推理模型参数 (reasoning_effort, max_completion_tokens)
%%%   - 流式 usage 统计 (stream_options.include_usage)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_openai).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

%% 默认值
-define(OPENAI_BASE_URL, <<"https://api.openai.com">>).
-define(OPENAI_ENDPOINT, <<"/v1/chat/completions">>).
-define(OPENAI_MODEL, <<"gpt-4">>).
-define(OPENAI_TIMEOUT, 60000).
-define(OPENAI_MAX_TOKENS, 4096).
-define(OPENAI_TEMPERATURE, 0.7).

%% Config 中可选参数与 OpenAI API 字段的映射
-define(OPTIONAL_PARAMS, [
    {frequency_penalty, <<"frequency_penalty">>},
    {presence_penalty, <<"presence_penalty">>},
    {n, <<"n">>},
    {seed, <<"seed">>},
    {stop, <<"stop">>},
    {logprobs, <<"logprobs">>},
    {top_logprobs, <<"top_logprobs">>},
    {logit_bias, <<"logit_bias">>},
    {user, <<"user">>},
    {parallel_tool_calls, <<"parallel_tool_calls">>},
    {reasoning_effort, <<"reasoning_effort">>},
    {service_tier, <<"service_tier">>},
    {response_format, <<"response_format">>}
]).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"OpenAI">>.

default_config() ->
    #{
        base_url => ?OPENAI_BASE_URL,
        model => ?OPENAI_MODEL,
        timeout => ?OPENAI_TIMEOUT,
        max_tokens => ?OPENAI_MAX_TOKENS,
        temperature => ?OPENAI_TEMPERATURE
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
    Url = build_url(Config, ?OPENAI_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = #{timeout => maps:get(timeout, Config, ?OPENAI_TIMEOUT)},
    beamai_llm_http_client:request(Url, Headers, Body, Opts, beamai_llm_response_parser:parser_openai()).

%% @doc 发送流式聊天请求
%% 流式累加结果经 finalize_openai_stream 转换为与同步模式一致的统一响应
%% （含分片工具调用拼接和末尾 usage chunk 捕获）。
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, ?OPENAI_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = #{
        timeout => maps:get(timeout, Config, ?OPENAI_TIMEOUT),
        finalizer => fun(Acc) ->
            beamai_llm_provider_common:finalize_openai_stream(Acc, openai)
        end
    },
    beamai_llm_http_client:stream_request(Url, Headers, Body, Opts, Callback,
                                          fun beamai_llm_provider_common:accumulate_openai_event/2).

%%====================================================================
%% 请求构建（Provider 特定）
%%====================================================================

%% @private 构建请求 URL（使用公共模块）
build_url(Config, DefaultEndpoint) ->
    beamai_llm_provider_common:build_url(Config, DefaultEndpoint, ?OPENAI_BASE_URL).

%% @private 构建请求头（使用公共模块）
build_headers(Config) ->
    beamai_llm_provider_common:build_bearer_auth_headers(Config).

%% @private 构建请求体（使用管道模式）
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    Base = #{
        <<"model">> => maps:get(model, Config, ?OPENAI_MODEL),
        <<"messages">> => beamai_llm_message_adapter:to_openai(Messages),
        <<"temperature">> => maps:get(temperature, Config, ?OPENAI_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> add_max_tokens(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_top_p(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Config, ?OPTIONAL_PARAMS) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tools(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tool_choice(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Request) end,
        fun(B) -> maybe_add_stream_options(B, Config, Request) end
    ]).

%% @private 添加 token 上限
%% 推理模型（o 系列）使用 max_completion_tokens，配置了则优先；
%% 否则使用传统的 max_tokens。
add_max_tokens(Body, #{max_completion_tokens := N}) when is_integer(N), N > 0 ->
    Body#{<<"max_completion_tokens">> => N};
add_max_tokens(Body, Config) ->
    Body#{<<"max_tokens">> => maps:get(max_tokens, Config, ?OPENAI_MAX_TOKENS)}.

%% @private 流式模式下添加 stream_options
%% 默认开启 include_usage，使末尾 chunk 携带 usage 统计；
%% 可通过 Config 的 stream_include_usage => false 关闭
%% （部分 OpenAI 兼容代理不支持该参数）。
maybe_add_stream_options(Body, Config, #{stream := true}) ->
    case maps:get(stream_include_usage, Config, true) of
        true -> Body#{<<"stream_options">> => #{<<"include_usage">> => true}};
        false -> Body
    end;
maybe_add_stream_options(Body, _Config, _Request) ->
    Body.
