%%%-------------------------------------------------------------------
%%% @doc DeepSeek LLM Provider 实现
%%%
%%% 支持 DeepSeek API，包括 deepseek-chat 和 deepseek-reasoner 模型。
%%% DeepSeek API 与 OpenAI API 兼容，使用相同的请求/响应格式。
%%%
%%% 支持的模型：
%%%   - deepseek-chat: 通用对话模型
%%%   - deepseek-reasoner: 推理增强模型（响应额外携带 reasoning_content）
%%%
%%% 支持的功能：
%%%   - 基本对话 (chat/stream_chat)
%%%   - 工具调用 (tools + tool_choice)，流式分片工具调用累加
%%%   - 思维链内容 (reasoning_content，同步与流式均支持)
%%%   - 采样参数 (temperature, top_p, frequency_penalty, presence_penalty)
%%%   - 停止序列 (stop) / 对数概率 (logprobs, top_logprobs)
%%%   - JSON 输出模式 (response_format)
%%%
%%% 注意：DeepSeek 不支持 OpenAI 的 seed / n / logit_bias /
%%% parallel_tool_calls / stream_options 等参数，因此不会发送。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_deepseek).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

%% 默认值
-define(DEEPSEEK_BASE_URL, <<"https://api.deepseek.com">>).
-define(DEEPSEEK_ENDPOINT, <<"/chat/completions">>).
-define(DEEPSEEK_MODEL, <<"deepseek-chat">>).
-define(DEEPSEEK_TIMEOUT, 60000).
-define(DEEPSEEK_MAX_TOKENS, 4096).
-define(DEEPSEEK_TEMPERATURE, 1.0).

%% Config 中可选参数与 DeepSeek API 字段的映射
%% （DeepSeek 支持的 OpenAI 兼容参数子集）
-define(OPTIONAL_PARAMS, [
    {frequency_penalty, <<"frequency_penalty">>},
    {presence_penalty, <<"presence_penalty">>},
    {stop, <<"stop">>},
    {logprobs, <<"logprobs">>},
    {top_logprobs, <<"top_logprobs">>}
]).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"DeepSeek">>.

default_config() ->
    #{
        base_url => ?DEEPSEEK_BASE_URL,
        model => ?DEEPSEEK_MODEL,
        timeout => ?DEEPSEEK_TIMEOUT,
        max_tokens => ?DEEPSEEK_MAX_TOKENS,
        temperature => ?DEEPSEEK_TEMPERATURE
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
%% 使用 DeepSeek 专用解析器，提取 reasoning_content（deepseek-reasoner）
chat(Config, Request) ->
    Url = build_url(Config, ?DEEPSEEK_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = #{timeout => maps:get(timeout, Config, ?DEEPSEEK_TIMEOUT)},
    beamai_llm_http_client:request(Url, Headers, Body, Opts, beamai_llm_response_parser:parser_deepseek()).

%% @doc 发送流式聊天请求
%% 流式累加结果经 finalize_openai_stream 转换为与同步模式一致的统一响应
%% （含分片工具调用拼接和 reasoning_content 累加）。
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, ?DEEPSEEK_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = #{
        timeout => maps:get(timeout, Config, ?DEEPSEEK_TIMEOUT),
        finalizer => fun(Acc) ->
            beamai_llm_provider_common:finalize_openai_stream(Acc, deepseek)
        end
    },
    beamai_llm_http_client:stream_request(Url, Headers, Body, Opts, Callback,
                                          fun beamai_llm_provider_common:accumulate_openai_event/2).

%%====================================================================
%% 请求构建（使用公共模块）
%%====================================================================

%% @private 构建请求 URL
build_url(Config, DefaultEndpoint) ->
    beamai_llm_provider_common:build_url(Config, DefaultEndpoint, ?DEEPSEEK_BASE_URL).

%% @private 构建请求头
build_headers(Config) ->
    beamai_llm_provider_common:build_bearer_auth_headers(Config).

%% @private 构建请求体（使用管道模式）
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    Base = #{
        <<"model">> => maps:get(model, Config, ?DEEPSEEK_MODEL),
        <<"messages">> => beamai_llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?DEEPSEEK_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?DEEPSEEK_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> beamai_llm_provider_common:maybe_add_top_p(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Config, ?OPTIONAL_PARAMS) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tools(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tool_choice(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Request) end,
        fun(B) -> maybe_add_response_format(B, Config, Request) end
    ]).

%% @private 添加响应格式（JSON 模式）
%% 优先取 Request，其次取 Config，支持 #{<<"type">> => <<"json_object">>}
maybe_add_response_format(Body, Config, Request) ->
    case maps:get(response_format, Request, maps:get(response_format, Config, undefined)) of
        Format when is_map(Format) -> Body#{<<"response_format">> => Format};
        _ -> Body
    end.
