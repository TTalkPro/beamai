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
%%%   - 上下文硬盘缓存统计（usage.details 中的 prompt_cache_hit/miss_tokens）
%%%   - Chat Prefix Completion (beta)：末尾 assistant 消息标 prefix => true，
%%%     自动路由到 /beta 端点
%%%   - FIM 填空补全 (beta)：fim/2 和 stream_fim/3
%%%
%%% 注意：
%%%   - DeepSeek 不支持 OpenAI 的 seed / n / logit_bias /
%%%     parallel_tool_calls / stream_options 等参数，因此不会发送。
%%%   - deepseek-reasoner 不支持 temperature / top_p / penalty（静默忽略）
%%%     和 logprobs / top_logprobs（直接报错），构建请求时自动剔除。
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

%% DeepSeek 特有 API（beta）
-export([fim/2, stream_fim/3]).

-ifdef(TEST).
-export([build_request_body/2, build_fim_request_body/2, chat_endpoint/1]).
-endif.

%% 默认值
-define(DEEPSEEK_BASE_URL, <<"https://api.deepseek.com">>).
-define(DEEPSEEK_ENDPOINT, <<"/chat/completions">>).
-define(DEEPSEEK_CHAT_BETA_ENDPOINT, <<"/beta/chat/completions">>).
-define(DEEPSEEK_FIM_ENDPOINT, <<"/beta/completions">>).
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

%% deepseek-reasoner 不支持的请求字段（发送 logprobs 会直接报 400）
-define(REASONER_UNSUPPORTED_FIELDS, [
    <<"temperature">>, <<"top_p">>,
    <<"frequency_penalty">>, <<"presence_penalty">>,
    <<"logprobs">>, <<"top_logprobs">>
]).

%% FIM 请求级参数映射
-define(FIM_REQUEST_PARAMS, [
    {suffix, <<"suffix">>},
    {echo, <<"echo">>},
    {logprobs, <<"logprobs">>},   %% FIM 的 logprobs 为整数（0-20），与 chat 不同
    {stop, <<"stop">>}
]).

%% FIM Config 级参数映射
-define(FIM_CONFIG_PARAMS, [
    {frequency_penalty, <<"frequency_penalty">>},
    {presence_penalty, <<"presence_penalty">>},
    {stop, <<"stop">>}
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
%% 和缓存统计。消息含 prefix => true 时自动路由到 /beta 端点。
chat(Config, Request) ->
    Url = build_url(Config, chat_endpoint(Request)),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = #{timeout => maps:get(timeout, Config, ?DEEPSEEK_TIMEOUT)},
    beamai_llm_http_client:request(Url, Headers, Body, Opts, beamai_llm_response_parser:parser_deepseek()).

%% @doc 发送流式聊天请求
%% 流式累加结果经 finalize_openai_stream 转换为与同步模式一致的统一响应
%% （含分片工具调用拼接和 reasoning_content 累加）。
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, chat_endpoint(Request)),
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
%% FIM 填空补全 API（beta）
%%====================================================================

%% @doc 发送 FIM（Fill-In-the-Middle）填空补全请求
%%
%% 使用 /beta/completions 端点，根据 prompt（前缀）和可选的
%% suffix（后缀）补全中间内容，适用于代码补全等场景。
%%
%% Request 参数：
%%   prompt   - 必填，补全前缀
%%   suffix   - 可选，补全后缀
%%   echo     - 可选，是否回显 prompt
%%   logprobs - 可选，整数（0-20）
%%   stop     - 可选，停止序列
%%
%% ```erlang
%% {ok, Resp} = beamai_llm_provider_deepseek:fim(Config, #{
%%     prompt => <<"def fib(n):">>,
%%     suffix => <<"    return fib(n-1) + fib(n-2)">>
%% }),
%% Completion = beamai_llm_response:content(Resp)
%% ```
-spec fim(map(), map()) -> {ok, map()} | {error, term()}.
fim(Config, Request) ->
    Url = build_fim_url(Config),
    Headers = build_headers(Config),
    Body = build_fim_request_body(Config, Request),
    Opts = #{timeout => maps:get(timeout, Config, ?DEEPSEEK_TIMEOUT)},
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_llm_response_parser:parser_deepseek_fim()).

%% @doc 发送流式 FIM 填空补全请求
-spec stream_fim(map(), map(), fun((term()) -> any())) -> {ok, map()} | {error, term()}.
stream_fim(Config, Request, Callback) ->
    Url = build_fim_url(Config),
    Headers = build_headers(Config),
    Body = build_fim_request_body(Config, Request#{stream => true}),
    Opts = #{
        timeout => maps:get(timeout, Config, ?DEEPSEEK_TIMEOUT),
        finalizer => fun(Acc) ->
            beamai_llm_provider_common:finalize_completions_stream(
                Acc, beamai_llm_response_parser:parser_deepseek_fim())
        end
    },
    beamai_llm_http_client:stream_request(Url, Headers, Body, Opts, Callback,
                                          fun beamai_llm_provider_common:accumulate_completions_event/2).

%%====================================================================
%% 请求构建（使用公共模块）
%%====================================================================

%% @private 构建请求 URL
build_url(Config, DefaultEndpoint) ->
    beamai_llm_provider_common:build_url(Config, DefaultEndpoint, ?DEEPSEEK_BASE_URL).

%% @private 构建 FIM 请求 URL
%% FIM 使用独立端点，可通过 Config 的 fim_endpoint 覆盖
%% （与 chat 的 endpoint 配置互不干扰）。
build_fim_url(Config) ->
    BaseUrl = maps:get(base_url, Config, ?DEEPSEEK_BASE_URL),
    Endpoint = maps:get(fim_endpoint, Config, ?DEEPSEEK_FIM_ENDPOINT),
    <<BaseUrl/binary, Endpoint/binary>>.

%% @private 选择 chat 端点
%% Chat Prefix Completion（beta）：任一消息标记 prefix => true 时，
%% 路由到 /beta 端点（参考 Spring AI DeepSeekApi 的端点切换逻辑）。
chat_endpoint(#{messages := Messages}) when is_list(Messages) ->
    HasPrefix = lists:any(
        fun(#{prefix := true}) -> true;
           (_) -> false
        end, Messages),
    case HasPrefix of
        true -> ?DEEPSEEK_CHAT_BETA_ENDPOINT;
        false -> ?DEEPSEEK_ENDPOINT
    end;
chat_endpoint(_) ->
    ?DEEPSEEK_ENDPOINT.

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
        fun(B) -> maybe_add_response_format(B, Config, Request) end,
        fun(B) -> maybe_filter_reasoner_params(B) end
    ]).

%% @private 构建 FIM 请求体
build_fim_request_body(Config, Request) ->
    Base = #{
        <<"model">> => maps:get(model, Config, ?DEEPSEEK_MODEL),
        <<"prompt">> => maps:get(prompt, Request, <<>>),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?DEEPSEEK_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?DEEPSEEK_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> beamai_llm_provider_common:maybe_add_top_p(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Config, ?FIM_CONFIG_PARAMS) end,
        %% Request 级参数后应用，可覆盖 Config 同名参数（如 stop）
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Request, ?FIM_REQUEST_PARAMS) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Request) end
    ]).

%% @private 添加响应格式（JSON 模式）
%% 优先取 Request，其次取 Config，支持 #{<<"type">> => <<"json_object">>}
maybe_add_response_format(Body, Config, Request) ->
    case maps:get(response_format, Request, maps:get(response_format, Config, undefined)) of
        Format when is_map(Format) -> Body#{<<"response_format">> => Format};
        _ -> Body
    end.

%% @private deepseek-reasoner 模型剔除不支持的参数
%% reasoner 忽略 temperature / top_p / penalty，
%% 但 logprobs / top_logprobs 会直接报 400，统一剔除以保证安全。
maybe_filter_reasoner_params(#{<<"model">> := Model} = Body) ->
    case is_reasoner_model(Model) of
        true -> maps:without(?REASONER_UNSUPPORTED_FIELDS, Body);
        false -> Body
    end;
maybe_filter_reasoner_params(Body) ->
    Body.

%% @private 判断是否为推理模型（前缀匹配，兼容版本化模型名）
is_reasoner_model(<<"deepseek-reasoner", _/binary>>) -> true;
is_reasoner_model(_) -> false.
