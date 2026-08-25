%%%-------------------------------------------------------------------
%%% @doc OpenRouter LLM Provider 实现
%%%
%%% OpenRouter 是多家模型的统一网关，接口与 OpenAI Chat Completions 兼容，
%%% 并额外提供模型回退、供应商路由与提示词压缩等能力。
%%% API 文档: https://openrouter.ai/docs/api-reference
%%%
%%% 支持的功能：
%%%   - 基本对话 (chat/stream_chat)
%%%   - 多模态输入（图片 / PDF，经 message_adapter 转换）
%%%   - 工具调用 (tools + tool_choice)，流式分片工具调用累加
%%%   - 模型回退 (models + route => `<<"fallback">>')
%%%   - 供应商路由 (provider: order / sort / allow_fallbacks / ignore ...)
%%%   - 提示词压缩 (transforms，如 `[<<"middle-out">>]')
%%%   - 推理控制 (reasoning: #{effort => ..., max_tokens => ..., exclude => ...})
%%%   - 用量与成本统计 (usage.include，响应 usage.cost 进 metadata)
%%%   - 采样参数 (temperature / top_p / top_k / min_p / top_a /
%%%     repetition_penalty / frequency_penalty / presence_penalty)
%%%
%%% == 应用标识 ==
%%%
%%% OpenRouter 依据 `HTTP-Referer' 和 `X-Title' 在排行榜中展示来源应用，
%%% 通过 Config 的 `site_url' / `site_name' 设置（可选）。
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% Config = beamai_chat_model:create(openrouter, #{
%%%     api_key => <<"sk-or-...">>,
%%%     model => <<"anthropic/claude-sonnet-4">>,
%%%     models => [<<"openai/gpt-4o">>],
%%%     route => <<"fallback">>,
%%%     provider => #{<<"sort">> => <<"throughput">>},
%%%     site_name => <<"beamai">>
%%% }),
%%% {ok, Resp} = beamai_chat_model:chat(Config, Messages).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_openrouter).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

-ifdef(TEST).
-export([build_request_body/2, build_headers/1]).
-endif.

%% 默认值
-define(OPENROUTER_BASE_URL, <<"https://openrouter.ai/api">>).
-define(OPENROUTER_ENDPOINT, <<"/v1/chat/completions">>).
-define(OPENROUTER_MODEL, <<"openai/gpt-4o-mini">>).
-define(OPENROUTER_MAX_TOKENS, 4096).
-define(OPENROUTER_TEMPERATURE, 0.7).

%% Config 中可选参数与 OpenRouter API 字段的映射
-define(OPTIONAL_PARAMS, [
    {frequency_penalty, <<"frequency_penalty">>},
    {presence_penalty, <<"presence_penalty">>},
    {repetition_penalty, <<"repetition_penalty">>},
    {top_k, <<"top_k">>},
    {min_p, <<"min_p">>},
    {top_a, <<"top_a">>},
    {seed, <<"seed">>},
    {stop, <<"stop">>},
    {logit_bias, <<"logit_bias">>},
    {logprobs, <<"logprobs">>},
    {top_logprobs, <<"top_logprobs">>},
    {user, <<"user">>},
    {response_format, <<"response_format">>},
    %% OpenRouter 特有：模型回退与供应商路由
    {models, <<"models">>},
    {route, <<"route">>},
    {provider, <<"provider">>},
    {transforms, <<"transforms">>},
    {reasoning, <<"reasoning">>}
]).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"OpenRouter">>.

default_config() ->
    #{
        base_url => ?OPENROUTER_BASE_URL,
        model => ?OPENROUTER_MODEL,
        timeout => beamai_llm_provider_common:default_timeout(openrouter),
        max_tokens => ?OPENROUTER_MAX_TOKENS,
        temperature => ?OPENROUTER_TEMPERATURE
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
        timeout => beamai_llm_provider_common:request_timeout(Config, openrouter),
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    }, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_llm_response_parser:parser_openrouter()).

%% @doc 发送流式聊天请求
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, openrouter),
        finalizer => fun(Acc) ->
            beamai_llm_provider_common:finalize_openai_stream(Acc, openrouter)
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
    beamai_llm_provider_common:build_url(Config, ?OPENROUTER_ENDPOINT, ?OPENROUTER_BASE_URL).

%% @private 构建请求头
%% 在标准 Bearer 头基础上追加应用标识（OpenRouter 排行榜来源）。
build_headers(Config) ->
    Base = beamai_llm_provider_common:build_bearer_auth_headers(Config),
    Base
        ++ optional_header(<<"HTTP-Referer">>, maps:get(site_url, Config, undefined))
        ++ optional_header(<<"X-Title">>, maps:get(site_name, Config, undefined)).

%% @private 可选请求头
optional_header(_Name, undefined) -> [];
optional_header(Name, Value) when is_binary(Value) -> [{Name, Value}];
optional_header(_Name, _Value) -> [].

%% @private 构建请求体（使用管道模式）
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    Base = #{
        <<"model">> => maps:get(model, Config, ?OPENROUTER_MODEL),
        <<"messages">> => beamai_llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?OPENROUTER_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?OPENROUTER_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> beamai_llm_provider_common:maybe_add_top_p(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Config, ?OPTIONAL_PARAMS) end,
        fun(B) -> maybe_add_usage_accounting(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tools(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tool_choice(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Request) end
    ]).

%% @private 开启用量与成本统计
%% OpenRouter 用 usage.include（而非 OpenAI 的 stream_options），
%% 开启后响应 usage 携带 cost 等字段，流式末尾 chunk 同样返回。
maybe_add_usage_accounting(Body, Config) ->
    case maps:get(include_usage, Config, false) of
        true -> Body#{<<"usage">> => #{<<"include">> => true}};
        _ -> Body
    end.
