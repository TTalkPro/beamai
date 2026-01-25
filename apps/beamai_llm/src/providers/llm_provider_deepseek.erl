%%%-------------------------------------------------------------------
%%% @doc DeepSeek LLM Provider 实现
%%%
%%% 支持 DeepSeek API，包括 deepseek-chat 和 deepseek-reasoner 模型。
%%% DeepSeek API 与 OpenAI API 兼容，使用相同的请求/响应格式。
%%%
%%% 支持的模型：
%%%   - deepseek-chat: 通用对话模型
%%%   - deepseek-reasoner: 推理增强模型
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_provider_deepseek).
-behaviour(llm_provider_behaviour).

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
chat(Config, Request) ->
    Url = build_url(Config, ?DEEPSEEK_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = #{timeout => maps:get(timeout, Config, ?DEEPSEEK_TIMEOUT)},
    llm_http_client:request(Url, Headers, Body, Opts, fun llm_response_adapter:parse_openai/1).

%% @doc 发送流式聊天请求
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, ?DEEPSEEK_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = #{timeout => maps:get(timeout, Config, ?DEEPSEEK_TIMEOUT)},
    llm_http_client:stream_request(Url, Headers, Body, Opts, Callback, fun accumulate_event/2).

%%====================================================================
%% 请求构建（使用公共模块）
%%====================================================================

%% @private 构建请求 URL
build_url(Config, DefaultEndpoint) ->
    llm_provider_common:build_url(Config, DefaultEndpoint, ?DEEPSEEK_BASE_URL).

%% @private 构建请求头
build_headers(Config) ->
    llm_provider_common:build_bearer_auth_headers(Config).

%% @private 构建请求体（使用管道模式）
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    Base = #{
        <<"model">> => maps:get(model, Config, ?DEEPSEEK_MODEL),
        <<"messages">> => llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?DEEPSEEK_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?DEEPSEEK_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> llm_provider_common:maybe_add_stream(B, Request) end,
        fun(B) -> llm_provider_common:maybe_add_tools(B, Request) end,
        fun(B) -> llm_provider_common:maybe_add_top_p(B, Config) end,
        fun(B) -> maybe_add_response_format(B, Request) end
    ]).

%% @private 添加响应格式（JSON 模式，DeepSeek 特有）
maybe_add_response_format(Body, #{response_format := Format}) when is_map(Format) ->
    Body#{<<"response_format">> => Format};
maybe_add_response_format(Body, _) ->
    Body.

%%====================================================================
%% 流式事件累加（使用公共模块）
%%====================================================================

%% @private DeepSeek/OpenAI 格式事件累加器
accumulate_event(Event, Acc) ->
    llm_provider_common:accumulate_openai_event(Event, Acc).
