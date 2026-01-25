%%%-------------------------------------------------------------------
%%% @doc OpenAI LLM Provider 实现
%%%
%%% 支持 OpenAI API 及兼容接口（如 Azure OpenAI、vLLM）。
%%% 使用 llm_http_client 处理公共 HTTP 逻辑。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_provider_openai).
-behaviour(llm_provider_behaviour).

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
    llm_http_client:request(Url, Headers, Body, Opts, fun llm_response_adapter:parse_openai/1).

%% @doc 发送流式聊天请求
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, ?OPENAI_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = #{timeout => maps:get(timeout, Config, ?OPENAI_TIMEOUT)},
    llm_http_client:stream_request(Url, Headers, Body, Opts, Callback, fun accumulate_event/2).

%%====================================================================
%% 请求构建（Provider 特定）
%%====================================================================

%% @private 构建请求 URL（使用公共模块）
build_url(Config, DefaultEndpoint) ->
    llm_provider_common:build_url(Config, DefaultEndpoint, ?OPENAI_BASE_URL).

%% @private 构建请求头（使用公共模块）
build_headers(Config) ->
    llm_provider_common:build_bearer_auth_headers(Config).

%% @private 构建请求体（使用管道模式）
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    Base = #{
        <<"model">> => maps:get(model, Config, ?OPENAI_MODEL),
        <<"messages">> => llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?OPENAI_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?OPENAI_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> llm_provider_common:maybe_add_stream(B, Request) end,
        fun(B) -> llm_provider_common:maybe_add_tools(B, Request) end
    ]).

%%====================================================================
%% 流式事件累加（使用公共模块）
%%====================================================================

%% @private OpenAI 格式事件累加器（委托给公共模块）
accumulate_event(Event, Acc) ->
    llm_provider_common:accumulate_openai_event(Event, Acc).
