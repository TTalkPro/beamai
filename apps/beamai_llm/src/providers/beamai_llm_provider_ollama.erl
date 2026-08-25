%%%-------------------------------------------------------------------
%%% @doc Ollama 本地 LLM Provider 实现
%%%
%%% 支持 Ollama 本地运行的模型（Llama, Mistral, Qwen 等）。
%%% 使用 beamai_llm_http_client 处理公共 HTTP 逻辑。
%%%
%%% 特点：
%%%   - 支持 OpenAI 兼容 API（/v1/chat/completions）
%%%   - 支持原生 Ollama 响应格式
%%%   - 无需 API Key
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_ollama).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([base_url/1, endpoint/2, headers/2, body/2, parser/1,
         stream_accumulator/1, stream_finalizer/1]).
-export([supports_tools/0, supports_streaming/0]).

%% 默认值
-define(OLLAMA_BASE_URL, <<"http://localhost:11434">>).
-define(OLLAMA_ENDPOINT, <<"/v1/chat/completions">>).
-define(OLLAMA_MODEL, <<"llama3.2">>).
-define(OLLAMA_MAX_TOKENS, 4096).
-define(OLLAMA_TEMPERATURE, 0.7).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"Ollama">>.

default_config() ->
    #{
        base_url => ?OLLAMA_BASE_URL,
        model => ?OLLAMA_MODEL,
        timeout => beamai_llm_provider_common:default_timeout(ollama),
        max_tokens => ?OLLAMA_MAX_TOKENS,
        temperature => ?OLLAMA_TEMPERATURE
    }.

validate_config(Config) ->
    case maps:get(model, Config, undefined) of
        undefined -> {error, missing_model};
        _ -> ok
    end.

supports_tools() -> true.
supports_streaming() -> true.

%%====================================================================
%% 聊天 API
%%====================================================================

%% @doc 发送聊天请求
chat(Config, Request) ->
    beamai_llm_http_provider:chat(?MODULE, Config, Request).

%% @doc 发送流式聊天请求
%% 默认端点为 OpenAI 兼容（/v1/chat/completions），流式经 finalize_openai_stream
%% 转为统一 beamai_chat_response（与同步一致）；原生格式分片也累加到同一 content 字段。
stream_chat(Config, Request, Callback) ->
    beamai_llm_http_provider:stream_chat(?MODULE, Config, Request, Callback).

%%====================================================================
%% 声明式回调：底层信息（怎么发由 beamai_llm_http_provider 统一负责）
%%====================================================================

base_url(_Config) -> ?OLLAMA_BASE_URL.

endpoint(_Config, _Request) -> ?OLLAMA_ENDPOINT.

headers(_Config, _Request) -> build_headers().

body(Config, Request) -> build_request_body(Config, Request).

parser(_Config) -> beamai_llm_response_parser:parser_ollama().

stream_accumulator(_Config) -> fun accumulate_event/2.

stream_finalizer(_Config) ->
    fun(Acc) -> beamai_llm_provider_common:finalize_openai_stream(Acc, ollama) end.

%%====================================================================
%% 请求构建（Provider 特定）
%%====================================================================


%% @private 构建请求头（Ollama 无需认证）
build_headers() ->
    [{<<"Content-Type">>, <<"application/json">>}].

%% @private 构建请求体
build_request_body(Config, Request) ->
    Messages = beamai_chat_request:messages(Request),
    Base = #{
        <<"model">> => maps:get(model, Config, ?OLLAMA_MODEL),
        <<"messages">> => beamai_llm_message_adapter:to_openai(Messages),
        <<"stream">> => beamai_chat_request:option(Request, stream, false)
    },
    build_body_pipeline(Base, Config, Request).

%% @private 请求体构建管道（使用宏）
build_body_pipeline(Body, Config, Request) ->
    ?BUILD_BODY_PIPELINE(Body, [
        fun(B) -> maybe_add_options(B, Config) end,
        fun(B) -> maybe_add_tools(B, beamai_chat_request:options(Request)) end
    ]).

%% @private 添加 Ollama 特有选项
maybe_add_options(Body, Config) ->
    Options = build_options(Config),
    case map_size(Options) of
        0 -> Body;
        _ -> Body#{<<"options">> => Options}
    end.

%% @private 构建选项 Map
build_options(Config) ->
    lists:foldl(fun({ConfigKey, OptionsKey}, Acc) ->
        case maps:get(ConfigKey, Config, undefined) of
            undefined -> Acc;
            Value -> Acc#{OptionsKey => Value}
        end
    end, #{}, [
        {temperature, <<"temperature">>},
        {max_tokens, <<"num_predict">>}
    ]).

%% @private 添加工具定义（使用公共模块）
maybe_add_tools(Body, Options) ->
    beamai_llm_provider_common:maybe_add_tools(Body, Options).

%%====================================================================
%% 流式事件累加（支持两种格式）
%%====================================================================

%% @private Ollama 原生格式事件累加（累加到与 OpenAI 累加器一致的 content 字段，
%% 便于统一 finalize_openai_stream 重建）
accumulate_event(#{<<"message">> := #{<<"content">> := Content}}, Acc) ->
    Acc#{content => <<(maps:get(content, Acc, <<>>))/binary, Content/binary>>};
%% @private OpenAI 兼容格式事件累加（使用公共模块）
accumulate_event(#{<<"choices">> := _} = Event, Acc) ->
    beamai_llm_provider_common:accumulate_openai_event(Event, Acc);
accumulate_event(_, Acc) ->
    Acc.
