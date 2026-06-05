%%%-------------------------------------------------------------------
%%% @doc Anthropic Claude LLM Provider 实现
%%%
%%% 支持 Anthropic Claude API（Claude 4 系列）。
%%% 使用 beamai_llm_http_client 处理公共 HTTP 逻辑。
%%%
%%% 支持的功能：
%%%   - 基本对话 (chat/stream_chat)
%%%   - 工具调用 (tools + tool_choice)，流式 input_json_delta 累加
%%%   - Extended Thinking (thinking 配置)，流式 thinking/signature delta 累加
%%%   - 采样参数 (temperature, top_p, top_k)
%%%   - 停止序列 (stop_sequences)
%%%   - 用户元数据 (metadata) / 服务层级 (service_tier) / 数据驻留 (inference_geo)
%%%   - 流式 usage 统计（message_start 的 input_tokens + message_delta 的 output_tokens）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_anthropic).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

%% 默认值
-define(ANTHROPIC_BASE_URL, <<"https://api.anthropic.com">>).
-define(ANTHROPIC_ENDPOINT, <<"/v1/messages">>).
-define(ANTHROPIC_MODEL, <<"claude-sonnet-4-5-20250929">>).
-define(ANTHROPIC_TIMEOUT, 60000).
-define(ANTHROPIC_MAX_TOKENS, 8192).
-define(API_VERSION, <<"2023-06-01">>).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"Anthropic Claude">>.

default_config() ->
    #{
        base_url => ?ANTHROPIC_BASE_URL,
        model => ?ANTHROPIC_MODEL,
        timeout => ?ANTHROPIC_TIMEOUT,
        max_tokens => ?ANTHROPIC_MAX_TOKENS
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
    Url = build_url(Config, ?ANTHROPIC_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = #{timeout => maps:get(timeout, Config, ?ANTHROPIC_TIMEOUT)},
    beamai_llm_http_client:request(Url, Headers, Body, Opts, beamai_llm_response_parser:parser_anthropic()).

%% @doc 发送流式聊天请求
%% 使用公共模块的 Anthropic 事件累加器，完整处理
%% content_block_start/delta/stop、message_start/delta 事件，
%% 流式结束后重建为与同步模式一致的统一响应
%% （含 tool_use 块、thinking 块和 usage 统计）。
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, ?ANTHROPIC_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = #{
        timeout => maps:get(timeout, Config, ?ANTHROPIC_TIMEOUT),
        finalizer => fun beamai_llm_provider_common:finalize_anthropic_stream/1
    },
    beamai_llm_http_client:stream_request(Url, Headers, Body, Opts, Callback,
                                          fun beamai_llm_provider_common:accumulate_anthropic_event/2).

%%====================================================================
%% 请求构建（Provider 特定）
%%====================================================================

%% @private 构建请求 URL（使用公共模块）
build_url(Config, DefaultEndpoint) ->
    beamai_llm_provider_common:build_url(Config, DefaultEndpoint, ?ANTHROPIC_BASE_URL).

%% @private 构建请求头（Anthropic 特有的 x-api-key 和 anthropic-version）
build_headers(#{api_key := ApiKey}) ->
    [
        {<<"x-api-key">>, ApiKey},
        {<<"anthropic-version">>, ?API_VERSION},
        {<<"Content-Type">>, <<"application/json">>}
    ].

%% @private 构建请求体（使用管道模式）
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    {SystemPrompt, UserMessages} = extract_system_prompt(Messages),
    Base = #{
        <<"model">> => maps:get(model, Config, ?ANTHROPIC_MODEL),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?ANTHROPIC_MAX_TOKENS),
        <<"messages">> => beamai_llm_message_adapter:to_anthropic(UserMessages)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> maybe_add_system(B, SystemPrompt) end,
        fun(B) -> maybe_add_tools(B, Request) end,
        fun(B) -> maybe_add_tool_choice(B, Request) end,
        fun(B) -> maybe_add_thinking(B, Config) end,
        fun(B) -> maybe_add_temperature(B, Config) end,
        fun(B) -> maybe_add_top_p(B, Config) end,
        fun(B) -> maybe_add_top_k(B, Config) end,
        fun(B) -> maybe_add_stop_sequences(B, Config, Request) end,
        fun(B) -> maybe_add_metadata(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Config, [
            {service_tier, <<"service_tier">>},
            {inference_geo, <<"inference_geo">>}
        ]) end,
        fun(B) -> maybe_add_stream(B, Request) end
    ]).

%% @private 提取系统提示（Anthropic 需要单独的 system 字段）
extract_system_prompt(Messages) ->
    case lists:partition(fun(#{role := R}) -> R =:= system end, Messages) of
        {[], Rest} -> {undefined, Rest};
        {[#{content := C} | _], Rest} -> {C, Rest}
    end.

%% @private 添加系统提示
maybe_add_system(Body, undefined) -> Body;
maybe_add_system(Body, SystemPrompt) -> Body#{<<"system">> => SystemPrompt}.

%% @private 添加工具定义
maybe_add_tools(Body, #{tools := Tools}) when Tools =/= [] ->
    Body#{<<"tools">> => beamai_llm_tool_adapter:to_anthropic(Tools)};
maybe_add_tools(Body, _) ->
    Body.

%% @private 添加 tool_choice（Anthropic 格式）
%% 支持: auto | any | none | {tool, Name}
maybe_add_tool_choice(Body, #{tool_choice := auto}) ->
    Body#{<<"tool_choice">> => #{<<"type">> => <<"auto">>}};
maybe_add_tool_choice(Body, #{tool_choice := any}) ->
    Body#{<<"tool_choice">> => #{<<"type">> => <<"any">>}};
maybe_add_tool_choice(Body, #{tool_choice := none}) ->
    Body#{<<"tool_choice">> => #{<<"type">> => <<"none">>}};
maybe_add_tool_choice(Body, #{tool_choice := {tool, Name}}) when is_binary(Name) ->
    Body#{<<"tool_choice">> => #{<<"type">> => <<"tool">>, <<"name">> => Name}};
maybe_add_tool_choice(Body, #{tool_choice := Choice}) when is_map(Choice) ->
    %% 直接传入 map 格式（高级用法）
    Body#{<<"tool_choice">> => Choice};
maybe_add_tool_choice(Body, _) ->
    Body.

%% @private 添加 Extended Thinking 配置
%% Config 中 thinking => #{type => enabled, budget_tokens => N}
%% 或简写 thinking => N（budget_tokens 数值）
maybe_add_thinking(Body, #{thinking := #{type := enabled, budget_tokens := Budget}}) ->
    Body#{<<"thinking">> => #{<<"type">> => <<"enabled">>, <<"budget_tokens">> => Budget}};
maybe_add_thinking(Body, #{thinking := #{type := disabled}}) ->
    Body#{<<"thinking">> => #{<<"type">> => <<"disabled">>}};
maybe_add_thinking(Body, #{thinking := #{type := adaptive}}) ->
    Body#{<<"thinking">> => #{<<"type">> => <<"adaptive">>}};
maybe_add_thinking(Body, #{thinking := Budget}) when is_integer(Budget), Budget >= 1024 ->
    Body#{<<"thinking">> => #{<<"type">> => <<"enabled">>, <<"budget_tokens">> => Budget}};
maybe_add_thinking(Body, _) ->
    Body.

%% @private 添加温度参数
maybe_add_temperature(Body, #{temperature := T}) when is_number(T) ->
    Body#{<<"temperature">> => T};
maybe_add_temperature(Body, _) ->
    Body.

%% @private 添加 top_p 参数
maybe_add_top_p(Body, #{top_p := P}) when is_number(P) ->
    Body#{<<"top_p">> => P};
maybe_add_top_p(Body, _) ->
    Body.

%% @private 添加 top_k 参数
maybe_add_top_k(Body, #{top_k := K}) when is_integer(K) ->
    Body#{<<"top_k">> => K};
maybe_add_top_k(Body, _) ->
    Body.

%% @private 添加停止序列（从 Config 或 Request 获取）
maybe_add_stop_sequences(Body, Config, Request) ->
    case maps:get(stop_sequences, Request, maps:get(stop_sequences, Config, undefined)) of
        undefined -> Body;
        Seqs when is_list(Seqs), Seqs =/= [] ->
            Body#{<<"stop_sequences">> => Seqs};
        _ -> Body
    end.

%% @private 添加元数据（user_id 等）
maybe_add_metadata(Body, #{metadata := Meta}) when is_map(Meta), map_size(Meta) > 0 ->
    Body#{<<"metadata">> => Meta};
maybe_add_metadata(Body, _) ->
    Body.

%% @private 添加流式标志
maybe_add_stream(Body, #{stream := true}) -> Body#{<<"stream">> => true};
maybe_add_stream(Body, _) -> Body.
