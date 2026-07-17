%%%-------------------------------------------------------------------
%%% @doc Anthropic Claude LLM Provider 实现
%%%
%%% 支持 Anthropic Claude API（Claude 4 系列）。
%%% 使用 beamai_llm_http_client 处理公共 HTTP 逻辑。
%%%
%%% 支持的功能：
%%%   - 基本对话 (chat/stream_chat)
%%%   - 多模态输入（图片 / PDF 文档，经 message_adapter 转换）
%%%   - 工具调用 (tools + tool_choice)，流式 input_json_delta 累加
%%%   - Extended Thinking (thinking 配置)，流式 thinking/signature delta 累加
%%%   - 采样参数 (temperature, top_p, top_k)
%%%   - 停止序列 (stop_sequences)
%%%   - 用户元数据 (metadata) / 服务层级 (service_tier) / 数据驻留 (inference_geo)
%%%   - Prompt 缓存：cache_control 注入（system / tools / conversation 策略，
%%%     可选 TTL），降低重复上下文的费用
%%%   - 内置 Web Search 工具 (web_search)
%%%   - 引用 Citations（document 请求侧 + 响应侧解析到 metadata）
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

-ifdef(TEST).
-export([build_request_body/2]).
-endif.

%% 默认值
-define(ANTHROPIC_BASE_URL, <<"https://api.anthropic.com">>).
-define(ANTHROPIC_ENDPOINT, <<"/v1/messages">>).
-define(ANTHROPIC_MODEL, <<"claude-sonnet-4-5-20250929">>).
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
        timeout => beamai_llm_provider_common:default_timeout(anthropic),
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
    Opts = beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, anthropic),
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    }, Config),
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
    Opts = beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, anthropic),
        finalizer => fun beamai_llm_provider_common:finalize_anthropic_stream/1,
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    }, Config),
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
    {SystemPrompt, UserMessages} = beamai_llm_message_adapter:extract_system_prompt(Messages),
    {CacheStrategy, CacheControl} = parse_cache_strategy(Config),
    Base = #{
        <<"model">> => maps:get(model, Config, ?ANTHROPIC_MODEL),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?ANTHROPIC_MAX_TOKENS),
        <<"messages">> => apply_conversation_cache(
            beamai_llm_message_adapter:to_anthropic(UserMessages), CacheStrategy, CacheControl)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> maybe_add_system(B, SystemPrompt, CacheStrategy, CacheControl) end,
        fun(B) -> maybe_add_tools(B, Request, CacheStrategy, CacheControl) end,
        fun(B) -> maybe_add_web_search(B, Config) end,
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

%% @private 添加系统提示
%% 命中 system 缓存策略时，将 system 转为带 cache_control 的 text block 列表，
%% 否则保持为纯字符串。
maybe_add_system(Body, undefined, _Strategy, _CC) -> Body;
maybe_add_system(Body, SystemPrompt, Strategy, CC) ->
    case caches_system(Strategy) of
        true ->
            Body#{<<"system">> => [#{
                <<"type">> => <<"text">>,
                <<"text">> => SystemPrompt,
                <<"cache_control">> => CC
            }]};
        false ->
            Body#{<<"system">> => SystemPrompt}
    end.

%% @private 添加工具定义
%% 命中 tools 缓存策略时，在最后一个 tool 上注入 cache_control，
%% 该断点之前的所有 tools 定义将被缓存。
maybe_add_tools(Body, #{tools := Tools}, Strategy, CC) when Tools =/= [] ->
    Formatted = beamai_llm_tool_adapter:to_anthropic(Tools),
    Final = case caches_tools(Strategy) of
        true -> add_cache_control_to_last(Formatted, CC);
        false -> Formatted
    end,
    Body#{<<"tools">> => Final};
maybe_add_tools(Body, _, _Strategy, _CC) ->
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

%%====================================================================
%% Prompt 缓存（cache_control 注入）
%%====================================================================

%% @private 解析缓存策略与 cache_control 值
%%
%% Config 中 cache_control 可为：
%%   - none | system_only | tools_only | system_and_tools | conversation（原子）
%%   - #{strategy => 上述原子, ttl => <<"5m">> | <<"1h">>}（带 TTL）
%%
%% 返回 {Strategy, CacheControl}，CacheControl 形如
%% #{<<"type">> => <<"ephemeral">>}（含 ttl 时附带 <<"ttl">>）。
-spec parse_cache_strategy(map()) -> {atom(), map()}.
parse_cache_strategy(Config) ->
    case maps:get(cache_control, Config, none) of
        #{strategy := Strategy} = Spec ->
            CC = case maps:get(ttl, Spec, undefined) of
                undefined -> #{<<"type">> => <<"ephemeral">>};
                Ttl -> #{<<"type">> => <<"ephemeral">>, <<"ttl">> => Ttl}
            end,
            {Strategy, CC};
        Strategy when is_atom(Strategy) ->
            {Strategy, #{<<"type">> => <<"ephemeral">>}}
    end.

%% @private 是否缓存 system 提示
caches_system(system_only) -> true;
caches_system(system_and_tools) -> true;
caches_system(_) -> false.

%% @private 是否缓存 tools 定义
caches_tools(tools_only) -> true;
caches_tools(system_and_tools) -> true;
caches_tools(_) -> false.

%% @private 在列表最后一个元素（map）上注入 cache_control
add_cache_control_to_last([], _CC) -> [];
add_cache_control_to_last(List, CC) ->
    {Init, [Last]} = lists:split(length(List) - 1, List),
    Init ++ [Last#{<<"cache_control">> => CC}].

%% @private conversation 策略：在最后一条消息的末尾内容块上注入 cache_control
%% 使截至当前的完整会话历史被缓存（增量缓存，每轮在新消息处打断点）。
apply_conversation_cache(Messages, conversation, CC) when Messages =/= [] ->
    {Init, [Last]} = lists:split(length(Messages) - 1, Messages),
    Init ++ [cache_last_content_block(Last, CC)];
apply_conversation_cache(Messages, _Strategy, _CC) ->
    Messages.

%% @private 在单条消息的最后一个内容块上注入 cache_control
%% content 为字符串时先转为单个 text block。
cache_last_content_block(#{<<"content">> := Content} = Msg, CC) when is_binary(Content) ->
    Msg#{<<"content">> => [#{
        <<"type">> => <<"text">>, <<"text">> => Content, <<"cache_control">> => CC
    }]};
cache_last_content_block(#{<<"content">> := Blocks} = Msg, CC) when is_list(Blocks), Blocks =/= [] ->
    Msg#{<<"content">> => add_cache_control_to_last(Blocks, CC)};
cache_last_content_block(Msg, _CC) ->
    Msg.

%%====================================================================
%% 内置 Web Search 工具（P2）
%%====================================================================

%% @private 注入 Anthropic 内置 Web Search 工具
%%
%% Config 中 web_search 可为：
%%   - true（启用，使用默认配置）
%%   - #{max_uses => N, allowed_domains => [...], blocked_domains => [...],
%%       user_location => Map}
%%
%% 该工具追加到 tools 列表（与用户自定义工具共存）。
maybe_add_web_search(Body, #{web_search := false}) -> Body;
maybe_add_web_search(Body, #{web_search := WebSearch}) ->
    Tool = build_web_search_tool(WebSearch),
    Existing = maps:get(<<"tools">>, Body, []),
    Body#{<<"tools">> => Existing ++ [Tool]};
maybe_add_web_search(Body, _) ->
    Body.

%% @private 构建 web search 工具定义
build_web_search_tool(Opts) when is_map(Opts) ->
    Base = #{<<"type">> => <<"web_search_20250305">>, <<"name">> => <<"web_search">>},
    beamai_llm_provider_common:maybe_add_params(Base, Opts, [
        {max_uses, <<"max_uses">>},
        {allowed_domains, <<"allowed_domains">>},
        {blocked_domains, <<"blocked_domains">>},
        {user_location, <<"user_location">>}
    ]);
build_web_search_tool(_) ->
    #{<<"type">> => <<"web_search_20250305">>, <<"name">> => <<"web_search">>}.
