%%%-------------------------------------------------------------------
%%% @doc Anthropic Claude LLM Provider 实现
%%%
%%% 支持 Anthropic Claude API（Claude 3 系列）。
%%% 使用 llm_http_client 处理公共 HTTP 逻辑。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_provider_anthropic).
-behaviour(llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

%% 默认值
-define(ANTHROPIC_BASE_URL, <<"https://api.anthropic.com">>).
-define(ANTHROPIC_ENDPOINT, <<"/v1/messages">>).
-define(ANTHROPIC_MODEL, <<"claude-3-5-sonnet-20241022">>).
-define(ANTHROPIC_TIMEOUT, 60000).
-define(ANTHROPIC_MAX_TOKENS, 4096).
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
    llm_http_client:request(Url, Headers, Body, Opts, fun llm_response_adapter:parse_anthropic/1).

%% @doc 发送流式聊天请求
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config, ?ANTHROPIC_ENDPOINT),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = #{timeout => maps:get(timeout, Config, ?ANTHROPIC_TIMEOUT)},
    llm_http_client:stream_request(Url, Headers, Body, Opts, Callback, fun accumulate_event/2).

%%====================================================================
%% 请求构建（Provider 特定）
%%====================================================================

%% @private 构建请求 URL（使用公共模块）
build_url(Config, DefaultEndpoint) ->
    llm_provider_common:build_url(Config, DefaultEndpoint, ?ANTHROPIC_BASE_URL).

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
        <<"messages">> => llm_message_adapter:to_anthropic(UserMessages)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> maybe_add_system(B, SystemPrompt) end,
        fun(B) -> maybe_add_tools(B, Request) end,
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
    Body#{<<"tools">> => llm_tool_adapter:to_anthropic(Tools)};
maybe_add_tools(Body, _) ->
    Body.

%% @private 添加流式标志
maybe_add_stream(Body, #{stream := true}) -> Body#{<<"stream">> => true};
maybe_add_stream(Body, _) -> Body.

%%====================================================================
%% 流式事件累加（Anthropic 特有格式）
%%====================================================================

%% @private Anthropic 格式事件累加器
%% Anthropic 使用不同的事件类型：message_start, content_block_delta, message_delta
accumulate_event(#{<<"type">> := <<"content_block_delta">>, <<"delta">> := Delta}, Acc) ->
    Text = maps:get(<<"text">>, Delta, <<>>),
    Acc#{content => <<(maps:get(content, Acc))/binary, Text/binary>>};
accumulate_event(#{<<"type">> := <<"message_start">>, <<"message">> := Msg}, Acc) ->
    Acc#{
        id => maps:get(<<"id">>, Msg, <<>>),
        model => maps:get(<<"model">>, Msg, <<>>)
    };
accumulate_event(#{<<"type">> := <<"message_delta">>, <<"delta">> := Delta}, Acc) ->
    Acc#{finish_reason => maps:get(<<"stop_reason">>, Delta, <<>>)};
accumulate_event(_, Acc) ->
    Acc.
