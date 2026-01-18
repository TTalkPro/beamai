%%%-------------------------------------------------------------------
%%% @doc MCP Streamable HTTP 传输实现
%%%
%%% 实现 MCP 2025-11-25 版本的 Streamable HTTP 传输。
%%% 支持客户端模式。
%%%
%%% == 配置参数 ==
%%%
%%% ```erlang
%%% #{
%%%     transport => http,
%%%     url => <<"https://example.com/mcp">>,    %% 必填：服务器端点
%%%     headers => [{<<"Authorization">>, <<"Bearer token">>}], %% 可选
%%%     timeout => 30000,                         %% 可选：请求超时
%%%     session_id => undefined                   %% 可选：会话 ID
%%% }
%%% ```
%%%
%%% == Streamable HTTP 协议 ==
%%%
%%% 请求流程：
%%% 1. 客户端发送 POST 请求，Content-Type: application/json
%%% 2. 服务器可能返回：
%%%    - JSON 响应 (Content-Type: application/json)
%%%    - SSE 流 (Content-Type: text/event-stream)
%%% 3. 服务器可以在响应头中返回 Mcp-Session-Id
%%% 4. 客户端在后续请求中携带 Mcp-Session-Id
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_transport_http).

-behaviour(beamai_mcp_transport).

%%====================================================================
%% 行为回调导出
%%====================================================================

-export([
    connect/1,
    send/2,
    recv/2,
    close/1,
    is_connected/1
]).

%% 额外 API
-export([
    request/2,
    request/3
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(http_state, {
    url :: binary(),
    headers = [] :: [{binary(), binary()}],
    timeout :: pos_integer(),
    session_id :: binary() | undefined,
    %% 当前请求的 SSE 流引用
    stream_ref :: reference() | undefined,
    %% SSE 解析缓冲区
    buffer = <<>> :: binary(),
    %% 消息队列
    message_queue = [] :: [binary()],
    %% 是否已连接（已完成初始化握手）
    connected = false :: boolean()
}).

-type state() :: #http_state{}.

%%====================================================================
%% 行为回调实现
%%====================================================================

%% @doc 创建 HTTP 传输状态
%%
%% 注意：Streamable HTTP 是无状态的，connect 只是初始化配置。
%%
%% @param Config 配置参数
%% @returns {ok, State} | {error, Reason}
-spec connect(map()) -> {ok, state()} | {error, term()}.
connect(#{url := Url} = Config) ->
    Headers = maps:get(headers, Config, []),
    Timeout = maps:get(timeout, Config, 30000),
    SessionId = maps:get(session_id, Config, undefined),

    State = #http_state{
        url = Url,
        headers = Headers,
        timeout = Timeout,
        session_id = SessionId,
        connected = true
    },
    {ok, State};
connect(_) ->
    {error, missing_url}.

%% @doc 发送请求并等待响应
%%
%% Streamable HTTP 是请求-响应模式，send 和 recv 组合使用。
%%
%% @param Message JSON 消息
%% @param State 状态
%% @returns {ok, NewState} | {error, Reason}
-spec send(binary(), state()) -> {ok, state()} | {error, term()}.
send(Message, #http_state{url = Url, headers = Headers,
                           timeout = Timeout, session_id = SessionId} = State) ->
    %% 构建请求头
    RequestHeaders = build_request_headers(Headers, SessionId),

    %% 发送 POST 请求（异步模式以支持 SSE 流）
    case hackney:request(post, Url, RequestHeaders, Message, [async, {recv_timeout, Timeout}]) of
        {ok, Ref} ->
            {ok, State#http_state{stream_ref = Ref, buffer = <<>>, message_queue = []}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% @doc 接收响应
%%
%% @param Timeout 超时时间
%% @param State 状态
%% @returns {ok, Message, NewState} | {error, Reason}
-spec recv(timeout(), state()) -> {ok, binary(), state()} | {error, term()}.
recv(_Timeout, #http_state{message_queue = [Msg | Rest]} = State) ->
    %% 队列中有消息
    {ok, Msg, State#http_state{message_queue = Rest}};
recv(_Timeout, #http_state{stream_ref = undefined}) ->
    %% 没有活跃的请求
    {error, no_pending_request};
recv(Timeout, #http_state{stream_ref = Ref} = State) ->
    receive_response(Ref, Timeout, State).

%% @doc 关闭连接
%%
%% @param State 状态
%% @returns ok | {error, Reason}
-spec close(state()) -> ok | {error, term()}.
close(#http_state{stream_ref = Ref}) when Ref =/= undefined ->
    hackney:close(Ref),
    ok;
close(_) ->
    ok.

%% @doc 检查连接状态
%%
%% @param State 状态
%% @returns boolean()
-spec is_connected(state()) -> boolean().
is_connected(#http_state{connected = Connected}) ->
    Connected.

%%====================================================================
%% 额外 API
%%====================================================================

%% @doc 发送请求并同步等待响应
%%
%% @param Message JSON 消息
%% @param State 状态
%% @returns {ok, Response, NewState} | {error, Reason}
-spec request(binary(), state()) -> {ok, binary(), state()} | {error, term()}.
request(Message, State) ->
    request(Message, State#http_state.timeout, State).

%% @doc 发送请求并同步等待响应（指定超时）
-spec request(binary(), timeout(), state()) -> {ok, binary(), state()} | {error, term()}.
request(Message, Timeout, State) ->
    case send(Message, State) of
        {ok, NewState} ->
            recv(Timeout, NewState);
        Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 构建请求头
-spec build_request_headers([{binary(), binary()}], binary() | undefined) ->
    [{binary(), binary()}].
build_request_headers(Headers, SessionId) ->
    BaseHeaders = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Accept">>, <<"application/json, text/event-stream">>}
        | Headers
    ],
    case SessionId of
        undefined -> BaseHeaders;
        _ -> [{<<"Mcp-Session-Id">>, SessionId} | BaseHeaders]
    end.

%% @private 接收响应
-spec receive_response(reference(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_response(Ref, Timeout, State) ->
    receive
        {hackney_response, Ref, {status, Status, _Reason}} when Status >= 200, Status < 300 ->
            receive_response(Ref, Timeout, State);
        {hackney_response, Ref, {status, Status, Reason}} ->
            hackney:close(Ref),
            {error, {http_error, Status, Reason}};
        {hackney_response, Ref, {headers, Headers}} ->
            %% 检查 Content-Type 和 Session-Id
            ContentType = get_header(Headers, <<"content-type">>),
            NewSessionId = get_header(Headers, <<"mcp-session-id">>),
            NewState = case NewSessionId of
                undefined -> State;
                _ -> State#http_state{session_id = NewSessionId}
            end,
            %% 根据 Content-Type 处理响应
            case is_sse_content_type(ContentType) of
                true ->
                    %% SSE 流响应
                    receive_sse_response(Ref, Timeout, NewState);
                false ->
                    %% JSON 响应
                    receive_json_response(Ref, Timeout, NewState)
            end;
        {hackney_response, Ref, done} ->
            {error, unexpected_done};
        {hackney_response, Ref, {error, Reason}} ->
            {error, Reason}
    after Timeout ->
        hackney:close(Ref),
        {error, timeout}
    end.

%% @private 接收 JSON 响应
-spec receive_json_response(reference(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_json_response(Ref, Timeout, State) ->
    receive_json_body(Ref, <<>>, Timeout, State).

-spec receive_json_body(reference(), binary(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_json_body(Ref, Acc, Timeout, State) ->
    receive
        {hackney_response, Ref, done} ->
            hackney:close(Ref),
            {ok, Acc, State#http_state{stream_ref = undefined}};
        {hackney_response, Ref, Data} when is_binary(Data) ->
            receive_json_body(Ref, <<Acc/binary, Data/binary>>, Timeout, State);
        {hackney_response, Ref, {error, Reason}} ->
            {error, Reason}
    after Timeout ->
        hackney:close(Ref),
        {error, timeout}
    end.

%% @private 接收 SSE 响应
-spec receive_sse_response(reference(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_sse_response(Ref, Timeout, #http_state{buffer = Buffer} = State) ->
    receive
        {hackney_response, Ref, done} ->
            hackney:close(Ref),
            %% 处理剩余缓冲区
            {_Remaining, Events} = beamai_sse:parse(Buffer),
            Messages = extract_messages(Events),
            case Messages of
                [Msg | Rest] ->
                    {ok, Msg, State#http_state{
                        stream_ref = undefined,
                        buffer = <<>>,
                        message_queue = Rest
                    }};
                [] ->
                    {error, no_message}
            end;
        {hackney_response, Ref, Data} when is_binary(Data) ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            {Remaining, Events} = beamai_sse:parse(NewBuffer),
            Messages = extract_messages(Events),
            case Messages of
                [Msg | Rest] ->
                    {ok, Msg, State#http_state{
                        buffer = Remaining,
                        message_queue = Rest
                    }};
                [] ->
                    receive_sse_response(Ref, Timeout, State#http_state{buffer = Remaining})
            end;
        {hackney_response, Ref, {error, Reason}} ->
            {error, Reason}
    after Timeout ->
        hackney:close(Ref),
        {error, timeout}
    end.

%% @private 获取响应头
-spec get_header([{binary(), binary()}], binary()) -> binary() | undefined.
get_header(Headers, Name) ->
    LowerName = string:lowercase(Name),
    case lists:keyfind(LowerName, 1, [{string:lowercase(K), V} || {K, V} <- Headers]) of
        {_, Value} -> Value;
        false -> undefined
    end.

%% @private 检查是否为 SSE Content-Type
-spec is_sse_content_type(binary() | undefined) -> boolean().
is_sse_content_type(undefined) -> false;
is_sse_content_type(ContentType) ->
    binary:match(ContentType, <<"text/event-stream">>) =/= nomatch.

%% @private 从 SSE 事件中提取消息
-spec extract_messages([map()]) -> [binary()].
extract_messages(Events) ->
    lists:filtermap(fun(Event) ->
        case Event of
            #{event := <<"message">>, data := Data} ->
                {true, Data};
            #{data := Data} when not is_map_key(event, Event) ->
                {true, Data};
            _ ->
                false
        end
    end, Events).
