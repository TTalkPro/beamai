%%%-------------------------------------------------------------------
%%% @doc MCP SSE 传输实现
%%%
%%% 通过 Server-Sent Events 与 MCP 服务器通信。
%%% 支持客户端模式。
%%%
%%% == 配置参数 ==
%%%
%%% ```erlang
%%% #{
%%%     transport => sse,
%%%     url => <<"https://example.com/mcp/sse">>, %% 必填：SSE 端点
%%%     headers => [{<<"Authorization">>, <<"Bearer token">>}], %% 可选
%%%     timeout => 30000                          %% 可选：连接超时
%%% }
%%% ```
%%%
%%% == SSE 协议 ==
%%%
%%% SSE 连接流程：
%%% 1. 客户端发起 GET 请求到 SSE 端点
%%% 2. 服务器返回 endpoint 事件，包含 POST 端点 URL
%%% 3. 客户端向 POST 端点发送 JSON-RPC 请求
%%% 4. 服务器通过 SSE 流返回响应
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_transport_sse).

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

%%====================================================================
%% 类型定义
%%====================================================================

-record(sse_state, {
    %% SSE 连接
    sse_ref :: reference() | undefined,
    sse_url :: binary(),
    %% POST 端点（从 endpoint 事件获取）
    post_url :: binary() | undefined,
    %% 消息队列
    message_queue = [] :: [binary()],
    %% SSE 解析缓冲区
    buffer = <<>> :: binary(),
    %% 请求头
    headers = [] :: [{binary(), binary()}],
    %% 超时配置
    timeout :: pos_integer()
}).

-type state() :: #sse_state{}.

%%====================================================================
%% 行为回调实现
%%====================================================================

%% @doc 建立 SSE 连接
%%
%% @param Config 配置参数
%% @returns {ok, State} | {error, Reason}
-spec connect(map()) -> {ok, state()} | {error, term()}.
connect(#{url := Url} = Config) ->
    Headers = maps:get(headers, Config, []),
    Timeout = maps:get(timeout, Config, 30000),

    State = #sse_state{
        sse_url = Url,
        headers = Headers,
        timeout = Timeout
    },

    %% 发起 SSE 连接请求
    case start_sse_connection(Url, Headers) of
        {ok, Ref} ->
            %% 等待 endpoint 事件
            NewState = State#sse_state{sse_ref = Ref},
            wait_for_endpoint(NewState, Timeout);
        {error, Reason} ->
            {error, {sse_connect_failed, Reason}}
    end;
connect(_) ->
    {error, missing_url}.

%% @doc 发送消息
%%
%% @param Message JSON 消息
%% @param State 状态
%% @returns {ok, NewState} | {error, Reason}
-spec send(binary(), state()) -> {ok, state()} | {error, term()}.
send(_Message, #sse_state{post_url = undefined}) ->
    {error, no_post_endpoint};
send(Message, #sse_state{post_url = PostUrl, headers = Headers, timeout = Timeout} = State) ->
    %% 通过 POST 发送消息
    RequestHeaders = [
        {<<"Content-Type">>, <<"application/json">>}
        | Headers
    ],
    case hackney:request(post, PostUrl, RequestHeaders, Message,
                         [{recv_timeout, Timeout}, with_body]) of
        {ok, Status, _RespHeaders, _Body} when Status >= 200, Status < 300 ->
            {ok, State};
        {ok, Status, _RespHeaders, Body} ->
            {error, {http_error, Status, Body}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% @doc 接收消息
%%
%% @param Timeout 超时时间
%% @param State 状态
%% @returns {ok, Message, NewState} | {error, Reason}
-spec recv(timeout(), state()) -> {ok, binary(), state()} | {error, term()}.
recv(_Timeout, #sse_state{message_queue = [Msg | Rest]} = State) ->
    %% 队列中有消息，直接返回
    {ok, Msg, State#sse_state{message_queue = Rest}};
recv(Timeout, #sse_state{sse_ref = Ref, buffer = Buffer} = State) when Ref =/= undefined ->
    %% 等待 SSE 事件
    receive_sse_events(Ref, Buffer, Timeout, State);
recv(_Timeout, _State) ->
    {error, not_connected}.

%% @doc 关闭连接
%%
%% @param State 状态
%% @returns ok | {error, Reason}
-spec close(state()) -> ok | {error, term()}.
close(#sse_state{sse_ref = Ref}) when Ref =/= undefined ->
    hackney:close(Ref),
    ok;
close(_) ->
    ok.

%% @doc 检查连接状态
%%
%% @param State 状态
%% @returns boolean()
-spec is_connected(state()) -> boolean().
is_connected(#sse_state{sse_ref = Ref, post_url = PostUrl}) ->
    Ref =/= undefined andalso PostUrl =/= undefined.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 发起 SSE 连接
-spec start_sse_connection(binary(), [{binary(), binary()}]) ->
    {ok, reference()} | {error, term()}.
start_sse_connection(Url, Headers) ->
    RequestHeaders = [
        {<<"Accept">>, <<"text/event-stream">>},
        {<<"Cache-Control">>, <<"no-cache">>}
        | Headers
    ],
    case hackney:request(get, Url, RequestHeaders, <<>>, [async]) of
        {ok, Ref} ->
            {ok, Ref};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 等待 endpoint 事件
-spec wait_for_endpoint(state(), timeout()) -> {ok, state()} | {error, term()}.
wait_for_endpoint(#sse_state{sse_ref = Ref} = State, Timeout) ->
    receive
        {hackney_response, Ref, {status, Status, _Reason}} when Status >= 200, Status < 300 ->
            wait_for_endpoint(State, Timeout);
        {hackney_response, Ref, {status, Status, Reason}} ->
            {error, {http_error, Status, Reason}};
        {hackney_response, Ref, {headers, _Headers}} ->
            wait_for_endpoint(State, Timeout);
        {hackney_response, Ref, done} ->
            {error, connection_closed};
        {hackney_response, Ref, {error, Reason}} ->
            {error, Reason};
        {hackney_response, Ref, Data} when is_binary(Data) ->
            %% 解析 SSE 数据
            Buffer = State#sse_state.buffer,
            NewBuffer = <<Buffer/binary, Data/binary>>,
            case parse_endpoint_event(NewBuffer) of
                {ok, PostUrl, Rest} ->
                    {ok, State#sse_state{post_url = PostUrl, buffer = Rest}};
                {need_more, Rest} ->
                    wait_for_endpoint(State#sse_state{buffer = Rest}, Timeout)
            end
    after Timeout ->
        {error, timeout}
    end.

%% @private 解析 endpoint 事件
-spec parse_endpoint_event(binary()) -> {ok, binary(), binary()} | {need_more, binary()}.
parse_endpoint_event(Buffer) ->
    %% 使用 beamai_sse 解析
    {Remaining, Events} = beamai_sse:parse(Buffer),
    %% 查找 endpoint 事件
    case find_endpoint_event(Events) of
        {ok, PostUrl} ->
            {ok, PostUrl, Remaining};
        not_found ->
            {need_more, Remaining}
    end.

%% @private 查找 endpoint 事件
-spec find_endpoint_event([map()]) -> {ok, binary()} | not_found.
find_endpoint_event([]) ->
    not_found;
find_endpoint_event([#{event := <<"endpoint">>, data := Data} | _Rest]) ->
    %% 解析 endpoint URL
    case jsx:decode(Data, [return_maps]) of
        #{<<"uri">> := Uri} -> {ok, Uri};
        #{<<"url">> := Url} -> {ok, Url};
        _ -> not_found
    end;
find_endpoint_event([_ | Rest]) ->
    find_endpoint_event(Rest).

%% @private 接收 SSE 事件
-spec receive_sse_events(reference(), binary(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_sse_events(Ref, Buffer, Timeout, State) ->
    receive
        {hackney_response, Ref, done} ->
            {error, connection_closed};
        {hackney_response, Ref, {error, Reason}} ->
            {error, Reason};
        {hackney_response, Ref, Data} when is_binary(Data) ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            {Remaining, Events} = beamai_sse:parse(NewBuffer),
            %% 提取消息事件
            Messages = extract_messages(Events),
            case Messages of
                [Msg | Rest] ->
                    %% 返回第一条消息，其余放入队列
                    NewQueue = State#sse_state.message_queue ++ Rest,
                    {ok, Msg, State#sse_state{buffer = Remaining, message_queue = NewQueue}};
                [] ->
                    receive_sse_events(Ref, Remaining, Timeout, State)
            end
    after Timeout ->
        {error, timeout}
    end.

%% @private 从事件中提取消息
-spec extract_messages([map()]) -> [binary()].
extract_messages(Events) ->
    lists:filtermap(fun(Event) ->
        case Event of
            #{event := <<"message">>, data := Data} ->
                {true, Data};
            #{data := Data} when not is_map_key(event, Event) ->
                %% 默认事件类型是 message
                {true, Data};
            _ ->
                false
        end
    end, Events).
