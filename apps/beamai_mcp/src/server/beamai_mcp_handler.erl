%%%-------------------------------------------------------------------
%%% @doc MCP 请求处理模块（框架无关）
%%%
%%% 提供 MCP 协议的请求处理逻辑，与具体 HTTP 框架解耦。
%%% 用户可以将此模块集成到任何 HTTP 服务器框架中。
%%%
%%% == 设计理念 ==
%%%
%%% 本模块只负责：
%%% - 解析和验证 MCP 请求
%%% - 调用 beamai_mcp_server 处理业务逻辑
%%% - 格式化响应
%%%
%%% 不负责：
%%% - HTTP 服务器启动/配置
%%% - 路由配置
%%% - TLS/认证（由用户的 HTTP 框架处理）
%%%
%%% == 支持的传输模式 ==
%%%
%%% 1. Streamable HTTP (POST + JSON/SSE 响应)
%%% 2. SSE (GET 建立流，POST 发送请求)
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 初始化配置
%%% Config = beamai_mcp_handler:init(#{
%%%     tools => [Tool1, Tool2],
%%%     resources => [Resource1],
%%%     server_info => #{name => <<"my-server">>, version => <<"1.0">>}
%%% }).
%%%
%%% %% 处理 HTTP POST 请求
%%% {ResponseType, ResponseData, NewConfig} =
%%%     beamai_mcp_handler:handle_post(RequestBody, Headers, Config).
%%%
%%% %% ResponseType = json | sse
%%% %% 根据 ResponseType 设置响应头和发送数据
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_handler).

-include("beamai_mcp.hrl").

%%====================================================================
%% API 导出
%%====================================================================

-export([
    init/1,
    handle_post/3,
    handle_post/4,
    handle_sse_init/2,
    handle_sse_message/3,
    format_sse_response/2,
    get_session_id/1,
    close/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(handler_state, {
    %% MCP 服务器进程
    server_pid :: pid() | undefined,
    %% 会话 ID
    session_id :: binary() | undefined,
    %% 配置
    config :: map(),
    %% SSE 端点 URL（用于 SSE 模式）
    sse_endpoint :: binary() | undefined,
    %% 是否为 SSE 模式
    sse_mode = false :: boolean()
}).

-type handler_state() :: #handler_state{}.
-type response_type() :: json | sse | no_content.
-type response() :: {response_type(), binary() | iolist(), handler_state()}.

-export_type([handler_state/0, response_type/0, response/0]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化 Handler 状态
%%
%% @param Config 配置参数
%%   - tools: 工具列表 [#mcp_tool{}]
%%   - resources: 资源列表 [#mcp_resource{}]
%%   - prompts: 提示列表 [#mcp_prompt{}]
%%   - server_info: 服务器信息 map
%%   - sse_endpoint: SSE 端点 URL（SSE 模式需要）
%% @returns handler_state()
-spec init(map()) -> handler_state().
init(Config) ->
    %% 启动 MCP 服务器进程
    {ok, ServerPid} = beamai_mcp_server:start_link(Config),
    #handler_state{
        server_pid = ServerPid,
        config = Config,
        sse_endpoint = maps:get(sse_endpoint, Config, undefined)
    }.

%% @doc 处理 HTTP POST 请求
%%
%% 这是 Streamable HTTP 模式的主要入口。
%% 根据请求内容和服务器能力，返回 JSON 或 SSE 响应。
%%
%% @param Body 请求体（JSON）
%% @param Headers 请求头 [{Name, Value}]
%% @param State Handler 状态
%% @returns {ResponseType, ResponseData, NewState}
-spec handle_post(binary(), [{binary(), binary()}], handler_state()) -> response().
handle_post(Body, Headers, State) ->
    AcceptSSE = accepts_sse(Headers),
    handle_post(Body, Headers, AcceptSSE, State).

%% @doc 处理 HTTP POST 请求（指定是否接受 SSE）
-spec handle_post(binary(), [{binary(), binary()}], boolean(), handler_state()) -> response().
handle_post(Body, Headers, AcceptSSE, #handler_state{server_pid = ServerPid} = State) ->
    %% 提取会话 ID
    SessionId = get_header(Headers, <<"mcp-session-id">>),
    NewState = State#handler_state{session_id = SessionId},

    %% 解码请求
    case beamai_mcp_jsonrpc:decode(Body) of
        {ok, Request} ->
            %% 处理请求
            case beamai_mcp_server:handle_request(ServerPid, Request) of
                {ok, Response} ->
                    %% 更新会话 ID
                    FinalState = maybe_update_session_id(NewState),
                    case AcceptSSE of
                        true ->
                            %% 返回 SSE 格式
                            SSEData = format_sse_response(<<"message">>, Response),
                            {sse, SSEData, FinalState};
                        false ->
                            %% 返回 JSON 格式
                            {json, Response, FinalState}
                    end;
                {error, _Reason} ->
                    ErrorResponse = beamai_mcp_jsonrpc:internal_error(null),
                    {json, ErrorResponse, NewState}
            end;
        {error, parse_error} ->
            ErrorResponse = beamai_mcp_jsonrpc:parse_error(null),
            {json, ErrorResponse, NewState};
        {error, _} ->
            ErrorResponse = beamai_mcp_jsonrpc:invalid_request(null),
            {json, ErrorResponse, NewState}
    end.

%% @doc 初始化 SSE 连接
%%
%% 用于传统 SSE 模式（GET 请求建立流）。
%% 返回初始的 endpoint 事件，告知客户端 POST 端点。
%%
%% @param Headers 请求头
%% @param State Handler 状态
%% @returns {ok, InitialData, NewState} | {error, Reason}
-spec handle_sse_init([{binary(), binary()}], handler_state()) ->
    {ok, iolist(), handler_state()} | {error, term()}.
handle_sse_init(_Headers, #handler_state{sse_endpoint = undefined}) ->
    {error, sse_endpoint_not_configured};
handle_sse_init(_Headers, #handler_state{sse_endpoint = Endpoint} = State) ->
    SessionId = generate_session_id(),
    NewState = State#handler_state{
        session_id = SessionId,
        sse_mode = true
    },
    %% 发送 endpoint 事件
    EndpointData = jsx:encode(#{<<"uri">> => Endpoint}),
    InitialEvent = format_sse_response(<<"endpoint">>, EndpointData),
    {ok, InitialEvent, NewState}.

%% @doc 处理 SSE 模式下的消息
%%
%% 在 SSE 模式下，客户端通过 POST 发送请求到指定端点，
%% 响应通过 SSE 流返回。
%%
%% @param Body 请求体
%% @param Headers 请求头
%% @param State Handler 状态
%% @returns {ok, ResponseData, NewState} | {error, Reason}
-spec handle_sse_message(binary(), [{binary(), binary()}], handler_state()) ->
    {ok, iolist(), handler_state()} | {error, term()}.
handle_sse_message(Body, _Headers, #handler_state{server_pid = ServerPid} = State) ->
    case beamai_mcp_jsonrpc:decode(Body) of
        {ok, Request} ->
            case beamai_mcp_server:handle_request(ServerPid, Request) of
                {ok, Response} ->
                    SSEData = format_sse_response(<<"message">>, Response),
                    {ok, SSEData, State};
                {error, _Reason} ->
                    ErrorResponse = beamai_mcp_jsonrpc:internal_error(
                        maps:get(<<"id">>, Request, null)
                    ),
                    SSEData = format_sse_response(<<"message">>, ErrorResponse),
                    {ok, SSEData, State}
            end;
        {error, _} ->
            ErrorResponse = beamai_mcp_jsonrpc:parse_error(null),
            SSEData = format_sse_response(<<"message">>, ErrorResponse),
            {ok, SSEData, State}
    end.

%% @doc 格式化 SSE 响应
%%
%% @param EventType 事件类型
%% @param Data 数据（binary 或会被 JSON 编码的 term）
%% @returns iolist
-spec format_sse_response(binary(), binary() | term()) -> iolist().
format_sse_response(EventType, Data) when is_binary(Data) ->
    beamai_sse:encode_event(EventType, Data);
format_sse_response(EventType, Data) ->
    beamai_sse:encode_event(EventType, Data).

%% @doc 获取当前会话 ID
-spec get_session_id(handler_state()) -> binary() | undefined.
get_session_id(#handler_state{session_id = SessionId}) ->
    SessionId.

%% @doc 关闭 Handler
%%
%% 停止关联的 MCP 服务器进程。
-spec close(handler_state()) -> ok.
close(#handler_state{server_pid = undefined}) ->
    ok;
close(#handler_state{server_pid = ServerPid}) ->
    try
        beamai_mcp_server:stop(ServerPid)
    catch
        exit:noproc -> ok;
        exit:{noproc, _} -> ok
    end,
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 检查是否接受 SSE
-spec accepts_sse([{binary(), binary()}]) -> boolean().
accepts_sse(Headers) ->
    case get_header(Headers, <<"accept">>) of
        undefined -> false;
        Accept ->
            binary:match(Accept, <<"text/event-stream">>) =/= nomatch
    end.

%% @private 获取请求头
-spec get_header([{binary(), binary()}], binary()) -> binary() | undefined.
get_header(Headers, Name) ->
    LowerName = string:lowercase(Name),
    case lists:keyfind(LowerName, 1, [{string:lowercase(K), V} || {K, V} <- Headers]) of
        {_, Value} -> Value;
        false -> undefined
    end.

%% @private 可能更新会话 ID
-spec maybe_update_session_id(handler_state()) -> handler_state().
maybe_update_session_id(#handler_state{session_id = undefined, server_pid = ServerPid} = State) ->
    case beamai_mcp_server:get_session(ServerPid) of
        {ok, #mcp_session{id = Id}} ->
            State#handler_state{session_id = Id};
        _ ->
            State
    end;
maybe_update_session_id(State) ->
    State.

%% @private 生成会话 ID
-spec generate_session_id() -> binary().
generate_session_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFF),
    iolist_to_binary(io_lib:format("mcp-~.16b-~.4b", [Timestamp, Random])).
