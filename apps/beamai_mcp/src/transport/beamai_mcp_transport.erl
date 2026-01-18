%%%-------------------------------------------------------------------
%%% @doc MCP 传输层行为定义
%%%
%%% 定义 MCP 传输层的通用接口。
%%% 所有传输实现（Stdio、SSE、HTTP）都需要实现此行为。
%%%
%%% == 传输类型 ==
%%%
%%% - Stdio: 通过标准输入输出与本地进程通信（仅客户端）
%%% - SSE: Server-Sent Events 流式传输（客户端和服务端）
%%% - HTTP: Streamable HTTP POST + SSE（客户端和服务端）
%%%
%%% == 回调函数 ==
%%%
%%% - connect/1: 建立连接
%%% - send/2: 发送消息
%%% - recv/2: 接收消息
%%% - close/1: 关闭连接
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_transport).

%%====================================================================
%% 行为定义
%%====================================================================

%% @doc 建立传输连接
%%
%% @param Config 传输配置
%% @returns {ok, State} | {error, Reason}
-callback connect(Config :: map()) ->
    {ok, State :: term()} | {error, Reason :: term()}.

%% @doc 发送消息
%%
%% @param Message 要发送的消息（已编码的 JSON）
%% @param State 传输状态
%% @returns {ok, NewState} | {error, Reason}
-callback send(Message :: binary(), State :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

%% @doc 接收消息
%%
%% @param Timeout 超时时间（毫秒）
%% @param State 传输状态
%% @returns {ok, Message, NewState} | {error, Reason}
-callback recv(Timeout :: timeout(), State :: term()) ->
    {ok, Message :: binary(), NewState :: term()} |
    {error, Reason :: term()}.

%% @doc 关闭连接
%%
%% @param State 传输状态
%% @returns ok | {error, Reason}
-callback close(State :: term()) ->
    ok | {error, Reason :: term()}.

%% 可选回调：检查连接状态
-callback is_connected(State :: term()) -> boolean().

-optional_callbacks([is_connected/1]).

%%====================================================================
%% API 导出
%%====================================================================

-export([
    create/1,
    connect/2,
    send/3,
    recv/3,
    close/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type transport() :: {Module :: module(), State :: term()}.

-export_type([transport/0]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 根据配置创建传输层
%%
%% @param Config 传输配置
%%   - transport: stdio | sse | http
%%   - 其他传输特定配置
%% @returns {ok, Transport} | {error, Reason}
-spec create(map()) -> {ok, transport()} | {error, term()}.
create(#{transport := stdio} = Config) ->
    case beamai_mcp_transport_stdio:connect(Config) of
        {ok, State} -> {ok, {beamai_mcp_transport_stdio, State}};
        Error -> Error
    end;
create(#{transport := sse} = Config) ->
    case beamai_mcp_transport_sse:connect(Config) of
        {ok, State} -> {ok, {beamai_mcp_transport_sse, State}};
        Error -> Error
    end;
create(#{transport := http} = Config) ->
    case beamai_mcp_transport_http:connect(Config) of
        {ok, State} -> {ok, {beamai_mcp_transport_http, State}};
        Error -> Error
    end;
create(#{transport := Transport}) ->
    {error, {unsupported_transport, Transport}};
create(_) ->
    {error, missing_transport}.

%% @doc 建立连接
%%
%% @param Module 传输模块
%% @param Config 传输配置
%% @returns {ok, State} | {error, Reason}
-spec connect(module(), map()) -> {ok, term()} | {error, term()}.
connect(Module, Config) ->
    Module:connect(Config).

%% @doc 发送消息
%%
%% @param Module 传输模块
%% @param Message 消息
%% @param State 传输状态
%% @returns {ok, NewState} | {error, Reason}
-spec send(module(), binary(), term()) -> {ok, term()} | {error, term()}.
send(Module, Message, State) ->
    Module:send(Message, State).

%% @doc 接收消息
%%
%% @param Module 传输模块
%% @param Timeout 超时
%% @param State 传输状态
%% @returns {ok, Message, NewState} | {error, Reason}
-spec recv(module(), timeout(), term()) -> {ok, binary(), term()} | {error, term()}.
recv(Module, Timeout, State) ->
    Module:recv(Timeout, State).

%% @doc 关闭连接
%%
%% @param Module 传输模块
%% @param State 传输状态
%% @returns ok | {error, Reason}
-spec close(module(), term()) -> ok | {error, term()}.
close(Module, State) ->
    Module:close(State).
