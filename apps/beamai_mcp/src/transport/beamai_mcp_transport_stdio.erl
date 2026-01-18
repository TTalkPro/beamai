%%%-------------------------------------------------------------------
%%% @doc MCP Stdio 传输实现
%%%
%%% 通过标准输入输出与外部 MCP 服务器进程通信。
%%% 仅用于客户端场景。
%%%
%%% == 配置参数 ==
%%%
%%% ```erlang
%%% #{
%%%     transport => stdio,
%%%     command => "npx",                    %% 必填：命令
%%%     args => ["-y", "@mcp/server", "/tmp"], %% 可选：参数列表
%%%     env => [{"KEY", "value"}],           %% 可选：环境变量
%%%     cd => "/path/to/dir"                 %% 可选：工作目录
%%% }
%%% ```
%%%
%%% == 协议格式 ==
%%%
%%% MCP Stdio 使用换行符分隔的 JSON 消息：
%%% - 每条消息是一行 JSON
%%% - 消息以 \n 结尾
%%% - 无需 Content-Length 头
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_transport_stdio).

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

-record(stdio_state, {
    port :: port(),
    buffer = <<>> :: binary(),
    command :: string()
}).

-type state() :: #stdio_state{}.

%%====================================================================
%% 行为回调实现
%%====================================================================

%% @doc 启动外部进程并建立 Stdio 连接
%%
%% @param Config 配置参数
%% @returns {ok, State} | {error, Reason}
-spec connect(map()) -> {ok, state()} | {error, term()}.
connect(#{command := Command} = Config) ->
    Args = maps:get(args, Config, []),
    Env = maps:get(env, Config, []),
    Cd = maps:get(cd, Config, undefined),

    %% 构建 port 选项
    PortOpts = build_port_opts(Command, Args, Env, Cd),

    try
        %% 打开 port 启动外部进程
        Port = open_port({spawn_executable, find_executable(Command)}, PortOpts),
        {ok, #stdio_state{
            port = Port,
            buffer = <<>>,
            command = Command
        }}
    catch
        error:Reason ->
            {error, {port_open_failed, Reason}}
    end;
connect(_) ->
    {error, missing_command}.

%% @doc 发送消息到外部进程
%%
%% @param Message JSON 消息
%% @param State 状态
%% @returns {ok, NewState} | {error, Reason}
-spec send(binary(), state()) -> {ok, state()} | {error, term()}.
send(Message, #stdio_state{port = Port} = State) ->
    %% 添加换行符
    Data = <<Message/binary, "\n">>,
    try
        true = port_command(Port, Data),
        {ok, State}
    catch
        error:badarg ->
            {error, port_closed}
    end.

%% @doc 从外部进程接收消息
%%
%% @param Timeout 超时时间
%% @param State 状态
%% @returns {ok, Message, NewState} | {error, Reason}
-spec recv(timeout(), state()) -> {ok, binary(), state()} | {error, term()}.
recv(Timeout, #stdio_state{port = Port, buffer = Buffer} = State) ->
    %% 先检查缓冲区是否有完整消息
    case extract_message(Buffer) of
        {ok, Message, Rest} ->
            {ok, Message, State#stdio_state{buffer = Rest}};
        need_more ->
            %% 等待更多数据
            receive_data(Port, Buffer, Timeout, State)
    end.

%% @doc 关闭连接
%%
%% @param State 状态
%% @returns ok | {error, Reason}
-spec close(state()) -> ok | {error, term()}.
close(#stdio_state{port = Port}) ->
    try
        port_close(Port),
        ok
    catch
        error:badarg ->
            ok  %% 已经关闭
    end.

%% @doc 检查连接状态
%%
%% @param State 状态
%% @returns boolean()
-spec is_connected(state()) -> boolean().
is_connected(#stdio_state{port = Port}) ->
    try
        erlang:port_info(Port) =/= undefined
    catch
        error:badarg -> false
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 构建 port 选项
-spec build_port_opts(string(), [string()], [{string(), string()}], string() | undefined) ->
    [term()].
build_port_opts(_Command, Args, Env, Cd) ->
    BaseOpts = [
        {args, Args},
        binary,
        stream,
        use_stdio,
        exit_status,
        stderr_to_stdout
    ],

    WithEnv = case Env of
        [] -> BaseOpts;
        _ -> [{env, Env} | BaseOpts]
    end,

    case Cd of
        undefined -> WithEnv;
        _ -> [{cd, Cd} | WithEnv]
    end.

%% @private 查找可执行文件路径
-spec find_executable(string()) -> string().
find_executable(Command) ->
    case os:find_executable(Command) of
        false ->
            %% 尝试直接使用（可能是绝对路径）
            Command;
        Path ->
            Path
    end.

%% @private 从缓冲区提取完整消息
-spec extract_message(binary()) -> {ok, binary(), binary()} | need_more.
extract_message(Buffer) ->
    case binary:split(Buffer, <<"\n">>) of
        [Line, Rest] when Line =/= <<>> ->
            {ok, Line, Rest};
        [<<>>, Rest] ->
            %% 空行，跳过
            extract_message(Rest);
        [_] ->
            need_more
    end.

%% @private 接收数据
-spec receive_data(port(), binary(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_data(Port, Buffer, Timeout, State) ->
    receive
        {Port, {data, Data}} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            case extract_message(NewBuffer) of
                {ok, Message, Rest} ->
                    {ok, Message, State#stdio_state{buffer = Rest}};
                need_more ->
                    receive_data(Port, NewBuffer, Timeout, State)
            end;
        {Port, {exit_status, Status}} ->
            {error, {process_exited, Status}};
        {Port, eof} ->
            {error, eof}
    after Timeout ->
        {error, timeout}
    end.
