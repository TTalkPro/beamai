%%%-------------------------------------------------------------------
%%% @doc Shell 命令执行插件
%%%
%%% 提供 shell 命令执行能力：
%%% - shell_execute: 执行 shell 命令，支持超时控制和输出限制
%%%
%%% 该插件标记为危险操作，执行前需要人工审批。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_plugin_shell).

-behaviour(beamai_plugin_behaviour).

-export([plugin_info/0, functions/0]).
-export([handle_execute/2]).

%%====================================================================
%% 插件行为回调
%%====================================================================

%% @doc 返回插件元数据
%%
%% 提供插件的名称和描述信息，用于插件注册和发现。
%%
%% @returns 插件信息映射，包含 name 和 description 字段
plugin_info() ->
    #{name => <<"shell">>,
      description => <<"Shell command execution">>}.

%% @doc 返回函数定义列表
%%
%% 返回本插件提供的所有可调用函数定义。
%% 当前仅包含 shell_execute 函数。
%%
%% @returns 函数定义列表
functions() ->
    [shell_execute_func()].

%%====================================================================
%% 函数定义
%%====================================================================

%% @doc 构建 shell 命令执行函数定义
%% 该函数标记为危险操作（dangerous）且需要审批（requires_approval）
shell_execute_func() ->
    beamai_function:new(<<"shell_execute">>, fun ?MODULE:handle_execute/2, #{
        description => <<"Execute shell command with timeout and output limits.">>,
        parameters => #{
            <<"command">> => #{type => string, description => <<"Shell command to execute">>, required => true},
            <<"timeout">> => #{type => integer, description => <<"Timeout in milliseconds (default 30000)">>},
            <<"working_dir">> => #{type => string, description => <<"Working directory (optional)">>}
        },
        metadata => #{dangerous => true, requires_approval => true}
    }).

%%====================================================================
%% 处理器函数
%%====================================================================

%% @doc 执行 shell 命令
%%
%% 在指定工作目录下执行 shell 命令，支持超时控制。
%% 执行前会进行命令安全检查，防止执行危险命令。
%%
%% @param Args 参数映射，包含：
%%   - command: 要执行的 shell 命令（必填）
%%   - timeout: 超时时间（毫秒），默认 30000
%%   - working_dir: 工作目录（可选）
%% @param Context 调用上下文（本函数未使用）
%% @returns {ok, 执行结果映射} | {error, 错误原因}
handle_execute(Args, _Context) ->
    Command = maps:get(<<"command">>, Args),
    Timeout = maps:get(<<"timeout">>, Args, 30000),
    WorkingDir = maps:get(<<"working_dir">>, Args, undefined),
    case beamai_tool_security:check_command(Command) of
        ok -> do_execute(Command, Timeout, WorkingDir);
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% 输出最大字节数限制（100KB）
-define(MAX_OUTPUT_SIZE, 102400).

%% @doc 执行命令的核心逻辑
%% 将二进制命令转为字符串，构建执行选项，
%% 带超时执行命令并格式化返回结果
do_execute(Command, Timeout, WorkingDir) ->
    CommandStr = binary_to_list(Command),
    Opts = build_exec_opts(WorkingDir),
    try
        Result = execute_with_timeout(CommandStr, Timeout, Opts),
        format_result(Result)
    catch
        error:timeout -> {error, {timeout, Timeout}};
        Class:Reason -> {error, {execution_error, {Class, Reason}}}
    end.

%% @doc 构建执行选项
%% 如果指定了工作目录，添加 {cd, Dir} 选项
build_exec_opts(undefined) -> [];
build_exec_opts(WorkingDir) -> [{cd, binary_to_list(WorkingDir)}].

%% @doc 带超时的命令执行
%% 在独立链接进程中执行命令，超时后强制终止进程并抛出 timeout 异常
execute_with_timeout(Command, Timeout, Opts) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun() ->
        Result = execute_command(Command, Opts),
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        error(timeout)
    end.

%% @doc 通过 Erlang Port 执行命令
%% 使用 open_port 创建系统进程，收集标准输出和退出状态
execute_command(Command, Opts) ->
    PortOpts = [stream, exit_status, use_stdio, stderr_to_stdout, binary] ++ Opts,
    try
        Port = open_port({spawn, Command}, PortOpts),
        collect_output(Port, [])
    catch
        error:Reason -> {error, Reason}
    end.

%% @doc 收集 Port 输出数据
%% 循环接收 Port 数据消息，直到收到退出状态或超时（60秒硬超时）
collect_output(Port, Acc) ->
    receive
        {Port, {data, Data}} -> collect_output(Port, [Data | Acc]);
        {Port, {exit_status, Status}} ->
            Output = iolist_to_binary(lists:reverse(Acc)),
            {ok, Status, Output}
    after 60000 ->
        port_close(Port),
        {error, timeout}
    end.

%% @doc 格式化执行结果
%% 将原始执行结果转换为标准化的返回格式：
%% - 退出码为 0 表示成功
%% - 退出码非 0 表示失败
%% - 错误情况包装为 execution_error
format_result({ok, 0, Output}) ->
    {ok, #{success => true, exit_code => 0, output => truncate_output(Output)}};
format_result({ok, ExitCode, Output}) ->
    {ok, #{success => false, exit_code => ExitCode, output => truncate_output(Output)}};
format_result({error, Reason}) ->
    {error, {execution_error, Reason}}.

%% @doc 截断过长的输出内容
%% 当输出超过 MAX_OUTPUT_SIZE 时，截取前部分并添加截断提示
truncate_output(Output) when byte_size(Output) > ?MAX_OUTPUT_SIZE ->
    Truncated = binary:part(Output, 0, ?MAX_OUTPUT_SIZE),
    <<Truncated/binary, "\n... (output truncated)">>;
truncate_output(Output) -> Output.
