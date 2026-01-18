%%%-------------------------------------------------------------------
%%% @doc Shell 命令工具
%%%
%%% 提供 Shell 命令执行相关的工具：
%%% - shell_execute: 执行 Shell 命令
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tools_shell).

-include("beamai_tools.hrl").

%% 导入 DSL 函数
-import(beamai_tool, [define/5]).

%% API
-export([all/0]).

%% 工具处理器
-export([
    handle_execute/1,
    handle_execute/2
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 获取所有 Shell 工具定义
-spec all() -> [tool_def()].
all() ->
    [shell_execute_tool()].

%%====================================================================
%% 工具定义（使用 DSL）
%%====================================================================

%% @private shell_execute 工具
shell_execute_tool() ->
    define(<<"shell_execute">>,
           <<"执行 Shell 命令。支持超时控制和输出限制。"/utf8>>,
           #{category => shell,
             permissions => [shell_access],
             metadata => #{dangerous => true, requires_approval => true}},
           [
               {<<"command">>, string, <<"要执行的 Shell 命令"/utf8>>, required},
               {<<"timeout">>, integer, <<"命令超时时间（毫秒，默认 30000）"/utf8>>, {default, 30000}},
               {<<"working_dir">>, string, <<"工作目录（可选）"/utf8>>}
           ],
           fun ?MODULE:handle_execute/2).

%%====================================================================
%% 工具处理器
%%====================================================================

%% @doc 执行 Shell 命令
handle_execute(Args) ->
    handle_execute(Args, #{}).

handle_execute(Args, _Context) ->
    Command = maps:get(<<"command">>, Args),
    Timeout = maps:get(<<"timeout">>, Args, 30000),
    WorkingDir = maps:get(<<"working_dir">>, Args, undefined),

    %% 安全检查
    case beamai_tool_security:check_command(Command) of
        ok ->
            do_execute(Command, Timeout, WorkingDir);
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 执行命令
do_execute(Command, Timeout, WorkingDir) ->
    CommandStr = binary_to_list(Command),

    %% 构建执行选项
    Opts = build_exec_opts(WorkingDir),

    %% 使用 open_port 执行
    try
        Result = execute_with_timeout(CommandStr, Timeout, Opts),
        format_result(Result)
    catch
        error:timeout ->
            {error, {timeout, Timeout}};
        Class:Reason ->
            {error, {execution_error, {Class, Reason}}}
    end.

%% @private 构建执行选项
build_exec_opts(undefined) ->
    [];
build_exec_opts(WorkingDir) ->
    [{cd, binary_to_list(WorkingDir)}].

%% @private 带超时执行命令
execute_with_timeout(Command, Timeout, Opts) ->
    Parent = self(),
    Ref = make_ref(),

    %% 启动执行进程
    Pid = spawn_link(fun() ->
        Result = execute_command(Command, Opts),
        Parent ! {Ref, Result}
    end),

    %% 等待结果或超时
    receive
        {Ref, Result} ->
            Result
    after Timeout ->
        exit(Pid, kill),
        error(timeout)
    end.

%% @private 执行命令
execute_command(Command, Opts) ->
    %% 使用 open_port 执行命令
    PortOpts = [
        stream,
        exit_status,
        use_stdio,
        stderr_to_stdout,
        binary
    ] ++ Opts,

    try
        Port = open_port({spawn, Command}, PortOpts),
        collect_output(Port, [])
    catch
        error:Reason ->
            {error, Reason}
    end.

%% @private 收集输出
collect_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_output(Port, [Data | Acc]);
        {Port, {exit_status, Status}} ->
            Output = iolist_to_binary(lists:reverse(Acc)),
            {ok, Status, Output}
    after 60000 ->
        port_close(Port),
        {error, timeout}
    end.

%% @private 格式化结果
format_result({ok, 0, Output}) ->
    %% 成功执行
    {ok, #{
        success => true,
        exit_code => 0,
        output => truncate_output(Output)
    }};
format_result({ok, ExitCode, Output}) ->
    %% 非零退出码
    {ok, #{
        success => false,
        exit_code => ExitCode,
        output => truncate_output(Output)
    }};
format_result({error, Reason}) ->
    {error, {execution_error, Reason}}.

%% @private 截断输出（最大 100KB）
-define(MAX_OUTPUT_SIZE, 102400).

truncate_output(Output) when byte_size(Output) > ?MAX_OUTPUT_SIZE ->
    Truncated = binary:part(Output, 0, ?MAX_OUTPUT_SIZE),
    <<Truncated/binary, "\n... (output truncated)">>;
truncate_output(Output) ->
    Output.
