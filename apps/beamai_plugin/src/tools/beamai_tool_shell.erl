%%%-------------------------------------------------------------------
%%% @doc Shell 命令执行工具模块
%%%
%%% 提供 shell 命令执行能力：
%%% - shell_execute: 执行 shell 命令，支持超时控制和输出限制
%%%
%%% 该工具标记为危险操作，执行前需要人工审批。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_shell).

-behaviour(beamai_tool_behaviour).

-export([tool_info/0, tools/0]).
-export([handle_execute/2]).

%%====================================================================
%% 工具行为回调
%%====================================================================

tool_info() ->
    #{description => <<"Shell command execution">>,
      tags => [<<"shell">>, <<"system">>]}.

tools() ->
    [shell_execute_tool()].

%%====================================================================
%% 工具定义
%%====================================================================

shell_execute_tool() ->
    #{
        name => <<"shell_execute">>,
        handler => fun ?MODULE:handle_execute/2,
        description => <<"Execute shell command with timeout and output limits.">>,
        tag => <<"shell">>,
        parameters => #{
            <<"command">> => #{type => string, description => <<"Shell command to execute">>, required => true},
            <<"timeout">> => #{type => integer, description => <<"Timeout in milliseconds (default 30000)">>},
            <<"working_dir">> => #{type => string, description => <<"Working directory (optional)">>}
        },
        metadata => #{dangerous => true, requires_approval => true}
    }.

%%====================================================================
%% 处理器函数
%%====================================================================

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

-define(MAX_OUTPUT_SIZE, 102400).

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

build_exec_opts(undefined) -> [];
build_exec_opts(WorkingDir) -> [{cd, binary_to_list(WorkingDir)}].

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

execute_command(Command, Opts) ->
    PortOpts = [stream, exit_status, use_stdio, stderr_to_stdout, binary] ++ Opts,
    try
        Port = open_port({spawn, Command}, PortOpts),
        collect_output(Port, [])
    catch
        error:Reason -> {error, Reason}
    end.

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

format_result({ok, 0, Output}) ->
    {ok, #{success => true, exit_code => 0, output => truncate_output(Output)}};
format_result({ok, ExitCode, Output}) ->
    {ok, #{success => false, exit_code => ExitCode, output => truncate_output(Output)}};
format_result({error, Reason}) ->
    {error, {execution_error, Reason}}.

truncate_output(Output) when byte_size(Output) > ?MAX_OUTPUT_SIZE ->
    Truncated = binary:part(Output, 0, ?MAX_OUTPUT_SIZE),
    <<Truncated/binary, "\n... (output truncated)">>;
truncate_output(Output) -> Output.
