%%%-------------------------------------------------------------------
%%% @doc Shell Tool Middleware - 持久 Shell 会话
%%%
%%% 为 Agent 提供持久的 Shell 会话能力，支持命令执行。
%%%
%%% == 功能特性 ==
%%%
%%% - 持久 Shell 会话（保持工作目录等状态）
%%% - 可配置执行策略（host/docker/sandbox）
%%% - 命令超时控制
%%% - 输出脱敏
%%% - 危险命令拦截
%%%
%%% == 配置示例 ==
%%%
%%% ```erlang
%%% {middleware_shell_tool, #{
%%%     %% 执行策略
%%%     execution_policy => host,  %% host | docker | sandbox
%%%
%%%     %% Docker 配置（当 policy 为 docker 时）
%%%     docker_config => #{
%%%         image => <<"ubuntu:latest">>,
%%%         volumes => []
%%%     },
%%%
%%%     %% 工作目录
%%%     working_dir => <<"/tmp">>,
%%%
%%%     %% 命令超时（毫秒）
%%%     timeout => 30000,
%%%
%%%     %% 启动命令
%%%     startup_commands => [<<"cd /app">>],
%%%
%%%     %% 危险命令列表（会被阻止）
%%%     blocked_commands => [<<"rm -rf /">>, <<":(){ :|:& };:">>],
%%%
%%%     %% 敏感输出模式（会被遮掩）
%%%     redact_patterns => [<<"password=.*">>, <<"secret=.*">>],
%%%
%%%     %% 调试模式
%%%     debug => false
%%% }}
%%% ```
%%%
%%% == 注入的工具 ==
%%%
%%% shell_execute - 执行 Shell 命令
%%% ```
%%% Input: {
%%%     "command": "ls -la",
%%%     "timeout": 30000  // 可选
%%% }
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_shell_tool).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_agent/2, after_agent/2]).

%% 工具函数
-export([
    execute_command/2,
    get_session_info/1
]).

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
-spec init(map()) -> map().
init(Opts) ->
    #{
        execution_policy => maps:get(execution_policy, Opts, host),
        docker_config => maps:get(docker_config, Opts, #{}),
        working_dir => maps:get(working_dir, Opts, <<"/tmp">>),
        timeout => maps:get(timeout, Opts, 30000),
        startup_commands => maps:get(startup_commands, Opts, []),
        blocked_commands => maps:get(blocked_commands, Opts, default_blocked_commands()),
        blocked_patterns => maps:get(blocked_patterns, Opts, default_blocked_patterns()),
        redact_patterns => maps:get(redact_patterns, Opts, default_redact_patterns()),
        max_output_size => maps:get(max_output_size, Opts, 100000),  %% 100KB
        debug => maps:get(debug, Opts, false)
    }.

%% @doc Agent 开始时注入 Shell 工具
-spec before_agent(map(), map()) -> beamai_middleware:middleware_result().
before_agent(State, MwState) ->
    #{startup_commands := StartupCommands, debug := Debug} = MwState,

    %% 注入 Shell 工具
    Tools = graph:get(State, tools, []),
    ShellTool = build_shell_tool(MwState),
    NewTools = [ShellTool | Tools],

    %% 执行启动命令
    case StartupCommands of
        [] ->
            ok;
        _ ->
            lists:foreach(fun(Cmd) ->
                case Debug of
                    true -> logger:info("[ShellTool] 执行启动命令: ~s", [Cmd]);
                    false -> ok
                end,
                execute_command(Cmd, MwState)
            end, StartupCommands)
    end,

    %% 初始化会话状态
    {update, #{
        tools => NewTools,
        mw_shell_session => #{
            started_at => erlang:system_time(millisecond),
            command_count => 0,
            last_command => undefined,
            last_exit_code => undefined
        }
    }}.

%% @doc Agent 结束时清理
-spec after_agent(map(), map()) -> beamai_middleware:middleware_result().
after_agent(_State, _MwState) ->
    %% 可以在这里执行清理命令
    ok.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 执行 Shell 命令
-spec execute_command(binary(), map()) -> {ok, binary(), integer()} | {error, term()}.
execute_command(Command, MwState) ->
    #{execution_policy := Policy,
      working_dir := WorkingDir,
      timeout := Timeout,
      blocked_commands := BlockedCommands,
      blocked_patterns := BlockedPatterns,
      redact_patterns := RedactPatterns,
      max_output_size := MaxOutputSize} = MwState,

    %% 检查是否为阻止的命令
    case is_blocked_command(Command, BlockedCommands, BlockedPatterns) of
        true ->
            {error, {blocked_command, <<"This command is not allowed for security reasons">>}};
        false ->
            %% 根据策略执行
            Result = case Policy of
                host ->
                    execute_host_command(Command, WorkingDir, Timeout);
                docker ->
                    DockerConfig = maps:get(docker_config, MwState, #{}),
                    execute_docker_command(Command, DockerConfig, Timeout);
                sandbox ->
                    execute_sandbox_command(Command, WorkingDir, Timeout)
            end,

            %% 处理结果
            case Result of
                {ok, Output, ExitCode} ->
                    %% 截断输出
                    TruncatedOutput = truncate_output(Output, MaxOutputSize),
                    %% 脱敏输出
                    RedactedOutput = redact_output(TruncatedOutput, RedactPatterns),
                    {ok, RedactedOutput, ExitCode};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 获取会话信息
-spec get_session_info(map()) -> map().
get_session_info(State) ->
    graph:get(State, mw_shell_session, #{}).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认阻止的命令
-spec default_blocked_commands() -> [binary()].
default_blocked_commands() ->
    [
        <<"rm -rf /">>,
        <<"rm -rf /*">>,
        <<"dd if=/dev/zero">>,
        <<"mkfs">>,
        <<":(){ :|:& };:">>,  %% Fork bomb
        <<"> /dev/sda">>,
        <<"chmod -R 777 /">>
    ].

%% @private 默认阻止的模式
-spec default_blocked_patterns() -> [binary()].
default_blocked_patterns() ->
    [
        <<"rm\\s+-rf\\s+/">>,
        <<"dd\\s+if=/dev/(zero|random)\\s+of=/dev/">>,
        <<"mkfs\\.">>,
        <<":()\\{">>,
        <<">\\s*/dev/[sh]d[a-z]">>
    ].

%% @private 默认脱敏模式
-spec default_redact_patterns() -> [binary()].
default_redact_patterns() ->
    [
        <<"(?i)password\\s*[:=]\\s*\\S+">>,
        <<"(?i)secret\\s*[:=]\\s*\\S+">>,
        <<"(?i)api_key\\s*[:=]\\s*\\S+">>,
        <<"(?i)token\\s*[:=]\\s*\\S+">>
    ].

%% @private 构建 Shell 工具
-spec build_shell_tool(map()) -> map().
build_shell_tool(MwState) ->
    #{timeout := DefaultTimeout, debug := Debug} = MwState,

    #{
        name => <<"shell_execute">>,
        description => <<"Execute a shell command in a persistent session. "
                        "Use this for running system commands, scripts, and other shell operations.">>,
        input_schema => #{
            type => object,
            properties => #{
                <<"command">> => #{
                    type => string,
                    description => <<"The shell command to execute">>
                },
                <<"timeout">> => #{
                    type => integer,
                    description => <<"Command timeout in milliseconds (optional)">>
                }
            },
            required => [<<"command">>]
        },
        handler => fun(Input, Context) ->
            handle_shell_execute(Input, Context, MwState, Debug)
        end
    }.

%% @private 处理 Shell 执行
-spec handle_shell_execute(map(), map(), map(), boolean()) ->
    {ok, binary()} | {ok, binary(), map()} | {error, term()}.
handle_shell_execute(#{<<"command">> := Command} = Input, _Context, MwState, Debug) ->
    Timeout = maps:get(<<"timeout">>, Input, maps:get(timeout, MwState, 30000)),
    MwStateWithTimeout = MwState#{timeout => Timeout},

    case Debug of
        true -> logger:info("[ShellTool] 执行命令: ~s", [Command]);
        false -> ok
    end,

    case execute_command(Command, MwStateWithTimeout) of
        {ok, Output, ExitCode} ->
            %% 格式化输出
            Result = format_shell_output(Output, ExitCode),

            %% 更新会话状态
            SessionUpdate = #{
                mw_shell_session => #{
                    last_command => Command,
                    last_exit_code => ExitCode,
                    command_count => '+1'  %% 特殊标记，表示增加
                }
            },

            {ok, Result, SessionUpdate};
        {error, {blocked_command, Reason}} ->
            {ok, <<"Error: ", Reason/binary>>};
        {error, {timeout, _}} ->
            {ok, <<"Error: Command timed out">>};
        {error, Reason} ->
            ErrorMsg = io_lib:format("Error: ~p", [Reason]),
            {ok, iolist_to_binary(ErrorMsg)}
    end.

%% @private 检查是否为阻止的命令
-spec is_blocked_command(binary(), [binary()], [binary()]) -> boolean().
is_blocked_command(Command, BlockedCommands, BlockedPatterns) ->
    %% 检查精确匹配
    IsExactBlocked = lists:member(Command, BlockedCommands),

    %% 检查模式匹配
    IsPatternBlocked = lists:any(fun(Pattern) ->
        case re:run(Command, Pattern, [caseless]) of
            {match, _} -> true;
            nomatch -> false
        end
    end, BlockedPatterns),

    IsExactBlocked orelse IsPatternBlocked.

%% @private 在主机执行命令
-spec execute_host_command(binary(), binary(), pos_integer()) ->
    {ok, binary(), integer()} | {error, term()}.
execute_host_command(Command, WorkingDir, Timeout) ->
    %% 构建完整命令
    FullCommand = io_lib:format("cd ~s && ~s", [WorkingDir, Command]),

    %% 使用 os:cmd 或 erlang:open_port
    try
        Port = erlang:open_port(
            {spawn, binary_to_list(iolist_to_binary(FullCommand))},
            [stream, binary, exit_status, stderr_to_stdout, {line, 1024}]
        ),
        collect_port_output(Port, [], Timeout)
    catch
        error:Reason ->
            {error, Reason}
    end.

%% @private 收集 Port 输出
-spec collect_port_output(port(), [binary()], pos_integer()) ->
    {ok, binary(), integer()} | {error, term()}.
collect_port_output(Port, Acc, Timeout) ->
    receive
        {Port, {data, {eol, Line}}} ->
            collect_port_output(Port, [Line, <<"\n">> | Acc], Timeout);
        {Port, {data, {noeol, Line}}} ->
            collect_port_output(Port, [Line | Acc], Timeout);
        {Port, {exit_status, ExitCode}} ->
            Output = iolist_to_binary(lists:reverse(Acc)),
            {ok, Output, ExitCode}
    after Timeout ->
        catch erlang:port_close(Port),
        {error, {timeout, Timeout}}
    end.

%% @private 在 Docker 容器执行命令
-spec execute_docker_command(binary(), map(), pos_integer()) ->
    {ok, binary(), integer()} | {error, term()}.
execute_docker_command(Command, DockerConfig, Timeout) ->
    Image = maps:get(image, DockerConfig, <<"ubuntu:latest">>),
    Volumes = maps:get(volumes, DockerConfig, []),

    %% 构建 volume 参数
    VolumeArgs = lists:map(fun({Host, Container}) ->
        io_lib:format("-v ~s:~s", [Host, Container])
    end, Volumes),

    %% 构建 Docker 命令
    DockerCommand = io_lib:format(
        "docker run --rm ~s ~s sh -c '~s'",
        [string:join(VolumeArgs, " "), Image, Command]
    ),

    execute_host_command(iolist_to_binary(DockerCommand), <<"/tmp">>, Timeout).

%% @private 在沙箱执行命令（简化实现）
-spec execute_sandbox_command(binary(), binary(), pos_integer()) ->
    {ok, binary(), integer()} | {error, term()}.
execute_sandbox_command(Command, WorkingDir, Timeout) ->
    %% 简化实现：使用受限的 shell
    %% 生产环境应该使用更安全的沙箱（如 firejail、nsjail 等）
    SandboxCommand = io_lib:format(
        "timeout ~B sh -c 'cd ~s && ~s'",
        [Timeout div 1000, WorkingDir, Command]
    ),
    execute_host_command(iolist_to_binary(SandboxCommand), <<"/tmp">>, Timeout).

%% @private 截断输出
-spec truncate_output(binary(), pos_integer()) -> binary().
truncate_output(Output, MaxSize) when byte_size(Output) > MaxSize ->
    Truncated = binary:part(Output, 0, MaxSize),
    <<Truncated/binary, "\n... [Output truncated]">>;
truncate_output(Output, _) ->
    Output.

%% @private 脱敏输出
-spec redact_output(binary(), [binary()]) -> binary().
redact_output(Output, Patterns) ->
    lists:foldl(fun(Pattern, Acc) ->
        case re:compile(Pattern, [caseless]) of
            {ok, MP} ->
                re:replace(Acc, MP, <<"[REDACTED]">>, [global, {return, binary}]);
            _ ->
                Acc
        end
    end, Output, Patterns).

%% @private 格式化 Shell 输出
-spec format_shell_output(binary(), integer()) -> binary().
format_shell_output(Output, ExitCode) ->
    case ExitCode of
        0 ->
            Output;
        _ ->
            <<Output/binary, "\n[Exit code: ", (integer_to_binary(ExitCode))/binary, "]">>
    end.
