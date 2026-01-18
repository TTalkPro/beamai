%%%-------------------------------------------------------------------
%%% @doc 工具安全控制模块
%%%
%%% 提供工具执行的安全检查函数（无状态）：
%%% - 路径检查
%%% - 命令检查
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_security).

%% API
-export([
    check_path/1,
    check_path/2,
    check_command/1,
    check_command/2,
    default_config/0
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type security_config() :: #{
    %% 文件安全
    allowed_paths => [binary()],
    blocked_paths => [binary()],

    %% Shell 安全
    blocked_commands => [binary()]
}.

-export_type([security_config/0]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 默认安全配置
-spec default_config() -> security_config().
default_config() ->
    #{
        allowed_paths => [],
        blocked_paths => [<<".git">>, <<"node_modules">>, <<".env">>],
        blocked_commands => [
            <<"rm -rf /">>,
            <<":(){ :|:& };:">>,
            <<"mkfs">>,
            <<"dd if=/dev/zero">>
        ]
    }.

%% @doc 检查路径是否允许访问（使用默认配置）
-spec check_path(binary()) -> ok | {error, term()}.
check_path(Path) ->
    check_path(Path, default_config()).

%% @doc 检查路径是否允许访问
-spec check_path(binary(), security_config()) -> ok | {error, term()}.
check_path(Path, Config) ->
    BlockedPaths = maps:get(blocked_paths, Config, []),
    AllowedPaths = maps:get(allowed_paths, Config, []),

    %% 检查是否在黑名单中
    case is_path_blocked(Path, BlockedPaths) of
        true ->
            {error, {path_blocked, Path}};
        false ->
            %% 如果白名单为空，允许所有非黑名单路径
            %% 如果白名单非空，必须在白名单中
            case AllowedPaths of
                [] -> ok;
                _ ->
                    case is_path_allowed(Path, AllowedPaths) of
                        true -> ok;
                        false -> {error, {path_not_allowed, Path}}
                    end
            end
    end.

%% @doc 检查命令是否允许执行（使用默认配置）
-spec check_command(binary()) -> ok | {error, term()}.
check_command(Command) ->
    check_command(Command, default_config()).

%% @doc 检查命令是否允许执行
-spec check_command(binary(), security_config()) -> ok | {error, term()}.
check_command(Command, Config) ->
    BlockedCommands = maps:get(blocked_commands, Config, []),

    case is_command_blocked(Command, BlockedCommands) of
        true ->
            {error, {command_blocked, Command}};
        false ->
            ok
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 检查路径是否在黑名单中
-spec is_path_blocked(binary(), [binary()]) -> boolean().
is_path_blocked(Path, BlockedPaths) ->
    lists:any(fun(Blocked) ->
        binary:match(Path, Blocked) =/= nomatch
    end, BlockedPaths).

%% @private 检查路径是否在白名单中
-spec is_path_allowed(binary(), [binary()]) -> boolean().
is_path_allowed(Path, AllowedPaths) ->
    lists:any(fun(Allowed) ->
        %% 路径以白名单项开头
        case binary:match(Path, Allowed) of
            {0, _} -> true;
            _ -> false
        end
    end, AllowedPaths).

%% @private 检查命令是否在黑名单中
-spec is_command_blocked(binary(), [binary()]) -> boolean().
is_command_blocked(Command, BlockedCommands) ->
    lists:any(fun(Blocked) ->
        binary:match(Command, Blocked) =/= nomatch
    end, BlockedCommands).
