%%%-------------------------------------------------------------------
%%% @doc 工具安全检查模块
%%%
%%% 提供无状态的工具执行安全检查功能：
%%% - 路径安全验证（白名单/黑名单机制）
%%% - 命令安全验证（黑名单机制）
%%%
%%% 用于在工具执行前进行安全校验，防止危险的文件路径访问和命令执行。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_security).

-export([
    check_path/1,
    check_path/2,
    check_command/1,
    check_command/2,
    default_config/0
]).

%% @type security_config() :: map().
%% 安全配置映射类型，包含以下字段：
%%   - allowed_paths: 允许访问的路径白名单列表
%%   - blocked_paths: 禁止访问的路径黑名单列表
%%   - blocked_commands: 禁止执行的命令黑名单列表
-type security_config() :: #{
    allowed_paths => [binary()],
    blocked_paths => [binary()],
    blocked_commands => [binary()]
}.

-export_type([security_config/0]).

%%====================================================================
%% 公共 API
%%====================================================================

%% @doc 获取默认安全配置。
%%
%% 返回包含默认黑名单规则的安全配置：
%% - 路径白名单为空（不限制可访问路径）
%% - 路径黑名单包含 .git、node_modules、.env 等敏感目录/文件
%% - 命令黑名单包含 rm -rf /、fork 炸弹、mkfs、dd 等危险命令
%%
%% @returns 默认安全配置映射
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

%% @doc 路径安全检查（使用默认配置）。
%%
%% 使用默认安全配置对指定路径进行安全检查。
%%
%% @param Path 待检查的文件路径（二进制字符串）
%% @returns ok 表示路径安全；{error, {path_blocked, Path}} 表示路径被黑名单禁止；
%%          {error, {path_not_allowed, Path}} 表示路径不在白名单中
-spec check_path(binary()) -> ok | {error, term()}.
check_path(Path) ->
    check_path(Path, default_config()).

%% @doc 路径安全检查（使用指定配置）。
%%
%% 按照以下优先级进行检查：
%% 1. 首先检查路径是否匹配黑名单中的任意模式，匹配则拒绝
%% 2. 如果白名单为空，则允许所有未被黑名单拦截的路径
%% 3. 如果白名单非空，则路径必须匹配白名单中的某个前缀才被允许
%%
%% @param Path   待检查的文件路径（二进制字符串）
%% @param Config 安全配置映射
%% @returns ok 表示路径安全；{error, Reason} 表示路径不安全
-spec check_path(binary(), security_config()) -> ok | {error, term()}.
check_path(Path, Config) ->
    BlockedPaths = maps:get(blocked_paths, Config, []),
    AllowedPaths = maps:get(allowed_paths, Config, []),
    case is_path_blocked(Path, BlockedPaths) of
        true ->
            {error, {path_blocked, Path}};
        false ->
            case AllowedPaths of
                [] -> ok;
                _ ->
                    case is_path_allowed(Path, AllowedPaths) of
                        true -> ok;
                        false -> {error, {path_not_allowed, Path}}
                    end
            end
    end.

%% @doc 命令安全检查（使用默认配置）。
%%
%% 使用默认安全配置对指定命令进行安全检查。
%%
%% @param Command 待检查的命令（二进制字符串）
%% @returns ok 表示命令安全；{error, {command_blocked, Command}} 表示命令被禁止
-spec check_command(binary()) -> ok | {error, term()}.
check_command(Command) ->
    check_command(Command, default_config()).

%% @doc 命令安全检查（使用指定配置）。
%%
%% 检查命令是否包含黑名单中的任意危险命令片段。
%% 使用子串匹配方式检测，只要命令中包含黑名单中的任意模式即被拒绝。
%%
%% @param Command 待检查的命令（二进制字符串）
%% @param Config  安全配置映射
%% @returns ok 表示命令安全；{error, {command_blocked, Command}} 表示命令被禁止
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
%% 内部辅助函数
%%====================================================================

%% @private
%% 检查路径是否匹配黑名单中的任意模式
%% 使用 binary:match 进行子串匹配，路径中包含黑名单字符串即视为被禁止
is_path_blocked(Path, BlockedPaths) ->
    lists:any(fun(Blocked) ->
        binary:match(Path, Blocked) =/= nomatch
    end, BlockedPaths).

%% @private
%% 检查路径是否匹配白名单中的任意前缀
%% 使用 binary:match 检查匹配位置，只有从位置 0 开始匹配才视为允许
%% 即白名单路径必须是目标路径的前缀
is_path_allowed(Path, AllowedPaths) ->
    lists:any(fun(Allowed) ->
        case binary:match(Path, Allowed) of
            {0, _} -> true;
            _ -> false
        end
    end, AllowedPaths).

%% @private
%% 检查命令是否匹配黑名单中的任意危险命令片段
%% 使用 binary:match 进行子串匹配，命令中包含黑名单字符串即视为被禁止
is_command_blocked(Command, BlockedCommands) ->
    lists:any(fun(Blocked) ->
        binary:match(Command, Blocked) =/= nomatch
    end, BlockedCommands).
