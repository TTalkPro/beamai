%%%-------------------------------------------------------------------
%%% @doc 磁盘文件系统后端
%%%
%%% 基于真实文件系统的实现，提供：
%%% - 沙箱隔离（root 目录限制）
%%% - 路径安全检查
%%% - 原子写入保证
%%%
%%% 安全特性：
%%% - 所有操作限制在 root 目录内
%%% - 防止 ../ 路径逃逸
%%% - 自动创建父目录
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_fs_disk).

-import(beamai_deepagent_fs_backend, [normalize_path/2, is_safe_path/2]).

%%====================================================================
%% 导出 API
%%====================================================================

-export([new/1]).
-export([ls/2, read_file/2, write_file/3, delete_file/2]).
-export([mkdir/2, exists/2, is_dir/2, glob/2]).

%%====================================================================
%% 创建后端
%%====================================================================

%% @doc 创建磁盘后端
-spec new(binary()) -> map().
new(Root) ->
    NormalizedRoot = normalize_root(Root),
    %% 确保根目录存在
    ok = filelib:ensure_dir(binary_to_list(<<NormalizedRoot/binary, "/.keep">>)),
    #{
        type => filesystem,
        root => NormalizedRoot,
        state => #{}
    }.

%%====================================================================
%% 文件操作
%%====================================================================

%% @doc 列出目录内容
-spec ls(binary(), map()) -> {ok, [map()]} | {error, term()}.
ls(Path, #{root := Root}) ->
    FullPath = normalize_path(Path, Root),
    case is_safe_path(FullPath, Root) of
        false -> {error, {access_denied, Path}};
        true ->
            PathStr = binary_to_list(FullPath),
            case file:list_dir(PathStr) of
                {ok, Names} ->
                    Entries = [make_entry(FullPath, list_to_binary(N)) || N <- Names],
                    {ok, Entries};
                {error, Reason} ->
                    {error, {Reason, Path}}
            end
    end.

%% @doc 读取文件
-spec read_file(binary(), map()) -> {ok, binary()} | {error, term()}.
read_file(Path, #{root := Root}) ->
    FullPath = normalize_path(Path, Root),
    case is_safe_path(FullPath, Root) of
        false -> {error, {access_denied, Path}};
        true -> file:read_file(FullPath)
    end.

%% @doc 写入文件
-spec write_file(binary(), binary(), map()) -> {ok, map()} | {error, term()}.
write_file(Path, Content, #{root := Root} = Backend) ->
    FullPath = normalize_path(Path, Root),
    case is_safe_path(FullPath, Root) of
        false -> {error, {access_denied, Path}};
        true ->
            ok = filelib:ensure_dir(binary_to_list(FullPath)),
            case file:write_file(FullPath, Content) of
                ok -> {ok, Backend};
                {error, Reason} -> {error, {Reason, Path}}
            end
    end.

%% @doc 删除文件
-spec delete_file(binary(), map()) -> {ok, map()} | {error, term()}.
delete_file(Path, #{root := Root} = Backend) ->
    FullPath = normalize_path(Path, Root),
    case is_safe_path(FullPath, Root) of
        false -> {error, {access_denied, Path}};
        true ->
            case file:delete(FullPath) of
                ok -> {ok, Backend};
                {error, Reason} -> {error, {Reason, Path}}
            end
    end.

%%====================================================================
%% 目录操作
%%====================================================================

%% @doc 创建目录
-spec mkdir(binary(), map()) -> {ok, map()} | {error, term()}.
mkdir(Path, #{root := Root} = Backend) ->
    FullPath = normalize_path(Path, Root),
    case is_safe_path(FullPath, Root) of
        false -> {error, {access_denied, Path}};
        true ->
            case filelib:ensure_dir(binary_to_list(<<FullPath/binary, "/.keep">>)) of
                ok -> {ok, Backend};
                {error, Reason} -> {error, {Reason, Path}}
            end
    end.

%% @doc 检查是否存在
-spec exists(binary(), map()) -> boolean().
exists(Path, #{root := Root}) ->
    FullPath = normalize_path(Path, Root),
    filelib:is_file(binary_to_list(FullPath)) orelse
    filelib:is_dir(binary_to_list(FullPath)).

%% @doc 检查是否为目录
-spec is_dir(binary(), map()) -> boolean().
is_dir(Path, #{root := Root}) ->
    FullPath = normalize_path(Path, Root),
    filelib:is_dir(binary_to_list(FullPath)).

%%====================================================================
%% 模式匹配
%%====================================================================

%% @doc 模式匹配文件
-spec glob(binary(), map()) -> {ok, [binary()]} | {error, term()}.
glob(Pattern, #{root := Root}) ->
    FullPattern = normalize_path(Pattern, Root),
    Matches = filelib:wildcard(binary_to_list(FullPattern)),
    SafeMatches = [list_to_binary(M) || M <- Matches,
                   is_safe_path(list_to_binary(M), Root)],
    {ok, [strip_root(M, Root) || M <- SafeMatches]}.

%%====================================================================
%% 私有函数
%%====================================================================

%% @private 规范化根路径
-spec normalize_root(binary()) -> binary().
normalize_root(Root) when is_binary(Root) ->
    case binary:last(Root) of
        $/ -> binary:part(Root, 0, byte_size(Root) - 1);
        _ -> Root
    end.

%% @private 移除根路径前缀
-spec strip_root(binary(), binary()) -> binary().
strip_root(Path, Root) ->
    RootLen = byte_size(Root),
    case Path of
        <<Root:RootLen/binary, Rest/binary>> ->
            case Rest of
                <<"/", RelPath/binary>> -> RelPath;
                <<>> -> <<".">>;
                _ -> Rest
            end;
        _ -> Path
    end.

%% @private 创建目录条目
-spec make_entry(binary(), binary()) -> map().
make_entry(Dir, Name) ->
    FullPath = <<Dir/binary, "/", Name/binary>>,
    PathStr = binary_to_list(FullPath),
    Type = case filelib:is_dir(PathStr) of
        true -> directory;
        false -> file
    end,
    Entry = #{name => Name, type => Type},
    case Type of
        file ->
            case file:read_file_info(PathStr) of
                {ok, Info} -> Entry#{size => element(2, Info)};
                _ -> Entry
            end;
        directory -> Entry
    end.
