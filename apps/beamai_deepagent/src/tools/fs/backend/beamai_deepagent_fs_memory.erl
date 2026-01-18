%%%-------------------------------------------------------------------
%%% @doc 内存文件系统后端
%%%
%%% 基于 Map/Sets 的内存文件系统实现，用于：
%%% - 单元测试
%%% - 临时任务
%%% - 隔离环境
%%%
%%% 特点：
%%% - 不依赖真实文件系统
%%% - 状态完全在内存中
%%% - 支持沙箱路径限制
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_fs_memory).

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

%% @doc 创建内存后端
-spec new(binary()) -> map().
new(Root) ->
    NormalizedRoot = normalize_root(Root),
    #{
        type => memory,
        root => NormalizedRoot,
        state => #{
            files => #{},
            dirs => sets:from_list([NormalizedRoot])
        }
    }.

%%====================================================================
%% 文件操作
%%====================================================================

%% @doc 列出目录内容
-spec ls(binary(), map()) -> {ok, [map()]} | {error, term()}.
ls(Path, #{root := Root, state := State}) ->
    FullPath = normalize_path(Path, Root),
    case is_safe_path(FullPath, Root) of
        false -> {error, {access_denied, Path}};
        true ->
            Files = maps:get(files, State, #{}),
            Dirs = maps:get(dirs, State, sets:new()),
            {ok, collect_entries(FullPath, Files, Dirs)}
    end.

%% @doc 读取文件
-spec read_file(binary(), map()) -> {ok, binary()} | {error, term()}.
read_file(Path, #{root := Root, state := State}) ->
    FullPath = normalize_path(Path, Root),
    case is_safe_path(FullPath, Root) of
        false -> {error, {access_denied, Path}};
        true ->
            Files = maps:get(files, State, #{}),
            case maps:find(FullPath, Files) of
                {ok, Content} -> {ok, Content};
                error -> {error, {enoent, Path}}
            end
    end.

%% @doc 写入文件
-spec write_file(binary(), binary(), map()) -> {ok, map()} | {error, term()}.
write_file(Path, Content, #{root := Root, state := State} = Backend) ->
    FullPath = normalize_path(Path, Root),
    case is_safe_path(FullPath, Root) of
        false -> {error, {access_denied, Path}};
        true ->
            Files = maps:get(files, State, #{}),
            NewFiles = maps:put(FullPath, Content, Files),
            %% 确保父目录存在
            ParentDir = filename:dirname(FullPath),
            Dirs = maps:get(dirs, State, sets:new()),
            NewDirs = ensure_parent_dirs(ParentDir, Root, Dirs),
            NewState = State#{files => NewFiles, dirs => NewDirs},
            {ok, Backend#{state => NewState}}
    end.

%% @doc 删除文件
-spec delete_file(binary(), map()) -> {ok, map()} | {error, term()}.
delete_file(Path, #{root := Root, state := State} = Backend) ->
    FullPath = normalize_path(Path, Root),
    case is_safe_path(FullPath, Root) of
        false -> {error, {access_denied, Path}};
        true ->
            Files = maps:get(files, State, #{}),
            NewFiles = maps:remove(FullPath, Files),
            NewState = State#{files => NewFiles},
            {ok, Backend#{state => NewState}}
    end.

%%====================================================================
%% 目录操作
%%====================================================================

%% @doc 创建目录
-spec mkdir(binary(), map()) -> {ok, map()} | {error, term()}.
mkdir(Path, #{root := Root, state := State} = Backend) ->
    FullPath = normalize_path(Path, Root),
    case is_safe_path(FullPath, Root) of
        false -> {error, {access_denied, Path}};
        true ->
            Dirs = maps:get(dirs, State, sets:new()),
            NewDirs = ensure_parent_dirs(FullPath, Root, Dirs),
            NewState = State#{dirs => NewDirs},
            {ok, Backend#{state => NewState}}
    end.

%% @doc 检查是否存在
-spec exists(binary(), map()) -> boolean().
exists(Path, #{root := Root, state := State}) ->
    FullPath = normalize_path(Path, Root),
    Files = maps:get(files, State, #{}),
    Dirs = maps:get(dirs, State, sets:new()),
    maps:is_key(FullPath, Files) orelse sets:is_element(FullPath, Dirs).

%% @doc 检查是否为目录
-spec is_dir(binary(), map()) -> boolean().
is_dir(Path, #{root := Root, state := State}) ->
    FullPath = normalize_path(Path, Root),
    Dirs = maps:get(dirs, State, sets:new()),
    sets:is_element(FullPath, Dirs).

%%====================================================================
%% 模式匹配
%%====================================================================

%% @doc 模式匹配文件
-spec glob(binary(), map()) -> {ok, [binary()]} | {error, term()}.
glob(Pattern, #{root := Root, state := State}) ->
    FullPattern = normalize_path(Pattern, Root),
    Files = maps:get(files, State, #{}),
    Regex = glob_to_regex(FullPattern),
    Matches = [strip_root(P, Root) || P <- maps:keys(Files),
               re:run(P, Regex, [{capture, none}]) =:= match],
    {ok, Matches}.

%%====================================================================
%% 私有函数 - 辅助
%%====================================================================

%% @private 规范化根路径（移除尾部斜杠）
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

%% @private 收集目录条目
-spec collect_entries(binary(), map(), sets:set()) -> [map()].
collect_entries(DirPath, Files, Dirs) ->
    DirPathSlash = <<DirPath/binary, "/">>,
    DirLen = byte_size(DirPathSlash),

    %% 收集文件
    FileEntries = maps:fold(fun(Path, _Content, Acc) ->
        case binary:match(Path, DirPathSlash) of
            {0, DirLen} ->
                Rest = binary:part(Path, DirLen, byte_size(Path) - DirLen),
                case binary:match(Rest, <<"/">>) of
                    nomatch -> [#{name => Rest, type => file} | Acc];
                    _ -> Acc
                end;
            _ -> Acc
        end
    end, [], Files),

    %% 收集子目录
    DirEntries = sets:fold(fun(Path, Acc) ->
        case binary:match(Path, DirPathSlash) of
            {0, DirLen} ->
                Rest = binary:part(Path, DirLen, byte_size(Path) - DirLen),
                case binary:match(Rest, <<"/">>) of
                    nomatch when Rest =/= <<>> ->
                        [#{name => Rest, type => directory} | Acc];
                    _ -> Acc
                end;
            _ -> Acc
        end
    end, [], Dirs),

    FileEntries ++ DirEntries.

%% @private 确保父目录存在
-spec ensure_parent_dirs(binary(), binary(), sets:set()) -> sets:set().
ensure_parent_dirs(Path, Root, Dirs) ->
    case Path of
        Root -> Dirs;
        _ ->
            NewDirs = sets:add_element(Path, Dirs),
            Parent = filename:dirname(Path),
            ensure_parent_dirs(Parent, Root, NewDirs)
    end.

%% @private 将 glob 模式转换为正则表达式
-spec glob_to_regex(binary()) -> binary().
glob_to_regex(Pattern) ->
    PatternStr = binary_to_list(Pattern),
    Escaped = re:replace(PatternStr, "[.^$+{}\\[\\]\\\\|()]", "\\\\&",
                         [global, {return, list}]),
    Converted = lists:flatten(
        string:replace(
            string:replace(Escaped, "*", ".*", all),
            "?", ".", all)),
    list_to_binary(["^", Converted, "$"]).
