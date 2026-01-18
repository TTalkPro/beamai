%%%-------------------------------------------------------------------
%%% @doc 文件系统后端统一接口
%%%
%%% 提供可插拔的文件系统抽象，路由到具体实现：
%%% - memory: 内存模式，用于测试和临时任务
%%% - filesystem: 真实文件系统模式
%%%
%%% 所有路径操作都限制在 root 目录内（沙箱安全）。
%%%
%%% 使用示例：
%%% ```
%%% Backend = beamai_deepagent_fs_backend:new(#{type => memory}),
%%% {ok, Backend1} = beamai_deepagent_fs_backend:write_file(<<"test.txt">>, <<"Hello">>, Backend),
%%% {ok, Content} = beamai_deepagent_fs_backend:read_file(<<"test.txt">>, Backend1).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_fs_backend).

%%====================================================================
%% 导出 API
%%====================================================================

%% 创建后端
-export([new/1, new/2]).

%% 文件操作
-export([ls/2, read_file/2, write_file/3, delete_file/2]).

%% 目录操作
-export([mkdir/2, exists/2, is_dir/2]).

%% 模式匹配
-export([glob/2]).

%% 路径工具（供其他模块使用）
-export([normalize_path/2, is_safe_path/2]).

%%====================================================================
%% 类型定义
%%====================================================================

-type backend_type() :: memory | filesystem.

-type backend() :: #{
    type := backend_type(),
    root := binary(),
    state := map()
}.

-type entry() :: #{
    name := binary(),
    type := file | directory,
    size => non_neg_integer()
}.

-export_type([backend/0, backend_type/0, entry/0]).

%%====================================================================
%% 创建后端
%%====================================================================

%% @doc 从配置创建后端
-spec new(map()) -> backend().
new(Opts) ->
    Type = maps:get(type, Opts, memory),
    Root = maps:get(root, Opts, get_default_root()),
    new(Type, Root).

%% @private 获取默认根目录（当前工作目录）
-spec get_default_root() -> binary().
get_default_root() ->
    case file:get_cwd() of
        {ok, Cwd} -> list_to_binary(Cwd);
        {error, _} -> <<".">>
    end.

%% @doc 创建指定类型的后端
-spec new(backend_type(), binary()) -> backend().
new(memory, Root) ->
    beamai_deepagent_fs_memory:new(Root);
new(filesystem, Root) ->
    beamai_deepagent_fs_disk:new(Root).

%%====================================================================
%% 文件操作（路由到具体实现）
%%====================================================================

%% @doc 列出目录内容
-spec ls(binary(), backend()) -> {ok, [entry()]} | {error, term()}.
ls(Path, #{type := memory} = B) -> beamai_deepagent_fs_memory:ls(Path, B);
ls(Path, #{type := filesystem} = B) -> beamai_deepagent_fs_disk:ls(Path, B).

%% @doc 读取文件内容
-spec read_file(binary(), backend()) -> {ok, binary()} | {error, term()}.
read_file(Path, #{type := memory} = B) -> beamai_deepagent_fs_memory:read_file(Path, B);
read_file(Path, #{type := filesystem} = B) -> beamai_deepagent_fs_disk:read_file(Path, B).

%% @doc 写入文件
-spec write_file(binary(), binary(), backend()) -> {ok, backend()} | {error, term()}.
write_file(Path, Content, #{type := memory} = B) ->
    beamai_deepagent_fs_memory:write_file(Path, Content, B);
write_file(Path, Content, #{type := filesystem} = B) ->
    beamai_deepagent_fs_disk:write_file(Path, Content, B).

%% @doc 删除文件
-spec delete_file(binary(), backend()) -> {ok, backend()} | {error, term()}.
delete_file(Path, #{type := memory} = B) -> beamai_deepagent_fs_memory:delete_file(Path, B);
delete_file(Path, #{type := filesystem} = B) -> beamai_deepagent_fs_disk:delete_file(Path, B).

%%====================================================================
%% 目录操作
%%====================================================================

%% @doc 创建目录
-spec mkdir(binary(), backend()) -> {ok, backend()} | {error, term()}.
mkdir(Path, #{type := memory} = B) -> beamai_deepagent_fs_memory:mkdir(Path, B);
mkdir(Path, #{type := filesystem} = B) -> beamai_deepagent_fs_disk:mkdir(Path, B).

%% @doc 检查路径是否存在
-spec exists(binary(), backend()) -> boolean().
exists(Path, #{type := memory} = B) -> beamai_deepagent_fs_memory:exists(Path, B);
exists(Path, #{type := filesystem} = B) -> beamai_deepagent_fs_disk:exists(Path, B).

%% @doc 检查是否为目录
-spec is_dir(binary(), backend()) -> boolean().
is_dir(Path, #{type := memory} = B) -> beamai_deepagent_fs_memory:is_dir(Path, B);
is_dir(Path, #{type := filesystem} = B) -> beamai_deepagent_fs_disk:is_dir(Path, B).

%%====================================================================
%% 模式匹配
%%====================================================================

%% @doc 模式匹配文件
-spec glob(binary(), backend()) -> {ok, [binary()]} | {error, term()}.
glob(Pattern, #{type := memory} = B) -> beamai_deepagent_fs_memory:glob(Pattern, B);
glob(Pattern, #{type := filesystem} = B) -> beamai_deepagent_fs_disk:glob(Pattern, B).

%%====================================================================
%% 路径工具
%%====================================================================

%% @doc 规范化路径（相对于根目录）
-spec normalize_path(binary(), binary()) -> binary().
normalize_path(<<"/">>, Root) -> Root;
normalize_path(<<"/", _/binary>> = AbsPath, Root) -> <<Root/binary, AbsPath/binary>>;
normalize_path(RelPath, Root) -> <<Root/binary, "/", RelPath/binary>>.

%% @doc 检查路径是否安全（在沙箱内）
-spec is_safe_path(binary(), binary()) -> boolean().
is_safe_path(Path, Root) ->
    case binary:match(Path, <<"..">>) of
        nomatch ->
            case binary:match(Path, Root) of
                {0, _} -> true;
                _ -> false
            end;
        _ -> false
    end.
