%%%-------------------------------------------------------------------
%%% @doc 文件系统后端单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_fs_backend_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试：后端创建
%%====================================================================

new_memory_backend_test() ->
    Backend = beamai_deepagent_fs_backend:new(#{type => memory}),
    ?assertEqual(memory, maps:get(type, Backend)),
    %% 默认 root 是 /workspace
    ?assert(is_binary(maps:get(root, Backend))).

new_memory_backend_with_root_test() ->
    Backend = beamai_deepagent_fs_backend:new(#{type => memory, root => <<"/myroot">>}),
    ?assertEqual(<<"/myroot">>, maps:get(root, Backend)).

%%====================================================================
%% 测试：内存模式文件操作
%%====================================================================

memory_write_and_read_test() ->
    Backend0 = beamai_deepagent_fs_backend:new(#{type => memory}),
    Content = <<"Hello, World!">>,

    %% 写入文件
    {ok, Backend1} = beamai_deepagent_fs_backend:write_file(<<"test.txt">>, Content, Backend0),

    %% 读取文件
    {ok, ReadContent} = beamai_deepagent_fs_backend:read_file(<<"test.txt">>, Backend1),
    ?assertEqual(Content, ReadContent).

memory_read_nonexistent_test() ->
    Backend = beamai_deepagent_fs_backend:new(#{type => memory}),
    Result = beamai_deepagent_fs_backend:read_file(<<"missing.txt">>, Backend),
    ?assertMatch({error, _}, Result).

memory_mkdir_test() ->
    Backend0 = beamai_deepagent_fs_backend:new(#{type => memory}),

    %% 创建目录
    {ok, Backend1} = beamai_deepagent_fs_backend:mkdir(<<"subdir">>, Backend0),

    %% 在子目录写入文件
    {ok, Backend2} = beamai_deepagent_fs_backend:write_file(<<"subdir/file.txt">>, <<"content">>, Backend1),

    %% 列出目录
    {ok, Entries} = beamai_deepagent_fs_backend:ls(<<"subdir">>, Backend2),
    ?assertEqual(1, length(Entries)),
    ?assertEqual(<<"file.txt">>, maps:get(name, hd(Entries))).

memory_write_creates_parent_dirs_test() ->
    Backend0 = beamai_deepagent_fs_backend:new(#{type => memory}),

    %% 写入嵌套路径的文件（应自动创建父目录）
    {ok, Backend1} = beamai_deepagent_fs_backend:write_file(<<"a/b/c.txt">>, <<"nested">>, Backend0),

    %% 读取文件
    {ok, Content} = beamai_deepagent_fs_backend:read_file(<<"a/b/c.txt">>, Backend1),
    ?assertEqual(<<"nested">>, Content).

%%====================================================================
%% 测试：Glob 模式匹配
%%====================================================================

memory_glob_test() ->
    Backend0 = beamai_deepagent_fs_backend:new(#{type => memory}),
    {ok, Backend1} = beamai_deepagent_fs_backend:write_file(<<"a.txt">>, <<"a">>, Backend0),
    {ok, Backend2} = beamai_deepagent_fs_backend:write_file(<<"b.txt">>, <<"b">>, Backend1),
    {ok, Backend3} = beamai_deepagent_fs_backend:write_file(<<"c.md">>, <<"c">>, Backend2),

    %% 匹配 .txt 文件
    {ok, Matches} = beamai_deepagent_fs_backend:glob(<<"*.txt">>, Backend3),
    ?assertEqual(2, length(Matches)).

%%====================================================================
%% 测试：文件存在检查
%%====================================================================

memory_exists_test() ->
    Backend0 = beamai_deepagent_fs_backend:new(#{type => memory}),
    {ok, Backend1} = beamai_deepagent_fs_backend:write_file(<<"test.txt">>, <<"Hello">>, Backend0),

    ?assertEqual(true, beamai_deepagent_fs_backend:exists(<<"test.txt">>, Backend1)),
    ?assertEqual(false, beamai_deepagent_fs_backend:exists(<<"missing.txt">>, Backend1)).

memory_is_dir_test() ->
    Backend0 = beamai_deepagent_fs_backend:new(#{type => memory}),
    {ok, Backend1} = beamai_deepagent_fs_backend:mkdir(<<"mydir">>, Backend0),
    {ok, Backend2} = beamai_deepagent_fs_backend:write_file(<<"file.txt">>, <<"content">>, Backend1),

    ?assertEqual(true, beamai_deepagent_fs_backend:is_dir(<<"mydir">>, Backend2)),
    ?assertEqual(false, beamai_deepagent_fs_backend:is_dir(<<"file.txt">>, Backend2)).
