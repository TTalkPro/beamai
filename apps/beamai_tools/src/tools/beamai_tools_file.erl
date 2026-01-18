%%%-------------------------------------------------------------------
%%% @doc 文件操作工具
%%%
%%% 提供文件系统操作相关的工具：
%%% - file_read: 读取文件内容
%%% - file_write: 写入文件内容
%%% - file_glob: 按模式搜索文件
%%% - file_grep: 按内容搜索文件
%%% - file_list: 列出目录内容
%%% - file_mkdir: 创建目录
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tools_file).

-include("beamai_tools.hrl").

%% 导入 DSL 函数
-import(beamai_tool, [define/5, string/3, int/3, bool/3]).

%% API
-export([all/0]).

%% 工具处理器
-export([
    handle_read/1,
    handle_read/2,
    handle_write/1,
    handle_write/2,
    handle_glob/1,
    handle_glob/2,
    handle_grep/1,
    handle_grep/2,
    handle_list/1,
    handle_list/2,
    handle_mkdir/1,
    handle_mkdir/2
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 获取所有文件工具定义
-spec all() -> [tool_def()].
all() ->
    [
        file_read_tool(),
        file_write_tool(),
        file_glob_tool(),
        file_grep_tool(),
        file_list_tool(),
        file_mkdir_tool()
    ].

%%====================================================================
%% 工具定义（使用 DSL）
%%====================================================================

%% @private file_read 工具
file_read_tool() ->
    define(<<"file_read">>,
           <<"读取文件内容。支持指定行范围和编码。"/utf8>>,
           #{category => file, permissions => [file_read]},
           [
               {<<"path">>, string, <<"文件路径（绝对路径或相对路径）"/utf8>>, required},
               {<<"start_line">>, integer, <<"起始行号（1-indexed，可选）"/utf8>>},
               {<<"end_line">>, integer, <<"结束行号（可选）"/utf8>>},
               {<<"encoding">>, string, <<"文件编码（默认 utf-8）"/utf8>>, {default, <<"utf-8">>}}
           ],
           fun ?MODULE:handle_read/2).

%% @private file_write 工具
file_write_tool() ->
    define(<<"file_write">>,
           <<"写入内容到文件。如果文件不存在会创建，存在则覆盖。"/utf8>>,
           #{category => file, permissions => [file_write]},
           [
               {<<"path">>, string, <<"文件路径"/utf8>>, required},
               {<<"content">>, string, <<"要写入的内容"/utf8>>, required},
               {<<"mode">>, string, <<"写入模式：write（覆盖）或 append（追加）"/utf8>>,
                   [{default, <<"write">>}, {enum, [<<"write">>, <<"append">>]}]}
           ],
           fun ?MODULE:handle_write/2).

%% @private file_glob 工具
file_glob_tool() ->
    define(<<"file_glob">>,
           <<"按 glob 模式搜索文件。支持 ** 和 * 通配符。"/utf8>>,
           #{category => file, permissions => [file_read]},
           [
               {<<"pattern">>, string, <<"Glob 模式，如 **/*.erl、src/*.js"/utf8>>, required},
               {<<"path">>, string, <<"基础搜索路径（默认当前目录）"/utf8>>, {default, <<".">>}},
               {<<"max_results">>, integer, <<"最大返回结果数（默认 100）"/utf8>>, {default, 100}}
           ],
           fun ?MODULE:handle_glob/2).

%% @private file_grep 工具
file_grep_tool() ->
    define(<<"file_grep">>,
           <<"使用正则表达式搜索文件内容。"/utf8>>,
           #{category => file, permissions => [file_read]},
           [
               {<<"pattern">>, string, <<"正则表达式模式"/utf8>>, required},
               {<<"path">>, string, <<"搜索路径（文件或目录）"/utf8>>, required},
               {<<"file_pattern">>, string, <<"文件名模式（如 *.erl），可选"/utf8>>},
               {<<"context_lines">>, integer, <<"上下文行数（默认 0）"/utf8>>, {default, 0}},
               {<<"max_results">>, integer, <<"最大返回结果数（默认 50）"/utf8>>, {default, 50}}
           ],
           fun ?MODULE:handle_grep/2).

%% @private file_list 工具
file_list_tool() ->
    define(<<"file_list">>,
           <<"列出目录内容。"/utf8>>,
           #{category => file, permissions => [file_read]},
           [
               {<<"path">>, string, <<"目录路径"/utf8>>, {default, <<".">>}},
               {<<"show_hidden">>, boolean, <<"是否显示隐藏文件（默认 false）"/utf8>>, {default, false}},
               {<<"recursive">>, boolean, <<"是否递归列出（默认 false）"/utf8>>, {default, false}}
           ],
           fun ?MODULE:handle_list/2).

%% @private file_mkdir 工具
file_mkdir_tool() ->
    define(<<"file_mkdir">>,
           <<"创建目录。支持创建多级目录。"/utf8>>,
           #{category => file, permissions => [file_write]},
           [
               {<<"path">>, string, <<"目录路径"/utf8>>, required}
           ],
           fun ?MODULE:handle_mkdir/2).

%%====================================================================
%% 工具处理器
%%====================================================================

%% @doc 读取文件
handle_read(Args) ->
    handle_read(Args, #{}).

handle_read(Args, _Context) ->
    Path = maps:get(<<"path">>, Args),
    StartLine = maps:get(<<"start_line">>, Args, undefined),
    EndLine = maps:get(<<"end_line">>, Args, undefined),

    %% 安全检查
    case beamai_tool_security:check_path(Path) of
        ok ->
            do_read_file(Path, StartLine, EndLine);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 写入文件
handle_write(Args) ->
    handle_write(Args, #{}).

handle_write(Args, _Context) ->
    Path = maps:get(<<"path">>, Args),
    Content = maps:get(<<"content">>, Args),
    Mode = maps:get(<<"mode">>, Args, <<"write">>),

    %% 安全检查
    case beamai_tool_security:check_path(Path) of
        ok ->
            do_write_file(Path, Content, Mode);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Glob 搜索
handle_glob(Args) ->
    handle_glob(Args, #{}).

handle_glob(Args, _Context) ->
    Pattern = maps:get(<<"pattern">>, Args),
    BasePath = maps:get(<<"path">>, Args, <<".">>),
    MaxResults = maps:get(<<"max_results">>, Args, 100),

    case beamai_tool_security:check_path(BasePath) of
        ok ->
            do_glob(Pattern, BasePath, MaxResults);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Grep 搜索
handle_grep(Args) ->
    handle_grep(Args, #{}).

handle_grep(Args, _Context) ->
    Pattern = maps:get(<<"pattern">>, Args),
    Path = maps:get(<<"path">>, Args),
    FilePattern = maps:get(<<"file_pattern">>, Args, undefined),
    ContextLines = maps:get(<<"context_lines">>, Args, 0),
    MaxResults = maps:get(<<"max_results">>, Args, 50),

    case beamai_tool_security:check_path(Path) of
        ok ->
            do_grep(Pattern, Path, FilePattern, ContextLines, MaxResults);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 列出目录
handle_list(Args) ->
    handle_list(Args, #{}).

handle_list(Args, _Context) ->
    Path = maps:get(<<"path">>, Args, <<".">>),
    ShowHidden = maps:get(<<"show_hidden">>, Args, false),
    Recursive = maps:get(<<"recursive">>, Args, false),

    case beamai_tool_security:check_path(Path) of
        ok ->
            do_list_dir(Path, ShowHidden, Recursive);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建目录
handle_mkdir(Args) ->
    handle_mkdir(Args, #{}).

handle_mkdir(Args, _Context) ->
    Path = maps:get(<<"path">>, Args),

    case beamai_tool_security:check_path(Path) of
        ok ->
            do_mkdir(Path);
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 读取文件内容
do_read_file(Path, StartLine, EndLine) ->
    PathStr = binary_to_list(Path),
    case file:read_file(PathStr) of
        {ok, Content} ->
            FilteredContent = filter_lines(Content, StartLine, EndLine),
            {ok, FilteredContent};
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

%% @private 过滤行
filter_lines(Content, undefined, undefined) ->
    Content;
filter_lines(Content, StartLine, EndLine) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    Start = max(1, StartLine),
    End = case EndLine of
        undefined -> length(Lines);
        E -> min(E, length(Lines))
    end,
    SelectedLines = lists:sublist(Lines, Start, End - Start + 1),
    iolist_to_binary(lists:join(<<"\n">>, SelectedLines)).

%% @private 写入文件
do_write_file(Path, Content, Mode) ->
    PathStr = binary_to_list(Path),
    WriteMode = case Mode of
        <<"append">> -> [append, binary];
        _ -> [write, binary]
    end,
    case file:write_file(PathStr, Content, WriteMode) of
        ok ->
            {ok, #{success => true, path => Path, bytes_written => byte_size(Content)}};
        {error, Reason} ->
            {error, {file_write_error, Reason}}
    end.

%% @private Glob 搜索
do_glob(Pattern, BasePath, MaxResults) ->
    %% 使用 filelib:wildcard
    PatternStr = binary_to_list(Pattern),
    BasePathStr = binary_to_list(BasePath),
    FullPattern = filename:join(BasePathStr, PatternStr),
    Files = filelib:wildcard(FullPattern),
    LimitedFiles = lists:sublist(Files, MaxResults),
    {ok, #{
        files => [list_to_binary(F) || F <- LimitedFiles],
        count => length(LimitedFiles),
        truncated => length(Files) > MaxResults
    }}.

%% @private Grep 搜索
do_grep(Pattern, Path, FilePattern, ContextLines, MaxResults) ->
    %% 编译正则表达式
    case re:compile(Pattern) of
        {ok, RE} ->
            %% 获取要搜索的文件列表
            Files = get_files_for_grep(Path, FilePattern),
            %% 搜索文件
            Results = grep_files(Files, RE, ContextLines, MaxResults),
            {ok, #{
                matches => Results,
                count => length(Results)
            }};
        {error, Reason} ->
            {error, {invalid_pattern, Reason}}
    end.

%% @private 获取要搜索的文件
get_files_for_grep(Path, undefined) ->
    PathStr = binary_to_list(Path),
    case filelib:is_dir(PathStr) of
        true -> filelib:wildcard(filename:join(PathStr, "**/*"));
        false -> [PathStr]
    end;
get_files_for_grep(Path, FilePattern) ->
    PathStr = binary_to_list(Path),
    PatternStr = binary_to_list(FilePattern),
    filelib:wildcard(filename:join(PathStr, "**/" ++ PatternStr)).

%% @private 搜索文件内容
grep_files(Files, RE, ContextLines, MaxResults) ->
    grep_files(Files, RE, ContextLines, MaxResults, []).

grep_files([], _RE, _ContextLines, _MaxResults, Acc) ->
    lists:reverse(Acc);
grep_files(_, _RE, _ContextLines, MaxResults, Acc) when length(Acc) >= MaxResults ->
    lists:reverse(Acc);
grep_files([File | Rest], RE, ContextLines, MaxResults, Acc) ->
    case filelib:is_regular(File) of
        true ->
            case file:read_file(File) of
                {ok, Content} ->
                    Matches = find_matches(Content, RE, File, ContextLines),
                    NewAcc = Acc ++ lists:sublist(Matches, MaxResults - length(Acc)),
                    grep_files(Rest, RE, ContextLines, MaxResults, NewAcc);
                {error, _} ->
                    grep_files(Rest, RE, ContextLines, MaxResults, Acc)
            end;
        false ->
            grep_files(Rest, RE, ContextLines, MaxResults, Acc)
    end.

%% @private 在内容中查找匹配
find_matches(Content, RE, File, _ContextLines) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    find_matches_in_lines(Lines, RE, File, 1, []).

find_matches_in_lines([], _RE, _File, _LineNum, Acc) ->
    lists:reverse(Acc);
find_matches_in_lines([Line | Rest], RE, File, LineNum, Acc) ->
    NewAcc = case re:run(Line, RE) of
        {match, _} ->
            [#{
                file => list_to_binary(File),
                line_number => LineNum,
                content => Line
            } | Acc];
        nomatch ->
            Acc
    end,
    find_matches_in_lines(Rest, RE, File, LineNum + 1, NewAcc).

%% @private 列出目录
do_list_dir(Path, ShowHidden, Recursive) ->
    PathStr = binary_to_list(Path),
    case file:list_dir(PathStr) of
        {ok, Files} ->
            FilteredFiles = case ShowHidden of
                true -> Files;
                false -> [F || F <- Files, hd(F) =/= $.]
            end,
            Entries = build_entries(PathStr, FilteredFiles, Recursive),
            {ok, #{
                path => Path,
                entries => Entries,
                count => length(Entries)
            }};
        {error, Reason} ->
            {error, {list_dir_error, Reason}}
    end.

%% @private 构建目录条目
build_entries(BasePath, Files, Recursive) ->
    lists:flatmap(fun(File) ->
        FullPath = filename:join(BasePath, File),
        IsDir = filelib:is_dir(FullPath),
        Entry = #{
            name => list_to_binary(File),
            path => list_to_binary(FullPath),
            type => case IsDir of true -> directory; false -> file end
        },
        case {IsDir, Recursive} of
            {true, true} ->
                case file:list_dir(FullPath) of
                    {ok, SubFiles} ->
                        SubEntries = build_entries(FullPath, SubFiles, true),
                        [Entry#{children => SubEntries}];
                    {error, _} ->
                        [Entry]
                end;
            _ ->
                [Entry]
        end
    end, Files).

%% @private 创建目录
do_mkdir(Path) ->
    PathStr = binary_to_list(Path),
    case filelib:ensure_dir(filename:join(PathStr, "dummy")) of
        ok ->
            case file:make_dir(PathStr) of
                ok ->
                    {ok, #{success => true, path => Path}};
                {error, eexist} ->
                    {ok, #{success => true, path => Path, already_exists => true}};
                {error, Reason} ->
                    {error, {mkdir_error, Reason}}
            end;
        {error, Reason} ->
            {error, {mkdir_error, Reason}}
    end.
