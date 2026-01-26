%%%-------------------------------------------------------------------
%%% @doc 文件系统工具模块
%%%
%%% 提供完整的文件系统操作能力：
%%% - file_read: 读取文件内容（支持行范围和编码设置）
%%% - file_write: 写入文件内容（支持覆盖和追加模式）
%%% - file_glob: 按通配符模式搜索文件
%%% - file_grep: 按正则表达式搜索文件内容
%%% - file_list: 列出目录内容（支持递归）
%%% - file_mkdir: 创建目录（支持嵌套创建）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_file).

-behaviour(beamai_tool_behaviour).

-export([tool_info/0, tools/0]).

%% 工具处理器导出
-export([
    handle_read/2,
    handle_write/2,
    handle_glob/2,
    handle_grep/2,
    handle_list/2,
    handle_mkdir/2
]).

%%====================================================================
%% 工具行为回调
%%====================================================================

%% @doc 返回工具模块元信息
tool_info() ->
    #{description => <<"File system operations">>,
      tags => [<<"io">>, <<"file">>]}.

%% @doc 返回工具定义列表
tools() ->
    [
        file_read_tool(),
        file_write_tool(),
        file_glob_tool(),
        file_grep_tool(),
        file_list_tool(),
        file_mkdir_tool()
    ].

%%====================================================================
%% 工具定义
%%====================================================================

file_read_tool() ->
    #{
        name => <<"file_read">>,
        handler => fun ?MODULE:handle_read/2,
        description => <<"Read file content. Supports line range and encoding.">>,
        tag => <<"io">>,
        parameters => #{
            <<"path">> => #{type => string, description => <<"File path">>, required => true},
            <<"start_line">> => #{type => integer, description => <<"Start line (1-indexed, optional)">>},
            <<"end_line">> => #{type => integer, description => <<"End line (optional)">>},
            <<"encoding">> => #{type => string, description => <<"File encoding (default utf-8)">>}
        }
    }.

file_write_tool() ->
    #{
        name => <<"file_write">>,
        handler => fun ?MODULE:handle_write/2,
        description => <<"Write content to file. Creates if not exists, overwrites if exists.">>,
        tag => <<"io">>,
        parameters => #{
            <<"path">> => #{type => string, description => <<"File path">>, required => true},
            <<"content">> => #{type => string, description => <<"Content to write">>, required => true},
            <<"mode">> => #{type => string, description => <<"Write mode: write or append">>,
                           enum => [<<"write">>, <<"append">>]}
        }
    }.

file_glob_tool() ->
    #{
        name => <<"file_glob">>,
        handler => fun ?MODULE:handle_glob/2,
        description => <<"Search files by glob pattern. Supports ** and * wildcards.">>,
        tag => <<"search">>,
        parameters => #{
            <<"pattern">> => #{type => string, description => <<"Glob pattern, e.g. **/*.erl">>, required => true},
            <<"path">> => #{type => string, description => <<"Base search path (default .).">>},
            <<"max_results">> => #{type => integer, description => <<"Max results (default 100)">>}
        }
    }.

file_grep_tool() ->
    #{
        name => <<"file_grep">>,
        handler => fun ?MODULE:handle_grep/2,
        description => <<"Search file contents using regex.">>,
        tag => <<"search">>,
        parameters => #{
            <<"pattern">> => #{type => string, description => <<"Regex pattern">>, required => true},
            <<"path">> => #{type => string, description => <<"Search path (file or directory)">>, required => true},
            <<"file_pattern">> => #{type => string, description => <<"File name pattern (e.g. *.erl)">>},
            <<"context_lines">> => #{type => integer, description => <<"Context lines (default 0)">>},
            <<"max_results">> => #{type => integer, description => <<"Max results (default 50)">>}
        }
    }.

file_list_tool() ->
    #{
        name => <<"file_list">>,
        handler => fun ?MODULE:handle_list/2,
        description => <<"List directory contents.">>,
        tag => <<"io">>,
        parameters => #{
            <<"path">> => #{type => string, description => <<"Directory path">>},
            <<"show_hidden">> => #{type => boolean, description => <<"Show hidden files (default false)">>},
            <<"recursive">> => #{type => boolean, description => <<"Recursive listing (default false)">>}
        }
    }.

file_mkdir_tool() ->
    #{
        name => <<"file_mkdir">>,
        handler => fun ?MODULE:handle_mkdir/2,
        description => <<"Create directory. Supports nested directories.">>,
        tag => <<"io">>,
        parameters => #{
            <<"path">> => #{type => string, description => <<"Directory path">>, required => true}
        }
    }.

%%====================================================================
%% 处理器函数
%%====================================================================

handle_read(Args, _Context) ->
    Path = maps:get(<<"path">>, Args),
    StartLine = maps:get(<<"start_line">>, Args, undefined),
    EndLine = maps:get(<<"end_line">>, Args, undefined),
    case beamai_tool_security:check_path(Path) of
        ok -> do_read_file(Path, StartLine, EndLine);
        {error, Reason} -> {error, Reason}
    end.

handle_write(Args, _Context) ->
    Path = maps:get(<<"path">>, Args),
    Content = maps:get(<<"content">>, Args),
    Mode = maps:get(<<"mode">>, Args, <<"write">>),
    case beamai_tool_security:check_path(Path) of
        ok -> do_write_file(Path, Content, Mode);
        {error, Reason} -> {error, Reason}
    end.

handle_glob(Args, _Context) ->
    Pattern = maps:get(<<"pattern">>, Args),
    BasePath = maps:get(<<"path">>, Args, <<".">>),
    MaxResults = maps:get(<<"max_results">>, Args, 100),
    case beamai_tool_security:check_path(BasePath) of
        ok -> do_glob(Pattern, BasePath, MaxResults);
        {error, Reason} -> {error, Reason}
    end.

handle_grep(Args, _Context) ->
    Pattern = maps:get(<<"pattern">>, Args),
    Path = maps:get(<<"path">>, Args),
    FilePattern = maps:get(<<"file_pattern">>, Args, undefined),
    ContextLines = maps:get(<<"context_lines">>, Args, 0),
    MaxResults = maps:get(<<"max_results">>, Args, 50),
    case beamai_tool_security:check_path(Path) of
        ok -> do_grep(Pattern, Path, FilePattern, ContextLines, MaxResults);
        {error, Reason} -> {error, Reason}
    end.

handle_list(Args, _Context) ->
    Path = maps:get(<<"path">>, Args, <<".">>),
    ShowHidden = maps:get(<<"show_hidden">>, Args, false),
    Recursive = maps:get(<<"recursive">>, Args, false),
    case beamai_tool_security:check_path(Path) of
        ok -> do_list_dir(Path, ShowHidden, Recursive);
        {error, Reason} -> {error, Reason}
    end.

handle_mkdir(Args, _Context) ->
    Path = maps:get(<<"path">>, Args),
    case beamai_tool_security:check_path(Path) of
        ok -> do_mkdir(Path);
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

do_read_file(Path, StartLine, EndLine) ->
    PathStr = binary_to_list(Path),
    case file:read_file(PathStr) of
        {ok, Content} ->
            FilteredContent = filter_lines(Content, StartLine, EndLine),
            {ok, FilteredContent};
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

filter_lines(Content, undefined, undefined) -> Content;
filter_lines(Content, StartLine, EndLine) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    Start = max(1, StartLine),
    End = case EndLine of
        undefined -> length(Lines);
        E -> min(E, length(Lines))
    end,
    SelectedLines = lists:sublist(Lines, Start, End - Start + 1),
    iolist_to_binary(lists:join(<<"\n">>, SelectedLines)).

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

do_glob(Pattern, BasePath, MaxResults) ->
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

do_grep(Pattern, Path, FilePattern, _ContextLines, MaxResults) ->
    case re:compile(Pattern) of
        {ok, RE} ->
            Files = get_files_for_grep(Path, FilePattern),
            Results = grep_files(Files, RE, MaxResults),
            {ok, #{matches => Results, count => length(Results)}};
        {error, Reason} ->
            {error, {invalid_pattern, Reason}}
    end.

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

grep_files(Files, RE, MaxResults) ->
    grep_files(Files, RE, MaxResults, []).

grep_files([], _RE, _MaxResults, Acc) -> lists:reverse(Acc);
grep_files(_, _RE, MaxResults, Acc) when length(Acc) >= MaxResults -> lists:reverse(Acc);
grep_files([File | Rest], RE, MaxResults, Acc) ->
    case filelib:is_regular(File) of
        true ->
            case file:read_file(File) of
                {ok, Content} ->
                    Matches = find_matches(Content, RE, File),
                    NewAcc = Acc ++ lists:sublist(Matches, MaxResults - length(Acc)),
                    grep_files(Rest, RE, MaxResults, NewAcc);
                {error, _} ->
                    grep_files(Rest, RE, MaxResults, Acc)
            end;
        false ->
            grep_files(Rest, RE, MaxResults, Acc)
    end.

find_matches(Content, RE, File) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    find_matches_in_lines(Lines, RE, File, 1, []).

find_matches_in_lines([], _RE, _File, _LineNum, Acc) -> lists:reverse(Acc);
find_matches_in_lines([Line | Rest], RE, File, LineNum, Acc) ->
    NewAcc = case re:run(Line, RE) of
        {match, _} ->
            [#{file => list_to_binary(File), line_number => LineNum, content => Line} | Acc];
        nomatch -> Acc
    end,
    find_matches_in_lines(Rest, RE, File, LineNum + 1, NewAcc).

do_list_dir(Path, ShowHidden, Recursive) ->
    PathStr = binary_to_list(Path),
    case file:list_dir(PathStr) of
        {ok, Files} ->
            FilteredFiles = case ShowHidden of
                true -> Files;
                false -> [F || F <- Files, hd(F) =/= $.]
            end,
            Entries = build_entries(PathStr, FilteredFiles, Recursive),
            {ok, #{path => Path, entries => Entries, count => length(Entries)}};
        {error, Reason} ->
            {error, {list_dir_error, Reason}}
    end.

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
                    {error, _} -> [Entry]
                end;
            _ -> [Entry]
        end
    end, Files).

do_mkdir(Path) ->
    PathStr = binary_to_list(Path),
    case filelib:ensure_dir(filename:join(PathStr, "dummy")) of
        ok ->
            case file:make_dir(PathStr) of
                ok -> {ok, #{success => true, path => Path}};
                {error, eexist} -> {ok, #{success => true, path => Path, already_exists => true}};
                {error, Reason} -> {error, {mkdir_error, Reason}}
            end;
        {error, Reason} ->
            {error, {mkdir_error, Reason}}
    end.
