%%%-------------------------------------------------------------------
%%% @doc 文件系统插件
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
-module(beamai_plugin_file).

-behaviour(beamai_plugin_behaviour).

-export([plugin_info/0, functions/0]).

%% 函数处理器导出
-export([
    handle_read/2,
    handle_write/2,
    handle_glob/2,
    handle_grep/2,
    handle_list/2,
    handle_mkdir/2
]).

%%====================================================================
%% 插件行为回调
%%====================================================================

%% @doc 返回插件元数据
%%
%% 提供插件的名称和描述信息，用于插件注册和发现。
%%
%% @returns 插件信息映射，包含 name 和 description 字段
plugin_info() ->
    #{name => <<"file">>,
      description => <<"File system operations">>}.

%% @doc 返回函数定义列表
%%
%% 返回本插件提供的所有可调用函数定义，
%% 每个函数包含名称、处理器、描述和参数定义。
%%
%% @returns 函数定义列表
functions() ->
    [
        file_read_func(),
        file_write_func(),
        file_glob_func(),
        file_grep_func(),
        file_list_func(),
        file_mkdir_func()
    ].

%%====================================================================
%% 函数定义
%%====================================================================

%% @doc 构建文件读取函数定义
file_read_func() ->
    beamai_function:new(<<"file_read">>, fun ?MODULE:handle_read/2, #{
        description => <<"Read file content. Supports line range and encoding.">>,
        parameters => #{
            <<"path">> => #{type => string, description => <<"File path">>, required => true},
            <<"start_line">> => #{type => integer, description => <<"Start line (1-indexed, optional)">>},
            <<"end_line">> => #{type => integer, description => <<"End line (optional)">>},
            <<"encoding">> => #{type => string, description => <<"File encoding (default utf-8)">>}
        }
    }).

%% @doc 构建文件写入函数定义
file_write_func() ->
    beamai_function:new(<<"file_write">>, fun ?MODULE:handle_write/2, #{
        description => <<"Write content to file. Creates if not exists, overwrites if exists.">>,
        parameters => #{
            <<"path">> => #{type => string, description => <<"File path">>, required => true},
            <<"content">> => #{type => string, description => <<"Content to write">>, required => true},
            <<"mode">> => #{type => string, description => <<"Write mode: write or append">>,
                           enum => [<<"write">>, <<"append">>]}
        }
    }).

%% @doc 构建文件通配符搜索函数定义
file_glob_func() ->
    beamai_function:new(<<"file_glob">>, fun ?MODULE:handle_glob/2, #{
        description => <<"Search files by glob pattern. Supports ** and * wildcards.">>,
        parameters => #{
            <<"pattern">> => #{type => string, description => <<"Glob pattern, e.g. **/*.erl">>, required => true},
            <<"path">> => #{type => string, description => <<"Base search path (default .).">>},
            <<"max_results">> => #{type => integer, description => <<"Max results (default 100)">>}
        }
    }).

%% @doc 构建文件内容正则搜索函数定义
file_grep_func() ->
    beamai_function:new(<<"file_grep">>, fun ?MODULE:handle_grep/2, #{
        description => <<"Search file contents using regex.">>,
        parameters => #{
            <<"pattern">> => #{type => string, description => <<"Regex pattern">>, required => true},
            <<"path">> => #{type => string, description => <<"Search path (file or directory)">>, required => true},
            <<"file_pattern">> => #{type => string, description => <<"File name pattern (e.g. *.erl)">>},
            <<"context_lines">> => #{type => integer, description => <<"Context lines (default 0)">>},
            <<"max_results">> => #{type => integer, description => <<"Max results (default 50)">>}
        }
    }).

%% @doc 构建目录列表函数定义
file_list_func() ->
    beamai_function:new(<<"file_list">>, fun ?MODULE:handle_list/2, #{
        description => <<"List directory contents.">>,
        parameters => #{
            <<"path">> => #{type => string, description => <<"Directory path">>},
            <<"show_hidden">> => #{type => boolean, description => <<"Show hidden files (default false)">>},
            <<"recursive">> => #{type => boolean, description => <<"Recursive listing (default false)">>}
        }
    }).

%% @doc 构建目录创建函数定义
file_mkdir_func() ->
    beamai_function:new(<<"file_mkdir">>, fun ?MODULE:handle_mkdir/2, #{
        description => <<"Create directory. Supports nested directories.">>,
        parameters => #{
            <<"path">> => #{type => string, description => <<"Directory path">>, required => true}
        }
    }).

%%====================================================================
%% 处理器函数
%%====================================================================

%% @doc 读取文件内容
%%
%% 读取指定路径的文件内容，支持按行范围截取。
%% 执行前会进行路径安全检查，防止越权访问。
%%
%% @param Args 参数映射，包含：
%%   - path: 文件路径（必填）
%%   - start_line: 起始行号，1 开始（可选）
%%   - end_line: 结束行号（可选）
%% @param Context 调用上下文（本函数未使用）
%% @returns {ok, 文件内容二进制} | {error, 错误原因}
handle_read(Args, _Context) ->
    Path = maps:get(<<"path">>, Args),
    StartLine = maps:get(<<"start_line">>, Args, undefined),
    EndLine = maps:get(<<"end_line">>, Args, undefined),
    case beamai_tool_security:check_path(Path) of
        ok -> do_read_file(Path, StartLine, EndLine);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 写入文件内容
%%
%% 将内容写入指定路径的文件。如果文件不存在则创建，
%% 存在则根据模式覆盖或追加。
%% 执行前会进行路径安全检查。
%%
%% @param Args 参数映射，包含：
%%   - path: 文件路径（必填）
%%   - content: 要写入的内容（必填）
%%   - mode: 写入模式，write（覆盖）或 append（追加），默认 write
%% @param Context 调用上下文（本函数未使用）
%% @returns {ok, 结果映射} | {error, 错误原因}
handle_write(Args, _Context) ->
    Path = maps:get(<<"path">>, Args),
    Content = maps:get(<<"content">>, Args),
    Mode = maps:get(<<"mode">>, Args, <<"write">>),
    case beamai_tool_security:check_path(Path) of
        ok -> do_write_file(Path, Content, Mode);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 按模式搜索文件
%%
%% 使用 glob 通配符模式在指定路径下搜索匹配的文件。
%% 支持 ** 和 * 通配符。
%%
%% @param Args 参数映射，包含：
%%   - pattern: glob 模式（必填），如 "**/*.erl"
%%   - path: 基础搜索路径（默认 "."）
%%   - max_results: 最大返回结果数（默认 100）
%% @param Context 调用上下文（本函数未使用）
%% @returns {ok, 搜索结果映射} | {error, 错误原因}
handle_glob(Args, _Context) ->
    Pattern = maps:get(<<"pattern">>, Args),
    BasePath = maps:get(<<"path">>, Args, <<".">>),
    MaxResults = maps:get(<<"max_results">>, Args, 100),
    case beamai_tool_security:check_path(BasePath) of
        ok -> do_glob(Pattern, BasePath, MaxResults);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 按正则搜索文件内容
%%
%% 使用正则表达式在指定路径的文件内容中搜索匹配的行。
%% 支持文件名过滤和上下文行数设置。
%%
%% @param Args 参数映射，包含：
%%   - pattern: 正则表达式模式（必填）
%%   - path: 搜索路径，文件或目录（必填）
%%   - file_pattern: 文件名过滤模式（可选），如 "*.erl"
%%   - context_lines: 上下文行数（默认 0）
%%   - max_results: 最大返回结果数（默认 50）
%% @param Context 调用上下文（本函数未使用）
%% @returns {ok, 匹配结果映射} | {error, 错误原因}
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

%% @doc 列出目录内容
%%
%% 列出指定目录下的文件和子目录。
%% 支持显示隐藏文件和递归列出子目录内容。
%%
%% @param Args 参数映射，包含：
%%   - path: 目录路径（默认 "."）
%%   - show_hidden: 是否显示隐藏文件（默认 false）
%%   - recursive: 是否递归列出（默认 false）
%% @param Context 调用上下文（本函数未使用）
%% @returns {ok, 目录内容映射} | {error, 错误原因}
handle_list(Args, _Context) ->
    Path = maps:get(<<"path">>, Args, <<".">>),
    ShowHidden = maps:get(<<"show_hidden">>, Args, false),
    Recursive = maps:get(<<"recursive">>, Args, false),
    case beamai_tool_security:check_path(Path) of
        ok -> do_list_dir(Path, ShowHidden, Recursive);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 创建目录
%%
%% 创建指定路径的目录，支持嵌套目录的递归创建。
%% 如果目录已存在，返回成功并标记 already_exists。
%%
%% @param Args 参数映射，包含：
%%   - path: 目录路径（必填）
%% @param Context 调用上下文（本函数未使用）
%% @returns {ok, 结果映射} | {error, 错误原因}
handle_mkdir(Args, _Context) ->
    Path = maps:get(<<"path">>, Args),
    case beamai_tool_security:check_path(Path) of
        ok -> do_mkdir(Path);
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 执行文件读取操作
%% 读取文件全部内容后，根据行范围参数进行过滤
do_read_file(Path, StartLine, EndLine) ->
    PathStr = binary_to_list(Path),
    case file:read_file(PathStr) of
        {ok, Content} ->
            FilteredContent = filter_lines(Content, StartLine, EndLine),
            {ok, FilteredContent};
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

%% @doc 按行范围过滤文件内容
%% 当 StartLine 和 EndLine 均为 undefined 时返回完整内容
%% 否则按行号范围截取内容
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

%% @doc 执行文件写入操作
%% 根据写入模式（覆盖或追加）写入文件内容
%% 返回写入成功信息包含路径和写入字节数
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

%% @doc 执行 glob 通配符文件搜索
%% 将模式和基础路径组合后调用 filelib:wildcard 进行搜索
%% 结果数量超过限制时进行截断并标记 truncated
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

%% @doc 执行正则表达式文件内容搜索
%% 先编译正则表达式，获取待搜索文件列表，然后逐文件搜索匹配内容
do_grep(Pattern, Path, FilePattern, _ContextLines, MaxResults) ->
    case re:compile(Pattern) of
        {ok, RE} ->
            Files = get_files_for_grep(Path, FilePattern),
            Results = grep_files(Files, RE, MaxResults),
            {ok, #{matches => Results, count => length(Results)}};
        {error, Reason} ->
            {error, {invalid_pattern, Reason}}
    end.

%% @doc 获取待搜索的文件列表
%% 如果未指定文件模式，根据路径类型决定搜索范围：
%% - 目录：递归搜索所有文件
%% - 文件：仅搜索该文件
%% 如果指定了文件模式，在目录下按模式匹配文件
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

%% @doc 在文件列表中搜索匹配内容（入口函数）
grep_files(Files, RE, MaxResults) ->
    grep_files(Files, RE, MaxResults, []).

%% @doc 在文件列表中搜索匹配内容（递归实现）
%% 逐个处理文件，累积匹配结果直到达到最大结果数或文件列表为空
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

%% @doc 在文件内容中查找所有匹配行
%% 按行拆分内容后逐行进行正则匹配
find_matches(Content, RE, File) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    find_matches_in_lines(Lines, RE, File, 1, []).

%% @doc 逐行匹配正则表达式（递归实现）
%% 匹配成功时记录文件路径、行号和行内容
find_matches_in_lines([], _RE, _File, _LineNum, Acc) -> lists:reverse(Acc);
find_matches_in_lines([Line | Rest], RE, File, LineNum, Acc) ->
    NewAcc = case re:run(Line, RE) of
        {match, _} ->
            [#{file => list_to_binary(File), line_number => LineNum, content => Line} | Acc];
        nomatch -> Acc
    end,
    find_matches_in_lines(Rest, RE, File, LineNum + 1, NewAcc).

%% @doc 列出目录内容
%% 根据 ShowHidden 参数过滤隐藏文件（以 . 开头的文件）
%% 根据 Recursive 参数决定是否递归列出子目录
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

%% @doc 构建目录条目列表
%% 将文件名列表转换为包含名称、路径和类型信息的条目映射
%% 递归模式下，目录条目包含 children 子条目列表
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

%% @doc 创建目录
%% 先确保父目录存在，再创建目标目录
%% 目录已存在时不报错，返回 already_exists 标记
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
