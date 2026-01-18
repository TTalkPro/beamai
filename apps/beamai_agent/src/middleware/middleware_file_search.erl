%%%-------------------------------------------------------------------
%%% @doc File Search Middleware - 文件搜索工具
%%%
%%% 为 Agent 提供文件系统搜索能力，支持 glob 和 grep 搜索。
%%%
%%% == 功能特性 ==
%%%
%%% - Glob 模式文件搜索
%%% - Grep 内容搜索（支持正则）
%%% - 文件大小限制
%%% - 搜索路径限制
%%% - 结果数量限制
%%%
%%% == 配置示例 ==
%%%
%%% ```erlang
%%% {middleware_file_search, #{
%%%     %% 允许搜索的根目录
%%%     allowed_paths => [<<"/app">>, <<"/home/user/project">>],
%%%
%%%     %% 禁止搜索的目录
%%%     excluded_paths => [<<".git">>, <<"node_modules">>, <<"_build">>],
%%%
%%%     %% 最大文件大小（字节，用于 grep）
%%%     max_file_size => 1048576,  %% 1MB
%%%
%%%     %% 最大结果数量
%%%     max_results => 100,
%%%
%%%     %% 最大搜索深度
%%%     max_depth => 10,
%%%
%%%     %% 调试模式
%%%     debug => false
%%% }}
%%% ```
%%%
%%% == 注入的工具 ==
%%%
%%% file_glob - 按模式搜索文件
%%% ```
%%% Input: {
%%%     "pattern": "**/*.erl",
%%%     "path": "/app/src"  // 可选
%%% }
%%% ```
%%%
%%% file_grep - 搜索文件内容
%%% ```
%%% Input: {
%%%     "pattern": "function\\s+\\w+",
%%%     "path": "/app/src",
%%%     "file_pattern": "*.erl",  // 可选
%%%     "context_lines": 2  // 可选，上下文行数
%%% }
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_file_search).

-behaviour(beamai_middleware).

%% 需要引入 file_info record
-include_lib("kernel/include/file.hrl").

%% Middleware 回调
-export([init/1, before_agent/2]).

%% 工具函数
-export([
    glob_search/3,
    grep_search/4,
    read_file/2
]).

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
-spec init(map()) -> map().
init(Opts) ->
    #{
        allowed_paths => maps:get(allowed_paths, Opts, [<<".">>]),
        excluded_paths => maps:get(excluded_paths, Opts, default_excluded_paths()),
        max_file_size => maps:get(max_file_size, Opts, 1048576),  %% 1MB
        max_results => maps:get(max_results, Opts, 100),
        max_depth => maps:get(max_depth, Opts, 10),
        max_context_lines => maps:get(max_context_lines, Opts, 5),
        debug => maps:get(debug, Opts, false)
    }.

%% @doc Agent 开始时注入搜索工具
-spec before_agent(map(), map()) -> beamai_middleware:middleware_result().
before_agent(State, MwState) ->
    %% 注入搜索工具
    Tools = graph:get(State, tools, []),
    GlobTool = build_glob_tool(MwState),
    GrepTool = build_grep_tool(MwState),
    ReadTool = build_read_tool(MwState),

    NewTools = [GlobTool, GrepTool, ReadTool | Tools],

    {update, #{tools => NewTools}}.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc Glob 模式搜索
-spec glob_search(binary(), binary(), map()) -> {ok, [binary()]} | {error, term()}.
glob_search(Pattern, BasePath, MwState) ->
    #{allowed_paths := AllowedPaths,
      excluded_paths := ExcludedPaths,
      max_results := MaxResults,
      max_depth := MaxDepth} = MwState,

    %% 验证路径
    case is_path_allowed(BasePath, AllowedPaths) of
        false ->
            {error, {path_not_allowed, BasePath}};
        true ->
            %% 执行 glob 搜索
            Results = do_glob_search(Pattern, BasePath, ExcludedPaths, MaxDepth, MaxResults),
            {ok, Results}
    end.

%% @doc Grep 内容搜索
-spec grep_search(binary(), binary(), binary() | undefined, map()) ->
    {ok, [map()]} | {error, term()}.
grep_search(Pattern, BasePath, FilePattern, MwState) ->
    #{allowed_paths := AllowedPaths,
      excluded_paths := ExcludedPaths,
      max_file_size := MaxFileSize,
      max_results := MaxResults,
      max_context_lines := MaxContextLines} = MwState,

    %% 验证路径
    case is_path_allowed(BasePath, AllowedPaths) of
        false ->
            {error, {path_not_allowed, BasePath}};
        true ->
            %% 首先找到要搜索的文件
            GlobPattern = case FilePattern of
                undefined -> <<"**/*">>;
                _ -> FilePattern
            end,
            Files = do_glob_search(GlobPattern, BasePath, ExcludedPaths, 10, 1000),

            %% 在文件中搜索
            Results = do_grep_search(Pattern, Files, MaxFileSize, MaxResults, MaxContextLines),
            {ok, Results}
    end.

%% @doc 读取文件内容
-spec read_file(binary(), map()) -> {ok, binary()} | {error, term()}.
read_file(FilePath, MwState) ->
    #{allowed_paths := AllowedPaths, max_file_size := MaxFileSize} = MwState,

    case is_path_allowed(FilePath, AllowedPaths) of
        false ->
            {error, {path_not_allowed, FilePath}};
        true ->
            case file:read_file_info(FilePath) of
                {ok, #file_info{size = Size}} when Size > MaxFileSize ->
                    {error, {file_too_large, Size, MaxFileSize}};
                {ok, _} ->
                    file:read_file(FilePath);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认排除路径
-spec default_excluded_paths() -> [binary()].
default_excluded_paths() ->
    [
        <<".git">>,
        <<".svn">>,
        <<".hg">>,
        <<"node_modules">>,
        <<"_build">>,
        <<"deps">>,
        <<"vendor">>,
        <<"__pycache__">>,
        <<".cache">>,
        <<".npm">>,
        <<"target">>,
        <<"dist">>,
        <<"build">>
    ].

%% @private 构建 Glob 工具
-spec build_glob_tool(map()) -> map().
build_glob_tool(MwState) ->
    #{debug := Debug} = MwState,

    #{
        name => <<"file_glob">>,
        description => <<"Search for files matching a glob pattern. "
                        "Examples: '**/*.erl' (all Erlang files), 'src/**/*.js' (JS files in src)">>,
        input_schema => #{
            type => object,
            properties => #{
                <<"pattern">> => #{
                    type => string,
                    description => <<"Glob pattern (e.g., '**/*.erl', 'src/*.js')">>
                },
                <<"path">> => #{
                    type => string,
                    description => <<"Base path to search in (optional, defaults to current directory)">>
                }
            },
            required => [<<"pattern">>]
        },
        handler => fun(Input, _Context) ->
            handle_glob(Input, MwState, Debug)
        end
    }.

%% @private 构建 Grep 工具
-spec build_grep_tool(map()) -> map().
build_grep_tool(MwState) ->
    #{debug := Debug} = MwState,

    #{
        name => <<"file_grep">>,
        description => <<"Search for content in files using regex. "
                        "Returns matching lines with context.">>,
        input_schema => #{
            type => object,
            properties => #{
                <<"pattern">> => #{
                    type => string,
                    description => <<"Regular expression pattern to search for">>
                },
                <<"path">> => #{
                    type => string,
                    description => <<"Base path to search in">>
                },
                <<"file_pattern">> => #{
                    type => string,
                    description => <<"Glob pattern for files to search (optional, e.g., '*.erl')">>
                },
                <<"context_lines">> => #{
                    type => integer,
                    description => <<"Number of context lines before and after match (optional)">>
                }
            },
            required => [<<"pattern">>, <<"path">>]
        },
        handler => fun(Input, _Context) ->
            handle_grep(Input, MwState, Debug)
        end
    }.

%% @private 构建 Read 工具
-spec build_read_tool(map()) -> map().
build_read_tool(MwState) ->
    #{debug := Debug} = MwState,

    #{
        name => <<"file_read">>,
        description => <<"Read the contents of a file">>,
        input_schema => #{
            type => object,
            properties => #{
                <<"path">> => #{
                    type => string,
                    description => <<"Path to the file to read">>
                },
                <<"start_line">> => #{
                    type => integer,
                    description => <<"Starting line number (optional, 1-indexed)">>
                },
                <<"end_line">> => #{
                    type => integer,
                    description => <<"Ending line number (optional)">>
                }
            },
            required => [<<"path">>]
        },
        handler => fun(Input, _Context) ->
            handle_read(Input, MwState, Debug)
        end
    }.

%% @private 处理 Glob 搜索
-spec handle_glob(map(), map(), boolean()) -> {ok, binary()}.
handle_glob(#{<<"pattern">> := Pattern} = Input, MwState, Debug) ->
    BasePath = maps:get(<<"path">>, Input, <<".">>),

    case Debug of
        true -> logger:info("[FileSearch] Glob: ~s in ~s", [Pattern, BasePath]);
        false -> ok
    end,

    case glob_search(Pattern, BasePath, MwState) of
        {ok, Files} ->
            Result = format_glob_results(Files),
            {ok, Result};
        {error, Reason} ->
            ErrorMsg = io_lib:format("Error: ~p", [Reason]),
            {ok, iolist_to_binary(ErrorMsg)}
    end.

%% @private 处理 Grep 搜索
-spec handle_grep(map(), map(), boolean()) -> {ok, binary()}.
handle_grep(#{<<"pattern">> := Pattern, <<"path">> := BasePath} = Input, MwState, Debug) ->
    FilePattern = maps:get(<<"file_pattern">>, Input, undefined),

    case Debug of
        true -> logger:info("[FileSearch] Grep: ~s in ~s", [Pattern, BasePath]);
        false -> ok
    end,

    case grep_search(Pattern, BasePath, FilePattern, MwState) of
        {ok, Matches} ->
            Result = format_grep_results(Matches),
            {ok, Result};
        {error, Reason} ->
            ErrorMsg = io_lib:format("Error: ~p", [Reason]),
            {ok, iolist_to_binary(ErrorMsg)}
    end.

%% @private 处理文件读取
-spec handle_read(map(), map(), boolean()) -> {ok, binary()}.
handle_read(#{<<"path">> := FilePath} = Input, MwState, Debug) ->
    StartLine = maps:get(<<"start_line">>, Input, undefined),
    EndLine = maps:get(<<"end_line">>, Input, undefined),

    case Debug of
        true -> logger:info("[FileSearch] Read: ~s", [FilePath]);
        false -> ok
    end,

    case read_file(FilePath, MwState) of
        {ok, Content} ->
            %% 可选：截取特定行
            FilteredContent = case {StartLine, EndLine} of
                {undefined, undefined} ->
                    Content;
                {Start, End} ->
                    Lines = binary:split(Content, <<"\n">>, [global]),
                    filter_lines(Lines, Start, End)
            end,
            {ok, FilteredContent};
        {error, {file_too_large, Size, MaxSize}} ->
            ErrorMsg = io_lib:format("File too large: ~B bytes (max: ~B)", [Size, MaxSize]),
            {ok, iolist_to_binary(ErrorMsg)};
        {error, Reason} ->
            ErrorMsg = io_lib:format("Error reading file: ~p", [Reason]),
            {ok, iolist_to_binary(ErrorMsg)}
    end.

%% @private 检查路径是否允许
-spec is_path_allowed(binary(), [binary()]) -> boolean().
is_path_allowed(_Path, []) ->
    true;  %% 如果没有限制，允许所有路径
is_path_allowed(Path, AllowedPaths) ->
    %% 规范化路径
    NormalizedPath = normalize_path(Path),

    lists:any(fun(AllowedPath) ->
        NormalizedAllowed = normalize_path(AllowedPath),
        %% 检查是否为允许路径的子路径
        binary:match(NormalizedPath, NormalizedAllowed) =:= {0, byte_size(NormalizedAllowed)} orelse
        NormalizedPath =:= NormalizedAllowed
    end, AllowedPaths).

%% @private 规范化路径
-spec normalize_path(binary()) -> binary().
normalize_path(Path) ->
    %% 简单实现：去除尾部斜杠
    case binary:last(Path) of
        $/ -> binary:part(Path, 0, byte_size(Path) - 1);
        _ -> Path
    end.

%% @private 执行 Glob 搜索
-spec do_glob_search(binary(), binary(), [binary()], pos_integer(), pos_integer()) -> [binary()].
do_glob_search(Pattern, BasePath, ExcludedPaths, MaxDepth, MaxResults) ->
    %% 使用 filelib:wildcard 的简化实现
    %% 生产环境可以考虑使用更高效的实现
    FullPattern = filename:join(binary_to_list(BasePath), binary_to_list(Pattern)),

    try
        Files = filelib:wildcard(FullPattern),

        %% 过滤排除路径
        FilteredFiles = lists:filter(fun(File) ->
            FileBin = list_to_binary(File),
            not is_excluded(FileBin, ExcludedPaths)
        end, Files),

        %% 限制结果数量
        Limited = lists:sublist(FilteredFiles, MaxResults),

        %% 转换为 binary
        [list_to_binary(F) || F <- Limited]
    catch
        _:_ -> []
    end.

%% @private 检查是否被排除
-spec is_excluded(binary(), [binary()]) -> boolean().
is_excluded(Path, ExcludedPaths) ->
    lists:any(fun(Excluded) ->
        binary:match(Path, Excluded) =/= nomatch
    end, ExcludedPaths).

%% @private 执行 Grep 搜索
-spec do_grep_search(binary(), [binary()], pos_integer(), pos_integer(), pos_integer()) -> [map()].
do_grep_search(Pattern, Files, MaxFileSize, MaxResults, MaxContextLines) ->
    %% 编译正则
    case re:compile(Pattern, [caseless, multiline]) of
        {ok, MP} ->
            %% 在每个文件中搜索
            {Results, _} = lists:foldl(fun(File, {Acc, Count}) ->
                case Count >= MaxResults of
                    true ->
                        {Acc, Count};
                    false ->
                        case search_file(File, MP, MaxFileSize, MaxContextLines) of
                            [] ->
                                {Acc, Count};
                            Matches ->
                                NewCount = Count + length(Matches),
                                TakeCount = min(length(Matches), MaxResults - Count),
                                {Acc ++ lists:sublist(Matches, TakeCount), NewCount}
                        end
                end
            end, {[], 0}, Files),
            Results;
        {error, _} ->
            []
    end.

%% @private 在单个文件中搜索
-spec search_file(binary(), term(), pos_integer(), pos_integer()) -> [map()].
search_file(FilePath, CompiledPattern, MaxFileSize, ContextLines) ->
    case file:read_file_info(FilePath) of
        {ok, #file_info{size = Size}} when Size =< MaxFileSize ->
            case file:read_file(FilePath) of
                {ok, Content} ->
                    find_matches_in_content(FilePath, Content, CompiledPattern, ContextLines);
                {error, _} ->
                    []
            end;
        _ ->
            []
    end.

%% @private 在内容中查找匹配
-spec find_matches_in_content(binary(), binary(), term(), pos_integer()) -> [map()].
find_matches_in_content(FilePath, Content, CompiledPattern, ContextLines) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    LineCount = length(Lines),

    {Matches, _} = lists:foldl(fun(Line, {Acc, LineNum}) ->
        case re:run(Line, CompiledPattern, [{capture, first, binary}]) of
            {match, [_MatchText]} ->
                %% 获取上下文
                StartLine = max(1, LineNum - ContextLines),
                EndLine = min(LineCount, LineNum + ContextLines),
                ContextLinesList = lists:sublist(Lines, StartLine, EndLine - StartLine + 1),
                Context = format_context_lines(ContextLinesList, StartLine, LineNum),

                Match = #{
                    file => FilePath,
                    line_number => LineNum,
                    line => Line,
                    context => Context
                },
                {[Match | Acc], LineNum + 1};
            nomatch ->
                {Acc, LineNum + 1}
        end
    end, {[], 1}, Lines),

    lists:reverse(Matches).

%% @private 格式化上下文行
-spec format_context_lines([binary()], pos_integer(), pos_integer()) -> binary().
format_context_lines(Lines, StartLine, MatchLine) ->
    Formatted = lists:map(fun({Line, Num}) ->
        LineNum = StartLine + Num - 1,
        Marker = case LineNum =:= MatchLine of
            true -> <<">>">>;
            false -> <<"  ">>
        end,
        io_lib:format("~s ~4B: ~s", [Marker, LineNum, Line])
    end, lists:zip(Lines, lists:seq(1, length(Lines)))),
    iolist_to_binary(string:join(Formatted, "\n")).

%% @private 过滤行
-spec filter_lines([binary()], pos_integer() | undefined, pos_integer() | undefined) -> binary().
filter_lines(Lines, Start, End) ->
    StartIdx = case Start of undefined -> 1; S -> S end,
    EndIdx = case End of undefined -> length(Lines); E -> E end,

    FilteredLines = lists:sublist(Lines, StartIdx, EndIdx - StartIdx + 1),

    %% 添加行号
    Formatted = lists:map(fun({Line, Num}) ->
        io_lib:format("~4B: ~s", [StartIdx + Num - 1, Line])
    end, lists:zip(FilteredLines, lists:seq(1, length(FilteredLines)))),

    iolist_to_binary(string:join(Formatted, "\n")).

%% @private 格式化 Glob 结果
-spec format_glob_results([binary()]) -> binary().
format_glob_results([]) ->
    <<"No files found">>;
format_glob_results(Files) ->
    Header = io_lib:format("Found ~B files:\n", [length(Files)]),
    FileList = [[<<"  ">>, F, <<"\n">>] || F <- Files],
    iolist_to_binary([Header | FileList]).

%% @private 格式化 Grep 结果
-spec format_grep_results([map()]) -> binary().
format_grep_results([]) ->
    <<"No matches found">>;
format_grep_results(Matches) ->
    Header = io_lib:format("Found ~B matches:\n\n", [length(Matches)]),

    MatchTexts = lists:map(fun(#{file := File, line_number := LineNum, context := Context}) ->
        io_lib:format("~s:~B\n~s\n", [File, LineNum, Context])
    end, Matches),

    iolist_to_binary([Header | string:join(MatchTexts, "\n---\n")]).
