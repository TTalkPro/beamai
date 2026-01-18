%%%-------------------------------------------------------------------
%%% @doc Agent Deep 文件系统工具处理器
%%%
%%% 实现文件操作工具的具体逻辑。
%%% 所有处理器都是纯函数，通过 State 传递文件系统后端。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_fs_handlers).

%% API
-export([handle_ls/2, handle_read_file/2, handle_write_file/2]).
-export([handle_edit_file/2, handle_glob/2, handle_mkdir/2]).

%%====================================================================
%% 处理器实现
%%====================================================================

%% @doc 处理 ls 命令
-spec handle_ls(map(), map()) -> map().
handle_ls(Args, State) ->
    Path = maps:get(<<"path">>, Args, <<".">>),
    Backend = get_backend(State),
    case beamai_deepagent_fs_backend:ls(Path, Backend) of
        {ok, Entries} ->
            FormattedEntries = format_entries(Entries),
            #{
                action => ls,
                success => true,
                path => Path,
                entries => FormattedEntries,
                count => length(Entries),
                message => format_ls_message(Path, Entries)
            };
        {error, Reason} ->
            #{
                action => ls,
                success => false,
                path => Path,
                error => format_error(Reason)
            }
    end.

%% @doc 处理 read_file 命令
-spec handle_read_file(map(), map()) -> map().
handle_read_file(Args, State) ->
    Path = maps:get(<<"path">>, Args),
    Backend = get_backend(State),
    case beamai_deepagent_fs_backend:read_file(Path, Backend) of
        {ok, Content} ->
            #{
                action => read_file,
                success => true,
                path => Path,
                content => Content,
                size => byte_size(Content),
                message => <<"文件读取成功"/utf8>>
            };
        {error, Reason} ->
            #{
                action => read_file,
                success => false,
                path => Path,
                error => format_error(Reason)
            }
    end.

%% @doc 处理 write_file 命令
-spec handle_write_file(map(), map()) -> map().
handle_write_file(Args, State) ->
    Path = maps:get(<<"path">>, Args),
    Content = maps:get(<<"content">>, Args),
    Backend = get_backend(State),
    case beamai_deepagent_fs_backend:write_file(Path, Content, Backend) of
        {ok, NewBackend} ->
            #{
                action => write_file,
                success => true,
                path => Path,
                size => byte_size(Content),
                message => <<"文件写入成功"/utf8>>,
                update_backend => NewBackend
            };
        {error, Reason} ->
            #{
                action => write_file,
                success => false,
                path => Path,
                error => format_error(Reason)
            }
    end.

%% @doc 处理 edit_file 命令
-spec handle_edit_file(map(), map()) -> map().
handle_edit_file(Args, State) ->
    Path = maps:get(<<"path">>, Args),
    OldString = maps:get(<<"old_string">>, Args),
    NewString = maps:get(<<"new_string">>, Args),
    Backend = get_backend(State),

    case beamai_deepagent_fs_backend:read_file(Path, Backend) of
        {ok, Content} ->
            %% 检查 old_string 出现次数
            case count_occurrences(Content, OldString) of
                0 ->
                    #{
                        action => edit_file,
                        success => false,
                        path => Path,
                        error => <<"未找到要替换的字符串"/utf8>>
                    };
                1 ->
                    %% 执行替换
                    NewContent = binary:replace(Content, OldString, NewString),
                    case beamai_deepagent_fs_backend:write_file(Path, NewContent, Backend) of
                        {ok, NewBackend} ->
                            #{
                                action => edit_file,
                                success => true,
                                path => Path,
                                message => <<"文件编辑成功"/utf8>>,
                                update_backend => NewBackend
                            };
                        {error, WriteReason} ->
                            #{
                                action => edit_file,
                                success => false,
                                path => Path,
                                error => format_error(WriteReason)
                            }
                    end;
                N ->
                    #{
                        action => edit_file,
                        success => false,
                        path => Path,
                        error => iolist_to_binary(
                            io_lib:format("字符串出现 ~p 次，请提供更具体的上下文使其唯一", [N]))
                    }
            end;
        {error, Reason} ->
            #{
                action => edit_file,
                success => false,
                path => Path,
                error => format_error(Reason)
            }
    end.

%% @doc 处理 glob 命令
-spec handle_glob(map(), map()) -> map().
handle_glob(Args, State) ->
    Pattern = maps:get(<<"pattern">>, Args),
    Backend = get_backend(State),
    case beamai_deepagent_fs_backend:glob(Pattern, Backend) of
        {ok, Matches} ->
            #{
                action => glob,
                success => true,
                pattern => Pattern,
                matches => Matches,
                count => length(Matches),
                message => iolist_to_binary(
                    io_lib:format("找到 ~p 个匹配文件", [length(Matches)]))
            };
        {error, Reason} ->
            #{
                action => glob,
                success => false,
                pattern => Pattern,
                error => format_error(Reason)
            }
    end.

%% @doc 处理 mkdir 命令
-spec handle_mkdir(map(), map()) -> map().
handle_mkdir(Args, State) ->
    Path = maps:get(<<"path">>, Args),
    Backend = get_backend(State),
    case beamai_deepagent_fs_backend:mkdir(Path, Backend) of
        {ok, NewBackend} ->
            #{
                action => mkdir,
                success => true,
                path => Path,
                message => <<"目录创建成功"/utf8>>,
                update_backend => NewBackend
            };
        {error, Reason} ->
            #{
                action => mkdir,
                success => false,
                path => Path,
                error => format_error(Reason)
            }
    end.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 获取文件系统后端
%%
%% 从状态获取后端，如果不存在则创建默认内存后端
get_backend(State) ->
    case graph_state:get(State, filesystem) of
        undefined -> beamai_deepagent_fs_backend:new(#{type => memory});
        Backend -> Backend
    end.

%% @private 格式化目录条目
format_entries(Entries) ->
    [format_entry(E) || E <- Entries].

format_entry(#{name := Name, type := Type} = Entry) ->
    Base = #{name => Name, type => Type},
    case maps:find(size, Entry) of
        {ok, Size} -> Base#{size => Size};
        error -> Base
    end.

%% @private 格式化 ls 输出消息
format_ls_message(Path, Entries) ->
    FileCount = length([E || E <- Entries, maps:get(type, E) =:= file]),
    DirCount = length([E || E <- Entries, maps:get(type, E) =:= directory]),
    unicode:characters_to_binary(
        io_lib:format("目录 ~ts: ~p 个文件, ~p 个子目录",
                      [Path, FileCount, DirCount])).

%% @private 格式化错误信息
format_error({Reason, Path}) when is_atom(Reason) ->
    iolist_to_binary(io_lib:format("~p: ~s", [Reason, Path]));
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason);
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @private 计算字符串出现次数
count_occurrences(Content, Pattern) ->
    count_occurrences(Content, Pattern, 0).

count_occurrences(Content, Pattern, Count) ->
    case binary:match(Content, Pattern) of
        nomatch -> Count;
        {Pos, Len} ->
            Rest = binary:part(Content, Pos + Len, byte_size(Content) - Pos - Len),
            count_occurrences(Rest, Pattern, Count + 1)
    end.
