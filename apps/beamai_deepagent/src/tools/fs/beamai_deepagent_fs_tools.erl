%%%-------------------------------------------------------------------
%%% @doc Agent Deep 文件系统工具定义
%%%
%%% 定义文件操作工具的 JSON Schema，供 LLM 调用。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_fs_tools).

%% API
-export([all/0]).
-export([ls_tool/0, read_file_tool/0, write_file_tool/0]).
-export([edit_file_tool/0, glob_tool/0, mkdir_tool/0]).

%%====================================================================
%% 工具集合
%%====================================================================

%% @doc 获取所有文件系统工具
-spec all() -> [map()].
all() ->
    [
        ls_tool(),
        read_file_tool(),
        write_file_tool(),
        edit_file_tool(),
        glob_tool(),
        mkdir_tool()
    ].

%%====================================================================
%% 工具定义
%%====================================================================

%% @doc ls 工具 - 列出目录内容
-spec ls_tool() -> map().
ls_tool() ->
    #{
        name => <<"ls">>,
        description => <<"列出指定目录的内容。返回文件和子目录列表。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"path">> => #{
                    type => string,
                    description => <<"要列出的目录路径，相对于工作目录。默认为当前目录。"/utf8>>
                }
            },
            required => []
        },
        handler => fun beamai_deepagent_fs_handlers:handle_ls/2
    }.

%% @doc read_file 工具 - 读取文件内容
-spec read_file_tool() -> map().
read_file_tool() ->
    #{
        name => <<"read_file">>,
        description => <<"读取指定文件的内容。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"path">> => #{
                    type => string,
                    description => <<"要读取的文件路径"/utf8>>
                }
            },
            required => [<<"path">>]
        },
        handler => fun beamai_deepagent_fs_handlers:handle_read_file/2
    }.

%% @doc write_file 工具 - 写入文件
-spec write_file_tool() -> map().
write_file_tool() ->
    #{
        name => <<"write_file">>,
        description => <<"将内容写入指定文件。如果文件不存在则创建，如果存在则覆盖。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"path">> => #{
                    type => string,
                    description => <<"要写入的文件路径"/utf8>>
                },
                <<"content">> => #{
                    type => string,
                    description => <<"要写入的内容"/utf8>>
                }
            },
            required => [<<"path">>, <<"content">>]
        },
        handler => fun beamai_deepagent_fs_handlers:handle_write_file/2
    }.

%% @doc edit_file 工具 - 编辑文件
-spec edit_file_tool() -> map().
edit_file_tool() ->
    #{
        name => <<"edit_file">>,
        description => <<"编辑文件，将指定的旧字符串替换为新字符串。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"path">> => #{
                    type => string,
                    description => <<"要编辑的文件路径"/utf8>>
                },
                <<"old_string">> => #{
                    type => string,
                    description => <<"要被替换的字符串（必须唯一）"/utf8>>
                },
                <<"new_string">> => #{
                    type => string,
                    description => <<"替换后的新字符串"/utf8>>
                }
            },
            required => [<<"path">>, <<"old_string">>, <<"new_string">>]
        },
        handler => fun beamai_deepagent_fs_handlers:handle_edit_file/2
    }.

%% @doc glob 工具 - 模式匹配文件
-spec glob_tool() -> map().
glob_tool() ->
    #{
        name => <<"glob">>,
        description => <<"使用通配符模式搜索文件。支持 * 和 ? 通配符。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"pattern">> => #{
                    type => string,
                    description => <<"搜索模式，如 *.txt 或 src/**/*.erl"/utf8>>
                }
            },
            required => [<<"pattern">>]
        },
        handler => fun beamai_deepagent_fs_handlers:handle_glob/2
    }.

%% @doc mkdir 工具 - 创建目录
-spec mkdir_tool() -> map().
mkdir_tool() ->
    #{
        name => <<"mkdir">>,
        description => <<"创建目录。如果父目录不存在会自动创建。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"path">> => #{
                    type => string,
                    description => <<"要创建的目录路径"/utf8>>
                }
            },
            required => [<<"path">>]
        },
        handler => fun beamai_deepagent_fs_handlers:handle_mkdir/2
    }.
