%%%-------------------------------------------------------------------
%%% @doc Agent Deep TodoList 工具定义
%%%
%%% 使用 beamai_tools 公共工具库的定义，结合 beamai_deepagent 特有的处理器。
%%%
%%% 与直接使用 beamai_tools 的区别：
%%% - 使用 graph_state 管理状态
%%% - 返回格式适配 beamai_deepagent 的工具执行流程
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_todo_tools).

-include_lib("beamai_tools/include/beamai_tools.hrl").

%% API
-export([all/0]).
-export([write_todos_tool/0, read_todos_tool/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type todo_status() :: pending | in_progress | completed.

-type todo() :: #{
    id := binary(),
    content := binary(),
    status := todo_status(),
    created_at := integer()
}.

-export_type([todo/0, todo_status/0]).

%%====================================================================
%% 工具集合
%%====================================================================

%% @doc 获取所有 TodoList 工具
%%
%% 从 beamai_tools 获取工具定义，替换为 beamai_deepagent 特有的处理器。
-spec all() -> [map()].
all() ->
    [
        write_todos_tool(),
        read_todos_tool()
    ].

%%====================================================================
%% 工具定义
%%====================================================================

%% @doc write_todos 工具
%%
%% 使用 beamai_tools 的定义，覆盖 handler 为 beamai_deepagent 实现。
-spec write_todos_tool() -> map().
write_todos_tool() ->
    case beamai_tools:find_tool(<<"write_todos">>) of
        {ok, ToolDef} ->
            %% 工具定义已经是 map 格式，直接替换 handler
            ToolDef#{handler => fun beamai_deepagent_todo_handlers:handle_write_todos/2};
        {error, not_found} ->
            %% 降级：使用本地定义
            fallback_write_todos_tool()
    end.

%% @doc read_todos 工具
%%
%% 使用 beamai_tools 的定义，覆盖 handler 为 beamai_deepagent 实现。
-spec read_todos_tool() -> map().
read_todos_tool() ->
    case beamai_tools:find_tool(<<"read_todos">>) of
        {ok, ToolDef} ->
            %% 工具定义已经是 map 格式，直接替换 handler
            ToolDef#{handler => fun beamai_deepagent_todo_handlers:handle_read_todos/2};
        {error, not_found} ->
            fallback_read_todos_tool()
    end.

%%====================================================================
%% 降级定义（当 beamai_tools 不可用时）
%%====================================================================

%% @private 降级 write_todos 定义
fallback_write_todos_tool() ->
    #{
        name => <<"write_todos">>,
        description => <<"创建或更新任务列表。用于跟踪需要完成的步骤。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"todos">> => #{
                    type => array,
                    description => <<"任务列表"/utf8>>,
                    items => #{
                        type => object,
                        properties => #{
                            <<"content">> => #{
                                type => string,
                                description => <<"任务内容描述"/utf8>>
                            },
                            <<"status">> => #{
                                type => string,
                                enum => [<<"pending">>, <<"in_progress">>, <<"completed">>],
                                description => <<"任务状态"/utf8>>
                            }
                        },
                        required => [<<"content">>, <<"status">>]
                    }
                }
            },
            required => [<<"todos">>]
        },
        handler => fun beamai_deepagent_todo_handlers:handle_write_todos/2
    }.

%% @private 降级 read_todos 定义
fallback_read_todos_tool() ->
    #{
        name => <<"read_todos">>,
        description => <<"读取当前的任务列表，查看所有任务及其状态。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{},
            required => []
        },
        handler => fun beamai_deepagent_todo_handlers:handle_read_todos/2
    }.
