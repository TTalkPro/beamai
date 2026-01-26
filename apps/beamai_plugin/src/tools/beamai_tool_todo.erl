%%%-------------------------------------------------------------------
%%% @doc 任务管理工具模块
%%%
%%% 提供任务列表管理工具：
%%% - write_todos: 创建或更新任务列表
%%% - read_todos: 读取当前任务列表及状态统计
%%%
%%% 任务数据通过上下文（Context）进行持久化。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_todo).

-behaviour(beamai_tool_behaviour).

-export([tool_info/0, tools/0]).
-export([handle_write_todos/2, handle_read_todos/2]).

-define(TODOS_KEY, todos).

%%====================================================================
%% 工具行为回调
%%====================================================================

tool_info() ->
    #{description => <<"Task list management">>,
      tags => [<<"todo">>, <<"task">>]}.

tools() ->
    [write_todos_tool(), read_todos_tool()].

%%====================================================================
%% 工具定义
%%====================================================================

write_todos_tool() ->
    TodoItemSchema = #{
        type => object,
        properties => #{
            <<"content">> => #{type => string, description => <<"Task description">>},
            <<"status">> => #{type => string,
                             description => <<"Status: pending, in_progress, completed">>,
                             enum => [<<"pending">>, <<"in_progress">>, <<"completed">>]}
        },
        required => [<<"content">>, <<"status">>]
    },
    #{
        name => <<"write_todos">>,
        handler => fun ?MODULE:handle_write_todos/2,
        description => <<"Create or update task list.">>,
        tag => <<"todo">>,
        parameters => #{
            <<"todos">> => #{
                type => array,
                description => <<"Task list">>,
                items => TodoItemSchema,
                required => true
            }
        }
    }.

read_todos_tool() ->
    #{
        name => <<"read_todos">>,
        handler => fun ?MODULE:handle_read_todos/2,
        description => <<"Read current task list and status.">>,
        tag => <<"todo">>
    }.

%%====================================================================
%% 处理器函数
%%====================================================================

handle_write_todos(Args, Context) ->
    TodosInput = maps:get(<<"todos">>, Args, []),
    ExistingTodos = maps:get(?TODOS_KEY, Context, []),
    {NewTodos, Stats} = process_todos(TodosInput, ExistingTodos),
    FormattedTodos = format_todos(NewTodos),
    Result = #{
        success => true,
        todos => FormattedTodos,
        stats => Stats,
        summary => format_summary(Stats)
    },
    {ok, Result, #{?TODOS_KEY => NewTodos}}.

handle_read_todos(_Args, Context) ->
    Todos = maps:get(?TODOS_KEY, Context, []),
    Stats = compute_stats(Todos),
    FormattedTodos = format_todos(Todos),
    {ok, #{
        success => true,
        todos => FormattedTodos,
        stats => Stats,
        summary => format_summary(Stats)
    }}.

%%====================================================================
%% 内部函数
%%====================================================================

process_todos(NewTodos, _ExistingTodos) ->
    ProcessedTodos = lists:map(fun add_metadata/1, NewTodos),
    Stats = compute_stats(ProcessedTodos),
    {ProcessedTodos, Stats}.

add_metadata(Todo) ->
    Now = erlang:system_time(millisecond),
    Id = maps:get(<<"id">>, Todo, generate_id()),
    Status = maps:get(<<"status">>, Todo, <<"pending">>),
    Todo#{
        <<"id">> => Id,
        <<"created_at">> => maps:get(<<"created_at">>, Todo, Now),
        <<"updated_at">> => Now,
        <<"status">> => Status
    }.

generate_id() ->
    Rand = rand:uniform(16#FFFFFFFF),
    iolist_to_binary(io_lib:format("todo_~8.16.0b", [Rand])).

compute_stats(Todos) ->
    Total = length(Todos),
    Pending = count_by_status(Todos, <<"pending">>),
    InProgress = count_by_status(Todos, <<"in_progress">>),
    Completed = count_by_status(Todos, <<"completed">>),
    #{
        total => Total,
        pending => Pending,
        in_progress => InProgress,
        completed => Completed,
        completion_rate => case Total of
            0 -> 0;
            _ -> round(Completed / Total * 100)
        end
    }.

count_by_status(Todos, Status) ->
    length([T || T <- Todos, maps:get(<<"status">>, T, <<"pending">>) =:= Status]).

format_todos(Todos) ->
    lists:map(fun(Todo) ->
        #{
            id => maps:get(<<"id">>, Todo),
            content => maps:get(<<"content">>, Todo),
            status => maps:get(<<"status">>, Todo),
            created_at => maps:get(<<"created_at">>, Todo),
            updated_at => maps:get(<<"updated_at">>, Todo)
        }
    end, Todos).

format_summary(#{total := Total, pending := Pending,
                 in_progress := InProgress, completed := Completed,
                 completion_rate := Rate}) ->
    iolist_to_binary(io_lib:format(
        "Total: ~p, Pending: ~p, In Progress: ~p, Completed: ~p (~p%)",
        [Total, Pending, InProgress, Completed, Rate]
    )).
