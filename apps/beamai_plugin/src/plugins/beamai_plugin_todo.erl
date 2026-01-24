%%%-------------------------------------------------------------------
%%% @doc 任务管理插件
%%%
%%% 提供任务列表管理工具：
%%% - write_todos: 创建或更新任务列表
%%% - read_todos: 读取当前任务列表及状态统计
%%%
%%% 任务数据通过上下文（Context）进行持久化。
%%% 每个任务包含唯一 ID、内容、状态和时间戳信息。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_plugin_todo).

-behaviour(beamai_plugin_behaviour).

-export([plugin_info/0, functions/0]).
-export([handle_write_todos/2, handle_read_todos/2]).

%% 任务列表在上下文中的存储键名
-define(TODOS_KEY, todos).

%%====================================================================
%% 插件行为回调
%%====================================================================

%% @doc 返回插件元数据
%%
%% 提供插件的名称和描述信息，用于插件注册和发现。
%%
%% @returns 插件信息映射，包含 name 和 description 字段
plugin_info() ->
    #{name => <<"todo">>,
      description => <<"Task list management">>}.

%% @doc 返回函数定义列表
%%
%% 返回本插件提供的所有可调用函数定义，
%% 包含写入任务和读取任务两个函数。
%%
%% @returns 函数定义列表
functions() ->
    [write_todos_func(), read_todos_func()].

%%====================================================================
%% 函数定义
%%====================================================================

%% @doc 构建写入任务函数定义
%% 定义任务项的 JSON Schema，包含 content 和 status 字段
write_todos_func() ->
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
    Parameters = #{
        <<"todos">> => #{
            type => array,
            description => <<"Task list">>,
            items => TodoItemSchema,
            required => true
        }
    },
    beamai_function:new(<<"write_todos">>, fun ?MODULE:handle_write_todos/2, #{
        description => <<"Create or update task list.">>,
        parameters => Parameters
    }).

%% @doc 构建读取任务函数定义
%% 无需参数，直接读取上下文中的任务列表
read_todos_func() ->
    beamai_function:new(<<"read_todos">>, fun ?MODULE:handle_read_todos/2, #{
        description => <<"Read current task list and status.">>
    }).

%%====================================================================
%% 处理器函数
%%====================================================================

%% @doc 创建或更新任务列表
%%
%% 接收新的任务列表，为每个任务添加元数据（ID、时间戳等），
%% 计算统计信息并返回格式化后的结果。
%% 更新后的任务列表通过第三个返回元素写回上下文。
%%
%% @param Args 参数映射，包含：
%%   - todos: 任务列表，每项包含 content 和 status
%% @param Context 调用上下文，包含已有任务列表
%% @returns {ok, 结果映射, 上下文更新映射}
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

%% @doc 读取当前任务列表
%%
%% 从上下文中获取当前任务列表，计算统计信息，
%% 返回格式化后的任务列表和摘要。
%%
%% @param Args 参数映射（本函数未使用）
%% @param Context 调用上下文，包含当前任务列表
%% @returns {ok, 结果映射}
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

%% @doc 处理任务列表
%% 为每个任务添加元数据，并计算统计信息
%% 当前实现直接替换整个列表（不合并已有任务）
process_todos(NewTodos, _ExistingTodos) ->
    ProcessedTodos = lists:map(fun add_metadata/1, NewTodos),
    Stats = compute_stats(ProcessedTodos),
    {ProcessedTodos, Stats}.

%% @doc 为单个任务添加元数据
%% 生成唯一 ID（如果没有）、设置创建时间和更新时间、确保状态字段存在
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

%% @doc 生成唯一的任务 ID
%% 格式为 "todo_" 加 8 位十六进制随机数
generate_id() ->
    Rand = rand:uniform(16#FFFFFFFF),
    iolist_to_binary(io_lib:format("todo_~8.16.0b", [Rand])).

%% @doc 计算任务统计信息
%% 统计各状态的任务数量和完成率
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

%% @doc 统计指定状态的任务数量
count_by_status(Todos, Status) ->
    length([T || T <- Todos, maps:get(<<"status">>, T, <<"pending">>) =:= Status]).

%% @doc 格式化任务列表
%% 将内部任务映射转换为标准化的输出格式，提取关键字段
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

%% @doc 格式化统计摘要为可读字符串
%% 输出格式：Total: N, Pending: N, In Progress: N, Completed: N (N%)
format_summary(#{total := Total, pending := Pending,
                 in_progress := InProgress, completed := Completed,
                 completion_rate := Rate}) ->
    iolist_to_binary(io_lib:format(
        "Total: ~p, Pending: ~p, In Progress: ~p, Completed: ~p (~p%)",
        [Total, Pending, InProgress, Completed, Rate]
    )).
