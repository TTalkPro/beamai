%%%-------------------------------------------------------------------
%%% @doc 任务列表工具
%%%
%%% 提供任务管理相关的工具：
%%% - write_todos: 创建或更新任务列表
%%% - read_todos: 读取当前任务列表
%%%
%%% 任务列表通过 context 进行持久化。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tools_todo).

-include("beamai_tools.hrl").

%% 导入 DSL 函数
-import(beamai_tool, [define/5, no_params/0]).

%% API
-export([all/0]).

%% 工具处理器
-export([
    handle_write_todos/1,
    handle_write_todos/2,
    handle_read_todos/1,
    handle_read_todos/2
]).

%% Context 键
-define(TODOS_KEY, todos).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 获取所有任务列表工具定义
-spec all() -> [tool_def()].
all() ->
    [write_todos_tool(), read_todos_tool()].

%%====================================================================
%% 工具定义（使用 DSL）
%%====================================================================

%% @private write_todos 工具
%%
%% 使用嵌套对象定义任务项结构
write_todos_tool() ->
    %% 任务项的 schema 定义
    TodoItemSchema = #{
        type => object,
        properties => #{
            <<"content">> => #{type => string, description => <<"任务描述"/utf8>>},
            <<"status">> => #{
                type => string,
                description => <<"任务状态：pending、in_progress、completed"/utf8>>,
                enum => [<<"pending">>, <<"in_progress">>, <<"completed">>]
            }
        },
        required => [<<"content">>, <<"status">>]
    },

    %% 直接使用完整的参数 schema
    Parameters = #{
        type => object,
        properties => #{
            <<"todos">> => #{
                type => array,
                description => <<"任务列表"/utf8>>,
                items => TodoItemSchema
            }
        },
        required => [<<"todos">>]
    },

    define(<<"write_todos">>,
           <<"创建或更新任务列表。用于跟踪需要完成的步骤。"/utf8>>,
           #{category => todo},
           Parameters,
           fun ?MODULE:handle_write_todos/2).

%% @private read_todos 工具
read_todos_tool() ->
    define(<<"read_todos">>,
           <<"读取当前的任务列表，查看所有任务及其状态。"/utf8>>,
           #{category => todo},
           [],  %% 无参数
           fun ?MODULE:handle_read_todos/2).

%%====================================================================
%% 工具处理器
%%====================================================================

%% @doc 写入任务列表
handle_write_todos(Args) ->
    handle_write_todos(Args, #{}).

handle_write_todos(Args, Context) ->
    TodosInput = maps:get(<<"todos">>, Args, []),

    %% 获取现有任务
    ExistingTodos = maps:get(?TODOS_KEY, Context, []),

    %% 处理任务更新
    {NewTodos, Stats} = process_todos(TodosInput, ExistingTodos),

    %% 格式化结果
    FormattedTodos = format_todos(NewTodos),

    %% 返回结果和上下文更新
    Result = #{
        success => true,
        todos => FormattedTodos,
        stats => Stats,
        summary => format_summary(Stats)
    },
    {ok, Result, #{?TODOS_KEY => NewTodos}}.

%% @doc 读取任务列表
handle_read_todos(Args) ->
    handle_read_todos(Args, #{}).

handle_read_todos(_Args, Context) ->
    Todos = maps:get(?TODOS_KEY, Context, []),
    Stats = compute_stats(Todos),
    FormattedTodos = format_todos(Todos),

    Result = #{
        success => true,
        todos => FormattedTodos,
        stats => Stats,
        summary => format_summary(Stats)
    },
    {ok, Result}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 处理任务列表
%%
%% 合并新任务和现有任务。
%% 注意：当前实现是完全替换，ExistingTodos 保留用于未来合并逻辑。
process_todos(NewTodos, _ExistingTodos) ->
    %% 为新任务添加时间戳和 ID
    ProcessedTodos = lists:map(fun(Todo) ->
        add_metadata(Todo)
    end, NewTodos),

    %% 计算统计信息
    Stats = compute_stats(ProcessedTodos),

    {ProcessedTodos, Stats}.

%% @private 添加任务元数据
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

%% @private 生成任务 ID
generate_id() ->
    Rand = rand:uniform(16#FFFFFFFF),
    iolist_to_binary(io_lib:format("todo_~8.16.0b", [Rand])).

%% @private 计算统计信息
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

%% @private 按状态计数
count_by_status(Todos, Status) ->
    length([T || T <- Todos, maps:get(<<"status">>, T, <<"pending">>) =:= Status]).

%% @private 格式化任务列表
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

%% @private 格式化摘要
format_summary(#{total := Total, pending := Pending,
                 in_progress := InProgress, completed := Completed,
                 completion_rate := Rate}) ->
    iolist_to_binary(io_lib:format(
        "任务总数: ~p, 待处理: ~p, 进行中: ~p, 已完成: ~p (~p%)",
        [Total, Pending, InProgress, Completed, Rate]
    )).
