%%%-------------------------------------------------------------------
%%% @doc Agent Deep TodoList 工具处理器
%%%
%%% 实现 TodoList 工具的具体逻辑。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_todo_handlers).

%% API
-export([handle_write_todos/2, handle_read_todos/2]).

%% 辅助函数
-export([format_todos_for_display/1]).

%%====================================================================
%% 处理器实现
%%====================================================================

%% @doc 处理 write_todos 命令
-spec handle_write_todos(map(), map()) -> map().
handle_write_todos(Args, State) ->
    TodosInput = maps:get(<<"todos">>, Args, []),
    ExistingTodos = get_todos(State),

    %% 合并或创建 todos
    NewTodos = merge_todos(ExistingTodos, TodosInput),

    %% 统计信息
    Stats = compute_stats(NewTodos),

    #{
        action => write_todos,
        success => true,
        todos => NewTodos,
        stats => Stats,
        message => format_write_message(Stats),
        update_todos => NewTodos
    }.

%% @doc 处理 read_todos 命令
-spec handle_read_todos(map(), map()) -> map().
handle_read_todos(_Args, State) ->
    Todos = get_todos(State),
    Stats = compute_stats(Todos),

    #{
        action => read_todos,
        success => true,
        todos => Todos,
        stats => Stats,
        message => format_read_message(Todos, Stats)
    }.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 从状态获取 todos
get_todos(State) ->
    case graph_state:get(State, todos) of
        undefined -> [];
        Todos -> Todos
    end.

%% @private 合并新旧 todos
merge_todos(ExistingTodos, NewTodosInput) ->
    %% 构建现有 todo 的内容索引
    ExistingMap = maps:from_list([{maps:get(content, T), T} || T <- ExistingTodos]),

    %% 处理新输入
    {UpdatedMap, _} = lists:foldl(
        fun(TodoInput, {AccMap, Idx}) ->
            Content = maps:get(<<"content">>, TodoInput),
            Status = parse_status(maps:get(<<"status">>, TodoInput, <<"pending">>)),

            NewTodo = case maps:find(Content, AccMap) of
                {ok, Existing} ->
                    %% 更新现有 todo 的状态
                    Existing#{status => Status};
                error ->
                    %% 创建新 todo
                    #{
                        id => generate_id(Idx),
                        content => Content,
                        status => Status,
                        created_at => erlang:system_time(millisecond)
                    }
            end,
            {maps:put(Content, NewTodo, AccMap), Idx + 1}
        end,
        {ExistingMap, length(ExistingTodos) + 1},
        NewTodosInput
    ),

    %% 按创建时间排序
    lists:sort(
        fun(A, B) ->
            maps:get(created_at, A) =< maps:get(created_at, B)
        end,
        maps:values(UpdatedMap)
    ).

%% @private 解析状态字符串
parse_status(<<"pending">>) -> pending;
parse_status(<<"in_progress">>) -> in_progress;
parse_status(<<"completed">>) -> completed;
parse_status(_) -> pending.

%% @private 生成 todo ID
generate_id(Idx) ->
    iolist_to_binary(io_lib:format("todo_~p", [Idx])).

%% @private 计算统计信息
compute_stats(Todos) ->
    lists:foldl(
        fun(Todo, Acc) ->
            Status = maps:get(status, Todo),
            Key = case Status of
                pending -> pending_count;
                in_progress -> in_progress_count;
                completed -> completed_count
            end,
            maps:update_with(Key, fun(V) -> V + 1 end, 1, Acc)
        end,
        #{total => length(Todos),
          pending_count => 0,
          in_progress_count => 0,
          completed_count => 0},
        Todos
    ).

%% @private 格式化 write 消息
format_write_message(Stats) ->
    Total = maps:get(total, Stats),
    Completed = maps:get(completed_count, Stats),
    InProgress = maps:get(in_progress_count, Stats),
    Pending = maps:get(pending_count, Stats),

    unicode:characters_to_binary(
        io_lib:format(
            "任务列表已更新: ~p 个任务 (~p 待办, ~p 进行中, ~p 已完成)",
            [Total, Pending, InProgress, Completed])).

%% @private 格式化 read 消息
format_read_message([], _Stats) ->
    <<"当前没有任务"/utf8>>;
format_read_message(Todos, Stats) ->
    Total = maps:get(total, Stats),
    Completed = maps:get(completed_count, Stats),

    Header = unicode:characters_to_binary(
        io_lib:format("任务进度: ~p/~p 已完成~n", [Completed, Total])),

    TodoLines = [format_todo_line(T) || T <- Todos],
    iolist_to_binary([Header | TodoLines]).

%% @private 格式化单个 todo 行
format_todo_line(#{content := Content, status := Status}) ->
    Marker = case Status of
        completed -> <<"[x]">>;
        in_progress -> <<"[>]">>;
        pending -> <<"[ ]">>
    end,
    iolist_to_binary(io_lib:format("~s ~s~n", [Marker, Content])).

%% @doc 格式化 todos 用于显示（公开函数供其他模块使用）
-spec format_todos_for_display([map()]) -> binary().
format_todos_for_display([]) ->
    <<"无任务"/utf8>>;
format_todos_for_display(Todos) ->
    Lines = [format_todo_line(T) || T <- Todos],
    iolist_to_binary(Lines).
