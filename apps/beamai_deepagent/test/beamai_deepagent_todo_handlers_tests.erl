%%%-------------------------------------------------------------------
%%% @doc TodoList 处理器单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_todo_handlers_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

mock_state() ->
    graph_state:new(#{todos => []}).

mock_state_with_todos(Todos) ->
    graph_state:new(#{todos => Todos}).

%%====================================================================
%% 测试：write_todos
%%====================================================================

write_todos_empty_test() ->
    State = mock_state(),
    Args = #{<<"todos">> => []},

    Result = beamai_deepagent_todo_handlers:handle_write_todos(Args, State),

    ?assertEqual(true, maps:get(success, Result)),
    ?assertEqual([], maps:get(todos, Result)).

write_todos_new_items_test() ->
    State = mock_state(),
    Args = #{<<"todos">> => [
        #{<<"content">> => <<"Task 1">>, <<"status">> => <<"pending">>},
        #{<<"content">> => <<"Task 2">>, <<"status">> => <<"in_progress">>}
    ]},

    Result = beamai_deepagent_todo_handlers:handle_write_todos(Args, State),

    ?assertEqual(true, maps:get(success, Result)),
    Todos = maps:get(todos, Result),
    ?assertEqual(2, length(Todos)),

    %% 验证统计
    Stats = maps:get(stats, Result),
    ?assertEqual(2, maps:get(total, Stats)),
    ?assertEqual(1, maps:get(pending_count, Stats)),
    ?assertEqual(1, maps:get(in_progress_count, Stats)).

write_todos_update_existing_test() ->
    %% 创建已有的 todo
    ExistingTodo = #{
        id => <<"todo_1">>,
        content => <<"Task 1">>,
        status => pending,
        created_at => 1000
    },
    State = mock_state_with_todos([ExistingTodo]),

    %% 更新状态
    Args = #{<<"todos">> => [
        #{<<"content">> => <<"Task 1">>, <<"status">> => <<"completed">>}
    ]},

    Result = beamai_deepagent_todo_handlers:handle_write_todos(Args, State),

    ?assertEqual(true, maps:get(success, Result)),
    Todos = maps:get(todos, Result),
    ?assertEqual(1, length(Todos)),

    %% 验证状态已更新
    [UpdatedTodo] = Todos,
    ?assertEqual(completed, maps:get(status, UpdatedTodo)).

%%====================================================================
%% 测试：read_todos
%%====================================================================

read_todos_empty_test() ->
    State = mock_state(),

    Result = beamai_deepagent_todo_handlers:handle_read_todos(#{}, State),

    ?assertEqual(true, maps:get(success, Result)),
    ?assertEqual([], maps:get(todos, Result)).

read_todos_with_items_test() ->
    Todos = [
        #{id => <<"1">>, content => <<"Task 1">>, status => completed, created_at => 1000},
        #{id => <<"2">>, content => <<"Task 2">>, status => pending, created_at => 2000}
    ],
    State = mock_state_with_todos(Todos),

    Result = beamai_deepagent_todo_handlers:handle_read_todos(#{}, State),

    ?assertEqual(true, maps:get(success, Result)),
    ?assertEqual(2, length(maps:get(todos, Result))),

    Stats = maps:get(stats, Result),
    ?assertEqual(2, maps:get(total, Stats)),
    ?assertEqual(1, maps:get(completed_count, Stats)),
    ?assertEqual(1, maps:get(pending_count, Stats)).

%%====================================================================
%% 测试：格式化显示
%%====================================================================

format_todos_empty_test() ->
    Result = beamai_deepagent_todo_handlers:format_todos_for_display([]),
    ?assert(is_binary(Result)).

format_todos_with_items_test() ->
    Todos = [
        #{content => <<"Task 1">>, status => completed},
        #{content => <<"Task 2">>, status => in_progress},
        #{content => <<"Task 3">>, status => pending}
    ],

    Result = beamai_deepagent_todo_handlers:format_todos_for_display(Todos),

    ?assert(binary:match(Result, <<"[x]">>) =/= nomatch),
    ?assert(binary:match(Result, <<"[>]">>) =/= nomatch),
    ?assert(binary:match(Result, <<"[ ]">>) =/= nomatch).
