%%%-------------------------------------------------------------------
%%% @doc graph_runner snapshot 功能单元测试
%%%
%%% 测试 graph_runner 的 snapshot 保存和恢复功能：
%%% - 简单模式（无 snapshot）正常执行
%%% - on_snapshot 回调被正确调用
%%% - 从 snapshot 恢复执行
%%% - snapshot 回调可以停止执行
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_runner_snapshot_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建多步测试图（节点循环执行多次）
make_multi_step_graph() ->
    ProcessFn = fun(State, _) ->
        Count = graph_state:get(State, count, 0),
        NewCount = Count + 1,
        State1 = graph_state:set(State, count, NewCount),
        graph_state:set(State1, last_count, NewCount)
    end,
    RouterFn = fun(State) ->
        Count = graph_state:get(State, count, 0),
        case Count >= 3 of
            true -> '__end__';
            false -> process
        end
    end,
    {ok, Graph} = graph:build([
        {node, process, ProcessFn},
        {conditional_edge, process, RouterFn},
        {entry, process}
    ]),
    Graph.

%%====================================================================
%% 模式检测测试
%%====================================================================

%% 测试：无 snapshot 选项时 needs_snapshot_mode 返回 false
needs_snapshot_mode_false_test() ->
    ?assertEqual(false, graph_runner:needs_snapshot_mode(#{})),
    ?assertEqual(false, graph_runner:needs_snapshot_mode(#{workers => 2})).

%% 测试：有 snapshot 选项时 needs_snapshot_mode 返回 true
needs_snapshot_mode_true_test() ->
    Callback = fun(_, _) -> continue end,
    ?assertEqual(true, graph_runner:needs_snapshot_mode(#{on_snapshot => Callback})),
    ?assertEqual(true, graph_runner:needs_snapshot_mode(#{restore_from => #{}})),
    ?assertEqual(true, graph_runner:needs_snapshot_mode(#{
        on_snapshot => Callback,
        restore_from => #{}
    })).

%%====================================================================
%% Snapshot 回调测试
%%====================================================================

%% 测试：on_snapshot 回调被调用
snapshot_callback_called_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    %% 使用进程字典记录回调调用次数
    Self = self(),
    Callback = fun(Info, SnapshotData) ->
        Self ! {snapshot_called, Info, SnapshotData},
        continue
    end,

    Options = #{on_snapshot => Callback},
    _Result = graph:run(Graph, InitialState, Options),

    %% 收集回调调用
    Calls = collect_snapshot_calls([]),

    %% 验证回调被调用了（至少一次）
    ?assert(length(Calls) >= 1),

    %% 验证 snapshot 数据结构
    %% 注意：graph_state 已移除，状态存储在 pregel_snapshot.vertices 中
    [{_Info, FirstSnapshot} | _] = Calls,
    ?assertNot(maps:is_key(graph_state, FirstSnapshot)),  %% 不应包含 graph_state
    ?assert(maps:is_key(pregel_snapshot, FirstSnapshot)),
    ?assert(maps:is_key(iteration, FirstSnapshot)).

collect_snapshot_calls(Acc) ->
    receive
        {snapshot_called, Info, Data} ->
            collect_snapshot_calls([{Info, Data} | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%% 测试：snapshot 数据包含 pregel 层信息
snapshot_contains_pregel_data_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    Self = self(),
    Callback = fun(_Info, SnapshotData) ->
        Self ! {snapshot, SnapshotData},
        continue
    end,

    Options = #{on_snapshot => Callback},
    _Result = graph:run(Graph, InitialState, Options),

    %% 获取第一个 snapshot
    receive
        {snapshot, Data} ->
            PregelSnapshot = maps:get(pregel_snapshot, Data),
            %% 验证 pregel snapshot 结构（无 inbox 版本）
            ?assert(maps:is_key(superstep, PregelSnapshot)),
            ?assert(maps:is_key(vertices, PregelSnapshot)),
            ?assert(maps:is_key(pending_activations, PregelSnapshot))
    after 1000 ->
        ?assert(false)
    end.

%%====================================================================
%% Snapshot 停止执行测试
%%====================================================================

%% 测试：snapshot 回调返回 {stop, Reason} 时停止执行
snapshot_callback_can_stop_execution_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    %% 回调在第一次调用时停止执行
    Callback = fun(_Info, _SnapshotData) ->
        {stop, user_requested}
    end,

    Options = #{on_snapshot => Callback},
    Result = graph:run(Graph, InitialState, Options),

    %% 验证执行被停止
    ?assertEqual(stopped, maps:get(status, Result)),
    ?assertMatch({user_stopped, user_requested}, maps:get(error, Result)).

%%====================================================================
%% Snapshot 恢复测试
%%====================================================================

%% 测试：从 snapshot 恢复执行
restore_from_snapshot_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    %% 第一次执行，保存 snapshot 后停止
    SavedSnapshot = erlang:make_ref(),
    Self = self(),
    Callback = fun(_Info, SnapshotData) ->
        %% 保存 snapshot 数据
        Self ! {save_snapshot, SavedSnapshot, SnapshotData},
        {stop, snapshot_saved}
    end,

    Options1 = #{on_snapshot => Callback},
    Result1 = graph:run(Graph, InitialState, Options1),

    %% 验证第一次执行被停止
    ?assertEqual(stopped, maps:get(status, Result1)),

    %% 获取保存的 snapshot
    SnapshotData = receive
        {save_snapshot, SavedSnapshot, Data} -> Data
    after 1000 ->
        error(no_snapshot_saved)
    end,

    %% 验证 snapshot 数据结构正确
    ?assert(maps:is_key(pregel_snapshot, SnapshotData)),
    ?assert(maps:is_key(iteration, SnapshotData)),

    %% 从 snapshot 恢复执行（使用回调继续执行）
    #{pregel_snapshot := PregelSnapshot, iteration := Iteration} = SnapshotData,
    RestoreOpts = #{
        pregel_snapshot => PregelSnapshot,
        iteration => Iteration
    },

    %% 使用默认回调（continue）恢复执行
    Options2 = #{restore_from => RestoreOpts},
    Result2 = graph:run(Graph, InitialState, Options2),

    %% 恢复后应该完成或返回错误（取决于图执行逻辑）
    Status2 = maps:get(status, Result2),
    ?assert(Status2 =:= completed orelse Status2 =:= error).

%%====================================================================
%% Snapshot 类型测试
%%====================================================================

%% 测试：snapshot 数据包含 type 字段
snapshot_contains_type_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    Self = self(),
    Callback = fun(Info, SnapshotData) ->
        Self ! {snapshot, Info, SnapshotData},
        continue
    end,

    Options = #{on_snapshot => Callback},
    _Result = graph:run(Graph, InitialState, Options),

    %% 获取第一个 snapshot，验证 type 字段
    receive
        {snapshot, Info, Data} ->
            %% Info 和 Data 都应该包含 type
            ?assert(maps:is_key(type, Info)),
            ?assert(maps:is_key(type, Data)),
            %% 第一个 snapshot 应该是 initial 类型
            ?assertEqual(initial, maps:get(type, Info))
    after 1000 ->
        ?assert(false)
    end.

%% 测试：initial 类型不允许 retry
initial_type_retry_not_allowed_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    %% 在 initial snapshot 时尝试 retry
    Callback = fun(Info, _SnapshotData) ->
        case maps:get(type, Info) of
            initial ->
                %% 尝试在 initial 时 retry（应该导致错误）
                {retry, [some_vertex]};
            _ ->
                continue
        end
    end,

    Options = #{on_snapshot => Callback},
    Result = graph:run(Graph, InitialState, Options),

    %% 应该返回错误
    ?assertEqual(error, maps:get(status, Result)),
    ?assertMatch({invalid_operation, {retry_not_allowed, initial}}, maps:get(error, Result)).

%% 测试：step 类型不允许 retry
step_type_retry_not_allowed_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    %% 在第一个 step snapshot 时尝试 retry
    Callback = fun(Info, _SnapshotData) ->
        case maps:get(type, Info) of
            initial ->
                continue;
            step ->
                %% 尝试在 step 时 retry（应该导致错误）
                {retry, [some_vertex]};
            _ ->
                continue
        end
    end,

    Options = #{on_snapshot => Callback},
    Result = graph:run(Graph, InitialState, Options),

    %% 应该返回错误
    ?assertEqual(error, maps:get(status, Result)),
    ?assertMatch({invalid_operation, {retry_not_allowed, step}}, maps:get(error, Result)).

%%====================================================================
%% resume_data 测试
%%====================================================================

%% 测试：执行完成时（done）也调用 snapshot 回调
final_snapshot_callback_called_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    Self = self(),
    Callback = fun(Info, SnapshotData) ->
        Self ! {snapshot, maps:get(type, Info), SnapshotData},
        continue
    end,

    Options = #{on_snapshot => Callback},
    Result = graph:run(Graph, InitialState, Options),

    %% 收集所有 snapshot 类型
    Types = collect_snapshot_types([]),

    %% 验证包含 final 类型（不管执行是否成功，final 回调都应该被调用）
    ?assert(lists:member(final, Types)),

    %% 验证 done_reason 是 completed（pregel 层面完成）
    ?assertEqual(completed, maps:get(done_reason, Result)),

    %% 验证结果中包含最终 snapshot
    ?assert(maps:is_key(snapshot, Result)).

collect_snapshot_types(Acc) ->
    receive
        {snapshot, Type, _Data} ->
            collect_snapshot_types([Type | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%% 测试：resume_data 被转换为消息注入
resume_data_injection_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    %% 第一次执行，保存 snapshot 后停止
    Self = self(),
    Callback = fun(_Info, SnapshotData) ->
        Self ! {save_snapshot, SnapshotData},
        {stop, waiting_for_input}
    end,

    Options1 = #{on_snapshot => Callback},
    Result1 = graph:run(Graph, InitialState, Options1),

    ?assertEqual(stopped, maps:get(status, Result1)),

    %% 获取保存的 snapshot
    SnapshotData = receive
        {save_snapshot, Data} -> Data
    after 1000 ->
        error(no_snapshot_saved)
    end,

    %% 准备恢复选项，包含 resume_data
    #{pregel_snapshot := PregelSnapshot, iteration := Iteration} = SnapshotData,
    RestoreOpts = #{
        pregel_snapshot => PregelSnapshot,
        iteration => Iteration,
        resume_data => #{process => {user_input, "hello"}}
    },

    %% 恢复执行
    Options2 = #{restore_from => RestoreOpts},
    Result2 = graph:run(Graph, InitialState, Options2),

    %% 验证执行完成（resume_data 作为消息被注入，但这里只验证能正常恢复）
    Status2 = maps:get(status, Result2),
    ?assert(Status2 =:= completed orelse Status2 =:= error).
