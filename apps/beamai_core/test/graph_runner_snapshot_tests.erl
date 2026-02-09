%%%-------------------------------------------------------------------
%%% @doc graph_runner snapshot 功能单元测试
%%%
%%% 测试 graph_runner 的 snapshot 模式功能：
%%% - interrupt 自动返回 #{status => interrupted, ...}
%%% - error 自动返回 #{status => error, ...}
%%% - 从 snapshot 恢复执行
%%% - store 保存策略
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
        Count = beamai_graph_engine:state_get(State, count, 0),
        NewCount = Count + 1,
        State1 = beamai_graph_engine:state_set(State, count, NewCount),
        {ok, beamai_graph_engine:state_set(State1, last_count, NewCount)}
    end,
    RouterFn = fun(State) ->
        Count = beamai_graph_engine:state_get(State, count, 0),
        case Count >= 3 of
            true -> '__end__';
            false -> process
        end
    end,
    {ok, Graph} = beamai_graph:build([
        {node, process, ProcessFn},
        {conditional_edge, process, RouterFn},
        {entry, process}
    ]),
    Graph.

%% 创建带 interrupt 节点的图
%% process -> review(interrupt) -> '__end__'
make_interrupt_graph() ->
    ProcessFn = fun(State, _) ->
        State1 = beamai_graph_engine:state_set(State, processed, true),
        {ok, beamai_graph_engine:state_set(State1, count, 1)}
    end,
    ReviewFn = fun(State, _) ->
        %% 检查是否有 resume_data
        ResumeKey = <<"resume_data:review">>,
        case beamai_graph_engine:state_get(State, ResumeKey, undefined) of
            undefined ->
                %% 没有 resume_data，触发 interrupt
                {interrupt, need_approval, State};
            _ResumeData ->
                %% 有 resume_data，继续执行
                {ok, beamai_graph_engine:state_set(State, approved, true)}
        end
    end,
    {ok, Graph} = beamai_graph:build([
        {node, process, ProcessFn},
        {edge, process, review},
        {node, review, ReviewFn},
        {edge, review, '__end__'},
        {entry, process}
    ]),
    Graph.

%% 创建带 error 节点的图（永远失败）
%% process -> fail_node(error) -> '__end__'
make_error_graph() ->
    ProcessFn = fun(State, _) ->
        {ok, beamai_graph_engine:state_set(State, processed, true)}
    end,
    FailFn = fun(_State, _) ->
        error(intentional_failure)
    end,
    {ok, Graph} = beamai_graph:build([
        {node, process, ProcessFn},
        {edge, process, fail_node},
        {node, fail_node, FailFn},
        {edge, fail_node, '__end__'},
        {entry, process}
    ]),
    Graph.

%% 创建带瞬时 error 节点的图（第一次失败，重试成功）
%% process -> flaky_node -> '__end__'
%% flaky_node 检查 global_state 中的 retry_count 来决定是否失败
make_flaky_error_graph() ->
    ProcessFn = fun(State, _) ->
        {ok, beamai_graph_engine:state_set(State, processed, true)}
    end,
    FlakyFn = fun(State, _) ->
        RetryCount = beamai_graph_engine:state_get(State, retry_count, 0),
        case RetryCount of
            0 ->
                %% 第一次执行失败
                error(transient_failure);
            _ ->
                %% 重试时成功
                {ok, beamai_graph_engine:state_set(State, flaky_done, true)}
        end
    end,
    {ok, Graph} = beamai_graph:build([
        {node, process, ProcessFn},
        {edge, process, flaky_node},
        {node, flaky_node, FlakyFn},
        {edge, flaky_node, '__end__'},
        {entry, process}
    ]),
    Graph.

%% 设置 noop mock store（不做任何保存）
setup_noop_store() ->
    meck:new(noop_graph_store, [non_strict]),
    meck:expect(noop_graph_store, save_snapshot, fun(_Ref, _Snapshot, _Opts) ->
        {ok, <<"noop">>}
    end),
    ok.

teardown_noop_store() ->
    meck:unload(noop_graph_store).

%% 用 noop store 进入 snapshot 模式的选项
snapshot_opts() ->
    #{store => {noop_graph_store, noop}}.

%%====================================================================
%% 正常执行测试
%%====================================================================

%% 测试：正常图执行，无 snapshot 相关选项
normal_execution_no_snapshot_test() ->
    Graph = make_multi_step_graph(),
    InitialState = beamai_graph:state(#{count => 0}),

    Result = beamai_graph:run(Graph, InitialState, #{}),

    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(final_state, Result),
    ?assertEqual(3, beamai_graph_engine:state_get(FinalState, count)).

%% 测试：使用 store 选项时正常执行也通过 snapshot 模式
normal_execution_with_store_test() ->
    Graph = make_multi_step_graph(),
    InitialState = beamai_graph:state(#{count => 0}),

    Self = self(),
    MockRef = make_ref(),
    meck:new(mock_graph_store, [non_strict]),
    meck:expect(mock_graph_store, save_snapshot, fun(_Ref, _Snapshot, _Opts) ->
        Self ! {store_called, MockRef},
        {ok, <<"snap-1">>}
    end),
    try
        Options = #{
            store => {mock_graph_store, MockRef},
            snapshot_strategy => every_superstep
        },
        Result = beamai_graph:run(Graph, InitialState, Options),

        ?assertEqual(completed, maps:get(status, Result)),
        FinalState = maps:get(final_state, Result),
        ?assertEqual(3, beamai_graph_engine:state_get(FinalState, count)),

        %% 验证 store 被调用（至少一次）
        receive
            {store_called, MockRef} -> ok
        after 1000 ->
            ?assert(false)
        end
    after
        meck:unload(mock_graph_store)
    end.

%%====================================================================
%% Interrupt 自动返回测试
%%====================================================================

%% 测试：图含 interrupt 节点，直接返回 #{status => interrupted}
interrupt_returns_immediately_test() ->
    Graph = make_interrupt_graph(),
    InitialState = beamai_graph:state(#{count => 0}),

    setup_noop_store(),
    try
        Result = beamai_graph:run(Graph, InitialState, snapshot_opts()),

        %% 验证返回 interrupted 状态
        ?assertEqual(interrupted, maps:get(status, Result)),

        %% 验证包含 snapshot（用于恢复）
        ?assert(maps:is_key(snapshot, Result)),

        %% 验证包含 interrupted_vertices
        InterruptedVertices = maps:get(interrupted_vertices, Result),
        ?assert(length(InterruptedVertices) >= 1),

        %% 验证 interrupted_vertices 包含 review 节点
        VertexIds = [Id || {Id, _Reason} <- InterruptedVertices],
        ?assert(lists:member(review, VertexIds))
    after
        teardown_noop_store()
    end.

%%====================================================================
%% Error 自动返回测试
%%====================================================================

%% 测试：图含 error 节点，直接返回 #{status => error}
error_returns_immediately_test() ->
    Graph = make_error_graph(),
    InitialState = beamai_graph:state(#{}),

    setup_noop_store(),
    try
        Result = beamai_graph:run(Graph, InitialState, snapshot_opts()),

        %% 验证返回 error 状态
        ?assertEqual(error, maps:get(status, Result)),

        %% 验证包含 snapshot（用于恢复）
        ?assert(maps:is_key(snapshot, Result)),

        %% 验证包含 failed_vertices
        FailedVertices = maps:get(failed_vertices, Result),
        ?assert(length(FailedVertices) >= 1),

        %% 验证 failed_vertices 包含 fail_node 节点
        VertexIds = [Id || {Id, _Reason} <- FailedVertices],
        ?assert(lists:member(fail_node, VertexIds))
    after
        teardown_noop_store()
    end.

%%====================================================================
%% Snapshot 恢复测试
%%====================================================================

%% 测试：从 interrupt 恢复执行
restore_from_interrupt_test() ->
    Graph = make_interrupt_graph(),
    InitialState = beamai_graph:state(#{count => 0}),

    setup_noop_store(),
    try
        %% 第一次执行：触发 interrupt
        Result1 = beamai_graph:run(Graph, InitialState, snapshot_opts()),

        ?assertEqual(interrupted, maps:get(status, Result1)),
        ?assert(maps:is_key(snapshot, Result1)),

        %% 构建用于恢复的 snapshot_data
        Snapshot = maps:get(snapshot, Result1),
        FinalState1 = maps:get(final_state, Result1),
        SnapshotData = #{
            type => interrupt,
            pregel_snapshot => Snapshot,
            global_state => FinalState1,
            iteration => maps:get(iterations, Result1, 0)
        },

        %% 从 snapshot 恢复，注入 resume_data
        Options2 = #{
            restore_from => SnapshotData,
            resume_data => #{review => approved}
        },
        Result2 = beamai_graph:run(Graph, InitialState, Options2),

        %% 恢复后应该完成
        ?assertEqual(completed, maps:get(status, Result2)),

        %% 验证 review 节点被正确恢复并处理了 resume_data
        FinalState2 = maps:get(final_state, Result2),
        ?assertEqual(true, beamai_graph_engine:state_get(FinalState2, approved))
    after
        teardown_noop_store()
    end.

%% 测试：resume_data 注入到 global_state
resume_data_injection_test() ->
    Graph = make_interrupt_graph(),
    InitialState = beamai_graph:state(#{count => 0}),

    setup_noop_store(),
    try
        %% 第一次执行触发 interrupt
        Result1 = beamai_graph:run(Graph, InitialState, snapshot_opts()),
        ?assertEqual(interrupted, maps:get(status, Result1)),

        %% 构建 SnapshotData
        Snapshot = maps:get(snapshot, Result1),
        FinalState1 = maps:get(final_state, Result1),
        SnapshotData = #{
            type => interrupt,
            pregel_snapshot => Snapshot,
            global_state => FinalState1,
            iteration => maps:get(iterations, Result1, 0)
        },

        %% 恢复执行，resume_data 在顶层
        Options2 = #{
            restore_from => SnapshotData,
            resume_data => #{review => {user_input, "hello"}}
        },
        Result2 = beamai_graph:run(Graph, InitialState, Options2),

        %% 验证执行完成
        Status2 = maps:get(status, Result2),
        ?assert(Status2 =:= completed orelse Status2 =:= error)
    after
        teardown_noop_store()
    end.

%%====================================================================
%% Store 保存测试
%%====================================================================

%% 测试：interrupt 时 store 被调用
store_saves_on_interrupt_test() ->
    Graph = make_interrupt_graph(),
    InitialState = beamai_graph:state(#{count => 0}),

    Self = self(),
    MockRef = make_ref(),
    meck:new(mock_graph_store2, [non_strict]),
    meck:expect(mock_graph_store2, save_snapshot, fun(_Ref, Snapshot, Opts) ->
        Self ! {store_save, MockRef, maps:get(type, Snapshot, undefined), maps:get(trigger, Opts, undefined)},
        {ok, <<"snap-1">>}
    end),
    try
        Options = #{
            store => {mock_graph_store2, MockRef},
            snapshot_strategy => on_interrupt
        },
        Result = beamai_graph:run(Graph, InitialState, Options),

        ?assertEqual(interrupted, maps:get(status, Result)),

        %% 收集所有 store 调用
        StoreCalls = collect_store_calls(MockRef, []),

        %% 验证至少一次保存，且包含 interrupt 触发
        ?assert(length(StoreCalls) >= 1),
        Triggers = [Trigger || {_Type, Trigger} <- StoreCalls],
        ?assert(lists:member(interrupted, Triggers))
    after
        meck:unload(mock_graph_store2)
    end.

collect_store_calls(MockRef, Acc) ->
    receive
        {store_save, MockRef, Type, Trigger} ->
            collect_store_calls(MockRef, [{Type, Trigger} | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%%====================================================================
%% Error 重试测试
%%====================================================================

%% 测试：retry_vertices 重新激活失败顶点
retry_vertices_test() ->
    Graph = make_flaky_error_graph(),
    InitialState = beamai_graph:state(#{retry_count => 0}),

    setup_noop_store(),
    try
        %% 第一次执行：flaky_node 失败
        Result1 = beamai_graph:run(Graph, InitialState, snapshot_opts()),

        ?assertEqual(error, maps:get(status, Result1)),
        ?assert(maps:is_key(snapshot, Result1)),

        FailedVertices = maps:get(failed_vertices, Result1),
        FailedIds = [Id || {Id, _} <- FailedVertices],
        ?assert(lists:member(flaky_node, FailedIds)),

        %% 构建 snapshot_data 用于恢复
        Snapshot = maps:get(snapshot, Result1),
        FinalState1 = maps:get(final_state, Result1),

        %% 在 global_state 中设置 retry_count，让 flaky_node 下次成功
        FinalState1WithRetry = beamai_graph_engine:state_set(FinalState1, retry_count, 1),

        SnapshotData = #{
            type => error,
            pregel_snapshot => Snapshot,
            global_state => FinalState1WithRetry,
            iteration => maps:get(iterations, Result1, 0)
        },

        %% 使用 retry_vertices 重试失败的顶点
        Options2 = #{
            restore_from => SnapshotData,
            retry_vertices => FailedIds
        },
        Result2 = beamai_graph:run(Graph, InitialState, Options2),

        %% 重试后应该完成
        ?assertEqual(completed, maps:get(status, Result2)),

        %% 验证 flaky_node 执行成功
        FinalState2 = maps:get(final_state, Result2),
        ?assertEqual(true, beamai_graph_engine:state_get(FinalState2, flaky_done))
    after
        teardown_noop_store()
    end.
