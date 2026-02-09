%%%-------------------------------------------------------------------
%%% @doc beamai_graph_runtime 单元测试
%%%
%%% 测试场景：
%%% 1. 启动/停止生命周期
%%% 2. 自动运行到完成 + caller 通知
%%% 3. interrupt -> resume 流程
%%% 4. error -> retry 流程
%%% 5. 快照获取
%%% 6. 状态查询
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_runtime_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

make_simple_graph() ->
    ProcessFn = fun(State, _) ->
        Count = beamai_graph_engine:state_get(State, count, 0),
        {ok, beamai_graph_engine:state_set(State, count, Count + 1)}
    end,
    {ok, Graph} = beamai_graph:build([
        {node, process, ProcessFn},
        {edge, process, '__end__'},
        {entry, process}
    ]),
    Graph.

make_interrupt_graph() ->
    ProcessFn = fun(State, _) ->
        {ok, beamai_graph_engine:state_set(State, processed, true)}
    end,
    ReviewFn = fun
        (State, _Input, undefined) ->
            {interrupt, need_approval, State};
        (State, _Input, _ResumeData) ->
            {ok, beamai_graph_engine:state_set(State, approved, true)}
    end,
    {ok, Graph} = beamai_graph:build([
        {node, process, ProcessFn},
        {edge, process, review},
        {node, review, ReviewFn},
        {edge, review, '__end__'},
        {entry, process}
    ]),
    Graph.

make_error_graph() ->
    CounterRef = atomics:new(1, [{signed, true}]),
    FlakyFn = fun(State, _) ->
        Count = atomics:add_get(CounterRef, 1, 1),
        case Count of
            1 -> error(first_attempt_fails);
            _ -> {ok, beamai_graph_engine:state_set(State, fixed, true)}
        end
    end,
    {ok, Graph} = beamai_graph:build([
        {node, flaky, FlakyFn},
        {edge, flaky, '__end__'},
        {entry, flaky}
    ]),
    Graph.

%%====================================================================
%% 生命周期测试
%%====================================================================

%% 测试：启动并自动运行到完成
auto_run_to_completion_test() ->
    Graph = make_simple_graph(),
    InitialState = beamai_graph:state(#{count => 0}),

    {ok, Pid} = beamai_graph_runtime:start_link(Graph, InitialState,
        #{caller => self()}),

    %% 等待完成通知
    receive
        {graph_completed, Pid, FinalState} ->
            ?assertEqual(1, beamai_graph_engine:state_get(FinalState, count))
    after 5000 ->
        ?assert(false)
    end,

    beamai_graph_runtime:stop(Pid).

%%====================================================================
%% 状态查询测试
%%====================================================================

%% 测试：完成后获取状态
get_status_after_completion_test() ->
    Graph = make_simple_graph(),
    InitialState = beamai_graph:state(#{count => 0}),

    {ok, Pid} = beamai_graph_runtime:start_link(Graph, InitialState,
        #{caller => self()}),

    receive
        {graph_completed, Pid, _} -> ok
    after 5000 -> ?assert(false)
    end,

    {ok, Status} = beamai_graph_runtime:get_status(Pid),
    ?assertEqual(completed, maps:get(current_state, Status)),

    beamai_graph_runtime:stop(Pid).

%%====================================================================
%% Interrupt + Resume 测试
%%====================================================================

%% 测试：中断 -> 通知 caller -> resume -> 完成
interrupt_resume_test() ->
    Graph = make_interrupt_graph(),
    InitialState = beamai_graph:state(#{}),

    {ok, Pid} = beamai_graph_runtime:start_link(Graph, InitialState,
        #{caller => self()}),

    %% 等待中断通知
    receive
        {graph_interrupted, Pid, Info} ->
            ?assertEqual(interrupt, maps:get(type, Info))
    after 5000 ->
        ?assert(false)
    end,

    %% 验证状态
    {ok, Status} = beamai_graph_runtime:get_status(Pid),
    ?assertEqual(interrupted, maps:get(current_state, Status)),

    %% 恢复
    ok = beamai_graph_runtime:resume(Pid, #{review => approved}),

    %% 等待完成通知
    receive
        {graph_completed, Pid, FinalState} ->
            ?assertEqual(true, beamai_graph_engine:state_get(FinalState, approved))
    after 5000 ->
        ?assert(false)
    end,

    beamai_graph_runtime:stop(Pid).

%%====================================================================
%% Error + Retry 测试
%%====================================================================

%% 测试：错误 -> 通知 caller -> retry -> 完成
error_retry_test() ->
    Graph = make_error_graph(),
    InitialState = beamai_graph:state(#{}),

    {ok, Pid} = beamai_graph_runtime:start_link(Graph, InitialState,
        #{caller => self()}),

    %% 等待错误通知
    receive
        {graph_error, Pid, Info} ->
            ?assertEqual(error, maps:get(type, Info))
    after 5000 ->
        ?assert(false)
    end,

    %% 验证状态
    {ok, Status} = beamai_graph_runtime:get_status(Pid),
    ?assertEqual(error, maps:get(current_state, Status)),

    %% 重试
    ok = beamai_graph_runtime:retry(Pid, [flaky]),

    %% 等待完成通知
    receive
        {graph_completed, Pid, FinalState} ->
            ?assertEqual(true, beamai_graph_engine:state_get(FinalState, fixed))
    after 5000 ->
        ?assert(false)
    end,

    beamai_graph_runtime:stop(Pid).

%%====================================================================
%% Snapshot 测试
%%====================================================================

%% 测试：中断时获取快照
snapshot_on_interrupt_test() ->
    Graph = make_interrupt_graph(),
    InitialState = beamai_graph:state(#{}),

    {ok, Pid} = beamai_graph_runtime:start_link(Graph, InitialState,
        #{caller => self()}),

    %% 等待中断
    receive
        {graph_interrupted, Pid, _} -> ok
    after 5000 -> ?assert(false)
    end,

    %% 获取快照
    {ok, Snapshot} = beamai_graph_runtime:snapshot(Pid),
    ?assert(is_map(Snapshot)),
    ?assertEqual(true, maps:get('__graph_snapshot__', Snapshot)),

    beamai_graph_runtime:stop(Pid).
