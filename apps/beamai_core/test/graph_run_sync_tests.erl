%%%-------------------------------------------------------------------
%%% @doc beamai_graph:run_sync HIL 测试
%%%
%%% 测试场景：
%%% 1. 正常完成
%%% 2. 中断 -> 恢复 -> 完成
%%% 3. 错误返回
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_run_sync_tests).
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
    FailFn = fun(_State, _) ->
        error(intentional_failure)
    end,
    {ok, Graph} = beamai_graph:build([
        {node, fail_node, FailFn},
        {edge, fail_node, '__end__'},
        {entry, fail_node}
    ]),
    Graph.

%%====================================================================
%% 正常完成测试
%%====================================================================

%% 测试：run_sync 正常完成
run_sync_normal_completion_test() ->
    Graph = make_simple_graph(),
    InitialState = beamai_graph:state(#{count => 0}),

    {ok, FinalState} = beamai_graph:run_sync(Graph, InitialState),

    ?assertEqual(1, beamai_graph_engine:state_get(FinalState, count)).

%%====================================================================
%% HIL 测试
%%====================================================================

%% 测试：run_sync 中断 -> 恢复 -> 完成
run_sync_interrupt_resume_test() ->
    Graph = make_interrupt_graph(),
    InitialState = beamai_graph:state(#{}),

    %% 第一次执行：触发中断
    {interrupted, InterruptedVertices, Snapshot} =
        beamai_graph:run_sync(Graph, InitialState),

    %% 验证中断信息
    ?assert(length(InterruptedVertices) >= 1),
    VertexIds = [Id || {Id, _Reason} <- InterruptedVertices],
    ?assert(lists:member(review, VertexIds)),

    %% 验证快照
    ?assert(is_map(Snapshot)),
    ?assertEqual(true, maps:get('__graph_snapshot__', Snapshot)),

    %% 第二次执行：从快照恢复
    {ok, FinalState} = beamai_graph:run_sync(Graph, InitialState, #{
        snapshot => Snapshot,
        resume_data => #{review => approved}
    }),

    %% 验证最终状态
    ?assertEqual(true, beamai_graph_engine:state_get(FinalState, approved)),
    ?assertEqual(true, beamai_graph_engine:state_get(FinalState, processed)).

%%====================================================================
%% 错误测试
%%====================================================================

%% 测试：run_sync 遇到错误
run_sync_error_test() ->
    Graph = make_error_graph(),
    InitialState = beamai_graph:state(#{}),

    {error, FailedVertices} = beamai_graph:run_sync(Graph, InitialState),

    ?assert(is_list(FailedVertices)),
    ?assert(length(FailedVertices) >= 1).
