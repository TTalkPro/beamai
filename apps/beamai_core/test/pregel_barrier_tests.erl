%%%-------------------------------------------------------------------
%%% @doc pregel_barrier 单元测试
%%%
%%% 测试 pregel_barrier 的同步屏障功能：
%%% - 屏障创建和状态管理
%%% - Worker 结果汇总（含失败和中断信息）
%%% - 全局状态模式：delta 和 activations 收集
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_barrier_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 屏障基本功能测试
%%====================================================================

%% 测试：创建新屏障
new_barrier_test() ->
    Barrier = pregel_barrier:new(3),
    ?assertEqual(false, pregel_barrier:is_complete(Barrier)),
    ?assertEqual([], pregel_barrier:get_results(Barrier)).

%% 测试：记录 Worker 完成
record_done_test() ->
    Barrier0 = pregel_barrier:new(2),
    Result1 = #{worker_id => 0, active_count => 5, activations => [v1, v2]},
    Barrier1 = pregel_barrier:record_done(Result1, Barrier0),
    ?assertEqual(false, pregel_barrier:is_complete(Barrier1)),

    Result2 = #{worker_id => 1, active_count => 3, activations => [v3]},
    Barrier2 = pregel_barrier:record_done(Result2, Barrier1),
    ?assertEqual(true, pregel_barrier:is_complete(Barrier2)),
    ?assertEqual(2, length(pregel_barrier:get_results(Barrier2))).

%% 测试：重置屏障
reset_barrier_test() ->
    Barrier0 = pregel_barrier:new(2),
    Result = #{worker_id => 0, active_count => 5, activations => [v1]},
    Barrier1 = pregel_barrier:record_done(Result, Barrier0),

    Barrier2 = pregel_barrier:reset(3, Barrier1),
    ?assertEqual(false, pregel_barrier:is_complete(Barrier2)),
    ?assertEqual([], pregel_barrier:get_results(Barrier2)).

%%====================================================================
%% aggregate_results 测试
%%====================================================================

%% 测试：汇总空结果
aggregate_empty_results_test() ->
    Result = pregel_barrier:aggregate_results([]),
    ?assertEqual(0, maps:get(active_count, Result)),
    ?assertEqual([], maps:get(activations, Result)),
    ?assertEqual(0, maps:get(failed_count, Result)),
    ?assertEqual([], maps:get(failed_vertices, Result)),
    ?assertEqual(0, maps:get(interrupted_count, Result)),
    ?assertEqual([], maps:get(interrupted_vertices, Result)).

%% 测试：汇总正常结果（无失败、无中断）
aggregate_normal_results_test() ->
    Results = [
        #{worker_id => 0, active_count => 5, activations => [v1, v2],
          failed_count => 0, failed_vertices => [],
          interrupted_count => 0, interrupted_vertices => []},
        #{worker_id => 1, active_count => 3, activations => [v3],
          failed_count => 0, failed_vertices => [],
          interrupted_count => 0, interrupted_vertices => []}
    ],
    Result = pregel_barrier:aggregate_results(Results),
    ?assertEqual(8, maps:get(active_count, Result)),
    Activations = maps:get(activations, Result),
    ?assertEqual(3, length(Activations)),
    ?assert(lists:member(v1, Activations)),
    ?assert(lists:member(v2, Activations)),
    ?assert(lists:member(v3, Activations)),
    ?assertEqual(0, maps:get(failed_count, Result)),
    ?assertEqual([], maps:get(failed_vertices, Result)),
    ?assertEqual(0, maps:get(interrupted_count, Result)),
    ?assertEqual([], maps:get(interrupted_vertices, Result)).

%% 测试：汇总含失败的结果
aggregate_with_failures_test() ->
    Results = [
        #{worker_id => 0, active_count => 5, activations => [v4],
          failed_count => 1, failed_vertices => [{v1, error1}],
          interrupted_count => 0, interrupted_vertices => []},
        #{worker_id => 1, active_count => 3, activations => [v5, v6],
          failed_count => 2, failed_vertices => [{v2, error2}, {v3, error3}],
          interrupted_count => 0, interrupted_vertices => []}
    ],
    Result = pregel_barrier:aggregate_results(Results),
    ?assertEqual(8, maps:get(active_count, Result)),
    ?assertEqual(3, length(maps:get(activations, Result))),
    ?assertEqual(3, maps:get(failed_count, Result)),
    FailedVertices = maps:get(failed_vertices, Result),
    ?assertEqual(3, length(FailedVertices)),
    FailedIds = [Id || {Id, _} <- FailedVertices],
    ?assert(lists:member(v1, FailedIds)),
    ?assert(lists:member(v2, FailedIds)),
    ?assert(lists:member(v3, FailedIds)).

%% 测试：汇总含中断的结果
aggregate_with_interrupts_test() ->
    Results = [
        #{worker_id => 0, active_count => 5, activations => [v3],
          failed_count => 0, failed_vertices => [],
          interrupted_count => 1, interrupted_vertices => [{v1, need_input}]},
        #{worker_id => 1, active_count => 3, activations => [v4],
          failed_count => 0, failed_vertices => [],
          interrupted_count => 1, interrupted_vertices => [{v2, need_approval}]}
    ],
    Result = pregel_barrier:aggregate_results(Results),
    ?assertEqual(8, maps:get(active_count, Result)),
    ?assertEqual(2, length(maps:get(activations, Result))),
    ?assertEqual(0, maps:get(failed_count, Result)),
    ?assertEqual(2, maps:get(interrupted_count, Result)),
    InterruptedVertices = maps:get(interrupted_vertices, Result),
    ?assertEqual(2, length(InterruptedVertices)),
    InterruptedIds = [Id || {Id, _} <- InterruptedVertices],
    ?assert(lists:member(v1, InterruptedIds)),
    ?assert(lists:member(v2, InterruptedIds)).

%% 测试：汇总混合结果（失败+中断）
aggregate_mixed_results_test() ->
    Results = [
        #{worker_id => 0, active_count => 5, activations => [v4],
          failed_count => 1, failed_vertices => [{v1, error1}],
          interrupted_count => 1, interrupted_vertices => [{v2, need_input}]},
        #{worker_id => 1, active_count => 3, activations => [v5],
          failed_count => 1, failed_vertices => [{v3, error2}],
          interrupted_count => 0, interrupted_vertices => []}
    ],
    Result = pregel_barrier:aggregate_results(Results),
    ?assertEqual(8, maps:get(active_count, Result)),
    ?assertEqual(2, length(maps:get(activations, Result))),
    ?assertEqual(2, maps:get(failed_count, Result)),
    ?assertEqual(2, length(maps:get(failed_vertices, Result))),
    ?assertEqual(1, maps:get(interrupted_count, Result)),
    ?assertEqual(1, length(maps:get(interrupted_vertices, Result))).

%% 测试：汇总结果缺少字段时使用默认值
aggregate_with_missing_fields_test() ->
    %% 模拟只有 active_count 和 activations 的结果
    Results = [
        #{worker_id => 0, active_count => 5, activations => [v1, v2]},
        #{worker_id => 1, active_count => 3, activations => [v3]}
    ],
    Result = pregel_barrier:aggregate_results(Results),
    ?assertEqual(8, maps:get(active_count, Result)),
    ?assertEqual(3, length(maps:get(activations, Result))),
    %% 缺少的字段应使用默认值
    ?assertEqual(0, maps:get(failed_count, Result)),
    ?assertEqual([], maps:get(failed_vertices, Result)),
    ?assertEqual(0, maps:get(interrupted_count, Result)),
    ?assertEqual([], maps:get(interrupted_vertices, Result)).
