%%%-------------------------------------------------------------------
%%% @doc Pregel 同步屏障模块
%%%
%%% 提供 BSP（批量同步并行）模型的同步屏障实现。
%%% 负责跟踪超步同步状态和汇总 Worker 结果。
%%%
%%% 全局状态模式（无 inbox 版本）：
%%% - 收集所有 Worker 的 deltas（增量更新）
%%% - 收集所有 Worker 的 activations（替代 outbox）
%%% - deltas 用于合并到 Master 的 global_state
%%% - activations 用于确定下一超步激活哪些顶点
%%%
%%% 功能:
%%%   - 创建和管理同步屏障状态
%%%   - 记录 Worker 完成通知
%%%   - 检测屏障完成条件
%%%   - 汇总超步执行结果（含 deltas、activations、失败和中断信息）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_barrier).

-export([new/1, record_done/2, is_complete/1, get_results/1, reset/2]).
-export([aggregate_results/1]).

%% 类型导出
-export_type([t/0, superstep_results/0]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 同步屏障记录
-record(barrier, {
    expected :: non_neg_integer(),    %% 期望的 Worker 数
    received :: non_neg_integer(),    %% 已收到的完成通知数
    results  :: [map()]               %% Worker 结果列表
}).

-opaque t() :: #barrier{}.

%% Delta 类型（与 pregel_worker 一致）
-type delta() :: #{atom() | binary() => term()}.

%% 超步结果汇总类型（无 inbox 版本）
-type superstep_results() :: #{
    active_count := non_neg_integer(),
    %% 所有 Worker 的 deltas 汇总（用于合并到 global_state）
    deltas := [delta()],
    %% 所有 Worker 的 activations 汇总（替代 outbox，用于下一超步激活）
    activations := [term()],
    failed_count := non_neg_integer(),
    failed_vertices := [{term(), term()}],
    interrupted_count := non_neg_integer(),
    interrupted_vertices := [{term(), term()}]
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 创建新的同步屏障
-spec new(non_neg_integer()) -> t().
new(NumWorkers) ->
    #barrier{
        expected = NumWorkers,
        received = 0,
        results = []
    }.

%% @doc 记录 Worker 完成通知
-spec record_done(map(), t()) -> t().
record_done(Result, #barrier{received = R, results = Rs} = B) ->
    B#barrier{
        received = R + 1,
        results = [Result | Rs]
    }.

%% @doc 检查屏障是否完成
-spec is_complete(t()) -> boolean().
is_complete(#barrier{expected = E, received = R}) ->
    R >= E.

%% @doc 获取所有结果
-spec get_results(t()) -> [map()].
get_results(#barrier{results = Rs}) ->
    Rs.

%% @doc 重置屏障以供下一超步使用
-spec reset(non_neg_integer(), t()) -> t().
reset(NumWorkers, _Barrier) ->
    new(NumWorkers).

%% @doc 汇总所有 Worker 的执行结果
%%
%% 将多个 Worker 的结果合并为单一的超步结果，包括：
%% - 活跃顶点数
%% - deltas 列表（用于合并到 global_state）
%% - activations 列表（用于下一超步激活顶点）
%% - 失败顶点数和列表
%% - 中断顶点数和列表
%%
%% @param Results Worker 结果列表
%% @returns 汇总后的超步结果
-spec aggregate_results([map()]) -> superstep_results().
aggregate_results(Results) ->
    InitAcc = empty_results(),
    lists:foldl(fun merge_worker_result/2, InitAcc, Results).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 创建空的结果结构（无 inbox 版本）
-spec empty_results() -> superstep_results().
empty_results() ->
    #{
        active_count => 0,
        deltas => [],
        activations => [],
        failed_count => 0,
        failed_vertices => [],
        interrupted_count => 0,
        interrupted_vertices => []
    }.

%% @private 合并单个 Worker 的结果到累加器
-spec merge_worker_result(map(), superstep_results()) -> superstep_results().
merge_worker_result(WorkerResult, Acc) ->
    #{
        active_count => maps:get(active_count, Acc) +
                        maps:get(active_count, WorkerResult, 0),
        %% 汇总所有 Worker 的 deltas（用于合并到 global_state）
        deltas => maps:get(deltas, WorkerResult, []) ++
                  maps:get(deltas, Acc),
        %% 汇总所有 Worker 的 activations（替代 outbox）
        activations => maps:get(activations, WorkerResult, []) ++
                       maps:get(activations, Acc),
        failed_count => maps:get(failed_count, Acc) +
                        maps:get(failed_count, WorkerResult, 0),
        failed_vertices => maps:get(failed_vertices, WorkerResult, []) ++
                           maps:get(failed_vertices, Acc),
        interrupted_count => maps:get(interrupted_count, Acc) +
                             maps:get(interrupted_count, WorkerResult, 0),
        interrupted_vertices => maps:get(interrupted_vertices, WorkerResult, []) ++
                                maps:get(interrupted_vertices, Acc)
    }.
