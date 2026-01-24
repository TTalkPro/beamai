%%%-------------------------------------------------------------------
%%% @doc Pregel 重试逻辑模块
%%%
%%% 本模块负责处理顶点重试和延迟提交的逻辑。
%%% 从 pregel_master 拆分出来，专注于重试级别的处理。
%%%
%%% 核心职责:
%%% - 执行顶点重试：当顶点执行失败时，可以请求重试
%%% - 执行延迟提交重试：合并 pending_deltas 和新的重试结果
%%% - 收集重试失败信息：追踪哪些顶点仍然失败
%%% - 合并 pending deltas：将重试产生的 deltas 与暂存的 deltas 合并
%%%
%%% 重试机制说明:
%%% 当超步执行中有顶点失败时，调用者可以选择:
%%% 1. 直接停止执行并返回错误
%%% 2. 调用 retry 重新执行失败的顶点
%%%
%%% 延迟提交与重试:
%%% - 失败时，deltas 被暂存到 pending_deltas
%%% - 重试时，会重新执行指定顶点
%%% - 重试成功后，pending_deltas + 重试 deltas 一起应用到全局状态
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_retry).

%% === API 导出 ===
-export([
    execute_retry/4,
    execute_deferred_retry/5,
    collect_retry_failures/1,
    get_retried_vertex_ids/1,
    merge_pending_deltas/3
]).

%% === 类型定义 ===
-type vertex_id() :: pregel_vertex:vertex_id().
-type field_reducers() :: pregel_master:field_reducers().
-type delta() :: pregel_master:delta().

%%====================================================================
%% API
%%====================================================================

%% @doc 执行顶点重试（无 pending_deltas 时）
%%
%% 当顶点执行失败且没有 pending_deltas 时调用此函数。
%% 会向所有 Worker 发送重试请求，重新执行指定的顶点。
%%
%% 返回值包含:
%% - RetryDeltas: 重试产生的状态增量
%% - RetryActivations: 重试后需要激活的顶点
%% - StillFailed: 仍然失败的顶点列表
%% - StillInterrupted: 仍然中断的顶点列表
%% - RetryResults: 原始重试结果（用于进一步分析）
-spec execute_retry(
    #{non_neg_integer() => pid()},  %% Worker 进程映射
    [vertex_id()],                   %% 要重试的顶点 ID 列表
    graph_state:state(),             %% 当前全局状态
    field_reducers()                 %% 字段级 reducer 配置
) -> {
    RetryDeltas :: [delta()],
    RetryActivations :: [vertex_id()],
    StillFailed :: [{vertex_id(), term()}],
    StillInterrupted :: [{vertex_id(), term()}],
    RetryResults :: [pregel_worker:retry_result()]
}.
execute_retry(Workers, VertexIds, _GlobalState, _FieldReducers) ->
    %% 向所有 Workers 发送重试请求
    %% 每个 Worker 会检查指定的顶点是否在自己管理范围内
    RetryResults = maps:fold(
        fun(_WorkerId, Pid, Acc) ->
            case pregel_worker:retry_vertices(Pid, VertexIds) of
                {ok, Result} -> [Result | Acc];
                _ -> Acc
            end
        end,
        [],
        Workers
    ),

    %% 收集重试产生的 deltas 和 activations
    RetryDeltas = lists:flatmap(
        fun(R) -> maps:get(deltas, R, []) end,
        RetryResults
    ),
    RetryActivations = lists:flatmap(
        fun(R) -> maps:get(activations, R, []) end,
        RetryResults
    ),

    %% 检查是否仍有失败的顶点
    {StillFailed, StillInterrupted} = collect_retry_failures(RetryResults),

    {RetryDeltas, RetryActivations, StillFailed, StillInterrupted, RetryResults}.

%% @doc 执行延迟提交重试
%%
%% 当存在 pending_deltas（之前的 deltas 未提交）时调用此函数。
%% 会将重试产生的新 deltas 与 pending_deltas 合并。
%%
%% 合并策略:
%% - 重试顶点的新 delta 会追加到 pending_deltas
%% - 最终应用时，后面的 delta 会覆盖前面的（last-write-win）
%%
%% 返回值包含合并后的 deltas 和 activations。
-spec execute_deferred_retry(
    #{non_neg_integer() => pid()},  %% Worker 进程映射
    [vertex_id()],                   %% 要重试的顶点 ID 列表
    [delta()] | undefined,           %% 暂存的 deltas
    [vertex_id()] | undefined,       %% 暂存的 activations
    graph_state:state()              %% 当前全局状态
) -> {
    MergedDeltas :: [delta()],
    MergedActivations :: [vertex_id()],
    StillFailed :: [{vertex_id(), term()}],
    StillInterrupted :: [{vertex_id(), term()}],
    RetryResults :: [pregel_worker:retry_result()]
}.
execute_deferred_retry(Workers, VertexIds, PendingDeltas, PendingActivations, _GlobalState) ->
    %% 向所有 Workers 发送重试请求
    RetryResults = maps:fold(
        fun(_WorkerId, Pid, Acc) ->
            case pregel_worker:retry_vertices(Pid, VertexIds) of
                {ok, Result} -> [Result | Acc];
                _ -> Acc
            end
        end,
        [],
        Workers
    ),

    %% 收集重试产生的新 deltas 和 activations
    RetryDeltas = lists:flatmap(
        fun(R) -> maps:get(deltas, R, []) end,
        RetryResults
    ),
    RetryActivations = lists:flatmap(
        fun(R) -> maps:get(activations, R, []) end,
        RetryResults
    ),

    %% 合并 deltas（新 deltas 追加到 pending_deltas 后面）
    RetriedVertexIds = get_retried_vertex_ids(RetryResults),
    SafePendingDeltas = case PendingDeltas of
        undefined -> [];
        _ -> PendingDeltas
    end,
    MergedDeltas = merge_pending_deltas(SafePendingDeltas, RetryDeltas, RetriedVertexIds),

    %% 合并 activations
    CurrentPendingActivations = case PendingActivations of
        undefined -> [];
        _ -> PendingActivations
    end,
    MergedActivations = CurrentPendingActivations ++ RetryActivations,

    %% 检查是否仍有失败的顶点
    {StillFailed, StillInterrupted} = collect_retry_failures(RetryResults),

    {MergedDeltas, MergedActivations, StillFailed, StillInterrupted, RetryResults}.

%% @doc 收集重试失败信息
%%
%% 从重试结果中提取仍然失败和中断的顶点。
%% 返回两个列表: (失败顶点, 中断顶点)，每个元素是 {VertexId, Reason} 元组。
-spec collect_retry_failures([pregel_worker:retry_result()]) ->
    {[{vertex_id(), term()}], [{vertex_id(), term()}]}.
collect_retry_failures(RetryResults) ->
    lists:foldl(
        fun(R, {FAcc, IAcc}) ->
            %% 合并失败列表和中断列表
            {maps:get(failed_vertices, R, []) ++ FAcc,
             maps:get(interrupted_vertices, R, []) ++ IAcc}
        end,
        {[], []},
        RetryResults
    ).

%% @doc 获取重试的顶点 ID 列表
%%
%% 从重试结果中提取实际被重试的顶点 ID。
%% 用于在合并 deltas 时确定哪些 deltas 需要被替换。
-spec get_retried_vertex_ids([pregel_worker:retry_result()]) -> [vertex_id()].
get_retried_vertex_ids(RetryResults) ->
    lists:flatmap(
        fun(R) ->
            Vertices = maps:get(vertices, R, #{}),
            maps:keys(Vertices)
        end,
        RetryResults
    ).

%% @doc 合并 pending deltas 和重试 deltas
%%
%% 将重试产生的新 deltas 追加到 pending_deltas 后面。
%% 这样在最终应用时，新的 deltas 会覆盖旧的（last-write-win 语义）。
%%
%% 注意: RetriedVertexIds 参数目前未使用，保留用于未来的优化
%% （如按顶点 ID 精确替换 delta）。
-spec merge_pending_deltas([delta()], [delta()], [vertex_id()]) -> [delta()].
merge_pending_deltas(PendingDeltas, RetryDeltas, _RetriedVertexIds) ->
    %% 简单实现：直接追加新的 deltas
    %% 因为 deltas 是增量，后应用的会覆盖先应用的
    PendingDeltas ++ RetryDeltas.
