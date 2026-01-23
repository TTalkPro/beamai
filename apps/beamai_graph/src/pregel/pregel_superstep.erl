%%%-------------------------------------------------------------------
%%% @doc Pregel 超步执行模块
%%%
%%% 本模块负责 Pregel BSP（Bulk Synchronous Parallel）模型中超步的执行逻辑。
%%% 从 pregel_master 拆分出来，专注于超步级别的处理。
%%%
%%% 核心职责:
%%% - 完成超步处理：收集 Worker 结果、应用 deltas、更新全局状态
%%% - 构建超步信息：为调用者提供超步执行的详细信息
%%% - 确定 checkpoint 类型：根据执行结果判断是否有错误/中断
%%% - 管理 activations：处理下一超步需要激活的顶点
%%%
%%% 延迟提交机制:
%%% 当超步中有顶点执行失败或被中断时，不会立即应用 deltas 到全局状态，
%%% 而是将其暂存为 pending_deltas。这允许调用者:
%%% 1. 检查失败原因
%%% 2. 决定是否重试
%%% 3. 重试成功后再应用合并后的 deltas
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_superstep).

%% === API 导出 ===
-export([
    complete_superstep/7,
    build_superstep_info/2,
    determine_checkpoint_type/1,
    get_activations_for_superstep/2,
    group_activations_by_worker/2,
    separate_dispatches/1,
    build_vertex_inputs/1,
    group_vertex_inputs_by_worker/2
]).

%% === 类型定义 ===
-type vertex_id() :: pregel_vertex:vertex_id().
-type vertex_inputs() :: #{vertex_id() => [graph_dispatch:dispatch()]}.
-type checkpoint_type() :: initial | step | error | interrupt | final.
-type superstep_info() :: pregel_master:superstep_info().
-type field_reducers() :: pregel_master:field_reducers().
-type delta() :: pregel_master:delta().

-export_type([checkpoint_type/0, vertex_inputs/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 完成超步处理
%%
%% 这是超步执行的核心函数，负责:
%% 1. 汇总所有 Worker 的计算结果
%% 2. 收集 deltas（状态增量）和 activations（下一步激活列表）
%% 3. 检查是否有失败或中断的顶点
%% 4. 根据是否有错误决定：立即应用 deltas 或延迟提交
%% 5. 判断终止条件（所有顶点停止且无待激活顶点，或达到最大超步数）
%% 6. 构建回复消息
%%
%% 延迟提交规则:
%% - 有错误/中断时: deltas 暂存到 pending_deltas，不应用到 global_state
%% - 无错误时: deltas 立即应用到 global_state
%%
%% 返回值是一个 8 元组，包含更新后的所有状态信息。
-spec complete_superstep(
    pregel_barrier:t(),           %% 同步屏障（包含 Worker 结果）
    non_neg_integer(),            %% 当前超步号
    pos_integer(),                %% 最大超步数
    graph_state:state(),          %% 当前全局状态
    field_reducers(),             %% 字段级 reducer 配置
    [{vertex_id(), term()}],      %% 累积的失败顶点列表
    gen_server:from()             %% step 调用者（用于构建回复）
) -> {
    NewGlobalState :: graph_state:state(),           %% 新的全局状态
    NewPendingDeltas :: [delta()] | undefined,       %% 暂存的 deltas（延迟提交时有值）
    NewPendingActivations :: [vertex_id()] | undefined,  %% 暂存的 activations
    NewSuperstep :: non_neg_integer(),               %% 新的超步号
    Halted :: boolean(),                             %% 是否已终止
    Reply :: pregel_master:step_result(),            %% 给调用者的回复
    UpdatedResults :: map(),                         %% 更新后的结果映射
    NewCumulativeFailures :: [{vertex_id(), term()}] %% 累积的失败列表
}.
complete_superstep(Barrier, Superstep, MaxSupersteps, GlobalState,
                   FieldReducers, CumulativeFailures, _StepCaller) ->
    %% ===== 第1步：汇总 Worker 结果 =====
    Results = pregel_barrier:get_results(Barrier),
    AggregatedResults = pregel_barrier:aggregate_results(Results),
    TotalActive = maps:get(active_count, AggregatedResults),

    %% ===== 第2步：收集 deltas 和 activations =====
    AllDeltas = maps:get(deltas, AggregatedResults, []),
    AllActivations = maps:get(activations, AggregatedResults, []),
    TotalActivations = length(AllActivations),

    %% ===== 第3步：检查错误并累积失败信息 =====
    FailedVertices = maps:get(failed_vertices, AggregatedResults, []),
    FailedCount = maps:get(failed_count, AggregatedResults, 0),
    InterruptedCount = maps:get(interrupted_count, AggregatedResults, 0),
    HasError = FailedCount > 0 orelse InterruptedCount > 0,

    %% 累积失败信息（跨超步追踪，用于最终报告）
    NewCumulativeFailures = case FailedVertices of
        [] -> CumulativeFailures;
        _ -> lists:usort(FailedVertices ++ CumulativeFailures)
    end,

    %% ===== 第4步：决定是否延迟提交 =====
    {NewGlobalState, NewPendingDeltas, NewPendingActivations} = case HasError of
        true ->
            %% 延迟提交：暂存 deltas 和 activations，等待重试或处理
            {GlobalState, AllDeltas, AllActivations};
        false ->
            %% 正常提交：使用 field_reducers 合并 deltas 到全局状态
            UpdatedState = graph_state_reducer:apply_deltas(GlobalState, AllDeltas, FieldReducers),
            {UpdatedState, undefined, undefined}
    end,

    %% ===== 第5步：更新结果映射 =====
    UpdatedResults = AggregatedResults#{
        activation_count => TotalActivations,
        superstep => Superstep
    },

    %% ===== 第6步：检查终止条件 =====
    %% 终止条件：(所有顶点停止 且 无待激活顶点 且 无错误) 或 达到最大超步数
    Halted = (TotalActive =:= 0) andalso (TotalActivations =:= 0) andalso (not HasError),
    MaxReached = Superstep >= MaxSupersteps - 1,
    IsDone = Halted orelse MaxReached,

    %% ===== 第7步：确定 checkpoint 类型并构建信息 =====
    Type = case IsDone of
        true -> final;
        false -> determine_checkpoint_type(UpdatedResults)
    end,
    Info = build_superstep_info(Type, UpdatedResults),

    %% ===== 第8步：计算新超步号 =====
    NewSuperstep = if IsDone -> Superstep; true -> Superstep + 1 end,

    %% ===== 第9步：构建回复 =====
    Reply = case IsDone of
        true ->
            Reason = if Halted -> completed; true -> max_supersteps end,
            {done, Reason, Info};
        false ->
            {continue, Info}
    end,

    {NewGlobalState, NewPendingDeltas, NewPendingActivations,
     NewSuperstep, IsDone, Reply, UpdatedResults, NewCumulativeFailures}.

%% @doc 构建超步信息
%%
%% 将超步执行结果打包成标准的 superstep_info 格式。
%% Type 参数指定 checkpoint 类型（initial/step/error/interrupt/final）。
%%
%% 返回的信息包含:
%% - type: checkpoint 类型
%% - superstep: 超步号
%% - active_count: 活跃顶点数
%% - activation_count: 下一步待激活顶点数
%% - failed_count/failed_vertices: 失败顶点信息
%% - interrupted_count/interrupted_vertices: 中断顶点信息
-spec build_superstep_info(checkpoint_type(), map() | undefined) -> superstep_info().
build_superstep_info(Type, undefined) ->
    %% 无结果时返回默认值（用于初始 checkpoint）
    #{
        type => Type,
        superstep => 0,
        active_count => 0,
        activation_count => 0,
        failed_count => 0,
        failed_vertices => [],
        interrupted_count => 0,
        interrupted_vertices => []
    };
build_superstep_info(Type, Results) ->
    %% 从结果中提取信息
    #{
        type => Type,
        superstep => maps:get(superstep, Results, 0),
        active_count => maps:get(active_count, Results, 0),
        activation_count => maps:get(activation_count, Results, 0),
        failed_count => maps:get(failed_count, Results, 0),
        failed_vertices => maps:get(failed_vertices, Results, []),
        interrupted_count => maps:get(interrupted_count, Results, 0),
        interrupted_vertices => maps:get(interrupted_vertices, Results, [])
    }.

%% @doc 根据执行结果判断 checkpoint 类型
%%
%% Checkpoint 类型决定了调用者如何处理当前状态:
%% - interrupt: 有顶点请求中断（human-in-the-loop），优先级最高
%% - error: 有顶点执行失败，需要决定是否重试
%% - step: 正常超步完成，可以继续下一超步
%%
%% 注意: initial 和 final 类型由外层逻辑确定，不在此函数判断。
-spec determine_checkpoint_type(map()) -> checkpoint_type().
determine_checkpoint_type(Results) ->
    InterruptedCount = maps:get(interrupted_count, Results, 0),
    FailedCount = maps:get(failed_count, Results, 0),
    if
        InterruptedCount > 0 -> interrupt;  %% 中断优先级最高
        FailedCount > 0 -> error;
        true -> step
    end.

%% @doc 获取下一超步要激活的顶点列表
%%
%% 激活列表的来源（按优先级）:
%% 1. pending_activations: 延迟提交时暂存的激活列表
%% 2. 上次超步结果中的 activations: 正常执行时的激活列表
%% 3. 空列表: 首次超步时，表示激活所有顶点
-spec get_activations_for_superstep([vertex_id()] | undefined, map() | undefined) ->
    [vertex_id()].
get_activations_for_superstep(PendingActivations, _LastResults) when is_list(PendingActivations) ->
    %% 有 pending_activations 时优先使用（恢复场景或延迟提交）
    PendingActivations;
get_activations_for_superstep(undefined, undefined) ->
    %% 首次超步，返回空列表表示激活所有顶点
    [];
get_activations_for_superstep(undefined, LastResults) ->
    %% 从上次结果获取 activations
    maps:get(activations, LastResults, []).

%% @doc 按目标 Worker 分组激活列表
%%
%% 将 activations 按顶点所属的 Worker 分组。
%% 使用哈希分区策略确保同一顶点始终被分配到同一 Worker。
%%
%% 返回格式: #{WorkerId => [VertexId1, VertexId2, ...]}
-spec group_activations_by_worker([vertex_id()], pos_integer()) ->
    #{non_neg_integer() => [vertex_id()]}.
group_activations_by_worker(Activations, NumWorkers) ->
    lists:foldl(
        fun(VertexId, Acc) ->
            %% 使用哈希分区确定 WorkerId
            WorkerId = pregel_partition:worker_id(VertexId, NumWorkers, hash),
            Existing = maps:get(WorkerId, Acc, []),
            Acc#{WorkerId => [VertexId | Existing]}
        end,
        #{},
        Activations
    ).

%%====================================================================
%% Dispatch 处理
%%====================================================================

%% @doc 从激活列表中分离 dispatch 项
%%
%% 将混合列表分为 dispatch 项和普通 vertex_id 项。
-spec separate_dispatches([term()]) -> {[{dispatch, graph_dispatch:dispatch()}], [vertex_id()]}.
separate_dispatches(Activations) ->
    lists:partition(fun({dispatch, _}) -> true; (_) -> false end, Activations).

%% @doc 将 dispatch 列表转为 VertexInputs 格式
%%
%% 按目标节点分组：#{NodeId => [Dispatch1, Dispatch2, ...]}
-spec build_vertex_inputs([{dispatch, graph_dispatch:dispatch()}]) -> vertex_inputs().
build_vertex_inputs(Dispatches) ->
    lists:foldl(fun({dispatch, D}, Acc) ->
        NodeId = graph_dispatch:get_node(D),
        Existing = maps:get(NodeId, Acc, []),
        Acc#{NodeId => Existing ++ [D]}
    end, #{}, Dispatches).

%% @doc VertexInputs 按 Worker 分组
%%
%% 将 VertexInputs 按顶点所属的 Worker 分组。
-spec group_vertex_inputs_by_worker(vertex_inputs(), pos_integer()) -> #{non_neg_integer() => vertex_inputs()}.
group_vertex_inputs_by_worker(VertexInputs, NumWorkers) ->
    maps:fold(fun(NodeId, Dispatches, Acc) ->
        WorkerId = pregel_partition:worker_id(NodeId, NumWorkers, hash),
        WorkerVI = maps:get(WorkerId, Acc, #{}),
        Acc#{WorkerId => WorkerVI#{NodeId => Dispatches}}
    end, #{}, VertexInputs).
