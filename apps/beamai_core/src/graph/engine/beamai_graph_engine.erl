%%%-------------------------------------------------------------------
%%% @doc 图执行引擎 - 纯函数核心
%%%
%%% 从 graph_runner.erl 提取的纯函数核心模块。
%%% 不包含 gen_server，仅提供纯函数 API。
%%%
%%% 三层 API:
%%% 1. 纯函数核心（不依赖进程）: new, do_step, do_retry, do_resume, run
%%% 2. 无进程 API（在调用者进程内运行到完成）: execute, execute/3
%%% 3. 访问器: current_state, context, superstep, take_snapshot, etc.
%%%
%%% 核心特点:
%%% - 无分区：所有顶点直接存储在状态中
%%% - do_step/1 同步执行完整超步
%%% - 统一池：普通顶点和 dispatch fan-out 共用 beamai_graph_pool
%%% - 单顶点失败隔离：失败顶点不影响其他顶点结果
%%% - 延迟 delta：有错误时 defer deltas，重试成功后 apply
%%% - run/1：循环 do_step，自动处理 interrupt/error/完成
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_engine).

%% === 构造 ===
-export([new/3, from_restored/2]).

%% === 纯函数核心 ===
-export([do_step/1, do_retry/2, do_resume/2]).

%% === 运行到完成（内部循环 do_step）===
-export([run/1]).

%% === 访问器 ===
-export([current_state/1, context/1, superstep/1,
         take_snapshot/1, extract_snapshot_data/1,
         last_results/1, last_info/1, build_result/1,
         drain_effects/1, resume_data/1]).

%% === 无进程 API（在调用者进程内运行到完成）===
-export([execute/2, execute/3]).

%% === 高级 run API（含 snapshot/store）===
-export([run_graph/2, run_graph/3]).


%% 类型导出 - 低级（来自 graph_executor）
-export_type([opts/0, result/0, restore_opts/0, snapshot_data/0]).
-export_type([step_result/0, superstep_info/0, snapshot_type/0, done_reason/0]).
-export_type([field_reducer/0, field_reducers/0, delta/0]).
-export_type([compute_context/0, compute_result/0, compute_status/0, vertex_id/0, compute_fn/0]).

%% 类型导出 - 高级（来自 graph_runner/graph_snapshot）
-export_type([run_options/0, run_result/0]).
-export_type([snapshot_strategy/0, store_config/0]).
-export_type([runner_snapshot_data/0]).

%% 类型导出 - 引擎
-export_type([engine/0, effect/0]).

%%====================================================================
%% 类型定义 - 低级（原 graph_executor）
%%====================================================================

-type graph() :: beamai_pregel_graph:graph().
-type vertex_id() :: beamai_pregel_vertex:vertex_id().
-type vertex() :: beamai_pregel_vertex:vertex().
-type compute_fn() :: fun((compute_context()) -> compute_result()).

%% Delta 类型
-type delta() :: #{atom() | binary() => term()}.

%% 字段 reducer 类型
-type field_reducer() :: fun((OldValue :: term(), NewValue :: term()) -> term()).
-type field_reducers() :: #{atom() | binary() => field_reducer()}.

%% 计算上下文（传递给计算函数）
-type compute_context() :: #{
    vertex_id := vertex_id(),
    vertex := vertex(),
    context := beamai_context:t(),
    vertex_input := map() | undefined,
    superstep := non_neg_integer(),
    num_vertices := non_neg_integer(),
    resume_data => term() | undefined
}.
%% 计算结果状态
-type compute_status() :: ok | {error, term()} | {interrupt, term()}.

%% 计算结果
-type compute_result() :: #{
    delta := delta(),
    activations => [vertex_id()],
    status := compute_status()
}.

%% Snapshot 数据（执行器级别）
-type snapshot_data() :: #{
    superstep := non_neg_integer(),
    context := beamai_context:t(),
    pending_deltas := [delta()] | undefined,
    pending_activations := [vertex_id()] | undefined,
    vertices := #{vertex_id() => vertex()}
}.

%% Snapshot 类型
-type snapshot_type() :: initial | step | error | interrupt | final.

%% 超步信息
-type superstep_info() :: #{
    type := snapshot_type(),
    superstep := non_neg_integer(),
    active_count := non_neg_integer(),
    activation_count := non_neg_integer(),
    failed_count := non_neg_integer(),
    failed_vertices := [{vertex_id(), term()}],
    interrupted_count := non_neg_integer(),
    interrupted_vertices := [{vertex_id(), term()}]
}.

%% step 返回值
-type step_result() ::
    {continue, superstep_info()} |
    {done, done_reason(), superstep_info()}.

-type done_reason() :: completed | max_supersteps.

%% Snapshot 恢复选项
-type restore_opts() :: #{
    superstep := non_neg_integer(),
    context := beamai_context:t(),
    pending_deltas => [delta()],
    pending_activations => [vertex_id()],
    vertices => #{vertex_id() => vertex()}
}.

%% 执行选项
-type opts() :: #{
    max_supersteps => pos_integer(),
    context => beamai_context:t(),
    field_reducers => field_reducers(),
    restore_from => restore_opts()
}.

%% 执行结果（低级）
-type result() :: #{
    status := completed | max_supersteps,
    context := beamai_context:t(),
    graph := graph(),
    supersteps := non_neg_integer(),
    stats := #{atom() => term()},
    failed_count => non_neg_integer(),
    failed_vertices => [{vertex_id(), term()}]
}.

%%====================================================================
%% 类型定义 - 高级（原 graph_runner/graph_snapshot）
%%====================================================================

%% Runner 级别 Snapshot 数据（含执行器层状态）
-type runner_snapshot_data() :: #{
    type := snapshot_type(),
    pregel_snapshot := snapshot_data(),
    context := beamai_context:t(),
    iteration := non_neg_integer()
}.

%% Snapshot 策略类型
-type snapshot_strategy() ::
    every_superstep |
    {every_n, pos_integer()} |
    on_interrupt |
    on_error.

%% Store 配置类型
-type store_config() :: {module(), beamai_graph_store_behaviour:store_ref()}.

-type run_options() :: #{
    workers => pos_integer(),
    max_iterations => pos_integer(),
    max_supersteps => pos_integer(),
    timeout => pos_integer(),
    %% Snapshot 恢复选项
    restore_from => runner_snapshot_data(),
    resume_data => #{vertex_id() => term()},
    retry_vertices => [vertex_id()],
    run_id => binary(),
    %% Context 选项
    context => beamai_context:t(),
    field_reducers => field_reducers(),
    %% Store 相关选项
    store => store_config(),
    snapshot_strategy => snapshot_strategy(),
    graph_name => atom() | binary()
}.

-type run_result() :: #{
    status := completed | interrupted | error | max_iterations,
    final_state := beamai_context:t(),
    iterations := non_neg_integer(),
    error => term(),
    done_reason => done_reason(),
    snapshot => snapshot_data(),
    interrupted_vertices => [{vertex_id(), term()}],
    failed_vertices => [{vertex_id(), term()}]
}.

%%====================================================================
%% 类型定义 - 引擎新增
%%====================================================================

-type effect() ::
    {superstep_completed, superstep_info()} |
    {interrupted, superstep_info()} |
    {error_occurred, superstep_info()} |
    {completed, done_reason(), superstep_info()}.

%%====================================================================
%% 内部状态
%%====================================================================

-record(engine, {
    graph            :: graph(),
    vertices         :: #{vertex_id() => vertex()},
    compute_fn       :: compute_fn(),

    max_supersteps   :: pos_integer(),
    superstep        :: non_neg_integer(),

    context          :: beamai_context:t(),
    field_reducers   :: field_reducers(),
    pending_deltas   :: [delta()] | undefined,
    pending_activations :: [vertex_id()] | undefined,

    last_results     :: map() | undefined,
    cumulative_failures :: [{vertex_id(), term()}],

    pool_name        :: atom(),
    pool_timeout     :: pos_integer(),

    restore_from     :: restore_opts() | undefined,
    initialized      :: boolean(),
    initial_returned :: boolean(),
    halted           :: boolean(),

    %% 新增字段
    resume_data = #{} :: #{vertex_id() => term()},
    current_state = idle :: idle | running | interrupted | completed | error,
    effects = [] :: [effect()]
}).

-opaque engine() :: #engine{}.

%%====================================================================
%% 构造
%%====================================================================

%% @doc 创建新引擎（不启动进程）
-spec new(graph(), compute_fn(), opts()) -> {ok, engine()}.
new(Graph, ComputeFn, Opts) ->
    Engine = new_engine(Graph, ComputeFn, Opts),
    {ok, Engine#engine{current_state = running}}.

%% @doc 从恢复数据重建引擎
-spec from_restored(map(), map()) -> {ok, engine()}.
from_restored(Restored, Opts) ->
    PregelGraph = maps:get(pregel_graph, Opts),
    ComputeFn = maps:get(compute_fn, Opts),
    FieldReducers = maps:get(field_reducers, Opts, #{}),

    %% Restored 包含 restore_opts 格式的数据
    EngineOpts = #{
        field_reducers => FieldReducers,
        restore_from => Restored
    },


    Engine = new_engine(PregelGraph, ComputeFn, EngineOpts),
    {ok, Engine#engine{current_state = running}}.

%%====================================================================
%% 纯函数核心
%%====================================================================

%% @doc 执行单个超步（纯函数）
-spec do_step(engine()) -> {step_result(), engine()}.
do_step(#engine{halted = true} = Engine) ->
    Info = beamai_graph_executor_utils:build_superstep_info(final, Engine#engine.last_results),
    {{done, get_done_reason(Engine), Info}, Engine};
do_step(#engine{initialized = false} = Engine) ->
    EngineReady = inject_restore_activations(Engine),
    EngineFinal = EngineReady#engine{
        initialized = true,
        initial_returned = true,
        restore_from = undefined
    },
    Info = beamai_graph_executor_utils:build_superstep_info(initial, undefined),
    {{continue, Info}, EngineFinal};
do_step(Engine) ->
    execute_superstep(Engine).

%% @doc 重试失败顶点（纯函数）
-spec do_retry([vertex_id()], engine()) -> {step_result(), engine()}.
do_retry(_VertexIds, #engine{halted = true} = Engine) ->
    Info = beamai_graph_executor_utils:build_superstep_info(final, Engine#engine.last_results),
    {{done, get_done_reason(Engine), Info}, Engine};
do_retry(_VertexIds, #engine{last_results = undefined} = Engine) ->
    {{error, no_previous_step}, Engine};
do_retry(VertexIds, #engine{pending_deltas = undefined} = Engine) ->
    execute_retry_direct(VertexIds, Engine);
do_retry(VertexIds, Engine) ->
    execute_deferred_retry(VertexIds, Engine).

%% @doc 恢复中断（纯函数）= 存储 resume_data 到引擎字段 + 重试中断顶点
-spec do_resume(#{vertex_id() => term()}, engine()) -> {step_result(), engine()}.
do_resume(ResumeData, #engine{last_results = LastResults} = Engine) ->
    %% 1. 获取中断顶点 + resume_data 键
    InterruptedIds = case LastResults of
        undefined -> [];
        _ -> [Id || {Id, _} <- maps:get(interrupted_vertices, LastResults, [])]
    end,
    AllIds = lists:usort(maps:keys(ResumeData) ++ InterruptedIds),
    %% 2. 存入 engine field，更新引擎状态并调用 do_retry
    Engine1 = Engine#engine{resume_data = ResumeData, current_state = running},
    do_retry(AllIds, Engine1).

%%====================================================================
%% 运行到完成（循环 do_step + effects）
%%====================================================================

%% @doc 循环 do_step，遇到 interrupt/error/完成 时停止
-spec run(engine()) -> {ok, engine()}.
run(#engine{current_state = CS} = E) when CS =/= running -> {ok, E};
run(E) ->
    {StepResult, E1} = do_step(E),
    case StepResult of
        {continue, #{type := interrupt} = Info} ->
            E2 = E1#engine{current_state = interrupted},
            {ok, add_effect({interrupted, Info}, E2)};
        {continue, #{type := error} = Info} ->
            E2 = E1#engine{current_state = error},
            {ok, add_effect({error_occurred, Info}, E2)};
        {continue, Info} ->
            run(add_effect({superstep_completed, Info}, E1));
        {done, Reason, Info} ->
            E2 = E1#engine{current_state = completed},
            {ok, add_effect({completed, Reason, Info}, E2)}
    end.

%%====================================================================
%% 访问器
%%====================================================================

%% @doc 获取引擎当前状态
-spec current_state(engine()) -> idle | running | interrupted | completed | error.
current_state(#engine{current_state = CS}) -> CS.

%% @doc 获取 context
-spec context(engine()) -> beamai_context:t().
context(#engine{context = Ctx}) -> Ctx.

%% @doc 获取当前超步号
-spec superstep(engine()) -> non_neg_integer().
superstep(#engine{superstep = S}) -> S.

%% @doc 获取上次结果
-spec last_results(engine()) -> map() | undefined.
last_results(#engine{last_results = LR}) -> LR.

%% @doc 获取上次超步信息（从 last_results 构建）
-spec last_info(engine()) -> superstep_info().
last_info(#engine{last_results = undefined}) ->
    beamai_graph_executor_utils:build_superstep_info(step, undefined);
last_info(#engine{last_results = Results}) ->
    Type = beamai_graph_executor_utils:determine_snapshot_type(Results),
    beamai_graph_executor_utils:build_superstep_info(Type, Results).

%% @doc 获取快照数据（委托到 graph_snapshot）
-spec take_snapshot(engine()) -> beamai_graph_state:snapshot().
take_snapshot(Engine) ->
    beamai_graph_state:take_snapshot(Engine).

%% @doc 提取 snapshot_data（执行器级别）
-spec extract_snapshot_data(engine()) -> snapshot_data().
extract_snapshot_data(#engine{
    superstep = Superstep,
    context = Context,
    pending_deltas = PendingDeltas,
    pending_activations = PendingActivations,
    vertices = Vertices,
    resume_data = ResumeData,
    last_results = Results
}) ->
    Activations = case PendingActivations of
        undefined ->
            case Results of
                undefined -> undefined;
                _ -> maps:get(activations, Results, undefined)
            end;
        _ -> PendingActivations
    end,
    Base = #{
        superstep => Superstep,
        context => Context,
        pending_deltas => PendingDeltas,
        pending_activations => Activations,
        vertices => Vertices
    },
    case map_size(ResumeData) of
        0 -> Base;
        _ -> Base#{resume_data => ResumeData}
    end.

%% @doc 返回并清空 effects 列表
-spec drain_effects(engine()) -> {[effect()], engine()}.
drain_effects(#engine{effects = Effects} = Engine) ->
    {lists:reverse(Effects), Engine#engine{effects = []}}.

%% @doc 获取 resume_data
-spec resume_data(engine()) -> #{vertex_id() => term()}.
resume_data(#engine{resume_data = RD}) -> RD.

%% @doc 构建低级结果（从 engine）
-spec build_result(engine()) -> result().
build_result(#engine{
    superstep = Superstep,
    graph = OriginalGraph,
    vertices = Vertices,
    context = Context,
    cumulative_failures = CumulativeFailures
} = Engine) ->
    FinalGraph = beamai_graph_executor_utils:rebuild_graph(OriginalGraph, Vertices),
    FailedCount = length(CumulativeFailures),
    #{
        status => get_done_reason(Engine),
        context => Context,
        graph => FinalGraph,
        supersteps => Superstep + 1,
        stats => #{},
        failed_count => FailedCount,
        failed_vertices => CumulativeFailures
    }.

%%====================================================================
%% 引擎内部字段访问（供 graph_snapshot 使用）
%%====================================================================

%% 这些不导出到模块外，通过 snapshot 模块内部的 engine_fields/1 间接访问
%% 但 Erlang 不支持 friend module，所以通过 extract_snapshot_data 暴露

%%====================================================================
%% 无进程 API - 低级
%%====================================================================

%% @doc 执行图计算（使用默认选项）
-spec execute(graph(), compute_fn()) -> result().
execute(Graph, ComputeFn) ->
    execute(Graph, ComputeFn, #{}).

%% @doc 执行图计算（带选项）
-spec execute(graph(), compute_fn(), opts()) -> result().
execute(Graph, ComputeFn, Opts) ->
    Engine = new_engine(Graph, ComputeFn, Opts),
    execute_loop(Engine).

%%====================================================================
%% 高级 run API（含 snapshot/store）
%%====================================================================

%% @doc 运行图，使用初始 context
-spec run_graph(beamai_graph_builder:graph(), beamai_context:t()) -> run_result().
run_graph(Graph, InitialState) ->
    run_graph(Graph, InitialState, #{}).

%% @doc 运行图，使用初始 context 和选项
-spec run_graph(beamai_graph_builder:graph(), beamai_context:t(), run_options()) -> run_result().
run_graph(Graph, InitialState, Options) ->
    OptionsWithState = ensure_context(Options, InitialState),
    case needs_snapshot_mode(OptionsWithState) of
        true ->
            run_with_snapshot(Graph, InitialState, OptionsWithState);
        false ->
            run_simple(Graph, InitialState, OptionsWithState)
    end.

%%====================================================================
%% 超步执行（核心算法）
%%====================================================================

%% @private 执行完整超步（纯函数，返回 {step_result(), engine()}）
-spec execute_superstep(engine()) -> {step_result(), engine()}.
execute_superstep(#engine{
    vertices = Vertices,
    compute_fn = ComputeFn,
    superstep = Superstep,
    context = Context,
    field_reducers = FieldReducers,
    pending_activations = PendingActivations,
    last_results = LastResults,
    cumulative_failures = CumulativeFailures,
    max_supersteps = MaxSupersteps,
    pool_name = PoolName,
    pool_timeout = PoolTimeout
} = Engine) ->
    NumVertices = maps:size(Vertices),

    %% 1. 获取 activations
    Activations = beamai_graph_executor_utils:get_activations_for_superstep(PendingActivations, LastResults),

    %% 2. 分离 dispatch 项与普通激活
    {DispatchItems, NormalActivations} = beamai_graph_executor_utils:separate_dispatches(Activations),
    VertexInputs = beamai_graph_executor_utils:build_vertex_inputs(DispatchItems),
    DispatchNodeIds = maps:keys(VertexInputs),
    AllActivationIds = lists:usort(NormalActivations ++ DispatchNodeIds),

    %% 3. 筛选活跃顶点
    ActiveVertices = beamai_graph_executor_utils:filter_active_vertices(Vertices, AllActivationIds),

    %% 4. 构建扁平任务列表
    Tasks = beamai_graph_executor_task:build_task_list(ActiveVertices, VertexInputs),

    %% 5. 执行任务
    {Deltas, NewActivations, FailedVertices, InterruptedVertices} =
        beamai_graph_executor_task:execute_tasks(Tasks, ComputeFn, Context, Superstep, NumVertices,
                      PoolName, PoolTimeout, Engine#engine.resume_data),

    %% 6. 更新顶点状态（halt 计算完成的顶点）
    NewVertices = beamai_graph_executor_utils:update_vertex_states(Vertices, ActiveVertices, FailedVertices, InterruptedVertices),

    %% 7. 检查错误并累积失败信息
    FailedCount = length(FailedVertices),
    InterruptedCount = length(InterruptedVertices),
    HasError = FailedCount > 0 orelse InterruptedCount > 0,

    NewCumulativeFailures = case FailedVertices of
        [] -> CumulativeFailures;
        _ -> lists:usort(FailedVertices ++ CumulativeFailures)
    end,

    %% 8. 决定是否延迟提交
    TotalActivations = length(NewActivations),
    {NewContext, NewPendingDeltas, NewPendingActivations} = case HasError of
        true ->
            {Context, Deltas, NewActivations};
        false ->
            UpdatedCtx = beamai_context_reducer:apply_deltas(Context, Deltas, FieldReducers),
            {UpdatedCtx, undefined, undefined}
    end,

    %% 9. 更新结果映射
    TotalActive = beamai_graph_executor_utils:count_active(NewVertices),
    UpdatedResults = #{
        active_count => TotalActive,
        deltas => Deltas,
        activations => NewActivations,
        activation_count => TotalActivations,
        failed_count => FailedCount,
        failed_vertices => FailedVertices,
        interrupted_count => InterruptedCount,
        interrupted_vertices => InterruptedVertices,
        superstep => Superstep
    },

    %% 10. 检查终止条件
    Halted = (TotalActive =:= 0) andalso (TotalActivations =:= 0) andalso (not HasError),
    MaxReached = Superstep >= MaxSupersteps - 1,
    IsDone = Halted orelse MaxReached,

    %% 11. 确定 snapshot 类型
    Type = case IsDone of
        true -> final;
        false -> beamai_graph_executor_utils:determine_snapshot_type(UpdatedResults)
    end,
    Info = beamai_graph_executor_utils:build_superstep_info(Type, UpdatedResults),

    %% 12. 计算新超步号
    NewSuperstep = if IsDone -> Superstep; true -> Superstep + 1 end,

    %% 13. 构建回复
    Reply = case IsDone of
        true ->
            Reason = if Halted -> completed; true -> max_supersteps end,
            {done, Reason, Info};
        false ->
            {continue, Info}
    end,

    %% 清理已成功执行顶点的 resume_data
    SuccessIds = [Id || {Id, _, _} <- Tasks,
                  not lists:keymember(Id, 1, FailedVertices),
                  not lists:keymember(Id, 1, InterruptedVertices)],
    CleanedRD = maps:without(SuccessIds, Engine#engine.resume_data),

    NewEngine = Engine#engine{
        vertices = NewVertices,
        context = NewContext,
        pending_deltas = NewPendingDeltas,
        pending_activations = NewPendingActivations,
        last_results = UpdatedResults,
        cumulative_failures = NewCumulativeFailures,
        superstep = NewSuperstep,
        halted = IsDone,
        resume_data = CleanedRD
    },
    {Reply, NewEngine}.

%%====================================================================
%% 重试逻辑
%%====================================================================

%% @private 直接重试（无 pending_deltas，纯函数）
-spec execute_retry_direct([vertex_id()], engine()) -> {step_result(), engine()}.
execute_retry_direct(VertexIds, #engine{
    vertices = Vertices,
    compute_fn = ComputeFn,
    superstep = Superstep,
    context = Context,
    field_reducers = FieldReducers,
    last_results = LastResults,
    pool_name = PoolName,
    pool_timeout = PoolTimeout
} = Engine) ->
    NumVertices = maps:size(Vertices),

    RetryVertices = maps:with(VertexIds, Vertices),
    Tasks = [{Id, V, undefined} || {Id, V} <- maps:to_list(RetryVertices)],

    {RetryDeltas, RetryActivations, StillFailed, StillInterrupted} =
        beamai_graph_executor_task:execute_tasks(Tasks, ComputeFn, Context, Superstep, NumVertices,
                      PoolName, PoolTimeout, Engine#engine.resume_data),

    HasError = length(StillFailed) > 0 orelse length(StillInterrupted) > 0,

    {NewContext, NewPendingDeltas, NewPendingActivations} = case HasError of
        true ->
            LastActivations = maps:get(activations, LastResults, []),
            {Context, RetryDeltas, LastActivations ++ RetryActivations};
        false ->
            UpdatedCtx = beamai_context_reducer:apply_deltas(Context, RetryDeltas, FieldReducers),
            {UpdatedCtx, undefined, undefined}
    end,

    %% 清理已成功执行顶点的 resume_data
    SuccessIds = [Id || {Id, _} <- maps:to_list(RetryVertices),
                  not lists:keymember(Id, 1, StillFailed),
                  not lists:keymember(Id, 1, StillInterrupted)],
    CleanedRD = maps:without(SuccessIds, Engine#engine.resume_data),

    UpdatedResults = LastResults#{
        failed_count => length(StillFailed),
        failed_vertices => StillFailed,
        interrupted_count => length(StillInterrupted),
        interrupted_vertices => StillInterrupted,
        activation_count => maps:get(activation_count, LastResults, 0) + length(RetryActivations),
        activations => maps:get(activations, LastResults, []) ++ RetryActivations,
        superstep => Superstep
    },

    Type = beamai_graph_executor_utils:determine_snapshot_type(UpdatedResults),
    Info = beamai_graph_executor_utils:build_superstep_info(Type, UpdatedResults),

    NewEngine = Engine#engine{
        context = NewContext,
        pending_deltas = NewPendingDeltas,
        pending_activations = NewPendingActivations,
        last_results = UpdatedResults,
        resume_data = CleanedRD
    },
    {{continue, Info}, NewEngine}.

%% @private 延迟提交重试（纯函数）
-spec execute_deferred_retry([vertex_id()], engine()) -> {step_result(), engine()}.
execute_deferred_retry(VertexIds, #engine{
    vertices = Vertices,
    compute_fn = ComputeFn,
    superstep = Superstep,
    context = Context,
    field_reducers = FieldReducers,
    pending_deltas = PendingDeltas,
    pending_activations = PendingActivations,
    pool_name = PoolName,
    pool_timeout = PoolTimeout
} = Engine) ->
    NumVertices = maps:size(Vertices),

    RetryVertices = maps:with(VertexIds, Vertices),
    Tasks = [{Id, V, undefined} || {Id, V} <- maps:to_list(RetryVertices)],

    {RetryDeltas, RetryActivations, StillFailed, StillInterrupted} =
        beamai_graph_executor_task:execute_tasks(Tasks, ComputeFn, Context, Superstep, NumVertices,
                      PoolName, PoolTimeout, Engine#engine.resume_data),

    HasError = length(StillFailed) > 0 orelse length(StillInterrupted) > 0,

    SafePendingDeltas = case PendingDeltas of
        undefined -> [];
        _ -> PendingDeltas
    end,
    MergedDeltas = SafePendingDeltas ++ RetryDeltas,

    SafePendingActivations = case PendingActivations of
        undefined -> [];
        _ -> PendingActivations
    end,
    MergedActivations = SafePendingActivations ++ RetryActivations,

    {NewContext, NewPendingDeltas, NewPendingActivations} = case HasError of
        true ->
            {Context, MergedDeltas, MergedActivations};
        false ->
            UpdatedCtx = beamai_context_reducer:apply_deltas(Context, MergedDeltas, FieldReducers),
            {UpdatedCtx, undefined, undefined}
    end,

    %% 清理已成功执行顶点的 resume_data
    SuccessIds = [Id || {Id, _} <- maps:to_list(RetryVertices),
                  not lists:keymember(Id, 1, StillFailed),
                  not lists:keymember(Id, 1, StillInterrupted)],
    CleanedRD = maps:without(SuccessIds, Engine#engine.resume_data),

    UpdatedResults = #{
        failed_count => length(StillFailed),
        failed_vertices => StillFailed,
        interrupted_count => length(StillInterrupted),
        interrupted_vertices => StillInterrupted,
        activation_count => length(MergedActivations),
        activations => MergedActivations,
        superstep => Superstep,
        active_count => 0
    },

    Type = beamai_graph_executor_utils:determine_snapshot_type(UpdatedResults),
    Info = beamai_graph_executor_utils:build_superstep_info(Type, UpdatedResults),

    NewEngine = Engine#engine{
        context = NewContext,
        pending_deltas = NewPendingDeltas,
        pending_activations = NewPendingActivations,
        last_results = UpdatedResults,
        resume_data = CleanedRD
    },
    {{continue, Info}, NewEngine}.

%%====================================================================
%% 无进程执行循环 - 低级
%%====================================================================

%% @private 低级执行循环
-spec execute_loop(engine()) -> result().
execute_loop(Engine) ->
    case do_step(Engine) of
        {{continue, #{type := error} = Info}, NewEngine} ->
            build_early_termination_result(NewEngine, Info);
        {{continue, #{type := interrupt} = Info}, NewEngine} ->
            build_early_termination_result(NewEngine, Info);
        {{continue, _Info}, NewEngine} ->
            execute_loop(NewEngine);
        {{done, _Reason, _Info}, NewEngine} ->
            build_result(NewEngine)
    end.

%%====================================================================
%% 无进程执行 - 高级（run_graph 相关）
%%====================================================================

%% @private 确保 Options 中有 context
-spec ensure_context(run_options(), beamai_context:t()) -> run_options().
ensure_context(Options, InitialState) ->
    case maps:is_key(context, Options) of
        true -> Options;
        false -> Options#{context => InitialState}
    end.

%% @private 检查是否需要 snapshot 模式
-spec needs_snapshot_mode(run_options()) -> boolean().
needs_snapshot_mode(Options) ->
    maps:is_key(restore_from, Options) orelse
    maps:is_key(store, Options).

%% @private 简单执行模式（无 snapshot）
-spec run_simple(beamai_graph_builder:graph(), beamai_context:t(), run_options()) -> run_result().
run_simple(Graph, InitialState, Options) ->
    #{pregel_graph := PregelGraph} = Graph,

    MaxIterations = maps:get(max_iterations, Graph, 100),
    Context = maps:get(context, Options, InitialState),
    FieldReducers = maps:get(field_reducers, Options, #{}),

    ExecutorOpts = #{
        max_supersteps => maps:get(max_supersteps, Options, MaxIterations),
        context => Context,
        field_reducers => FieldReducers
    },

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = execute(PregelGraph, ComputeFn, ExecutorOpts),

    ExecutorResult = beamai_graph_compute:from_pregel_result(Result),
    handle_pregel_result(ExecutorResult, InitialState, Options).

%% @private Snapshot 执行模式
-spec run_with_snapshot(beamai_graph_builder:graph(), beamai_context:t(), run_options()) -> run_result().
run_with_snapshot(Graph, _InitialState, Options) ->
    #{pregel_graph := PregelGraph} = Graph,

    OptionsWithRunId = ensure_run_id(Options),

    ActualContext = maps:get(context, OptionsWithRunId),

    SnapshotData = maps:get(restore_from, OptionsWithRunId, undefined),
    {FinalContext, ExecutorRestoreOpts, StartIteration} =
        prepare_restore_options(SnapshotData, OptionsWithRunId, ActualContext),

    MaxIterations = maps:get(max_iterations, Graph, 100),
    FieldReducers = maps:get(field_reducers, OptionsWithRunId, #{}),

    ExecutorOpts0 = #{
        max_supersteps => maps:get(max_supersteps, OptionsWithRunId, MaxIterations),
        context => FinalContext,
        field_reducers => FieldReducers
    },

    ExecutorOpts = case ExecutorRestoreOpts of
        undefined -> ExecutorOpts0;
        _ -> ExecutorOpts0#{restore_from => ExecutorRestoreOpts}
    end,

    ComputeFn = beamai_graph_compute:compute_fn(),
    Engine = new_engine(PregelGraph, ComputeFn, ExecutorOpts),
    run_loop(Engine, StartIteration, OptionsWithRunId).

%% @private 高级执行循环（含 snapshot/store）
-spec run_loop(engine(), non_neg_integer(), run_options()) -> run_result().
run_loop(Engine, Iteration, Options) ->
    case do_step(Engine) of
        {{continue, Info}, NewEngine} ->
            SnapshotData = build_runner_snapshot_data(NewEngine, Info, Iteration, Options),
            Type = maps:get(type, Info),

            maybe_save_to_store(SnapshotData, Type, Options),

            case Type of
                interrupt ->
                    build_interrupted_run_result(SnapshotData, Info, Iteration);
                error ->
                    build_error_run_result(SnapshotData, Info, Iteration);
                _ ->
                    run_loop(NewEngine, next_iteration(Type, Iteration), Options)
            end;

        {{done, Reason, Info}, NewEngine} ->
            handle_done(NewEngine, Reason, Info, Iteration, Options)
    end.

%%====================================================================
%% 结果构建
%%====================================================================

%% @private 构建提前终止时的低级结果
-spec build_early_termination_result(engine(), superstep_info()) -> result().
build_early_termination_result(#engine{
    superstep = Superstep,
    context = Context
}, Info) ->
    #{
        status => completed,
        context => Context,
        graph => #{vertices => #{}},
        supersteps => Superstep,
        stats => #{},
        failed_count => maps:get(failed_count, Info, 0),
        failed_vertices => maps:get(failed_vertices, Info, []),
        interrupted_count => maps:get(interrupted_count, Info, 0),
        interrupted_vertices => maps:get(interrupted_vertices, Info, [])
    }.

%% @private 处理 Pregel 引擎执行结果（高级 run_simple 用）
-spec handle_pregel_result({ok, beamai_context:t()} | {error, term()}, beamai_context:t(), run_options()) -> run_result().
handle_pregel_result({ok, FinalState}, _InitialState, _Options) ->
    #{status => completed, final_state => FinalState, iterations => 0};
handle_pregel_result({error, {partial_result, PartialState, max_iterations_exceeded}}, _InitialState, Options) ->
    MaxIter = maps:get(max_iterations, Options, 100),
    #{status => max_iterations, final_state => PartialState, iterations => MaxIter};
handle_pregel_result({error, {partial_result, PartialState, Reason}}, _InitialState, _Options) ->
    #{status => error, final_state => PartialState, iterations => 0, error => Reason};
handle_pregel_result({error, max_iterations_exceeded}, InitialState, Options) ->
    MaxIter = maps:get(max_iterations, Options, 100),
    #{status => max_iterations, final_state => InitialState, iterations => MaxIter, error => max_iterations_exceeded};
handle_pregel_result({error, Reason}, InitialState, _Options) ->
    #{status => error, final_state => InitialState, iterations => 0, error => Reason}.

%%====================================================================
%% Snapshot 数据构建
%%====================================================================

%% @private 构建 runner 级别 snapshot 数据
-spec build_runner_snapshot_data(engine(), superstep_info(), non_neg_integer(), run_options()) ->
    runner_snapshot_data().
build_runner_snapshot_data(Engine, Info, Iteration, Options) ->
    RunId = maps:get(run_id, Options),
    PregelCheckpoint = extract_snapshot_data(Engine),
    CurrentContext = Engine#engine.context,
    Type = maps:get(type, Info),
    Superstep = maps:get(superstep, Info, 0),

    Vertices = maps:get(vertices, PregelCheckpoint, #{}),
    {ActiveVertices, CompletedVertices} = classify_vertices(Vertices),

    #{
        type => Type,
        pregel_snapshot => PregelCheckpoint,
        context => CurrentContext,
        iteration => Iteration,
        run_id => RunId,
        active_vertices => ActiveVertices,
        completed_vertices => CompletedVertices,
        superstep => Superstep
    }.

%% @private 分类顶点状态
-spec classify_vertices(#{atom() => beamai_pregel_vertex:vertex()}) ->
    {ActiveVertices :: [atom()], CompletedVertices :: [atom()]}.
classify_vertices(Vertices) ->
    maps:fold(
        fun('__start__', _Vertex, Acc) -> Acc;
           ('__end__', _Vertex, Acc) -> Acc;
           (Id, Vertex, {Active, Completed}) ->
            case beamai_pregel_vertex:is_active(Vertex) of
                true -> {[Id | Active], Completed};
                false -> {Active, [Id | Completed]}
            end
        end,
        {[], []},
        Vertices
    ).

%%====================================================================
%% Snapshot 恢复
%%====================================================================

%% @private 准备从 snapshot 恢复的选项
-spec prepare_restore_options(runner_snapshot_data() | undefined, run_options(), beamai_context:t()) ->
    {beamai_context:t(), restore_opts() | undefined, non_neg_integer()}.
prepare_restore_options(undefined, _Options, Context) ->
    {Context, undefined, 0};
prepare_restore_options(SnapshotData, Options, _DefaultContext) ->
    PregelCheckpoint = maps:get(pregel_snapshot, SnapshotData),

    Context = case maps:get(context, SnapshotData, undefined) of
        undefined ->
            maps:get(context, PregelCheckpoint, beamai_context:new());
        C -> C
    end,
    Iteration = maps:get(iteration, SnapshotData, 0),
    ResumeData = maps:get(resume_data, Options, #{}),
    RetryVertices = maps:get(retry_vertices, Options, []),

    #{superstep := Superstep, vertices := Vertices} = PregelCheckpoint,

    RawPendingActivations = maps:get(pending_activations, PregelCheckpoint, []),
    PendingActivations = case RawPendingActivations of
        undefined -> [];
        List when is_list(List) -> List
    end,

    ResumeVertexIds = maps:keys(ResumeData),
    AllActivations = lists:usort(ResumeVertexIds ++ RetryVertices ++ PendingActivations),

    PregelRestoreOpts = #{
        superstep => Superstep,
        vertices => Vertices,
        pending_activations => AllActivations,
        context => Context,
        resume_data => ResumeData
    },

    {Context, PregelRestoreOpts, Iteration}.

%% @private 确保 Options 中存在 run_id
-spec ensure_run_id(run_options()) -> run_options().
ensure_run_id(Options) ->
    case maps:is_key(run_id, Options) of
        true -> Options;
        false -> Options#{run_id => beamai_id:gen_id(<<"run">>)}
    end.

%%====================================================================
%% 高级结果构建
%%====================================================================

%% @private 构建 interrupted 结果
-spec build_interrupted_run_result(runner_snapshot_data(), superstep_info(), non_neg_integer()) ->
    run_result().
build_interrupted_run_result(SnapshotData, Info, Iteration) ->
    PregelCheckpoint = maps:get(pregel_snapshot, SnapshotData),
    Context = maps:get(context, SnapshotData),
    InterruptedVertices = maps:get(interrupted_vertices, Info, []),
    #{
        status => interrupted,
        final_state => Context,
        iterations => Iteration,
        snapshot => PregelCheckpoint,
        interrupted_vertices => InterruptedVertices
    }.

%% @private 构建 error 结果
-spec build_error_run_result(runner_snapshot_data(), superstep_info(), non_neg_integer()) ->
    run_result().
build_error_run_result(SnapshotData, Info, Iteration) ->
    PregelCheckpoint = maps:get(pregel_snapshot, SnapshotData),
    Context = maps:get(context, SnapshotData),
    FailedVertices = maps:get(failed_vertices, Info, []),
    #{
        status => error,
        final_state => Context,
        iterations => Iteration,
        snapshot => PregelCheckpoint,
        failed_vertices => FailedVertices
    }.

%% @private 计算下一次迭代数
-spec next_iteration(snapshot_type(), non_neg_integer()) -> non_neg_integer().
next_iteration(initial, Iteration) -> Iteration;
next_iteration(_, Iteration) -> Iteration + 1.

%% @private 处理执行完成（done 分支）
-spec handle_done(engine(), done_reason(), superstep_info(),
                  non_neg_integer(), run_options()) -> run_result().
handle_done(Engine, Reason, Info, Iteration, Options) ->
    RunId = maps:get(run_id, Options),
    Superstep = maps:get(superstep, Info, 0),

    PregelCheckpoint = extract_snapshot_data(Engine),
    FinalContext = Engine#engine.context,

    Vertices = maps:get(vertices, PregelCheckpoint, #{}),
    {ActiveVertices, CompletedVertices} = classify_vertices(Vertices),

    SnapshotData = #{
        type => final,
        pregel_snapshot => PregelCheckpoint,
        context => FinalContext,
        iteration => Iteration,
        run_id => RunId,
        active_vertices => ActiveVertices,
        completed_vertices => CompletedVertices,
        superstep => Superstep
    },

    maybe_save_to_store(SnapshotData, final, Options),

    Result = build_result(Engine),
    PregelResult = beamai_graph_compute:from_pregel_result(Result),
    FinalResult = handle_pregel_result(PregelResult, FinalContext, Options),

    FinalResult#{
        iterations => Iteration,
        done_reason => Reason,
        snapshot => PregelCheckpoint
    }.

%%====================================================================
%% Store 持久化
%%====================================================================

%% @private 根据 store/strategy 配置决定是否保存
-spec maybe_save_to_store(runner_snapshot_data(), snapshot_type(), run_options()) -> ok.
maybe_save_to_store(SnapshotData, Type, Options) ->
    case maps:get(store, Options, undefined) of
        undefined ->
            ok;
        {StoreModule, StoreRef} ->
            Strategy = maps:get(snapshot_strategy, Options, every_superstep),
            GraphName = maps:get(graph_name, Options, undefined),
            Superstep = maps:get(superstep, SnapshotData, 0),
            case should_save(Strategy, Type, Superstep) of
                true ->
                    do_save_snapshot(StoreModule, StoreRef, SnapshotData, GraphName, Type);
                false ->
                    ok
            end
    end.

%% @private 根据策略判断是否需要保存
-spec should_save(snapshot_strategy(), atom(), non_neg_integer()) -> boolean().
should_save(every_superstep, _Type, _Superstep) ->
    true;
should_save({every_n, N}, _Type, Superstep) ->
    Superstep rem N =:= 0;
should_save(on_interrupt, interrupt, _Superstep) ->
    true;
should_save(on_interrupt, final, _Superstep) ->
    true;
should_save(on_interrupt, _Type, _Superstep) ->
    false;
should_save(on_error, error, _Superstep) ->
    true;
should_save(on_error, final, _Superstep) ->
    true;
should_save(on_error, _Type, _Superstep) ->
    false.

%% @private 执行快照保存
-spec do_save_snapshot(module(), term(), runner_snapshot_data(), atom() | binary() | undefined, atom()) -> ok.
do_save_snapshot(StoreModule, StoreRef, SnapshotData, GraphName, TriggerType) ->
    Superstep = maps:get(superstep, SnapshotData, 0),
    SaveOpts = #{
        graph_name => GraphName,
        trigger => trigger_from_type(TriggerType),
        superstep => Superstep,
        metadata => #{}
    },
    case StoreModule:save_snapshot(StoreRef, SnapshotData, SaveOpts) of
        {ok, _SnapshotId} ->
            ok;
        {error, Reason} ->
            error_logger:warning_msg("Failed to save graph snapshot: ~p~n", [Reason]),
            ok
    end.

%% @private 将 snapshot type 转换为 trigger type
-spec trigger_from_type(atom()) -> beamai_graph_store_behaviour:trigger_type().
trigger_from_type(initial) -> superstep_completed;
trigger_from_type(step) -> superstep_completed;
trigger_from_type(error) -> error_occurred;
trigger_from_type(interrupt) -> interrupted;
trigger_from_type(final) -> completed;
trigger_from_type(stopped) -> stopped.

%%====================================================================
%% 内部辅助函数
%%====================================================================

%% @private 初始化引擎状态
-spec new_engine(graph(), compute_fn(), opts()) -> engine().
new_engine(Graph, ComputeFn, Opts) ->
    RestoreOpts = maps:get(restore_from, Opts, undefined),
    InitialSuperstep = get_restore_superstep(RestoreOpts),

    Context = case RestoreOpts of
        #{context := RestoredCtx} -> RestoredCtx;
        _ -> maps:get(context, Opts, beamai_context:new())
    end,

    PendingDeltas = case RestoreOpts of
        #{pending_deltas := PD} -> PD;
        _ -> undefined
    end,
    PendingActivations = case RestoreOpts of
        #{pending_activations := PA} -> PA;
        _ -> undefined
    end,

    ResumeData = case RestoreOpts of
        #{resume_data := RD} -> RD;
        _ -> #{}
    end,

    Vertices = beamai_graph_executor_utils:get_all_vertices(Graph, RestoreOpts),
    PoolTimeout = application:get_env(beamai_core, graph_pool_timeout, 30000),

    #engine{
        graph = Graph,
        vertices = Vertices,
        compute_fn = ComputeFn,
        max_supersteps = maps:get(max_supersteps, Opts, 100),
        superstep = InitialSuperstep,
        context = Context,
        field_reducers = maps:get(field_reducers, Opts, beamai_context_reducer:default_reducers()),
        pending_deltas = PendingDeltas,
        pending_activations = PendingActivations,
        last_results = undefined,
        cumulative_failures = [],
        pool_name = beamai_graph_pool,
        pool_timeout = PoolTimeout,
        restore_from = RestoreOpts,
        initialized = false,
        initial_returned = false,
        halted = false,
        resume_data = ResumeData,
        current_state = idle,
        effects = []
    }.

%% @private 添加 effect
-spec add_effect(effect(), engine()) -> engine().
add_effect(Effect, #engine{effects = Effects} = Engine) ->
    Engine#engine{effects = [Effect | Effects]}.

%% @private 获取终止原因
-spec get_done_reason(engine()) -> done_reason().
get_done_reason(#engine{superstep = Superstep, max_supersteps = MaxSupersteps, last_results = Results}) ->
    TotalActive = maps:get(active_count, Results, 0),
    TotalActivations = maps:get(activation_count, Results, 0),
    Halted = (TotalActive =:= 0) andalso (TotalActivations =:= 0),
    case Halted of
        true -> completed;
        false when Superstep >= MaxSupersteps - 1 -> max_supersteps;
        false -> completed
    end.

%% @private 获取恢复选项中的超步号
-spec get_restore_superstep(restore_opts() | undefined) -> non_neg_integer().
get_restore_superstep(undefined) -> 0;
get_restore_superstep(#{superstep := Superstep}) -> Superstep;
get_restore_superstep(_) -> 0.

%% @private 注入恢复的 activations
-spec inject_restore_activations(engine()) -> engine().
inject_restore_activations(#engine{restore_from = undefined} = Engine) ->
    Engine;
inject_restore_activations(#engine{restore_from = RestoreOpts} = Engine) ->
    Activations = case RestoreOpts of
        #{pending_activations := PA} -> PA;
        _ -> undefined
    end,
    Engine#engine{pending_activations = Activations}.


