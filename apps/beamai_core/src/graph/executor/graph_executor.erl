%%%-------------------------------------------------------------------
%%% @doc Graph Executor - 统一图执行引擎
%%%
%%% 替代 pregel_master + pregel_worker + pregel_barrier + pregel_superstep + pregel_retry。
%%% 单协调进程 + 统一 poolboy 池架构。
%%%
%%% 核心特点:
%%% - 无分区：所有顶点直接存储在协调进程状态中
%%% - 无 barrier：step/1 在 handle_call 内同步执行完整超步
%%% - 统一池：普通顶点和 dispatch fan-out 共用 beamai_graph_pool
%%% - 单顶点失败隔离：失败顶点不影响其他顶点结果
%%% - 延迟 delta：有错误时 defer deltas，重试成功后 apply
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_executor).
-behaviour(gen_server).

%% 生命周期 API（替代 pregel:start/step/stop）
-export([start_link/3, step/1, retry/2, get_snapshot_data/1,
         get_global_state/1, get_result/1, stop/1]).
%% 简化运行 API（替代 pregel:run）
-export([run/2, run/3]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 类型导出
-export_type([opts/0, result/0, restore_opts/0, snapshot_data/0]).
-export_type([step_result/0, superstep_info/0, snapshot_type/0, done_reason/0]).
-export_type([field_reducer/0, field_reducers/0, delta/0]).
-export_type([context/0, compute_result/0, compute_status/0, vertex_id/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type graph() :: pregel_graph:graph().
-type vertex_id() :: pregel_vertex:vertex_id().
-type vertex() :: pregel_vertex:vertex().
-type compute_fn() :: fun((context()) -> compute_result()).

%% Delta 类型
-type delta() :: #{atom() | binary() => term()}.

%% 字段 reducer 类型
-type field_reducer() :: fun((OldValue :: term(), NewValue :: term()) -> term()).
-type field_reducers() :: #{atom() | binary() => field_reducer()}.

%% 计算上下文（传递给计算函数）
-type context() :: #{
    vertex_id := vertex_id(),
    vertex := vertex(),
    global_state := graph_state:state(),
    vertex_input := map() | undefined,
    superstep := non_neg_integer(),
    num_vertices := non_neg_integer()
}.

%% 计算结果状态
-type compute_status() :: ok | {error, term()} | {interrupt, term()}.

%% 计算结果
-type compute_result() :: #{
    delta := delta(),
    activations => [vertex_id()],
    status := compute_status()
}.

%% Snapshot 数据
-type snapshot_data() :: #{
    superstep := non_neg_integer(),
    global_state := graph_state:state(),
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
    global_state := graph_state:state(),
    pending_deltas => [delta()],
    pending_activations => [vertex_id()],
    vertices => #{vertex_id() => vertex()}
}.

%% 执行选项
-type opts() :: #{
    max_supersteps => pos_integer(),
    global_state => graph_state:state(),
    field_reducers => field_reducers(),
    restore_from => restore_opts()
}.

%% 执行结果
-type result() :: #{
    status := completed | max_supersteps,
    global_state := graph_state:state(),
    graph := graph(),
    supersteps := non_neg_integer(),
    stats := #{atom() => term()},
    failed_count => non_neg_integer(),
    failed_vertices => [{vertex_id(), term()}]
}.

%% 内部状态
-record(state, {
    graph            :: graph(),
    vertices         :: #{vertex_id() => vertex()},
    compute_fn       :: compute_fn(),

    max_supersteps   :: pos_integer(),
    superstep        :: non_neg_integer(),

    global_state     :: graph_state:state(),
    field_reducers   :: field_reducers(),
    pending_deltas   :: [delta()] | undefined,
    pending_activations :: [vertex_id()] | undefined,

    last_results     :: map() | undefined,
    cumulative_failures :: [{vertex_id(), term()}],

    pool_name        :: atom(),
    pool_timeout     :: pos_integer(),

    step_caller      :: gen_server:from() | undefined,
    restore_from     :: restore_opts() | undefined,
    initialized      :: boolean(),
    initial_returned :: boolean(),
    halted           :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Executor 进程
-spec start_link(graph(), compute_fn(), opts()) -> {ok, pid()} | {error, term()}.
start_link(Graph, ComputeFn, Opts) ->
    gen_server:start_link(?MODULE, {Graph, ComputeFn, Opts}, []).

%% @doc 执行单个超步（同步调用）
-spec step(pid()) -> step_result().
step(Executor) ->
    gen_server:call(Executor, step, infinity).

%% @doc 重试指定顶点
-spec retry(pid(), [vertex_id()]) -> step_result().
retry(Executor, VertexIds) ->
    gen_server:call(Executor, {retry, VertexIds}, infinity).

%% @doc 获取当前 snapshot 数据
-spec get_snapshot_data(pid()) -> snapshot_data().
get_snapshot_data(Executor) ->
    gen_server:call(Executor, get_snapshot_data, infinity).

%% @doc 获取当前全局状态
-spec get_global_state(pid()) -> graph_state:state().
get_global_state(Executor) ->
    gen_server:call(Executor, get_global_state, infinity).

%% @doc 获取最终结果
-spec get_result(pid()) -> result().
get_result(Executor) ->
    gen_server:call(Executor, get_result, infinity).

%% @doc 停止 Executor
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc 执行图计算（使用默认选项）
-spec run(graph(), compute_fn()) -> result().
run(Graph, ComputeFn) ->
    run(Graph, ComputeFn, #{}).

%% @doc 执行图计算（带选项）
-spec run(graph(), compute_fn(), opts()) -> result().
run(Graph, ComputeFn, Opts) ->
    {ok, Executor} = start_link(Graph, ComputeFn, Opts),
    try
        run_loop(Executor)
    after
        stop(Executor)
    end.

%%====================================================================
%% gen_server 回调
%%====================================================================

init({Graph, ComputeFn, Opts}) ->
    RestoreOpts = maps:get(restore_from, Opts, undefined),
    InitialSuperstep = get_restore_superstep(RestoreOpts),

    GlobalState = case RestoreOpts of
        #{global_state := RestoredGS} -> RestoredGS;
        _ -> maps:get(global_state, Opts, graph_state:new())
    end,

    PendingDeltas = case RestoreOpts of
        #{pending_deltas := PD} -> PD;
        _ -> undefined
    end,
    PendingActivations = case RestoreOpts of
        #{pending_activations := PA} -> PA;
        _ -> undefined
    end,

    %% 获取所有顶点（无分区）
    Vertices = graph_executor_utils:get_all_vertices(Graph, RestoreOpts),

    PoolTimeout = application:get_env(beamai_core, graph_pool_timeout, 30000),

    State = #state{
        graph = Graph,
        vertices = Vertices,
        compute_fn = ComputeFn,
        max_supersteps = maps:get(max_supersteps, Opts, 100),
        superstep = InitialSuperstep,
        global_state = GlobalState,
        field_reducers = maps:get(field_reducers, Opts, #{}),
        pending_deltas = PendingDeltas,
        pending_activations = PendingActivations,
        last_results = undefined,
        cumulative_failures = [],
        pool_name = beamai_graph_pool,
        pool_timeout = PoolTimeout,
        step_caller = undefined,
        restore_from = RestoreOpts,
        initialized = false,
        initial_returned = false,
        halted = false
    },
    {ok, State}.

handle_call(step, _From, #state{halted = true} = State) ->
    Info = graph_executor_utils:build_superstep_info(final, State#state.last_results),
    {reply, {done, get_done_reason(State), Info}, State};

handle_call(step, _From, #state{initialized = false} = State) ->
    %% 首次 step：返回 initial snapshot
    StateReady = inject_restore_activations(State),
    StateFinal = StateReady#state{
        restore_from = undefined,
        initialized = true,
        initial_returned = true
    },
    Info = graph_executor_utils:build_superstep_info(initial, undefined),
    {reply, {continue, Info}, StateFinal};

handle_call(step, _From, #state{initialized = true} = State) ->
    %% 后续 step：同步执行完整超步
    NewState = execute_superstep(State),
    {reply, NewState#state.step_caller, reset_step_caller(NewState)};

handle_call({retry, _VertexIds}, _From, #state{halted = true} = State) ->
    Info = graph_executor_utils:build_superstep_info(final, State#state.last_results),
    {reply, {done, get_done_reason(State), Info}, State};

handle_call({retry, _VertexIds}, _From, #state{last_results = undefined} = State) ->
    {reply, {error, no_previous_step}, State};

handle_call({retry, VertexIds}, _From, #state{pending_deltas = undefined} = State) ->
    NewState = execute_retry_direct(VertexIds, State),
    {reply, NewState#state.step_caller, reset_step_caller(NewState)};

handle_call({retry, VertexIds}, _From, State) ->
    NewState = execute_deferred_retry(VertexIds, State),
    {reply, NewState#state.step_caller, reset_step_caller(NewState)};

handle_call(get_snapshot_data, _From, #state{
    superstep = Superstep,
    global_state = GlobalState,
    pending_deltas = PendingDeltas,
    pending_activations = PendingActivations,
    vertices = Vertices,
    last_results = Results
} = State) ->
    Activations = case PendingActivations of
        undefined ->
            case Results of
                undefined -> undefined;
                _ -> maps:get(activations, Results, undefined)
            end;
        _ -> PendingActivations
    end,
    Data = #{
        superstep => Superstep,
        global_state => GlobalState,
        pending_deltas => PendingDeltas,
        pending_activations => Activations,
        vertices => Vertices
    },
    {reply, Data, State};

handle_call(get_global_state, _From, #state{global_state = GlobalState} = State) ->
    {reply, GlobalState, State};

handle_call(get_result, _From, #state{halted = false} = State) ->
    {reply, {error, not_halted}, State};

handle_call(get_result, _From, #state{
    superstep = Superstep,
    graph = OriginalGraph,
    vertices = Vertices,
    global_state = GlobalState,
    cumulative_failures = CumulativeFailures,
    halted = true
} = State) ->
    FinalGraph = graph_executor_utils:rebuild_graph(OriginalGraph, Vertices),
    FailedCount = length(CumulativeFailures),
    Result = #{
        status => get_done_reason(State),
        global_state => GlobalState,
        graph => FinalGraph,
        supersteps => Superstep + 1,
        stats => #{},
        failed_count => FailedCount,
        failed_vertices => CumulativeFailures
    },
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% 超步执行（核心算法）
%%====================================================================

%% @private 执行完整超步（同步）
-spec execute_superstep(#state{}) -> #state{}.
execute_superstep(#state{
    vertices = Vertices,
    compute_fn = ComputeFn,
    superstep = Superstep,
    global_state = GlobalState,
    field_reducers = FieldReducers,
    pending_activations = PendingActivations,
    last_results = LastResults,
    cumulative_failures = CumulativeFailures,
    max_supersteps = MaxSupersteps,
    pool_name = PoolName,
    pool_timeout = PoolTimeout
} = State) ->
    NumVertices = maps:size(Vertices),

    %% 1. 获取 activations
    Activations = graph_executor_utils:get_activations_for_superstep(PendingActivations, LastResults),

    %% 2. 分离 dispatch 项与普通激活
    {DispatchItems, NormalActivations} = graph_executor_utils:separate_dispatches(Activations),
    VertexInputs = graph_executor_utils:build_vertex_inputs(DispatchItems),
    DispatchNodeIds = maps:keys(VertexInputs),
    AllActivationIds = lists:usort(NormalActivations ++ DispatchNodeIds),

    %% 3. 筛选活跃顶点
    ActiveVertices = graph_executor_utils:filter_active_vertices(Vertices, AllActivationIds),

    %% 4. 构建扁平任务列表
    Tasks = graph_executor_task:build_task_list(ActiveVertices, VertexInputs),

    %% 5. 执行任务
    {Deltas, NewActivations, FailedVertices, InterruptedVertices} =
        graph_executor_task:execute_tasks(Tasks, ComputeFn, GlobalState, Superstep, NumVertices,
                      PoolName, PoolTimeout),

    %% 6. 更新顶点状态（halt 计算完成的顶点）
    NewVertices = graph_executor_utils:update_vertex_states(Vertices, ActiveVertices, FailedVertices, InterruptedVertices),

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
    {NewGlobalState, NewPendingDeltas, NewPendingActivations} = case HasError of
        true ->
            {GlobalState, Deltas, NewActivations};
        false ->
            UpdatedState = graph_state_reducer:apply_deltas(GlobalState, Deltas, FieldReducers),
            {UpdatedState, undefined, undefined}
    end,

    %% 9. 更新结果映射
    TotalActive = graph_executor_utils:count_active(NewVertices),
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
        false -> graph_executor_utils:determine_snapshot_type(UpdatedResults)
    end,
    Info = graph_executor_utils:build_superstep_info(Type, UpdatedResults),

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

    State#state{
        vertices = NewVertices,
        global_state = NewGlobalState,
        pending_deltas = NewPendingDeltas,
        pending_activations = NewPendingActivations,
        last_results = UpdatedResults,
        cumulative_failures = NewCumulativeFailures,
        superstep = NewSuperstep,
        halted = IsDone,
        step_caller = Reply
    }.

%%====================================================================
%% 重试逻辑
%%====================================================================

%% @private 直接重试（无 pending_deltas）
-spec execute_retry_direct([vertex_id()], #state{}) -> #state{}.
execute_retry_direct(VertexIds, #state{
    vertices = Vertices,
    compute_fn = ComputeFn,
    superstep = Superstep,
    global_state = GlobalState,
    field_reducers = FieldReducers,
    last_results = LastResults,
    pool_name = PoolName,
    pool_timeout = PoolTimeout
} = State) ->
    NumVertices = maps:size(Vertices),

    %% 构建重试任务
    RetryVertices = maps:with(VertexIds, Vertices),
    Tasks = [{Id, V, undefined} || {Id, V} <- maps:to_list(RetryVertices)],

    %% 执行重试
    {RetryDeltas, RetryActivations, StillFailed, StillInterrupted} =
        graph_executor_task:execute_tasks(Tasks, ComputeFn, GlobalState, Superstep, NumVertices,
                      PoolName, PoolTimeout),

    HasError = length(StillFailed) > 0 orelse length(StillInterrupted) > 0,

    {NewGlobalState, NewPendingDeltas, NewPendingActivations} = case HasError of
        true ->
            LastActivations = maps:get(activations, LastResults, []),
            {GlobalState, RetryDeltas, LastActivations ++ RetryActivations};
        false ->
            UpdatedState = graph_state_reducer:apply_deltas(GlobalState, RetryDeltas, FieldReducers),
            {UpdatedState, undefined, undefined}
    end,

    UpdatedResults = LastResults#{
        failed_count => length(StillFailed),
        failed_vertices => StillFailed,
        interrupted_count => length(StillInterrupted),
        interrupted_vertices => StillInterrupted,
        activation_count => maps:get(activation_count, LastResults, 0) + length(RetryActivations),
        activations => maps:get(activations, LastResults, []) ++ RetryActivations,
        superstep => Superstep
    },

    Type = graph_executor_utils:determine_snapshot_type(UpdatedResults),
    Info = graph_executor_utils:build_superstep_info(Type, UpdatedResults),

    State#state{
        global_state = NewGlobalState,
        pending_deltas = NewPendingDeltas,
        pending_activations = NewPendingActivations,
        last_results = UpdatedResults,
        step_caller = {continue, Info}
    }.

%% @private 延迟提交重试
-spec execute_deferred_retry([vertex_id()], #state{}) -> #state{}.
execute_deferred_retry(VertexIds, #state{
    vertices = Vertices,
    compute_fn = ComputeFn,
    superstep = Superstep,
    global_state = GlobalState,
    field_reducers = FieldReducers,
    pending_deltas = PendingDeltas,
    pending_activations = PendingActivations,
    pool_name = PoolName,
    pool_timeout = PoolTimeout
} = State) ->
    NumVertices = maps:size(Vertices),

    RetryVertices = maps:with(VertexIds, Vertices),
    Tasks = [{Id, V, undefined} || {Id, V} <- maps:to_list(RetryVertices)],

    {RetryDeltas, RetryActivations, StillFailed, StillInterrupted} =
        graph_executor_task:execute_tasks(Tasks, ComputeFn, GlobalState, Superstep, NumVertices,
                      PoolName, PoolTimeout),

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

    {NewGlobalState, NewPendingDeltas, NewPendingActivations} = case HasError of
        true ->
            {GlobalState, MergedDeltas, MergedActivations};
        false ->
            UpdatedState = graph_state_reducer:apply_deltas(GlobalState, MergedDeltas, FieldReducers),
            {UpdatedState, undefined, undefined}
    end,

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

    Type = graph_executor_utils:determine_snapshot_type(UpdatedResults),
    Info = graph_executor_utils:build_superstep_info(Type, UpdatedResults),

    State#state{
        global_state = NewGlobalState,
        pending_deltas = NewPendingDeltas,
        pending_activations = NewPendingActivations,
        last_results = UpdatedResults,
        step_caller = {continue, Info}
    }.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 获取终止原因
-spec get_done_reason(#state{}) -> done_reason().
get_done_reason(#state{superstep = Superstep, max_supersteps = MaxSupersteps, last_results = Results}) ->
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
-spec inject_restore_activations(#state{}) -> #state{}.
inject_restore_activations(#state{restore_from = undefined} = State) ->
    State;
inject_restore_activations(#state{restore_from = RestoreOpts} = State) ->
    Activations = case RestoreOpts of
        #{pending_activations := PA} -> PA;
        _ -> undefined
    end,
    State#state{pending_activations = Activations}.

%% @private 重置 step_caller（reply 已经通过 step_caller 字段传递）
-spec reset_step_caller(#state{}) -> #state{}.
reset_step_caller(State) ->
    State#state{step_caller = undefined}.

%% @private 内部执行循环
-spec run_loop(pid()) -> result().
run_loop(Executor) ->
    case step(Executor) of
        {continue, #{type := error} = Info} ->
            build_early_termination_result(Executor, Info);
        {continue, #{type := interrupt} = Info} ->
            build_early_termination_result(Executor, Info);
        {continue, _Info} ->
            run_loop(Executor);
        {done, _Reason, _Info} ->
            get_result(Executor)
    end.

%% @private 构建提前终止时的结果
-spec build_early_termination_result(pid(), superstep_info()) -> result().
build_early_termination_result(Executor, Info) ->
    SnapshotData = get_snapshot_data(Executor),
    #{
        superstep := Superstep,
        global_state := GlobalState
    } = SnapshotData,
    #{
        status => completed,
        global_state => GlobalState,
        graph => #{vertices => #{}},
        supersteps => Superstep,
        stats => #{},
        failed_count => maps:get(failed_count, Info, 0),
        failed_vertices => maps:get(failed_vertices, Info, []),
        interrupted_count => maps:get(interrupted_count, Info, 0),
        interrupted_vertices => maps:get(interrupted_vertices, Info, [])
    }.
