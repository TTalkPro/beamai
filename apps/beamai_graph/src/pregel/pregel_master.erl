%%%-------------------------------------------------------------------
%%% @doc Pregel Master 进程模块 (全局状态模式)
%%%
%%% 协调整个 Pregel 图计算:
%%% - 持有全局状态 (global_state)
%%% - 启动和管理 Worker 进程
%%% - 协调超步执行（BSP 同步屏障）
%%% - 收集 Worker 的 delta 并合并到全局状态
%%% - 广播全局状态给所有 Worker
%%% - 延迟提交：出错时暂存 delta，不 apply
%%%
%%% 执行模式: 步进式执行，由外部控制循环
%%% 设计模式: gen_server 行为模式 + 协调者模式
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_master).
-behaviour(gen_server).

%% API
-export([start_link/3, step/1, get_snapshot_data/1, get_result/1, stop/1]).
-export([get_global_state/1]).
%% 重试 API
-export([retry/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 类型导出
-export_type([opts/0, result/0, restore_opts/0, snapshot_data/0]).
-export_type([step_result/0, superstep_info/0, snapshot_type/0]).
-export_type([field_reducer/0, field_reducers/0, delta/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type graph() :: pregel_graph:graph().
-type vertex_id() :: pregel_vertex:vertex_id().
-type vertex() :: pregel_vertex:vertex().
-type compute_fn() :: fun((pregel_worker:compute_context()) -> pregel_worker:compute_result()).

%% Delta 类型：简单的字段 => 值映射
-type delta() :: #{atom() | binary() => term()}.

%% 字段 reducer 类型
-type field_reducer() :: fun((OldValue :: term(), NewValue :: term()) -> term()).
-type field_reducers() :: #{atom() | binary() => field_reducer()}.

%% Snapshot 数据 (全局状态模式 - 无 inbox 版本)
-type snapshot_data() :: #{
    superstep := non_neg_integer(),
    global_state := graph_state:state(),
    pending_deltas := [delta()] | undefined,
    pending_activations := [vertex_id()] | undefined,
    vertices := #{vertex_id() => vertex()}
}.

%% Snapshot 类型
%% - initial: 超步 0 执行前的初始状态
%% - step: 正常超步完成
%% - error: 超步完成但有失败的顶点
%% - interrupt: 超步完成但有中断的顶点（human-in-the-loop）
%% - final: 执行结束（completed 或 max_supersteps）
-type snapshot_type() :: initial | step | error | interrupt | final.

%% 超步信息（step 返回给调用者）
-type superstep_info() :: #{
    type := snapshot_type(),           %% snapshot 类型
    superstep := non_neg_integer(),
    active_count := non_neg_integer(),
    activation_count := non_neg_integer(),  %% 下一超步待激活的顶点数
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

%% Snapshot 恢复选项 (无 inbox 版本)
-type restore_opts() :: #{
    superstep := non_neg_integer(),
    global_state := graph_state:state(),
    pending_deltas => [delta()],
    pending_activations => [vertex_id()],
    vertices => #{vertex_id() => vertex()}
}.

%% Pregel 执行选项
-type opts() :: #{
    max_supersteps => pos_integer(),
    num_workers => pos_integer(),
    global_state => graph_state:state(),        %% 初始全局状态
    field_reducers => field_reducers(),         %% 字段级 reducer 配置
    restore_from => restore_opts()              %% 从 snapshot 恢复
}.

%% Pregel 执行结果
-type result() :: #{
    status := completed | max_supersteps,
    global_state := graph_state:state(),
    graph := graph(),
    supersteps := non_neg_integer(),
    stats := #{atom() => term()}
}.

%% Master 内部状态 (无 inbox 版本)
%% 注：节点计算逻辑和路由规则已存储在 vertex value 中，无需通过 config 传递
-record(state, {
    graph            :: graph(),                    %% 原始图
    compute_fn       :: compute_fn(),               %% 计算函数
    max_supersteps   :: pos_integer(),              %% 最大超步数
    num_workers      :: pos_integer(),              %% Worker 数
    workers          :: #{non_neg_integer() => pid()},  %% Worker 映射
    superstep        :: non_neg_integer(),          %% 当前超步
    barrier          :: pregel_barrier:t(),         %% 同步屏障
    step_caller      :: gen_server:from() | undefined,  %% step 调用者
    restore_from     :: restore_opts() | undefined,  %% 恢复选项（启动后消费）
    %% 全局状态模式字段
    global_state     :: graph_state:state(),        %% 全局状态
    field_reducers   :: field_reducers(),           %% 字段级 reducer
    pending_deltas   :: [delta()] | undefined,      %% 延迟提交的 deltas
    pending_activations :: [vertex_id()] | undefined,  %% 延迟提交的 activations
    %% 超步结果（用于 get_snapshot_data 和重试）
    last_results     :: pregel_barrier:superstep_results() | undefined,
    %% 累积失败（跨超步追踪）
    cumulative_failures :: [{vertex_id(), term()}],
    %% 状态标志
    initialized      :: boolean(),                  %% 是否已初始化 workers
    initial_returned :: boolean(),                  %% 是否已返回 initial snapshot
    halted           :: boolean()                   %% 是否已终止
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Master 进程
-spec start_link(graph(), compute_fn(), opts()) -> {ok, pid()} | {error, term()}.
start_link(Graph, ComputeFn, Opts) ->
    gen_server:start_link(?MODULE, {Graph, ComputeFn, Opts}, []).

%% @doc 执行单个超步（同步调用）
%% 返回 {continue, Info} 表示可以继续，{done, Reason, Info} 表示已终止
-spec step(pid()) -> step_result().
step(Master) ->
    gen_server:call(Master, step, infinity).

%% @doc 重试指定顶点
-spec retry(pid(), [vertex_id()]) -> step_result().
retry(Master, VertexIds) ->
    gen_server:call(Master, {retry, VertexIds}, infinity).

%% @doc 获取当前 snapshot 数据
-spec get_snapshot_data(pid()) -> snapshot_data().
get_snapshot_data(Master) ->
    gen_server:call(Master, get_snapshot_data, infinity).

%% @doc 获取当前全局状态
-spec get_global_state(pid()) -> graph_state:state().
get_global_state(Master) ->
    gen_server:call(Master, get_global_state, infinity).

%% @doc 获取最终结果（仅在 halted 后调用）
-spec get_result(pid()) -> result().
get_result(Master) ->
    gen_server:call(Master, get_result, infinity).

%% @doc 停止 Master 和所有 Worker
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server 回调
%%====================================================================

init({Graph, ComputeFn, Opts}) ->
    %% 解析恢复选项
    RestoreOpts = maps:get(restore_from, Opts, undefined),
    InitialSuperstep = get_restore_superstep(RestoreOpts),

    %% 初始化全局状态
    GlobalState = case RestoreOpts of
        #{global_state := RestoredGS} -> RestoredGS;
        _ -> maps:get(global_state, Opts, graph_state:new())
    end,

    %% 初始化 pending_deltas 和 pending_activations（从恢复选项）
    PendingDeltas = case RestoreOpts of
        #{pending_deltas := PD} -> PD;
        _ -> undefined
    end,
    PendingActivations = case RestoreOpts of
        #{pending_activations := PA} -> PA;
        _ -> undefined
    end,

    State = #state{
        graph = Graph,
        compute_fn = ComputeFn,
        max_supersteps = maps:get(max_supersteps, Opts, 100),
        num_workers = maps:get(num_workers, Opts, erlang:system_info(schedulers)),
        workers = #{},
        superstep = InitialSuperstep,
        barrier = pregel_barrier:new(0),
        step_caller = undefined,
        restore_from = RestoreOpts,
        global_state = GlobalState,
        field_reducers = maps:get(field_reducers, Opts, #{}),
        pending_deltas = PendingDeltas,
        pending_activations = PendingActivations,
        last_results = undefined,
        cumulative_failures = [],
        initialized = false,
        initial_returned = false,
        halted = false
    },
    {ok, State}.

handle_call(step, _From, #state{halted = true} = State) ->
    %% 已终止，返回最后的信息
    Info = pregel_superstep:build_superstep_info(final, State#state.last_results),
    {reply, {done, get_done_reason(State), Info}, State};

handle_call(step, _From, #state{initialized = false} = State) ->
    %% 首次 step：初始化 workers，返回 initial snapshot
    %% 不启动超步，让调用者有机会保存初始状态
    StateWithWorkers = start_workers(State),
    %% 注入恢复的 activations（如果有）
    StateReady = inject_restore_activations(StateWithWorkers),
    StateFinal = StateReady#state{
        restore_from = undefined,
        initialized = true,
        initial_returned = true
    },
    %% 构建 initial 类型的 info
    Info = pregel_superstep:build_superstep_info(initial, undefined),
    {reply, {continue, Info}, StateFinal};

handle_call(step, From, #state{initialized = true, pending_activations = PendingActivations} = State) ->
    %% 后续 step：广播全局状态，启动超步执行
    broadcast_global_state(State),
    %% 获取要激活的顶点列表（pending_activations 或 last_results 的 activations）
    Activations = pregel_superstep:get_activations_for_superstep(PendingActivations, State#state.last_results),
    NewState = State#state{
        step_caller = From,
        barrier = pregel_barrier:new(maps:size(State#state.workers)),
        pending_activations = undefined  %% 清除已使用的 pending_activations
    },
    broadcast_start_superstep(NewState, Activations),
    {noreply, NewState};

handle_call({retry, _VertexIds}, _From, #state{halted = true} = State) ->
    %% 已终止，不能重试
    Info = pregel_superstep:build_superstep_info(final, State#state.last_results),
    {reply, {done, get_done_reason(State), Info}, State};

handle_call({retry, _VertexIds}, _From, #state{last_results = undefined} = State) ->
    %% 还没有执行过 step，不能重试
    {reply, {error, no_previous_step}, State};

handle_call({retry, VertexIds}, From, #state{pending_deltas = undefined} = State) ->
    %% 没有 pending_deltas，不能重试
    NewState = State#state{step_caller = From},
    execute_retry(VertexIds, NewState);

handle_call({retry, VertexIds}, From, #state{pending_deltas = _PendingDeltas} = State) ->
    %% 有 pending_deltas，执行延迟提交重试
    NewState = State#state{step_caller = From},
    execute_deferred_retry(VertexIds, NewState);

handle_call(get_snapshot_data, _From, #state{
    superstep = Superstep,
    global_state = GlobalState,
    pending_deltas = PendingDeltas,
    pending_activations = PendingActivations,
    workers = Workers,
    last_results = Results
} = State) ->
    Vertices = collect_vertices_from_workers(Workers),
    %% 获取 activations：优先使用 pending_activations，否则从 last_results 获取
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
    workers = Workers,
    graph = OriginalGraph,
    global_state = GlobalState,
    cumulative_failures = CumulativeFailures,
    halted = true
} = State) ->
    FinalGraph = collect_final_graph(Workers, OriginalGraph),
    %% 使用累积的失败信息（跨超步追踪）
    FailedCount = length(CumulativeFailures),
    Result = #{
        status => get_done_reason(State),
        global_state => GlobalState,
        graph => FinalGraph,
        supersteps => Superstep + 1,
        stats => #{num_workers => maps:size(Workers)},
        %% 添加累积失败信息
        failed_count => FailedCount,
        failed_vertices => CumulativeFailures
    },
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({worker_done, _WorkerPid, Result}, State) ->
    NewState = handle_worker_done(Result, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{workers = Workers}) ->
    %% 停止所有 Worker
    maps:foreach(fun(_Id, Pid) -> catch pregel_worker:stop(Pid) end, Workers),
    ok.

%%====================================================================
%% Worker 管理
%%====================================================================

%% @private 启动所有 Worker 进程
%%
%% 节点计算逻辑和路由规则已存储在 vertex value 中，无需通过 config 传递
-spec start_workers(#state{}) -> #state{}.
start_workers(#state{
    graph = Graph,
    compute_fn = ComputeFn,
    num_workers = NumWorkers,
    global_state = GlobalState,
    restore_from = RestoreOpts
} = State) ->
    %% 分区图
    Partitions = pregel_partition:partition_graph(Graph, NumWorkers),
    NumVertices = pregel_graph:size(Graph),

    %% 获取恢复的顶点状态
    RestoredVertices = get_restore_vertices(RestoreOpts),

    %% 启动 Worker（传入全局状态）
    Workers = start_worker_processes(Partitions, ComputeFn, NumWorkers, NumVertices,
                                      RestoredVertices, GlobalState),

    %% 广播 Worker PID 映射
    broadcast_worker_pids(Workers),

    State#state{
        workers = Workers,
        barrier = pregel_barrier:new(maps:size(Workers))
    }.

%% @private 启动各 Worker 进程
%%
%% vertex value 已包含 node（计算函数）和 edges（路由规则）
-spec start_worker_processes(#{non_neg_integer() => graph()},
                              compute_fn(),
                              pos_integer(),
                              non_neg_integer(),
                              #{vertex_id() => vertex()},
                              graph_state:state()) ->
    #{non_neg_integer() => pid()}.
start_worker_processes(Partitions, ComputeFn, NumWorkers, NumVertices,
                       RestoredVertices, GlobalState) ->
    maps:fold(
        fun(WorkerId, WorkerGraph, Acc) ->
            %% 获取分区顶点（vertex value 包含 node 和 edges）
            PartitionVertices = vertices_to_map(pregel_graph:vertices(WorkerGraph)),
            %% 合并恢复的顶点（恢复的顶点覆盖原顶点）
            Vertices = merge_restored_vertices(PartitionVertices, RestoredVertices),
            Opts = #{
                worker_id => WorkerId,
                master => self(),
                vertices => Vertices,
                compute_fn => ComputeFn,
                num_workers => NumWorkers,
                num_vertices => NumVertices,
                global_state => GlobalState
            },
            {ok, Pid} = pregel_worker:start_link(WorkerId, Opts),
            Acc#{WorkerId => Pid}
        end,
        #{},
        Partitions
    ).

%% @private 将顶点列表转换为映射
-spec vertices_to_map([pregel_vertex:vertex()]) -> #{pregel_vertex:vertex_id() => pregel_vertex:vertex()}.
vertices_to_map(Vertices) ->
    maps:from_list([{pregel_vertex:id(V), V} || V <- Vertices]).

%% @private 广播 Worker PID 映射
-spec broadcast_worker_pids(#{non_neg_integer() => pid()}) -> ok.
broadcast_worker_pids(Workers) ->
    maps:foreach(
        fun(_Id, Pid) ->
            gen_server:cast(Pid, {update_worker_pids, Workers})
        end,
        Workers
    ).

%%====================================================================
%% 超步协调
%%====================================================================

%% @private 广播全局状态给所有 Worker
-spec broadcast_global_state(#state{}) -> ok.
broadcast_global_state(#state{workers = Workers, global_state = GlobalState}) ->
    maps:foreach(
        fun(_Id, Pid) ->
            gen_server:cast(Pid, {global_state, GlobalState})
        end,
        Workers
    ).

%% @private 广播开始超步（带 activations，支持 dispatch）
-spec broadcast_start_superstep(#state{}, [vertex_id() | {dispatch, graph_dispatch:dispatch()}]) -> ok.
broadcast_start_superstep(#state{workers = Workers, num_workers = NumWorkers, superstep = Superstep}, Activations) ->
    %% 分离 dispatch 项和普通 activations
    {DispatchItems, NormalActivations} = pregel_superstep:separate_dispatches(Activations),
    VertexInputs = pregel_superstep:build_vertex_inputs(DispatchItems),
    DispatchNodeIds = maps:keys(VertexInputs),
    AllActivations = lists:usort(NormalActivations ++ DispatchNodeIds),
    %% 按 Worker 分组
    GroupedActivations = pregel_superstep:group_activations_by_worker(AllActivations, NumWorkers),
    GroupedVertexInputs = pregel_superstep:group_vertex_inputs_by_worker(VertexInputs, NumWorkers),
    maps:foreach(
        fun(WorkerId, Pid) ->
            WorkerActivations = maps:get(WorkerId, GroupedActivations, []),
            WorkerVertexInputs = maps:get(WorkerId, GroupedVertexInputs, #{}),
            pregel_worker:start_superstep(Pid, Superstep, WorkerActivations, WorkerVertexInputs)
        end,
        Workers
    ).

%% @private 处理 Worker 完成通知
-spec handle_worker_done(map(), #state{}) -> #state{}.
handle_worker_done(Result, #state{barrier = Barrier} = State) ->
    NewBarrier = pregel_barrier:record_done(Result, Barrier),
    NewState = State#state{barrier = NewBarrier},
    case pregel_barrier:is_complete(NewBarrier) of
        true -> complete_superstep(NewState);
        false -> NewState
    end.

%%====================================================================
%% Delta 处理与全局状态更新
%%====================================================================

%% Delta 应用逻辑委托给 graph_state_reducer 模块，避免代码重复。
%% 参见 graph_state_reducer:apply_deltas/3

%% @private 完成超步处理
%%
%% 全局状态模式（无 inbox 版本）：收集所有 Worker 的 delta 和 activations
%% 委托给 pregel_superstep 模块处理
-spec complete_superstep(#state{}) -> #state{}.
complete_superstep(#state{
    barrier = Barrier,
    superstep = Superstep,
    max_supersteps = MaxSupersteps,
    global_state = GlobalState,
    field_reducers = FieldReducers,
    cumulative_failures = CumulativeFailures,
    step_caller = Caller
} = State) ->
    %% 委托给 pregel_superstep 模块
    {NewGlobalState, NewPendingDeltas, NewPendingActivations,
     NewSuperstep, Halted, Reply, UpdatedResults, NewCumulativeFailures} =
        pregel_superstep:complete_superstep(
            Barrier, Superstep, MaxSupersteps, GlobalState,
            FieldReducers, CumulativeFailures, Caller),

    %% 更新状态
    NewState = State#state{
        global_state = NewGlobalState,
        pending_deltas = NewPendingDeltas,
        pending_activations = NewPendingActivations,
        last_results = UpdatedResults,
        cumulative_failures = NewCumulativeFailures,
        step_caller = undefined,
        superstep = NewSuperstep,
        halted = Halted
    },

    %% 回复调用者
    gen_server:reply(Caller, Reply),

    NewState.

%% @private 执行顶点重试（无 pending_deltas）
%% 委托给 pregel_retry 模块处理
-spec execute_retry([vertex_id()], #state{}) -> {noreply, #state{}}.
execute_retry(VertexIds, #state{
    workers = Workers,
    superstep = Superstep,
    last_results = LastResults,
    global_state = GlobalState,
    field_reducers = FieldReducers,
    step_caller = Caller
} = State) ->
    %% 1. 广播当前全局状态
    broadcast_global_state(State),

    %% 2. 委托给 pregel_retry 模块执行重试
    {RetryDeltas, RetryActivations, StillFailed, StillInterrupted, _RetryResults} =
        pregel_retry:execute_retry(Workers, VertexIds, GlobalState, FieldReducers),

    HasError = length(StillFailed) > 0 orelse length(StillInterrupted) > 0,

    %% 3. 根据是否有错误决定处理方式
    {NewGlobalState, NewPendingDeltas, NewPendingActivations} = case HasError of
        true ->
            %% 仍有错误，暂存
            LastActivations = maps:get(activations, LastResults, []),
            {GlobalState, RetryDeltas, LastActivations ++ RetryActivations};
        false ->
            %% 成功，apply deltas
            UpdatedState = graph_state_reducer:apply_deltas(GlobalState, RetryDeltas, FieldReducers),
            {UpdatedState, undefined, undefined}
    end,

    %% 4. 更新结果
    UpdatedResults = LastResults#{
        failed_count => length(StillFailed),
        failed_vertices => StillFailed,
        interrupted_count => length(StillInterrupted),
        interrupted_vertices => StillInterrupted,
        activation_count => maps:get(activation_count, LastResults, 0) + length(RetryActivations),
        activations => maps:get(activations, LastResults, []) ++ RetryActivations,
        superstep => Superstep
    },

    %% 5. 确定 snapshot 类型并构建返回信息
    Type = pregel_superstep:determine_snapshot_type(UpdatedResults),
    Info = pregel_superstep:build_superstep_info(Type, UpdatedResults),

    %% 6. 更新状态
    NewState = State#state{
        global_state = NewGlobalState,
        pending_deltas = NewPendingDeltas,
        pending_activations = NewPendingActivations,
        last_results = UpdatedResults,
        step_caller = undefined
    },

    %% 7. 回复调用者
    gen_server:reply(Caller, {continue, Info}),

    {noreply, NewState}.

%% @private 执行延迟提交重试
%% 委托给 pregel_retry 模块处理
-spec execute_deferred_retry([vertex_id()], #state{}) -> {noreply, #state{}}.
execute_deferred_retry(VertexIds, #state{
    workers = Workers,
    superstep = Superstep,
    pending_deltas = PendingDeltas,
    pending_activations = PendingActivations,
    global_state = GlobalState,
    field_reducers = FieldReducers,
    step_caller = Caller
} = State) ->
    %% 1. 广播当前全局状态
    broadcast_global_state(State),

    %% 2. 委托给 pregel_retry 模块执行延迟提交重试
    {MergedDeltas, MergedActivations, StillFailed, StillInterrupted, _RetryResults} =
        pregel_retry:execute_deferred_retry(
            Workers, VertexIds, PendingDeltas, PendingActivations, GlobalState),

    HasError = length(StillFailed) > 0 orelse length(StillInterrupted) > 0,

    %% 3. 根据是否有错误决定处理方式
    {NewGlobalState, NewPendingDeltas, NewPendingActivations} = case HasError of
        true ->
            %% 仍有错误，继续暂存
            {GlobalState, MergedDeltas, MergedActivations};
        false ->
            %% 全部成功，apply 所有 deltas
            UpdatedState = graph_state_reducer:apply_deltas(GlobalState, MergedDeltas, FieldReducers),
            {UpdatedState, undefined, undefined}
    end,

    %% 4. 构建更新结果
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

    %% 5. 确定 snapshot 类型并构建返回信息
    Type = pregel_superstep:determine_snapshot_type(UpdatedResults),
    Info = pregel_superstep:build_superstep_info(Type, UpdatedResults),

    %% 6. 更新状态
    NewState = State#state{
        global_state = NewGlobalState,
        pending_deltas = NewPendingDeltas,
        pending_activations = NewPendingActivations,
        last_results = UpdatedResults,
        step_caller = undefined
    },

    %% 7. 回复调用者
    gen_server:reply(Caller, {continue, Info}),

    {noreply, NewState}.

%%====================================================================
%% 辅助函数
%%====================================================================

%% 超步相关函数已移至 pregel_superstep 模块
%% 重试相关函数已移至 pregel_retry 模块

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

%% @private 收集最终图
-spec collect_final_graph(#{non_neg_integer() => pid()}, graph()) -> graph().
collect_final_graph(Workers, OriginalGraph) ->
    AllVertices = maps:fold(
        fun(_WorkerId, Pid, Acc) ->
            maps:merge(Acc, pregel_worker:get_vertices(Pid))
        end,
        #{},
        Workers
    ),
    pregel_graph:map(OriginalGraph, fun(Vertex) ->
        Id = pregel_vertex:id(Vertex),
        maps:get(Id, AllVertices, Vertex)
    end).

%% @private 从所有 Worker 收集顶点状态
-spec collect_vertices_from_workers(#{non_neg_integer() => pid()}) ->
    #{vertex_id() => vertex()}.
collect_vertices_from_workers(Workers) ->
    maps:fold(
        fun(_WorkerId, Pid, Acc) ->
            maps:merge(Acc, pregel_worker:get_vertices(Pid))
        end,
        #{},
        Workers
    ).

%%====================================================================
%% Snapshot 恢复辅助函数
%%====================================================================

%% @private 获取恢复选项中的超步号
-spec get_restore_superstep(restore_opts() | undefined) -> non_neg_integer().
get_restore_superstep(undefined) -> 0;
get_restore_superstep(#{superstep := Superstep}) -> Superstep;
get_restore_superstep(_) -> 0.

%% @private 获取恢复选项中的顶点状态
-spec get_restore_vertices(restore_opts() | undefined) -> #{vertex_id() => vertex()}.
get_restore_vertices(undefined) -> #{};
get_restore_vertices(#{vertices := Vertices}) -> Vertices;
get_restore_vertices(_) -> #{}.

%% @private 获取恢复选项中的 activations 列表
-spec get_restore_activations(restore_opts() | undefined) -> [vertex_id()] | undefined.
get_restore_activations(undefined) -> undefined;
get_restore_activations(#{pending_activations := Activations}) -> Activations;
get_restore_activations(_) -> undefined.

%% @private 注入恢复的 activations 到状态
-spec inject_restore_activations(#state{}) -> #state{}.
inject_restore_activations(#state{restore_from = RestoreOpts} = State) ->
    Activations = get_restore_activations(RestoreOpts),
    State#state{pending_activations = Activations}.

%% @private 合并恢复的顶点到分区顶点
-spec merge_restored_vertices(#{vertex_id() => vertex()},
                               #{vertex_id() => vertex()}) ->
    #{vertex_id() => vertex()}.
merge_restored_vertices(PartitionVertices, RestoredVertices) ->
    maps:fold(
        fun(Id, RestoredVertex, Acc) ->
            case maps:is_key(Id, Acc) of
                true -> Acc#{Id => RestoredVertex};
                false -> Acc
            end
        end,
        PartitionVertices,
        RestoredVertices
    ).
