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
-export([start_link/3, step/1, get_checkpoint_data/1, get_result/1, stop/1]).
-export([get_global_state/1]).
%% 重试 API
-export([retry/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 类型导出
-export_type([opts/0, result/0, restore_opts/0, checkpoint_data/0]).
-export_type([step_result/0, superstep_info/0, checkpoint_type/0]).
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

%% Checkpoint 数据 (全局状态模式)
-type checkpoint_data() :: #{
    superstep := non_neg_integer(),
    global_state := graph_state:state(),
    pending_deltas := [delta()] | undefined,
    vertices := #{vertex_id() => vertex()},
    vertex_inbox := #{vertex_id() => [term()]}
}.

%% Checkpoint 类型
%% - initial: 超步 0 执行前的初始状态
%% - step: 正常超步完成
%% - error: 超步完成但有失败的顶点
%% - interrupt: 超步完成但有中断的顶点（human-in-the-loop）
%% - final: 执行结束（completed 或 max_supersteps）
-type checkpoint_type() :: initial | step | error | interrupt | final.

%% 超步信息（step 返回给调用者）
-type superstep_info() :: #{
    type := checkpoint_type(),           %% checkpoint 类型
    superstep := non_neg_integer(),
    active_count := non_neg_integer(),
    message_count := non_neg_integer(),
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

%% Checkpoint 恢复选项
-type restore_opts() :: #{
    superstep := non_neg_integer(),
    global_state := graph_state:state(),
    pending_deltas => [delta()],
    vertices => #{vertex_id() => vertex()},
    messages => [{vertex_id(), term()}]
}.

%% Pregel 执行选项
-type opts() :: #{
    max_supersteps => pos_integer(),
    num_workers => pos_integer(),
    global_state => graph_state:state(),        %% 初始全局状态
    field_reducers => field_reducers(),         %% 字段级 reducer 配置
    restore_from => restore_opts()              %% 从 checkpoint 恢复
}.

%% Pregel 执行结果
-type result() :: #{
    status := completed | max_supersteps,
    global_state := graph_state:state(),
    graph := graph(),
    supersteps := non_neg_integer(),
    stats := #{atom() => term()}
}.

%% Master 内部状态
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
    pending_inbox    :: #{vertex_id() => [term()]} | undefined,  %% 延迟提交的 inbox
    %% 超步结果（用于 get_checkpoint_data 和重试）
    last_results     :: pregel_barrier:superstep_results() | undefined,
    %% 状态标志
    initialized      :: boolean(),                  %% 是否已初始化 workers
    initial_returned :: boolean(),                  %% 是否已返回 initial checkpoint
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

%% @doc 获取当前 checkpoint 数据
-spec get_checkpoint_data(pid()) -> checkpoint_data().
get_checkpoint_data(Master) ->
    gen_server:call(Master, get_checkpoint_data, infinity).

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

    %% 初始化 pending_deltas（从恢复选项）
    PendingDeltas = case RestoreOpts of
        #{pending_deltas := PD} -> PD;
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
        pending_inbox = undefined,
        last_results = undefined,
        initialized = false,
        initial_returned = false,
        halted = false
    },
    {ok, State}.

handle_call(step, _From, #state{halted = true} = State) ->
    %% 已终止，返回最后的信息
    Info = build_superstep_info(final, State#state.last_results),
    {reply, {done, get_done_reason(State), Info}, State};

handle_call(step, _From, #state{initialized = false} = State) ->
    %% 首次 step：初始化 workers，返回 initial checkpoint
    %% 不启动超步，让调用者有机会保存初始状态
    StateWithWorkers = start_workers(State),
    inject_restore_messages(StateWithWorkers),
    StateReady = StateWithWorkers#state{
        restore_from = undefined,
        initialized = true,
        initial_returned = true
    },
    %% 构建 initial 类型的 info
    Info = build_superstep_info(initial, undefined),
    {reply, {continue, Info}, StateReady};

handle_call(step, From, #state{initialized = true} = State) ->
    %% 后续 step：广播全局状态，启动超步执行
    broadcast_global_state(State),
    NewState = State#state{
        step_caller = From,
        barrier = pregel_barrier:new(maps:size(State#state.workers))
    },
    broadcast_start_superstep(NewState),
    {noreply, NewState};

handle_call({retry, _VertexIds}, _From, #state{halted = true} = State) ->
    %% 已终止，不能重试
    Info = build_superstep_info(final, State#state.last_results),
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

handle_call(get_checkpoint_data, _From, #state{
    superstep = Superstep,
    global_state = GlobalState,
    pending_deltas = PendingDeltas,
    workers = Workers,
    last_results = Results
} = State) ->
    Vertices = collect_vertices_from_workers(Workers),
    Inbox = case Results of
        undefined -> #{};
        _ -> maps:get(inbox, Results, #{})
    end,
    Data = #{
        superstep => Superstep,
        global_state => GlobalState,
        pending_deltas => PendingDeltas,
        vertices => Vertices,
        vertex_inbox => Inbox
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
    halted = true
} = State) ->
    FinalGraph = collect_final_graph(Workers, OriginalGraph),
    Result = #{
        status => get_done_reason(State),
        global_state => GlobalState,
        graph => FinalGraph,
        supersteps => Superstep + 1,
        stats => #{num_workers => maps:size(Workers)}
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
            %% 获取分区顶点
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

%% @private 广播开始超步
-spec broadcast_start_superstep(#state{}) -> ok.
broadcast_start_superstep(#state{workers = Workers, superstep = Superstep}) ->
    maps:foreach(
        fun(_Id, Pid) -> pregel_worker:start_superstep(Pid, Superstep) end,
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

%% @private 应用所有 deltas 到全局状态
-spec apply_deltas(graph_state:state(), [delta()], field_reducers()) -> graph_state:state().
apply_deltas(State, Deltas, FieldReducers) ->
    lists:foldl(fun(Delta, AccState) ->
        apply_delta(Delta, AccState, FieldReducers)
    end, State, Deltas).

%% @private 应用单个 delta 到状态
-spec apply_delta(delta(), graph_state:state(), field_reducers()) -> graph_state:state().
apply_delta(Delta, State, FieldReducers) ->
    maps:fold(fun(Field, NewValue, AccState) ->
        OldValue = graph_state:get(AccState, Field),
        %% 标准化 Field 为 binary，因为 graph_state 内部使用 binary 键
        %% FieldReducers 键可能是 atom 或 binary，需要同时尝试
        NormalizedField = normalize_field_key(Field),
        Reducer = find_reducer(Field, NormalizedField, FieldReducers),
        MergedValue = apply_field_reducer(Reducer, OldValue, NewValue),
        graph_state:set(AccState, Field, MergedValue)
    end, State, Delta).

%% @private 标准化字段键为 binary
-spec normalize_field_key(atom() | binary()) -> binary().
normalize_field_key(Field) when is_atom(Field) ->
    atom_to_binary(Field, utf8);
normalize_field_key(Field) when is_binary(Field) ->
    Field.

%% @private 查找字段对应的 reducer
%% 同时尝试原始键和标准化后的 binary 键
-spec find_reducer(term(), binary(), field_reducers()) -> field_reducer().
find_reducer(OrigField, NormalizedField, FieldReducers) ->
    case maps:get(OrigField, FieldReducers, undefined) of
        undefined ->
            maps:get(NormalizedField, FieldReducers, fun last_write_win/2);
        Reducer ->
            Reducer
    end.

%% @private 应用字段 reducer
-spec apply_field_reducer(field_reducer(), term(), term()) -> term().
apply_field_reducer(_Reducer, undefined, NewValue) ->
    NewValue;
apply_field_reducer(_Reducer, OldValue, undefined) ->
    OldValue;
apply_field_reducer(Reducer, OldValue, NewValue) ->
    Reducer(OldValue, NewValue).

%% @private 默认 reducer: last_write_win
-spec last_write_win(term(), term()) -> term().
last_write_win(_Old, New) -> New.

%% @private 完成超步处理
%%
%% 全局状态模式：收集所有 Worker 的 delta，合并到全局状态
-spec complete_superstep(#state{}) -> #state{}.
complete_superstep(#state{
    barrier = Barrier,
    superstep = Superstep,
    max_supersteps = MaxSupersteps,
    workers = Workers,
    num_workers = NumWorkers,
    global_state = GlobalState,
    field_reducers = FieldReducers,
    step_caller = Caller
} = State) ->
    %% 1. 汇总结果
    Results = pregel_barrier:get_results(Barrier),
    AggregatedResults = pregel_barrier:aggregate_results(Results),
    TotalActive = maps:get(active_count, AggregatedResults),

    %% 2. 收集所有 deltas
    AllDeltas = maps:get(deltas, AggregatedResults, []),

    %% 3. 检查是否有失败/中断
    FailedCount = maps:get(failed_count, AggregatedResults, 0),
    InterruptedCount = maps:get(interrupted_count, AggregatedResults, 0),
    HasError = FailedCount > 0 orelse InterruptedCount > 0,

    %% 4. 根据是否有错误决定处理方式
    {NewGlobalState, NewPendingDeltas, NewPendingInbox} = case HasError of
        true ->
            %% 延迟提交：暂存 deltas，不 apply
            Inbox = maps:get(inbox, AggregatedResults, #{}),
            {GlobalState, AllDeltas, Inbox};
        false ->
            %% 正常提交：apply deltas 到全局状态
            UpdatedState = apply_deltas(GlobalState, AllDeltas, FieldReducers),
            {UpdatedState, undefined, undefined}
    end,

    %% 5. 获取并路由 outbox 消息（节点间非状态消息）
    AllOutbox = maps:get(outbox, AggregatedResults, []),
    route_all_messages(AllOutbox, Workers, NumWorkers),
    TotalMessages = length(AllOutbox),

    %% 6. 更新 AggregatedResults
    UpdatedResults = AggregatedResults#{
        message_count => TotalMessages,
        superstep => Superstep
    },

    %% 7. 检查终止条件
    Halted = (TotalActive =:= 0) andalso (TotalMessages =:= 0) andalso (not HasError),
    MaxReached = Superstep >= MaxSupersteps - 1,
    IsDone = Halted orelse MaxReached,

    %% 8. 确定 checkpoint 类型并构建返回信息
    Type = case IsDone of
        true -> final;
        false -> determine_checkpoint_type(UpdatedResults)
    end,
    Info = build_superstep_info(Type, UpdatedResults),

    %% 9. 更新状态
    NewState = State#state{
        global_state = NewGlobalState,
        pending_deltas = NewPendingDeltas,
        pending_inbox = NewPendingInbox,
        last_results = UpdatedResults,
        step_caller = undefined,
        superstep = if IsDone -> Superstep; true -> Superstep + 1 end,
        halted = IsDone
    },

    %% 10. 回复调用者
    Reply = case IsDone of
        true ->
            Reason = if Halted -> completed; true -> max_supersteps end,
            {done, Reason, Info};
        false ->
            {continue, Info}
    end,
    gen_server:reply(Caller, Reply),

    NewState.

%% @private 执行顶点重试（无 pending_deltas）
-spec execute_retry([vertex_id()], #state{}) -> {noreply, #state{}}.
execute_retry(VertexIds, #state{
    workers = Workers,
    num_workers = NumWorkers,
    superstep = Superstep,
    last_results = LastResults,
    global_state = GlobalState,
    field_reducers = FieldReducers,
    step_caller = Caller
} = State) ->
    %% 1. 从上次结果获取 inbox
    Inbox = maps:get(inbox, LastResults, #{}),

    %% 2. 广播当前全局状态
    broadcast_global_state(State),

    %% 3. 调用所有 Workers 重试指定顶点
    RetryResults = maps:fold(
        fun(_WorkerId, Pid, Acc) ->
            case pregel_worker:retry_vertices(Pid, VertexIds, Inbox) of
                {ok, Result} -> [Result | Acc];
                _ -> Acc
            end
        end,
        [],
        Workers
    ),

    %% 4. 收集重试产生的 deltas
    RetryDeltas = lists:flatmap(
        fun(R) -> maps:get(deltas, R, []) end,
        RetryResults
    ),

    %% 5. 检查是否仍有失败
    {StillFailed, StillInterrupted} = collect_retry_failures(RetryResults),
    HasError = length(StillFailed) > 0 orelse length(StillInterrupted) > 0,

    %% 6. 根据是否有错误决定处理方式
    {NewGlobalState, NewPendingDeltas, NewPendingInbox} = case HasError of
        true ->
            %% 仍有错误，暂存
            {GlobalState, RetryDeltas, Inbox};
        false ->
            %% 成功，apply deltas
            UpdatedState = apply_deltas(GlobalState, RetryDeltas, FieldReducers),
            {UpdatedState, undefined, undefined}
    end,

    %% 7. 路由 outbox 消息
    RetryOutbox = lists:flatmap(
        fun(R) -> maps:get(outbox, R, []) end,
        RetryResults
    ),
    route_all_messages(RetryOutbox, Workers, NumWorkers),

    %% 8. 更新结果
    UpdatedResults = LastResults#{
        failed_count => length(StillFailed),
        failed_vertices => StillFailed,
        interrupted_count => length(StillInterrupted),
        interrupted_vertices => StillInterrupted,
        message_count => maps:get(message_count, LastResults, 0) + length(RetryOutbox),
        superstep => Superstep
    },

    %% 9. 确定 checkpoint 类型并构建返回信息
    Type = determine_checkpoint_type(UpdatedResults),
    Info = build_superstep_info(Type, UpdatedResults),

    %% 10. 更新状态
    NewState = State#state{
        global_state = NewGlobalState,
        pending_deltas = NewPendingDeltas,
        pending_inbox = NewPendingInbox,
        last_results = UpdatedResults,
        step_caller = undefined
    },

    %% 11. 回复调用者
    gen_server:reply(Caller, {continue, Info}),

    {noreply, NewState}.

%% @private 执行延迟提交重试
-spec execute_deferred_retry([vertex_id()], #state{}) -> {noreply, #state{}}.
execute_deferred_retry(VertexIds, #state{
    workers = Workers,
    num_workers = NumWorkers,
    superstep = Superstep,
    pending_deltas = PendingDeltas,
    pending_inbox = PendingInbox,
    global_state = GlobalState,
    field_reducers = FieldReducers,
    step_caller = Caller
} = State) ->
    %% 1. 广播当前全局状态
    broadcast_global_state(State),

    %% 2. 调用所有 Workers 重试指定顶点
    Inbox = case PendingInbox of
        undefined -> #{};
        _ -> PendingInbox
    end,
    RetryResults = maps:fold(
        fun(_WorkerId, Pid, Acc) ->
            case pregel_worker:retry_vertices(Pid, VertexIds, Inbox) of
                {ok, Result} -> [Result | Acc];
                _ -> Acc
            end
        end,
        [],
        Workers
    ),

    %% 3. 收集重试产生的新 deltas
    RetryDeltas = lists:flatmap(
        fun(R) -> maps:get(deltas, R, []) end,
        RetryResults
    ),

    %% 4. 合并 deltas（替换重试顶点的 delta）
    RetriedVertexIds = get_retried_vertex_ids(RetryResults),
    MergedDeltas = merge_pending_deltas(PendingDeltas, RetryDeltas, RetriedVertexIds),

    %% 5. 检查是否仍有失败
    {StillFailed, StillInterrupted} = collect_retry_failures(RetryResults),
    HasError = length(StillFailed) > 0 orelse length(StillInterrupted) > 0,

    %% 6. 根据是否有错误决定处理方式
    {NewGlobalState, NewPendingDeltas, NewPendingInbox} = case HasError of
        true ->
            %% 仍有错误，继续暂存
            {GlobalState, MergedDeltas, Inbox};
        false ->
            %% 全部成功，apply 所有 deltas
            UpdatedState = apply_deltas(GlobalState, MergedDeltas, FieldReducers),
            {UpdatedState, undefined, undefined}
    end,

    %% 7. 路由 outbox 消息
    RetryOutbox = lists:flatmap(
        fun(R) -> maps:get(outbox, R, []) end,
        RetryResults
    ),
    route_all_messages(RetryOutbox, Workers, NumWorkers),

    %% 8. 构建更新结果
    UpdatedResults = #{
        failed_count => length(StillFailed),
        failed_vertices => StillFailed,
        interrupted_count => length(StillInterrupted),
        interrupted_vertices => StillInterrupted,
        message_count => length(RetryOutbox),
        superstep => Superstep,
        active_count => 0
    },

    %% 9. 确定 checkpoint 类型并构建返回信息
    Type = determine_checkpoint_type(UpdatedResults),
    Info = build_superstep_info(Type, UpdatedResults),

    %% 10. 更新状态
    NewState = State#state{
        global_state = NewGlobalState,
        pending_deltas = NewPendingDeltas,
        pending_inbox = NewPendingInbox,
        last_results = UpdatedResults,
        step_caller = undefined
    },

    %% 11. 回复调用者
    gen_server:reply(Caller, {continue, Info}),

    {noreply, NewState}.

%% @private 收集重试失败信息
-spec collect_retry_failures([pregel_worker:retry_result()]) ->
    {[{vertex_id(), term()}], [{vertex_id(), term()}]}.
collect_retry_failures(RetryResults) ->
    lists:foldl(
        fun(R, {FAcc, IAcc}) ->
            {maps:get(failed_vertices, R, []) ++ FAcc,
             maps:get(interrupted_vertices, R, []) ++ IAcc}
        end,
        {[], []},
        RetryResults
    ).

%% @private 获取重试的顶点 ID 列表
-spec get_retried_vertex_ids([pregel_worker:retry_result()]) -> [vertex_id()].
get_retried_vertex_ids(RetryResults) ->
    lists:flatmap(
        fun(R) ->
            Vertices = maps:get(vertices, R, #{}),
            maps:keys(Vertices)
        end,
        RetryResults
    ).

%% @private 合并 pending deltas 和重试 deltas
%% 重试顶点的 delta 替换原有的
-spec merge_pending_deltas([delta()], [delta()], [vertex_id()]) -> [delta()].
merge_pending_deltas(PendingDeltas, RetryDeltas, _RetriedVertexIds) ->
    %% 简单实现：直接追加新的 deltas
    %% 因为 deltas 是增量，重试的结果会覆盖之前的
    PendingDeltas ++ RetryDeltas.

%% @private 统一路由所有消息到目标 Worker
-spec route_all_messages([{term(), term()}],
                          #{non_neg_integer() => pid()},
                          pos_integer()) -> ok.
route_all_messages([], _Workers, _NumWorkers) ->
    ok;
route_all_messages(Messages, Workers, NumWorkers) ->
    %% 按目标 Worker 分组消息
    GroupedMessages = group_messages_by_worker(Messages, NumWorkers),
    %% 发送到各 Worker
    maps:foreach(
        fun(TargetWorkerId, Msgs) ->
            case maps:get(TargetWorkerId, Workers, undefined) of
                undefined -> ok;
                Pid -> pregel_worker:receive_messages(Pid, Msgs)
            end
        end,
        GroupedMessages
    ).

%% @private 按目标 Worker 分组消息
-spec group_messages_by_worker([{term(), term()}], pos_integer()) ->
    #{non_neg_integer() => [{term(), term()}]}.
group_messages_by_worker(Messages, NumWorkers) ->
    lists:foldl(
        fun({TargetVertex, Value}, Acc) ->
            WorkerId = pregel_partition:worker_id(TargetVertex, NumWorkers, hash),
            Existing = maps:get(WorkerId, Acc, []),
            Acc#{WorkerId => [{TargetVertex, Value} | Existing]}
        end,
        #{},
        Messages
    ).

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 构建超步信息（带类型）
-spec build_superstep_info(checkpoint_type(), pregel_barrier:superstep_results() | undefined) -> superstep_info().
build_superstep_info(Type, undefined) ->
    #{
        type => Type,
        superstep => 0,
        active_count => 0,
        message_count => 0,
        failed_count => 0,
        failed_vertices => [],
        interrupted_count => 0,
        interrupted_vertices => []
    };
build_superstep_info(Type, Results) ->
    #{
        type => Type,
        superstep => maps:get(superstep, Results, 0),
        active_count => maps:get(active_count, Results, 0),
        message_count => maps:get(message_count, Results, 0),
        failed_count => maps:get(failed_count, Results, 0),
        failed_vertices => maps:get(failed_vertices, Results, []),
        interrupted_count => maps:get(interrupted_count, Results, 0),
        interrupted_vertices => maps:get(interrupted_vertices, Results, [])
    }.

%% @private 根据结果判断 checkpoint 类型
-spec determine_checkpoint_type(pregel_barrier:superstep_results()) -> checkpoint_type().
determine_checkpoint_type(Results) ->
    InterruptedCount = maps:get(interrupted_count, Results, 0),
    FailedCount = maps:get(failed_count, Results, 0),
    if
        InterruptedCount > 0 -> interrupt;
        FailedCount > 0 -> error;
        true -> step
    end.

%% @private 获取终止原因
-spec get_done_reason(#state{}) -> done_reason().
get_done_reason(#state{superstep = Superstep, max_supersteps = MaxSupersteps, last_results = Results}) ->
    TotalActive = maps:get(active_count, Results, 0),
    TotalMessages = maps:get(message_count, Results, 0),
    Halted = (TotalActive =:= 0) andalso (TotalMessages =:= 0),
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
%% Checkpoint 恢复辅助函数
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

%% @private 获取恢复选项中的消息列表
-spec get_restore_messages(restore_opts() | undefined) -> [{vertex_id(), term()}].
get_restore_messages(undefined) -> [];
get_restore_messages(#{messages := Messages}) -> Messages;
get_restore_messages(_) -> [].

%% @private 注入恢复的消息到 Workers
-spec inject_restore_messages(#state{}) -> ok.
inject_restore_messages(#state{
    restore_from = RestoreOpts,
    workers = Workers,
    num_workers = NumWorkers
}) ->
    Messages = get_restore_messages(RestoreOpts),
    route_all_messages(Messages, Workers, NumWorkers).

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
