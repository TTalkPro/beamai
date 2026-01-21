%%%-------------------------------------------------------------------
%%% @doc Pregel Master 进程模块
%%%
%%% 协调整个 Pregel 图计算:
%%% - 启动和管理 Worker 进程
%%% - 协调超步执行（BSP 同步屏障）
%%% - 检测终止条件（所有顶点停止且无消息）
%%%
%%% 执行模式: 步进式执行，由外部控制循环
%%% 设计模式: gen_server 行为模式 + 协调者模式
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_master).
-behaviour(gen_server).

%% API
-export([start_link/3, step/1, get_checkpoint_data/1, get_result/1, stop/1]).
%% 重试 API
-export([retry/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 类型导出
-export_type([opts/0, result/0, restore_opts/0, checkpoint_data/0]).
-export_type([step_result/0, superstep_info/0, checkpoint_type/0]).
-export_type([state_reducer/0, reducer_context/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type graph() :: pregel_graph:graph().
-type vertex_id() :: pregel_vertex:vertex_id().
-type vertex() :: pregel_vertex:vertex().
-type compute_fn() :: fun((pregel_worker:context()) -> pregel_worker:context()).

%% Checkpoint 数据
%% vertex_inbox: 各顶点的 inbox（计算前的消息），用于单顶点重启
-type checkpoint_data() :: #{
    superstep := non_neg_integer(),
    vertices := #{vertex_id() => vertex()},
    pending_messages := [{vertex_id(), term()}],
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

%% State reducer 上下文
-type reducer_context() :: #{
    target_vertex := vertex_id(),    %% 目标顶点 ID
    superstep := non_neg_integer(),  %% 当前超步
    messages := [term()]             %% 该顶点收到的所有消息
}.

%% State reducer 函数类型
%% 输入：包含目标顶点、superstep、消息列表的上下文
%% 输出：整合后的消息列表
-type state_reducer() :: fun((reducer_context()) -> [term()]).

%% Checkpoint 恢复选项
%% superstep: 起始超步号
%% vertices: 恢复的顶点状态（覆盖原图中对应顶点）
%% messages: 待投递消息列表（可选，在第一个超步前注入）
-type restore_opts() :: #{
    superstep := non_neg_integer(),
    vertices := #{vertex_id() => vertex()},
    messages => [{vertex_id(), term()}]
}.

%% Pregel 执行选项
-type opts() :: #{
    max_supersteps => pos_integer(),
    num_workers => pos_integer(),
    restore_from => restore_opts(),  %% 从 checkpoint 恢复
    state_reducer => state_reducer()  %% 消息整合函数（默认 last_write_win）
}.

%% Pregel 执行结果
-type result() :: #{
    status := completed | max_supersteps,
    graph := graph(),
    supersteps := non_neg_integer(),
    stats := #{atom() => term()}
}.

%% Master 内部状态
%%
%% 注意：pending_messages 已移除
%% BSP 模型改进：Worker 在超步结束时上报 outbox，Master 集中路由
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
    state_reducer    :: state_reducer() | undefined,  %% 消息整合函数
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
        state_reducer = maps:get(state_reducer, Opts, undefined),
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
    %% 后续 step：启动超步执行
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

handle_call({retry, VertexIds}, From, #state{last_results = _LastResults} = State) ->
    %% 执行重试
    NewState = State#state{step_caller = From},
    execute_retry(VertexIds, NewState);

handle_call(get_checkpoint_data, _From, #state{
    superstep = Superstep,
    workers = Workers,
    last_results = Results
} = State) ->
    Vertices = collect_vertices_from_workers(Workers),
    %% 处理 last_results 为 undefined 的情况（initial checkpoint）
    {Outbox, Inbox} = case Results of
        undefined -> {[], #{}};
        _ -> {maps:get(outbox, Results, []), maps:get(inbox, Results, #{})}
    end,
    Data = #{
        superstep => Superstep,
        vertices => Vertices,
        pending_messages => Outbox,
        vertex_inbox => Inbox
    },
    {reply, Data, State};

handle_call(get_result, _From, #state{
    halted = false
} = State) ->
    {reply, {error, not_halted}, State};

handle_call(get_result, _From, #state{
    superstep = Superstep,
    workers = Workers,
    graph = OriginalGraph,
    halted = true
} = State) ->
    FinalGraph = collect_final_graph(Workers, OriginalGraph),
    Result = #{
        status => get_done_reason(State),
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
%% 如果有恢复选项，使用恢复的顶点状态覆盖原图顶点
-spec start_workers(#state{}) -> #state{}.
start_workers(#state{
    graph = Graph,
    compute_fn = ComputeFn,
    num_workers = NumWorkers,
    restore_from = RestoreOpts
} = State) ->
    %% 分区图
    Partitions = pregel_partition:partition_graph(Graph, NumWorkers),
    NumVertices = pregel_graph:size(Graph),

    %% 获取恢复的顶点状态
    RestoredVertices = get_restore_vertices(RestoreOpts),

    %% 启动 Worker（传入恢复的顶点状态）
    Workers = start_worker_processes(Partitions, ComputeFn, NumWorkers, NumVertices, RestoredVertices),

    %% 广播 Worker PID 映射
    broadcast_worker_pids(Workers),

    State#state{
        workers = Workers,
        barrier = pregel_barrier:new(maps:size(Workers))
    }.

%% @private 启动各 Worker 进程
%% RestoredVertices: 恢复的顶点状态映射，用于覆盖分区中对应的顶点
-spec start_worker_processes(#{non_neg_integer() => graph()},
                              compute_fn(),
                              pos_integer(),
                              non_neg_integer(),
                              #{vertex_id() => vertex()}) ->
    #{non_neg_integer() => pid()}.
start_worker_processes(Partitions, ComputeFn, NumWorkers, NumVertices, RestoredVertices) ->
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
                num_vertices => NumVertices
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
%% State Reducer
%%====================================================================

%% @private 应用 state reducer
%% 按目标顶点分组消息，对每组应用 reducer
-spec apply_state_reducer([{term(), term()}], non_neg_integer(),
                           state_reducer() | undefined) ->
    [{term(), term()}].
apply_state_reducer(Outbox, Superstep, undefined) ->
    %% 默认: last_write_win
    apply_state_reducer(Outbox, Superstep, fun default_reducer/1);
apply_state_reducer(Outbox, Superstep, Reducer) ->
    %% 1. 按目标顶点分组
    Grouped = group_by_target(Outbox),
    %% 2. 对每组应用 reducer（传入上下文）
    maps:fold(fun(TargetId, Messages, Acc) ->
        Context = #{
            target_vertex => TargetId,
            superstep => Superstep,
            messages => Messages
        },
        Reduced = Reducer(Context),
        [{TargetId, M} || M <- Reduced] ++ Acc
    end, [], Grouped).

%% @private 按目标顶点分组消息
-spec group_by_target([{term(), term()}]) -> #{term() => [term()]}.
group_by_target(Messages) ->
    lists:foldl(fun({Target, Value}, Acc) ->
        Existing = maps:get(Target, Acc, []),
        Acc#{Target => Existing ++ [Value]}  %% 保持顺序
    end, #{}, Messages).

%% @private 默认 reducer: last_write_win，只保留最后一个值
-spec default_reducer(reducer_context()) -> [term()].
default_reducer(#{messages := []}) -> [];
default_reducer(#{messages := Messages}) -> [lists:last(Messages)].

%% @private 完成超步处理
%%
%% BSP 模型：从所有 Worker 的 outbox 汇总消息，统一路由到目标 Worker
-spec complete_superstep(#state{}) -> #state{}.
complete_superstep(#state{
    barrier = Barrier,
    superstep = Superstep,
    max_supersteps = MaxSupersteps,
    workers = Workers,
    num_workers = NumWorkers,
    state_reducer = StateReducer,
    step_caller = Caller
} = State) ->
    %% 1. 汇总结果（返回 map 格式，包含所有 Worker 的 outbox）
    Results = pregel_barrier:get_results(Barrier),
    AggregatedResults = pregel_barrier:aggregate_results(Results),
    TotalActive = maps:get(active_count, AggregatedResults),

    %% 2. 获取所有 outbox 消息
    AllOutbox = maps:get(outbox, AggregatedResults, []),

    %% 3. 应用 state reducer
    ReducedOutbox = apply_state_reducer(AllOutbox, Superstep, StateReducer),
    TotalMessages = length(ReducedOutbox),  %% 使用 reduced 后的消息数

    %% 4. 路由消息
    route_all_messages(ReducedOutbox, Workers, NumWorkers),

    %% 5. 更新 AggregatedResults（用于 checkpoint 和重试）
    UpdatedResults = AggregatedResults#{
        message_count => TotalMessages,
        outbox => ReducedOutbox  %% 使用 reduced 后的 outbox
    },

    %% 6. 检查终止条件
    Halted = (TotalActive =:= 0) andalso (TotalMessages =:= 0),
    MaxReached = Superstep >= MaxSupersteps - 1,
    IsDone = Halted orelse MaxReached,

    %% 7. 确定 checkpoint 类型并构建返回信息
    Type = case IsDone of
        true -> final;
        false -> determine_checkpoint_type(UpdatedResults)
    end,
    Info = build_superstep_info(Type, UpdatedResults),

    %% 8. 更新状态
    NewState = State#state{
        last_results = UpdatedResults,
        step_caller = undefined,
        superstep = if IsDone -> Superstep; true -> Superstep + 1 end,
        halted = IsDone
    },

    %% 9. 回复调用者
    Reply = case IsDone of
        true ->
            Reason = if Halted -> completed; true -> max_supersteps end,
            {done, Reason, Info};
        false ->
            {continue, Info}
    end,
    gen_server:reply(Caller, Reply),

    NewState.

%% @private 执行顶点重试
-spec execute_retry([vertex_id()], #state{}) -> {noreply, #state{}}.
execute_retry(VertexIds, #state{
    workers = Workers,
    num_workers = NumWorkers,
    superstep = Superstep,
    last_results = LastResults,
    state_reducer = StateReducer,
    step_caller = Caller
} = State) ->
    %% 1. 从上次结果获取 inbox
    Inbox = maps:get(inbox, LastResults, #{}),

    %% 2. 调用所有 Workers 重试指定顶点
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

    %% 3. 合并重试结果
    MergedResults = merge_retry_results(RetryResults, LastResults),

    %% 4. 获取重试产生的 outbox 消息
    RetryOutbox = lists:flatmap(
        fun(R) -> maps:get(outbox, R, []) end,
        RetryResults
    ),

    %% 5. 应用 state reducer
    ReducedRetryOutbox = apply_state_reducer(RetryOutbox, Superstep, StateReducer),

    %% 6. 路由消息
    route_all_messages(ReducedRetryOutbox, Workers, NumWorkers),

    %% 7. 更新消息计数
    UpdatedResults = MergedResults#{
        message_count => maps:get(message_count, MergedResults, 0) + length(ReducedRetryOutbox)
    },

    %% 8. 确定 checkpoint 类型并构建返回信息
    Type = determine_checkpoint_type(UpdatedResults),
    Info = build_superstep_info(Type, UpdatedResults),

    %% 9. 更新状态
    NewState = State#state{
        last_results = UpdatedResults,
        step_caller = undefined
    },

    %% 10. 回复调用者
    gen_server:reply(Caller, {continue, Info}),

    {noreply, NewState}.

%% @private 合并重试结果到上次结果
%%
%% 更新 failed_vertices 和 interrupted_vertices
-spec merge_retry_results([pregel_worker:retry_result()], pregel_barrier:superstep_results()) ->
    pregel_barrier:superstep_results().
merge_retry_results(RetryResults, LastResults) ->
    %% 收集重试后仍然失败/中断的顶点
    {StillFailed, StillInterrupted} = lists:foldl(
        fun(R, {FAcc, IAcc}) ->
            {maps:get(failed_vertices, R, []) ++ FAcc,
             maps:get(interrupted_vertices, R, []) ++ IAcc}
        end,
        {[], []},
        RetryResults
    ),

    %% 更新结果
    LastResults#{
        failed_count => length(StillFailed),
        failed_vertices => StillFailed,
        interrupted_count => length(StillInterrupted),
        interrupted_vertices => StillInterrupted
    }.

%% @private 统一路由所有消息到目标 Worker
%%
%% BSP 模型的消息路由：
%% 1. 按目标顶点 ID 计算目标 Worker ID
%% 2. 按 Worker 分组消息
%% 3. 批量发送到各 Worker 的 inbox
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
                undefined ->
                    %% Worker 不存在，记录警告（不应该发生）
                    ok;
                Pid ->
                    pregel_worker:receive_messages(Pid, Msgs)
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
%% 优先级：interrupt > error > step
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
        false -> completed  %% 不应该到这里
    end.

%% @private 收集最终图
-spec collect_final_graph(#{non_neg_integer() => pid()}, graph()) -> graph().
collect_final_graph(Workers, OriginalGraph) ->
    %% 从所有 Worker 收集顶点状态
    AllVertices = maps:fold(
        fun(_WorkerId, Pid, Acc) ->
            maps:merge(Acc, pregel_worker:get_vertices(Pid))
        end,
        #{},
        Workers
    ),
    %% 更新图中的顶点
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
%% 如果没有恢复选项，返回 0（从头开始）
-spec get_restore_superstep(restore_opts() | undefined) -> non_neg_integer().
get_restore_superstep(undefined) -> 0;
get_restore_superstep(#{superstep := Superstep}) -> Superstep;
get_restore_superstep(_) -> 0.

%% @private 获取恢复选项中的顶点状态
%% 如果没有恢复选项，返回空映射
-spec get_restore_vertices(restore_opts() | undefined) -> #{vertex_id() => vertex()}.
get_restore_vertices(undefined) -> #{};
get_restore_vertices(#{vertices := Vertices}) -> Vertices;
get_restore_vertices(_) -> #{}.

%% @private 获取恢复选项中的消息列表
%% 如果没有恢复选项或没有消息字段，返回空列表
-spec get_restore_messages(restore_opts() | undefined) -> [{vertex_id(), term()}].
get_restore_messages(undefined) -> [];
get_restore_messages(#{messages := Messages}) -> Messages;
get_restore_messages(_) -> [].

%% @private 注入恢复的消息到 Workers
%% 使用现有的 route_all_messages 函数进行路由
-spec inject_restore_messages(#state{}) -> ok.
inject_restore_messages(#state{
    restore_from = RestoreOpts,
    workers = Workers,
    num_workers = NumWorkers
}) ->
    Messages = get_restore_messages(RestoreOpts),
    route_all_messages(Messages, Workers, NumWorkers).

%% @private 合并恢复的顶点到分区顶点
%% 恢复的顶点覆盖分区中对应 ID 的顶点
-spec merge_restored_vertices(#{vertex_id() => vertex()},
                               #{vertex_id() => vertex()}) ->
    #{vertex_id() => vertex()}.
merge_restored_vertices(PartitionVertices, RestoredVertices) ->
    maps:fold(
        fun(Id, RestoredVertex, Acc) ->
            case maps:is_key(Id, Acc) of
                true -> Acc#{Id => RestoredVertex};
                false -> Acc  %% 忽略不在分区中的顶点
            end
        end,
        PartitionVertices,
        RestoredVertices
    ).
