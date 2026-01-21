%%%-------------------------------------------------------------------
%%% @doc Pregel Master 进程模块
%%%
%%% 协调整个 Pregel 图计算:
%%% - 启动和管理 Worker 进程
%%% - 协调超步执行（BSP 同步屏障）
%%% - 检测终止条件（所有顶点停止且无消息）
%%%
%%% 设计模式: gen_server 行为模式 + 协调者模式
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_master).
-behaviour(gen_server).

%% API
-export([run/3, run/4, start_link/3, stop/1]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 类型导出
-export_type([opts/0, result/0, restore_opts/0]).
-export_type([superstep_complete_info/0, superstep_complete_result/0, checkpoint_data/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type graph() :: pregel_graph:graph().
-type vertex_id() :: pregel_vertex:vertex_id().
-type vertex() :: pregel_vertex:vertex().
-type compute_fn() :: fun((pregel_worker:context()) -> pregel_worker:context()).

%% 回调类型定义
-type callback_type() :: initial | step | final.

%% Checkpoint 数据（按需获取）
-type checkpoint_data() :: #{
    superstep := non_neg_integer(),
    vertices := #{vertex_id() => vertex()},
    pending_messages := [{vertex_id(), term()}]
}.

%% 超步完成回调信息
-type superstep_complete_info() :: #{
    type := callback_type(),
    superstep := non_neg_integer(),
    active_count := non_neg_integer(),
    message_count := non_neg_integer(),
    failed_count := non_neg_integer(),
    failed_vertices := [{vertex_id(), term()}],
    interrupted_count := non_neg_integer(),
    interrupted_vertices := [{vertex_id(), term()}],
    get_checkpoint_data := fun(() -> checkpoint_data())
}.

%% 回调返回值
-type superstep_complete_result() :: continue | {stop, term()}.

%% 超步完成回调函数类型
-type superstep_complete_callback() :: fun((superstep_complete_info()) -> superstep_complete_result()).

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
    combiner => pregel_combiner:spec(),
    max_supersteps => pos_integer(),
    num_workers => pos_integer(),
    on_superstep => fun((non_neg_integer(), graph()) -> ok),
    on_superstep_complete => superstep_complete_callback(),
    restore_from => restore_opts()  %% 从 checkpoint 恢复
}.

%% Pregel 执行结果
-type result() :: #{
    status := completed | max_supersteps | {stopped, term()},
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
    combiner         :: pregel_combiner:spec() | undefined,  %% 合并器
    max_supersteps   :: pos_integer(),              %% 最大超步数
    num_workers      :: pos_integer(),              %% Worker 数
    workers          :: #{non_neg_integer() => pid()},  %% Worker 映射
    superstep        :: non_neg_integer(),          %% 当前超步
    barrier          :: pregel_barrier:t(),         %% 同步屏障
    caller           :: gen_server:from() | undefined,  %% 调用者
    on_superstep     :: fun((non_neg_integer(), graph()) -> ok) | undefined,
    on_superstep_complete :: superstep_complete_callback() | undefined,  %% 超步完成回调
    restore_from     :: restore_opts() | undefined  %% 恢复选项（启动后消费）
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 执行 Pregel 计算
-spec run(graph(), compute_fn(), opts()) -> result().
run(Graph, ComputeFn, Opts) ->
    {ok, Pid} = start_link(Graph, ComputeFn, Opts),
    try
        gen_server:call(Pid, start_execution, infinity)
    after
        stop(Pid)
    end.

%% @doc 带超时的 Pregel 计算
-spec run(graph(), compute_fn(), opts(), timeout()) -> result().
run(Graph, ComputeFn, Opts, Timeout) ->
    {ok, Pid} = start_link(Graph, ComputeFn, Opts),
    try
        gen_server:call(Pid, start_execution, Timeout)
    after
        stop(Pid)
    end.

%% @doc 启动 Master 进程
-spec start_link(graph(), compute_fn(), opts()) -> {ok, pid()} | {error, term()}.
start_link(Graph, ComputeFn, Opts) ->
    gen_server:start_link(?MODULE, {Graph, ComputeFn, Opts}, []).

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
        combiner = maps:get(combiner, Opts, undefined),
        max_supersteps = maps:get(max_supersteps, Opts, 100),
        num_workers = maps:get(num_workers, Opts, erlang:system_info(schedulers)),
        workers = #{},
        superstep = InitialSuperstep,
        barrier = pregel_barrier:new(0),
        caller = undefined,
        on_superstep = maps:get(on_superstep, Opts, undefined),
        on_superstep_complete = maps:get(on_superstep_complete, Opts, undefined),
        restore_from = RestoreOpts
    },
    {ok, State}.

handle_call(start_execution, From, State) ->
    StateWithCaller = State#state{caller = From},
    %% 启动 Workers（传入恢复的顶点状态）
    StateWithWorkers = start_workers(StateWithCaller),
    %% 注入恢复的消息（如果有）
    inject_restore_messages(StateWithWorkers),
    %% 清除 restore_from（已消费）
    StateReady = StateWithWorkers#state{restore_from = undefined},
    %% 调用 initial 回调
    case call_superstep_complete(initial, empty_superstep_results(), StateReady) of
        continue ->
            %% 继续执行第一个超步
            broadcast_start_superstep(StateReady),
            {noreply, StateReady};
        {stop, Reason} ->
            %% 回调要求停止，返回结果
            finish_execution({stopped, Reason}, StateReady),
            {noreply, StateReady}
    end;

handle_call(get_graph, _From, #state{graph = Graph} = State) ->
    {reply, Graph, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({worker_done, _WorkerPid, Result}, State) ->
    NewState = handle_worker_done(Result, State),
    {noreply, NewState};

%% 注意：route_messages 已移除
%% Worker 不再实时发送消息，改为在超步结束时上报 outbox
%% Master 在 complete_superstep 中统一路由

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
    combiner = Combiner,
    num_workers = NumWorkers,
    restore_from = RestoreOpts
} = State) ->
    %% 分区图
    Partitions = pregel_partition:partition_graph(Graph, NumWorkers),
    NumVertices = pregel_graph:size(Graph),

    %% 获取恢复的顶点状态
    RestoredVertices = get_restore_vertices(RestoreOpts),

    %% 启动 Worker（传入恢复的顶点状态）
    Workers = start_worker_processes(Partitions, ComputeFn, Combiner, NumWorkers, NumVertices, RestoredVertices),

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
                              pregel_combiner:spec() | undefined,
                              pos_integer(),
                              non_neg_integer(),
                              #{vertex_id() => vertex()}) ->
    #{non_neg_integer() => pid()}.
start_worker_processes(Partitions, ComputeFn, Combiner, NumWorkers, NumVertices, RestoredVertices) ->
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
                combiner => Combiner,
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
broadcast_start_superstep(#state{
    workers = Workers,
    superstep = Superstep,
    on_superstep = OnSuperstep,
    graph = Graph
}) ->
    %% 调用超步回调（如果有）
    maybe_call_superstep_callback(OnSuperstep, Superstep, Graph),
    %% 通知所有 Worker
    maps:foreach(
        fun(_Id, Pid) -> pregel_worker:start_superstep(Pid, Superstep) end,
        Workers
    ).

%% @private 可选调用超步回调
-spec maybe_call_superstep_callback(fun((non_neg_integer(), graph()) -> ok) | undefined,
                                     non_neg_integer(), graph()) -> ok.
maybe_call_superstep_callback(undefined, _, _) -> ok;
maybe_call_superstep_callback(Fun, Superstep, Graph) -> Fun(Superstep, Graph).

%% @private 处理 Worker 完成通知
-spec handle_worker_done(map(), #state{}) -> #state{}.
handle_worker_done(Result, #state{barrier = Barrier} = State) ->
    NewBarrier = pregel_barrier:record_done(Result, Barrier),
    NewState = State#state{barrier = NewBarrier},
    case pregel_barrier:is_complete(NewBarrier) of
        true -> complete_superstep(NewState);
        false -> NewState
    end.

%% @private 完成超步处理
%%
%% BSP 模型：从所有 Worker 的 outbox 汇总消息，统一路由到目标 Worker
-spec complete_superstep(#state{}) -> #state{}.
complete_superstep(#state{
    barrier = Barrier,
    superstep = Superstep,
    max_supersteps = MaxSupersteps,
    workers = Workers,
    num_workers = NumWorkers
} = State) ->
    %% 1. 汇总结果（返回 map 格式，包含所有 Worker 的 outbox）
    Results = pregel_barrier:get_results(Barrier),
    AggregatedResults = pregel_barrier:aggregate_results(Results),
    TotalActive = maps:get(active_count, AggregatedResults),
    TotalMessages = maps:get(message_count, AggregatedResults),

    %% 2. 从汇总结果中获取所有 outbox 消息，统一路由
    AllOutbox = maps:get(outbox, AggregatedResults, []),
    route_all_messages(AllOutbox, Workers, NumWorkers),

    %% 3. 检查终止条件
    Halted = (TotalActive =:= 0) andalso (TotalMessages =:= 0),
    MaxReached = Superstep >= MaxSupersteps - 1,

    %% 4. 判断回调类型并调用回调
    CallbackType = determine_callback_type(Halted, MaxReached),
    case call_superstep_complete(CallbackType, AggregatedResults, State) of
        {stop, Reason} ->
            %% 回调要求停止
            finish_execution({stopped, Reason}, State);
        continue ->
            %% 根据终止条件决定下一步
            case {Halted, MaxReached} of
                {true, _} -> finish_execution(completed, State);
                {_, true} -> finish_execution(max_supersteps, State);
                {false, false} -> start_next_superstep(State)
            end
    end.

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

%% @private 开始下一超步
-spec start_next_superstep(#state{}) -> #state{}.
start_next_superstep(#state{superstep = Superstep, workers = Workers} = State) ->
    NewState = State#state{
        superstep = Superstep + 1,
        barrier = pregel_barrier:new(maps:size(Workers))
    },
    broadcast_start_superstep(NewState),
    NewState.

%%====================================================================
%% 完成处理
%%====================================================================

%% @private 完成执行
-spec finish_execution(completed | max_supersteps | {stopped, term()}, #state{}) -> #state{}.
finish_execution(Status, #state{
    caller = Caller,
    superstep = Superstep,
    workers = Workers,
    graph = OriginalGraph
} = State) ->
    %% 收集最终图
    FinalGraph = collect_final_graph(Workers, OriginalGraph),

    Result = #{
        status => Status,
        graph => FinalGraph,
        supersteps => Superstep + 1,
        stats => #{num_workers => maps:size(Workers)}
    },

    gen_server:reply(Caller, Result),
    State#state{caller = undefined}.

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

%%====================================================================
%% 回调机制
%%====================================================================

%% @private 调用超步完成回调
%%
%% 如果没有配置回调，直接返回 continue。
%% 否则构建回调信息并调用回调函数。
-spec call_superstep_complete(callback_type(), pregel_barrier:superstep_results(), #state{}) ->
    superstep_complete_result().
call_superstep_complete(_Type, _Results, #state{on_superstep_complete = undefined}) ->
    continue;
call_superstep_complete(Type, Results, State) ->
    Info = build_superstep_complete_info(Type, Results, State),
    Callback = State#state.on_superstep_complete,
    Callback(Info).

%% @private 构建超步完成回调信息
-spec build_superstep_complete_info(callback_type(), pregel_barrier:superstep_results(), #state{}) ->
    superstep_complete_info().
build_superstep_complete_info(Type, Results, #state{
    superstep = Superstep,
    workers = Workers
}) ->
    %% 从汇总结果中获取 outbox（已路由的消息）
    Outbox = maps:get(outbox, Results, []),
    #{
        type => Type,
        superstep => Superstep,
        active_count => maps:get(active_count, Results, 0),
        message_count => maps:get(message_count, Results, 0),
        failed_count => maps:get(failed_count, Results, 0),
        failed_vertices => maps:get(failed_vertices, Results, []),
        interrupted_count => maps:get(interrupted_count, Results, 0),
        interrupted_vertices => maps:get(interrupted_vertices, Results, []),
        get_checkpoint_data => make_get_checkpoint_data(Superstep, Workers, Outbox)
    }.

%% @private 创建按需获取 checkpoint 数据的函数
%%
%% 返回一个闭包，调用时收集当前状态快照。
%% 注意：pending_messages 现在来自汇总的 outbox，而不是 Master 缓存
-spec make_get_checkpoint_data(non_neg_integer(),
                                #{non_neg_integer() => pid()},
                                [{term(), term()}]) ->
    fun(() -> checkpoint_data()).
make_get_checkpoint_data(Superstep, Workers, Outbox) ->
    fun() ->
        Vertices = collect_vertices_from_workers(Workers),
        #{
            superstep => Superstep,
            vertices => Vertices,
            pending_messages => Outbox  %% 已路由的消息列表
        }
    end.

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

%% @private 判断回调类型
%%
%% - initial: 执行前（superstep = 0，尚未开始）
%% - final: 终止条件满足或达到最大超步
%% - step: 超步完成但未终止
-spec determine_callback_type(boolean(), boolean()) -> callback_type().
determine_callback_type(true, _) -> final;      %% 终止
determine_callback_type(_, true) -> final;      %% 达到最大超步
determine_callback_type(false, false) -> step.  %% 继续执行

%% @private 创建空的结果（用于 initial 回调）
-spec empty_superstep_results() -> pregel_barrier:superstep_results().
empty_superstep_results() ->
    #{
        active_count => 0,
        message_count => 0,
        outbox => [],
        failed_count => 0,
        failed_vertices => [],
        interrupted_count => 0,
        interrupted_vertices => []
    }.

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
