%%%-------------------------------------------------------------------
%%% @doc Pregel Worker 进程模块
%%%
%%% 每个 Worker 管理一个图分区，职责包括:
%%% - 执行本地顶点的计算函数
%%% - 与 Master 进程同步超步状态
%%% - 从 Master 接收全局状态广播
%%% - 从 Master 接收激活列表
%%% - 收集顶点计算产生的 delta 并上报给 Master
%%%
%%% 全局状态模式（无 inbox 版本）:
%%% - Worker 不再持有顶点 value，只持有顶点拓扑（id, edges）
%%% - Worker 不再维护 inbox，激活由 Master 集中管理
%%% - 计算函数从 global_state 读取数据
%%% - 计算函数返回 delta（增量更新）和 activations（激活列表）
%%% - Master 负责合并 delta、收集 activations、广播新的全局状态
%%%
%%% 设计模式: gen_server 行为模式 + 策略模式（计算函数）
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_worker).
-behaviour(gen_server).

%% API
-export([start_link/2, stop/1]).
-export([start_superstep/3, start_superstep/4]).
-export([get_state/1, get_vertices/1]).
-export([retry_vertices/2]).
-export([update_global_state/2]).

%% 内部函数导出（用于测试）
-export([compute_vertices/6]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 类型导出
-export_type([opts/0, context/0, compute_result/0, compute_status/0, retry_result/0, delta/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type vertex_id() :: pregel_vertex:vertex_id().
-type vertex() :: pregel_vertex:vertex().

%% Delta: 增量更新，field => value 映射
-type delta() :: #{atom() | binary() => term()}.

%% Worker 配置选项
-type opts() :: #{
    worker_id := non_neg_integer(),
    master := pid(),
    vertices := #{vertex_id() => vertex()},
    compute_fn := fun((context()) -> compute_result()),
    num_workers := pos_integer(),
    num_vertices => non_neg_integer(),
    global_state => graph_state:state()       %% 初始全局状态
}.

%% 计算上下文（传递给计算函数）
%% 无 inbox 版本：不再传递 messages，节点从 global_state 获取所有数据
%% 扁平化模式：vertex 直接包含 fun_/metadata/routing_edges 字段
-type context() :: #{
    vertex_id := vertex_id(),
    vertex := vertex(),                 %% 完整顶点（扁平化结构）
    global_state := graph_state:state(),
    vertex_input := map() | undefined,  %% dispatch 分发的输入参数
    superstep := non_neg_integer(),
    num_vertices := non_neg_integer()
}.

%% 计算结果状态
%% ok - 计算成功
%% {error, Reason} - 计算失败，Reason 为失败原因
%% {interrupt, Reason} - 计算中断，用于 human-in-the-loop 场景
-type compute_status() :: ok | {error, term()} | {interrupt, term()}.

%% 计算结果（计算函数必须返回此结构）
%% delta - 状态增量（成功时用于合并到 global_state）
%% activations - 要激活的顶点ID列表（替代 outbox）
%% status - 计算状态（必需）
-type compute_result() :: #{
    delta := delta(),
    activations => [vertex_id()],
    status := compute_status()
}.

%% 计算结果累加器（内部使用）
-type compute_acc() :: {
    Deltas :: [delta()],
    Activations :: [vertex_id()],
    FailedVertices :: [{vertex_id(), term()}],
    InterruptedVertices :: [{vertex_id(), term()}]
}.

%% 重试结果
-type retry_result() :: #{
    deltas := [delta()],
    activations := [vertex_id()],
    failed_vertices := [{vertex_id(), term()}],
    interrupted_vertices := [{vertex_id(), term()}]
}.

%% 内部状态（无 inbox）
-record(state, {
    worker_id     :: non_neg_integer(),         % Worker ID
    master        :: pid(),                      % Master 进程
    vertices      :: #{vertex_id() => vertex()}, % 本地顶点（含 value: #{node, edges}）
    compute_fn    :: fun((context()) -> compute_result()),  % 计算函数
    superstep     :: non_neg_integer(),         % 当前超步
    num_workers   :: pos_integer(),             % Worker 总数
    num_vertices  :: non_neg_integer(),         % 全图顶点总数
    global_state  :: graph_state:state()        % 全局状态（从 Master 广播）
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Worker 进程
-spec start_link(non_neg_integer(), opts()) -> {ok, pid()} | {error, term()}.
start_link(WorkerId, Opts) ->
    gen_server:start_link(?MODULE, Opts#{worker_id => WorkerId}, []).

%% @doc 停止 Worker
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc 开始新的超步
%% Activations: 本 Worker 需要激活的顶点ID列表
-spec start_superstep(pid(), non_neg_integer(), [vertex_id()]) -> ok.
start_superstep(Pid, Superstep, Activations) ->
    start_superstep(Pid, Superstep, Activations, #{}).

%% @doc 开始新的超步（带 VertexInputs）
-spec start_superstep(pid(), non_neg_integer(), [vertex_id()], pregel_superstep:vertex_inputs()) -> ok.
start_superstep(Pid, Superstep, Activations, VertexInputs) ->
    gen_server:cast(Pid, {start_superstep, Superstep, Activations, VertexInputs}).

%% @doc 更新全局状态（由 Master 广播调用）
-spec update_global_state(pid(), graph_state:state()) -> ok.
update_global_state(Pid, GlobalState) ->
    gen_server:cast(Pid, {global_state, GlobalState}).

%% @doc 获取 Worker 状态（调试用）
-spec get_state(pid()) -> map().
get_state(Pid) ->
    gen_server:call(Pid, get_state).

%% @doc 获取 Worker 的所有顶点
-spec get_vertices(pid()) -> #{vertex_id() => vertex()}.
get_vertices(Pid) ->
    gen_server:call(Pid, get_vertices).

%% @doc 重试指定顶点的计算
%%
%% 用于单顶点重启场景：
%% - VertexIds: 要重试的顶点 ID 列表（只计算本 Worker 拥有的顶点）
%%
%% 返回重试结果（同步调用）
-spec retry_vertices(pid(), [vertex_id()]) ->
    {ok, retry_result()} | {error, term()}.
retry_vertices(Pid, VertexIds) ->
    gen_server:call(Pid, {retry_vertices, VertexIds}).

%%====================================================================
%% gen_server 回调
%%====================================================================

init(Opts) ->
    #{
        worker_id := WorkerId,
        master := Master,
        vertices := Vertices,
        compute_fn := ComputeFn,
        num_workers := NumWorkers
    } = Opts,

    State = #state{
        worker_id = WorkerId,
        master = Master,
        vertices = Vertices,
        compute_fn = ComputeFn,
        superstep = 0,
        num_workers = NumWorkers,
        num_vertices = maps:get(num_vertices, Opts, maps:size(Vertices)),
        global_state = maps:get(global_state, Opts, graph_state:new())
    },
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, state_to_map(State), State};

handle_call(get_vertices, _From, #state{vertices = Vertices} = State) ->
    {reply, Vertices, State};

handle_call({retry_vertices, VertexIds}, _From, State) ->
    {Reply, NewState} = do_retry_vertices(VertexIds, State),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({start_superstep, Superstep, Activations, VertexInputs}, State) ->
    NewState = execute_superstep(Activations, VertexInputs, State#state{superstep = Superstep}),
    {noreply, NewState};

handle_cast({global_state, GlobalState}, State) ->
    %% 接收 Master 广播的全局状态
    {noreply, State#state{global_state = GlobalState}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% 超步执行
%%====================================================================

%% @private 执行一个超步
%%
%% 无 inbox 版本：
%% - Activations 参数指定要激活的顶点
%% - 计算函数从 global_state 读取数据
%% - 计算函数返回 delta 和 activations
-spec execute_superstep([vertex_id()], pregel_superstep:vertex_inputs(), #state{}) -> #state{}.
execute_superstep(Activations, VertexInputs, #state{
    vertices = Vertices,
    compute_fn = ComputeFn,
    superstep = Superstep,
    num_vertices = NumVertices,
    global_state = GlobalState
} = State) ->
    %% 1. 筛选需要计算的顶点（被激活的 + 本身活跃的）
    ActiveVertices = filter_active_vertices(Vertices, Activations),

    %% 2. 执行所有顶点计算（vertex value 包含 node 和 edges）
    {Deltas, NewActivations, FailedVertices, InterruptedVertices} = compute_vertices(
        ActiveVertices, ComputeFn, Superstep, NumVertices, GlobalState, VertexInputs
    ),

    %% 3. 更新顶点状态（halt 计算完成的顶点）
    NewVertices = update_vertex_states(Vertices, ActiveVertices, FailedVertices, InterruptedVertices),

    %% 4. 通知 Master 完成
    NewState = State#state{vertices = NewVertices},
    notify_master_done(NewState, Deltas, NewActivations, FailedVertices, InterruptedVertices),

    %% 5. 返回更新后的状态
    NewState.

%% @private 筛选需要计算的顶点
%% 顶点激活条件：在激活列表中 OR 本身未 halted
-spec filter_active_vertices(#{vertex_id() => vertex()}, [vertex_id()]) ->
    #{vertex_id() => vertex()}.
filter_active_vertices(Vertices, Activations) ->
    ActivationSet = sets:from_list(Activations),
    maps:filter(
        fun(Id, V) ->
            sets:is_element(Id, ActivationSet) orelse pregel_vertex:is_active(V)
        end,
        Vertices
    ).

%% @doc 执行所有顶点计算
%%
%% 无 inbox 版本：计算函数不再接收 messages 参数
%% 扁平化模式：vertex 直接包含 fun_/metadata/routing_edges
%% VertexInputs: dispatch 分发的输入参数
-spec compute_vertices(
    ActiveVertices :: #{vertex_id() => vertex()},
    ComputeFn :: fun((context()) -> compute_result()),
    Superstep :: non_neg_integer(),
    NumVertices :: non_neg_integer(),
    GlobalState :: graph_state:state(),
    VertexInputs :: pregel_superstep:vertex_inputs()
) -> compute_acc().
compute_vertices(ActiveVertices, ComputeFn, Superstep, NumVertices, GlobalState, VertexInputs) ->
    InitAcc = {[], [], [], []},  %% {Deltas, Activations, Failed, Interrupted}
    maps:fold(
        fun(Id, Vertex, Acc) ->
            case maps:get(Id, VertexInputs, []) of
                [] ->
                    %% 无 dispatch 输入，普通执行
                    Context = make_context(Id, Vertex, GlobalState, undefined, Superstep, NumVertices),
                    Result = ComputeFn(Context),
                    process_compute_result(Id, Result, Acc);
                Dispatches ->
                    %% 有 dispatch 输入，根据池可用性决定并发或顺序执行
                    case dispatch_pool_available(Dispatches) of
                        false ->
                            %% 顺序回退（单 dispatch 或池不可用）
                            lists:foldl(fun(D, InnerAcc) ->
                                Input = graph_dispatch:get_input(D),
                                Context = make_context(Id, Vertex, GlobalState, Input, Superstep, NumVertices),
                                Result = ComputeFn(Context),
                                process_compute_result(Id, Result, InnerAcc)
                            end, Acc, Dispatches);
                        {true, PoolName, Timeout} ->
                            execute_dispatches_concurrent(
                                Id, Vertex, GlobalState, Superstep, NumVertices,
                                ComputeFn, Dispatches, Acc, PoolName, Timeout)
                    end
            end
        end,
        InitAcc,
        ActiveVertices
    ).

%% @private 处理单个顶点的计算结果
-spec process_compute_result(vertex_id(), compute_result(), compute_acc()) -> compute_acc().
process_compute_result(_Id, #{status := ok, delta := Delta} = Result,
                       {DeltaAcc, ActAcc, FailedAcc, InterruptedAcc}) ->
    %% 成功：收集 delta 和 activations
    Activations = maps:get(activations, Result, []),
    NewDelta = case maps:size(Delta) of
        0 -> DeltaAcc;
        _ -> [Delta | DeltaAcc]
    end,
    {NewDelta, Activations ++ ActAcc, FailedAcc, InterruptedAcc};
process_compute_result(Id, #{status := {error, Reason}},
                       {DeltaAcc, ActAcc, FailedAcc, InterruptedAcc}) ->
    %% 失败：记录失败信息，不收集 delta
    {DeltaAcc, ActAcc, [{Id, Reason} | FailedAcc], InterruptedAcc};
process_compute_result(Id, #{status := {interrupt, Reason}} = Result,
                       {DeltaAcc, ActAcc, FailedAcc, InterruptedAcc}) ->
    %% 中断：记录中断信息，可选地收集 delta
    NewDeltaAcc = case maps:get(delta, Result, #{}) of
        Delta when map_size(Delta) > 0 -> [Delta | DeltaAcc];
        _ -> DeltaAcc
    end,
    {NewDeltaAcc, ActAcc, FailedAcc, [{Id, Reason} | InterruptedAcc]}.

%% @private 创建计算上下文
%% 扁平化模式：传递完整顶点，包含 fun_/metadata/routing_edges
-spec make_context(vertex_id(), vertex(), graph_state:state(),
                   map() | undefined, non_neg_integer(), non_neg_integer()) -> context().
make_context(VertexId, Vertex, GlobalState, VertexInput, Superstep, NumVertices) ->
    #{
        vertex_id => VertexId,
        vertex => Vertex,
        global_state => GlobalState,
        vertex_input => VertexInput,
        superstep => Superstep,
        num_vertices => NumVertices
    }.

%%====================================================================
%% Dispatch 并发执行
%%====================================================================

%% @private 检查 dispatch 池是否可用
-spec dispatch_pool_available([term()]) -> false | {true, atom(), pos_integer()}.
dispatch_pool_available(Dispatches) when length(Dispatches) =< 1 ->
    false;
dispatch_pool_available(_) ->
    case whereis(beamai_dispatch_pool) of
        undefined -> false;
        _Pid ->
            Timeout = application:get_env(beamai_graph, dispatch_timeout, 30000),
            {true, beamai_dispatch_pool, Timeout}
    end.

%% @private 并发执行 dispatches
-spec execute_dispatches_concurrent(
    vertex_id(), vertex(), graph_state:state(),
    non_neg_integer(), non_neg_integer(),
    fun((context()) -> compute_result()),
    [term()], compute_acc(), atom(), pos_integer()
) -> compute_acc().
execute_dispatches_concurrent(Id, Vertex, GlobalState, Superstep, NumVertices,
                              ComputeFn, Dispatches, Acc, PoolName, Timeout) ->
    Parent = self(),
    Ref = make_ref(),

    %% 为每个 dispatch 启动 monitor 进程
    PidRefs = lists:map(fun(D) ->
        Input = graph_dispatch:get_input(D),
        Context = make_context(Id, Vertex, GlobalState, Input, Superstep, NumVertices),
        spawn_monitor(fun() ->
            Result = execute_in_pool(ComputeFn, Context, PoolName, Timeout),
            Parent ! {dispatch_result, Ref, self(), Result}
        end)
    end, Dispatches),

    %% 统一回收结果
    Deadline = erlang:monotonic_time(millisecond) + Timeout + 5000,
    collect_dispatch_results(Id, PidRefs, Ref, Acc, Deadline).

%% @private 在池中执行单个 dispatch
-spec execute_in_pool(fun((map()) -> map()), map(), atom(), pos_integer()) ->
    {ok, map()} | {error, term()}.
execute_in_pool(ComputeFn, Context, PoolName, Timeout) ->
    try
        Worker = poolboy:checkout(PoolName, true, Timeout),
        try
            pregel_dispatch_worker:execute(Worker, ComputeFn, Context)
        after
            poolboy:checkin(PoolName, Worker)
        end
    catch
        exit:{timeout, _} ->
            {error, {dispatch_pool_timeout, Timeout}};
        Class:Reason ->
            {error, {dispatch_pool_error, {Class, Reason}}}
    end.

%% @private 回收所有 dispatch 结果
-spec collect_dispatch_results(
    vertex_id(), [{pid(), reference()}], reference(), compute_acc(), integer()
) -> compute_acc().
collect_dispatch_results(_Id, [], _Ref, Acc, _Deadline) ->
    Acc;
collect_dispatch_results(Id, [{Pid, MonRef} | Rest], Ref, Acc, Deadline) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {dispatch_result, Ref, Pid, {ok, ComputeResult}} ->
            erlang:demonitor(MonRef, [flush]),
            NewAcc = process_compute_result(Id, ComputeResult, Acc),
            collect_dispatch_results(Id, Rest, Ref, NewAcc, Deadline);
        {dispatch_result, Ref, Pid, {error, Reason}} ->
            erlang:demonitor(MonRef, [flush]),
            ErrorResult = #{delta => #{}, activations => [], status => {error, Reason}},
            NewAcc = process_compute_result(Id, ErrorResult, Acc),
            collect_dispatch_results(Id, Rest, Ref, NewAcc, Deadline);
        {'DOWN', MonRef, process, Pid, Reason} ->
            ErrorResult = #{delta => #{}, activations => [], status => {error, {dispatch_crash, Reason}}},
            NewAcc = process_compute_result(Id, ErrorResult, Acc),
            collect_dispatch_results(Id, Rest, Ref, NewAcc, Deadline)
    after Remaining ->
        erlang:demonitor(MonRef, [flush]),
        exit(Pid, kill),
        ErrorResult = #{delta => #{}, activations => [], status => {error, dispatch_timeout}},
        NewAcc = process_compute_result(Id, ErrorResult, Acc),
        collect_dispatch_results(Id, Rest, Ref, NewAcc, Deadline)
    end.

%% @private 更新顶点状态
-spec update_vertex_states(
    #{vertex_id() => vertex()},
    #{vertex_id() => vertex()},
    [{vertex_id(), term()}],
    [{vertex_id(), term()}]
) -> #{vertex_id() => vertex()}.
update_vertex_states(AllVertices, ActiveVertices, _FailedVertices, _InterruptedVertices) ->
    %% 计算完成的顶点自动 halt
    ActiveIds = maps:keys(ActiveVertices),
    maps:map(
        fun(Id, V) ->
            case lists:member(Id, ActiveIds) of
                true -> pregel_vertex:halt(V);
                false -> V
            end
        end,
        AllVertices
    ).

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 通知 Master 超步完成
%%
%% 无 inbox 版本：
%% - 不再上报 inbox
%% - 上报 activations 替代 outbox
-spec notify_master_done(
    State :: #state{},
    Deltas :: [delta()],
    Activations :: [vertex_id()],
    FailedVertices :: [{vertex_id(), term()}],
    InterruptedVertices :: [{vertex_id(), term()}]
) -> ok.
notify_master_done(#state{worker_id = WorkerId, master = Master, vertices = Vertices},
                   Deltas, Activations, FailedVertices, InterruptedVertices) ->
    Result = #{
        worker_id => WorkerId,
        active_count => pregel_utils:map_count(fun pregel_vertex:is_active/1, Vertices),
        %% deltas 列表（用于合并到 global_state）
        deltas => Deltas,
        %% activations 列表（替代 outbox，用于激活下一超步的顶点）
        activations => Activations,
        %% 失败信息
        failed_count => length(FailedVertices),
        failed_vertices => FailedVertices,
        %% 中断信息（human-in-the-loop）
        interrupted_count => length(InterruptedVertices),
        interrupted_vertices => InterruptedVertices
    },
    gen_server:cast(Master, {worker_done, self(), Result}).

%% @private 将状态转换为 map（调试用）
-spec state_to_map(#state{}) -> map().
state_to_map(#state{
    worker_id = WorkerId,
    vertices = Vertices,
    superstep = Superstep,
    global_state = GlobalState
}) ->
    #{
        worker_id => WorkerId,
        vertex_count => maps:size(Vertices),
        superstep => Superstep,
        global_state_keys => graph_state:keys(GlobalState)
    }.

%%====================================================================
%% 顶点重试
%%====================================================================

%% @private 执行顶点重试（无 inbox 版本）
-spec do_retry_vertices([vertex_id()], #state{}) ->
    {{ok, retry_result()}, #state{}}.
do_retry_vertices(VertexIds, #state{
    vertices = Vertices,
    compute_fn = ComputeFn,
    superstep = Superstep,
    num_vertices = NumVertices,
    global_state = GlobalState
} = State) ->
    %% 1. 筛选本 Worker 拥有的顶点
    LocalVertexIds = [Id || Id <- VertexIds, maps:is_key(Id, Vertices)],

    %% 2. 构建要重试的顶点映射
    RetryVertices = maps:with(LocalVertexIds, Vertices),

    %% 3. 执行顶点计算（vertex value 已包含 node 和 edges）
    {Deltas, Activations, FailedVertices, InterruptedVertices} = compute_vertices(
        RetryVertices, ComputeFn, Superstep, NumVertices, GlobalState, #{}
    ),

    %% 4. 构建结果
    Result = #{
        deltas => Deltas,
        activations => Activations,
        failed_vertices => FailedVertices,
        interrupted_vertices => InterruptedVertices
    },

    {{ok, Result}, State}.
