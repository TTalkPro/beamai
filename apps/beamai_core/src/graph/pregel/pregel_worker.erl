%%%-------------------------------------------------------------------
%%% @doc Pregel Worker 进程模块
%%%
%%% 每个 Worker 管理一个图分区，职责包括:
%%% - 执行本地顶点的计算函数
%%% - 处理消息的本地路由和跨 Worker 转发
%%% - 与 Master 进程同步超步状态
%%%
%%% 设计模式: gen_server 行为模式 + 策略模式（计算函数）
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_worker).
-behaviour(gen_server).

%% API
-export([start_link/2, stop/1]).
-export([start_superstep/2, receive_messages/2]).
-export([get_state/1, get_vertices/1]).
-export([retry_vertices/3]).

%% 内部函数导出（用于测试）
-export([compute_vertices/6]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 类型导出
-export_type([opts/0, context/0, compute_result/0, compute_status/0, retry_result/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type vertex_id() :: pregel_vertex:vertex_id().
-type vertex() :: pregel_vertex:vertex().

%% Worker 配置选项
-type opts() :: #{
    worker_id := non_neg_integer(),
    master := pid(),
    vertices := #{vertex_id() => vertex()},
    compute_fn := fun((context()) -> context()),
    num_workers := pos_integer(),
    num_vertices => non_neg_integer(),
    worker_pids => #{non_neg_integer() => pid()}
}.

%% 计算上下文（传递给计算函数）
-type context() :: #{
    vertex := vertex(),
    messages := [term()],
    superstep := non_neg_integer(),
    num_vertices := non_neg_integer(),
    outbox := [{vertex_id(), term()}]
}.

%% 计算结果状态
%% ok - 计算成功
%% {error, Reason} - 计算失败，Reason 为失败原因
%% {interrupt, Reason} - 计算中断，用于 human-in-the-loop 场景
-type compute_status() :: ok | {error, term()} | {interrupt, term()}.

%% 计算结果（计算函数必须返回此结构）
%% vertex - 新顶点状态（成功时更新，失败/中断时保持原值）
%% outbox - 发出的消息（失败/中断时应为空列表）
%% status - 计算状态（必需）
-type compute_result() :: #{
    vertex := vertex(),
    outbox := [{vertex_id(), term()}],
    status := compute_status()
}.

%% 计算结果累加器（内部使用）
%% 用于 fold 过程中收集顶点计算结果
-type compute_acc() :: {
    Vertices :: #{vertex_id() => vertex()},
    Outbox :: [{vertex_id(), term()}],
    FailedVertices :: [{vertex_id(), term()}],
    InterruptedVertices :: [{vertex_id(), term()}]
}.

%% 重试结果
%% vertices: 更新后的顶点状态
%% outbox: 重试产生的输出消息
%% failed_vertices: 仍然失败的顶点
%% interrupted_vertices: 中断的顶点
-type retry_result() :: #{
    vertices := #{vertex_id() => vertex()},
    outbox := [{vertex_id(), term()}],
    failed_vertices := [{vertex_id(), term()}],
    interrupted_vertices := [{vertex_id(), term()}]
}.

%% 内部状态
-record(state, {
    worker_id     :: non_neg_integer(),    % Worker ID
    master        :: pid(),                 % Master 进程
    vertices      :: #{vertex_id() => vertex()},  % 本地顶点
    inbox         :: #{vertex_id() => [term()]},  % 收件箱
    compute_fn    :: fun((context()) -> context()),  % 计算函数
    superstep     :: non_neg_integer(),    % 当前超步
    num_workers   :: pos_integer(),        % Worker 总数
    num_vertices  :: non_neg_integer(),    % 全图顶点总数
    worker_pids   :: #{non_neg_integer() => pid()}  % Worker PID 映射
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
-spec start_superstep(pid(), non_neg_integer()) -> ok.
start_superstep(Pid, Superstep) ->
    gen_server:cast(Pid, {start_superstep, Superstep}).

%% @doc 接收来自其他 Worker 的消息
-spec receive_messages(pid(), [{vertex_id(), term()}]) -> ok.
receive_messages(Pid, Messages) ->
    gen_server:cast(Pid, {receive_messages, Messages}).

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
%% - Inbox: 顶点 inbox 映射（计算时使用的消息）
%%
%% 返回重试结果（同步调用）
-spec retry_vertices(pid(), [vertex_id()], #{vertex_id() => [term()]}) ->
    {ok, retry_result()} | {error, term()}.
retry_vertices(Pid, VertexIds, Inbox) ->
    gen_server:call(Pid, {retry_vertices, VertexIds, Inbox}).

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
        inbox = #{},
        compute_fn = ComputeFn,
        superstep = 0,
        num_workers = NumWorkers,
        num_vertices = maps:get(num_vertices, Opts, maps:size(Vertices)),
        worker_pids = maps:get(worker_pids, Opts, #{})
    },
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, state_to_map(State), State};

handle_call(get_vertices, _From, #state{vertices = Vertices} = State) ->
    {reply, Vertices, State};

handle_call({retry_vertices, VertexIds, Inbox}, _From, State) ->
    {Reply, NewState} = do_retry_vertices(VertexIds, Inbox, State),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({start_superstep, Superstep}, State) ->
    NewState = execute_superstep(State#state{superstep = Superstep}),
    {noreply, NewState};

handle_cast({receive_messages, Messages}, State) ->
    NewState = add_to_inbox(Messages, State),
    {noreply, NewState};

handle_cast({update_worker_pids, WorkerPids}, State) ->
    {noreply, State#state{worker_pids = WorkerPids}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({local_messages, Messages}, State) ->
    NewState = add_to_inbox(Messages, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% 超步执行
%%====================================================================

%% @private 执行一个超步
%% 返回更新后的 Worker 状态
%%
%% BSP 模型：消息在超步结束时统一上报给 Master，由 Master 集中路由
%% 不再实时发送消息，确保消息可靠性
-spec execute_superstep(#state{}) -> #state{}.
execute_superstep(#state{
    vertices = Vertices,
    inbox = Inbox,
    compute_fn = ComputeFn,
    superstep = Superstep,
    num_vertices = NumVertices
} = State) ->
    %% 1. 筛选需要计算的顶点（有消息或活跃）
    ActiveVertices = filter_active_vertices(Vertices, Inbox),

    %% 2. 执行所有顶点计算（返回失败和中断列表）
    {NewVertices, Outbox, FailedVertices, InterruptedVertices} = compute_vertices(
        ActiveVertices, Vertices, Inbox, ComputeFn, Superstep, NumVertices
    ),

    %% 3. 通知 Master 完成（含 inbox、outbox、失败和中断信息）
    %% 注意：inbox 用于 checkpoint，支持单顶点重启
    %% 消息合并由 Master 的 state_reducer 统一处理
    notify_master_done(State, Inbox, NewVertices, Outbox, FailedVertices, InterruptedVertices),

    %% 4. 返回更新后的状态
    State#state{vertices = NewVertices, inbox = #{}}.

%% @private 筛选需要计算的顶点
-spec filter_active_vertices(#{vertex_id() => vertex()},
                             #{vertex_id() => [term()]}) ->
    #{vertex_id() => vertex()}.
filter_active_vertices(Vertices, Inbox) ->
    InboxKeys = maps:keys(Inbox),
    maps:filter(
        fun(Id, V) ->
            lists:member(Id, InboxKeys) orelse pregel_vertex:is_active(V)
        end,
        Vertices
    ).

%% @doc 执行所有顶点计算
%%
%% 根据计算函数返回的 status 字段处理计算结果：
%% - status == ok: 更新顶点，收集 outbox
%% - status == {error, Reason}: 记录失败，不更新顶点，不发消息
%% - status == {interrupt, Reason}: 记录中断，不更新顶点，不发消息
%%
%% @returns {顶点集合, 输出消息, 失败顶点, 中断顶点}
-spec compute_vertices(
    ActiveVertices :: #{vertex_id() => vertex()},
    AllVertices :: #{vertex_id() => vertex()},
    Inbox :: #{vertex_id() => [term()]},
    ComputeFn :: fun((context()) -> compute_result()),
    Superstep :: non_neg_integer(),
    NumVertices :: non_neg_integer()
) -> compute_acc().
compute_vertices(ActiveVertices, AllVertices, Inbox, ComputeFn, Superstep, NumVertices) ->
    InitAcc = {AllVertices, [], [], []},  %% {Vertices, Outbox, Failed, Interrupted}
    maps:fold(
        fun(Id, Vertex, Acc) ->
            Messages = maps:get(Id, Inbox, []),
            ActiveVertex = activate_if_has_messages(Vertex, Messages),
            Context = make_context(ActiveVertex, Messages, Superstep, NumVertices),
            Result = ComputeFn(Context),
            process_compute_result(Id, Result, Acc)
        end,
        InitAcc,
        ActiveVertices
    ).

%% @private 处理单个顶点的计算结果
%% 根据 status 字段决定如何处理结果：
%% - ok: 更新顶点状态，收集输出消息
%% - error: 记录到失败列表
%% - interrupt: 记录到中断列表
-spec process_compute_result(vertex_id(), compute_result(), compute_acc()) -> compute_acc().
process_compute_result(Id, #{status := ok, vertex := NewVertex, outbox := Out},
                       {VAcc, OAcc, FailedAcc, InterruptedAcc}) ->
    %% 成功：更新顶点，收集消息
    {VAcc#{Id => NewVertex}, Out ++ OAcc, FailedAcc, InterruptedAcc};
process_compute_result(Id, #{status := {error, Reason}},
                       {VAcc, OAcc, FailedAcc, InterruptedAcc}) ->
    %% 失败：记录失败信息，不更新顶点，不收集消息
    {VAcc, OAcc, [{Id, Reason} | FailedAcc], InterruptedAcc};
process_compute_result(Id, #{status := {interrupt, Reason}, vertex := NewVertex},
                       {VAcc, OAcc, FailedAcc, InterruptedAcc}) ->
    %% 中断：更新顶点状态（保存业务信息如 tool_calls），但不收集消息
    {VAcc#{Id => NewVertex}, OAcc, FailedAcc, [{Id, Reason} | InterruptedAcc]};
process_compute_result(Id, #{status := {interrupt, Reason}},
                       {VAcc, OAcc, FailedAcc, InterruptedAcc}) ->
    %% 中断（无顶点更新）：只记录中断信息
    {VAcc, OAcc, FailedAcc, [{Id, Reason} | InterruptedAcc]}.

%% @private 如果有消息则激活顶点
-spec activate_if_has_messages(vertex(), [term()]) -> vertex().
activate_if_has_messages(Vertex, []) -> Vertex;
activate_if_has_messages(Vertex, _) -> pregel_vertex:activate(Vertex).

%% @private 创建计算上下文
-spec make_context(vertex(), [term()], non_neg_integer(), non_neg_integer()) -> context().
make_context(Vertex, Messages, Superstep, NumVertices) ->
    #{
        vertex => Vertex,
        messages => Messages,
        superstep => Superstep,
        num_vertices => NumVertices,
        outbox => []
    }.

%%====================================================================
%% 消息处理
%%====================================================================

%% @private 添加消息到收件箱
-spec add_to_inbox([{vertex_id(), term()}], #state{}) -> #state{}.
add_to_inbox(Messages, #state{inbox = Inbox} = State) ->
    NewInbox = pregel_utils:merge_message_groups(
        Inbox,
        pregel_utils:group_messages(Messages)
    ),
    State#state{inbox = NewInbox}.

%% 注意：route_messages, group_by_target_worker, send_to_worker, apply_combiner 函数已移除
%% BSP 模型改进：Worker 不再直接路由消息，改为上报 outbox 给 Master
%% Master 在 complete_superstep 中统一路由所有消息
%% 消息合并由 Master 的 state_reducer 统一处理

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 通知 Master 超步完成
%%
%% 上报内容包括：
%% - 活跃顶点数和消息数（用于终止检测）
%% - inbox 消息映射（用于 checkpoint，支持单顶点重启）
%% - outbox 消息列表（用于 Master 集中路由）
%% - 失败和中断顶点信息（用于错误处理）
-spec notify_master_done(
    State :: #state{},
    Inbox :: #{vertex_id() => [term()]},
    Vertices :: #{vertex_id() => vertex()},
    Outbox :: [{vertex_id(), term()}],
    FailedVertices :: [{vertex_id(), term()}],
    InterruptedVertices :: [{vertex_id(), term()}]
) -> ok.
notify_master_done(#state{worker_id = WorkerId, master = Master},
                   Inbox, Vertices, Outbox, FailedVertices, InterruptedVertices) ->
    Result = #{
        worker_id => WorkerId,
        active_count => pregel_utils:map_count(fun pregel_vertex:is_active/1, Vertices),
        message_count => length(Outbox),
        %% inbox 消息映射（用于 checkpoint，支持单顶点重启）
        inbox => Inbox,
        %% outbox 消息列表（用于 Master 集中路由）
        outbox => Outbox,
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
    inbox = Inbox,
    superstep = Superstep
}) ->
    #{
        worker_id => WorkerId,
        vertex_count => maps:size(Vertices),
        inbox_count => maps:size(Inbox),
        superstep => Superstep
    }.

%%====================================================================
%% 顶点重试
%%====================================================================

%% @private 执行顶点重试
%%
%% 只重新计算指定的顶点，使用提供的 inbox 消息。
%% 返回 {Result, NewState}，其中 Result 包含重试结果。
%% 消息合并由 Master 的 state_reducer 统一处理。
-spec do_retry_vertices([vertex_id()], #{vertex_id() => [term()]}, #state{}) ->
    {{ok, retry_result()}, #state{}}.
do_retry_vertices(VertexIds, Inbox, #state{
    vertices = Vertices,
    compute_fn = ComputeFn,
    superstep = Superstep,
    num_vertices = NumVertices
} = State) ->
    %% 1. 筛选本 Worker 拥有的顶点
    LocalVertexIds = [Id || Id <- VertexIds, maps:is_key(Id, Vertices)],

    %% 2. 构建要重试的顶点映射
    RetryVertices = maps:with(LocalVertexIds, Vertices),

    %% 3. 执行顶点计算
    {NewVertices, Outbox, FailedVertices, InterruptedVertices} = compute_vertices(
        RetryVertices, Vertices, Inbox, ComputeFn, Superstep, NumVertices
    ),

    %% 4. 构建结果
    Result = #{
        vertices => maps:with(LocalVertexIds, NewVertices),
        outbox => Outbox,
        failed_vertices => FailedVertices,
        interrupted_vertices => InterruptedVertices
    },

    %% 5. 更新 Worker 状态（合并新顶点状态）
    NewState = State#state{vertices = NewVertices},

    {{ok, Result}, NewState}.
