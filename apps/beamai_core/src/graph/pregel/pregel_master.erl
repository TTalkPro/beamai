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
-export_type([opts/0, result/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type graph() :: pregel_graph:graph().
-type compute_fn() :: fun((pregel_worker:context()) -> pregel_worker:context()).

%% Pregel 执行选项
-type opts() :: #{
    combiner => pregel_combiner:spec(),
    max_supersteps => pos_integer(),
    num_workers => pos_integer(),
    on_superstep => fun((non_neg_integer(), graph()) -> ok)
}.

%% Pregel 执行结果
-type result() :: #{
    status := completed | max_supersteps,
    graph := graph(),
    supersteps := non_neg_integer(),
    stats := #{atom() => term()}
}.

%% Master 内部状态
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
    pending_messages :: #{non_neg_integer() => [{term(), term()}]}  %% 待发送消息
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
    State = #state{
        graph = Graph,
        compute_fn = ComputeFn,
        combiner = maps:get(combiner, Opts, undefined),
        max_supersteps = maps:get(max_supersteps, Opts, 100),
        num_workers = maps:get(num_workers, Opts, erlang:system_info(schedulers)),
        workers = #{},
        superstep = 0,
        barrier = pregel_barrier:new(0),
        caller = undefined,
        on_superstep = maps:get(on_superstep, Opts, undefined),
        pending_messages = #{}
    },
    {ok, State}.

handle_call(start_execution, From, State) ->
    NewState = start_workers(State#state{caller = From}),
    broadcast_start_superstep(NewState),
    {noreply, NewState};

handle_call(get_graph, _From, #state{graph = Graph} = State) ->
    {reply, Graph, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({worker_done, _WorkerPid, Result}, State) ->
    NewState = handle_worker_done(Result, State),
    {noreply, NewState};

handle_cast({route_messages, TargetWorkerId, Messages}, State) ->
    NewState = handle_route_messages(TargetWorkerId, Messages, State),
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
    combiner = Combiner,
    num_workers = NumWorkers
} = State) ->
    %% 分区图
    Partitions = pregel_partition:partition_graph(Graph, NumWorkers),
    NumVertices = pregel_graph:size(Graph),

    %% 启动 Worker
    Workers = start_worker_processes(Partitions, ComputeFn, Combiner, NumWorkers, NumVertices),

    %% 广播 Worker PID 映射
    broadcast_worker_pids(Workers),

    State#state{
        workers = Workers,
        barrier = pregel_barrier:new(maps:size(Workers))
    }.

%% @private 启动各 Worker 进程
-spec start_worker_processes(#{non_neg_integer() => graph()},
                              compute_fn(),
                              pregel_combiner:spec() | undefined,
                              pos_integer(),
                              non_neg_integer()) ->
    #{non_neg_integer() => pid()}.
start_worker_processes(Partitions, ComputeFn, Combiner, NumWorkers, NumVertices) ->
    maps:fold(
        fun(WorkerId, WorkerGraph, Acc) ->
            Vertices = vertices_to_map(pregel_graph:vertices(WorkerGraph)),
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
-spec complete_superstep(#state{}) -> #state{}.
complete_superstep(#state{
    barrier = Barrier,
    superstep = Superstep,
    max_supersteps = MaxSupersteps,
    workers = Workers,
    pending_messages = PendingMessages
} = State) ->
    %% 1. 汇总结果
    Results = pregel_barrier:get_results(Barrier),
    {TotalActive, TotalMessages} = pregel_barrier:aggregate_results(Results),

    %% 2. 发送待处理消息
    deliver_pending_messages(PendingMessages, Workers),

    %% 3. 检查终止条件
    Halted = (TotalActive =:= 0) andalso (TotalMessages =:= 0),
    MaxReached = Superstep >= MaxSupersteps - 1,

    %% 4. 决定下一步
    case {Halted, MaxReached} of
        {true, _} -> finish_execution(completed, State);
        {_, true} -> finish_execution(max_supersteps, State);
        {false, false} -> start_next_superstep(State)
    end.

%% @private 发送待处理消息
-spec deliver_pending_messages(#{non_neg_integer() => [{term(), term()}]},
                                #{non_neg_integer() => pid()}) -> ok.
deliver_pending_messages(PendingMessages, Workers) ->
    maps:foreach(
        fun(TargetWorkerId, Messages) ->
            case maps:get(TargetWorkerId, Workers, undefined) of
                undefined -> ok;
                Pid -> pregel_worker:receive_messages(Pid, Messages)
            end
        end,
        PendingMessages
    ).

%% @private 开始下一超步
-spec start_next_superstep(#state{}) -> #state{}.
start_next_superstep(#state{superstep = Superstep, workers = Workers} = State) ->
    NewState = State#state{
        superstep = Superstep + 1,
        barrier = pregel_barrier:new(maps:size(Workers)),
        pending_messages = #{}
    },
    broadcast_start_superstep(NewState),
    NewState.

%%====================================================================
%% 完成处理
%%====================================================================

%% @private 完成执行
-spec finish_execution(completed | max_supersteps, #state{}) -> #state{}.
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
%% 消息路由
%%====================================================================

%% @private 处理消息路由请求
-spec handle_route_messages(non_neg_integer(), [{term(), term()}], #state{}) -> #state{}.
handle_route_messages(TargetWorkerId, Messages, #state{
    pending_messages = Pending,
    workers = Workers
} = State) ->
    case maps:get(TargetWorkerId, Workers, undefined) of
        undefined ->
            %% Worker 不存在，缓存消息
            Existing = maps:get(TargetWorkerId, Pending, []),
            State#state{pending_messages = Pending#{TargetWorkerId => Messages ++ Existing}};
        Pid ->
            %% 直接发送
            pregel_worker:receive_messages(Pid, Messages),
            State
    end.
