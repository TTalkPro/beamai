%%%-------------------------------------------------------------------
%%% @doc 图执行器 - 任务构建、执行与结果处理
%%%
%%% 从 graph_executor 提取的纯函数模块。
%%% 负责任务列表构建、poolboy 并行执行、计算结果处理。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_executor_task).

-export([make_context/6, make_context/7, build_task_list/2, execute_tasks/8,
         process_compute_result/3]).

-type vertex_id() :: beamai_graph_engine:vertex_id().
-type vertex() :: beamai_pregel_vertex:vertex().
-type compute_fn() :: beamai_graph_engine:compute_fn().
-type context() :: beamai_graph_engine:compute_context().
-type compute_result() :: beamai_graph_engine:compute_result().
-type delta() :: beamai_graph_engine:delta().

%%====================================================================
%% 导出函数
%%====================================================================

%% @doc 创建计算上下文
-spec make_context(vertex_id(), vertex(), beamai_context:t(),
                   map() | undefined, non_neg_integer(), non_neg_integer()) -> context().
make_context(VertexId, Vertex, Context, VertexInput, Superstep, NumVertices) ->
    make_context(VertexId, Vertex, Context, VertexInput, Superstep, NumVertices, undefined).

%% @doc 创建计算上下文（含 resume_data）
-spec make_context(vertex_id(), vertex(), beamai_context:t(),
                   map() | undefined, non_neg_integer(), non_neg_integer(),
                   term() | undefined) -> context().
make_context(VertexId, Vertex, Context, VertexInput, Superstep, NumVertices, ResumeData) ->
    #{
        vertex_id => VertexId,
        vertex => Vertex,
        context => Context,
        vertex_input => VertexInput,
        superstep => Superstep,
        num_vertices => NumVertices,
        resume_data => ResumeData
    }.

%% @doc 构建扁平任务列表
-spec build_task_list(#{vertex_id() => vertex()},
                      #{vertex_id() => [beamai_graph_dispatch:dispatch()]}) ->
    [{vertex_id(), vertex(), map() | undefined}].
build_task_list(ActiveVertices, VertexInputs) ->
    maps:fold(
        fun(Id, Vertex, Acc) ->
            case maps:get(Id, VertexInputs, []) of
                [] ->
                    [{Id, Vertex, undefined} | Acc];
                Dispatches ->
                    [{Id, Vertex, beamai_graph_dispatch:get_input(D)} || D <- Dispatches] ++ Acc
            end
        end,
        [],
        ActiveVertices
    ).

%% @doc 执行任务列表
-spec execute_tasks(
    [{vertex_id(), vertex(), map() | undefined}],
    compute_fn(), beamai_context:t(), non_neg_integer(),
    non_neg_integer(), atom(), pos_integer(),
    #{vertex_id() => term()}
) -> {[delta()], [vertex_id()], [{vertex_id(), term()}], [{vertex_id(), term()}]}.
execute_tasks([], _ComputeFn, _Context, _Superstep, _NumVertices, _PoolName, _PoolTimeout, _ResumeDataMap) ->
    {[], [], [], []};
execute_tasks([{Id, Vertex, VertexInput}], ComputeFn, Context, Superstep, NumVertices, _PoolName, _PoolTimeout, ResumeDataMap) ->
    %% 单任务：协调进程内直接执行（零开销优化）
    RD = maps:get(Id, ResumeDataMap, undefined),
    ComputeCtx = make_context(Id, Vertex, Context, VertexInput, Superstep, NumVertices, RD),
    Result = safe_compute(ComputeFn, ComputeCtx),
    process_compute_result(Id, Result, {[], [], [], []});
execute_tasks(Tasks, ComputeFn, Context, Superstep, NumVertices, PoolName, PoolTimeout, ResumeDataMap) ->
    %% 多任务：并行执行
    execute_parallel_tasks(Tasks, ComputeFn, Context, Superstep, NumVertices,
                           PoolName, PoolTimeout, ResumeDataMap).

%% @doc 处理单个顶点的计算结果
-spec process_compute_result(vertex_id(), compute_result(),
    {[delta()], [vertex_id()], [{vertex_id(), term()}], [{vertex_id(), term()}]}) ->
    {[delta()], [vertex_id()], [{vertex_id(), term()}], [{vertex_id(), term()}]}.
process_compute_result(_Id, #{status := ok, delta := Delta} = Result,
                       {DeltaAcc, ActAcc, FailedAcc, InterruptedAcc}) ->
    Activations = maps:get(activations, Result, []),
    NewDelta = case maps:size(Delta) of
        0 -> DeltaAcc;
        _ -> [Delta | DeltaAcc]
    end,
    {NewDelta, Activations ++ ActAcc, FailedAcc, InterruptedAcc};
process_compute_result(Id, #{status := {error, Reason}},
                       {DeltaAcc, ActAcc, FailedAcc, InterruptedAcc}) ->
    {DeltaAcc, ActAcc, [{Id, Reason} | FailedAcc], InterruptedAcc};
process_compute_result(Id, #{status := {interrupt, Reason}} = Result,
                       {DeltaAcc, ActAcc, FailedAcc, InterruptedAcc}) ->
    NewDeltaAcc = case maps:get(delta, Result, #{}) of
        Delta when map_size(Delta) > 0 -> [Delta | DeltaAcc];
        _ -> DeltaAcc
    end,
    {NewDeltaAcc, ActAcc, FailedAcc, [{Id, Reason} | InterruptedAcc]}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 安全执行计算函数（捕获异常）
-spec safe_compute(compute_fn(), context()) -> compute_result().
safe_compute(ComputeFn, Context) ->
    try
        ComputeFn(Context)
    catch
        Class:Reason:Stack ->
            #{delta => #{}, activations => [], status => {error, {compute_error, {Class, Reason, Stack}}}}
    end.

%% @private 并行执行任务
-spec execute_parallel_tasks(
    [{vertex_id(), vertex(), map() | undefined}],
    compute_fn(), beamai_context:t(), non_neg_integer(),
    non_neg_integer(), atom(), pos_integer(),
    #{vertex_id() => term()}
) -> {[delta()], [vertex_id()], [{vertex_id(), term()}], [{vertex_id(), term()}]}.
execute_parallel_tasks(Tasks, ComputeFn, Context, Superstep, NumVertices,
                       PoolName, PoolTimeout, ResumeDataMap) ->
    Parent = self(),
    Ref = make_ref(),
    Deadline = erlang:monotonic_time(millisecond) + PoolTimeout + 5000,

    %% 全部 spawn，每个进程独立 poolboy:checkout 阻塞
    PidRefs = lists:map(fun({Id, Vertex, VertexInput}) ->
        RD = maps:get(Id, ResumeDataMap, undefined),
        ComputeCtx = make_context(Id, Vertex, Context, VertexInput, Superstep, NumVertices, RD),
        spawn_monitor(fun() ->
            Result = execute_in_pool(ComputeFn, ComputeCtx, PoolName, PoolTimeout),
            Parent ! {task_result, Ref, self(), Id, Result}
        end)
    end, Tasks),

    collect_all_results(PidRefs, Ref, Deadline, {[], [], [], []}).

%% @private 在池中执行计算
-spec execute_in_pool(compute_fn(), context(), atom(), pos_integer()) ->
    {ok, compute_result()} | {error, term()}.
execute_in_pool(ComputeFn, Context, PoolName, Timeout) ->
    try
        Worker = poolboy:checkout(PoolName, true, Timeout),
        try
            beamai_graph_pool_worker:execute(Worker, ComputeFn, Context)
        after
            poolboy:checkin(PoolName, Worker)
        end
    catch
        exit:{timeout, _} ->
            {error, {pool_checkout_timeout, Timeout}};
        exit:{noproc, _} ->
            {error, {pool_worker_not_found}};
        exit:{{nodedown, _}, _} ->
            {error, {pool_worker_nodedown}};
        exit:{normal, _} ->
            {error, {pool_worker_exited_normal}};
        exit:{Reason, _} ->
            {error, {pool_worker_exit, Reason}};
        Class:Reason ->
            {error, {pool_error, {Class, Reason}}}
    end.

%% @private 收集所有结果
-spec collect_all_results(
    [{pid(), reference()}], reference(), integer(),
    {[delta()], [vertex_id()], [{vertex_id(), term()}], [{vertex_id(), term()}]}
) -> {[delta()], [vertex_id()], [{vertex_id(), term()}], [{vertex_id(), term()}]}.
collect_all_results([], _Ref, _Deadline, Acc) ->
    Acc;
collect_all_results([{Pid, MonRef} | Rest], Ref, Deadline, Acc) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {task_result, Ref, Pid, Id, {ok, ComputeResult}} ->
            erlang:demonitor(MonRef, [flush]),
            NewAcc = process_compute_result(Id, ComputeResult, Acc),
            collect_all_results(Rest, Ref, Deadline, NewAcc);

        {task_result, Ref, Pid, Id, {error, Reason}} ->
            erlang:demonitor(MonRef, [flush]),
            ErrorResult = #{delta => #{}, activations => [], status => {error, Reason}},
            NewAcc = process_compute_result(Id, ErrorResult, Acc),
            collect_all_results(Rest, Ref, Deadline, NewAcc);

        {'DOWN', MonRef, process, Pid, Reason} ->
            ErrorResult = #{delta => #{}, activations => [], status => {error, {task_crash, Reason}}},
            NewAcc = process_compute_result(unknown, ErrorResult, Acc),
            collect_all_results(Rest, Ref, Deadline, NewAcc)

    after Remaining ->
        erlang:demonitor(MonRef, [flush]),
        exit(Pid, kill),
        ErrorResult = #{delta => #{}, activations => [], status => {error, execution_timeout}},
        NewAcc = process_compute_result(unknown, ErrorResult, Acc),
        collect_all_results(Rest, Ref, Deadline, NewAcc)
    end.
