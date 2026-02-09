%%%-------------------------------------------------------------------
%%% @doc Graph Snapshot 执行模块
%%%
%%% 本模块负责带 Snapshot 的图执行逻辑。
%%% 从 graph_runner 拆分出来，专注于 snapshot 级别的处理。
%%%
%%% 核心功能:
%%% - Snapshot 执行循环：每个超步完成后自动处理 interrupt/error
%%% - 从 Snapshot 恢复：支持从中断点恢复执行
%%% - Store 持久化：根据策略自动保存 snapshot 到 store
%%% - 结果构建：将 Pregel 结果转换为 graph_runner 格式
%%%
%%% 执行模式说明:
%%% interrupt/error 自动返回给调用者，不需要回调：
%%% 1. step → 按 strategy 保存 store → 继续
%%% 2. interrupt → 保存 store → 返回 #{status => interrupted, ...}
%%% 3. error → 保存 store → 返回 #{status => error, ...}
%%% 4. done → 保存 store → 返回最终结果
%%%
%%% 从 Snapshot 恢复:
%%% 通过 restore_from 选项直接传入 snapshot_data() 即可恢复执行。
%%% resume_data 通过 run_options 顶层传入，会合并到 global_state。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_snapshot).

%% === API 导出 ===
-export([
    run_with_snapshot/4,
    prepare_restore_options/3,
    ensure_run_id/1,
    classify_vertices/1
]).

%% === 类型定义 ===
-type state() :: graph_state:state().
-type run_options() :: graph_runner:run_options().
-type run_result() :: graph_runner:run_result().
-type snapshot_data() :: graph_runner:snapshot_data().

%%====================================================================
%% API
%%====================================================================

%% @doc Snapshot 模式执行入口
%%
%% 使用步进式 API 执行图，interrupt/error 自动返回。
%% 节点计算逻辑和路由规则已存储在 vertex value 中。
%%
%% 执行流程:
%% 1. 准备恢复选项（如果有）
%% 2. 构建 Pregel 执行选项
%% 3. 启动 Pregel Master
%% 4. 进入执行循环
-spec run_with_snapshot(map(), state(), state(), run_options()) -> run_result().
run_with_snapshot(Graph, _InitialState, ActualGlobalState, Options) ->
    #{pregel_graph := PregelGraph} = Graph,

    %% 确保 run_id 存在（整个执行过程中保持不变）
    OptionsWithRunId = ensure_run_id(Options),

    %% 检查是否从 snapshot 恢复
    SnapshotData = maps:get(restore_from, OptionsWithRunId, undefined),
    {FinalGlobalState, ExecutorRestoreOpts, StartIteration} =
        prepare_restore_options(SnapshotData, OptionsWithRunId, ActualGlobalState),

    %% 准备执行选项（max_iterations 直接从 Graph 获取）
    MaxIterations = maps:get(max_iterations, Graph, 100),
    FieldReducers = maps:get(field_reducers, OptionsWithRunId, #{}),

    ExecutorOpts0 = #{
        max_supersteps => maps:get(max_supersteps, OptionsWithRunId, MaxIterations),
        global_state => FinalGlobalState,
        field_reducers => FieldReducers
    },

    %% 如果有恢复选项，添加到 ExecutorOpts
    ExecutorOpts = case ExecutorRestoreOpts of
        undefined -> ExecutorOpts0;
        _ -> ExecutorOpts0#{restore_from => ExecutorRestoreOpts}
    end,

    %% 启动 Executor 并进入执行循环
    ComputeFn = graph_compute:compute_fn(),
    {ok, Executor} = graph_executor:start_link(PregelGraph, ComputeFn, ExecutorOpts),
    try
        run_loop(Executor, StartIteration, OptionsWithRunId)
    after
        graph_executor:stop(Executor)
    end.

%% @doc 准备从 snapshot 恢复的选项
%%
%% 处理 restore_from 选项，从 snapshot_data() 提取全局状态和 Pregel 恢复选项。
%%
%% 恢复有两种模式：
%% - resume_data: interrupt 恢复，注入用户数据到 global_state（键格式: <<"resume_data:vertex_id">>）
%% - retry_vertices: error 重试，仅重新激活指定顶点，不修改状态
%%
%% 返回值: {FinalGlobalState, PregelRestoreOpts, StartIteration}
-spec prepare_restore_options(snapshot_data() | undefined, run_options(), state()) ->
    {state(), graph_executor:restore_opts() | undefined, non_neg_integer()}.
prepare_restore_options(undefined, _Options, GlobalState) ->
    %% 无恢复选项，使用默认值
    {GlobalState, undefined, 0};
prepare_restore_options(SnapshotData, Options, _DefaultGlobalState) ->
    %% 从 snapshot_data 中提取 pregel snapshot
    PregelCheckpoint = maps:get(pregel_snapshot, SnapshotData),

    %% 恢复全局状态（优先使用 SnapshotData 中的，其次是 PregelCheckpoint 中的）
    GlobalState = maps:get(global_state, SnapshotData,
                          maps:get(global_state, PregelCheckpoint, graph_state:new())),
    Iteration = maps:get(iteration, SnapshotData, 0),
    ResumeData = maps:get(resume_data, Options, #{}),
    RetryVertices = maps:get(retry_vertices, Options, []),

    %% 构建 pregel restore_opts
    #{superstep := Superstep, vertices := Vertices} = PregelCheckpoint,

    %% 处理 pending_activations（可能是 undefined 或列表）
    RawPendingActivations = maps:get(pending_activations, PregelCheckpoint, []),
    PendingActivations = case RawPendingActivations of
        undefined -> [];
        List when is_list(List) -> List
    end,

    %% 合并激活列表：resume_data 顶点 + retry_vertices + 原有 pending
    ResumeVertexIds = maps:keys(ResumeData),
    AllActivations = lists:usort(ResumeVertexIds ++ RetryVertices ++ PendingActivations),

    %% 将 resume_data 合并到 global_state
    %% 键格式: <<"resume_data:vertex_id">>，节点可通过此键获取恢复数据
    %% retry_vertices 不注入任何数据，仅激活
    FinalGlobalState = case map_size(ResumeData) of
        0 -> GlobalState;
        _ -> maps:fold(
                 fun(VertexId, Data, Acc) ->
                     %% 将 vertex_id 转换为 binary
                     VertexIdBin = if
                         is_atom(VertexId) -> atom_to_binary(VertexId, utf8);
                         is_binary(VertexId) -> VertexId;
                         true -> iolist_to_binary(io_lib:format("~p", [VertexId]))
                     end,
                     Key = <<"resume_data:", VertexIdBin/binary>>,
                     graph_state:set(Acc, Key, Data)
                 end,
                 GlobalState,
                 ResumeData
             )
    end,

    PregelRestoreOpts = #{
        superstep => Superstep,
        vertices => Vertices,
        pending_activations => AllActivations,
        global_state => FinalGlobalState
    },

    {FinalGlobalState, PregelRestoreOpts, Iteration}.

%% @doc 确保 Options 中存在 run_id
%%
%% run_id 用于标识一次完整的执行过程，在恢复执行时保持不变。
-spec ensure_run_id(run_options()) -> run_options().
ensure_run_id(Options) ->
    case maps:is_key(run_id, Options) of
        true -> Options;
        false -> Options#{run_id => beamai_id:gen_id(<<"run">>)}
    end.

%% @doc 分类顶点状态
%%
%% 将顶点分为两类:
%% - 活跃顶点: 尚未完成计算的顶点
%% - 已完成顶点: 已调用 vote_to_halt 的顶点
%%
%% 特殊节点（__start__, __end__）不计入分类。
-spec classify_vertices(#{atom() => pregel_vertex:vertex()}) ->
    {ActiveVertices :: [atom()], CompletedVertices :: [atom()]}.
classify_vertices(Vertices) ->
    maps:fold(
        fun('__start__', _Vertex, Acc) -> Acc;  %% 跳过起始节点
           ('__end__', _Vertex, Acc) -> Acc;    %% 跳过终止节点
           (Id, Vertex, {Active, Completed}) ->
            case pregel_vertex:is_active(Vertex) of
                true -> {[Id | Active], Completed};
                false -> {Active, [Id | Completed]}
            end
        end,
        {[], []},
        Vertices
    ).

%%====================================================================
%% 内部函数：执行循环
%%====================================================================

%% @private 执行循环
%%
%% 每次循环:
%% 1. 调用 graph_executor:step 执行一个超步
%% 2. 构建 snapshot_data
%% 3. 根据类型自动处理：
%%    - step/initial: 保存 store → 继续
%%    - interrupt: 保存 store → 返回 interrupted
%%    - error: 保存 store → 返回 error
-spec run_loop(pid(), non_neg_integer(), run_options()) -> run_result().
run_loop(Executor, Iteration, Options) ->
    case graph_executor:step(Executor) of
        {continue, Info} ->
            %% 构建 snapshot 数据
            SnapshotData = build_snapshot_data(Executor, Info, Iteration, Options),
            Type = maps:get(type, Info),

            %% 根据策略保存到 store
            maybe_save_to_store(SnapshotData, Type, Options),

            %% 根据类型决定下一步
            case Type of
                interrupt ->
                    build_interrupted_result(SnapshotData, Info, Iteration);
                error ->
                    build_error_result(SnapshotData, Info, Iteration);
                _ ->
                    %% initial/step: 继续执行
                    run_loop(Executor, next_iteration(Type, Iteration), Options)
            end;

        {done, Reason, Info} ->
            handle_done(Executor, Reason, Info, Iteration, Options)
    end.

%% @private 构建 snapshot 数据
-spec build_snapshot_data(pid(), graph_executor:superstep_info(), non_neg_integer(), run_options()) ->
    snapshot_data().
build_snapshot_data(Executor, Info, Iteration, Options) ->
    RunId = maps:get(run_id, Options),
    PregelCheckpoint = graph_executor:get_snapshot_data(Executor),
    CurrentGlobalState = graph_executor:get_global_state(Executor),
    Type = maps:get(type, Info),
    Superstep = maps:get(superstep, Info, 0),

    %% 分类顶点状态
    Vertices = maps:get(vertices, PregelCheckpoint, #{}),
    {ActiveVertices, CompletedVertices} = classify_vertices(Vertices),

    #{
        type => Type,
        pregel_snapshot => PregelCheckpoint,
        global_state => CurrentGlobalState,
        iteration => Iteration,
        run_id => RunId,
        active_vertices => ActiveVertices,
        completed_vertices => CompletedVertices,
        superstep => Superstep
    }.

%% @private 构建 interrupted 结果
-spec build_interrupted_result(snapshot_data(), graph_executor:superstep_info(), non_neg_integer()) ->
    run_result().
build_interrupted_result(SnapshotData, Info, Iteration) ->
    PregelCheckpoint = maps:get(pregel_snapshot, SnapshotData),
    GlobalState = maps:get(global_state, SnapshotData),
    InterruptedVertices = maps:get(interrupted_vertices, Info, []),
    #{
        status => interrupted,
        final_state => GlobalState,
        iterations => Iteration,
        snapshot => PregelCheckpoint,
        interrupted_vertices => InterruptedVertices
    }.

%% @private 构建 error 结果（保留 snapshot 用于恢复）
-spec build_error_result(snapshot_data(), graph_executor:superstep_info(), non_neg_integer()) ->
    run_result().
build_error_result(SnapshotData, Info, Iteration) ->
    PregelCheckpoint = maps:get(pregel_snapshot, SnapshotData),
    GlobalState = maps:get(global_state, SnapshotData),
    FailedVertices = maps:get(failed_vertices, Info, []),
    #{
        status => error,
        final_state => GlobalState,
        iterations => Iteration,
        snapshot => PregelCheckpoint,
        failed_vertices => FailedVertices
    }.

%% @private 计算下一次迭代数
-spec next_iteration(graph_executor:snapshot_type(), non_neg_integer()) -> non_neg_integer().
next_iteration(initial, Iteration) -> Iteration;
next_iteration(_, Iteration) -> Iteration + 1.

%% @private 处理执行完成（done 分支）
-spec handle_done(pid(), graph_executor:done_reason(), graph_executor:superstep_info(),
                  non_neg_integer(), run_options()) -> run_result().
handle_done(Executor, Reason, Info, Iteration, Options) ->
    RunId = maps:get(run_id, Options),
    Superstep = maps:get(superstep, Info, 0),

    %% 获取最终状态
    PregelCheckpoint = graph_executor:get_snapshot_data(Executor),
    FinalGlobalState = graph_executor:get_global_state(Executor),

    %% 分类顶点
    Vertices = maps:get(vertices, PregelCheckpoint, #{}),
    {ActiveVertices, CompletedVertices} = classify_vertices(Vertices),

    %% 构建最终 snapshot 数据
    SnapshotData = #{
        type => final,
        pregel_snapshot => PregelCheckpoint,
        global_state => FinalGlobalState,
        iteration => Iteration,
        run_id => RunId,
        active_vertices => ActiveVertices,
        completed_vertices => CompletedVertices,
        superstep => Superstep
    },

    %% 保存最终 snapshot 到 store
    maybe_save_to_store(SnapshotData, final, Options),

    %% 转换执行器结果为 graph_runner 格式
    Result = graph_executor:get_result(Executor),
    PregelResult = graph_compute:from_pregel_result(Result),
    FinalResult = graph_runner:handle_pregel_result(PregelResult, FinalGlobalState, Options),

    %% 添加额外信息
    FinalResult#{
        iterations => Iteration,
        done_reason => Reason,
        snapshot => PregelCheckpoint
    }.

%%====================================================================
%% 内部函数：Store 持久化
%%====================================================================

%% @private 根据 store/strategy 配置决定是否保存
-spec maybe_save_to_store(snapshot_data(), graph_executor:snapshot_type(), run_options()) -> ok.
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
-spec should_save(graph_runner:snapshot_strategy(), atom(), non_neg_integer()) -> boolean().
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
-spec do_save_snapshot(module(), term(), snapshot_data(), atom() | binary() | undefined, atom()) -> ok.
do_save_snapshot(StoreModule, StoreRef, SnapshotData, GraphName, TriggerType) ->
    Superstep = maps:get(superstep, SnapshotData, 0),
    SaveOpts = #{
        graph_name => GraphName,
        trigger => trigger_from_type(TriggerType),
        superstep => Superstep,
        metadata => #{}
    },
    %% 错误仅记录日志，不影响图执行
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

