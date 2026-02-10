%%%-------------------------------------------------------------------
%%% @doc 图执行运行器 - 高级 run API
%%%
%%% 从 beamai_graph_engine.erl 提取的高级运行器模块。
%%% 负责 snapshot 管理、store 持久化、恢复选项构建等。
%%%
%%% 低级纯函数引擎核心在 beamai_graph_engine 中。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_runner).

%% === 高级 run API ===
-export([run_graph/2, run_graph/3]).

%% 类型导出
-export_type([run_options/0, run_result/0]).
-export_type([snapshot_strategy/0, store_config/0]).
-export_type([runner_snapshot_data/0]).

%%====================================================================
%% 类型定义
%%====================================================================

%% Runner 级别 Snapshot 数据（含执行器层状态）
-type runner_snapshot_data() :: #{
    type := beamai_graph_engine:snapshot_type(),
    pregel_snapshot := beamai_graph_engine:snapshot_data(),
    context := beamai_context:t(),
    iteration := non_neg_integer(),
    run_id => binary(),
    active_vertices => [beamai_graph_engine:vertex_id()],
    completed_vertices => [beamai_graph_engine:vertex_id()],
    superstep => non_neg_integer()
}.

%% Snapshot 策略类型
-type snapshot_strategy() ::
    every_superstep |
    {every_n, pos_integer()} |
    on_interrupt |
    on_error.

%% Store 配置类型
-type store_config() :: {module(), beamai_graph_store_behaviour:store_ref()}.

-type run_options() :: #{
    workers => pos_integer(),
    max_iterations => pos_integer(),
    max_supersteps => pos_integer(),
    timeout => pos_integer(),
    %% Snapshot 恢复选项
    restore_from => runner_snapshot_data(),
    resume_data => #{beamai_graph_engine:vertex_id() => term()},
    retry_vertices => [beamai_graph_engine:vertex_id()],
    run_id => binary(),
    %% Context 选项
    context => beamai_context:t(),
    field_reducers => beamai_graph_engine:field_reducers(),
    %% Store 相关选项
    store => store_config(),
    snapshot_strategy => snapshot_strategy(),
    graph_name => atom() | binary()
}.

-type run_result() :: #{
    status := completed | interrupted | error | max_iterations,
    final_state := beamai_context:t(),
    iterations := non_neg_integer(),
    error => term(),
    done_reason => beamai_graph_engine:done_reason(),
    snapshot => beamai_graph_engine:snapshot_data(),
    interrupted_vertices => [{beamai_graph_engine:vertex_id(), term()}],
    failed_vertices => [{beamai_graph_engine:vertex_id(), term()}]
}.

%%====================================================================
%% 高级 run API（含 snapshot/store）
%%====================================================================

%% @doc 运行图，使用初始 context
-spec run_graph(beamai_graph_builder:graph(), beamai_context:t()) -> run_result().
run_graph(Graph, InitialState) ->
    run_graph(Graph, InitialState, #{}).

%% @doc 运行图，使用初始 context 和选项
-spec run_graph(beamai_graph_builder:graph(), beamai_context:t(), run_options()) -> run_result().
run_graph(Graph, InitialState, Options) ->
    OptionsWithState = ensure_context(Options, InitialState),
    case needs_snapshot_mode(OptionsWithState) of
        true ->
            run_with_snapshot(Graph, InitialState, OptionsWithState);
        false ->
            run_simple(Graph, InitialState, OptionsWithState)
    end.

%%====================================================================
%% 执行模式
%%====================================================================

%% @private 确保 Options 中有 context
-spec ensure_context(run_options(), beamai_context:t()) -> run_options().
ensure_context(Options, InitialState) ->
    case maps:is_key(context, Options) of
        true -> Options;
        false -> Options#{context => InitialState}
    end.

%% @private 检查是否需要 snapshot 模式
-spec needs_snapshot_mode(run_options()) -> boolean().
needs_snapshot_mode(Options) ->
    maps:is_key(restore_from, Options) orelse
    maps:is_key(store, Options).

%% @private 简单执行模式（无 snapshot）
-spec run_simple(beamai_graph_builder:graph(), beamai_context:t(), run_options()) -> run_result().
run_simple(Graph, InitialState, Options) ->
    #{pregel_graph := PregelGraph} = Graph,

    MaxIterations = maps:get(max_iterations, Graph, 100),
    Context = maps:get(context, Options, InitialState),
    FieldReducers = maps:get(field_reducers, Options, #{}),

    ExecutorOpts = #{
        max_supersteps => maps:get(max_supersteps, Options, MaxIterations),
        context => Context,
        field_reducers => FieldReducers
    },

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = beamai_graph_engine:execute(PregelGraph, ComputeFn, ExecutorOpts),

    ExecutorResult = beamai_graph_compute:from_pregel_result(Result),
    handle_pregel_result(ExecutorResult, InitialState, Options).

%% @private Snapshot 执行模式
-spec run_with_snapshot(beamai_graph_builder:graph(), beamai_context:t(), run_options()) -> run_result().
run_with_snapshot(Graph, _InitialState, Options) ->
    #{pregel_graph := PregelGraph} = Graph,

    OptionsWithRunId = ensure_run_id(Options),

    ActualContext = maps:get(context, OptionsWithRunId),

    SnapshotData = maps:get(restore_from, OptionsWithRunId, undefined),
    {FinalContext, ExecutorRestoreOpts, StartIteration} =
        prepare_restore_options(SnapshotData, OptionsWithRunId, ActualContext),

    MaxIterations = maps:get(max_iterations, Graph, 100),
    FieldReducers = maps:get(field_reducers, OptionsWithRunId, #{}),

    ExecutorOpts0 = #{
        max_supersteps => maps:get(max_supersteps, OptionsWithRunId, MaxIterations),
        context => FinalContext,
        field_reducers => FieldReducers
    },

    ExecutorOpts = case ExecutorRestoreOpts of
        undefined -> ExecutorOpts0;
        _ -> ExecutorOpts0#{restore_from => ExecutorRestoreOpts}
    end,

    ComputeFn = beamai_graph_compute:compute_fn(),
    {ok, Engine} = beamai_graph_engine:new(PregelGraph, ComputeFn, ExecutorOpts),
    run_loop(Engine, StartIteration, OptionsWithRunId).

%% @private 高级执行循环（含 snapshot/store）
-spec run_loop(beamai_graph_engine:engine(), non_neg_integer(), run_options()) -> run_result().
run_loop(Engine, Iteration, Options) ->
    case beamai_graph_engine:do_step(Engine) of
        {{continue, Info}, NewEngine} ->
            SnapshotData = build_runner_snapshot_data(NewEngine, Info, Iteration, Options),
            Type = maps:get(type, Info),

            maybe_save_to_store(SnapshotData, Type, Options),

            case Type of
                interrupt ->
                    build_interrupted_run_result(SnapshotData, Info, Iteration);
                error ->
                    build_error_run_result(SnapshotData, Info, Iteration);
                _ ->
                    run_loop(NewEngine, next_iteration(Type, Iteration), Options)
            end;

        {{done, Reason, Info}, NewEngine} ->
            handle_done(NewEngine, Reason, Info, Iteration, Options)
    end.

%%====================================================================
%% 结果处理
%%====================================================================

%% @private 处理 Pregel 引擎执行结果（高级 run_simple 用）
-spec handle_pregel_result({ok, beamai_context:t()} | {error, term()}, beamai_context:t(), run_options()) -> run_result().
handle_pregel_result({ok, FinalState}, _InitialState, _Options) ->
    #{status => completed, final_state => FinalState, iterations => 0};
handle_pregel_result({error, {partial_result, PartialState, max_iterations_exceeded}}, _InitialState, Options) ->
    MaxIter = maps:get(max_iterations, Options, 100),
    #{status => max_iterations, final_state => PartialState, iterations => MaxIter, error => max_iterations_exceeded};
handle_pregel_result({error, {partial_result, PartialState, Reason}}, _InitialState, _Options) ->
    #{status => error, final_state => PartialState, iterations => 0, error => Reason}.

%%====================================================================
%% Snapshot 数据构建
%%====================================================================

%% @private 构建 runner 级别 snapshot 数据
-spec build_runner_snapshot_data(beamai_graph_engine:engine(), beamai_graph_engine:superstep_info(),
                                 non_neg_integer(), run_options()) ->
    runner_snapshot_data().
build_runner_snapshot_data(Engine, Info, Iteration, Options) ->
    RunId = maps:get(run_id, Options),
    PregelCheckpoint = beamai_graph_engine:extract_snapshot_data(Engine),
    CurrentContext = beamai_graph_engine:context(Engine),
    Type = maps:get(type, Info),
    Superstep = maps:get(superstep, Info, 0),

    Vertices = maps:get(vertices, PregelCheckpoint, #{}),
    {ActiveVertices, CompletedVertices} = classify_vertices(Vertices),

    #{
        type => Type,
        pregel_snapshot => PregelCheckpoint,
        context => CurrentContext,
        iteration => Iteration,
        run_id => RunId,
        active_vertices => ActiveVertices,
        completed_vertices => CompletedVertices,
        superstep => Superstep
    }.

%% @private 分类顶点状态
-spec classify_vertices(#{atom() => beamai_pregel_vertex:vertex()}) ->
    {ActiveVertices :: [atom()], CompletedVertices :: [atom()]}.
classify_vertices(Vertices) ->
    maps:fold(
        fun('__start__', _Vertex, Acc) -> Acc;
           ('__end__', _Vertex, Acc) -> Acc;
           (Id, Vertex, {Active, Completed}) ->
            case beamai_pregel_vertex:is_active(Vertex) of
                true -> {[Id | Active], Completed};
                false -> {Active, [Id | Completed]}
            end
        end,
        {[], []},
        Vertices
    ).

%%====================================================================
%% Snapshot 恢复
%%====================================================================

%% @private 准备从 snapshot 恢复的选项
-spec prepare_restore_options(runner_snapshot_data() | undefined, run_options(), beamai_context:t()) ->
    {beamai_context:t(), beamai_graph_engine:restore_opts() | undefined, non_neg_integer()}.
prepare_restore_options(undefined, _Options, Context) ->
    {Context, undefined, 0};
prepare_restore_options(SnapshotData, Options, _DefaultContext) ->
    PregelCheckpoint = maps:get(pregel_snapshot, SnapshotData),

    Context = case maps:get(context, SnapshotData, undefined) of
        undefined ->
            maps:get(context, PregelCheckpoint, beamai_context:new());
        C -> C
    end,
    Iteration = maps:get(iteration, SnapshotData, 0),
    ResumeData = maps:get(resume_data, Options, #{}),
    RetryVertices = maps:get(retry_vertices, Options, []),

    #{superstep := Superstep, vertices := Vertices} = PregelCheckpoint,

    RawPendingActivations = maps:get(pending_activations, PregelCheckpoint, []),
    PendingActivations = case RawPendingActivations of
        undefined -> [];
        List when is_list(List) -> List
    end,

    ResumeVertexIds = maps:keys(ResumeData),
    AllActivations = lists:usort(ResumeVertexIds ++ RetryVertices ++ PendingActivations),

    PregelRestoreOpts = #{
        superstep => Superstep,
        vertices => Vertices,
        pending_activations => AllActivations,
        context => Context,
        resume_data => ResumeData
    },

    {Context, PregelRestoreOpts, Iteration}.

%% @private 确保 Options 中存在 run_id
-spec ensure_run_id(run_options()) -> run_options().
ensure_run_id(Options) ->
    case maps:is_key(run_id, Options) of
        true -> Options;
        false -> Options#{run_id => beamai_id:gen_id(<<"run">>)}
    end.

%%====================================================================
%% 高级结果构建
%%====================================================================

%% @private 构建 interrupted 结果
-spec build_interrupted_run_result(runner_snapshot_data(), beamai_graph_engine:superstep_info(),
                                   non_neg_integer()) ->
    run_result().
build_interrupted_run_result(SnapshotData, Info, Iteration) ->
    PregelCheckpoint = maps:get(pregel_snapshot, SnapshotData),
    Context = maps:get(context, SnapshotData),
    InterruptedVertices = maps:get(interrupted_vertices, Info, []),
    #{
        status => interrupted,
        final_state => Context,
        iterations => Iteration,
        snapshot => PregelCheckpoint,
        interrupted_vertices => InterruptedVertices
    }.

%% @private 构建 error 结果
-spec build_error_run_result(runner_snapshot_data(), beamai_graph_engine:superstep_info(),
                             non_neg_integer()) ->
    run_result().
build_error_run_result(SnapshotData, Info, Iteration) ->
    PregelCheckpoint = maps:get(pregel_snapshot, SnapshotData),
    Context = maps:get(context, SnapshotData),
    FailedVertices = maps:get(failed_vertices, Info, []),
    #{
        status => error,
        final_state => Context,
        iterations => Iteration,
        snapshot => PregelCheckpoint,
        failed_vertices => FailedVertices
    }.

%% @private 计算下一次迭代数
-spec next_iteration(beamai_graph_engine:snapshot_type(), non_neg_integer()) -> non_neg_integer().
next_iteration(initial, Iteration) -> Iteration;
next_iteration(_, Iteration) -> Iteration + 1.

%% @private 处理执行完成（done 分支）
-spec handle_done(beamai_graph_engine:engine(), beamai_graph_engine:done_reason(),
                  beamai_graph_engine:superstep_info(),
                  non_neg_integer(), run_options()) -> run_result().
handle_done(Engine, Reason, Info, Iteration, Options) ->
    RunId = maps:get(run_id, Options),
    Superstep = maps:get(superstep, Info, 0),

    PregelCheckpoint = beamai_graph_engine:extract_snapshot_data(Engine),
    FinalContext = beamai_graph_engine:context(Engine),

    Vertices = maps:get(vertices, PregelCheckpoint, #{}),
    {ActiveVertices, CompletedVertices} = classify_vertices(Vertices),

    SnapshotData = #{
        type => final,
        pregel_snapshot => PregelCheckpoint,
        context => FinalContext,
        iteration => Iteration,
        run_id => RunId,
        active_vertices => ActiveVertices,
        completed_vertices => CompletedVertices,
        superstep => Superstep
    },

    maybe_save_to_store(SnapshotData, final, Options),

    Result = beamai_graph_engine:build_result(Engine),
    PregelResult = beamai_graph_compute:from_pregel_result(Result),
    FinalResult = handle_pregel_result(PregelResult, FinalContext, Options),

    FinalResult#{
        iterations => Iteration,
        done_reason => Reason,
        snapshot => PregelCheckpoint
    }.

%%====================================================================
%% Store 持久化
%%====================================================================

%% @private 根据 store/strategy 配置决定是否保存
-spec maybe_save_to_store(runner_snapshot_data(), beamai_graph_engine:snapshot_type(), run_options()) -> ok.
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
-spec should_save(snapshot_strategy(), atom(), non_neg_integer()) -> boolean().
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
-spec do_save_snapshot(module(), term(), runner_snapshot_data(), atom() | binary() | undefined, atom()) -> ok.
do_save_snapshot(StoreModule, StoreRef, SnapshotData, GraphName, TriggerType) ->
    Superstep = maps:get(superstep, SnapshotData, 0),
    SaveOpts = #{
        graph_name => GraphName,
        trigger => trigger_from_type(TriggerType),
        superstep => Superstep,
        metadata => #{}
    },
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
trigger_from_type(final) -> completed.
