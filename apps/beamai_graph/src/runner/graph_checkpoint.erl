%%%-------------------------------------------------------------------
%%% @doc Graph Checkpoint 执行模块
%%%
%%% 本模块负责带 Checkpoint 回调的图执行逻辑。
%%% 从 graph_runner 拆分出来，专注于 checkpoint 级别的处理。
%%%
%%% 核心功能:
%%% - Checkpoint 执行循环：每个超步完成后调用回调函数
%%% - Checkpoint 回调处理：根据回调结果决定继续、停止或重试
%%% - 从 Checkpoint 恢复：支持从中断点恢复执行
%%% - 结果构建：将 Pregel 结果转换为 graph_runner 格式
%%%
%%% Checkpoint 模式说明:
%%% 当提供 on_checkpoint 回调时，图执行进入 checkpoint 模式:
%%% 1. 每个超步完成后，构建 checkpoint_data 并调用回调
%%% 2. 回调返回 continue 则继续下一超步
%%% 3. 回调返回 {stop, Reason} 则停止执行，保存 checkpoint
%%% 4. 回调返回 {retry, VertexIds} 则重试指定顶点（仅 error 类型有效）
%%%
%%% 从 Checkpoint 恢复:
%%% 通过 restore_from 选项可以从之前保存的 checkpoint 恢复执行:
%%% - pregel_checkpoint: 必须，包含超步、顶点等 pregel 层状态
%%% - global_state: 可选，恢复时的全局状态
%%% - resume_data: 可选，注入的用户数据（会合并到 global_state）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_checkpoint).

%% === API 导出 ===
-export([
    run_with_checkpoint/4,
    run_checkpoint_loop/4,
    handle_checkpoint_continue/8,
    handle_checkpoint_done/6,
    prepare_restore_options/2,
    build_stopped_result/3,
    build_error_result_from_checkpoint/3,
    default_checkpoint_callback/2,
    ensure_run_id/1,
    classify_vertices/1
]).

%% === 类型定义 ===
-type state() :: graph_state:state().
-type run_options() :: graph_runner:run_options().
-type run_result() :: graph_runner:run_result().
-type checkpoint_data() :: graph_runner:checkpoint_data().
-type checkpoint_callback() :: graph_runner:checkpoint_callback().
-type checkpoint_callback_result() :: graph_runner:checkpoint_callback_result().
-type restore_options() :: graph_runner:restore_options().

%%====================================================================
%% API
%%====================================================================

%% @doc Checkpoint 模式执行入口
%%
%% 使用步进式 API 执行图，每个超步完成后调用 checkpoint 回调。
%% 节点计算逻辑和路由规则已存储在 vertex value 中。
%%
%% 执行流程:
%% 1. 准备恢复选项（如果有）
%% 2. 构建 Pregel 执行选项
%% 3. 启动 Pregel Master
%% 4. 进入 checkpoint 执行循环
-spec run_with_checkpoint(map(), state(), state(), run_options()) -> run_result().
run_with_checkpoint(Graph, _InitialState, ActualGlobalState, Options) ->
    #{pregel_graph := PregelGraph} = Graph,

    %% 确保 run_id 存在（整个执行过程中保持不变）
    OptionsWithRunId = ensure_run_id(Options),

    %% 检查是否从 checkpoint 恢复
    RestoreOpts = maps:get(restore_from, OptionsWithRunId, undefined),
    {FinalGlobalState, PregelRestoreOpts, StartIteration} =
        prepare_restore_options(RestoreOpts, ActualGlobalState),

    %% 准备 Pregel 执行选项（max_iterations 直接从 Graph 获取）
    MaxIterations = maps:get(max_iterations, Graph, 100),
    FieldReducers = maps:get(field_reducers, OptionsWithRunId, #{}),

    PregelOpts0 = #{
        max_supersteps => maps:get(max_supersteps, OptionsWithRunId, MaxIterations),
        num_workers => maps:get(workers, OptionsWithRunId, 1),
        global_state => FinalGlobalState,
        field_reducers => FieldReducers
    },

    %% 如果有恢复选项，添加到 PregelOpts
    PregelOpts = case PregelRestoreOpts of
        undefined -> PregelOpts0;
        _ -> PregelOpts0#{restore_from => PregelRestoreOpts}
    end,

    %% 启动 Pregel Master 并进入执行循环
    ComputeFn = graph_compute:compute_fn(),
    {ok, Master} = pregel:start(PregelGraph, ComputeFn, PregelOpts),
    try
        CheckpointCallback = maps:get(on_checkpoint, OptionsWithRunId, fun default_checkpoint_callback/2),
        run_checkpoint_loop(Master, CheckpointCallback, StartIteration, OptionsWithRunId)
    after
        pregel:stop(Master)
    end.

%% @doc 准备从 checkpoint 恢复的选项
%%
%% 处理 restore_from 选项，提取全局状态和 Pregel 恢复选项。
%% resume_data 会被合并到 global_state 中（键格式: <<"resume_data:vertex_id">>）。
%%
%% 返回值: {FinalGlobalState, PregelRestoreOpts, StartIteration}
-spec prepare_restore_options(restore_options() | undefined, state()) ->
    {state(), pregel:restore_opts() | undefined, non_neg_integer()}.
prepare_restore_options(undefined, GlobalState) ->
    %% 无恢复选项，使用默认值
    {GlobalState, undefined, 0};
prepare_restore_options(RestoreOpts, _DefaultGlobalState) ->
    %% 从恢复选项中提取 pregel checkpoint
    PregelCheckpoint = maps:get(pregel_checkpoint, RestoreOpts),

    %% 恢复全局状态（优先使用 RestoreOpts 中的，其次是 PregelCheckpoint 中的）
    GlobalState = maps:get(global_state, RestoreOpts,
                          maps:get(global_state, PregelCheckpoint, graph_state:new())),
    Iteration = maps:get(iteration, RestoreOpts, 0),
    ResumeData = maps:get(resume_data, RestoreOpts, #{}),

    %% 构建 pregel restore_opts
    #{superstep := Superstep, vertices := Vertices} = PregelCheckpoint,

    %% 处理 pending_activations（可能是 undefined 或列表）
    RawPendingActivations = maps:get(pending_activations, PregelCheckpoint, []),
    PendingActivations = case RawPendingActivations of
        undefined -> [];
        List when is_list(List) -> List
    end,

    %% 将 resume_data 中的顶点添加到激活列表
    ResumeVertexIds = maps:keys(ResumeData),
    AllActivations = lists:usort(ResumeVertexIds ++ PendingActivations),

    %% 将 resume_data 合并到 global_state
    %% 键格式: <<"resume_data:vertex_id">>，节点可通过此键获取恢复数据
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

%% @doc 默认 checkpoint 回调
%%
%% 不做任何处理，总是返回 continue。
%% 用于未指定回调时的默认行为。
-spec default_checkpoint_callback(pregel:superstep_info(), checkpoint_data()) -> continue.
default_checkpoint_callback(_Info, _CheckpointData) ->
    continue.

%% @doc Checkpoint 执行循环
%%
%% 核心执行循环，每次循环:
%% 1. 调用 pregel:step 执行一个超步
%% 2. 构建 checkpoint_data
%% 3. 调用 checkpoint 回调
%% 4. 根据回调结果决定下一步操作
-spec run_checkpoint_loop(pid(), checkpoint_callback(), non_neg_integer(), run_options()) ->
    run_result().
run_checkpoint_loop(Master, CheckpointCallback, Iteration, Options) ->
    RunId = maps:get(run_id, Options),
    case pregel:step(Master) of
        {continue, Info} ->
            %% ===== 继续执行：构建 checkpoint 并调用回调 =====
            PregelCheckpoint = pregel:get_checkpoint_data(Master),
            CurrentGlobalState = pregel:get_global_state(Master),
            Type = maps:get(type, Info),
            Superstep = maps:get(superstep, Info, 0),

            %% 分类顶点状态（活跃 vs 已完成）
            Vertices = maps:get(vertices, PregelCheckpoint, #{}),
            {ActiveVertices, CompletedVertices} = classify_vertices(Vertices),

            %% 构建 checkpoint 数据（包含完整的执行状态）
            CheckpointData = #{
                type => Type,
                pregel_checkpoint => PregelCheckpoint,
                global_state => CurrentGlobalState,
                iteration => Iteration,
                run_id => RunId,
                active_vertices => ActiveVertices,
                completed_vertices => CompletedVertices,
                superstep => Superstep
            },

            %% 调用用户提供的 checkpoint 回调
            CallbackResult = CheckpointCallback(Info, CheckpointData),

            %% 根据类型和回调结果决定下一步
            handle_checkpoint_continue(
                Type, CallbackResult, Master, CheckpointCallback,
                CheckpointData, Info, Iteration, Options);

        {done, Reason, Info} ->
            %% ===== 执行完成 =====
            handle_checkpoint_done(Master, Reason, Info, Iteration, CheckpointCallback, Options)
    end.

%% @doc 处理 continue 分支的不同 checkpoint 类型
%%
%% 根据 checkpoint 类型和回调结果决定操作:
%% - initial: 初始状态，只允许 continue 或 stop
%% - step: 正常超步完成，只允许 continue 或 stop
%% - error: 有失败顶点，允许 continue、stop 或 retry
%% - interrupt: 有中断顶点，只允许 continue 或 stop
%% - final: 不应出现在 continue 分支
-spec handle_checkpoint_continue(
    pregel:checkpoint_type(),
    checkpoint_callback_result(),
    pid(),
    checkpoint_callback(),
    checkpoint_data(),
    pregel:superstep_info(),
    non_neg_integer(),
    run_options()
) -> run_result().

%% === initial 类型处理 ===
handle_checkpoint_continue(initial, continue, Master, Callback, _Data, _Info, Iteration, Options) ->
    %% 继续执行下一超步
    run_checkpoint_loop(Master, Callback, Iteration, Options);
handle_checkpoint_continue(initial, {stop, Reason}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    %% 用户请求停止
    build_stopped_result(Data, Reason, Iteration);
handle_checkpoint_continue(initial, {retry, _}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    %% initial 类型不允许 retry
    build_error_result_from_checkpoint(Data, {invalid_operation, {retry_not_allowed, initial}}, Iteration);

%% === step 类型处理 ===
handle_checkpoint_continue(step, continue, Master, Callback, _Data, _Info, Iteration, Options) ->
    run_checkpoint_loop(Master, Callback, Iteration + 1, Options);
handle_checkpoint_continue(step, {stop, Reason}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    build_stopped_result(Data, Reason, Iteration);
handle_checkpoint_continue(step, {retry, _}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    %% step 类型不允许 retry（没有失败的顶点）
    build_error_result_from_checkpoint(Data, {invalid_operation, {retry_not_allowed, step}}, Iteration);

%% === error 类型处理（支持重试） ===
handle_checkpoint_continue(error, continue, Master, Callback, _Data, _Info, Iteration, Options) ->
    %% 忽略错误，继续执行
    run_checkpoint_loop(Master, Callback, Iteration + 1, Options);
handle_checkpoint_continue(error, {stop, Reason}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    build_stopped_result(Data, Reason, Iteration);
handle_checkpoint_continue(error, {retry, VertexIds}, Master, Callback, _Data, _Info, Iteration, Options) ->
    %% 重试失败的顶点
    RunId = maps:get(run_id, Options),
    case pregel:retry(Master, VertexIds) of
        {continue, NewInfo} ->
            %% 重试后继续执行：构建新的 checkpoint 并递归处理
            NewPregelCheckpoint = pregel:get_checkpoint_data(Master),
            NewGlobalState = pregel:get_global_state(Master),
            NewType = maps:get(type, NewInfo),
            NewSuperstep = maps:get(superstep, NewInfo, 0),

            NewVertices = maps:get(vertices, NewPregelCheckpoint, #{}),
            {NewActiveVertices, NewCompletedVertices} = classify_vertices(NewVertices),

            NewCheckpointData = #{
                type => NewType,
                pregel_checkpoint => NewPregelCheckpoint,
                global_state => NewGlobalState,
                iteration => Iteration,
                run_id => RunId,
                active_vertices => NewActiveVertices,
                completed_vertices => NewCompletedVertices,
                superstep => NewSuperstep
            },

            %% 重试后再次调用回调，让用户决定下一步
            NewCallbackResult = Callback(NewInfo, NewCheckpointData),
            handle_checkpoint_continue(
                NewType, NewCallbackResult, Master, Callback,
                NewCheckpointData, NewInfo, Iteration, Options);
        {done, Reason, DoneInfo} ->
            handle_checkpoint_done(Master, Reason, DoneInfo, Iteration, Callback, Options)
    end;

%% === interrupt 类型处理 ===
handle_checkpoint_continue(interrupt, continue, Master, Callback, _Data, _Info, Iteration, Options) ->
    run_checkpoint_loop(Master, Callback, Iteration + 1, Options);
handle_checkpoint_continue(interrupt, {stop, Reason}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    build_stopped_result(Data, Reason, Iteration);
handle_checkpoint_continue(interrupt, {retry, _}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    %% interrupt 类型不允许 retry（中断需要用户输入，不是简单重试能解决的）
    build_error_result_from_checkpoint(Data, {invalid_operation, {retry_not_allowed, interrupt}}, Iteration);

%% === final 类型处理（不应出现在 continue 分支） ===
handle_checkpoint_continue(final, _, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    build_error_result_from_checkpoint(Data, {invalid_state, final_in_continue}, Iteration).

%% @doc 处理执行完成（done 分支）
%%
%% 执行完成时:
%% 1. 构建最终的 checkpoint 数据
%% 2. 调用 checkpoint 回调（type=final）
%% 3. 获取并转换 Pregel 结果
-spec handle_checkpoint_done(pid(), pregel:done_reason(), pregel:superstep_info(),
                             non_neg_integer(), checkpoint_callback(), run_options()) -> run_result().
handle_checkpoint_done(Master, Reason, Info, Iteration, CheckpointCallback, Options) ->
    RunId = maps:get(run_id, Options),
    Superstep = maps:get(superstep, Info, 0),

    %% 获取最终状态
    PregelCheckpoint = pregel:get_checkpoint_data(Master),
    FinalGlobalState = pregel:get_global_state(Master),

    %% 分类顶点
    Vertices = maps:get(vertices, PregelCheckpoint, #{}),
    {ActiveVertices, CompletedVertices} = classify_vertices(Vertices),

    %% 构建最终 checkpoint 数据
    CheckpointData = #{
        type => final,
        pregel_checkpoint => PregelCheckpoint,
        global_state => FinalGlobalState,
        iteration => Iteration,
        run_id => RunId,
        active_vertices => ActiveVertices,
        completed_vertices => CompletedVertices,
        superstep => Superstep
    },

    %% 调用回调（通知执行完成）
    _ = CheckpointCallback(Info#{type => final}, CheckpointData),

    %% 转换 Pregel 结果为 graph_runner 格式
    Result = pregel:get_result(Master),
    PregelResult = graph_compute:from_pregel_result(Result),
    FinalResult = handle_pregel_result(PregelResult, FinalGlobalState, Options),

    %% 添加额外信息
    FinalResult#{
        iterations => Iteration,
        done_reason => Reason,
        checkpoint => PregelCheckpoint
    }.

%% @doc 构建用户停止时的结果
-spec build_stopped_result(checkpoint_data(), term(), non_neg_integer()) -> run_result().
build_stopped_result(CheckpointData, Reason, Iteration) ->
    PregelCheckpoint = maps:get(pregel_checkpoint, CheckpointData),
    GlobalState = maps:get(global_state, CheckpointData),
    Type = maps:get(type, CheckpointData),
    #{
        status => stopped,
        final_state => GlobalState,
        iterations => Iteration,
        error => {user_stopped, Reason},
        checkpoint => PregelCheckpoint,
        checkpoint_type => Type
    }.

%% @doc 构建错误结果
-spec build_error_result_from_checkpoint(checkpoint_data(), term(), non_neg_integer()) -> run_result().
build_error_result_from_checkpoint(CheckpointData, Reason, Iteration) ->
    PregelCheckpoint = maps:get(pregel_checkpoint, CheckpointData),
    GlobalState = maps:get(global_state, CheckpointData),
    Type = maps:get(type, CheckpointData),
    #{
        status => error,
        final_state => GlobalState,
        iterations => Iteration,
        error => Reason,
        checkpoint => PregelCheckpoint,
        checkpoint_type => Type
    }.

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
%% 内部函数
%%====================================================================

%% @private 处理 Pregel 引擎执行结果
%%
%% 将 graph_compute:from_pregel_result/1 的返回值转换为 run_result 格式。
-spec handle_pregel_result({ok, state()} | {error, term()}, state(), run_options()) -> run_result().
handle_pregel_result({ok, FinalState}, _InitialState, _Options) ->
    #{status => completed, final_state => FinalState, iterations => 0};
handle_pregel_result({error, {partial_result, PartialState, max_iterations_exceeded}}, _InitialState, Options) ->
    MaxIter = maps:get(max_iterations, Options, 100),
    #{status => max_iterations, final_state => PartialState, iterations => MaxIter};
handle_pregel_result({error, {partial_result, PartialState, Reason}}, _InitialState, _Options) ->
    #{status => error, final_state => PartialState, iterations => 0, error => Reason};
handle_pregel_result({error, max_iterations_exceeded}, InitialState, Options) ->
    MaxIter = maps:get(max_iterations, Options, 100),
    #{status => max_iterations, final_state => InitialState, iterations => MaxIter, error => max_iterations_exceeded};
handle_pregel_result({error, Reason}, InitialState, _Options) ->
    #{status => error, final_state => InitialState, iterations => 0, error => Reason}.
