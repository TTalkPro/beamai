%%%-------------------------------------------------------------------
%%% @doc 图执行引擎模块
%%%
%%% 使用 Pregel 分布式图计算引擎执行图。
%%%
%%% 主要功能:
%%% - run/2,3: 批量执行，使用 Pregel BSP 模型
%%% - stream/2,3: 流式执行，逐步返回状态
%%% - step/2: 单步执行，用于调试和流式迭代
%%%
%%% Pregel 引擎特点:
%%% - 利用 Pregel BSP 模型执行
%%% - 可支持并行 Worker
%%% - 与 pregel 模块完全整合
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_runner).

%% API 导出
-export([run/2, run/3]).
-export([step/2]).
-export([stream/2, stream/3]).
%% 内部函数导出（用于测试）
-export([needs_checkpoint_mode/1]).

%% 类型定义
-type graph() :: graph_builder:graph().
-type node_id() :: graph_node:node_id().
-type state() :: graph_state:state().

%% Checkpoint 数据类型（包含 pregel 层状态）
%% 注意：graph 层状态存储在各顶点的 value.result 中，
%% 多扇出场景下状态分散在多个顶点，只有 __end__ 才有合并后的最终状态
%%
%% Checkpoint 类型说明：
%% - initial: 超步 0 执行前的初始状态
%% - step: 正常超步完成
%% - error: 超步完成但有失败的顶点
%% - interrupt: 超步完成但有中断的顶点（human-in-the-loop）
%% - final: 执行结束
-type checkpoint_data() :: #{
    type := pregel:checkpoint_type(),              %% checkpoint 类型
    pregel_checkpoint := pregel:checkpoint_data(), %% pregel 层 checkpoint（含所有顶点状态）
    iteration := non_neg_integer()                 %% 当前迭代次数
}.

%% Checkpoint 回调结果类型
%% - continue: 继续执行下一超步
%% - {stop, Reason}: 停止执行，保存 checkpoint 以便恢复
%% - {retry, VertexIds}: 重试指定顶点（仅 error 类型有效，同步操作）
-type checkpoint_callback_result() ::
    continue |
    {stop, term()} |
    {retry, [pregel:vertex_id()]}.

%% Checkpoint 回调函数类型
%% 输入：superstep_info 和 checkpoint_data
%% 返回：checkpoint_callback_result()
-type checkpoint_callback() :: fun((pregel:superstep_info(), checkpoint_data()) ->
    checkpoint_callback_result()).

%% Checkpoint 恢复选项
%% 注意：graph 层状态从 pregel_checkpoint.vertices 中恢复
%% resume_data 用于 human-in-loop 场景，将用户输入注入到对应顶点
-type restore_options() :: #{
    pregel_checkpoint := pregel:checkpoint_data(),  %% pregel checkpoint 数据（含顶点状态）
    iteration => non_neg_integer(),                 %% 迭代次数（可选）
    resume_data => #{pregel:vertex_id() => term()}  %% 恢复时注入的用户数据（可选）
}.

-type run_options() :: #{
    workers => pos_integer(),        %% Pregel worker 数量 (默认 1)
    trace => boolean(),              %% 启用执行追踪 (用于 stream/step)
    max_iterations => pos_integer(), %% 最大迭代次数
    timeout => pos_integer(),        %% 超时时间 (毫秒)
    %% Checkpoint 相关选项
    on_checkpoint => checkpoint_callback(),  %% 每个超步完成后的回调
    restore_from => restore_options(),       %% 从 checkpoint 恢复
    run_id => binary()                       %% 外部传入的执行 ID
}.

-export_type([checkpoint_data/0, checkpoint_callback/0, checkpoint_callback_result/0, restore_options/0]).

-type execution_context() :: #{
    graph := graph(),
    current_node := node_id(),
    state := state(),
    iteration := non_neg_integer(),
    trace := [trace_entry()],
    options := run_options()
}.

-type trace_entry() :: #{
    superstep := non_neg_integer(),
    node := node_id(),
    state_before := state(),
    state_after := state(),
    next_node := node_id() | [node_id()]
}.

-type run_result() :: #{
    status := completed | error | max_iterations | stopped,
    final_state := state(),
    iterations := non_neg_integer(),
    trace => [trace_entry()],
    error => term(),
    done_reason => pregel:done_reason()  %% checkpoint 模式下的完成原因
}.

-export_type([run_options/0, run_result/0, execution_context/0, trace_entry/0]).

%%====================================================================
%% 主执行 API
%%====================================================================

%% @doc 运行图，使用初始状态
-spec run(graph(), state()) -> run_result().
run(Graph, InitialState) ->
    run(Graph, InitialState, #{}).

%% @doc 运行图，使用初始状态和选项
%%
%% 使用 Pregel 引擎执行图，返回执行结果。
%%
%% 执行模式:
%% - 简单模式: 不提供 checkpoint 选项时，直接使用 pregel:run
%% - Checkpoint 模式: 提供 on_checkpoint 或 restore_from 时，使用步进式 API
%%
%% Checkpoint 选项:
%% - on_checkpoint: 每个超步完成后调用的回调函数，用于保存 checkpoint
%% - restore_from: 从 checkpoint 恢复执行
-spec run(graph(), state(), run_options()) -> run_result().
run(Graph, InitialState, Options) ->
    case needs_checkpoint_mode(Options) of
        true ->
            run_with_checkpoint(Graph, InitialState, Options);
        false ->
            run_simple(Graph, InitialState, Options)
    end.

%% @private 检查是否需要 checkpoint 模式
-spec needs_checkpoint_mode(run_options()) -> boolean().
needs_checkpoint_mode(Options) ->
    maps:is_key(on_checkpoint, Options) orelse maps:is_key(restore_from, Options).

%% @private 简单执行模式（无 checkpoint）
-spec run_simple(graph(), state(), run_options()) -> run_result().
run_simple(Graph, InitialState, Options) ->
    #{pregel_graph := PregelGraph, config := Config} = Graph,

    %% 注入初始状态到 __start__ 顶点
    PregelGraphWithState = graph_compute:inject_initial_state(PregelGraph, InitialState),

    %% 准备执行选项
    MaxIterations = maps:get(max_iterations, Config, 100),
    PregelOpts = #{
        max_supersteps => maps:get(max_supersteps, Options, MaxIterations),
        num_workers => maps:get(workers, Options, 1)
    },

    %% 使用全局计算函数执行
    ComputeFn = graph_compute:compute_fn(),
    Result = pregel:run(PregelGraphWithState, ComputeFn, PregelOpts),

    %% 提取结果
    PregelResult = graph_compute:from_pregel_result(Result),
    handle_pregel_result(PregelResult, InitialState, Options).

%% @private Checkpoint 执行模式（使用步进式 API）
-spec run_with_checkpoint(graph(), state(), run_options()) -> run_result().
run_with_checkpoint(Graph, InitialState, Options) ->
    #{pregel_graph := PregelGraph, config := Config} = Graph,

    %% 检查是否从 checkpoint 恢复
    RestoreOpts = maps:get(restore_from, Options, undefined),
    {ActualInitialState, PregelRestoreOpts, StartIteration} =
        prepare_restore_options(RestoreOpts, InitialState, PregelGraph),

    %% 注入初始状态到 __start__ 顶点
    PregelGraphWithState = graph_compute:inject_initial_state(PregelGraph, ActualInitialState),

    %% 准备执行选项
    MaxIterations = maps:get(max_iterations, Config, 100),
    PregelOpts0 = #{
        max_supersteps => maps:get(max_supersteps, Options, MaxIterations),
        num_workers => maps:get(workers, Options, 1)
    },
    %% 如果有恢复选项，添加到 PregelOpts
    PregelOpts = case PregelRestoreOpts of
        undefined -> PregelOpts0;
        _ -> PregelOpts0#{restore_from => PregelRestoreOpts}
    end,

    %% 使用步进式 API 执行
    ComputeFn = graph_compute:compute_fn(),
    {ok, Master} = pregel:start(PregelGraphWithState, ComputeFn, PregelOpts),
    try
        CheckpointCallback = maps:get(on_checkpoint, Options, fun default_checkpoint_callback/2),
        run_checkpoint_loop(Master, CheckpointCallback, StartIteration, Options)
    after
        pregel:stop(Master)
    end.

%% @private 准备恢复选项
%% 返回：{InitialState, PregelRestoreOpts, StartIteration}
%% 注意：恢复时使用原始 InitialState，实际状态从 pregel vertices 恢复
%% resume_data 会被转换为消息注入到对应顶点
-spec prepare_restore_options(restore_options() | undefined, state(), pregel:graph()) ->
    {state(), pregel:restore_opts() | undefined, non_neg_integer()}.
prepare_restore_options(undefined, InitialState, _PregelGraph) ->
    {InitialState, undefined, 0};
prepare_restore_options(RestoreOpts, InitialState, _PregelGraph) ->
    %% 从恢复选项中提取数据
    PregelCheckpoint = maps:get(pregel_checkpoint, RestoreOpts),
    Iteration = maps:get(iteration, RestoreOpts, 0),
    ResumeData = maps:get(resume_data, RestoreOpts, #{}),

    %% 构建 pregel restore_opts
    %% 状态从 vertices 中恢复，不需要单独的 graph_state
    #{superstep := Superstep, vertices := Vertices, pending_messages := PendingMessages} = PregelCheckpoint,

    %% 将 resume_data 转换为消息并合并到 pending_messages
    %% resume_data: #{vertex_id() => term()} -> [{vertex_id(), {resume, term()}}]
    ResumeMessages = maps:fold(
        fun(VertexId, Data, Acc) ->
            [{VertexId, {resume, Data}} | Acc]
        end,
        [],
        ResumeData
    ),
    AllMessages = ResumeMessages ++ PendingMessages,

    PregelRestoreOpts = #{
        superstep => Superstep,
        vertices => Vertices,
        messages => AllMessages
    },

    {InitialState, PregelRestoreOpts, Iteration}.

%% @private 默认 checkpoint 回调（不做任何事）
-spec default_checkpoint_callback(pregel:superstep_info(), checkpoint_data()) -> continue.
default_checkpoint_callback(_Info, _CheckpointData) ->
    continue.

%% @private Checkpoint 执行循环
%% 注意：状态存储在 pregel 顶点中，不在参数中传递
-spec run_checkpoint_loop(pid(), checkpoint_callback(), non_neg_integer(), run_options()) ->
    run_result().
run_checkpoint_loop(Master, CheckpointCallback, Iteration, Options) ->
    RunId = maps:get(run_id, Options, generate_run_id()),
    case pregel:step(Master) of
        {continue, Info} ->
            %% 1. 总是先获取 checkpoint（超步已完成或初始化完成）
            PregelCheckpoint = pregel:get_checkpoint_data(Master),
            Type = maps:get(type, Info),
            Superstep = maps:get(superstep, Info, 0),

            %% 提取顶点信息
            Vertices = maps:get(vertices, PregelCheckpoint, #{}),
            {ActiveVertices, CompletedVertices} = classify_vertices(Vertices),

            CheckpointData = #{
                type => Type,
                pregel_checkpoint => PregelCheckpoint,
                iteration => Iteration,
                %% 新增字段
                run_id => RunId,
                active_vertices => ActiveVertices,
                completed_vertices => CompletedVertices,
                superstep => Superstep
            },

            %% 2. 调用 checkpoint 回调
            CallbackResult = CheckpointCallback(Info, CheckpointData),

            %% 3. 根据类型和回调结果决定下一步
            handle_checkpoint_continue(
                Type, CallbackResult, Master, CheckpointCallback,
                CheckpointData, Info, Iteration, Options);

        {done, Reason, Info} ->
            handle_checkpoint_done(Master, Reason, Info, Iteration, CheckpointCallback, Options)
    end.

%% @private 处理 continue 分支的不同 checkpoint 类型
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

%% initial 类型：只允许 continue 或 stop
handle_checkpoint_continue(initial, continue, Master, Callback, _Data, _Info, Iteration, Options) ->
    %% initial 不增加 iteration，因为还没执行真正的超步
    run_checkpoint_loop(Master, Callback, Iteration, Options);
handle_checkpoint_continue(initial, {stop, Reason}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    build_stopped_result(Data, Reason, Iteration);
handle_checkpoint_continue(initial, {retry, _}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    %% initial 类型不支持 retry
    build_error_result_from_checkpoint(Data, {invalid_operation, {retry_not_allowed, initial}}, Iteration);

%% step 类型：只允许 continue 或 stop
handle_checkpoint_continue(step, continue, Master, Callback, _Data, _Info, Iteration, Options) ->
    run_checkpoint_loop(Master, Callback, Iteration + 1, Options);
handle_checkpoint_continue(step, {stop, Reason}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    build_stopped_result(Data, Reason, Iteration);
handle_checkpoint_continue(step, {retry, _}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    %% step 类型不支持 retry（没有失败的顶点）
    build_error_result_from_checkpoint(Data, {invalid_operation, {retry_not_allowed, step}}, Iteration);

%% error 类型：支持 continue、stop、retry
handle_checkpoint_continue(error, continue, Master, Callback, _Data, _Info, Iteration, Options) ->
    %% 忽略错误，继续执行
    run_checkpoint_loop(Master, Callback, Iteration + 1, Options);
handle_checkpoint_continue(error, {stop, Reason}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    build_stopped_result(Data, Reason, Iteration);
handle_checkpoint_continue(error, {retry, VertexIds}, Master, Callback, _Data, _Info, Iteration, Options) ->
    RunId = maps:get(run_id, Options, generate_run_id()),
    %% 重试指定顶点
    case pregel:retry(Master, VertexIds) of
        {continue, NewInfo} ->
            %% 重试后继续，获取新的 checkpoint
            NewPregelCheckpoint = pregel:get_checkpoint_data(Master),
            NewType = maps:get(type, NewInfo),
            NewSuperstep = maps:get(superstep, NewInfo, 0),

            %% 提取顶点信息
            NewVertices = maps:get(vertices, NewPregelCheckpoint, #{}),
            {NewActiveVertices, NewCompletedVertices} = classify_vertices(NewVertices),

            NewCheckpointData = #{
                type => NewType,
                pregel_checkpoint => NewPregelCheckpoint,
                iteration => Iteration,
                %% 新增字段
                run_id => RunId,
                active_vertices => NewActiveVertices,
                completed_vertices => NewCompletedVertices,
                superstep => NewSuperstep
            },
            %% 递归调用处理新的状态（不增加 iteration）
            NewCallbackResult = Callback(NewInfo, NewCheckpointData),
            handle_checkpoint_continue(
                NewType, NewCallbackResult, Master, Callback,
                NewCheckpointData, NewInfo, Iteration, Options);
        {done, Reason, Info} ->
            handle_checkpoint_done(Master, Reason, Info, Iteration, Callback, Options)
    end;

%% interrupt 类型：只允许 continue 或 stop（异步恢复通过 restore_from + resume_data）
handle_checkpoint_continue(interrupt, continue, Master, Callback, _Data, _Info, Iteration, Options) ->
    %% 忽略中断，继续执行（不推荐，通常应该 stop）
    run_checkpoint_loop(Master, Callback, Iteration + 1, Options);
handle_checkpoint_continue(interrupt, {stop, Reason}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    build_stopped_result(Data, Reason, Iteration);
handle_checkpoint_continue(interrupt, {retry, _}, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    %% interrupt 类型不支持 retry（中断不是错误）
    build_error_result_from_checkpoint(Data, {invalid_operation, {retry_not_allowed, interrupt}}, Iteration);

%% final 类型：不应该出现在 continue 分支，但为完整性处理
handle_checkpoint_continue(final, _, _Master, _Callback, Data, _Info, Iteration, _Options) ->
    build_error_result_from_checkpoint(Data, {invalid_state, final_in_continue}, Iteration).

%% @private 处理 done 分支
%% 注意：done 时也需要调用 checkpoint 回调，让上层有机会保存最终状态
-spec handle_checkpoint_done(pid(), pregel:done_reason(), pregel:superstep_info(),
                             non_neg_integer(), checkpoint_callback(), run_options()) -> run_result().
handle_checkpoint_done(Master, Reason, Info, Iteration, CheckpointCallback, Options) ->
    RunId = maps:get(run_id, Options, generate_run_id()),
    Superstep = maps:get(superstep, Info, 0),

    %% 1. 获取最终的 checkpoint 数据
    PregelCheckpoint = pregel:get_checkpoint_data(Master),

    %% 提取顶点信息
    Vertices = maps:get(vertices, PregelCheckpoint, #{}),
    {ActiveVertices, CompletedVertices} = classify_vertices(Vertices),

    CheckpointData = #{
        type => final,
        pregel_checkpoint => PregelCheckpoint,
        iteration => Iteration,
        %% 新增字段
        run_id => RunId,
        active_vertices => ActiveVertices,
        completed_vertices => CompletedVertices,
        superstep => Superstep
    },

    %% 2. 调用 checkpoint 回调，让上层有机会保存最终状态
    %% final 类型时，回调返回值被忽略（已经完成，无法继续或重试）
    _ = CheckpointCallback(Info#{type => final}, CheckpointData),

    %% 3. 获取执行结果
    Result = pregel:get_result(Master),
    PregelResult = graph_compute:from_pregel_result(Result),
    FinalResult = handle_pregel_result(PregelResult, graph_state:new(), Options),
    FinalResult#{
        iterations => Iteration,
        done_reason => Reason,
        checkpoint => PregelCheckpoint  %% 返回最终 checkpoint 以便查看
    }.

%% @private 构建 stopped 结果
-spec build_stopped_result(checkpoint_data(), term(), non_neg_integer()) -> run_result().
build_stopped_result(CheckpointData, Reason, Iteration) ->
    PregelCheckpoint = maps:get(pregel_checkpoint, CheckpointData),
    Type = maps:get(type, CheckpointData),
    CurrentState = extract_current_state_from_checkpoint(PregelCheckpoint),
    #{
        status => stopped,
        final_state => CurrentState,
        iterations => Iteration,
        error => {user_stopped, Reason},
        checkpoint => PregelCheckpoint,
        checkpoint_type => Type
    }.

%% @private 从 checkpoint 构建错误结果
-spec build_error_result_from_checkpoint(checkpoint_data(), term(), non_neg_integer()) -> run_result().
build_error_result_from_checkpoint(CheckpointData, Reason, Iteration) ->
    PregelCheckpoint = maps:get(pregel_checkpoint, CheckpointData),
    Type = maps:get(type, CheckpointData),
    CurrentState = extract_current_state_from_checkpoint(PregelCheckpoint),
    #{
        status => error,
        final_state => CurrentState,
        iterations => Iteration,
        error => Reason,
        checkpoint => PregelCheckpoint,
        checkpoint_type => Type
    }.

%% @private 从 checkpoint 数据中提取当前状态
%% 优先从 __end__ 顶点提取，否则合并所有有结果的顶点状态
-spec extract_current_state_from_checkpoint(pregel:checkpoint_data()) -> state().
extract_current_state_from_checkpoint(#{vertices := Vertices}) ->
    %% 优先检查 __end__ 顶点
    case maps:get('__end__', Vertices, undefined) of
        undefined ->
            extract_state_from_vertices(Vertices);
        EndVertex ->
            case maps:get(result, pregel_vertex:value(EndVertex), undefined) of
                {ok, State} -> State;
                _ -> extract_state_from_vertices(Vertices)
            end
    end.

%% @private 从多个顶点中提取并合并状态
-spec extract_state_from_vertices(#{term() => pregel_vertex:vertex()}) -> state().
extract_state_from_vertices(Vertices) ->
    %% 收集所有有结果的顶点状态（排除 __start__）
    States = maps:fold(
        fun('__start__', _V, Acc) -> Acc;
           (_Id, V, Acc) ->
                case maps:get(result, pregel_vertex:value(V), undefined) of
                    {ok, State} -> [State | Acc];
                    _ -> Acc
                end
        end,
        [],
        Vertices
    ),
    %% 合并所有状态
    case States of
        [] -> graph_state:new();
        [Single] -> Single;
        Multiple -> lists:foldl(fun graph_state:merge/2, graph_state:new(), Multiple)
    end.

%% @private 处理 Pregel 引擎执行结果
-spec handle_pregel_result({ok, state()} | {error, term()}, state(), run_options()) -> run_result().
handle_pregel_result({ok, FinalState}, _InitialState, _Options) ->
    #{status => completed, final_state => FinalState, iterations => 0};
handle_pregel_result({error, {partial_result, PartialState, Reason}}, _InitialState, _Options) ->
    #{status => error, final_state => PartialState, iterations => 0, error => Reason};
handle_pregel_result({error, max_iterations_exceeded}, InitialState, Options) ->
    MaxIter = maps:get(max_iterations, Options, 100),
    #{status => max_iterations, final_state => InitialState, iterations => MaxIter, error => max_iterations_exceeded};
handle_pregel_result({error, Reason}, InitialState, _Options) ->
    #{status => error, final_state => InitialState, iterations => 0, error => Reason}.

%% @doc 执行单步，返回新上下文
-spec step(graph(), execution_context()) -> {ok, execution_context()} | {done, run_result()}.
step(_Graph, #{current_node := '__end__'} = Context) ->
    {done, build_result(completed, Context)};
step(_Graph, #{iteration := Iter, options := #{max_iterations := Max}} = Context)
  when Iter >= Max ->
    {done, build_result(max_iterations, Context)};
step(Graph, Context) ->
    execute_step(Graph, Context).

%%====================================================================
%% 流式执行
%%====================================================================

%% @doc 流式执行，每步产生状态
-spec stream(graph(), state()) -> fun(() -> stream_result()).
stream(Graph, InitialState) ->
    stream(Graph, InitialState, #{}).

%% @doc 流式执行，带选项
-spec stream(graph(), state(), run_options()) -> fun(() -> stream_result()).
stream(Graph, InitialState, Options) ->
    Context = init_context(Graph, InitialState, Options),
    fun() -> stream_next(Graph, Context) end.

-type stream_result() :: {yield, state(), fun(() -> stream_result())} | {done, run_result()}.

%% @doc 获取下一个流式结果
-spec stream_next(graph(), execution_context()) -> stream_result().
stream_next(Graph, Context) ->
    case step(Graph, Context) of
        {ok, NewContext} ->
            State = maps:get(state, NewContext),
            {yield, State, fun() -> stream_next(Graph, NewContext) end};
        {done, Result} ->
            {done, Result}
    end.

%%====================================================================
%% 内部: 上下文管理
%%====================================================================

%% @doc 初始化执行上下文
-spec init_context(graph(), state(), run_options()) -> execution_context().
init_context(Graph, InitialState, Options) ->
    MergedOptions = merge_options(Graph, Options),
    #{
        graph => Graph,
        current_node => '__start__',
        state => InitialState,
        iteration => 0,
        trace => [],
        options => MergedOptions
    }.

%% @doc 合并选项与图配置
-spec merge_options(graph(), run_options()) -> run_options().
merge_options(#{config := Config}, Options) ->
    DefaultOptions = #{
        trace => false,
        max_iterations => maps:get(max_iterations, Config, 100),
        timeout => maps:get(timeout, Config, 30000)
    },
    maps:merge(DefaultOptions, Options).

%%====================================================================
%% 内部: 单步执行 (用于 stream/step)
%%====================================================================

%% @doc 执行单个超步
-spec execute_step(graph(), execution_context()) -> {ok, execution_context()} | {done, run_result()}.
execute_step(Graph, Context) ->
    #{current_node := NodeId, state := State, iteration := Iter} = Context,
    case execute_node(Graph, NodeId, State) of
        {ok, NewState} ->
            route_to_next(Graph, NodeId, NewState, Context, Iter);
        {error, Reason} ->
            {done, build_error_result(Reason, Context)}
    end.

%% @doc 执行单个节点
-spec execute_node(graph(), node_id(), state()) -> {ok, state()} | {error, term()}.
execute_node(#{nodes := Nodes}, NodeId, State) ->
    case maps:find(NodeId, Nodes) of
        {ok, Node} ->
            graph_node:execute(Node, State);
        error ->
            {error, {node_not_found, NodeId}}
    end.

%% @doc 路由到下一个节点
-spec route_to_next(graph(), node_id(), state(), execution_context(), non_neg_integer()) ->
    {ok, execution_context()}.
route_to_next(Graph, NodeId, NewState, Context, Iter) ->
    {ok, NextNode} = find_next_node(Graph, NodeId, NewState),
    NewContext = update_context(Context, NodeId, NewState, NextNode, Iter),
    {ok, NewContext}.

%% @doc 查找下一个节点
-spec find_next_node(graph(), node_id(), state()) -> {ok, node_id()}.
find_next_node(#{edges := EdgeMap}, NodeId, State) ->
    case maps:find(NodeId, EdgeMap) of
        {ok, Edges} ->
            resolve_edges(Edges, State);
        error ->
            %% 无出边则隐式终止
            {ok, '__end__'}
    end.

%% @doc 解析边，确定下一节点
%% 如果所有边都失败，返回 '__end__'
-spec resolve_edges([graph_edge:edge()], state()) -> {ok, node_id()}.
resolve_edges([], _State) ->
    {ok, '__end__'};
resolve_edges([Edge | Rest], State) ->
    case graph_edge:resolve(Edge, State) of
        {ok, NextNode} when is_atom(NextNode) ->
            {ok, NextNode};
        {ok, NextNodes} when is_list(NextNodes) ->
            %% 并行执行时取第一个 (简化处理)
            {ok, hd(NextNodes)};
        {error, _} ->
            %% 尝试下一条边
            resolve_edges(Rest, State)
    end.

%%====================================================================
%% 内部: 上下文更新
%%====================================================================

%% @doc 成功执行后更新上下文
-spec update_context(execution_context(), node_id(), state(), node_id(), non_neg_integer()) ->
    execution_context().
update_context(Context, NodeId, NewState, NextNode, Iter) ->
    TraceEntry = maybe_trace(Context, NodeId, NewState, NextNode),
    Context#{
        current_node => NextNode,
        state => NewState,
        iteration => Iter + 1,
        trace => TraceEntry
    }.

%% @doc 若启用追踪则添加追踪条目
-spec maybe_trace(execution_context(), node_id(), state(), node_id()) -> [trace_entry()].
maybe_trace(#{options := #{trace := true}, trace := Trace, state := OldState, iteration := Iter},
            NodeId, NewState, NextNode) ->
    Entry = #{
        superstep => Iter,
        node => NodeId,
        state_before => OldState,
        state_after => NewState,
        next_node => NextNode
    },
    [Entry | Trace];
maybe_trace(#{trace := Trace}, _NodeId, _NewState, _NextNode) ->
    Trace.

%%====================================================================
%% 内部: 结果构建
%%====================================================================

%% @doc 构建成功结果
-spec build_result(completed | max_iterations, execution_context()) -> run_result().
build_result(Status, #{state := State, iteration := Iter, trace := Trace, options := #{trace := TraceEnabled}}) ->
    BaseResult = #{
        status => Status,
        final_state => State,
        iterations => Iter
    },
    maybe_add_trace(BaseResult, Trace, TraceEnabled).

%% @doc 构建错误结果
-spec build_error_result(term(), execution_context()) -> run_result().
build_error_result(Reason, #{state := State, iteration := Iter, trace := Trace, options := #{trace := TraceEnabled}}) ->
    BaseResult = #{
        status => error,
        final_state => State,
        iterations => Iter,
        error => Reason
    },
    maybe_add_trace(BaseResult, Trace, TraceEnabled).

%% @doc 若启用追踪则添加到结果
-spec maybe_add_trace(run_result(), [trace_entry()], boolean()) -> run_result().
maybe_add_trace(Result, Trace, true) ->
    Result#{trace => lists:reverse(Trace)};
maybe_add_trace(Result, _Trace, false) ->
    Result.

%%====================================================================
%% 内部: 顶点分类和 ID 生成
%%====================================================================

%% @private 分类顶点状态
%% 返回 {ActiveVertices, CompletedVertices}
-spec classify_vertices(#{atom() => pregel_vertex:vertex()}) ->
    {[atom()], [atom()]}.
classify_vertices(Vertices) ->
    maps:fold(
        fun(Id, Vertex, {Active, Completed}) ->
            case pregel_vertex:is_active(Vertex) of
                true -> {[Id | Active], Completed};
                false -> {Active, [Id | Completed]}
            end
        end,
        {[], []},
        Vertices
    ).

%% @private 生成执行 ID
-spec generate_run_id() -> binary().
generate_run_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                   [A, B, C, D, E])).
