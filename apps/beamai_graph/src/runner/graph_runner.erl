%%%-------------------------------------------------------------------
%%% @doc 图执行引擎模块
%%%
%%% 使用 Pregel 分布式图计算引擎执行图。
%%%
%%% 全局状态模式：
%%% - Master 持有 global_state，Worker 只负责计算
%%% - 节点返回 delta（增量更新）而不是完整状态
%%% - 使用 field_reducers 按字段合并 delta
%%% - 支持延迟提交：出错时暂存 delta，不 apply
%%%
%%% 主要功能:
%%% - run/2,3: 批量执行，使用 Pregel BSP 模型
%%% - stream/2,3: 流式执行，逐步返回状态
%%% - step/2: 单步执行，用于调试和流式迭代
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
%% 全局状态模式：状态在 global_state 中，不在顶点 value 中
%%
%% Checkpoint 类型说明：
%% - initial: 超步 0 执行前的初始状态
%% - step: 正常超步完成
%% - error: 超步完成但有失败的顶点
%% - interrupt: 超步完成但有中断的顶点（human-in-the-loop）
%% - final: 执行结束
-type checkpoint_data() :: #{
    type := pregel:checkpoint_type(),              %% checkpoint 类型
    pregel_checkpoint := pregel:checkpoint_data(), %% pregel 层 checkpoint
    global_state := state(),                       %% 当前全局状态
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
%% 全局状态模式（无 inbox 版本）：状态从 global_state 恢复
%% resume_data 中的数据会被合并到 global_state 中
-type restore_options() :: #{
    pregel_checkpoint := pregel:checkpoint_data(),  %% pregel checkpoint 数据
    global_state => state(),                        %% 恢复时的全局状态
    iteration => non_neg_integer(),                 %% 迭代次数（可选）
    resume_data => #{pregel:vertex_id() => term()}  %% 恢复时注入的用户数据（合并到 global_state）
}.

-type run_options() :: #{
    workers => pos_integer(),        %% Pregel worker 数量 (默认 1)
    trace => boolean(),              %% 启用执行追踪 (用于 stream/step)
    max_iterations => pos_integer(), %% 最大迭代次数
    timeout => pos_integer(),        %% 超时时间 (毫秒)
    %% Checkpoint 相关选项
    on_checkpoint => checkpoint_callback(),  %% 每个超步完成后的回调
    restore_from => restore_options(),       %% 从 checkpoint 恢复
    run_id => binary(),                      %% 外部传入的执行 ID
    %% 全局状态选项
    global_state => state(),                 %% 初始全局状态
    field_reducers => pregel_master:field_reducers()   %% 字段级 Reducer 配置
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
%% 全局状态模式：
%% - global_state: 初始全局状态（如果未提供，使用 InitialState）
%% - field_reducers: 字段级 Reducer 配置
%%
%% 执行模式:
%% - 简单模式: 不提供 checkpoint 选项时，直接使用 pregel:run
%% - Checkpoint 模式: 提供 on_checkpoint 或 restore_from 时，使用步进式 API
-spec run(graph(), state(), run_options()) -> run_result().
run(Graph, InitialState, Options) ->
    %% 如果未提供 global_state，使用 InitialState
    OptionsWithState = ensure_global_state(Options, InitialState),
    case needs_checkpoint_mode(OptionsWithState) of
        true ->
            run_with_checkpoint(Graph, InitialState, OptionsWithState);
        false ->
            run_simple(Graph, InitialState, OptionsWithState)
    end.

%% @private 确保 Options 中有 global_state
-spec ensure_global_state(run_options(), state()) -> run_options().
ensure_global_state(Options, InitialState) ->
    case maps:is_key(global_state, Options) of
        true -> Options;
        false -> Options#{global_state => InitialState}
    end.

%% @private 检查是否需要 checkpoint 模式
-spec needs_checkpoint_mode(run_options()) -> boolean().
needs_checkpoint_mode(Options) ->
    maps:is_key(on_checkpoint, Options) orelse maps:is_key(restore_from, Options).

%% @private 简单执行模式（无 checkpoint）
%%
%% 节点计算逻辑和路由边已存储在顶点中（扁平化结构），无需额外配置
-spec run_simple(graph(), state(), run_options()) -> run_result().
run_simple(Graph, InitialState, Options) ->
    #{pregel_graph := PregelGraph} = Graph,

    %% 准备执行选项（max_iterations 直接从 Graph 获取）
    MaxIterations = maps:get(max_iterations, Graph, 100),
    GlobalState = maps:get(global_state, Options, InitialState),
    FieldReducers = maps:get(field_reducers, Options, #{}),

    PregelOpts = #{
        max_supersteps => maps:get(max_supersteps, Options, MaxIterations),
        num_workers => maps:get(workers, Options, 1),
        global_state => GlobalState,
        field_reducers => FieldReducers
    },

    %% 使用全局计算函数执行
    ComputeFn = graph_compute:compute_fn(),
    Result = pregel:run(PregelGraph, ComputeFn, PregelOpts),

    %% 提取结果
    PregelResult = graph_compute:from_pregel_result(Result),
    handle_pregel_result(PregelResult, InitialState, Options).

%% @private Checkpoint 执行模式（使用步进式 API）
%%
%% 委托给 graph_checkpoint 模块处理
-spec run_with_checkpoint(graph(), state(), run_options()) -> run_result().
run_with_checkpoint(Graph, InitialState, Options) ->
    ActualGlobalState = maps:get(global_state, Options, InitialState),
    graph_checkpoint:run_with_checkpoint(Graph, InitialState, ActualGlobalState, Options).

%% Checkpoint 相关函数已移至 graph_checkpoint 模块

%% @private 处理 Pregel 引擎执行结果
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
%%
%% 从简化的图结构直接获取 max_iterations
-spec merge_options(graph(), run_options()) -> run_options().
merge_options(Graph, Options) ->
    DefaultOptions = #{
        trace => false,
        max_iterations => maps:get(max_iterations, Graph, 100),
        timeout => 30000  %% timeout 使用默认值
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
        {command, Cmd} ->
            handle_command_step(Graph, NodeId, Cmd, State, Context, Iter);
        {error, Reason} ->
            {done, build_error_result(Reason, Context)}
    end.

%% @doc 执行单个节点
-spec execute_node(graph(), node_id(), state()) ->
    {ok, state()} | {command, graph_command:command()} | {error, term()}.
execute_node(#{nodes := Nodes}, NodeId, State) ->
    case maps:find(NodeId, Nodes) of
        {ok, Node} ->
            graph_node:execute(Node, State);
        error ->
            {error, {node_not_found, NodeId}}
    end.

%% @doc 处理 Command 模式的步进执行
%%
%% Command 的 update 直接应用为状态更新
%% Command 的 goto 覆盖边路由
-spec handle_command_step(graph(), node_id(), graph_command:command(),
                          state(), execution_context(), non_neg_integer()) ->
    {ok, execution_context()}.
handle_command_step(Graph, NodeId, Cmd, OldState, Context, Iter) ->
    Delta = graph_command:get_update(Cmd),
    NewState = graph_state:set_many(OldState, Delta),
    case graph_command:get_goto(Cmd) of
        undefined ->
            route_to_next(Graph, NodeId, NewState, Context, Iter);
        Target when is_atom(Target) ->
            NewContext = update_context(Context, NodeId, NewState, Target, Iter),
            {ok, NewContext};
        [First | _] when is_atom(First) ->
            %% 多节点并行：legacy runner 仅支持取第一个
            NewContext = update_context(Context, NodeId, NewState, First, Iter),
            {ok, NewContext};
        _ ->
            %% Dispatch 在 legacy runner 中不支持，回退到 __end__
            NewContext = update_context(Context, NodeId, NewState, '__end__', Iter),
            {ok, NewContext}
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
            {ok, '__end__'}
    end.

%% @doc 解析边，确定下一节点
-spec resolve_edges([graph_edge:edge()], state()) -> {ok, node_id()}.
resolve_edges([], _State) ->
    {ok, '__end__'};
resolve_edges([Edge | Rest], State) ->
    case graph_edge:resolve(Edge, State) of
        {ok, NextNode} when is_atom(NextNode) ->
            {ok, NextNode};
        {ok, NextNodes} when is_list(NextNodes) ->
            {ok, hd(NextNodes)};
        {error, _} ->
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

%% 顶点分类和 ID 生成已移至 graph_checkpoint 模块
