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

%% 类型定义
-type graph() :: graph_builder:graph().
-type node_id() :: graph_node:node_id().
-type state() :: graph_state:state().

-type run_options() :: #{
    workers => pos_integer(),        %% Pregel worker 数量 (默认 1)
    trace => boolean(),              %% 启用执行追踪 (用于 stream/step)
    max_iterations => pos_integer(), %% 最大迭代次数
    timeout => pos_integer()         %% 超时时间 (毫秒)
}.

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
    status := completed | error | max_iterations,
    final_state := state(),
    iterations := non_neg_integer(),
    trace => [trace_entry()],
    error => term()
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
%% 直接使用预构建的 Pregel 图，无需运行时转换。
-spec run(graph(), state(), run_options()) -> run_result().
run(Graph, InitialState, Options) ->
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
