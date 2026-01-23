%%%-------------------------------------------------------------------
%%% @doc Pregel 主入口 API 模块（全局状态模式 - 无 inbox 版本）
%%%
%%% 提供统一的 API 接口:
%%% - 图构建: new_graph, add_vertex, add_edge, from_edges
%%% - Pregel 执行: run, start, step, get_result, stop
%%% - 结果查询: get_result_graph, get_result_status, get_result_global_state
%%%
%%% 全局状态模式（无 inbox 版本）说明：
%%% - Master 持有 global_state，Worker 是纯计算单元
%%% - 计算函数返回 #{delta => Map, activations => List, status => ok}
%%% - delta 是增量更新，通过 field_reducers 合并到 global_state
%%% - activations 是要激活的顶点ID列表（替代旧的 outbox 消息）
%%% - 顶点只是拓扑结构（id + edges），不含 value
%%% - 节点不再通过 inbox 接收消息，而是通过被激活来触发计算
%%%
%%% 使用示例:
%%% <pre>
%%% %% 构建图（顶点只包含拓扑结构）
%%% G0 = pregel:new_graph(),
%%% G1 = pregel:add_vertex(G0, a, []),
%%% G2 = pregel:add_vertex(G1, b, []),
%%% G3 = pregel:add_edge(G2, a, b),
%%%
%%% %% 定义计算函数（返回 delta + activations）
%%% ComputeFn = fun(Ctx) ->
%%%     #{vertex_id := Id, global_state := State} = Ctx,
%%%     Value = graph_state:get(State, Id, 0),
%%%     %% 激活邻居节点（只需ID列表，不传递数据）
%%%     Activations = [b],
%%%     Delta = #{Id => Value + 1},
%%%     #{delta => Delta, activations => Activations, status => ok}
%%% end,
%%%
%%% %% 执行（指定初始全局状态）
%%% Result = pregel:run(G3, ComputeFn, #{
%%%     max_supersteps => 10,
%%%     global_state => #{a => 1, b => 2}
%%% }),
%%% FinalState = pregel:get_result_global_state(Result).
%%% </pre>
%%%
%%% 设计模式: 门面模式（Facade）
%%% @end
%%%-------------------------------------------------------------------
-module(pregel).

%% 图构建
-export([new_graph/0]).
-export([add_vertex/2, add_vertex/3]).
-export([add_edge/3, add_edge/4]).
-export([from_edges/1, from_edges/2]).

%% Pregel 执行 - 步进式 API
-export([start/3, step/1, retry/2, get_checkpoint_data/1, get_result/1, stop/1]).
-export([get_global_state/1]).
%% Pregel 执行 - 简化 API（内部使用步进式）
-export([run/2, run/3]).

%% 计算上下文 - 读取
-export([get_vertex/1, get_vertex_id/1]).
-export([get_superstep/1]).
-export([get_neighbors/1, get_edges/1, get_num_vertices/1]).

%% 计算上下文 - 修改
-export([vote_to_halt/1]).

%% 结果查询
-export([get_result_graph/1, get_result_status/1, get_result_global_state/1]).

%% 图操作委托
-export([get_graph_vertex/2, vertices/1, vertex_count/1, map_vertices/2]).

%% 类型导出
-export_type([graph/0, vertex/0, compute_fn/0, context/0, opts/0, result/0]).
-export_type([step_result/0, superstep_info/0, checkpoint_data/0, checkpoint_type/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type graph() :: pregel_graph:graph().
-type vertex() :: pregel_vertex:vertex().
-type vertex_id() :: pregel_vertex:vertex_id().
-type edge() :: pregel_vertex:edge().

%% 计算上下文
-type context() :: pregel_worker:context().

%% 计算函数
-type compute_fn() :: fun((context()) -> context()).

%% 执行选项和结果
-type opts() :: pregel_master:opts().
-type result() :: pregel_master:result().
-type step_result() :: pregel_master:step_result().
-type superstep_info() :: pregel_master:superstep_info().
-type checkpoint_data() :: pregel_master:checkpoint_data().
-type checkpoint_type() :: pregel_master:checkpoint_type().

%%====================================================================
%% 图构建 API
%%====================================================================

%% @doc 创建空图
-spec new_graph() -> graph().
new_graph() ->
    pregel_graph:new().

%% @doc 添加顶点（仅ID）
-spec add_vertex(graph(), vertex_id()) -> graph().
add_vertex(Graph, Id) ->
    pregel_graph:add_vertex(Graph, Id).

%% @doc 添加顶点（带边）
%% 全局状态模式：顶点不再包含 value，第三个参数为 edges
-spec add_vertex(graph(), vertex_id(), [edge()]) -> graph().
add_vertex(Graph, Id, Edges) ->
    pregel_graph:add_vertex(Graph, Id, Edges).

%% @doc 添加边（权重默认为1）
-spec add_edge(graph(), vertex_id(), vertex_id()) -> graph().
add_edge(Graph, From, To) ->
    pregel_graph:add_edge(Graph, From, To).

%% @doc 添加带权重的边
-spec add_edge(graph(), vertex_id(), vertex_id(), number()) -> graph().
add_edge(Graph, From, To, Weight) ->
    pregel_graph:add_edge(Graph, From, To, Weight).

%% @doc 从边列表构建图
-spec from_edges([{vertex_id(), vertex_id()} |
                  {vertex_id(), vertex_id(), number()}]) -> graph().
from_edges(Edges) ->
    pregel_graph:from_edges(Edges).

%% @doc 从边列表和初始值构建图
-spec from_edges([{vertex_id(), vertex_id()} |
                  {vertex_id(), vertex_id(), number()}],
                 #{vertex_id() => term()}) -> graph().
from_edges(Edges, InitialValues) ->
    pregel_graph:from_edges(Edges, InitialValues).

%%====================================================================
%% Pregel 执行 API - 步进式
%%====================================================================

%% @doc 启动 Pregel 执行（返回 Master 进程）
-spec start(graph(), compute_fn(), opts()) -> {ok, pid()} | {error, term()}.
start(Graph, ComputeFn, Opts) ->
    pregel_master:start_link(Graph, ComputeFn, Opts).

%% @doc 执行单个超步
-spec step(pid()) -> step_result().
step(Master) ->
    pregel_master:step(Master).

%% @doc 重试指定顶点
-spec retry(pid(), [vertex_id()]) -> step_result().
retry(Master, VertexIds) ->
    pregel_master:retry(Master, VertexIds).

%% @doc 获取当前 checkpoint 数据
-spec get_checkpoint_data(pid()) -> checkpoint_data().
get_checkpoint_data(Master) ->
    pregel_master:get_checkpoint_data(Master).

%% @doc 获取最终结果（仅在终止后调用）
-spec get_result(pid()) -> result() | {error, not_halted}.
get_result(Master) ->
    pregel_master:get_result(Master).

%% @doc 停止 Pregel 执行
-spec stop(pid()) -> ok.
stop(Master) ->
    pregel_master:stop(Master).

%% @doc 获取当前全局状态（运行中）
-spec get_global_state(pid()) -> map().
get_global_state(Master) ->
    pregel_master:get_global_state(Master).

%%====================================================================
%% Pregel 执行 API - 简化（内部使用步进式）
%%====================================================================

%% @doc 执行 Pregel 计算（使用默认选项）
-spec run(graph(), compute_fn()) -> result().
run(Graph, ComputeFn) ->
    run(Graph, ComputeFn, #{}).

%% @doc 执行 Pregel 计算（带选项）
%% 可用选项:
%% - max_supersteps: 最大超步数（默认100）
%% - num_workers: Worker 数量（默认CPU核心数）
%% - state_reducer: 消息整合函数（默认 last_write_win）
-spec run(graph(), compute_fn(), opts()) -> result().
run(Graph, ComputeFn, Opts) ->
    {ok, Master} = start(Graph, ComputeFn, Opts),
    try
        run_loop(Master)
    after
        stop(Master)
    end.

%% @private 内部执行循环
%%
%% 无 checkpoint 回调时，遇到 error 或 interrupt 也应停止执行。
%% 调用方可通过结果中的 failed_vertices 字段获取详细信息。
-spec run_loop(pid()) -> result().
run_loop(Master) ->
    case step(Master) of
        {continue, #{type := error} = Info} ->
            %% 有节点失败，停止执行并构建部分结果
            build_early_termination_result(Master, Info, error);
        {continue, #{type := interrupt} = Info} ->
            %% 有节点中断（human-in-the-loop），停止执行
            build_early_termination_result(Master, Info, interrupt);
        {continue, _Info} ->
            run_loop(Master);
        {done, _Reason, _Info} ->
            get_result(Master)
    end.

%% @private 构建提前终止时的结果
-spec build_early_termination_result(pid(), superstep_info(), error | interrupt) -> result().
build_early_termination_result(Master, Info, _Reason) ->
    CheckpointData = get_checkpoint_data(Master),
    #{
        superstep := Superstep,
        global_state := GlobalState
    } = CheckpointData,
    #{
        status => completed,  %% 使用 completed 但携带 failed_vertices
        global_state => GlobalState,
        graph => #{vertices => #{}},  %% 简化图（run 不使用）
        supersteps => Superstep,
        stats => #{},
        failed_count => maps:get(failed_count, Info, 0),
        failed_vertices => maps:get(failed_vertices, Info, []),
        interrupted_count => maps:get(interrupted_count, Info, 0),
        interrupted_vertices => maps:get(interrupted_vertices, Info, [])
    }.

%%====================================================================
%% 计算上下文 - 读取 API
%%====================================================================

%% @doc 获取当前顶点
-spec get_vertex(context()) -> vertex().
get_vertex(#{vertex := Vertex}) -> Vertex.

%% @doc 获取当前顶点ID
-spec get_vertex_id(context()) -> vertex_id().
get_vertex_id(#{vertex := Vertex}) ->
    pregel_vertex:id(Vertex).

%% @doc 获取当前超步编号
-spec get_superstep(context()) -> non_neg_integer().
get_superstep(#{superstep := Superstep}) -> Superstep.

%% @doc 获取邻居ID列表
-spec get_neighbors(context()) -> [vertex_id()].
get_neighbors(#{vertex := Vertex}) ->
    pregel_vertex:neighbors(Vertex).

%% @doc 获取出边列表
-spec get_edges(context()) -> [edge()].
get_edges(#{vertex := Vertex}) ->
    pregel_vertex:edges(Vertex).

%% @doc 获取全局顶点数量
-spec get_num_vertices(context()) -> non_neg_integer().
get_num_vertices(#{num_vertices := N}) -> N.

%%====================================================================
%% 计算上下文 - 修改 API
%%====================================================================

%% @doc 投票停止（标记顶点为非活跃）
-spec vote_to_halt(context()) -> context().
vote_to_halt(#{vertex := Vertex} = Ctx) ->
    Ctx#{vertex => pregel_vertex:halt(Vertex)}.

%%====================================================================
%% 结果查询 API
%%====================================================================

%% @doc 获取结果图
-spec get_result_graph(result()) -> graph().
get_result_graph(#{graph := Graph}) -> Graph.

%% @doc 获取结果状态
-spec get_result_status(result()) -> completed | max_supersteps.
get_result_status(#{status := Status}) -> Status.

%% @doc 获取结果中的全局状态
-spec get_result_global_state(result()) -> map().
get_result_global_state(#{global_state := GlobalState}) -> GlobalState.

%%====================================================================
%% 图操作委托 API
%%====================================================================

%% @doc 获取图中的顶点
-spec get_graph_vertex(graph(), vertex_id()) -> vertex() | undefined.
get_graph_vertex(Graph, VertexId) ->
    pregel_graph:get(Graph, VertexId).

%% @doc 获取所有顶点列表
-spec vertices(graph()) -> [vertex()].
vertices(Graph) ->
    pregel_graph:vertices(Graph).

%% @doc 获取顶点数量
-spec vertex_count(graph()) -> non_neg_integer().
vertex_count(Graph) ->
    pregel_graph:size(Graph).

%% @doc 对所有顶点应用函数
-spec map_vertices(graph(), fun((vertex()) -> vertex())) -> graph().
map_vertices(Graph, Fun) ->
    pregel_graph:map(Graph, Fun).
