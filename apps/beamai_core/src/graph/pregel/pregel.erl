%%%-------------------------------------------------------------------
%%% @doc Pregel 主入口 API 模块（全局状态模式）
%%%
%%% 提供统一的 API 接口:
%%% - 图构建: new_graph, add_vertex, add_edge, from_edges
%%% - Pregel 执行: run, start, step, get_result, stop
%%% - 结果查询: get_result_graph, get_result_status, get_result_global_state
%%%
%%% 全局状态模式说明：
%%% - Master 持有 global_state，Worker 是纯计算单元
%%% - 计算函数返回 #{delta => Map, outbox => List, status => ok}
%%% - delta 是增量更新，通过 field_reducers 合并到 global_state
%%% - 顶点只是拓扑结构（id + edges），不含 value
%%%
%%% 使用示例:
%%% <pre>
%%% %% 构建图（顶点只包含拓扑结构）
%%% G0 = pregel:new_graph(),
%%% G1 = pregel:add_vertex(G0, a, []),
%%% G2 = pregel:add_vertex(G1, b, []),
%%% G3 = pregel:add_edge(G2, a, b),
%%%
%%% %% 定义计算函数（返回 delta + outbox）
%%% ComputeFn = fun(Ctx) ->
%%%     #{vertex_id := Id, global_state := State, vertex := Vertex} = Ctx,
%%%     Value = graph_state:get(State, Id, 0),
%%%     Neighbors = pregel_vertex:neighbors(Vertex),
%%%     Outbox = [{N, Value} || N <- Neighbors],
%%%     Delta = #{Id => Value + 1},
%%%     #{delta => Delta, outbox => Outbox, status => ok}
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
-export([new_graph/0, new_graph/1]).
-export([add_vertex/2, add_vertex/3]).
-export([add_edge/3, add_edge/4]).
-export([from_edges/1, from_edges/2]).

%% Pregel 执行 - 步进式 API
-export([start/3, step/1, retry/2, get_checkpoint_data/1, get_result/1, stop/1]).
%% Pregel 执行 - 简化 API（内部使用步进式）
-export([run/2, run/3]).

%% 计算上下文 - 读取
-export([get_vertex/1, get_vertex_id/1, get_vertex_value/1]).
-export([get_messages/1, get_superstep/1]).
-export([get_neighbors/1, get_edges/1, get_num_vertices/1]).

%% 计算上下文 - 修改
-export([set_value/2, vote_to_halt/1]).
-export([send_message/3, send_to_all_neighbors/2, send_to_edges/2]).

%% 结果查询
-export([get_result_graph/1, get_result_status/1, get_result_global_state/1]).
-export([vertex_values/1, vertex_value/2]).

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

%% @doc 创建带配置的图
-spec new_graph(pregel_graph:config()) -> graph().
new_graph(Config) ->
    pregel_graph:new(Config).

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
-spec run_loop(pid()) -> result().
run_loop(Master) ->
    case step(Master) of
        {continue, _Info} ->
            run_loop(Master);
        {done, _Reason, _Info} ->
            get_result(Master)
    end.

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

%% @doc 获取当前顶点值 (已废弃 - 全局状态模式下使用 global_state)
%% 保留此函数用于向后兼容，但返回 undefined
-spec get_vertex_value(context()) -> term().
get_vertex_value(_Ctx) ->
    undefined.

%% @doc 获取收到的消息列表
-spec get_messages(context()) -> [term()].
get_messages(#{messages := Messages}) -> Messages.

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

%% @doc 设置顶点值 (已废弃 - 全局状态模式下使用 delta)
%% 保留此函数用于向后兼容，但不做任何操作
-spec set_value(context(), term()) -> context().
set_value(Ctx, _Value) ->
    Ctx.

%% @doc 投票停止（标记顶点为非活跃）
-spec vote_to_halt(context()) -> context().
vote_to_halt(#{vertex := Vertex} = Ctx) ->
    Ctx#{vertex => pregel_vertex:halt(Vertex)}.

%% @doc 发送消息给指定顶点
-spec send_message(context(), vertex_id(), term()) -> context().
send_message(#{outbox := Outbox} = Ctx, Target, Value) ->
    Ctx#{outbox => [{Target, Value} | Outbox]}.

%% @doc 向所有邻居发送相同消息
-spec send_to_all_neighbors(context(), term()) -> context().
send_to_all_neighbors(Ctx, Value) ->
    lists:foldl(
        fun(Neighbor, AccCtx) -> send_message(AccCtx, Neighbor, Value) end,
        Ctx,
        get_neighbors(Ctx)
    ).

%% @doc 向所有出边发送消息（基于边信息）
%% EdgeFn: fun(Edge) -> MessageValue
-spec send_to_edges(context(), fun((edge()) -> term())) -> context().
send_to_edges(Ctx, EdgeFn) ->
    lists:foldl(
        fun(#{target := Target} = Edge, AccCtx) ->
            send_message(AccCtx, Target, EdgeFn(Edge))
        end,
        Ctx,
        get_edges(Ctx)
    ).

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

%% @doc 获取所有顶点值映射 (已废弃 - 全局状态模式下使用 global_state)
%% 保留此函数用于向后兼容，但返回空 map
-spec vertex_values(result() | graph()) -> #{vertex_id() => term()}.
vertex_values(#{global_state := GlobalState}) ->
    GlobalState;
vertex_values(#{graph := _Graph}) ->
    #{};
vertex_values(_Graph) ->
    #{}.

%% @doc 获取指定顶点的值 (已废弃 - 全局状态模式下使用 global_state)
%% 保留此函数用于向后兼容，但返回 undefined
-spec vertex_value(result() | graph(), vertex_id()) -> term() | undefined.
vertex_value(#{global_state := GlobalState}, Key) ->
    maps:get(Key, GlobalState, undefined);
vertex_value(_, _) ->
    undefined.

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
