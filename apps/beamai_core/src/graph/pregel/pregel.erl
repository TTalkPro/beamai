%%%-------------------------------------------------------------------
%%% @doc Pregel 主入口 API 模块
%%%
%%% 提供统一的 API 接口:
%%% - 图构建: new_graph, add_vertex, add_edge, from_edges
%%% - Pregel 执行: run
%%% - 计算上下文: get_*, set_*, send_*, vote_to_halt
%%% - 结果查询: get_result_*, vertex_values, vertex_value
%%%
%%% 使用示例:
%%% <pre>
%%% %% 构建图
%%% G0 = pregel:new_graph(),
%%% G1 = pregel:add_vertex(G0, a, 1.0),
%%% G2 = pregel:add_vertex(G1, b, 2.0),
%%% G3 = pregel:add_edge(G2, a, b),
%%%
%%% %% 定义计算函数
%%% ComputeFn = fun(Ctx) ->
%%%     Value = pregel:get_vertex_value(Ctx),
%%%     pregel:send_to_all_neighbors(Ctx, Value)
%%% end,
%%%
%%% %% 执行
%%% Result = pregel:run(G3, ComputeFn, #{max_supersteps => 10}).
%%% </pre>
%%%
%%% 设计模式: 门面模式（Facade）
%%% @end
%%%-------------------------------------------------------------------
-module(pregel).

%% 图构建
-export([new_graph/0, new_graph/1]).
-export([add_vertex/2, add_vertex/3, add_vertex/4]).
-export([add_edge/3, add_edge/4]).
-export([from_edges/1, from_edges/2]).

%% Pregel 执行
-export([run/2, run/3]).

%% 计算上下文 - 读取
-export([get_vertex/1, get_vertex_id/1, get_vertex_value/1]).
-export([get_messages/1, get_superstep/1]).
-export([get_neighbors/1, get_edges/1, get_num_vertices/1]).

%% 计算上下文 - 修改
-export([set_value/2, vote_to_halt/1]).
-export([send_message/3, send_to_all_neighbors/2, send_to_edges/2]).

%% 结果查询
-export([get_result_graph/1, get_result_status/1]).
-export([vertex_values/1, vertex_value/2]).

%% 图操作委托
-export([get_graph_vertex/2, vertices/1, vertex_count/1, map_vertices/2]).

%% 类型导出
-export_type([graph/0, vertex/0, compute_fn/0, context/0, opts/0, result/0]).

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

%% @doc 添加顶点（带值）
-spec add_vertex(graph(), vertex_id(), term()) -> graph().
add_vertex(Graph, Id, Value) ->
    pregel_graph:add_vertex(Graph, Id, Value).

%% @doc 添加顶点（带值和边）
-spec add_vertex(graph(), vertex_id(), term(), [edge()]) -> graph().
add_vertex(Graph, Id, Value, Edges) ->
    pregel_graph:add_vertex(Graph, Id, Value, Edges).

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
%% Pregel 执行 API
%%====================================================================

%% @doc 执行 Pregel 计算（使用默认选项）
-spec run(graph(), compute_fn()) -> result().
run(Graph, ComputeFn) ->
    run(Graph, ComputeFn, #{}).

%% @doc 执行 Pregel 计算（带选项）
%% 可用选项:
%% - combiner: 消息合并器（sum, min, max 等）
%% - max_supersteps: 最大超步数（默认100）
%% - num_workers: Worker 数量（默认CPU核心数）
-spec run(graph(), compute_fn(), opts()) -> result().
run(Graph, ComputeFn, Opts) ->
    pregel_master:run(Graph, ComputeFn, Opts).

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

%% @doc 获取当前顶点值
-spec get_vertex_value(context()) -> term().
get_vertex_value(#{vertex := Vertex}) ->
    pregel_vertex:value(Vertex).

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

%% @doc 设置顶点值
-spec set_value(context(), term()) -> context().
set_value(#{vertex := Vertex} = Ctx, Value) ->
    Ctx#{vertex => pregel_vertex:set_value(Vertex, Value)}.

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

%% @doc 获取所有顶点值映射
-spec vertex_values(result() | graph()) -> #{vertex_id() => term()}.
vertex_values(#{graph := Graph}) ->
    vertex_values(Graph);
vertex_values(Graph) ->
    maps:from_list([
        {pregel_vertex:id(V), pregel_vertex:value(V)}
        || V <- pregel_graph:vertices(Graph)
    ]).

%% @doc 获取指定顶点的值
-spec vertex_value(result() | graph(), vertex_id()) -> term() | undefined.
vertex_value(#{graph := Graph}, VertexId) ->
    vertex_value(Graph, VertexId);
vertex_value(Graph, VertexId) ->
    case pregel_graph:get(Graph, VertexId) of
        undefined -> undefined;
        Vertex -> pregel_vertex:value(Vertex)
    end.

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
