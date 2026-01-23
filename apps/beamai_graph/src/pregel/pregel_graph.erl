%%%-------------------------------------------------------------------
%%% @doc Pregel 图结构模块
%%%
%%% 本模块管理 Pregel 计算图的拓扑结构，包括顶点和边的 CRUD 操作。
%%% 图采用不可变数据结构设计，所有修改操作返回新图。
%%%
%%% 数据结构:
%%% - vertices: 顶点映射表 #{vertex_id() => vertex()}
%%%
%%% 设计模式: 组合模式 + 不可变数据结构
%%%
%%% 使用示例:
%%% ```
%%% %% 创建空图并添加顶点
%%% G0 = pregel_graph:new(),
%%% G1 = pregel_graph:add_vertex(G0, node_a),
%%% G2 = pregel_graph:add_vertex(G1, node_b),
%%%
%%% %% 添加边（node_a -> node_b）
%%% G3 = pregel_graph:add_edge(G2, node_a, node_b),
%%%
%%% %% 或者从边列表快速构建
%%% G = pregel_graph:from_edges([{a, b}, {b, c}, {c, a}]).
%%% ```
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_graph).

%% === 构造函数 ===
-export([new/0, from_edges/1, from_edges/2]).

%% === 顶点操作 ===
-export([add_vertex/2, add_vertex/3]).
-export([add_vertex_flat/5]).
-export([get/2, has/2, remove/2, update/3]).
-export([vertices/1, ids/1, size/1]).

%% === 边操作 ===
-export([add_edge/3, add_edge/4, remove_edge/3]).
-export([edges/2, neighbors/2]).

%% === 批量操作 ===
-export([map/2, filter/2, fold/3]).

%% === 状态查询 ===
-export([active_count/1, halted_count/1]).

%% === 类型导出 ===
-export_type([graph/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type vertex_id() :: pregel_vertex:vertex_id().
-type vertex() :: pregel_vertex:vertex().
-type edge() :: pregel_vertex:edge().

%% @type graph() :: #{vertices := #{vertex_id() => vertex()}}.
%% Pregel 图结构，仅包含顶点映射表。
%% 顶点映射表存储所有顶点及其出边信息。
-type graph() :: #{
    vertices := #{vertex_id() => vertex()}
}.

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建空图
%%
%% 返回一个不包含任何顶点的空图。
%% 这是构建图的起点，后续可通过 add_vertex/2,3 和 add_edge/3,4 添加内容。
-spec new() -> graph().
new() ->
    #{vertices => #{}}.

%% @doc 从边列表构建图
%%
%% 根据边列表自动创建所有相关顶点，并建立边关系。
%% 边格式支持两种:
%% - {From, To}: 权重默认为 1
%% - {From, To, Weight}: 指定权重
%%
%% 示例:
%% ```
%% G = pregel_graph:from_edges([{a, b}, {b, c, 2.0}, {c, a}]).
%% ```
-spec from_edges([{vertex_id(), vertex_id()} |
                  {vertex_id(), vertex_id(), number()}]) -> graph().
from_edges(Edges) ->
    from_edges(Edges, #{}).

%% @doc 从边列表和额外顶点ID构建图
%%
%% 除了边列表中的顶点外，还会创建 ExtraIds 中指定的顶点。
%% ExtraIds 的值会被忽略，仅使用其键作为顶点ID。
%%
%% 用途: 当图中存在孤立顶点（无出边也无入边）时使用。
-spec from_edges([{vertex_id(), vertex_id()} |
                  {vertex_id(), vertex_id(), number()}],
                 #{vertex_id() => term()}) -> graph().
from_edges(Edges, ExtraIds) ->
    %% 1. 收集所有顶点ID（边端点 + 额外ID）
    EdgeIds = collect_vertex_ids(Edges),
    InitIds = maps:keys(ExtraIds),
    AllIds = lists:usort(EdgeIds ++ InitIds),
    %% 2. 创建所有顶点
    Graph0 = create_vertices(AllIds),
    %% 3. 添加边关系
    add_edges(Graph0, Edges).

%%====================================================================
%% 顶点操作
%%====================================================================

%% @doc 添加顶点（仅ID，无出边）
%%
%% 创建一个新顶点，初始状态为活跃，无出边。
%% 如果顶点已存在，会被覆盖。
-spec add_vertex(graph(), vertex_id()) -> graph().
add_vertex(Graph, Id) ->
    add_vertex(Graph, Id, []).

%% @doc 添加顶点（带出边或值）
%%
%% 第三个参数的解释取决于其类型:
%% - 列表 [edge()]: 作为图拓扑边处理（向后兼容旧API）
%% - 其他类型: 作为顶点值存储（通常是 map，如 #{node => ..., edges => ...}）
%%
%% 顶点值用于存储计算逻辑（graph_node）和路由规则（graph_edge）。
-spec add_vertex(graph(), vertex_id(), [edge()] | term()) -> graph().
add_vertex(#{vertices := Vertices} = Graph, Id, EdgesOrValue) when is_list(EdgesOrValue) ->
    %% 列表参数 -> 图拓扑边
    Vertex = pregel_vertex:new(Id, EdgesOrValue),
    Graph#{vertices => Vertices#{Id => Vertex}};
add_vertex(#{vertices := Vertices} = Graph, Id, Value) ->
    %% 非列表参数 -> 顶点值
    Vertex = pregel_vertex:new(Id, [], Value),
    Graph#{vertices => Vertices#{Id => Vertex}}.

%% @doc 添加扁平化顶点（推荐用于 Graph 执行）
%%
%% 直接将计算函数、元数据和路由边存储在顶点上，无需嵌套 value 结构。
%% 这是 Graph 执行的推荐方式，简化了属性访问。
%%
%% 参数:
%% - Graph: 当前图
%% - Id: 顶点ID（通常与节点ID相同）
%% - Fun: 节点计算函数
%% - Metadata: 节点元数据 map
%% - RoutingEdges: 路由边列表（条件边）
-spec add_vertex_flat(graph(), vertex_id(), term(), map(), list()) -> graph().
add_vertex_flat(#{vertices := Vertices} = Graph, Id, Fun, Metadata, RoutingEdges) ->
    Vertex = pregel_vertex:new_flat(Id, Fun, Metadata, RoutingEdges),
    Graph#{vertices => Vertices#{Id => Vertex}}.

%% @doc 获取指定顶点
%%
%% 返回顶点或 undefined（如果不存在）。
-spec get(graph(), vertex_id()) -> vertex() | undefined.
get(#{vertices := Vertices}, Id) ->
    maps:get(Id, Vertices, undefined).

%% @doc 检查顶点是否存在
-spec has(graph(), vertex_id()) -> boolean().
has(#{vertices := Vertices}, Id) ->
    maps:is_key(Id, Vertices).

%% @doc 移除顶点
%%
%% 注意: 这只会移除顶点本身，不会移除其他顶点指向它的边。
%% 如需完全删除，还需手动清理相关边。
-spec remove(graph(), vertex_id()) -> graph().
remove(#{vertices := Vertices} = Graph, Id) ->
    Graph#{vertices => maps:remove(Id, Vertices)}.

%% @doc 更新顶点
%%
%% 用新顶点替换指定ID的顶点。
%% 常用于更新顶点状态（如标记为 halted）。
-spec update(graph(), vertex_id(), vertex()) -> graph().
update(#{vertices := Vertices} = Graph, Id, Vertex) ->
    Graph#{vertices => Vertices#{Id => Vertex}}.

%% @doc 获取所有顶点列表
-spec vertices(graph()) -> [vertex()].
vertices(#{vertices := Vertices}) ->
    maps:values(Vertices).

%% @doc 获取所有顶点ID列表
-spec ids(graph()) -> [vertex_id()].
ids(#{vertices := Vertices}) ->
    maps:keys(Vertices).

%% @doc 获取顶点数量
-spec size(graph()) -> non_neg_integer().
size(#{vertices := Vertices}) ->
    maps:size(Vertices).

%%====================================================================
%% 边操作
%%====================================================================

%% @doc 添加边（权重默认为1）
%%
%% 从 From 顶点添加一条指向 To 的出边。
%% 如果 From 顶点不存在，会自动创建。
-spec add_edge(graph(), vertex_id(), vertex_id()) -> graph().
add_edge(Graph, From, To) ->
    add_edge(Graph, From, To, 1).

%% @doc 添加带权重的边
%%
%% 权重可用于最短路径等算法。
%% 如果边已存在，权重会被更新。
-spec add_edge(graph(), vertex_id(), vertex_id(), number()) -> graph().
add_edge(#{vertices := Vertices} = Graph, From, To, Weight) ->
    %% 获取或创建源顶点
    Vertex = case maps:get(From, Vertices, undefined) of
        undefined -> pregel_vertex:new(From);
        V -> V
    end,
    %% 添加出边
    NewVertex = pregel_vertex:add_edge(Vertex, To, Weight),
    Graph#{vertices => Vertices#{From => NewVertex}}.

%% @doc 移除边
%%
%% 删除从 From 到 To 的出边。
%% 如果 From 顶点或边不存在，返回原图。
-spec remove_edge(graph(), vertex_id(), vertex_id()) -> graph().
remove_edge(#{vertices := Vertices} = Graph, From, To) ->
    case maps:get(From, Vertices, undefined) of
        undefined ->
            Graph;
        Vertex ->
            NewVertex = pregel_vertex:remove_edge(Vertex, To),
            Graph#{vertices => Vertices#{From => NewVertex}}
    end.

%% @doc 获取顶点的所有出边
%%
%% 返回边列表，每条边包含目标顶点ID和权重。
%% 如果顶点不存在，返回空列表。
-spec edges(graph(), vertex_id()) -> [edge()].
edges(Graph, Id) ->
    case get(Graph, Id) of
        undefined -> [];
        Vertex -> pregel_vertex:edges(Vertex)
    end.

%% @doc 获取顶点的所有邻居ID
%%
%% 返回所有出边指向的顶点ID列表。
%% 如果顶点不存在，返回空列表。
-spec neighbors(graph(), vertex_id()) -> [vertex_id()].
neighbors(Graph, Id) ->
    case get(Graph, Id) of
        undefined -> [];
        Vertex -> pregel_vertex:neighbors(Vertex)
    end.

%%====================================================================
%% 批量操作
%%====================================================================

%% @doc 对所有顶点应用变换函数
%%
%% 返回新图，其中每个顶点都经过 Fun 处理。
%% 常用于批量更新顶点状态。
-spec map(graph(), fun((vertex()) -> vertex())) -> graph().
map(#{vertices := Vertices} = Graph, Fun) ->
    Graph#{vertices => maps:map(fun(_Id, V) -> Fun(V) end, Vertices)}.

%% @doc 过滤顶点
%%
%% 保留满足谓词 Pred 的顶点，移除其他顶点。
-spec filter(graph(), fun((vertex()) -> boolean())) -> graph().
filter(#{vertices := Vertices} = Graph, Pred) ->
    Graph#{vertices => maps:filter(fun(_Id, V) -> Pred(V) end, Vertices)}.

%% @doc 折叠所有顶点
%%
%% 对所有顶点进行累积计算，类似 lists:foldl。
%% 常用于统计或聚合操作。
-spec fold(graph(), fun((vertex(), Acc) -> Acc), Acc) -> Acc.
fold(#{vertices := Vertices}, Fun, Acc) ->
    maps:fold(fun(_Id, V, A) -> Fun(V, A) end, Acc, Vertices).

%%====================================================================
%% 状态查询
%%====================================================================

%% @doc 获取活跃顶点数量
%%
%% 活跃顶点是指未调用 vote_to_halt 的顶点。
%% 当所有顶点都不活跃时，Pregel 计算终止。
-spec active_count(graph()) -> non_neg_integer().
active_count(#{vertices := Vertices}) ->
    pregel_utils:map_count(fun pregel_vertex:is_active/1, Vertices).

%% @doc 获取已停止顶点数量
%%
%% 已停止顶点是指调用了 vote_to_halt 的顶点。
-spec halted_count(graph()) -> non_neg_integer().
halted_count(#{vertices := Vertices}) ->
    pregel_utils:map_count(fun pregel_vertex:is_halted/1, Vertices).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从边列表收集所有顶点ID
%% 提取边的两端顶点，去重后返回。
-spec collect_vertex_ids([{vertex_id(), vertex_id()} |
                          {vertex_id(), vertex_id(), number()}]) -> [vertex_id()].
collect_vertex_ids(Edges) ->
    lists:usort(lists:flatmap(
        fun({From, To}) -> [From, To];
           ({From, To, _Weight}) -> [From, To]
        end,
        Edges
    )).

%% @private 根据ID列表创建顶点
%% 为每个ID创建一个空顶点（无出边）。
-spec create_vertices([vertex_id()]) -> graph().
create_vertices(Ids) ->
    lists:foldl(
        fun(Id, G) ->
            add_vertex(G, Id)
        end,
        new(),
        Ids
    ).

%% @private 批量添加边
%% 遍历边列表，逐条添加到图中。
-spec add_edges(graph(), [{vertex_id(), vertex_id()} |
                          {vertex_id(), vertex_id(), number()}]) -> graph().
add_edges(Graph, Edges) ->
    lists:foldl(
        fun({From, To}, G) -> add_edge(G, From, To);
           ({From, To, Weight}, G) -> add_edge(G, From, To, Weight)
        end,
        Graph,
        Edges
    ).
