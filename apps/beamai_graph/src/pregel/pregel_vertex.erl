%%%-------------------------------------------------------------------
%%% @doc Pregel 顶点模块
%%%
%%% 顶点是 Pregel 图计算的基本单元。本模块提供顶点的创建、读取和修改操作。
%%%
%%% 顶点数据结构:
%%% - id: 唯一标识符（任意 Erlang term）
%%% - edges: 出边列表，每条边包含 target（目标顶点ID）和 weight（权重）
%%% - value: 可选的顶点值，用于存储静态计算逻辑和路由规则
%%% - halted: 布尔值，表示顶点是否已投票停止
%%%
%%% 全局状态模式说明:
%%% 在全局状态模式下，运行时数据由 Master 的 global_state 管理。
%%% 顶点的 value 字段存储静态配置，典型结构为:
%%% ```
%%% #{
%%%     node => graph_node(),     %% 节点计算逻辑
%%%     edges => [graph_edge()]   %% 路由规则（条件边）
%%% }
%%% ```
%%%
%%% 设计模式: 不可变数据结构
%%% - 所有修改操作返回新顶点，原顶点不变
%%% - 适合并发环境，无需加锁
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_vertex).

%% === 构造函数 ===
-export([new/1, new/2, new/3]).
-export([new_flat/4]).

%% === 读取操作 ===
-export([id/1, edges/1, value/1, neighbors/1, out_degree/1]).
-export([is_halted/1, is_active/1]).

%% === 扁平化顶点属性访问（新API）===
-export([fun_/1, metadata/1, routing_edges/1]).

%% === 修改操作 ===
-export([halt/1, activate/1]).
-export([add_edge/2, add_edge/3, remove_edge/2, set_edges/2]).

%% === 边操作 ===
-export([make_edge/2]).

%% === 类型导出 ===
-export_type([vertex/0, vertex_id/0, edge/0]).

%%====================================================================
%% 类型定义
%%====================================================================

%% @type vertex_id() :: term().
%% 顶点唯一标识符，可以是任意 Erlang term（通常是 atom 或 binary）。
-type vertex_id() :: term().

%% @type edge() :: #{target := vertex_id(), weight := number()}.
%% 有向边，从当前顶点指向目标顶点。
%% - target: 目标顶点ID
%% - weight: 边权重（用于最短路径等算法）
-type edge() :: #{
    target := vertex_id(),
    weight := number()
}.

%% @type vertex() :: #{id := vertex_id(), ...}.
%% 顶点完整结构。支持两种模式:
%%
%% 传统模式（向后兼容）:
%% - id: 顶点唯一标识符
%% - edges: 出边列表（图拓扑结构）
%% - value: 可选的顶点值（存储计算逻辑和路由规则）
%% - halted: 是否已投票停止（用于 Pregel 终止判断）
%%
%% 扁平化模式（推荐，用于 Graph 执行）:
%% - id: 顶点唯一标识符
%% - fun_: 节点计算函数
%% - metadata: 节点元数据
%% - routing_edges: 路由边列表（条件边）
%% - halted: 是否已投票停止
-type vertex() :: #{
    id := vertex_id(),
    %% 传统模式字段
    edges => [edge()],
    value => term(),
    %% 扁平化模式字段
    fun_ => term(),
    metadata => map(),
    routing_edges => list(),
    %% 共用字段
    halted := boolean()
}.

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建顶点（仅指定ID）
%%
%% 创建一个新顶点，初始状态为活跃（halted=false），无出边。
%% 这是最简单的构造方式，后续可通过 add_edge 添加边。
-spec new(vertex_id()) -> vertex().
new(Id) ->
    new(Id, []).

%% @doc 创建顶点（指定ID和出边）
%%
%% 创建顶点并设置初始出边列表。
%% 边列表中每条边需符合 edge() 类型规范。
-spec new(vertex_id(), [edge()]) -> vertex().
new(Id, Edges) ->
    #{id => Id, edges => Edges, halted => false}.

%% @doc 创建顶点（指定ID、出边和值）
%%
%% Value 参数用于存储顶点的静态配置，典型用法:
%% ```
%% pregel_vertex:new(my_node, [], #{
%%     node => MyGraphNode,
%%     edges => [ConditionalEdge1, ConditionalEdge2]
%% }).
%% ```
-spec new(vertex_id(), [edge()], term()) -> vertex().
new(Id, Edges, Value) ->
    #{id => Id, edges => Edges, value => Value, halted => false}.

%% @doc 创建扁平化顶点（推荐用于 Graph 执行）
%%
%% 直接在顶点上存储计算函数、元数据和路由边，无需嵌套 value 结构。
%% 这是 Graph 执行的推荐方式，简化了属性访问。
%%
%% 参数:
%% - Id: 顶点唯一标识符（通常与节点ID相同）
%% - Fun: 节点计算函数（来自 graph_node）
%% - Metadata: 节点元数据 map
%% - RoutingEdges: 路由边列表（条件边，用于决定下一步激活哪些顶点）
%%
%% 示例:
%% ```
%% pregel_vertex:new_flat(my_node, fun(State) -> ... end, #{type => llm}, [Edge1, Edge2]).
%% ```
-spec new_flat(vertex_id(), term(), map(), list()) -> vertex().
new_flat(Id, Fun, Metadata, RoutingEdges) ->
    #{
        id => Id,
        fun_ => Fun,
        metadata => Metadata,
        routing_edges => RoutingEdges,
        halted => false
    }.

%%====================================================================
%% 读取操作
%%====================================================================

%% @doc 获取顶点ID
-spec id(vertex()) -> vertex_id().
id(#{id := Id}) -> Id.

%% @doc 获取所有出边（Pregel 拓扑边）
%%
%% 返回出边列表，每条边包含 target 和 weight。
%% 扁平化模式下，此字段不存在，返回空列表。
-spec edges(vertex()) -> [edge()].
edges(#{edges := E}) -> E;
edges(_) -> [].

%% @doc 获取顶点值
%%
%% 返回存储的值。如果顶点未设置值，返回 undefined。
%% 通常用于获取节点的计算逻辑和路由规则。
-spec value(vertex()) -> term().
value(#{value := V}) -> V;
value(_) -> undefined.

%% @doc 获取节点计算函数（扁平化模式）
%%
%% 返回直接存储在顶点上的计算函数。
%% 如果顶点使用传统模式（value 字段），返回 undefined。
-spec fun_(vertex()) -> term().
fun_(#{fun_ := F}) -> F;
fun_(_) -> undefined.

%% @doc 获取节点元数据（扁平化模式）
%%
%% 返回直接存储在顶点上的元数据 map。
%% 如果顶点使用传统模式，返回空 map。
-spec metadata(vertex()) -> map().
metadata(#{metadata := M}) -> M;
metadata(_) -> #{}.

%% @doc 获取路由边列表（扁平化模式）
%%
%% 返回直接存储在顶点上的路由边（条件边）列表。
%% 如果顶点使用传统模式，返回空列表。
-spec routing_edges(vertex()) -> list().
routing_edges(#{routing_edges := E}) -> E;
routing_edges(_) -> [].

%% @doc 获取所有邻居顶点ID
%%
%% 返回所有出边指向的目标顶点ID列表。
%% 等价于: [Target || #{target := Target} <- edges(V)]
-spec neighbors(vertex()) -> [vertex_id()].
neighbors(#{edges := Edges}) ->
    [T || #{target := T} <- Edges].

%% @doc 获取出度（出边数量）
-spec out_degree(vertex()) -> non_neg_integer().
out_degree(#{edges := E}) -> length(E).

%% @doc 检查顶点是否已停止
%%
%% 已停止的顶点不会参与后续计算，除非被重新激活。
-spec is_halted(vertex()) -> boolean().
is_halted(#{halted := H}) -> H.

%% @doc 检查顶点是否活跃
%%
%% 活跃顶点会在每个超步参与计算。
%% 当所有顶点都不活跃且无待激活顶点时，Pregel 计算终止。
-spec is_active(vertex()) -> boolean().
is_active(V) -> not is_halted(V).

%%====================================================================
%% 修改操作
%%====================================================================

%% @doc 投票停止（将顶点标记为已停止）
%%
%% 在计算函数中调用此操作，表示顶点已完成计算。
%% 注意：返回新顶点，原顶点不变。
-spec halt(vertex()) -> vertex().
halt(V) -> V#{halted => true}.

%% @doc 激活顶点（将已停止的顶点重新激活）
%%
%% 当收到消息或被显式激活时，已停止的顶点会被重新激活。
-spec activate(vertex()) -> vertex().
activate(V) -> V#{halted => false}.

%% @doc 添加边（权重默认为1）
%%
%% 向顶点添加一条指向目标顶点的边。
%% 如果到该目标的边已存在，会被新边替换。
-spec add_edge(vertex(), vertex_id()) -> vertex().
add_edge(V, TargetId) ->
    add_edge(V, TargetId, 1).

%% @doc 添加带权重的边
%%
%% 如果到该目标的边已存在，权重会被更新。
%% 内部实现：先过滤掉旧边，再添加新边。
-spec add_edge(vertex(), vertex_id(), number()) -> vertex().
add_edge(#{edges := Edges} = V, TargetId, Weight) ->
    NewEdge = make_edge(TargetId, Weight),
    %% 移除指向同一目标的旧边（如果存在）
    FilteredEdges = [E || #{target := T} = E <- Edges, T =/= TargetId],
    V#{edges => [NewEdge | FilteredEdges]}.

%% @doc 移除边
%%
%% 删除指向目标顶点的边。如果边不存在，返回原顶点。
-spec remove_edge(vertex(), vertex_id()) -> vertex().
remove_edge(#{edges := Edges} = V, TargetId) ->
    V#{edges => [E || #{target := T} = E <- Edges, T =/= TargetId]}.

%% @doc 设置所有出边（替换现有边）
%%
%% 用新的边列表完全替换顶点的出边。
%% 常用于批量更新场景。
-spec set_edges(vertex(), [edge()]) -> vertex().
set_edges(V, Edges) -> V#{edges => Edges}.

%%====================================================================
%% 边操作
%%====================================================================

%% @doc 创建边
%%
%% 构造一条边的数据结构。
%% 这是创建边的标准方式，确保数据格式正确。
-spec make_edge(vertex_id(), number()) -> edge().
make_edge(Target, Weight) ->
    #{target => Target, weight => Weight}.
