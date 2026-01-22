%%%-------------------------------------------------------------------
%%% @doc Pregel 顶点模块
%%%
%%% 顶点是图计算的基本单元，每个顶点包含:
%%% - id: 唯一标识符
%%% - edges: 出边列表
%%% - halted: 是否已投票停止
%%%
%%% 全局状态模式说明：
%%% 顶点不再包含 value 字段。在全局状态模式下，所有状态数据
%%% 由 Master 持有的 global_state 管理，顶点只是纯计算单元，
%%% 负责表示图的拓扑结构（id + edges）和活跃状态（halted）。
%%%
%%% 设计模式: 不可变数据结构 + 函数式操作
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_vertex).

%% 构造函数
-export([new/1, new/2]).

%% 读取操作
-export([id/1, edges/1, neighbors/1, out_degree/1]).
-export([is_halted/1, is_active/1]).

%% 修改操作
-export([halt/1, activate/1]).
-export([add_edge/2, add_edge/3, remove_edge/2, set_edges/2]).

%% 边操作
-export([make_edge/2]).

%% 类型导出
-export_type([vertex/0, vertex_id/0, edge/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type vertex_id() :: term().

-type edge() :: #{
    target := vertex_id(),
    weight := number()
}.

%% 顶点类型（全局状态模式：不含 value）
-type vertex() :: #{
    id := vertex_id(),
    edges := [edge()],
    halted := boolean()
}.

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建顶点（仅ID，无边）
-spec new(vertex_id()) -> vertex().
new(Id) ->
    new(Id, []).

%% @doc 创建顶点（ID + 边）
-spec new(vertex_id(), [edge()]) -> vertex().
new(Id, Edges) ->
    #{id => Id, edges => Edges, halted => false}.

%%====================================================================
%% 读取操作
%%====================================================================

%% @doc 获取顶点ID
-spec id(vertex()) -> vertex_id().
id(#{id := Id}) -> Id.

%% @doc 获取所有出边
-spec edges(vertex()) -> [edge()].
edges(#{edges := E}) -> E.

%% @doc 获取所有邻居ID
-spec neighbors(vertex()) -> [vertex_id()].
neighbors(#{edges := Edges}) ->
    [T || #{target := T} <- Edges].

%% @doc 获取出度
-spec out_degree(vertex()) -> non_neg_integer().
out_degree(#{edges := E}) -> length(E).

%% @doc 检查是否已停止
-spec is_halted(vertex()) -> boolean().
is_halted(#{halted := H}) -> H.

%% @doc 检查是否活跃
-spec is_active(vertex()) -> boolean().
is_active(V) -> not is_halted(V).

%%====================================================================
%% 修改操作
%%====================================================================

%% @doc 投票停止
-spec halt(vertex()) -> vertex().
halt(V) -> V#{halted => true}.

%% @doc 激活顶点
-spec activate(vertex()) -> vertex().
activate(V) -> V#{halted => false}.

%% @doc 添加边（默认权重为1）
-spec add_edge(vertex(), vertex_id()) -> vertex().
add_edge(V, TargetId) ->
    add_edge(V, TargetId, 1).

%% @doc 添加带权重的边（已存在则更新权重）
-spec add_edge(vertex(), vertex_id(), number()) -> vertex().
add_edge(#{edges := Edges} = V, TargetId, Weight) ->
    NewEdge = make_edge(TargetId, Weight),
    %% 过滤掉旧边，添加新边
    FilteredEdges = [E || #{target := T} = E <- Edges, T =/= TargetId],
    V#{edges => [NewEdge | FilteredEdges]}.

%% @doc 移除边
-spec remove_edge(vertex(), vertex_id()) -> vertex().
remove_edge(#{edges := Edges} = V, TargetId) ->
    V#{edges => [E || #{target := T} = E <- Edges, T =/= TargetId]}.

%% @doc 设置所有边
-spec set_edges(vertex(), [edge()]) -> vertex().
set_edges(V, Edges) -> V#{edges => Edges}.

%%====================================================================
%% 边操作
%%====================================================================

%% @doc 创建边
-spec make_edge(vertex_id(), number()) -> edge().
make_edge(Target, Weight) ->
    #{target => Target, weight => Weight}.
