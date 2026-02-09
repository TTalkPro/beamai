%%%-------------------------------------------------------------------
%%% @doc Pregel 图结构模块
%%%
%%% 本模块管理 Pregel 计算图的拓扑结构。
%%% 图采用不可变数据结构设计，所有修改操作返回新图。
%%%
%%% 数据结构:
%%% - vertices: 顶点映射表 #{vertex_id() => vertex()}
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_pregel_graph).

%% === 构造函数 ===
-export([new/0]).

%% === 顶点操作 ===
-export([add_vertex_flat/5]).
-export([get/2, has/2, remove/2, update/3]).
-export([vertices/1, ids/1, size/1]).

%% === 批量操作 ===
-export([map/2, filter/2, fold/3]).

%% === 状态查询 ===
-export([active_count/1, halted_count/1]).

%% === 类型导出 ===
-export_type([graph/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type vertex_id() :: beamai_pregel_vertex:vertex_id().
-type vertex() :: beamai_pregel_vertex:vertex().

%% Pregel 图结构，仅包含顶点映射表。
-type graph() :: #{
    vertices := #{vertex_id() => vertex()}
}.

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建空图
-spec new() -> graph().
new() ->
    #{vertices => #{}}.

%%====================================================================
%% 顶点操作
%%====================================================================

%% @doc 添加扁平化顶点
%%
%% 直接将计算函数、元数据和路由边存储在顶点上。
-spec add_vertex_flat(graph(), vertex_id(), term(), map(), list()) -> graph().
add_vertex_flat(#{vertices := Vertices} = Graph, Id, Fun, Metadata, RoutingEdges) ->
    Vertex = beamai_pregel_vertex:new_flat(Id, Fun, Metadata, RoutingEdges),
    Graph#{vertices => Vertices#{Id => Vertex}}.

%% @doc 获取指定顶点
-spec get(graph(), vertex_id()) -> vertex() | undefined.
get(#{vertices := Vertices}, Id) ->
    maps:get(Id, Vertices, undefined).

%% @doc 检查顶点是否存在
-spec has(graph(), vertex_id()) -> boolean().
has(#{vertices := Vertices}, Id) ->
    maps:is_key(Id, Vertices).

%% @doc 移除顶点
-spec remove(graph(), vertex_id()) -> graph().
remove(#{vertices := Vertices} = Graph, Id) ->
    Graph#{vertices => maps:remove(Id, Vertices)}.

%% @doc 更新顶点
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
%% 批量操作
%%====================================================================

%% @doc 对所有顶点应用变换函数
-spec map(graph(), fun((vertex()) -> vertex())) -> graph().
map(#{vertices := Vertices} = Graph, Fun) ->
    Graph#{vertices => maps:map(fun(_Id, V) -> Fun(V) end, Vertices)}.

%% @doc 过滤顶点
-spec filter(graph(), fun((vertex()) -> boolean())) -> graph().
filter(#{vertices := Vertices} = Graph, Pred) ->
    Graph#{vertices => maps:filter(fun(_Id, V) -> Pred(V) end, Vertices)}.

%% @doc 折叠所有顶点
-spec fold(graph(), fun((vertex(), Acc) -> Acc), Acc) -> Acc.
fold(#{vertices := Vertices}, Fun, Acc) ->
    maps:fold(fun(_Id, V, A) -> Fun(V, A) end, Acc, Vertices).

%%====================================================================
%% 状态查询
%%====================================================================

%% @doc 获取活跃顶点数量
-spec active_count(graph()) -> non_neg_integer().
active_count(#{vertices := Vertices}) ->
    beamai_pregel_utils:map_count(fun beamai_pregel_vertex:is_active/1, Vertices).

%% @doc 获取已停止顶点数量
-spec halted_count(graph()) -> non_neg_integer().
halted_count(#{vertices := Vertices}) ->
    beamai_pregel_utils:map_count(fun beamai_pregel_vertex:is_halted/1, Vertices).
