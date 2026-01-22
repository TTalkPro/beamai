%%%-------------------------------------------------------------------
%%% @doc Pregel 图结构模块
%%%
%%% 管理图中的所有顶点和边:
%%% - vertices: 顶点映射 #{vertex_id => vertex}
%%% - config: 图配置（最大超步数、检查点间隔等）
%%%
%%% 设计模式: 组合模式 + 不可变数据结构
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_graph).

%% 构造函数
-export([new/0, new/1, from_edges/1, from_edges/2]).

%% 顶点操作
-export([add_vertex/2, add_vertex/3]).
-export([get/2, has/2, remove/2, update/3]).
-export([vertices/1, ids/1, size/1]).

%% 边操作
-export([add_edge/3, add_edge/4, remove_edge/3]).
-export([edges/2, neighbors/2]).

%% 批量操作
-export([map/2, filter/2, fold/3]).

%% 状态查询
-export([active_count/1, halted_count/1]).

%% 配置
-export([config/1, set_config/2]).

%% 类型导出
-export_type([graph/0, config/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type vertex_id() :: pregel_vertex:vertex_id().
-type vertex() :: pregel_vertex:vertex().
-type edge() :: pregel_vertex:edge().

%% 图配置
-type config() :: #{
    max_supersteps => pos_integer(),
    checkpoint_interval => pos_integer() | undefined
}.

%% 图结构
-type graph() :: #{
    vertices := #{vertex_id() => vertex()},
    config := config()
}.

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建空图
-spec new() -> graph().
new() ->
    new(#{}).

%% @doc 创建带配置的空图
-spec new(config()) -> graph().
new(Config) ->
    DefaultConfig = #{
        max_supersteps => 100,
        checkpoint_interval => undefined
    },
    #{
        vertices => #{},
        config => maps:merge(DefaultConfig, Config)
    }.

%% @doc 从边列表构建图
%% Edges 格式: [{From, To}] 或 [{From, To, Weight}]
-spec from_edges([{vertex_id(), vertex_id()} |
                  {vertex_id(), vertex_id(), number()}]) -> graph().
from_edges(Edges) ->
    from_edges(Edges, #{}).

%% @doc 从边列表和附加顶点ID构建图
%% 全局状态模式：InitialValues 参数已废弃，仅用于向后兼容
%% ExtraIds: 额外的顶点ID映射（只取keys，values被忽略）
-spec from_edges([{vertex_id(), vertex_id()} |
                  {vertex_id(), vertex_id(), number()}],
                 #{vertex_id() => term()}) -> graph().
from_edges(Edges, ExtraIds) ->
    %% 收集所有顶点ID（包括边端点和额外ID）
    EdgeIds = collect_vertex_ids(Edges),
    InitIds = maps:keys(ExtraIds),
    AllIds = lists:usort(EdgeIds ++ InitIds),
    %% 创建顶点
    Graph0 = create_vertices(AllIds),
    %% 添加边
    add_edges(Graph0, Edges).

%%====================================================================
%% 顶点操作
%%====================================================================

%% @doc 添加顶点（仅ID）
-spec add_vertex(graph(), vertex_id()) -> graph().
add_vertex(Graph, Id) ->
    add_vertex(Graph, Id, []).

%% @doc 添加顶点（带边）
-spec add_vertex(graph(), vertex_id(), [edge()]) -> graph().
add_vertex(#{vertices := Vertices} = Graph, Id, Edges) ->
    Vertex = pregel_vertex:new(Id, Edges),
    Graph#{vertices => Vertices#{Id => Vertex}}.

%% @doc 获取顶点
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

%% @doc 获取所有顶点ID
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
-spec add_edge(graph(), vertex_id(), vertex_id()) -> graph().
add_edge(Graph, From, To) ->
    add_edge(Graph, From, To, 1).

%% @doc 添加带权重的边
-spec add_edge(graph(), vertex_id(), vertex_id(), number()) -> graph().
add_edge(#{vertices := Vertices} = Graph, From, To, Weight) ->
    Vertex = case maps:get(From, Vertices, undefined) of
        undefined -> pregel_vertex:new(From);
        V -> V
    end,
    NewVertex = pregel_vertex:add_edge(Vertex, To, Weight),
    Graph#{vertices => Vertices#{From => NewVertex}}.

%% @doc 移除边
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
-spec edges(graph(), vertex_id()) -> [edge()].
edges(Graph, Id) ->
    case get(Graph, Id) of
        undefined -> [];
        Vertex -> pregel_vertex:edges(Vertex)
    end.

%% @doc 获取顶点的所有邻居
-spec neighbors(graph(), vertex_id()) -> [vertex_id()].
neighbors(Graph, Id) ->
    case get(Graph, Id) of
        undefined -> [];
        Vertex -> pregel_vertex:neighbors(Vertex)
    end.

%%====================================================================
%% 批量操作
%%====================================================================

%% @doc 对所有顶点应用函数
-spec map(graph(), fun((vertex()) -> vertex())) -> graph().
map(#{vertices := Vertices} = Graph, Fun) ->
    Graph#{vertices => maps:map(fun(_Id, V) -> Fun(V) end, Vertices)}.

%% @doc 过滤顶点
-spec filter(graph(), fun((vertex()) -> boolean())) -> graph().
filter(#{vertices := Vertices} = Graph, Pred) ->
    Graph#{vertices => maps:filter(fun(_Id, V) -> Pred(V) end, Vertices)}.

%% @doc 折叠顶点
-spec fold(graph(), fun((vertex(), Acc) -> Acc), Acc) -> Acc.
fold(#{vertices := Vertices}, Fun, Acc) ->
    maps:fold(fun(_Id, V, A) -> Fun(V, A) end, Acc, Vertices).

%%====================================================================
%% 状态查询
%%====================================================================

%% @doc 获取活跃顶点数量
-spec active_count(graph()) -> non_neg_integer().
active_count(#{vertices := Vertices}) ->
    pregel_utils:map_count(fun pregel_vertex:is_active/1, Vertices).

%% @doc 获取已停止顶点数量
-spec halted_count(graph()) -> non_neg_integer().
halted_count(#{vertices := Vertices}) ->
    pregel_utils:map_count(fun pregel_vertex:is_halted/1, Vertices).

%%====================================================================
%% 配置
%%====================================================================

%% @doc 获取配置
-spec config(graph()) -> config().
config(#{config := Config}) -> Config.

%% @doc 设置配置
-spec set_config(graph(), config()) -> graph().
set_config(Graph, Config) ->
    Graph#{config => Config}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从边列表收集所有顶点ID
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
