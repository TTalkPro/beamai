%%%-------------------------------------------------------------------
%%% @doc 图构建器模块
%%%
%%% 提供流畅的 DSL 接口来构建图。
%%% 支持链式调用风格:
%%%
%%% <pre>
%%% B0 = graph_builder:new(),
%%% B1 = graph_builder:add_node(B0, process, ProcessFun),
%%% B2 = graph_builder:add_edge(B1, process, '__end__'),
%%% B3 = graph_builder:set_entry(B2, process),
%%% {ok, Graph} = graph_builder:compile(B3).
%%% </pre>
%%%
%%% 设计原则:
%%% - 不可变性: 每次操作返回新构建器
%%% - 验证前置: 编译前完整验证
%%% - 错误明确: 清晰的错误信息
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_builder).

%% API 导出
-export([new/0, new/1]).
-export([add_node/3, add_node/4]).
-export([add_edge/3]).
-export([add_fanout_edge/3]).
-export([add_conditional_edge/3, add_conditional_edge/4]).
-export([set_entry/2]).
-export([compile/1]).
-export([validate/1]).

%% 类型定义
-type node_id() :: graph_node:node_id().
-type builder() :: #{
    nodes := #{node_id() => graph_node:graph_node()},
    edges := [graph_edge:edge()],
    entry := node_id() | undefined,
    config := config()
}.
-type config() :: #{
    max_iterations => pos_integer(),
    timeout => pos_integer()
}.
%% 编译后的图结构（简化版）
%%
%% 只保留必要字段:
%% - pregel_graph: 预构建的 Pregel 图（顶点已包含所有计算信息）
%% - entry: 入口节点ID
%% - max_iterations: 最大迭代次数
%%
%% 注意: nodes/edges/config 已整合到 pregel_graph 的顶点中
-type graph() :: #{
    pregel_graph := pregel:graph(),           %% 预构建的 Pregel 图
    entry := node_id(),                       %% 入口节点
    max_iterations := pos_integer()           %% 最大迭代次数
}.

-export_type([builder/0, graph/0, config/0]).

%% 默认配置常量
-define(DEFAULT_MAX_ITERATIONS, 100).
-define(DEFAULT_TIMEOUT, 30000).

%%====================================================================
%% 构建器创建
%%====================================================================

%% @doc 创建新构建器
-spec new() -> builder().
new() ->
    new(#{}).

%% @doc 创建带配置的构建器
-spec new(config()) -> builder().
new(Config) ->
    BaseNodes = add_special_nodes(#{}),
    #{
        nodes => BaseNodes,
        edges => [],
        entry => undefined,
        config => merge_config(Config)
    }.

%%====================================================================
%% 节点操作
%%====================================================================

%% @doc 添加节点
-spec add_node(builder(), node_id(), graph_node:node_fun()) -> builder().
add_node(Builder, Id, Fun) ->
    add_node(Builder, Id, Fun, #{}).

%% @doc 添加带元数据的节点
-spec add_node(builder(), node_id(), graph_node:node_fun(), graph_node:metadata()) -> builder().
add_node(#{nodes := Nodes} = Builder, Id, Fun, Metadata) ->
    validate_node_id(Id),
    Node = graph_node:new(Id, Fun, Metadata),
    Builder#{nodes => Nodes#{Id => Node}}.

%%====================================================================
%% 边操作
%%====================================================================

%% @doc 添加直接边
-spec add_edge(builder(), node_id(), node_id()) -> builder().
add_edge(#{edges := Edges} = Builder, From, To) ->
    Edge = graph_edge:direct(From, To),
    Builder#{edges => [Edge | Edges]}.

%% @doc 添加扇出边 (并行分发到多个目标)
%% From: 源节点
%% Targets: 目标节点列表
-spec add_fanout_edge(builder(), node_id(), [node_id()]) -> builder().
add_fanout_edge(#{edges := Edges} = Builder, From, Targets) ->
    Edge = graph_edge:fanout(From, Targets),
    Builder#{edges => [Edge | Edges]}.

%% @doc 添加条件边 (路由函数)
-spec add_conditional_edge(builder(), node_id(), graph_edge:router_fun()) -> builder().
add_conditional_edge(#{edges := Edges} = Builder, From, RouterFun) ->
    Edge = graph_edge:conditional(From, RouterFun),
    Builder#{edges => [Edge | Edges]}.

%% @doc 添加条件边 (路由映射)
-spec add_conditional_edge(builder(), node_id(), graph_edge:router_fun(), graph_edge:route_map()) -> builder().
add_conditional_edge(#{edges := Edges} = Builder, From, RouterFun, RouteMap) ->
    Edge = graph_edge:conditional(From, RouterFun, RouteMap),
    Builder#{edges => [Edge | Edges]}.

%%====================================================================
%% 入口设置
%%====================================================================

%% @doc 设置入口节点 (__start__ 后的第一个节点)
-spec set_entry(builder(), node_id()) -> builder().
set_entry(Builder, NodeId) ->
    Builder#{entry => NodeId}.

%%====================================================================
%% 编译
%%====================================================================

%% @doc 编译构建器为可执行图
-spec compile(builder()) -> {ok, graph()} | {error, term()}.
compile(Builder) ->
    case validate(Builder) of
        ok ->
            build_graph(Builder);
        {error, _} = Error ->
            Error
    end.

%% @doc 构建最终图结构
%%
%% 生成简化的 Pregel 图，顶点直接包含计算函数和路由边。
%% 输出结构只包含必要字段，无冗余数据。
-spec build_graph(builder()) -> {ok, graph()}.
build_graph(#{nodes := Nodes, edges := Edges, entry := Entry, config := Config}) ->
    %% 按源节点分组边
    EdgeMap = group_edges_by_source(Edges),
    %% 添加 __start__ 到入口节点的边
    StartEdge = graph_edge:direct('__start__', Entry),
    FinalEdgeMap = add_edge_to_map(EdgeMap, '__start__', StartEdge),

    %% 直接构建 Pregel 图（使用扁平化顶点结构）
    PregelGraph = build_pregel_graph(Nodes, FinalEdgeMap),

    %% 简化输出：只包含必要字段
    MaxIterations = maps:get(max_iterations, Config, ?DEFAULT_MAX_ITERATIONS),
    {ok, #{
        pregel_graph => PregelGraph,
        entry => Entry,
        max_iterations => MaxIterations
    }}.

%% @doc 构建 Pregel 图
%%
%% 扁平化模式：顶点直接包含 fun_/metadata/routing_edges 字段
%% 无需嵌套的 vertex_value 结构，简化属性访问
%%
%% 图执行模式：除 __start__ 外，所有顶点初始为 halted 状态
%% 这确保执行从 __start__ 开始，通过边激活后续顶点
-spec build_pregel_graph(#{node_id() => graph_node:graph_node()},
                         #{node_id() => [graph_edge:edge()]}) -> pregel:graph().
build_pregel_graph(Nodes, EdgeMap) ->
    EmptyGraph = pregel:new_graph(),
    Graph = maps:fold(
        fun(NodeId, Node, AccGraph) ->
            %% 从 graph_node 提取 fun_ 和 metadata
            Fun = graph_node:fun_(Node),
            Metadata = graph_node:metadata(Node),
            %% 获取该节点的路由边
            RoutingEdges = maps:get(NodeId, EdgeMap, []),
            %% 使用扁平化结构添加顶点
            pregel_graph:add_vertex_flat(AccGraph, NodeId, Fun, Metadata, RoutingEdges)
        end,
        EmptyGraph,
        Nodes
    ),
    %% 将除 __start__ 外的所有顶点设为 halted
    %% 这确保图执行从 __start__ 开始流动
    halt_non_start_vertices(Graph).

%% @private 将除 __start__ 外的所有顶点设为 halted
-spec halt_non_start_vertices(pregel:graph()) -> pregel:graph().
halt_non_start_vertices(Graph) ->
    pregel_graph:map(Graph, fun(Vertex) ->
        case pregel_vertex:id(Vertex) of
            '__start__' -> Vertex;  %% __start__ 保持 active
            _ -> pregel_vertex:halt(Vertex)  %% 其他顶点 halted
        end
    end).

%%====================================================================
%% 验证
%%====================================================================

%% @doc 验证构建器配置
-spec validate(builder()) -> ok | {error, term()}.
validate(#{nodes := Nodes, edges := Edges, entry := Entry}) ->
    validate_chain([
        fun() -> validate_entry(Entry, Nodes) end,
        fun() -> validate_edges(Edges, Nodes) end,
        fun() -> validate_reachability(Entry, Edges) end
    ]).

%% @doc 执行验证链
-spec validate_chain([fun(() -> ok | {error, term()})]) -> ok | {error, term()}.
validate_chain([]) ->
    ok;
validate_chain([Check | Rest]) ->
    case Check() of
        ok -> validate_chain(Rest);
        {error, _} = Error -> Error
    end.

%% @doc 验证入口点
-spec validate_entry(node_id() | undefined, map()) -> ok | {error, term()}.
validate_entry(undefined, _Nodes) ->
    {error, entry_not_set};
validate_entry(Entry, Nodes) ->
    case maps:is_key(Entry, Nodes) of
        true -> ok;
        false -> {error, {entry_node_not_found, Entry}}
    end.

%% @doc 验证所有边引用的节点存在
-spec validate_edges([graph_edge:edge()], map()) -> ok | {error, term()}.
validate_edges([], _Nodes) ->
    ok;
validate_edges([Edge | Rest], Nodes) ->
    case validate_single_edge(Edge, Nodes) of
        ok -> validate_edges(Rest, Nodes);
        {error, _} = Error -> Error
    end.

%% @doc 验证单条边
-spec validate_single_edge(graph_edge:edge(), map()) -> ok | {error, term()}.
validate_single_edge(Edge, Nodes) ->
    From = graph_edge:from(Edge),
    case maps:is_key(From, Nodes) of
        true -> validate_edge_target(Edge, Nodes);
        false -> {error, {source_node_not_found, From}}
    end.

%% @doc 验证边的目标节点
-spec validate_edge_target(graph_edge:edge(), map()) -> ok | {error, term()}.
validate_edge_target(Edge, Nodes) ->
    case graph_edge:is_direct(Edge) of
        true ->
            To = graph_edge:target(Edge),
            case maps:is_key(To, Nodes) of
                true -> ok;
                false -> {error, {target_node_not_found, To}}
            end;
        false ->
            %% 条件边在运行时验证
            ok
    end.

%% @doc 验证可达性 (简化检查)
-spec validate_reachability(node_id(), [graph_edge:edge()]) -> ok | {error, term()}.
validate_reachability(_Entry, _Edges) ->
    %% TODO: 实现完整的可达性检查
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 添加特殊节点 (__start__ 和 __end__)
-spec add_special_nodes(map()) -> map().
add_special_nodes(Nodes) ->
    StartNode = graph_node:start_node(),
    EndNode = graph_node:end_node(),
    Nodes#{'__start__' => StartNode, '__end__' => EndNode}.

%% @doc 合并用户配置与默认配置
-spec merge_config(config()) -> config().
merge_config(UserConfig) ->
    DefaultConfig = #{
        max_iterations => ?DEFAULT_MAX_ITERATIONS,
        timeout => ?DEFAULT_TIMEOUT
    },
    maps:merge(DefaultConfig, UserConfig).

%% @doc 验证节点 ID (防止覆盖特殊节点)
-spec validate_node_id(node_id()) -> ok.
validate_node_id('__start__') ->
    error({reserved_node_id, '__start__'});
validate_node_id('__end__') ->
    error({reserved_node_id, '__end__'});
validate_node_id(_) ->
    ok.

%% @doc 按源节点分组边
-spec group_edges_by_source([graph_edge:edge()]) -> #{node_id() => [graph_edge:edge()]}.
group_edges_by_source(Edges) ->
    lists:foldl(fun group_edge/2, #{}, Edges).

%% @doc 将边加入分组
-spec group_edge(graph_edge:edge(), map()) -> map().
group_edge(Edge, Acc) ->
    From = graph_edge:from(Edge),
    add_edge_to_map(Acc, From, Edge).

%% @doc 将边添加到边映射
-spec add_edge_to_map(map(), node_id(), graph_edge:edge()) -> map().
add_edge_to_map(EdgeMap, NodeId, Edge) ->
    Existing = maps:get(NodeId, EdgeMap, []),
    EdgeMap#{NodeId => [Edge | Existing]}.
