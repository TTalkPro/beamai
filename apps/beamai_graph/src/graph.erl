%%%-------------------------------------------------------------------
%%% @doc 图流程控制主模块 (LangGraph 风格，Pregel 执行引擎)
%%%
%%% 提供图执行框架的统一 API 入口。
%%% 整合构建器、执行器和状态管理。
%%%
%%% 基于 LangGraph 思想实现，使用 Pregel 作为底层执行引擎:
%%% - 节点: 转换状态的函数
%%% - 边: 控制流程走向 (直接或条件)
%%% - 状态: 在图中流动的不可变数据
%%% - 超步: 每次节点执行周期
%%%
%%% 执行模式:
%%% - run/2,3: 批量执行，使用 Pregel BSP 模型
%%% - stream/2,3: 流式执行，逐步返回状态
%%%
%%% 使用示例:
%%%
%%% 方式一: 声明式 DSL (推荐)
%%% <pre>
%%% {ok, Graph} = graph:build([
%%%     {node, process, ProcessFun},
%%%     {edge, process, '__end__'},
%%%     {entry, process}
%%% ]),
%%% Result = graph:run(Graph, graph:state(#{input => Data})).
%%% </pre>
%%%
%%% 方式二: 命令式 Builder
%%% <pre>
%%% B0 = graph:builder(),
%%% B1 = graph:add_node(B0, process, ProcessFun),
%%% B2 = graph:add_edge(B1, process, '__end__'),
%%% B3 = graph:set_entry(B2, process),
%%% {ok, Graph} = graph:compile(B3).
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph).

%% DSL API (声明式)
-export([build/1, build/2]).

%% 构建器 API (命令式)
-export([builder/0, builder/1]).
-export([add_node/3, add_node/4]).
-export([add_edge/3]).
-export([add_fanout_edge/3]).
-export([add_conditional_edge/3, add_conditional_edge/4]).
-export([set_entry/2]).
-export([compile/1]).

%% 执行 API
-export([run/2, run/3]).
-export([stream/2, stream/3]).

%% 状态 API (便捷重导出)
-export([state/0, state/1]).
-export([get/2, get/3, set/3]).

%% 便捷函数
-export([start_node/0, end_node/0]).

%% 类型定义
-type graph() :: graph_builder:graph().
-type builder() :: graph_builder:builder().
-type state() :: graph_state:state().
-type node_id() :: graph_node:node_id().
-type node_fun() :: graph_node:node_fun().
-type router_fun() :: graph_edge:router_fun().
-type route_map() :: graph_edge:route_map().
-type engine() :: graph_runner:engine().
-type run_options() :: graph_runner:run_options().
-type run_result() :: graph_runner:run_result().

-export_type([graph/0, builder/0, state/0, node_id/0, node_fun/0,
              router_fun/0, route_map/0, engine/0, run_options/0, run_result/0]).

%%====================================================================
%% DSL API (声明式)
%%====================================================================

%% @doc 从 DSL 元素列表构建图
%% 支持的元素:
%% - {node, Name, Fun}           节点
%% - {edge, From, To}            直接边
%% - {fanout, From, Targets}     扇出边
%% - {conditional_edge, From, Fun} 条件边
%% - {entry, Node}               入口节点
-spec build([graph_dsl:dsl_element()]) -> {ok, graph()} | {error, term()}.
build(Specs) ->
    graph_dsl:build(Specs).

%% @doc 从 DSL 元素列表构建图 (带默认配置)
-spec build([graph_dsl:dsl_element()], map()) -> {ok, graph()} | {error, term()}.
build(Specs, Config) ->
    graph_dsl:build(Specs, Config).

%%====================================================================
%% 构建器 API (命令式)
%%====================================================================

%% @doc 创建新构建器
-spec builder() -> builder().
builder() ->
    graph_builder:new().

%% @doc 创建带配置的构建器
-spec builder(map()) -> builder().
builder(Config) ->
    graph_builder:new(Config).

%% @doc 添加节点
-spec add_node(builder(), node_id(), node_fun()) -> builder().
add_node(Builder, Id, Fun) ->
    graph_builder:add_node(Builder, Id, Fun).

%% @doc 添加带元数据的节点
-spec add_node(builder(), node_id(), node_fun(), map()) -> builder().
add_node(Builder, Id, Fun, Metadata) ->
    graph_builder:add_node(Builder, Id, Fun, Metadata).

%% @doc 添加直接边
-spec add_edge(builder(), node_id(), node_id()) -> builder().
add_edge(Builder, From, To) ->
    graph_builder:add_edge(Builder, From, To).

%% @doc 添加扇出边 (并行分发到多个目标)
-spec add_fanout_edge(builder(), node_id(), [node_id()]) -> builder().
add_fanout_edge(Builder, From, Targets) ->
    graph_builder:add_fanout_edge(Builder, From, Targets).

%% @doc 添加条件边 (路由函数)
-spec add_conditional_edge(builder(), node_id(), router_fun()) -> builder().
add_conditional_edge(Builder, From, RouterFun) ->
    graph_builder:add_conditional_edge(Builder, From, RouterFun).

%% @doc 添加条件边 (路由映射)
-spec add_conditional_edge(builder(), node_id(), router_fun(), route_map()) -> builder().
add_conditional_edge(Builder, From, RouterFun, RouteMap) ->
    graph_builder:add_conditional_edge(Builder, From, RouterFun, RouteMap).

%% @doc 设置入口节点
-spec set_entry(builder(), node_id()) -> builder().
set_entry(Builder, NodeId) ->
    graph_builder:set_entry(Builder, NodeId).

%% @doc 编译图
-spec compile(builder()) -> {ok, graph()} | {error, term()}.
compile(Builder) ->
    graph_builder:compile(Builder).

%%====================================================================
%% 执行 API
%%====================================================================

%% @doc 运行图
-spec run(graph(), state()) -> run_result().
run(Graph, InitialState) ->
    graph_runner:run(Graph, InitialState).

%% @doc 运行图，带选项
-spec run(graph(), state(), run_options()) -> run_result().
run(Graph, InitialState, Options) ->
    graph_runner:run(Graph, InitialState, Options).

%% @doc 流式执行图
-spec stream(graph(), state()) -> fun().
stream(Graph, InitialState) ->
    graph_runner:stream(Graph, InitialState).

%% @doc 流式执行图，带选项
-spec stream(graph(), state(), run_options()) -> fun().
stream(Graph, InitialState, Options) ->
    graph_runner:stream(Graph, InitialState, Options).

%%====================================================================
%% 状态 API (便捷重导出)
%%====================================================================

%% @doc 创建空状态
-spec state() -> state().
state() ->
    graph_state:new().

%% @doc 从 Map 创建状态
-spec state(map()) -> state().
state(Map) ->
    graph_state:new(Map).

%% @doc 获取状态值
-spec get(state(), atom()) -> term().
get(State, Key) ->
    graph_state:get(State, Key).

%% @doc 获取状态值，带默认值
-spec get(state(), atom(), term()) -> term().
get(State, Key, Default) ->
    graph_state:get(State, Key, Default).

%% @doc 设置状态值
-spec set(state(), atom(), term()) -> state().
set(State, Key, Value) ->
    graph_state:set(State, Key, Value).

%%====================================================================
%% 便捷函数
%%====================================================================

%% @doc 获取起始节点 ID
-spec start_node() -> node_id().
start_node() ->
    '__start__'.

%% @doc 获取终止节点 ID
-spec end_node() -> node_id().
end_node() ->
    '__end__'.
