%%%-------------------------------------------------------------------
%%% @doc 图边定义模块
%%%
%%% 边连接节点并控制图中的流程走向。
%%% 支持三种边类型:
%%% - 直接边: 无条件转移到单一目标
%%% - 扇出边: 无条件转移到多个目标 (并行分发)
%%% - 条件边: 基于状态的动态路由
%%%
%%% 设计原则:
%%% - 路由分离: 路由逻辑独立于节点
%%% - 灵活性: 支持函数和映射两种路由方式
%%% - 安全性: 路由错误被捕获并报告
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_edge).

%% API 导出
-export([direct/2]).
-export([fanout/2]).
-export([conditional/2, conditional/3]).
-export([from/1, target/1, targets/1, router/1]).
-export([resolve/2]).
-export([is_conditional/1, is_direct/1, is_fanout/1]).
-export([is_valid/1]).

%% 类型定义
-type node_id() :: beamai_graph_node:node_id().
-type router_fun() :: fun((beamai_context:t()) -> node_id() | [node_id()]).
-type route_map() :: #{term() => node_id()}.

-type edge() :: #{
    type := direct | fanout | conditional,
    from := node_id(),
    to => node_id(),
    targets => [node_id()],
    router => router_fun(),
    route_map => route_map()
}.

-export_type([edge/0, router_fun/0, route_map/0]).

%%====================================================================
%% 边创建
%%====================================================================

%% @doc 创建直接边 (无条件转移)
%% From: 源节点
%% To: 目标节点
-spec direct(node_id(), node_id()) -> edge().
direct(From, To) when is_atom(From), is_atom(To) ->
    #{
        type => direct,
        from => From,
        to => To
    }.

%% @doc 创建扇出边 (同时发送到多个目标，实现并行分发)
%% From: 源节点
%% Targets: 目标节点列表
-spec fanout(node_id(), [node_id()]) -> edge().
fanout(From, Targets) when is_atom(From), is_list(Targets) ->
    #{
        type => fanout,
        from => From,
        targets => Targets
    }.

%% @doc 创建条件边 (基于路由函数)
%% RouterFun: 接收状态，返回目标节点
-spec conditional(node_id(), router_fun()) -> edge().
conditional(From, RouterFun) when is_atom(From), is_function(RouterFun, 1) ->
    #{
        type => conditional,
        from => From,
        router => RouterFun
    }.

%% @doc 创建条件边 (基于路由映射)
%% RouterFun: 返回映射键
%% RouteMap: 键到节点的映射
-spec conditional(node_id(), router_fun(), route_map()) -> edge().
conditional(From, RouterFun, RouteMap) when is_atom(From),
                                             is_function(RouterFun, 1),
                                             is_map(RouteMap) ->
    #{
        type => conditional,
        from => From,
        router => RouterFun,
        route_map => RouteMap
    }.

%%====================================================================
%% 边访问器
%%====================================================================

%% @doc 获取源节点 ID
-spec from(edge()) -> node_id().
from(#{from := From}) -> From.

%% @doc 获取目标节点 ID (仅直接边有效)
-spec target(edge()) -> node_id() | undefined.
target(#{type := direct, to := To}) -> To;
target(#{type := fanout}) -> undefined;
target(#{type := conditional}) -> undefined.

%% @doc 获取目标节点列表 (仅扇出边有效)
-spec targets(edge()) -> [node_id()] | undefined.
targets(#{type := fanout, targets := Targets}) -> Targets;
targets(_) -> undefined.

%% @doc 获取路由函数 (仅条件边有效)
-spec router(edge()) -> router_fun() | undefined.
router(#{type := conditional, router := Router}) -> Router;
router(#{type := direct}) -> undefined.

%%====================================================================
%% 边解析
%%====================================================================

%% @doc 解析边，获取下一个节点
%% 根据当前状态确定目标节点
-spec resolve(edge(), beamai_context:t()) -> {ok, node_id() | [node_id()] | {dispatches, [beamai_graph_dispatch:dispatch()]}} | {error, term()}.
resolve(#{type := direct, to := To}, _State) ->
    {ok, To};
resolve(#{type := fanout, targets := Targets}, _State) ->
    {ok, Targets};
resolve(#{type := conditional} = Edge, State) ->
    resolve_conditional(Edge, State).

%% @doc 解析条件边
-spec resolve_conditional(edge(), beamai_context:t()) -> {ok, node_id() | [node_id()] | {dispatches, [beamai_graph_dispatch:dispatch()]}} | {error, term()}.
resolve_conditional(#{router := Router} = Edge, State) ->
    try Router(State) of
        Result ->
            apply_route_map(Edge, Result)
    catch
        Class:Reason:Stack ->
            {error, {router_error, Class, Reason, Stack}}
    end.

%% @doc 应用路由映射 (如果存在)
%% 支持三种路由结果:
%% 1. 单一节点 (atom)
%% 2. 节点列表 (静态并行)
%% 3. Send 列表 (动态并行，每个 Send 有独立状态)
-spec apply_route_map(edge(), term()) -> {ok, node_id() | [node_id()] | {dispatches, [beamai_graph_dispatch:dispatch()]}} | {error, term()}.
apply_route_map(#{route_map := RouteMap}, Key) ->
    case maps:find(Key, RouteMap) of
        {ok, Target} -> {ok, Target};
        error -> {error, {no_route_for_key, Key}}
    end;
apply_route_map(_Edge, NodeId) when is_atom(NodeId) ->
    {ok, NodeId};
apply_route_map(_Edge, [First | _] = Result) ->
    %% 判断是节点列表还是 Send 列表
    case beamai_graph_dispatch:is_dispatch(First) of
        true ->
            %% Dispatch 列表 - 动态并行分发
            {ok, {dispatches, Result}};
        false when is_atom(First) ->
            %% 节点列表 - 静态并行
            {ok, Result};
        false ->
            {error, {invalid_router_result, Result}}
    end;
apply_route_map(_Edge, []) ->
    %% 空列表视为结束
    {ok, '__end__'};
apply_route_map(_Edge, Other) ->
    %% 检查是否是单个 Send
    case beamai_graph_dispatch:is_dispatch(Other) of
        true -> {ok, {dispatches, [Other]}};
        false -> {error, {invalid_router_result, Other}}
    end.

%%====================================================================
%% 边类型谓词
%%====================================================================

%% @doc 检查是否为条件边
-spec is_conditional(edge()) -> boolean().
is_conditional(#{type := conditional}) -> true;
is_conditional(_) -> false.

%% @doc 检查是否为直接边
-spec is_direct(edge()) -> boolean().
is_direct(#{type := direct}) -> true;
is_direct(_) -> false.

%% @doc 检查是否为扇出边
-spec is_fanout(edge()) -> boolean().
is_fanout(#{type := fanout}) -> true;
is_fanout(_) -> false.

%%====================================================================
%% 边验证
%%====================================================================

%% @doc 验证边结构是否有效
-spec is_valid(term()) -> boolean().
is_valid(#{type := direct, from := From, to := To})
  when is_atom(From), is_atom(To) ->
    true;
is_valid(#{type := fanout, from := From, targets := Targets})
  when is_atom(From), is_list(Targets), length(Targets) > 0 ->
    lists:all(fun is_atom/1, Targets);
is_valid(#{type := conditional, from := From, router := Router})
  when is_atom(From), is_function(Router, 1) ->
    true;
is_valid(_) ->
    false.
