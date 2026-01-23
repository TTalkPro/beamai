%%%-------------------------------------------------------------------
%%% @doc Graph Pregel 计算函数模块
%%%
%%% 提供全局的 Pregel 计算函数，用于 Graph 执行。
%%%
%%% 全局状态模式:
%%% - 计算函数从 global_state 读取数据
%%% - 计算函数返回 delta（增量更新）
%%% - 消息用于协调执行流程（activate 消息），不携带状态
%%% - Master 负责合并 delta 并广播新的 global_state
%%%
%%% 核心功能:
%%% - compute_fn/0: 返回全局 Pregel 计算函数
%%% - from_pregel_result/1: 从 Pregel 结果提取最终状态
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_compute).

%% API 导出
-export([compute_fn/0]).
-export([from_pregel_result/1]).

%% 类型定义
-type delta() :: #{atom() | binary() => term()}.

-export_type([delta/0]).

%% 特殊节点常量
-define(START_NODE, '__start__').
-define(END_NODE, '__end__').

%%====================================================================
%% API
%%====================================================================

%% @doc 返回全局 Pregel 计算函数
%%
%% 全局状态模式（无 inbox 版本）：
%% - 从 context 中的 global_state 读取当前状态
%% - 返回 delta（增量更新）而不是更新 vertex value
%% - 返回 activations（顶点ID列表）指定下一步激活的顶点
%%
%% 返回值包含 status 字段表示执行状态：
%% - status => ok: 计算成功
%% - status => {error, Reason}: 计算失败
%% - status => {interrupt, Reason}: 请求中断（human-in-the-loop）
-spec compute_fn() -> fun((pregel_worker:context()) -> pregel_worker:compute_result()).
compute_fn() ->
    fun(Ctx) ->
        try
            execute_node(Ctx)
        catch
            throw:{interrupt, Reason, Delta, Activations} ->
                %% 中断：返回 delta 和中断状态
                #{delta => Delta, activations => Activations, status => {interrupt, Reason}};
            Class:Reason:_Stacktrace ->
                %% 失败时返回错误状态，空 delta
                #{delta => #{}, activations => [], status => {error, {Class, Reason}}}
        end
    end.

%% @private 执行节点计算
%%
%% 无 inbox 版本：节点通过被激活来触发计算
%% - 不再从 inbox 读取消息
%% - 从 global_state 读取所有需要的数据
-spec execute_node(pregel_worker:context()) -> pregel_worker:compute_result().
execute_node(Ctx) ->
    #{vertex_id := VertexId, global_state := GlobalState} = Ctx,

    case VertexId of
        ?START_NODE ->
            handle_start_node(Ctx, GlobalState);
        ?END_NODE ->
            handle_end_node(Ctx);
        _ ->
            handle_regular_node(Ctx, GlobalState)
    end.

%% @doc 从 Pregel 结果中提取最终状态
%%
%% 全局状态模式：直接返回 global_state
%% 如果有失败的顶点，返回错误
-spec from_pregel_result(pregel:result()) -> {ok, graph_state:state()} | {error, term()}.
from_pregel_result(Result) ->
    %% 首先检查是否有失败的顶点
    FailedCount = maps:get(failed_count, Result, 0),
    case FailedCount > 0 of
        true ->
            GlobalState = pregel:get_result_global_state(Result),
            FailedVertices = maps:get(failed_vertices, Result, []),
            {error, {partial_result, GlobalState, {node_failures, FailedVertices}}};
        false ->
            case pregel:get_result_status(Result) of
                completed ->
                    GlobalState = pregel:get_result_global_state(Result),
                    {ok, GlobalState};
                max_supersteps ->
                    GlobalState = pregel:get_result_global_state(Result),
                    {error, {partial_result, GlobalState, max_iterations_exceeded}};
                {error, Reason} ->
                    GlobalState = pregel:get_result_global_state(Result),
                    {error, {partial_result, GlobalState, Reason}}
            end
    end.

%%====================================================================
%% 节点处理
%%====================================================================

%% @private 处理起始节点
%%
%% 无 inbox 版本：节点被激活时执行
%% - 超步 0 时自动激活
%% - 后续超步由 Master 通过 activations 激活
%% 扁平化模式：顶点直接包含 fun_/metadata/routing_edges
-spec handle_start_node(pregel_worker:context(), graph_state:state()) ->
    pregel_worker:compute_result().
handle_start_node(Ctx, GlobalState) ->
    #{vertex := Vertex} = Ctx,
    execute_and_route(Ctx, GlobalState, Vertex).

%% @private 处理终止节点
%%
%% 无 inbox 版本：终止节点被激活时标记执行完成
-spec handle_end_node(pregel_worker:context()) ->
    pregel_worker:compute_result().
handle_end_node(_Ctx) ->
    %% 终止节点被激活，标记完成，不激活其他节点
    #{delta => #{}, activations => [], status => ok}.

%% @private 处理普通节点
%%
%% 无 inbox 版本：节点被激活时执行
%% - resume 数据现在通过 global_state 传递
%% 扁平化模式：顶点直接包含 fun_/metadata/routing_edges
-spec handle_regular_node(pregel_worker:context(), graph_state:state()) ->
    pregel_worker:compute_result().
handle_regular_node(Ctx, GlobalState) ->
    #{vertex := Vertex} = Ctx,
    execute_and_route(Ctx, GlobalState, Vertex).

%%====================================================================
%% 节点执行
%%====================================================================

%% @private 执行节点并路由到下一节点
%%
%% 扁平化模式：Vertex 直接包含 fun_/metadata/routing_edges
%% 使用 pregel_vertex 访问器获取属性
-spec execute_and_route(pregel_worker:context(), graph_state:state(), pregel_vertex:vertex()) ->
    pregel_worker:compute_result().
execute_and_route(Ctx, GlobalState, Vertex) ->
    #{vertex_id := VertexId} = Ctx,
    VertexInput = maps:get(vertex_input, Ctx, undefined),
    %% 使用扁平化访问器获取属性
    Fun = pregel_vertex:fun_(Vertex),
    RoutingEdges = pregel_vertex:routing_edges(Vertex),

    case Fun of
        undefined ->
            %% 无计算函数，直接路由
            route_to_next(GlobalState, RoutingEdges);
        _ ->
            %% 执行节点函数
            execute_fun_and_route(VertexId, Fun, GlobalState, RoutingEdges, VertexInput)
    end.

%% @private 执行节点函数并路由
-spec execute_fun_and_route(pregel_vertex:vertex_id(), term(), graph_state:state(), list(), map() | undefined) ->
    pregel_worker:compute_result().
execute_fun_and_route(VertexId, Fun, GlobalState, RoutingEdges, VertexInput) ->
    try Fun(GlobalState, VertexInput) of
        {ok, NewState} ->
            %% 成功：计算 delta，激活下游顶点
            Delta = compute_delta(GlobalState, NewState),
            Activations = build_activations(RoutingEdges, NewState),
            #{delta => Delta, activations => Activations, status => ok};
        {interrupt, Reason, NewState} ->
            %% 中断：保存 delta，但不激活下游
            Delta = compute_delta(GlobalState, NewState),
            throw({interrupt, Reason, Delta, []});
        {error, Reason} ->
            %% 失败：抛出异常
            throw({node_execution_error, VertexId, Reason})
    catch
        Class:Error:Stacktrace ->
            throw({node_execution_error, VertexId, {Class, Error, Stacktrace}})
    end.

%% @private 直接路由（无节点执行）
-spec route_to_next(graph_state:state(), [graph_edge:edge()]) ->
    pregel_worker:compute_result().
route_to_next(State, Edges) ->
    Activations = build_activations(Edges, State),
    #{delta => #{}, activations => Activations, status => ok}.

%% @private 计算 delta（状态差异）
%%
%% 简单实现：返回 NewState 中与 OldState 不同的字段
%% 注意：这里假设状态变化是由节点显式设置的
-spec compute_delta(graph_state:state(), graph_state:state()) -> delta().
compute_delta(OldState, NewState) ->
    %% 获取新状态的所有键
    NewKeys = graph_state:keys(NewState),

    %% 找出变化的字段
    lists:foldl(
        fun(Key, Acc) ->
            OldValue = graph_state:get(OldState, Key),
            NewValue = graph_state:get(NewState, Key),
            case OldValue =:= NewValue of
                true -> Acc;
                false -> Acc#{Key => NewValue}
            end
        end,
        #{},
        NewKeys
    ).

%%====================================================================
%% 激活构建
%%====================================================================

%% @private 构建要激活的顶点列表
%%
%% 无 inbox 版本：返回顶点ID列表或 dispatch 项
-spec build_activations([graph_edge:edge()], graph_state:state()) -> [atom() | {dispatch, graph_dispatch:dispatch()}].
build_activations([], _State) ->
    %% 无边，激活终止节点
    [?END_NODE];
build_activations(Edges, State) ->
    lists:foldl(
        fun(Edge, Acc) ->
            case graph_edge:resolve(Edge, State) of
                {ok, TargetNode} when is_atom(TargetNode) ->
                    [TargetNode | Acc];
                {ok, TargetNodes} when is_list(TargetNodes) ->
                    TargetNodes ++ Acc;
                {ok, {dispatches, DispatchList}} ->
                    [{dispatch, D} || D <- DispatchList] ++ Acc;
                {error, _} ->
                    Acc
            end
        end,
        [],
        Edges
    ).

