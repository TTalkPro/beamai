%%%-------------------------------------------------------------------
%%% @doc Graph Pregel 计算函数模块
%%%
%%% 提供全局的 Pregel 计算函数，用于 Graph 执行。
%%%
%%% 全局状态模式:
%%% - 计算函数从 context 读取数据
%%% - 计算函数返回 delta（增量更新）
%%% - 消息用于协调执行流程（activate 消息），不携带状态
%%% - Master 负责合并 delta 并广播新的 context
%%%
%%% 核心功能:
%%% - compute_fn/0: 返回全局 Pregel 计算函数
%%% - from_pregel_result/1: 从 Pregel 结果提取最终状态
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_compute).

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
%% - 从 compute_context 中的 context 读取当前状态
%% - 返回 delta（增量更新）而不是更新 vertex value
%% - 返回 activations（顶点ID列表）指定下一步激活的顶点
%%
%% 返回值包含 status 字段表示执行状态：
%% - status => ok: 计算成功
%% - status => {error, Reason}: 计算失败
%% - status => {interrupt, Reason}: 请求中断（human-in-the-loop）
-spec compute_fn() -> fun((beamai_graph_engine:compute_context()) -> beamai_graph_engine:compute_result()).
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
%% - 从 context 读取所有需要的数据
-spec execute_node(beamai_graph_engine:compute_context()) -> beamai_graph_engine:compute_result().
execute_node(Ctx) ->
    #{vertex_id := VertexId, context := Context} = Ctx,

    case VertexId of
        ?START_NODE ->
            handle_start_node(Ctx, Context);
        ?END_NODE ->
            handle_end_node(Ctx);
        _ ->
            handle_regular_node(Ctx, Context)
    end.

%% @doc 从 Pregel 结果中提取最终状态
%%
%% 全局状态模式：直接返回 context
%% 如果有失败的顶点，返回错误
-spec from_pregel_result(beamai_graph_engine:result()) -> {ok, beamai_context:t()} | {error, term()}.
from_pregel_result(Result) ->
    %% 首先检查是否有失败的顶点
    FailedCount = maps:get(failed_count, Result, 0),
    Context = maps:get(context, Result),
    case FailedCount > 0 of
        true ->
            FailedVertices = maps:get(failed_vertices, Result, []),
            {error, {partial_result, Context, {node_failures, FailedVertices}}};
        false ->
            case maps:get(status, Result) of
                completed ->
                    {ok, Context};
                max_supersteps ->
                    {error, {partial_result, Context, max_iterations_exceeded}};
                {error, Reason} ->
                    {error, {partial_result, Context, Reason}}
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
-spec handle_start_node(beamai_graph_engine:compute_context(), beamai_context:t()) ->
    beamai_graph_engine:compute_result().
handle_start_node(Ctx, Context) ->
    #{vertex := Vertex} = Ctx,
    execute_and_route(Ctx, Context, Vertex).

%% @private 处理终止节点
%%
%% 无 inbox 版本：终止节点被激活时标记执行完成
-spec handle_end_node(beamai_graph_engine:compute_context()) ->
    beamai_graph_engine:compute_result().
handle_end_node(_Ctx) ->
    %% 终止节点被激活，标记完成，不激活其他节点
    #{delta => #{}, activations => [], status => ok}.

%% @private 处理普通节点
%%
%% 无 inbox 版本：节点被激活时执行
%% - resume 数据现在通过 context 传递
%% 扁平化模式：顶点直接包含 fun_/metadata/routing_edges
-spec handle_regular_node(beamai_graph_engine:compute_context(), beamai_context:t()) ->
    beamai_graph_engine:compute_result().
handle_regular_node(Ctx, Context) ->
    #{vertex := Vertex} = Ctx,
    execute_and_route(Ctx, Context, Vertex).

%%====================================================================
%% 节点执行
%%====================================================================

%% @private 执行节点并路由到下一节点
%%
%% 扁平化模式：Vertex 直接包含 fun_/metadata/routing_edges
%% 使用 pregel_vertex 访问器获取属性
-spec execute_and_route(beamai_graph_engine:compute_context(), beamai_context:t(), beamai_pregel_vertex:vertex()) ->
    beamai_graph_engine:compute_result().
execute_and_route(Ctx, Context, Vertex) ->
    #{vertex_id := VertexId} = Ctx,
    VertexInput = maps:get(vertex_input, Ctx, undefined),
    ResumeData = maps:get(resume_data, Ctx, undefined),
    %% 使用扁平化访问器获取属性
    Fun = beamai_pregel_vertex:fun_(Vertex),
    RoutingEdges = beamai_pregel_vertex:routing_edges(Vertex),

    case Fun of
        undefined ->
            %% 无计算函数，直接路由
            route_to_next(Context, RoutingEdges);
        _ ->
            %% 执行节点函数
            execute_fun_and_route(VertexId, Fun, Context, RoutingEdges, VertexInput, ResumeData)
    end.

%% @private 根据 arity 调用节点函数：fun/2 或 fun/3
call_node_fun(Fun, State, Input, ResumeData) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 3} -> Fun(State, Input, ResumeData);
        {arity, 2} -> Fun(State, Input)
    end.

%% @private 执行节点函数并路由
-spec execute_fun_and_route(beamai_pregel_vertex:vertex_id(), term(), beamai_context:t(), list(), map() | undefined, term() | undefined) ->
    beamai_graph_engine:compute_result().
execute_fun_and_route(VertexId, Fun, Context, RoutingEdges, VertexInput, ResumeData) ->
    try call_node_fun(Fun, Context, VertexInput, ResumeData) of
        {ok, NewCtx} ->
            %% 成功：计算 delta，激活下游顶点
            Delta = compute_delta(Context, NewCtx),
            Activations = build_activations(RoutingEdges, NewCtx),
            #{delta => Delta, activations => Activations, status => ok};
        {command, Cmd} when is_map(Cmd) ->
            %% Command 模式：delta 直接使用，goto 覆盖路由
            handle_command(Cmd, Context, RoutingEdges);
        {interrupt, Reason, NewCtx} ->
            %% 中断：保存 delta，但不激活下游
            Delta = compute_delta(Context, NewCtx),
            throw({interrupt, Reason, Delta, []});
        {error, Reason} ->
            %% 失败：抛出异常
            throw({node_execution_error, VertexId, Reason})
    catch
        Class:Error:Stacktrace ->
            throw({node_execution_error, VertexId, {Class, Error, Stacktrace}})
    end.

%% @private 直接路由（无节点执行）
-spec route_to_next(beamai_context:t(), [beamai_graph_edge:edge()]) ->
    beamai_graph_engine:compute_result().
route_to_next(State, Edges) ->
    Activations = build_activations(Edges, State),
    #{delta => #{}, activations => Activations, status => ok}.

%% @private 计算 delta（状态差异）
%%
%% 简单实现：返回 NewCtx 中与 OldCtx 不同的字段
%% 注意：这里假设状态变化是由节点显式设置的
-spec compute_delta(beamai_context:t(), beamai_context:t()) -> delta().
compute_delta(OldCtx, NewCtx) ->
    maps:fold(
        fun(Key, NewValue, Acc) ->
            case Key of
                '__context__' -> Acc;  %% 跳过标记字段
                _ ->
                    OldValue = maps:get(Key, OldCtx, undefined),
                    case OldValue =:= NewValue of
                        true -> Acc;
                        false -> Acc#{Key => NewValue}
                    end
            end
        end,
        #{},
        NewCtx
    ).

%%====================================================================
%% Command 处理
%%====================================================================

%% @private 处理 Command 返回值
%%
%% Command 的 update 直接作为 delta（跳过 compute_delta）
%% Command 的 goto 覆盖边路由（undefined 时回退正常路由）
-spec handle_command(beamai_graph_command:command(), beamai_context:t(), list()) ->
    beamai_graph_engine:compute_result().
handle_command(Cmd, Context, RoutingEdges) ->
    RawDelta = beamai_graph_command:get_update(Cmd),
    %% 标准化 delta 中的 key（Command 的 update 可能含原始 atom key）
    Delta = normalize_delta(RawDelta),
    Goto = beamai_graph_command:get_goto(Cmd),
    Activations = resolve_goto(Goto, Context, Delta, RoutingEdges),
    #{delta => Delta, activations => Activations, status => ok}.

%% @private 解析 goto 目标为 activations 列表
-spec resolve_goto(beamai_graph_command:goto_target() | undefined, beamai_context:t(), map(), list()) ->
    [atom() | {dispatch, beamai_graph_dispatch:dispatch()}].
resolve_goto(undefined, Context, Delta, RoutingEdges) ->
    %% 无 goto：合并 delta 到状态后使用正常边路由
    MergedCtx = beamai_context:set_many(Context, Delta),
    build_activations(RoutingEdges, MergedCtx);
resolve_goto(Target, _, _, _) when is_atom(Target) ->
    [Target];
resolve_goto([], _, _, _) ->
    ['__end__'];
resolve_goto([First | _] = Targets, _, _, _) when is_atom(First) ->
    Targets;
resolve_goto([First | _] = Dispatches, _, _, _) when is_map(First) ->
    [{dispatch, D} || D <- Dispatches];
resolve_goto(Dispatch, _, _, _) when is_map(Dispatch) ->
    [{dispatch, Dispatch}].

%%====================================================================
%% 激活构建
%%====================================================================

%% @private 构建要激活的顶点列表
%%
%% 无 inbox 版本：返回顶点ID列表或 dispatch 项
-spec build_activations([beamai_graph_edge:edge()], beamai_context:t()) -> [atom() | {dispatch, beamai_graph_dispatch:dispatch()}].
build_activations([], _State) ->
    %% 无边，激活终止节点
    [?END_NODE];
build_activations(Edges, State) ->
    lists:foldl(
        fun(Edge, Acc) ->
            case beamai_graph_edge:resolve(Edge, State) of
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

%%====================================================================
%% 内部辅助
%%====================================================================

%% @private 标准化 delta 的 key
%%
%% Command 的 update map 可能含原始 atom key（如 #{count => 1}），
%% 需要通过 normalize_key 转为 binary，与 context 中的用户变量 key 一致。
-spec normalize_delta(map()) -> map().
normalize_delta(Delta) ->
    maps:fold(
        fun(K, V, Acc) -> Acc#{beamai_context:normalize_key(K) => V} end,
        #{},
        Delta
    ).

