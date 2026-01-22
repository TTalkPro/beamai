%%%-------------------------------------------------------------------
%%% @doc Graph Pregel 计算函数模块
%%%
%%% 提供全局的 Pregel 计算函数，用于 Graph 执行。
%%% 该函数是无状态的，完全依赖 vertex value 中的信息。
%%%
%%% 核心功能:
%%% - compute_fn/0: 返回全局 Pregel 计算函数
%%% - inject_initial_state/2: 注入初始状态到 __start__ 顶点
%%% - from_pregel_result/1: 从 Pregel 结果提取状态
%%%
%%% 顶点值结构 (vertex_value):
%%% #{
%%%     node := graph_node:graph_node(),
%%%     edges := [graph_edge:edge()],
%%%     result := undefined | {ok, state()} | {error, term()} | {interrupt, term(), state()},
%%%     initial_state => graph_state:state(),   %% 仅 __start__ 节点
%%%     activated => boolean()                   %% 仅 __start__ 节点
%%% }
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_compute).

%% API 导出
-export([compute_fn/0]).
-export([inject_initial_state/2]).
-export([from_pregel_result/1]).

%% 类型定义
-type vertex_value() :: #{
    node := graph_node:graph_node(),
    edges := [graph_edge:edge()],
    result := undefined | {ok, graph_state:state()} | {error, term()} | {interrupt, term(), graph_state:state()},
    initial_state => graph_state:state(),
    activated => boolean()
}.

-export_type([vertex_value/0]).

%% 特殊节点常量
-define(START_NODE, '__start__').
-define(END_NODE, '__end__').

%%====================================================================
%% API
%%====================================================================

%% @doc 返回全局 Pregel 计算函数
%%
%% 该函数是无状态的，完全依赖 vertex value 中的节点定义和边信息。
%% 返回值包含 status 字段表示执行状态：
%% - status => ok: 计算成功
%% - status => {error, Reason}: 计算失败
%% - status => {interrupt, Reason}: 请求中断（human-in-the-loop）
%%
%% 异常处理：
%% - 捕获所有异常，转换为 {error, {Class, Reason}} 格式
%% - 失败时保持原顶点状态，outbox 为空
-spec compute_fn() -> pregel:compute_fn().
compute_fn() ->
    fun(Ctx) ->
        try
            execute_node(Ctx)
        catch
            throw:{interrupt, Reason, NewCtx} ->
                %% 中断：返回中断状态，更新顶点但不发送消息
                NewCtx#{status => {interrupt, Reason}};
            Class:Reason:_Stacktrace ->
                %% 失败时返回错误状态
                make_error_result(Ctx, {Class, Reason})
        end
    end.

%% @private 执行节点计算（内部逻辑，不含异常处理）
%% 返回带 status => ok 的 context，或抛出 interrupt/error
-spec execute_node(pregel:context()) -> pregel_worker:compute_result().
execute_node(Ctx) ->
    VertexId = pregel:get_vertex_id(Ctx),
    VertexValue = pregel:get_vertex_value(Ctx),
    Messages = pregel:get_messages(Ctx),
    Superstep = pregel:get_superstep(Ctx),

    Result = case VertexId of
        ?START_NODE ->
            handle_start_node(Ctx, VertexValue, Superstep);
        ?END_NODE ->
            handle_end_node(Ctx, Messages);
        _ ->
            handle_regular_node(Ctx, VertexValue, Messages)
    end,
    %% 添加 status => ok（interrupt 和 error 通过 throw 处理）
    Result#{status => ok}.

%% @private 构造错误结果
%% 保持原顶点状态，outbox 为空，设置错误状态
-spec make_error_result(pregel:context(), term()) -> pregel_worker:compute_result().
make_error_result(Ctx, Reason) ->
    #{
        vertex => maps:get(vertex, Ctx),
        outbox => [],
        status => {error, Reason}
    }.

%% @doc 注入初始状态到起始节点
-spec inject_initial_state(pregel:graph(), graph_state:state()) -> pregel:graph().
inject_initial_state(Graph, InitialState) ->
    case pregel:get_graph_vertex(Graph, ?START_NODE) of
        undefined ->
            Graph;
        Vertex ->
            #{value := Value} = Vertex,
            NewValue = Value#{
                initial_state => InitialState,
                activated => true
            },
            pregel:map_vertices(Graph, fun(V) ->
                case pregel_vertex:id(V) of
                    ?START_NODE -> pregel_vertex:set_value(pregel_vertex:activate(V), NewValue);
                    _ -> V
                end
            end)
    end.

%% @doc 从 Pregel 结果中提取最终状态
-spec from_pregel_result(pregel:result()) -> {ok, graph_state:state()} | {error, term()}.
from_pregel_result(Result) ->
    Graph = pregel:get_result_graph(Result),
    Status = pregel:get_result_status(Result),

    %% 优先从 __end__ 顶点提取结果
    EndResult = case pregel:get_graph_vertex(Graph, ?END_NODE) of
        undefined ->
            undefined;
        EndVertex ->
            EndValue = pregel_vertex:value(EndVertex),
            maps:get(result, EndValue, undefined)
    end,

    case EndResult of
        {ok, FinalState} ->
            {ok, FinalState};
        {error, Reason} ->
            {error, Reason};
        undefined ->
            %% __end__ 节点没有结果，尝试从其他节点获取最后状态
            case find_last_result(Graph) of
                {ok, LastState} ->
                    case Status of
                        max_supersteps ->
                            %% 达到最大超步，返回部分结果
                            {error, {partial_result, LastState, max_iterations_exceeded}};
                        completed ->
                            {ok, LastState}
                    end;
                {error, no_result_found} ->
                    case Status of
                        max_supersteps ->
                            {error, max_iterations_exceeded};
                        completed ->
                            {error, no_result_found}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%%====================================================================
%% 节点处理
%%====================================================================

%% @doc 处理起始节点
-spec handle_start_node(pregel:context(), vertex_value(), non_neg_integer()) -> pregel:context().
handle_start_node(Ctx, VertexValue, 0) ->
    case maps:get(activated, VertexValue, false) of
        true  -> execute_start_node(Ctx, VertexValue);
        false -> pregel:vote_to_halt(Ctx)
    end;
handle_start_node(Ctx, _VertexValue, _Superstep) ->
    pregel:vote_to_halt(Ctx).

%% @doc 执行起始节点逻辑
-spec execute_start_node(pregel:context(), vertex_value()) -> pregel:context().
execute_start_node(Ctx, VertexValue) ->
    Node = maps:get(node, VertexValue),
    State = maps:get(initial_state, VertexValue),
    Edges = maps:get(edges, VertexValue, []),
    finish_node_execution(Ctx, VertexValue, Node, State, Edges).

%% @doc 处理终止节点
-spec handle_end_node(pregel:context(), [term()]) -> pregel:context().
handle_end_node(Ctx, []) ->
    pregel:vote_to_halt(Ctx);
handle_end_node(Ctx, Messages) ->
    case aggregate_state_messages(Messages) of
        {ok, State} ->
            VertexValue = pregel:get_vertex_value(Ctx),
            NewValue = VertexValue#{result => {ok, State}},
            Ctx1 = pregel:set_value(Ctx, NewValue),
            pregel:vote_to_halt(Ctx1);
        {error, _} ->
            pregel:vote_to_halt(Ctx)
    end.

%% @doc 处理普通节点
-spec handle_regular_node(pregel:context(), vertex_value(), [term()]) -> pregel:context().
handle_regular_node(Ctx, _VertexValue, []) ->
    pregel:vote_to_halt(Ctx);
handle_regular_node(Ctx, VertexValue, Messages) ->
    case aggregate_state_messages(Messages) of
        {ok, State} ->
            Node = maps:get(node, VertexValue),
            Edges = maps:get(edges, VertexValue, []),
            finish_node_execution(Ctx, VertexValue, Node, State, Edges);
        {error, no_state_messages} ->
            pregel:vote_to_halt(Ctx)
    end.

%% @doc 完成节点执行
%%
%% 执行节点逻辑并处理结果：
%% - 成功时：更新顶点值，发送消息到下一节点
%% - 中断时：抛出 interrupt，由 compute_fn 捕获，更新顶点但不发送消息
%% - 失败时：抛出异常，由 compute_fn 的 try-catch 捕获
%%
%% 注意：中断和失败都通过 throw 处理，由 compute_fn 统一捕获
-spec finish_node_execution(pregel:context(), vertex_value(), graph_node:graph_node(),
                            graph_state:state(), [graph_edge:edge()]) -> pregel:context().
finish_node_execution(Ctx, VertexValue, Node, State, Edges) ->
    case graph_node:execute(Node, State) of
        {ok, NewState} ->
            %% 成功：发送消息并更新顶点
            Ctx1 = send_to_next_nodes(Ctx, Edges, NewState),
            NewValue = VertexValue#{result => {ok, NewState}},
            pregel:vote_to_halt(pregel:set_value(Ctx1, NewValue));
        {interrupt, Reason, NewState} ->
            %% 中断：更新顶点状态（保存 pending_action 等信息），但不发送消息
            %% 通过 throw 让 compute_fn 返回 {interrupt, Reason} 状态
            NewValue = VertexValue#{result => {interrupt, Reason, NewState}},
            NewCtx = pregel:vote_to_halt(pregel:set_value(Ctx, NewValue)),
            throw({interrupt, Reason, NewCtx});
        {error, Reason} ->
            %% 失败：抛出异常，由 compute_fn 捕获
            throw({node_execution_error, Reason})
    end.

%%====================================================================
%% 消息处理
%%====================================================================

%% @doc 聚合多个状态消息
%%
%% 注意：在正常情况下，Pregel 的 state_reducer 已经将多个状态合并为一个。
%% 此函数仍然处理多状态情况，以保持向后兼容性。
%% 使用 graph_state_reducer 进行字段级合并（append/merge 策略）。
-spec aggregate_state_messages([term()]) -> {ok, graph_state:state()} | {error, no_state_messages}.
aggregate_state_messages(Messages) ->
    States = [S || {state, S} <- Messages],
    case States of
        [] ->
            {error, no_state_messages};
        [SingleState] ->
            {ok, SingleState};
        MultipleStates ->
            %% 使用字段级 Reducer 合并（与 pregel state_reducer 一致）
            MergedState = graph_state_reducer:merge_states(MultipleStates),
            {ok, MergedState}
    end.

%% @doc 根据边定义发送状态到下一个节点
-spec send_to_next_nodes(pregel:context(), [graph_edge:edge()], graph_state:state()) -> pregel:context().
send_to_next_nodes(Ctx, [], State) ->
    pregel:send_message(Ctx, ?END_NODE, {state, State});
send_to_next_nodes(Ctx, Edges, State) ->
    lists:foldl(
        fun(Edge, AccCtx) ->
            case graph_edge:resolve(Edge, State) of
                {ok, TargetNode} when is_atom(TargetNode) ->
                    pregel:send_message(AccCtx, TargetNode, {state, State});
                {ok, TargetNodes} when is_list(TargetNodes) ->
                    lists:foldl(
                        fun(Target, Acc) ->
                            pregel:send_message(Acc, Target, {state, State})
                        end,
                        AccCtx,
                        TargetNodes
                    );
                {error, _Reason} ->
                    AccCtx
            end
        end,
        Ctx,
        Edges
    ).

%%====================================================================
%% 结果提取
%%====================================================================

%% @doc 查找最后一个有结果的节点
-spec find_last_result(pregel:graph()) -> {ok, graph_state:state()} | {error, term()}.
find_last_result(Graph) ->
    Vertices = pregel:vertices(Graph),
    Results = [
        {pregel_vertex:id(V), maps:get(result, pregel_vertex:value(V), undefined)}
        || V <- Vertices
    ],

    ValidResults = [{Id, R} || {Id, {ok, _} = R} <- Results, Id =/= ?START_NODE],

    case ValidResults of
        [] ->
            %% 没有成功的结果，查找是否有错误信息
            ErrorResults = [{Id, R} || {Id, {error, _} = R} <- Results],
            case ErrorResults of
                [{_NodeId, {error, Reason}} | _] ->
                    %% 返回第一个错误
                    {error, Reason};
                [] ->
                    %% 检查是否有带 error 字段的状态
                    find_error_in_states(Results)
            end;
        _ ->
            case lists:keyfind(summarize, 1, ValidResults) of
                {summarize, {ok, State}} ->
                    {ok, State};
                false ->
                    AllStates = [S || {_, {ok, S}} <- ValidResults],
                    MergedState = merge_all_states(AllStates),
                    {ok, MergedState}
            end
    end.

%% @doc 从状态中查找错误信息
-spec find_error_in_states([{atom(), term()}]) -> {error, term()}.
find_error_in_states([]) ->
    {error, no_result_found};
find_error_in_states([{_Id, {ok, State}} | Rest]) ->
    %% 检查状态中是否有 error 字段
    case graph_state:get(State, error) of
        undefined ->
            find_error_in_states(Rest);
        ErrorReason ->
            {error, ErrorReason}
    end;
find_error_in_states([_ | Rest]) ->
    find_error_in_states(Rest).

%% @doc 合并所有状态
%%
%% 使用 graph_state_reducer 进行字段级合并。
-spec merge_all_states([graph_state:state()]) -> graph_state:state().
merge_all_states(States) ->
    graph_state_reducer:merge_states(States).
