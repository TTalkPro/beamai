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
%%%     result := undefined | {ok, state()} | {error, term()},
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
    result := undefined | {ok, graph_state:state()} | {error, term()},
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
%% 该函数是无状态的，完全依赖 vertex value 中的节点定义和边信息
-spec compute_fn() -> pregel:compute_fn().
compute_fn() ->
    fun(Ctx) ->
        VertexId = pregel:get_vertex_id(Ctx),
        VertexValue = pregel:get_vertex_value(Ctx),
        Messages = pregel:get_messages(Ctx),
        Superstep = pregel:get_superstep(Ctx),

        case VertexId of
            ?START_NODE ->
                handle_start_node(Ctx, VertexValue, Superstep);
            ?END_NODE ->
                handle_end_node(Ctx, Messages);
            _ ->
                handle_regular_node(Ctx, VertexValue, Messages)
        end
    end.

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
            case Status of
                max_supersteps ->
                    {error, max_iterations_exceeded};
                completed ->
                    find_last_result(Graph)
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
-spec finish_node_execution(pregel:context(), vertex_value(), graph_node:graph_node(),
                            graph_state:state(), [graph_edge:edge()]) -> pregel:context().
finish_node_execution(Ctx, VertexValue, Node, State, Edges) ->
    case graph_node:execute(Node, State) of
        {ok, NewState} ->
            Ctx1 = send_to_next_nodes(Ctx, Edges, NewState),
            NewValue = VertexValue#{result => {ok, NewState}},
            pregel:vote_to_halt(pregel:set_value(Ctx1, NewValue));
        {error, Reason} ->
            NewValue = VertexValue#{result => {error, Reason}},
            pregel:vote_to_halt(pregel:set_value(Ctx, NewValue))
    end.

%%====================================================================
%% 消息处理
%%====================================================================

%% @doc 聚合多个状态消息
-spec aggregate_state_messages([term()]) -> {ok, graph_state:state()} | {error, no_state_messages}.
aggregate_state_messages(Messages) ->
    States = [S || {state, S} <- Messages],
    case States of
        [] ->
            {error, no_state_messages};
        [SingleState] ->
            {ok, SingleState};
        MultipleStates ->
            MergedState = merge_states(MultipleStates),
            {ok, MergedState}
    end.

%% @doc 合并多个状态
-spec merge_states([graph_state:state()]) -> graph_state:state().
merge_states([First | Rest]) ->
    lists:foldl(fun merge_two_states/2, First, Rest).

%% @doc 合并两个状态
-spec merge_two_states(graph_state:state(), graph_state:state()) -> graph_state:state().
merge_two_states(State1, State2) ->
    Data1 = graph_state:to_map(State1),
    Data2 = graph_state:to_map(State2),
    MergedData = maps:merge(Data1, Data2),
    graph_state:new(MergedData).

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
            {error, no_result_found};
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

%% @doc 合并所有状态
-spec merge_all_states([graph_state:state()]) -> graph_state:state().
merge_all_states([]) ->
    graph_state:new();
merge_all_states([Single]) ->
    Single;
merge_all_states(States) ->
    lists:foldl(fun merge_two_states/2, graph_state:new(), States).
