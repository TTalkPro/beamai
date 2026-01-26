%%%-------------------------------------------------------------------
%%% @doc Pregel 图分区模块
%%%
%%% 将顶点分配到不同的 Worker 进程:
%%% - hash: 基于哈希的分区（默认，均匀分布）
%%% - range: 范围分区（适用于有序键）
%%% - custom: 自定义分区函数
%%%
%%% 设计模式: 策略模式
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_partition).

%% 分区策略
-export([hash/0, range/1, custom/1]).

%% 分区操作
-export([worker_id/3, partition/3, partition_graph/2, partition_graph/3]).

%% 类型导出
-export_type([strategy/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type vertex_id() :: term().

%% 分区策略类型
-type strategy() ::
    hash |
    {range, [vertex_id()]} |
    {custom, fun((vertex_id(), pos_integer()) -> non_neg_integer())}.

%%====================================================================
%% 分区策略构造函数
%%====================================================================

%% @doc 哈希分区策略（默认）
-spec hash() -> strategy().
hash() -> hash.

%% @doc 范围分区策略
%% Boundaries: 分区边界列表，长度 = NumWorkers - 1
-spec range([vertex_id()]) -> strategy().
range(Boundaries) -> {range, Boundaries}.

%% @doc 自定义分区策略
-spec custom(fun((vertex_id(), pos_integer()) -> non_neg_integer())) -> strategy().
custom(Fun) -> {custom, Fun}.

%%====================================================================
%% 分区操作
%%====================================================================

%% @doc 获取顶点所属的 Worker ID
-spec worker_id(vertex_id(), pos_integer(), strategy()) -> non_neg_integer().
worker_id(VertexId, NumWorkers, hash) ->
    erlang:phash2(VertexId, NumWorkers);
worker_id(VertexId, NumWorkers, {range, Boundaries}) ->
    find_range(VertexId, Boundaries, 0, NumWorkers);
worker_id(VertexId, NumWorkers, {custom, Fun}) ->
    Fun(VertexId, NumWorkers) rem NumWorkers.

%% @doc 将顶点列表分区到各 Worker
%% 返回: #{WorkerId => [Vertex]}
-spec partition([pregel_vertex:vertex()], pos_integer(), strategy()) ->
    #{non_neg_integer() => [pregel_vertex:vertex()]}.
partition(Vertices, NumWorkers, Strategy) ->
    lists:foldl(
        fun(Vertex, Acc) ->
            Id = pregel_vertex:id(Vertex),
            WorkerId = worker_id(Id, NumWorkers, Strategy),
            Existing = maps:get(WorkerId, Acc, []),
            Acc#{WorkerId => [Vertex | Existing]}
        end,
        #{},
        Vertices
    ).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 查找范围分区
-spec find_range(vertex_id(), [vertex_id()], non_neg_integer(), pos_integer()) ->
    non_neg_integer().
find_range(_VertexId, [], Current, _NumWorkers) ->
    Current;
find_range(VertexId, [Boundary | Rest], Current, NumWorkers) ->
    case VertexId < Boundary of
        true -> Current;
        false -> find_range(VertexId, Rest, Current + 1, NumWorkers)
    end.

%%====================================================================
%% 图分区操作
%%====================================================================

%% @doc 将图分区到多个 Worker（使用默认哈希策略）
%% 返回: #{WorkerId => Graph}
-spec partition_graph(pregel_graph:graph(), pos_integer()) ->
    #{non_neg_integer() => pregel_graph:graph()}.
partition_graph(Graph, NumWorkers) ->
    partition_graph(Graph, NumWorkers, hash).

%% @doc 将图分区到多个 Worker（使用指定策略）
%% 返回: #{WorkerId => Graph}
%%
%% 全局状态模式：顶点 value 包含 node 和 routing edges
%% 同时保留顶点的 halted 状态
-spec partition_graph(pregel_graph:graph(), pos_integer(), strategy()) ->
    #{non_neg_integer() => pregel_graph:graph()}.
partition_graph(Graph, NumWorkers, Strategy) ->
    %% 1. 获取所有顶点并按 Worker 分区
    Vertices = pregel_graph:vertices(Graph),
    PartitionedVertices = partition(Vertices, NumWorkers, Strategy),

    %% 2. 为每个分区创建子图（保留 vertex value 和 halted 状态）
    maps:map(
        fun(_WorkerId, WorkerVertices) ->
            lists:foldl(
                fun(V, G) ->
                    Id = pregel_vertex:id(V),
                    IsHalted = pregel_vertex:is_halted(V),
                    %% 判断顶点模式：扁平化 vs 传统
                    IsFlatVertex = pregel_vertex:fun_(V) =/= undefined,
                    G1 = case IsFlatVertex of
                        true ->
                            %% 扁平化模式：直接使用 add_vertex_flat
                            Fun = pregel_vertex:fun_(V),
                            Metadata = pregel_vertex:metadata(V),
                            RoutingEdges = pregel_vertex:routing_edges(V),
                            pregel_graph:add_vertex_flat(G, Id, Fun, Metadata, RoutingEdges);
                        false ->
                            %% 传统模式：检查 value
                            Value = pregel_vertex:value(V),
                            case Value of
                                undefined ->
                                    %% 无 value 时，使用 pregel edges
                                    Edges = pregel_vertex:edges(V),
                                    pregel_graph:add_vertex(G, Id, Edges);
                                _ ->
                                    %% 有 value 时，保留 value
                                    pregel_graph:add_vertex(G, Id, Value)
                            end
                    end,
                    %% 保留 halted 状态
                    case IsHalted of
                        true ->
                            Vertex = pregel_graph:get(G1, Id),
                            HaltedVertex = pregel_vertex:halt(Vertex),
                            pregel_graph:update(G1, Id, HaltedVertex);
                        false ->
                            G1
                    end
                end,
                pregel_graph:new(),
                WorkerVertices
            )
        end,
        PartitionedVertices
    ).
