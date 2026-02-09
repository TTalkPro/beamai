%%%-------------------------------------------------------------------
%%% @doc 图快照序列化/反序列化
%%%
%%% 负责从引擎中提取可序列化的快照数据，
%%% 以及从快照 + Graph 重建引擎可用的恢复数据。
%%%
%%% 设计要点：
%%% - 函数引用不可序列化，快照不存 Graph 本身
%%% - vertices 只保留 id + halted 状态
%%% - 恢复时需要重新提供 Graph（含 node funs）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_state).

-export([take/1, restore/2]).
-export_type([snapshot/0]).

-type vertex_id() :: beamai_pregel_vertex:vertex_id().

-type vertex_snapshot() :: #{id := atom(), halted := boolean()}.

-type snapshot() :: #{
    '__graph_snapshot__' := true,
    superstep := non_neg_integer(),
    global_state := beamai_graph_engine:state(),
    vertices := #{vertex_id() => vertex_snapshot()},
    pending_deltas := [map()] | undefined,
    pending_activations := [vertex_id()] | undefined,
    last_results := map() | undefined,
    cumulative_failures := [{vertex_id(), term()}],
    current_state := atom(),
    timestamp := integer()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 从引擎提取可序列化快照
%%
%% 去掉函数引用（vertices 只保留 id + halted），
%% 保留所有可序列化的状态数据。
-spec take(beamai_graph_engine:engine()) -> snapshot().
take(Engine) ->
    %% 通过 extract_snapshot_data 获取引擎内部数据
    SnapshotData = beamai_graph_engine:extract_snapshot_data(Engine),

    #{
        superstep := Superstep,
        global_state := GlobalState,
        pending_deltas := PendingDeltas,
        pending_activations := PendingActivations,
        vertices := Vertices
    } = SnapshotData,

    %% 序列化顶点：只保留 id + halted
    SerializedVertices = maps:map(
        fun(Id, Vertex) ->
            #{id => Id, halted => not beamai_pregel_vertex:is_active(Vertex)}
        end,
        Vertices
    ),

    #{
        '__graph_snapshot__' => true,
        superstep => Superstep,
        global_state => GlobalState,
        vertices => SerializedVertices,
        pending_deltas => PendingDeltas,
        pending_activations => PendingActivations,
        last_results => beamai_graph_engine:last_results(Engine),
        cumulative_failures => [],
        current_state => beamai_graph_engine:current_state(Engine),
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc 从快照 + Graph 重建引擎恢复数据
%%
%% 用 Graph 中的 pregel_graph 重建 vertices（恢复 fun_ 和 routing_edges），
%% 合并 snapshot 中的 halted 状态。
%% 返回 restore_opts 格式的 map，可传给 beamai_graph_engine:new/3 的 restore_from。
-spec restore(snapshot(), beamai_graph_builder:graph()) ->
    {ok, beamai_graph_engine:restore_opts()} | {error, term()}.
restore(#{
    '__graph_snapshot__' := true,
    superstep := Superstep,
    global_state := GlobalState,
    vertices := SnapshotVertices,
    pending_deltas := PendingDeltas,
    pending_activations := PendingActivations
}, Graph) ->
    #{pregel_graph := PregelGraph} = Graph,

    %% 从 Graph 获取完整顶点（含 fun_ 和 routing_edges）
    BaseVertices = beamai_graph_executor_utils:vertices_to_map(
        beamai_pregel_graph:vertices(PregelGraph)),

    %% 合并 snapshot 中的 halted 状态
    RestoredVertices = maps:map(
        fun(Id, BaseVertex) ->
            case maps:get(Id, SnapshotVertices, undefined) of
                undefined ->
                    BaseVertex;
                #{halted := true} ->
                    beamai_pregel_vertex:halt(BaseVertex);
                #{halted := false} ->
                    beamai_pregel_vertex:activate(BaseVertex);
                _ ->
                    BaseVertex
            end
        end,
        BaseVertices
    ),

    RestoreOpts = #{
        superstep => Superstep,
        global_state => GlobalState,
        vertices => RestoredVertices,
        pending_deltas => PendingDeltas,
        pending_activations => PendingActivations
    },

    {ok, RestoreOpts};

restore(_, _) ->
    {error, invalid_snapshot}.
