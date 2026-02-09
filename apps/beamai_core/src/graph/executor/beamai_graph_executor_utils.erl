%%%-------------------------------------------------------------------
%%% @doc 图执行器 - 顶点管理、activation 处理、snapshot 工具
%%%
%%% 从 graph_executor 提取的纯函数模块。
%%% 负责顶点集合操作、activation 分离与聚合、snapshot 类型判定。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_executor_utils).

-export([get_all_vertices/2, vertices_to_map/1, merge_restored_vertices/2,
         filter_active_vertices/2, update_vertex_states/4, rebuild_graph/2,
         count_active/1,
         get_activations_for_superstep/2, separate_dispatches/1,
         build_vertex_inputs/1,
         determine_snapshot_type/1, build_superstep_info/2]).

-type vertex_id() :: beamai_graph_engine:vertex_id().
-type vertex() :: beamai_pregel_vertex:vertex().
-type graph() :: beamai_pregel_graph:graph().
-type snapshot_type() :: beamai_graph_engine:snapshot_type().
-type superstep_info() :: beamai_graph_engine:superstep_info().

%%====================================================================
%% 顶点管理
%%====================================================================

%% @doc 获取所有顶点（合并恢复的顶点）
-spec get_all_vertices(graph(), beamai_graph_engine:restore_opts() | undefined) ->
    #{vertex_id() => vertex()}.
get_all_vertices(Graph, RestoreOpts) ->
    BaseVertices = vertices_to_map(beamai_pregel_graph:vertices(Graph)),
    RestoredVertices = case RestoreOpts of
        #{vertices := V} -> V;
        _ -> #{}
    end,
    merge_restored_vertices(BaseVertices, RestoredVertices).

%% @doc 将顶点列表转换为映射
-spec vertices_to_map([vertex()]) -> #{vertex_id() => vertex()}.
vertices_to_map(Vertices) ->
    maps:from_list([{beamai_pregel_vertex:id(V), V} || V <- Vertices]).

%% @doc 合并恢复的顶点到基础顶点
-spec merge_restored_vertices(#{vertex_id() => vertex()}, #{vertex_id() => vertex()}) ->
    #{vertex_id() => vertex()}.
merge_restored_vertices(BaseVertices, RestoredVertices) ->
    maps:fold(
        fun(Id, RestoredVertex, Acc) ->
            case maps:is_key(Id, Acc) of
                true -> Acc#{Id => RestoredVertex};
                false -> Acc
            end
        end,
        BaseVertices,
        RestoredVertices
    ).

%% @doc 重建图
-spec rebuild_graph(graph(), #{vertex_id() => vertex()}) -> graph().
rebuild_graph(OriginalGraph, Vertices) ->
    beamai_pregel_graph:map(OriginalGraph, fun(Vertex) ->
        Id = beamai_pregel_vertex:id(Vertex),
        maps:get(Id, Vertices, Vertex)
    end).

%% @doc 筛选需要计算的顶点
-spec filter_active_vertices(#{vertex_id() => vertex()}, [vertex_id()]) ->
    #{vertex_id() => vertex()}.
filter_active_vertices(Vertices, Activations) ->
    ActivationSet = sets:from_list(Activations),
    maps:filter(
        fun(Id, V) ->
            sets:is_element(Id, ActivationSet) orelse beamai_pregel_vertex:is_active(V)
        end,
        Vertices
    ).

%% @doc 更新顶点状态
-spec update_vertex_states(
    #{vertex_id() => vertex()},
    #{vertex_id() => vertex()},
    [{vertex_id(), term()}],
    [{vertex_id(), term()}]
) -> #{vertex_id() => vertex()}.
update_vertex_states(AllVertices, ActiveVertices, _FailedVertices, _InterruptedVertices) ->
    ActiveIds = maps:keys(ActiveVertices),
    maps:map(
        fun(Id, V) ->
            case lists:member(Id, ActiveIds) of
                true -> beamai_pregel_vertex:halt(V);
                false -> V
            end
        end,
        AllVertices
    ).

%% @doc 统计活跃顶点数
-spec count_active(#{vertex_id() => vertex()}) -> non_neg_integer().
count_active(Vertices) ->
    beamai_pregel_utils:map_count(fun beamai_pregel_vertex:is_active/1, Vertices).

%%====================================================================
%% Activation 处理
%%====================================================================

%% @doc 获取下一超步要激活的顶点列表
-spec get_activations_for_superstep([vertex_id()] | undefined, map() | undefined) ->
    [vertex_id()].
get_activations_for_superstep(PendingActivations, _LastResults) when is_list(PendingActivations) ->
    PendingActivations;
get_activations_for_superstep(undefined, undefined) ->
    [];
get_activations_for_superstep(undefined, LastResults) ->
    maps:get(activations, LastResults, []).

%% @doc 分离 dispatch 项和普通 activations
-spec separate_dispatches([term()]) -> {[{dispatch, beamai_graph_dispatch:dispatch()}], [vertex_id()]}.
separate_dispatches(Activations) ->
    lists:partition(fun({dispatch, _}) -> true; (_) -> false end, Activations).

%% @doc 将 dispatch 列表转为 VertexInputs 格式
-spec build_vertex_inputs([{dispatch, beamai_graph_dispatch:dispatch()}]) ->
    #{vertex_id() => [beamai_graph_dispatch:dispatch()]}.
build_vertex_inputs(Dispatches) ->
    lists:foldl(fun({dispatch, D}, Acc) ->
        NodeId = beamai_graph_dispatch:get_node(D),
        Existing = maps:get(NodeId, Acc, []),
        Acc#{NodeId => Existing ++ [D]}
    end, #{}, Dispatches).

%%====================================================================
%% Snapshot 信息
%%====================================================================

%% @doc 确定 snapshot 类型
-spec determine_snapshot_type(map()) -> snapshot_type().
determine_snapshot_type(Results) ->
    InterruptedCount = maps:get(interrupted_count, Results, 0),
    FailedCount = maps:get(failed_count, Results, 0),
    if
        InterruptedCount > 0 -> interrupt;
        FailedCount > 0 -> error;
        true -> step
    end.

%% @doc 构建超步信息
-spec build_superstep_info(snapshot_type(), map() | undefined) -> superstep_info().
build_superstep_info(Type, undefined) ->
    #{
        type => Type,
        superstep => 0,
        active_count => 0,
        activation_count => 0,
        failed_count => 0,
        failed_vertices => [],
        interrupted_count => 0,
        interrupted_vertices => []
    };
build_superstep_info(Type, Results) ->
    #{
        type => Type,
        superstep => maps:get(superstep, Results, 0),
        active_count => maps:get(active_count, Results, 0),
        activation_count => maps:get(activation_count, Results, 0),
        failed_count => maps:get(failed_count, Results, 0),
        failed_vertices => maps:get(failed_vertices, Results, []),
        interrupted_count => maps:get(interrupted_count, Results, 0),
        interrupted_vertices => maps:get(interrupted_vertices, Results, [])
    }.
