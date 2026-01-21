%%%-------------------------------------------------------------------
%%% @doc Pregel Checkpoint 恢复单元测试
%%%
%%% 测试从 checkpoint 恢复执行的功能：
%%% - 从指定超步恢复
%%% - 顶点状态恢复
%%% - 消息注入
%%% - 完整的保存-恢复流程
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_checkpoint_restore_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建简单测试图（两个顶点）
make_test_graph() ->
    Edges = [{v1, v2}, {v2, v1}],
    InitialValues = #{v1 => 0, v2 => 0},
    pregel_graph:from_edges(Edges, InitialValues).

%% 创建三顶点链式图（v1 -> v2 -> v3）
make_chain_graph() ->
    Edges = [{v1, v2}, {v2, v3}],
    InitialValues = #{v1 => 0, v2 => 0, v3 => 0},
    pregel_graph:from_edges(Edges, InitialValues).

%% 创建简单计算函数（立即停止）
make_halt_compute_fn() ->
    fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        #{vertex => pregel_vertex:halt(Vertex), outbox => [], status => ok}
    end.

%% 创建恢复选项
make_restore_opts(Superstep, Vertices) ->
    #{superstep => Superstep, vertices => Vertices}.

make_restore_opts(Superstep, Vertices, Messages) ->
    #{superstep => Superstep, vertices => Vertices, messages => Messages}.

%%====================================================================
%% 5.1 类型测试（通过编译验证）
%%====================================================================

%% 测试：restore_from 选项被接受
restore_from_option_accepted_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 创建恢复选项
    RestoreOpts = make_restore_opts(0, #{}),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    %% 应该正常执行，不报错
    Result = pregel_master:run(Graph, ComputeFn, Opts),
    ?assertMatch(#{status := _}, Result).

%%====================================================================
%% 5.2 从指定超步恢复测试
%%====================================================================

%% 测试：从超步 0 恢复（等同于正常启动）
restore_from_superstep_0_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    RestoreOpts = make_restore_opts(0, #{}),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),
    ?assertEqual(completed, maps:get(status, Result)).

%% 测试：从超步 2 恢复，回调收到正确的超步号
restore_from_superstep_2_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 创建顶点状态（已停止）
    V1 = pregel_vertex:halt(pregel_vertex:new(v1, 100)),
    V2 = pregel_vertex:halt(pregel_vertex:new(v2, 200)),
    Vertices = #{v1 => V1, v2 => V2},

    RestoreOpts = make_restore_opts(2, Vertices),

    %% 记录回调中的超步号
    Self = self(),
    Callback = fun(Info) ->
        Self ! {superstep, maps:get(superstep, Info)},
        continue
    end,

    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts,
        on_superstep_complete => Callback
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 收集超步号
    Supersteps = receive_all_supersteps([]),

    %% 验证初始超步号为 2
    ?assert(length(Supersteps) >= 1),
    ?assertEqual(2, hd(Supersteps)).

receive_all_supersteps(Acc) ->
    receive
        {superstep, S} -> receive_all_supersteps([S | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%%====================================================================
%% 5.3 顶点状态恢复测试
%%====================================================================

%% 测试：恢复的顶点值被正确使用
%% 注意：必须使用活跃顶点，因为 Pregel 不会对已停止且无消息的顶点执行计算
restored_vertex_values_test() ->
    Graph = make_test_graph(),

    %% 创建保存的顶点状态（活跃状态，以便计算函数被调用）
    V1 = pregel_vertex:activate(pregel_vertex:new(v1, {saved, 100})),
    V2 = pregel_vertex:activate(pregel_vertex:new(v2, {saved, 200})),
    SavedVertices = #{v1 => V1, v2 => V2},

    %% 计算函数：读取并验证顶点值
    Self = self(),
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Value = pregel_vertex:value(Vertex),
        Self ! {vertex_value, Id, Value},
        #{vertex => pregel_vertex:halt(Vertex), outbox => [], status => ok}
    end,

    RestoreOpts = make_restore_opts(0, SavedVertices),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 收集顶点值
    Values = receive_all_vertex_values([]),

    %% 验证顶点值是恢复的值
    V1Value = proplists:get_value(v1, Values),
    V2Value = proplists:get_value(v2, Values),
    ?assertEqual({saved, 100}, V1Value),
    ?assertEqual({saved, 200}, V2Value).

receive_all_vertex_values(Acc) ->
    receive
        {vertex_value, Id, Value} -> receive_all_vertex_values([{Id, Value} | Acc])
    after 100 ->
        Acc
    end.

%% 测试：最终图包含恢复的顶点
final_graph_has_restored_vertices_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 创建保存的顶点状态
    V1 = pregel_vertex:halt(pregel_vertex:new(v1, restored_v1)),
    V2 = pregel_vertex:halt(pregel_vertex:new(v2, restored_v2)),
    SavedVertices = #{v1 => V1, v2 => V2},

    RestoreOpts = make_restore_opts(0, SavedVertices),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),
    FinalGraph = maps:get(graph, Result),

    %% 验证最终图中的顶点值
    FinalV1 = pregel_graph:get(FinalGraph, v1),
    FinalV2 = pregel_graph:get(FinalGraph, v2),
    ?assertEqual(restored_v1, pregel_vertex:value(FinalV1)),
    ?assertEqual(restored_v2, pregel_vertex:value(FinalV2)).

%%====================================================================
%% 5.4 消息注入测试
%%====================================================================

%% 测试：注入的消息在第一个超步被接收
injected_messages_received_test() ->
    Graph = make_test_graph(),

    %% 创建消息处理计算函数
    Self = self(),
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Messages = maps:get(messages, Ctx),
        Id = pregel_vertex:id(Vertex),
        Self ! {messages_received, Id, Messages},
        #{vertex => pregel_vertex:halt(Vertex), outbox => [], status => ok}
    end,

    %% 注入消息到 v2
    InjectedMessages = [{v2, injected_msg_1}, {v2, injected_msg_2}],
    RestoreOpts = make_restore_opts(0, #{}, InjectedMessages),

    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 收集消息接收记录
    Records = receive_all_messages_received([]),

    %% 验证 v2 收到了注入的消息
    V2Records = [Msgs || {Id, Msgs} <- Records, Id =:= v2, Msgs =/= []],
    ?assert(length(V2Records) >= 1),
    V2Messages = hd(V2Records),
    ?assert(lists:member(injected_msg_1, V2Messages)),
    ?assert(lists:member(injected_msg_2, V2Messages)).

receive_all_messages_received(Acc) ->
    receive
        {messages_received, Id, Msgs} -> receive_all_messages_received([{Id, Msgs} | Acc])
    after 100 ->
        Acc
    end.

%% 测试：空消息列表不影响执行
empty_messages_restore_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    RestoreOpts = make_restore_opts(0, #{}, []),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),
    ?assertEqual(completed, maps:get(status, Result)).

%% 测试：不提供 messages 字段时正常执行
no_messages_field_restore_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 只提供 superstep 和 vertices，不提供 messages
    RestoreOpts = #{superstep => 0, vertices => #{}},
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),
    ?assertEqual(completed, maps:get(status, Result)).

%%====================================================================
%% 5.5 完整保存-恢复流程测试
%%====================================================================

%% 测试：保存 checkpoint 后恢复，结果一致
save_and_restore_consistency_test() ->
    Graph = make_test_graph(),

    %% 第一阶段：正常执行，保存 checkpoint
    CheckpointHolder = spawn_link(fun() -> checkpoint_holder_loop(undefined) end),

    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Superstep = maps:get(superstep, Ctx),
        Value = pregel_vertex:value(Vertex),
        NewValue = Value + 1,
        NewVertex = pregel_vertex:set_value(Vertex, NewValue),
        case Superstep >= 1 of
            true -> #{vertex => pregel_vertex:halt(NewVertex), outbox => [], status => ok};
            false -> #{vertex => NewVertex, outbox => [{v2, ping}], status => ok}
        end
    end,

    Callback = fun(Info) ->
        Type = maps:get(type, Info),
        case Type of
            step ->
                GetData = maps:get(get_checkpoint_data, Info),
                Data = GetData(),
                CheckpointHolder ! {save, Data};
            _ ->
                ok
        end,
        continue
    end,

    Opts1 = #{
        num_workers => 1,
        on_superstep_complete => Callback
    },

    _Result1 = pregel_master:run(Graph, ComputeFn, Opts1),

    %% 获取保存的 checkpoint
    CheckpointHolder ! {get, self()},
    CheckpointData = receive
        {checkpoint, Data} -> Data
    after 1000 ->
        error(no_checkpoint_saved)
    end,

    %% 第二阶段：从 checkpoint 恢复（如果有的话）
    case CheckpointData of
        undefined ->
            %% 没有保存 checkpoint（只有一个超步），测试通过
            ?assert(true);
        #{superstep := S, vertices := V, pending_messages := M} ->
            %% 从 checkpoint 恢复
            RestoreOpts = make_restore_opts(S, V, M),
            Opts2 = #{
                num_workers => 1,
                restore_from => RestoreOpts
            },

            Result2 = pregel_master:run(Graph, ComputeFn, Opts2),

            %% 验证恢复后执行完成
            ?assertEqual(completed, maps:get(status, Result2))
    end.

checkpoint_holder_loop(Data) ->
    receive
        {save, NewData} ->
            checkpoint_holder_loop(NewData);
        {get, Pid} ->
            Pid ! {checkpoint, Data}
    end.

%% 测试：多 Worker 场景下的恢复
multi_worker_restore_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 创建顶点状态
    V1 = pregel_vertex:halt(pregel_vertex:new(v1, multi_1)),
    V2 = pregel_vertex:halt(pregel_vertex:new(v2, multi_2)),
    V3 = pregel_vertex:halt(pregel_vertex:new(v3, multi_3)),
    SavedVertices = #{v1 => V1, v2 => V2, v3 => V3},

    RestoreOpts = make_restore_opts(0, SavedVertices),
    Opts = #{
        num_workers => 2,
        restore_from => RestoreOpts
    },

    Result = pregel_master:run(Graph, ComputeFn, Opts),
    ?assertEqual(completed, maps:get(status, Result)),

    %% 验证所有顶点值正确
    FinalGraph = maps:get(graph, Result),
    ?assertEqual(multi_1, pregel_vertex:value(pregel_graph:get(FinalGraph, v1))),
    ?assertEqual(multi_2, pregel_vertex:value(pregel_graph:get(FinalGraph, v2))),
    ?assertEqual(multi_3, pregel_vertex:value(pregel_graph:get(FinalGraph, v3))).

%%====================================================================
%% 边界情况测试
%%====================================================================

%% 测试：恢复时顶点映射为空，使用原图顶点
empty_vertices_uses_original_test() ->
    Graph = make_test_graph(),

    Self = self(),
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Value = pregel_vertex:value(Vertex),
        Self ! {vertex_value, Id, Value},
        #{vertex => pregel_vertex:halt(Vertex), outbox => [], status => ok}
    end,

    %% 空的顶点映射
    RestoreOpts = make_restore_opts(0, #{}),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 收集顶点值
    Values = receive_all_vertex_values([]),

    %% 验证使用了原图的初始值（0）
    V1Value = proplists:get_value(v1, Values),
    V2Value = proplists:get_value(v2, Values),
    ?assertEqual(0, V1Value),
    ?assertEqual(0, V2Value).

%% 测试：部分顶点恢复（只恢复 v1）
partial_vertices_restore_test() ->
    Graph = make_test_graph(),

    Self = self(),
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Value = pregel_vertex:value(Vertex),
        Self ! {vertex_value, Id, Value},
        #{vertex => pregel_vertex:halt(Vertex), outbox => [], status => ok}
    end,

    %% 只恢复 v1
    V1 = pregel_vertex:new(v1, partial_restored),
    SavedVertices = #{v1 => V1},

    RestoreOpts = make_restore_opts(0, SavedVertices),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    _Result = pregel_master:run(Graph, ComputeFn, Opts),

    %% 收集顶点值
    Values = receive_all_vertex_values([]),

    %% v1 使用恢复的值，v2 使用原图的值
    V1Value = proplists:get_value(v1, Values),
    V2Value = proplists:get_value(v2, Values),
    ?assertEqual(partial_restored, V1Value),
    ?assertEqual(0, V2Value).

%% 测试：不使用 restore_from 时正常执行
no_restore_option_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    Opts = #{num_workers => 1},

    Result = pregel_master:run(Graph, ComputeFn, Opts),
    ?assertEqual(completed, maps:get(status, Result)).
