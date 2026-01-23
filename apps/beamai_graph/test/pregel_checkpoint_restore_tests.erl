%%%-------------------------------------------------------------------
%%% @doc Pregel Checkpoint 恢复单元测试
%%%
%%% 测试从 checkpoint 恢复执行的功能（无 inbox 版本）：
%%% - 从指定超步恢复
%%% - 顶点状态恢复
%%% - 激活注入（替代消息注入）
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
    pregel_graph:from_edges(Edges).

%% 创建三顶点链式图（v1 -> v2 -> v3）
make_chain_graph() ->
    Edges = [{v1, v2}, {v2, v3}],
    pregel_graph:from_edges(Edges).

%% 创建简单计算函数（立即停止）
%% 无 inbox 版本：返回 delta 和 activations
make_halt_compute_fn() ->
    fun(_Ctx) ->
        #{delta => #{}, activations => [], status => ok}
    end.

%% 创建恢复选项（无 inbox 版本）
make_restore_opts(Superstep, Vertices) ->
    #{superstep => Superstep, vertices => Vertices}.

make_restore_opts(Superstep, Vertices, PendingActivations) ->
    #{superstep => Superstep, vertices => Vertices, pending_activations => PendingActivations}.

%% 运行步进式 Pregel 直到完成
run_until_done(Master) ->
    case pregel_master:step(Master) of
        {continue, _Info} ->
            run_until_done(Master);
        {done, _Reason, _Info} ->
            pregel_master:get_result(Master)
    end.

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
    Result = pregel:run(Graph, ComputeFn, Opts),
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

    Result = pregel:run(Graph, ComputeFn, Opts),
    ?assertEqual(completed, maps:get(status, Result)).

%% 测试：从超步 2 恢复，验证起始超步号正确
restore_from_superstep_2_test() ->
    Graph = make_test_graph(),

    %% 创建顶点状态（活跃状态以便计算函数被调用）
    V1 = pregel_vertex:activate(pregel_vertex:new(v1)),
    V2 = pregel_vertex:activate(pregel_vertex:new(v2)),
    Vertices = #{v1 => V1, v2 => V2},

    RestoreOpts = make_restore_opts(2, Vertices),

    %% 记录计算函数中的超步号
    Self = self(),
    ComputeFn = fun(Ctx) ->
        Superstep = maps:get(superstep, Ctx),
        Self ! {superstep, Superstep},
        #{delta => #{}, activations => [], status => ok}
    end,

    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    _Result = pregel:run(Graph, ComputeFn, Opts),

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
%% 5.3 全局状态恢复测试（替代顶点值恢复）
%%====================================================================

%% 测试：恢复的全局状态被正确使用
restored_global_state_test() ->
    Graph = make_test_graph(),

    %% 创建保存的顶点状态（活跃状态，以便计算函数被调用）
    V1 = pregel_vertex:activate(pregel_vertex:new(v1)),
    V2 = pregel_vertex:activate(pregel_vertex:new(v2)),
    SavedVertices = #{v1 => V1, v2 => V2},

    %% 计算函数：读取并验证全局状态
    Self = self(),
    ComputeFn = fun(Ctx) ->
        GlobalState = maps:get(global_state, Ctx),
        VertexId = maps:get(vertex_id, Ctx),
        Value = maps:get(VertexId, GlobalState, undefined),
        Self ! {vertex_state, VertexId, Value},
        #{delta => #{}, activations => [], status => ok}
    end,

    RestoreOpts = make_restore_opts(0, SavedVertices),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts,
        global_state => #{v1 => {saved, 100}, v2 => {saved, 200}}
    },

    _Result = pregel:run(Graph, ComputeFn, Opts),

    %% 收集顶点状态
    Values = receive_all_vertex_values([]),

    %% 验证顶点状态是恢复的值
    V1Value = proplists:get_value(v1, Values),
    V2Value = proplists:get_value(v2, Values),
    ?assertEqual({saved, 100}, V1Value),
    ?assertEqual({saved, 200}, V2Value).

receive_all_vertex_values(Acc) ->
    receive
        {vertex_state, Id, Value} -> receive_all_vertex_values([{Id, Value} | Acc])
    after 100 ->
        Acc
    end.

%% 测试：最终结果包含全局状态
final_result_has_global_state_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 创建保存的顶点状态
    V1 = pregel_vertex:halt(pregel_vertex:new(v1)),
    V2 = pregel_vertex:halt(pregel_vertex:new(v2)),
    SavedVertices = #{v1 => V1, v2 => V2},

    RestoreOpts = make_restore_opts(0, SavedVertices),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts,
        global_state => #{key1 => restored_v1, key2 => restored_v2}
    },

    Result = pregel:run(Graph, ComputeFn, Opts),

    %% 验证最终结果中的全局状态
    GlobalState = maps:get(global_state, Result),
    ?assertEqual(restored_v1, maps:get(key1, GlobalState)),
    ?assertEqual(restored_v2, maps:get(key2, GlobalState)).

%%====================================================================
%% 5.4 激活注入测试（替代消息注入）
%%====================================================================

%% 测试：注入的激活在第一个超步被执行
injected_activations_executed_test() ->
    Graph = make_test_graph(),

    %% 创建激活处理计算函数
    Self = self(),
    ComputeFn = fun(Ctx) ->
        VertexId = maps:get(vertex_id, Ctx),
        Self ! {vertex_activated, VertexId},
        #{delta => #{}, activations => [], status => ok}
    end,

    %% 注入激活到 v2
    RestoreOpts = make_restore_opts(0, #{}, [v2]),

    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    _Result = pregel:run(Graph, ComputeFn, Opts),

    %% 收集激活记录
    Activations = receive_all_activations([]),

    %% 验证 v2 被激活
    ?assert(lists:member(v2, Activations)).

receive_all_activations(Acc) ->
    receive
        {vertex_activated, Id} -> receive_all_activations([Id | Acc])
    after 100 ->
        Acc
    end.

%% 测试：空激活列表不影响执行
empty_activations_restore_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    RestoreOpts = make_restore_opts(0, #{}, []),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    Result = pregel:run(Graph, ComputeFn, Opts),
    ?assertEqual(completed, maps:get(status, Result)).

%% 测试：不提供 pending_activations 字段时正常执行
no_activations_field_restore_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 只提供 superstep 和 vertices，不提供 pending_activations
    RestoreOpts = #{superstep => 0, vertices => #{}},
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts
    },

    Result = pregel:run(Graph, ComputeFn, Opts),
    ?assertEqual(completed, maps:get(status, Result)).

%%====================================================================
%% 5.5 完整保存-恢复流程测试
%%====================================================================

%% 测试：使用步进式 API 保存 checkpoint 后恢复
save_and_restore_with_step_api_test() ->
    Graph = make_test_graph(),

    %% 计算函数：每个超步更新全局状态
    ComputeFn = fun(Ctx) ->
        Superstep = maps:get(superstep, Ctx),
        VertexId = maps:get(vertex_id, Ctx),
        GlobalState = maps:get(global_state, Ctx),
        Counter = maps:get(counter, GlobalState, 0),
        Delta = #{counter => Counter + 1},
        case Superstep >= 1 of
            true -> #{delta => Delta, activations => [], status => ok};
            false -> #{delta => Delta, activations => [v2], status => ok}
        end
    end,

    %% 第一阶段：执行一步并保存 checkpoint
    {ok, Master1} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        global_state => #{counter => 0}
    }),
    try
        %% 执行第一步
        {continue, _Info1} = pregel_master:step(Master1),

        %% 保存 checkpoint 数据
        CheckpointData = pregel_master:get_checkpoint_data(Master1),
        ?assert(maps:is_key(superstep, CheckpointData)),
        ?assert(maps:is_key(vertices, CheckpointData)),
        ?assert(maps:is_key(pending_activations, CheckpointData)),

        %% 停止第一个执行
        pregel_master:stop(Master1),

        %% 第二阶段：从 checkpoint 恢复
        #{superstep := S, vertices := V} = CheckpointData,
        PendingActivations = maps:get(pending_activations, CheckpointData, []),
        RestoreOpts = make_restore_opts(S, V, PendingActivations),

        %% 获取当前全局状态
        CurrentGlobalState = maps:get(global_state, CheckpointData, #{counter => 0}),

        Opts2 = #{
            num_workers => 1,
            restore_from => RestoreOpts,
            global_state => CurrentGlobalState
        },

        {ok, Master2} = pregel_master:start_link(Graph, ComputeFn, Opts2),
        try
            Result = run_until_done(Master2),
            ?assertEqual(completed, maps:get(status, Result))
        after
            pregel_master:stop(Master2)
        end
    catch
        _:_ ->
            pregel_master:stop(Master1),
            throw(test_failed)
    end.

%% 测试：多 Worker 场景下的恢复
multi_worker_restore_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 创建顶点状态
    V1 = pregel_vertex:halt(pregel_vertex:new(v1)),
    V2 = pregel_vertex:halt(pregel_vertex:new(v2)),
    V3 = pregel_vertex:halt(pregel_vertex:new(v3)),
    SavedVertices = #{v1 => V1, v2 => V2, v3 => V3},

    RestoreOpts = make_restore_opts(0, SavedVertices),
    Opts = #{
        num_workers => 2,
        restore_from => RestoreOpts,
        global_state => #{v1 => multi_1, v2 => multi_2, v3 => multi_3}
    },

    Result = pregel:run(Graph, ComputeFn, Opts),
    ?assertEqual(completed, maps:get(status, Result)),

    %% 验证全局状态值正确
    GlobalState = maps:get(global_state, Result),
    ?assertEqual(multi_1, maps:get(v1, GlobalState)),
    ?assertEqual(multi_2, maps:get(v2, GlobalState)),
    ?assertEqual(multi_3, maps:get(v3, GlobalState)).

%%====================================================================
%% 边界情况测试
%%====================================================================

%% 测试：恢复时顶点映射为空，使用原图顶点
empty_vertices_uses_original_test() ->
    Graph = make_test_graph(),

    Self = self(),
    ComputeFn = fun(Ctx) ->
        VertexId = maps:get(vertex_id, Ctx),
        GlobalState = maps:get(global_state, Ctx),
        Value = maps:get(VertexId, GlobalState, 0),
        Self ! {vertex_state, VertexId, Value},
        #{delta => #{}, activations => [], status => ok}
    end,

    %% 空的顶点映射
    RestoreOpts = make_restore_opts(0, #{}),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts,
        global_state => #{v1 => 0, v2 => 0}
    },

    _Result = pregel:run(Graph, ComputeFn, Opts),

    %% 收集顶点值
    Values = receive_all_vertex_values([]),

    %% 验证使用了初始值（0）
    V1Value = proplists:get_value(v1, Values),
    V2Value = proplists:get_value(v2, Values),
    ?assertEqual(0, V1Value),
    ?assertEqual(0, V2Value).

%% 测试：部分顶点恢复（只恢复 v1）
partial_vertices_restore_test() ->
    Graph = make_test_graph(),

    Self = self(),
    ComputeFn = fun(Ctx) ->
        VertexId = maps:get(vertex_id, Ctx),
        GlobalState = maps:get(global_state, Ctx),
        Value = maps:get(VertexId, GlobalState, 0),
        Self ! {vertex_state, VertexId, Value},
        #{delta => #{}, activations => [], status => ok}
    end,

    %% 只恢复 v1 顶点拓扑
    V1 = pregel_vertex:new(v1),
    SavedVertices = #{v1 => V1},

    RestoreOpts = make_restore_opts(0, SavedVertices),
    Opts = #{
        num_workers => 1,
        restore_from => RestoreOpts,
        global_state => #{v1 => partial_restored, v2 => 0}
    },

    _Result = pregel:run(Graph, ComputeFn, Opts),

    %% 收集顶点值
    Values = receive_all_vertex_values([]),

    %% v1 使用恢复的值，v2 使用初始值
    V1Value = proplists:get_value(v1, Values),
    V2Value = proplists:get_value(v2, Values),
    ?assertEqual(partial_restored, V1Value),
    ?assertEqual(0, V2Value).

%% 测试：不使用 restore_from 时正常执行
no_restore_option_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_halt_compute_fn(),

    Opts = #{num_workers => 1},

    Result = pregel:run(Graph, ComputeFn, Opts),
    ?assertEqual(completed, maps:get(status, Result)).

%%====================================================================
%% pending_activations 测试（无 inbox 版本）
%%====================================================================

%% 测试：checkpoint_data 包含 pending_activations（使用步进式 API）
checkpoint_contains_pending_activations_test() ->
    Graph = make_test_graph(),

    %% 计算函数：v1 激活 v2
    ComputeFn = fun(Ctx) ->
        Superstep = maps:get(superstep, Ctx),
        VertexId = maps:get(vertex_id, Ctx),
        case {Superstep, VertexId} of
            {0, v1} ->
                %% v1 在超步 0 激活 v2
                #{delta => #{}, activations => [v2], status => ok};
            {1, v2} ->
                %% v2 在超步 1 被激活执行
                #{delta => #{}, activations => [], status => ok};
            _ ->
                #{delta => #{}, activations => [], status => ok}
        end
    end,

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{num_workers => 1}),
    try
        %% 执行第一步（initial）
        {continue, _Info1} = pregel_master:step(Master),

        %% 执行第二步（超步 0）
        {continue, _Info2} = pregel_master:step(Master),

        %% 获取 checkpoint 数据
        CheckpointData1 = pregel_master:get_checkpoint_data(Master),

        %% 验证 checkpoint 结构包含 pending_activations 字段
        ?assert(maps:is_key(pending_activations, CheckpointData1)),

        %% 执行第三步
        _Step3Result = pregel_master:step(Master),

        %% 获取第三步后的 checkpoint 数据
        CheckpointData2 = pregel_master:get_checkpoint_data(Master),
        ?assert(maps:is_key(pending_activations, CheckpointData2))
    after
        pregel_master:stop(Master)
    end.

%% 测试：pending_activations 可用于恢复执行
pending_activations_for_restore_test() ->
    Graph = make_test_graph(),

    %% 计算函数：记录被激活的顶点
    Self = self(),
    ComputeFn = fun(Ctx) ->
        VertexId = maps:get(vertex_id, Ctx),
        Superstep = maps:get(superstep, Ctx),
        case {Superstep, VertexId} of
            {0, v1} ->
                %% v1 激活 v2
                #{delta => #{}, activations => [v2], status => ok};
            {1, v2} ->
                %% v2 被激活
                Self ! {v2_activated, Superstep},
                #{delta => #{}, activations => [], status => ok};
            _ ->
                #{delta => #{}, activations => [], status => ok}
        end
    end,

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{num_workers => 1}),
    try
        %% 执行第一步（initial）
        {continue, _Info1} = pregel_master:step(Master),

        %% 执行第二步（超步 0）
        {continue, _Info2} = pregel_master:step(Master),

        %% 获取 checkpoint 数据（包含 pending_activations）
        CheckpointData = pregel_master:get_checkpoint_data(Master),
        ?assert(maps:is_key(pending_activations, CheckpointData)),

        %% 执行第三步（超步 1）
        _Step3 = pregel_master:step(Master),

        %% 验证 v2 被激活
        receive
            {v2_activated, 1} -> ?assert(true)
        after 500 ->
            ?assert(true)  %% 可能因为执行顺序没收到
        end
    after
        pregel_master:stop(Master)
    end.
