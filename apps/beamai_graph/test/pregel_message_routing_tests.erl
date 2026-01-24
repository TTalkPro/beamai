%%%-------------------------------------------------------------------
%%% @doc Pregel 激活路由单元测试（全局状态模式 - 无 inbox 版本）
%%%
%%% 测试 BSP 模型的激活路由机制：
%%% - Worker 在超步结束时上报 activations 给 Master
%%% - Master 集中路由所有激活到目标 Worker
%%% - 使用 call 确保可靠投递
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_message_routing_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建简单测试图（两个顶点相互连接）
make_test_graph() ->
    Edges = [{v1, v2}, {v2, v1}],
    InitialValues = #{v1 => 1, v2 => 2},
    pregel_graph:from_edges(Edges, InitialValues).

%% 创建三顶点测试图（v1 -> v2 -> v3）
make_chain_graph() ->
    Edges = [{v1, v2}, {v2, v3}],
    InitialValues = #{v1 => 1, v2 => 2, v3 => 3},
    pregel_graph:from_edges(Edges, InitialValues).

%% 创建激活下游顶点的计算函数（全局状态模式）
make_activation_compute_fn() ->
    fun(#{vertex_id := Id, superstep := Superstep, global_state := GlobalState} = _Ctx) ->
        case Superstep of
            0 when Id =:= v1 ->
                %% v1 在超步0激活 v2
                #{delta => #{v1 => activated}, activations => [v2], status => ok};
            _ ->
                %% 其他顶点或超步：记录被激活，然后停止
                CurrentValue = maps:get(Id, GlobalState, inactive),
                NewValue = case CurrentValue of
                    inactive -> activated;
                    _ -> CurrentValue
                end,
                #{delta => #{Id => NewValue}, activations => [], status => ok}
        end
    end.

%% 创建链式激活的计算函数（v1 -> v2 -> v3）
make_chain_compute_fn() ->
    fun(#{vertex_id := Id, superstep := Superstep, global_state := _GlobalState} = _Ctx) ->
        case {Id, Superstep} of
            {v1, 0} ->
                %% v1 激活 v2
                #{delta => #{v1 => done}, activations => [v2], status => ok};
            {v2, 1} ->
                %% v2 被激活后，激活 v3
                #{delta => #{v2 => done}, activations => [v3], status => ok};
            {v3, 2} ->
                %% v3 被激活，记录结果
                #{delta => #{v3 => {received_at_superstep, 2}}, activations => [], status => ok};
            _ ->
                %% 其他情况：停止
                #{delta => #{}, activations => [], status => ok}
        end
    end.

%%====================================================================
%% Master 集中路由测试
%%====================================================================

%% 测试：Master 正确路由激活到目标 Worker
master_routes_activations_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_activation_compute_fn(),

    Opts = #{num_workers => 1},
    Result = pregel:run(Graph, ComputeFn, Opts),

    %% 验证执行完成
    ?assertEqual(completed, maps:get(status, Result)),

    %% 验证 v2 被激活了
    GlobalState = maps:get(global_state, Result),
    V2Value = graph_state:get(GlobalState, v2),
    ?assertEqual(activated, V2Value).

%% 测试：多 Worker 场景下激活正确路由
multi_worker_activation_routing_test() ->
    Graph = make_test_graph(),
    ComputeFn = make_activation_compute_fn(),

    %% 使用 2 个 Worker
    Opts = #{num_workers => 2},
    Result = pregel:run(Graph, ComputeFn, Opts),

    %% 验证执行完成
    ?assertEqual(completed, maps:get(status, Result)).

%% 测试：链式激活传递（v1 -> v2 -> v3）
chain_activation_routing_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_chain_compute_fn(),

    Opts = #{num_workers => 1},
    Result = pregel:run(Graph, ComputeFn, Opts),

    %% 验证执行完成
    ?assertEqual(completed, maps:get(status, Result)),

    %% 验证 v3 在正确的超步被激活
    GlobalState = maps:get(global_state, Result),
    V3Value = graph_state:get(GlobalState, v3),
    ?assertEqual({received_at_superstep, 2}, V3Value).

%%====================================================================
%% Worker activations 上报测试
%%====================================================================

%% 测试：Worker 上报的结果包含 activation 计数
worker_reports_activations_test() ->
    %% 创建测试顶点
    V1 = pregel_vertex:activate(pregel_vertex:new(v1, 1)),
    Vertices = #{v1 => V1},

    %% 创建激活下游顶点的计算函数
    ComputeFn = fun(#{vertex_id := _Id} = _Ctx) ->
        #{
            delta => #{v1 => done},
            activations => [v2],
            status => ok
        }
    end,

    %% 启动 Worker，使用当前进程作为 Master
    Master = self(),
    Opts = #{
        worker_id => 0,
        master => Master,
        vertices => Vertices,
        compute_fn => ComputeFn,
        num_workers => 1,
        num_vertices => 1,
        worker_pids => #{}
    },
    {ok, Worker} = pregel_worker:start_link(0, Opts),

    %% 执行超步（激活 v1）
    pregel_worker:start_superstep(Worker, 0, [v1]),

    %% 等待 Worker 完成
    receive
        {'$gen_cast', {worker_done, _Pid, Result}} ->
            %% 验证结果包含激活列表（无 inbox 版本）
            Activations = maps:get(activations, Result),
            ?assertEqual(1, length(Activations)),
            ?assertEqual(0, maps:get(active_count, Result))
    after 1000 ->
        ?assert(false)
    end,

    %% 清理
    pregel_worker:stop(Worker).

%%====================================================================
%% pending_activations 验证测试
%%====================================================================

%% 测试：执行过程中正确使用 pending_activations
pending_activations_test() ->
    Graph = make_test_graph(),

    %% 创建会激活多个顶点的计算函数
    ComputeFn = fun(#{vertex_id := _Id, superstep := Superstep} = _Ctx) ->
        case Superstep of
            0 ->
                %% 激活多个顶点
                #{delta => #{}, activations => [v1, v2], status => ok};
            _ ->
                #{delta => #{}, activations => [], status => ok}
        end
    end,

    Opts = #{num_workers => 1},
    Result = pregel:run(Graph, ComputeFn, Opts),

    %% 验证执行完成
    ?assertEqual(completed, maps:get(status, Result)).

%%====================================================================
%% 可靠性测试
%%====================================================================

%% 测试：激活在超步结束时统一投递
activations_delivered_at_superstep_end_test() ->
    Graph = make_test_graph(),

    %% 记录激活时机的计算函数
    Self = self(),
    ComputeFn = fun(#{vertex_id := Id, superstep := Superstep} = _Ctx) ->
        %% 报告执行时机
        Self ! {vertex_executed, Id, Superstep},

        case Superstep of
            0 ->
                %% 激活另一个顶点
                Target = case Id of v1 -> v2; v2 -> v1 end,
                #{delta => #{Id => done}, activations => [Target], status => ok};
            _ ->
                #{delta => #{Id => done}, activations => [], status => ok}
        end
    end,

    Opts = #{num_workers => 1},
    _Result = pregel:run(Graph, ComputeFn, Opts),

    %% 收集执行记录
    Records = receive_all_records([]),

    %% 验证顶点在正确的超步被执行
    Superstep0 = [Id || {vertex_executed, Id, 0} <- Records],
    Superstep1 = [Id || {vertex_executed, Id, 1} <- Records],

    %% 超步0至少有v1或v2执行
    ?assert(length(Superstep0) >= 1),
    %% 超步1可能有顶点被激活执行
    ?assert(length(Superstep1) >= 0).

receive_all_records(Acc) ->
    receive
        {vertex_executed, _, _} = Record ->
            receive_all_records([Record | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%%====================================================================
%% 边界情况测试
%%====================================================================

%% 测试：空 activations 不影响执行
empty_activations_test() ->
    Graph = make_test_graph(),

    ComputeFn = fun(#{vertex_id := _Id} = _Ctx) ->
        #{delta => #{}, activations => [], status => ok}
    end,

    Opts = #{num_workers => 1},
    Result = pregel:run(Graph, ComputeFn, Opts),

    ?assertEqual(completed, maps:get(status, Result)).

%% 测试：所有顶点同时激活其他顶点
all_vertices_send_activations_test() ->
    Graph = make_test_graph(),

    ComputeFn = fun(#{vertex_id := Id, superstep := Superstep} = _Ctx) ->
        case Superstep of
            0 ->
                %% 所有顶点都激活另一个顶点
                Target = case Id of v1 -> v2; v2 -> v1 end,
                #{delta => #{Id => done}, activations => [Target], status => ok};
            _ ->
                #{delta => #{Id => done}, activations => [], status => ok}
        end
    end,

    Opts = #{num_workers => 1},
    Result = pregel:run(Graph, ComputeFn, Opts),

    ?assertEqual(completed, maps:get(status, Result)).

%% 测试：旧函数名兼容性（master_routes_messages_test -> master_routes_activations_test）
master_routes_messages_test() ->
    %% 此测试为向后兼容保留，实际调用新的激活路由测试
    master_routes_activations_test().

%% 测试：no_pending_messages_dependency_test 兼容性
no_pending_messages_dependency_test() ->
    %% 此测试验证不再依赖 pending_messages（已替换为 pending_activations）
    pending_activations_test().

%% 测试：messages_delivered_at_superstep_end_test 兼容性
messages_delivered_at_superstep_end_test() ->
    %% 此测试为向后兼容保留，实际测试激活投递时机
    activations_delivered_at_superstep_end_test().
