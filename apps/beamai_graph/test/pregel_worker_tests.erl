%%%-------------------------------------------------------------------
%%% @doc pregel_worker 错误处理与中断支持单元测试
%%%
%%% 测试 pregel_worker 的错误处理和中断能力：
%%% - compute_result 数据结构契约（ok/error/interrupt）
%%% - 全局状态模式下返回 delta 和 activations
%%% - notify_master_done 上报失败和中断信息
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_worker_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建测试用的顶点（全局状态模式：不含 value）
make_test_vertex(Id) ->
    pregel_vertex:new(Id).

%% 创建成功的计算函数
%% 全局状态模式：计算函数返回 #{delta, activations, status => ok}
make_success_compute_fn() ->
    fun(Ctx) ->
        #{vertex_id := Id, global_state := State} = Ctx,
        %% 成功时：返回 delta，发送激活信号，状态为 ok
        Counter = maps:get(counter, State, 0),
        Delta = #{counter => Counter + 1},
        #{
            delta => Delta,
            activations => [target_id],
            status => ok
        }
    end.

%% 创建失败的计算函数
%% 计算函数返回 #{delta => #{}, activations => [], status => {error, Reason}}
make_failure_compute_fn(Reason) ->
    fun(_Ctx) ->
        #{
            delta => #{},
            activations => [],
            status => {error, Reason}
        }
    end.

%% 创建部分失败的计算函数（根据顶点 ID 决定成功或失败）
make_partial_failure_compute_fn(FailIds) ->
    fun(Ctx) ->
        #{vertex_id := Id, global_state := State} = Ctx,
        case lists:member(Id, FailIds) of
            true ->
                #{
                    delta => #{},
                    activations => [],
                    status => {error, {vertex_error, Id}}
                };
            false ->
                Counter = maps:get(counter, State, 0),
                Delta = #{counter => Counter + 1},
                #{
                    delta => Delta,
                    activations => [target_id],
                    status => ok
                }
        end
    end.

%% 创建中断的计算函数（human-in-the-loop 场景）
%% 计算函数返回 #{delta => #{}, activations => [], status => {interrupt, Reason}}
make_interrupt_compute_fn(Reason) ->
    fun(_Ctx) ->
        #{
            delta => #{},
            activations => [],
            status => {interrupt, Reason}
        }
    end.

%% 创建混合状态的计算函数（根据顶点 ID 决定 ok/error/interrupt）
%% StatusMap: #{vertex_id() => ok | {error, term()} | {interrupt, term()}}
make_mixed_status_compute_fn(StatusMap) ->
    fun(Ctx) ->
        #{vertex_id := Id, global_state := State} = Ctx,
        case maps:get(Id, StatusMap, ok) of
            ok ->
                Counter = maps:get(counter, State, 0),
                Delta = #{counter => Counter + 1},
                #{
                    delta => Delta,
                    activations => [target_id],
                    status => ok
                };
            {error, Reason} ->
                #{
                    delta => #{},
                    activations => [],
                    status => {error, Reason}
                };
            {interrupt, Reason} ->
                #{
                    delta => #{},
                    activations => [],
                    status => {interrupt, Reason}
                }
        end
    end.

%%====================================================================
%% notify_master_done 结果验证测试
%%====================================================================

%% 测试辅助：启动 Worker 并执行超步
start_worker_for_test(Vertices, ComputeFn, GlobalState) ->
    %% 创建一个假的 Master（就是当前测试进程）
    Master = self(),
    Opts = #{
        worker_id => 0,
        master => Master,
        vertices => Vertices,
        compute_fn => ComputeFn,
        global_state => GlobalState,
        num_workers => 1,
        num_vertices => maps:size(Vertices),
        worker_pids => #{}
    },
    {ok, Worker} = pregel_worker:start_link(0, Opts),
    Worker.

%% 测试：Worker 上报成功（无失败、无中断）
worker_done_no_failures_test() ->
    %% 准备
    V1 = pregel_vertex:activate(make_test_vertex(v1)),
    Vertices = #{v1 => V1},
    GlobalState = #{counter => 0},
    ComputeFn = make_success_compute_fn(),
    Worker = start_worker_for_test(Vertices, ComputeFn, GlobalState),

    %% 执行超步（空 activations 表示激活所有顶点）
    pregel_worker:start_superstep(Worker, 0, []),

    %% 等待 Worker 完成并发送结果
    receive
        {'$gen_cast', {worker_done, _Pid, Result}} ->
            %% 验证结果包含失败信息字段
            ?assertEqual(0, maps:get(failed_count, Result)),
            ?assertEqual([], maps:get(failed_vertices, Result)),
            %% 验证结果包含中断信息字段
            ?assertEqual(0, maps:get(interrupted_count, Result)),
            ?assertEqual([], maps:get(interrupted_vertices, Result)),
            %% 验证返回了 deltas
            ?assert(maps:is_key(deltas, Result)),
            ?assertEqual(1, length(maps:get(deltas, Result))),
            %% 验证返回了 activations
            ?assert(maps:is_key(activations, Result))
    after 1000 ->
        ?assert(false)
    end,

    %% 清理
    pregel_worker:stop(Worker).

%% 测试：Worker 上报失败信息（有失败、无中断）
worker_done_with_failures_test() ->
    %% 准备
    V1 = pregel_vertex:activate(make_test_vertex(v1)),
    V2 = pregel_vertex:activate(make_test_vertex(v2)),
    Vertices = #{v1 => V1, v2 => V2},
    GlobalState = #{counter => 0},
    ComputeFn = make_partial_failure_compute_fn([v2]),  %% v2 失败
    Worker = start_worker_for_test(Vertices, ComputeFn, GlobalState),

    %% 执行超步
    pregel_worker:start_superstep(Worker, 0, []),

    %% 等待 Worker 完成
    receive
        {'$gen_cast', {worker_done, _Pid, Result}} ->
            %% 验证失败计数
            ?assertEqual(1, maps:get(failed_count, Result)),
            %% 验证失败顶点列表
            FailedVertices = maps:get(failed_vertices, Result),
            ?assertEqual(1, length(FailedVertices)),
            [{FailedId, _Reason}] = FailedVertices,
            ?assertEqual(v2, FailedId),
            %% 验证无中断
            ?assertEqual(0, maps:get(interrupted_count, Result)),
            ?assertEqual([], maps:get(interrupted_vertices, Result))
    after 1000 ->
        ?assert(false)
    end,

    %% 清理
    pregel_worker:stop(Worker).

%% 测试：Worker 上报失败信息（全部失败、无中断）
worker_done_all_failures_test() ->
    %% 准备
    V1 = pregel_vertex:activate(make_test_vertex(v1)),
    V2 = pregel_vertex:activate(make_test_vertex(v2)),
    Vertices = #{v1 => V1, v2 => V2},
    GlobalState = #{counter => 0},
    ComputeFn = make_failure_compute_fn(all_failed),
    Worker = start_worker_for_test(Vertices, ComputeFn, GlobalState),

    %% 执行超步
    pregel_worker:start_superstep(Worker, 0, []),

    %% 等待 Worker 完成
    receive
        {'$gen_cast', {worker_done, _Pid, Result}} ->
            %% 验证全部失败
            ?assertEqual(2, maps:get(failed_count, Result)),
            %% activations 应为空
            ?assertEqual([], maps:get(activations, Result)),
            FailedVertices = maps:get(failed_vertices, Result),
            ?assertEqual(2, length(FailedVertices)),
            %% 验证无中断
            ?assertEqual(0, maps:get(interrupted_count, Result)),
            ?assertEqual([], maps:get(interrupted_vertices, Result))
    after 1000 ->
        ?assert(false)
    end,

    %% 清理
    pregel_worker:stop(Worker).

%%====================================================================
%% 类型契约验证测试
%%====================================================================

%% 测试：compute_result 结构验证 - 成功（全局状态模式）
compute_result_success_structure_test() ->
    Result = #{
        delta => #{counter => 1},
        activations => [v2],
        status => ok
    },
    %% 验证必需字段存在
    ?assert(maps:is_key(delta, Result)),
    ?assert(maps:is_key(activations, Result)),
    ?assert(maps:is_key(status, Result)),
    ?assertEqual(ok, maps:get(status, Result)).

%% 测试：compute_result 结构验证 - 失败
compute_result_failure_structure_test() ->
    Result = #{
        delta => #{},
        activations => [],
        status => {error, some_reason}
    },
    ?assert(maps:is_key(status, Result)),
    ?assertMatch({error, _}, maps:get(status, Result)).

%% 测试：compute_result 结构验证 - 中断
compute_result_interrupt_structure_test() ->
    Result = #{
        delta => #{},
        activations => [],
        status => {interrupt, #{reason => need_approval, data => some_data}}
    },
    ?assert(maps:is_key(status, Result)),
    ?assertMatch({interrupt, _}, maps:get(status, Result)).

%%====================================================================
%% Worker 中断上报测试
%%====================================================================

%% 测试：Worker 上报中断信息
worker_done_with_interrupts_test() ->
    %% 准备
    V1 = pregel_vertex:activate(make_test_vertex(v1)),
    V2 = pregel_vertex:activate(make_test_vertex(v2)),
    Vertices = #{v1 => V1, v2 => V2},
    GlobalState = #{counter => 0},
    StatusMap = #{v2 => {interrupt, need_approval}},  %% v2 中断
    ComputeFn = make_mixed_status_compute_fn(StatusMap),
    Worker = start_worker_for_test(Vertices, ComputeFn, GlobalState),

    %% 执行超步
    pregel_worker:start_superstep(Worker, 0, []),

    %% 等待 Worker 完成
    receive
        {'$gen_cast', {worker_done, _Pid, Result}} ->
            %% 验证中断计数
            ?assertEqual(1, maps:get(interrupted_count, Result)),
            %% 验证中断顶点列表
            InterruptedVertices = maps:get(interrupted_vertices, Result),
            ?assertEqual(1, length(InterruptedVertices)),
            [{InterruptedId, _Reason}] = InterruptedVertices,
            ?assertEqual(v2, InterruptedId),
            %% 验证无失败
            ?assertEqual(0, maps:get(failed_count, Result))
    after 1000 ->
        ?assert(false)
    end,

    %% 清理
    pregel_worker:stop(Worker).

%% 测试：Worker 上报混合状态（失败+中断）
worker_done_mixed_failures_and_interrupts_test() ->
    %% 准备：v1 成功，v2 失败，v3 中断
    V1 = pregel_vertex:activate(make_test_vertex(v1)),
    V2 = pregel_vertex:activate(make_test_vertex(v2)),
    V3 = pregel_vertex:activate(make_test_vertex(v3)),
    Vertices = #{v1 => V1, v2 => V2, v3 => V3},
    GlobalState = #{counter => 0},
    StatusMap = #{
        v2 => {error, some_error},
        v3 => {interrupt, need_input}
    },
    ComputeFn = make_mixed_status_compute_fn(StatusMap),
    Worker = start_worker_for_test(Vertices, ComputeFn, GlobalState),

    %% 执行超步
    pregel_worker:start_superstep(Worker, 0, []),

    %% 等待 Worker 完成
    receive
        {'$gen_cast', {worker_done, _Pid, Result}} ->
            %% 验证失败
            ?assertEqual(1, maps:get(failed_count, Result)),
            FailedVertices = maps:get(failed_vertices, Result),
            ?assertEqual(1, length(FailedVertices)),
            %% 验证中断
            ?assertEqual(1, maps:get(interrupted_count, Result)),
            InterruptedVertices = maps:get(interrupted_vertices, Result),
            ?assertEqual(1, length(InterruptedVertices)),
            %% 验证 activations（只有 v1 发送激活）
            Activations = maps:get(activations, Result),
            ?assertEqual(1, length(Activations))
    after 1000 ->
        ?assert(false)
    end,

    %% 清理
    pregel_worker:stop(Worker).

%% 测试：只激活指定的顶点（v3 已 halted，不会被计算）
worker_selective_activation_test() ->
    %% 准备：v1, v2 活跃；v3 已 halted
    V1 = pregel_vertex:activate(make_test_vertex(v1)),
    V2 = pregel_vertex:activate(make_test_vertex(v2)),
    V3 = pregel_vertex:halt(make_test_vertex(v3)),  %% v3 已停止
    Vertices = #{v1 => V1, v2 => V2, v3 => V3},
    GlobalState = #{counter => 0},
    ComputeFn = make_success_compute_fn(),
    Worker = start_worker_for_test(Vertices, ComputeFn, GlobalState),

    %% 执行超步 - 空 activations 表示激活所有非 halted 顶点
    pregel_worker:start_superstep(Worker, 0, []),

    %% 等待 Worker 完成
    receive
        {'$gen_cast', {worker_done, _Pid, Result}} ->
            %% 验证返回了 deltas（只有 2 个，v1 和 v2）
            ?assertEqual(2, length(maps:get(deltas, Result))),
            %% active_count 是超步后仍活跃的顶点数，计算完成的顶点会 halt
            %% 所以超步后所有顶点都 halted，active_count = 0
            ?assertEqual(0, maps:get(active_count, Result))
    after 1000 ->
        ?assert(false)
    end,

    %% 清理
    pregel_worker:stop(Worker).
