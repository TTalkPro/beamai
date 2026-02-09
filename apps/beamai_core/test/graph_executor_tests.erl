%%%-------------------------------------------------------------------
%%% @doc graph_executor 模块单元测试
%%%
%%% 测试场景：
%%% 1. 基本顺序图执行（A → B → C → END）
%%% 2. 条件路由（conditional edge）
%%% 3. 循环执行（loop back）
%%% 4. 静态扇出（fanout edge）
%%% 5. 动态 dispatch（fan_out + field_reducers）
%%% 6. 单顶点失败隔离
%%% 7. 重试机制（retry after error）
%%% 8. 中断与恢复（interrupt + retry）
%%% 9. 快照数据获取
%%% 10. step-by-step 执行模式
%%%
%%% 注意：graph_state 内部使用 binary keys，node functions 必须使用
%%% graph_state:get/set API 或 binary keys 访问状态。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_executor_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Pool 管理（需要 poolboy 的测试使用）
%%====================================================================

%% 启动测试用 poolboy 池
start_test_pool() ->
    PoolArgs = [
        {name, {local, beamai_graph_pool}},
        {worker_module, beamai_graph_pool_worker},
        {size, 4},
        {max_overflow, 4},
        {strategy, fifo}
    ],
    {ok, Pid} = poolboy:start_link(PoolArgs, []),
    Pid.

%% 停止测试用 poolboy 池
stop_test_pool(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 构建简单的顺序图: __start__ -> NodeA -> NodeB -> __end__
build_sequential_graph(NodeAFun, NodeBFun) ->
    G0 = beamai_pregel_graph:new(),
    G1 = beamai_pregel_graph:add_vertex_flat(G0, '__start__', undefined, #{},
            [beamai_graph_edge:direct('__start__', node_a)]),
    G2 = beamai_pregel_graph:add_vertex_flat(G1, node_a, NodeAFun, #{},
            [beamai_graph_edge:direct(node_a, node_b)]),
    G3 = beamai_pregel_graph:add_vertex_flat(G2, node_b, NodeBFun, #{},
            [beamai_graph_edge:direct(node_b, '__end__')]),
    G4 = beamai_pregel_graph:add_vertex_flat(G3, '__end__', undefined, #{}, []),
    halt_non_start(G4).

%% 构建单节点图: __start__ -> Node -> __end__
build_single_node_graph(NodeFun) ->
    G0 = beamai_pregel_graph:new(),
    G1 = beamai_pregel_graph:add_vertex_flat(G0, '__start__', undefined, #{},
            [beamai_graph_edge:direct('__start__', node_a)]),
    G2 = beamai_pregel_graph:add_vertex_flat(G1, node_a, NodeFun, #{},
            [beamai_graph_edge:direct(node_a, '__end__')]),
    G3 = beamai_pregel_graph:add_vertex_flat(G2, '__end__', undefined, #{}, []),
    halt_non_start(G3).

%% 将除 __start__ 外的所有顶点设为 halted
halt_non_start(Graph) ->
    beamai_pregel_graph:map(Graph, fun(Vertex) ->
        case beamai_pregel_vertex:id(Vertex) of
            '__start__' -> Vertex;
            _ -> beamai_pregel_vertex:halt(Vertex)
        end
    end).

%% 简单的计算函数：将 counter 加 1（使用 graph_state API）
increment_fun() ->
    fun(State, _Input) ->
        Counter = beamai_graph_engine:state_get(State, counter, 0),
        {ok, beamai_graph_engine:state_set(State, counter, Counter + 1)}
    end.

%% 追加节点名到列表的计算函数（使用 graph_state API）
append_node_fun(NodeName) ->
    fun(State, _Input) ->
        Visited = beamai_graph_engine:state_get(State, visited, []),
        {ok, beamai_graph_engine:state_set(State, visited, Visited ++ [NodeName])}
    end.

%% 总是失败的计算函数
failing_fun() ->
    fun(_State, _Input) ->
        {error, intentional_failure}
    end.

%% 中断的计算函数（使用 graph_state API）
interrupt_fun(Reason) ->
    fun(State, _Input) ->
        {interrupt, Reason, beamai_graph_engine:state_set(State, interrupted, true)}
    end.

%%====================================================================
%% 基本 run/3 测试
%%====================================================================

%% 测试：单节点图执行
single_node_run_test() ->
    Graph = build_single_node_graph(increment_fun()),
    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{counter => 0}),
    Result = beamai_graph_engine:execute(Graph, ComputeFn, #{global_state => InitialState}),

    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(global_state, Result),
    ?assertEqual(1, beamai_graph_engine:state_get(FinalState, counter)).

%% 测试：顺序图执行（两个节点都递增 counter）
sequential_run_test() ->
    Graph = build_sequential_graph(increment_fun(), increment_fun()),
    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{counter => 0}),
    Result = beamai_graph_engine:execute(Graph, ComputeFn, #{global_state => InitialState}),

    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(global_state, Result),
    ?assertEqual(2, beamai_graph_engine:state_get(FinalState, counter)).

%% 测试：追踪节点执行顺序
execution_order_test() ->
    Graph = build_sequential_graph(append_node_fun(a), append_node_fun(b)),
    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{visited => []}),
    Result = beamai_graph_engine:execute(Graph, ComputeFn, #{global_state => InitialState}),

    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(global_state, Result),
    ?assertEqual([a, b], beamai_graph_engine:state_get(FinalState, visited)).

%%====================================================================
%% 条件路由测试
%%====================================================================

%% 测试：条件路由根据状态选择不同路径
conditional_routing_test() ->
    RouterFun = fun(State) ->
        case beamai_graph_engine:state_get(State, route) of
            left -> left_node;
            right -> right_node
        end
    end,

    G0 = beamai_pregel_graph:new(),
    G1 = beamai_pregel_graph:add_vertex_flat(G0, '__start__', undefined, #{},
            [beamai_graph_edge:direct('__start__', router)]),
    G2 = beamai_pregel_graph:add_vertex_flat(G1, router,
            fun(State, _) -> {ok, State} end, #{},
            [beamai_graph_edge:conditional(router, RouterFun)]),
    G3 = beamai_pregel_graph:add_vertex_flat(G2, left_node,
            append_node_fun(left), #{},
            [beamai_graph_edge:direct(left_node, '__end__')]),
    G4 = beamai_pregel_graph:add_vertex_flat(G3, right_node,
            append_node_fun(right), #{},
            [beamai_graph_edge:direct(right_node, '__end__')]),
    G5 = beamai_pregel_graph:add_vertex_flat(G4, '__end__', undefined, #{}, []),
    Graph = halt_non_start(G5),

    ComputeFn = beamai_graph_compute:compute_fn(),

    %% 路由到左边
    InitialLeft = beamai_graph_engine:state_new(#{route => left, visited => []}),
    ResultLeft = beamai_graph_engine:execute(Graph, ComputeFn, #{global_state => InitialLeft}),
    FinalLeft = maps:get(global_state, ResultLeft),
    ?assertEqual([left], beamai_graph_engine:state_get(FinalLeft, visited)),

    %% 路由到右边
    InitialRight = beamai_graph_engine:state_new(#{route => right, visited => []}),
    ResultRight = beamai_graph_engine:execute(Graph, ComputeFn, #{global_state => InitialRight}),
    FinalRight = maps:get(global_state, ResultRight),
    ?assertEqual([right], beamai_graph_engine:state_get(FinalRight, visited)).

%%====================================================================
%% 循环执行测试
%%====================================================================

%% 测试：循环执行直到条件满足
loop_execution_test() ->
    LoopFun = fun(State, _Input) ->
        Counter = beamai_graph_engine:state_get(State, counter, 0),
        {ok, beamai_graph_engine:state_set(State, counter, Counter + 1)}
    end,
    LoopRouter = fun(State) ->
        case beamai_graph_engine:state_get(State, counter) >= 3 of
            true -> '__end__';
            false -> loop_node
        end
    end,

    G0 = beamai_pregel_graph:new(),
    G1 = beamai_pregel_graph:add_vertex_flat(G0, '__start__', undefined, #{},
            [beamai_graph_edge:direct('__start__', loop_node)]),
    G2 = beamai_pregel_graph:add_vertex_flat(G1, loop_node, LoopFun, #{},
            [beamai_graph_edge:conditional(loop_node, LoopRouter)]),
    G3 = beamai_pregel_graph:add_vertex_flat(G2, '__end__', undefined, #{}, []),
    Graph = halt_non_start(G3),

    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{counter => 0}),
    Result = beamai_graph_engine:execute(Graph, ComputeFn, #{
        global_state => InitialState,
        max_supersteps => 20
    }),

    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(global_state, Result),
    ?assertEqual(3, beamai_graph_engine:state_get(FinalState, counter)).

%%====================================================================
%% 静态扇出测试
%%====================================================================

%% 测试：扇出到多个节点并行执行（需要 poolboy 池）
fanout_test() ->
    PoolPid = start_test_pool(),
    try
        G0 = beamai_pregel_graph:new(),
        G1 = beamai_pregel_graph:add_vertex_flat(G0, '__start__', undefined, #{},
                [beamai_graph_edge:direct('__start__', splitter)]),
        G2 = beamai_pregel_graph:add_vertex_flat(G1, splitter,
                fun(State, _) -> {ok, State} end, #{},
                [beamai_graph_edge:fanout(splitter, [worker_a, worker_b, worker_c])]),
        G3 = beamai_pregel_graph:add_vertex_flat(G2, worker_a, increment_fun(), #{},
                [beamai_graph_edge:direct(worker_a, '__end__')]),
        G4 = beamai_pregel_graph:add_vertex_flat(G3, worker_b, increment_fun(), #{},
                [beamai_graph_edge:direct(worker_b, '__end__')]),
        G5 = beamai_pregel_graph:add_vertex_flat(G4, worker_c, increment_fun(), #{},
                [beamai_graph_edge:direct(worker_c, '__end__')]),
        G6 = beamai_pregel_graph:add_vertex_flat(G5, '__end__', undefined, #{}, []),
        Graph = halt_non_start(G6),

        ComputeFn = beamai_graph_compute:compute_fn(),
        InitialState = beamai_graph_engine:state_new(#{counter => 0}),

        %% 三个 worker 并行执行，每个都读 counter=0 并写 counter=1
        %% 没有 field_reducer 时最后一个 delta 覆盖前面的
        Result = beamai_graph_engine:execute(Graph, ComputeFn, #{global_state => InitialState}),

        ?assertEqual(completed, maps:get(status, Result)),
        FinalState = maps:get(global_state, Result),
        ?assertEqual(1, beamai_graph_engine:state_get(FinalState, counter))
    after
        stop_test_pool(PoolPid)
    end.

%% 测试：扇出 + field_reducer 累加（需要 poolboy 池）
fanout_with_reducer_test() ->
    PoolPid = start_test_pool(),
    try
        G0 = beamai_pregel_graph:new(),
        G1 = beamai_pregel_graph:add_vertex_flat(G0, '__start__', undefined, #{},
                [beamai_graph_edge:direct('__start__', splitter)]),
        G2 = beamai_pregel_graph:add_vertex_flat(G1, splitter,
                fun(State, _) -> {ok, State} end, #{},
                [beamai_graph_edge:fanout(splitter, [worker_a, worker_b, worker_c])]),
        G3 = beamai_pregel_graph:add_vertex_flat(G2, worker_a, increment_fun(), #{},
                [beamai_graph_edge:direct(worker_a, '__end__')]),
        G4 = beamai_pregel_graph:add_vertex_flat(G3, worker_b, increment_fun(), #{},
                [beamai_graph_edge:direct(worker_b, '__end__')]),
        G5 = beamai_pregel_graph:add_vertex_flat(G4, worker_c, increment_fun(), #{},
                [beamai_graph_edge:direct(worker_c, '__end__')]),
        G6 = beamai_pregel_graph:add_vertex_flat(G5, '__end__', undefined, #{}, []),
        Graph = halt_non_start(G6),

        ComputeFn = beamai_graph_compute:compute_fn(),
        InitialState = beamai_graph_engine:state_new(#{counter => 0}),

        %% 使用加法 reducer：三个 worker 各返回 delta #{<<"counter">> => 1}
        %% reducer 将累加: 0 + 1 + 1 + 1 = 3
        %% field_reducers 使用 binary key 匹配 graph_state 的 binary keys
        AddReducer = fun(Old, New) -> Old + New end,
        Result = beamai_graph_engine:execute(Graph, ComputeFn, #{
            global_state => InitialState,
            field_reducers => #{<<"counter">> => AddReducer}
        }),

        ?assertEqual(completed, maps:get(status, Result)),
        FinalState = maps:get(global_state, Result),
        ?assertEqual(3, beamai_graph_engine:state_get(FinalState, counter))
    after
        stop_test_pool(PoolPid)
    end.

%%====================================================================
%% max_supersteps 测试
%%====================================================================

%% 测试：超过最大超步数时返回 max_supersteps
max_supersteps_test() ->
    InfiniteLoopFun = fun(State, _) -> {ok, State} end,

    G0 = beamai_pregel_graph:new(),
    G1 = beamai_pregel_graph:add_vertex_flat(G0, '__start__', undefined, #{},
            [beamai_graph_edge:direct('__start__', loop_node)]),
    G2 = beamai_pregel_graph:add_vertex_flat(G1, loop_node, InfiniteLoopFun, #{},
            [beamai_graph_edge:direct(loop_node, loop_node)]),
    G3 = beamai_pregel_graph:add_vertex_flat(G2, '__end__', undefined, #{}, []),
    Graph = halt_non_start(G3),

    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{}),
    Result = beamai_graph_engine:execute(Graph, ComputeFn, #{
        global_state => InitialState,
        max_supersteps => 5
    }),

    ?assertEqual(max_supersteps, maps:get(status, Result)).

%%====================================================================
%% step-by-step 执行模式测试
%%====================================================================

%% 测试：do_step 纯函数分步执行
step_by_step_test() ->
    Graph = build_single_node_graph(increment_fun()),
    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{counter => 0}),

    {ok, E0} = beamai_graph_engine:new(Graph, ComputeFn,
        #{global_state => InitialState}),

    %% 第一步：返回 initial snapshot
    {{continue, Info0}, E1} = beamai_graph_engine:do_step(E0),
    ?assertEqual(initial, maps:get(type, Info0)),

    %% 第二步：执行 __start__ -> 激活 node_a
    {{continue, _Info1}, E2} = beamai_graph_engine:do_step(E1),

    %% 第三步：执行 node_a -> 激活 __end__
    {{continue, _Info2}, E3} = beamai_graph_engine:do_step(E2),

    %% 第四步：执行 __end__ -> 完成
    {{done, completed, Info3}, E4} = beamai_graph_engine:do_step(E3),
    ?assertEqual(final, maps:get(type, Info3)),

    %% 已经 halted，再次 step 也返回 done
    {{done, completed, _}, _E5} = beamai_graph_engine:do_step(E4).

%%====================================================================
%% 快照数据测试
%%====================================================================

%% 测试：获取 snapshot 数据
snapshot_data_test() ->
    Graph = build_single_node_graph(increment_fun()),
    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{counter => 0}),

    {ok, E0} = beamai_graph_engine:new(Graph, ComputeFn,
        #{global_state => InitialState}),

    %% 初始 step
    {{continue, _}, E1} = beamai_graph_engine:do_step(E0),

    %% 获取 snapshot
    Snapshot = beamai_graph_engine:extract_snapshot_data(E1),
    ?assert(is_map(Snapshot)),
    ?assert(maps:is_key(superstep, Snapshot)),
    ?assert(maps:is_key(global_state, Snapshot)),
    ?assert(maps:is_key(vertices, Snapshot)),

    %% 获取 global_state
    GlobalState = beamai_graph_engine:global_state(E1),
    ?assertEqual(0, beamai_graph_engine:state_get(GlobalState, counter)).

%%====================================================================
%% 错误处理和单顶点失败隔离测试
%%====================================================================

%% 测试：run 遇到错误时提前终止
error_early_termination_test() ->
    Graph = build_single_node_graph(failing_fun()),
    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{counter => 0}),

    Result = beamai_graph_engine:execute(Graph, ComputeFn, #{global_state => InitialState}),

    %% run_loop 在 error 时提前终止
    ?assert(maps:get(failed_count, Result, 0) > 0).

%% 测试：do_step 模式下错误返回 continue（非 done）
error_step_returns_continue_test() ->
    Graph = build_single_node_graph(failing_fun()),
    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{}),

    {ok, E0} = beamai_graph_engine:new(Graph, ComputeFn,
        #{global_state => InitialState}),

    %% initial step
    {{continue, _}, E1} = beamai_graph_engine:do_step(E0),
    %% __start__ step
    {{continue, _}, E2} = beamai_graph_engine:do_step(E1),
    %% node_a 执行失败 step
    {{continue, Info}, _E3} = beamai_graph_engine:do_step(E2),
    ?assertEqual(error, maps:get(type, Info)),
    ?assert(maps:get(failed_count, Info) > 0).

%%====================================================================
%% 中断测试
%%====================================================================

%% 测试：节点中断时 delta 被保存
interrupt_test() ->
    Graph = build_single_node_graph(interrupt_fun(need_user_input)),
    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{interrupted => false}),

    {ok, E0} = beamai_graph_engine:new(Graph, ComputeFn,
        #{global_state => InitialState}),

    %% initial step
    {{continue, _}, E1} = beamai_graph_engine:do_step(E0),
    %% __start__ step
    {{continue, _}, E2} = beamai_graph_engine:do_step(E1),
    %% node_a 中断 step
    {{continue, Info}, _E3} = beamai_graph_engine:do_step(E2),
    ?assertEqual(interrupt, maps:get(type, Info)),
    ?assert(maps:get(interrupted_count, Info) > 0).

%%====================================================================
%% 重试测试
%%====================================================================

%% 测试：重试失败的顶点
retry_after_error_test() ->
    CounterRef = atomics:new(1, [{signed, true}]),
    RetryFun = fun(State, _Input) ->
        Count = atomics:add_get(CounterRef, 1, 1),
        case Count of
            1 -> {error, first_attempt_fails};
            _ -> {ok, beamai_graph_engine:state_set(State, retried, true)}
        end
    end,

    Graph = build_single_node_graph(RetryFun),
    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{retried => false}),

    {ok, E0} = beamai_graph_engine:new(Graph, ComputeFn,
        #{global_state => InitialState}),

    %% initial
    {{continue, _}, E1} = beamai_graph_engine:do_step(E0),
    %% __start__
    {{continue, _}, E2} = beamai_graph_engine:do_step(E1),
    %% node_a 首次失败
    {{continue, Info}, E3} = beamai_graph_engine:do_step(E2),
    ?assertEqual(error, maps:get(type, Info)),

    %% 重试 node_a
    {{continue, RetryInfo}, _E4} = beamai_graph_engine:do_retry([node_a], E3),
    ?assertEqual(0, maps:get(failed_count, RetryInfo)).

%%====================================================================
%% Command 模式测试
%%====================================================================

%% 测试：使用 Command 跳转到指定节点
command_goto_test() ->
    CommandFun = fun(_State, _Input) ->
        Cmd = beamai_graph_command:goto(node_c, #{<<"step">> => command_used}),
        {command, Cmd}
    end,

    G0 = beamai_pregel_graph:new(),
    G1 = beamai_pregel_graph:add_vertex_flat(G0, '__start__', undefined, #{},
            [beamai_graph_edge:direct('__start__', node_a)]),
    G2 = beamai_pregel_graph:add_vertex_flat(G1, node_a, CommandFun, #{},
            [beamai_graph_edge:direct(node_a, node_b)]),
    G3 = beamai_pregel_graph:add_vertex_flat(G2, node_b, append_node_fun(b), #{},
            [beamai_graph_edge:direct(node_b, '__end__')]),
    G4 = beamai_pregel_graph:add_vertex_flat(G3, node_c, append_node_fun(c), #{},
            [beamai_graph_edge:direct(node_c, '__end__')]),
    G5 = beamai_pregel_graph:add_vertex_flat(G4, '__end__', undefined, #{}, []),
    Graph = halt_non_start(G5),

    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{visited => []}),
    Result = beamai_graph_engine:execute(Graph, ComputeFn, #{global_state => InitialState}),

    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(global_state, Result),
    ?assertEqual([c], beamai_graph_engine:state_get(FinalState, visited)),
    ?assertEqual(command_used, beamai_graph_engine:state_get(FinalState, step)).

%%====================================================================
%% 恢复（Restore）测试
%%====================================================================

%% 测试：从 snapshot 恢复执行（需要 poolboy 池，因为恢复时可能有多个活跃顶点）
restore_from_snapshot_test() ->
    PoolPid = start_test_pool(),
    try
        Graph = build_single_node_graph(increment_fun()),
        ComputeFn = beamai_graph_compute:compute_fn(),

        %% 模拟从 superstep 2 恢复，counter 已经是 5
        RestoredState = beamai_graph_engine:state_new(#{counter => 5}),

        %% 恢复所有顶点状态为 halted（模拟真实场景）
        Vertices = maps:from_list([
            {'__start__', beamai_pregel_vertex:halt(beamai_pregel_vertex:new_flat('__start__', undefined, #{}, [beamai_graph_edge:direct('__start__', node_a)]))},
            {node_a, beamai_pregel_vertex:halt(beamai_pregel_vertex:new_flat(node_a, increment_fun(), #{}, [beamai_graph_edge:direct(node_a, '__end__')]))},
            {'__end__', beamai_pregel_vertex:halt(beamai_pregel_vertex:new_flat('__end__', undefined, #{}, []))}
        ]),

        %% 恢复时指定 pending_activations 为 __end__ (即将完成)
        RestoreOpts = #{
            superstep => 2,
            global_state => RestoredState,
            pending_activations => ['__end__'],
            vertices => Vertices
        },

        {ok, E0} = beamai_graph_engine:new(Graph, ComputeFn,
            #{restore_from => RestoreOpts}),

        %% initial step（带 restore）
        {{continue, _}, E1} = beamai_graph_engine:do_step(E0),

        %% 执行 __end__（1 个任务，inline 执行）
        {{done, completed, _}, E2} = beamai_graph_engine:do_step(E1),

        %% 验证最终状态保持了恢复的 counter
        Result = beamai_graph_engine:build_result(E2),
        FinalState = maps:get(global_state, Result),
        ?assertEqual(5, beamai_graph_engine:state_get(FinalState, counter))
    after
        stop_test_pool(PoolPid)
    end.

%%====================================================================
%% get_result 测试
%%====================================================================

%% 测试：在未完成时引擎状态为 running（非 completed）
engine_state_before_halted_test() ->
    Graph = build_single_node_graph(increment_fun()),
    ComputeFn = beamai_graph_compute:compute_fn(),
    InitialState = beamai_graph_engine:state_new(#{counter => 0}),

    {ok, E0} = beamai_graph_engine:new(Graph, ComputeFn,
        #{global_state => InitialState}),

    {{continue, _}, E1} = beamai_graph_engine:do_step(E0),
    %% 引擎状态为 running（new/3 设置）
    ?assertEqual(running, beamai_graph_engine:current_state(E1)).
