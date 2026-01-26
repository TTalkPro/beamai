%%%-------------------------------------------------------------------
%%% @doc Pregel Dispatch Pool 单元测试
%%%
%%% 测试 dispatch 并发执行功能：
%%% - pregel_dispatch_worker 基本功能
%%% - 池可用性检查逻辑
%%% - 并发执行与顺序回退
%%% - 错误隔离
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_dispatch_pool_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% pregel_dispatch_worker 测试
%%====================================================================

worker_start_stop_test() ->
    {ok, Pid} = pregel_dispatch_worker:start_link([]),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

worker_execute_success_test() ->
    {ok, Pid} = pregel_dispatch_worker:start_link([]),
    ComputeFn = fun(Context) ->
        Input = maps:get(vertex_input, Context),
        #{delta => #{result => Input}, activations => [], status => ok}
    end,
    Context = #{vertex_input => hello, vertex_id => a, vertex => #{},
                global_state => #{}, superstep => 0, num_vertices => 1},
    Result = pregel_dispatch_worker:execute(Pid, ComputeFn, Context),
    ?assertEqual({ok, #{delta => #{result => hello}, activations => [], status => ok}}, Result),
    gen_server:stop(Pid).

worker_execute_error_isolation_test() ->
    {ok, Pid} = pregel_dispatch_worker:start_link([]),
    ComputeFn = fun(_Context) ->
        error(intentional_crash)
    end,
    Context = #{vertex_input => undefined, vertex_id => a, vertex => #{},
                global_state => #{}, superstep => 0, num_vertices => 1},
    Result = pregel_dispatch_worker:execute(Pid, ComputeFn, Context),
    ?assertMatch({error, {dispatch_error, {error, intentional_crash, _}}}, Result),
    %% Worker 进程仍然存活
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

worker_execute_throw_isolation_test() ->
    {ok, Pid} = pregel_dispatch_worker:start_link([]),
    ComputeFn = fun(_Context) ->
        throw(some_throw)
    end,
    Context = #{vertex_input => undefined, vertex_id => a, vertex => #{},
                global_state => #{}, superstep => 0, num_vertices => 1},
    Result = pregel_dispatch_worker:execute(Pid, ComputeFn, Context),
    ?assertMatch({error, {dispatch_error, {throw, some_throw, _}}}, Result),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

worker_reusable_after_error_test() ->
    {ok, Pid} = pregel_dispatch_worker:start_link([]),
    %% 第一次：错误
    BadFn = fun(_) -> error(boom) end,
    Ctx = #{vertex_input => undefined, vertex_id => a, vertex => #{},
            global_state => #{}, superstep => 0, num_vertices => 1},
    {error, _} = pregel_dispatch_worker:execute(Pid, BadFn, Ctx),
    %% 第二次：正常
    GoodFn = fun(_) -> #{delta => #{ok => true}, activations => [], status => ok} end,
    Result = pregel_dispatch_worker:execute(Pid, GoodFn, Ctx),
    ?assertEqual({ok, #{delta => #{ok => true}, activations => [], status => ok}}, Result),
    gen_server:stop(Pid).

%%====================================================================
%% dispatch_pool_available 测试（通过 compute_vertices 间接测试）
%%====================================================================

sequential_fallback_single_dispatch_test() ->
    %% 单个 dispatch 应当走顺序路径（即使池可用）
    ComputeFn = fun(Context) ->
        Input = maps:get(vertex_input, Context),
        #{delta => #{value => Input}, activations => [], status => ok}
    end,
    Vertices = #{a => #{id => a, edges => []}},
    Dispatch = #{target => a, input => #{data => 1}},
    VertexInputs = #{a => [Dispatch]},

    {Deltas, _Acts, Failed, _Interrupted} = pregel_worker:compute_vertices(
        Vertices, ComputeFn, 0, 1, #{}, VertexInputs
    ),
    ?assertEqual([], Failed),
    ?assertEqual(1, length(Deltas)).

sequential_fallback_no_pool_test() ->
    %% 多个 dispatch 但池不可用，应走顺序回退
    %% 跳过测试如果池已被其他测试启动
    case whereis(beamai_dispatch_pool) of
        undefined ->
            ComputeFn = fun(Context) ->
                Input = maps:get(vertex_input, Context),
                #{delta => #{value => Input}, activations => [], status => ok}
            end,
            Vertices = #{a => #{id => a, edges => []}},
            D1 = #{target => a, input => #{data => 1}},
            D2 = #{target => a, input => #{data => 2}},
            VertexInputs = #{a => [D1, D2]},

            {Deltas, _Acts, Failed, _Interrupted} = pregel_worker:compute_vertices(
                Vertices, ComputeFn, 0, 1, #{}, VertexInputs
            ),
            ?assertEqual([], Failed),
            ?assertEqual(2, length(Deltas));
        _Pid ->
            %% 池已启动，跳过此测试
            ok
    end.

%%====================================================================
%% 池并发执行测试（需要启动 poolboy）
%%====================================================================

%% @private 在当前进程中启动 dispatch 池
%% 如果池已经存在（由 beamai_core 启动），直接复用
start_dispatch_pool() ->
    application:set_env(beamai_core, dispatch_timeout, 5000),
    case whereis(beamai_dispatch_pool) of
        undefined ->
            PoolArgs = [
                {name, {local, beamai_dispatch_pool}},
                {worker_module, pregel_dispatch_worker},
                {size, 5},
                {max_overflow, 10},
                {strategy, fifo}
            ],
            {ok, Pid} = poolboy:start_link(PoolArgs, []),
            {started, Pid};
        ExistingPid ->
            {reused, ExistingPid}
    end.

%% @private 停止 dispatch 池（仅停止自己启动的池）
stop_dispatch_pool({started, Pid}) ->
    unlink(Pid),
    poolboy:stop(beamai_dispatch_pool),
    application:unset_env(beamai_core, dispatch_timeout);
stop_dispatch_pool({reused, _Pid}) ->
    %% 复用的池不停止
    ok.

concurrent_dispatch_all_test() ->
    PoolHandle = start_dispatch_pool(),
    try
        ComputeFn = fun(Context) ->
            Input = maps:get(vertex_input, Context),
            #{delta => #{value => Input}, activations => [], status => ok}
        end,
        Vertices = #{a => #{id => a, edges => []}},
        D1 = #{target => a, input => #{data => 1}},
        D2 = #{target => a, input => #{data => 2}},
        D3 = #{target => a, input => #{data => 3}},
        VertexInputs = #{a => [D1, D2, D3]},

        {Deltas, _Acts, Failed, _Interrupted} = pregel_worker:compute_vertices(
            Vertices, ComputeFn, 0, 1, #{}, VertexInputs
        ),
        ?assertEqual([], Failed),
        ?assertEqual(3, length(Deltas)),
        lists:foreach(fun(D) ->
            ?assert(maps:is_key(value, D))
        end, Deltas)
    after
        stop_dispatch_pool(PoolHandle)
    end.

concurrent_dispatch_error_test() ->
    PoolHandle = start_dispatch_pool(),
    try
        %% 第二个 dispatch 抛异常，不影响其他
        ComputeFn = fun(Context) ->
            case maps:get(vertex_input, Context) of
                #{data := 2} -> error(boom);
                Input -> #{delta => #{value => Input}, activations => [], status => ok}
            end
        end,
        Vertices = #{a => #{id => a, edges => []}},
        D1 = #{target => a, input => #{data => 1}},
        D2 = #{target => a, input => #{data => 2}},
        D3 = #{target => a, input => #{data => 3}},
        VertexInputs = #{a => [D1, D2, D3]},

        {Deltas, _Acts, Failed, _Interrupted} = pregel_worker:compute_vertices(
            Vertices, ComputeFn, 0, 1, #{}, VertexInputs
        ),
        %% 2 个成功产生 delta，1 个失败记录到 Failed
        ?assertEqual(2, length(Deltas)),
        ?assertEqual(1, length(Failed))
    after
        stop_dispatch_pool(PoolHandle)
    end.

concurrent_dispatch_multi_vertex_test() ->
    PoolHandle = start_dispatch_pool(),
    try
        ComputeFn = fun(Context) ->
            Input = maps:get(vertex_input, Context),
            VId = maps:get(vertex_id, Context),
            #{delta => #{vertex => VId, value => Input}, activations => [], status => ok}
        end,
        Vertices = #{
            a => #{id => a, edges => []},
            b => #{id => b, edges => []}
        },
        VertexInputs = #{
            a => [#{target => a, input => #{d => 1}}, #{target => a, input => #{d => 2}}],
            b => [#{target => b, input => #{d => 3}}, #{target => b, input => #{d => 4}}]
        },

        {Deltas, _Acts, Failed, _Interrupted} = pregel_worker:compute_vertices(
            Vertices, ComputeFn, 0, 2, #{}, VertexInputs
        ),
        ?assertEqual([], Failed),
        ?assertEqual(4, length(Deltas))
    after
        stop_dispatch_pool(PoolHandle)
    end.

%%====================================================================
%% 并发性验证测试
%%====================================================================

true_concurrency_test() ->
    PoolHandle = start_dispatch_pool(),
    try
        %% 通过 sleep 验证并发：如果顺序执行需 300ms+，并发应 < 250ms
        ComputeFn = fun(Context) ->
            _Input = maps:get(vertex_input, Context),
            timer:sleep(100),
            #{delta => #{done => true}, activations => [], status => ok}
        end,
        Vertices = #{a => #{id => a, edges => []}},
        Dispatches = [#{target => a, input => #{i => I}} || I <- lists:seq(1, 3)],
        VertexInputs = #{a => Dispatches},

        T1 = erlang:monotonic_time(millisecond),
        {Deltas, _Acts, Failed, _Interrupted} = pregel_worker:compute_vertices(
            Vertices, ComputeFn, 0, 1, #{}, VertexInputs
        ),
        T2 = erlang:monotonic_time(millisecond),
        Elapsed = T2 - T1,

        ?assertEqual([], Failed),
        ?assertEqual(3, length(Deltas)),
        %% 并发执行应在 250ms 内完成（3 * 100ms 顺序需要 300ms）
        ?assert(Elapsed < 250)
    after
        stop_dispatch_pool(PoolHandle)
    end.
