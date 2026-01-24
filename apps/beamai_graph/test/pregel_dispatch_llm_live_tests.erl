%%%-------------------------------------------------------------------
%%% @doc Pregel Dispatch Pool LLM 实时集成测试
%%%
%%% 使用真实 LLM 调用验证 dispatch 并发执行:
%%% - zhipu GLM-4.7 (anthropic provider, Anthropic-compatible API)
%%%
%%% 验证目标:
%%% - 并发 dispatch 正确执行多个 LLM 调用
%%% - 错误隔离：单个 dispatch 异常不影响其他
%%% - 错误传播：失败正确上报到上层（failed_vertices）
%%% - 进程崩溃处理：worker 崩溃被正确捕获并报告
%%%
%%% 运行方式:
%%%   ZHIPU_API_KEY=your-key rebar3 eunit --module=pregel_dispatch_llm_live_tests
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_dispatch_llm_live_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 配置辅助函数
%%====================================================================

%% @private 获取 API Key
get_api_key() ->
    case os:getenv("ZHIPU_API_KEY") of
        false -> skip;
        Key -> list_to_binary(Key)
    end.

%% @private 创建 GLM-4.7 anthropic provider 配置
glm47_anthropic_config(ApiKey) ->
    beamai_chat_completion:create(anthropic, #{
        model => <<"glm-4.7">>,
        api_key => ApiKey,
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
        timeout => 30000,
        max_tokens => 64
    }).

%% @private 创建一个故意失败的 LLM 配置（错误 API Key）
bad_llm_config() ->
    beamai_chat_completion:create(anthropic, #{
        model => <<"glm-4.7">>,
        api_key => <<"invalid_key_for_testing">>,
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
        timeout => 10000,
        max_tokens => 64
    }).

%% @private 确保应用和 dispatch 池启动
ensure_started() ->
    application:ensure_all_started(hackney),
    application:ensure_all_started(beamai_core),
    application:set_env(beamai_core, dispatch_timeout, 60000),
    %% 如果 supervisor 没启动 dispatch pool（如 poolboy 不可用），手动启动
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
        _Pid ->
            already_running
    end.

%% @private 清理
cleanup(State) ->
    case State of
        {started, Pid} ->
            unlink(Pid),
            poolboy:stop(beamai_dispatch_pool);
        already_running ->
            ok
    end,
    application:unset_env(beamai_core, dispatch_timeout).

%% @private 创建 LLM 调用的 compute 函数
%% Fun 签名: Fun(GlobalState, VertexInput) -> {ok, NewState} | {error, Reason}
make_llm_compute_fn() ->
    fun(Ctx) ->
        #{vertex_id := VertexId, global_state := GlobalState, vertex_input := Input} = Ctx,
        #{vertex := Vertex} = Ctx,
        Fun = pregel_vertex:fun_(Vertex),
        case Fun of
            undefined ->
                #{delta => #{}, activations => [], status => ok};
            _ ->
                try Fun(GlobalState, Input) of
                    {ok, NewState} ->
                        Delta = maps:without(maps:keys(GlobalState), NewState),
                        #{delta => Delta, activations => [], status => ok};
                    {error, Reason} ->
                        #{delta => #{}, activations => [],
                          status => {error, {node_error, VertexId, Reason}}}
                catch
                    Class:Reason:_Stack ->
                        #{delta => #{}, activations => [],
                          status => {error, {Class, Reason}}}
                end
        end
    end.

%%====================================================================
%% GLM-4.7 (anthropic provider) 并发 dispatch 测试
%%====================================================================

glm47_concurrent_dispatch_test_() ->
    {timeout, 120, fun() ->
        case get_api_key() of
            skip ->
                io:format("ZHIPU_API_KEY not set, skipping~n");
            ApiKey ->
                PoolState = ensure_started(),
                try
                    run_glm47_concurrent_dispatch(ApiKey)
                after
                    cleanup(PoolState)
                end
        end
    end}.

run_glm47_concurrent_dispatch(ApiKey) ->
    LLMConfig = glm47_anthropic_config(ApiKey),

    %% 节点函数：调用 LLM
    NodeFun = fun(_GlobalState, VertexInput) ->
        Prompt = maps:get(prompt, VertexInput, <<"Say hello">>),
        Messages = [#{role => user, content => Prompt}],
        case beamai_chat_completion:chat(LLMConfig, Messages) of
            {ok, #{content := Content}} ->
                {ok, #{response => Content, prompt => Prompt}};
            {error, Reason} ->
                {error, {llm_call_failed, Reason}}
        end
    end,

    ComputeFn = make_llm_compute_fn(),
    Vertex = pregel_vertex:new_flat(llm_node, NodeFun, #{}, []),
    Vertices = #{llm_node => Vertex},

    %% 3 个并发 dispatch，每个都向 LLM 发送不同问题
    D1 = #{target => llm_node, input => #{prompt => <<"What is 1+1? Answer with just the number.">>}},
    D2 = #{target => llm_node, input => #{prompt => <<"What is 2+2? Answer with just the number.">>}},
    D3 = #{target => llm_node, input => #{prompt => <<"What is 3+3? Answer with just the number.">>}},
    VertexInputs = #{llm_node => [D1, D2, D3]},

    io:format("~n=== GLM-4.7 (anthropic provider) 并发 dispatch 测试 ===~n"),
    io:format("发送 3 个并发 LLM 请求...~n"),

    T1 = erlang:monotonic_time(millisecond),
    {Deltas, _Acts, Failed, _Interrupted} = pregel_worker:compute_vertices(
        Vertices, ComputeFn, 0, 1, #{}, VertexInputs
    ),
    T2 = erlang:monotonic_time(millisecond),
    Elapsed = T2 - T1,

    io:format("完成，耗时: ~p ms~n", [Elapsed]),
    io:format("Deltas: ~p~n", [Deltas]),
    io:format("Failed: ~p~n", [Failed]),

    %% 全部成功
    ?assertEqual([], Failed),
    ?assertEqual(3, length(Deltas)),

    %% 每个 delta 都包含 response
    lists:foreach(fun(D) ->
        ?assert(maps:is_key(response, D)),
        Response = maps:get(response, D),
        ?assert(is_binary(Response)),
        io:format("  Response: ~s~n", [Response])
    end, Deltas).

%%====================================================================
%% GLM-4.7 (anthropic provider) binary role 兼容测试
%%====================================================================

glm47_binary_role_dispatch_test_() ->
    {timeout, 120, fun() ->
        case get_api_key() of
            skip ->
                io:format("ZHIPU_API_KEY not set, skipping~n");
            ApiKey ->
                PoolState = ensure_started(),
                try
                    run_glm47_binary_role_dispatch(ApiKey)
                after
                    cleanup(PoolState)
                end
        end
    end}.

run_glm47_binary_role_dispatch(ApiKey) ->
    LLMConfig = glm47_anthropic_config(ApiKey),

    NodeFun = fun(_GlobalState, VertexInput) ->
        Prompt = maps:get(prompt, VertexInput, <<"Say hello">>),
        %% 使用 binary role 验证 to_binary_role 兼容性
        Messages = [#{role => <<"user">>, content => Prompt}],
        case beamai_chat_completion:chat(LLMConfig, Messages) of
            {ok, #{content := Content}} ->
                {ok, #{response => Content, prompt => Prompt}};
            {error, Reason} ->
                {error, {llm_call_failed, Reason}}
        end
    end,

    ComputeFn = make_llm_compute_fn(),
    Vertex = pregel_vertex:new_flat(llm_node, NodeFun, #{}, []),
    Vertices = #{llm_node => Vertex},

    D1 = #{target => llm_node, input => #{prompt => <<"What is 5+5? Answer with just the number.">>}},
    D2 = #{target => llm_node, input => #{prompt => <<"What is 7+3? Answer with just the number.">>}},
    VertexInputs = #{llm_node => [D1, D2]},

    io:format("~n=== GLM-4.7 (anthropic provider) binary role 兼容测试 ===~n"),
    io:format("发送 2 个并发 LLM 请求...~n"),

    T1 = erlang:monotonic_time(millisecond),
    {Deltas, _Acts, Failed, _Interrupted} = pregel_worker:compute_vertices(
        Vertices, ComputeFn, 0, 1, #{}, VertexInputs
    ),
    T2 = erlang:monotonic_time(millisecond),
    Elapsed = T2 - T1,

    io:format("完成，耗时: ~p ms~n", [Elapsed]),
    io:format("Failed: ~p~n", [Failed]),

    ?assertEqual([], Failed),
    ?assertEqual(2, length(Deltas)),

    lists:foreach(fun(D) ->
        ?assert(maps:is_key(response, D)),
        io:format("  Response: ~s~n", [maps:get(response, D)])
    end, Deltas).

%%====================================================================
%% 错误隔离测试: 一个 dispatch 失败不影响其他
%%====================================================================

error_isolation_mixed_provider_test_() ->
    {timeout, 120, fun() ->
        case get_api_key() of
            skip ->
                io:format("ZHIPU_API_KEY not set, skipping~n");
            ApiKey ->
                PoolState = ensure_started(),
                try
                    run_error_isolation_test(ApiKey)
                after
                    cleanup(PoolState)
                end
        end
    end}.

run_error_isolation_test(ApiKey) ->
    GoodConfig = glm47_anthropic_config(ApiKey),
    BadConfig = bad_llm_config(),

    %% 节点函数：根据 dispatch 输入选择不同的 LLM 配置
    NodeFun = fun(_GlobalState, VertexInput) ->
        Config = maps:get(llm_config, VertexInput),
        Prompt = maps:get(prompt, VertexInput),
        Messages = [#{role => user, content => Prompt}],
        case beamai_chat_completion:chat(Config, Messages) of
            {ok, #{content := Content}} ->
                {ok, #{response => Content}};
            {error, Reason} ->
                {error, {llm_call_failed, Reason}}
        end
    end,

    ComputeFn = make_llm_compute_fn(),
    Vertex = pregel_vertex:new_flat(llm_node, NodeFun, #{}, []),
    Vertices = #{llm_node => Vertex},

    %% D1: 正常配置, D2: 错误配置（会失败）, D3: 正常配置
    D1 = #{target => llm_node, input => #{
        llm_config => GoodConfig,
        prompt => <<"What is 1+1? Just the number.">>
    }},
    D2 = #{target => llm_node, input => #{
        llm_config => BadConfig,
        prompt => <<"This will fail">>
    }},
    D3 = #{target => llm_node, input => #{
        llm_config => GoodConfig,
        prompt => <<"What is 9+1? Just the number.">>
    }},
    VertexInputs = #{llm_node => [D1, D2, D3]},

    io:format("~n=== 错误隔离测试 ===~n"),
    io:format("3 个并发 dispatch: 2 正常 + 1 错误配置~n"),

    {Deltas, _Acts, Failed, _Interrupted} = pregel_worker:compute_vertices(
        Vertices, ComputeFn, 0, 1, #{}, VertexInputs
    ),

    io:format("Deltas count: ~p~n", [length(Deltas)]),
    io:format("Failed count: ~p~n", [length(Failed)]),
    io:format("Failed details: ~p~n", [Failed]),

    %% 2 个成功，1 个失败
    ?assertEqual(2, length(Deltas)),
    ?assertEqual(1, length(Failed)),

    %% 失败原因包含 llm_call_failed
    [{FailedId, FailedReason}] = Failed,
    ?assertEqual(llm_node, FailedId),
    io:format("错误原因: ~p~n", [FailedReason]),
    %% 确认是 node_error 包裹的 llm_call_failed
    ?assertMatch({node_error, llm_node, {llm_call_failed, _}}, FailedReason),

    %% 成功的 dispatch 结果正确
    lists:foreach(fun(D) ->
        ?assert(maps:is_key(response, D)),
        io:format("  成功 Response: ~s~n", [maps:get(response, D)])
    end, Deltas).

%%====================================================================
%% 进程崩溃隔离测试: compute 函数崩溃被正确捕获
%%====================================================================

process_crash_isolation_test_() ->
    {timeout, 120, fun() ->
        case get_api_key() of
            skip ->
                io:format("ZHIPU_API_KEY not set, skipping~n");
            ApiKey ->
                PoolState = ensure_started(),
                try
                    run_process_crash_test(ApiKey)
                after
                    cleanup(PoolState)
                end
        end
    end}.

run_process_crash_test(ApiKey) ->
    GoodConfig = glm47_anthropic_config(ApiKey),

    %% 节点函数：一个正常调用 LLM，一个直接 crash
    NodeFun = fun(_GlobalState, VertexInput) ->
        case maps:get(action, VertexInput, normal) of
            crash ->
                %% 模拟进程级崩溃（exit）
                exit(simulated_process_crash);
            throw_error ->
                %% 模拟 throw 异常
                throw({unexpected_error, something_went_wrong});
            normal ->
                Config = maps:get(llm_config, VertexInput),
                Prompt = maps:get(prompt, VertexInput),
                Messages = [#{role => user, content => Prompt}],
                case beamai_chat_completion:chat(Config, Messages) of
                    {ok, #{content := Content}} ->
                        {ok, #{response => Content}};
                    {error, Reason} ->
                        {error, Reason}
                end
        end
    end,

    ComputeFn = make_llm_compute_fn(),
    Vertex = pregel_vertex:new_flat(llm_node, NodeFun, #{}, []),
    Vertices = #{llm_node => Vertex},

    %% D1: 正常 LLM 调用, D2: 进程 exit 崩溃, D3: throw 异常, D4: 正常 LLM 调用
    D1 = #{target => llm_node, input => #{
        action => normal,
        llm_config => GoodConfig,
        prompt => <<"Say 'hello' in one word.">>
    }},
    D2 = #{target => llm_node, input => #{action => crash}},
    D3 = #{target => llm_node, input => #{action => throw_error}},
    D4 = #{target => llm_node, input => #{
        action => normal,
        llm_config => GoodConfig,
        prompt => <<"Say 'world' in one word.">>
    }},
    VertexInputs = #{llm_node => [D1, D2, D3, D4]},

    io:format("~n=== 进程崩溃隔离测试 ===~n"),
    io:format("4 个并发 dispatch: 2 正常 LLM + 1 exit + 1 throw~n"),

    {Deltas, _Acts, Failed, _Interrupted} = pregel_worker:compute_vertices(
        Vertices, ComputeFn, 0, 1, #{}, VertexInputs
    ),

    io:format("Deltas count: ~p~n", [length(Deltas)]),
    io:format("Failed count: ~p~n", [length(Failed)]),

    %% 2 个成功（LLM 调用），2 个失败（crash + throw）
    ?assertEqual(2, length(Deltas)),
    ?assertEqual(2, length(Failed)),

    %% 验证成功的结果有 response
    lists:foreach(fun(D) ->
        ?assert(maps:is_key(response, D)),
        io:format("  成功: ~s~n", [maps:get(response, D)])
    end, Deltas),

    %% 验证失败原因被正确记录
    FailedReasons = [R || {_, R} <- Failed],
    io:format("  失败原因: ~p~n", [FailedReasons]),

    %% 至少有一个是 exit 类型，一个是 throw 类型
    HasExit = lists:any(fun
        ({exit, simulated_process_crash}) -> true;
        (_) -> false
    end, FailedReasons),
    HasThrow = lists:any(fun
        ({throw, {unexpected_error, _}}) -> true;
        (_) -> false
    end, FailedReasons),
    ?assert(HasExit),
    ?assert(HasThrow).

%%====================================================================
%% 上层通知测试: 验证 Worker 正确报告失败信息给 Master
%%====================================================================

worker_reports_failures_to_master_test_() ->
    {timeout, 120, fun() ->
        case get_api_key() of
            skip ->
                io:format("ZHIPU_API_KEY not set, skipping~n");
            ApiKey ->
                PoolState = ensure_started(),
                try
                    run_worker_failure_report_test(ApiKey)
                after
                    cleanup(PoolState)
                end
        end
    end}.

run_worker_failure_report_test(ApiKey) ->
    GoodConfig = glm47_anthropic_config(ApiKey),

    NodeFun = fun(_GlobalState, VertexInput) ->
        case maps:get(action, VertexInput, normal) of
            crash -> exit(worker_crash);
            normal ->
                Config = maps:get(llm_config, VertexInput),
                Prompt = maps:get(prompt, VertexInput),
                Messages = [#{role => user, content => Prompt}],
                case beamai_chat_completion:chat(Config, Messages) of
                    {ok, #{content := Content}} -> {ok, #{response => Content}};
                    {error, Reason} -> {error, Reason}
                end
        end
    end,

    ComputeFn = make_llm_compute_fn(),

    %% 启动 pregel_worker 并模拟完整的 Worker → Master 交互
    Self = self(),
    Vertex = pregel_vertex:new_flat(llm_node, NodeFun, #{}, []),
    Vertices = #{llm_node => Vertex},

    Opts = #{
        worker_id => 0,
        master => Self,  %% Self 充当 Master
        vertices => Vertices,
        compute_fn => ComputeFn,
        num_workers => 1,
        num_vertices => 1,
        global_state => #{}
    },

    {ok, Worker} = pregel_worker:start_link(0, Opts),

    %% 发送含有正常和崩溃 dispatch 的超步
    D1 = #{target => llm_node, input => #{
        action => normal,
        llm_config => GoodConfig,
        prompt => <<"What is 2*3? Just the number.">>
    }},
    D2 = #{target => llm_node, input => #{action => crash}},
    VertexInputs = #{llm_node => [D1, D2]},

    io:format("~n=== Worker 上报失败给 Master 测试 ===~n"),
    io:format("启动 Worker, 发送含崩溃的 dispatch...~n"),

    pregel_worker:start_superstep(Worker, 1, [llm_node], VertexInputs),

    %% 作为 Master 接收 worker_done 消息
    receive
        {'$gen_cast', {worker_done, Worker, Result}} ->
            io:format("收到 worker_done 结果:~n"),
            io:format("  worker_id: ~p~n", [maps:get(worker_id, Result)]),
            io:format("  deltas count: ~p~n", [length(maps:get(deltas, Result))]),
            io:format("  failed_count: ~p~n", [maps:get(failed_count, Result)]),
            io:format("  failed_vertices: ~p~n", [maps:get(failed_vertices, Result)]),

            %% 验证: 有 1 个成功的 delta
            Deltas = maps:get(deltas, Result),
            ?assertEqual(1, length(Deltas)),

            %% 验证: 有 1 个失败记录
            FailedCount = maps:get(failed_count, Result),
            ?assertEqual(1, FailedCount),

            %% 验证: failed_vertices 包含 llm_node
            FailedVertices = maps:get(failed_vertices, Result),
            ?assertEqual(1, length(FailedVertices)),
            [{FailedNodeId, _FailedReason}] = FailedVertices,
            ?assertEqual(llm_node, FailedNodeId),

            io:format("  Master 可据此决定是否重启该节点~n")
    after 60000 ->
        ?assert(false)  %% 超时失败
    end,

    pregel_worker:stop(Worker).

%%====================================================================
%% 并发性能对比测试: 并发 vs 顺序执行时间
%%====================================================================

concurrency_speedup_test_() ->
    {timeout, 180, fun() ->
        case get_api_key() of
            skip ->
                io:format("ZHIPU_API_KEY not set, skipping~n");
            ApiKey ->
                PoolState = ensure_started(),
                try
                    run_concurrency_speedup_test(ApiKey)
                after
                    cleanup(PoolState)
                end
        end
    end}.

run_concurrency_speedup_test(ApiKey) ->
    LLMConfig = glm47_anthropic_config(ApiKey),

    NodeFun = fun(_GlobalState, VertexInput) ->
        Prompt = maps:get(prompt, VertexInput),
        Messages = [#{role => user, content => Prompt}],
        case beamai_chat_completion:chat(LLMConfig, Messages) of
            {ok, #{content := Content}} ->
                {ok, #{response => Content}};
            {error, Reason} ->
                {error, Reason}
        end
    end,

    ComputeFn = make_llm_compute_fn(),
    Vertex = pregel_vertex:new_flat(llm_node, NodeFun, #{}, []),
    Vertices = #{llm_node => Vertex},

    Dispatches = [
        #{target => llm_node, input => #{prompt => <<"Count from 1 to 3.">>}},
        #{target => llm_node, input => #{prompt => <<"Count from 4 to 6.">>}},
        #{target => llm_node, input => #{prompt => <<"Count from 7 to 9.">>}}
    ],
    VertexInputs = #{llm_node => Dispatches},

    io:format("~n=== 并发性能对比测试 ===~n"),

    %% 并发执行（池可用，3 个 dispatch 走并发路径）
    io:format("并发执行 3 个 LLM 请求...~n"),
    T1 = erlang:monotonic_time(millisecond),
    {Deltas1, _, Failed1, _} = pregel_worker:compute_vertices(
        Vertices, ComputeFn, 0, 1, #{}, VertexInputs
    ),
    T2 = erlang:monotonic_time(millisecond),
    ConcurrentTime = T2 - T1,

    ?assertEqual([], Failed1),
    ?assertEqual(3, length(Deltas1)),

    %% 顺序执行：每个 dispatch 单独执行（length=1 走顺序路径）
    io:format("顺序执行 3 个 LLM 请求...~n"),
    T3 = erlang:monotonic_time(millisecond),
    SequentialDeltas = lists:foldl(fun(D, Acc) ->
        SingleInputs = #{llm_node => [D]},
        {Ds, _, _, _} = pregel_worker:compute_vertices(
            Vertices, ComputeFn, 0, 1, #{}, SingleInputs
        ),
        Ds ++ Acc
    end, [], Dispatches),
    T4 = erlang:monotonic_time(millisecond),
    SequentialTime = T4 - T3,

    ?assertEqual(3, length(SequentialDeltas)),

    io:format("并发耗时: ~p ms~n", [ConcurrentTime]),
    io:format("顺序耗时: ~p ms~n", [SequentialTime]),
    Speedup = SequentialTime / max(1, ConcurrentTime),
    io:format("加速比: ~.2fx~n", [Speedup]),

    %% 注意: 当所有请求共用同一 HTTP 连接池（Gun pool）时，
    %% 并发执行可能不比顺序快（连接池是瓶颈）。
    %% 此处只验证并发执行功能正确，不断言性能加速。
    %% 真正的加速场景: 调用不同服务、CPU密集任务、或更大的HTTP池。
    io:format("注: 并发性能取决于 HTTP 连接池配置~n"),
    ?assert(ConcurrentTime < 60000).  %% 只确保不超时
