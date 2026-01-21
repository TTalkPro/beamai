%%%-------------------------------------------------------------------
%%% @doc pregel_master 步进式 API 和 state reducer 单元测试
%%%
%%% 测试:
%%% - 步进式执行 API (start/step/get_result/stop)
%%% - State reducer 功能
%%% - Checkpoint 数据获取
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_master_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建简单测试图（三顶点链式）
%% v1 -> v2 -> v3
make_chain_graph() ->
    Edges = [{v1, v2}, {v2, v3}],
    InitialValues = #{v1 => 1, v2 => 2, v3 => 3},
    pregel_graph:from_edges(Edges, InitialValues).

%% 创建扇形图（一个中心顶点，多个源顶点）
%% v1 -> v_center
%% v2 -> v_center
%% v3 -> v_center
make_fan_graph() ->
    Edges = [{v1, v_center}, {v2, v_center}, {v3, v_center}],
    InitialValues = #{v1 => 10, v2 => 20, v3 => 30, v_center => 0},
    pregel_graph:from_edges(Edges, InitialValues).

%% 创建简单计算函数（所有顶点立即停止）
make_halt_compute_fn() ->
    fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        HaltedVertex = pregel_vertex:halt(Vertex),
        #{vertex => HaltedVertex, outbox => [], status => ok}
    end.

%% 创建发送同一目标多条消息的计算函数
make_send_multi_to_target_compute_fn() ->
    fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Superstep = maps:get(superstep, Ctx),
        Messages = maps:get(messages, Ctx, []),
        case Superstep of
            0 ->
                case Id of
                    v1 ->
                        %% v1 向 v2 发送 3 条消息
                        Outbox = [{v2, msg1}, {v2, msg2}, {v2, msg3}],
                        #{vertex => pregel_vertex:halt(Vertex), outbox => Outbox, status => ok};
                    _ ->
                        #{vertex => Vertex, outbox => [], status => ok}
                end;
            _ ->
                %% 后续超步：保存收到的消息并停止
                NewVertex = pregel_vertex:set_value(Vertex, Messages),
                #{vertex => pregel_vertex:halt(NewVertex), outbox => [], status => ok}
        end
    end.

%% 运行步进式 Pregel 直到完成
run_until_done(Master) ->
    case pregel_master:step(Master) of
        {continue, _Info} ->
            run_until_done(Master);
        {done, _Reason, _Info} ->
            pregel_master:get_result(Master)
    end.

%%====================================================================
%% 步进式 API 测试
%%====================================================================

%% 测试：基本步进式执行
step_api_basic_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_halt_compute_fn(),

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{num_workers => 1}),
    try
        %% 第一步返回 initial checkpoint（不执行超步）
        Step1 = pregel_master:step(Master),
        ?assertMatch({continue, #{type := initial}}, Step1),

        %% 第二步执行超步 0，应该完成
        Step2 = pregel_master:step(Master),
        ?assertMatch({done, completed, _}, Step2),

        %% 获取结果
        Result = pregel_master:get_result(Master),
        ?assertEqual(completed, maps:get(status, Result))
    after
        pregel_master:stop(Master)
    end.

%% 测试：多步执行
step_api_multi_step_test() ->
    Graph = make_chain_graph(),
    %% 创建需要多步的计算函数
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Superstep = maps:get(superstep, Ctx),
        case Superstep of
            0 ->
                case Id of
                    v1 ->
                        Outbox = [{v2, hello}],
                        #{vertex => pregel_vertex:halt(Vertex), outbox => Outbox, status => ok};
                    _ ->
                        #{vertex => Vertex, outbox => [], status => ok}
                end;
            _ ->
                #{vertex => pregel_vertex:halt(Vertex), outbox => [], status => ok}
        end
    end,

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{num_workers => 1}),
    try
        %% 第一步返回 initial checkpoint（不执行超步）
        Step1 = pregel_master:step(Master),
        ?assertMatch({continue, #{type := initial}}, Step1),

        %% 第二步执行超步 0，v1 发送消息给 v2，应该继续
        Step2 = pregel_master:step(Master),
        ?assertMatch({continue, #{type := step}}, Step2),

        %% 第三步执行超步 1，v2 处理消息并停止，应该完成
        Step3 = pregel_master:step(Master),
        ?assertMatch({done, completed, _}, Step3)
    after
        pregel_master:stop(Master)
    end.

%% 测试：get_checkpoint_data
get_checkpoint_data_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_halt_compute_fn(),

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{num_workers => 1}),
    try
        %% 执行一步
        _Step = pregel_master:step(Master),

        %% 获取 checkpoint 数据
        CheckpointData = pregel_master:get_checkpoint_data(Master),

        ?assert(maps:is_key(superstep, CheckpointData)),
        ?assert(maps:is_key(vertices, CheckpointData)),
        ?assert(maps:is_key(pending_messages, CheckpointData)),
        ?assert(maps:is_key(vertex_inbox, CheckpointData))
    after
        pregel_master:stop(Master)
    end.

%%====================================================================
%% 默认 State Reducer 测试（last_write_win）
%%====================================================================

%% 测试：默认 reducer 只保留最后一条消息
default_state_reducer_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_send_multi_to_target_compute_fn(),

    %% 不传 state_reducer，使用默认的 last_write_win
    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{num_workers => 1}),
    try
        Result = run_until_done(Master),

        %% 验证执行成功
        ?assertEqual(completed, maps:get(status, Result)),

        %% 获取结果图
        FinalGraph = maps:get(graph, Result),

        %% 获取 v2 的最终值（应该只有最后一条消息）
        V2 = pregel_graph:get(FinalGraph, v2),
        V2Value = pregel_vertex:value(V2),

        %% 默认 last_write_win 应该只保留最后一条消息
        ?assertEqual([msg3], V2Value)
    after
        pregel_master:stop(Master)
    end.

%%====================================================================
%% 自定义 State Reducer 测试
%%====================================================================

%% 测试：append reducer 保留所有消息
custom_state_reducer_append_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_send_multi_to_target_compute_fn(),

    %% 自定义 reducer：保留所有消息
    AppendReducer = fun(#{messages := Msgs}) -> Msgs end,

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        state_reducer => AppendReducer
    }),
    try
        Result = run_until_done(Master),

        %% 验证执行成功
        ?assertEqual(completed, maps:get(status, Result)),

        %% 获取结果图
        FinalGraph = maps:get(graph, Result),

        %% 获取 v2 的最终值（应该包含所有消息）
        V2 = pregel_graph:get(FinalGraph, v2),
        V2Value = pregel_vertex:value(V2),

        %% append reducer 应该保留所有消息
        ?assertEqual([msg1, msg2, msg3], V2Value)
    after
        pregel_master:stop(Master)
    end.

%% 测试：数值求和 reducer
state_reducer_sum_test() ->
    Graph = make_fan_graph(),

    %% 创建发送数值消息的计算函数
    ComputeFn = fun(Ctx) ->
        Vertex = maps:get(vertex, Ctx),
        Id = pregel_vertex:id(Vertex),
        Superstep = maps:get(superstep, Ctx),
        Messages = maps:get(messages, Ctx, []),
        case Superstep of
            0 ->
                case Id of
                    v_center ->
                        #{vertex => Vertex, outbox => [], status => ok};
                    _ ->
                        %% 源顶点向中心发送自己的值
                        Value = pregel_vertex:value(Vertex),
                        #{vertex => pregel_vertex:halt(Vertex), outbox => [{v_center, Value}], status => ok}
                end;
            _ ->
                %% 保存收到的消息并停止
                NewVertex = pregel_vertex:set_value(Vertex, Messages),
                #{vertex => pregel_vertex:halt(NewVertex), outbox => [], status => ok}
        end
    end,

    %% 数值求和 reducer
    SumReducer = fun(#{messages := Msgs}) -> [lists:sum(Msgs)] end,

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        state_reducer => SumReducer
    }),
    try
        Result = run_until_done(Master),

        %% 验证执行成功
        ?assertEqual(completed, maps:get(status, Result)),

        %% 获取结果图
        FinalGraph = maps:get(graph, Result),

        %% 获取 v_center 的最终值
        VCenter = pregel_graph:get(FinalGraph, v_center),
        VCenterValue = pregel_vertex:value(VCenter),

        %% 求和 reducer：10 + 20 + 30 = 60
        ?assertEqual([60], VCenterValue)
    after
        pregel_master:stop(Master)
    end.

%%====================================================================
%% 边界情况测试
%%====================================================================

%% 测试：空消息列表
state_reducer_empty_messages_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_halt_compute_fn(),

    %% 自定义 reducer
    AppendReducer = fun(#{messages := Msgs}) -> Msgs end,

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        state_reducer => AppendReducer
    }),
    try
        Result = run_until_done(Master),
        ?assertEqual(completed, maps:get(status, Result))
    after
        pregel_master:stop(Master)
    end.

%% 测试：reducer 返回空列表
state_reducer_returns_empty_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_send_multi_to_target_compute_fn(),

    %% 过滤掉所有消息的 reducer
    FilterReducer = fun(#{messages := _Msgs}) -> [] end,

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        state_reducer => FilterReducer
    }),
    try
        Result = run_until_done(Master),

        %% 验证执行成功
        ?assertEqual(completed, maps:get(status, Result)),

        %% 获取结果图
        FinalGraph = maps:get(graph, Result),

        %% v2 不应该收到任何消息
        V2 = pregel_graph:get(FinalGraph, v2),
        V2Value = pregel_vertex:value(V2),

        %% reducer 返回空列表，v2 不应该收到消息
        ?assertEqual([], V2Value)
    after
        pregel_master:stop(Master)
    end.

%%====================================================================
%% 简化 API 测试（pregel:run）
%%====================================================================

%% 测试：pregel:run 简化 API
pregel_run_api_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_halt_compute_fn(),

    Result = pregel:run(Graph, ComputeFn, #{num_workers => 1}),

    ?assertEqual(completed, maps:get(status, Result)).
