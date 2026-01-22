%%%-------------------------------------------------------------------
%%% @doc pregel_master 步进式 API 和全局状态模式单元测试
%%%
%%% 测试:
%%% - 步进式执行 API (start/step/get_result/stop)
%%% - 全局状态和 field_reducers 功能
%%% - Delta 增量更新合并
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
    pregel_graph:from_edges(Edges).

%% 创建扇形图（一个中心顶点，多个源顶点）
%% v1 -> v_center
%% v2 -> v_center
%% v3 -> v_center
make_fan_graph() ->
    Edges = [{v1, v_center}, {v2, v_center}, {v3, v_center}],
    pregel_graph:from_edges(Edges).

%% 创建简单计算函数（所有顶点立即停止）
%% 全局状态模式：返回 delta 而非 vertex
make_halt_compute_fn() ->
    fun(Ctx) ->
        #{delta => #{}, outbox => [], status => ok}
    end.

%% 创建发送消息并更新状态的计算函数
%% 全局状态模式：通过 delta 更新状态
make_send_compute_fn() ->
    fun(Ctx) ->
        #{vertex_id := Id, global_state := State, superstep := Superstep} = Ctx,
        case Superstep of
            0 ->
                case Id of
                    v1 ->
                        %% v1 向 v2 发送激活消息，更新全局状态
                        Outbox = [{v2, activate}],
                        Delta = #{v1_done => true},
                        #{delta => Delta, outbox => Outbox, status => ok};
                    _ ->
                        #{delta => #{}, outbox => [], status => ok}
                end;
            _ ->
                %% 后续超步：标记完成
                Delta = #{atom_to_binary(Id, utf8) => done},
                #{delta => Delta, outbox => [], status => ok}
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
    InitialState = #{test => initial},

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        global_state => InitialState
    }),
    try
        %% 第一步返回 initial checkpoint（不执行超步）
        Step1 = pregel_master:step(Master),
        ?assertMatch({continue, #{type := initial}}, Step1),

        %% 第二步执行超步 0，应该完成
        Step2 = pregel_master:step(Master),
        ?assertMatch({done, completed, _}, Step2),

        %% 获取结果
        Result = pregel_master:get_result(Master),
        ?assertEqual(completed, maps:get(status, Result)),
        %% 验证全局状态存在
        ?assert(maps:is_key(global_state, Result))
    after
        pregel_master:stop(Master)
    end.

%% 测试：多步执行
step_api_multi_step_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_send_compute_fn(),
    InitialState = #{started => true},

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        global_state => InitialState
    }),
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
    InitialState = #{checkpoint_test => true},

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        global_state => InitialState
    }),
    try
        %% 执行一步
        _Step = pregel_master:step(Master),

        %% 获取 checkpoint 数据
        CheckpointData = pregel_master:get_checkpoint_data(Master),

        ?assert(maps:is_key(superstep, CheckpointData)),
        ?assert(maps:is_key(vertices, CheckpointData)),
        ?assert(maps:is_key(vertex_inbox, CheckpointData)),
        ?assert(maps:is_key(pending_deltas, CheckpointData)),
        %% 全局状态模式：检查 global_state
        ?assert(maps:is_key(global_state, CheckpointData))
    after
        pregel_master:stop(Master)
    end.

%%====================================================================
%% 全局状态和 Delta 测试
%%====================================================================

%% 测试：全局状态初始化
global_state_initialization_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_halt_compute_fn(),
    InitialState = #{key1 => value1, key2 => value2},

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        global_state => InitialState
    }),
    try
        Result = run_until_done(Master),

        %% 验证执行成功
        ?assertEqual(completed, maps:get(status, Result)),

        %% 验证全局状态保留初始值
        GlobalState = maps:get(global_state, Result),
        ?assertEqual(value1, maps:get(key1, GlobalState)),
        ?assertEqual(value2, maps:get(key2, GlobalState))
    after
        pregel_master:stop(Master)
    end.

%% 测试：Delta 合并
delta_merge_test() ->
    Graph = make_chain_graph(),
    %% 创建更新状态的计算函数
    ComputeFn = fun(Ctx) ->
        #{vertex_id := Id, superstep := Superstep} = Ctx,
        case Superstep of
            0 ->
                %% 超步 0：每个顶点更新自己的键
                Key = list_to_atom(atom_to_list(Id) ++ "_result"),
                Delta = #{Key => Id},
                #{delta => Delta, outbox => [], status => ok};
            _ ->
                #{delta => #{}, outbox => [], status => ok}
        end
    end,
    InitialState = #{initial => true},

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        global_state => InitialState
    }),
    try
        Result = run_until_done(Master),

        %% 验证执行成功
        ?assertEqual(completed, maps:get(status, Result)),

        %% 验证所有顶点的 delta 都被合并
        %% 注意：graph_state 将所有键转换为 binary，使用 graph_state:get 访问
        GlobalState = maps:get(global_state, Result),
        ?assertEqual(v1, graph_state:get(GlobalState, v1_result)),
        ?assertEqual(v2, graph_state:get(GlobalState, v2_result)),
        ?assertEqual(v3, graph_state:get(GlobalState, v3_result))
    after
        pregel_master:stop(Master)
    end.

%%====================================================================
%% Field Reducers 测试
%%====================================================================

%% 测试：append field reducer
field_reducer_append_test() ->
    Graph = make_fan_graph(),
    %% 创建向 messages 列表追加的计算函数
    ComputeFn = fun(Ctx) ->
        #{vertex_id := Id, superstep := Superstep} = Ctx,
        case Superstep of
            0 ->
                case Id of
                    v_center ->
                        #{delta => #{}, outbox => [], status => ok};
                    _ ->
                        %% 源顶点发送消息并更新 messages 列表
                        Delta = #{messages => [Id]},
                        #{delta => Delta, outbox => [{v_center, activate}], status => ok}
                end;
            _ ->
                #{delta => #{}, outbox => [], status => ok}
        end
    end,
    InitialState = #{messages => []},

    %% 配置 append reducer 用于 messages 字段
    %% 注意：graph_state 将 atom 键转换为 binary，需要使用 binary 键
    FieldReducers = #{
        <<"messages">> => fun graph_state_reducer:append_reducer/2
    },

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        global_state => InitialState,
        field_reducers => FieldReducers
    }),
    try
        Result = run_until_done(Master),

        %% 验证执行成功
        ?assertEqual(completed, maps:get(status, Result)),

        %% 验证 messages 被正确追加
        GlobalState = maps:get(global_state, Result),
        Messages = graph_state:get(GlobalState, messages),
        %% 所有源顶点都应该在 messages 列表中
        ?assertEqual(3, length(Messages)),
        ?assert(lists:member(v1, Messages)),
        ?assert(lists:member(v2, Messages)),
        ?assert(lists:member(v3, Messages))
    after
        pregel_master:stop(Master)
    end.

%% 测试：merge field reducer
field_reducer_merge_test() ->
    Graph = make_chain_graph(),
    %% 创建合并 context map 的计算函数
    ComputeFn = fun(Ctx) ->
        #{vertex_id := Id, superstep := Superstep} = Ctx,
        case Superstep of
            0 ->
                Key = atom_to_binary(Id, utf8),
                Delta = #{context => #{Key => true}},
                #{delta => Delta, outbox => [], status => ok};
            _ ->
                #{delta => #{}, outbox => [], status => ok}
        end
    end,
    InitialState = #{context => #{}},

    %% 配置 merge reducer 用于 context 字段
    %% 注意：graph_state 将 atom 键转换为 binary，需要使用 binary 键
    FieldReducers = #{
        <<"context">> => fun graph_state_reducer:merge_reducer/2
    },

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        global_state => InitialState,
        field_reducers => FieldReducers
    }),
    try
        Result = run_until_done(Master),

        %% 验证执行成功
        ?assertEqual(completed, maps:get(status, Result)),

        %% 验证 context 被正确合并
        GlobalState = maps:get(global_state, Result),
        Context = graph_state:get(GlobalState, context),
        ?assertEqual(true, maps:get(<<"v1">>, Context)),
        ?assertEqual(true, maps:get(<<"v2">>, Context)),
        ?assertEqual(true, maps:get(<<"v3">>, Context))
    after
        pregel_master:stop(Master)
    end.

%%====================================================================
%% 边界情况测试
%%====================================================================

%% 测试：空 delta
empty_delta_test() ->
    Graph = make_chain_graph(),
    ComputeFn = make_halt_compute_fn(),
    InitialState = #{initial => true},

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{
        num_workers => 1,
        global_state => InitialState
    }),
    try
        Result = run_until_done(Master),
        ?assertEqual(completed, maps:get(status, Result)),
        %% 状态应保持不变
        GlobalState = maps:get(global_state, Result),
        ?assertEqual(true, maps:get(initial, GlobalState))
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
    InitialState = #{test => value},

    Result = pregel:run(Graph, ComputeFn, #{
        num_workers => 1,
        global_state => InitialState
    }),

    ?assertEqual(completed, maps:get(status, Result)).
