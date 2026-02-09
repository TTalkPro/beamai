%%%-------------------------------------------------------------------
%%% @doc graph_command 模块单元测试
%%%
%%% 测试覆盖：
%%% - 构造函数和访问器
%%% - is_command 验证
%%% - graph_compute 集成：goto 覆盖边路由
%%% - graph_compute 集成：无 goto 回退正常路由
%%% - graph_compute 集成：goto 多节点并行
%%% - graph_compute 集成：goto '__end__'
%%% - graph_compute 集成：goto dispatch 列表
%%% - graph_compute 集成：无边无 goto 默认 '__end__'
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_command_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 构造函数测试
%%====================================================================

%% 测试：new/1 创建完整 Command
new_full_command_test() ->
    Cmd = beamai_graph_command:new(#{
        update => #{key => value},
        goto => next_node,
        graph => current
    }),
    ?assertEqual(#{key => value}, beamai_graph_command:get_update(Cmd)),
    ?assertEqual(next_node, beamai_graph_command:get_goto(Cmd)),
    ?assertEqual(current, beamai_graph_command:get_graph(Cmd)),
    ?assert(beamai_graph_command:is_command(Cmd)).

%% 测试：new/1 使用默认值
new_defaults_test() ->
    Cmd = beamai_graph_command:new(#{}),
    ?assertEqual(#{}, beamai_graph_command:get_update(Cmd)),
    ?assertEqual(undefined, beamai_graph_command:get_goto(Cmd)),
    ?assertEqual(undefined, beamai_graph_command:get_graph(Cmd)),
    ?assert(beamai_graph_command:is_command(Cmd)).

%% 测试：new/1 仅 update
new_update_only_test() ->
    Cmd = beamai_graph_command:new(#{update => #{a => 1, b => 2}}),
    ?assertEqual(#{a => 1, b => 2}, beamai_graph_command:get_update(Cmd)),
    ?assertEqual(undefined, beamai_graph_command:get_goto(Cmd)).

%% 测试：new/1 仅 goto
new_goto_only_test() ->
    Cmd = beamai_graph_command:new(#{goto => target_node}),
    ?assertEqual(#{}, beamai_graph_command:get_update(Cmd)),
    ?assertEqual(target_node, beamai_graph_command:get_goto(Cmd)).

%% 测试：goto/1 快捷构造器
goto_shortcut_test() ->
    Cmd = beamai_graph_command:goto(my_target),
    ?assertEqual(my_target, beamai_graph_command:get_goto(Cmd)),
    ?assertEqual(#{}, beamai_graph_command:get_update(Cmd)).

%% 测试：goto/2 快捷构造器
goto_with_update_test() ->
    Cmd = beamai_graph_command:goto(my_target, #{x => 1}),
    ?assertEqual(my_target, beamai_graph_command:get_goto(Cmd)),
    ?assertEqual(#{x => 1}, beamai_graph_command:get_update(Cmd)).

%% 测试：update/1 快捷构造器
update_shortcut_test() ->
    Cmd = beamai_graph_command:update(#{foo => bar}),
    ?assertEqual(#{foo => bar}, beamai_graph_command:get_update(Cmd)),
    ?assertEqual(undefined, beamai_graph_command:get_goto(Cmd)).

%% 测试：update/2 快捷构造器
update_with_goto_test() ->
    Cmd = beamai_graph_command:update(#{foo => bar}, next),
    ?assertEqual(#{foo => bar}, beamai_graph_command:get_update(Cmd)),
    ?assertEqual(next, beamai_graph_command:get_goto(Cmd)).

%%====================================================================
%% is_command 验证测试
%%====================================================================

%% 测试：有效的 Command 通过验证
is_command_valid_test() ->
    Cmd = beamai_graph_command:new(#{update => #{}, goto => node_a}),
    ?assert(beamai_graph_command:is_command(Cmd)).

%% 测试：非 map 不是 Command
is_command_non_map_test() ->
    ?assertNot(beamai_graph_command:is_command(not_a_map)),
    ?assertNot(beamai_graph_command:is_command(42)),
    ?assertNot(beamai_graph_command:is_command([])).

%% 测试：缺少 __command__ 标记不是 Command
is_command_no_marker_test() ->
    ?assertNot(beamai_graph_command:is_command(#{update => #{}, goto => a, graph => undefined})).

%% 测试：goto 为 atom 列表是有效的
is_command_goto_atom_list_test() ->
    Cmd = beamai_graph_command:new(#{goto => [a, b, c]}),
    ?assert(beamai_graph_command:is_command(Cmd)).

%% 测试：goto 为空列表是有效的
is_command_goto_empty_list_test() ->
    Cmd = beamai_graph_command:new(#{goto => []}),
    ?assert(beamai_graph_command:is_command(Cmd)).

%% 测试：goto 为 dispatch 是有效的
is_command_goto_dispatch_test() ->
    D = beamai_graph_dispatch:dispatch(target_node, #{input => 1}),
    Cmd = beamai_graph_command:new(#{goto => D}),
    ?assert(beamai_graph_command:is_command(Cmd)).

%% 测试：goto 为 dispatch 列表是有效的
is_command_goto_dispatch_list_test() ->
    D1 = beamai_graph_dispatch:dispatch(node_a, #{x => 1}),
    D2 = beamai_graph_dispatch:dispatch(node_b, #{x => 2}),
    Cmd = beamai_graph_command:new(#{goto => [D1, D2]}),
    ?assert(beamai_graph_command:is_command(Cmd)).

%% 测试：graph 为 parent 是有效的
is_command_graph_parent_test() ->
    Cmd = beamai_graph_command:new(#{graph => parent}),
    ?assert(beamai_graph_command:is_command(Cmd)).

%% 测试：无效 graph 值
new_invalid_graph_test() ->
    ?assertError({invalid_command_options, _},
                 beamai_graph_command:new(#{graph => invalid_value})).

%% 测试：无效 goto 值
new_invalid_goto_test() ->
    ?assertError({invalid_command_options, _},
                 beamai_graph_command:new(#{goto => 12345})).

%%====================================================================
%% graph_compute 集成测试
%%====================================================================

%% 辅助函数：创建带 Command 节点的测试上下文
make_command_context(VertexId, GlobalState, Fun, RoutingEdges) ->
    #{
        vertex_id => VertexId,
        context => GlobalState,
        vertex_input => undefined,
        superstep => 1,
        num_vertices => 3,
        vertex => beamai_pregel_vertex:new_flat(VertexId, Fun, #{}, RoutingEdges)
    }.

%% 测试：goto 覆盖边路由
command_goto_overrides_edge_routing_test() ->
    %% 节点返回 Command，goto 到 target_b
    %% 边指向 target_a
    Fun = fun(_State, _) ->
        {command, beamai_graph_command:goto(target_b, #{result => done})}
    end,
    Edge = beamai_graph_edge:direct(test_node, target_a),
    GlobalState = beamai_context:new(#{initial => true}),
    Ctx = make_command_context(test_node, GlobalState, Fun, [Edge]),

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    ?assertEqual(ok, maps:get(status, Result)),
    %% goto 覆盖了边路由，应该激活 target_b 而不是 target_a
    ?assertEqual([target_b], maps:get(activations, Result)),
    %% delta 应该是 Command 的 update（key 已标准化为 binary）
    Delta = maps:get(delta, Result),
    ?assertEqual(done, maps:get(<<"result">>, Delta)).

%% 测试：无 goto 时回退正常边路由
command_no_goto_falls_back_to_edge_routing_test() ->
    %% 节点返回 Command，goto 为 undefined
    %% 边使用条件路由
    Fun = fun(_State, _) ->
        {command, beamai_graph_command:update(#{flag => true})}
    end,
    %% 条件边：根据 flag 值路由
    Router = fun(State) ->
        case beamai_context:get(State, flag) of
            true -> route_a;
            _ -> route_b
        end
    end,
    Edge = beamai_graph_edge:conditional(test_node, Router),
    GlobalState = beamai_context:new(#{}),
    Ctx = make_command_context(test_node, GlobalState, Fun, [Edge]),

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    ?assertEqual(ok, maps:get(status, Result)),
    %% delta 合并后 flag=true，路由应该选择 route_a
    ?assertEqual([route_a], maps:get(activations, Result)),
    %% delta 是 update 内容（key 已标准化为 binary）
    Delta = maps:get(delta, Result),
    ?assertEqual(true, maps:get(<<"flag">>, Delta)).

%% 测试：goto 多节点并行
command_goto_multiple_nodes_test() ->
    Fun = fun(_State, _) ->
        {command, beamai_graph_command:new(#{
            update => #{step => parallel},
            goto => [worker_a, worker_b, worker_c]
        })}
    end,
    GlobalState = beamai_context:new(#{}),
    Ctx = make_command_context(test_node, GlobalState, Fun, []),

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    ?assertEqual(ok, maps:get(status, Result)),
    ?assertEqual([worker_a, worker_b, worker_c], maps:get(activations, Result)),
    Delta = maps:get(delta, Result),
    ?assertEqual(parallel, maps:get(<<"step">>, Delta)).

%% 测试：goto '__end__'
command_goto_end_test() ->
    Fun = fun(_State, _) ->
        {command, beamai_graph_command:goto('__end__', #{done => true})}
    end,
    Edge = beamai_graph_edge:direct(test_node, next_node),
    GlobalState = beamai_context:new(#{}),
    Ctx = make_command_context(test_node, GlobalState, Fun, [Edge]),

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    ?assertEqual(ok, maps:get(status, Result)),
    %% goto __end__ 覆盖边路由
    ?assertEqual(['__end__'], maps:get(activations, Result)).

%% 测试：goto dispatch 列表
command_goto_dispatch_list_test() ->
    D1 = beamai_graph_dispatch:dispatch(node_a, #{batch => 1}),
    D2 = beamai_graph_dispatch:dispatch(node_b, #{batch => 2}),
    Fun = fun(_State, _) ->
        {command, beamai_graph_command:new(#{
            update => #{dispatched => true},
            goto => [D1, D2]
        })}
    end,
    GlobalState = beamai_context:new(#{}),
    Ctx = make_command_context(test_node, GlobalState, Fun, []),

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    ?assertEqual(ok, maps:get(status, Result)),
    Activations = maps:get(activations, Result),
    ?assertEqual(2, length(Activations)),
    %% 验证 activations 是 {dispatch, D} 形式
    [{dispatch, AD1}, {dispatch, AD2}] = Activations,
    ?assertEqual(node_a, beamai_graph_dispatch:get_node(AD1)),
    ?assertEqual(node_b, beamai_graph_dispatch:get_node(AD2)).

%% 测试：goto 单个 dispatch
command_goto_single_dispatch_test() ->
    D = beamai_graph_dispatch:dispatch(worker, #{task => analyze}),
    Fun = fun(_State, _) ->
        {command, beamai_graph_command:new(#{goto => D})}
    end,
    GlobalState = beamai_context:new(#{}),
    Ctx = make_command_context(test_node, GlobalState, Fun, []),

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    ?assertEqual(ok, maps:get(status, Result)),
    [{dispatch, AD}] = maps:get(activations, Result),
    ?assertEqual(worker, beamai_graph_dispatch:get_node(AD)).

%% 测试：无边无 goto 默认到 '__end__'
command_no_edges_no_goto_defaults_to_end_test() ->
    Fun = fun(_State, _) ->
        {command, beamai_graph_command:update(#{x => 1})}
    end,
    GlobalState = beamai_context:new(#{}),
    Ctx = make_command_context(test_node, GlobalState, Fun, []),

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    ?assertEqual(ok, maps:get(status, Result)),
    %% 无边且无 goto，build_activations 默认激活 __end__
    ?assertEqual(['__end__'], maps:get(activations, Result)).

%% 测试：goto 空列表默认到 '__end__'
command_goto_empty_list_defaults_to_end_test() ->
    Fun = fun(_State, _) ->
        {command, beamai_graph_command:new(#{goto => []})}
    end,
    GlobalState = beamai_context:new(#{}),
    Ctx = make_command_context(test_node, GlobalState, Fun, []),

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    ?assertEqual(ok, maps:get(status, Result)),
    ?assertEqual(['__end__'], maps:get(activations, Result)).

%% 测试：Command 的 update 跳过 compute_delta（直接作为 delta）
command_update_bypasses_compute_delta_test() ->
    %% 设置 GlobalState 中已有 existing 字段
    GlobalState = beamai_context:new(#{existing => old_value}),
    Fun = fun(_State, _) ->
        %% 只更新 new_field，不改变 existing
        {command, beamai_graph_command:update(#{new_field => 42})}
    end,
    Ctx = make_command_context(test_node, GlobalState, Fun, []),

    ComputeFn = beamai_graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    Delta = maps:get(delta, Result),
    %% delta 应该只包含 Command 的 update 内容（key 已标准化为 binary）
    ?assertEqual(#{<<"new_field">> => 42}, Delta),
    %% 不应该包含 existing 字段（不像 compute_delta 会对比全量状态）
    ?assertNot(maps:is_key(<<"existing">>, Delta)).

%%====================================================================
%% graph_node 集成测试
%%====================================================================

%% 测试：beamai_graph_node:execute 正确传递 Command 结果
graph_node_execute_command_test() ->
    Fun = fun(_State, _) ->
        {command, beamai_graph_command:goto(next)}
    end,
    Node = beamai_graph_node:new(test_node, Fun),
    State = beamai_context:new(#{}),
    Result = beamai_graph_node:execute(Node, State),
    ?assertMatch({command, _}, Result),
    {command, Cmd} = Result,
    ?assertEqual(next, beamai_graph_command:get_goto(Cmd)).

%% 测试：beamai_graph_node:execute 拒绝无效 Command
graph_node_execute_invalid_command_test() ->
    Fun = fun(_State, _) ->
        {command, #{not_a_command => true}}
    end,
    Node = beamai_graph_node:new(test_node, Fun),
    State = beamai_context:new(#{}),
    Result = beamai_graph_node:execute(Node, State),
    ?assertMatch({error, {invalid_command, test_node, _}}, Result).
