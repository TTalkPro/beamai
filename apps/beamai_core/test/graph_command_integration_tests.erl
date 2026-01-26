%%%-------------------------------------------------------------------
%%% @doc Graph Command 端到端集成测试
%%%
%%% 测试 Command 返回类型在完整图执行流程中的行为：
%%% - Pregel 引擎（graph_runner:run）
%%% - 流式执行（graph_runner:stream）
%%% - 与现有 {ok, State} 代码的兼容性
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_command_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Pregel 引擎集成测试
%%====================================================================

%% 测试：Command goto 在完整图执行中正确路由
command_goto_full_graph_test() ->
    %% 构建图：start -> decide -> (target_a | target_b) -> __end__
    %% decide 节点使用 Command 直接指定路由
    DecideFun = fun(State, _) ->
        Value = graph_state:get(State, value, 0),
        Target = case Value > 5 of
            true -> target_a;
            false -> target_b
        end,
        {command, graph_command:goto(Target, #{decided => true})}
    end,
    TargetAFun = fun(State, _) ->
        {ok, graph_state:set(State, result, <<"path_a">>)}
    end,
    TargetBFun = fun(State, _) ->
        {ok, graph_state:set(State, result, <<"path_b">>)}
    end,

    {ok, Graph} = graph:build([
        {node, decide, DecideFun},
        {node, target_a, TargetAFun},
        {node, target_b, TargetBFun},
        {edge, target_a, '__end__'},
        {edge, target_b, '__end__'},
        {entry, decide}
    ]),

    %% 测试路径 A（value > 5）
    StateA = graph:state(#{value => 10}),
    ResultA = graph:run(Graph, StateA),
    ?assertEqual(completed, maps:get(status, ResultA)),
    FinalA = maps:get(final_state, ResultA),
    ?assertEqual(<<"path_a">>, graph_state:get(FinalA, result)),
    ?assertEqual(true, graph_state:get(FinalA, decided)),

    %% 测试路径 B（value <= 5）
    StateB = graph:state(#{value => 3}),
    ResultB = graph:run(Graph, StateB),
    ?assertEqual(completed, maps:get(status, ResultB)),
    FinalB = maps:get(final_state, ResultB),
    ?assertEqual(<<"path_b">>, graph_state:get(FinalB, result)),
    ?assertEqual(true, graph_state:get(FinalB, decided)).

%% 测试：Command update 正确合并到全局状态
command_update_merges_to_state_test() ->
    %% 节点使用 Command update 设置多个字段
    ProcessFun = fun(State, _) ->
        Count = graph_state:get(State, count, 0),
        {command, graph_command:new(#{
            update => #{count => Count + 1, last_node => process},
            goto => check
        })}
    end,
    CheckFun = fun(State, _) ->
        Count = graph_state:get(State, count, 0),
        case Count >= 3 of
            true ->
                {command, graph_command:goto('__end__', #{done => true})};
            false ->
                {command, graph_command:goto(process)}
        end
    end,

    {ok, Graph} = graph:build([
        {node, process, ProcessFun},
        {node, check, CheckFun},
        {entry, process}
    ]),

    InitialState = graph:state(#{count => 0}),
    Result = graph:run(Graph, InitialState),
    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(final_state, Result),
    ?assertEqual(3, graph_state:get(FinalState, count)),
    ?assertEqual(process, graph_state:get(FinalState, last_node)),
    ?assertEqual(true, graph_state:get(FinalState, done)).

%% 测试：Command 与普通 {ok, State} 节点混合使用
command_mixed_with_ok_nodes_test() ->
    %% start -> step1 (ok) -> step2 (command) -> step3 (ok) -> end
    Step1Fun = fun(State, _) ->
        {ok, graph_state:set(State, step1, done)}
    end,
    Step2Fun = fun(_State, _) ->
        {command, graph_command:new(#{
            update => #{step2 => done},
            goto => step3
        })}
    end,
    Step3Fun = fun(State, _) ->
        {ok, graph_state:set(State, step3, done)}
    end,

    {ok, Graph} = graph:build([
        {node, step1, Step1Fun},
        {node, step2, Step2Fun},
        {node, step3, Step3Fun},
        {edge, step1, step2},
        {edge, step3, '__end__'},
        {entry, step1}
    ]),

    InitialState = graph:state(#{}),
    Result = graph:run(Graph, InitialState),
    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(final_state, Result),
    ?assertEqual(done, graph_state:get(FinalState, step1)),
    ?assertEqual(done, graph_state:get(FinalState, step2)),
    ?assertEqual(done, graph_state:get(FinalState, step3)).

%% 测试：Command goto '__end__' 终止执行
command_goto_end_terminates_test() ->
    ProcessFun = fun(_State, _) ->
        {command, graph_command:goto('__end__', #{terminated => early})}
    end,
    %% 即使有边指向 next，Command 的 goto 应覆盖
    NextFun = fun(State, _) ->
        {ok, graph_state:set(State, should_not_reach, true)}
    end,

    {ok, Graph} = graph:build([
        {node, process, ProcessFun},
        {node, next, NextFun},
        {edge, process, next},
        {edge, next, '__end__'},
        {entry, process}
    ]),

    InitialState = graph:state(#{}),
    Result = graph:run(Graph, InitialState),
    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(final_state, Result),
    ?assertEqual(early, graph_state:get(FinalState, terminated)),
    %% next 节点不应该被执行
    ?assertEqual(undefined, graph_state:get(FinalState, should_not_reach)).

%% 测试：Command 无 goto 时回退到边路由
command_no_goto_uses_edge_routing_test() ->
    %% 节点使用 Command 但不指定 goto，依赖条件边路由
    ProcessFun = fun(State, _) ->
        Count = graph_state:get(State, count, 0),
        {command, graph_command:update(#{count => Count + 1})}
    end,
    RouterFn = fun(State) ->
        Count = graph_state:get(State, count, 0),
        case Count >= 2 of
            true -> '__end__';
            false -> process
        end
    end,

    {ok, Graph} = graph:build([
        {node, process, ProcessFun},
        {conditional_edge, process, RouterFn},
        {entry, process}
    ]),

    InitialState = graph:state(#{count => 0}),
    Result = graph:run(Graph, InitialState),
    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(final_state, Result),
    ?assertEqual(2, graph_state:get(FinalState, count)).

%% 测试：Command goto 多节点并行（Pregel 模式）
command_goto_parallel_nodes_test() ->
    %% 分发节点同时激活多个 worker
    DispatchFun = fun(_State, _) ->
        {command, graph_command:new(#{
            update => #{dispatched => true},
            goto => [worker_a, worker_b]
        })}
    end,
    WorkerAFun = fun(State, _) ->
        {ok, graph_state:set(State, worker_a_done, true)}
    end,
    WorkerBFun = fun(State, _) ->
        {ok, graph_state:set(State, worker_b_done, true)}
    end,

    {ok, Graph} = graph:build([
        {node, dispatch, DispatchFun},
        {node, worker_a, WorkerAFun},
        {node, worker_b, WorkerBFun},
        {edge, worker_a, '__end__'},
        {edge, worker_b, '__end__'},
        {entry, dispatch}
    ]),

    InitialState = graph:state(#{}),
    Result = graph:run(Graph, InitialState),
    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(final_state, Result),
    ?assertEqual(true, graph_state:get(FinalState, dispatched)),
    ?assertEqual(true, graph_state:get(FinalState, worker_a_done)),
    ?assertEqual(true, graph_state:get(FinalState, worker_b_done)).

%%====================================================================
%% 回归测试：现有 {ok, State} 代码不受影响
%%====================================================================

%% 测试：纯 {ok, State} 图执行不受 Command 改动影响
existing_ok_state_unaffected_test() ->
    ProcessFun = fun(State, _) ->
        Count = graph_state:get(State, count, 0),
        {ok, graph_state:set(State, count, Count + 1)}
    end,
    RouterFn = fun(State) ->
        case graph_state:get(State, count, 0) >= 3 of
            true -> '__end__';
            false -> process
        end
    end,

    {ok, Graph} = graph:build([
        {node, process, ProcessFun},
        {conditional_edge, process, RouterFn},
        {entry, process}
    ]),

    InitialState = graph:state(#{count => 0}),
    Result = graph:run(Graph, InitialState),
    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(final_state, Result),
    ?assertEqual(3, graph_state:get(FinalState, count)).
