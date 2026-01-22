%%%-------------------------------------------------------------------
%%% @doc graph_compute 模块的全局状态模式单元测试
%%%
%%% 测试 graph_compute 的能力：
%%% - compute_fn 的 try-catch 包装
%%% - 全局状态模式下返回 delta
%%% - 成功时返回 status => ok
%%% - 异常时返回 status => {error, Reason}
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_compute_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建简单的测试上下文（全局状态模式）
make_test_context(VertexId, GlobalState) ->
    #{
        vertex_id => VertexId,
        global_state => GlobalState,
        inbox => [],
        superstep => 0,
        num_vertices => 1,
        config => #{nodes => #{}}
    }.

%%====================================================================
%% compute_fn 错误处理测试
%%====================================================================

%% 测试：成功执行时返回 status => ok 和 delta
compute_fn_success_returns_ok_status_test() ->
    %% 准备：创建一个 __end__ 节点上下文
    GlobalState = #{test => value},
    Ctx = make_test_context('__end__', GlobalState),

    %% 执行
    ComputeFn = graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    %% 验证：status 为 ok
    ?assertEqual(ok, maps:get(status, Result)),
    %% 验证：返回了 delta
    ?assert(maps:is_key(delta, Result)),
    ?assert(is_map(maps:get(delta, Result))).

%% 测试：异常时返回 status => {error, Reason}
compute_fn_exception_returns_error_status_test() ->
    %% 准备：创建一个配置了不存在节点的上下文
    GlobalState = #{},
    Ctx = #{
        vertex_id => test_node,
        global_state => GlobalState,
        inbox => [activate],  %% 有激活消息以触发处理
        superstep => 0,
        num_vertices => 1,
        config => #{
            nodes => #{
                test_node => #{
                    node => #{
                        id => test_node,
                        %% 缺少 function 或 function 会抛异常
                        function => fun(_State) -> throw(intentional_error) end
                    },
                    edges => []
                }
            }
        }
    },

    %% 执行
    ComputeFn = graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    %% 验证：status 为 error
    ?assertMatch({error, _}, maps:get(status, Result)),
    %% 验证：返回结构包含必要字段
    ?assert(maps:is_key(delta, Result)),
    ?assert(maps:is_key(outbox, Result)),
    %% 验证：outbox 为空（失败时不发送消息）
    ?assertEqual([], maps:get(outbox, Result)),
    %% 验证：delta 为空（失败时不更新状态）
    ?assertEqual(#{}, maps:get(delta, Result)).

%% 测试：返回结构符合 compute_result 类型（全局状态模式）
compute_fn_result_structure_test() ->
    %% 准备
    GlobalState = #{initial => true},
    Ctx = make_test_context('__end__', GlobalState),

    %% 执行
    ComputeFn = graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    %% 验证：包含所有必要字段
    ?assert(maps:is_key(delta, Result)),
    ?assert(maps:is_key(outbox, Result)),
    ?assert(maps:is_key(status, Result)).

%%====================================================================
%% 全局状态模式特定测试
%%====================================================================

%% 测试：__start__ 节点在超步0被激活
start_node_activated_at_superstep_0_test() ->
    GlobalState = #{messages => [], system_prompt => <<"test">>},
    Ctx = #{
        vertex_id => '__start__',
        global_state => GlobalState,
        inbox => [],
        superstep => 0,
        num_vertices => 3,
        config => #{
            nodes => #{
                '__start__' => #{
                    edges => [graph_edge:direct('__start__', next_node)]
                }
            }
        }
    },

    ComputeFn = graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    %% 验证：status 为 ok
    ?assertEqual(ok, maps:get(status, Result)),
    %% 验证：发送了激活消息到下一节点
    Outbox = maps:get(outbox, Result),
    ?assert(length(Outbox) > 0).

%% 测试：__end__ 节点收到激活消息后正常完成
end_node_completes_on_activate_test() ->
    GlobalState = #{result => <<"final">>},
    Ctx = #{
        vertex_id => '__end__',
        global_state => GlobalState,
        inbox => [activate],
        superstep => 1,
        num_vertices => 3,
        config => #{
            nodes => #{
                '__end__' => #{}
            }
        }
    },

    ComputeFn = graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    %% 验证：status 为 ok
    ?assertEqual(ok, maps:get(status, Result)),
    %% 验证：不发送消息（终止节点）
    ?assertEqual([], maps:get(outbox, Result)),
    %% 验证：delta 为空（终止节点不更新状态）
    ?assertEqual(#{}, maps:get(delta, Result)).

%% 测试：普通节点无消息时保持 halt
regular_node_no_messages_halts_test() ->
    GlobalState = #{},
    Ctx = #{
        vertex_id => regular_node,
        global_state => GlobalState,
        inbox => [],  %% 无消息
        superstep => 1,
        num_vertices => 3,
        config => #{
            nodes => #{
                regular_node => #{}
            }
        }
    },

    ComputeFn = graph_compute:compute_fn(),
    Result = ComputeFn(Ctx),

    %% 验证：status 为 ok
    ?assertEqual(ok, maps:get(status, Result)),
    %% 验证：delta 为空
    ?assertEqual(#{}, maps:get(delta, Result)),
    %% 验证：不发送消息
    ?assertEqual([], maps:get(outbox, Result)).

%%====================================================================
%% from_pregel_result 测试
%%====================================================================

%% 测试：从完成的结果提取全局状态
from_pregel_result_completed_test() ->
    Result = #{
        status => completed,
        global_state => #{key => value, count => 42}
    },

    {ok, State} = graph_compute:from_pregel_result(Result),

    ?assertEqual(value, maps:get(key, State)),
    ?assertEqual(42, maps:get(count, State)).

%% 测试：从超过最大超步的结果提取状态和错误
from_pregel_result_max_supersteps_test() ->
    Result = #{
        status => max_supersteps,
        global_state => #{partial => result}
    },

    {error, {partial_result, State, max_iterations_exceeded}} =
        graph_compute:from_pregel_result(Result),

    ?assertEqual(result, maps:get(partial, State)).
