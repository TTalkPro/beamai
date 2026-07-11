%%%-------------------------------------------------------------------
%%% @doc beamai_tool 瞬态重试测试（仅 transient 类重试 + 指数退避）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_retry_tests).

-include_lib("eunit/include/eunit.hrl").

%% 计数器工具：第 N 次调用前自增；handler 由传入 fun 决定返回
counting_tool(Ctr, Handler) ->
    #{name => <<"ct">>, parameters => #{},
      retry => #{max_retries => 3, initial_delay_ms => 1},
      handler => fun(_Args) ->
          N = counters:get(Ctr, 1) + 1,
          counters:add(Ctr, 1, 1),
          Handler(N)
      end}.

%% transient 错误重试直到成功
transient_retry_succeeds_test() ->
    Ctr = counters:new(1, []),
    Tool = counting_tool(Ctr, fun(N) ->
        case N < 3 of
            true -> {error, timeout};          %% transient → 重试
            false -> {ok, <<"done">>}
        end
    end),
    ?assertEqual({ok, <<"done">>}, beamai_tool:invoke(Tool, #{})),
    ?assertEqual(3, counters:get(Ctr, 1)).      %% 前两次失败重试，第三次成功

%% semantic 错误不重试（一次即返回）
semantic_not_retried_test() ->
    Ctr = counters:new(1, []),
    Tool = counting_tool(Ctr, fun(_N) -> {error, bad_argument} end),  %% semantic
    ?assertEqual({error, bad_argument}, beamai_tool:invoke(Tool, #{})),
    ?assertEqual(1, counters:get(Ctr, 1)).

%% transient 重试耗尽后返回最后一次错误
transient_exhausted_test() ->
    Ctr = counters:new(1, []),
    Tool0 = counting_tool(Ctr, fun(_N) -> {error, timeout} end),
    Tool = Tool0#{retry => #{max_retries => 2, initial_delay_ms => 1}},
    ?assertEqual({error, timeout}, beamai_tool:invoke(Tool, #{})),
    ?assertEqual(3, counters:get(Ctr, 1)).      %% 1 初次 + 2 重试

%% 未开 retry 则不重试（即便 transient）
no_retry_by_default_test() ->
    Ctr = counters:new(1, []),
    Tool = #{name => <<"nr">>, parameters => #{},
             handler => fun(_Args) ->
                 counters:add(Ctr, 1, 1),
                 {error, timeout}
             end},
    ?assertEqual({error, timeout}, beamai_tool:invoke(Tool, #{})),
    ?assertEqual(1, counters:get(Ctr, 1)).

%% retry => true 用缺省 #{max_retries=>2}
retry_true_uses_defaults_test() ->
    Ctr = counters:new(1, []),
    Tool = #{name => <<"rt">>, parameters => #{}, retry => true,
             handler => fun(_Args) ->
                 counters:add(Ctr, 1, 1),
                 {error, timeout}
             end},
    ?assertEqual({error, timeout}, beamai_tool:invoke(Tool, #{})),
    ?assertEqual(3, counters:get(Ctr, 1)).      %% 1 + 2 缺省重试
