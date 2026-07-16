%%%-------------------------------------------------------------------
%%% @doc tool_spec 的 timeout 字段：声明了才强制执行
%%%
%%% 此字段曾长期是**空转的**——`invoke/3' 读了它、传给 `call_handler/4'，而后者
%%% 四个子句全部忽略该参数。声明 100ms 的工具能跑满 2 秒。这些测试钉住修复后
%%% 的语义。
%%%
%%% 策略是**声明式**的：不声明即不限时（框架不替调用者判断「多久算太久」），
%%% 声明了才到点 kill。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_timeout_tests).

-include_lib("eunit/include/eunit.hrl").

%% {M,F} handler 的被调函数
-export([slow_handler/2]).

%%====================================================================
%% timeout 强制执行
%%====================================================================

%% 声明 timeout=100 的工具睡 2000 → 到点返回，而非跑满
declared_timeout_enforced_test() ->
    Tool = #{name => <<"slow">>, timeout => 100,
             handler => fun(_) -> timer:sleep(2000), {ok, <<"never">>} end},
    {Elapsed, Result} = timer:tc(fun() -> beamai_tool:invoke(Tool, #{}) end),
    ?assert(Elapsed div 1000 < 1000),
    ?assertMatch({error, #{class := timeout, reason := tool_timeout,
                           timeout_ms := 100}}, Result).

%% 不声明 timeout → 不限时，正常工具照常完成
undeclared_timeout_allows_normal_tool_test() ->
    Tool = #{name => <<"fast">>, handler => fun(_) -> {ok, <<"done">>} end},
    ?assertEqual({ok, <<"done">>}, beamai_tool:invoke(Tool, #{})).

%% 显式声明 infinity（等同不声明）
infinity_timeout_not_enforced_test() ->
    Tool = #{name => <<"long">>, timeout => infinity,
             handler => fun(_) -> timer:sleep(50), {ok, <<"done">>} end},
    ?assertEqual({ok, <<"done">>}, beamai_tool:invoke(Tool, #{})).

%% 超时归为 transient——这决定了它可被 retry
timeout_classified_transient_test() ->
    Tool = #{name => <<"slow">>, timeout => 50,
             handler => fun(_) -> timer:sleep(1000), {ok, <<"never">>} end},
    {error, Reason} = beamai_tool:invoke(Tool, #{}),
    ?assertEqual(transient, beamai_tool_error:classify(Reason)).

%% 声明 retry 的工具，超时后会重试（transient）——总耗时覆盖多次尝试
timeout_triggers_retry_test_() ->
    {timeout, 10, fun() ->
        Counter = counters:new(1, []),
        Tool = #{name => <<"flaky">>, timeout => 100,
                 retry => #{max_retries => 2, initial_delay_ms => 10},
                 handler => fun(_) ->
                     counters:add(Counter, 1, 1),
                     timer:sleep(1000),
                     {ok, <<"never">>}
                 end},
        {error, _} = beamai_tool:invoke(Tool, #{}),
        %% 初次 + 2 次重试 = 3 次调用
        ?assertEqual(3, counters:get(Counter, 1))
    end}.

%%====================================================================
%% 既有错误语义不变
%%====================================================================

%% handler 自身 raise → 仍归一为 #{class, reason, stacktrace}（形状不变）
handler_raise_shape_unchanged_test() ->
    Tool = #{name => <<"bad">>, handler => fun(_) -> error(badthing) end},
    ?assertMatch({error, #{class := error, reason := badthing, stacktrace := [_ | _]}},
                 beamai_tool:invoke(Tool, #{})).

%% handler 返回的 throw 同样被捕获
handler_throw_caught_test() ->
    Tool = #{name => <<"bad">>, handler => fun(_) -> throw(nope) end},
    ?assertMatch({error, #{class := throw, reason := nope}},
                 beamai_tool:invoke(Tool, #{})).

%% 逃出 try/catch 的退出信号（工具内 spawn_link 崩溃）→ 归一为该工具的 error，
%% 且**不打死调用者**（故障单元收窄到单个工具）
handler_link_crash_isolated_test() ->
    Tool = #{name => <<"boom">>,
             handler => fun(_) ->
                 spawn_link(fun() -> exit(boom) end),
                 timer:sleep(50),
                 {ok, <<"unreachable">>}
             end},
    Result = beamai_tool:invoke(Tool, #{}),
    ?assertMatch({error, #{class := exit}}, Result),
    %% 调用者仍活着（能继续跑）
    ?assertEqual({ok, <<"alive">>},
                 beamai_tool:invoke(#{name => <<"ok">>,
                                      handler => fun(_) -> {ok, <<"alive">>} end}, #{})).

%% fun/2、{M,F} 形态同样受 timeout 约束（不是只有 fun/1 被改）
fun2_handler_timeout_enforced_test() ->
    Tool = #{name => <<"slow2">>, timeout => 100,
             handler => fun(_Args, _Ctx) -> timer:sleep(2000), {ok, <<"never">>} end},
    ?assertMatch({error, #{reason := tool_timeout}}, beamai_tool:invoke(Tool, #{})).

mf_handler_timeout_enforced_test() ->
    Tool = #{name => <<"slow3">>, timeout => 100,
             handler => {?MODULE, slow_handler}},
    ?assertMatch({error, #{reason := tool_timeout}}, beamai_tool:invoke(Tool, #{})).

slow_handler(_Args, _Ctx) -> timer:sleep(2000), {ok, <<"never">>}.

%%====================================================================
%% Writes 语义不受影响
%%====================================================================

%% handler 跑在子进程里，{ok, V, Writes} 三元返回要完整带回来
writes_survive_handler_process_test() ->
    Tool = #{name => <<"w">>,
             handler => fun(_) -> {ok, <<"v">>, #{<<"k">> => <<"written">>}} end},
    ?assertEqual({ok, <<"v">>, #{<<"k">> => <<"written">>}},
                 beamai_tool:invoke(Tool, #{})).
