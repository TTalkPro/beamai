%%%-------------------------------------------------------------------
%%% @doc ToolCallingManager 隔离性测试
%%%
%%% 契约：**工具执行永远不能带崩调用者进程**，无论走哪个 manager、批次多大、
%%% 是串行还是并发。隔离是 manager 级的不变量（见
%%% {@link beamai_tool_batch_worker}），不是批次大小的副产品。
%%%
%%% 这里挑的崩法是 try/catch **挡不住**的那种——spawn_link 的子进程崩溃，
%%% 退出信号绕过 try/catch 直接打向宿主进程。只有进程边界能挡。用普通 raise
%%% 测不出隔离（try/catch 就吃掉了）。
%%%
%%% <b>防线是两层的，各测各的</b>：
%%% <ul>
%%%   <li><b>工具层</b>——{@link beamai_tool:invoke/3} 让 handler 跑在子进程里
%%%       （为了强制 timeout）。故 <b>handler 内</b>的崩溃故障单元是「单个工具」：
%%%       该工具 error，同批其它工具照常完成。</li>
%%%   <li><b>批层</b>——{@link beamai_tool_batch_worker} 的 worker。接住 handler
%%%       <b>之外</b>的崩溃（filter、批调度自身）。故障单元是「批」：整批合成
%%%       error。这是保护调用者的最后一道，也是 manager 级隔离契约本身。</li>
%%% </ul>
%%% 两层都要有测试，否则工具层一改，批层的测试就会悄悄变成空跑。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tcm_isolation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 隔离：四条路径都不能带崩调用者
%%====================================================================

%% 单工具（批内 1 个 → 内部退化串行）：曾经在调用者进程 inline 跑，会被打死
concurrent_single_tool_isolated_test() ->
    ?assertMatch({survived, _}, run_crash_batch(concurrent, 1, true)).

%% 多工具并发
concurrent_multi_tool_isolated_test() ->
    ?assertMatch({survived, _}, run_crash_batch(concurrent, 2, true)).

%% parallel=false（并发 manager 但关掉并发 → 内部串行）
concurrent_parallel_off_isolated_test() ->
    ?assertMatch({survived, _}, run_crash_batch(concurrent, 2, false)).

%% sequential manager：永远串行，同样必须隔离
sequential_isolated_test() ->
    ?assertMatch({survived, _}, run_crash_batch(sequential, 2, true)).

%%====================================================================
%% 工具层：handler 内的崩溃，故障单元是「单个工具」
%%====================================================================

%% 每个 tool_call 都有同构的 error 结果（LLM 协议要求每个 tool_call_id 都有
%% 对应 tool 结果，缺一个就是坏消息序列）
tool_crash_synthesizes_result_per_tool_call_test() ->
    {survived, #{messages := Msgs, records := Records}} =
        run_crash_batch(sequential, 3, false),
    ?assertEqual(3, length(Msgs)),
    ?assertEqual(3, length(Records)),
    %% tool_call_id 一一对应且保序
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>],
                 [maps:get(tool_call_id, M) || M <- Msgs]),
    ?assert(lists:all(fun(R) -> maps:is_key(error, R) end, Records)).

%% 崩溃工具不牵连同批的健康工具——故障单元真的是「单个工具」而非「批」
tool_crash_does_not_poison_batch_test() ->
    Boom = crash_tool(),
    Good = #{name => <<"good">>, description => <<"fine">>,
             handler => fun(_) -> {ok, <<"fine">>} end},
    K = beamai_kernel:add_tools(beamai_kernel:new(), [Boom, Good]),
    TCs = [#{id => <<"1">>, name => <<"boom">>, arguments => #{}},
           #{id => <<"2">>, name => <<"good">>, arguments => #{}}],
    #{records := Records} = beamai_tool_calling_manager:execute_tool_calls(
        beamai_tool_calling_manager:sequential(), K, TCs, #{}),
    [BoomRec, GoodRec] = Records,
    ?assert(maps:is_key(error, BoomRec)),
    %% 健康工具照常拿到真结果
    ?assertNot(maps:is_key(error, GoodRec)),
    ?assertEqual(<<"fine">>, maps:get(result, GoodRec)).

%%====================================================================
%% 批层：handler 之外的崩溃，故障单元是「批」，调用者恒存活
%%====================================================================

%% filter 跑在批 worker 里、在 handler 子进程之外——它崩了只有批级边界能接。
%% 这是 manager 级隔离契约本身（工具层的子进程对此无能为力）。
filter_crash_isolated_at_batch_level_test() ->
    {Outcome, #{messages := Msgs, records := Records}} = run_crashing_filter_batch(2),
    ?assertEqual(survived, Outcome),
    %% 整批合成 error，每个 tool_call 各一条
    ?assertEqual(2, length(Msgs)),
    ?assert(lists:all(fun(R) -> maps:is_key(error, R) end, Records)),
    %% 批崩溃是基础设施故障，须归 transient（而非缺省的 semantic）——
    %% 错误分类驱动屏障处的路由，归错类会让它被当成模型的语义错误
    ?assert(lists:all(fun(R) -> maps:get(class, maps:get(error, R)) =:= transient end,
                      Records)),
    %% 且这条错误确实来自批级边界，而非被工具层接住
    ?assert(lists:all(fun(M) ->
        binary:match(maps:get(content, M), <<"tool_batch_crash">>) =/= nomatch
    end, Msgs)).

%% 批崩溃 → context 退回批前快照（本批 writes 全丢，不半折叠）
batch_crash_rolls_back_context_test() ->
    Ctx0 = beamai_context:with_state(beamai_context:new(), #{<<"k">> => <<"before">>}),
    {survived, #{context := Ctx1}} = run_crashing_filter_batch(1, Ctx0),
    ?assertEqual(<<"before">>, beamai_context:state_get(Ctx1, <<"k">>, undefined)).

%%====================================================================
%% 卡死工具不能冻住调用者（串行路径唯一的时间防线）
%%====================================================================

%% 串行路径没有内层 gather 截止，批级截止是它唯一的防线：
%% 工具无限阻塞 → 到点 kill worker、整批合成 timeout error，调用者拿回控制权。
hanging_tool_does_not_freeze_caller_test_() ->
    {setup,
     fun() ->
         Old = {application:get_env(beamai_agent, tool_gather_timeout),
                application:get_env(beamai_agent, tool_batch_grace)},
         application:set_env(beamai_agent, tool_gather_timeout, 300),
         application:set_env(beamai_agent, tool_batch_grace, 200),
         Old
     end,
     fun({OldGather, OldGrace}) ->
         restore_env(tool_gather_timeout, OldGather),
         restore_env(tool_batch_grace, OldGrace)
     end,
     {timeout, 10, fun hanging_tool_times_out/0}}.

hanging_tool_times_out() ->
    Tool = #{
        name => <<"hang">>,
        description => <<"blocks forever">>,
        handler => fun(_Args) -> timer:sleep(infinity) end
    },
    K = beamai_kernel:add_tools(beamai_kernel:new(), [Tool]),
    TCs = [#{id => <<"1">>, name => <<"hang">>, arguments => #{}}],
    {Elapsed, #{messages := Msgs, records := Records}} = timer:tc(fun() ->
        beamai_tool_calling_manager:execute_tool_calls(
            beamai_tool_calling_manager:sequential(), K, TCs, #{})
    end),
    %% 到点返回，而非永远挂起
    ?assert(Elapsed div 1000 < 3000),
    %% 每个 tool_call 仍有同构的 error 结果
    ?assertEqual(1, length(Msgs)),
    ?assertMatch([#{error := #{}}], Records).

restore_env(Key, undefined) -> application:unset_env(beamai_agent, Key);
restore_env(Key, {ok, V}) -> application:set_env(beamai_agent, Key, V).

%%====================================================================
%% 正常路径不受影响
%%====================================================================

%% 隔离不能改变正常结果：worker 里算出的 writes 要跨进程带回来
writes_survive_isolation_test() ->
    Tool = #{
        name => <<"writer">>,
        description => <<"writes state">>,
        handler => fun(_Args) -> {ok, <<"ok">>, #{<<"k">> => <<"written">>}} end
    },
    K = beamai_kernel:add_tools(beamai_kernel:new(), [Tool]),
    TCs = [#{id => <<"1">>, name => <<"writer">>, arguments => #{}}],
    #{context := Ctx} = beamai_tool_calling_manager:execute_tool_calls(
        beamai_tool_calling_manager:concurrent(), K, TCs, #{}),
    ?assertEqual(<<"written">>, beamai_context:state_get(Ctx, <<"k">>, undefined)).

%% on_result 必须仍在**调用者进程**触发（worker 内经 proxy 转发回来）
on_result_fires_in_caller_process_test() ->
    Tool = #{
        name => <<"ok">>,
        description => <<"fine">>,
        handler => fun(_Args) -> {ok, <<"fine">>} end
    },
    K = beamai_kernel:add_tools(beamai_kernel:new(), [Tool]),
    TCs = [#{id => <<"1">>, name => <<"ok">>, arguments => #{}}],
    Caller = self(),
    OnResult = fun(_CR) -> Caller ! {fired_in, self()}, ok end,
    _ = beamai_tool_calling_manager:execute_tool_calls(
        beamai_tool_calling_manager:concurrent(), K, TCs, #{on_result => OnResult}),
    receive
        {fired_in, Pid} -> ?assertEqual(Caller, Pid)
    after 2000 -> ?assert(false)
    end.

%%====================================================================
%% manager 级执行策略（构造时给定）
%%====================================================================

%% manager 的 tool_timeout 是**缺省**：未声明 timeout 的工具吃它
manager_tool_timeout_applies_test() ->
    TCM = beamai_tool_calling_manager:sequential(#{tool_timeout => 100}),
    #{records := [Record]} = run_slow_tool(TCM, _Declared = undefined),
    ?assertEqual(transient, maps:get(class, maps:get(error, Record))),
    ?assertMatch(<<"tool_timeout">>, maps:get(message, maps:get(error, Record))).

%% 工具自己声明的 timeout **压过** manager 的缺省——工具最了解自己要跑多久
declared_tool_timeout_beats_manager_default_test() ->
    %% manager 说 100ms 就杀，但工具声明 infinity → 工具赢，正常完成
    TCM = beamai_tool_calling_manager:sequential(#{tool_timeout => 100}),
    #{records := [Record]} = run_slow_tool(TCM, _Declared = infinity),
    ?assertNot(maps:is_key(error, Record)),
    ?assertEqual(<<"slow-done">>, maps:get(result, Record)).

%% 不给 manager 策略 → 回落 beamai_tool 内置缺省（30s），慢工具照常完成
no_manager_timeout_falls_back_to_builtin_test() ->
    TCM = beamai_tool_calling_manager:sequential(),
    #{records := [Record]} = run_slow_tool(TCM, _Declared = undefined),
    ?assertNot(maps:is_key(error, Record)),
    ?assertEqual(<<"slow-done">>, maps:get(result, Record)).

%% manager 的 batch_timeout 压过 app env
manager_batch_timeout_applies_test_() ->
    {timeout, 10, fun() ->
        TCM = beamai_tool_calling_manager:sequential(#{batch_timeout => 300}),
        Tool = #{name => <<"hang">>, description => <<"blocks">>,
                 handler => fun(_) -> timer:sleep(infinity) end},
        K = beamai_kernel:add_tools(beamai_kernel:new(), [Tool]),
        TCs = [#{id => <<"1">>, name => <<"hang">>, arguments => #{}}],
        {Elapsed, #{records := [Record]}} = timer:tc(fun() ->
            beamai_tool_calling_manager:execute_tool_calls(TCM, K, TCs, #{})
        end),
        ?assert(Elapsed div 1000 < 3000),
        ?assert(maps:is_key(error, Record))
    end}.

%% 下发的执行策略不得渗进返回的 context——它是本批的执行细节，
%% 会跨轮穿线甚至被持久化进暂停载荷
manager_timeout_does_not_leak_into_context_test() ->
    TCM = beamai_tool_calling_manager:sequential(#{tool_timeout => 5000}),
    #{context := Ctx} = run_slow_tool(TCM, _Declared = undefined),
    ?assertEqual(undefined, beamai_context:default_tool_timeout(Ctx)).

%% 跑一个睡 300ms 的工具；Declared 为该工具声明的 timeout（undefined 即不声明）
run_slow_tool(TCM, Declared) ->
    Base = #{name => <<"slow">>, description => <<"sleeps a bit">>,
             handler => fun(_) -> timer:sleep(300), {ok, <<"slow-done">>} end},
    Tool = case Declared of
        undefined -> Base;
        T -> Base#{timeout => T}
    end,
    K = beamai_kernel:add_tools(beamai_kernel:new(), [Tool]),
    TCs = [#{id => <<"1">>, name => <<"slow">>, arguments => #{}}],
    beamai_tool_calling_manager:execute_tool_calls(TCM, K, TCs, #{}).

%%====================================================================
%% Helpers
%%====================================================================

%% 工具：handler 内 spawn_link 一个必崩子进程（退出信号绕过 try/catch）
crash_tool() ->
    #{
        name => <<"boom">>,
        description => <<"links to a dying process">>,
        handler => fun(_Args) ->
            spawn_link(fun() -> exit(boom) end),
            timer:sleep(50),
            {ok, <<"unreachable">>}
        end
    }.

run_crash_batch(Kind, N, Parallel) ->
    run_crash_batch(Kind, N, Parallel, beamai_context:new()).

%% 跑 N 个崩溃工具，返回 {survived, Result} | {died, Reason}
run_crash_batch(Kind, N, Parallel, Context) ->
    K = beamai_kernel:add_tools(beamai_kernel:new(), [crash_tool()]),
    TCs = [#{id => integer_to_binary(I), name => <<"boom">>, arguments => #{}}
           || I <- lists:seq(1, N)],
    run_batch(kind_to_tcm(Kind), K, TCs, Parallel, Context).

run_crashing_filter_batch(N) ->
    run_crashing_filter_batch(N, beamai_context:new()).

%% around_tool filter 里 spawn_link 一个必崩子进程：filter 跑在批 worker 中、
%% 在 handler 子进程之外，故只有批级边界能接住它
run_crashing_filter_batch(N, Context) ->
    Filter = beamai_filter:new(<<"crasher">>, #{
        around_tool => fun(Req, _FCtx, Next) ->
            spawn_link(fun() -> exit(filter_boom) end),
            timer:sleep(50),
            Next(Req)
        end
    }),
    Tool = #{name => <<"ok">>, description => <<"fine">>,
             handler => fun(_) -> {ok, <<"fine">>} end},
    K = beamai_kernel:add_tools(beamai_kernel:new(#{}, [Filter]), [Tool]),
    TCs = [#{id => integer_to_binary(I), name => <<"ok">>, arguments => #{}}
           || I <- lists:seq(1, N)],
    run_batch(beamai_tool_calling_manager:sequential(), K, TCs, false, Context).

kind_to_tcm(concurrent) -> beamai_tool_calling_manager:concurrent();
kind_to_tcm(sequential) -> beamai_tool_calling_manager:sequential().

%% 在独立进程里跑 manager，模拟「用户的 agent 进程」，观测它是否存活
run_batch(TCM, K, TCs, Parallel, Context) ->
    Parent = self(),
    {Pid, MRef} = spawn_monitor(fun() ->
        R = beamai_tool_calling_manager:execute_tool_calls(
                TCM, K, TCs, #{parallel => Parallel, context => Context}),
        Parent ! {self(), done, R}
    end),
    receive
        {Pid, done, R} ->
            erlang:demonitor(MRef, [flush]),
            {survived, R};
        {'DOWN', MRef, process, Pid, Reason} ->
            {died, Reason}
    after 10000 ->
        {hung, timeout}
    end.
