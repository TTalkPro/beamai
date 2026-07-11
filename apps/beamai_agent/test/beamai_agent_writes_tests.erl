%%%-------------------------------------------------------------------
%%% @doc 工具 writes 折叠 + 跨轮/中断-resume state 恢复测试
%%%
%%% 覆盖 design/context_split_parallel_tools.md §7 清单：并行/串行折叠、
%%% 快照隔离、last-writer 按 index 序、错误 writes 归零、fun/1 writes、
%%% 跨轮可见、中断-resume state 恢复。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_writes_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

%% 构造一个 OpenAI 嵌套格式 tool_call
tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

%% 构造带 state_slots 的 kernel（工具为 {Name, Handler} 列表）
kernel(Tools, Slots) ->
    K0 = beamai_kernel:new(#{state_slots => Slots}),
    lists:foldl(fun({Name, Handler}, K) ->
        beamai_kernel:add_tool(K,
            #{name => Name, parameters => #{}, handler => Handler})
    end, K0, Tools).

%% 从 CallRecords 里取某工具的 result
result_of(Records, Name) ->
    case [R || #{name := N, result := R} <- Records, N =:= Name] of
        [R | _] -> R;
        [] -> undefined
    end.

conj() -> #{init => [], reduce => fun(Acc, V) -> [V | Acc] end}.

%%====================================================================
%% 折叠：并行 / 串行
%%====================================================================

parallel_fold_conj_test() ->
    K = kernel([
        {<<"wa">>, fun(_, _) -> {ok, <<"a">>, #{<<"notes">> => a}} end},
        {<<"wb">>, fun(_, _) -> {ok, <<"b">>, #{<<"notes">> => b}} end}
    ], #{<<"notes">> => conj()}),
    {Msgs, _Recs, Ctx} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"wa">>), tc(<<"2">>, <<"wb">>)], beamai_context:new(), true),
    ?assertEqual(2, length(Msgs)),
    ?assertEqual([b, a], beamai_context:state_get(Ctx, <<"notes">>)).

serial_matches_parallel_semantics_test() ->
    %% 串行路径与并行路径状态语义一致（快照 + 屏障折叠）
    Tools = [
        {<<"wa">>, fun(_, _) -> {ok, <<"a">>, #{<<"notes">> => a}} end},
        {<<"wb">>, fun(_, _) -> {ok, <<"b">>, #{<<"notes">> => b}} end}
    ],
    TCs = [tc(<<"1">>, <<"wa">>), tc(<<"2">>, <<"wb">>)],
    {_, _, CtxP} = beamai_agent_utils:execute_tools(kernel(Tools, #{<<"notes">> => conj()}), TCs, beamai_context:new(), true),
    {_, _, CtxS} = beamai_agent_utils:execute_tools(kernel(Tools, #{<<"notes">> => conj()}), TCs, beamai_context:new(), false),
    ?assertEqual(beamai_context:state_get(CtxP, <<"notes">>),
                 beamai_context:state_get(CtxS, <<"notes">>)).

%%====================================================================
%% 快照隔离：同批 B 看不到 A 的写
%%====================================================================

snapshot_isolation_test() ->
    K = kernel([
        {<<"wa">>, fun(_, _) -> {ok, <<"a">>, #{<<"notes">> => <<"A">>}} end},
        {<<"rb">>, fun(_, Ctx) ->
            Saw = beamai_context:state_get(Ctx, <<"notes">>, none),
            {ok, iolist_to_binary(io_lib:format("~p", [Saw]))}
        end}
    ], #{}),
    {_, Recs, _} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"wa">>), tc(<<"2">>, <<"rb">>)], beamai_context:new(), true),
    %% rb 拿轮初快照（空 state），看不到同批 wa 的写
    ?assertEqual(<<"none">>, result_of(Recs, <<"rb">>)).

%%====================================================================
%% last-writer 按 index 序而非完成序
%%====================================================================

last_writer_by_index_not_completion_test() ->
    %% index1 慢（最后完成）、index2 快（最先完成）；未声明槽按 index 序
    %% 折叠 ⇒ index2 胜（若按完成序则 index1 胜）
    K = kernel([
        {<<"slow1">>, fun(_, _) -> timer:sleep(80), {ok, <<>>, #{<<"k">> => one}} end},
        {<<"fast2">>, fun(_, _) -> {ok, <<>>, #{<<"k">> => two}} end}
    ], #{}),
    {_, _, Ctx} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"slow1">>), tc(<<"2">>, <<"fast2">>)], beamai_context:new(), true),
    ?assertEqual(two, beamai_context:state_get(Ctx, <<"k">>)).

%%====================================================================
%% 错误 / crash 的 writes 归零（事务性）
%%====================================================================

error_writes_zeroed_test() ->
    K = kernel([
        {<<"ok">>, fun(_, _) -> {ok, <<>>, #{<<"notes">> => good}} end},
        {<<"boom">>, fun(_, _) -> error(boom) end}
    ], #{<<"notes">> => conj()}),
    {_, _, Ctx} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"ok">>), tc(<<"2">>, <<"boom">>)], beamai_context:new(), true),
    %% boom 崩溃 → 其 writes 不参与折叠，只剩 ok 的写
    ?assertEqual([good], beamai_context:state_get(Ctx, <<"notes">>)).

%%====================================================================
%% fun/1 工具也可返回 writes（读写正交）
%%====================================================================

arity1_tool_writes_test() ->
    K = kernel([
        {<<"f1">>, fun(_Args) -> {ok, <<"v">>, #{<<"notes">> => z}} end}
    ], #{<<"notes">> => conj()}),
    {_, _, Ctx} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"f1">>)], beamai_context:new(), false),
    ?assertEqual([z], beamai_context:state_get(Ctx, <<"notes">>)).

%%====================================================================
%% 跨轮可见：本轮工具写、下轮工具读（完整 agent loop）
%%====================================================================

cross_turn_state_visible_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    CC = counters:new(1, []),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [tc(<<"c1">>, <<"writer">>)]}};
            2 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [tc(<<"c2">>, <<"reader">>)]}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    K = beamai_kernel:add_service(
        kernel([
            {<<"writer">>, fun(_, _) -> {ok, <<"w">>, #{<<"note">> => <<"hello">>}} end},
            {<<"reader">>, fun(_, Ctx) ->
                {ok, <<"SAW:", (beamai_context:state_get(Ctx, <<"note">>, <<"none">>))/binary>>}
            end}
        ], #{}),
        beamai_chat_completion:create(mock, #{})),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K}),
        {ok, Result, _} = beamai_agent:run(Agent, <<"go">>),
        Records = maps:get(tool_calls_made, Result, []),
        %% reader 在下一轮读到上一轮 writer 折叠进 state 的 note
        ?assertEqual(<<"SAW:hello">>, result_of(Records, <<"reader">>))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 中断-resume：中断前累积的 state 恢复后可读
%%====================================================================

interrupt_resume_restores_state_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    CC = counters:new(1, []),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 ->
                %% 同批：writer（安全，先执行并写 state）+ ask_human（中断）
                {ok, #{content => null, finish_reason => <<"tool_calls">>,
                       tool_calls => [tc(<<"c1">>, <<"writer">>),
                                      tc(<<"c2">>, <<"ask_human">>)]}};
            2 ->
                %% resume 后：reader 读中断前写入的 state
                {ok, #{content => null, finish_reason => <<"tool_calls">>,
                       tool_calls => [tc(<<"c3">>, <<"reader">>)]}};
            _ ->
                {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    K = beamai_kernel:add_service(
        kernel([
            {<<"writer">>, fun(_, _) -> {ok, <<"w">>, #{<<"note">> => <<"kept">>}} end},
            {<<"reader">>, fun(_, Ctx) ->
                {ok, <<"SAW:", (beamai_context:state_get(Ctx, <<"note">>, <<"none">>))/binary>>}
            end}
        ], #{}),
        beamai_chat_completion:create(mock, #{})),
    try
        {ok, Agent} = beamai_agent:new(#{
            kernel => K,
            interrupt_tools => [#{name => <<"ask_human">>, description => <<"ask">>,
                                  parameters => #{type => object, properties => #{}}}]
        }),
        {interrupt, _Info, Agent1} = beamai_agent:run(Agent, <<"go">>),
        ?assert(beamai_agent:is_interrupted(Agent1)),
        {ok, Result, _} = beamai_agent:resume(Agent1, <<"approved">>),
        Records = maps:get(tool_calls_made, Result, []),
        %% 中断前 writer 的写跨越中断被恢复，reader 读得到
        ?assertEqual(<<"SAW:kept">>, result_of(Records, <<"reader">>))
    after
        meck:unload(beamai_chat_completion)
    end.
