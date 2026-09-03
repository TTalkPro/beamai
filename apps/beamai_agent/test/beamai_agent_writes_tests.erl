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

%% 构造带 state_slots 的 ChatClient（工具为 {Name, Handler} 列表）
chat_client(Tools, Slots) ->
    K0 = beamai_chat_client:new(#{state_slots => Slots}),
    lists:foldl(fun({Name, Handler}, K) ->
        beamai_chat_client:add_tool(K,
            #{name => Name, parameters => #{}, handler => Handler})
    end, K0, Tools).

%% 构造 ChatClient，指定部分工具为 serial（Tools 为 {Name, Handler, Serial}）
chat_client_serial(Tools) ->
    lists:foldl(fun({Name, Handler, Serial}, K) ->
        beamai_chat_client:add_tool(K,
            #{name => Name, parameters => #{}, handler => Handler, serial => Serial})
    end, beamai_chat_client:new(), Tools).

%% 从 CallRecords 里取某工具的 result
result_of(Records, Name) ->
    case [R || #{name := N, result := R} <- Records, N =:= Name] of
        [R | _] -> R;
        [] -> undefined
    end.

conj() -> #{init => [], reduce => fun(Acc, V) -> [V | Acc] end}.

%%====================================================================
%% per-tool serial：批内含 serial → 整批退化串行
%%====================================================================

%% 每个工具向调用进程发 start/done 事件，用于判定是否重叠执行
ev_tool(Name) ->
    Parent = self(),
    fun(_, _) ->
        Parent ! {ev, Name, start},
        timer:sleep(40),
        Parent ! {ev, Name, done},
        {ok, Name}
    end.

drain_events(0, Acc) -> lists:reverse(Acc);
drain_events(N, Acc) ->
    receive {ev, Name, Kind} -> drain_events(N - 1, [{Name, Kind} | Acc])
    after 2000 -> lists:reverse(Acc)
    end.

serial_tool_degrades_batch_test() ->
    %% wb 标 serial → 即使 Parallel=true 且两工具，整批退化串行（无重叠）
    K = chat_client_serial([
        {<<"wa">>, ev_tool(<<"wa">>), false},
        {<<"wb">>, ev_tool(<<"wb">>), true}
    ]),
    {_, _, _} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"wa">>), tc(<<"2">>, <<"wb">>)], beamai_context:new(), true),
    Events = drain_events(4, []),
    %% 串行 ⇒ 每个 start 紧跟自己的 done（不交错）
    ?assertEqual(4, length(Events)),
    [{N1, start}, {N1b, done}, {N2, start}, {N2b, done}] = Events,
    ?assertEqual(N1, N1b),
    ?assertEqual(N2, N2b).

all_parallel_no_serial_overlaps_test() ->
    %% 均非 serial → 并发执行，两 start 先于任一 done（重叠）
    K = chat_client_serial([
        {<<"wa">>, ev_tool(<<"wa">>), false},
        {<<"wb">>, ev_tool(<<"wb">>), false}
    ]),
    {_, _, _} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"wa">>), tc(<<"2">>, <<"wb">>)], beamai_context:new(), true),
    Events = drain_events(4, []),
    [{_, K1}, {_, K2} | _] = Events,
    %% 前两个事件都是 start ⇒ 两工具重叠在跑
    ?assertEqual(start, K1),
    ?assertEqual(start, K2).

serial_tool_writes_still_fold_test() ->
    %% serial 退化串行后，writes 仍按屏障折叠（状态语义不变）
    K = beamai_chat_client:add_tool(
        beamai_chat_client:new(#{state_slots => #{<<"notes">> => conj()}}),
        #{name => <<"sw">>, parameters => #{}, serial => true,
          handler => fun(_, _) -> {ok, <<"a">>, #{<<"notes">> => a}} end}),
    K2 = beamai_chat_client:add_tool(K,
        #{name => <<"sw2">>, parameters => #{},
          handler => fun(_, _) -> {ok, <<"b">>, #{<<"notes">> => b}} end}),
    {_, _, Ctx} = beamai_agent_utils:execute_tools(
        K2, [tc(<<"1">>, <<"sw">>), tc(<<"2">>, <<"sw2">>)], beamai_context:new(), true),
    ?assertEqual([b, a], beamai_context:state_get(Ctx, <<"notes">>)).

%%====================================================================
%% 折叠：并行 / 串行
%%====================================================================

parallel_fold_conj_test() ->
    K = chat_client([
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
    {_, _, CtxP} = beamai_agent_utils:execute_tools(chat_client(Tools, #{<<"notes">> => conj()}), TCs, beamai_context:new(), true),
    {_, _, CtxS} = beamai_agent_utils:execute_tools(chat_client(Tools, #{<<"notes">> => conj()}), TCs, beamai_context:new(), false),
    ?assertEqual(beamai_context:state_get(CtxP, <<"notes">>),
                 beamai_context:state_get(CtxS, <<"notes">>)).

%%====================================================================
%% 快照隔离：同批 B 看不到 A 的写
%%====================================================================

snapshot_isolation_test() ->
    K = chat_client([
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
    K = chat_client([
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
    K = chat_client([
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
    K = chat_client([
        {<<"f1">>, fun(_Args) -> {ok, <<"v">>, #{<<"notes">> => z}} end}
    ], #{<<"notes">> => conj()}),
    {_, _, Ctx} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"f1">>)], beamai_context:new(), false),
    ?assertEqual([z], beamai_context:state_get(Ctx, <<"notes">>)).

%%====================================================================
%% writes 进历史（只存不发）
%%====================================================================

writes_in_stored_msg_stripped_from_wire_test() ->
    %% 写状态的工具 → 结果消息带 writes 元数据（进存储）
    K = chat_client([
        {<<"w">>, fun(_, _) -> {ok, <<"r">>, #{<<"note">> => <<"kept">>}} end}
    ], #{}),
    {Msgs, _, _} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"w">>)], beamai_context:new(), false),
    [ToolMsg] = Msgs,
    ?assertEqual(#{<<"note">> => <<"kept">>}, maps:get(writes, ToolMsg)),
    %% wire 层（message_adapter）白名单构建 → writes 天然剥落
    [Wire] = beamai_llm_message_adapter:to_provider([ToolMsg], openai),
    ?assertNot(maps:is_key(writes, Wire)),
    ?assertNot(maps:is_key(<<"writes">>, Wire)),
    ?assertEqual(<<"tool">>, maps:get(<<"role">>, Wire)).

failed_tool_has_no_writes_in_msg_test() ->
    %% 失败工具无 writes → 结果消息不含 writes 键（历史自然缺席）
    K = chat_client([
        {<<"boom">>, fun(_, _) -> {error, bad} end}
    ], #{}),
    {Msgs, _, _} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"boom">>)], beamai_context:new(), false),
    [ToolMsg] = Msgs,
    ?assertNot(maps:is_key(writes, ToolMsg)).

%%====================================================================
%% on_tool_result 实时触发
%%====================================================================

on_tool_result_fires_per_tool_test() ->
    %% 每个工具完成即触发 on_tool_result（本例 3 工具 → 3 次回调）
    Parent = self(),
    K = chat_client([
        {<<"t1">>, fun(_, _) -> {ok, <<"r1">>} end},
        {<<"t2">>, fun(_, _) -> {ok, <<"r2">>} end},
        {<<"t3">>, fun(_, _) -> {ok, <<"r3">>} end}
    ], #{}),
    OnResult = fun(#{name := N, result := R}) -> Parent ! {fired, N, R}, ok end,
    {_, Records, _} = beamai_agent_utils:execute_tools(
        K, [tc(<<"1">>, <<"t1">>), tc(<<"2">>, <<"t2">>), tc(<<"3">>, <<"t3">>)],
        beamai_context:new(), true, OnResult),
    Fired = collect_fired(3, []),
    ?assertEqual(3, length(Fired)),
    %% 每个工具都触发了一次（顺序不保证，用集合比对）
    ?assertEqual(lists:sort([<<"t1">>, <<"t2">>, <<"t3">>]),
                 lists:sort([N || {N, _} <- Fired])),
    %% CallRecords 按原始 tool_call 序（确定）
    ?assertEqual([<<"t1">>, <<"t2">>, <<"t3">>],
                 [maps:get(name, R) || R <- Records]).

collect_fired(0, Acc) -> Acc;
collect_fired(N, Acc) ->
    receive {fired, Name, Res} -> collect_fired(N - 1, [{Name, Res} | Acc])
    after 2000 -> Acc
    end.

%%====================================================================
%% 跨轮可见：本轮工具写、下轮工具读（完整 agent loop）
%%====================================================================

cross_turn_state_visible_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CC = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [tc(<<"c1">>, <<"writer">>)]}};
            2 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [tc(<<"c2">>, <<"reader">>)]}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    K = beamai_chat_client:add_chat_model(
        chat_client([
            {<<"writer">>, fun(_, _) -> {ok, <<"w">>, #{<<"note">> => <<"hello">>}} end},
            {<<"reader">>, fun(_, Ctx) ->
                {ok, <<"SAW:", (beamai_context:state_get(Ctx, <<"note">>, <<"none">>))/binary>>}
            end}
        ], #{}),
        beamai_chat_model:create(mock, #{})),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K}),
        {ok, Result, _} = beamai_agent:run(Agent, <<"go">>),
        Records = maps:get(tool_calls_made, Result, []),
        %% reader 在下一轮读到上一轮 writer 折叠进 state 的 note
        ?assertEqual(<<"SAW:hello">>, result_of(Records, <<"reader">>))
    after
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% 中断-resume：中断前累积的 state 恢复后可读
%%====================================================================

interrupt_resume_restores_state_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CC = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
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
    K = beamai_chat_client:add_chat_model(
        chat_client([
            {<<"writer">>, fun(_, _) -> {ok, <<"w">>, #{<<"note">> => <<"kept">>}} end},
            {<<"reader">>, fun(_, Ctx) ->
                {ok, <<"SAW:", (beamai_context:state_get(Ctx, <<"note">>, <<"none">>))/binary>>}
            end}
        ], #{}),
        beamai_chat_model:create(mock, #{})),
    try
        {ok, Agent} = beamai_agent:new(#{
            chat_client => K,
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
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% on_state_change 回调 + initial_state 种子
%%====================================================================

%% @private mock 一轮工具调用后收尾的 LLM
mock_one_tool_round(ToolName) ->
    CC = counters:new(1, []),
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, tool_calls => [tc(<<"c1">>, ToolName)],
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    ok.

recv_state() ->
    receive {state, S} -> S after 300 -> timeout end.

%% 工具写了 state → 屏障折叠后触发 on_state_change，参数是**整份** state
on_state_change_fires_on_write_test() ->
    ok = mock_one_tool_round(<<"w">>),
    Self = self(),
    K0 = chat_client([{<<"w">>, fun(_, _) -> {ok, <<"r">>, #{<<"note">> => <<"hi">>}} end}],
                     #{}),
    K = beamai_chat_client:add_chat_model(K0, beamai_chat_model:create(mock, #{})),
    try
        {ok, Agent} = beamai_agent:new(#{
            chat_client => K, memory => false,
            callbacks => #{on_state_change => fun(S, _Meta) -> Self ! {state, S} end}}),
        {ok, _, _} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(#{<<"note">> => <<"hi">>}, recv_state())
    after
        meck:unload(beamai_chat_model)
    end.

%% 工具没写 state → 不触发（绝大多数工具不写，每批都发就是噪音）
on_state_change_silent_without_write_test() ->
    ok = mock_one_tool_round(<<"plain">>),
    Self = self(),
    K0 = chat_client([{<<"plain">>, fun(_, _) -> {ok, <<"r">>} end}], #{}),
    K = beamai_chat_client:add_chat_model(K0, beamai_chat_model:create(mock, #{})),
    try
        {ok, Agent} = beamai_agent:new(#{
            chat_client => K, memory => false,
            callbacks => #{on_state_change => fun(S, _Meta) -> Self ! {state, S} end}}),
        {ok, _, _} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(timeout, recv_state())
    after
        meck:unload(beamai_chat_model)
    end.

%% initial_state 种进每个 turn 的 context：工具读得到，写完的整份 state 含两者
initial_state_seeds_turn_test() ->
    ok = mock_one_tool_round(<<"bump">>),
    Self = self(),
    Bump = fun(_Args, Ctx) ->
        N = beamai_context:state_get(Ctx, <<"counter">>, 0),
        {ok, <<"ok">>, #{<<"counter">> => N + 1}}
    end,
    K0 = chat_client([{<<"bump">>, Bump}], #{}),
    K = beamai_chat_client:add_chat_model(K0, beamai_chat_model:create(mock, #{})),
    try
        {ok, Agent} = beamai_agent:new(#{
            chat_client => K, memory => false,
            initial_state => #{<<"counter">> => 41, <<"keep">> => <<"me">>},
            callbacks => #{on_state_change => fun(S, _Meta) -> Self ! {state, S} end}}),
        {ok, _, _} = beamai_agent:run(Agent, <<"go">>),
        %% 工具在种子之上增量：41 → 42；没被碰的键原样留着
        ?assertEqual(#{<<"counter">> => 42, <<"keep">> => <<"me">>}, recv_state())
    after
        meck:unload(beamai_chat_model)
    end.

%% 不给 initial_state 时行为不变（从空 state 起步）
no_initial_state_starts_empty_test() ->
    ok = mock_one_tool_round(<<"peek">>),
    Self = self(),
    Peek = fun(_Args, Ctx) ->
        {ok, <<"ok">>, #{<<"seen">> => beamai_context:get_state(Ctx)}}
    end,
    K0 = chat_client([{<<"peek">>, Peek}], #{}),
    K = beamai_chat_client:add_chat_model(K0, beamai_chat_model:create(mock, #{})),
    try
        {ok, Agent} = beamai_agent:new(#{
            chat_client => K, memory => false,
            callbacks => #{on_state_change => fun(S, _Meta) -> Self ! {state, S} end}}),
        {ok, _, _} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(#{<<"seen">> => #{}}, recv_state())
    after
        meck:unload(beamai_chat_model)
    end.
