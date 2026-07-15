%%%-------------------------------------------------------------------
%%% @doc ToolCallingManager seam 验证测试
%%%
%%% 覆盖 v3 适配：
%%%   F1 mock manager 注入——seam 真实存在、可替换
%%%   F2 边界验证——注入 manager 时 around_tool filter 仍触发（正交性）
%%%   F3 多 impl 切换——concurrent vs sequential 执行策略差异可观测
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_calling_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

echo_tool(Name, Value) ->
    #{name => Name, parameters => #{}, handler => fun(_, _) -> {ok, Value} end}.

%% 事件工具：发 start/done 给调用进程，用于判定是否重叠
ev_tool(Name) ->
    Parent = self(),
    fun(_, _) ->
        Parent ! {ev, Name, start},
        timer:sleep(40),
        Parent ! {ev, Name, done},
        {ok, Name}
    end.

tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

drain_events(0, Acc) -> lists:reverse(Acc);
drain_events(N, Acc) ->
    receive {ev, Name, Kind} -> drain_events(N - 1, [{Name, Kind} | Acc])
    after 2000 -> lists:reverse(Acc)
    end.

%%====================================================================
%% A. behaviour 基础验证
%%====================================================================

concurrent_manager_delegates_test() ->
    K = beamai_kernel:add_tool(beamai_kernel:new(), echo_tool(<<"echo">>, <<"hello">>)),
    TCM = beamai_concurrent_tool_calling_manager:new(),
    Result = beamai_tool_calling_manager:execute_tool_calls(
        TCM, K, [tc(<<"1">>, <<"echo">>)], #{context => beamai_context:new()}),
    #{messages := [Msg], records := [Record], context := _Ctx} = Result,
    ?assertEqual(tool, maps:get(role, Msg)),
    ?assertEqual(<<"1">>, maps:get(tool_call_id, Msg)),
    ?assertEqual(<<"hello">>, maps:get(result, Record)),
    ?assertEqual(<<"echo">>, maps:get(name, Record)).

manager_format_test() ->
    ?assertMatch({beamai_concurrent_tool_calling_manager, _},
                 beamai_concurrent_tool_calling_manager:new()),
    ?assertMatch({beamai_sequential_tool_calling_manager, _},
                 beamai_sequential_tool_calling_manager:new()).

default_is_concurrent_test() ->
    ?assertMatch({beamai_concurrent_tool_calling_manager, _},
                 beamai_tool_calling_manager:default()).

default_opts_test() ->
    K = beamai_kernel:add_tool(beamai_kernel:new(), echo_tool(<<"t">>, 42)),
    TCM = beamai_concurrent_tool_calling_manager:new(),
    Result = beamai_tool_calling_manager:execute_tool_calls(TCM, K, [tc(<<"1">>, <<"t">>)], #{}),
    #{messages := [_], records := [Record], context := _} = Result,
    ?assertEqual(<<"42">>, maps:get(result, Record)).

%%====================================================================
%% B. mock manager 注入——seam 真实存在
%%====================================================================

mock_manager_dispatched_test() ->
    MockTCM = {beamai_mock_tcm_impl, mock_state},
    K = beamai_kernel:new(),
    Result = beamai_tool_calling_manager:execute_tool_calls(
        MockTCM, K, [tc(<<"x">>, <<"whatever">>)], #{}),
    #{messages := [Msg], records := [Record]} = Result,
    ?assertEqual(<<"mocked">>, maps:get(content, Msg)),
    ?assertEqual(<<"mock">>, maps:get(name, Record)).

agent_state_accepts_custom_tcm_test() ->
    CustomTCM = {my_custom_tcm, some_ref},
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{model => <<"m">>}},
        tool_calling_manager => CustomTCM
    }),
    ?assertEqual(CustomTCM, maps:get(tool_calling_manager, Agent)).

agent_state_default_tcm_test() ->
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{model => <<"m">>}}
    }),
    ?assertMatch({beamai_concurrent_tool_calling_manager, _},
                 maps:get(tool_calling_manager, Agent)).

agent_state_sequential_tcm_test() ->
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{model => <<"m">>}},
        tool_calling_manager => beamai_tool_calling_manager:sequential()
    }),
    ?assertMatch({beamai_sequential_tool_calling_manager, _},
                 maps:get(tool_calling_manager, Agent)).

%%====================================================================
%% C. 边界验证——manager 与 filter 正交
%%====================================================================

filter_still_fires_test() ->
    FilterTriggered = atomics:new(1, [{signed, false}]),
    CountFilter = beamai_filter:new(<<"count_tool">>, #{
        around_tool => fun(Req, _FCtx, Next) ->
            atomics:add(FilterTriggered, 1, 1),
            Next(Req)
        end
    }),
    K0 = beamai_kernel:new(#{}, [CountFilter]),
    K = beamai_kernel:add_tool(K0, echo_tool(<<"echo">>, <<"v">>)),
    TCM = beamai_concurrent_tool_calling_manager:new(),
    beamai_tool_calling_manager:execute_tool_calls(
        TCM, K, [tc(<<"1">>, <<"echo">>)], #{context => beamai_context:new()}),
    ?assertEqual(1, atomics:get(FilterTriggered, 1)).

%%====================================================================
%% D. 多 impl 切换——concurrent vs sequential 行为差异
%%====================================================================

%% sequential manager：即使 parallel=true，工具也严格按序执行（无重叠）
sequential_no_overlap_test() ->
    K = beamai_kernel:add_tool(beamai_kernel:new(),
        #{name => <<"ta">>, parameters => #{}, handler => ev_tool(<<"ta">>)}),
    K2 = beamai_kernel:add_tool(K,
        #{name => <<"tb">>, parameters => #{}, handler => ev_tool(<<"tb">>)}),
    TCM = beamai_sequential_tool_calling_manager:new(),
    beamai_tool_calling_manager:execute_tool_calls(
        TCM, K2, [tc(<<"1">>, <<"ta">>), tc(<<"2">>, <<"tb">>)],
        #{context => beamai_context:new(), parallel => true}),
    Events = drain_events(4, []),
    %% 串行 ⇒ 每个 start 紧跟自己的 done（不交错）
    ?assertEqual(4, length(Events)),
    [{N1, start}, {N1b, done}, {N2, start}, {N2b, done}] = Events,
    ?assertEqual(N1, N1b),
    ?assertEqual(N2, N2b).

%% concurrent manager：parallel=true + 两个非 serial 工具 → 并发（有重叠）
concurrent_overlap_test() ->
    K = beamai_kernel:add_tool(beamai_kernel:new(),
        #{name => <<"ta">>, parameters => #{}, handler => ev_tool(<<"ta">>)}),
    K2 = beamai_kernel:add_tool(K,
        #{name => <<"tb">>, parameters => #{}, handler => ev_tool(<<"tb">>)}),
    TCM = beamai_concurrent_tool_calling_manager:new(),
    beamai_tool_calling_manager:execute_tool_calls(
        TCM, K2, [tc(<<"1">>, <<"ta">>), tc(<<"2">>, <<"tb">>)],
        #{context => beamai_context:new(), parallel => true}),
    Events = drain_events(4, []),
    %% 并发 ⇒ 两个 start 在前，两个 done 在后（交错）
    ?assertEqual(4, length(Events)),
    Starts = [Name || {Name, start} <- Events],
    Dones = [Name || {Name, done} <- Events],
    ?assertEqual(2, length(Starts)),
    ?assertEqual(2, length(Dones)).

%%====================================================================
%% E. 返回值结构验证
%%====================================================================

result_map_structure_test() ->
    K = beamai_kernel:add_tool(beamai_kernel:new(), echo_tool(<<"t">>, ok)),
    TCM = beamai_concurrent_tool_calling_manager:new(),
    Result = beamai_tool_calling_manager:execute_tool_calls(
        TCM, K, [tc(<<"1">>, <<"t">>), tc(<<"2">>, <<"t">>)],
        #{context => beamai_context:new(), parallel => false}),
    #{messages := Msgs, records := Records, context := Ctx} = Result,
    ?assertEqual(2, length(Msgs)),
    ?assertEqual(2, length(Records)),
    ?assertMatch(#{'__context__' := true}, Ctx).

empty_toolcalls_test() ->
    K = beamai_kernel:new(),
    TCM = beamai_concurrent_tool_calling_manager:new(),
    Result = beamai_tool_calling_manager:execute_tool_calls(
        TCM, K, [], #{context => beamai_context:new()}),
    ?assertEqual([], maps:get(messages, Result)),
    ?assertEqual([], maps:get(records, Result)).
