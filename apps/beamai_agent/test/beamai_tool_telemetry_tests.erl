%%%-------------------------------------------------------------------
%%% @doc Tool lifecycle telemetry 埋点测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_telemetry_tests).

-include_lib("eunit/include/eunit.hrl").

telemetry_test_() ->
    {foreach, fun reset/0, fun(_) -> reset() end, [
        fun lifecycle_has_same_batch_id/0,
        fun finished_records_result_policy_fields/0,
        fun worker_crash_is_reported/0,
        fun batch_timeout_is_reported/0
    ]}.

lifecycle_has_same_batch_id() ->
    install_collector(self()),
    K = echo_client(),
    {_, Records, _} = beamai_agent_utils:execute_tools(
                         K, [tc(<<"1">>, <<"echo">>)], beamai_context:new(), false),
    Queued = wait([beamai, tool, queued]),
    Started = wait([beamai, tool, started]),
    Finished = wait([beamai, tool, finished]),
    ?assertEqual([queued, started, finished],
                 [event_kind(Event) || Event <- [Queued, Started, Finished]]),
    BatchId = maps:get(batch_id, maps:get(metadata, Queued)),
    ?assertEqual(BatchId, maps:get(batch_id, maps:get(metadata, Started))),
    ?assertEqual(BatchId, maps:get(batch_id, maps:get(metadata, Finished))),
    ?assertEqual(<<"1">>, maps:get(tool_call_id, maps:get(metadata, Finished))),
    ?assertMatch([#{name := <<"echo">>}], Records).

finished_records_result_policy_fields() ->
    install_collector(self()),
    K = tool_client(<<"fail">>, fun(_, _) -> {error, missing_input} end),
    {_, _, _} = beamai_agent_utils:execute_tools(
                  K, [tc(<<"err">>, <<"fail">>)], beamai_context:new(), false),
    _ = wait([beamai, tool, queued]),
    _ = wait([beamai, tool, started]),
    Finished = wait([beamai, tool, finished]),
    Data = maps:get(data, Finished),
    ?assertEqual(error, maps:get(status, Data)),
    ?assert(is_integer(maps:get(duration_ms, Data))),
    ?assert(is_integer(maps:get(result_size, Data))),
    ?assert(maps:get(result_size, Data) > 0).

worker_crash_is_reported() ->
    install_collector(self()),
    CrashFilter = beamai_filter:new(<<"crash_worker">>, #{
        around_tool => fun(_Req, _FCtx, _Next) -> exit(worker_killed) end
    }),
    K0 = beamai_chat_client:new(#{}, [CrashFilter]),
    K = beamai_chat_client:add_tool(
          K0, #{name => <<"boom">>, parameters => #{},
                handler => fun(_, _) -> {ok, <<"unused">>} end}),
    {_, _, _} = beamai_agent_utils:execute_tools(
                  K, [tc(<<"crash-1">>, <<"boom">>), tc(<<"crash-2">>, <<"boom">>)],
                  beamai_context:new(#{kill => true}), true),
    Crash = wait([beamai, tool, crash]),
    ?assertEqual(error, maps:get(level, Crash)),
    ?assertEqual(1, maps:get(attempt, maps:get(data, Crash))).

batch_timeout_is_reported() ->
    install_collector(self()),
    K = tool_client(<<"slow">>, fun(_, _) -> timer:sleep(100), {ok, <<"late">>} end),
    TCM = beamai_tool_calling_manager:concurrent(#{batch_timeout => 1}),
    Result = beamai_tool_calling_manager:execute_tool_calls(
               TCM, K, [tc(<<"slow">>, <<"slow">>)],
               #{context => beamai_context:new()}),
    #{records := [_]} = Result,
    _ = wait([beamai, tool, queued]),
    Timeout = wait([beamai, tool, timeout]),
    ?assertEqual(warning, maps:get(level, Timeout)),
    ?assertEqual(tool_timeout,
                 maps:get(reason_class, maps:get(data, Timeout))).

echo_client() ->
    tool_client(<<"echo">>, fun(_A, _C) -> {ok, <<"done">>} end).

tool_client(Name, Handler) ->
    beamai_chat_client:add_tool(
      beamai_chat_client:new(),
      #{name => Name, parameters => #{}, handler => Handler}).

tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

install_collector(Self) ->
    ok = beamai_telemetry:add_handler(
           test, fun(Event) -> Self ! {telemetry, Event} end).

event_kind(#{name := [beamai, tool, Kind]}) -> Kind.

wait(Name) ->
    receive
        {telemetry, #{name := Name} = Event} -> Event;
        {telemetry, _} -> wait(Name)
    after 1000 -> erlang:error({telemetry_timeout, Name})
    end.

reset() ->
    application:unset_env(beamai_core, telemetry).
