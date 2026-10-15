%%%-------------------------------------------------------------------
%%% @doc Offline tests for the repeatable benchmark harness.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_benchmark_tests).

-include_lib("eunit/include/eunit.hrl").

available_scenarios_are_sorted_test() ->
    Scenarios = beamai_benchmark:available(),
    ?assertEqual(lists:sort(Scenarios), Scenarios),
    ?assertEqual(15, length(Scenarios)),
    ?assert(lists:member(noop, Scenarios)),
    ?assert(lists:member(filter_chain_16, Scenarios)),
    ?assert(lists:member(tool_dispatch_256, Scenarios)),
    ?assert(lists:member(tool_manager_concurrent_16, Scenarios)),
    ?assert(lists:member(tool_manager_sequential_16, Scenarios)),
    ?assert(lists:member(memory_history_append_read, Scenarios)),
    ?assert(lists:member(agent_event_fanout_4, Scenarios)),
    ?assert(lists:member(http_pool_acquire_release, Scenarios)),
    ?assert(lists:member(provider_openai_encode_decode, Scenarios)).

single_scenario_produces_statistics_test() ->
    Result = beamai_benchmark:run(noop, #{iterations => 20, warmup => 2}),
    ?assertEqual(noop, maps:get(scenario, Result)),
    ?assertEqual(20, maps:get(iterations, Result)),
    ?assertEqual(2, maps:get(warmup, Result)),
    Stats = maps:get(stats, Result),
    ?assert(maps:get(p50_us, Stats) >= 0),
    ?assert(maps:get(p95_us, Stats) >= maps:get(p50_us, Stats)),
    ?assert(maps:get(max_us, Stats) >= maps:get(p95_us, Stats)),
    Environment = maps:get(environment, Result),
    ?assert(maps:is_key(otp_release, Environment)),
    ?assert(maps:is_key(cpu_topology, Environment)),
    ?assert(is_binary(maps:get(timestamp_utc, Environment))).

formatted_output_contains_required_fields_test() ->
    Result = beamai_benchmark:run(noop, #{iterations => 3, warmup => 0}),
    Text = beamai_benchmark:format([Result]),
    ?assertEqual(true, string:find(Text, <<"scenario">>) =/= nomatch),
    ?assertEqual(true, string:find(Text, <<"p50_us">>) =/= nomatch),
    ?assertEqual(true, string:find(Text, <<"p95_us">>) =/= nomatch),
    ?assertEqual(true, string:find(Text, <<"max_us">>) =/= nomatch),
    ?assertEqual(true, string:find(Text, <<"cpu_topology">>) =/= nomatch).

core_scenarios_execute_with_setup_and_cleanup_test() ->
    Scenarios = [filter_chain_0, filter_chain_1, filter_chain_4,
                 filter_chain_16, tool_dispatch_1, tool_dispatch_8,
                 tool_dispatch_64, tool_dispatch_256,
                 tool_manager_concurrent_16, tool_manager_sequential_16,
                 memory_history_append_read, agent_event_fanout_4,
                 provider_openai_encode_decode],
    lists:foreach(fun(Scenario) ->
        Result = beamai_benchmark:run(
                   Scenario, #{iterations => 2, warmup => 1}),
        ?assertEqual(Scenario, maps:get(scenario, Result)),
        ?assertEqual(2, maps:get(iterations, Result))
    end, Scenarios).

http_pool_scenario_reuses_local_connection_test() ->
    Result = beamai_benchmark:run(
               http_pool_acquire_release, #{iterations => 5, warmup => 1}),
    ?assertEqual(http_pool_acquire_release, maps:get(scenario, Result)),
    ?assertEqual(5, maps:get(iterations, Result)).
