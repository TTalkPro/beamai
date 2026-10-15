%%%-------------------------------------------------------------------
%%% @doc Offline tests for the repeatable benchmark harness.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_benchmark_tests).

-include_lib("eunit/include/eunit.hrl").

available_scenarios_are_sorted_test() ->
    Scenarios = beamai_benchmark:available(),
    ?assertEqual(lists:sort(Scenarios), Scenarios),
    ?assert(lists:member(noop, Scenarios)).

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
