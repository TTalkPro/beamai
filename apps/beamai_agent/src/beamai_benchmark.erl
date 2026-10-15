%%%-------------------------------------------------------------------
%%% @doc Repeatable offline performance benchmark harness.
%%%
%%% The harness measures one closure call at a time and summarizes wall-clock
%%% samples. Scenarios must use fixed inputs and mock providers so results are
%%% suitable for before/after comparisons, not absolute capacity claims.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_benchmark).

-export([available/0, run/0, run/1, run/2, format/1]).

-export_type([scenario/0, options/0, result/0]).

-type scenario() :: atom().
-type options() :: #{
    iterations => pos_integer(),
    warmup => non_neg_integer()
}.
-type result() :: #{
    scenario := scenario(),
    iterations := pos_integer(),
    warmup := non_neg_integer(),
    stats := stats(),
    environment := map()
}.
-type stats() :: #{
    mean_us := float(),
    p50_us := number(),
    p95_us := number(),
    max_us := number()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Return stable, benchmarkable scenario names.
-spec available() -> [scenario()].
available() ->
    [noop].

%% @doc Format structured results as the same text emitted by `run/0,1,2'.
-spec format([result()]) -> binary().
format(Results) ->
    format_results(Results).

%% @doc Run all available scenarios with default parameters.
-spec run() -> [result()].
run() ->
    Results = [run(Scenario) || Scenario <- available()],
    print_results(Results),
    Results.

%% @doc Run one scenario with default parameters.
-spec run(scenario()) -> result().
run(Scenario) ->
    run(Scenario, #{}).

%% @doc Run one scenario and return its summary.
%%
%% Warmup calls are discarded. Each measured call is timed independently so
%% p95 / max expose tail behavior instead of hiding it inside a total duration.
-spec run(scenario(), options()) -> result().
run(Scenario, Opts) ->
    case maps:find(Scenario, scenario_map()) of
        {ok, Fun} ->
            Iterations = maps:get(iterations, Opts, 1000),
            Warmup = maps:get(warmup, Opts, 10),
            run_scenario(Scenario, Fun, Iterations, Warmup);
        error ->
            erlang:error({unknown_benchmark_scenario, Scenario, available()})
    end.

%%====================================================================
%% Internal functions
%%====================================================================

scenario_map() ->
    #{noop => fun() -> ok end}.

run_scenario(Scenario, Fun, Iterations, Warmup) ->
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, Warmup)),
    Samples = [measure(Fun) || _ <- lists:seq(1, Iterations)],
    Result = #{
        scenario => Scenario,
        iterations => Iterations,
        warmup => Warmup,
        stats => stats(Samples),
        environment => environment()
    },
    print_results([Result]),
    Result.

measure(Fun) ->
    Started = erlang:monotonic_time(nanosecond),
    Fun(),
    erlang:convert_time_unit(
      erlang:monotonic_time(nanosecond) - Started, native, microsecond).

stats([]) ->
    erlang:error(empty_benchmark_sample_set);
stats(Samples0) ->
    Samples = lists:sort(Samples0),
    Count = length(Samples),
    Sum = lists:sum(Samples),
    #{
        mean_us => Sum / Count,
        p50_us => percentile(Samples, 50),
        p95_us => percentile(Samples, 95),
        max_us => lists:last(Samples)
    }.

percentile(Samples, Percentage) when is_list(Samples), is_integer(Percentage),
                                      Percentage >= 0, Percentage =< 100 ->
    Count = length(Samples),
    Index = max(1, ceil(Percentage * Count / 100)),
    lists:nth(Index, Samples).

environment() ->
    #{
        otp_release => erlang:system_info(otp_release),
        cpu_topology => erlang:system_info(cpu_topology),
        logical_processors => erlang:system_info(logical_processors_available),
        schedulers_configured => erlang:system_info(schedulers),
        schedulers_online => erlang:system_info(schedulers_online),
        timestamp_utc =>
            unicode:characters_to_binary(
              calendar:system_time_to_rfc3339(erlang:system_time(second),
                                              [{unit, second}, {offset, "Z"}]))
    }.

print_results(Results) ->
    io:format("~ts", [format_results(Results)]).

format_results(Results) ->
    Header = io_lib:format(
               "~-18s ~-11s ~-10s ~-10s ~-10s~n",
               ["scenario", "iterations", "p50_us", "p95_us", "max_us"]),
    Rows = [row(Result) || Result <- Results],
    Environment = environment_lines(Results),
    iolist_to_binary([Header, Rows, $\n, Environment]).

row(#{scenario := Scenario, iterations := Iterations, stats := Stats}) ->
    io_lib:format(
      "~-18ts ~-11b ~-10.2f ~-10.2f ~-10.2f~n",
      [atom_to_list(Scenario), Iterations,
       microseconds(maps:get(p50_us, Stats)),
       microseconds(maps:get(p95_us, Stats)),
       microseconds(maps:get(max_us, Stats))]).

microseconds(Microseconds) when is_integer(Microseconds) ->
    Microseconds + 0.0;
microseconds(Microseconds) when is_float(Microseconds) ->
    Microseconds.

environment_lines([]) ->
    [];
environment_lines([#{environment := Environment} | _]) ->
    io_lib:format(
      "environment otp=~ts cpu_topology=~0p logical_processors=~b "
      "schedulers=~b/~b warmup_context=yes timestamp=~ts~n",
      [maps:get(otp_release, Environment),
       maps:get(cpu_topology, Environment),
       maps:get(logical_processors, Environment),
       maps:get(schedulers_configured, Environment),
       maps:get(schedulers_online, Environment),
       maps:get(timestamp_utc, Environment)]).
