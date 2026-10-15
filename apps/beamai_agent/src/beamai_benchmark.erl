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
    lists:sort(maps:keys(scenario_map())).

%% @doc Format structured results as the same text emitted by `run/0,1,2'.
-spec format([result()]) -> binary().
format(Results) ->
    format_results(Results).

%% @doc Run all available scenarios with default parameters.
-spec run() -> [result()].
run() ->
    Results = [run_scenario(Scenario, maps:get(Scenario, scenario_map()),
                            maps:get(run, maps:get(Scenario, scenario_map())),
                            1000, 10, false)
               || Scenario <- available()],
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
        {ok, #{run := Fun} = Definition} ->
            Iterations = maps:get(iterations, Opts, 1000),
            Warmup = maps:get(warmup, Opts, 10),
            run_scenario(Scenario, Definition, Fun, Iterations, Warmup, true);
        error ->
            erlang:error({unknown_benchmark_scenario, Scenario, available()})
    end.

%%====================================================================
%% Internal functions
%%====================================================================

scenario_map() ->
    maps:merge(
      maps:merge(noop_scenarios(), filter_scenarios()),
      maps:merge(
        maps:merge(tool_dispatch_scenarios(), tool_manager_scenarios()),
        maps:merge(
          maps:merge(memory_scenarios(), event_scenarios()),
          maps:merge(http_pool_scenarios(), provider_scenarios())))).

noop_scenarios() ->
    #{noop => scenario(fun() -> ok end)}.

filter_scenarios() ->
    maps:from_list([
        {scenario_name(filter_chain, Layers),
         scenario(filter_scenario(Layers))} ||
            Layers <- [0, 1, 4, 16]
    ]).

tool_dispatch_scenarios() ->
    maps:from_list([
        {scenario_name(tool_dispatch, Count),
         scenario(tool_dispatch_scenario(Count))} ||
            Count <- [1, 8, 64, 256]
    ]).

tool_manager_scenarios() ->
    maps:from_list([
        {tool_manager_concurrent_16,
         scenario(tool_manager_scenario(beamai_tool_calling_manager:concurrent()))},
        {tool_manager_sequential_16,
         scenario(tool_manager_scenario(beamai_tool_calling_manager:sequential()))}
    ]).

memory_scenarios() ->
    #{memory_history_append_read =>
          #{setup => fun setup_memory/0,
            run => fun run_memory/1,
            cleanup => fun cleanup_memory/1}}.

event_scenarios() ->
    #{agent_event_fanout_4 =>
          #{setup => fun setup_event_fanout/0,
            run => fun run_event_fanout/1,
            cleanup => fun(_SetupResult) -> ok end}}.

http_pool_scenarios() ->
    #{http_pool_acquire_release =>
          #{setup => fun setup_http_pool/0,
            run => fun run_http_pool/1,
            cleanup => fun cleanup_http_pool/1}}.

provider_scenarios() ->
    #{provider_openai_encode_decode =>
          #{setup => fun setup_openai_adapter/0,
            run => fun run_openai_adapter/1,
            cleanup => fun(_SetupResult) -> ok end}}.

scenario(Run) ->
    #{setup => fun() -> ok end,
      run => fun(_SetupResult) -> Run() end,
      cleanup => fun(_SetupResult) -> ok end}.

scenario_name(Prefix, Count) ->
    list_to_atom(atom_to_list(Prefix) ++ "_" ++ integer_to_list(Count)).

filter_scenario(Layers) ->
    Filters = [beamai_filter:new(
                 <<"benchmark_filter_", (integer_to_binary(Index))/binary>>,
                 #{around_chat => fun(Request, _FCtx, Next) -> Next(Request) end})
               || Index <- lists:seq(1, Layers)],
    Request = #{messages => [], context => beamai_context:new()},
    Terminal = fun(InnerRequest) -> InnerRequest end,
    fun() ->
        _ = beamai_filter_chain:run_with_context(
              Filters, around_chat, Terminal, Request),
        ok
    end.

tool_dispatch_scenario(Count) ->
    {Manager, ChatClient, Calls, Context} = tool_benchmark_inputs(
                                              beamai_tool_calling_manager:concurrent(),
                                              Count),
    fun() ->
        _ = beamai_tool_calling_manager:execute_tool_calls(
              Manager, ChatClient, Calls, #{context => Context}),
        ok
    end.

tool_manager_scenario(Manager) ->
    {CapturedManager, ChatClient, Calls, Context} =
        tool_benchmark_inputs(Manager, 16),
    fun() ->
        _ = beamai_tool_calling_manager:execute_tool_calls(
              CapturedManager, ChatClient, Calls,
              #{context => Context, parallel => true}),
        ok
    end.

tool_benchmark_inputs(Manager, Count) ->
    Tool = beamai_tool:new(<<"benchmark_noop">>, fun(_Args) -> {ok, ok} end),
    ChatClient = beamai_chat_client:add_tool(beamai_chat_client:new(), Tool),
    Calls = lists:duplicate(Count, benchmark_tool_call()),
    {Manager, ChatClient, Calls, beamai_context:new()}.

benchmark_tool_call() ->
    #{
        id => <<"call_benchmark">>,
        type => function,
        function => #{name => <<"benchmark_noop">>, arguments => <<"{}">>}
    }.

setup_memory() ->
    StoreName = benchmark_name(memory_benchmark),
    {ok, _} = beamai_chat_memory_ets:start_link(StoreName),
    #{
        provider => beamai_memory_provider:default(
                      beamai_chat_memory_ets:handle(StoreName)),
        conversation_id => <<"benchmark-memory">>,
        messages => [beamai_message:user(<<"benchmark">>)],
        store_name => StoreName
    }.

run_memory(#{provider := Provider, conversation_id := ConversationId,
             messages := Messages}) ->
    ok = beamai_memory_provider:append(Provider, ConversationId, Messages),
    _ = beamai_memory_provider:history(Provider, ConversationId),
    ok = beamai_memory_provider:clear(Provider, ConversationId),
    ok.

cleanup_memory(#{store_name := StoreName}) ->
    ok = beamai_chat_memory_ets:stop(StoreName).

setup_event_fanout() ->
    Consumers = [fun(_Event) -> ok end || _ <- lists:seq(1, 4)],
    Event = beamai_agent_event:new(
              state_changed,
              #{agent_id => <<"benchmark-agent">>,
                run_id => <<"benchmark-run">>,
                conversation_id => <<"benchmark-conversation">>,
                timestamp => 0},
              #{changed => [<<"benchmark">>]},
              1),
    {Event, Consumers}.

run_event_fanout({Event, Consumers}) ->
    ok = beamai_agent_event:publish(Event, Consumers).

setup_http_pool() ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, ListenSocket} = gen_tcp:listen(
                           0, [binary, {ip, {127, 0, 0, 1}},
                               {backlog, 1}, {active, false}]),
    {ok, Port} = inet:port(ListenSocket),
    Parent = self(),
    Listener = spawn_link(fun() -> http_listener(Parent, ListenSocket) end),
    ok = gen_tcp:controlling_process(ListenSocket, Listener),
    Listener ! {socket, ListenSocket},
    PoolName = benchmark_name(http_pool_benchmark),
    PoolConfig = #{
        max_connections_per_host => 1,
        connect_timeout => 1000,
        idle_timeout => 60000,
        protocols => [http],
        tls_insecure => false
    },
    case gen_server:start({local, PoolName}, beamai_http_pool,
                          [PoolName, PoolConfig], []) of
        {ok, _PoolPid} ->
            #{pool => PoolName,
              url => iolist_to_binary(
                       ["http://127.0.0.1:", integer_to_binary(Port),
                        "/benchmark"]),
              listener => Listener};
        {error, Reason} ->
            exit(Listener, kill),
            erlang:error({benchmark_http_pool_failed, Reason})
    end.

run_http_pool(#{pool := Pool, url := Url}) ->
    {ok, Connection} = beamai_http_pool:get_connection(Pool, Url),
    ok = beamai_http_pool:return_connection(Pool, Connection).

cleanup_http_pool(#{pool := Pool, listener := Listener}) ->
    ok = gen_server:stop(Pool),
    Listener ! stop,
    receive
        listener_stopped -> ok
    after 1000 -> ok
    end.

http_listener(Parent, ListenSocket) ->
    receive
        {socket, Socket} -> http_listening(Parent, Socket);
        stop ->
            gen_tcp:close(ListenSocket),
            Parent ! listener_stopped
    end.

http_listening(Parent, ListenSocket) ->
    receive
        stop ->
            gen_tcp:close(ListenSocket),
            Parent ! listener_stopped
    after 0 ->
        case gen_tcp:accept(ListenSocket) of
            {ok, ClientSocket} ->
                Holder = spawn(fun() -> timer:sleep(60000) end),
                ok = gen_tcp:controlling_process(ClientSocket, Holder),
                http_listening(Parent, ListenSocket);
            {error, closed} ->
                ok
        end
    end.

setup_openai_adapter() ->
    Config = (beamai_llm_provider_openai:default_config())#{api_key => <<"benchmark">>},
    Messages = [beamai_message:user(<<"benchmark prompt">>),
                beamai_message:assistant(<<"benchmark response">>)],
    BenchmarkTool = beamai_tool:new(
                      <<"benchmark_tool">>,
                      fun(_Args) -> {ok, ok} end,
                      #{description => <<"No-op benchmark tool">>}),
    Tools = [beamai_tool:to_tool_spec(BenchmarkTool)],
    Request = beamai_chat_request:new(
                Messages,
                #{tools => Tools, tool_choice => auto, stream => false}),
    #{
      config => Config,
      request => Request,
      parser => beamai_llm_provider_openai:parser(Config),
      response => benchmark_openai_response()
    }.

run_openai_adapter(#{config := Config, request := Request,
                     parser := Parser, response := Response}) ->
    _Body = beamai_llm_provider_openai:body(Config, Request),
    {ok, _Parsed} = Parser(Response),
    ok.

benchmark_openai_response() ->
    #{
        <<"id">> => <<"chatcmpl-benchmark">>,
        <<"object">> => <<"chat.completion">>,
        <<"created">> => 0,
        <<"model">> => <<"benchmark-model">>,
        <<"choices">> => [
            #{
                <<"index">> => 0,
                <<"message">> => #{
                    <<"role">> => <<"assistant">>,
                    <<"content">> => <<"benchmark response">>
                },
                <<"finish_reason">> => <<"stop">>
            }
        ],
        <<"usage">> => #{
            <<"prompt_tokens">> => 2,
            <<"completion_tokens">> => 2,
            <<"total_tokens">> => 4
        }
    }.

benchmark_name(Prefix) ->
    list_to_atom(atom_to_list(Prefix) ++ "_" ++
                     integer_to_list(erlang:unique_integer([positive]))).

run_scenario(Scenario, Definition, Fun, Iterations, Warmup, Print) ->
    Setup = maps:get(setup, Definition, fun() -> ok end),
    Cleanup = maps:get(cleanup, Definition, fun(_SetupResult) -> ok end),
    SetupResult = Setup(),
    lists:foreach(fun(_) -> Fun(SetupResult) end, lists:seq(1, Warmup)),
    try
        Samples = [measure(fun() -> Fun(SetupResult) end)
                   || _ <- lists:seq(1, Iterations)],
        Result = #{
            scenario => Scenario,
            iterations => Iterations,
            warmup => Warmup,
            stats => stats(Samples),
            environment => environment()
        },
        case Print of
            true -> print_results([Result]);
            false -> ok
        end,
        Result
    after
        Cleanup(SetupResult)
    end.

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
               "~-34s ~-11s ~-10s ~-10s ~-10s ~-10s~n",
               ["scenario", "iterations", "p50_us", "p95_us", "max_us",
                "mean_us"]),
    Rows = [row(Result) || Result <- Results],
    Environment = environment_lines(Results),
    iolist_to_binary([Header, Rows, $\n, Environment]).

row(#{scenario := Scenario, iterations := Iterations, stats := Stats}) ->
    io_lib:format(
      "~-34ts ~-11b ~-10.2f ~-10.2f ~-10.2f ~-10.2f~n",
      [atom_to_list(Scenario), Iterations,
       microseconds(maps:get(p50_us, Stats)),
       microseconds(maps:get(p95_us, Stats)),
       microseconds(maps:get(max_us, Stats)),
       microseconds(maps:get(mean_us, Stats))]).

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
