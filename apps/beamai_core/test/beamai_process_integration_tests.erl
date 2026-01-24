-module(beamai_process_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Integration tests using the beamai_process facade API
%%====================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         fun facade_builder_subtest/0,
         fun end_to_end_sequential_subtest/0,
         fun facade_fanout_subtest/0,
         fun transform_binding_subtest/0,
         fun facade_cycle_subtest/0,
         fun facade_fanin_subtest/0,
         fun facade_hitl_subtest/0
     ]}.

setup() ->
    {ok, Pid} = beamai_process_sup:start_link(),
    Pid.

cleanup(Pid) ->
    unlink(Pid),
    exit(Pid, shutdown),
    ok.

%%====================================================================
%% Facade builder API test
%%====================================================================

facade_builder_subtest() ->
    P0 = beamai_process:builder(my_workflow),
    P1 = beamai_process:add_step(P0, step_a, test_steps, #{type => passthrough, output_event => a_done}),
    P2 = beamai_process:add_step(P1, step_b, test_steps, #{type => passthrough, output_event => b_done}),
    P3 = beamai_process:on_event(P2, start, step_a, input),
    P4 = beamai_process:on_event(P3, a_done, step_b, input),
    P5 = beamai_process:set_initial_event(P4, start, #{msg => <<"go">>}),
    P6 = beamai_process:set_execution_mode(P5, sequential),
    {ok, Def} = beamai_process:build(P6),
    ?assert(maps:get('__process_spec__', Def)),
    ?assertEqual(my_workflow, maps:get(name, Def)).

%%====================================================================
%% End-to-end workflow with facade
%%====================================================================

end_to_end_sequential_subtest() ->
    P0 = beamai_process:builder(e2e_seq),
    P1 = beamai_process:add_step(P0, producer, test_steps,
                                  #{type => passthrough, output_event => produced}),
    P2 = beamai_process:add_step(P1, consumer, test_steps,
                                  #{type => passthrough, output_event => consumed}),
    P3 = beamai_process:on_event(P2, trigger, producer, input),
    P4 = beamai_process:on_event(P3, produced, consumer, input),
    P5 = beamai_process:set_initial_event(P4, trigger, #{data => 42}),
    P6 = beamai_process:set_execution_mode(P5, sequential),
    {ok, Def} = beamai_process:build(P6),
    {ok, Result} = beamai_process:run_sync(Def, #{timeout => 5000}),
    #{producer := ProdState, consumer := ConsState} = Result,
    ?assertEqual(1, maps:get(activation_count, ProdState)),
    ?assertEqual(1, maps:get(activation_count, ConsState)).

%%====================================================================
%% Fanout with facade
%%====================================================================

facade_fanout_subtest() ->
    P0 = beamai_process:builder(facade_fanout),
    P1 = beamai_process:add_step(P0, worker_a, test_steps,
                                  #{type => passthrough, output_event => a_done}),
    P2 = beamai_process:add_step(P1, worker_b, test_steps,
                                  #{type => passthrough, output_event => b_done}),
    P3 = beamai_process:on_event(P2, dispatch, worker_a, input),
    P4 = beamai_process:on_event(P3, dispatch, worker_b, input),
    P5 = beamai_process:set_initial_event(P4, dispatch, #{task => go}),
    P6 = beamai_process:set_execution_mode(P5, sequential),
    {ok, Def} = beamai_process:build(P6),
    {ok, Result} = beamai_process:run_sync(Def, #{timeout => 5000}),
    #{worker_a := WA, worker_b := WB} = Result,
    ?assertEqual(1, maps:get(activation_count, WA)),
    ?assertEqual(1, maps:get(activation_count, WB)).

%%====================================================================
%% Transform binding test
%%====================================================================

transform_binding_subtest() ->
    P0 = beamai_process:builder(transform),
    P1 = beamai_process:add_step(P0, step_a, test_steps,
                                  #{type => passthrough, output_event => done}),
    P2 = beamai_process:on_event(P1, start, step_a, input,
                                  fun(Data) -> #{transformed => Data} end),
    P3 = beamai_process:set_initial_event(P2, start, original_data),
    P4 = beamai_process:set_execution_mode(P3, sequential),
    {ok, Def} = beamai_process:build(P4),
    {ok, Result} = beamai_process:run_sync(Def, #{timeout => 5000}),
    #{step_a := StA} = Result,
    ?assertEqual(1, maps:get(activation_count, StA)).

%%====================================================================
%% Cycle with facade
%%====================================================================

facade_cycle_subtest() ->
    P0 = beamai_process:builder(facade_cycle),
    P1 = beamai_process:add_step(P0, looper, test_steps,
                                  #{type => counter, max_count => 5,
                                    output_event => finished, loop_event => again}),
    P2 = beamai_process:on_event(P1, kick, looper, input),
    P3 = beamai_process:on_event(P2, again, looper, input),
    P4 = beamai_process:set_initial_event(P3, kick),
    P5 = beamai_process:set_execution_mode(P4, sequential),
    {ok, Def} = beamai_process:build(P5),
    {ok, Result} = beamai_process:run_sync(Def, #{timeout => 5000}),
    #{looper := LooperState} = Result,
    ?assertEqual(5, maps:get(activation_count, LooperState)),
    #{state := #{count := 5}} = LooperState.

%%====================================================================
%% Fan-in with facade
%%====================================================================

facade_fanin_subtest() ->
    P0 = beamai_process:builder(facade_fanin),
    P1 = beamai_process:add_step(P0, left, test_steps,
                                  #{type => passthrough, output_event => left_out}),
    P2 = beamai_process:add_step(P1, right, test_steps,
                                  #{type => passthrough, output_event => right_out}),
    P3 = beamai_process:add_step(P2, joiner, test_steps,
                                  #{type => passthrough, output_event => joined,
                                    required_inputs => [left_in, right_in]}),
    P4 = beamai_process:on_event(P3, go, left, input),
    P5 = beamai_process:on_event(P4, go, right, input),
    P6 = beamai_process:on_event(P5, left_out, joiner, left_in),
    P7 = beamai_process:on_event(P6, right_out, joiner, right_in),
    P8 = beamai_process:set_initial_event(P7, go, start),
    P9 = beamai_process:set_execution_mode(P8, sequential),
    {ok, Def} = beamai_process:build(P9),
    {ok, Result} = beamai_process:run_sync(Def, #{timeout => 5000}),
    #{joiner := JoinerState} = Result,
    ?assertEqual(1, maps:get(activation_count, JoinerState)).

%%====================================================================
%% HITL with facade
%%====================================================================

facade_hitl_subtest() ->
    P0 = beamai_process:builder(facade_hitl),
    P1 = beamai_process:add_step(P0, human_step, test_steps, #{type => pause}),
    P2 = beamai_process:on_event(P1, ask, human_step, input),
    P3 = beamai_process:set_initial_event(P2, ask, #{question => <<"confirm?">>}),
    P4 = beamai_process:set_execution_mode(P3, sequential),
    {ok, Def} = beamai_process:build(P4),
    {ok, Pid} = beamai_process:start(Def),
    timer:sleep(100),
    {ok, #{state := paused}} = beamai_process:get_status(Pid),
    ok = beamai_process:resume(Pid, <<"yes">>),
    timer:sleep(100),
    {ok, #{state := completed}} = beamai_process:get_status(Pid),
    beamai_process:stop(Pid).

%%====================================================================
%% Concurrent mode test (requires poolboy)
%%====================================================================

concurrent_test_() ->
    {setup,
     fun start_concurrent/0,
     fun stop_concurrent/1,
     [fun concurrent_fanout_subtest/0]
    }.

concurrent_fanout_subtest() ->
    P0 = beamai_process:builder(concurrent_test),
    P1 = beamai_process:add_step(P0, w1, test_steps,
                                  #{type => passthrough, output_event => w1_done}),
    P2 = beamai_process:add_step(P1, w2, test_steps,
                                  #{type => passthrough, output_event => w2_done}),
    P3 = beamai_process:on_event(P2, go, w1, input),
    P4 = beamai_process:on_event(P3, go, w2, input),
    P5 = beamai_process:set_initial_event(P4, go, #{data => concurrent}),
    {ok, Def} = beamai_process:build(P5),
    {ok, Result} = beamai_process:run_sync(Def, #{timeout => 5000}),
    #{w1 := W1, w2 := W2} = Result,
    ?assertEqual(1, maps:get(activation_count, W1)),
    ?assertEqual(1, maps:get(activation_count, W2)).

%%====================================================================
%% Helpers
%%====================================================================

start_concurrent() ->
    PoolArgs = [
        {name, {local, beamai_process_pool}},
        {worker_module, beamai_process_worker},
        {size, 5},
        {max_overflow, 10}
    ],
    {ok, PoolPid} = poolboy:start_link(PoolArgs, []),
    {ok, SupPid} = beamai_process_sup:start_link(),
    {PoolPid, SupPid}.

stop_concurrent({PoolPid, SupPid}) ->
    unlink(SupPid),
    exit(SupPid, shutdown),
    unlink(PoolPid),
    exit(PoolPid, shutdown),
    ok.
