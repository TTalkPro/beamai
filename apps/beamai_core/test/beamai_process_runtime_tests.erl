-module(beamai_process_runtime_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test fixture - start process supervisor
%%====================================================================

runtime_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         fun linear_pipeline_subtest/0,
         fun fanout_subtest/0,
         fun fanin_subtest/0,
         fun cycle_subtest/0,
         fun pause_resume_subtest/0,
         fun snapshot_subtest/0,
         fun send_event_subtest/0,
         fun error_no_handler_subtest/0,
         fun get_status_subtest/0
     ]}.

setup() ->
    {ok, Pid} = beamai_process_sup:start_link(),
    Pid.

cleanup(Pid) ->
    unlink(Pid),
    exit(Pid, shutdown),
    ok.

%%====================================================================
%% Linear pipeline test: A -> B -> C
%%====================================================================

linear_pipeline_subtest() ->
    B0 = beamai_process_builder:new(linear),
    B1 = beamai_process_builder:add_step(B0, step_a, test_steps,
                                          #{type => passthrough, output_event => a_out}),
    B2 = beamai_process_builder:add_step(B1, step_b, test_steps,
                                          #{type => passthrough, output_event => b_out}),
    B3 = beamai_process_builder:add_step(B2, step_c, test_steps,
                                          #{type => passthrough, output_event => c_out}),
    B4 = beamai_process_builder:add_binding(B3,
        beamai_process_event:binding(start, step_a, input)),
    B5 = beamai_process_builder:add_binding(B4,
        beamai_process_event:binding(a_out, step_b, input)),
    B6 = beamai_process_builder:add_binding(B5,
        beamai_process_event:binding(b_out, step_c, input)),
    B7 = beamai_process_builder:add_initial_event(B6,
        beamai_process_event:new(start, #{msg => <<"hello">>})),
    B8 = beamai_process_builder:set_execution_mode(B7, sequential),
    {ok, Def} = beamai_process_builder:compile(B8),
    {ok, StepsResult} = beamai_process:run_sync(Def, #{timeout => 5000}),
    #{step_a := StA, step_b := StB, step_c := StC} = StepsResult,
    ?assertEqual(1, maps:get(activation_count, StA)),
    ?assertEqual(1, maps:get(activation_count, StB)),
    ?assertEqual(1, maps:get(activation_count, StC)).

%%====================================================================
%% Fan-out test: one event triggers two steps
%%====================================================================

fanout_subtest() ->
    B0 = beamai_process_builder:new(fanout),
    B1 = beamai_process_builder:add_step(B0, step_a, test_steps,
                                          #{type => passthrough, output_event => a_out}),
    B2 = beamai_process_builder:add_step(B1, step_b, test_steps,
                                          #{type => passthrough, output_event => b_out}),
    B3 = beamai_process_builder:add_binding(B2,
        beamai_process_event:binding(start, step_a, input)),
    B4 = beamai_process_builder:add_binding(B3,
        beamai_process_event:binding(start, step_b, input)),
    B5 = beamai_process_builder:add_initial_event(B4,
        beamai_process_event:new(start, trigger)),
    B6 = beamai_process_builder:set_execution_mode(B5, sequential),
    {ok, Def} = beamai_process_builder:compile(B6),
    {ok, StepsResult} = beamai_process:run_sync(Def, #{timeout => 5000}),
    #{step_a := StA, step_b := StB} = StepsResult,
    ?assertEqual(1, maps:get(activation_count, StA)),
    ?assertEqual(1, maps:get(activation_count, StB)).

%%====================================================================
%% Fan-in test: step waits for two inputs
%%====================================================================

fanin_subtest() ->
    B0 = beamai_process_builder:new(fanin),
    B1 = beamai_process_builder:add_step(B0, src_a, test_steps,
                                          #{type => passthrough, output_event => out_a}),
    B2 = beamai_process_builder:add_step(B1, src_b, test_steps,
                                          #{type => passthrough, output_event => out_b}),
    B3 = beamai_process_builder:add_step(B2, target, test_steps,
                                          #{type => passthrough, output_event => done,
                                            required_inputs => [a, b]}),
    B4 = beamai_process_builder:add_binding(B3,
        beamai_process_event:binding(start, src_a, input)),
    B5 = beamai_process_builder:add_binding(B4,
        beamai_process_event:binding(start, src_b, input)),
    B6 = beamai_process_builder:add_binding(B5,
        beamai_process_event:binding(out_a, target, a)),
    B7 = beamai_process_builder:add_binding(B6,
        beamai_process_event:binding(out_b, target, b)),
    B8 = beamai_process_builder:add_initial_event(B7,
        beamai_process_event:new(start, data)),
    B9 = beamai_process_builder:set_execution_mode(B8, sequential),
    {ok, Def} = beamai_process_builder:compile(B9),
    {ok, StepsResult} = beamai_process:run_sync(Def, #{timeout => 5000}),
    #{target := StTarget} = StepsResult,
    ?assertEqual(1, maps:get(activation_count, StTarget)).

%%====================================================================
%% Cycle test: step loops N times
%%====================================================================

cycle_subtest() ->
    B0 = beamai_process_builder:new(cycle),
    B1 = beamai_process_builder:add_step(B0, counter, test_steps,
                                          #{type => counter, max_count => 3,
                                            output_event => done,
                                            loop_event => loop}),
    B2 = beamai_process_builder:add_binding(B1,
        beamai_process_event:binding(start, counter, input)),
    B3 = beamai_process_builder:add_binding(B2,
        beamai_process_event:binding(loop, counter, input)),
    B4 = beamai_process_builder:add_initial_event(B3,
        beamai_process_event:new(start, go)),
    B5 = beamai_process_builder:set_execution_mode(B4, sequential),
    {ok, Def} = beamai_process_builder:compile(B5),
    {ok, StepsResult} = beamai_process:run_sync(Def, #{timeout => 5000}),
    #{counter := StCounter} = StepsResult,
    ?assertEqual(3, maps:get(activation_count, StCounter)),
    #{state := #{count := 3}} = StCounter.

%%====================================================================
%% Pause/Resume (HITL) test
%%====================================================================

pause_resume_subtest() ->
    B0 = beamai_process_builder:new(hitl),
    B1 = beamai_process_builder:add_step(B0, pauser, test_steps,
                                          #{type => pause}),
    B2 = beamai_process_builder:add_binding(B1,
        beamai_process_event:binding(start, pauser, input)),
    B3 = beamai_process_builder:add_initial_event(B2,
        beamai_process_event:new(start, trigger)),
    B4 = beamai_process_builder:set_execution_mode(B3, sequential),
    {ok, Def} = beamai_process_builder:compile(B4),
    {ok, Pid} = beamai_process:start(Def),
    timer:sleep(100),
    {ok, #{state := paused, paused_step := pauser}} = beamai_process:get_status(Pid),
    ok = beamai_process:resume(Pid, <<"human_response">>),
    timer:sleep(100),
    {ok, #{state := completed}} = beamai_process:get_status(Pid),
    beamai_process:stop(Pid).

%%====================================================================
%% Snapshot test
%%====================================================================

snapshot_subtest() ->
    B0 = beamai_process_builder:new(snap_test),
    B1 = beamai_process_builder:add_step(B0, pauser, test_steps,
                                          #{type => pause}),
    B2 = beamai_process_builder:add_binding(B1,
        beamai_process_event:binding(start, pauser, input)),
    B3 = beamai_process_builder:add_initial_event(B2,
        beamai_process_event:new(start, data)),
    B4 = beamai_process_builder:set_execution_mode(B3, sequential),
    {ok, Def} = beamai_process_builder:compile(B4),
    {ok, Pid} = beamai_process:start(Def),
    timer:sleep(100),
    {ok, Snapshot} = beamai_process:snapshot(Pid),
    ?assert(maps:get('__process_snapshot__', Snapshot)),
    ?assertEqual(paused, maps:get(current_state, Snapshot)),
    beamai_process:stop(Pid).

%%====================================================================
%% Send external event test
%%====================================================================

send_event_subtest() ->
    B0 = beamai_process_builder:new(ext_event),
    B1 = beamai_process_builder:add_step(B0, step_a, test_steps,
                                          #{type => passthrough, output_event => done}),
    B2 = beamai_process_builder:add_binding(B1,
        beamai_process_event:binding(external, step_a, input)),
    B3 = beamai_process_builder:set_execution_mode(B2, sequential),
    {ok, Def} = beamai_process_builder:compile(B3),
    {ok, Pid} = beamai_process:start(Def),
    timer:sleep(50),
    %% No initial events, so process stays idle
    {ok, #{state := idle}} = beamai_process:get_status(Pid),
    %% Send an external event to trigger step_a
    beamai_process:send_event(Pid, beamai_process_event:new(external, #{data => 1})),
    timer:sleep(100),
    {ok, #{state := completed}} = beamai_process:get_status(Pid),
    beamai_process:stop(Pid).

%%====================================================================
%% Error handling test
%%====================================================================

error_no_handler_subtest() ->
    B0 = beamai_process_builder:new(error_test),
    B1 = beamai_process_builder:add_step(B0, step_a, test_steps,
                                          #{type => passthrough, output_event => done}),
    B2 = beamai_process_builder:add_binding(B1,
        beamai_process_event:binding(start, step_a, input)),
    B3 = beamai_process_builder:add_initial_event(B2,
        beamai_process_event:new(start, ok)),
    B4 = beamai_process_builder:set_execution_mode(B3, sequential),
    {ok, Def} = beamai_process_builder:compile(B4),
    {ok, _} = beamai_process:run_sync(Def, #{timeout => 5000}).

%%====================================================================
%% Status query test
%%====================================================================

get_status_subtest() ->
    B0 = beamai_process_builder:new(status_test),
    B1 = beamai_process_builder:add_step(B0, step_a, test_steps,
                                          #{type => passthrough, output_event => done}),
    B2 = beamai_process_builder:add_binding(B1,
        beamai_process_event:binding(start, step_a, input)),
    B3 = beamai_process_builder:add_initial_event(B2,
        beamai_process_event:new(start, data)),
    B4 = beamai_process_builder:set_execution_mode(B3, sequential),
    {ok, Def} = beamai_process_builder:compile(B4),
    {ok, Pid} = beamai_process:start(Def),
    timer:sleep(100),
    {ok, Status} = beamai_process:get_status(Pid),
    ?assertEqual(completed, maps:get(state, Status)),
    ?assertEqual(status_test, maps:get(name, Status)),
    beamai_process:stop(Pid).
