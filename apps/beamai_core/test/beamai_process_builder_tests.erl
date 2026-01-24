-module(beamai_process_builder_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Builder construction tests
%%====================================================================

new_builder_test() ->
    B = beamai_process_builder:new(my_process),
    ?assert(maps:get('__process_builder__', B)),
    ?assertEqual(my_process, maps:get(name, B)),
    ?assertEqual(#{}, maps:get(steps, B)),
    ?assertEqual([], maps:get(bindings, B)),
    ?assertEqual([], maps:get(initial_events, B)),
    ?assertEqual(undefined, maps:get(error_handler, B)),
    ?assertEqual(concurrent, maps:get(execution_mode, B)).

add_step_test() ->
    B0 = beamai_process_builder:new(test),
    B1 = beamai_process_builder:add_step(B0, step_a, test_steps),
    Steps = maps:get(steps, B1),
    ?assert(maps:is_key(step_a, Steps)),
    #{step_a := StepDef} = Steps,
    ?assert(maps:get('__step_def__', StepDef)),
    ?assertEqual(step_a, maps:get(id, StepDef)),
    ?assertEqual(test_steps, maps:get(module, StepDef)),
    ?assertEqual(#{}, maps:get(config, StepDef)),
    ?assertEqual([input], maps:get(required_inputs, StepDef)).

add_step_with_config_test() ->
    B0 = beamai_process_builder:new(test),
    Config = #{type => passthrough, required_inputs => [a, b]},
    B1 = beamai_process_builder:add_step(B0, my_step, test_steps, Config),
    #{my_step := StepDef} = maps:get(steps, B1),
    ?assertEqual([a, b], maps:get(required_inputs, StepDef)),
    ?assertEqual(Config, maps:get(config, StepDef)).

add_binding_test() ->
    B0 = beamai_process_builder:new(test),
    Binding = beamai_process_event:binding(ev, step_a, input),
    B1 = beamai_process_builder:add_binding(B0, Binding),
    ?assertEqual([Binding], maps:get(bindings, B1)).

add_initial_event_test() ->
    B0 = beamai_process_builder:new(test),
    Event = beamai_process_event:new(start, #{}),
    B1 = beamai_process_builder:add_initial_event(B0, Event),
    ?assertEqual([Event], maps:get(initial_events, B1)).

set_execution_mode_test() ->
    B0 = beamai_process_builder:new(test),
    B1 = beamai_process_builder:set_execution_mode(B0, sequential),
    ?assertEqual(sequential, maps:get(execution_mode, B1)).

%%====================================================================
%% Compile tests
%%====================================================================

compile_valid_test() ->
    B0 = beamai_process_builder:new(test),
    B1 = beamai_process_builder:add_step(B0, step_a, test_steps,
                                          #{type => passthrough}),
    Binding = beamai_process_event:binding(start, step_a, input),
    B2 = beamai_process_builder:add_binding(B1, Binding),
    Event = beamai_process_event:new(start, #{}),
    B3 = beamai_process_builder:add_initial_event(B2, Event),
    {ok, Def} = beamai_process_builder:compile(B3),
    ?assert(maps:get('__process_spec__', Def)),
    ?assertEqual(test, maps:get(name, Def)),
    ?assert(maps:is_key(step_a, maps:get(steps, Def))).

compile_invalid_module_test() ->
    B0 = beamai_process_builder:new(test),
    B1 = beamai_process_builder:add_step(B0, step_a, nonexistent_module_xyz),
    {error, Errors} = beamai_process_builder:compile(B1),
    ?assert(length(Errors) > 0),
    [{step_module_not_found, step_a, nonexistent_module_xyz}] = Errors.

compile_invalid_binding_target_test() ->
    B0 = beamai_process_builder:new(test),
    B1 = beamai_process_builder:add_step(B0, step_a, test_steps,
                                          #{type => passthrough}),
    Binding = beamai_process_event:binding(ev, nonexistent_step, input),
    B2 = beamai_process_builder:add_binding(B1, Binding),
    {error, Errors} = beamai_process_builder:compile(B2),
    ?assert(lists:any(
        fun({binding_target_not_found, nonexistent_step}) -> true;
           (_) -> false
        end, Errors)).
