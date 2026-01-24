-module(beamai_process_step_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Step init tests
%%====================================================================

init_step_test() ->
    StepSpec = make_step_spec(step_a, test_steps, #{type => passthrough}),
    {ok, StepState} = beamai_process_step:init_step(StepSpec),
    ?assert(maps:get('__step_runtime__', StepState)),
    ?assertEqual(StepSpec, maps:get(step_spec, StepState)),
    ?assertEqual(#{}, maps:get(collected_inputs, StepState)),
    ?assertEqual(0, maps:get(activation_count, StepState)).

%%====================================================================
%% Input collection tests
%%====================================================================

collect_input_test() ->
    StepSpec = make_step_spec(step_a, test_steps, #{type => passthrough}),
    {ok, StepState} = beamai_process_step:init_step(StepSpec),
    StepsState = #{step_a => StepState},
    StepsState1 = beamai_process_step:collect_input(step_a, input, <<"data">>, StepsState),
    #{step_a := Updated} = StepsState1,
    ?assertEqual(#{input => <<"data">>}, maps:get(collected_inputs, Updated)).

collect_multiple_inputs_test() ->
    StepSpec = make_step_spec(step_a, test_steps, #{type => accumulator,
                                                   required_inputs => [a, b]}),
    {ok, StepState} = beamai_process_step:init_step(StepSpec),
    StepsState0 = #{step_a => StepState},
    StepsState1 = beamai_process_step:collect_input(step_a, a, 1, StepsState0),
    StepsState2 = beamai_process_step:collect_input(step_a, b, 2, StepsState1),
    #{step_a := Updated} = StepsState2,
    ?assertEqual(#{a => 1, b => 2}, maps:get(collected_inputs, Updated)).

collect_input_unknown_step_test() ->
    StepsState = #{},
    Result = beamai_process_step:collect_input(unknown, input, data, StepsState),
    ?assertEqual(#{}, Result).

%%====================================================================
%% Activation check tests
%%====================================================================

check_activation_all_inputs_test() ->
    StepSpec = make_step_spec(step_a, test_steps, #{type => passthrough,
                                                   required_inputs => [input]}),
    {ok, StepState0} = beamai_process_step:init_step(StepSpec),
    StepsState = beamai_process_step:collect_input(step_a, input, data, #{step_a => StepState0}),
    #{step_a := StepState} = StepsState,
    ?assert(beamai_process_step:check_activation(StepState, StepSpec)).

check_activation_missing_inputs_test() ->
    StepSpec = make_step_spec(step_a, test_steps, #{type => passthrough,
                                                   required_inputs => [a, b]}),
    {ok, StepState0} = beamai_process_step:init_step(StepSpec),
    StepsState = beamai_process_step:collect_input(step_a, a, 1, #{step_a => StepState0}),
    #{step_a := StepState} = StepsState,
    ?assertNot(beamai_process_step:check_activation(StepState, StepSpec)).

%%====================================================================
%% Execution tests
%%====================================================================

execute_passthrough_test() ->
    StepSpec = make_step_spec(step_a, test_steps, #{type => passthrough,
                                                   output_event => done}),
    {ok, StepState} = beamai_process_step:init_step(StepSpec),
    Inputs = #{input => <<"hello">>},
    Context = beamai_context:new(),
    {events, Events, NewState} = beamai_process_step:execute(StepState, Inputs, Context),
    ?assertEqual(1, length(Events)),
    [Event] = Events,
    ?assertEqual(done, maps:get(name, Event)),
    ?assertEqual(step_a, maps:get(source, Event)),
    ?assertEqual(1, maps:get(activation_count, NewState)).

execute_pause_test() ->
    StepSpec = make_step_spec(step_a, test_steps, #{type => pause}),
    {ok, StepState} = beamai_process_step:init_step(StepSpec),
    Inputs = #{input => trigger},
    Context = beamai_context:new(),
    {pause, Reason, _NewState} = beamai_process_step:execute(StepState, Inputs, Context),
    ?assertEqual(awaiting_human_input, Reason).

%%====================================================================
%% Clear inputs tests
%%====================================================================

clear_inputs_test() ->
    StepSpec = make_step_spec(step_a, test_steps, #{type => passthrough}),
    {ok, StepState0} = beamai_process_step:init_step(StepSpec),
    StepsState = beamai_process_step:collect_input(step_a, input, data, #{step_a => StepState0}),
    #{step_a := StepState} = StepsState,
    Cleared = beamai_process_step:clear_inputs(StepState),
    ?assertEqual(#{}, maps:get(collected_inputs, Cleared)).

%%====================================================================
%% Helpers
%%====================================================================

make_step_spec(Id, Module, Config) ->
    #{
        '__step_spec__' => true,
        id => Id,
        module => Module,
        config => Config,
        required_inputs => maps:get(required_inputs, Config, [input])
    }.
