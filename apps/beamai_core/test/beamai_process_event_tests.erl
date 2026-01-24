-module(beamai_process_event_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Event creation tests
%%====================================================================

new_event_test() ->
    E = beamai_process_event:new(my_event, #{key => value}),
    ?assert(maps:get('__process_event__', E)),
    ?assertEqual(my_event, maps:get(name, E)),
    ?assertEqual(public, maps:get(type, E)),
    ?assertEqual(undefined, maps:get(source, E)),
    ?assertEqual(#{key => value}, maps:get(data, E)),
    ?assert(is_binary(maps:get(id, E))),
    ?assert(is_integer(maps:get(timestamp, E))).

new_event_with_opts_test() ->
    E = beamai_process_event:new(ev, data, #{type => internal, source => step_a}),
    ?assertEqual(internal, maps:get(type, E)),
    ?assertEqual(step_a, maps:get(source, E)).

error_event_test() ->
    E = beamai_process_event:error_event(my_step, timeout),
    ?assertEqual(error, maps:get(name, E)),
    ?assertEqual(error, maps:get(type, E)),
    ?assertEqual(my_step, maps:get(source, E)),
    #{reason := timeout, source := my_step} = maps:get(data, E).

system_event_test() ->
    E = beamai_process_event:system_event(tick, #{}),
    ?assertEqual(system, maps:get(type, E)),
    ?assertEqual(tick, maps:get(name, E)).

%%====================================================================
%% Binding tests
%%====================================================================

binding_test() ->
    B = beamai_process_event:binding(event_a, step_b, input_x),
    ?assert(maps:get('__event_binding__', B)),
    ?assertEqual(event_a, maps:get(event_name, B)),
    ?assertEqual(step_b, maps:get(target_step, B)),
    ?assertEqual(input_x, maps:get(target_input, B)),
    ?assertEqual(undefined, maps:get(transform, B)).

binding_with_transform_test() ->
    T = fun(X) -> X * 2 end,
    B = beamai_process_event:binding(ev, step, inp, T),
    ?assertEqual(T, maps:get(transform, B)).

%%====================================================================
%% Routing tests
%%====================================================================

route_single_test() ->
    Event = beamai_process_event:new(output, <<"hello">>),
    Bindings = [beamai_process_event:binding(output, step_b, input)],
    Result = beamai_process_event:route(Event, Bindings),
    ?assertEqual([{step_b, input, <<"hello">>}], Result).

route_no_match_test() ->
    Event = beamai_process_event:new(other_event, data),
    Bindings = [beamai_process_event:binding(output, step_b, input)],
    Result = beamai_process_event:route(Event, Bindings),
    ?assertEqual([], Result).

route_fanout_test() ->
    Event = beamai_process_event:new(trigger, #{x => 1}),
    Bindings = [
        beamai_process_event:binding(trigger, step_a, input),
        beamai_process_event:binding(trigger, step_b, input),
        beamai_process_event:binding(other, step_c, input)
    ],
    Result = beamai_process_event:route(Event, Bindings),
    ?assertEqual(2, length(Result)),
    ?assert(lists:member({step_a, input, #{x => 1}}, Result)),
    ?assert(lists:member({step_b, input, #{x => 1}}, Result)).

route_with_transform_test() ->
    Event = beamai_process_event:new(num_event, 5),
    Transform = fun(X) -> X * 10 end,
    Bindings = [beamai_process_event:binding(num_event, step_a, value, Transform)],
    Result = beamai_process_event:route(Event, Bindings),
    ?assertEqual([{step_a, value, 50}], Result).
