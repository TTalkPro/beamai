%%%-------------------------------------------------------------------
%%% @doc Offline tests for the unified Agent event model.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_event_tests).

-include_lib("eunit/include/eunit.hrl").

metadata() ->
    #{agent_id => <<"agent-1">>,
      run_id => <<"run-1">>,
      conversation_id => <<"conv-1">>,
      request_id => <<"req-1">>,
      turn_count => 2,
      timestamp => 123}.

new_normalizes_common_fields_test() ->
    Event = beamai_agent_event:new(tool_finished, metadata(),
                                   #{status => complete, duration_ms => 4}, 9),
    ?assertEqual(tool_finished, maps:get(type, Event)),
    ?assertEqual(9, maps:get(sequence, Event)),
    ?assertEqual(123, maps:get(timestamp, Event)),
    ?assertEqual(<<"agent-1">>, maps:get(agent_id, Event)),
    ?assertEqual(<<"run-1">>, maps:get(run_id, Event)),
    ?assertEqual(<<"conv-1">>, maps:get(conversation_id, Event)),
    ?assertEqual(#{status => complete, duration_ms => 4}, maps:get(data, Event)),
    ?assertEqual(#{request_id => <<"req-1">>, turn_count => 2},
                 maps:get(metadata, Event)).

sort_groups_by_run_then_sequence_test() ->
    Run1Event2 = beamai_agent_event:new(turn_started, metadata(), #{}, 2),
    Run1Event1 = beamai_agent_event:new(run_started, metadata(), #{}, 1),
    BaseMetadata = metadata(),
    Run2Metadata = BaseMetadata#{run_id => <<"run-2">>},
    Run2Event1 = beamai_agent_event:new(run_started, Run2Metadata, #{}, 1),
    Sorted = beamai_agent_event:sort([Run1Event2, Run2Event1, Run1Event1]),
    ?assertEqual([<<"run-1">>, <<"run-1">>, <<"run-2">>],
                 [maps:get(run_id, Event) || Event <- Sorted]),
    ?assertEqual([1, 2, 1],
                 [maps:get(sequence, Event) || Event <- Sorted]).

telemetry_adapter_preserves_correlation_test() ->
    Event = beamai_agent_event:new(interrupt_requested, metadata(),
                                   #{interrupt_type => tool_request}, 7),
    Telemetry = beamai_agent_event:to_telemetry(Event),
    ?assertEqual([beamai, agent, interrupted], maps:get(name, Telemetry)),
    ?assertEqual(warning, maps:get(level, Telemetry)),
    ?assertEqual(#{interrupt_type => tool_request}, maps:get(data, Telemetry)),
    ?assertEqual(7, maps:get(sequence, maps:get(metadata, Telemetry))),
    ?assertEqual(<<"req-1">>, maps:get(request_id, maps:get(metadata, Telemetry))).

all_event_types_have_telemetry_names_test() ->
    Types = [run_started, run_finished, turn_started, turn_finished,
             chat_started, chat_finished, tool_queued, tool_started,
             tool_finished, interrupt_requested, approval_requested,
             resumed, state_changed, error_occurred],
    ?assertEqual(length(Types), length(lists:usort(Types))),
    lists:foreach(fun(Type) ->
                          Name = beamai_agent_event:telemetry_name(Type),
                          ?assertMatch([beamai | _], Name)
                  end, Types).

publish_isolates_consumer_crashes_test() ->
    Event = beamai_agent_event:new(state_changed, metadata(), #{changed => [k]}, 1),
    Good = fun(Received) -> erlang:send(self(), {event, Received}) end,
    Bad = fun(_Event) -> throw(consumer_crash) end,
    ?assertEqual(ok, beamai_agent_event:publish(Event, [Bad, Good])),
    receive
        {event, Event} -> ok
    after 1000 ->
        ct:fail("event consumer was not invoked")
    end.
