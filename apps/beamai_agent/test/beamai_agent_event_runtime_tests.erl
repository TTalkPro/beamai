%%%-------------------------------------------------------------------
%%% @doc Runtime integration tests for unified Agent events; all offline.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_event_runtime_tests).

-include_lib("eunit/include/eunit.hrl").

successful_run_has_monotonic_sequence_test() ->
    Collector = collector(),
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{}},
        event_consumers => [crashing_consumer(), collector_consumer(Collector)]
    }),
    {ok, _, _} = beamai_agent:run(Agent, <<"private question">>),
    Events = collect(Collector),
    Types = [maps:get(type, Event) || Event <- Events],
    ?assertEqual([run_started, turn_started, chat_started, chat_finished,
                  run_finished, turn_finished], Types),
    ?assertEqual([1, 2, 3, 4, 5, 6],
                 [maps:get(sequence, Event) || Event <- Events]),
    RunIds = lists:usort([maps:get(run_id, Event) || Event <- Events]),
    ?assertMatch([_RunId], RunIds),
    lists:foreach(
      fun(Event) ->
              ?assertNot(maps:is_key(prompt, maps:get(data, Event)))
      end, Events).

tool_events_are_paired_and_safe_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    Calls = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_Config, _Messages, _Opts) ->
        counters:add(Calls, 1, 1),
        case counters:get(Calls, 1) of
            1 -> tool_response(<<"call_event">>, <<"event_tool">>);
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    try
        Collector = collector(),
        K0 = beamai_chat_client:add_chat_model(
               beamai_chat_client:new(), beamai_chat_model:create(mock, #{})),
        K = beamai_chat_client:add_tool(
              K0, #{name => <<"event_tool">>, parameters => #{},
                    handler => fun(_Args, _Ctx) -> {ok, <<"private result">>} end}),
        {ok, Agent} = beamai_agent:new(#{
            chat_client => K,
            event_consumers => collector_consumer(Collector)
        }),
        {ok, _, _} = beamai_agent:run(Agent, <<"use event_tool">>),
        Events = collect(Collector),
        ByType = event_index(Events),
        ?assertMatch([#{data := #{tool := <<"event_tool">>},
                        metadata := #{tool_call_id := <<"call_event">>}}],
                     maps:get(tool_queued, ByType)),
        ?assertMatch([#{data := #{tool := <<"event_tool">>},
                        metadata := #{tool_call_id := <<"call_event">>}}],
                     maps:get(tool_started, ByType)),
        ?assertMatch([#{data := #{tool := <<"event_tool">>, status := ok},
                        metadata := #{tool_call_id := <<"call_event">>}}],
                     maps:get(tool_finished, ByType)),
        lists:foreach(
          fun(Event) ->
              Data = maps:get(data, Event),
              ?assertNot(maps:is_key(args, Data)),
              ?assertNot(maps:is_key(result, Data))
          end,
          maps:get(tool_queued, ByType) ++
          maps:get(tool_started, ByType) ++ maps:get(tool_finished, ByType))
    after
        meck:unload(beamai_chat_model)
    end.

sequence_resets_for_new_run_test() ->
    Collector = collector(),
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{}},
        event_consumers => collector_consumer(Collector)
    }),
    {ok, _, Agent1} = beamai_agent:run(Agent, <<"one">>),
    _ = collect(Collector),
    {ok, _, _} = beamai_agent:run(Agent1, <<"two">>),
    Events = collect(Collector),
    RunStarts = [Event || Event <- Events,
                          run_started =:= maps:get(type, Event)],
    ?assertEqual([1], [maps:get(sequence, Event) || Event <- RunStarts]),
    ?assertMatch([#{type := run_started}, _ | _], Events).

%%====================================================================
%% Helpers
%%====================================================================

collector() -> self().

collector_consumer(Collector) ->
    fun(Event) -> Collector ! {event, Event} end.

crashing_consumer() ->
    fun(_Event) -> throw(event_consumer_crash) end.

collect(Collector) ->
    collect(Collector, []).

collect(Collector, Acc) ->
    receive
        {event, Event} -> collect(Collector, Acc ++ [Event])
    after 100 ->
        lists:sort(fun(A, B) ->
                           maps:get(sequence, A) =< maps:get(sequence, B)
                   end, Acc)
    end.

event_index(Events) ->
    lists:foldl(
      fun(Event, Acc) ->
              Type = maps:get(type, Event),
              maps:update_with(Type, fun(List) -> List ++ [Event] end, [Event], Acc)
      end, #{}, Events).

tool_response(Id, Name) ->
    {ok, #{content => null,
           tool_calls => [#{id => Id, type => <<"function">>,
                            function => #{name => Name,
                                          arguments => <<"{\"secret\":1}">>}}],
           finish_reason => <<"tool_calls">>}}.
