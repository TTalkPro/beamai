%%%-------------------------------------------------------------------
%%% @doc Agent lifecycle telemetry 埋点测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_telemetry_tests).

-include_lib("eunit/include/eunit.hrl").

telemetry_test_() ->
    {foreach, fun reset/0, fun(_) -> reset() end, [
        fun successful_run_is_paired/0,
        fun interrupt_and_resume_are_linked/0,
        fun state_change_has_safe_payload/0
    ]}.

successful_run_is_paired() ->
    install_collector(self()),
    {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}}}),
    {ok, _, _} = beamai_agent:run(Agent, <<"private question">>),
    ?assertEqual(
       [[beamai, agent, run, started],
        [beamai, agent, turn, started],
        [beamai, agent, run, finished],
        [beamai, agent, turn, finished]],
       agent_event_names()).

interrupt_and_resume_are_linked() ->
    install_collector(self()),
    meck:new(beamai_chat_model, [passthrough]),
    CC = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null,
                        tool_calls => [#{id => <<"call_1">>, type => <<"function">>,
                                         function => #{name => <<"do_it">>,
                                                       arguments => <<"{}">>}}],
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"ok">>, finish_reason => <<"stop">>}}
        end
    end),
    try
        K0 = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                               beamai_chat_model:create(mock, #{})),
        K = beamai_chat_client:add_tool(
              K0, #{name => <<"do_it">>, parameters => #{},
                    handler => fun(_A, _C) -> {ok, <<"done">>} end}),
        {ok, Agent} = beamai_agent:new(#{
            chat_client => K,
            callbacks => #{on_tool_call => fun(_N, _A) -> {interrupt, approval} end}
        }),
        {interrupt, _, Agent1} = beamai_agent:run(Agent, <<"go">>),
        {ok, _, _} = beamai_agent:resume(Agent1, <<"approved">>),
        Names = agent_event_names(),
        ?assertEqual(true, lists:member([beamai, agent, interrupted], Names)),
        ?assertEqual(true, lists:member([beamai, agent, resumed], Names))
    after
        meck:unload(beamai_chat_model)
    end.

state_change_has_safe_payload() ->
    install_collector(self()),
    meck:new(beamai_chat_model, [passthrough]),
    CC = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null,
                        tool_calls => [#{id => <<"call_state">>, type => <<"function">>,
                                         function => #{name => <<"save_it">>,
                                                       arguments => <<"{}">>}}],
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"saved">>, finish_reason => <<"stop">>}}
        end
    end),
    try
        K0 = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                               beamai_chat_model:create(mock, #{})),
        K = beamai_chat_client:add_tool(
              K0, #{name => <<"save_it">>, parameters => #{},
                    handler => fun(_A, _C) ->
                                       {ok, <<"done">>, #{<<"notes">> => <<"value">>}}
                               end}),
        {ok, Agent} = beamai_agent:new(#{
            chat_client => K,
            initial_state => #{<<"notes">> => <<>>}
        }),
        {ok, _, _} = beamai_agent:run(Agent, <<"go">>),
        State = wait([beamai, agent, state, changed]),
        ?assertEqual([<<"notes">>], maps:get(state_keys, maps:get(data, State))),
        ?assertNot(maps:is_key(state, maps:get(data, State)))
    after
        meck:unload(beamai_chat_model)
    end.

agent_event_names() ->
    agent_event_names([]).

agent_event_names(Acc) ->
    receive
        {telemetry, #{name := [beamai, agent | _] = Name}} ->
            agent_event_names(Acc ++ [Name]);
        {telemetry, _} ->
            agent_event_names(Acc)
    after 50 ->
        Acc
    end.

wait(Name) ->
    receive
        {telemetry, #{name := Name} = Event} -> Event;
        {telemetry, _} -> wait(Name)
    after 1000 -> erlang:error({telemetry_timeout, Name})
    end.

install_collector(Self) ->
    ok = beamai_telemetry:add_handler(
           test, fun(Event) -> Self ! {telemetry, Event} end).

reset() ->
    application:unset_env(beamai_core, telemetry).
