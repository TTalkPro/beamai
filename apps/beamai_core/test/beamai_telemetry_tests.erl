%%%-------------------------------------------------------------------
%%% @doc telemetry dispatch 基础设施测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_telemetry_tests).

-include_lib("eunit/include/eunit.hrl").

telemetry_test_() ->
    {foreach, fun reset/0, fun(_) -> reset() end, [
        fun dispatch_common_event_shape/0,
        fun disabled_config_is_noop/0,
        fun level_can_be_turned_off/0,
        fun handler_crash_is_isolated/0,
        fun handler_can_be_removed/0,
        fun default_logger_is_safe/0
    ]}.

dispatch_common_event_shape() ->
    Self = self(),
    ok = beamai_telemetry:add_handler(test, fun(Event) -> Self ! {event, Event} end),
    ok = beamai_telemetry:dispatch([beamai, tool, started], #{tool => <<"t">>},
                                   #{run_id => <<"run-1">>}),
    #{name := Name, level := info, timestamp := Ts, data := Data,
      metadata := Metadata} = receive_event(),
    ?assertEqual([beamai, tool, started], Name),
    ?assert(is_integer(Ts)),
    ?assertEqual(#{tool => <<"t">>}, Data),
    ?assertEqual(#{run_id => <<"run-1">>}, Metadata).

disabled_config_is_noop() ->
    ok = beamai_telemetry:set_config(#{enabled => false}),
    ok = beamai_telemetry:add_handler(test, fun(_) -> erlang:error(must_not_run) end),
    ?assertEqual(ok, beamai_telemetry:dispatch([beamai, test], #{}, #{})).

level_can_be_turned_off() ->
    ok = beamai_telemetry:set_config(#{enabled_levels => [info, warning]}),
    Self = self(),
    ok = beamai_telemetry:add_handler(test, fun(Event) -> Self ! {event, Event} end),
    ok = beamai_telemetry:dispatch(debug, [beamai, high, frequency], #{}, #{}),
    ok = beamai_telemetry:dispatch(warning, [beamai, important], #{}, #{}),
    #{level := warning, name := [beamai, important]} = receive_event(),
    ?assertEqual(timeout, receive_event()).

handler_crash_is_isolated() ->
    Self = self(),
    ok = beamai_telemetry:add_handler(crashing, fun(_) -> erlang:error(boom) end),
    ok = beamai_telemetry:add_handler(alive, fun(Event) -> Self ! {event, Event} end),
    ?assertEqual(ok, beamai_telemetry:dispatch([beamai, crash], #{}, #{run_id => r})),
    ?assertMatch(#{metadata := #{run_id := r}}, receive_event()).

handler_can_be_removed() ->
    ok = beamai_telemetry:add_handler(test, fun(_) -> erlang:error(must_not_run) end),
    ok = beamai_telemetry:remove_handler(test),
    ?assertEqual(ok, beamai_telemetry:dispatch([beamai], #{}, #{})).

default_logger_is_safe() ->
    ok = beamai_telemetry:add_handler(logger, fun beamai_telemetry:default_logger/1),
    ?assertEqual(
       ok,
       beamai_telemetry:dispatch(
         [beamai, llm, finished],
         #{secret => <<"do-not-log">>},
         #{run_id => <<"run">>, api_key => <<"do-not-log">>})).

receive_event() ->
    receive
        {event, Event} -> Event
    after 250 -> timeout
    end.

reset() ->
    application:unset_env(beamai_core, telemetry).
