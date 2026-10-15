%%%-------------------------------------------------------------------
%%% @doc ChatModel telemetry 埋点测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_chat_model_telemetry_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MSGS, [#{role => user, content => <<"private prompt">>}]).

telemetry_test_() ->
    {foreach, fun reset/0, fun(_) -> reset() end, [
        fun chat_emits_started_retry_finished/0,
        fun chat_error_has_error_class/0,
        fun stream_emits_started_finished/0,
        fun event_payload_has_no_prompt/0
    ]}.

chat_emits_started_retry_finished() ->
    Self = self(),
    install_collector(Self),
    {Config, _} = flaky(2),
    ?assertMatch({ok, _}, beamai_chat_model:chat(Config, ?MSGS, #{retry_delay => 1})),
    ?assertEqual(
       [[beamai, llm, request, started],
        [beamai, llm, retry],
        [beamai, llm, retry],
        [beamai, llm, request, finished]],
       event_names(4, [])).

chat_error_has_error_class() ->
    Self = self(),
    install_collector(Self),
    {Config, _} = flaky(1, #{error => {http_error, 401, <<"private body">>},
                             max_retries => 0}),
    ?assertMatch({error, _}, beamai_chat_model:chat(Config, ?MSGS)),
    #{data := Data} = wait_event([beamai, llm, request, finished]),
    ?assertEqual(error, maps:get(status, Data)),
    ?assertEqual(auth, maps:get(error_type, Data)),
    ?assertEqual(401, maps:get(http_status, Data)).

stream_emits_started_finished() ->
    Self = self(),
    install_collector(Self),
    Ctr = counters:new(1, []),
    Config = beamai_chat_model:create(
               {custom, beamai_flaky_provider},
               #{attempts => Ctr, events => [#{<<"delta">> => #{<<"text">> => <<"x">>}}]}),
    {ok, _} = beamai_chat_model:stream_chat(Config, ?MSGS, fun(_) -> ok end),
    ?assertEqual(
       [[beamai, llm, stream, started], [beamai, llm, stream, finished]],
       event_names(2, [])).

event_payload_has_no_prompt() ->
    Self = self(),
    install_collector(Self),
    {Config, _} = flaky(0),
    {ok, _} = beamai_chat_model:chat(Config, ?MSGS),
    Started = wait_event([beamai, llm, request, started]),
    _ = wait_event([beamai, llm, request, finished]),
    ?assertEqual(#{message_count => 1}, maps:get(data, Started)),
    ?assertNot(maps:is_key(messages, maps:get(data, Started))),
    ?assertNot(maps:is_key(messages, maps:get(metadata, Started))),
    ?assertEqual(<<"flaky-model">>, maps:get(model, maps:get(metadata, Started))).

flaky(FailN) -> flaky(FailN, #{}).

flaky(FailN, Extra) ->
    Ctr = counters:new(1, []),
    Config = beamai_chat_model:create(
               {custom, beamai_flaky_provider},
               maps:merge(#{attempts => Ctr, fail_n => FailN, retry_delay => 1},
                          Extra)),
    {Config, Ctr}.

install_collector(Self) ->
    ok = beamai_telemetry:add_handler(
           test, fun(Event) -> Self ! {telemetry, Event} end).

event_names(0, Acc) -> lists:reverse(Acc);
event_names(N, Acc) ->
    receive
        {telemetry, #{name := Name}} -> event_names(N - 1, [Name | Acc])
    after 500 -> {timeout, lists:reverse(Acc)}
    end.

wait_event(Name) ->
    receive
        {telemetry, #{name := Name} = Event} -> Event;
        {telemetry, _} -> wait_event(Name)
    after 500 -> erlang:error({telemetry_timeout, Name})
    end.

reset() ->
    application:unset_env(beamai_core, telemetry).
