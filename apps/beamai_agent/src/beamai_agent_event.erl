%%%-------------------------------------------------------------------
%%% @doc Unified Agent event model.
%%%
%%% The model is a pure data boundary between Agent runtime and observers:
%%% UI streams, audit sinks and telemetry adapters consume the same events
%%% without becoming part of Agent control flow. Runtime integration assigns
%%% a monotonically increasing `sequence' for each `run_id'; consumers sort
%%% by that value when parallel tool completion changes arrival order.
%%%
%%% Event payloads are intentionally conservative and must not contain
%%% prompts, complete tool results, API keys or URLs.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_event).

-export([new/4, sort/1, publish/2, to_telemetry/1, telemetry_name/1]).

-export_type([event_type/0, event/0, consumer/0, telemetry_event/0]).

-type event_type() ::
    run_started | run_finished |
    turn_started | turn_finished |
    chat_started | chat_finished |
    tool_queued | tool_started | tool_finished |
    interrupt_requested | approval_requested |
    resumed | state_changed |
    error_occurred.

-type event() :: #{
    type := event_type(),
    sequence := pos_integer(),
    timestamp := integer(),
    agent_id := term(),
    run_id := binary() | undefined,
    conversation_id := binary(),
    data := map(),
    metadata := map()
}.

-type consumer() :: fun((event()) -> term()).

-type telemetry_event() :: #{
    name := beamai_telemetry:event_name(),
    level := beamai_telemetry:level(),
    data := map(),
    metadata := map()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Create a normalized event.
%%
%% `Metadata' may carry callback metadata and event-specific correlation IDs.
%% The three primary correlation fields are promoted to top-level fields and
%% removed from the remaining metadata to avoid two sources of truth. An
%% explicit `timestamp' in metadata is preserved; otherwise wall-clock time
%% is captured here.
-spec new(event_type(), map(), map(), pos_integer()) -> event().
new(Type, Metadata, Data, Sequence)
  when is_atom(Type), is_map(Metadata), is_map(Data),
       is_integer(Sequence), Sequence > 0 ->
    Event = #{
        type => Type,
        sequence => Sequence,
        timestamp => maps:get(timestamp, Metadata, os:system_time(millisecond)),
        agent_id => maps:get(agent_id, Metadata, undefined),
        run_id => maps:get(run_id, Metadata, undefined),
        conversation_id => maps:get(conversation_id, Metadata, <<>>),
        data => Data,
        metadata => maps:without([agent_id, run_id, conversation_id, timestamp],
                                 Metadata)
    },
    Event.

%% @doc Sort events deterministically within and across runs.
%%
%% The primary run grouping is `run_id'; `sequence' then defines the order
%% inside a run. Timestamp is only a final tie-breaker for compatible model
%% versions that explicitly reuse a sequence.
-spec sort([event()]) -> [event()].
sort(Events) ->
    lists:sort(fun order/2, Events).

%% @doc Publish one event to one consumer or a list of consumers.
%%
%% Consumer return values are ignored. A crashing consumer cannot affect other
%% consumers or Agent execution.
-spec publish(event(), consumer() | [consumer()]) -> ok.
publish(Event, Consumer) when is_function(Consumer, 1) ->
    publish(Event, [Consumer]);
publish(Event, Consumers) when is_list(Consumers) ->
    lists:foreach(fun(Consumer) -> run_consumer(Consumer, Event) end, Consumers).

%% @doc Adapt a unified event to the `beamai_telemetry' event shape.
%%
%% The returned map contains the name/data/metadata arguments for
%% `beamai_telemetry:dispatch/3'; telemetry supplies the outer dispatch
%% timestamp.
-spec to_telemetry(event()) -> telemetry_event().
to_telemetry(Event) ->
    #{
        name => telemetry_name(event_type(Event)),
        level => telemetry_level(event_type(Event)),
        data => maps:get(data, Event),
        metadata => telemetry_metadata(Event)
    }.

%% @doc Map a unified event type to its telemetry event name.
-spec telemetry_name(event_type()) -> beamai_telemetry:event_name().
telemetry_name(run_started) -> [beamai, agent, run, started];
telemetry_name(run_finished) -> [beamai, agent, run, finished];
telemetry_name(turn_started) -> [beamai, agent, turn, started];
telemetry_name(turn_finished) -> [beamai, agent, turn, finished];
telemetry_name(chat_started) -> [beamai, agent, chat, started];
telemetry_name(chat_finished) -> [beamai, agent, chat, finished];
telemetry_name(tool_queued) -> [beamai, tool, queued];
telemetry_name(tool_started) -> [beamai, tool, started];
telemetry_name(tool_finished) -> [beamai, tool, finished];
telemetry_name(interrupt_requested) -> [beamai, agent, interrupted];
telemetry_name(approval_requested) -> [beamai, agent, approval, requested];
telemetry_name(resumed) -> [beamai, agent, resumed];
telemetry_name(state_changed) -> [beamai, agent, state, changed];
telemetry_name(error_occurred) -> [beamai, agent, error, occurred].

%%====================================================================
%% Internal functions
%%====================================================================

event_type(Event) -> maps:get(type, Event).

-spec order(event(), event()) -> boolean().
order(Left, Right) ->
    {maps:get(run_id, Left), maps:get(sequence, Left),
     maps:get(timestamp, Left)}
    =<
    {maps:get(run_id, Right), maps:get(sequence, Right),
     maps:get(timestamp, Right)}.

-spec telemetry_metadata(event()) -> map().
telemetry_metadata(Event) ->
    maps:merge(maps:get(metadata, Event), #{
        agent_id => maps:get(agent_id, Event),
        run_id => maps:get(run_id, Event),
        conversation_id => maps:get(conversation_id, Event),
        sequence => maps:get(sequence, Event),
        event_timestamp => maps:get(timestamp, Event)
    }).

-spec telemetry_level(event_type()) -> beamai_telemetry:level().
telemetry_level(error_occurred) -> error;
telemetry_level(interrupt_requested) -> warning;
telemetry_level(_Observation) -> info.

-spec run_consumer(consumer(), event()) -> ok.
run_consumer(Consumer, Event) when is_function(Consumer, 1) ->
    try Consumer(Event)
    catch Class:Reason:Stack ->
        logger:warning(
          "beamai agent event consumer crashed class=~p reason=~p "
          "event=~ts stack_depth=~p",
          [Class, Reason,
           event_name_text(beamai_agent_event:telemetry_name(event_type(Event))),
           length(Stack)])
    end;
run_consumer(Consumer, _Event) ->
    logger:warning("beamai agent event ignored invalid consumer=~0p",
                   [Consumer]).

-spec event_name_text(beamai_telemetry:event_name()) -> binary().
event_name_text(Name) ->
    iolist_to_binary(lists:join(".", [atom_to_list(Part) || Part <- Name])).
