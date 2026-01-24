%%%-------------------------------------------------------------------
%%% @doc Test helper step modules for process framework tests
%%% @end
%%%-------------------------------------------------------------------
-module(test_steps).

-behaviour(beamai_step_behaviour).

%% Step behaviour callbacks
-export([init/1, can_activate/2, on_activate/3, on_resume/3]).

%%====================================================================
%% Passthrough step - emits output event with input data
%%====================================================================

init(#{type := passthrough} = Config) ->
    OutputEvent = maps:get(output_event, Config, output),
    {ok, #{type => passthrough, output_event => OutputEvent}};

%%====================================================================
%% Accumulator step - accumulates inputs, emits when count reached
%%====================================================================

init(#{type := accumulator} = Config) ->
    Target = maps:get(target_count, Config, 2),
    OutputEvent = maps:get(output_event, Config, accumulated),
    {ok, #{type => accumulator, items => [], target_count => Target,
           output_event => OutputEvent}};

%%====================================================================
%% Pause step - pauses on first activation, resumes with data
%%====================================================================

init(#{type := pause} = _Config) ->
    {ok, #{type => pause, resumed => false, resume_data => undefined}};

%%====================================================================
%% Counter step - counts activations, emits count
%%====================================================================

init(#{type := counter} = Config) ->
    MaxCount = maps:get(max_count, Config, 3),
    OutputEvent = maps:get(output_event, Config, count_output),
    LoopEvent = maps:get(loop_event, Config, undefined),
    {ok, #{type => counter, count => 0, max_count => MaxCount,
           output_event => OutputEvent, loop_event => LoopEvent}};

%%====================================================================
%% Fanout step - emits multiple events
%%====================================================================

init(#{type := fanout} = Config) ->
    Events = maps:get(events, Config, []),
    {ok, #{type => fanout, events => Events}};

%%====================================================================
%% Default
%%====================================================================

init(Config) ->
    OutputEvent = maps:get(output_event, Config, output),
    {ok, #{type => passthrough, output_event => OutputEvent}}.

%%====================================================================
%% can_activate - always true if we have inputs
%%====================================================================

can_activate(_Inputs, _State) ->
    true.

%%====================================================================
%% on_activate
%%====================================================================

on_activate(Inputs, #{type := passthrough, output_event := OutputEvent} = State, _Context) ->
    Event = beamai_process_event:new(OutputEvent, Inputs),
    {ok, #{events => [Event], state => State}};

on_activate(Inputs, #{type := accumulator, items := Items, target_count := Target,
                      output_event := OutputEvent} = State, _Context) ->
    NewItems = Items ++ [Inputs],
    case length(NewItems) >= Target of
        true ->
            Event = beamai_process_event:new(OutputEvent, #{items => NewItems}),
            {ok, #{events => [Event], state => State#{items => []}}};
        false ->
            {ok, #{events => [], state => State#{items => NewItems}}}
    end;

on_activate(_Inputs, #{type := pause, resumed := false} = State, _Context) ->
    {pause, awaiting_human_input, State};

on_activate(Inputs, #{type := pause, resumed := true} = State, _Context) ->
    Event = beamai_process_event:new(resumed_output, #{
        inputs => Inputs,
        resume_data => maps:get(resume_data, State)
    }),
    {ok, #{events => [Event], state => State}};

on_activate(_Inputs, #{type := counter, count := Count, max_count := MaxCount,
                       output_event := OutputEvent, loop_event := LoopEvent} = State,
            _Context) ->
    NewCount = Count + 1,
    NewState = State#{count => NewCount},
    case NewCount >= MaxCount of
        true ->
            Event = beamai_process_event:new(OutputEvent, #{count => NewCount}),
            {ok, #{events => [Event], state => NewState}};
        false ->
            case LoopEvent of
                undefined ->
                    {ok, #{events => [], state => NewState}};
                _ ->
                    Event = beamai_process_event:new(LoopEvent, #{count => NewCount}),
                    {ok, #{events => [Event], state => NewState}}
            end
    end;

on_activate(_Inputs, #{type := fanout, events := EventDefs} = State, _Context) ->
    Events = lists:map(
        fun({Name, Data}) -> beamai_process_event:new(Name, Data) end,
        EventDefs
    ),
    {ok, #{events => Events, state => State}}.

%%====================================================================
%% on_resume (for pause step)
%%====================================================================

on_resume(Data, #{type := pause} = State, _Context) ->
    NewState = State#{resumed => true, resume_data => Data},
    {ok, #{events => [], state => NewState}}.
