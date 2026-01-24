%%%-------------------------------------------------------------------
%%% @doc Step behaviour for process framework
%%%
%%% Defines the callback interface that step modules must implement.
%%% Steps are the building blocks of a process workflow.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_step_behaviour).

-type config() :: map().
-type state() :: term().
-type inputs() :: #{atom() => term()}.
-type context() :: beamai_context:t().
-type event() :: beamai_process_event:event().

-type activate_result() ::
    {ok, #{events => [event()], state => state()}} |
    {error, term()} |
    {pause, term(), state()}.

-type resume_result() ::
    {ok, #{events => [event()], state => state()}} |
    {error, term()}.

-export_type([config/0, state/0, inputs/0, activate_result/0, resume_result/0]).

%% Required callbacks
-callback init(Config :: config()) -> {ok, state()} | {error, term()}.
-callback can_activate(Inputs :: inputs(), State :: state()) -> boolean().
-callback on_activate(Inputs :: inputs(), State :: state(), Context :: context()) ->
    activate_result().

%% Optional callbacks
-callback on_resume(Data :: term(), State :: state(), Context :: context()) ->
    resume_result().
-callback on_terminate(Reason :: term(), State :: state()) -> ok.

-optional_callbacks([on_resume/3, on_terminate/2]).
