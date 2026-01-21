%%%-------------------------------------------------------------------
%%% @doc Pure function API for agent operations
%%%
%%% Provides stateless, pure function operations without gen_server.
%%% Ideal for:
%%% - Single-turn conversations
%%% - High-concurrency scenarios
%%% - Stateless services
%%% - Testing and debugging
%%%
%%% For stateful, process-based operations, see beamai_agent_server.erl.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_api).

-include("beamai_agent.hrl").

%% State creation
-export([
    create_state/1,
    create_state/2
]).

%% Execution
-export([
    run_once/2,
    run_with_state/3
]).

%% State import/export
-export([
    export_state/1,
    import_state/2
]).

%%====================================================================
%% State Creation
%%====================================================================

%% @doc Create agent state (auto-generated ID)
%%
%% Creates state for pure function mode without storage backend.
%% Automatically generates a unique Agent ID.
%%
%% @param Config Agent configuration map
%% @returns {ok, State} | {error, Reason}
-spec create_state(map()) -> {ok, #state{}} | {error, term()}.
create_state(Config) ->
    Id = beamai_utils:gen_id(),
    create_state(Id, Config).

%% @doc Create agent state (specified ID)
%%
%% Creates state with specified ID for pure function mode.
%% Storage is disabled in pure function mode (no auto checkpoint).
%%
%% @param Id Agent ID
%% @param Config Agent configuration map
%% @returns {ok, State} | {error, Reason}
-spec create_state(binary(), map()) -> {ok, #state{}} | {error, term()}.
create_state(Id, Config) ->
    %% Force disable storage (pure function mode)
    PureConfig = Config#{
        enable_storage => false
    },
    beamai_agent_init:create_state(Id, PureConfig).

%%====================================================================
%% Execution
%%====================================================================

%% @doc Single execution (no state preservation)
%%
%% Executes a single conversation turn and returns the result.
%% For single-turn use cases without state management.
%%
%% @param Config Agent configuration
%% @param Message User message
%% @returns {ok, Result} | {error, Reason}
-spec run_once(map(), binary()) -> {ok, map()} | {error, term()}.
run_once(Config, Message) ->
    case create_state(Config) of
        {ok, State} ->
            case run_with_state(State, Message, #{}) of
                {ok, Result, _NewState} -> {ok, Result};
                {error, Reason, _NewState} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Execute with state (multi-turn support)
%%
%% Executes a conversation turn with explicit state management.
%% Returns updated state for multi-turn conversations.
%%
%% Use cases:
%% - Multi-turn conversations with context preservation
%% - Explicit state flow control
%% - State serialization and recovery
%%
%% @param State Current agent state
%% @param Message User message
%% @param Opts Execution options
%% @returns {ok, Result, NewState} | {error, Reason, NewState}
-spec run_with_state(#state{}, binary(), map()) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
run_with_state(#state{config = #agent_config{callbacks = Callbacks}} = State, Message, Opts) ->
    RunId = beamai_agent_callbacks:generate_run_id(),
    State1 = State#state{run_id = RunId},
    Metadata = beamai_agent_callbacks:build_metadata(State1),

    %% Invoke chain_start callback
    beamai_agent_callbacks:invoke(on_chain_start, [Message, Metadata], Callbacks),

    case beamai_agent_runner:execute(Message, Opts, State1) of
        {ok, Result, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_end, [Result, Metadata],
                                          NewState#state.config#agent_config.callbacks),
            {ok, Result, NewState#state{run_id = undefined}};
        {error, Reason, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_error, [Reason, Metadata],
                                          NewState#state.config#agent_config.callbacks),
            {error, Reason, NewState#state{run_id = undefined}}
    end.

%%====================================================================
%% State Import/Export
%%====================================================================

%% @doc Export state for external persistence
%%
%% Exports conversation state as serializable map for storage in
%% Redis, PostgreSQL, files, etc.
%%
%% Exported content (consistent with checkpoint):
%% - messages: Compressed messages (for LLM calls)
%% - full_messages: Complete history (for audit/debug/rollback)
%% - scratchpad: Intermediate step records
%% - context: User-defined context data
%%
%% Configuration data is NOT exported (should be passed in import_state):
%% - tools, llm_config, middlewares, system_prompt, etc.
%%
%% @param State Agent state record
%% @returns Exported state map
-spec export_state(#state{}) -> map().
export_state(#state{} = State) ->
    #{
        messages => State#state.messages,
        full_messages => State#state.full_messages,
        scratchpad => State#state.scratchpad,
        context => State#state.context
    }.

%% @doc Import state from exported data
%%
%% Restores conversation state from export_state/1 output.
%% Configuration (tools, llm, middlewares, etc.) is passed via Config.
%%
%% @param ExportedData Map from export_state/1
%% @param Config Agent configuration
%% @returns {ok, State} | {error, Reason}
-spec import_state(map(), map()) -> {ok, #state{}} | {error, term()}.
import_state(ExportedData, Config) when is_map(ExportedData), is_map(Config) ->
    Messages = maps:get(messages, ExportedData, []),
    FullMessages = maps:get(full_messages, ExportedData, []),
    Scratchpad = maps:get(scratchpad, ExportedData, []),
    Context = maps:get(context, ExportedData, #{}),

    %% Force disable storage (pure function mode)
    PureConfig = Config#{
        enable_storage => false
    },

    %% Create new state
    case create_state(PureConfig) of
        {ok, State} ->
            %% Restore conversation state (consistent with checkpoint restore)
            CurrentCtx = State#state.context,
            MergedCtx = maps:merge(CurrentCtx, Context),
            {ok, State#state{
                messages = Messages,
                full_messages = FullMessages,
                scratchpad = Scratchpad,
                context = MergedCtx
            }};
        {error, Reason} ->
            {error, Reason}
    end.
