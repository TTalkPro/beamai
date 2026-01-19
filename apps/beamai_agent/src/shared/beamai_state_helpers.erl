%%%-------------------------------------------------------------------
%%% @doc State helpers for common state operations
%%%
%%% Provides reusable state manipulation functions to eliminate
%%% duplication across beamai_middleware_nodes, beamai_tool_node,
%%% and beamai_llm_node modules.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_state_helpers).

-include_lib("beamai_core/include/beamai_common.hrl").

%% State setters
-export([
    set_error/2,
    set_halt/3,
    set_interrupt/3,
    set_many/2
]).

%% State getters
-export([
    get_messages/1,
    get_messages/2,
    get_tool_calls/1,
    get_context/1,
    get_callbacks/1,
    get_callback_meta/1
]).

%% Message operations
-export([
    append_message/2,
    append_messages/2,
    sync_full_messages/3,
    sync_full_messages_list/3
]).

%%====================================================================
%% State Setters
%%====================================================================

%% @doc Set error state with reason
%%
%% Sets both error and status fields for error handling.
%%
%% @param State The current graph state
%% @param Reason The error reason
%% @returns Updated state with error information
-spec set_error(map(), term()) -> map().
set_error(State, Reason) ->
    set_many(State, [{error, Reason}, {status, error}]).

%% @doc Set halt state with reason and finish reason
%%
%% Used when middleware or validation halts execution.
%%
%% @param State The current graph state
%% @param Reason The halt reason
%% @param FinishReason The finish reason for response
%% @returns Updated state with halt information
-spec set_halt(map(), term(), binary()) -> map().
set_halt(State, Reason, FinishReason) ->
    set_many(State, [
        {halted, true},
        {halt_reason, Reason},
        {finish_reason, FinishReason}
    ]).

%% @doc Set interrupt state for human-in-the-loop
%%
%% Used when execution needs user confirmation.
%%
%% @param State The current graph state
%% @param Action The pending action requiring confirmation
%% @param Point The interrupt point (before_model, before_tools, etc.)
%% @returns Updated state with interrupt information
-spec set_interrupt(map(), map(), atom()) -> map().
set_interrupt(State, Action, Point) ->
    set_many(State, [
        {interrupted, true},
        {pending_action, Action},
        {interrupt_point, Point}
    ]).

%% @doc Batch set multiple state values
%%
%% Wrapper around beamai_state_utils:set_many/2.
%%
%% @param State The current graph state
%% @param Pairs List of {Key, Value} tuples
%% @returns Updated state
-spec set_many(map(), [{atom(), term()}]) -> map().
set_many(State, Pairs) ->
    ?SET_STATE_MANY(State, Pairs).

%%====================================================================
%% State Getters
%%====================================================================

%% @doc Get messages from state with default messages key
-spec get_messages(map()) -> [map()].
get_messages(State) ->
    get_messages(State, messages).

%% @doc Get messages from state with specified key
-spec get_messages(map(), atom()) -> [map()].
get_messages(State, Key) ->
    graph:get(State, Key, []).

%% @doc Get tool calls from state
-spec get_tool_calls(map()) -> [map()].
get_tool_calls(State) ->
    graph:get(State, tool_calls, []).

%% @doc Get context from state
-spec get_context(map()) -> map().
get_context(State) ->
    graph:get(State, context, #{}).

%% @doc Get callbacks from state
-spec get_callbacks(map()) -> map().
get_callbacks(State) ->
    graph:get(State, callbacks, #{}).

%% @doc Get callback metadata from state
-spec get_callback_meta(map()) -> map().
get_callback_meta(State) ->
    graph:get(State, callback_meta, #{}).

%%====================================================================
%% Message Operations
%%====================================================================

%% @doc Append a single message to messages list
-spec append_message(map(), map()) -> [map()].
append_message(State, Message) ->
    Messages = get_messages(State),
    Messages ++ [Message].

%% @doc Append multiple messages to messages list
-spec append_messages(map(), [map()]) -> [map()].
append_messages(State, NewMessages) ->
    Messages = get_messages(State),
    Messages ++ NewMessages.

%% @doc Sync a message to full_messages if it exists
%%
%% Appends message to full_messages list for complete history tracking.
%% Returns updated pairs list for batch state update.
%%
%% @param BasePairs Base state update pairs
%% @param Message Message to append to full_messages
%% @param State Current graph state
%% @returns Updated pairs list
-spec sync_full_messages([{atom(), term()}], map(), map()) -> [{atom(), term()}].
sync_full_messages(BasePairs, Message, State) ->
    beamai_agent_utils:append_to_full_messages(BasePairs, Message, State).

%% @doc Sync multiple messages to full_messages if it exists
%%
%% @param BasePairs Base state update pairs
%% @param Messages Messages to append to full_messages
%% @param State Current graph state
%% @returns Updated pairs list
-spec sync_full_messages_list([{atom(), term()}], [map()], map()) -> [{atom(), term()}].
sync_full_messages_list(BasePairs, Messages, State) ->
    beamai_agent_utils:append_list_to_full_messages(BasePairs, Messages, State).
