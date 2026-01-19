%%%-------------------------------------------------------------------
%%% @doc LLM Call Node Module
%%%
%%% Creates graph nodes for LLM interaction.
%%% Delegates core logic to beamai_llm_core for shared implementation.
%%%
%%% Responsibilities:
%%%   - Build LLM requests
%%%   - Process LLM responses
%%%   - Trigger callbacks
%%%   - Build assistant messages
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_node).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Node creation API
-export([create/1, create/2]).

%% Internal functions (for beamai_nodes delegation)
-export([build_llm_opts/3]).
-export([process_llm_response/4]).
-export([build_assistant_message/2]).

%%====================================================================
%% Node Creation API
%%====================================================================

%% @doc Create LLM call node (default config keys)
%%
%% Creates a graph node function that executes LLM calls.
%% Uses default state keys: tools, messages, system_prompt.
%%
%% @param LLMConfig LLM configuration (provider, model, etc.)
%% @returns Node function fun(State) -> {ok, NewState} | {error, Reason}
-spec create(map()) -> fun((map()) -> {ok, map()} | {error, term()}).
create(LLMConfig) ->
    create(LLMConfig, #{}).

%% @doc Create LLM call node (with options)
%%
%% Options:
%%   - tools_key: Key for tools list in state (default: tools)
%%   - messages_key: Key for messages list in state (default: messages)
%%   - system_prompt_key: Key for system prompt in state (default: system_prompt)
%%
%% @param LLMConfig LLM configuration
%% @param Opts Node options
%% @returns Node function
-spec create(map(), map()) -> fun((map()) -> {ok, map()} | {error, term()}).
create(LLMConfig, Opts) ->
    %% Extract config keys
    ToolsKey = maps:get(tools_key, Opts, tools),
    MsgsKey = maps:get(messages_key, Opts, messages),
    PromptKey = maps:get(system_prompt_key, Opts, system_prompt),

    fun(State) ->
        %% Get data from state
        Messages = graph:get(State, MsgsKey, []),
        SystemPrompt = graph:get(State, PromptKey, <<>>),
        Tools = graph:get(State, ToolsKey, []),

        %% Build complete messages with system prompt
        AllMsgs = beamai_message:prepend_system(SystemPrompt, Messages),

        %% Invoke on_llm_start callback
        ?INVOKE_CALLBACK_FROM_STATE(on_llm_start, [AllMsgs], State),

        %% Build LLM options and execute call
        LLMOpts = build_llm_opts(Tools, LLMConfig, State),
        execute_llm_call(LLMConfig, AllMsgs, LLMOpts, Messages, State, MsgsKey)
    end.

%%====================================================================
%% Internal Functions (also used by beamai_nodes)
%%====================================================================

%% @doc Build LLM call options
%%
%% Delegates to beamai_agent_utils.
%%
%% @param Tools Tool definitions
%% @param LLMConfig LLM configuration
%% @param State Graph state
%% @returns LLM options map
-spec build_llm_opts([map()], map(), map()) -> map().
build_llm_opts(Tools, LLMConfig, State) ->
    beamai_llm_core:build_llm_opts(Tools, LLMConfig, State).

%% @doc Process LLM response
%%
%% Delegates to beamai_llm_core for shared implementation.
%%
%% @param Response LLM response
%% @param Messages Original messages
%% @param State Graph state
%% @param MsgsKey Messages key in state
%% @returns {ok, NewState}
-spec process_llm_response(map(), [map()], map(), atom()) -> {ok, map()}.
process_llm_response(Response, Messages, State, MsgsKey) ->
    beamai_llm_core:process_response(Response, Messages, State, MsgsKey).

%% @doc Build assistant message
%%
%% Delegates to beamai_llm_core.
%%
%% @param Content Message content
%% @param ToolCalls Tool calls list
%% @returns Assistant message map
-spec build_assistant_message(binary() | null, [map()]) -> map().
build_assistant_message(Content, ToolCalls) ->
    beamai_llm_core:build_assistant_msg(Content, ToolCalls).

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Execute LLM call and process result
-spec execute_llm_call(map(), [map()], map(), [map()], map(), atom()) ->
    {ok, map()}.
execute_llm_call(LLMConfig, AllMsgs, LLMOpts, Messages, State, MsgsKey) ->
    case llm_client:chat(LLMConfig, AllMsgs, LLMOpts) of
        {ok, Response} ->
            %% Success: invoke callback and process response
            ?INVOKE_CALLBACK_FROM_STATE(on_llm_end, [Response], State),
            process_llm_response(Response, Messages, State, MsgsKey);

        {error, Reason} ->
            %% Failure: invoke error callback and set error state
            ?INVOKE_CALLBACK_FROM_STATE(on_llm_error, [Reason], State),
            ErrorState = beamai_state_helpers:set_error(State, Reason),
            {ok, ErrorState}
    end.
