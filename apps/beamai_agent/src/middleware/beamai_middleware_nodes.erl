%%%-------------------------------------------------------------------
%%% @doc Middleware-integrated Node Module
%%%
%%% Provides graph nodes with middleware hooks integrated.
%%% Uses composition with shared core modules to eliminate duplication.
%%%
%%% == Node Types ==
%%% - llm_node: LLM call with before_model/after_model hooks
%%% - tool_node: Tool execution with before_tools/after_tools hooks
%%% - agent_start_node: Agent start with before_agent hook
%%% - agent_end_node: Agent end with after_agent hook
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware_nodes).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Node creation API
-export([
    llm_node/2,
    llm_node/3,
    tool_node/2,
    agent_start_node/1,
    agent_end_node/1
]).

%% Middleware processing (public for testing)
-export([
    run_before_model/2,
    run_after_model/2,
    run_before_tools/2,
    run_after_tools/2,
    run_before_agent/2,
    run_after_agent/2
]).

%%====================================================================
%% Node Creation API
%%====================================================================

%% @doc Create middleware-wrapped LLM node
-spec llm_node(map(), list()) -> fun((map()) -> {ok, map()} | {error, term()}).
llm_node(LLMConfig, Middlewares) ->
    llm_node(LLMConfig, Middlewares, #{}).

%% @doc Create middleware-wrapped LLM node with options
%%
%% Execution flow:
%%   1. Initialize middleware chain
%%   2. Run before_model hooks
%%   3. Call LLM (using shared core)
%%   4. Run after_model hooks
-spec llm_node(map(), list(), map()) -> fun((map()) -> {ok, map()} | {error, term()}).
llm_node(LLMConfig, Middlewares, Opts) ->
    MwChain = beamai_middleware_runner:init(Middlewares),
    Keys = extract_keys(Opts),

    fun(State) ->
        State0 = graph:set(State, mw_chain, MwChain),
        handle_before_model(run_before_model(State0, MwChain),
                           State0, LLMConfig, MwChain, Keys)
    end.

%% @doc Create middleware-wrapped tool node
%%
%% Execution flow:
%%   1. Run before_tools hooks
%%   2. Execute tools (using shared core)
%%   3. Run after_tools hooks
-spec tool_node(map(), list()) -> fun((map()) -> {ok, map()}).
tool_node(ToolHandlers, Middlewares) ->
    MwChain = beamai_middleware_runner:init(Middlewares),

    fun(State) ->
        Chain = graph:get(State, mw_chain, MwChain),
        handle_before_tools(run_before_tools(State, Chain),
                           State, ToolHandlers, Chain)
    end.

%% @doc Create agent start node
-spec agent_start_node(list()) -> fun((map()) -> {ok, map()}).
agent_start_node(Middlewares) ->
    MwChain = beamai_middleware_runner:init(Middlewares),

    fun(State) ->
        State0 = graph:set(State, mw_chain, MwChain),
        handle_agent_hook(run_before_agent(State0, MwChain), State0, before_agent)
    end.

%% @doc Create agent end node
-spec agent_end_node(list()) -> fun((map()) -> {ok, map()}).
agent_end_node(Middlewares) ->
    MwChain = beamai_middleware_runner:init(Middlewares),

    fun(State) ->
        Chain = graph:get(State, mw_chain, MwChain),
        %% after_agent goto/halt typically ignored (already ending)
        case run_after_agent(State, Chain) of
            {ok, State1} -> {ok, State1};
            {_, State1} -> {ok, State1}
        end
    end.

%%====================================================================
%% Middleware Hook Runners
%%====================================================================

-spec run_before_model(map(), list()) -> beamai_middleware_runner:run_result().
run_before_model(State, MwChain) ->
    beamai_middleware_runner:run_hook(before_model, State, MwChain).

-spec run_after_model(map(), list()) -> beamai_middleware_runner:run_result().
run_after_model(State, MwChain) ->
    beamai_middleware_runner:run_hook(after_model, State, MwChain).

-spec run_before_tools(map(), list()) -> beamai_middleware_runner:run_result().
run_before_tools(State, MwChain) ->
    beamai_middleware_runner:run_hook(before_tools, State, MwChain).

-spec run_after_tools(map(), list()) -> beamai_middleware_runner:run_result().
run_after_tools(State, MwChain) ->
    beamai_middleware_runner:run_hook(after_tools, State, MwChain).

-spec run_before_agent(map(), list()) -> beamai_middleware_runner:run_result().
run_before_agent(State, MwChain) ->
    beamai_middleware_runner:run_hook(before_agent, State, MwChain).

-spec run_after_agent(map(), list()) -> beamai_middleware_runner:run_result().
run_after_agent(State, MwChain) ->
    beamai_middleware_runner:run_hook(after_agent, State, MwChain).

%%====================================================================
%% Before Model Hook Handlers
%%====================================================================

-spec handle_before_model(term(), map(), map(), list(), map()) ->
    {ok, map()} | {error, term()}.
handle_before_model({ok, State1}, _State0, LLMConfig, MwChain, Keys) ->
    execute_llm_with_hooks(State1, LLMConfig, MwChain, Keys);

handle_before_model({goto, '__end__', State1}, _State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, graph:set(State1, finish_reason, <<"middleware_skip">>)};

handle_before_model({goto, tools, State1}, _State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, graph:set(State1, mw_goto_tools, true)};

handle_before_model({halt, Reason}, State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, beamai_state_helpers:set_halt(State0, Reason, <<"middleware_halt">>)};

handle_before_model({interrupt, Action, State1}, _State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, beamai_state_helpers:set_interrupt(State1, Action, before_model)}.

%%====================================================================
%% Before Tools Hook Handlers
%%====================================================================

-spec handle_before_tools(term(), map(), map(), list()) -> {ok, map()}.
handle_before_tools({ok, State1}, _State, ToolHandlers, MwChain) ->
    execute_tools_with_hooks(State1, ToolHandlers, MwChain);

handle_before_tools({goto, '__end__', State1}, _State, _ToolHandlers, _MwChain) ->
    {ok, graph:set(State1, tool_calls, [])};

handle_before_tools({goto, model, State1}, _State, _ToolHandlers, _MwChain) ->
    {ok, graph:set(State1, mw_goto_model, true)};

handle_before_tools({halt, Reason}, State, _ToolHandlers, _MwChain) ->
    {ok, beamai_state_helpers:set_error(State, Reason)};

handle_before_tools({interrupt, Action, State1}, _State, _ToolHandlers, _MwChain) ->
    {ok, beamai_state_helpers:set_interrupt(State1, Action, before_tools)}.

%%====================================================================
%% After Model Hook Handlers
%%====================================================================

-spec handle_after_model(term(), map()) -> {ok, map()}.
handle_after_model({ok, State1}, _State) ->
    {ok, State1};

handle_after_model({goto, Target, State1}, _State) ->
    {ok, graph:set(State1, mw_goto, Target)};

handle_after_model({halt, Reason}, State) ->
    {ok, beamai_state_helpers:set_error(State, Reason)};

handle_after_model({interrupt, Action, State1}, _State) ->
    {ok, beamai_state_helpers:set_interrupt(State1, Action, after_model)}.

%%====================================================================
%% After Tools Hook Handlers
%%====================================================================

-spec handle_after_tools(term(), map()) -> {ok, map()}.
handle_after_tools({ok, State1}, _State) ->
    {ok, State1};

handle_after_tools({goto, tools, State1}, _State) ->
    {ok, graph:set(State1, mw_retry_tools, true)};

handle_after_tools({goto, Target, State1}, _State) ->
    {ok, graph:set(State1, mw_goto, Target)};

handle_after_tools({halt, Reason}, State) ->
    {ok, beamai_state_helpers:set_error(State, Reason)};

handle_after_tools({interrupt, Action, State1}, _State) ->
    {ok, beamai_state_helpers:set_interrupt(State1, Action, after_tools)}.

%%====================================================================
%% Agent Hook Handlers
%%====================================================================

-spec handle_agent_hook(term(), map(), atom()) -> {ok, map()}.
handle_agent_hook({ok, State1}, _State, _Hook) ->
    {ok, State1};

handle_agent_hook({halt, Reason}, State, _Hook) ->
    {ok, beamai_state_helpers:set_error(State, Reason)};

handle_agent_hook({interrupt, Action, State1}, _State, Hook) ->
    {ok, beamai_state_helpers:set_interrupt(State1, Action, Hook)};

handle_agent_hook({goto, _Target, State1}, _State, _Hook) ->
    %% Agent hooks ignore goto (already at start/end)
    {ok, State1}.

%%====================================================================
%% LLM Execution (using shared core)
%%====================================================================

-spec execute_llm_with_hooks(map(), map(), list(), map()) -> {ok, map()} | {error, term()}.
execute_llm_with_hooks(State, LLMConfig, MwChain, #{tools_key := ToolsKey,
                                                     msgs_key := MsgsKey,
                                                     prompt_key := PromptKey}) ->
    %% Get data from state
    Messages = graph:get(State, MsgsKey, []),
    SystemPrompt = graph:get(State, PromptKey, <<>>),
    Tools = graph:get(State, ToolsKey, []),
    AllMsgs = beamai_message:prepend_system(SystemPrompt, Messages),

    %% Invoke callbacks and build options
    invoke_callback(on_llm_start, [AllMsgs], State),
    LLMOpts = beamai_llm_core:build_llm_opts(Tools, LLMConfig, State),

    %% Execute LLM call
    case llm_client:chat(LLMConfig, AllMsgs, LLMOpts) of
        {ok, Response} ->
            invoke_callback(on_llm_end, [Response], State),
            %% Use shared core for response processing
            {ok, State1} = beamai_llm_core:process_response(Response, Messages, State, MsgsKey),
            %% Run after_model hooks
            handle_after_model(run_after_model(State1, MwChain), State1);
        {error, Reason} ->
            invoke_callback(on_llm_error, [Reason], State),
            {ok, beamai_state_helpers:set_error(State, Reason)}
    end.

%%====================================================================
%% Tool Execution (using shared core)
%%====================================================================

-spec execute_tools_with_hooks(map(), map(), list()) -> {ok, map()}.
execute_tools_with_hooks(State, ToolHandlers, MwChain) ->
    %% Get data from state
    ToolCalls = beamai_state_helpers:get_tool_calls(State),
    Messages = beamai_state_helpers:get_messages(State),
    Context = beamai_state_helpers:get_context(State),

    %% Store original calls for middleware access
    State0 = graph:set(State, mw_original_tool_calls, ToolCalls),

    %% Execute using shared core
    {Results, CtxUpdates} = beamai_tool_core:execute_calls(
        ToolCalls, ToolHandlers, Context, State0),

    %% Build tool messages and update state
    ToolMessages = beamai_agent_utils:build_tool_messages(ToolCalls, Results),
    BaseUpdates = [
        {messages, Messages ++ ToolMessages},
        {tool_results, Results},
        {tool_calls, []},
        {context, maps:merge(Context, CtxUpdates)}
    ],
    AllUpdates = beamai_state_helpers:sync_full_messages_list(
        BaseUpdates, ToolMessages, State0),
    State1 = beamai_state_helpers:set_many(State0, AllUpdates),

    %% Run after_tools hooks
    handle_after_tools(run_after_tools(State1, MwChain), State1).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Extract state keys from options
-spec extract_keys(map()) -> map().
extract_keys(Opts) ->
    #{
        tools_key => maps:get(tools_key, Opts, tools),
        msgs_key => maps:get(messages_key, Opts, messages),
        prompt_key => maps:get(system_prompt_key, Opts, system_prompt)
    }.

%% @private Invoke callback helper
-spec invoke_callback(atom(), list(), map()) -> ok.
invoke_callback(CallbackName, Args, State) ->
    Callbacks = beamai_state_helpers:get_callbacks(State),
    Meta = beamai_state_helpers:get_callback_meta(State),
    beamai_callback_utils:invoke(CallbackName, Args, Callbacks, Meta).
