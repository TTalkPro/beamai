%%%-------------------------------------------------------------------
%%% @doc Graph Execution Module
%%%
%%% Handles agent graph execution:
%%% - Graph building (via node registry)
%%% - Graph execution
%%% - Routing decisions
%%% - Interrupt handling
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_runner).

-include("beamai_agent.hrl").

%% API exports
-export([
    build_graph/1,
    execute/3,
    rebuild_graph/1
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Build agent execution graph
%%
%% Creates a compiled graph for graph:run/2.
%% Uses node registry for configurable pipeline.
%%
%% @param Opts Graph options
%% @returns {ok, CompiledGraph} | {error, Reason}
-spec build_graph(map()) -> {ok, map()} | {error, term()}.
build_graph(Opts) ->
    MaxIterations = maps:get(max_iterations, Opts, 10),

    %% Build nodes using registry
    Nodes = beamai_node_registry:build_pipeline(Opts),

    %% Build graph from nodes
    build_graph_from_nodes(Nodes, MaxIterations).

%% @doc Execute graph
%%
%% Executes compiled graph and returns result.
%% Appends new messages to history, preserves full conversation.
%%
%% Message compression is handled by middleware_summarization in
%% the before_model hook.
%%
%% @param Msg User message
%% @param Opts Execution options
%% @param State Agent state record
%% @returns {ok, Result, NewState} | {error, Reason, NewState}
-spec execute(binary(), map(), #state{}) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
execute(Msg, Opts, #state{graph = Graph, system_prompt = Prompt,
                           tools = Tools, max_iterations = MaxIter,
                           messages = HistoryMsgs,
                           full_messages = HistoryFullMsgs,
                           context = Context,
                           callbacks = Callbacks, run_id = RunId,
                           id = AgentId, name = AgentName} = State) ->
    %% Build callback metadata
    CallbackMeta = #{
        agent_id => AgentId,
        agent_name => AgentName,
        run_id => RunId,
        timestamp => erlang:system_time(millisecond)
    },

    %% Append new user message to history
    NewUserMsg = #{role => user, content => Msg},
    AllMessages = HistoryMsgs ++ [NewUserMsg],
    AllFullMessages = HistoryFullMsgs ++ [NewUserMsg],

    %% Build context with original input
    ContextWithInput = Context#{
        original_input => Msg,
        last_user_message => Msg
    },

    %% Build initial graph state
    InitState = graph:state(#{
        messages => AllMessages,
        full_messages => AllFullMessages,
        system_prompt => Prompt,
        tools => [to_tool_spec(Tool) || Tool <- Tools],
        max_iterations => MaxIter,
        iteration => 0,
        scratchpad => [],
        context => ContextWithInput,
        callbacks => beamai_agent_callbacks:to_map(Callbacks),
        callback_meta => CallbackMeta
    }),

    %% Build run options with run_id
    RunOptions = build_run_options(Opts, RunId),

    %% Execute graph (Pregel engine)
    handle_graph_result(graph:run(Graph, InitState, RunOptions), State).

%% @doc Rebuild graph from current state
%%
%% Used after tool/config changes.
%%
%% @param State Current agent state
%% @returns {ok, NewState} | {error, Reason}
-spec rebuild_graph(#state{}) -> {ok, #state{}} | {error, term()}.
rebuild_graph(#state{tools = Tools, system_prompt = Prompt,
                     llm_config = LLMConfig, max_iterations = MaxIter,
                     middlewares = Middlewares, response_format = RF} = State) ->
    Opts = #{
        tools => Tools,
        system_prompt => Prompt,
        llm => LLMConfig,
        max_iterations => MaxIter,
        middlewares => Middlewares,
        response_format => RF
    },
    case build_graph(Opts) of
        {ok, NewGraph} ->
            {ok, State#state{graph = NewGraph}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal Functions - Run Options
%%====================================================================

%% @private Build run options for graph execution
%%
%% Includes run_id and any checkpoint-related options from Opts.
-spec build_run_options(map(), binary() | undefined) -> map().
build_run_options(Opts, RunId) ->
    %% Extract checkpoint-related options
    BaseOpts = maps:with([on_checkpoint, restore_from], Opts),
    %% Add run_id if present
    case RunId of
        undefined -> BaseOpts;
        _ -> BaseOpts#{run_id => RunId}
    end.

%%====================================================================
%% Internal Functions - Graph Building
%%====================================================================

%% @private Build graph from nodes map
-spec build_graph_from_nodes(map(), pos_integer()) -> {ok, map()} | {error, term()}.
build_graph_from_nodes(Nodes, MaxIterations) ->
    %% Create builder
    Builder0 = graph:builder(#{
        max_iterations => MaxIterations * 2
    }),

    %% Add all nodes
    Builder1 = add_all_nodes(Builder0, Nodes),

    %% Add edges
    Builder2 = add_all_edges(Builder1),

    %% Set entry and compile
    Builder3 = graph:set_entry(Builder2, llm_call),
    graph:compile(Builder3).

%% @private Add all nodes to builder
-spec add_all_nodes(map(), map()) -> map().
add_all_nodes(Builder, Nodes) ->
    NodeList = [
        {llm_call, maps:get(llm_call, Nodes)},
        {record_llm, maps:get(record_llm, Nodes)},
        {validate_response, maps:get(validate_response, Nodes)},
        {check_before_tools, maps:get(check_before_tools, Nodes)},
        {execute_tools, maps:get(execute_tools, Nodes)},
        {record_tools, maps:get(record_tools, Nodes)},
        {check_after_tools, maps:get(check_after_tools, Nodes)},
        {increment_iter, maps:get(increment_iter, Nodes)}
    ],
    lists:foldl(
        fun({Name, Node}, Acc) -> graph:add_node(Acc, Name, Node) end,
        Builder,
        NodeList
    ).

%% @private Add all edges to builder
-spec add_all_edges(map()) -> map().
add_all_edges(Builder) ->
    %% Sequential edges
    Builder1 = graph:add_edge(Builder, llm_call, record_llm),
    Builder2 = graph:add_edge(Builder1, record_llm, validate_response),
    Builder3 = graph:add_edge(Builder2, execute_tools, record_tools),
    Builder4 = graph:add_edge(Builder3, record_tools, check_after_tools),
    Builder5 = graph:add_edge(Builder4, increment_iter, llm_call),

    %% Conditional edges
    Builder6 = graph:add_conditional_edge(Builder5, validate_response, fun route_after_llm/1),
    Builder7 = graph:add_conditional_edge(Builder6, check_before_tools, fun route_before_tools/1),
    graph:add_conditional_edge(Builder7, check_after_tools, fun route_after_tools/1).

%%====================================================================
%% Internal Functions - Routing
%%====================================================================

%% @private Route after LLM call
-spec route_after_llm(map()) -> atom().
route_after_llm(State) ->
    case beamai_nodes:route_after_llm(State) of
        '__end__' -> '__end__';
        execute_tools -> check_before_tools
    end.

%% @private Route after check_before_tools
%%
%% Handles routing after the pre-tool checkpoint:
%% - rejected: end execution (user rejected the action)
%% - normal flow: continue to execute_tools
%%
%% Note: interrupt is now handled at pregel level, not by routing to '__end__'
-spec route_before_tools(map()) -> atom().
route_before_tools(State) ->
    Rejected = graph:get(State, rejected, false),
    case Rejected of
        true -> '__end__';
        false -> execute_tools
    end.

%% @private Route after check_after_tools
%%
%% Handles routing after the post-tool checkpoint:
%% - rejected: end execution
%% - normal flow: continue to increment_iter (next LLM call)
%%
%% Note: interrupt is now handled at pregel level, not by routing to '__end__'
-spec route_after_tools(map()) -> atom().
route_after_tools(State) ->
    Rejected = graph:get(State, rejected, false),
    case Rejected of
        true -> '__end__';
        false -> increment_iter
    end.

%%====================================================================
%% Internal Functions - Result Handling
%%====================================================================

%% @private Handle graph execution result
%%
%% Saves messages, full_messages, and scratchpad to state.
-spec handle_graph_result(map(), #state{}) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
handle_graph_result(#{status := completed, final_state := FinalState}, State) ->
    Result = build_result(FinalState),
    NewState = extract_and_update_state(FinalState, State),
    {ok, Result, NewState};

handle_graph_result(#{status := max_iterations, final_state := FinalState}, State) ->
    Result = build_result(FinalState),
    NewState = extract_and_update_state(FinalState, State),
    {ok, Result#{warning => max_iterations_reached}, NewState};

%% Handle stopped status (interrupt at pregel level)
handle_graph_result(#{status := stopped, final_state := FinalState,
                      checkpoint := Checkpoint, checkpoint_type := Type}, State) ->
    Result = build_result(FinalState),
    NewState = extract_and_update_state(FinalState, State),
    %% Return with interrupt info for the caller to handle
    {ok, Result#{
        status => interrupted,
        checkpoint => Checkpoint,
        checkpoint_type => Type,
        pending_action => graph:get(FinalState, pending_action, undefined),
        interrupt_point => graph:get(FinalState, interrupt_point, undefined)
    }, NewState};

handle_graph_result(#{status := error, error := Reason, final_state := FinalState}, State) ->
    NewState = extract_and_update_state(FinalState, State),
    {error, Reason, NewState};

handle_graph_result(#{status := error, final_state := FinalState}, State) ->
    NewState = extract_and_update_state(FinalState, State),
    {error, unknown_error, NewState};

handle_graph_result(UnexpectedResult, State) ->
    logger:warning("Unexpected graph result: ~p", [UnexpectedResult]),
    {error, {unexpected_graph_result, UnexpectedResult}, State}.

%% @private Extract data from graph state and update agent state
-spec extract_and_update_state(map(), #state{}) -> #state{}.
extract_and_update_state(FinalState, State) ->
    Messages = graph:get(FinalState, messages, []),
    FullMessages = graph:get(FinalState, full_messages, []),
    Scratchpad = graph:get(FinalState, scratchpad, []),
    Context = graph:get(FinalState, context, #{}),
    State#state{
        messages = Messages,
        full_messages = FullMessages,
        scratchpad = Scratchpad,
        context = Context
    }.

%% @private Build result from graph state
-spec build_result(map()) -> map().
build_result(FinalState) ->
    Messages = graph:get(FinalState, messages, []),
    FinalResponse = beamai_nodes:extract_final_response(Messages),
    FinishReason = graph:get(FinalState, finish_reason, <<"stop">>),
    ValidatedContent = graph:get(FinalState, validated_content, undefined),

    Result = #{
        status => completed,
        messages => Messages,
        final_response => FinalResponse,
        finish_reason => FinishReason,
        iterations => graph:get(FinalState, iteration, 0)
    },

    maybe_add_validated_content(Result, ValidatedContent).

%% @private Add validated content if present
-spec maybe_add_validated_content(map(), term()) -> map().
maybe_add_validated_content(Result, undefined) -> Result;
maybe_add_validated_content(Result, ValidatedContent) ->
    Result#{validated_content => ValidatedContent}.

%%====================================================================
%% Internal Functions - Helpers
%%====================================================================

%% @private Convert tool definition to spec (remove handler)
-spec to_tool_spec(map()) -> map().
to_tool_spec(Tool) ->
    case maps:is_key(handler, Tool) of
        true -> maps:remove(handler, Tool);
        false -> Tool
    end.
