%%%-------------------------------------------------------------------
%%% @doc Shared tool execution core module
%%%
%%% Provides unified tool execution logic used by both:
%%% - beamai_tool_node (standard tool execution)
%%% - beamai_middleware_nodes (middleware-wrapped tool execution)
%%%
%%% This eliminates ~100 lines of duplicated code between the two modules.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_core).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Execution context record (replaces 5+ parameter functions)
-record(tool_ctx, {
    handlers  :: #{binary() => function()},
    context   :: map(),
    state     :: map()
}).

%% API
-export([
    execute_calls/4,
    execute_single/3,
    safe_execute/3,
    call_handler/3,
    process_result/1
]).

%% Tool info extraction
-export([
    extract_tool_info/1
]).

%% Callback helpers
-export([
    invoke_tool_callbacks/4
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Execute a list of tool calls
%%
%% Iterates through tool calls, executing each with the provided handlers.
%% Accumulates results and context updates.
%%
%% @param ToolCalls List of tool call maps
%% @param Handlers Map of tool name to handler function
%% @param Context Current execution context
%% @param State Graph state (for callbacks)
%% @returns {Results, ContextUpdates}
-spec execute_calls([map()], #{binary() => function()}, map(), map()) ->
    {[{ok, binary()} | {error, term()}], map()}.
execute_calls(ToolCalls, Handlers, Context, State) ->
    Ctx = #tool_ctx{handlers = Handlers, context = Context, state = State},
    lists:foldl(fun(ToolCall, {Results, CtxAcc}) ->
        UpdatedCtx = Ctx#tool_ctx{context = CtxAcc},
        {Result, NewCtx} = execute_single(ToolCall, UpdatedCtx, true),
        {Results ++ [Result], maps:merge(CtxAcc, NewCtx)}
    end, {[], Context}, ToolCalls).

%% @doc Execute a single tool call
%%
%% @param ToolCall The tool call map
%% @param Ctx Tool execution context record
%% @param InvokeCallbacks Whether to invoke callbacks
%% @returns {Result, ContextUpdates}
-spec execute_single(map(), #tool_ctx{}, boolean()) ->
    {{ok, binary()} | {error, term()}, map()}.
execute_single(ToolCall, #tool_ctx{handlers = Handlers, context = Context,
                                    state = State}, InvokeCallbacks) ->
    {Name, Args} = extract_tool_info(ToolCall),

    %% Invoke start callback if enabled
    case InvokeCallbacks of
        true -> invoke_callback(on_tool_start, [Name, Args], State);
        false -> ok
    end,

    %% Execute tool
    case maps:get(Name, Handlers, undefined) of
        undefined ->
            Error = {unknown_tool, Name},
            case InvokeCallbacks of
                true -> invoke_callback(on_tool_error, [Name, Error], State);
                false -> ok
            end,
            {{error, Error}, #{}};
        Handler ->
            {Result, CtxUpdates} = safe_execute(Handler, Args, Context),
            case InvokeCallbacks of
                true -> invoke_tool_callbacks(Name, Result, State, tool_result);
                false -> ok
            end,
            {Result, CtxUpdates}
    end.

%% @doc Safely execute a tool handler
%%
%% Wraps tool execution with try/catch to handle exceptions.
%% Normalizes return values to standard format.
%%
%% @param Handler The tool handler function
%% @param Args Tool arguments
%% @param Context Execution context
%% @returns {Result, ContextUpdates}
-spec safe_execute(function(), map(), map()) ->
    {{ok, binary()} | {error, term()}, map()}.
safe_execute(Handler, Args, Context) ->
    try
        RawResult = call_handler(Handler, Args, Context),
        process_result(RawResult)
    catch
        Class:Reason:_Stack ->
            {{error, {Class, Reason}}, #{}}
    end.

%% @doc Call handler based on its arity
%%
%% Supports two handler signatures:
%%   - Handler(Args) -> Result
%%   - Handler(Args, Context) -> Result
%%
%% @param Handler The handler function
%% @param Args Tool arguments
%% @param Context Execution context
%% @returns Handler result
-spec call_handler(function(), map(), map()) -> term().
call_handler(Handler, Args, Context) ->
    case erlang:fun_info(Handler, arity) of
        {arity, 1} -> Handler(Args);
        {arity, 2} -> Handler(Args, Context);
        _ -> Handler(Args)
    end.

%% @doc Process tool execution result
%%
%% Normalizes various return formats to standard {Result, ContextUpdates}.
%%
%% Supported formats:
%%   - {Result, CtxUpdates} when is_map(CtxUpdates) -> {{ok, Binary}, CtxUpdates}
%%   - {ok, Result, CtxUpdates} -> {{ok, Binary}, CtxUpdates}
%%   - {ok, Result} -> {{ok, Binary}, #{}}
%%   - {error, Reason} -> {{error, Reason}, #{}}
%%   - Result -> {{ok, Binary}, #{}}
%%
%% @param Result Raw tool result
%% @returns {StandardizedResult, ContextUpdates}
-spec process_result(term()) -> {{ok, binary()} | {error, term()}, map()}.
process_result({Result, CtxUpdates}) when is_map(CtxUpdates) ->
    {{ok, beamai_utils:to_binary(Result)}, CtxUpdates};
process_result({ok, Result, CtxUpdates}) when is_map(CtxUpdates) ->
    {{ok, beamai_utils:to_binary(Result)}, CtxUpdates};
process_result({ok, Result}) ->
    {{ok, beamai_utils:to_binary(Result)}, #{}};
process_result({error, Reason}) ->
    {{error, Reason}, #{}};
process_result(Result) ->
    {{ok, beamai_utils:to_binary(Result)}, #{}}.

%%====================================================================
%% Tool Info Extraction
%%====================================================================

%% @doc Extract tool name and arguments from tool call
%%
%% Handles both OpenAI-style and direct format tool calls.
%%
%% @param ToolCall The tool call map
%% @returns {Name, Args}
-spec extract_tool_info(map()) -> {binary(), map()}.
extract_tool_info(ToolCall) ->
    beamai_agent_utils:extract_tool_info(ToolCall).

%%====================================================================
%% Callback Helpers
%%====================================================================

%% @doc Invoke tool result callbacks
%%
%% @param Name Tool name
%% @param Result Tool execution result
%% @param State Graph state
%% @param Type Callback type (tool_result for standard callbacks)
-spec invoke_tool_callbacks(binary(), {ok, binary()} | {error, term()}, map(), atom()) -> ok.
invoke_tool_callbacks(Name, {ok, Result}, State, _Type) ->
    invoke_callback(on_tool_end, [Name, Result], State);
invoke_tool_callbacks(Name, {error, Reason}, State, _Type) ->
    invoke_callback(on_tool_error, [Name, Reason], State).

%% @private Invoke callback helper
-spec invoke_callback(atom(), list(), map()) -> ok.
invoke_callback(CallbackName, Args, State) ->
    Callbacks = graph:get(State, callbacks, #{}),
    Meta = graph:get(State, callback_meta, #{}),
    beamai_callback_utils:invoke(CallbackName, Args, Callbacks, Meta).
