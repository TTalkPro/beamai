%%%-------------------------------------------------------------------
%%% @doc Graph-based Agent Facade Module
%%%
%%% This module serves as the API facade for the agent system.
%%% Actual implementation is delegated to specialized modules:
%%%
%%% - beamai_agent_server: gen_server implementation (stateful)
%%% - beamai_agent_api: Pure function API (stateless)
%%% - beamai_agent_init: Initialization logic
%%% - beamai_agent_runner: Graph execution logic
%%% - beamai_agent_callbacks: Callback management
%%% - beamai_agent_checkpoint: Checkpoint persistence
%%%
%%% == Usage Modes ==
%%%
%%% === Pure Function Mode (Recommended) ===
%%% For single conversations, high concurrency, stateless services.
%%% Main API: run_once/2, create_state/1, run_with_state/3
%%%
%%% === Process Mode (Advanced) ===
%%% For long-running services, supervision trees, auto state management.
%%% Main API: start_link/2, run/2, save_checkpoint/1
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent).

-behaviour(gen_server).

-include("beamai_agent.hrl").

%% Process API (delegates to beamai_agent_server)
-export([start_link/2, stop/1, run/2, run/3]).
-export([add_tool/2, remove_tool/2, set_system_prompt/2]).

%% Pure function API (delegates to beamai_agent_api)
-export([
    run_once/2,
    run_with_state/3,
    create_state/1,
    create_state/2,
    export_state/1,
    import_state/2
]).

%% Scratchpad API
-export([get_scratchpad/1, clear_scratchpad/1]).

%% Messages API
-export([get_messages/1, clear_messages/1]).

%% Context API
-export([get_context/1, get_context/2, get_context/3]).
-export([set_context/2, update_context/2, put_context/3]).

%% Meta API (进程级元数据，不参与对话)
-export([get_meta/1, get_meta/2, get_meta/3]).
-export([set_meta/2, put_meta/3]).

%% Checkpoint API
-export([save_checkpoint/1, save_checkpoint/2]).
-export([load_checkpoint/2, load_latest_checkpoint/1]).
-export([list_checkpoints/1, list_checkpoints/2]).
-export([restore_from_checkpoint/2]).

%% Interrupt control API
-export([resume/2, abort/1]).

%% Callback API
-export([set_callbacks/2, get_callbacks/1]).
-export([emit_custom_event/3, emit_custom_event/4]).

%% Middleware API
-export([get_middlewares/1, add_middleware/2, remove_middleware/2]).
-export([set_middlewares/2]).

%% Graph building API
-export([build_graph/1]).

%% Coordinator API
-export([start_pipeline/2, start_orchestrator/2]).

%% gen_server callbacks (for backward compatibility)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%====================================================================
%% Process API (delegates to beamai_agent_server)
%%====================================================================

%% @doc Start Graph Agent
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Id, Opts) ->
    gen_server:start_link(?MODULE, {Id, Opts}, []).

%% @doc Stop Agent
-spec stop(pid()) -> ok.
stop(Pid) ->
    beamai_agent_server:stop(Pid).

%% @doc Run conversation
-spec run(pid(), binary()) -> {ok, map()} | {error, term()}.
run(Pid, Msg) ->
    run(Pid, Msg, #{}).

%% @doc Run conversation with options
-spec run(pid(), binary(), map()) -> {ok, map()} | {error, term()}.
run(Pid, Msg, Opts) ->
    Timeout = maps:get(timeout, Opts, 300000),
    gen_server:call(Pid, {run, Msg, Opts}, Timeout).

%% @doc Add tool to Agent
-spec add_tool(pid(), map()) -> ok | {error, term()}.
add_tool(Pid, Tool) ->
    gen_server:call(Pid, {tool, add, Tool}).

%% @doc Remove tool from Agent
-spec remove_tool(pid(), binary()) -> ok | {error, term()}.
remove_tool(Pid, Name) ->
    gen_server:call(Pid, {tool, remove, Name}).

%% @doc Set system prompt
-spec set_system_prompt(pid(), binary()) -> ok | {error, term()}.
set_system_prompt(Pid, Prompt) ->
    gen_server:call(Pid, {config, set_prompt, Prompt}).

%%====================================================================
%% Scratchpad API
%%====================================================================

%% @doc Get scratchpad steps
-spec get_scratchpad(pid()) -> [map()].
get_scratchpad(Pid) ->
    gen_server:call(Pid, {state, get_scratchpad}).

%% @doc Clear scratchpad
-spec clear_scratchpad(pid()) -> ok.
clear_scratchpad(Pid) ->
    gen_server:call(Pid, {state, clear_scratchpad}).

%%====================================================================
%% Messages API
%%====================================================================

%% @doc Get conversation history
-spec get_messages(pid()) -> [map()].
get_messages(Pid) ->
    gen_server:call(Pid, {state, get_messages}).

%% @doc Clear conversation history
-spec clear_messages(pid()) -> ok.
clear_messages(Pid) ->
    gen_server:call(Pid, {state, clear_messages}).

%%====================================================================
%% Context API
%%====================================================================

%% @doc Get full context
-spec get_context(pid()) -> map().
get_context(Pid) ->
    gen_server:call(Pid, {context, get}).

%% @doc Get context value
-spec get_context(pid(), atom() | binary()) -> term() | undefined.
get_context(Pid, Key) ->
    get_context(Pid, Key, undefined).

%% @doc Get context value with default
-spec get_context(pid(), atom() | binary(), term()) -> term().
get_context(Pid, Key, Default) ->
    Context = gen_server:call(Pid, {context, get}),
    maps:get(Key, Context, Default).

%% @doc Set entire context
-spec set_context(pid(), map()) -> ok.
set_context(Pid, Context) when is_map(Context) ->
    gen_server:call(Pid, {context, set, Context}).

%% @doc Update context (merge)
-spec update_context(pid(), map()) -> ok.
update_context(Pid, Updates) when is_map(Updates) ->
    gen_server:call(Pid, {context, update, Updates}).

%% @doc Set single context value
-spec put_context(pid(), atom() | binary(), term()) -> ok.
put_context(Pid, Key, Value) ->
    gen_server:call(Pid, {context, put, Key, Value}).

%%====================================================================
%% Meta API (进程级元数据，不参与对话)
%%====================================================================

%% @doc Get full meta
-spec get_meta(pid()) -> map().
get_meta(Pid) ->
    gen_server:call(Pid, {meta, get}).

%% @doc Get meta value
-spec get_meta(pid(), atom() | binary()) -> term() | undefined.
get_meta(Pid, Key) ->
    get_meta(Pid, Key, undefined).

%% @doc Get meta value with default
-spec get_meta(pid(), atom() | binary(), term()) -> term().
get_meta(Pid, Key, Default) ->
    Meta = gen_server:call(Pid, {meta, get}),
    maps:get(Key, Meta, Default).

%% @doc Set entire meta
-spec set_meta(pid(), map()) -> ok.
set_meta(Pid, Meta) when is_map(Meta) ->
    gen_server:call(Pid, {meta, set, Meta}).

%% @doc Set single meta value
-spec put_meta(pid(), atom() | binary(), term()) -> ok.
put_meta(Pid, Key, Value) ->
    gen_server:call(Pid, {meta, put, Key, Value}).

%%====================================================================
%% Checkpoint API
%%====================================================================

%% @doc Save checkpoint
-spec save_checkpoint(pid()) -> {ok, binary()} | {error, term()}.
save_checkpoint(Pid) ->
    save_checkpoint(Pid, #{}).

%% @doc Save checkpoint with metadata
-spec save_checkpoint(pid(), map()) -> {ok, binary()} | {error, term()}.
save_checkpoint(Pid, Meta) ->
    gen_server:call(Pid, {checkpoint, save, Meta}).

%% @doc Load checkpoint
-spec load_checkpoint(pid(), binary()) -> {ok, map()} | {error, term()}.
load_checkpoint(Pid, CheckpointId) ->
    gen_server:call(Pid, {checkpoint, load, CheckpointId}).

%% @doc Load latest checkpoint
-spec load_latest_checkpoint(pid()) -> {ok, map()} | {error, term()}.
load_latest_checkpoint(Pid) ->
    gen_server:call(Pid, {checkpoint, load_latest}).

%% @doc List checkpoints
-spec list_checkpoints(pid()) -> {ok, [map()]} | {error, term()}.
list_checkpoints(Pid) ->
    list_checkpoints(Pid, #{}).

%% @doc List checkpoints with options
-spec list_checkpoints(pid(), map()) -> {ok, [map()]} | {error, term()}.
list_checkpoints(Pid, Opts) ->
    gen_server:call(Pid, {checkpoint, list, Opts}).

%% @doc Restore from checkpoint
-spec restore_from_checkpoint(pid(), binary()) -> ok | {error, term()}.
restore_from_checkpoint(Pid, CheckpointId) ->
    gen_server:call(Pid, {checkpoint, restore, CheckpointId}).

%%====================================================================
%% Interrupt Control API
%%====================================================================

%% @doc Resume interrupted execution
-spec resume(pid(), confirm | reject | {modify, term()}) -> ok | {error, term()}.
resume(Pid, Action) ->
    gen_server:call(Pid, {interrupt, resume, Action}, 60000).

%% @doc Abort current execution
-spec abort(pid()) -> ok.
abort(Pid) ->
    gen_server:call(Pid, {interrupt, abort}).

%%====================================================================
%% Callback API
%%====================================================================

%% @doc Set callback handlers
-spec set_callbacks(pid(), map()) -> ok.
set_callbacks(Pid, CallbackOpts) ->
    gen_server:call(Pid, {callback, set, CallbackOpts}).

%% @doc Get current callback config
-spec get_callbacks(pid()) -> map().
get_callbacks(Pid) ->
    gen_server:call(Pid, {callback, get}).

%% @doc Emit custom event
-spec emit_custom_event(pid(), atom() | binary(), term()) -> ok.
emit_custom_event(Pid, EventName, Data) ->
    emit_custom_event(Pid, EventName, Data, #{}).

%% @doc Emit custom event with metadata
-spec emit_custom_event(pid(), atom() | binary(), term(), map()) -> ok.
emit_custom_event(Pid, EventName, Data, Metadata) ->
    gen_server:cast(Pid, {emit_custom_event, EventName, Data, Metadata}).

%%====================================================================
%% Middleware API
%%====================================================================

%% @doc Get current middlewares
-spec get_middlewares(pid()) -> [term()].
get_middlewares(Pid) ->
    gen_server:call(Pid, {middleware, get}).

%% @doc Add middleware
-spec add_middleware(pid(), term()) -> ok | {error, term()}.
add_middleware(Pid, MiddlewareSpec) ->
    gen_server:call(Pid, {middleware, add, MiddlewareSpec}).

%% @doc Remove middleware
-spec remove_middleware(pid(), module()) -> ok | {error, term()}.
remove_middleware(Pid, Module) ->
    gen_server:call(Pid, {middleware, remove, Module}).

%% @doc Set all middlewares
-spec set_middlewares(pid(), [term()]) -> ok | {error, term()}.
set_middlewares(Pid, Middlewares) ->
    gen_server:call(Pid, {middleware, set, Middlewares}).

%%====================================================================
%% Graph Building API
%%====================================================================

%% @doc Build Agent execution graph
-spec build_graph(map()) -> {ok, map()} | {error, term()}.
build_graph(Opts) ->
    beamai_agent_runner:build_graph(Opts).

%%====================================================================
%% Pure Function API (delegates to beamai_agent_api)
%%====================================================================

%% @doc Single execution (no state, recommended for single conversations)
-spec run_once(map(), binary()) -> {ok, map()} | {error, term()}.
run_once(Config, Message) ->
    beamai_agent_api:run_once(Config, Message).

%% @doc Execute with state (multi-turn conversations)
-spec run_with_state(#state{}, binary(), map()) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
run_with_state(State, Message, Opts) ->
    beamai_agent_api:run_with_state(State, Message, Opts).

%% @doc Create Agent state (auto-generated ID)
-spec create_state(map()) -> {ok, #state{}} | {error, term()}.
create_state(Config) ->
    beamai_agent_api:create_state(Config).

%% @doc Create Agent state (specified ID)
-spec create_state(binary(), map()) -> {ok, #state{}} | {error, term()}.
create_state(Id, Config) ->
    beamai_agent_api:create_state(Id, Config).

%% @doc Export state for external persistence
-spec export_state(pid() | #state{}) -> map().
export_state(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, {state, export});
export_state(#state{} = State) ->
    beamai_agent_api:export_state(State).

%% @doc Import state
-spec import_state(pid(), map()) -> ok;
                  (map(), map()) -> {ok, #state{}} | {error, term()}.
import_state(Pid, ExportedData) when is_pid(Pid) ->
    gen_server:call(Pid, {state, import, ExportedData});
import_state(ExportedData, Config) when is_map(ExportedData), is_map(Config) ->
    beamai_agent_api:import_state(ExportedData, Config).

%%====================================================================
%% Coordinator API
%%====================================================================

%% @doc Start Pipeline coordinator
-spec start_pipeline(binary(), map()) -> {ok, pid()} | {error, term()}.
start_pipeline(Id, Opts) ->
    beamai_coordinator:start_pipeline(Id, Opts).

%% @doc Start Orchestrator coordinator
-spec start_orchestrator(binary(), map()) -> {ok, pid()} | {error, term()}.
start_orchestrator(Id, Opts) ->
    beamai_coordinator:start_orchestrator(Id, Opts).

%%====================================================================
%% gen_server Callbacks (delegates to beamai_agent_server logic)
%%====================================================================

%% @doc Initialize
init({Id, Opts}) ->
    case beamai_agent_init:create_state(Id, Opts) of
        {ok, State} -> {ok, State};
        {error, Reason} -> {stop, Reason}
    end.

%% @doc Handle synchronous calls
handle_call({run, Msg, Opts}, _From, State) ->
    handle_run(Msg, Opts, State);

handle_call({tool, add, Tool}, _From, #state{tools = Tools, tool_handlers = Handlers} = State) ->
    NewTools = [Tool | Tools],
    NewHandlers = maps:merge(Handlers, beamai_nodes:build_tool_handlers([Tool])),
    handle_rebuild(State#state{tools = NewTools, tool_handlers = NewHandlers}, State);

handle_call({tool, remove, Name}, _From, #state{tools = Tools, tool_handlers = Handlers} = State) ->
    NewTools = lists:filter(fun(#{name := ToolName}) -> ToolName =/= Name end, Tools),
    NewHandlers = maps:remove(Name, Handlers),
    handle_rebuild(State#state{tools = NewTools, tool_handlers = NewHandlers}, State);

handle_call({config, set_prompt, Prompt}, _From, State) ->
    handle_rebuild(State#state{system_prompt = Prompt}, State);

handle_call({state, get_scratchpad}, _From, #state{scratchpad = Pad} = State) ->
    {reply, lists:reverse(Pad), State};

handle_call({state, clear_scratchpad}, _From, State) ->
    {reply, ok, State#state{scratchpad = []}};

handle_call({state, get_messages}, _From, #state{messages = Msgs} = State) ->
    {reply, Msgs, State};

handle_call({state, clear_messages}, _From, State) ->
    {reply, ok, State#state{messages = []}};

handle_call({state, export}, _From, State) ->
    Exported = #{
        messages => State#state.messages,
        full_messages => State#state.full_messages,
        scratchpad => State#state.scratchpad,
        context => State#state.context
    },
    {reply, Exported, State};

handle_call({state, import, ExportedData}, _From, #state{context = CurrentCtx} = State) ->
    Messages = maps:get(messages, ExportedData, []),
    FullMessages = maps:get(full_messages, ExportedData, []),
    Scratchpad = maps:get(scratchpad, ExportedData, []),
    Context = maps:get(context, ExportedData, #{}),
    MergedCtx = maps:merge(CurrentCtx, Context),
    NewState = State#state{
        messages = Messages,
        full_messages = FullMessages,
        scratchpad = Scratchpad,
        context = MergedCtx
    },
    {reply, ok, NewState};

handle_call({context, get}, _From, #state{context = Context} = State) ->
    {reply, Context, State};

handle_call({context, set, NewContext}, _From, State) ->
    {reply, ok, State#state{context = NewContext}};

handle_call({context, update, Updates}, _From, #state{context = Context} = State) ->
    {reply, ok, State#state{context = maps:merge(Context, Updates)}};

handle_call({context, put, Key, Value}, _From, #state{context = Context} = State) ->
    {reply, ok, State#state{context = maps:put(Key, Value, Context)}};

handle_call({meta, get}, _From, #state{meta = Meta} = State) ->
    {reply, Meta, State};

handle_call({meta, set, NewMeta}, _From, State) ->
    {reply, ok, State#state{meta = NewMeta}};

handle_call({meta, put, Key, Value}, _From, #state{meta = Meta} = State) ->
    {reply, ok, State#state{meta = maps:put(Key, Value, Meta)}};

handle_call({checkpoint, save, Meta}, _From, State) ->
    {reply, beamai_agent_checkpoint:save(Meta, State), State};

handle_call({checkpoint, load, CpId}, _From, State) ->
    {reply, beamai_agent_checkpoint:load(CpId, State), State};

handle_call({checkpoint, load_latest}, _From, State) ->
    {reply, beamai_agent_checkpoint:load_latest(State), State};

handle_call({checkpoint, list, Opts}, _From, State) ->
    {reply, beamai_agent_checkpoint:list(Opts, State), State};

handle_call({checkpoint, restore, CpId}, _From, State) ->
    case beamai_agent_checkpoint:restore(CpId, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({interrupt, resume, Action}, _From, #state{pending_action = Pending} = State)
  when Pending =/= undefined ->
    NewState = State#state{pending_action = undefined},
    case Action of
        confirm -> {reply, ok, NewState};
        reject -> {reply, ok, NewState};
        {modify, _} -> {reply, ok, NewState};
        _ -> {reply, {error, invalid_action}, State}
    end;

handle_call({interrupt, resume, _}, _From, State) ->
    {reply, {error, no_pending_action}, State};

handle_call({interrupt, abort}, _From, State) ->
    {reply, ok, State#state{pending_action = undefined}};

handle_call({callback, get}, _From, #state{callbacks = Callbacks} = State) ->
    {reply, beamai_agent_callbacks:to_map(Callbacks), State};

handle_call({callback, set, CallbackOpts}, _From, #state{callbacks = Callbacks} = State) ->
    NewCallbacks = beamai_agent_callbacks:update(Callbacks, CallbackOpts),
    {reply, ok, State#state{callbacks = NewCallbacks}};

handle_call({middleware, get}, _From, #state{middlewares = Middlewares} = State) ->
    {reply, Middlewares, State};

handle_call({middleware, add, Spec}, _From, #state{middlewares = Middlewares} = State) ->
    handle_update_middlewares(Middlewares ++ [Spec], State);

handle_call({middleware, remove, Module}, _From, #state{middlewares = Middlewares} = State) ->
    NewMiddlewares = lists:filter(fun(Spec) ->
        extract_middleware_module(Spec) =/= Module
    end, Middlewares),
    handle_update_middlewares(NewMiddlewares, State);

handle_call({middleware, set, NewMiddlewares}, _From, State) ->
    handle_update_middlewares(NewMiddlewares, State);

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous messages
handle_cast({emit_custom_event, EventName, Data, Metadata}, State) ->
    FullMetadata = maps:merge(beamai_agent_callbacks:build_metadata(State), Metadata),
    beamai_agent_callbacks:invoke(on_custom_event, [EventName, Data, FullMetadata],
                                   State#state.callbacks),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle other messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate callback
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Handle run request
-spec handle_run(binary(), map(), #state{}) ->
    {reply, {ok, map()} | {error, term()}, #state{}}.
handle_run(Msg, Opts, State) ->
    RunId = beamai_agent_callbacks:generate_run_id(),
    State1 = State#state{run_id = RunId},
    Metadata = beamai_agent_callbacks:build_metadata(State1),

    beamai_agent_callbacks:invoke(on_chain_start, [Msg, Metadata], State1#state.callbacks),

    case beamai_agent_runner:execute(Msg, Opts, State1) of
        {ok, Result, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_end, [Result, Metadata],
                                          NewState#state.callbacks),
            FinalState = beamai_agent_checkpoint:maybe_auto_save(Result, NewState),
            {reply, {ok, Result}, FinalState#state{run_id = undefined}};
        {error, Reason, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_error, [Reason, Metadata],
                                          NewState#state.callbacks),
            {reply, {error, Reason}, NewState#state{run_id = undefined}}
    end.

%% @private Handle graph rebuild
-spec handle_rebuild(#state{}, #state{}) -> {reply, ok | {error, term()}, #state{}}.
handle_rebuild(NewState, OldState) ->
    case beamai_agent_runner:rebuild_graph(NewState) of
        {ok, UpdatedState} -> {reply, ok, UpdatedState};
        {error, Reason} -> {reply, {error, Reason}, OldState}
    end.

%% @private Handle middleware update
-spec handle_update_middlewares([term()], #state{}) ->
    {reply, ok | {error, term()}, #state{}}.
handle_update_middlewares(NewMiddlewares, #state{tools = Tools, system_prompt = Prompt,
                                                  llm_config = LLMConfig, max_iterations = MaxIter,
                                                  response_format = RF} = State) ->
    NewChain = case NewMiddlewares of
        [] -> undefined;
        _ -> beamai_middleware_runner:init(NewMiddlewares)
    end,

    Opts = #{
        tools => Tools,
        system_prompt => Prompt,
        llm => LLMConfig,
        max_iterations => MaxIter,
        response_format => RF,
        middlewares => NewMiddlewares
    },

    case beamai_agent_runner:build_graph(Opts) of
        {ok, NewGraph} ->
            NewState = State#state{
                middlewares = NewMiddlewares,
                middleware_chain = NewChain,
                graph = NewGraph
            },
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

%% @private Extract middleware module from spec
-spec extract_middleware_module(term()) -> module().
extract_middleware_module({Module, _Opts, _Priority}) -> Module;
extract_middleware_module({Module, _Opts}) -> Module;
extract_middleware_module(Module) when is_atom(Module) -> Module.
