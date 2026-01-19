%%%-------------------------------------------------------------------
%%% @doc Node Registry for Pipeline Configuration
%%%
%%% Provides configurable pipeline node definitions, allowing:
%%% - Default pipeline configuration
%%% - Custom pipeline building
%%% - Node override support
%%% - Middleware integration
%%%
%%% This module decouples pipeline configuration from hard-coded node
%%% lists in beamai_agent_runner.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_node_registry).

-export([
    default_pipeline/0,
    default_pipeline_with_middleware/0,
    build_pipeline/1,
    build_pipeline/2,
    get_node/2,
    get_node/3
]).

%% Node spec type
-type node_spec() :: {atom(), {module(), atom(), list()}} | {atom(), passthrough}.
-type pipeline() :: [node_spec()].

-export_type([node_spec/0, pipeline/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Get default pipeline configuration
%%
%% Returns the standard agent execution pipeline:
%% 1. llm_call - Call LLM
%% 2. record_llm - Record LLM response to scratchpad
%% 3. validate_response - Validate response format
%% 4. check_before_tools - Pre-tool checkpoint
%% 5. execute_tools - Execute tool calls
%% 6. record_tools - Record tool results to scratchpad
%% 7. check_after_tools - Post-tool checkpoint
%% 8. increment_iter - Increment iteration counter
%%
%% @returns Pipeline specification list
-spec default_pipeline() -> pipeline().
default_pipeline() ->
    [
        {llm_call, {beamai_nodes, llm_node, []}},
        {record_llm, {beamai_nodes, scratchpad_node, [llm_response]}},
        {validate_response, {beamai_nodes, validate_node, []}},
        {check_before_tools, passthrough},
        {execute_tools, {beamai_nodes, tool_node, []}},
        {record_tools, {beamai_nodes, scratchpad_node, [tool_result]}},
        {check_after_tools, passthrough},
        {increment_iter, {beamai_nodes, iteration_node, []}}
    ].

%% @doc Get default pipeline with middleware support
%%
%% Returns pipeline with middleware-wrapped nodes.
%%
%% @returns Pipeline specification list
-spec default_pipeline_with_middleware() -> pipeline().
default_pipeline_with_middleware() ->
    [
        {llm_call, {beamai_middleware_nodes, llm_node, []}},
        {record_llm, {beamai_nodes, scratchpad_node, [llm_response]}},
        {validate_response, {beamai_nodes, validate_node, []}},
        {check_before_tools, passthrough},
        {execute_tools, {beamai_middleware_nodes, tool_node, []}},
        {record_tools, {beamai_nodes, scratchpad_node, [tool_result]}},
        {check_after_tools, passthrough},
        {increment_iter, {beamai_nodes, iteration_node, []}}
    ].

%% @doc Build pipeline with options
%%
%% Creates pipeline nodes from configuration. Supports:
%% - Custom pipeline selection (via pipeline key)
%% - LLM configuration injection
%% - Tool handlers injection
%% - Response format validation
%% - Middleware configuration
%%
%% @param Opts Pipeline options map
%% @returns Map of node name to node function
-spec build_pipeline(map()) -> #{atom() => function()}.
build_pipeline(Opts) ->
    build_pipeline(Opts, #{}).

%% @doc Build pipeline with options and overrides
%%
%% @param Opts Pipeline options
%% @param Overrides Node override map #{node_name => custom_fun}
%% @returns Map of node name to node function
-spec build_pipeline(map(), map()) -> #{atom() => function()}.
build_pipeline(Opts, Overrides) ->
    %% Get base pipeline
    BasePipeline = select_base_pipeline(Opts),

    %% Extract configuration
    LLMConfig = maps:get(llm, Opts, #{}),
    Tools = maps:get(tools, Opts, []),
    ToolHandlers = beamai_nodes:build_tool_handlers(Tools),
    ResponseFormat = maps:get(response_format, Opts, undefined),
    Middlewares = maps:get(middlewares, Opts, []),

    %% Build context for node creation
    Context = #{
        llm_config => LLMConfig,
        tool_handlers => ToolHandlers,
        response_format => ResponseFormat,
        middlewares => Middlewares
    },

    %% Build nodes with overrides
    build_nodes_from_pipeline(BasePipeline, Context, Overrides).

%% @doc Get a specific node from pipeline config
%%
%% @param NodeName The node name to get
%% @param Opts Pipeline options
%% @returns Node function or undefined
-spec get_node(atom(), map()) -> function() | undefined.
get_node(NodeName, Opts) ->
    get_node(NodeName, Opts, undefined).

%% @doc Get a specific node with default
%%
%% @param NodeName The node name to get
%% @param Opts Pipeline options
%% @param Default Default value if not found
%% @returns Node function or default
-spec get_node(atom(), map(), function() | undefined) -> function() | undefined.
get_node(NodeName, Opts, Default) ->
    Nodes = build_pipeline(Opts),
    maps:get(NodeName, Nodes, Default).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Select base pipeline based on options
-spec select_base_pipeline(map()) -> pipeline().
select_base_pipeline(Opts) ->
    case maps:get(pipeline, Opts, undefined) of
        undefined ->
            %% Auto-select based on middlewares
            case maps:get(middlewares, Opts, []) of
                [] -> default_pipeline();
                _ -> default_pipeline_with_middleware()
            end;
        CustomPipeline when is_list(CustomPipeline) ->
            CustomPipeline;
        _ ->
            default_pipeline()
    end.

%% @private Build node functions from pipeline spec
-spec build_nodes_from_pipeline(pipeline(), map(), map()) -> #{atom() => function()}.
build_nodes_from_pipeline(Pipeline, Context, Overrides) ->
    lists:foldl(fun({NodeName, Spec}, Acc) ->
        NodeFun = case maps:get(NodeName, Overrides, undefined) of
            undefined -> create_node_from_spec(NodeName, Spec, Context);
            CustomFun -> CustomFun
        end,
        Acc#{NodeName => NodeFun}
    end, #{}, Pipeline).

%% @private Create node function from specification
-spec create_node_from_spec(atom(), node_spec() | passthrough, map()) -> function().
create_node_from_spec(_NodeName, passthrough, _Context) ->
    fun(State) -> {ok, State} end;

create_node_from_spec(llm_call, {Module, Fun, _Args}, Context) ->
    LLMConfig = maps:get(llm_config, Context),
    Middlewares = maps:get(middlewares, Context),
    case Middlewares of
        [] -> Module:Fun(LLMConfig);
        _ -> Module:Fun(LLMConfig, Middlewares)
    end;

create_node_from_spec(execute_tools, {Module, Fun, _Args}, Context) ->
    ToolHandlers = maps:get(tool_handlers, Context),
    Middlewares = maps:get(middlewares, Context),
    case Middlewares of
        [] -> Module:Fun(ToolHandlers);
        _ -> Module:Fun(ToolHandlers, Middlewares)
    end;

create_node_from_spec(validate_response, {Module, Fun, _Args}, Context) ->
    ResponseFormat = maps:get(response_format, Context),
    Module:Fun(ResponseFormat);

create_node_from_spec(record_llm, {Module, Fun, Args}, _Context) ->
    apply(Module, Fun, Args);

create_node_from_spec(record_tools, {Module, Fun, Args}, _Context) ->
    apply(Module, Fun, Args);

create_node_from_spec(increment_iter, {Module, Fun, _Args}, _Context) ->
    Module:Fun();

create_node_from_spec(_NodeName, {Module, Fun, Args}, _Context) ->
    apply(Module, Fun, Args).
