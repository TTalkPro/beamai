# API Reference

This document provides the main API reference for each module of the BeamAI Framework.

## Table of Contents

- [beamai_agent - Simple Agent](#beamai_agent---simple-agent)
- [beamai_coordinator - Multi-Agent Coordinator](#beamai_coordinator---multi-agent-coordinator)
- [Middleware System](#middleware-system)
- [beamai_deepagent - Deep Agent](#beamai_deepagent---deep-agent)
- [beamai_llm - LLM Client](#beamai_llm---llm-client)
- [beamai_memory - Memory Management](#beamai_memory---memory-management)
- [beamai_tools - Tool Library](#beamai_tools---tool-library)
- [beamai_core - Core Module](#beamai_core---core-module)
  - [HTTP Client](#http-client)
  - [HTTP Backend Configuration](#http-backend-configuration)
- [beamai_a2a - A2A Protocol](#beamai_a2a---a2a-protocol)
- [beamai_mcp - MCP Protocol](#beamai_mcp---mcp-protocol)
- [beamai_rag - RAG Functionality](#beamai_rag---rag-functionality)

---

## beamai_agent - Simple Agent

Simple Agent implementation using the ReAct pattern.

### Lifecycle Management

```erlang
%% Start Agent (process mode)
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_agent:start_link(AgentId, Config).

%% Stop Agent
-spec stop(pid()) -> ok.
beamai_agent:stop(Agent).
```

### Execution API

```erlang
%% Run Agent (process mode)
-spec run(pid(), binary()) -> {ok, map()} | {error, term()}.
beamai_agent:run(Agent, Input).

%% Single execution (pure function mode)
-spec run_once(map(), binary()) -> {ok, map()} | {error, term()}.
beamai_agent:run_once(Config, Input).

%% Execute with state
-spec run_with_state(state(), binary(), map()) -> {ok, map(), state()} | {error, term()}.
beamai_agent:run_with_state(State, Input, Opts).
```

### State Management

```erlang
%% Create state
-spec create_state(map()) -> {ok, state()}.
-spec create_state(binary(), map()) -> {ok, state()}.
beamai_agent:create_state(Config).
beamai_agent:create_state(AgentId, Config).

%% Export/Import state
-spec export_state(state()) -> map().
-spec import_state(map(), map()) -> {ok, state()}.
beamai_agent:export_state(State).
beamai_agent:import_state(ExportedData, Config).
```

### Checkpoint Management

```erlang
%% Save checkpoint
-spec save_checkpoint(pid()) -> {ok, binary()} | {error, term()}.
-spec save_checkpoint(pid(), map()) -> {ok, binary()} | {error, term()}.
beamai_agent:save_checkpoint(Agent).
beamai_agent:save_checkpoint(Agent, Metadata).

%% Load checkpoint
-spec load_checkpoint(pid(), binary()) -> {ok, map()} | {error, term()}.
-spec load_latest_checkpoint(pid()) -> {ok, map()} | {error, term()}.
beamai_agent:load_checkpoint(Agent, CheckpointId).
beamai_agent:load_latest_checkpoint(Agent).

%% Restore from checkpoint
-spec restore_from_checkpoint(pid(), binary()) -> ok | {error, term()}.
beamai_agent:restore_from_checkpoint(Agent, CheckpointId).

%% List checkpoints
-spec list_checkpoints(pid()) -> {ok, [map()]} | {error, term()}.
beamai_agent:list_checkpoints(Agent).
```

### Callback Management

```erlang
%% Get/Set callbacks
-spec get_callbacks(pid()) -> map().
-spec set_callbacks(pid(), map()) -> ok.
beamai_agent:get_callbacks(Agent).
beamai_agent:set_callbacks(Agent, Callbacks).

%% Emit custom event
-spec emit_custom_event(pid(), atom(), map()) -> ok.
beamai_agent:emit_custom_event(Agent, EventName, Data).
```

### Context API (User-defined Context)

Context is used to store user-defined data, which participates in conversations and checkpoint persistence.

```erlang
%% Get complete Context
-spec get_context(pid()) -> map().
beamai_agent:get_context(Agent).

%% Get Context value
-spec get_context(pid(), atom() | binary()) -> term() | undefined.
-spec get_context(pid(), atom() | binary(), term()) -> term().
beamai_agent:get_context(Agent, Key).
beamai_agent:get_context(Agent, Key, Default).

%% Set Context
-spec set_context(pid(), map()) -> ok.
-spec update_context(pid(), map()) -> ok.
-spec put_context(pid(), atom() | binary(), term()) -> ok.
beamai_agent:set_context(Agent, NewContext).
beamai_agent:update_context(Agent, Updates).
beamai_agent:put_context(Agent, Key, Value).
```

### Meta API (Process-level Metadata)

Meta is used to store process-level metadata, **does not participate in conversations**, suitable for storing coordinator information and other runtime data.

```erlang
%% Get complete Meta
-spec get_meta(pid()) -> map().
beamai_agent:get_meta(Agent).

%% Get Meta value
-spec get_meta(pid(), atom() | binary()) -> term() | undefined.
-spec get_meta(pid(), atom() | binary(), term()) -> term().
beamai_agent:get_meta(Agent, Key).
beamai_agent:get_meta(Agent, Key, Default).

%% Set Meta
-spec set_meta(pid(), map()) -> ok.
-spec put_meta(pid(), atom() | binary(), term()) -> ok.
beamai_agent:set_meta(Agent, NewMeta).
beamai_agent:put_meta(Agent, Key, Value).
```

**Context vs Meta Comparison:**

| Feature | Context | Meta |
|---------|---------|------|
| Participates in conversation | Yes | No |
| Checkpoint persistence | Yes | No |
| Typical use cases | User data, conversation state | Coordinator info, runtime config |

### Configuration Options

```erlang
Config = #{
    system_prompt => binary(),           %% System prompt
    tools => [tool_def()],               %% Tool list
    llm => llm_config(),                 %% LLM configuration
    max_iterations => integer(),         %% Maximum iterations, default 10
    storage => beamai_memory(),          %% Optional: storage instance
    callbacks => callback_map(),         %% Optional: callback functions
    middleware => [middleware_spec()]    %% Optional: middleware
}.
```

---

## beamai_coordinator - Multi-Agent Coordinator

The coordinator is used to manage multiple Agents working together, supporting two modes: Pipeline (sequential execution) and Orchestrator (orchestrated execution).

**Important Change (v2.1):** Coordinator API parameters changed from using `Id` to using `CoordinatorPid`. Coordinator metadata is now stored in the Agent's `meta` field, solving the original ETS table process ownership issue.

### Starting Coordinator

```erlang
%% Generic start interface
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_link(Id, Opts).

%% Pipeline mode: tasks are passed sequentially between workers
-spec start_pipeline(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_pipeline(Id, Opts).

%% Orchestrator mode: coordinator orchestrates multiple workers
-spec start_orchestrator(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_orchestrator(Id, Opts).
```

### Configuration Options

```erlang
Opts = #{
    agents => [agent_def()],           %% Worker Agent definition list
    llm => llm_config(),               %% LLM configuration
    system_prompt => binary(),         %% Optional: coordinator system prompt
    max_iterations => integer()        %% Optional: maximum iterations, default 10
}.

%% Agent definition
agent_def() = #{
    name := binary(),                  %% Worker name (required)
    system_prompt := binary()          %% Worker system prompt (required)
}.
```

### Stopping Coordinator

```erlang
%% Stop coordinator and all its workers
-spec stop(pid()) -> ok | {error, term()}.
beamai_coordinator:stop(CoordinatorPid).
```

### Workers Management

```erlang
%% Get all workers
-spec get_workers(pid()) -> {ok, #{binary() => pid()}} | {error, term()}.
beamai_coordinator:get_workers(CoordinatorPid).

%% Get specific worker
-spec get_worker(pid(), binary()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:get_worker(CoordinatorPid, WorkerName).
```

### Task Delegation

```erlang
%% Delegate task to specific worker
-spec delegate(pid(), binary(), binary()) -> {ok, binary()} | {error, term()}.
beamai_coordinator:delegate(CoordinatorPid, WorkerName, Task).

%% Delegate task to multiple workers in parallel
-spec delegate_parallel(pid(), [binary()], binary()) -> {ok, map()} | {error, term()}.
beamai_coordinator:delegate_parallel(CoordinatorPid, WorkerNames, Task).
%% Returns: {ok, #{WorkerName => {ok, Result} | {error, Reason}}}
```

### State Export/Import

```erlang
%% Export coordinator complete state
-spec export_coordinator(pid()) -> {ok, map()} | {error, term()}.
beamai_coordinator:export_coordinator(CoordinatorPid).

%% Import coordinator state (creates new coordinator)
-spec import_coordinator(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:import_coordinator(NewId, ExportedData).
```

### Usage Examples

```erlang
%% Pipeline example: Translation pipeline
LLM = llm_client:create(bailian, #{model => <<"qwen-plus">>, api_key => ApiKey}),

{ok, Pipeline} = beamai_coordinator:start_pipeline(<<"translator">>, #{
    agents => [
        #{name => <<"cn_to_en">>, system_prompt => <<"Translate to English">>},
        #{name => <<"polisher">>, system_prompt => <<"Polish the English">>}
    ],
    llm => LLM
}),

%% Call a specific worker directly
{ok, Result} = beamai_coordinator:delegate(Pipeline, <<"cn_to_en">>, <<"Hello World">>),

%% Stop
beamai_coordinator:stop(Pipeline).
```

```erlang
%% Orchestrator example: Multi-expert consultation
{ok, Panel} = beamai_coordinator:start_orchestrator(<<"experts">>, #{
    agents => [
        #{name => <<"tech">>, system_prompt => <<"Technical expert">>},
        #{name => <<"biz">>, system_prompt => <<"Business expert">>}
    ],
    llm => LLM
}),

%% Consult multiple experts in parallel
{ok, Results} = beamai_coordinator:delegate_parallel(
    Panel, [<<"tech">>, <<"biz">>], <<"Analyze AI trends">>
),

beamai_coordinator:stop(Panel).
```

---

## Middleware System

Interceptor mechanism during Agent execution. The Middleware system is located in the `beamai_tools` module and is shared by `beamai_agent` and `beamai_deepagent`.

Detailed documentation: [MIDDLEWARE.md](MIDDLEWARE.md)

### beamai_middleware Behavior

```erlang
%% All callbacks are optional
-callback init(Opts :: map()) -> middleware_state().
-callback before_agent(State, MwState) -> middleware_result().
-callback after_agent(State, MwState) -> middleware_result().
-callback before_model(State, MwState) -> middleware_result().
-callback after_model(State, MwState) -> middleware_result().
-callback before_tools(State, MwState) -> middleware_result().
-callback after_tools(State, MwState) -> middleware_result().
```

### Return Value Types

```erlang
-type middleware_result() ::
    ok |                              %% No modification
    {update, map()} |                 %% Update graph state
    {goto, model | tools | '__end__'} |  %% Jump
    {update_goto, map(), goto_target()} |  %% Update and jump
    {halt, term()} |                  %% Halt execution
    {interrupt, interrupt_action()}.  %% Interrupt waiting for confirmation
```

### beamai_middleware_runner

```erlang
%% Initialize Middleware chain
-spec init([middleware_spec()]) -> middleware_chain().
beamai_middleware_runner:init(Specs).

%% Middleware specification format
Specs = [
    {middleware_module, Opts},           %% Module + Options
    {middleware_module, Opts, Priority}, %% Module + Options + Priority
    middleware_module                    %% Module name only
].

%% Execute hook
-spec run_hook(hook_name(), graph_state(), middleware_chain()) -> run_result().
beamai_middleware_runner:run_hook(HookName, State, Middlewares).

%% Get Middleware state
-spec get_middleware_state(module(), middleware_chain()) -> {ok, state()} | {error, not_found}.
beamai_middleware_runner:get_middleware_state(Module, Chain).
```

### beamai_middleware_presets

```erlang
%% Preset configurations
-spec default() -> [middleware_spec()].
-spec minimal() -> [middleware_spec()].
-spec production() -> [middleware_spec()].
-spec development() -> [middleware_spec()].
-spec human_in_loop() -> [middleware_spec()].

%% Presets with options
-spec default(map()) -> [middleware_spec()].
beamai_middleware_presets:default(#{
    call_limit => #{max_model_calls => 30},
    summarization => #{window_size => 25}
}).

%% Individual Middleware configuration
-spec call_limit(map()) -> middleware_spec().
-spec summarization(map()) -> middleware_spec().
-spec human_approval(map()) -> middleware_spec().
-spec tool_retry(map()) -> middleware_spec().
```

### Built-in Middleware

| Middleware | Module | Main Configuration |
|------------|--------|-------------------|
| Call Limit | `middleware_call_limit` | `max_model_calls`, `max_tool_calls`, `max_iterations` |
| Context Summarization | `middleware_summarization` | `window_size`, `max_tokens`, `summarize` |
| Human Approval | `middleware_human_approval` | `mode`, `timeout`, `tools` |
| Tool Retry | `middleware_tool_retry` | `max_retries`, `backoff` |
| Model Retry | `middleware_model_retry` | `max_retries`, `retryable_errors` |
| Model Fallback | `middleware_model_fallback` | `fallback_models`, `trigger_errors` |
| PII Detection | `middleware_pii_detection` | `action`, `types` |
| Tool Selection | `middleware_tool_selector` | `strategy`, `whitelist` |

### Custom Middleware Example

```erlang
-module(my_logging_middleware).
-behaviour(beamai_middleware).

-export([init/1, before_model/2, after_model/2]).

init(Opts) ->
    #{log_level => maps:get(log_level, Opts, info)}.

before_model(State, #{log_level := Level}) ->
    Messages = graph_state:get(State, messages, []),
    log(Level, "LLM Request: ~p messages", [length(Messages)]),
    {update, #{request_start => erlang:system_time(millisecond)}}.

after_model(State, #{log_level := Level}) ->
    Start = graph_state:get(State, request_start, 0),
    Duration = erlang:system_time(millisecond) - Start,
    log(Level, "LLM Response: ~pms", [Duration]),
    ok.

log(info, Fmt, Args) -> logger:info(Fmt, Args);
log(debug, Fmt, Args) -> logger:debug(Fmt, Args).
```

---

## beamai_deepagent - Deep Agent

Deep Agent supporting planning and parallel execution.

### Creation and Execution

```erlang
%% Create configuration
-spec new() -> config().
-spec new(map()) -> config().
beamai_deepagent:new().
beamai_deepagent:new(Opts).

%% Run Agent
-spec run(config(), binary()) -> {ok, result()} | {error, term()}.
beamai_deepagent:run(Config, Task).
```

### Result Queries

```erlang
%% Get plan
-spec get_plan(result()) -> plan() | undefined.
beamai_deepagent:get_plan(Result).

%% Get execution trace
-spec get_trace(result()) -> trace().
beamai_deepagent:get_trace(Result).
```

### Configuration Options

```erlang
Config = #{
    llm => llm_config(),                 %% LLM configuration
    tools => [tool_def()],               %% Custom tools
    system_prompt => binary(),           %% System prompt
    max_depth => integer(),              %% Maximum recursion depth, default 3
    max_iterations => integer(),         %% Maximum iterations, default 50
    planning_enabled => boolean(),       %% Enable planning, default true
    planning_mode => full | simple,      %% Planning mode, default full
    reflection_enabled => boolean(),     %% Enable reflection, default true
    filesystem_enabled => boolean(),     %% Enable filesystem tools
    filesystem => filesystem_config(),   %% Filesystem configuration
    human_in_loop => #{enabled => boolean()}  %% Human-in-loop configuration
}.
```

### Tool Provider

DeepAgent provides tools through `beamai_deepagent_tool_provider`, implementing the `beamai_tool_provider` behavior.

```erlang
%% Get tools via beamai_tool_registry
Config = #{depth => 0, planning_mode => full},
Tools = beamai_tool_registry:from_config(#{
    providers => [{beamai_deepagent_tool_provider, Config}]
}).

%% Direct access to tool collections
beamai_deepagent_tool_provider:base_tools().       %% Base tools
beamai_deepagent_tool_provider:plan_tools().       %% Plan tools
beamai_deepagent_tool_provider:subtask_tools().    %% Subtask tools
beamai_deepagent_tool_provider:reflect_tools().    %% Reflect tools
beamai_deepagent_tool_provider:filesystem_tools(). %% Filesystem tools
beamai_deepagent_tool_provider:todo_tools().       %% TodoList tools
beamai_deepagent_tool_provider:human_tools().      %% Human interaction tools

%% Provider interface
beamai_deepagent_tool_provider:info().             %% Get Provider info
beamai_deepagent_tool_provider:available().        %% Check if available
beamai_deepagent_tool_provider:list_tools(Opts).   %% Get tool list
beamai_deepagent_tool_provider:find_tool(Name, Opts). %% Find tool
```

### Tool Condition Judgment

| Tool Set | Condition |
|----------|-----------|
| Base tools | Always available |
| Plan tools | `planning_mode=full` and `depth=0` |
| TodoList tools | `planning_mode=simple` |
| Subtask tools | `depth < max_depth` |
| Reflect tools | `reflection_enabled=true` |
| Filesystem tools | `filesystem_enabled=true` or has `filesystem` config |
| Human tools | `human_in_loop.enabled=true` |

---

## beamai_llm - LLM Client

LLM client with multi-provider support.

### LLM Configuration Management

LLM configuration must be created using `llm_client:create/2`, achieving separation of configuration from Agent:

```erlang
%% Create LLM configuration (must use llm_client:create/2)
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    temperature => 0.7
}).

%% Configuration reuse: multiple Agents share the same configuration
{ok, Agent1} = beamai_agent:start_link(<<"agent1">>, #{llm => LLM, ...}),
{ok, Agent2} = beamai_agent:start_link(<<"agent2">>, #{llm => LLM, ...}).

%% Configuration merging: create new configuration based on existing one
HighTempLLM = llm_client:merge_config(LLM, #{temperature => 0.9}).

%% Validate configuration
true = llm_client:is_valid_config(LLM).
```

**Advantages:**
- Configuration reuse: multiple Agents share the same LLM configuration
- Centralized management: API Key, model parameters unified configuration
- Type safety: Agent validates configuration at startup
- Easy testing: LLM configuration can be validated independently

### Configuration and Chat

```erlang
%% Create configuration
-spec create(provider(), map()) -> llm_config().
llm_client:create(Provider, Opts).

%% Validate configuration
-spec is_valid_config(term()) -> boolean().
llm_client:is_valid_config(Config).

%% Chat
-spec chat(llm_config(), [message()]) -> {ok, response()} | {error, term()}.
llm_client:chat(Config, Messages).

%% Streaming chat
-spec stream_chat(llm_config(), [message()], callback()) -> {ok, response()} | {error, term()}.
llm_client:stream_chat(Config, Messages, Callback).

%% Chat with tools
-spec with_tools(llm_config(), [message()], [tool()]) -> {ok, response()} | {error, term()}.
llm_client:with_tools(Config, Messages, Tools).
```

### Provider Management

```erlang
%% List Providers
-spec list_providers() -> [atom()].
llm_client:list_providers().

%% Provider info
-spec provider_info(atom()) -> map().
llm_client:provider_info(Provider).
```

### Supported Providers

| Provider | Module | API Mode | Features |
|----------|--------|----------|----------|
| `openai` | llm_provider_openai | OpenAI | Chat, streaming, tool calling |
| `anthropic` | llm_provider_anthropic | Anthropic | Chat, streaming, tool calling |
| `deepseek` | llm_provider_deepseek | OpenAI compatible | Chat, streaming, tool calling |
| `zhipu` | llm_provider_zhipu | OpenAI compatible | Chat, streaming, tool calling, async |
| `bailian` | llm_provider_bailian | DashScope native | Chat, streaming, tool calling, web search |
| `ollama` | llm_provider_ollama | OpenAI compatible | Chat, streaming |

### DeepSeek Detailed Description

DeepSeek Provider uses OpenAI compatible API, supporting `deepseek-chat` and `deepseek-reasoner` models.

**Supported Models:**
- `deepseek-chat`: General conversation model (default)
- `deepseek-reasoner`: Reasoning enhanced model

**Configuration Example:**
```erlang
LLM = llm_client:create(deepseek, #{
    model => <<"deepseek-chat">>,
    api_key => list_to_binary(os:getenv("DEEPSEEK_API_KEY")),
    max_tokens => 4096,
    temperature => 1.0
}).
```

### Alibaba Cloud Bailian (DashScope) Detailed Description

Bailian Provider uses DashScope native API, automatically selecting endpoints based on model type:
- **Text generation models** (`qwen-plus`, `qwen-max`, `qwen-turbo`): Use `/api/v1/services/aigc/text-generation/generation`
- **Multimodal models** (`qwen-vl-plus`, `qwen-audio`, etc.): Use `/api/v1/services/aigc/multimodal-generation/generation`

**Unique Parameters:**
- `enable_search => true`: Enable web search functionality
- `tool_choice => <<"required">>`: Force tool calling

**Streaming Output:**
- Request header: `X-DashScope-SSE: enable`
- Parameter: `parameters.incremental_output: true`

### LLM Configuration Parameters

`llm_client:create/2` supports the following parameters:

```erlang
LLM = llm_client:create(Provider, #{
    model => binary(),                   %% Model name (required)
    api_key => binary(),                 %% API Key (required, except ollama)
    base_url => binary(),                %% Optional: custom URL
    timeout => integer(),                %% Optional: timeout (milliseconds)
    max_tokens => integer(),             %% Optional: maximum tokens
    temperature => float()               %% Optional: temperature parameter (0.0 - 2.0)
}).
```

**Provider Types:** `openai | anthropic | deepseek | zhipu | bailian | ollama`

---

## beamai_memory - Memory Management

Unified memory and checkpoint management system.

### Creation and Configuration

```erlang
%% Create Memory instance
-spec new(map()) -> {ok, memory()} | {error, term()}.
beamai_memory:new(Config).

Config = #{
    checkpointer => #{backend => ets | sqlite},
    store => #{backend => ets | sqlite},
    context_store => {module(), term()}
}.
```

### Checkpoint Operations

```erlang
%% Save checkpoint
-spec save_checkpoint(memory(), config(), state_data()) -> {ok, memory()}.
beamai_memory:save_checkpoint(Memory, Config, StateData).

%% Load checkpoint
-spec load_checkpoint(memory(), config()) -> {ok, state_data()} | {error, not_found}.
-spec load_latest_checkpoint(memory(), config()) -> {ok, state_data()} | {error, not_found}.
beamai_memory:load_checkpoint(Memory, Config).
beamai_memory:load_latest_checkpoint(Memory, Config).

%% List checkpoints
-spec list_checkpoints(memory(), config()) -> {ok, [checkpoint_info()]}.
beamai_memory:list_checkpoints(Memory, Config).

%% Checkpoint count
-spec checkpoint_count(memory(), config()) -> non_neg_integer().
beamai_memory:checkpoint_count(Memory, Config).
```

### Store Operations

```erlang
%% Store data
-spec put(memory(), namespace(), key(), value()) -> {ok, memory()}.
beamai_memory:put(Memory, Namespace, Key, Value).

%% Search data
-spec search(memory(), namespace(), filter()) -> {ok, [item()]}.
beamai_memory:search(Memory, Namespace, Filter).
```

---

## beamai_tools - Tool Library and Middleware System

Unified tool definition and management, as well as Agent execution middleware system.

beamai_tools contains two core functionalities:
- **Tool System**: Tool definition, registration, Provider mechanism
- **Middleware System**: Agent execution interception, enhancement, and control (see [Middleware System](#middleware-system))

### Tool Retrieval

```erlang
%% Get tools
-spec get_tools(category() | [category()]) -> [tool_def()].
-spec get_tools(category(), map()) -> [tool_def()].
beamai_tools:get_tools(Categories).
beamai_tools:get_tools(Categories, Opts).

%% Get all tools
-spec get_all_tools() -> [tool_def()].
beamai_tools:get_all_tools().

%% Find tool
-spec find_tool(binary()) -> {ok, tool_def()} | {error, not_found}.
beamai_tools:find_tool(Name).
```

### Tool Execution

```erlang
%% Execute tool
-spec execute(binary(), map()) -> {ok, term()} | {error, term()}.
-spec execute(binary(), map(), map()) -> {ok, term()} | {error, term()}.
beamai_tools:execute(ToolName, Args).
beamai_tools:execute(ToolName, Args, Opts).
```

### Tool Conversion

```erlang
%% Convert to LLM format
-spec to_llm_spec(tool_def()) -> map().
-spec to_llm_specs([tool_def()]) -> [map()].
beamai_tools:to_llm_spec(Tool).
beamai_tools:to_llm_specs(Tools).
```

### Tool Registry

```erlang
%% Build tool list
Registry = beamai_tool_registry:new(),
R1 = beamai_tool_registry:add_tools(Registry, Tools),
R2 = beamai_tool_registry:add_provider(R1, Provider),
Tools = beamai_tool_registry:build(R2).

%% Convenience function
Tools = beamai_tool_registry:from_config(#{
    tools => [Tool1, Tool2],
    providers => [Provider1, Provider2]
}).
```

---

## beamai_core - Core Module

### Graph Execution Engine

```erlang
%% Build Graph
Graph = graph_builder:new()
    |> graph_builder:add_node(NodeName, {Module, Opts})
    |> graph_builder:add_edge(From, To, Condition)
    |> graph_builder:set_entry(EntryNode)
    |> graph_builder:build().

%% Execute Graph
-spec run(graph(), state()) -> {ok, state()} | {error, term()}.
graph_runner:run(Graph, InitialState).

%% Graph DSL
Graph = graph_dsl:compile(#{
    nodes => #{...},
    edges => [...],
    entry => atom()
}).
```

### Graph State

```erlang
%% Create state
-spec new(map()) -> state().
graph_state:new(Data).

%% Read/Write state
-spec get(state(), key()) -> value().
-spec set(state(), key(), value()) -> state().
graph_state:get(State, Key).
graph_state:set(State, Key, Value).
```

### Pregel Distributed Computing

```erlang
%% Create Pregel graph
{ok, Graph} = pregel_graph:new(Config).

%% Add vertices and edges
pregel_graph:add_vertex(Graph, VertexId, Data).
pregel_graph:add_edge(Graph, From, To, Weight).

%% Run computation
{ok, Result} = pregel:run(Graph, ComputeFn, MaxIterations).
```

### HTTP Client

BeamAI provides a unified HTTP client interface, supporting both Gun and Hackney backends.

```erlang
%% Send request (automatically uses configured backend)
-spec request(method(), url(), headers(), body(), opts()) -> {ok, response()} | {error, term()}.
beamai_http:request(Method, Url, Headers, Body, Opts).

%% Convenience functions
-spec get(url(), headers()) -> {ok, response()} | {error, term()}.
-spec post(url(), headers(), body()) -> {ok, response()} | {error, term()}.
beamai_http:get(Url, Headers).
beamai_http:post(Url, Headers, Body).

%% Streaming request (SSE)
-spec stream_request(url(), headers(), body(), callback(), opts()) -> {ok, term()} | {error, term()}.
beamai_http:stream_request(Url, Headers, Body, Callback, Opts).
```

### HTTP Backend Configuration

```erlang
%% Application configuration (sys.config)
{beamai_core, [
    %% Select HTTP backend: beamai_http_gun (default) or beamai_http_hackney
    {http_backend, beamai_http_gun},

    %% Gun connection pool configuration
    {http_pool, #{
        max_connections => 100,        %% Maximum connections
        connection_timeout => 30000,   %% Connection timeout (milliseconds)
        idle_timeout => 60000          %% Idle timeout (milliseconds)
    }}
]}.
```

**Backend Comparison:**

| Feature | Gun (default) | Hackney |
|---------|---------------|---------|
| HTTP/2 | Supported | Not supported |
| Connection pool | beamai_http_pool | hackney built-in pool |
| TLS | Automatically uses system CA certificates (OTP 25+) | hackney default config |
| Dependency | gun 2.1.0 | hackney |
| Recommended scenario | Production, needs HTTP/2 | Legacy system compatibility |

### HTTP Connection Pool (Gun Backend)

When using the Gun backend, beamai_http_pool is automatically started as a child process of the beamai_core application.

```erlang
%% Connection pool API
-spec checkout(host(), port(), protocol()) -> {ok, connection()} | {error, term()}.
beamai_http_pool:checkout(Host, Port, Protocol).

-spec checkin(connection()) -> ok.
beamai_http_pool:checkin(Conn).

%% View connection pool status
-spec get_stats() -> map().
beamai_http_pool:get_stats().
```

---

## beamai_a2a - A2A Protocol

Agent-to-Agent communication protocol implementation.

### Server

```erlang
%% Start server
-spec start_link(map()) -> {ok, pid()}.
beamai_a2a_server:start_link(Config).

Config = #{
    handler => module(),                 %% Request handler
    port => integer(),                   %% HTTP port
    auth => auth_config()                %% Authentication configuration
}.
```

### Client

```erlang
%% Discover Agent
-spec discover(binary()) -> {ok, agent_card()} | {error, term()}.
beamai_a2a_client:discover(AgentUrl).

%% Send message
-spec send_message(binary(), message()) -> {ok, response()} | {error, term()}.
beamai_a2a_client:send_message(AgentUrl, Message).
```

### Agent Card

```erlang
%% Create Card
-spec new(map()) -> agent_card().
beamai_a2a_card:new(#{
    name => binary(),
    description => binary(),
    url => binary(),
    capabilities => [binary()]
}).

%% Validate Card
-spec validate(agent_card()) -> ok | {error, term()}.
beamai_a2a_card:validate(Card).
```

---

## beamai_mcp - MCP Protocol

Model Context Protocol implementation.

### Client

```erlang
%% Connect to MCP server
-spec connect(config()) -> {ok, client()} | {error, term()}.
beamai_mcp_client:connect(Config).

Config = #{
    transport => stdio | http | sse,
    command => binary(),                 %% stdio: command
    url => binary()                      %% http/sse: URL
}.

%% List tools
-spec list_tools(client()) -> {ok, [tool()]} | {error, term()}.
beamai_mcp_client:list_tools(Client).

%% Call tool
-spec call_tool(client(), binary(), map()) -> {ok, result()} | {error, term()}.
beamai_mcp_client:call_tool(Client, ToolName, Args).

%% List resources
-spec list_resources(client()) -> {ok, [resource()]} | {error, term()}.
beamai_mcp_client:list_resources(Client).
```

### Server

```erlang
%% Start server
-spec start_link(config()) -> {ok, pid()}.
beamai_mcp_server:start_link(Config).

%% Register tool
-spec register_tool(pid(), tool_def()) -> ok.
beamai_mcp_server:register_tool(Server, Tool).

%% Register resource
-spec register_resource(pid(), resource_def()) -> ok.
beamai_mcp_server:register_resource(Server, Resource).
```

---

## beamai_rag - RAG Functionality

Retrieval Augmented Generation module.

### Vector Embeddings

```erlang
%% Generate embeddings
-spec embed(binary(), config()) -> {ok, [float()]} | {error, term()}.
-spec embed_batch([binary()], config()) -> {ok, [[float()]]} | {error, term()}.
beamai_embeddings:embed(Text, Config).
beamai_embeddings:embed_batch(Texts, Config).
```

### Vector Store

```erlang
%% Create store
-spec new(config()) -> {ok, store()}.
beamai_vector_store:new(Config).

%% Add vector
-spec add(store(), binary(), [float()], map()) -> {ok, store()}.
beamai_vector_store:add(Store, Id, Vector, Metadata).

%% Similarity search
-spec search(store(), [float()], integer()) -> {ok, [result()]}.
beamai_vector_store:search(Store, QueryVector, TopK).
```

### RAG Workflow

```erlang
%% Initialize
-spec init(config()) -> {ok, rag_state()}.
beamai_rag:init(Config).

%% Add documents
-spec add_documents(rag_state(), [document()]) -> {ok, rag_state()}.
beamai_rag:add_documents(State, Documents).

%% Retrieve
-spec retrieve(rag_state(), binary(), integer()) -> {ok, [chunk()]}.
beamai_rag:retrieve(State, Query, TopK).

%% RAG query (retrieve + generate)
-spec query(rag_state(), binary()) -> {ok, response()}.
beamai_rag:query(State, Question).
```

---

## Common Types

### Tool Definition

```erlang
-type tool_def() :: #{
    name := binary(),
    description := binary(),
    parameters := json_schema(),
    handler := fun((map()) -> {ok, term()} | {error, term()})
                | fun((map(), map()) -> {ok, term()} | {error, term()})
}.
```

### Message Types

```erlang
-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    tool_call_id => binary()
}.
```

### LLM Response

```erlang
-type llm_response() :: #{
    id := binary(),
    model := binary(),
    content := binary() | null,
    tool_calls := [tool_call()],
    finish_reason := binary(),
    usage => usage_info()
}.
```

---

## Error Handling

All APIs return `{ok, Result}` or `{error, Reason}` format. Common error types:

| Error | Description |
|-------|-------------|
| `{error, missing_api_key}` | API Key not configured |
| `{error, timeout}` | Request timeout |
| `{error, {http_error, Code, Body}}` | HTTP error |
| `{error, {api_error, Details}}` | API returned error |
| `{error, not_found}` | Resource not found |
| `{error, storage_not_enabled}` | Storage not enabled |

---

## More Documentation

- [README.md](../README.md) - Project overview
- [ARCHITECTURE.md](ARCHITECTURE.md) - Architecture design
- Module READMEs:
  - [beamai_core](../apps/beamai_core/README.md)
  - [beamai_llm](../apps/beamai_llm/README.md)
  - [beamai_agent](../apps/beamai_agent/README.md)
  - [beamai_deepagent](../apps/beamai_deepagent/README.md)
  - [beamai_memory](../apps/beamai_memory/README.md)
  - [beamai_tools](../apps/beamai_tools/README.md)
  - [beamai_a2a](../apps/beamai_a2a/README.md)
  - [beamai_mcp](../apps/beamai_mcp/README.md)
  - [beamai_rag](../apps/beamai_rag/README.md)
