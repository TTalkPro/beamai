# API Reference

English | [中文](API_REFERENCE_EN.md)

This document provides the main API reference for the core modules of the BeamAI Framework.

## Table of Contents

- [beamai_core - Core Module](#beamai_core---core-module)
  - [HTTP Client](#http-client)
  - [HTTP Backend Configuration](#http-backend-configuration)
- [beamai_llm - LLM Client](#beamai_llm---llm-client)
- [beamai_memory - Memory Management](#beamai_memory---memory-management)

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

%% User context operations
-spec get_context(state()) -> map().
-spec get_context(state(), key()) -> value() | undefined.
-spec set_context(state(), map()) -> state().
-spec update_context(state(), map()) -> state().
graph_state:get_context(State).
graph_state:get_context(State, Key).
graph_state:set_context(State, Context).
graph_state:update_context(State, Updates).
```

### Graph State Reducer

Field-level reducers for merging node-returned deltas into global state.

```erlang
%% Apply delta
-spec apply_delta(state(), delta(), field_reducers()) -> state().
-spec apply_deltas(state(), [delta()], field_reducers()) -> state().
graph_state_reducer:apply_delta(State, Delta, FieldReducers).
graph_state_reducer:apply_deltas(State, Deltas, FieldReducers).

%% Built-in Reducers
graph_state_reducer:append_reducer(Old, New) -> list().
graph_state_reducer:merge_reducer(Old, New) -> map().
graph_state_reducer:increment_reducer(Old, Delta) -> number().
graph_state_reducer:last_write_win_reducer(Old, New) -> term().
```

**Reducer Configuration Format:**

```erlang
FieldReducers = #{
    %% Normal reducer
    <<"messages">> => fun graph_state_reducer:append_reducer/2,

    %% Transform reducer: accumulate counter_incr to counter
    <<"counter_incr">> => {transform, <<"counter">>, fun graph_state_reducer:increment_reducer/2}
}.
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

### Process Branch & Time Travel API

The Process framework provides branching and time-travel capabilities through a pluggable store backend.
All operations use the `{Module, Ref}` dynamic dispatch pattern via `beamai_process_store_behaviour` optional callbacks.

```erlang
-type store() :: {module(), term()}.

%% Branch API
-spec branch_from(store(), BranchName :: binary(), Opts :: map()) ->
    {ok, #{branch_thread_id := binary(), snapshot_id := binary()}} | {error, term()}.
-spec restore_branch(store(), BranchThreadId :: binary(),
                     ProcessSpec, Opts :: map()) -> {ok, pid()} | {error, term()}.
-spec list_branches(store(), Opts :: map()) -> {ok, [map()]} | {error, term()}.
-spec get_lineage(store(), Opts :: map()) -> {ok, [map()]} | {error, term()}.

%% Time Travel API
-spec go_back(store(), Steps :: pos_integer(), ProcessSpec) ->
    {ok, pid()} | {error, term()}.
-spec go_back(store(), Steps :: pos_integer(), ProcessSpec, Opts :: map()) ->
    {ok, pid()} | {error, term()}.
-spec go_forward(store(), Steps :: pos_integer(), ProcessSpec) ->
    {ok, pid()} | {error, term()}.
-spec go_forward(store(), Steps :: pos_integer(), ProcessSpec, Opts :: map()) ->
    {ok, pid()} | {error, term()}.
-spec goto_snapshot(store(), SnapshotId :: binary(), ProcessSpec) ->
    {ok, pid()} | {error, term()}.
-spec goto_snapshot(store(), SnapshotId :: binary(), ProcessSpec, Opts :: map()) ->
    {ok, pid()} | {error, term()}.
-spec list_history(store()) -> {ok, [map()]} | {error, term()}.
```

**Usage Example:**

```erlang
%% Create store reference
Store = {beamai_process_memory_store, {Mgr, #{thread_id => ThreadId}}},

%% Create a branch
{ok, #{branch_thread_id := BranchId}} =
    beamai_process:branch_from(Store, <<"experiment">>, #{}),

%% Time travel: go back 2 steps
{ok, Pid} = beamai_process:go_back(Store, 2, ProcessSpec),

%% List execution history
{ok, History} = beamai_process:list_history(Store).
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
  - [beamai_memory](../apps/beamai_memory/README.md)
