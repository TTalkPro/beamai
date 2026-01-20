# BeamAI Framework Architecture Design

English | [中文](ARCHITECTURE.md)

This document describes in detail the overall architecture, core design philosophy, and module responsibilities of the BeamAI Framework.

## Table of Contents

- [Architecture Overview](#architecture-overview)
- [Design Principles](#design-principles)
- [Layered Architecture](#layered-architecture)
- [Core Modules](#core-modules)
- [Execution Engine](#execution-engine)
- [Tool System](#tool-system)
- [Middleware System](#middleware-system)
- [Storage and Persistence](#storage-and-persistence)
- [Protocol Support](#protocol-support)
- [Data Flow](#data-flow)
- [Extension Mechanisms](#extension-mechanisms)

---

## Architecture Overview

BeamAI Framework is a high-performance AI Agent framework based on Erlang/OTP, adopting a layered architecture design:

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           Application Layer                              │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────────┐  │
│  │  beamai_agent   │  │ beamai_deepagent│  │    beamai_examples      │  │
│  │  (Simple Agent) │  │  (Deep Agent)   │  │    (Example Apps)       │  │
│  └────────┬────────┘  └────────┬────────┘  └───────────────────────┬─┘  │
└───────────┼─────────────────────┼───────────────────────────────────┼────┘
            │                     │                                   │
┌───────────┼─────────────────────┼───────────────────────────────────┼────┐
│           │              Service Layer                              │    │
│  ┌────────┴────────┐  ┌────────┴────────┐  ┌──────────────────────┐│    │
│  │   beamai_llm    │  │   beamai_rag    │  │    beamai_tools      ││    │
│  │  (LLM Client)   │  │ (RAG Functions) │  │(Tool Library + MW)   ││    │
│  └─────────────────┘  └─────────────────┘  └──────────────────────┘│    │
│  ┌─────────────────┐  ┌─────────────────┐                          │    │
│  │   beamai_a2a    │  │   beamai_mcp    │                          │    │
│  │  (A2A Protocol) │  │  (MCP Protocol) │                          │    │
│  └─────────────────┘  └─────────────────┘                          │    │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │
┌────────────────────────────────┼────────────────────────────────────────┐
│                         Core Layer                                      │
│  ┌─────────────────────────────┴─────────────────────────────────────┐  │
│  │                        beamai_core                                 │  │
│  │  ┌───────────┐  ┌───────────┐  ┌───────────┐  ┌───────────────┐   │  │
│  │  │   Graph   │  │  Pregel   │  │   Types   │  │   HTTP/SSE    │   │  │
│  │  │  Engine   │  │  Compute  │  │   Defs    │  │   Client      │   │  │
│  │  └───────────┘  └───────────┘  └───────────┘  └───────────────┘   │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                       beamai_memory                                │  │
│  │  ┌───────────┐  ┌───────────┐  ┌───────────────────────────────┐  │  │
│  │  │Checkpoint │  │   Store   │  │      Context Store            │  │  │
│  │  │  Manager  │  │  Storage  │  │      (ETS/SQLite)             │  │  │
│  │  └───────────┘  └───────────┘  └───────────────────────────────┘  │  │
│  └───────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Design Principles

### 1. OTP Design Patterns

- **Process Isolation**: Each Agent runs in an independent process, failures do not affect other Agents
- **Supervision Tree**: Uses Supervisor to manage process lifecycle
- **Behaviour**: Uses behaviours to define standard interfaces, such as `beamai_behaviour`, `beamai_tool_provider`, `beamai_middleware`

### 2. Separation of Concerns

- **Core Layer**: Provides foundational capabilities, independent of specific business logic
- **Service Layer**: Provides LLM, RAG, protocol and other services
- **Application Layer**: Implements specific Agent logic

### 3. Pluggable Architecture

- **Provider Mechanism**: LLM, tools, and storage all support multiple Providers
- **Middleware Mechanism**: Extend Agent behavior through middleware
- **Configurable Strategies**: Conflict resolution, retry strategies, etc. can be customized

### 4. High-Performance Design

- **Lightweight Processes**: Leverages Erlang lightweight processes for high concurrency
- **ETS Storage**: Uses ETS for high-speed in-memory storage
- **Connection Pool**: HTTP requests use Hackney connection pool

---

## Layered Architecture

### Core Layer (beamai_core, beamai_memory)

Provides the foundational capabilities of the framework:

| Module | Responsibility |
|------|------|
| `graph_*` | Graph execution engine, the core of Agent execution |
| `pregel_*` | Pregel distributed computation model |
| `beamai_types` | Common type definitions |
| `beamai_http` | HTTP client wrapper |
| `beamai_memory` | Memory and checkpoint management |

### Service Layer (beamai_llm, beamai_tools, beamai_rag, beamai_a2a, beamai_mcp)

Provides core AI-related services:

| Module | Responsibility |
|------|------|
| `beamai_llm` | Multi-Provider LLM client |
| `beamai_tools` | Tool registry, Provider mechanism, Middleware system |
| `beamai_rag` | Vector embedding and retrieval-augmented generation |
| `beamai_a2a` | Agent-to-Agent communication protocol |
| `beamai_mcp` | Model Context Protocol implementation |

### Application Layer (beamai_agent, beamai_deepagent)

Implements specific Agent patterns:

| Module | Responsibility |
|------|------|
| `beamai_agent` | ReAct pattern Agent, supports Middleware |
| `beamai_deepagent` | Recursive planning Agent, supports subtask dispatch |

---

## Core Modules

### beamai_core

```
beamai_core/
├── behaviours/
│   └── beamai_behaviour.erl      # Agent behaviour definition
├── types/
│   ├── beamai_types.erl          # Common types
│   ├── beamai_message.erl        # Message types
│   ├── beamai_result.erl         # Result types
│   └── beamai_tool.erl           # Tool types
├── graph/                         # Graph execution engine
│   ├── graph.erl                 # Graph structure
│   ├── graph_builder.erl         # Graph builder
│   ├── graph_runner.erl          # Graph executor
│   ├── graph_state.erl           # State management
│   ├── graph_node.erl            # Node definition
│   └── graph_dsl.erl             # DSL support
├── pregel/                        # Pregel distributed computation
│   ├── pregel.erl                # Pregel core
│   ├── pregel_master.erl         # Master node
│   └── pregel_worker.erl         # Worker node
└── utils/
    ├── beamai_http.erl           # HTTP client
    ├── beamai_sse.erl            # SSE support
    └── beamai_utils.erl          # Utility functions
```

### beamai_memory

```
beamai_memory/
├── beamai_memory.erl             # Main module
├── checkpointer/
│   ├── beamai_checkpointer.erl   # Checkpointer behaviour
│   ├── beamai_checkpointer_ets.erl
│   └── beamai_checkpointer_sqlite.erl
└── store/
    ├── beamai_store.erl          # Store behaviour
    ├── beamai_store_ets.erl
    └── beamai_store_sqlite.erl
```

---

## Execution Engine

### Graph Execution Engine

The Graph engine is the core of Agent execution, using a directed graph model:

```
                    ┌─────────────┐
                    │   Entry     │
                    │  (start)    │
                    └──────┬──────┘
                           │
                           ▼
                    ┌─────────────┐
             ┌──────│    LLM      │──────┐
             │      │   Node      │      │
             │      └─────────────┘      │
             │                           │
        has_tools?                  no_tools?
             │                           │
             ▼                           ▼
      ┌─────────────┐            ┌─────────────┐
      │   Tools     │            │    End      │
      │   Node      │            │  (finish)   │
      └──────┬──────┘            └─────────────┘
             │
             └───────────────┐
                             │
                             ▼
                      ┌─────────────┐
                      │    LLM      │
                      │   Node      │
                      └─────────────┘
```

#### Key Components

1. **graph_builder**: Builds the graph structure
2. **graph_runner**: Executes the graph, manages state transitions
3. **graph_state**: State object passed between nodes
4. **graph_node**: Node execution logic

#### State Transitions

```erlang
%% State structure
State = #{
    messages => [Message],           %% Conversation history
    tools => [ToolDef],              %% Available tools
    pending_tools => [ToolCall],     %% Pending tool calls
    tool_results => [ToolResult],    %% Tool execution results
    last_llm_response => Response,   %% Latest LLM response
    iteration => N,                  %% Current iteration count
    %% ... other state
}.
```

---

## Tool System

### Provider Mechanism

The tool system adopts the Provider pattern, supporting multiple tool sources:

```
┌─────────────────────────────────────────────────────────────────┐
│                     beamai_tool_registry                         │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                    from_config/1                           │  │
│  │  providers => [                                            │  │
│  │      {beamai_deepagent_tool_provider, Config},            │  │
│  │      {beamai_tool_provider_mcp, #{server => ...}},        │  │
│  │      beamai_tool_provider_builtin                          │  │
│  │  ]                                                         │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
            ┌─────────────────┼─────────────────┐
            │                 │                 │
            ▼                 ▼                 ▼
┌───────────────────┐ ┌───────────────┐ ┌───────────────┐
│beamai_deepagent_  │ │beamai_tool_   │ │beamai_tool_   │
│  tool_provider    │ │provider_mcp   │ │provider_builtin│
├───────────────────┤ ├───────────────┤ ├───────────────┤
│ - base_tools      │ │ - MCP Server  │ │ - file tools  │
│ - plan_tools      │ │   connection  │ │ - shell tools │
│ - subtask_tools   │ │ - remote tool │ │ - todo tools  │
│ - reflect_tools   │ │   invocation  │ │ - human tools │
│ - fs_tools        │ │               │ │               │
│ - todo_tools      │ │               │ │               │
│ - human_tools     │ │               │ │               │
└───────────────────┘ └───────────────┘ └───────────────┘
```

### Provider Behaviour Definition

```erlang
-behaviour(beamai_tool_provider).

%% Required callbacks
-callback list_tools(Opts :: map()) -> {ok, [tool_def()]} | {error, term()}.

%% Optional callbacks
-callback find_tool(Name :: binary(), Opts :: map()) -> {ok, tool_def()} | {error, not_found}.
-callback info() -> map().
-callback available() -> boolean().
```

### Tool Definition Format

```erlang
#{
    name => <<"tool_name">>,
    description => <<"Tool description">>,
    parameters => #{
        type => object,
        properties => #{
            <<"param1">> => #{type => string, description => <<"...">>}
        },
        required => [<<"param1">>]
    },
    handler => fun(Args) -> {ok, Result} end,
    %% Optional fields
    category => file | shell | todo | human | plan | custom,
    permissions => [file_read, file_write, shell_access],
    metadata => #{}
}
```

---

## Middleware System

The Middleware system is located in the `beamai_tools` module, providing interception, enhancement, and control capabilities during Agent execution.
Both beamai_agent and beamai_deepagent use the Middleware system by depending on beamai_tools.

### Execution Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                        Agent Execution                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────────┐                                           │
│  │   before_agent   │  <- Before Agent starts                   │
│  └────────┬─────────┘                                           │
│           │                                                      │
│           ▼                                                      │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                     Agent Loop                            │   │
│  │  ┌──────────────┐                                        │   │
│  │  │ before_model │  <- Check limits, modify messages      │   │
│  │  └──────┬───────┘                                        │   │
│  │         ▼                                                │   │
│  │  ┌──────────────┐                                        │   │
│  │  │   LLM Call   │                                        │   │
│  │  └──────┬───────┘                                        │   │
│  │         ▼                                                │   │
│  │  ┌──────────────┐                                        │   │
│  │  │ after_model  │  <- Process response, log              │   │
│  │  └──────┬───────┘                                        │   │
│  │         ▼                                                │   │
│  │  ┌──────────────┐                                        │   │
│  │  │ before_tools │  <- Human approval, parameter validation│   │
│  │  └──────┬───────┘                                        │   │
│  │         ▼                                                │   │
│  │  ┌──────────────┐                                        │   │
│  │  │Tool Execution│                                        │   │
│  │  └──────┬───────┘                                        │   │
│  │         ▼                                                │   │
│  │  ┌──────────────┐                                        │   │
│  │  │ after_tools  │  <- Result validation, failure retry   │   │
│  │  └──────┬───────┘                                        │   │
│  │         │                                                │   │
│  └─────────┴────────────────────────────────────────────────┘   │
│           │                                                      │
│           ▼                                                      │
│  ┌──────────────────┐                                           │
│  │   after_agent    │  <- Clean up resources, record stats      │
│  └──────────────────┘                                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Middleware Behaviour Definition

```erlang
-behaviour(beamai_middleware).

%% All callbacks are optional
-callback init(Opts :: map()) -> middleware_state().
-callback before_agent(State, MwState) -> middleware_result().
-callback after_agent(State, MwState) -> middleware_result().
-callback before_model(State, MwState) -> middleware_result().
-callback after_model(State, MwState) -> middleware_result().
-callback before_tools(State, MwState) -> middleware_result().
-callback after_tools(State, MwState) -> middleware_result().

%% Return value types
-type middleware_result() ::
    ok |                              %% Continue execution
    {update, map()} |                 %% Update state
    {goto, model | tools | '__end__'} |  %% Jump
    {update_goto, map(), goto_target()} |
    {halt, term()} |                  %% Halt
    {interrupt, interrupt_action()}.  %% Interrupt waiting for confirmation
```

### Built-in Middleware

| Middleware | Responsibility |
|------------|------|
| `middleware_call_limit` | Limit model/tool call count |
| `middleware_summarization` | Auto-compress long conversation history |
| `middleware_human_approval` | Human confirmation before tool execution |
| `middleware_tool_retry` | Auto-retry on tool failure |
| `middleware_model_retry` | Auto-retry on LLM failure |
| `middleware_model_fallback` | Switch to backup model on primary failure |
| `middleware_pii_detection` | Detect sensitive information |

---

## Storage and Persistence

### Storage Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      beamai_memory                           │
│  ┌─────────────────────────────────────────────────────────┐│
│  │                    Unified API                          ││
│  │  save_checkpoint/3, load_checkpoint/2, put/4, search/3  ││
│  └───────────────────────────┬─────────────────────────────┘│
│                              │                               │
│  ┌───────────────────────────┼───────────────────────────┐  │
│  │                           │                            │  │
│  │  ┌───────────────┐   ┌────┴────────┐                  │  │
│  │  │ Checkpointer  │   │   Store     │                  │  │
│  │  │ (Checkpoints) │   │ (Key-Value) │                  │  │
│  │  └───────┬───────┘   └──────┬──────┘                  │  │
│  │          │                  │                          │  │
│  │  ┌───────┴───────┐   ┌──────┴──────┐                  │  │
│  │  │  ETS/SQLite   │   │  ETS/SQLite │                  │  │
│  │  │   Backend     │   │   Backend   │                  │  │
│  │  └───────────────┘   └─────────────┘                  │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### Checkpoint Structure

```erlang
#{
    id => CheckpointId,
    timestamp => Timestamp,
    state_data => #{
        messages => [Message],
        context => #{...},
        %% Other Agent state
    },
    metadata => #{
        tag => <<"v1">>,
        description => <<"...">>
    }
}
```

---

## Protocol Support

### A2A (Agent-to-Agent)

Inter-Agent communication protocol, supporting service discovery and message passing:

```
┌─────────────────┐                    ┌─────────────────┐
│   Agent A       │                    │   Agent B       │
│  ┌───────────┐  │     discover()     │  ┌───────────┐  │
│  │ A2A Client├──┼───────────────────►│  │ A2A Server│  │
│  └───────────┘  │                    │  └───────────┘  │
│                 │◄───────────────────┤                 │
│                 │    Agent Card      │                 │
│  ┌───────────┐  │                    │  ┌───────────┐  │
│  │ A2A Client├──┼──send_message()───►│  │ A2A Server│  │
│  └───────────┘  │                    │  └───────────┘  │
└─────────────────┘                    └─────────────────┘
```

### MCP (Model Context Protocol)

Interact with MCP servers to obtain tools and resources:

```
┌─────────────────┐                    ┌─────────────────┐
│   BeamAI        │                    │   MCP Server    │
│  ┌───────────┐  │     connect()      │  ┌───────────┐  │
│  │ MCP Client├──┼───────────────────►│  │  Handler  │  │
│  └───────────┘  │                    │  └───────────┘  │
│                 │                    │                 │
│                 │◄──list_tools()────►│                 │
│                 │◄──call_tool()─────►│                 │
│                 │◄──list_resources()►│                 │
│                 │                    │                 │
└─────────────────┘                    └─────────────────┘

Supported transports:
- stdio: Standard input/output
- http: HTTP requests
- sse: Server-Sent Events
```

---

## Data Flow

### Simple Agent Data Flow

```
User Input
    │
    ▼
┌─────────────────┐
│  beamai_agent   │
│  (gen_server)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐     ┌─────────────────┐
│  Middleware     │────►│  graph_runner   │
│  (before_agent) │     └────────┬────────┘
└─────────────────┘              │
                                 ▼
                    ┌────────────────────────┐
                    │      Agent Loop        │
                    │  ┌──────────────────┐  │
                    │  │   LLM Node       │  │
                    │  │  (beamai_llm)    │  │
                    │  └────────┬─────────┘  │
                    │           │            │
                    │  ┌────────▼─────────┐  │
                    │  │   Tools Node     │  │
                    │  │(beamai_tools)    │  │
                    │  └──────────────────┘  │
                    └────────────────────────┘
                                 │
                                 ▼
                         Final response to user
```

### Deep Agent Data Flow

```
User Input
    │
    ▼
┌──────────────────────┐
│   beamai_deepagent   │
└──────────┬───────────┘
           │
           ▼
┌──────────────────────┐
│   Planning Phase     │ <- Create execution plan
│  (create_plan tool)  │
└──────────┬───────────┘
           │
           ▼
┌──────────────────────┐
│   Execution Phase    │
│  ┌────────────────┐  │
│  │   LLM Node     │  │
│  └───────┬────────┘  │
│          │           │
│  ┌───────▼────────┐  │
│  │  Tool Executor │──┼──► spawn_subtask ──► Subtask execution
│  └───────┬────────┘  │
│          │           │
│  ┌───────▼────────┐  │
│  │   Reflection   │  │ <- Reflect on current progress
│  │  (reflect tool)│  │
│  └────────────────┘  │
└──────────────────────┘
           │
           ▼
      Final Result
```

---

## Extension Mechanisms

### 1. Custom LLM Provider

```erlang
-module(my_llm_provider).
-behaviour(llm_provider).

-export([chat/3, stream_chat/4, info/0]).

info() ->
    #{name => <<"my_provider">>, supports => [chat, stream, tools]}.

chat(Config, Messages, Opts) ->
    %% Implement chat logic
    {ok, Response}.

stream_chat(Config, Messages, Callback, Opts) ->
    %% Implement streaming chat
    {ok, FinalResponse}.
```

### 2. Custom Tool Provider

```erlang
-module(my_tool_provider).
-behaviour(beamai_tool_provider).

-export([list_tools/1, find_tool/2, info/0, available/0]).

info() ->
    #{name => <<"my_provider">>, version => <<"1.0.0">>}.

available() -> true.

list_tools(Opts) ->
    Context = maps:get(context, Opts, #{}),
    {ok, filter_tools_by_context(my_tools(), Context)}.

find_tool(Name, Opts) ->
    %% Find tool
    ...
```

### 3. Custom Middleware

```erlang
-module(my_middleware).
-behaviour(beamai_middleware).

-export([init/1, before_model/2, after_model/2]).

init(Opts) ->
    #{my_option => maps:get(my_option, Opts, default)}.

before_model(State, MwState) ->
    %% Execute before LLM call
    {update, #{my_key => my_value}}.

after_model(State, MwState) ->
    %% Execute after LLM call
    ok.
```

### 4. Custom Storage Backend

```erlang
-module(my_store).
-behaviour(beamai_store).

-export([init/1, put/4, get/3, delete/3, search/3]).

init(Config) ->
    %% Initialize storage
    {ok, State}.

put(State, Namespace, Key, Value) ->
    %% Store data
    {ok, NewState}.

%% ...other callbacks
```

---

## Deployment Architecture

### Single Node Deployment

```
┌─────────────────────────────────────────┐
│              Erlang Node                 │
│  ┌─────────────────────────────────────┐│
│  │          Application Sup             ││
│  │  ┌───────────┐  ┌───────────────┐   ││
│  │  │Agent Sup  │  │  Memory Sup   │   ││
│  │  │ ┌───────┐ │  │ ┌───────────┐ │   ││
│  │  │ │Agent 1│ │  │ │ETS Store  │ │   ││
│  │  │ ├───────┤ │  │ ├───────────┤ │   ││
│  │  │ │Agent 2│ │  │ │Checkpointer│ │   ││
│  │  │ └───────┘ │  │ └───────────┘ │   ││
│  │  └───────────┘  └───────────────┘   ││
│  └─────────────────────────────────────┘│
└─────────────────────────────────────────┘
```

### Distributed Deployment

```
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│   Node A        │  │   Node B        │  │   Node C        │
│  ┌───────────┐  │  │  ┌───────────┐  │  │  ┌───────────┐  │
│  │ Agent 1   │  │  │  │ Agent 3   │  │  │  │ Agent 5   │  │
│  │ Agent 2   │  │  │  │ Agent 4   │  │  │  │ Agent 6   │  │
│  └───────────┘  │  │  └───────────┘  │  │  └───────────┘  │
│                 │  │                 │  │                 │
│  Pregel Worker  │  │  Pregel Worker  │  │  Pregel Master  │
└────────┬────────┘  └────────┬────────┘  └────────┬────────┘
         │                    │                    │
         └────────────────────┼────────────────────┘
                              │
                      ┌───────┴───────┐
                      │   Erlang      │
                      │  Distribution │
                      └───────────────┘
```

---

## Additional Resources

- [README.md](../README.md) - Project overview and quick start
- [API_REFERENCE.md](API_REFERENCE.md) - API reference documentation
- [MIDDLEWARE.md](MIDDLEWARE.md) - Middleware system documentation
- Module documentation:
  - [beamai_core](../apps/beamai_core/README.md)
  - [beamai_agent](../apps/beamai_agent/README.md)
  - [beamai_deepagent](../apps/beamai_deepagent/README.md)
  - [beamai_tools](../apps/beamai_tools/README.md)
  - [beamai_llm](../apps/beamai_llm/README.md)
  - [beamai_memory](../apps/beamai_memory/README.md)
