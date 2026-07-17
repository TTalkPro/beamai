# BeamAI Framework Dependencies

English | [中文](DEPENDENCIES.md)

This document describes the dependencies of the BeamAI Framework, including external dependencies and dependencies between internal modules.

## External Dependencies

### Runtime Dependencies

| Package | Version | Purpose |
|-------|------|------|
| [gun](https://github.com/ninenines/gun) | 2.1.0 | HTTP/1.1, HTTP/2, WebSocket client (the only built-in HTTP backend) |
| [uuid](https://github.com/okeuday/uuid) | 2.0.6 | UUID generation (package name: uuid_erl) |
| [esqlite](https://github.com/mmzeeman/esqlite3) | 0.8.8 | SQLite database support (for persistent storage) |

### HTTP Backend Configuration

The HTTP backend is pluggable via `beamai_http_behaviour`. The only built-in
implementation is `beamai_http_gun` (HTTP/2, async, backed by the three
purpose-shaped pools in `beamai_http_pool`); it is the default and needs no
configuration.

The abstraction remains so callers can swap the backend, primarily for testing:

```erlang
%% e.g. the fake backend used by the beamai_llm tests
application:set_env(beamai_core, http_backend, beamai_llm_fake_backend).
```

A custom backend must implement `beamai_http_behaviour`. Note that connection
pool semantics (`http_pool_short` and friends) are specific to the Gun backend
and are not auto-injected for other backends — see [HTTP_EN.md](HTTP_EN.md).

> A `beamai_http_hackney` (HTTP/1.1) backend once existed alongside Gun. It has
> been removed; everything now goes through Gun.

### Test Dependencies

| Package | Version | Purpose |
|-------|------|------|
| [meck](https://github.com/eproxus/meck) | 0.9.2 | Mock library for unit testing |

### Development Tools

| Plugin | Purpose |
|------|------|
| rebar3_proper | Property-based testing support |
| rebar3_ex_doc | Documentation generation |

## Internal Application Dependencies

### Dependency Hierarchy Diagram

This repository (beamai) contains only the three core apps; `beamai_tools` /
`beamai_mcp` / `beamai_a2a` / `beamai_rag` live in the extension project
[beamai_extra](https://github.com/TTalkPro/beamai_extra). The diagram below is the
combined view; the edges are taken from each app's `.app.src`:

```
                     extension project beamai_extra
    +---------------+  +---------------+  +---------------+  +-------------------+
    |  beamai_mcp   |  |  beamai_rag   |  |  beamai_a2a   |  |    beamai_tools   |
    | (MCP protocol)|  |    (RAG)      |  | (A2A protocol)|  | (Tools+Middleware)|
    +-------+-------+  +-------+-------+  +-------+-------+  +---------+---------+
            |                  |                  |                    |
            |                  |                  v                    |
            |                  |        +------------------+           |
            |                  |        |   beamai_agent   |           |
            |                  |        |  (SimpleAgent)   |           |
            |                  |        +--------+---------+           |
            |                  |                 |                     |
            |                  |                 v                     |
            |                  |        +------------------+           |
            |                  |        |    beamai_llm    |<----------+
            |                  |        |      (LLM)       |
            |                  |        +--------+---------+
            |                  |                 | implements beamai_chat_behaviour
            +------------------+-----------------+
                                                 v
                              +-----------------------------------+
                              |           beamai_core             |  <- Base Layer
                              |  Types / Kernel / Filter / HTTP   |
                              |  Behaviours: chat_behaviour,      |
                              |  chat_memory, memory_provider,    |
                              |  http_behaviour, tool_behaviour   |
                              +-----------------+-----------------+
                                                v
                              +-----------------------------------+
                              |  Erlang/OTP 27+ + external deps   |
                              |  (gun / uuid / esqlite / poolboy) |
                              +-----------------------------------+
```

**Dependency Direction Notes**:
- Arrows (->) indicate **compile-time dependencies** (the `applications` key of each `.app.src`)
- `beamai_agent` depends only on `beamai_core` + `beamai_llm` — **not** on `beamai_tools`
- `beamai_a2a` depends on `beamai_agent` (for Agent execution)
- `beamai_mcp` and `beamai_rag` depend only on `beamai_core`; `beamai_tools` depends on `beamai_core` + `beamai_llm`
- `beamai_core` defines the Behaviour interfaces and upper layers implement them; `beamai_core` is unaware of concrete implementations, decoupled via `{Module, Ref}` handle dispatch

### Application Dependency Details

#### beamai_core (Core Library)

**Dependencies**: No internal dependencies

**Provides**:
- Type definitions (beamai_types)
- Common utility functions (beamai_utils)
- JSON-RPC support (beamai_jsonrpc)
- SSE support (beamai_sse)
- **HTTP Client** (pluggable backends)
  - `beamai_http` - Unified API
  - `beamai_http_gun` - Gun backend (HTTP/2, the only built-in implementation)
  - `beamai_http_pool` - Gun connection pool management
- **Behaviour Definitions** (for dependency decoupling)
  - `beamai_chat_behaviour` - LLM chat interface (formerly beamai_llm_behaviour)
  - `beamai_chat_memory` - Conversation store interface (storage layer)
  - `beamai_memory_provider` - Agent memory policy interface (policy layer)
  - `beamai_http_behaviour` - HTTP client interface
  - `beamai_tool_behaviour` - Tool module interface

#### beamai_tools (Tool System + Middleware System)

**Dependencies**:
- beamai_core (Behaviour definitions, type definitions)
- beamai_llm

**Provides**:
- Entry point (beamai_tools)
- Tool security (beamai_tool_security)
- Built-in tools
  - File tools (beamai_tool_file)
  - Shell tools (beamai_tool_shell)
  - Todo tools (beamai_tool_todo)
  - Human interaction tools (beamai_tool_human)
- Middleware system
  - Middleware behaviour definition (beamai_middleware)
  - Middleware runner (beamai_middleware_runner)
  - Preset Middleware (beamai_middleware_presets)
  - Built-in Middleware (middleware_call_limit, middleware_summarization, etc.)

#### beamai_llm (LLM Integration)

**Dependencies**:
- beamai_core

**Provides**:
- LLM client (llm_client)
- Multi-provider support
  - OpenAI (beamai_llm_provider_openai)
  - Anthropic (beamai_llm_provider_anthropic)
  - DeepSeek (beamai_llm_provider_deepseek) - OpenAI compatible API
  - Ollama (beamai_llm_provider_ollama)
  - Zhipu AI (beamai_llm_provider_zhipu)
  - Alibaba Cloud DashScope (beamai_llm_provider_dashscope) - DashScope native API
- Message adapter (beamai_llm_message_adapter)
- Tool adapter (beamai_llm_tool_adapter)
- Response parser (beamai_llm_response_parser)

#### beamai_rag (RAG System)

**Dependencies**:
- beamai_core

**Provides**:
- RAG pipeline (beamai_rag)
- Document splitting (beamai_rag_splitter)
- Vector store (beamai_vector_store)
- Embedding support (beamai_embeddings)

#### beamai_agent (Agent System)

**Dependencies**:
- beamai_core (Kernel, Filter, types, memory behaviours)
- beamai_llm (LLM calls)

> **Note**: beamai_agent is the core orchestration layer and depends on **neither**
> beamai_tools **nor** beamai_mcp. Tools are registered into the kernel as maps /
> `beamai_tool`; MCP tools are integrated through beamai_mcp's adapter layer, not a
> reverse dependency.

**Provides**:
- Agent lifecycle (beamai_agent: new/run/stream/resume; beamai_agent_state: state and config resolution)
- ReAct tool loop (beamai_agent_tool_loop) — not a graph engine; the Agent has no notion of filters
- ToolCallingManager (tool-batch execution strategy; behaviour + `{Mod, Ref}` dispatch)
  - Behaviour definition (beamai_tool_calling_manager)
  - Concurrent implementation (beamai_concurrent_tool_calling_manager, default)
  - Sequential implementation (beamai_sequential_tool_calling_manager)
  - Batch worker (beamai_tool_batch_worker)
- Sub-agents (beamai_subagent_manager, beamai_agent_delegate)
- Callback system (beamai_agent_callbacks) — the Agent's only observation extension point, fired by the tool loop
- HITL interrupt/resume (beamai_agent_interrupt, beamai_agent_pause, beamai_pause_store[_ets])
- Branching and timeline (beamai_branch_store[_ets], beamai_timeline)

> **Memory is not listed here**: the Agent's cross-run memory is provided by
> `beamai_core`'s `beamai_memory_provider`, which the Agent calls explicitly in its tool
> loop (`memory` is a construction parameter orthogonal to `kernel`). See
> [MEMORY_EN.md](MEMORY_EN.md).

#### beamai_a2a (Agent-to-Agent Protocol)

**Dependencies**:
- beamai_core
- beamai_agent

**Provides**:
- A2A server (beamai_a2a_server)
- A2A client (beamai_a2a_client)
- Agent Card management (beamai_a2a_card)
- Authentication (beamai_a2a_auth)
- Task management (beamai_a2a_task)
- HTTP handler (beamai_a2a_http_handler)

#### beamai_mcp (MCP Protocol)

**Dependencies**:
- beamai_core (JSON-RPC, SSE support)
- gun (HTTP/SSE transports); cowboy (**optional**, only for the server-side handler)

**Provides**:
- MCP server (beamai_mcp_server, beamai_mcp_handler, beamai_mcp_session_registry;
  beamai_mcp_cowboy_handler is the cowboy entry point)
- MCP client (beamai_mcp_client, beamai_mcp_client_registry)
- Transport layer (dispatched by beamai_mcp_transport)
  - HTTP transport (beamai_mcp_transport_http_gun)
  - SSE transport (beamai_mcp_transport_sse_gun)
  - Stdio transport (beamai_mcp_transport_stdio)
- Protocol types and JSON-RPC (beamai_mcp_types, beamai_mcp_jsonrpc)
- Agent adapter (beamai_mcp_adapter) - Converts MCP tools to Agent tools

**Transport Layer Backend Configuration**:

```erlang
%% Use Gun backend (default, supports HTTP/2)
Config = #{
    transport => http,  %% or sse
    url => <<"https://example.com/mcp">>
}.
```

> The HTTP/SSE transports all run on Gun. The old `backend => hackney` config key is gone.

> **Note**: MCP core functionality can run independently, does not depend on beamai_agent.
> Only use the adapter layer when you need to integrate MCP tools into the Agent system.

#### beamai_examples (Examples, under `examples/`)

**Dependencies**: poolboy (beamai_core / beamai_llm / beamai_agent are supplied via
`ERL_LIBS`; see `examples/rebar.config`)

**Provides**:
- LLM config helper (example_llm_config)
- Kernel chat (example_kernel_chat)
- Streaming responses (example_streaming)
- Filter example (example_filter)
- Tool example (example_tool_refactored)

## Erlang/OTP Dependencies

BeamAI Framework uses the following Erlang/OTP standard libraries:

| Module | Purpose |
|------|------|
| gen_server | Process management |
| gen_statem | State machine |
| supervisor | Supervision tree |
| ets | In-memory storage |
| persistent_term | Persistent term storage |
| logger | Logging |
| crypto | Cryptographic functions |
| ssl | SSL/TLS support |
| json | JSON encoding/decoding (OTP 27+, replaces the former jsx dependency) |

## Installing Dependencies

```bash
# Fetch all dependencies
rebar3 get-deps

# Compile the project
rebar3 compile

# Run tests
rebar3 eunit
rebar3 ct
```

## Version Compatibility

- **Erlang/OTP**: 27.0 or higher (JSON encoding/decoding relies on the `json` module built into stdlib since OTP 27)
- **rebar3**: 3.20.0 or higher

## Optional Dependencies

The following dependencies are optional and can be enabled based on use cases:

| Dependency | Purpose | Enable Condition |
|------|------|----------|
| cowboy | HTTP server | A2A/MCP server functionality |
| gun | HTTP/2 client | High-performance HTTP requirements |
| eredis | Redis client | Redis storage backend |

## Updating Dependencies

Check compatibility before updating dependencies:

```bash
# Check outdated dependencies
rebar3 upgrade --all

# Update lock file
rebar3 lock
```
