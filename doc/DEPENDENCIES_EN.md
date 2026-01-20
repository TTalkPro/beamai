# BeamAI Framework Dependencies

This document describes the dependencies of the BeamAI Framework, including external dependencies and dependencies between internal modules.

## External Dependencies

### Runtime Dependencies

| Package | Version | Purpose |
|-------|------|------|
| [jsx](https://github.com/talentdeficit/jsx) | 3.1.0 | JSON encoding/decoding |
| [hackney](https://github.com/benoitc/hackney) | 1.20.1 | HTTP client (default backend) |
| [gun](https://github.com/ninenines/gun) | 2.1.0 | HTTP/1.1, HTTP/2, WebSocket client (optional backend) |
| [uuid](https://github.com/okeuday/uuid) | 2.0.6 | UUID generation (package name: uuid_erl) |
| [esqlite](https://github.com/mmzeeman/esqlite3) | 0.8.8 | SQLite database support (for persistent storage) |

### HTTP Backend Configuration

BeamAI supports two HTTP client backends, configurable via settings:

```erlang
%% Use Gun (default, supports HTTP/2)
application:set_env(beamai_core, http_backend, beamai_http_gun).

%% Use Hackney (stable and mature)
application:set_env(beamai_core, http_backend, beamai_http_hackney).
```

| Backend | Features | Use Cases |
|------|------|----------|
| gun | HTTP/2 support, async, modern design | Default backend, high-concurrency scenarios |
| hackney | Sync API, built-in connection pool, stable | Legacy code compatibility |

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

```
                         +-----------------+
                         | beamai_examples |
                         +--------+--------+
                                  | depends on all modules
        +-------------------------+-------------------------+
        |                         |                         |
        v                         v                         v
+---------------+       +-----------------+       +---------------+
|  beamai_mcp   |       |beamai_deepagent |       |  beamai_a2a   |
|(Protocol Ext) |       |  (Advanced Agent)|      | (A2A Protocol)|
+-------+-------+       +--------+--------+       +-------+-------+
        |                        |                        |
        |                        |                        v
        |                        |               +---------------+
        |                        |               | beamai_agent  |
        |                        |               |   (Agent)     |
        |                        |               +-------+-------+
        |                        |                       |
        +------------------------+-----------------------+
                                 |
    +----------------------------+----------------------------+
    |                 +----------+----------+                 |
    v                 v          v          v                 v
+-----------+   +-----------+  +-----------+  +-------------------+
|beamai_rag |   |beamai_llm |  | beamai_   |  |    beamai_tools   |
|  (RAG)    |   |  (LLM)    |  |  memory   |  | (Tools+Middleware)|
+-----+-----+   +-----+-----+  +-----+-----+  +---------+---------+
      |               |              |                  |
      |               | implements   | implements       |
      |               | Behaviour   | Behaviour        |
      |               |              |           +------+
      |               +------+-------+           |
      |                      |                   |
      |                      v                   |
      |         +------------------------+       |
      |         | Behaviour Interface    |<------+
      |         | Definition             |
      |         | (beamai_llm_behaviour, |
      |         |  beamai_buffer_behaviour)|
      |         +------------+-----------+
      |                      |
      +----------------------+
                             v
                  +---------------------------+
                  |       beamai_core         |  <- Base Layer
                  | (Types, Graph, Behaviour) |
                  +---------------------------+
                                 |
                                 v
                  +---------------------------+
                  |   Erlang/OTP + External   |
                  |       Dependencies        |
                  +---------------------------+

        - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        Runtime Optional Dependencies (injected via Adapter, not compile-time):
        beamai_tools ···> beamai_llm (llm_client)
        beamai_tools ···> beamai_memory (beamai_conversation_buffer)
```

**Dependency Direction Notes**:
- Solid arrows (->) indicate **compile-time dependencies** (top to bottom)
- Dashed arrows (···>) indicate **runtime optional dependencies** (injected via Adapter)
- `beamai_deepagent` **does not depend on** `beamai_agent`, they are parallel implementations
- `beamai_a2a` depends on `beamai_agent` (for Agent execution)
- `beamai_mcp` **optionally depends on** `beamai_agent` at the adapter layer (for tool conversion)
- `beamai_tools`, `beamai_llm`, `beamai_memory` are at the same level, all only depend on `beamai_core`
- `beamai_core` defines Behaviour interfaces, `beamai_llm` and `beamai_memory` implement these interfaces
- `beamai_tools` uses beamai_llm/memory at **runtime** via Adapter pattern, no compile-time dependency

### Application Dependency Details

#### beamai_core (Core Library)

**Dependencies**: No internal dependencies

**Provides**:
- Graph execution engine (Pregel model)
- State management (graph_state)
- Type definitions (beamai_types)
- Common utility functions (beamai_utils)
- JSON-RPC support (beamai_jsonrpc)
- SSE support (beamai_sse)
- **HTTP Client** (pluggable backends)
  - `beamai_http` - Unified API
  - `beamai_http_hackney` - Hackney backend (default)
  - `beamai_http_gun` - Gun backend (HTTP/2 support)
  - `beamai_http_pool` - Gun connection pool management
- **Behaviour Definitions** (for dependency decoupling)
  - `beamai_llm_behaviour` - LLM client interface
  - `beamai_buffer_behaviour` - Conversation buffer interface
  - `beamai_http_behaviour` - HTTP client interface

#### beamai_memory (Memory System)

**Dependencies**: No internal dependencies

**Provides**:
- Multi-type memory management
  - Semantic memory (beamai_semantic_memory)
  - Episodic memory (beamai_episodic_memory)
  - Procedural memory (beamai_procedural_memory)
  - Skill memory (beamai_skill_memory)
- Storage backends
  - ETS storage (beamai_store_ets)
  - SQLite storage (beamai_store_sqlite)
- Checkpoint system (beamai_checkpoint_*)
- Conversation buffer (beamai_conversation_buffer)

#### beamai_tools (Tool System + Middleware System)

**Dependencies**:
- beamai_core (Behaviour definitions, type definitions)

**Optional Dependencies** (decoupled via Adapter pattern):
- beamai_llm (LLM client, for intelligent tool filtering/simulation Middleware)
- beamai_memory (Conversation buffer, for summarization Middleware)

**Adapter Modules**:
- `beamai_llm_adapter` - Wraps LLM calls, defaults to `llm_client`
- `beamai_buffer_adapter` - Wraps conversation buffer, defaults to `beamai_conversation_buffer`

**Provides**:
- Tool definition and registration (beamai_tool, beamai_tool_registry)
- Tool provider (beamai_tool_provider)
- Tool security (beamai_tool_security)
- Built-in tools
  - File tools (beamai_tools_file)
  - Shell tools (beamai_tools_shell)
  - Todo tools (beamai_tools_todo)
  - Human interaction tools (beamai_tools_human)
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
  - OpenAI (llm_provider_openai)
  - Anthropic (llm_provider_anthropic)
  - Ollama (llm_provider_ollama)
  - Zhipu AI (llm_provider_zhipu)
- Message adapter (llm_message_adapter)
- Tool adapter (llm_tool_adapter)
- Response adapter (llm_response_adapter)

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
- beamai_core (headers: type definitions)
- beamai_llm (LLM calls)
- beamai_memory (checkpoints, memory management)
- beamai_tools (headers: tool definitions)

> **Note**: beamai_agent is the core orchestration layer, **does not depend on** beamai_mcp.
> MCP tools are integrated into Agent through beamai_mcp's adapter layer, not reverse dependency.

**Provides**:
- Agent lifecycle management (beamai_agent)
- Agent initialization (beamai_agent_init)
- Agent runner (beamai_agent_runner)
- Graph nodes
  - LLM node (beamai_llm_node)
  - Tool node (beamai_tool_node)
  - Middleware integration node (beamai_middleware_nodes)
- Callback system (beamai_callback_utils)

#### beamai_deepagent (Deep Agent System)

**Dependencies**:
- beamai_core
- beamai_llm
- beamai_memory
- beamai_tools (includes Middleware system)

**Provides**:
- Deep Agent core (beamai_deepagent)
- Planning system (beamai_deepagent_plan)
- Tool system
  - Filesystem tools (beamai_deepagent_fs_*)
  - Todo tools (beamai_deepagent_todo_*)
  - Human interaction tools (beamai_deepagent_human_*)
- Execution tracing (beamai_deepagent_trace)
- CLI UI (beamai_deepagent_cli_ui)

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
- beamai_tools (headers: tool definitions)
- beamai_agent (**optional**, only used by adapter layer)

**Provides**:
- MCP server (beamai_mcp_server)
- MCP client (beamai_mcp_client)
- Transport layer
  - HTTP transport
    - beamai_mcp_transport_http (Hackney backend, default)
    - beamai_mcp_transport_http_gun (Gun backend, HTTP/2 support)
  - SSE transport
    - beamai_mcp_transport_sse (Hackney backend, default)
    - beamai_mcp_transport_sse_gun (Gun backend, HTTP/2 support)
  - Stdio transport (beamai_mcp_transport_stdio)
- Tool proxy (beamai_mcp_tool_proxy)
- Agent adapter (beamai_mcp_adapter) - Converts MCP tools to Agent tools

**Transport Layer Backend Configuration**:

```erlang
%% Use Gun backend (default, supports HTTP/2)
Config = #{
    transport => http,  %% or sse
    url => <<"https://example.com/mcp">>
}.

%% Use Hackney backend
Config = #{
    transport => http,  %% or sse
    backend => hackney,
    url => <<"https://example.com/mcp">>
}.
```

> **Note**: MCP core functionality can run independently, does not depend on beamai_agent.
> Only use the adapter layer when you need to integrate MCP tools into the Agent system.

#### beamai_examples (Examples)

**Dependencies**:
- beamai_core
- beamai_llm
- beamai_memory
- beamai_tools
- beamai_agent
- beamai_deepagent
- beamai_a2a
- beamai_mcp

**Provides**:
- Simple Agent example (example_agent_simple)
- Deep Agent example (example_agent_deep)
- Interactive Agent example (example_agent_interactive)

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
| inets/httpc | HTTP client (backup) |

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

- **Erlang/OTP**: 25.0 or higher
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
