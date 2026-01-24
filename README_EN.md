# BeamAI - Erlang Agent Framework

English | [中文](README.md)

A high-performance AI Agent application framework based on Erlang/OTP.

## Features

- **Kernel/Plugin Architecture**: Semantic function registration and invocation system
  - Kernel core based on Semantic Kernel concepts
  - Plugin-based tool management and Middleware pipeline
  - Security validation and permission control

- **Process Framework**: Orchestratable process engine
  - Step definitions, conditional branching, parallel execution
  - Time travel and branch rollback
  - Event-driven with state snapshots

- **Simple Agent**: ReAct Agent based on tool loop
  - Custom tools and system prompts
  - Built-in Memory persistence
  - Complete callback system
  - Interrupt and resume support

- **Deep Agent**: Recursive planning Agent based on SubAgent architecture
  - Planner → Executor → Reflector pipeline
  - Parallel subtask execution
  - Coordinator multi-Agent orchestration

- **Graph Engine**: Graph computation based on LangGraph
  - Graph Builder/DSL
  - Pregel distributed computation model
  - State snapshots and conditional edges

- **Output Parser**: Structured output
  - JSON/XML/CSV parsing
  - Automatic retry mechanism

- **Protocol Support**: A2A and MCP
  - Agent-to-Agent communication protocol
  - Model Context Protocol integration

- **RAG**: Retrieval-Augmented Generation
  - Vector embeddings and similarity search
  - Text splitting

## Quick Start

### 1. Start Shell

```bash
export ZHIPU_API_KEY=your_key_here
rebar3 shell
```

### 2. Simple Agent (Basic Usage)

```erlang
%% Create LLM configuration (use beamai_chat_completion:create/2)
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

%% Create Agent state (pure function API)
{ok, State} = beamai_agent:new(#{
    system_prompt => <<"You are a helpful assistant.">>,
    llm => LLM
}),

%% Run Agent
{ok, Result, _NewState} = beamai_agent:run(State, <<"Hello!">>),

%% View result
Response = maps:get(final_response, Result).
```

### 3. Simple Agent (Multi-turn Conversation)

```erlang
%% Multi-turn conversation through state passing
{ok, State0} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"You are a memory assistant.">>
}),
{ok, _, State1} = beamai_agent:run(State0, <<"My name is John">>),
{ok, Result, _State2} = beamai_agent:run(State1, <<"What's my name?">>).
%% Agent will remember user's name is John
```

### 4. Simple Agent (Using Kernel + Plugin for Tools)

```erlang
%% Create Kernel
Kernel = beamai_kernel:new(),

%% Register tools via Plugin module
Kernel1 = beamai_kernel:add_plugin_from_module(Kernel, beamai_plugin_shell),

%% Or manually define tool functions
SearchFn = beamai_function:new(<<"search">>, <<"Search for information">>,
    fun(#{<<"query">> := Query}, _Context) ->
        {ok, <<"Search result: ", Query/binary>>}
    end,
    #{parameters => #{
        type => object,
        properties => #{
            <<"query">> => #{type => string, description => <<"Search keywords">>}
        },
        required => [<<"query">>]
    }}),

Kernel2 = beamai_kernel:add_plugin(Kernel1, <<"search_plugin">>, [SearchFn]),

%% Get tool specs for Agent use
Tools = beamai_kernel:get_tool_specs(Kernel2),

%% Create Agent with tools
{ok, State} = beamai_agent:new(#{
    system_prompt => <<"You are a search assistant.">>,
    tools => Tools,
    llm => LLM
}),

{ok, Result, _} = beamai_agent:run(State, <<"Search for Erlang tutorials">>).
```

### 5. Simple Agent (With Memory Persistence)

```erlang
%% Create storage backend
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% Create Agent with Memory (checkpoint auto-saved)
{ok, State0} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"You are a persistent assistant.">>,
    storage => Memory
}),

%% Conversation (checkpoint auto-saved)
{ok, _, State1} = beamai_agent:run(State0, <<"Remember: the password is 12345">>),
{ok, _, _State2} = beamai_agent:run(State1, <<"OK">>),

%% Later restore session
{ok, RestoredState} = beamai_agent:restore_from_memory(#{llm => LLM}, Memory),
{ok, Result, _} = beamai_agent:run(RestoredState, <<"What's the password?">>).
%% Agent will remember the password is 12345
```

### 6. Deep Agent (SubAgent Orchestration)

```erlang
%% Create Deep Agent configuration (new/1 returns config map directly)
Config = beamai_deepagent:new(#{
    llm => LLM,
    max_depth => 3,
    planning_enabled => true,
    reflection_enabled => true,
    system_prompt => <<"You are a research expert.">>,
    %% Use Plugin modules for tools
    plugins => [beamai_plugin_file, beamai_plugin_shell]
}),

%% Run complex task (Planner -> Executor -> Reflector)
{ok, Result} = beamai_deepagent:run(Config,
    <<"Analyze this codebase's architecture and provide optimization suggestions.">>),

%% View execution plan and trace
Plan = beamai_deepagent:get_plan(Result),
Trace = beamai_deepagent:get_trace(Result).
```

### 7. Process Framework (Process Orchestration)

```erlang
%% Build process using Process Builder
{ok, Process} = beamai_process_builder:new(<<"research_pipeline">>)
    |> beamai_process_builder:add_step(<<"research">>, #{
        handler => fun(Input, _Ctx) -> {ok, do_research(Input)} end
    })
    |> beamai_process_builder:add_step(<<"write">>, #{
        handler => fun(Input, _Ctx) -> {ok, do_write(Input)} end
    })
    |> beamai_process_builder:add_step(<<"review">>, #{
        handler => fun(Input, _Ctx) -> {ok, do_review(Input)} end
    })
    |> beamai_process_builder:build(),

%% Execute process
{ok, Result} = beamai_process_executor:run(Process, #{
    task => <<"Research Erlang concurrency model">>
}).
```

### 8. Output Parser (Structured Output)

```erlang
%% Create JSON parser
Parser = beamai_output_parser:json(#{
    schema => #{
        type => object,
        properties => #{
            <<"title">> => #{type => string},
            <<"count">> => #{type => integer},
            <<"items">> => #{type => array, items => #{type => string}}
        },
        required => [<<"title">>, <<"count">>]
    }
}),

%% Parse LLM response
{ok, Parsed} = beamai_output_parser:parse(Parser, LLMResponse).

%% Parse with retry
{ok, Parsed} = beamai_output_parser:parse_with_retry(Parser, LLMResponse, #{
    max_retries => 3
}).
```

## Architecture

### Application Structure

```
apps/
├── beamai_core/        # Core framework
│   ├── Kernel         # beamai_kernel, beamai_function, beamai_context,
│   │                  # beamai_filter, beamai_prompt, beamai_result
│   ├── Process        # beamai_process, beamai_process_builder,
│   │                  # beamai_process_runtime, beamai_process_step,
│   │                  # beamai_process_executor, beamai_process_event
│   ├── HTTP           # beamai_http, beamai_http_gun, beamai_http_hackney,
│   │                  # beamai_http_pool
│   ├── Behaviours     # beamai_llm_behaviour, beamai_http_behaviour,
│   │                  # beamai_step_behaviour, beamai_process_store_behaviour
│   └── Utils          # beamai_id, beamai_jsonrpc, beamai_sse, beamai_utils
│
├── beamai_graph/       # Graph computation engine
│   ├── Core           # graph, graph_node, graph_edge
│   ├── Builder        # graph_builder, graph_dsl
│   ├── Runner         # graph_runner, graph_snapshot
│   ├── State          # graph_state, graph_state_reducer, graph_command
│   └── Pregel         # pregel, pregel_master, pregel_worker, pregel_vertex
│
├── beamai_plugin/      # Plugin system
│   ├── Core           # beamai_plugins, beamai_plugin_behaviour, beamai_tool
│   ├── Middleware     # beamai_middleware, beamai_middleware_runner,
│   │                  # middleware_call_limit, middleware_tool_retry
│   ├── Security       # beamai_tool_security
│   └── Plugins        # beamai_plugin_file, beamai_plugin_shell,
│                      # beamai_plugin_human, beamai_plugin_todo
│
├── beamai_llm/         # LLM client
│   ├── Chat           # beamai_chat_completion
│   ├── Parser         # beamai_output_parser, beamai_parser_json
│   ├── Adapters       # llm_message_adapter, llm_response_adapter, llm_tool_adapter
│   └── Providers      # OpenAI, Anthropic, DeepSeek, Zhipu, Bailian, Ollama
│
├── beamai_agent/       # Agent implementation
│   ├── Core           # beamai_agent, beamai_agent_state, beamai_agent_callbacks
│   ├── Memory         # beamai_agent_memory
│   ├── Execution      # beamai_agent_tool_loop, beamai_agent_interrupt
│   └── Process Agent  # beamai_process_agent, beamai_process_agent_llm_step,
│                      # beamai_process_agent_tool_step
│
├── beamai_deepagent/   # Deep Agent (SubAgent architecture)
│   ├── Core           # beamai_deepagent, beamai_deepagent_plan,
│   │                  # beamai_deepagent_dependencies, beamai_deepagent_trace
│   └── SubAgents      # beamai_deepagent_planner, beamai_deepagent_executor,
│                      # beamai_deepagent_reflector, beamai_deepagent_parallel,
│                      # beamai_deepagent_coordinator
│
├── beamai_memory/      # Memory and context storage
│   ├── Context        # Context management
│   ├── Store          # ETS/SQLite storage backends
│   └── Snapshot       # Snapshots, branching, time travel
│
├── beamai_a2a/         # A2A protocol implementation
│   ├── Server         # A2A server
│   └── Client         # A2A client
│
├── beamai_mcp/         # MCP protocol implementation
│   ├── Server         # MCP server
│   └── Client         # MCP client
│
└── beamai_rag/         # RAG functionality
    ├── Embeddings     # Vector embeddings
    └── Vector Store   # Vector storage
```

### Dependency Relationships

```
┌─────────────────────────────────────┐
│   Agent Implementation Layer         │
│  (beamai_agent, beamai_deepagent)   │
└────────────────┬────────────────────┘
                 │
┌────────────────┴────────────────────┐
│   Services Layer                     │
│  (beamai_llm, beamai_plugin,        │
│   beamai_rag, beamai_a2a, beamai_mcp)│
└────────────────┬────────────────────┘
                 │
┌────────────────┴────────────────────┐
│   Core Layer                         │
│  (beamai_core, beamai_graph,        │
│   beamai_memory)                     │
└─────────────────────────────────────┘
```

See [DEPENDENCIES_EN.md](doc/DEPENDENCIES_EN.md) for details

## Core Concepts

### 1. Kernel Architecture

Kernel is BeamAI's core abstraction, managing Plugin and Function registration and invocation:

```erlang
%% Create Kernel instance
Kernel = beamai_kernel:new(),

%% Load plugin from module
Kernel1 = beamai_kernel:add_plugin_from_module(Kernel, beamai_plugin_file),

%% Invoke registered function
{ok, Result} = beamai_kernel:invoke(Kernel1, <<"file-read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}).
```

### 2. Process Framework

Orchestratable process engine supporting step definitions, branching, parallelism, and time travel:

```erlang
%% Build process
Process = beamai_process_builder:new(<<"my_process">>),
Process1 = beamai_process_builder:add_step(Process, <<"step1">>, #{
    handler => fun(Input, Ctx) -> {ok, transform(Input)} end
}),
{ok, Built} = beamai_process_builder:build(Process1),

%% Execute
{ok, Result} = beamai_process_executor:run(Built, InitialInput).
```

### 3. Graph Execution Engine

Graph computation engine based on LangGraph concepts (in beamai_graph app):

```erlang
%% Create graph
Builder = graph_builder:new(),
Builder1 = graph_builder:add_node(Builder, start, fun(State) ->
    {ok, State#{step => 1}}
end),
Builder2 = graph_builder:add_node(Builder1, finish, fun(State) ->
    {ok, State}
end),
Builder3 = graph_builder:add_edge(Builder2, start, finish),
Builder4 = graph_builder:set_entry_point(Builder3, start),
Builder5 = graph_builder:set_finish_point(Builder4, finish),

{ok, Graph} = graph_builder:compile(Builder5),
{ok, Result} = graph_runner:run(Graph, #{}).
```

### 4. Memory Persistence

Use beamai_memory for session persistence and time travel:

```erlang
%% Create Memory
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% Create Agent with storage (checkpoint auto-saved)
{ok, State} = beamai_agent:new(#{llm => LLM, storage => Memory}),
{ok, _, NewState} = beamai_agent:run(State, <<"Hello">>),

%% Restore session from Memory
{ok, RestoredState} = beamai_agent:restore_from_memory(#{llm => LLM}, Memory).
```

### 5. Callbacks

Listen to Agent execution events:

```erlang
{ok, State} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"You are an assistant">>,
    callbacks => #{
        on_llm_start => fun(Prompts, Meta) ->
            io:format("LLM call started~n")
        end,
        on_tool_start => fun(ToolName, Args, Meta) ->
            io:format("Executing tool: ~ts~n", [ToolName])
        end,
        on_agent_finish => fun(Result, Meta) ->
            io:format("Agent completed~n")
        end
    }
}).
```

## Configuration

### LLM Configuration

LLM configuration is created using `beamai_chat_completion:create/2`:

```erlang
%% Create LLM configuration
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    temperature => 0.7
}),

%% Configuration can be reused across multiple Agents
{ok, State1} = beamai_agent:new(#{llm => LLM, system_prompt => <<"Research assistant">>}),
{ok, State2} = beamai_agent:new(#{llm => LLM, system_prompt => <<"Writing assistant">>}).
```

**Supported Providers:**

| Provider | Module | API Mode | Description |
|----------|--------|----------|-------------|
| `anthropic` | llm_provider_anthropic | Anthropic | Anthropic Claude API |
| `openai` | llm_provider_openai | OpenAI | OpenAI API |
| `deepseek` | llm_provider_deepseek | OpenAI compatible | DeepSeek API |
| `zhipu` | llm_provider_zhipu | OpenAI compatible | Zhipu AI (GLM series) |
| `bailian` | llm_provider_bailian | DashScope native | Alibaba Cloud Bailian (Qwen series) |
| `ollama` | llm_provider_ollama | OpenAI compatible | Ollama local models |

### HTTP Backend Configuration

BeamAI supports both Gun and Hackney HTTP backends, with Gun as the default (supports HTTP/2).

```erlang
%% Configure in sys.config (optional)
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pool, #{
        max_connections => 100,
        connection_timeout => 30000
    }}
]}.
```

| Feature | Gun (default) | Hackney |
|---------|---------------|---------|
| HTTP/2 | Supported | Not supported |
| Connection Pool | Built-in beamai_http_pool | Relies on hackney pool |
| TLS | Automatically uses system CA certificates | hackney default config |
| Use Case | Recommended for production | Legacy system compatibility |

## Documentation

### Core Documentation

- **[doc/API_REFERENCE_EN.md](doc/API_REFERENCE_EN.md)** - API Reference
- **[doc/MIDDLEWARE_EN.md](doc/MIDDLEWARE_EN.md)** - Middleware System Documentation
- **[doc/CALLBACKS_EN.md](doc/CALLBACKS_EN.md)** - Callback System Documentation
- **[doc/ARCHITECTURE_EN.md](doc/ARCHITECTURE_EN.md)** - Architecture Design
- **[DEPENDENCIES_EN.md](doc/DEPENDENCIES_EN.md)** - Dependency Relationship Details

### Module Documentation

| Module | Description | Documentation |
|--------|-------------|---------------|
| **beamai_core** | Core framework: Kernel, Process Framework, HTTP, Behaviours | [README](apps/beamai_core/README_EN.md) |
| **beamai_graph** | Graph engine: graph building, execution, Pregel distributed computing | [README](apps/beamai_graph/README.md) |
| **beamai_plugin** | Plugin system: tool management, Middleware, security validation | [README](apps/beamai_plugin/README.md) |
| **beamai_llm** | LLM client: supports OpenAI, Anthropic, DeepSeek, Zhipu, Bailian, Ollama | [README](apps/beamai_llm/README_EN.md) |
| **beamai_agent** | Agent implementation: ReAct pattern, callback system, Process Agent | [README](apps/beamai_agent/README.md) |
| **beamai_deepagent** | Deep Agent: SubAgent orchestration, task planning, parallel execution, self-reflection | [README](apps/beamai_deepagent/README_EN.md) |
| **beamai_memory** | Memory management: Checkpoint, Store, time travel, branching | [README](apps/beamai_memory/README_EN.md) |
| **beamai_a2a** | A2A protocol: inter-Agent communication, server/client | [README](apps/beamai_a2a/README_EN.md) |
| **beamai_mcp** | MCP protocol: Model Context Protocol implementation | [README](apps/beamai_mcp/README_EN.md) |
| **beamai_rag** | RAG functionality: vector embeddings, similarity search | [README](apps/beamai_rag/README_EN.md) |

## Running Examples

```bash
# Compile
rebar3 compile

# Start Shell
rebar3 shell
```

## Project Statistics

- **Number of Applications**: 10
- **Source Files**: ~180 modules
- **Lines of Code**: ~62,000 lines
- **Test Coverage**: Continuously improving

## Performance

- Based on Erlang/OTP lightweight processes
- Graph engine optimizes execution paths
- Concurrent tool invocations
- HTTP connection pool (Gun, supports HTTP/2)
- ETS high-speed storage

## Design Principles

- **Simple**: Clear API, easy to understand
- **Modular**: Single responsibility for each module
- **Extensible**: Behaviour design, easy to customize
- **High Performance**: Leverages Erlang concurrency features
- **Observable**: Comprehensive logging, tracing, monitoring

## License

Apache-2.0

## Contributing

Issues and Pull Requests are welcome!
