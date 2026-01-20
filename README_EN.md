# Erlang Agent Framework

English | [中文](README.md)

A high-performance AI Agent application framework based on Erlang/OTP.

## Features

- **Simple Agent**: ReAct Agent based on Graph engine
  - Support for custom tools and system prompts
  - Built-in Scratchpad execution history
  - Checkpoint persistence support
  - Complete callback system

- **Coordinator Patterns**: Unified multi-Agent coordination
  - **Pipeline Pattern**: Sequential coordination (Researcher → Writer → Reviewer)
  - **Orchestrator Pattern**: Orchestration coordination (delegate, route, parallel calls to multiple workers)

- **Deep Agent**: Recursive planning Agent
  - Task Planning support
  - Self-Reflection support
  - Subtask distribution support

- **Output Parser**: Structured output
  - JSON Schema parsing
  - Automatic retry mechanism

## Quick Start

### 1. Start Shell

```bash
export ZHIPU_API_KEY=your_key_here
rebar3 shell
```

### 2. Simple Agent

```erlang
%% Create LLM configuration (must use llm_client:create/2)
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

%% Define tools
SearchTool = #{
    name => <<"search">>,
    description => <<"Search for information"/utf8>>,
    parameters => #{
        type => object,
        properties => #{
            <<"query">> => #{
                type => string,
                description => <<"Search keywords"/utf8>>
            }
        },
        required => [<<"query">>]
    },
    handler => fun(#{<<"query">> := Query}) ->
        %% Your search logic
        {ok, <<"Search result: ", Query/binary>>}
    end
},

%% Build tool list using Registry (recommended)
Tools = beamai_tool_registry:from_config(#{
    tools => [SearchTool],
    providers => [beamai_tool_provider_builtin]  %% Can add built-in tools
}),

%% Create Agent
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    system_prompt => <<"You are a helpful assistant."/utf8>>,
    tools => Tools,
    llm => LLM
}).

%% Run Agent
{ok, Result} = beamai_agent:run(Agent, <<"Search for Erlang tutorials"/utf8>>).

%% View result
Response = maps:get(final_response, Result).
```

### 3. Pipeline Pattern Coordinator (Sequential Coordination)

```erlang
%% Create LLM configuration
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

%% Create research team (Researcher → Writer → Reviewer)
{ok, Team} = beamai_agent:start_pipeline(<<"content_team">>, #{
    agents => [
        #{
            name => <<"researcher">>,
            system_prompt => <<"You are a researcher responsible for gathering materials."/utf8>>
        },
        #{
            name => <<"writer">>,
            system_prompt => <<"You are a writer responsible for writing articles."/utf8>>
        },
        #{
            name => <<"reviewer">>,
            system_prompt => <<"You are a reviewer responsible for quality checking."/utf8>>
        }
    ],
    llm => LLM
}).

%% Run task
{ok, Result} = beamai_agent:run(Team,
    <<"Research and write a 100-word introduction about Erlang's concurrency model."/utf8>>).
```

### 4. Orchestrator Pattern Coordinator (Orchestration Coordination)

```erlang
%% Create development team (orchestrator can delegate, route, and call multiple workers in parallel)
{ok, Team} = beamai_agent:start_orchestrator(<<"dev_team">>, #{
    agents => [
        #{
            name => <<"frontend">>,
            system_prompt => <<"You are a frontend development expert."/utf8>>
        },
        #{
            name => <<"backend">>,
            system_prompt => <<"You are a backend development expert."/utf8>>
        }
    ],
    llm => LLM  %% Reuse the same LLM configuration
}).

%% Run task
{ok, Result} = beamai_agent:run(Team,
    <<"Introduce RESTful API design from different perspectives."/utf8>>).
```

### 5. Deep Agent (Planning + Reflection)

```erlang
%% Create Deep Agent configuration
Config = beamai_deepagent:new(#{
    max_depth => 3,
    planning_enabled => true,
    reflection_enabled => true,
    system_prompt => <<"You are a research expert."/utf8>>,
    tools => [...],
    llm => LLM  %% Reuse the same LLM configuration
}).

%% Run complex task
{ok, Result} = beamai_deepagent:run(Config,
    <<"Analyze this codebase's architecture and provide optimization suggestions."/utf8>>).

%% View execution plan
Plan = beamai_deepagent:get_plan(Result).

%% View execution trace
Trace = beamai_deepagent:get_trace(Result).
```

## Architecture

### Application Structure

```
apps/
├── beamai_core/      # Core functionality + Persistence
│   ├── Behaviours   # beamai_behaviour, agent_persistence_behaviour
│   ├── HTTP         # beamai_http (Gun/Hackney client, Gun by default)
│   ├── Graph        # Graph execution engine
│   ├── Pregel       # Pregel distributed computing
│   └── Persistence      # agent_storage_ets, agent_storage_sup
│
├── beamai_llm/       # LLM client
│   └── Providers    # OpenAI, Anthropic, DeepSeek, Zhipu, Bailian, Ollama
│
├── beamai_rag/       # RAG functionality
│   ├── Embeddings   # Vector embeddings
│   └── Vector Store # Vector storage
│
├── beamai_memory/    # Memory and context storage
│   ├── Context      # Context management
│   └── Store        # ETS/SQLite storage backends
│
├── beamai_a2a/       # A2A protocol implementation
│   ├── Server       # A2A server
│   └── Client       # A2A client
│
├── beamai_mcp/       # MCP protocol implementation
│   ├── Server       # MCP server
│   └── Client       # MCP client
│
├── beamai_tools/    # Common tool library + Middleware system
│   ├── Tools        # Tool registration and execution
│   ├── Providers    # Tool sources (built-in, MCP)
│   └── Middleware   # Execution middleware (interception, enhancement)
│
├── beamai_agent/    # Simple Agent + Coordinator
│   ├── Graph Engine # Graph-based execution
│   ├── Scratchpad   # Execution history
│   ├── Checkpoint   # State persistence
│   ├── Callbacks    # Callback system
│   └── Coordinator  # Multi/Supervisor coordinator
│
└── beamai_deepagent/      # Deep Agent
    ├── Planning     # Task planning
    ├── Reflection   # Self-reflection
    └── Router      # Intelligent routing
```

### Dependency Relationships

```
┌─────────────────────────────────┐
│   Agent Implementation           │
│  (beamai_agent, beamai_deepagent)     │
└────────────┬────────────────────┘
             │
┌────────────┴────────────────────┐
│   Tools and Services Layer       │
│  (beamai_tools, beamai_llm,       │
│   beamai_rag, beamai_a2a, beamai_mcp) │
└────────────┬────────────────────┘
             │
┌────────────┴────────────────────┐
│   Core Layer                     │
│  (beamai_core, beamai_memory)     │
└─────────────────────────────────┘
```

See [DEPENDENCIES_EN.md](doc/DEPENDENCIES_EN.md) for details

## Core Concepts

### 1. Graph Execution Engine

beamai_agent uses a Graph engine to execute Agents:

```erlang
%% Graph definition
Graph = #{
    nodes => #{
        llm => {beamai_llm_node, #{}},
        tools => {beamai_tools_node, #{}}
    },
    edges => [
        {llm, tools, {condition, fun should_use_tools/1}}
    ]
}

%% Execute Graph
{ok, Result} = graph_runner:run(Graph, Input).
```

### 2. Scratchpad (Execution History)

Scratchpad records the execution process of each step:

```erlang
%% Get Scratchpad
{ok, Steps} = beamai_agent:get_scratchpad(Agent).

%% Each step contains:
%% - step_id: Step ID
%% - type: Step type (llm_call, tool_use, tool_result)
%% - content: Content
%% - timestamp: Timestamp
```

### 3. Checkpoint (State Persistence)

Save and restore Agent state:

```erlang
%% Save checkpoint
{ok, CheckpointId} = beamai_agent:save_checkpoint(Agent, #{
    metadata => #{tag => <<"v1">>}
}).

%% List all checkpoints
{ok, Checkpoints} = beamai_agent:list_checkpoints(Agent).

%% Restore from checkpoint
ok = beamai_agent:restore_from_checkpoint(Agent, CheckpointId).
```

### 4. Callbacks (Callback System)

Listen to Agent execution events, supporting 18 callback types:

```erlang
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    callbacks => #{
        %% LLM callbacks
        on_llm_start => fun(Prompts, Meta) ->
            io:format("LLM call started, message count: ~p~n", [length(Prompts)])
        end,
        on_llm_end => fun(Response, Meta) ->
            io:format("LLM response received~n")
        end,
        %% Tool callbacks
        on_tool_start => fun(ToolName, Args, Meta) ->
            io:format("Executing tool: ~ts~n", [ToolName])
        end,
        on_tool_end => fun(ToolName, Result, Meta) ->
            io:format("Tool completed: ~ts~n", [ToolName])
        end,
        %% Agent callbacks
        on_agent_finish => fun(Result, Meta) ->
            io:format("Agent completed~n")
        end
    }
}).

%% Dynamically set callbacks
ok = beamai_agent:set_callbacks(Agent, #{
    on_llm_start => fun(_Prompts, _Meta) -> ok end
}).

%% Send custom event
beamai_agent:emit_custom_event(Agent, my_event, #{value => 42}).
```

See [doc/CALLBACKS_EN.md](doc/CALLBACKS_EN.md) for details

## Configuration

### LLM Configuration

LLM configuration must be created using `llm_client:create/2` and can be reused across multiple Agents:

```erlang
%% Create LLM configuration (must use llm_client:create/2)
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    temperature => 0.7
}),

%% Configuration can be reused across multiple Agents
{ok, Agent1} = beamai_agent:start_link(<<"agent1">>, #{
    llm => LLM,
    tools => Tools1,
    system_prompt => <<"You are a research assistant."/utf8>>
}),

{ok, Agent2} = beamai_agent:start_link(<<"agent2">>, #{
    llm => LLM,
    tools => Tools2,
    system_prompt => <<"You are a writing assistant."/utf8>>
}).

%% Create new configuration based on existing one
HighTempLLM = llm_client:merge_config(LLM, #{temperature => 0.9}).
```

**Supported Providers:**

| Provider | Module | API Mode | Description |
|----------|--------|----------|-------------|
| `anthropic` | llm_provider_anthropic | Anthropic | Anthropic Claude API |
| `openai` | llm_provider_openai | OpenAI | OpenAI API |
| `deepseek` | llm_provider_deepseek | OpenAI compatible | DeepSeek API (deepseek-chat, deepseek-reasoner) |
| `zhipu` | llm_provider_zhipu | OpenAI compatible | Zhipu AI (GLM series) |
| `bailian` | llm_provider_bailian | DashScope native | Alibaba Cloud Bailian (Qwen series) |
| `ollama` | llm_provider_ollama | OpenAI compatible | Ollama local models |

### Agent Configuration Options

```erlang
Opts = #{
    %% Basic configuration
    id => <<"agent_id">>,
    system_prompt => Prompt,
    tools => [Tool1, Tool2],

    %% LLM configuration
    llm => LLMConfig,

    %% Execution configuration
    max_iterations => 10,       %% Maximum iterations
    timeout => 300000,          %% Timeout

    %% Checkpoint configuration
    enable_storage => true,     %% Enable storage
    auto_save => true,          %% Auto-save checkpoints

    %% Callback configuration
    callbacks => #{
        on_llm_start => fun(...), ...
    }
}.
```

### HTTP Backend Configuration

BeamAI supports both Gun and Hackney HTTP backends, with Gun as the default (supports HTTP/2).

```erlang
%% Configure in sys.config (optional)
{beamai_core, [
    %% HTTP backend selection: beamai_http_gun (default) or beamai_http_hackney
    {http_backend, beamai_http_gun},

    %% Gun connection pool configuration (only when using Gun backend)
    {http_pool, #{
        max_connections => 100,        %% Maximum connections
        connection_timeout => 30000    %% Connection timeout (milliseconds)
    }}
]}.
```

**Backend Comparison:**

| Feature | Gun (default) | Hackney |
|---------|---------------|---------|
| HTTP/2 | Supported | Not supported |
| Connection Pool | Built-in beamai_http_pool | Relies on hackney pool |
| TLS | Automatically uses system CA certificates | hackney default configuration |
| Use Case | Recommended for production | Legacy system compatibility |

## Advanced Features

### Custom Tools

```erlang
%% Tool definition (using parameters field)
#{name => <<"my_tool">>,
  description => <<"Tool description"/utf8>>,
  parameters => #{
      type => object,
      properties => #{
          <<"param1">> => #{type => string},
          <<"param2">> => #{type => integer}
      },
      required => [<<"param1">>]
  },
  handler => fun(Args, Context) ->
      %% Tool logic
      {ok, Result}
  end}

%% Register multiple tools using Registry
Tools = beamai_tool_registry:from_config(#{
    tools => [MyTool1, MyTool2],
    providers => [
        beamai_tool_provider_builtin,     %% Built-in tools
        {beamai_tool_provider_mcp, #{}}   %% MCP tools
    ]
}).
```

### Output Parser

```erlang
%% Define output schema
Schema = #{
    type => object,
    properties => #{
        <<"title">> => #{type => string},
        <<"count">> => #{type => integer},
        <<"items">> => #{
            type => array,
            items => #{type => string}
        }
    },
    required => [<<"title">>, <<"count">>]
}.

%% Use Parser
{ok, Parsed} = agent_output_parser:parse(
    LLMResponse,
    Schema,
    #{max_retries => 3}
).
```

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
| **beamai_core** | Core framework: Graph engine, Pregel distributed computing, behavior definitions | [README](apps/beamai_core/README_EN.md) |
| **beamai_llm** | LLM client: supports OpenAI, Anthropic, DeepSeek, Zhipu, Bailian, Ollama | [README](apps/beamai_llm/README_EN.md) |
| **beamai_agent** | Simple Agent: ReAct pattern, callback system, Checkpoint | [README](apps/beamai_agent/README_EN.md) |
| **beamai_deepagent** | Deep Agent: task planning, parallel execution, self-reflection | [README](apps/beamai_deepagent/README_EN.md) |
| **beamai_memory** | Memory management: Checkpoint, Store, time travel | [README](apps/beamai_memory/README_EN.md) |
| **beamai_tools** | Tool library + Middleware: Provider mechanism, tool registration, Middleware system | [README](apps/beamai_tools/README_EN.md) |
| **beamai_a2a** | A2A protocol: inter-Agent communication, server/client | [README](apps/beamai_a2a/README_EN.md) |
| **beamai_mcp** | MCP protocol: Model Context Protocol implementation | [README](apps/beamai_mcp/README_EN.md) |
| **beamai_rag** | RAG functionality: vector embeddings, similarity search | [README](apps/beamai_rag/README_EN.md) |

### Design and Implementation Documentation

- **[doc/DESIGN_PATTERNS.md](doc/DESIGN_PATTERNS.md)** - Design Patterns
- **[doc/OUTPUT_PARSER.md](doc/OUTPUT_PARSER.md)** - Output Parser Guide
- **[REFACTORING_REPORT.md](REFACTORING_REPORT.md)** - Refactoring Summary Report

## Running Examples

```bash
# Compile
rebar3 compile

# Start Shell
rebar3 shell

# Run interactive Deep Agent
examples/interactive_deep_agent.erl
```

## Project Statistics

- **Number of Applications**: 8
- **Lines of Code**: ~15,000 lines
- **Test Coverage**: Continuously improving
- **Documentation**: Complete API and architecture documentation

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

---

**Start building your AI Agent applications!**
