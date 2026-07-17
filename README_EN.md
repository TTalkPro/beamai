# BeamAI - Erlang Agent Framework

[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-26%2B-red.svg)](https://www.erlang.org/)
[![Build](https://img.shields.io/badge/build-rebar3-brightgreen.svg)](https://rebar3.org/)

English | [中文](README.md)

A high-performance AI Agent framework core library based on Erlang/OTP, providing foundational capabilities for building Agents.

> **Note**: This project is the core library of the BeamAI framework, providing Kernel, Filter (incl. conversation memory), LLM Client, and SimpleAgent core features.
>
> Advanced features (Deep Agent, Process Framework orchestration, storage/snapshot engine, Tools Library, RAG, A2A/MCP protocols, etc.) have been moved to the [beamai_extra](https://github.com/TTalkPro/beamai_extra) extension project.

## Core vs Extension

### Core Project (This Repository)
Foundational infrastructure for building AI Agents (three core responsibilities):
- **beamai_core** - Kernel foundation: Context, Filter (onion-style around model), Tool construction and invocation
- **beamai_agent** - SimpleAgent: a primarily ReAct-based Agent framework (cross-turn memory via filter-memory, multi-turn conversations, callbacks, interrupt/resume)
- **beamai_llm** - Unified LLM client (supports OpenAI, Anthropic, DeepSeek, Zhipu, DashScope, Ollama)

### Extension Project ([beamai_extra](https://github.com/TTalkPro/beamai_extra))
Advanced features built on top of the core library:
- **Deep Agent** - Recursive planning Agent based on SubAgent architecture
- **Tools Library** - Common tools like File, Shell, HTTP, etc.
- **RAG** - Retrieval-Augmented Generation
- **Protocol Support** - A2A (Agent-to-Agent), MCP (Model Context Protocol)

## Features

- **Kernel/Tool Architecture**: Semantic tool registration and invocation system
  - Kernel core based on Semantic Kernel concepts (stateless, does not record messages)
  - Unified Tool definition and management
  - Onion-style Filter interception and security validation

- **Conversation Memory (Memory Filter)**: history decoupled from the Kernel
  - Each invoke passes only the latest message; history managed by the Memory Filter keyed by `conversation_id`
  - Pluggable storage backends (default ETS / sliding-window wrapper / custom behaviour)
  - See [docs/MEMORY_EN.md](docs/MEMORY_EN.md)

- **Unified LLM Client**: 6 providers with unified sync/streaming
  - OpenAI, Anthropic, DeepSeek, Zhipu, DashScope, Ollama
  - Multimodal input, Anthropic caching/Web Search/citations, rate-limit headers, Retry-After retries, unified error structure

- **Output Parser**: Structured output
  - JSON/XML/CSV parsing
  - Automatic retry mechanism

## Quick Start

### 1. Start Shell

```bash
export ZHIPU_API_KEY=your_key_here
rebar3 shell
```

### 2. LLM Call

```erlang
%% Create LLM configuration
LLM = beamai_chat_completion:create(zhipu, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY"))
}),

%% Send chat request
{ok, Response} = beamai_chat_completion:chat(LLM, [
    {role, user, content, <<"你好！"/utf8>>}
]),
```

### 3. Kernel + Tool (Tool Registration)

```erlang
%% Create Kernel
Kernel = beamai_kernel:new(),

%% Define Tool
SearchTool = #{
    name => <<"search">>,
    description => <<"Search for information">>,
    parameters => #{
        <<"query">> => #{type => string, required => true, description => <<"Search keywords">>}
    },
    handler => fun(#{<<"query">> := Query}, _Context) ->
        {ok, <<"Search result: ", Query/binary>>}
    end
},

%% Register tool
Kernel1 = beamai_kernel:add_tool(Kernel, SearchTool),

%% Invoke a single tool
{ok, Result, _NewCtx} = beamai_kernel:invoke_tool(Kernel1, <<"search">>, #{
    <<"query">> => <<"Erlang"/utf8>>
}, beamai_context:new()).
```

### 4. Filter (Onion-style Interception)

```erlang
%% A filter has 3 optional around hooks (around_chat/around_tool/around_turn).
%% Each around wraps a single invocation with one closure fun(Req, FCtx, Next) -> Resp;
%% pre/post logic lives in one place, and not calling Next short-circuits.
%% Filters are given once when the kernel is built; registration order is layer
%% order (earlier in the list = more outer).

%% One around_tool: arg validation (short-circuit) + double the result
ValidateTransform = beamai:filter(<<"validate_transform">>, #{
    around_tool => fun(#{args := #{a := A}, context := Ctx} = Req, _FCtx, Next) ->
        case A > 1000 of
            true ->
                %% Over limit: skip tool execution by not calling Next
                #{result => {error, <<"a exceeds limit">>}, context => Ctx};
            false ->
                %% Normal: enter inner layer then double the result
                #{result := Result} = Resp = Next(Req),
                case is_number(Result) of
                    true  -> Resp#{result => Result * 2};
                    false -> Resp
                end
        end
    end
}),

K0 = beamai:kernel(#{}, [ValidateTransform]),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})),

%% Invoke (3 + 5 = 8, doubled in post = 16)
{ok, 16, _} = beamai:invoke_tool(K1, <<"add">>, #{a => 3, b => 5}, beamai:context()).
```

See the [Filter docs](docs/FILTER_EN.md).

> **Process orchestration / state snapshots** have been moved to [beamai_extra](https://github.com/TTalkPro/beamai_extra) (Process Framework, storage/snapshot engine).

### 5. Output Parser (Structured Output)

```erlang
%% Create JSON parser
Parser = beamai_output_parser:json(#{
    schema => #{
        type => object,
        properties => #{
            <<"title">> => #{type => string},
            <<"count">> => #{type => integer}
        },
        required => [<<"title">>, <<"count">>]
    }
}),

%% Parse LLM response
{ok, Parsed} = beamai_output_parser:parse(Parser, LLMResponse).
```

## Architecture

### Application Structure

```
apps/
├── beamai_core/        # Core framework
│   ├── Kernel         # beamai_kernel, beamai_tool, beamai_context,
│   │                  # beamai_filter, beamai_prompt, beamai_result
│   ├── Memory Filter  # beamai_memory_filter (history keyed by conversation_id)
│   ├── HTTP           # beamai_http, beamai_http_gun, beamai_http_hackney,
│   │                  # beamai_http_pool
│   ├── Behaviours     # beamai_chat_behaviour, beamai_http_behaviour
│   └── Utils          # beamai_id, beamai_jsonrpc, beamai_sse, beamai_utils
│
├── beamai_llm/         # LLM client
│   ├── Chat           # beamai_chat_completion, beamai_llm_error
│   ├── Parser         # beamai_output_parser, beamai_parser_json
│   ├── Adapters       # beamai_llm_message_adapter, beamai_llm_response_parser, beamai_llm_tool_adapter
│   └── Providers      # OpenAI, Anthropic, DeepSeek, Zhipu, DashScope, Ollama
│
└── beamai_agent/       # SimpleAgent (ReAct)
    └── Agent          # beamai_agent, beamai_agent_state, beamai_agent_tool_loop,
                       # beamai_agent_callbacks, beamai_agent_interrupt
```

> **The process-orchestration engine and storage/snapshot engine** (formerly beamai_process / beamai_memory)
> have been moved to [beamai_extra](https://github.com/TTalkPro/beamai_extra) and are no longer part of this project.

### Dependency Relationships

```
┌───────────────────────┐ ┌───────────────────────┐
│   Agent Layer         │ │   LLM Layer           │
│  (beamai_agent)       │ │  (beamai_llm)         │
└───────────┬───────────┘ └───────────┬───────────┘
            │                         │
┌───────────┴─────────────────────────┴───────────┐
│   Core Layer                                     │
│  (beamai_core)                                   │
└─────────────────────────────────────────────────┘
```

> beamai_core is decoupled via Behaviour interfaces and `{Module, Ref}` dynamic dispatch,
> with no dependency on upper-layer apps. beamai_llm and beamai_agent are peers with no mutual dependency.

See [DEPENDENCIES_EN.md](docs/DEPENDENCIES_EN.md) for details.

## Core Concepts

### 1. Kernel Architecture

Kernel is BeamAI's core abstraction, managing Tool registration and invocation:

```erlang
%% Create Kernel instance
Kernel = beamai_kernel:new(),

%% Load tool from a Tool module
Kernel1 = beamai_kernel:add_tool_module(Kernel, beamai_tool_file),

%% Or add a single tool
Tool = #{
    name => <<"read_file">>,
    description => <<"Read file content">>,
    parameters => #{
        <<"path">> => #{type => string, required => true}
    },
    handler => fun(#{<<"path">> := Path}, _Ctx) ->
        file:read_file(Path)
    end
},
Kernel2 = beamai_kernel:add_tool(Kernel1, Tool),

%% Invoke the registered tool
{ok, Result, _NewCtx} = beamai_kernel:invoke_tool(Kernel2, <<"read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}, beamai_context:new()).
```

### 2. Conversation Memory (Memory Filter)

The Kernel itself is stateless and does not record messages; multi-turn conversation history is managed by the **Memory Filter** (`beamai_memory_filter`) keyed by `conversation_id`. Each invoke carries only the latest message; the filter injects history and persists deltas.

- Pluggable storage backends (default ETS / sliding-window wrapper / custom behaviour)
- SimpleAgent's cross-turn memory is built on this
- See [docs/MEMORY_EN.md](docs/MEMORY_EN.md)

## Configuration

### LLM Configuration

LLM configuration is created using `beamai_chat_completion:create/2`:

```erlang
%% Create LLM configuration
LLM = beamai_chat_completion:create(zhipu, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    temperature => 0.7
}).

%% Send request
{ok, Response} = beamai_chat_completion:chat(LLM, [
    {role, user, content, <<"你好"/utf8>>}
]).
```

**Supported Providers:**

| Provider | Module | API Mode | Description |
|----------|--------|----------|-------------|
| `anthropic` | beamai_llm_provider_anthropic | Anthropic | Anthropic Claude API |
| `openai` | beamai_llm_provider_openai | OpenAI | OpenAI API |
| `deepseek` | beamai_llm_provider_deepseek | OpenAI compatible | DeepSeek API |
| `zhipu` | beamai_llm_provider_zhipu | OpenAI compatible | Zhipu AI (GLM series) |
| `dashscope` | beamai_llm_provider_dashscope | DashScope native | Alibaba Cloud DashScope (Qwen series) |
| `ollama` | beamai_llm_provider_ollama | OpenAI compatible | Ollama local models |

### HTTP Backend Configuration

BeamAI supports both Gun and Hackney HTTP backends, with Gun as the default (supports HTTP/2).

```erlang
%% Configure in sys.config (optional); the Gun backend runs three
%% purpose-shaped pools (short requests / SSE streaming / async
%% long-polling) — set only the pools and keys you want to override.
%% The legacy http_pool key still works. See docs/HTTP_EN.md
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pools, #{
        http_pool_stream => #{max_connections_per_host => 20,
                              idle_timeout => 120000}
    }}
]}.
```

| Feature | Gun (default) | Hackney |
|---------|---------------|---------|
| HTTP/2 | Supported | Not supported |
| Connection Pool | Built-in purpose-shaped pools (beamai_http_pool instances) | Relies on hackney pool |
| TLS | Automatically uses system CA certificates | hackney default config |
| Use Case | Recommended for production | Legacy system compatibility |

## Documentation

### Core Documentation

- **[docs/API_REFERENCE_EN.md](docs/API_REFERENCE_EN.md)** - API Reference
- **[docs/FILTER_EN.md](docs/FILTER_EN.md)** - Filter System Documentation
- **[docs/MEMORY_EN.md](docs/MEMORY_EN.md)** - Conversation Memory (Memory Filter) documentation
- **[docs/HTTP_EN.md](docs/HTTP_EN.md)** - HTTP connection pools (purpose-shaped pools, config & tuning)
- **[docs/OUTPUT_PARSER.md](docs/OUTPUT_PARSER.md)** - Output Parser Guide
- **[docs/DEPENDENCIES_EN.md](docs/DEPENDENCIES_EN.md)** - Dependency Relationship Details

### Module Documentation

| Module | Description | Documentation |
|--------|-------------|---------------|
| **beamai_core** | Core framework: Kernel, Context, Filter, Tool, HTTP, Behaviours | [README](apps/beamai_core/README_EN.md) |
| **beamai_agent** | SimpleAgent: ReAct Agent framework (multi-turn, callbacks, interrupt/resume) | [README](apps/beamai_agent/README_EN.md) |
| **beamai_llm** | LLM client: 6 providers with unified sync/streaming; multimodal input, Anthropic caching/Web Search/citations, rate-limit headers, Retry-After retries, unified error structure | [README](apps/beamai_llm/README_EN.md) |

## Running Examples

```bash
# Compile
rebar3 compile

# Start Shell
rebar3 shell
```

## Project Statistics

| Metric | Count |
|--------|-------|
| **OTP Applications** | 3 (beamai_core, beamai_agent, beamai_llm) |
| **Source Modules** | ~73 |
| **Test Files** | ~38 |
| **Unit Tests** | ~380 |

### Running Tests

```bash
# Run all tests
rebar3 eunit

# Run tests for a specific app
rebar3 eunit --app=beamai_llm

# Run type checking
rebar3 dialyzer
```

## Performance

- Based on Erlang/OTP lightweight processes
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
