# BeamAI Kernel Architecture

## Design Philosophy

From LangGraph's "everything is a graph node" to SK's "everything is a callable function":

```
LangGraph:  Graph -> Node -> Edge -> State
SK:         Kernel -> Plugin -> Function -> Service
```

Core shifts:
- **Graph is not the only orchestration** - just one way to compose Functions
- **Function is the smallest unit** - self-describing, discoverable, LLM-callable
- **Kernel is the runtime** - manages all capabilities (functions, services, filters)
- **Process is advanced orchestration** - Step/Event model (Phase 2)

## Architecture Layers

```
+--------------------------------------------------+
|                  beamai.erl (Facade)             |
+--------------------------------------------------+
|              beamai_kernel.erl (Runtime)          |
+--------+----------+----------+-------------------+
| Plugin | Function | Filter   | Context           |
+--------+----------+----------+-------------------+
|            Service Layer                          |
|  chat_completion | embedding | prompt             |
+--------------------------------------------------+
|            Connector Layer                        |
|  openai | anthropic | zhipu | ollama             |
+--------------------------------------------------+
```

## Core Components

### KernelFunction
The smallest unit of capability. A pure map with:
- `name` - function identifier
- `handler` - execution body (fun/1, fun/2, {M,F}, {M,F,A}, {service, Type, Config})
- `description` - for LLM discovery
- `parameters` - JSON Schema for arguments
- `return_type` - return value description

### Plugin
A logical grouping of functions. Pure data, no process:
- `name` - plugin identifier
- `functions` - list of function_def()
- Module convention via `plugin_info/0` and `functions/0` callbacks

### Kernel
Immutable runtime container (Map, not process):
- Holds plugins, services, filters, settings
- Provides invoke/chat/chat_with_tools APIs
- Tool schema generation for LLM integration

### Context
Execution context flowing through the call chain:
- Variables (key-value store)
- Message history
- Kernel reference
- Trace entries

### Filter
Pre/post invocation hooks:
- `pre_invocation` - before function execution
- `post_invocation` - after function execution
- `pre_chat` - before LLM call
- `post_chat` - after LLM call
- Priority-ordered pipeline

### Connector
Provider-specific LLM integration:
- Behaviour with `chat/3`, `chat_stream/3`, `embedding/3` callbacks
- Implementations: OpenAI, Anthropic, Zhipu, Ollama

## Tool Calling Loop

```
User Message -> LLM (with tool schemas)
                  |
                  v
            Tool Calls? --No--> Return Response
                  |
                 Yes
                  |
                  v
            Execute Functions via Kernel
                  |
                  v
            Append tool results to messages
                  |
                  v
            Loop back to LLM (max N iterations)
```

## Phases

- **Phase 1**: Kernel + Plugin + Function (this implementation)
- **Phase 2**: Process Framework (Step/Event orchestration)
- **Phase 3**: Agent (Kernel + Prompt + Memory)
- **Phase 4**: Memory (Semantic Memory as Service)
