# LangGraph vs Semantic Kernel Comparison

## Core Philosophy

| Aspect | LangGraph | Semantic Kernel |
|--------|-----------|-----------------|
| Primary Abstraction | Graph (Nodes + Edges) | Kernel (Functions + Services) |
| Execution Model | State machine traversal | Function invocation |
| Composition | Graph topology | Plugin registration |
| LLM Integration | Node in graph | Service + Tool calling |
| State | Graph State (shared mutable) | Context (immutable, threaded) |
| Extensibility | Custom nodes | Plugins + Filters |

## Mapping: LangGraph -> Semantic Kernel

| LangGraph Concept | SK Equivalent | Notes |
|-------------------|---------------|-------|
| Node | KernelFunction | Function is more composable |
| Edge | Direct invocation / Process Step | No explicit edge needed for simple flows |
| Conditional Edge | Filter / Process logic | Filters for cross-cutting, Process for complex flows |
| Graph State | Context | Immutable, passed through call chain |
| Tool | KernelFunction | Same concept, better integrated |
| Agent | Agent (Phase 3) | Kernel + Prompt + Memory |
| Subgraph | Plugin | Logical grouping |
| Checkpoint | Process State (Phase 2) | Step-level persistence |

## Why Semantic Kernel for BeamAI?

### Problems with Graph-First Approach
1. **Over-engineering simple tasks** - A single LLM call doesn't need a graph
2. **Rigid topology** - Changes require graph restructuring
3. **State coupling** - All nodes share mutable state
4. **Tool integration** - Tools are second-class citizens in graph model

### Benefits of Function-First Approach
1. **Simplicity** - Most tasks are just function calls
2. **Composability** - Functions compose naturally (no graph overhead)
3. **LLM-native** - Functions map directly to tool schemas
4. **Incremental complexity** - Add Process/Agent only when needed
5. **Erlang-natural** - Maps and functions are idiomatic Erlang

## BeamAI Module Mapping

| Old Module | New Module | Rationale |
|-----------|-----------|-----------|
| `beamai_tools` | `beamai_function` | Function = Tool, unified concept |
| `beamai_tool_registry` | `beamai_plugin` + `beamai_kernel` | Kernel is the registry |
| `beamai_tool_provider` | `beamai_plugin:from_module/1` | Module convention |
| `llm_client` + `llm_provider_behaviour` | `beamai_connector` | Connector behaviour |
| `llm_tool_adapter` | `beamai_function:to_tool_schema/2` | Built-in schema generation |
| `beamai_middleware` | `beamai_filter` | Filter pipeline |
| `graph_state` | `beamai_context` | Immutable context |
| `beamai_agent_runner` (tool loop) | `beamai_kernel:invoke_chat_with_tools` | Built into kernel |

## Migration Strategy

1. New modules coexist with old ones
2. Phase 1 complete: validate new architecture works end-to-end
3. Gradually migrate callers from old to new modules
4. Remove old modules once all callers migrated
