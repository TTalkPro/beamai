# BeamAI ChatClient Architecture

## Design Philosophy

From LangGraph's "everything is a graph node" to SK's "everything is a callable function":

```
LangGraph:  Graph -> Node -> Edge -> State
SK:         ChatClient -> Plugin -> Function -> Service
BeamAI:     ChatClient -> Tool -> Service
```

Core shifts:
- **Graph is not the only orchestration** - just one way to compose Tools
- **Tool is the smallest unit** - self-describing, discoverable, LLM-callable
- **ChatClient is the runtime** - manages all capabilities (tools, services, filters)
- **Process is advanced orchestration** - Step/Event model (Phase 2)

## Architecture Layers

```
+--------------------------------------------------+
|                  beamai.erl (Facade)             |
+--------------------------------------------------+
|              beamai_chat_client.erl (Runtime)          |
+--------+----------+----------+-------------------+
| Tool   | Filter   | Context  | Settings          |
+--------+----------+----------+-------------------+
|            Service Layer                          |
|  chat_completion | embedding | prompt             |
+--------------------------------------------------+
|            Connector Layer                        |
|  openai | anthropic | zhipu | ollama             |
+--------------------------------------------------+
```

## Core Components

### Tool
The smallest unit of capability. A pure map with:
- `name` - tool identifier
- `handler` - execution body (fun/1, fun/2, {M,F}, {M,F,A})
- `description` - for LLM discovery
- `parameters` - JSON Schema for arguments
- `tag` - categorization label(s) for grouping
- `timeout` - execution timeout (default 30s)
- `retry` - retry configuration
- `metadata` - additional tool metadata

### Tool Module
A logical grouping of tools that implements `beamai_tool_behaviour`:
- `tool_info/0` - optional module-level metadata (description, tags)
- `tools/0` - returns list of tool_spec()
- `filters/0` - optional filters to register in ChatClient

### ChatClient
Immutable runtime container (Map, not process):
- Holds tools, services, filters, settings
- Provides invoke/chat/chat_with_tools APIs
- Tool schema generation for LLM integration
- Drives the tool calling loop

### Context
Execution context flowing through the call chain:
- Variables (key-value store)
- Message history
- ChatClient reference
- Trace entries

### Filter
Pre/post invocation hooks:
- `pre_invocation` - before tool execution
- `post_invocation` - after tool execution
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
            Execute Tools via ChatClient
                  |
                  v
            Append tool results to messages
                  |
                  v
            Loop back to LLM (max N iterations)
```

## Tool Registration

Tools can be registered to ChatClient in three ways:

1. **Direct Registration**: Add tool_spec() directly
   ```erlang
   K1 = beamai:add_tool(K0, #{
       name => <<"get_weather">>,
       handler => fun get_weather/1,
       description => <<"Get weather for a city">>,
       tag => <<"weather">>,
       parameters => #{city => #{type => string, required => true}}
   })
   ```

2. **Batch Registration**: Add multiple tools at once
   ```erlang
   K2 = beamai:add_tools(K1, [Tool1, Tool2, Tool3])
   ```

3. **Module Registration**: Load from a tool module
   ```erlang
   K3 = beamai:add_tool_module(K2, beamai_tool_file)
   ```

## Tag System

Tools can be tagged for categorization and discovery:
- Tools can have a single tag or multiple tags
- Query tools by tag: `beamai:tools_by_tag(ChatClient, Tag)`
- Common tags: `"io"`, `"file"`, `"shell"`, `"todo"`, `"human"`

## Phases

- **Phase 1**: ChatClient + Tool + Service (current implementation) ✅
- **Phase 2**: Process Framework (Step/Event orchestration)
- **Phase 3**: Agent (ChatClient + Prompt + Memory)
- **Phase 4**: Memory (Semantic Memory as Service)