# BeamAI Core

English | [中文](README.md)

The core module of the BeamAI framework, providing the Kernel architecture, Filter/conversation memory, HTTP client, and behaviour definitions.

## Module Overview

### Kernel Subsystem

Core abstraction based on Semantic Kernel concepts, managing Tool registration and invocation:

- **beamai_kernel** - Kernel core, manages Tool registration and invocation (stateless, stores no messages)
- **beamai_tool** - Tool definitions, wraps callable tool functions
- **beamai_tool_behaviour** - Tool module behavior interface
- **beamai_context** - Context: carries agent state vars, conversation id, kernel ref, trace (stores no messages/history)
- **beamai_filter** / **beamai_filter_chain** - Onion-style filters (one filter bundles around_chat/around_tool hooks, with a per-filter isolated private context) wrapping tool execution and LLM calls (see [docs/FILTER_EN.md](../../docs/FILTER_EN.md))
- **beamai_prompt** - Prompt template management
- **beamai_result** - Tool call result types

### Conversation Memory Subsystem

History storage and injection, decoupled from the Kernel and keyed by `conversation_id` (see [docs/MEMORY_EN.md](../../docs/MEMORY_EN.md)):

- **beamai_chat_memory** - ChatMemory storage behaviour + dispatch API (handle `{Module, Ref}`)
- **beamai_chat_memory_ets** - Default ETS conversation store
- **beamai_memory_filter** - Memory Filter (kernel-level: around_chat stores delta + expands history before the call, stores reply after)
- **beamai_memory_provider** - Agent memory policy behaviour (history/append/prepare/clear)
- **beamai_memory_provider_default** - Default policy implementation (wraps a store; `new/2` adds an optional sliding window)

### LLM Subsystem

Unified abstraction layer for LLM responses:

- **beamai_llm_response** - Unified LLM response accessors (content, tool_calls, usage, etc.)

### HTTP Subsystem

Pluggable HTTP client (backend swappable via `beamai_http_behaviour`; Gun is the only built-in):

- **beamai_http** - Unified HTTP client interface
- **beamai_http_gun** - Gun HTTP/2 backend implementation (the only built-in backend)
- **beamai_http_pool** - HTTP connection pool management

### Behaviour Definitions

Framework behavior interface definitions:

- **beamai_chat_behaviour** - LLM chat interface (formerly beamai_llm_behaviour)
- **beamai_chat_memory** - Conversation store interface (storage layer: mem_get/mem_add/mem_clear)
- **beamai_http_behaviour** - HTTP backend behavior interface
- **beamai_memory_provider** - Agent memory policy interface (policy layer: history/append/prepare/clear)
- **beamai_tool_behaviour** - Tool module behavior interface

### Utilities and Protocols

- **beamai_id** - Unique ID generation (UUID)
- **beamai_jsonrpc** - JSON-RPC 2.0 encoding/decoding
- **beamai_sse** - Server-Sent Events (SSE) support
- **beamai_utils** - General utility functions

### Application Entry

- **beamai** - Main entry module
- **beamai_core_app** - OTP application callback
- **beamai_core_sup** - Top-level supervisor tree

## API Documentation

### beamai_kernel

```erlang
%% Create Kernel instance (filters given once; registration order is layer order:
%% earlier in the list = more outer)
beamai_kernel:new() -> kernel().
beamai_kernel:new(Settings) -> kernel().
beamai_kernel:new(Settings, Filters) -> kernel().       %% onion-style filter, see docs/FILTER_EN.md
%% Conversation memory = put the memory filter first in Filters, see docs/MEMORY_EN.md:
%% beamai_kernel:new(#{}, [beamai_memory_filter:memory_filter(Store)])

%% Add Tools
beamai_kernel:add_tool(Kernel, ToolSpec) -> kernel().
beamai_kernel:add_tools(Kernel, [ToolSpec]) -> kernel().
beamai_kernel:add_tool_module(Kernel, Module) -> kernel().

%% Add services
beamai_kernel:add_service(Kernel, Service) -> kernel().

%% Invoke API (kernel is single-shot only; the ReAct tool-calling loop lives in beamai_agent)
beamai_kernel:invoke_tool(Kernel, ToolName, Args, Context) -> {ok, Result, Context} | {error, Reason}.
beamai_kernel:invoke_chat(Kernel, Messages, Opts) -> {ok, Response, Context} | {error, Reason}.

%% Query API
beamai_kernel:get_tool(Kernel, Name) -> {ok, Tool} | error.
beamai_kernel:list_tools(Kernel) -> [Tool].
beamai_kernel:get_tools_by_tag(Kernel, Tag) -> [Tool].
beamai_kernel:get_tool_specs(Kernel) -> [ToolSpec].
beamai_kernel:get_tool_schemas(Kernel) -> [Schema].
beamai_kernel:get_tool_schemas(Kernel, Provider) -> [Schema].
beamai_kernel:get_service(Kernel) -> {ok, Service} | error.
```

### beamai_tool

```erlang
%% Create tool
beamai_tool:new(Name, Handler, Opts) -> tool_spec().

%% Name: Tool name (binary)
%% Handler: fun(Args, Context) -> {ok, Result} | {error, Reason}
%% Opts: #{description => Description, parameters => Schema, ...}
```

## Usage Examples

### Kernel + Tool

```erlang
%% Create Kernel
Kernel = beamai_kernel:new(),

%% Define tool
ReadFile = beamai_tool:new(
    <<"read_file">>,
    fun(#{<<"path">> := Path}, _Ctx) ->
        case file:read_file(Path) of
            {ok, Content} -> {ok, Content};
            {error, Reason} -> {error, Reason}
        end
    end,
    #{
        description => <<"Read file contents">>,
        parameters => #{
            type => object,
            properties => #{
                <<"path">> => #{type => string, description => <<"File path">>}
            },
            required => [<<"path">>]
        }
    }
),

%% Register to Kernel
Kernel1 = beamai_kernel:add_tools(Kernel, [ReadFile]),

%% Invoke a single tool
{ok, Content, _Ctx} = beamai_kernel:invoke_tool(Kernel1, <<"read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}, beamai_context:new()).
```

### Load Tool Module

```erlang
%% Load a tool module implementing beamai_tool_behaviour
Kernel = beamai_kernel:new(),
Kernel1 = beamai_kernel:add_tool_module(Kernel, beamai_tool_file),

%% List registered tools
Tools = beamai_kernel:get_tool_specs(Kernel1).
```

### Conversation Memory (multi-turn)

The Kernel is stateless; each invoke passes only the latest message, and history is managed
by the Memory Filter keyed by `conversation_id`. See [docs/MEMORY_EN.md](../../docs/MEMORY_EN.md).

```erlang
%% Start a conversation store; put the memory filter first (outermost) when building the kernel
{ok, _} = beamai_chat_memory_ets:start_link(my_mem),
Store = beamai_chat_memory_ets:handle(my_mem),
K0 = beamai_kernel:new(#{}, [beamai_memory_filter:memory_filter(Store)]),
K = beamai_kernel:add_service(K0, LlmConfig),

%% Identify the conversation with a conversation_id; pass only the latest message
Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"session-1">>),
{ok, R1, _} = beamai_kernel:invoke_chat(K, [#{role => user, content => <<"My name is Alice">>}], #{context => Ctx}),
{ok, R2, _} = beamai_kernel:invoke_chat(K, [#{role => user, content => <<"What's my name?">>}], #{context => Ctx}).
%% The second round's LLM sees the full history; without memory it is a stateless single-shot call.
%% For automatic tool execution with a multi-round loop, use beamai_agent (ReAct).
```

## Dependencies

- OTP's built-in `json` module - JSON encoding/decoding (requires OTP 27+)
- uuid - UUID generation
- gun - HTTP/2 client
- poolboy - Connection pooling

## License

Apache-2.0
