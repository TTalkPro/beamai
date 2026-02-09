# BeamAI Core

English | [中文](README.md)

The core module of the BeamAI framework, providing Kernel architecture, Process Framework, Graph Engine, HTTP client, and behavior definitions.

## Module Overview

### Kernel Subsystem

Core abstraction based on Semantic Kernel concepts, managing Tool registration and invocation:

- **beamai_kernel** - Kernel core, manages Tool registration and invocation
- **beamai_tool** - Tool definitions, wraps callable tool functions
- **beamai_tool_behaviour** - Tool module behavior interface
- **beamai_context** - Context management, passes execution environment info
- **beamai_filter** - Filters for pre/post tool call interception
- **beamai_prompt** - Prompt template management
- **beamai_result** - Tool call result types

### LLM Subsystem

Unified abstraction layer for LLM responses:

- **llm_response** - Unified LLM response accessors (content, tool_calls, usage, etc.)

### Process Framework Subsystem

Orchestratable process engine supporting step definitions, conditional branching, parallel execution, and time travel:

- **beamai_process** - Process definitions and core data structures
- **beamai_process_builder** - Process builder (Builder pattern)
- **beamai_process_runtime** - Process runtime
- **beamai_process_step** - Step definitions
- **beamai_process_step_transform** - Step transformations
- **beamai_process_executor** - Process executor
- **beamai_process_event** - Event system
- **beamai_process_state** - Process state management
- **beamai_process_worker** - Process worker
- **beamai_process_sup** - Process supervisor tree

### Graph Engine Subsystem

Declarative graph-based workflow engine:

- **graph** - Graph definitions and builder
- **graph_node** - Node definitions
- **graph_edge** - Edge definitions
- **graph_builder** - Graph builder
- **graph_dsl** - Declarative DSL
- **graph_runner** - Graph execution engine
- **graph_state** - Graph state management
- **graph_snapshot** - Graph state snapshots

### Pregel Subsystem

Distributed graph computation based on Pregel model:

- **pregel** - Pregel master and worker
- **pregel_master** - Master node
- **pregel_worker** - Worker nodes
- **pregel_vertex** - Vertex computation
- **pregel_dispatch_worker** - Dispatch pool worker

### HTTP Subsystem

Pluggable HTTP client supporting Gun and Hackney backends:

- **beamai_http** - Unified HTTP client interface
- **beamai_http_gun** - Gun HTTP/2 backend implementation
- **beamai_http_hackney** - Hackney HTTP/1.1 backend implementation
- **beamai_http_pool** - HTTP connection pool management

### Behaviour Definitions

Framework behavior interface definitions:

- **beamai_chat_behaviour** - LLM chat interface (formerly beamai_llm_behaviour)
- **beamai_http_behaviour** - HTTP backend behavior interface
- **beamai_step_behaviour** - Process step behavior interface
- **beamai_process_store_behaviour** - Process store behavior interface (with optional branch/time-travel callbacks)
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
%% Create Kernel instance
beamai_kernel:new() -> kernel().
beamai_kernel:new(Opts) -> kernel().

%% Add Tools
beamai_kernel:add_tool(Kernel, ToolSpec) -> kernel().
beamai_kernel:add_tools(Kernel, [ToolSpec]) -> kernel().
beamai_kernel:add_tool_module(Kernel, Module) -> kernel().

%% Add services and filters
beamai_kernel:add_service(Kernel, Service) -> kernel().
beamai_kernel:add_filter(Kernel, Filter) -> kernel().

%% Invoke API
beamai_kernel:invoke(Kernel, Messages, Opts) -> {ok, Response, Context} | {error, Reason}.
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

### beamai_process_builder

```erlang
%% Create process builder
beamai_process_builder:new(Name) -> builder().

%% Add step
beamai_process_builder:add_step(Builder, StepName, StepOpts) -> builder().

%% Build process
beamai_process_builder:build(Builder) -> {ok, Process} | {error, Reason}.
```

### beamai_process_executor

```erlang
%% Execute process
beamai_process_executor:run(Process, Input) -> {ok, Result} | {error, Reason}.
beamai_process_executor:run(Process, Input, Opts) -> {ok, Result} | {error, Reason}.
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

%% Invoke
{ok, Content, _Ctx} = beamai_kernel:invoke(Kernel1, <<"read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}).
```

### Load Tool Module

```erlang
%% Load a tool module implementing beamai_tool_behaviour
Kernel = beamai_kernel:new(),
Kernel1 = beamai_kernel:add_tool_module(Kernel, beamai_tool_file),

%% List registered tools
Tools = beamai_kernel:list_functions(Kernel1).
```

### Process Framework

```erlang
%% Build multi-step process
Builder = beamai_process_builder:new(<<"data_pipeline">>),

Builder1 = beamai_process_builder:add_step(Builder, <<"fetch">>, #{
    handler => fun(Input, _Ctx) ->
        {ok, Input#{data => fetch_data()}}
    end
}),

Builder2 = beamai_process_builder:add_step(Builder1, <<"transform">>, #{
    handler => fun(#{data := Data} = Input, _Ctx) ->
        {ok, Input#{data => transform(Data)}}
    end
}),

Builder3 = beamai_process_builder:add_step(Builder2, <<"save">>, #{
    handler => fun(#{data := Data} = Input, _Ctx) ->
        ok = save_data(Data),
        {ok, Input#{saved => true}}
    end
}),

{ok, Process} = beamai_process_builder:build(Builder3),
{ok, Result} = beamai_process_executor:run(Process, #{}).
```

### Graph Engine

```erlang
%% Build a simple graph using DSL
{ok, Graph} = graph:build([
    {node, greeting, fun(State, _Ctx) ->
        Name = graph_state:get(State, name, <<"World">>),
        Message = <<"Hello, ", Name/binary, "!">>,
        {ok, graph_state:set(State, message, Message)}
    end},
    {node, uppercase, fun(State, _Ctx) ->
        Message = graph_state:get(State, message, <<>>),
        Upper = string:uppercase(Message),
        {ok, graph_state:set(State, message, Upper)}
    end},
    {edge, greeting, uppercase},
    {edge, uppercase, '__end__'},
    {entry, greeting}
]),

%% Run the graph
InitialState = graph:state(#{name => <<"Erlang">>}),
Result = graph:run(Graph, InitialState).
```

## Dependencies

- jsx - JSON encoding/decoding
- uuid - UUID generation
- gun - HTTP/2 client
- hackney - HTTP/1.1 client
- poolboy - Connection pooling

## License

Apache-2.0
