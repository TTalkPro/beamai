# BeamAI Core

English | [中文](README.md)

The core module of the BeamAI framework, providing Kernel architecture, Process Framework, HTTP client, and behavior definitions.

## Module Overview

### Kernel Subsystem

Core abstraction based on Semantic Kernel concepts, managing Plugin and Function registration and invocation:

- **beamai_kernel** - Kernel core, manages Plugin/Function registration and invocation
- **beamai_function** - Function definitions, wraps callable tool functions
- **beamai_context** - Context management, passes execution environment info
- **beamai_filter** - Filters for pre/post function call interception
- **beamai_plugin** - Plugin definitions (internal to Kernel)
- **beamai_prompt** - Prompt template management
- **beamai_result** - Function call result types

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
- **beamai_process_memory_store** - Process state persistence
- **beamai_process_worker** - Process worker
- **beamai_process_sup** - Process supervisor tree

### HTTP Subsystem

Pluggable HTTP client supporting Gun and Hackney backends:

- **beamai_http** - Unified HTTP client interface
- **beamai_http_gun** - Gun HTTP/2 backend implementation
- **beamai_http_hackney** - Hackney HTTP/1.1 backend implementation
- **beamai_http_pool** - HTTP connection pool management

### Behaviour Definitions

Framework behavior interface definitions:

- **beamai_llm_behaviour** - LLM provider behavior interface
- **beamai_http_behaviour** - HTTP backend behavior interface
- **beamai_step_behaviour** - Process step behavior interface
- **beamai_process_store_behaviour** - Process store behavior interface

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

%% Add Plugin
beamai_kernel:add_plugin(Kernel, Name, Functions) -> kernel().
beamai_kernel:add_plugin(Kernel, Name, Functions, Opts) -> kernel().
beamai_kernel:add_plugin_from_module(Kernel, Module) -> kernel().

%% Add services and filters
beamai_kernel:add_service(Kernel, Service) -> kernel().
beamai_kernel:add_filter(Kernel, Filter) -> kernel().

%% Invoke functions
beamai_kernel:invoke(Kernel, FunctionName, Args) -> {ok, Result} | {error, Reason}.
beamai_kernel:invoke_tool(Kernel, ToolName, Args, Context) -> {ok, Result} | {error, Reason}.
beamai_kernel:invoke_chat(Kernel, Messages, Opts) -> {ok, Response} | {error, Reason}.

%% Query functions
beamai_kernel:get_function(Kernel, Name) -> {ok, Function} | error.
beamai_kernel:list_functions(Kernel) -> [Function].
beamai_kernel:get_tool_specs(Kernel) -> [ToolSpec].
beamai_kernel:get_tool_schemas(Kernel) -> [Schema].
```

### beamai_function

```erlang
%% Create function
beamai_function:new(Name, Description, Handler, Opts) -> function().

%% Name: Function name (binary)
%% Description: Function description (binary)
%% Handler: fun(Args, Context) -> {ok, Result} | {error, Reason}
%% Opts: #{parameters => Schema, ...}
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

### Kernel + Function

```erlang
%% Create Kernel
Kernel = beamai_kernel:new(),

%% Define function
ReadFile = beamai_function:new(
    <<"read_file">>,
    <<"Read file contents">>,
    fun(#{<<"path">> := Path}, _Ctx) ->
        case file:read_file(Path) of
            {ok, Content} -> {ok, Content};
            {error, Reason} -> {error, Reason}
        end
    end,
    #{parameters => #{
        type => object,
        properties => #{
            <<"path">> => #{type => string, description => <<"File path">>}
        },
        required => [<<"path">>]
    }}
),

%% Register to Kernel
Kernel1 = beamai_kernel:add_plugin(Kernel, <<"file_ops">>, [ReadFile]),

%% Invoke
{ok, Content} = beamai_kernel:invoke(Kernel1, <<"file_ops-read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}).
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

## Dependencies

- jsx - JSON encoding/decoding
- uuid - UUID generation
- gun - HTTP/2 client
- hackney - HTTP/1.1 client

## License

Apache-2.0
