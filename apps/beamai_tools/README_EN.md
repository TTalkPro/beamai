# BeamAI Tools

English | [中文](README.md)

Tool system module providing tool module loading, Middleware system, and built-in tool sets.

## Features

- Tool module loading and management
- Middleware interceptor chain
- Preset Middleware configurations
- Tool security validation
- Built-in tool modules (File, Shell, TODO, Human interaction)

## Module Overview

### Core Modules

- **beamai_tools** - Main API (load/2, load_all/2, with_middleware/2, presets/1)
- **beamai_tool_security** - Tool invocation security validation

### Middleware Modules

- **beamai_middleware** - Middleware behaviour definition
- **beamai_middleware_runner** - Middleware chain executor
- **beamai_middleware_presets** - Preset configurations (default, minimal, production, development, human_in_loop)
- **middleware_call_limit** - Call count limits
- **middleware_tool_retry** - Tool call retry
- **middleware_model_retry** - Model call retry
- **middleware_model_fallback** - Model fallback
- **middleware_human_approval** - Human approval

### Built-in Tool Modules

- **beamai_tool_file** - File operations (read, write, list, glob)
- **beamai_tool_shell** - Shell command execution
- **beamai_tool_todo** - Task management
- **beamai_tool_human** - Human input interaction

## API Documentation

### beamai_tools

```erlang
%% Load a single tool module into Kernel
beamai_tools:load(Kernel, beamai_tool_file) -> Kernel1.

%% Load multiple tool modules
beamai_tools:load_all(Kernel, [beamai_tool_file, beamai_tool_shell]) -> Kernel1.

%% Add Middleware
beamai_tools:with_middleware(Kernel, Middlewares) -> Kernel1.

%% Get preset Middleware configuration
beamai_tools:presets(default) -> [MiddlewareSpec].
beamai_tools:presets(production) -> [MiddlewareSpec].

%% List available built-in tool modules
beamai_tools:available() -> [ModuleName].

%% Define tool
beamai_tools:define_tool(Name, Handler) -> ToolSpec.
beamai_tools:define_tool(Name, Handler, Opts) -> ToolSpec.
beamai_tools:define_tool(Name, Description, Handler, Parameters) -> ToolSpec.
beamai_tools:define_tool(Name, Description, Handler, Parameters, Opts) -> ToolSpec.
```

## Usage Examples

### Basic Usage

```erlang
%% Create Kernel and load tools
Kernel = beamai_kernel:new(),
Kernel1 = beamai_tools:load_all(Kernel, [beamai_tool_file, beamai_tool_shell]),

%% Get tool specs for Agent use
Tools = beamai_kernel:get_tool_specs(Kernel1).
```

### Using Middleware

```erlang
%% Use preset Middleware
Kernel = beamai_kernel:new(),
Kernel1 = beamai_tools:load_all(Kernel, [beamai_tool_file]),
Kernel2 = beamai_tools:with_middleware(Kernel1,
    beamai_middleware_presets:production()),

%% Or use custom Middleware configuration
Kernel3 = beamai_tools:with_middleware(Kernel1, [
    {middleware_call_limit, #{max_model_calls => 20}},
    {middleware_tool_retry, #{max_retries => 3}}
]).
```

### Custom Tools

```erlang
%% Create tool using beamai_tools:define_tool
Tool = beamai_tools:define_tool(
    <<"get_weather">>,
    <<"Get weather for a city">>,
    fun(#{<<"city">> := City}, _Ctx) ->
        {ok, #{city => City, temp => 25}}
    end,
    #{
        type => object,
        properties => #{
            <<"city">> => #{type => string, description => <<"City name">>}
        },
        required => [<<"city">>]
    }
),

Kernel = beamai_kernel:new(),
Kernel1 = beamai_kernel:add_tools(Kernel, [Tool]).
```

## Middleware Presets

| Preset | Includes | Description |
|--------|----------|-------------|
| `default` | call_limit + model_retry | Default configuration |
| `minimal` | call_limit | Minimal configuration |
| `production` | call_limit + tool_retry + model_retry + model_fallback | Production environment |
| `development` | call_limit (relaxed) + tool_retry | Development debugging |
| `human_in_loop` | call_limit + human_approval | Human approval |

## Dependencies

- beamai_core (Kernel, tool behaviour definition)
- jsx (JSON encoding/decoding)

## License

Apache-2.0
