# beamai_tools - Common Tool Library and Middleware System

Unified definition, registration, and management module for Agent tools, along with the Agent execution middleware system.

## Core Features

### Tool System
- **Tool Management**: Unified tool definition format and retrieval interface
- **Provider Mechanism**: Support for multiple tool sources (built-in, custom, MCP)
- **Tool Registry**: Builder pattern with conflict resolution strategies
- **Tool Execution**: Unified tool invocation interface
- **LLM Adaptation**: Conversion to OpenAI/Anthropic function call formats

### Middleware System
- **Execution Interception**: Intercept and modify at various stages of Agent execution
- **Flow Control**: Support for jumps, halts, interrupts, and other control flows
- **Tool Enhancement**: Tool filtering, retry, emulation, and more
- **Context Management**: Conversation summarization, PII detection, etc.

## Module Structure

```
beamai_tools/
├── beamai_tools.erl              # Main API module
├── beamai_tool_registry.erl      # Tool registry
├── beamai_tool_provider.erl      # Provider behaviour definition
├── beamai_tool_security.erl      # Tool security checks
├── providers/
│   ├── beamai_tool_provider_builtin.erl  # Built-in Provider
│   └── beamai_tool_provider_custom.erl   # Custom Provider
├── tools/
│   ├── beamai_tools_file.erl     # File operation tools
│   ├── beamai_tools_shell.erl    # Shell command tools
│   ├── beamai_tools_todo.erl     # TODO management tools
│   └── beamai_tools_human.erl    # Human interaction tools
└── middleware/
    ├── beamai_middleware.erl          # Middleware behaviour definition
    ├── beamai_middleware_runner.erl   # Middleware chain runner
    ├── beamai_middleware_presets.erl  # Preset configurations
    ├── middleware_call_limit.erl      # Call count limiting
    ├── middleware_context_editing.erl # Context editing
    ├── middleware_file_search.erl     # File search enhancement
    ├── middleware_human_approval.erl  # Human approval
    ├── middleware_model_fallback.erl  # Model fallback
    ├── middleware_model_retry.erl     # Model retry
    ├── middleware_pii_detection.erl   # PII detection
    ├── middleware_shell_tool.erl      # Shell tool enhancement
    ├── middleware_summarization.erl   # Conversation summarization
    ├── middleware_todo_list.erl       # TODO management
    ├── middleware_tool_emulator.erl   # Tool emulation
    ├── middleware_tool_retry.erl      # Tool retry
    └── middleware_tool_selector.erl   # Intelligent tool selection
```

## Tool System

### Getting Tools

```erlang
%% Get tools by category
FileTools = beamai_tools:get_tools(file),

%% Get multiple categories
Tools = beamai_tools:get_tools([file, shell, todo]),

%% Get all tools
AllTools = beamai_tools:get_all_tools().
```

### Using the Tool Registry

```erlang
%% Builder pattern
Registry = beamai_tool_registry:new(),
R1 = beamai_tool_registry:add_tools(Registry, [MyTool1, MyTool2]),
R2 = beamai_tool_registry:add_provider(R1, beamai_tool_provider_builtin),
Tools = beamai_tool_registry:build(R2).

%% Convenience function
Tools = beamai_tool_registry:from_config(#{
    tools => [MyTool],
    providers => [
        {beamai_tool_provider_mcp, #{mcp_tag => file}},
        beamai_tool_provider_builtin
    ]
}).
```

### Executing Tools

```erlang
%% Execute tool by name
{ok, Result} = beamai_tools:execute(<<"file_read">>, #{
    path => <<"/tmp/test.txt">>
}).

%% Execute with context
{ok, Result} = beamai_tools:execute(<<"shell_exec">>, #{
    command => <<"ls -la">>
}, #{context => #{workspace => <<"/project">>}}).
```

### Converting to LLM Format

```erlang
%% Single tool
Spec = beamai_tools:to_llm_spec(Tool),

%% Batch conversion
Specs = beamai_tools:to_llm_specs(Tools).
```

### Tool Definition Format

```erlang
#{
    name => <<"tool_name">>,
    description => <<"Tool description"/utf8>>,
    parameters => #{
        type => object,
        properties => #{
            <<"param1">> => #{
                type => string,
                description => <<"Parameter description"/utf8>>
            }
        },
        required => [<<"param1">>]
    },
    handler => fun(Args, Context) ->
        %% Execution logic
        {ok, Result}
    end
}
```

### Built-in Tool Categories

| Category | Module | Tools |
|----------|--------|-------|
| `file` | beamai_tools_file | file_read, file_write, file_list, file_search |
| `shell` | beamai_tools_shell | shell_exec |
| `todo` | beamai_tools_todo | todo_add, todo_list, todo_complete |
| `human` | beamai_tools_human | ask_human, confirm_action |

## Middleware System

### Lifecycle Hooks

```
                   ┌─────────────────┐
                   │  before_agent   │
                   └────────┬────────┘
                            │
           ┌────────────────┼────────────────┐
           │                ▼                │
           │    ┌─────────────────────┐      │
           │    │    before_model     │      │
           │    └──────────┬──────────┘      │
           │               │                 │
           │               ▼                 │
           │    ┌─────────────────────┐      │
           │    │     LLM Call        │      │
           │    └──────────┬──────────┘      │
           │               │                 │
           │               ▼                 │
           │    ┌─────────────────────┐      │
           │    │     after_model     │      │
           │    └──────────┬──────────┘      │
 Agent     │               │                 │
 Loop      │               ▼                 │
           │    ┌─────────────────────┐      │
           │    │    before_tools     │      │
           │    └──────────┬──────────┘      │
           │               │                 │
           │               ▼                 │
           │    ┌─────────────────────┐      │
           │    │    Tool Execution   │      │
           │    └──────────┬──────────┘      │
           │               │                 │
           │               ▼                 │
           │    ┌─────────────────────┐      │
           │    │     after_tools     │      │
           │    └──────────┬──────────┘      │
           │               │                 │
           └───────────────┴─────────────────┘
                           │
                           ▼
                   ┌─────────────────┐
                   │   after_agent   │
                   └─────────────────┘
```

### Using Middleware

```erlang
%% Configure middleware list
Middlewares = [
    %% Call count limiting
    {middleware_call_limit, #{max_calls => 10}},

    %% Model retry
    {middleware_model_retry, #{max_retries => 3}},

    %% Tool retry
    {middleware_tool_retry, #{max_retries => 2}},

    %% Conversation summarization
    {middleware_summarization, #{
        window_size => 20,
        max_tokens => 4000
    }},

    %% Human approval
    {middleware_human_approval, #{
        require_approval => [<<"shell_exec">>, <<"file_write">>]
    }}
],

%% Use in Agent configuration
AgentConfig = #{
    middlewares => Middlewares,
    llm => LLMConfig,
    tools => Tools
}.
```

### Creating Custom Middleware

```erlang
-module(my_middleware).
-behaviour(beamai_middleware).

-export([init/1, before_model/2, after_model/2]).

init(Opts) ->
    #{max_calls => maps:get(max_calls, Opts, 10)}.

before_model(State, #{max_calls := MaxCalls} = _MwState) ->
    Count = maps:get(model_call_count, State, 0),
    case Count >= MaxCalls of
        true -> {halt, model_call_limit_exceeded};
        false -> {update, #{model_call_count => Count + 1}}
    end.

after_model(State, _MwState) ->
    ok.
```

### Middleware Return Values

| Return Value | Description |
|--------------|-------------|
| `ok` | No modification, continue execution |
| `{update, StateUpdates}` | Update graph state |
| `{goto, Node}` | Jump to specified node (model \| tools \| '__end__') |
| `{update_goto, StateUpdates, Node}` | Update state and jump |
| `{halt, Reason}` | Halt execution and return error |
| `{interrupt, Action}` | Interrupt and wait for user confirmation |

### Built-in Middleware

| Middleware | Description |
|------------|-------------|
| `middleware_call_limit` | Limit LLM/tool call count |
| `middleware_model_retry` | Auto-retry on LLM call failure |
| `middleware_model_fallback` | LLM model fallback strategy |
| `middleware_tool_retry` | Retry on tool execution failure |
| `middleware_tool_selector` | Intelligent tool selection (reduce tokens) |
| `middleware_tool_emulator` | Tool call emulation (for testing) |
| `middleware_summarization` | Conversation history compression |
| `middleware_pii_detection` | PII sensitive information detection |
| `middleware_human_approval` | Human approval for dangerous operations |
| `middleware_context_editing` | Dynamic context editing |
| `middleware_file_search` | File search enhancement |
| `middleware_shell_tool` | Persistent Shell session |
| `middleware_todo_list` | TODO task management |

### Using Presets

```erlang
%% Get preset configuration
SafePreset = beamai_middleware_presets:safe(),
DevPreset = beamai_middleware_presets:development(),

%% Combine presets
Middlewares = beamai_middleware_presets:combine([SafePreset, MyMiddlewares]).
```

## Provider Mechanism

### Creating a Custom Provider

```erlang
-module(my_tool_provider).
-behaviour(beamai_tool_provider).

-export([info/0, is_available/0, list_tools/1, find_tool/2]).

info() ->
    #{name => <<"My Provider">>, version => <<"1.0.0">>}.

is_available() -> true.

list_tools(Opts) ->
    Categories = maps:get(categories, Opts, all),
    {ok, filter_by_category(my_tools(), Categories)}.

find_tool(Name, _Opts) ->
    case lists:keyfind(Name, 1, my_tools()) of
        {Name, Tool} -> {ok, Tool};
        false -> {error, not_found}
    end.
```

### Using Multiple Providers

```erlang
Tools = beamai_tools:get_tools([file, custom], #{
    providers => [
        beamai_tool_provider_builtin,
        beamai_tool_provider_custom,
        beamai_tool_provider_mcp
    ]
}).
```

## Conflict Resolution Strategies

The tool registry supports multiple conflict resolution strategies:

```erlang
%% First added wins (default)
Tools = beamai_tool_registry:build(Registry, fun strategy_first_wins/2),

%% Last added wins
Tools = beamai_tool_registry:build(Registry, fun strategy_last_wins/2),

%% Error on conflict
Tools = beamai_tool_registry:build(Registry, fun strategy_error/2).
```

## API Reference

### beamai_tools

| Function | Description |
|----------|-------------|
| `get_tools/1,2` | Get tools by specified category |
| `get_all_tools/0,1` | Get all tools |
| `find_tool/1,2` | Find tool by name |
| `execute/2,3` | Execute tool |
| `to_llm_spec/1` | Convert to LLM format |
| `to_llm_specs/1` | Batch conversion |

### beamai_tool_registry

| Function | Description |
|----------|-------------|
| `new/0` | Create empty registry |
| `add_tools/2` | Add tool list |
| `add_provider/2,3` | Add Provider |
| `build/1,2` | Build tool list |
| `from_config/1` | Build from configuration |
| `from_providers/1,2` | Build from Provider list |

### beamai_middleware_runner

| Function | Description |
|----------|-------------|
| `init/1` | Initialize middleware chain |
| `run_hook/3,4` | Execute specified hook |
| `get_middleware_state/2` | Get middleware state |
| `set_middleware_state/3` | Set middleware state |

## Adapter Pattern (Dependency Decoupling)

beamai_tools decouples dependencies on `beamai_llm` and `beamai_memory` through the Adapter pattern.

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                      beamai_core                            │
│  ┌─────────────────────┐  ┌─────────────────────────────┐   │
│  │beamai_llm_behaviour │  │beamai_buffer_behaviour      │   │
│  │  - chat/2           │  │  - new/1                    │   │
│  │  - is_valid_config/1│  │  - count_tokens/2           │   │
│  └─────────────────────┘  │  - build_context/2          │   │
│            ▲              └─────────────────────────────┘   │
│            │ implements                 ▲ implements        │
└────────────┼────────────────────────────┼───────────────────┘
┌────────────┼────────────┐  ┌────────────┼───────────────────┐
│  beamai_llm│            │  │ beamai_memory                  │
│  ┌─────────┴─────────┐  │  │  ┌─────────┴─────────────────┐ │
│  │   llm_client      │  │  │  │beamai_conversation_buffer │ │
│  └───────────────────┘  │  │  └───────────────────────────┘ │
└─────────────────────────┘  └────────────────────────────────┘
             ▲ uses (via adapter)         ▲ uses (via adapter)
┌────────────┴────────────────────────────┴───────────────────┐
│                      beamai_tools                           │
│  ┌───────────────────┐  ┌─────────────────────────────────┐ │
│  │beamai_llm_adapter │  │beamai_buffer_adapter            │ │
│  └───────────────────┘  └─────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### Using Default Implementations

By default, Adapters use the following implementations:
- `beamai_llm_adapter` → `llm_client`
- `beamai_buffer_adapter` → `beamai_conversation_buffer`

### Custom Implementations

You can replace default implementations through Application environment variables:

```erlang
%% Configure in sys.config
{beamai_tools, [
    {llm_module, my_llm_client},
    {buffer_module, my_buffer}
]}
```

Or pass them at call time:

```erlang
%% LLM Adapter
beamai_llm_adapter:chat(Config, Request, #{llm_module => my_llm}).

%% Buffer Adapter
beamai_buffer_adapter:new(Opts, #{buffer_module => my_buffer}).
```

### Implementing Custom Modules

Custom modules need to implement the corresponding Behaviour:

```erlang
%% Custom LLM client
-module(my_llm_client).
-behaviour(beamai_llm_behaviour).

-export([chat/2, is_valid_config/1]).

chat(Config, Request) ->
    %% Implement LLM call logic
    {ok, Response}.

is_valid_config(Config) ->
    %% Validate configuration
    true.
```

```erlang
%% Custom Buffer
-module(my_buffer).
-behaviour(beamai_buffer_behaviour).

-export([new/1, count_tokens/2, build_context/2]).

new(Opts) -> #{...}.
count_tokens(Config, Messages) -> 0.
build_context(Config, Messages) -> {ok, #{messages => Messages}}.
```

## Dependencies

**Required Dependencies**:
- `beamai_core`: Behaviour definitions, base types
- `jsx`: JSON encoding/decoding

**Optional Dependencies** (decoupled via Adapter):
- `beamai_llm`: Provides `llm_client` (default LLM implementation)
- `beamai_memory`: Provides `beamai_conversation_buffer` (default Buffer implementation)

## License

Apache-2.0
