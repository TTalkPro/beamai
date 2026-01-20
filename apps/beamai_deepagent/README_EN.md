# Agent Deep

Deep Agent implementation supporting complex task planning, parallel execution, and reflection mechanisms.

## Features

- Task planning and decomposition
- Parallel execution
- Dependency management
- Reflection and self-correction
- Tool integration (file system, TODO management, etc.)
- Checkpoint and recovery

## Module Overview

### Core Modules

- **beamai_deepagent** - Main module
- **beamai_deepagent_plan** - Task planning
- **beamai_deepagent_router** - Routing decisions
- **beamai_deepagent_dependencies** - Dependency management
- **beamai_deepbeamai_result_analyzer** - Result analysis

### Execution Modules

- **beamai_deepagent_nodes** - Node definitions
- **beamai_deepbeamai_llm_node** - LLM node
- **beamai_deepagent_tool_executor** - Tool executor
- **beamai_deepagent_trace** - Execution tracing

### Tool Modules

- **beamai_deepagent_tool_provider** - Tool provider (implements beamai_tool_provider behavior)
- **beamai_deepagent_fs_tools** - File system tools
- **beamai_deepagent_fs_handlers** - File handlers
- **beamai_deepagent_fs_backend** - File backend
- **beamai_deepagent_todo_tools** - TODO management tools
- **beamai_deepagent_todo_handlers** - TODO handlers
- **beamai_deepagent_human_tools** - Human-in-the-loop tools
- **beamai_deepagent_base_tools** - Base tools (checkpoint, get_trace)
- **beamai_deepagent_plan_tools** - Planning and subtask tools

### Utility Modules

- **beamai_deepbeamai_messages** - Message handling
- **beamai_deepbeamai_utils** - Utility functions

## API Documentation

### beamai_deepagent

```erlang
%% Start Deep Agent
beamai_deepagent:start_link(Config) -> {ok, Pid} | {error, Reason}.

%% Execute task
beamai_deepagent:run(Pid, Task) -> {ok, Result} | {error, Reason}.
beamai_deepagent:run(Pid, Task, Options) -> {ok, Result} | {error, Reason}.

%% Stream execution
beamai_deepagent:run_stream(Pid, Task, Callback) -> {ok, Result} | {error, Reason}.

%% Stop
beamai_deepagent:stop(Pid) -> ok.
```

### Configuration Structure

```erlang
%% First create LLM configuration (must use llm_client:create/2)
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% DeepAgent configuration
Config = beamai_deepagent:new(#{
    %% LLM configuration (must be created using llm_client:create/2)
    llm => LLM,

    %% Tool configuration (optional)
    tools => [
        beamai_deepagent_fs_tools,      %% File system tools
        beamai_deepagent_todo_tools     %% TODO management tools
    ],

    %% Working directory (optional)
    workspace => <<"/tmp/agent_workspace">>,

    %% Maximum depth (optional)
    max_depth => 3,

    %% Maximum iterations (optional)
    max_iterations => 10,

    %% Enable reflection (optional)
    enable_reflection => true
}).
```

## Usage Examples

### Basic Usage

```erlang
%% Create LLM configuration
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% Create DeepAgent configuration
Config = beamai_deepagent:new(#{llm => LLM}),

%% Execute complex task
Task = <<"Analyze the Erlang code in the current directory, find all exported functions, and generate documentation.">>,
{ok, Result} = beamai_deepagent:run(Config, Task).
```

### Using File System Tools

```erlang
%% Create LLM configuration
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

Config = beamai_deepagent:new(#{
    llm => LLM,
    tools => [beamai_deepagent_fs_tools],
    workspace => <<"/tmp/my_workspace">>
}),

%% Agent can read and write files
Task = <<"Create a file named hello.txt with the content 'Hello, World!'">>,
{ok, Result} = beamai_deepagent:run(Config, Task).
```

### Using TODO Management

```erlang
Config = beamai_deepagent:new(#{
    llm => LLM,  %% Reuse previously created LLM configuration
    tools => [beamai_deepagent_todo_tools]
}),

%% Agent can manage task lists
Task = <<"Create a project plan with the following tasks: 1. Design architecture 2. Implement core functionality 3. Write tests">>,
{ok, Result} = beamai_deepagent:run(Config, Task).
```

### Stream Execution

```erlang
Callback = fun
    ({step, Step}) ->
        io:format("Executing step: ~p~n", [Step]);
    ({tool_call, Tool, Args}) ->
        io:format("Calling tool: ~s~n", [Tool]);
    ({tool_result, Tool, Result}) ->
        io:format("Tool result: ~p~n", [Result]);
    ({thinking, Thought}) ->
        io:format("Thinking: ~s~n", [Thought]);
    ({done, Result}) ->
        io:format("Done: ~p~n", [Result])
end,

beamai_deepagent:run_stream(Agent, Task, Callback).
```

### Using Zhipu AI

```erlang
%% Create Zhipu AI configuration (using Anthropic-compatible interface)
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

Config = beamai_deepagent:new(#{
    llm => LLM,
    tools => [beamai_deepagent_fs_tools]
}),

{ok, Result} = beamai_deepagent:run(Config, <<"Analyze project structure and generate report">>).
```

## Tool Management

DeepAgent uses the `beamai_tool_provider` mechanism to manage tools, providing tools through the `beamai_deepagent_tool_provider` module.

### Using Tool Provider

```erlang
%% Get tools through beamai_tool_registry
Config = #{depth => 0, planning_mode => full},
Tools = beamai_tool_registry:from_config(#{
    providers => [{beamai_deepagent_tool_provider, Config}]
}).

%% Direct access to tool collections
BaseTools = beamai_deepagent_tool_provider:base_tools().
PlanTools = beamai_deepagent_tool_provider:plan_tools().
FsTools = beamai_deepagent_tool_provider:filesystem_tools().
```

### Tool Condition Checking

Tool availability is dynamically determined based on configuration:

| Tool Set | Condition |
|----------|-----------|
| Base tools | Always available |
| Planning tools | `planning_mode=full` and `depth=0` |
| TodoList tools | `planning_mode=simple` |
| Subtask tools | `depth < max_depth` |
| Reflection tools | `reflection_enabled=true` |
| File system tools | `filesystem_enabled=true` or has `filesystem` config |
| Human tools | `human_in_loop.enabled=true` (enabled by default) |

### Combining with Other Providers

```erlang
%% Combine DeepAgent tools with MCP tools
Tools = beamai_tool_registry:from_config(#{
    providers => [
        {beamai_deepagent_tool_provider, Config},
        {beamai_tool_provider_mcp, #{server => my_mcp_server}}
    ]
}).
```

## Tool List

### File System Tools (beamai_deepagent_fs_tools)

| Tool Name | Description |
|-----------|-------------|
| `read_file` | Read file content |
| `write_file` | Write to file |
| `list_directory` | List directory contents |
| `create_directory` | Create directory |
| `delete_file` | Delete file |
| `file_exists` | Check if file exists |

### TODO Management Tools (beamai_deepagent_todo_tools)

| Tool Name | Description |
|-----------|-------------|
| `write_todos` | Write todo list |
| `read_todos` | Read todo list |

### Base Tools (beamai_deepagent_base_tools)

| Tool Name | Description |
|-----------|-------------|
| `checkpoint` | Create execution checkpoint |
| `get_trace` | Get execution trace |

### Planning Tools (beamai_deepagent_plan_tools)

| Tool Name | Description |
|-----------|-------------|
| `create_plan` | Create task plan |
| `update_plan` | Update task plan |
| `spawn_subtask` | Create subtask |
| `reflect` | Reflect on current progress |

### Human Interaction Tools (beamai_deepagent_human_tools)

| Tool Name | Description |
|-----------|-------------|
| `ask_human` | Ask user a question |
| `confirm_action` | Request user confirmation |

## Dependencies

- beamai_core
- beamai_llm
- beamai_memory
- beamai_tools

## License

Apache-2.0
