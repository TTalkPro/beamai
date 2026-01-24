# BeamAI Deep Agent

English | [中文](README.md)

Deep Agent implementation based on SubAgent architecture, supporting complex task planning, parallel execution, and reflection mechanisms.

## Features

- SubAgent orchestration architecture (Planner → Executor → Reflector)
- Task planning and decomposition
- Parallel subtask execution
- Dependency management
- Reflection and self-correction
- Coordinator multi-Agent orchestration
- Plugin tool integration

## Architecture

```
beamai_deepagent
├── beamai_deepagent          # Main API (new/1, run/2, run/3)
├── beamai_deepagent_planner  # Planner SubAgent
├── beamai_deepagent_executor # Executor SubAgent
├── beamai_deepagent_reflector # Reflector SubAgent
├── beamai_deepagent_parallel # Parallel execution management
├── beamai_deepagent_coordinator # Multi-Agent coordination
├── beamai_deepagent_plan_plugin # Planning tool plugin
├── beamai_deepagent_utils    # Utility functions
└── core/
    ├── beamai_deepagent_plan          # Plan data structures
    ├── beamai_deepagent_dependencies  # Dependency management
    └── beamai_deepagent_trace         # Execution tracing
```

## Module Overview

### Core API

- **beamai_deepagent** - Main module, provides `new/1`, `run/2`, `run/3`, `get_plan/1`, `get_trace/1`

### SubAgent Modules

- **beamai_deepagent_planner** - Planner: analyzes tasks, generates execution plans
- **beamai_deepagent_executor** - Executor: executes individual steps in the plan
- **beamai_deepagent_reflector** - Reflector: evaluates execution results, provides improvement suggestions
- **beamai_deepagent_parallel** - Parallel manager: manages parallel execution of multiple subtasks
- **beamai_deepagent_coordinator** - Coordinator: task distribution and coordination between Agents

### Tool Modules

- **beamai_deepagent_plan_plugin** - Planning tool plugin (implements beamai_plugin_behaviour)

### Core Data Structures

- **beamai_deepagent_plan** - Plan data structures and operations
- **beamai_deepagent_dependencies** - Task dependency graph management
- **beamai_deepagent_trace** - Execution trace recording

### Utility Modules

- **beamai_deepagent_utils** - General utility functions

## API Documentation

### beamai_deepagent

```erlang
%% Create configuration (returns config map directly, NOT {ok, Config})
-spec new(map()) -> config().
beamai_deepagent:new(Opts) -> Config.

%% Also create empty configuration
beamai_deepagent:new() -> Config.

%% Execute task
beamai_deepagent:run(Config, Task) -> {ok, Result} | {error, Reason}.
beamai_deepagent:run(Config, Task, Opts) -> {ok, Result} | {error, Reason}.

%% Get plan and trace
beamai_deepagent:get_plan(Result) -> Plan.
beamai_deepagent:get_trace(Result) -> Trace.
```

### Configuration Options

```erlang
Config = beamai_deepagent:new(#{
    %% LLM configuration (required, created with beamai_chat_completion:create/2)
    llm => LLM,

    %% Plugin list (optional, modules implementing beamai_plugin_behaviour)
    plugins => [beamai_plugin_file, beamai_plugin_shell],

    %% Custom tools (optional, list of tool maps)
    custom_tools => [#{name => ..., handler => ...}],

    %% Maximum recursion depth (default 3)
    max_depth => 3,

    %% Maximum parallelism (default 5)
    max_parallel => 5,

    %% Enable planning (default true)
    planning_enabled => true,

    %% Enable reflection (default true)
    reflection_enabled => true,

    %% Maximum tool iterations per sub-agent (default 10)
    max_tool_iterations => 10,

    %% Timeout per step in milliseconds (default 300000)
    timeout => 300000,

    %% System prompt
    system_prompt => <<"You are an expert">>,

    %% SubAgent-specific prompts
    planner_prompt => <<"...">>,
    executor_prompt => <<"...">>,
    reflector_prompt => <<"...">>,

    %% Event callbacks
    callbacks => #{
        on_plan_created => fun(Plan) -> ... end,
        on_step_start => fun(Step) -> ... end,
        on_step_end => fun(Step, Result) -> ... end,
        on_reflection => fun(Reflection) -> ... end
    }
}).
```

## Usage Examples

### Basic Usage

```erlang
%% Create LLM configuration
LLM = beamai_chat_completion:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% Create DeepAgent configuration
Config = beamai_deepagent:new(#{llm => LLM}),

%% Execute complex task
{ok, Result} = beamai_deepagent:run(Config,
    <<"Analyze the Erlang code in the current directory, find all exported functions, and generate documentation.">>),

%% View plan
Plan = beamai_deepagent:get_plan(Result),
Trace = beamai_deepagent:get_trace(Result).
```

### Using Plugin Tools

```erlang
Config = beamai_deepagent:new(#{
    llm => LLM,
    plugins => [beamai_plugin_file, beamai_plugin_shell],
    max_depth => 2
}),

{ok, Result} = beamai_deepagent:run(Config,
    <<"Read all .erl files in src/ directory and count lines of code">>).
```

### Execution with Callbacks

```erlang
Config = beamai_deepagent:new(#{
    llm => LLM,
    plugins => [beamai_plugin_file],
    callbacks => #{
        on_plan_created => fun(Plan) ->
            io:format("Plan created: ~p~n", [Plan])
        end,
        on_step_start => fun(Step) ->
            io:format("Starting step: ~p~n", [Step])
        end,
        on_reflection => fun(Reflection) ->
            io:format("Reflection: ~ts~n", [Reflection])
        end
    }
}),

{ok, Result} = beamai_deepagent:run(Config, <<"Analyze project architecture">>).
```

### Using Zhipu AI

```erlang
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

Config = beamai_deepagent:new(#{
    llm => LLM,
    plugins => [beamai_plugin_file]
}),

{ok, Result} = beamai_deepagent:run(Config, <<"Analyze project structure and generate report">>).
```

## Execution Flow

```
1. Receive task
   ↓
2. Planner analyzes task, generates execution plan
   ↓
3. Sort steps by dependencies
   ↓
4. For each step:
   a. Executor executes step (using Plugin tools)
   b. Supports parallel execution of independent steps
   ↓
5. Reflector evaluates execution results
   ↓
6. If improvements needed, update plan and re-execute
   ↓
7. Return final result
```

## Dependencies

- beamai_core (Kernel, Process Framework)
- beamai_llm (LLM calls)
- beamai_plugin (Plugin system)
- beamai_memory (State persistence)

## License

Apache-2.0
