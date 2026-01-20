# beamai_agent

English | [中文](README.md)

A simple ReAct Agent implementation with support for tool calling, Middleware, checkpoints, and multi-turn conversations.

## Features

- **ReAct Pattern**: Reasoning + Acting loop execution
- **Tool Calling**: Support for custom tools and built-in tools
- **Middleware System**: Extensible interceptor mechanism
- **Checkpoint Persistence**: Save and restore Agent state
- **Callback System**: Listen to execution events
- **Multi-turn Conversation**: Automatic conversation history management

## Module Overview

### Core Modules
- **beamai_agent** - Main module, gen_server implementation
- **beamai_agent_init** - Initialization logic
- **beamai_agent_runner** - Executor
- **beamai_agent_callbacks** - Callback handling
- **beamai_agent_checkpoint** - Checkpoint management

### Middleware Modules
- **beamai_middleware** - Middleware behavior definition
- **beamai_middleware_runner** - Middleware executor
- **beamai_middleware_presets** - Preset configurations
- **middleware_call_limit** - Call limiting
- **middleware_summarization** - Context summarization
- **middleware_human_approval** - Human approval
- **middleware_tool_retry** - Tool retry
- **middleware_model_retry** - Model retry
- **middleware_model_fallback** - Model fallback
- **middleware_pii_detection** - PII detection

### Coordinator Modules
- **beamai_coordinator** - Multi-Agent coordination
- **beamai_coordinator_common** - Coordinator common functions
- **beamai_nodes** - Node definitions

### beamai_coordinator API

The coordinator is used to manage multiple Agents working together, supporting two modes: Pipeline (sequential) and Orchestrator (orchestration).

```erlang
%% Start coordinator
beamai_coordinator:start_link(Id, Opts) -> {ok, Pid} | {error, Reason}.
beamai_coordinator:start_pipeline(Id, Opts) -> {ok, Pid} | {error, Reason}.
beamai_coordinator:start_orchestrator(Id, Opts) -> {ok, Pid} | {error, Reason}.

%% Stop coordinator (Note: parameter is Pid, not Id)
beamai_coordinator:stop(CoordinatorPid) -> ok | {error, Reason}.

%% Get Workers (parameter is Pid)
beamai_coordinator:get_workers(CoordinatorPid) -> {ok, #{Name => WorkerPid}} | {error, Reason}.
beamai_coordinator:get_worker(CoordinatorPid, WorkerName) -> {ok, WorkerPid} | {error, Reason}.

%% Delegate tasks (parameter is Pid)
beamai_coordinator:delegate(CoordinatorPid, WorkerName, Task) -> {ok, Result} | {error, Reason}.
beamai_coordinator:delegate_parallel(CoordinatorPid, [WorkerName], Task) -> {ok, #{Name => Result}}.

%% State export/import
beamai_coordinator:export_coordinator(CoordinatorPid) -> {ok, ExportedData} | {error, Reason}.
beamai_coordinator:import_coordinator(Id, ExportedData) -> {ok, Pid} | {error, Reason}.
```

**Note:** Starting from v2.1, the `beamai_coordinator` API parameters changed from using `Id` to using `CoordinatorPid`. Coordinator metadata is stored in the Agent's `meta` field, rather than in a shared ETS table.

## API Documentation

### beamai_agent

```erlang
%% Start Agent
beamai_agent:start_link(AgentId, Config) -> {ok, Pid} | {error, Reason}.

%% Stop Agent
beamai_agent:stop(Pid) -> ok.

%% Send message
beamai_agent:chat(Pid, Message) -> {ok, Response} | {error, Reason}.
beamai_agent:chat(Pid, Message, Timeout) -> {ok, Response} | {error, Reason}.

%% Checkpoint operations
beamai_agent:save_checkpoint(Pid) -> {ok, CheckpointId} | {error, Reason}.
beamai_agent:save_checkpoint(Pid, Metadata) -> {ok, CheckpointId} | {error, Reason}.
beamai_agent:load_checkpoint(Pid, CheckpointId) -> {ok, Checkpoint} | {error, Reason}.
beamai_agent:load_latest_checkpoint(Pid) -> {ok, Checkpoint} | {error, Reason}.
beamai_agent:list_checkpoints(Pid) -> {ok, [Checkpoint]} | {error, Reason}.
beamai_agent:restore_from_checkpoint(Pid, CheckpointId) -> ok | {error, Reason}.

%% Get state
beamai_agent:get_state(Pid) -> {ok, State}.
beamai_agent:get_messages(Pid) -> {ok, Messages}.

%% Context API (user-defined context, participates in conversation)
beamai_agent:get_context(Pid) -> Context.
beamai_agent:get_context(Pid, Key) -> Value | undefined.
beamai_agent:get_context(Pid, Key, Default) -> Value.
beamai_agent:set_context(Pid, Context) -> ok.
beamai_agent:put_context(Pid, Key, Value) -> ok.

%% Meta API (process-level metadata, does not participate in conversation)
beamai_agent:get_meta(Pid) -> Meta.
beamai_agent:get_meta(Pid, Key) -> Value | undefined.
beamai_agent:get_meta(Pid, Key, Default) -> Value.
beamai_agent:set_meta(Pid, Meta) -> ok.
beamai_agent:put_meta(Pid, Key, Value) -> ok.
```

### Configuration Structure

```erlang
%% First create LLM configuration (must use llm_client:create/2)
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% Agent configuration
Config = #{
    %% Required
    system_prompt => <<"You are a helpful assistant.">>,

    %% LLM configuration (must be created using llm_client:create/2)
    llm => LLM,

    %% Tool list (optional, recommended to use beamai_tool_registry to build)
    tools => beamai_tool_registry:from_config(#{
        tools => [MyCustomTool],
        providers => [beamai_tool_provider_builtin]
    }),

    %% Checkpoint storage (optional)
    storage => Memory,  %% beamai_memory instance

    %% Auto checkpoint (optional)
    auto_checkpoint => true,

    %% Maximum iterations (optional)
    max_iterations => 10,

    %% Middleware configuration (optional)
    middlewares => [
        {middleware_call_limit, #{max_model_calls => 20}},
        {middleware_summarization, #{window_size => 20}}
    ],

    %% Callback functions (optional)
    callbacks => #{
        on_llm_start => fun(Prompts, Meta) -> ok end,
        on_llm_end => fun(Response, Meta) -> ok end,
        on_tool_use => fun(ToolName, Args, Meta) -> ok end
    }
}.
```

## Middleware System

Middleware are interceptors during Agent execution that can intervene at various stages.

### Lifecycle Hooks

```
before_agent -> [before_model -> LLM -> after_model -> before_tools -> Tools -> after_tools]* -> after_agent
```

| Hook | Trigger Timing | Typical Use |
|------|----------------|-------------|
| `before_agent` | Before Agent starts | Initialization |
| `before_model` | Before LLM call | Check limits, modify messages |
| `after_model` | After LLM returns | Process response |
| `before_tools` | Before tool execution | Human approval |
| `after_tools` | After tool execution | Result validation |
| `after_agent` | After Agent ends | Cleanup, logging |

### Using Preset Configurations

```erlang
%% Default configuration
Config = #{
    middlewares => beamai_middleware_presets:default()
}.

%% Production environment configuration
Config = #{
    middlewares => beamai_middleware_presets:production()
}.

%% Human approval configuration
Config = #{
    middlewares => beamai_middleware_presets:human_in_loop()
}.
```

### Built-in Middleware

| Middleware | Description |
|------------|-------------|
| `middleware_call_limit` | Limit model/tool call count |
| `middleware_summarization` | Automatically compress long conversations |
| `middleware_human_approval` | Human confirmation before tool execution |
| `middleware_tool_retry` | Automatic retry on tool failure |
| `middleware_model_retry` | Automatic retry on LLM failure |
| `middleware_model_fallback` | Switch to backup model on primary failure |
| `middleware_pii_detection` | Detect sensitive information |

### Custom Middleware

```erlang
-module(my_middleware).
-behaviour(beamai_middleware).

-export([init/1, before_model/2]).

%% Initialization
init(Opts) ->
    #{max_calls => maps:get(max_calls, Opts, 10)}.

%% Check before LLM call
before_model(State, #{max_calls := Max} = _MwState) ->
    Count = graph_state:get(State, call_count, 0),
    case Count >= Max of
        true -> {halt, call_limit_exceeded};
        false -> {update, #{call_count => Count + 1}}
    end.
```

### Middleware Return Values

```erlang
ok                              %% Continue execution
{update, #{key => value}}       %% Update state
{goto, model | tools | '__end__'}  %% Jump to
{halt, Reason}                  %% Abort
{interrupt, Action}             %% Interrupt and wait for confirmation
```

Detailed documentation: [Middleware System](../../doc/MIDDLEWARE.md)

## Usage Examples

### Basic Usage

```erlang
%% Create LLM configuration
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% Configuration
Config = #{
    system_prompt => <<"You are a helpful assistant.">>,
    llm => LLM
},

%% Start Agent
{ok, Agent} = beamai_agent:start_link(<<"my-agent">>, Config),

%% Conversation
{ok, Response1} = beamai_agent:chat(Agent, <<"Hello!">>),
{ok, Response2} = beamai_agent:chat(Agent, <<"What can you do?">>),

%% Stop
beamai_agent:stop(Agent).
```

### Using Tools

```erlang
%% Create LLM configuration
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% Define calculator tool
CalculatorTool = #{
    name => <<"calculator">>,
    description => <<"Perform mathematical calculations">>,
    parameters => #{
        type => object,
        properties => #{
            <<"expression">> => #{type => string}
        },
        required => [<<"expression">>]
    },
    handler => fun(#{<<"expression">> := Expr}) ->
        %% Simple evaluation (needs safe handling in real applications)
        {ok, Tokens, _} = erl_scan:string(binary_to_list(Expr)),
        {ok, Parsed} = erl_parse:parse_exprs(Tokens),
        {value, Result, _} = erl_eval:exprs(Parsed, []),
        {ok, #{result => Result}}
    end
},

%% Use Registry to build tool list
Tools = beamai_tool_registry:from_config(#{
    tools => [CalculatorTool]
}),

Config = #{
    system_prompt => <<"You are an assistant that can do math.">>,
    llm => LLM,
    tools => Tools
},

{ok, Agent} = beamai_agent:start_link(<<"calc-agent">>, Config),
{ok, Response} = beamai_agent:chat(Agent, <<"What is 123 * 456?">>).
```

### Using Checkpoints

```erlang
%% Create LLM configuration
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% Create storage
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% Start Agent with storage
Config = #{
    system_prompt => <<"You are a helpful assistant.">>,
    llm => LLM,
    storage => Memory
},

{ok, Agent} = beamai_agent:start_link(<<"persistent-agent">>, Config),

%% Conversation
{ok, _} = beamai_agent:chat(Agent, <<"Remember: my name is Alice.">>),

%% Save checkpoint
{ok, CpId} = beamai_agent:save_checkpoint(Agent, #{tag => <<"after_intro">>}),

%% More conversation...
{ok, _} = beamai_agent:chat(Agent, <<"What's the weather?">>),

%% Restore to previous checkpoint
ok = beamai_agent:restore_from_checkpoint(Agent, CpId),

%% Now the Agent only remembers "my name is Alice"
{ok, Messages} = beamai_agent:get_messages(Agent).
```

### Using Alibaba Cloud Bailian

```erlang
%% Create Bailian configuration (Tongyi Qianwen)
LLM = llm_client:create(bailian, #{
    model => <<"qwen3-max">>,
    api_key => list_to_binary(os:getenv("BAILIAN_API_KEY"))
}),

Config = #{
    system_prompt => <<"You are a helpful AI assistant.">>,
    llm => LLM
},

{ok, Agent} = beamai_agent:start_link(<<"bailian-agent">>, Config),
{ok, Response} = beamai_agent:chat(Agent, <<"Hello! Please introduce yourself.">>).
```

### Using Zhipu AI

```erlang
%% Create Zhipu AI configuration (using Anthropic-compatible interface)
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

Config = #{
    system_prompt => <<"You are a helpful AI assistant.">>,
    llm => LLM
},

{ok, Agent} = beamai_agent:start_link(<<"zhipu-agent">>, Config),
{ok, Response} = beamai_agent:chat(Agent, <<"Hello! Please introduce yourself.">>).
```

### Using Coordinator (Pipeline Mode)

```erlang
%% Create LLM configuration
LLM = llm_client:create(bailian, #{
    model => <<"qwen-plus">>,
    api_key => list_to_binary(os:getenv("BAILIAN_API_KEY"))
}),

%% Define Agents in the Pipeline (translation pipeline)
Agents = [
    #{
        name => <<"translator">>,
        system_prompt => <<"You are a translation expert, translating Chinese to English.">>
    },
    #{
        name => <<"polisher">>,
        system_prompt => <<"You are an English polishing expert, optimizing English expressions.">>
    }
],

%% Start Pipeline coordinator
{ok, Coordinator} = beamai_coordinator:start_pipeline(<<"translation_pipeline">>, #{
    agents => Agents,
    llm => LLM
}),

%% Directly delegate to specified worker
{ok, Result} = beamai_coordinator:delegate(Coordinator, <<"translator">>, <<"Hello World">>),

%% Get all workers
{ok, Workers} = beamai_coordinator:get_workers(Coordinator),

%% Stop coordinator
beamai_coordinator:stop(Coordinator).
```

### Using Coordinator (Orchestrator Mode)

```erlang
%% Define multi-expert Agents
Agents = [
    #{name => <<"tech_expert">>, system_prompt => <<"You are a technology expert.">>},
    #{name => <<"business_expert">>, system_prompt => <<"You are a business expert.">>}
],

%% Start Orchestrator coordinator
{ok, Coordinator} = beamai_coordinator:start_orchestrator(<<"expert_panel">>, #{
    agents => Agents,
    llm => LLM
}),

%% Delegate to multiple workers in parallel
{ok, Results} = beamai_coordinator:delegate_parallel(
    Coordinator,
    [<<"tech_expert">>, <<"business_expert">>],
    <<"Analyze the impact of AI on the industry">>
),

%% Results = #{<<"tech_expert">> => {ok, "..."}, <<"business_expert">> => {ok, "..."}}

beamai_coordinator:stop(Coordinator).
```

## Dependencies

- beamai_core
- beamai_llm
- beamai_memory

## License

Apache-2.0
