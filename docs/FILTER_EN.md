# Filter System Documentation

English | [中文](FILTER.md)

The beamai_core Filter system provides a lightweight pipeline interception mechanism for intercepting, modifying, and controlling tool invocations and LLM requests.

## Table of Contents

- [Overview](#overview)
- [Filter Types](#filter-types)
- [API Reference](#api-reference)
- [Usage](#usage)
- [Complete Examples](#complete-examples)
- [Relationship with Middleware](#relationship-with-middleware)

---

## Overview

Filters are interceptors for Kernel tool invocations and Chat requests that can:

- **Modify arguments/results**: Modify parameters and return values before/after tool invocations
- **Modify messages**: Modify message lists and responses before/after LLM calls
- **Intercept execution**: Skip tool execution and return a value directly, or abort the pipeline with an error
- **Logging/auditing**: Record invocation logs, measure response length, etc.

### Execution Flow

```
┌──────────────────────────────────────────────────────┐
│                  Kernel Call Flow                      │
├──────────────────────────────────────────────────────┤
│                                                       │
│  Tool Invocation (invoke_tool):                       │
│  ┌─────────────────┐                                 │
│  │ pre_invocation   │  ← modify args, skip, reject   │
│  └────────┬────────┘                                 │
│           │                                           │
│           ▼                                           │
│  ┌─────────────────┐                                 │
│  │  Tool Execution  │                                │
│  └────────┬────────┘                                 │
│           │                                           │
│           ▼                                           │
│  ┌─────────────────┐                                 │
│  │ post_invocation  │  ← modify result               │
│  └─────────────────┘                                 │
│                                                       │
│  Chat Request (invoke_chat):                          │
│  ┌─────────────────┐                                 │
│  │   pre_chat       │  ← modify message list         │
│  └────────┬────────┘                                 │
│           │                                           │
│           ▼                                           │
│  ┌─────────────────┐                                 │
│  │   LLM Call       │                                │
│  └────────┬────────┘                                 │
│           │                                           │
│           ▼                                           │
│  ┌─────────────────┐                                 │
│  │   post_chat      │  ← modify response             │
│  └─────────────────┘                                 │
│                                                       │
└──────────────────────────────────────────────────────┘
```

### Core Modules

| Module | Location | Description |
|--------|----------|-------------|
| `beamai_filter` | `apps/beamai_core/src/kernel/` | Filter definition and pipeline execution |
| `beamai_kernel` | `apps/beamai_core/src/kernel/` | Kernel integration (register and invoke filters) |
| `beamai` | `apps/beamai_core/src/` | Top-level Facade (convenience API) |

---

## Filter Types

### Four Filter Types

| Type | Trigger | Typical Use |
|------|---------|-------------|
| `pre_invocation` | Before tool execution | Parameter validation, logging, permission checks |
| `post_invocation` | After tool execution | Result transformation, logging |
| `pre_chat` | Before LLM call | Inject system messages, content filtering, **conversation history injection** |
| `post_chat` | After LLM response | Content auditing, response transformation, **conversation history storage** |

> **Conversation memory** is built precisely on `pre_chat` / `post_chat`:
> `beamai_memory_filter` uses pre_chat(-1000) to store the delta and expand the full
> history, and post_chat(+1000) to store the reply, keyed by `conversation_id`.
> See [MEMORY_EN.md](MEMORY_EN.md).

### Filter Context (filter_context)

Filters receive and pass data through a `filter_context` map:

```erlang
%% pre_invocation / post_invocation
#{
    tool => ToolSpec,           %% Tool definition
    args => Args,               %% Invocation arguments (modifiable in pre)
    result => Result,           %% Execution result (only available in post)
    context => Context,         %% Execution context
    metadata => #{}             %% Additional metadata
}

%% pre_chat
#{
    messages => [Message],      %% Message list (modifiable)
    context => Context,         %% Execution context
    metadata => #{}
}

%% post_chat
#{
    result => Response,         %% LLM response (modifiable)
    context => Context,
    metadata => #{}
}
```

### Filter Results (filter_result)

```erlang
%% Continue execution, pass modified context to next filter
{continue, UpdatedFilterCtx}

%% Skip subsequent processing (including tool execution), return value directly
{skip, Value}

%% Abort pipeline, return error
{error, Reason}
```

---

## API Reference

### beamai_filter Module

#### Creating Filters

```erlang
%% Create filter (default priority 0)
-spec new(Name :: binary(), Type :: filter_type(), Handler :: fun()) -> filter_spec().
beamai_filter:new(<<"my_filter">>, pre_invocation, fun(Ctx) -> {continue, Ctx} end).

%% Create filter (with priority; lower values execute first)
-spec new(Name :: binary(), Type :: filter_type(), Handler :: fun(), Priority :: integer()) -> filter_spec().
beamai_filter:new(<<"my_filter">>, pre_invocation, Handler, 10).
```

#### Executing Filter Pipelines

```erlang
%% Execute pre-invocation filters
-spec apply_pre_filters(Filters, ToolSpec, Args, Context) ->
    {ok, FilteredArgs, FilteredContext} | {skip, Value} | {error, Reason}.

%% Execute post-invocation filters
-spec apply_post_filters(Filters, ToolSpec, Result, Context) ->
    {ok, FilteredResult, FilteredContext} | {error, Reason}.

%% Execute pre-chat filters
-spec apply_pre_chat_filters(Filters, Messages, Context) ->
    {ok, FilteredMessages, FilteredContext} | {error, Reason}.

%% Execute post-chat filters
-spec apply_post_chat_filters(Filters, Response, Context) ->
    {ok, FilteredResponse, FilteredContext} | {error, Reason}.

%% Sort filters by priority
-spec sort_filters(Filters) -> SortedFilters.
```

#### Filter Definition Types

```erlang
-type filter_spec() :: #{
    name := binary(),                                      %% Filter name (debug identifier)
    type := filter_type(),                                 %% Filter type
    handler := fun((filter_context()) -> filter_result()), %% Handler function
    priority => integer()                                  %% Priority (default 0)
}.

-type filter_type() :: pre_invocation | post_invocation | pre_chat | post_chat.

-type filter_result() ::
    {continue, filter_context()}   %% Pass to next filter
    | {skip, term()}               %% Skip execution
    | {error, term()}.             %% Abort pipeline
```

### beamai_kernel Integration

```erlang
%% Register filter with Kernel
beamai_kernel:add_filter(Kernel, FilterDef) -> UpdatedKernel.

%% Auto-load filters from tool module (module implements optional filters/0 callback)
beamai_kernel:add_tool_module(Kernel, Module) -> UpdatedKernel.

%% Invoke tool (automatically executes pre/post invocation filter pipeline)
beamai_kernel:invoke_tool(Kernel, ToolName, Args, Context) -> {ok, Result, Context} | {error, Reason}.

%% Chat request (automatically executes pre/post chat filter pipeline)
beamai_kernel:invoke_chat(Kernel, Messages, Opts) -> {ok, Response, Context} | {error, Reason}.
```

### beamai Convenience API

```erlang
%% Register a pre-built filter
beamai:add_filter(Kernel, FilterDef) -> UpdatedKernel.

%% Create and register a filter in one step (calls beamai_filter:new/3 internally)
beamai:add_filter(Kernel, Name, Type, Handler) -> UpdatedKernel.
```

---

## Usage

### 1. Register Filters with Kernel

```erlang
%% Method 1: Using beamai convenience API (recommended)
K0 = beamai:kernel(),
K1 = beamai:add_filter(K0, <<"logger">>, pre_invocation,
    fun(#{tool := #{name := Name}} = Ctx) ->
        io:format("Calling tool: ~ts~n", [Name]),
        {continue, Ctx}
    end).

%% Method 2: Manually create and register
Filter = beamai_filter:new(<<"logger">>, pre_invocation, Handler, 10),
K1 = beamai_kernel:add_filter(K0, Filter).
```

### 2. Auto-register Filters from Tool Modules

Tool modules can auto-register filters by implementing the optional `filters/0` callback:

```erlang
-module(my_tool_module).
-behaviour(beamai_tool_behaviour).

-export([tools/0, filters/0]).

tools() ->
    [#{name => <<"my_tool">>, handler => fun handle/2,
       description => <<"My tool">>}].

%% Optional callback: return list of filters
filters() ->
    [
        beamai_filter:new(<<"audit">>, post_invocation,
            fun(#{tool := #{name := Name}, result := Result} = Ctx) ->
                logger:info("Tool ~ts returned: ~p", [Name, Result]),
                {continue, Ctx}
            end)
    ].
```

When loading the module, Kernel automatically registers these filters:

```erlang
K1 = beamai_kernel:add_tool_module(K0, my_tool_module).
%% Both tools and filters are now registered
```

### 3. Filter Priority

Lower priority values execute first. Filters with equal priority execute in registration order.

```erlang
%% Validator (priority -10, executes first)
K1 = beamai_kernel:add_filter(K0,
    beamai_filter:new(<<"validator">>, pre_invocation, ValidateFn, -10)),

%% Logger (priority 0, default)
K2 = beamai_kernel:add_filter(K1,
    beamai_filter:new(<<"logger">>, pre_invocation, LogFn, 0)),

%% Transformer (priority 10, executes last)
K3 = beamai_kernel:add_filter(K2,
    beamai_filter:new(<<"transformer">>, pre_invocation, TransformFn, 10)).

%% Execution order: validator -> logger -> transformer
```

---

## Complete Examples

### Example 1: Parameter Validation + Logging

```erlang
%% Create Kernel and register tool
K0 = beamai:kernel(),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})),

%% Pre-invocation filter: log calls
K2 = beamai:add_filter(K1, <<"log">>, pre_invocation,
    fun(#{tool := #{name := Name}, args := Args} = Ctx) ->
        io:format("[LOG] ~ts(~p)~n", [Name, Args]),
        {continue, Ctx}
    end),

%% Pre-invocation filter: validate parameters (reject large values)
K3 = beamai:add_filter(K2, <<"validate">>, pre_invocation,
    fun(#{args := #{a := A}} = _Ctx) when A > 1000 ->
        {error, {validation_failed, <<"a exceeds limit">>}};
       (Ctx) ->
        {continue, Ctx}
    end),

%% Post-invocation filter: double the result
K4 = beamai:add_filter(K3, <<"double">>, post_invocation,
    fun(#{result := Result} = Ctx) ->
        {continue, Ctx#{result => Result * 2}}
    end),

%% Invoke (3 + 5 = 8, doubled = 16)
{ok, 16, _} = beamai:invoke_tool(K4, <<"add">>, #{a => 3, b => 5}, beamai:context()).

%% Invoke rejected
{error, {validation_failed, _}} = beamai:invoke_tool(K4, <<"add">>, #{a => 2000, b => 1}, beamai:context()).
```

### Example 2: Chat Message Injection + Response Auditing

```erlang
K0 = beamai:kernel(),
K1 = beamai:add_llm(K0, LLMConfig),

%% pre_chat: auto-inject system message
K2 = beamai:add_filter(K1, <<"inject_system">>, pre_chat,
    fun(#{messages := Msgs} = Ctx) ->
        HasSystem = lists:any(
            fun(#{role := R}) -> R =:= system; (_) -> false end,
            Msgs),
        case HasSystem of
            true ->
                {continue, Ctx};
            false ->
                SystemMsg = #{role => system,
                              content => <<"Answer concisely.">>},
                {continue, Ctx#{messages => [SystemMsg | Msgs]}}
        end
    end),

%% post_chat: log response length
K3 = beamai:add_filter(K2, <<"audit">>, post_chat,
    fun(#{result := #{content := Content}} = Ctx) when is_binary(Content) ->
        logger:info("Response length: ~B bytes", [byte_size(Content)]),
        {continue, Ctx};
       (Ctx) ->
        {continue, Ctx}
    end),

%% Send request (filter auto-injects system message)
{ok, Response, _Ctx} = beamai:chat(K3, [
    #{role => user, content => <<"What is a GenServer?">>}
]).
```

### Example 3: Caching with Skip

```erlang
%% pre_invocation: skip execution and return cached result
K1 = beamai:add_filter(K0, <<"cache">>, pre_invocation,
    fun(#{tool := #{name := Name}, args := Args} = Ctx) ->
        case lookup_cache(Name, Args) of
            {ok, Cached} ->
                {skip, Cached};     %% Skip tool execution, return cached value
            miss ->
                {continue, Ctx}     %% Cache miss, continue normal execution
        end
    end).
```

See [examples/src/example_filter.erl](../examples/src/example_filter.erl) for more examples.

---

## Relationship with Middleware

The [beamai_extra](https://github.com/TTalkPro/beamai_extra) extension project provides an advanced Middleware system (in beamai_tools) with stateful management, presets, call limits, human approval, retry, and fallback features.

Middleware internally converts to Filters via `beamai_middleware_runner:to_filters/1` and registers with Kernel, so both execute in the same filter pipeline.

| Feature | Filter (this document) | Middleware (beamai_extra) |
|---------|----------------------|--------------------------|
| Complexity | Lightweight, stateless | Full framework with state management |
| Presets | None | Provides production/development presets |
| Built-in Features | None | Call limits, human approval, retry, fallback |
| Use Cases | Simple interception: logging, validation, transformation | Complex control: rate limiting, retry, fallback |

---

## More Resources

- [beamai_core README](../apps/beamai_core/README_EN.md) - Kernel architecture documentation
- [API Reference](API_REFERENCE_EN.md) - API reference
- [Example Code](../examples/src/example_filter.erl) - Complete Filter example
