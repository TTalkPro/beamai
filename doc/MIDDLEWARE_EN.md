# Middleware System Documentation

English | [中文](MIDDLEWARE.md)

The Middleware system in beamai_plugin provides a flexible way to intercept, modify, and control various stages of Kernel function invocations.

## Table of Contents

- [Overview](#overview)
- [Lifecycle Hooks](#lifecycle-hooks)
- [Built-in Middleware](#built-in-middleware)
- [Preset Configurations](#preset-configurations)
- [Custom Middleware](#custom-middleware)
- [Configuration and Usage](#configuration-and-usage)
- [Advanced Usage](#advanced-usage)

---

## Overview

Middleware are interceptors in the Kernel function invocation process that can:

- **Modify Input/Output**: Modify context before and after LLM calls or tool invocations
- **Control Flow**: Skip, retry, or abort execution
- **Add Functionality**: Rate limiting, human approval, model fallback, etc.
- **Enforce Limits**: Call count limits, error retry, etc.

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    Kernel Function Invocation                  │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐                                           │
│  │   pre_chat   │  ← Before LLM call                        │
│  └──────┬───────┘                                           │
│         │                                                    │
│         ▼                                                    │
│  ┌──────────────┐                                           │
│  │   LLM Call   │                                           │
│  └──────┬───────┘                                           │
│         │                                                    │
│         ▼                                                    │
│  ┌──────────────┐                                           │
│  │  post_chat   │  ← After LLM response (can retry/fallback)│
│  └──────┬───────┘                                           │
│         │                                                    │
│         ▼                                                    │
│  ┌────────────────┐                                         │
│  │pre_invocation  │  ← Before tool execution (can approve)   │
│  └──────┬─────────┘                                         │
│         │                                                    │
│         ▼                                                    │
│  ┌──────────────┐                                           │
│  │Tool Execution│                                           │
│  └──────┬───────┘                                           │
│         │                                                    │
│         ▼                                                    │
│  ┌────────────────┐                                         │
│  │post_invocation │  ← After tool execution (can retry)      │
│  └────────────────┘                                         │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### Core Modules

| Module | Location | Description |
|--------|----------|-------------|
| `beamai_middleware` | `apps/beamai_plugin/src/middleware/` | Middleware behaviour definition |
| `beamai_middleware_runner` | `apps/beamai_plugin/src/middleware/` | Middleware chain executor |
| `beamai_middleware_presets` | `apps/beamai_plugin/src/middleware/` | Preset configurations |

---

## Lifecycle Hooks

### Hook List

| Hook | Trigger Timing | Typical Use |
|------|----------------|-------------|
| `pre_chat` | Before LLM call | Check call limits, modify messages |
| `post_chat` | After LLM response | Error retry, model fallback |
| `pre_invocation` | Before tool execution | Human approval, parameter validation |
| `post_invocation` | After tool execution | Failure retry, count updates |

### Return Value Types

Middleware hook functions can return the following values:

```erlang
%% No modification, continue execution
ok

%% Modify context and continue (passed to next Middleware)
{continue, UpdatedFilterCtx}

%% Skip subsequent processing, return result directly
{skip, Term}

%% Abort execution and return error
{error, Reason}
```

### FilterCtx Structure

Middleware passes context information via `FilterCtx`:

```erlang
%% pre_chat / post_chat
FilterCtx = #{
    result => ok | {error, Reason}   %% LLM call result (available in post_chat)
}

%% pre_invocation / post_invocation
FilterCtx = #{
    function => #{name => <<"tool_name">>, ...},  %% Function definition
    args => #{<<"param">> => Value},                %% Call arguments
    result => ok | {error, Error}                   %% Execution result (available in post_invocation)
}
```

---

## Built-in Middleware

### 1. middleware_call_limit - Call Limit

Limits various call counts during function invocation.

**Hooks**: `pre_chat`, `pre_invocation`, `post_invocation`
**Priority**: 10 (executes first)

```erlang
{middleware_call_limit, #{
    max_model_calls => 20,           %% Maximum model call count
    max_tool_calls => 50,            %% Maximum total tool call count
    max_tool_calls_per_turn => 10,   %% Maximum tool calls per turn
    max_iterations => 15,            %% Maximum iterations
    on_limit_exceeded => halt        %% Action when exceeded: halt | warn_and_continue
}}
```

### 2. middleware_human_approval - Human Approval

Requests human confirmation before tool execution.

**Hook**: `pre_invocation`
**Priority**: 50

```erlang
{middleware_human_approval, #{
    mode => all,                     %% all | selective | custom | none
    tools_requiring_approval => [<<"dangerous_tool">>],  %% Tools requiring approval in selective mode
    approval_fn => fun(FunctionName, Ctx) -> boolean() end,  %% Custom approval function
    approval_handler => fun(FunctionName, Ctx) -> approve | reject end,  %% Sync approval handler
    timeout => 60000,                %% Approval timeout (ms)
    timeout_action => reject         %% Timeout action: reject | confirm
}}
```

### 3. middleware_tool_retry - Tool Retry

Automatically retries when tool execution fails.

**Hook**: `post_invocation`
**Priority**: 80

```erlang
{middleware_tool_retry, #{
    max_retries => 3,                %% Maximum retry count
    backoff => #{
        type => exponential,         %% Backoff type: exponential | linear | constant
        initial_delay => 1000,       %% Initial delay (ms)
        max_delay => 30000,          %% Maximum delay (ms)
        multiplier => 2              %% Exponential factor
    },
    retryable_errors => all,         %% all | [error_type]
    retry_fn => fun(Error, Ctx) -> boolean() end,  %% Custom retry decision
    on_retry => fun(Error, RetryCount, Delay, Ctx) -> ok end,  %% Retry callback
    enable_delay => true             %% Whether to enable backoff delay
}}
```

**Helper functions:**

```erlang
%% Check if error is retryable
middleware_tool_retry:is_retryable(Error, MwState) -> boolean().

%% Calculate backoff delay
middleware_tool_retry:calculate_delay(RetryCount, BackoffConfig) -> pos_integer().
```

### 4. middleware_model_retry - Model Retry

Automatically retries when LLM call fails (with Jitter support).

**Hook**: `post_chat`
**Priority**: 90

```erlang
{middleware_model_retry, #{
    max_retries => 3,
    backoff => #{
        type => exponential,
        initial_delay => 1000,
        max_delay => 30000,
        multiplier => 2,
        jitter => true               %% Enable random jitter (avoid thundering herd)
    },
    retryable_errors => [timeout, rate_limit, server_error],
    retry_fn => fun(Error, Ctx) -> boolean() end,
    on_retry => fun(Error, RetryCount, Delay, Ctx) -> ok end
}}
```

### 5. middleware_model_fallback - Model Fallback

Switches to backup model when primary model fails.

**Hook**: `post_chat`
**Priority**: 95 (executes last)

```erlang
{middleware_model_fallback, #{
    fallback_models => [
        #{provider => openai, model => <<"gpt-3.5-turbo">>},
        #{provider => ollama, model => <<"llama2">>}
    ],
    trigger_errors => [rate_limit, timeout],  %% Error types that trigger fallback
    on_fallback => fun(OriginalError, FallbackModel) -> ok end  %% Fallback callback
}}
```

---

## Preset Configurations

### Using Presets

```erlang
%% Default configuration (call_limit + model_retry)
Middlewares = beamai_middleware_presets:default().

%% Minimal configuration (call_limit only)
Middlewares = beamai_middleware_presets:minimal().

%% Production environment (call_limit + tool_retry + model_retry + model_fallback)
Middlewares = beamai_middleware_presets:production().

%% Development debugging (relaxed call_limit + tool_retry)
Middlewares = beamai_middleware_presets:development().

%% Human approval (call_limit + human_approval)
Middlewares = beamai_middleware_presets:human_in_loop().
```

### Preset Comparison

| Preset | call_limit | tool_retry | model_retry | model_fallback | human_approval |
|--------|------------|------------|-------------|----------------|----------------|
| default | ✓ | - | ✓ | - | - |
| minimal | ✓ | - | - | - | - |
| production | ✓ (strict) | ✓ | ✓ | ✓ | - |
| development | ✓ (relaxed) | ✓ | - | - | - |
| human_in_loop | ✓ | - | - | - | ✓ |

### Custom Preset Options

```erlang
%% Customize default preset parameters
Middlewares = beamai_middleware_presets:default(#{
    call_limit => #{max_model_calls => 30},
    model_retry => #{max_retries => 5}
}).

%% Extend preset
Middlewares = beamai_middleware_presets:production() ++ [
    {my_custom_middleware, #{option => value}}
].
```

### Get Individual Middleware Config

```erlang
%% Get individual Middleware default configuration
CallLimit = beamai_middleware_presets:call_limit().
CallLimit2 = beamai_middleware_presets:call_limit(#{max_model_calls => 50}).

HumanApproval = beamai_middleware_presets:human_approval().
ToolRetry = beamai_middleware_presets:tool_retry().
ModelRetry = beamai_middleware_presets:model_retry().
ModelFallback = beamai_middleware_presets:model_fallback().
```

---

## Custom Middleware

### Basic Structure

```erlang
-module(my_middleware).
-behaviour(beamai_middleware).

%% Export callback functions (all callbacks are optional)
-export([init/1, pre_chat/2, post_chat/2,
         pre_invocation/2, post_invocation/2]).

%% Initialize Middleware state
init(Opts) ->
    #{
        my_option => maps:get(my_option, Opts, default_value),
        counter => 0
    }.

%% Before LLM call
pre_chat(FilterCtx, MwState) ->
    %% FilterCtx: filter context
    %% MwState: Middleware internal state
    ok.

%% After LLM response
post_chat(FilterCtx, MwState) ->
    case maps:get(result, FilterCtx, ok) of
        {error, _} ->
            %% Can decide to retry
            ok;
        ok ->
            ok
    end.

%% Before tool execution
pre_invocation(FilterCtx, MwState) ->
    FuncName = maps:get(name, maps:get(function, FilterCtx, #{}), <<>>),
    case is_blocked(FuncName) of
        true -> {error, {blocked_tool, FuncName}};
        false -> ok
    end.

%% After tool execution
post_invocation(FilterCtx, MwState) ->
    ok.
```

### Complete Example: Call Counter

```erlang
-module(middleware_counter).
-behaviour(beamai_middleware).

-export([init/1, pre_chat/2, pre_invocation/2]).

init(Opts) ->
    #{
        max_calls => maps:get(max_calls, Opts, 10),
        model_count => 0,
        tool_count => 0
    }.

%% Before model call - check and increment count
pre_chat(_FilterCtx, #{max_calls := MaxCalls, model_count := Count} = MwState) ->
    case Count >= MaxCalls of
        true ->
            {error, {model_call_limit_exceeded, Count}};
        false ->
            %% Update internal state (via runner's set_middleware_state)
            ok
    end.

%% Before tool call - count
pre_invocation(_FilterCtx, #{tool_count := Count} = _MwState) ->
    logger:info("Tool call #~p", [Count + 1]),
    ok.
```

### Complete Example: Request Logger

```erlang
-module(middleware_logger).
-behaviour(beamai_middleware).

-export([init/1, pre_chat/2, post_chat/2, pre_invocation/2, post_invocation/2]).

init(Opts) ->
    #{log_level => maps:get(log_level, Opts, info)}.

pre_chat(_FilterCtx, #{log_level := Level}) ->
    log(Level, ">>> LLM call starting"),
    ok.

post_chat(FilterCtx, #{log_level := Level}) ->
    case maps:get(result, FilterCtx, ok) of
        ok -> log(Level, "<<< LLM call succeeded");
        {error, Reason} -> log(Level, "<<< LLM call failed: ~p", [Reason])
    end,
    ok.

pre_invocation(FilterCtx, #{log_level := Level}) ->
    FuncName = maps:get(name, maps:get(function, FilterCtx, #{}), <<"unknown">>),
    Args = maps:get(args, FilterCtx, #{}),
    log(Level, ">>> Tool ~ts called with ~p", [FuncName, Args]),
    ok.

post_invocation(FilterCtx, #{log_level := Level}) ->
    case maps:get(result, FilterCtx, ok) of
        ok -> log(Level, "<<< Tool execution succeeded");
        {error, Reason} -> log(Level, "<<< Tool execution failed: ~p", [Reason])
    end,
    ok.

log(info, Fmt) -> logger:info(Fmt);
log(info, Fmt, Args) -> logger:info(Fmt, Args);
log(debug, Fmt) -> logger:debug(Fmt);
log(debug, Fmt, Args) -> logger:debug(Fmt, Args).
```

---

## Configuration and Usage

### Integration with Kernel

```erlang
%% Method 1: Using beamai_plugins with_middleware
Kernel = beamai_kernel:new(),
Kernel1 = beamai_plugins:load_all(Kernel, [beamai_plugin_file, beamai_plugin_shell]),
Kernel2 = beamai_plugins:with_middleware(Kernel1,
    beamai_middleware_presets:production()),

%% Method 2: Manually initialize Middleware chain
Chain = beamai_middleware_runner:init([
    {middleware_call_limit, #{max_model_calls => 15}},
    {middleware_tool_retry, #{max_retries => 5}},
    {my_custom_middleware, #{option => value}}
]).

%% Method 3: Convert to Kernel Filters
Filters = beamai_middleware_runner:to_filters(Chain),
Kernel3 = beamai_kernel:add_filter(Kernel, Filters).
```

### Middleware Configuration Format

```erlang
%% Full format: {module, options, priority}
{middleware_call_limit, #{max_model_calls => 20}, 10}

%% Omit priority: {module, options} (uses default priority 100)
{middleware_call_limit, #{max_model_calls => 20}}

%% Module name only (uses default options and priority)
middleware_call_limit
```

### Priority Description

- Lower values execute first
- Default priority is 100
- Built-in Middleware preset priorities:
  - 10: call_limit (boundary checks, executes first)
  - 50: human_approval (requires interaction)
  - 80: tool_retry (recovery mechanism)
  - 90: model_retry (recovery mechanism)
  - 95: model_fallback (last resort fallback)

---

## Advanced Usage

### Direct Hook Execution

```erlang
%% Initialize Middleware chain
Chain = beamai_middleware_runner:init([
    {middleware_call_limit, #{max_model_calls => 10}},
    {middleware_tool_retry, #{max_retries => 3}}
]),

%% Execute hook directly
FilterCtx = #{function => #{name => <<"my_tool">>}, args => #{}},
Result = beamai_middleware_runner:run_hook(pre_invocation, FilterCtx, Chain).
```

### Managing Middleware State

```erlang
%% Get Middleware internal state
{ok, State} = beamai_middleware_runner:get_middleware_state(middleware_call_limit, Chain).

%% Update Middleware internal state
NewChain = beamai_middleware_runner:set_middleware_state(
    middleware_call_limit,
    State#{model_call_count => 0},  %% Reset count
    Chain
).
```

### Modifying Context Passing

```erlang
%% Modify arguments in pre_invocation
pre_invocation(FilterCtx, _MwState) ->
    Args = maps:get(args, FilterCtx, #{}),
    %% Add default arguments
    NewArgs = maps:merge(#{<<"timeout">> => 30000}, Args),
    {continue, FilterCtx#{args => NewArgs}}.
```

### Conditional Skip

```erlang
%% Skip tool execution, return cached result
pre_invocation(FilterCtx, #{cache := Cache} = _MwState) ->
    FuncName = maps:get(name, maps:get(function, FilterCtx, #{}), <<>>),
    Args = maps:get(args, FilterCtx, #{}),
    CacheKey = {FuncName, Args},
    case maps:get(CacheKey, Cache, undefined) of
        undefined -> ok;  %% Cache miss, continue execution
        CachedResult -> {skip, CachedResult}  %% Return cached result
    end.
```

---

## API Reference

### beamai_middleware Behaviour

```erlang
-type middleware_state() :: map().
-type hook_name() :: pre_chat | post_chat | pre_invocation | post_invocation.
-type middleware_result() :: ok
                           | {continue, UpdatedFilterCtx :: map()}
                           | {skip, Term :: term()}
                           | {error, Reason :: term()}.

%% All callbacks are optional
-callback init(Opts :: map()) -> middleware_state().
-callback pre_chat(FilterCtx :: map(), MwState :: middleware_state()) -> middleware_result().
-callback post_chat(FilterCtx :: map(), MwState :: middleware_state()) -> middleware_result().
-callback pre_invocation(FilterCtx :: map(), MwState :: middleware_state()) -> middleware_result().
-callback post_invocation(FilterCtx :: map(), MwState :: middleware_state()) -> middleware_result().
```

### beamai_middleware_runner

```erlang
%% Initialize Middleware chain
-spec init([middleware_spec()]) -> middleware_chain().

%% Convert to Kernel Filters
-spec to_filters(middleware_chain()) -> [beamai_filter:filter_def()].

%% Execute hook
-spec run_hook(hook_name(), FilterCtx :: map(), middleware_chain()) ->
    ok | {continue, map()} | {skip, term()} | {error, term()}.

%% Get/Set Middleware state
-spec get_middleware_state(module(), middleware_chain()) -> {ok, state()} | {error, not_found}.
-spec set_middleware_state(module(), state(), middleware_chain()) -> middleware_chain().
```

### beamai_middleware_presets

```erlang
%% Preset configurations
-spec default() -> [middleware_spec()].
-spec default(map()) -> [middleware_spec()].
-spec minimal() -> [middleware_spec()].
-spec minimal(map()) -> [middleware_spec()].
-spec production() -> [middleware_spec()].
-spec production(map()) -> [middleware_spec()].
-spec development() -> [middleware_spec()].
-spec development(map()) -> [middleware_spec()].
-spec human_in_loop() -> [middleware_spec()].
-spec human_in_loop(map()) -> [middleware_spec()].

%% Individual Middleware configuration
-spec call_limit() -> middleware_spec().
-spec call_limit(map()) -> middleware_spec().
-spec human_approval() -> middleware_spec().
-spec human_approval(map()) -> middleware_spec().
-spec tool_retry() -> middleware_spec().
-spec tool_retry(map()) -> middleware_spec().
-spec model_retry() -> middleware_spec().
-spec model_retry(map()) -> middleware_spec().
-spec model_fallback() -> middleware_spec().
-spec model_fallback(map()) -> middleware_spec().
```

---

## More Resources

- [beamai_plugin README](../apps/beamai_plugin/README.md) - Plugin module documentation
- [beamai_core README](../apps/beamai_core/README.md) - Kernel architecture documentation
- [API Reference](API_REFERENCE.md) - API reference documentation
