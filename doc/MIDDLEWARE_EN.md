# Middleware System Documentation

English | [中文](MIDDLEWARE.md)

The Middleware system in beamai_agent provides a flexible way to intercept, modify, and control various stages of Agent execution.

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

Middleware are interceptors in the Agent execution process that can:

- **Modify Input/Output**: Modify messages before and after LLM calls
- **Control Flow**: Skip, retry, or abort execution
- **Add Functionality**: Logging, monitoring, human approval, etc.
- **Enforce Limits**: Call count limits, token limits, etc.

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                        Agent Execution                        │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐                                           │
│  │ before_agent │  ← Before Agent starts                     │
│  └──────┬───────┘                                           │
│         │                                                    │
│         ▼                                                    │
│  ┌────────────────────────────────────────────────────────┐ │
│  │                    Agent Loop                          │ │
│  │  ┌──────────────┐                                      │ │
│  │  │ before_model │  ← Before LLM call                   │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  │         ▼                                              │ │
│  │  ┌──────────────┐                                      │ │
│  │  │   LLM Call   │                                      │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  │         ▼                                              │ │
│  │  ┌──────────────┐                                      │ │
│  │  │ after_model  │  ← After LLM response                │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  │         ▼                                              │ │
│  │  ┌──────────────┐                                      │ │
│  │  │ before_tools │  ← Before tool execution             │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  │         ▼                                              │ │
│  │  ┌──────────────┐                                      │ │
│  │  │Tool Execution│                                      │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  │         ▼                                              │ │
│  │  ┌──────────────┐                                      │ │
│  │  │ after_tools  │  ← After tool execution              │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  └─────────┴──────────────────────────────────────────────┘ │
│         │                                                    │
│         ▼                                                    │
│  ┌──────────────┐                                           │
│  │ after_agent  │  ← After Agent ends                        │
│  └──────────────┘                                           │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## Lifecycle Hooks

### Hook List

| Hook | Trigger Time | Typical Use |
|------|----------|----------|
| `before_agent` | Before Agent execution starts | Initialize counters, record start time |
| `after_agent` | After Agent execution ends | Clean up resources, record end state |
| `before_model` | Before each LLM call | Check limits, modify messages, add context |
| `after_model` | After LLM returns | Process response, log, trigger follow-up actions |
| `before_tools` | Before tool execution | Human approval, parameter validation, tool filtering |
| `after_tools` | After tool execution | Result validation, failure retry, result transformation |

### Return Value Types

Middleware hook functions can return the following values:

```erlang
%% No modification, continue execution
ok

%% Update graph state
{update, #{key => value}}

%% Jump to specified node
{goto, model | tools | '__end__'}

%% Update state and jump
{update_goto, #{key => value}, model | tools | '__end__'}

%% Abort execution and return error
{halt, Reason}

%% Interrupt and wait for user confirmation
{interrupt, #{type => tool_approval, data => Data}}
```

---

## Built-in Middleware

### 1. middleware_call_limit - Call Limit

Limits various call counts during Agent execution.

```erlang
{middleware_call_limit, #{
    max_model_calls => 20,           %% Maximum model call count
    max_tool_calls => 50,            %% Maximum total tool call count
    max_tool_calls_per_turn => 10,   %% Maximum tool calls per turn
    max_iterations => 15,            %% Maximum iterations
    on_limit_exceeded => halt        %% Action when limit exceeded: halt | warn_and_continue
}}
```

### 2. middleware_summarization - Context Summarization

Automatically compresses long conversation history.

```erlang
{middleware_summarization, #{
    window_size => 20,               %% Keep most recent N messages
    max_tokens => 4000,              %% Token limit
    summarize => true,               %% Whether to generate summary
    compress_threshold => 30         %% Message count threshold to trigger compression
}}
```

### 3. middleware_human_approval - Human Approval

Requests human confirmation before tool execution.

```erlang
{middleware_human_approval, #{
    mode => all,                     %% all | selective | custom | none
    timeout => 60000,                %% Approval timeout (ms)
    timeout_action => reject,        %% Timeout action: reject | approve
    tools => [<<"dangerous_tool">>]  %% Tools requiring approval in selective mode
}}
```

### 4. middleware_tool_retry - Tool Retry

Automatically retries when tool execution fails.

```erlang
{middleware_tool_retry, #{
    max_retries => 3,                %% Maximum retry count
    backoff => #{
        type => exponential,         %% Backoff type: exponential | linear | constant
        initial_delay => 1000,       %% Initial delay (ms)
        max_delay => 30000,          %% Maximum delay (ms)
        multiplier => 2              %% Exponential factor
    },
    retryable_errors => all          %% all | [error_type]
}}
```

### 5. middleware_model_retry - Model Retry

Automatically retries when LLM call fails.

```erlang
{middleware_model_retry, #{
    max_retries => 3,
    backoff => #{type => exponential, initial_delay => 1000},
    retryable_errors => [timeout, rate_limit, server_error]
}}
```

### 6. middleware_model_fallback - Model Fallback

Switches to backup model when primary model fails.

```erlang
{middleware_model_fallback, #{
    fallback_models => [
        #{provider => openai, model => <<"gpt-3.5-turbo">>},
        #{provider => ollama, model => <<"llama2">>}
    ],
    trigger_errors => [rate_limit, timeout]
}}
```

### 7. middleware_pii_detection - PII Detection

Detects and handles personally identifiable information.

```erlang
{middleware_pii_detection, #{
    action => mask,                  %% mask | warn | block
    types => [email, phone, id_card],
    mask_char => <<"*">>
}}
```

### 8. middleware_tool_selector - Tool Selector

Dynamically selects available tools based on context.

```erlang
{middleware_tool_selector, #{
    strategy => context_based,       %% all | context_based | whitelist
    whitelist => [<<"search">>, <<"calculate">>],
    max_tools => 10
}}
```

### 9. middleware_todo_list - TODO Management

Provides task tracking capability for Agent.

```erlang
{middleware_todo_list, #{
    auto_create => true,             %% Auto-create TODO
    max_items => 20
}}
```

### 10. middleware_shell_tool - Shell Tool

Provides secure shell command execution.

```erlang
{middleware_shell_tool, #{
    allowed_commands => [<<"ls">>, <<"cat">>, <<"grep">>],
    timeout => 30000,
    sandbox => true
}}
```

### 11. middleware_file_search - File Search

Provides file and code search capability.

```erlang
{middleware_file_search, #{
    root_path => <<"/project">>,
    max_results => 100,
    excluded_paths => [<<"node_modules">>, <<".git">>]
}}
```

### 12. middleware_context_editing - Context Editing

Allows dynamic modification of conversation context.

```erlang
{middleware_context_editing, #{
    allow_message_deletion => true,
    allow_message_modification => false
}}
```

### 13. middleware_tool_emulator - Tool Emulator

Emulates tool responses in test environments.

```erlang
{middleware_tool_emulator, #{
    enabled => true,
    responses => #{
        <<"search">> => #{result => <<"mock search result">>}
    }
}}
```

---

## Preset Configurations

### Using Presets

```erlang
%% Default configuration
Middlewares = beamai_middleware_presets:default().

%% Minimal configuration
Middlewares = beamai_middleware_presets:minimal().

%% Production environment
Middlewares = beamai_middleware_presets:production().

%% Development debugging
Middlewares = beamai_middleware_presets:development().

%% Human approval
Middlewares = beamai_middleware_presets:human_in_loop().
```

### Preset Comparison

| Preset | call_limit | summarization | tool_retry | human_approval |
|------|------------|---------------|------------|----------------|
| default | ✓ | ✓ | - | - |
| minimal | ✓ | - | - | - |
| production | ✓ (strict) | ✓ | ✓ | - |
| development | ✓ (relaxed) | ✓ (debug) | ✓ | - |
| human_in_loop | ✓ | ✓ | - | ✓ |

### Custom Preset Options

```erlang
%% Customize default preset
Middlewares = beamai_middleware_presets:default(#{
    call_limit => #{max_model_calls => 30},
    summarization => #{window_size => 30}
}).

%% Extend preset
Middlewares = beamai_middleware_presets:default() ++ [
    {my_custom_middleware, #{option => value}}
].
```

---

## Custom Middleware

### Basic Structure

```erlang
-module(my_middleware).
-behaviour(beamai_middleware).

%% Export callback functions (all callbacks are optional)
-export([init/1, before_agent/2, after_agent/2,
         before_model/2, after_model/2,
         before_tools/2, after_tools/2]).

%% Initialize Middleware state
init(Opts) ->
    #{
        my_option => maps:get(my_option, Opts, default_value),
        counter => 0
    }.

%% Before Agent starts
before_agent(State, MwState) ->
    %% State: graph state (graph_state)
    %% MwState: Middleware internal state
    ok.

%% After Agent ends
after_agent(State, MwState) ->
    ok.

%% Before LLM call
before_model(State, MwState) ->
    %% Example: Add system message
    Messages = graph_state:get(State, messages, []),
    NewMsg = #{role => system, content => <<"Be concise.">>},
    {update, #{messages => [NewMsg | Messages]}}.

%% After LLM response
after_model(State, MwState) ->
    ok.

%% Before tool execution
before_tools(State, MwState) ->
    %% Example: Check dangerous tools
    PendingTools = graph_state:get(State, pending_tools, []),
    case contains_dangerous_tool(PendingTools) of
        true -> {halt, dangerous_tool_blocked};
        false -> ok
    end.

%% After tool execution
after_tools(State, MwState) ->
    ok.
```

### Complete Example: Call Counter

```erlang
-module(middleware_counter).
-behaviour(beamai_middleware).

-export([init/1, before_agent/2, before_model/2, after_agent/2]).

%% Initialize
init(Opts) ->
    #{
        max_calls => maps:get(max_calls, Opts, 10),
        current_calls => 0
    }.

%% Agent start - Reset counter
before_agent(_State, MwState) ->
    %% Store counter in graph state
    {update, #{middleware_counter => 0}}.

%% Before model call - Check and increment count
before_model(State, #{max_calls := MaxCalls} = MwState) ->
    Count = graph_state:get(State, middleware_counter, 0),
    case Count >= MaxCalls of
        true ->
            logger:warning("Middleware: Call limit exceeded (~p/~p)", [Count, MaxCalls]),
            {halt, {call_limit_exceeded, Count}};
        false ->
            {update, #{middleware_counter => Count + 1}}
    end.

%% Agent end - Record statistics
after_agent(State, _MwState) ->
    FinalCount = graph_state:get(State, middleware_counter, 0),
    logger:info("Middleware: Total model calls: ~p", [FinalCount]),
    ok.
```

### Complete Example: Request Logger

```erlang
-module(middleware_logger).
-behaviour(beamai_middleware).

-export([init/1, before_model/2, after_model/2, before_tools/2, after_tools/2]).

init(Opts) ->
    #{
        log_level => maps:get(log_level, Opts, info),
        include_content => maps:get(include_content, Opts, false)
    }.

before_model(State, #{log_level := Level, include_content := IncludeContent}) ->
    Messages = graph_state:get(State, messages, []),
    case IncludeContent of
        true ->
            log(Level, ">>> LLM Request: ~p messages~n~p", [length(Messages), Messages]);
        false ->
            log(Level, ">>> LLM Request: ~p messages", [length(Messages)])
    end,
    %% Record start time
    {update, #{mw_model_start_time => erlang:system_time(millisecond)}}.

after_model(State, #{log_level := Level}) ->
    StartTime = graph_state:get(State, mw_model_start_time, 0),
    Duration = erlang:system_time(millisecond) - StartTime,
    Response = graph_state:get(State, last_llm_response, #{}),
    Content = maps:get(content, Response, <<>>),
    log(Level, "<<< LLM Response (~pms): ~p chars", [Duration, byte_size(Content)]),
    ok.

before_tools(State, #{log_level := Level}) ->
    Tools = graph_state:get(State, pending_tools, []),
    ToolNames = [maps:get(name, T, unknown) || T <- Tools],
    log(Level, ">>> Tools to execute: ~p", [ToolNames]),
    ok.

after_tools(State, #{log_level := Level}) ->
    Results = graph_state:get(State, tool_results, []),
    log(Level, "<<< Tool results: ~p items", [length(Results)]),
    ok.

%% Internal log function
log(debug, Fmt, Args) -> logger:debug(Fmt, Args);
log(info, Fmt, Args) -> logger:info(Fmt, Args);
log(warning, Fmt, Args) -> logger:warning(Fmt, Args);
log(error, Fmt, Args) -> logger:error(Fmt, Args).
```

### Complete Example: Sensitive Word Filter

```erlang
-module(middleware_content_filter).
-behaviour(beamai_middleware).

-export([init/1, after_model/2]).

init(Opts) ->
    #{
        blocked_words => maps:get(blocked_words, Opts, []),
        replacement => maps:get(replacement, Opts, <<"[FILTERED]">>),
        action => maps:get(action, Opts, replace)  %% replace | block | warn
    }.

after_model(State, #{blocked_words := BlockedWords, replacement := Replacement, action := Action}) ->
    Response = graph_state:get(State, last_llm_response, #{}),
    Content = maps:get(content, Response, <<>>),

    case check_content(Content, BlockedWords) of
        {found, Word} ->
            case Action of
                block ->
                    {halt, {blocked_content, Word}};
                warn ->
                    logger:warning("Blocked word detected: ~p", [Word]),
                    ok;
                replace ->
                    FilteredContent = filter_content(Content, BlockedWords, Replacement),
                    NewResponse = Response#{content => FilteredContent},
                    {update, #{last_llm_response => NewResponse}}
            end;
        clean ->
            ok
    end.

check_content(Content, BlockedWords) ->
    LowerContent = string:lowercase(binary_to_list(Content)),
    case lists:filter(fun(Word) ->
        string:find(LowerContent, string:lowercase(binary_to_list(Word))) =/= nomatch
    end, BlockedWords) of
        [] -> clean;
        [First|_] -> {found, First}
    end.

filter_content(Content, BlockedWords, Replacement) ->
    lists:foldl(fun(Word, Acc) ->
        binary:replace(Acc, Word, Replacement, [global])
    end, Content, BlockedWords).
```

---

## Configuration and Usage

### Using in Agent Configuration

```erlang
%% Method 1: Using presets
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    system_prompt => <<"You are helpful.">>,
    llm => LLMConfig,
    middlewares => beamai_middleware_presets:default()
}).

%% Method 2: Manual configuration
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    system_prompt => <<"You are helpful.">>,
    llm => LLMConfig,
    middlewares => [
        {middleware_call_limit, #{max_model_calls => 15}},
        {middleware_summarization, #{window_size => 20}},
        {my_custom_middleware, #{option => value}}
    ]
}).

%% Method 3: Mixed configuration
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    middlewares => beamai_middleware_presets:production() ++ [
        {middleware_logger, #{log_level => debug}}
    ]
}).
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
- Recommended ranges:
  - 10-30: Pre-checks (limits, validation)
  - 40-60: Core functionality (approval, retry)
  - 70-90: Post-processing (logging, monitoring)

---

## Advanced Usage

### Accessing Graph State

```erlang
before_model(State, MwState) ->
    %% Read state
    Messages = graph_state:get(State, messages, []),
    Context = graph_state:get(State, context, #{}),

    %% Check custom key
    MyData = graph_state:get(State, my_custom_key, undefined),

    %% Update state
    {update, #{
        messages => Messages ++ [NewMessage],
        my_custom_key => NewValue
    }}.
```

### Flow Control

```erlang
%% Skip tool execution, return directly to LLM
before_tools(State, _MwState) ->
    case should_skip_tools(State) of
        true -> {goto, model};
        false -> ok
    end.

%% End Agent immediately
after_model(State, _MwState) ->
    case is_final_answer(State) of
        true -> {goto, '__end__'};
        false -> ok
    end.
```

### Interrupt and Resume

```erlang
%% Request human confirmation
before_tools(State, _MwState) ->
    Tools = graph_state:get(State, pending_tools, []),
    case needs_approval(Tools) of
        true ->
            {interrupt, #{
                type => tool_approval,
                data => #{tools => Tools},
                timeout => 60000
            }};
        false ->
            ok
    end.
```

### Inter-Middleware Communication

```erlang
%% Share data through graph state
before_model(State, _MwState) ->
    %% Set data for other Middleware to use
    {update, #{shared_data => #{timestamp => erlang:system_time()}}}.

after_model(State, _MwState) ->
    %% Read data set by other Middleware
    SharedData = graph_state:get(State, shared_data, #{}),
    %% Use SharedData...
    ok.
```

---

## API Reference

### beamai_middleware Behaviour

```erlang
%% All callbacks are optional
-callback init(Opts :: map()) -> middleware_state().
-callback before_agent(State, MwState) -> middleware_result().
-callback after_agent(State, MwState) -> middleware_result().
-callback before_model(State, MwState) -> middleware_result().
-callback after_model(State, MwState) -> middleware_result().
-callback before_tools(State, MwState) -> middleware_result().
-callback after_tools(State, MwState) -> middleware_result().
```

### beamai_middleware_runner

```erlang
%% Initialize Middleware chain
-spec init([middleware_spec()]) -> middleware_chain().

%% Execute hook
-spec run_hook(hook_name(), graph_state(), middleware_chain()) -> run_result().

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
-spec production() -> [middleware_spec()].
-spec development() -> [middleware_spec()].
-spec human_in_loop() -> [middleware_spec()].

%% Individual Middleware configuration
-spec call_limit() -> middleware_spec().
-spec call_limit(map()) -> middleware_spec().
-spec summarization() -> middleware_spec().
-spec human_approval() -> middleware_spec().
-spec tool_retry() -> middleware_spec().
```

---

## More Resources

- [beamai_agent README](../apps/beamai_agent/README.md)
- [API Reference](API_REFERENCE.md)
- [Architecture Design](ARCHITECTURE.md)
