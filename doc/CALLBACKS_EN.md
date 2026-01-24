# Callback System Documentation

English | [中文](CALLBACKS.md)

BeamAI Agent provides an event-driven callback system for listening and responding to various events during Agent execution.

## Table of Contents

- [Overview](#overview)
- [Callback Types](#callback-types)
- [Usage](#usage)
- [Callback Metadata](#callback-metadata)
- [API Reference](#api-reference)
- [Usage Examples](#usage-examples)
- [Best Practices](#best-practices)

---

## Overview

The Callback system is one of the core components of BeamAI Agent, allowing developers to inject custom logic at critical points during Agent execution, used for:

- **Monitoring and Logging**: Recording LLM calls, tool executions, and other events
- **Debugging**: Tracing Agent execution flow, locating issues
- **Streaming Output**: Receiving LLM-generated tokens in real-time
- **Interrupt Control**: Interrupting execution via tool callbacks
- **Integration**: Integrating with external systems (monitoring, notifications)

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Agent Execution Flow                           │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌──────────────────┐                                               │
│  │  on_turn_start   │  ← Each turn starts                           │
│  └────────┬─────────┘                                               │
│           │                                                          │
│           ▼                                                          │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │                      Execution Loop                            │ │
│  │  ┌──────────────────┐                                          │ │
│  │  │   on_llm_call    │  ← LLM call                              │ │
│  │  └────────┬─────────┘                                          │ │
│  │           │                                                    │ │
│  │           ▼                                                    │ │
│  │  ┌──────────────────┐                                          │ │
│  │  │    on_token      │  ← Streaming tokens (one by one)         │ │
│  │  └────────┬─────────┘                                          │ │
│  │           │                                                    │ │
│  │           ▼                                                    │ │
│  │  ┌──────────────────┐                                          │ │
│  │  │  on_tool_call    │  ← Tool call (can return interrupt)      │ │
│  │  └────────┬─────────┘                                          │ │
│  │           │                                                    │ │
│  └───────────┴────────────────────────────────────────────────────┘ │
│           │                                                          │
│           ▼                                                          │
│  ┌──────────────────┐     ┌──────────────────┐                      │
│  │  on_turn_end     │  or │ on_turn_error    │                      │
│  └──────────────────┘     └──────────────────┘                      │
│                                                                      │
│  ┌──────────────────┐     ┌──────────────────┐                      │
│  │  on_interrupt    │     │   on_resume      │  ← Interrupt/Resume  │
│  └──────────────────┘     └──────────────────┘                      │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### Core Module

| Module | Location | Description |
|--------|----------|-------------|
| `beamai_agent_callbacks` | `apps/beamai_agent/src/` | Callback management and invocation |

---

## Callback Types

BeamAI supports 8 callback types, covering the critical lifecycle of Agent execution.

### Callback List

| Callback Name | Trigger Timing | Parameters | Return Value |
|---------------|----------------|------------|--------------|
| `on_turn_start` | Each turn starts | `(Metadata)` | `ok` |
| `on_turn_end` | Each turn ends | `(Metadata)` | `ok` |
| `on_turn_error` | Each turn errors | `(Error, Metadata)` | `ok` |
| `on_llm_call` | LLM call | `(Messages, Metadata)` | `ok` |
| `on_tool_call` | Tool call | `(FunctionName, Args)` | `ok \| {interrupt, Reason}` |
| `on_token` | Streaming token generated | `(TokenText, Metadata)` | `ok` |
| `on_interrupt` | Agent interrupted | `(InterruptState, Metadata)` | `ok` |
| `on_resume` | Agent resumes from interrupt | `(InterruptState, Metadata)` | `ok` |

### Turn Callbacks (3 types)

Lifecycle events for each Agent execution turn:

```erlang
#{
    on_turn_start => fun(Meta) ->
        io:format("Turn ~p started~n", [maps:get(turn_count, Meta)])
    end,
    on_turn_end => fun(Meta) ->
        io:format("Turn ~p ended~n", [maps:get(turn_count, Meta)])
    end,
    on_turn_error => fun(Error, Meta) ->
        logger:error("Execution error: ~p", [Error])
    end
}
```

### LLM Callback (1 type)

LLM call event:

```erlang
#{
    on_llm_call => fun(Messages, Meta) ->
        io:format("LLM call, message count: ~p~n", [length(Messages)])
    end
}
```

### Tool Callback (1 type)

Tool call event. Notably, `on_tool_call` can return `{interrupt, Reason}` to interrupt execution:

```erlang
#{
    on_tool_call => fun(FunctionName, Args) ->
        io:format("Calling tool: ~ts~n", [FunctionName]),
        case FunctionName of
            <<"dangerous_tool">> ->
                %% Interrupt execution, wait for human confirmation
                {interrupt, #{reason => require_approval, tool => FunctionName}};
            _ ->
                ok
        end
    end
}
```

### Token Callback (1 type)

Token generation event during streaming output:

```erlang
#{
    on_token => fun(TokenText, Meta) ->
        io:format("~ts", [TokenText])  %% Real-time output
    end
}
```

### Interrupt/Resume Callbacks (2 types)

Agent interrupt and resume events:

```erlang
#{
    on_interrupt => fun(InterruptState, Meta) ->
        io:format("Agent interrupted: ~p~n", [InterruptState])
    end,
    on_resume => fun(InterruptState, Meta) ->
        io:format("Agent resumed execution~n")
    end
}
```

---

## Usage

### Setting Callbacks at Agent Creation

```erlang
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

{ok, State} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"You are an assistant">>,
    callbacks => #{
        on_turn_start => fun(Meta) ->
            io:format("Execution started~n")
        end,
        on_llm_call => fun(Messages, Meta) ->
            io:format("LLM call, ~p messages~n", [length(Messages)])
        end,
        on_tool_call => fun(FuncName, Args) ->
            io:format("Tool: ~ts~n", [FuncName]),
            ok
        end,
        on_turn_end => fun(Meta) ->
            io:format("Execution completed~n")
        end
    }
}),

{ok, Result, _NewState} = beamai_agent:run(State, <<"Hello">>).
```

---

## Callback Metadata

Each callback receives a `Metadata` parameter (except `on_tool_call`), containing execution context information:

```erlang
Metadata = #{
    agent_id => <<"agent_123">>,     %% Agent ID
    agent_name => <<"my_agent">>,    %% Agent name
    turn_count => 1,                 %% Current turn number
    timestamp => 1705658400000       %% Millisecond timestamp
}.
```

### Using Metadata

```erlang
#{
    on_llm_call => fun(Messages, Meta) ->
        AgentName = maps:get(agent_name, Meta, <<"unknown">>),
        TurnCount = maps:get(turn_count, Meta, 0),
        logger:info("[~ts] Turn ~p LLM call, ~p messages",
            [AgentName, TurnCount, length(Messages)])
    end
}
```

---

## API Reference

### beamai_agent_callbacks

```erlang
-type callbacks() :: #{
    on_turn_start => fun((map()) -> ok),
    on_turn_end => fun((map()) -> ok),
    on_turn_error => fun((term(), map()) -> ok),
    on_llm_call => fun((list(), map()) -> ok),
    on_tool_call => fun((binary(), map()) -> ok | {interrupt, term()}),
    on_token => fun((binary(), map()) -> ok),
    on_interrupt => fun((term(), map()) -> ok),
    on_resume => fun((term(), map()) -> ok)
}.

%% Safely invoke callback (exceptions don't affect Agent execution)
-spec invoke(atom(), list(), callbacks()) -> ok.
beamai_agent_callbacks:invoke(CallbackName, Args, Callbacks).

%% Build callback metadata
-spec build_metadata(agent_state()) -> map().
beamai_agent_callbacks:build_metadata(AgentState).
```

### Callback Safety Mechanisms

The callback system provides the following safety features:

- **Exception Isolation**: Exceptions within callback functions are caught and don't affect the Agent main flow
- **Unregistered Ignored**: Calling unregistered callback names directly returns `ok`
- **Optional Callbacks**: All callbacks are optional, only register what you need

---

## Usage Examples

### Example 1: Logging

```erlang
LogCallbacks = #{
    on_turn_start => fun(Meta) ->
        logger:info("[~ts] Turn ~p started",
            [maps:get(agent_name, Meta, <<>>), maps:get(turn_count, Meta, 0)])
    end,
    on_llm_call => fun(Messages, Meta) ->
        logger:info("[~ts] LLM call, ~p messages",
            [maps:get(agent_name, Meta, <<>>), length(Messages)])
    end,
    on_tool_call => fun(FuncName, _Args) ->
        logger:info("Tool call: ~ts", [FuncName]),
        ok
    end,
    on_turn_end => fun(Meta) ->
        logger:info("[~ts] Turn ~p ended",
            [maps:get(agent_name, Meta, <<>>), maps:get(turn_count, Meta, 0)])
    end
}.

{ok, State} = beamai_agent:new(#{
    llm => LLM,
    callbacks => LogCallbacks
}).
```

### Example 2: Performance Monitoring

```erlang
PerfCallbacks = #{
    on_turn_start => fun(_Meta) ->
        put(turn_start_time, erlang:system_time(millisecond))
    end,
    on_llm_call => fun(_Messages, _Meta) ->
        put(llm_start_time, erlang:system_time(millisecond))
    end,
    on_turn_end => fun(Meta) ->
        StartTime = get(turn_start_time),
        Duration = erlang:system_time(millisecond) - StartTime,
        logger:info("Turn ~p duration: ~p ms",
            [maps:get(turn_count, Meta, 0), Duration])
    end
}.
```

### Example 3: Streaming Output

```erlang
StreamCallbacks = #{
    on_token => fun(TokenText, _Meta) ->
        %% Real-time output to terminal
        io:format("~ts", [TokenText])
    end,
    on_turn_end => fun(_Meta) ->
        io:format("~n")  %% Newline
    end
}.

{ok, State} = beamai_agent:new(#{
    llm => LLM,
    callbacks => StreamCallbacks
}).
```

### Example 4: Tool Approval (Interrupt Mechanism)

```erlang
%% Use on_tool_call's interrupt return value for human approval
ApprovalCallbacks = #{
    on_tool_call => fun(FuncName, Args) ->
        DangerousTools = [<<"delete_file">>, <<"execute_command">>],
        case lists:member(FuncName, DangerousTools) of
            true ->
                io:format("Tool ~ts requires approval, args: ~p~n", [FuncName, Args]),
                %% Interrupt execution
                {interrupt, #{
                    reason => require_approval,
                    tool => FuncName,
                    args => Args
                }};
            false ->
                ok
        end
    end,
    on_interrupt => fun(InterruptState, _Meta) ->
        io:format("Agent interrupted, awaiting approval: ~p~n", [InterruptState])
    end,
    on_resume => fun(_InterruptState, _Meta) ->
        io:format("Agent resumed execution~n")
    end
}.
```

### Example 5: WebSocket Notifications

```erlang
%% Push events to WebSocket clients
WsCallbacks = #{
    on_turn_start => fun(Meta) ->
        ws_send(Meta, #{type => <<"turn_start">>})
    end,
    on_token => fun(TokenText, Meta) ->
        ws_send(Meta, #{type => <<"token">>, content => TokenText})
    end,
    on_tool_call => fun(FuncName, Args) ->
        ws_send(#{}, #{type => <<"tool_call">>, tool => FuncName, args => Args}),
        ok
    end,
    on_turn_end => fun(Meta) ->
        ws_send(Meta, #{type => <<"turn_end">>})
    end,
    on_turn_error => fun(Error, Meta) ->
        ws_send(Meta, #{type => <<"error">>, error => Error})
    end
}.

ws_send(Meta, Message) ->
    AgentId = maps:get(agent_id, Meta, <<"unknown">>),
    websocket_handler:send(AgentId, jsx:encode(Message)).
```

---

## Best Practices

### 1. Keep Callbacks Lightweight

Callbacks should execute quickly to avoid blocking the Agent main flow:

```erlang
%% Recommended: Async handling
on_llm_call => fun(Messages, Meta) ->
    spawn(fun() -> log_to_external_service(Messages, Meta) end)
end

%% Avoid: Synchronous blocking operations
on_llm_call => fun(Messages, Meta) ->
    %% This will block the Agent
    httpc:request(post, {Url, [], "application/json", Body}, [], [])
end
```

### 2. Exception Safety

Exceptions within callbacks won't affect Agent execution, but it's recommended to add error handling:

```erlang
on_turn_end => fun(Meta) ->
    try
        process_turn_result(Meta)
    catch
        Class:Reason:Stack ->
            logger:warning("Callback handling failed: ~p:~p", [Class, Reason])
    end
end
```

### 3. Use on_tool_call Interrupt Wisely

`on_tool_call` is the only callback that can affect execution flow:

```erlang
%% Only use interrupt for dangerous operations
on_tool_call => fun(FuncName, Args) ->
    case requires_approval(FuncName, Args) of
        true -> {interrupt, #{tool => FuncName}};
        false -> ok  %% Most cases should return ok
    end
end
```

### 4. Use Metadata to Correlate Events

Use `turn_count` and `agent_id` to correlate events from the same Agent:

```erlang
on_turn_start => fun(Meta) ->
    ets:insert(agent_events, {
        {maps:get(agent_id, Meta), maps:get(turn_count, Meta)},
        #{start_time => erlang:system_time(millisecond)}
    })
end
```

---

## More Resources

- [beamai_agent README](../apps/beamai_agent/README.md) - Agent module documentation
- [MIDDLEWARE.md](MIDDLEWARE.md) - Middleware system documentation
- [API_REFERENCE.md](API_REFERENCE.md) - API reference documentation
