# Callback System Documentation

English | [中文](CALLBACKS.md)

BeamAI Framework provides an event-driven callback system similar to LangChain, used to listen and respond to various events during Agent execution.

## Table of Contents

- [Overview](#overview)
- [Callback Types](#callback-types)
- [Usage](#usage)
- [Callback Metadata](#callback-metadata)
- [API Reference](#api-reference)
- [Usage Examples](#usage-examples)
- [Best Practices](#best-practices)
- [Extension Development](#extension-development)

---

## Overview

The Callback system is one of the core components of BeamAI Agent, allowing developers to inject custom logic at critical points during Agent execution, used for:

- **Monitoring and Logging**: Recording LLM calls, tool executions, and other events
- **Debugging**: Tracing Agent execution flow, locating issues
- **Integration**: Integrating with external systems (monitoring, analytics, notifications)
- **Extension**: Adding functionality without modifying core code

### Architecture Diagram

```
+-------------------------------------------------------------------------+
|                           Agent Execution Flow                           |
+-------------------------------------------------------------------------+
|                                                                          |
|  +------------------+                                                   |
|  |  on_chain_start  |  <- Agent execution starts                        |
|  +--------+---------+                                                   |
|           |                                                              |
|           v                                                              |
|  +----------------------------------------------------------------------+ |
|  |                         Execution Loop                                | |
|  |  +------------------+                                              | |
|  |  |  on_llm_start    |  <- LLM call starts                          | |
|  |  +--------+---------+                                              | |
|  |           |                                                        | |
|  |           v                                                        | |
|  |  +------------------+     +------------------+                     | |
|  |  |    LLM Call      | --> |  on_llm_new_token|  <- Streaming Token | |
|  |  +--------+---------+     +------------------+                     | |
|  |           |                                                        | |
|  |           v                                                        | |
|  |  +------------------+     +------------------+                     | |
|  |  |  on_llm_end      |  or |  on_llm_error    |                     | |
|  |  +--------+---------+     +------------------+                     | |
|  |           |                                                        | |
|  |           v                                                        | |
|  |  +------------------+                                              | |
|  |  |    on_text       |  <- Text content generated                   | |
|  |  +--------+---------+                                              | |
|  |           |                                                        | |
|  |           v                                                        | |
|  |  +------------------+                                              | |
|  |  | on_agent_action  |  <- Agent decides to execute action          | |
|  |  +--------+---------+                                              | |
|  |           |                                                        | |
|  |           v                                                        | |
|  |  +------------------+                                              | |
|  |  |  on_tool_start   |  <- Tool execution starts                    | |
|  |  +--------+---------+                                              | |
|  |           |                                                        | |
|  |           v                                                        | |
|  |  +------------------+     +------------------+                     | |
|  |  |  on_tool_end     |  or |  on_tool_error   |                     | |
|  |  +--------+---------+     +------------------+                     | |
|  |           |                                                        | |
|  +-----------+------------------------------------------------------+ |
|           |                                                              |
|           v                                                              |
|  +------------------+     +------------------+                          |
|  | on_agent_finish  |  or |  on_chain_error  |                          |
|  +------------------+     +------------------+                          |
|           |                                                              |
|           v                                                              |
|  +------------------+                                                   |
|  |  on_chain_end    |  <- Agent execution ends                          |
|  +------------------+                                                   |
|                                                                          |
+-------------------------------------------------------------------------+
```

### Core Modules

| Module | Location | Description |
|--------|----------|-------------|
| `beamai_agent_callbacks` | `apps/beamai_agent/src/` | Callback manager |
| `beamai_callback_utils` | `apps/beamai_core/src/utils/` | Callback utility functions |
| `beamai_agent.hrl` | `apps/beamai_agent/include/` | Callback type definitions |

---

## Callback Types

BeamAI supports 18 callback types, covering the complete Agent lifecycle.

### LLM Callbacks (4 types)

| Callback Name | Trigger Timing | Parameters |
|---------------|----------------|------------|
| `on_llm_start` | LLM call starts | `(Prompts, Meta)` |
| `on_llm_end` | LLM call succeeds | `(Response, Meta)` |
| `on_llm_error` | LLM call fails | `(Error, Meta)` |
| `on_llm_new_token` | Streaming output new token | `(Token, Meta)` |

```erlang
%% LLM callback example
#{
    on_llm_start => fun(Prompts, Meta) ->
        io:format("LLM call started, message count: ~p~n", [length(Prompts)])
    end,
    on_llm_end => fun(Response, Meta) ->
        Content = maps:get(content, Response, <<>>),
        io:format("LLM response: ~ts~n", [Content])
    end,
    on_llm_error => fun(Error, Meta) ->
        logger:error("LLM call failed: ~p", [Error])
    end
}
```

### Tool Callbacks (3 types)

| Callback Name | Trigger Timing | Parameters |
|---------------|----------------|------------|
| `on_tool_start` | Tool execution starts | `(ToolName, Args, Meta)` |
| `on_tool_end` | Tool execution succeeds | `(ToolName, Result, Meta)` |
| `on_tool_error` | Tool execution fails | `(ToolName, Error, Meta)` |

```erlang
%% Tool callback example
#{
    on_tool_start => fun(ToolName, Args, Meta) ->
        io:format("Executing tool: ~ts~nParameters: ~p~n", [ToolName, Args])
    end,
    on_tool_end => fun(ToolName, Result, Meta) ->
        io:format("Tool ~ts completed: ~ts~n", [ToolName, Result])
    end,
    on_tool_error => fun(ToolName, Error, Meta) ->
        logger:warning("Tool ~ts failed: ~p", [ToolName, Error])
    end
}
```

### Agent Callbacks (2 types)

| Callback Name | Trigger Timing | Parameters |
|---------------|----------------|------------|
| `on_agent_action` | Agent decides to execute action | `(Action, Meta)` |
| `on_agent_finish` | Agent completes (no tool calls) | `(Result, Meta)` |

```erlang
%% Agent callback example
#{
    on_agent_action => fun(Action, Meta) ->
        %% Action contains tool call information
        ToolCalls = maps:get(tool_calls, Action, []),
        io:format("Agent action: ~p tool calls~n", [length(ToolCalls)])
    end,
    on_agent_finish => fun(Result, Meta) ->
        Content = maps:get(content, Result, <<>>),
        io:format("Agent finished: ~ts~n", [Content])
    end
}
```

### Chain Callbacks (3 types)

| Callback Name | Trigger Timing | Parameters |
|---------------|----------------|------------|
| `on_chain_start` | Chain/Agent execution starts | `(Input, Meta)` |
| `on_chain_end` | Chain/Agent execution succeeds | `(Output, Meta)` |
| `on_chain_error` | Chain/Agent execution fails | `(Error, Meta)` |

```erlang
%% Chain callback example
#{
    on_chain_start => fun(Input, Meta) ->
        io:format("Starting execution, input: ~ts~n", [Input])
    end,
    on_chain_end => fun(Output, Meta) ->
        io:format("Execution completed~n")
    end,
    on_chain_error => fun(Error, Meta) ->
        logger:error("Execution failed: ~p", [Error])
    end
}
```

### Retriever Callbacks (3 types) - RAG Related

| Callback Name | Trigger Timing | Parameters |
|---------------|----------------|------------|
| `on_retriever_start` | Retrieval starts | `(Query, Meta)` |
| `on_retriever_end` | Retrieval succeeds | `(Documents, Meta)` |
| `on_retriever_error` | Retrieval fails | `(Error, Meta)` |

```erlang
%% Retriever callback example
#{
    on_retriever_start => fun(Query, Meta) ->
        io:format("Starting retrieval: ~ts~n", [Query])
    end,
    on_retriever_end => fun(Documents, Meta) ->
        io:format("Retrieved ~p documents~n", [length(Documents)])
    end
}
```

### Other Callbacks (3 types)

| Callback Name | Trigger Timing | Parameters |
|---------------|----------------|------------|
| `on_text` | Text content generated | `(Text, Meta)` |
| `on_retry` | Triggered on retry | `(RetryState, Meta)` |
| `on_custom_event` | Custom event | `(EventName, Data, Meta)` |

```erlang
%% Other callback examples
#{
    on_text => fun(Text, Meta) ->
        %% Only triggered when content is non-empty
        io:format("Generated text: ~ts~n", [Text])
    end,
    on_retry => fun(RetryState, Meta) ->
        io:format("Retry: ~p~n", [RetryState])
    end,
    on_custom_event => fun(EventName, Data, Meta) ->
        io:format("Custom event ~p: ~p~n", [EventName, Data])
    end
}
```

---

## Usage

### Setting Callbacks at Initialization

```erlang
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    system_prompt => <<"You are an assistant"/utf8>>,
    llm => LLMConfig,
    callbacks => #{
        on_llm_start => fun(Prompts, Meta) ->
            io:format("LLM started~n")
        end,
        on_llm_end => fun(Response, Meta) ->
            io:format("LLM ended~n")
        end,
        on_tool_start => fun(ToolName, Args, Meta) ->
            io:format("Tool: ~ts~n", [ToolName])
        end
    }
}).
```

### Setting Callbacks Dynamically

```erlang
%% Set new callbacks
ok = beamai_agent:set_callbacks(Agent, #{
    on_llm_start => fun(Prompts, Meta) ->
        io:format("New LLM callback~n")
    end
}).

%% Get current callback configuration
CallbacksMap = beamai_agent:get_callbacks(Agent).
```

### Sending Custom Events

```erlang
%% Send custom event
beamai_agent:emit_custom_event(Agent, my_event, #{value => 42}).

%% Send custom event with metadata
beamai_agent:emit_custom_event(Agent, my_event, #{value => 42}, #{
    source => <<"my_module">>
}).
```

### Backward Compatibility

Supports legacy `on_complete` and `on_error` options:

```erlang
%% Old configuration style (still supported)
{ok, Agent} = beamai_agent:start_link(<<"agent">>, #{
    on_complete => fun(Result) -> io:format("Complete: ~p~n", [Result]) end,
    on_error => fun(Error) -> io:format("Error: ~p~n", [Error]) end
}).

%% Internally automatically converted to:
%% on_complete -> on_agent_finish
%% on_error -> on_chain_error
```

---

## Callback Metadata

Each callback receives a `Meta` parameter containing execution context information:

```erlang
Meta = #{
    agent_id => <<"agent_123">>,     %% Agent ID
    agent_name => <<"my_agent">>,    %% Agent name
    run_id => <<"uuid-...">>,        %% Current run ID (UUID format)
    timestamp => 1705658400000       %% Millisecond timestamp
}.
```

### Using Metadata

```erlang
#{
    on_llm_start => fun(Prompts, Meta) ->
        AgentName = maps:get(agent_name, Meta),
        RunId = maps:get(run_id, Meta),
        Timestamp = maps:get(timestamp, Meta),
        logger:info("[~ts] LLM started (run: ~ts, time: ~p)",
            [AgentName, RunId, Timestamp])
    end
}
```

---

## API Reference

### beamai_agent Callback API

```erlang
%% Set callback handlers
-spec set_callbacks(pid(), map()) -> ok.
beamai_agent:set_callbacks(Agent, CallbackOpts).

%% Get current callback configuration
-spec get_callbacks(pid()) -> map().
beamai_agent:get_callbacks(Agent).

%% Send custom event
-spec emit_custom_event(pid(), atom() | binary(), term()) -> ok.
beamai_agent:emit_custom_event(Agent, EventName, Data).

%% Send custom event (with metadata)
-spec emit_custom_event(pid(), atom() | binary(), term(), map()) -> ok.
beamai_agent:emit_custom_event(Agent, EventName, Data, Metadata).
```

### beamai_agent_callbacks Internal API

```erlang
%% Initialize callback handler
-spec init(map()) -> #callbacks{}.
beamai_agent_callbacks:init(Opts).

%% Update callback handler
-spec update(#callbacks{}, map()) -> #callbacks{}.
beamai_agent_callbacks:update(Callbacks, Opts).

%% Invoke callback function
-spec invoke(atom(), list(), #callbacks{}) -> ok.
beamai_agent_callbacks:invoke(CallbackName, Args, Callbacks).

%% Convert callback record to map
-spec to_map(#callbacks{}) -> map().
beamai_agent_callbacks:to_map(Callbacks).

%% Build callback metadata
-spec build_metadata(#state{}) -> map().
beamai_agent_callbacks:build_metadata(State).

%% Generate run ID
-spec generate_run_id() -> binary().
beamai_agent_callbacks:generate_run_id().
```

### beamai_callback_utils Utility Functions

```erlang
%% Invoke callback function (without metadata)
-spec invoke(atom(), list(), map()) -> ok.
beamai_callback_utils:invoke(CallbackName, Args, Callbacks).

%% Invoke callback function (with metadata)
-spec invoke(atom(), list(), map(), map()) -> ok.
beamai_callback_utils:invoke(CallbackName, Args, Callbacks, Meta).

%% Invoke callback from graph state
-spec invoke_from_state(atom(), list(), map()) -> ok.
beamai_callback_utils:invoke_from_state(CallbackName, Args, State).

%% Conditional callback invocation
-spec maybe_invoke(boolean(), atom(), list(), map()) -> ok.
beamai_callback_utils:maybe_invoke(Condition, CallbackName, Args, State).
```

### Invocation Macros

```erlang
%% Defined in beamai_common.hrl

%% Direct callback invocation
?INVOKE_CALLBACK(Name, Args, Callbacks, Meta)

%% Invoke callback from graph state
?INVOKE_CALLBACK_FROM_STATE(Name, Args, State)
```

---

## Usage Examples

### Example 1: Logging

```erlang
%% Create logging callbacks
LogCallbacks = #{
    on_llm_start => fun(Prompts, Meta) ->
        logger:info("[~ts] LLM started, message count: ~p",
            [maps:get(agent_name, Meta), length(Prompts)])
    end,
    on_llm_end => fun(Response, Meta) ->
        logger:info("[~ts] LLM completed",
            [maps:get(agent_name, Meta)])
    end,
    on_tool_start => fun(ToolName, Args, Meta) ->
        logger:info("[~ts] Tool ~ts started",
            [maps:get(agent_name, Meta), ToolName])
    end,
    on_tool_end => fun(ToolName, Result, Meta) ->
        logger:info("[~ts] Tool ~ts completed",
            [maps:get(agent_name, Meta), ToolName])
    end
}.

{ok, Agent} = beamai_agent:start_link(<<"log_agent">>, #{
    system_prompt => <<"...">>,
    llm => LLMConfig,
    callbacks => LogCallbacks
}).
```

### Example 2: Performance Monitoring

```erlang
%% Create performance monitoring callbacks
PerfCallbacks = #{
    on_llm_start => fun(_Prompts, Meta) ->
        %% Record start time in process dictionary
        put(llm_start_time, erlang:system_time(millisecond))
    end,
    on_llm_end => fun(Response, Meta) ->
        StartTime = get(llm_start_time),
        Duration = erlang:system_time(millisecond) - StartTime,
        %% Send to monitoring system
        metrics:histogram(<<"llm.duration_ms">>, Duration),
        logger:info("LLM duration: ~p ms", [Duration])
    end,
    on_tool_start => fun(ToolName, _Args, _Meta) ->
        put({tool_start_time, ToolName}, erlang:system_time(millisecond))
    end,
    on_tool_end => fun(ToolName, _Result, _Meta) ->
        StartTime = get({tool_start_time, ToolName}),
        Duration = erlang:system_time(millisecond) - StartTime,
        metrics:histogram(<<"tool.duration_ms">>, Duration, #{tool => ToolName})
    end
}.
```

### Example 3: Progress Notifications

```erlang
%% Create progress notification callbacks (e.g., sending to WebSocket)
NotifyCallbacks = #{
    on_chain_start => fun(Input, Meta) ->
        notify_client(maps:get(run_id, Meta), #{
            type => <<"start">>,
            input => Input
        })
    end,
    on_llm_new_token => fun(Token, Meta) ->
        notify_client(maps:get(run_id, Meta), #{
            type => <<"token">>,
            content => Token
        })
    end,
    on_tool_start => fun(ToolName, Args, Meta) ->
        notify_client(maps:get(run_id, Meta), #{
            type => <<"tool_start">>,
            tool => ToolName
        })
    end,
    on_chain_end => fun(Output, Meta) ->
        notify_client(maps:get(run_id, Meta), #{
            type => <<"end">>,
            output => Output
        })
    end
}.

notify_client(RunId, Message) ->
    websocket_handler:send(RunId, jsx:encode(Message)).
```

### Example 4: Debug Tracing

```erlang
%% Create debug callbacks
DebugCallbacks = #{
    on_llm_start => fun(Prompts, Meta) ->
        io:format("~n=== LLM Call Started ===~n"),
        io:format("Agent: ~ts~n", [maps:get(agent_name, Meta)]),
        io:format("Message count: ~p~n", [length(Prompts)]),
        lists:foreach(fun(Msg) ->
            Role = maps:get(role, Msg),
            Content = maps:get(content, Msg, <<>>),
            io:format("  [~ts] ~ts~n", [Role, truncate(Content, 100)])
        end, Prompts)
    end,
    on_llm_end => fun(Response, Meta) ->
        io:format("~n=== LLM Response ===~n"),
        Content = maps:get(content, Response, <<>>),
        ToolCalls = maps:get(tool_calls, Response, []),
        io:format("Content: ~ts~n", [truncate(Content, 200)]),
        io:format("Tool calls: ~p~n", [length(ToolCalls)])
    end,
    on_tool_start => fun(ToolName, Args, _Meta) ->
        io:format("~n>>> Executing tool: ~ts~n", [ToolName]),
        io:format("    Parameters: ~p~n", [Args])
    end,
    on_tool_end => fun(ToolName, Result, _Meta) ->
        io:format("<<< Tool completed: ~ts~n", [ToolName]),
        io:format("    Result: ~ts~n", [truncate(Result, 100)])
    end
}.

truncate(Bin, MaxLen) when byte_size(Bin) > MaxLen ->
    <<(binary:part(Bin, 0, MaxLen))/binary, "...">>;
truncate(Bin, _) -> Bin.
```

### Example 5: Async Event Handling

```erlang
%% Use process messages for async handling
Self = self(),

AsyncCallbacks = #{
    on_llm_end => fun(Response, Meta) ->
        Self ! {llm_complete, maps:get(run_id, Meta), Response}
    end,
    on_tool_end => fun(ToolName, Result, Meta) ->
        Self ! {tool_complete, maps:get(run_id, Meta), ToolName, Result}
    end,
    on_chain_end => fun(Output, Meta) ->
        Self ! {agent_complete, maps:get(run_id, Meta), Output}
    end
}.

%% Receive events asynchronously
receive
    {llm_complete, RunId, Response} ->
        handle_llm_response(RunId, Response);
    {tool_complete, RunId, ToolName, Result} ->
        handle_tool_result(RunId, ToolName, Result);
    {agent_complete, RunId, Output} ->
        handle_agent_output(RunId, Output)
after 30000 ->
    timeout
end.
```

---

## Best Practices

### 1. Keep Callbacks Lightweight

Callbacks should execute quickly to avoid blocking the Agent main flow:

```erlang
%% Recommended: Async handling
on_llm_end => fun(Response, Meta) ->
    spawn(fun() -> process_response(Response, Meta) end)
end

%% Avoid: Synchronous blocking operations
on_llm_end => fun(Response, Meta) ->
    %% This will block the Agent
    httpc:request(post, {Url, [], "application/json", Body}, [], [])
end
```

### 2. Handle Callback Exceptions

Exceptions within callbacks won't affect Agent execution, but it's recommended to add error handling:

```erlang
on_llm_end => fun(Response, Meta) ->
    try
        process_response(Response)
    catch
        Class:Reason:Stack ->
            logger:warning("Callback handling failed: ~p:~p~n~p",
                [Class, Reason, Stack])
    end
end
```

### 3. Use Metadata to Correlate Events

Use `run_id` to correlate all events from the same execution:

```erlang
%% Use ETS to store execution context
on_chain_start => fun(Input, Meta) ->
    RunId = maps:get(run_id, Meta),
    ets:insert(run_context, {RunId, #{
        start_time => erlang:system_time(millisecond),
        input => Input
    }})
end,

on_chain_end => fun(Output, Meta) ->
    RunId = maps:get(run_id, Meta),
    [{_, Context}] = ets:lookup(run_context, RunId),
    Duration = erlang:system_time(millisecond) - maps:get(start_time, Context),
    %% Log complete execution information
    log_execution(RunId, Context, Output, Duration),
    ets:delete(run_context, RunId)
end
```

### 4. Dynamically Enable/Disable Callbacks

```erlang
%% Decide whether to enable callbacks based on configuration
Callbacks = case os:getenv("DEBUG") of
    "true" -> debug_callbacks();
    _ -> #{}
end,

{ok, Agent} = beamai_agent:start_link(<<"agent">>, #{
    callbacks => Callbacks
}).
```

### 5. Combine Multiple Callback Handlers

```erlang
%% Merge multiple callback configurations
merge_callbacks(Callbacks1, Callbacks2) ->
    maps:fold(fun(Key, Handler2, Acc) ->
        case maps:get(Key, Acc, undefined) of
            undefined ->
                maps:put(Key, Handler2, Acc);
            Handler1 ->
                %% Create combined handler
                Combined = fun(Args...) ->
                    Handler1(Args...),
                    Handler2(Args...)
                end,
                maps:put(Key, Combined, Acc)
        end
    end, Callbacks1, Callbacks2).

%% Usage
AllCallbacks = merge_callbacks(
    merge_callbacks(LogCallbacks, PerfCallbacks),
    NotifyCallbacks
).
```

---

## Extension Development

### Creating Custom Callback Handler Module

```erlang
-module(my_callback_handler).
-export([callbacks/0, callbacks/1]).

%% Default callback configuration
callbacks() ->
    callbacks(#{}).

%% Callback configuration with options
callbacks(Opts) ->
    LogLevel = maps:get(log_level, Opts, info),
    #{
        on_llm_start => fun(Prompts, Meta) ->
            log(LogLevel, "LLM started: ~p messages", [length(Prompts)])
        end,
        on_llm_end => fun(Response, Meta) ->
            log(LogLevel, "LLM ended", [])
        end,
        on_tool_start => fun(ToolName, Args, Meta) ->
            log(LogLevel, "Tool ~ts started", [ToolName])
        end,
        on_tool_end => fun(ToolName, Result, Meta) ->
            log(LogLevel, "Tool ~ts ended", [ToolName])
        end
    }.

log(debug, Fmt, Args) -> logger:debug(Fmt, Args);
log(info, Fmt, Args) -> logger:info(Fmt, Args);
log(warning, Fmt, Args) -> logger:warning(Fmt, Args).
```

### Using Custom Handler

```erlang
{ok, Agent} = beamai_agent:start_link(<<"agent">>, #{
    callbacks => my_callback_handler:callbacks(#{log_level => debug})
}).
```

---

## More Resources

- [ARCHITECTURE.md](ARCHITECTURE.md) - Architecture design documentation
- [MIDDLEWARE.md](MIDDLEWARE.md) - Middleware system documentation
- [API_REFERENCE.md](API_REFERENCE.md) - API reference documentation
- [beamai_agent README](../apps/beamai_agent/README.md) - Agent module documentation
