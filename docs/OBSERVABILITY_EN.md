# Observability

BeamAI ships a dependency-free telemetry dispatch. Critical paths emit plain-data
events without coupling business code to a monitoring backend. Applications can
inject handlers for logs, metrics, tracing, or test collectors.

## Event shape

```erlang
#{
    name => [beamai, llm, request, started],
    level => info,
    timestamp => nanoseconds,
    data => #{},       %% conservative domain summary; no bulk payloads
    metadata => #{}    %% run_id / request_id / conversation_id / batch_id, etc.
}
```

## Handlers

```erlang
Handler = fun(#{name := Name, data := Data, metadata := Meta}) ->
    collect(Name, Data, Meta)
end,
ok = beamai_telemetry:add_handler(app_trace, Handler).
```

Installing the same `id` replaces the old handler. Use
`beamai_telemetry:remove_handler(app_trace)` to remove it. A crashing handler is
caught by the dispatch layer and converted to a warning; LLM, Tool, and Agent
execution continues.

## Levels and default logging

`info`, `notice`, `warning`, and `error` are enabled by default. High-frequency
events must use `debug` and opt in explicitly.

```erlang
ok = beamai_telemetry:set_config(#{
    enabled => true,
    enabled_levels => [info, warning, error],
    handlers => [#{id => file_logger,
                   handler => fun beamai_telemetry:default_logger/1}]
}).
```

The built-in `default_logger/1` logs only the event name, level, and standard
correlation IDs. It never formats `data`, so prompts, full tool results, and API
keys do not enter the default log.

## Event naming

The planned stable names are listed below. Business modules must not invent a
different name for the same semantic event:

| Path | Events |
| --- | --- |
| LLM | `beamai.llm.request.started/finished`, `beamai.llm.retry`, `beamai.llm.stream.started/finished` |
| Tool | `beamai.tool.queued/started/finished/timeout/crash` |
| Agent | `beamai.agent.run.started/finished`, `beamai.agent.turn.started/finished`, `beamai.agent.interrupted/resumed/state_changed` |
| HTTP Pool | `beamai.http_pool.acquired/reused/failed/wait_timeout/idle_reaped` |

Events should carry `request_id`, `run_id`, `conversation_id`, or `batch_id` in
`metadata` whenever possible so one request or Agent run can be reconstructed.
