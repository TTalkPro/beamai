# Tool Safety Extension Points

> Chinese version: [TOOL_SAFETY.md](TOOL_SAFETY.md)

BeamAI tool execution already has process isolation, timeouts, and batch
isolation. This page describes tool-level safety declarations and execution
policies.

## Example Declaration

```erlang
Tool = #{
    name => <<"search_files">>,
    handler => fun(Args, Ctx) -> search_files(Args, Ctx) end,
    parameters => #{
        query => #{type => string, required => true}
    },
    strict_args => true,
    permissions => #{
        filesystem => true,
        network => false,
        process => false,
        env => false
    },
    result_redaction => [
        {<<"sk-[A-Za-z0-9]+">>, <<"[REDACTED]">>}
    ],
    max_result_bytes => 262144
}.
```

## Field Semantics

| Field | Default | Meaning |
|---|---|---|
| `strict_args` | `false` | Validate args with the JSON Schema generated from `parameters` before invocation; failures return `invalid_tool_args` |
| `permissions` | undeclared | Metadata convention for filesystem/network/process/env access; `beamai_tool:validate/1` validates its shape |
| `result_redaction` | `[]` | Ordered regex replacements applied after the tool result is encoded to text |
| `max_result_bytes` | unlimited | Hard limit for the final encoded text; excess results produce `tool_result_too_large` |

All policies are opt-in. Tools that omit these fields retain their existing
behavior; no forced migration is required.

## Execution Order

The innermost terminal in `beamai_tool_executor` runs after a successful handler:

1. Argument validation: before the handler when `strict_args => true`.
2. Handler execution: still uses the existing child process, timeout, and retry
   mechanisms.
3. Result redaction: only for tools with `result_redaction`.
4. Result size check: only for tools with `max_result_bytes`.
5. `around_tool` filters: outer filters can still rewrite the terminal result.

## Relationship to Approval and Streaming Redaction

- `sensitive => true` is consumed by `beamai_filters:approval_filter/1,2` for
  pre-execution approval.
- `token_redact_filter/2` handles streaming tokens.
- `result_redaction` handles the full tool result before it is returned to the
  model.

They solve different problems and compose cleanly.
