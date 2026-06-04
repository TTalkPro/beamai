# Filter System Documentation

English | [中文](FILTER.md)

beamai_core's Filter system provides a true **onion-style** interception mechanism for wrapping, rewriting, and controlling the area around tool execution and LLM calls. It uses an **around** form: each filter wraps "before → call inner → after" in a single closure, matching the common middleware pattern. Compared with splitting interception into separate before/after closures, around keeps the before/after logic in one place — bridged by closure-local variables — and short-circuiting is just "don't call inner", with no dedicated halt protocol.

## Table of Contents

- [Overview](#overview)
- [2 around hook points](#2-around-hook-points)
- [Per-filter private context](#per-filter-private-context)
- [Onion execution order](#onion-execution-order)
- [API Reference](#api-reference)
- [Usage](#usage)
- [Complete Examples](#complete-examples)
- [Relationship with Middleware](#relationship-with-middleware)

---

## Overview

A Filter is an onion-style interceptor around the Kernel's tool execution and chat calls. It can:

- **Rewrite the request**: modify args, message list, or call options before the call
- **Rewrite the response**: modify the tool result or LLM response after the call
- **Short-circuit**: the around closure can skip the inner layers (including the real tool execution / LLM call) by not calling `Next`, returning a result directly
- **Retry**: the around closure may call `Next` multiple times
- **Private state**: each filter has a per-name isolated private context that lives across one invoke (including each tool-loop iteration)
- **Logging/auditing**: record call logs, measure response length, etc.

Each filter is **one onion layer** — it binds at most 2 optional around hooks: `around_chat` for the chat chain and `around_tool` for the tool chain. An around closure looks like:

```erlang
fun(Request, FCtx, Next) -> Response | {Response, NewFCtx} end
```

- **Before**: rewrite `Request`
- **Call inner**: `Next(Request1)` yields `Response` (skip it = short-circuit, call it multiple times = retry)
- **After**: rewrite `Response`
- **Return**: `Response` (private state unchanged) or `{Response, NewFCtx}` (update private state)

The filter chain is composed by `beamai_filter_chain` into nested calls, with the innermost being the **terminal** (the real LLM call or tool execution).

### Core Modules

| Module | Location | Description |
|--------|----------|-------------|
| `beamai_filter` | `apps/beamai_core/src/kernel/` | Filter constructors and utilities |
| `beamai_filter_chain` | `apps/beamai_core/src/kernel/` | Onion chain composition and execution |
| `beamai_kernel` | `apps/beamai_core/src/kernel/` | Kernel integration (filter registration) |
| `beamai` | `apps/beamai_core/src/` | Top-level facade (convenience API) |

---

## 2 around hook points

A filter may define any subset of the following 2 hooks:

| hook | Meaning | Form |
|------|---------|------|
| `around_chat` | Wrap one LLM call | `fun(Request, FCtx, Next) -> Response \| {Response, NewFCtx}` |
| `around_tool` | Wrap one tool execution | `fun(Request, FCtx, Next) -> Response \| {Response, NewFCtx}` |

**Each chain uses only its own around:**

- The **chat chain** uses each filter's `around_chat`, wrapping one LLM call.
- The **tool chain** uses each filter's `around_tool`, wrapping one tool execution.

A filter without the corresponding around for a chain (no `around_chat` for chat, or no `around_tool` for tool) is **skipped** in that chain.

### Request / Response per chain

| Chain | Request | Response |
|-------|---------|----------|
| chat | `#{messages, context, opts}` | `#{response, context}` (response is a beamai_llm_response) |
| tool | `#{tool, args, context}` | `#{result, context}` |

Here `context` is the **shared context** (`beamai_context`) threaded through the whole chain, readable/writable by filters and the terminal. It is distinct from the filter **private context** below.

> **Conversation memory** is exactly one filter: `beamai_memory_filter:memory_filter(Store)` returns a **single** filter whose `around_chat` stores the round's delta into the store and replaces messages with the full history (keyed by `conversation_id`) before the call, then stores the assistant reply after. Since before and after share one closure, it only needs to look up `conversation_id` once. See [MEMORY_EN.md](MEMORY_EN.md).

### The order field

`order` decides the onion layering: **smaller is more outer** — its before runs earlier and its after runs later. Default `order` is 0. Equal `order` keeps registration order (stable sort).

### Filter spec map

A filter is a tagged map:

```erlang
-type filter() :: #{
    '__filter__' := true,
    name := binary(),                  %% name (debug id, also the private-context isolation key)
    order := integer(),                %% layer (smaller is more outer)
    hooks := #{                        %% any subset of the 2 around hooks
        around_chat => around_fun(),
        around_tool => around_fun()
    },
    init := map()                      %% private context initial value (seeded on first entry, default #{})
}.

-type around_fun() :: fun((Request, FCtx, Next) -> Response | {Response, NewFCtx}).
-type Next :: fun((Request) -> Response).
```

---

## Per-filter private context

Each filter has a **private context** (FCtx), isolated by filter name and separate from the shared `beamai_context`:

- The around closure **reads** private state via the 2nd parameter `FCtx` and **writes** it by returning `{Response, NewFCtx}`.
- Different filters' private contexts are mutually invisible (no clash even with the same internal keys).
- Private state is threaded along with the shared context and **lives across one invoke** — including each iteration of the tool-calling loop, and between a filter's own `around_chat` and `around_tool`.
- Use the 4th argument of `beamai_filter:new/4` to set the initial value (default `#{}`), seeded on first entry into that filter.

> Private context lives only within one invoke; it is not persisted across invokes. For cross-invoke state (e.g. a global counter), use an external store.

When a simple filter needs no private state, the around may just return `Response` (skipping the tuple).

---

## Onion execution order

For filters A (order 1) and B (order 2) wrapping the terminal (LLM) on the chat chain:

```
A before → B before → Terminal → B after → A after
```

Composition (`beamai_filter_chain:compose/3`, Phase = `around_chat`):

```
compose([A, B], Phase, Terminal)
  = fun(Req) -> A_around(Req, fun(R) -> B_around(R, Terminal) end) end
```

where `X_around` is "run X's before, `Next` into the inner, then run X's after on the way back".

- **Before**: each layer's before runs in ascending order (A first, B second).
- **terminal**: the innermost runs the real LLM call / tool execution.
- **After**: runs in **automatically reversed order** (B first, A second) — the natural unwinding of the nested call stack, no manual ordering needed.

If a filter's around does not call `Next`, that is a short-circuit (skip all inner layers); the filter constructs and returns the `Response` directly. Outer filters' after still runs.

The tool chain works the same way, replacing `around_chat` with `around_tool` (Phase = `around_tool`).

---

## API Reference

### beamai_filter module

#### Constructors

```erlang
%% Create a filter (default order 0, private state initial value #{}).
%% Hooks is a hook map, any subset of around_chat/around_tool.
-spec new(Name :: binary(), Hooks :: hooks()) -> filter().

%% Create a filter (with an explicit order, smaller is more outer)
-spec new(Name :: binary(), Hooks :: hooks(), Order :: integer()) -> filter().

%% Create a filter (with an explicit order and private state initial value Init)
-spec new(Name :: binary(), Hooks :: hooks(), Order :: integer(), Init :: map()) -> filter().
```

Hook form:

```erlang
-type hook_type() :: around_chat | around_tool.
-type hooks() :: #{
    around_chat => around_fun(),
    around_tool => around_fun()
}.
-type around_fun() :: fun((Request, FCtx, Next) -> Response | {Response, NewFCtx}).
```

Not calling `Next` short-circuits (skips the inner layers), returning the `Response` directly.

#### Utilities

```erlang
%% Stable ascending sort by order (equal order keeps registration order)
-spec sort([filter()]) -> [filter()].

%% Get one of the filter's hooks (undefined if absent)
-spec hook(filter(), hook_type()) -> around_fun() | undefined.

%% Get the filter's private context initial value
-spec init(filter()) -> map().
```

### beamai_filter_chain module

```erlang
%% Run one chain's filter onion.
%% Phase says which around hook the chain uses: around_chat for chat, around_tool
%% for tool. Only filters with the matching around enter the onion; others are
%% skipped. Terminal produces the innermost response; it throws on error, which
%% run/4 catches with try/catch, returning {ok, Response} | {error, Reason}.
-spec run(Filters :: [filter()],
          Phase :: around_chat | around_tool,
          Terminal :: fun((Request) -> Response),
          Request :: map()) -> {ok, Response} | {error, Reason}.

%% Compose the filter list and terminal into a single onion function
-spec compose(Filters :: [filter()], Phase :: phase(),
              Terminal :: fun()) -> fun((Request) -> Response).
```

### beamai_context private-context accessors

```erlang
%% Read one filter's private context (isolated by name, returns Default if absent)
-spec filter_state(Ctx, Name :: binary(), Default :: map()) -> map().

%% Write one filter's private context
-spec set_filter_state(Ctx, Name :: binary(), State :: map()) -> Ctx.
```

> These accessors are used by the onion chain to project/merge; filter code usually reads via the around's `FCtx` parameter and writes via returning `{Resp, NewFCtx}`, without calling them directly.

### beamai_kernel integration

```erlang
%% Register a filter with the Kernel (appended to filters, sorted by order at run time)
beamai_kernel:add_filter(Kernel, Filter) -> UpdatedKernel.

%% Auto-load filters from a tool module (the module implements the optional filters/0 callback)
beamai_kernel:add_tool_module(Kernel, Module) -> UpdatedKernel.
```

### beamai convenience API

```erlang
%% Register an already-built filter
beamai:add_filter(Kernel, Filter) -> UpdatedKernel.

%% Quickly create and register a filter (pass a hook map directly, order fixed to 0)
beamai:add_filter(Kernel, Name, Hooks) -> UpdatedKernel.
```

---

## Usage

### 1. Register a filter with the Kernel

```erlang
%% Option 1: use the beamai convenience API (hook map, order 0)
K0 = beamai:kernel(),
K1 = beamai:add_filter(K0, <<"logger">>, #{
    %% around_tool: log the tool name before
    around_tool => fun(#{tool := #{name := Name}, args := Args} = Req, _FCtx, Next) ->
        io:format("Calling tool: ~ts(~p)~n", [Name, Args]),
        Next(Req)
    end
}).

%% Option 2: build the filter manually and register (with an explicit order)
Filter = beamai_filter:new(<<"logger">>, #{around_tool => AroundFn}, 10),
K1 = beamai_kernel:add_filter(K0, Filter).
```

### 2. Auto-register filters from a tool module

A tool module can auto-register filters by implementing the optional `filters/0` callback:

```erlang
-module(my_tool_module).
-behaviour(beamai_tool_behaviour).

-export([tools/0, filters/0]).

tools() ->
    [#{name => <<"my_tool">>, handler => fun handle/2,
       description => <<"My tool">>}].

%% Optional callback: return a list of filters
filters() ->
    [
        beamai_filter:new(<<"audit">>, #{
            around_tool => fun(Req, _FCtx, Next) ->
                #{result := Result} = Resp = Next(Req),
                logger:info("Tool returned: ~p", [Result]),
                Resp
            end
        })
    ].
```

When the module is loaded, the Kernel auto-registers these filters:

```erlang
K1 = beamai_kernel:add_tool_module(K0, my_tool_module).
%% Both tools and filters are registered
```

### 3. Filter layering (order)

A smaller `order` is more outer: its before runs earlier and its after runs later. Equal `order` keeps registration order.

```erlang
%% Validator (order -10, outermost → before runs first)
K1 = beamai_kernel:add_filter(K0,
    beamai_filter:new(<<"validator">>, #{around_tool => ValidateFn}, -10)),

%% Logger (order 0, default)
K2 = beamai_kernel:add_filter(K1,
    beamai_filter:new(<<"logger">>, #{around_tool => LogFn}, 0)),

%% Transformer (order 10, innermost → before runs last, after runs first)
K3 = beamai_kernel:add_filter(K2,
    beamai_filter:new(<<"transformer">>, #{around_tool => TransformFn}, 10)).

%% Before execution order: validator → logger → transformer → Terminal
```

---

## Complete Examples

### Example 1: tool filter — log + double the result (one around_tool handles both sides)

```erlang
%% Create the Kernel and register a tool
K0 = beamai:kernel(),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})),

%% One around_tool closure: log the call before, rewrite result after
K2 = beamai:add_filter(K1, <<"log_and_double">>, #{
    around_tool => fun(#{tool := #{name := Name}, args := Args} = Req, _FCtx, Next) ->
        io:format("[LOG] ~ts(~p)~n", [Name, Args]),
        #{result := Result} = Resp = Next(Req),
        Resp#{result => Result * 2}
    end
}).

%% Call (3 + 5 = 8, doubled after = 16)
%% (tool execution goes through the Kernel's tool filter onion chain)
```

### Example 2: chat filter — inject a system message + audit (one around_chat handles both sides)

```erlang
K0 = beamai:kernel(),
K1 = beamai:add_llm(K0, LLMConfig),

%% One around_chat closure: inject a system message before, log response length after
K2 = beamai:add_filter(K1, <<"system_and_audit">>, #{
    around_chat => fun(#{messages := Msgs} = Req, _FCtx, Next) ->
        HasSystem = lists:any(
            fun(#{role := R}) -> R =:= system; (_) -> false end,
            Msgs),
        Req1 = case HasSystem of
            true ->
                Req;
            false ->
                SystemMsg = #{role => system,
                              content => <<"Answer concisely.">>},
                Req#{messages => [SystemMsg | Msgs]}
        end,
        #{response := Response} = Resp = Next(Req1),
        case beamai_llm_response:content(Response) of
            Content when is_binary(Content) ->
                logger:info("Response length: ~B bytes", [byte_size(Content)]);
            _ ->
                ok
        end,
        Resp
    end
}).

%% On each request, the chat filter chain injects a system message and audits the response.
```

### Example 3: short-circuit — around_tool does not call `Next`

```erlang
%% tool filter: short-circuit on validation failure (don't call Next, skip the real tool)
K1 = beamai:add_filter(K0, <<"guard">>, #{
    around_tool => fun(#{args := #{a := A}, context := Ctx} = Req, _FCtx, Next) ->
        case A > 1000 of
            true  -> #{result => {error, <<"a exceeds limit">>}, context => Ctx};
            false -> Next(Req)
        end
    end
}).
```

> Short-circuit means "don't call `Next`"; the filter constructs and returns the `Response` directly. The after of any outer filter wrapping it still runs — handy for unified cleanup on the outside.

### Example 4: private context — accumulate a counter across tool-loop iterations

```erlang
%% around_chat: count LLM calls within this invoke using the private context
K1 = beamai:add_filter(K0, <<"counter">>, #{
    around_chat => fun(Req, FCtx, Next) ->
        N = maps:get(calls, FCtx, 0),
        Resp = Next(Req),
        logger:info("LLM call #~B", [N + 1]),
        {Resp, FCtx#{calls => N + 1}}   %% return {Resp, NewFCtx} to write back private state
    end
}).
%% Each LLM call in the tool loop increments calls, isolated from other filters' private state.
```

See more in [examples/src/example_filter.erl](../examples/src/example_filter.erl).

---

## Relationship with middleware

This system uses the common **around middleware** form:

- The around closure `fun(Request, FCtx, Next) -> Response` maps to middleware's "receive request → call `next` → handle response".
- Before/after live in one closure, bridged by locals — no need to pass values through a shared context between before and after.
- Short-circuit = don't call `Next`; retry = call `Next` multiple times; no dedicated halt protocol.
- One filter bundles the arounds for both chains (`around_chat` / `around_tool`), each chain selected independently.

---

## Relationship with the Middleware framework

The [beamai_extra](https://github.com/TTalkPro/beamai_extra) extension project provides a more advanced Middleware system (in beamai_tools) with stateful management, preset configurations, call limits, human approval, retry, and fallback. Middleware is converted into filters registered with the Kernel, so both ultimately run in the same onion chain.

| Feature | Filter (this doc) | Middleware (beamai_extra) |
|---------|-------------------|---------------------------|
| Complexity | Lightweight, private context scoped to one invoke | Full framework, cross-call state management |
| Presets | None | production/development presets, etc. |
| Built-in features | None | Call limits, human approval, retry, fallback |
| Use cases | Simple wrapping: logging, validation, injection, caching | Complex control: rate limiting, retry, fallback |

---

## More Resources

- [beamai_core README](../apps/beamai_core/README_EN.md) - Kernel architecture docs
- [MEMORY_EN.md](MEMORY_EN.md) - Conversation memory (Memory filter)
- [API Reference](API_REFERENCE.md) - API reference docs
