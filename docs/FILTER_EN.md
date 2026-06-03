# Filter System Documentation

English | [中文](FILTER.md)

The beamai_core Filter system provides a true **onion-style** interception mechanism for wrapping, rewriting, and controlling tool execution and LLM calls. It mirrors the before/after decomposition of [Spring AI's Advisor](https://docs.spring.io/spring-ai/reference/api/advisors.html): rewrite the request on the way in, rewrite the response on the way out, and the outbound order reverses automatically. The difference: in beamai a **single filter** bundles the chat and tool before/after hooks, binding up to 4 optional hooks.

## Table of Contents

- [Overview](#overview)
- [4 Hook Points](#4-hook-points)
- [Onion Execution Order](#onion-execution-order)
- [API Reference](#api-reference)
- [Usage](#usage)
- [Complete Examples](#complete-examples)
- [Relationship with Middleware](#relationship-with-middleware)

---

## Overview

Filters are onion-style interceptors for Kernel tool execution and Chat calls that can:

- **Rewrite the request**: modify args, message lists, and options on the way in
- **Rewrite the response**: modify tool results or LLM responses on the way out
- **Short-circuit**: return `{halt, Response}` from a pre hook to skip the inner layers (including the real tool execution / LLM call) and return a result directly, while still running this layer's post hook
- **Logging/auditing**: record invocation logs, measure response length, etc.

Each filter is **one layer of the onion** — it binds up to 4 optional hooks: the chat chain's in/out (`pre_chat`/`post_chat`) and the tool chain's in/out (`pre_tool`/`post_tool`). A filter's pre/post form **one onion layer** wrapping the same call: the pre hook runs before the inner layers on the way in, and the post hook runs after them on the way out, reversing automatically. The filter list is composed into nested calls by `beamai_filter_chain`, with the **terminal** (the real LLM call or tool execution) at the very center.

### Core Modules

| Module | Location | Description |
|--------|----------|-------------|
| `beamai_filter` | `apps/beamai_core/src/kernel/` | Filter constructors and helper functions |
| `beamai_filter_chain` | `apps/beamai_core/src/kernel/` | Onion chain composition and execution |
| `beamai_kernel` | `apps/beamai_core/src/kernel/` | Kernel integration (filter registration) |
| `beamai` | `apps/beamai_core/src/` | Top-level Facade (convenience API) |

---

## 4 Hook Points

A single filter can define any subset of these 4 hooks:

| Hook | Meaning | Shape |
|------|---------|-------|
| `pre_chat` | chat inbound: rewrite the request before the LLM call | `fun(Request) -> Request \| {halt, Response}` |
| `post_chat` | chat outbound: rewrite the response after the LLM call | `fun(Response) -> Response` |
| `pre_tool` | tool inbound: rewrite tool args before tool execution | `fun(Request) -> Request \| {halt, Response}` |
| `post_tool` | tool outbound: rewrite the tool result after tool execution | `fun(Response) -> Response` |

**Each chain uses only its own hook pair:**

- The **chat chain** uses each filter's `(pre_chat, post_chat)` pair, wrapping one LLM call.
- The **tool chain** uses each filter's `(pre_tool, post_tool)` pair, wrapping one tool execution.

A filter with no relevant hooks for a given chain (neither `pre_chat` nor `post_chat` for the chat chain, or neither `pre_tool` nor `post_tool` for the tool chain) is **skipped** in that chain. A missing single hook is filled with the identity function.

### Request / Response per Chain

| Chain | Request | Response |
|-------|---------|----------|
| chat | `#{messages, context, opts}` | `#{response, context}` (response is a beamai_llm_response) |
| tool | `#{tool, args, context}` | `#{result, context}` |

> **Conversation memory** is exactly one filter: `beamai_memory_filter:memory_filter(Store)` returns a **single** filter whose `pre_chat` stores the round's delta messages and replaces `messages` with the full history from the store (keyed by `conversation_id`), and whose `post_chat` stores the assistant reply. Both halves live in one layer, and the onion chain naturally guarantees `pre_chat` runs ahead of the inner layers and `post_chat` runs after them. See [MEMORY_EN.md](MEMORY_EN.md).

### The `order` Field

`order` determines the onion layering: **smaller = more outer** — its pre hook runs earlier and its post hook runs later. The default `order` is 0. Filters with equal `order` keep their registration order (stable sort).

### The Filter Spec Map

A filter is a tagged map:

```erlang
-type filter() :: #{
    '__filter__' := true,
    name := binary(),                                  %% Name (debug identifier)
    order := integer(),                                %% Layer (smaller = more outer)
    hooks := #{                                        %% any subset of the 4 hooks
        pre_chat  => fun((Request)  -> Request | {halt, Response}),
        post_chat => fun((Response) -> Response),
        pre_tool  => fun((Request)  -> Request | {halt, Response}),
        post_tool => fun((Response) -> Response)
    }
}.
```

---

## Onion Execution Order

For filters A (order 1) and B (order 2) wrapping the terminal (LLM) on the chat chain:

```
A.pre_chat → B.pre_chat → Terminal → B.post_chat → A.post_chat
```

Composition (`beamai_filter_chain:compose/4`, Phase = `{pre_chat, post_chat}`):

```
compose([A, B], Phase, Terminal)
  = fun(Req) -> A_wrap(Req, fun(R) -> B_wrap(R, Terminal) end) end
```

where `X_wrap` means "run X's pre, descend into the inner layers, then run X's post on the way out."

- **Inbound**: each layer's pre hook runs in ascending `order` (A first, then B).
- **Terminal**: the innermost layer performs the real LLM call / tool execution.
- **Outbound**: post hooks run in **reverse order** automatically (B first, then A) — this is simply how the nested call stack unwinds; no manual ordering required.

A filter's pre hook returning `{halt, Response}` short-circuits (skips all inner layers), but **still runs this layer's post hook**.

The tool chain works the same way, replacing `pre_chat/post_chat` with `pre_tool/post_tool` (Phase = `{pre_tool, post_tool}`).

---

## API Reference

### beamai_filter Module

#### Constructors

```erlang
%% Create a filter (order defaults to 0).
%% Hooks is a hook map with any subset of pre_chat/post_chat/pre_tool/post_tool.
-spec new(Name :: binary(), Hooks :: hooks()) -> filter().

%% Create a filter (explicit order, smaller = more outer)
-spec new(Name :: binary(), Hooks :: hooks(), Order :: integer()) -> filter().
```

Hook shapes:

```erlang
-type hook_type() :: pre_chat | post_chat | pre_tool | post_tool.
-type hooks() :: #{
    pre_chat  => fun((Request)  -> Request | {halt, Response}),
    post_chat => fun((Response) -> Response),
    pre_tool  => fun((Request)  -> Request | {halt, Response}),
    post_tool => fun((Response) -> Response)
}.
```

A pre hook returning `{halt, Response}` short-circuits (skips inner layers) but still runs this layer's corresponding post hook.

#### Helper Functions

```erlang
%% Stable sort ascending by order (equal order keeps registration order)
-spec sort([filter()]) -> [filter()].

%% Get one of a filter's hooks (undefined if absent)
-spec hook(filter(), hook_type()) -> fun() | undefined.
```

### beamai_filter_chain Module

```erlang
%% Run the filter onion chain for one chain.
%% Phase selects which hook pair the chain uses: pass {pre_chat, post_chat}
%% for chat, {pre_tool, post_tool} for tool. Only filters with at least one
%% relevant hook enter the onion; the rest are skipped. Terminal produces the
%% innermost response and throws on error; run/4 catches the throw and returns
%% {ok, Response} | {error, Reason} uniformly.
-spec run(Filters :: [filter()],
          Phase :: {pre_chat, post_chat} | {pre_tool, post_tool},
          Terminal :: fun((Request) -> Response),
          Request :: map()) -> {ok, Response} | {error, Reason}.

%% Compose the filter list and terminal into a single onion function
%% (internal; external callers pass identity)
-spec compose(Filters :: [filter()], Phase :: phase(),
              Terminal :: fun(), identity) -> fun((Request) -> Response).
```

### beamai_kernel Integration

```erlang
%% Register a filter with the Kernel (appended to the filters list,
%% sorted by order at run time)
beamai_kernel:add_filter(Kernel, Filter) -> UpdatedKernel.

%% Auto-load filters from a tool module (module implements the optional
%% filters/0 callback)
beamai_kernel:add_tool_module(Kernel, Module) -> UpdatedKernel.
```

### beamai Convenience API

```erlang
%% Register a pre-built filter
beamai:add_filter(Kernel, Filter) -> UpdatedKernel.

%% Create and register a filter in one step (pass a hook map, order fixed at 0)
beamai:add_filter(Kernel, Name, Hooks) -> UpdatedKernel.
```

---

## Usage

### 1. Register a Filter with the Kernel

```erlang
%% Method 1: Using the beamai convenience API (hook map, order 0)
K0 = beamai:kernel(),
K1 = beamai:add_filter(K0, <<"logger">>, #{
    %% pre_tool: log the tool name on the way in
    pre_tool => fun(#{tool := #{name := Name}, args := Args} = Req) ->
        io:format("Calling tool: ~ts(~p)~n", [Name, Args]),
        Req
    end
}).

%% Method 2: Manually create and register (order can be specified)
Filter = beamai_filter:new(<<"logger">>, #{pre_tool => PreFn}, 10),
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

%% Optional callback: return a list of filters
filters() ->
    [
        beamai_filter:new(<<"audit">>, #{
            post_tool => fun(#{result := Result} = Resp) ->
                logger:info("Tool returned: ~p", [Result]),
                Resp
            end
        })
    ].
```

When loading the module, the Kernel automatically registers these filters:

```erlang
K1 = beamai_kernel:add_tool_module(K0, my_tool_module).
%% Both tools and filters are now registered
```

### 3. Filter Layering (order)

A smaller `order` is more outer: its pre hook runs earlier and its post hook runs later. Filters with equal `order` keep registration order.

```erlang
%% Validator (order -10, outermost -> pre_tool runs first)
K1 = beamai_kernel:add_filter(K0,
    beamai_filter:new(<<"validator">>, #{pre_tool => ValidateFn}, -10)),

%% Logger (order 0, default)
K2 = beamai_kernel:add_filter(K1,
    beamai_filter:new(<<"logger">>, #{pre_tool => LogFn}, 0)),

%% Transformer (order 10, innermost -> pre_tool runs last, post_tool runs first)
K3 = beamai_kernel:add_filter(K2,
    beamai_filter:new(<<"transformer">>, #{post_tool => TransformFn}, 10)).

%% pre_tool execution order: validator -> logger -> transformer -> Terminal
```

---

## Complete Examples

### Example 1: tool filter — logging + double the result (pre_tool + post_tool in ONE filter)

```erlang
%% Create Kernel and register a tool
K0 = beamai:kernel(),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})),

%% One filter binds both pre_tool (inbound logging) and post_tool (outbound doubling)
K2 = beamai:add_filter(K1, <<"log_and_double">>, #{
    %% pre_tool: record the call on the way in
    pre_tool => fun(#{tool := #{name := Name}, args := Args} = Req) ->
        io:format("[LOG] ~ts(~p)~n", [Name, Args]),
        Req
    end,
    %% post_tool: rewrite result on the way out
    post_tool => fun(#{result := Result} = Resp) ->
        Resp#{result => Result * 2}
    end
}).

%% Invoke (3 + 5 = 8, doubled on the way out = 16),
%% tool execution passing through the Kernel's tool filter onion chain.
```

### Example 2: chat filter — inject a system message + audit (pre_chat + post_chat in ONE filter)

```erlang
K0 = beamai:kernel(),
K1 = beamai:add_llm(K0, LLMConfig),

%% One filter: pre_chat injects a system message on the way in,
%% post_chat logs the response length on the way out
K2 = beamai:add_filter(K1, <<"system_and_audit">>, #{
    %% pre_chat: auto-inject a system message
    pre_chat => fun(#{messages := Msgs} = Req) ->
        HasSystem = lists:any(
            fun(#{role := R}) -> R =:= system; (_) -> false end,
            Msgs),
        case HasSystem of
            true ->
                Req;
            false ->
                SystemMsg = #{role => system,
                              content => <<"Answer concisely.">>},
                Req#{messages => [SystemMsg | Msgs]}
        end
    end,
    %% post_chat: log the response content length
    post_chat => fun(#{response := Response} = Resp) ->
        case beamai_llm_response:content(Response) of
            Content when is_binary(Content) ->
                logger:info("Response length: ~B bytes", [byte_size(Content)]);
            _ ->
                ok
        end,
        Resp
    end
}).

%% On send, the chat filter chain auto-injects the system message and audits the response.
```

### Example 3: short-circuit — pre_tool returns `{halt, Response}`

```erlang
%% tool filter: on a failed guard, short-circuit (skip the real tool execution),
%% still run this layer's post_tool
K1 = beamai:add_filter(K0, <<"guard">>, #{
    %% pre_tool: when the guard condition matches, halt with an error result
    pre_tool => fun(#{args := #{a := A}, context := Ctx} = Req) ->
        case A > 1000 of
            true  -> {halt, #{result => {error, <<"a exceeds limit">>},
                              context => Ctx}};
            false -> Req
        end
    end,
    %% post_tool: both halt and normal paths pass here (uniform finalization)
    post_tool => fun(Resp) ->
        logger:info("tool finished: ~p", [Resp]),
        Resp
    end
}).
```

> A pre hook returning `{halt, Response}` skips all inner layers but **still runs this layer's post hook** — convenient for uniform finalization on the short-circuit path (e.g. cache write-back, counting).

See [examples/src/example_filter.erl](../examples/src/example_filter.erl) for more examples.

---

## Mapping to Spring AI Advisor

This system mirrors the before/after decomposition of Spring AI's Advisor:

- Spring AI's `before(request, chain)` / `after(response, chain)` decomposition ⇄ beamai filter's pre/post hooks.
- Difference: in Spring AI one advisor maps to one chain; in beamai a **single filter** bundles the chat and tool before/after hooks (up to 4), with the chat chain taking `(pre_chat, post_chat)` and the tool chain taking `(pre_tool, post_tool)`.
- There is no standalone "around" form — short-circuit is achieved by returning `{halt, Response}` from a pre hook.

Both center on "pre/post wrap the same call, the outbound order reverses automatically."

---

## Relationship with Middleware

The [beamai_extra](https://github.com/TTalkPro/beamai_extra) extension project provides an advanced Middleware system (in beamai_tools) with stateful management, presets, call limits, human approval, retry, and fallback features. Middleware internally converts to filters and registers with the Kernel, so both execute in the same onion chain.

| Feature | Filter (this document) | Middleware (beamai_extra) |
|---------|------------------------|---------------------------|
| Complexity | Lightweight, stateless | Full framework with state management |
| Presets | None | Provides production/development presets |
| Built-in Features | None | Call limits, human approval, retry, fallback |
| Use Cases | Simple wrapping: logging, validation, injection, caching | Complex control: rate limiting, retry, fallback |

---

## More Resources

- [beamai_core README](../apps/beamai_core/README_EN.md) - Kernel architecture documentation
- [MEMORY_EN.md](MEMORY_EN.md) - Conversation memory (Memory filter)
- [API Reference](API_REFERENCE_EN.md) - API reference
