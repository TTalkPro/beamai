# Filter System Documentation

English | [中文](FILTER.md)

beamai_core's Filter system provides a true **onion-style** interception mechanism for wrapping, rewriting, and controlling the area around tool execution and LLM calls. It uses an **around** form: each filter wraps "before → call inner → after" in a single closure, matching the common middleware pattern. Compared with splitting interception into separate before/after closures, around keeps the before/after logic in one place — bridged by closure-local variables — and short-circuiting is just "don't call inner", with no dedicated halt protocol.

## Table of Contents

- [Overview](#overview)
- [4 around hook points](#4-around-hook-points)
- [The loop is a chain link too](#the-loop-is-a-chain-link-too)
- [token_transform (token-stream transform)](#token_transform-token-stream-transform)
- [Built-in filters](#built-in-filters)
- [Per-filter private context](#per-filter-private-context)
- [Onion execution order](#onion-execution-order)
- [API Reference](#api-reference)
- [Usage](#usage)
- [Complete Examples](#complete-examples)
- [Relationship with Middleware](#relationship-with-middleware)

---

## Overview

A Filter is an onion-style interceptor around the ChatClient's tool execution and chat calls. It can:

- **Rewrite the request**: modify args, message list, or call options before the call
- **Rewrite the response**: modify the tool result or LLM response after the call
- **Short-circuit**: the around closure can skip the inner layers (including the real tool execution / LLM call) by not calling `Next`, returning a result directly
- **Retry**: the around closure may call `Next` multiple times
- **Private state**: each filter has a per-name isolated private context that lives across one invoke (including each tool-loop iteration)
- **Logging/auditing**: record call logs, measure response length, etc.

Each filter is **one onion layer** — it binds at most 4 optional around hooks: `around_turn` for the turn chain (wrapping the whole tool loop, used by the Agent layer), `around_step` for the step chain (one ReAct iteration), `around_chat` for the chat chain (that round's LLM call), and `around_tool` for the tool chain (one tool execution) — plus a streaming-only `token_transform` hook (token-stream transform, not part of the onion; see its own section). An around closure looks like:

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
| `beamai_filter` | `apps/beamai_core/src/core/` | Filter constructors and utilities |
| `beamai_filter_chain` | `apps/beamai_core/src/core/` | Onion chain composition and execution |
| `beamai_chat_client` | `apps/beamai_core/src/core/` | ChatClient integration (filter registration) |
| `beamai` | `apps/beamai_core/src/` | Top-level facade (convenience API) |

---

## 4 around hook points

A filter may define any subset of the following 4 hooks:

| hook | Granularity | Form |
|------|-------------|------|
| `around_turn` | Once per turn (the whole tool loop, Agent layer) | as below (Response is the tool-loop result tuple) |
| `around_step` | Once per ReAct iteration (**including that round's tool execution**) | as below (Response is a step-response map) |
| `around_chat` | **Exactly once** per loop iteration (that round's LLM call) | `fun(Request, FCtx, Next) -> Response \| {Response, NewFCtx}` |
| `around_tool` | Once per tool call (inside parallel tasks) | same as above |

Layering (outer → inner):

```
around_turn            once per turn          RAG injection / final-answer validation / turn budget
  tool_loop filter     the loop driver is itself a chain link (fully replaceable)
    around_step        once per iteration     per-round budget / trace / iteration rewriting
      around_chat      that round's LLM call  memory / accounting / audit
        provider       the actual chat call   ← retry lives *inside* it, invisible to the chain
      around_tool      once per tool call     timeout / approval / argument rewriting
```

**Each chain uses only its own around:**

- The **turn chain** uses `around_turn`, wrapping the whole tool loop; its innermost element is the **loop filter**, whose terminal is the step chain.
- The **step chain** uses `around_step`, wrapping one iteration — note it also wraps that round's **tool execution**, which `around_chat` cannot.
- The **chat chain** uses `around_chat`, wrapping that round's LLM call; its terminal is the provider call.
- The **tool chain** uses `around_tool`, wrapping one tool execution.

A filter without the corresponding around for a chain is **skipped** in that chain. One filter may declare several hooks; its relative layering is the same in every chain.

### Request / Response per chain

| Chain | Request | Response |
|-------|---------|----------|
| step | `#{messages, context, iteration, tool_calls_made}` | `#{status, messages, context, tool_calls_made, ...}` (see below) |
| chat | `#{messages, context, opts}`; streaming also carries `stream => true` | `#{response, context}` (response is a beamai_llm_response) |
| tool | `#{tool, args, context}` | `#{result, context}` |
| turn | `#{messages, context, resume, load_history}` | tool-loop result tuple (`{ok, Resp, TCM, Iter, Messages}` \| `{interrupt, _, _}` \| `{error, _}`; interrupt/error must pass through, never re-enter) |

To **re-enter** the turn chain (validate-retry / evaluator), use the result's 5th element
`Messages` — the complete message sequence of that run (cross-run history + this turn's new
messages + every round's assistant/tool-result messages, up to the final answer):

```erlang
%% Continue from the previous run: the filter rebuilds the full context itself,
%% independent of whether the agent has memory enabled.
Next(Req#{messages => Messages ++ [Feedback], load_history => false})
```

The request's `messages` means **this turn's new messages** (not the full history), and
`load_history` defaults to `true` (letting the tool loop prepend cross-run history). Passing
only the new messages and relying on the loop to bring the original question back **loses that
question when `memory => false`** — the model then receives nothing but "your last answer
failed validation, please fix it" and hallucinates. That is exactly why `load_history` and the
5th element exist.

Here `context` is the **shared context** (`beamai_context`) threaded through the whole chain, readable/writable by filters and the terminal. It is distinct from the filter **private context** below.

> **Conversation memory** is exactly one filter: `beamai_memory_filter:memory_filter(Store)` returns a **single** filter whose `around_chat` stores the round's delta into the store and replaces messages with the full history (keyed by `conversation_id`) before the call, then stores the assistant reply after. Since before and after share one closure, it only needs to look up `conversation_id` once. See [MEMORY_EN.md](MEMORY_EN.md).

### Registration order is layer order

Filters are given **all at once** when the ChatClient is built, via `beamai_chat_client:new(Settings, Filters)`.
**List position decides the onion layering**: earlier = more outer (before runs earlier, after runs later).
There is no order field and no runtime sorting — to change layering, reorder the list
(matching clj-agent's flat-vector model).

### Filter spec map

A filter is a tagged map:

```erlang
-type filter() :: #{
    '__filter__' := true,
    name := binary(),                  %% name (debug id, also the private-context isolation key)
    hooks := #{                        %% any subset of the 5 hooks
        around_chat => around_fun(),
        around_step => around_fun(),
        around_tool => around_fun(),
        around_turn => around_fun(),
        token_transform => token_transform()         %% token-stream transform (see below)
    },
    init := map()                      %% private context initial value (seeded on first entry, default #{})
}.

-type around_fun() :: fun((Request, FCtx, Next) -> Response | {Response, NewFCtx}).
-type Next :: fun((Request) -> Response).
```

---

## The loop is a chain link too

The ReAct tool loop is not a `while` hard-wired into the agent — it is the **innermost filter of
the turn chain** (`beamai_agent_tool_loop:loop_filter/1`). Its `Next` is not another turn filter
but the **step chain**: calling it once runs one iteration.

```
turn chain: [user turn filters ...] ++ [loop filter]
                                          │ Next = step chain
                                          ▼
step chain: [user step filters ...] ++ step_terminal (the actual iteration)
```

"Once per conversation" (`around_turn`) and "re-entered once per round" (`around_step`) are now
two distinct hooks instead of fighting over one.

### The step contract

Step request:

| Field | Meaning |
|---|---|
| `messages` | the full message sequence this round starts from |
| `context` | the shared context, threaded round to round |
| `iteration` | iterations already used (cumulative across interrupts, starts at 0) |
| `tool_calls_made` | tool-call records so far |

The step response is one of four `status` values — the loop driver knows only these:

| status | Meaning | Extra fields |
|---|---|---|
| `continue` | tools ran, results merged, go to the next round | `messages` / `context` / `tool_calls_made` |
| `final` | this round is the final answer (plain reply or return_direct) | `response` |
| `interrupt` | HITL / environment pause | `type`, `interrupt_context` |
| `error` | this round failed, the loop stops | `reason` |

A step filter may rewrite `messages` / `context` in the request, or **synthesize a status without
calling Next** — short-circuiting the whole iteration (serving from cache, for instance).

### Replacing the loop strategy

Since the loop is a chain link, swapping it needs no change to agent or ChatClient code — give the
agent a `loop_filter` (a constructor that receives this run's LoopOpts and returns an
`around_turn` filter):

```erlang
{ok, Agent} = beamai_agent:new(#{
    chat_client => K,
    loop_filter => fun(_LoopOpts) ->
        beamai:filter(<<"my_loop">>, #{
            around_turn => fun(Req, _FCtx, Next) ->
                %% Next is the step chain: drive it however you like
                StepReq = #{messages => maps:get(messages, Req),
                            context => maps:get(context, Req),
                            iteration => 0, tool_calls_made => []},
                #{messages := Msgs, tool_calls_made := Made} = Next(StepReq),
                {ok, MyResponse, Made, 1, Msgs}   %% return the tool-loop result tuple
            end
        })
    end
}).
```

A custom loop owes only two things: drive `Next` per the step contract, and return the tool-loop
result tuple the turn chain expects. Iteration caps and `max_tool_calls` are the default loop's
policy — a custom loop owns its own.

> The default loop also performs resume's one-shot dispatch (first entry continues the interrupted
> turn, re-entry starts a fresh loop). A custom loop that wants HITL resume must handle the
> `continuation` in its `LoopOpts`.

---

## Where retry lives

**Not on the chain.** Provider retry lives inside `beamai_chat_model:chat/3` — *below* the entire
filter stack, so a retry's re-entry cannot reach any filter. Memory/accounting filters on
`around_chat` therefore run exactly once per round with no layering discipline required.

```erlang
%% Three levels: per-call Opts > provider Config > framework default (max_retries => 0 disables)
LLM = beamai_chat_model:create(anthropic, #{model => M, api_key => K, max_retries => 5}),
beamai_chat_client:invoke_chat(ChatClient, Messages, #{max_retries => 0}).   %% no retry this call
```

Trade-offs:

- Filters cannot see individual attempts (they see one logical call) → use the `on_retry`
  callback in chat opts to observe them.
- The streaming path does **not** retry (tokens already reached the sink); for fault tolerance
  re-run the whole turn from `around_turn`.

> An earlier version made retry a filter on an `around_llm` chain; that was reverted. Once retry
> sits at the bottom of the stack, that chain fires 1:1 with the chat chain and shares its exact
> contract — the difference collapses to ordering, which list position already decides. See
> `design/retry_back_to_chat_model.md`.

---

## token_transform (token-stream transform)

`token_transform` is a streaming-only hook (mirroring clj-agent's `:token-xf` and the
operator idea of Spring AI's `StreamAdvisor`): the `token_transform` values collected from
the filters (in **registration order**) are composed into a token-transform chain
applied to the **outbound stream** delivered to the on-token sink. The around chains
handle "rewrite request/response"; `token_transform` handles "per-token intervention" —
rewrite, swallow, or buffer-then-release.

```erlang
-type token_data() :: #{token := binary(), meta := map()}.
-type token_transform() :: #{
    init  => term(),      %% initial state (default undefined)
    step  := fun((token_data(), State) -> {[token_data()], State}),  %% 1→N
    flush => fun((State) -> [token_data()])   %% flush buffered residue on normal end (optional)
}.
```

Three hard capabilities (Erlang has no transducers; step/flush is the equivalent):

- **1→N**: `step` takes one token in, emits 0/1/N out (swallow = empty list, buffer = keep in State);
- **Cross-chunk state**: State is threaded explicitly, scoped to **one LLM stream**
  (instantiated from `init` on each terminal execution; each tool-loop round gets fresh state);
- **End-of-stream flush**: after the stream ends **normally**, each layer's `flush` is
  called in cascade (an outer layer's residue passes through inner steps before the sink,
  then the inner layer flushes its own); **error paths never flush** — buffered content
  is dropped, half-formed answers never leak.

**Hard boundary: the token chain transforms delivery, not the answer.** Only the
outbound stream to the TokenCallback is transformed; the normalized response returned
by `invoke_chat_stream` bypasses it — memory persistence, turn results, and subsequent
tool-loop rounds all use the original full answer.

| What to change | Which chain |
|---|---|
| What the user sees in real time (redaction / withholding / buffered release) | `token_transform` |
| What this turn's final answer is (validate-retry / rewrite) | `around_turn` (validation_turn_filter) |

The synchronous path (`invoke_chat`) ignores `token_transform` entirely; with no token_transform
filters the streaming path degrades to a zero-overhead passthrough.

The built-in `token_redact_filter` / `hold_release_filter` are covered in [Built-in filters](#built-in-filters).

```erlang
%% Streaming redaction: the sink sees redacted tokens; the final response is untouched
K = beamai:chat_client(#{}, [
    beamai_filters:token_redact_filter(<<"sk-\\w+">>, <<"[KEY]">>)
]),
{ok, Resp, _} = beamai_chat_client:invoke_chat_stream(K, Messages, #{}, OnToken).
```

---

## Built-in filters

The ready-made filters in `beamai_filters` are all pure constructors — put them in `new/2`'s
filters list when building the ChatClient. (They broadly track Spring AI's Advisor system; the
item-by-item trade-offs are in `design/spring_advisor_alignment.md`.)

| filter | Chain | Description |
|---|---|---|
| `beamai_agent_tool_loop:loop_filter(LoopOpts)` | turn | The **default ReAct loop**, appended by the agent at the innermost position of the turn chain; replace it wholesale via the agent's `loop_filter` |
| `logging_filter()` | turn/chat/tool | One pair of debug logs each for turn/chat/tool. Put it first in the list for the full picture; placed after another filter it only sees rewrites made inside that layer |
| `safeguard_filter(Words)` / `(Words, Opts)` | chat | Short-circuits on a sensitive-word hit without calling the LLM, returning a reply with `finish_reason=content_filtered`. Opts: `failure_response`, `case_sensitive` (default `false`) |
| `timeout_filter(Ms)` | tool | Wall-clock timeout for a single tool execution → `{error, timeout}` (classified transient) |
| `approval_filter(ApproveFun)` | tool | Only intercepts tools marked `sensitive => true`; a rejection goes back to the model as a normal tool result. Non-interactive — for interactive approval use the `on_tool_call` callback |
| `validation_turn_filter(ValidateFun, MaxRetries)` | turn | Validates the final answer; on failure the reason is fed back and the loop re-entered; returns as-is once retries are exhausted |
| `schema_validation_turn_filter(Schema, MaxRetries[, Opts])` | turn | The JSON Schema specialization of the above, see below |
| `token_redact_filter(Pattern, Replacement)` | token | Stateless per-token regex redaction. Known limitation: a secret split across two chunks escapes detection |
| `hold_release_filter(CheckFun)` | token | Review-then-release: buffers the whole stream so nothing leaks, and reviews the full text at end of stream |

### safeguard_filter: capability boundary

```erlang
K = beamai:chat_client(#{}, [beamai_filters:safeguard_filter([<<"forbidden-word">>])]).
```

Sitting on the chat chain means **every LLM call inside the loop passes through it**: it catches
not only user input, but equally sensitive content carried back in when tool results are fed
into the model.

But it is plain substring matching, **not content safety**: obfuscation, transliteration,
Unicode homoglyphs, and content split across messages all get through. Use it as a coarse
pre-filter and backstop; for real content safety, plug in a dedicated moderation model.

### schema_validation_turn_filter: self-correcting structured output

```erlang
Schema = #{type => object,
           properties => #{<<"name">> => #{type => string},
                           <<"age">> => #{type => integer, minimum => 0}},
           required => [<<"name">>, <<"age">>]},
K = beamai:chat_client(#{}, [beamai_filters:schema_validation_turn_filter(Schema, 2)]).
```

"Take the text → parse JSON → check against the Schema"; on failure the Schema errors are fed
back and the loop re-entered, retrying `MaxRetries` times. Once retries are exhausted it returns
the last (still invalid) response as-is and **does not raise** — the failure only surfaces
downstream at parse time.

The Schema reuses `beamai_tool`'s parameter Schema shape directly (atom keys work too), and the
validator is `beamai_json_schema` (zero dependencies, a practical subset of DRAFT 2020-12;
`$ref`/`$defs`/`patternProperties`/`if-then-else`/`format` are not supported).

Opts: `max_errors` (how many errors to collect per round, default all; for Schemas with many
fields 5~10 is advisable, otherwise stuffing every error into the feedback blows up the prompt)
and `code_fence` (whether to strip ```` ```json ```` fences, default `true`).

It works best paired with the provider's native structured output (json_schema) — the native
constraint handles most cases, and this filter catches what slips through.

### Tool search: on-demand reveal for large tool sets

`beamai_tool_search` separates "registration" (can it be executed) from "advertisement" (can the
model see it): register everything, but advertise only a single `tool_search` tool on the first
round. To get any work done the model must first describe what it needs and search; only on the
next round do the matching tools become visible. Spring measured 34~64% token savings with 28
tools.

```erlang
Tools = [...],                                    %% all tools
{SearchTool, Filter} = beamai_tool_search:new(Tools, #{}),
K0 = beamai_chat_client:new(#{}, [Filter]),
K = beamai_chat_client:add_tools(K0, [SearchTool | Tools]).   %% register everything
```

Opts: `index_module` (default `beamai_tool_index_keyword`, `beamai_tool_index_regex` also
available), `index_opts`, `max_results` (default 5), `accumulate` (default `true`, the union of
all searches so far; `false` honors only the most recent one), and `tool_name` (default
`<<"tool_search">>`).

Key points:

- **Unindexed tools pass through untouched** — anything in the advertised list the filter never
  indexed (e.g. interrupt tools appended by the agent at runtime) is left alone, never silently
  swallowed.
- **The search tool is always advertised**, otherwise one missed round would leave no way to
  search again.
- **The model can still call non-advertised tools** (ChatClient execution does not consult the
  advertised list), so a direct call from earlier context won't fail.
- The index backend is a behaviour (`beamai_tool_index`); vector backends are left for
  beamai_extra to plug in.
- No automatic system-prompt injection (to avoid dual system messages); if you want stronger
  steering, splice `beamai_tool_search:default_system_suffix/0` into your `system_prompt`
  yourself.

---

## Per-filter private context

Each filter has a **private context** (FCtx), isolated by filter name and separate from the shared `beamai_context`:

- The around closure **reads** private state via the 2nd parameter `FCtx` and **writes** it by returning `{Response, NewFCtx}`.
- Different filters' private contexts are mutually invisible (no clash even with the same internal keys).
- Private state is threaded along with the shared context and **lives across one invoke** — including each iteration of the tool-calling loop, and between a filter's own `around_chat` and `around_tool`.
- Use the 3rd argument of `beamai_filter:new/3` to set the initial value (default `#{}`), seeded on first entry into that filter.

> Private context lives only within one invoke; it is not persisted across invokes. For cross-invoke state (e.g. a global counter), use an external store.

When a simple filter needs no private state, the around may just return `Response` (skipping the tuple).

---

## Onion execution order

For the filters list `[A, B]` (A earlier = outer) wrapping the terminal (LLM) on the chat chain:

```
A before → B before → Terminal → B after → A after
```

Composition (`beamai_filter_chain:compose/3`, Phase = `around_chat`):

```
compose([A, B], Phase, Terminal)
  = fun(Req) -> A_around(Req, fun(R) -> B_around(R, Terminal) end) end
```

where `X_around` is "run X's before, `Next` into the inner, then run X's after on the way back".

- **Before**: each layer's before runs in list order (A first, B second).
- **terminal**: the innermost runs the real LLM call / tool execution.
- **After**: runs in **automatically reversed order** (B first, A second) — the natural unwinding of the nested call stack, no manual ordering needed.

If a filter's around does not call `Next`, that is a short-circuit (skip all inner layers); the filter constructs and returns the `Response` directly. Outer filters' after still runs.

The other chains work the same way, replacing `around_chat` with that chain's hook name
(Phase = `around_step` / `around_tool` / `around_turn`).

The turn chain has one twist: its terminal is the **inner step chain** (the loop filter runs one
iteration per `Next` call) — the same filter keeps its relative layering in both chains (A always
outside B).

---

## API Reference

### beamai_filter module

#### Constructors

```erlang
%% Create a filter (private state initial value #{}).
%% Hooks is a hook map, any subset of around_chat/around_step/around_tool/
%% around_turn/token_transform.
-spec new(Name :: binary(), Hooks :: hooks()) -> filter().

%% Create a filter (with an explicit private state initial value Init)
-spec new(Name :: binary(), Hooks :: hooks(), Init :: map()) -> filter().
```

Hook form:

```erlang
-type hook_type() :: around_chat | around_step | around_tool | around_turn |
                     token_transform.
-type hooks() :: #{
    around_chat => around_fun(),
    around_step => around_fun(),
    around_tool => around_fun(),
    around_turn => around_fun(),
    token_transform => token_transform()
}.
-type around_fun() :: fun((Request, FCtx, Next) -> Response | {Response, NewFCtx}).
```

Not calling `Next` short-circuits (skips the inner layers), returning the `Response` directly.

#### Utilities

```erlang
%% Get one of the filter's hooks (undefined if absent)
-spec hook(filter(), hook_type()) -> around_fun() | undefined.

%% Get the filter's private context initial value
-spec init(filter()) -> map().
```

### beamai_filter_chain module

```erlang
%% Run one chain's filter onion.
%% Phase says which around hook the chain uses: around_chat for chat,
%% around_step for step, around_tool
%% for tool. Only filters with the matching around enter the onion; others are
%% skipped. Terminal produces the innermost response; it throws on error, which
%% run/4 catches with try/catch, returning {ok, Response} | {error, Reason}.
-spec run(Filters :: [filter()],
          Phase :: hook_type(),
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

### beamai_chat_client integration

```erlang
%% Give the full filter list once when building the ChatClient
%% (registration order is layer order: earlier in the list = more outer)
beamai_chat_client:new(Settings, Filters) -> ChatClient.
```

Filters **cannot be appended after construction** — layering is decided entirely by
this list's order. For conversation memory, put
`beamai_memory_filter:memory_filter(Store)` **first** in the list (outermost:
expand the full history before inner filters run).

Tool modules (`beamai_chat_client:add_tool_module/2`) provide tools only; they do not carry filters.

> **system_prompts layering**: `system_prompts` given in `invoke_chat`'s `Opts` are
> appended at call time as a temporary **innermost** filter — prepending system
> messages after all user filters and right before the LLM. Hence user chat filters
> see messages **without** system prompts, and the memory filter never stores them
> into history.

### beamai convenience API

```erlang
%% Create a ChatClient (full filter list given once)
beamai:chat_client(Settings, Filters) -> ChatClient.

%% Quickly create a filter (pass a hook map directly; put it in chat_client/2's Filters list)
beamai:filter(Name, Hooks) -> Filter.
beamai:filter(Name, Hooks, Init) -> Filter.
```

---

## Usage

### 1. Give filters when building the ChatClient

```erlang
Logger = beamai:filter(<<"logger">>, #{
    %% around_tool: log the tool name before
    around_tool => fun(#{tool := #{name := Name}, args := Args} = Req, _FCtx, Next) ->
        io:format("Calling tool: ~ts(~p)~n", [Name, Args]),
        Next(Req)
    end
}),
K0 = beamai:chat_client(#{}, [Logger]).
```

### 2. Filter layering (registration order is layer order)

Earlier in the list = more outer: its before runs earlier and its after runs later.

```erlang
Validator   = beamai:filter(<<"validator">>, #{around_tool => ValidateFn}),
Logger      = beamai:filter(<<"logger">>, #{around_tool => LogFn}),
Transformer = beamai:filter(<<"transformer">>, #{around_tool => TransformFn}),

%% List order is onion layering (validator outermost, transformer innermost)
K = beamai:chat_client(#{}, [Validator, Logger, Transformer]).

%% Before execution order: validator → logger → transformer → Terminal
%% After execution order: transformer → logger → validator (auto-reversed)
```

---

## Complete Examples

### Example 1: tool filter — log + double the result (one around_tool handles both sides)

```erlang
%% One around_tool closure: log the call before, rewrite result after
LogDouble = beamai:filter(<<"log_and_double">>, #{
    around_tool => fun(#{tool := #{name := Name}, args := Args} = Req, _FCtx, Next) ->
        io:format("[LOG] ~ts(~p)~n", [Name, Args]),
        #{result := Result} = Resp = Next(Req),
        Resp#{result => Result * 2}
    end
}),

%% Create the ChatClient (filters given once) and register a tool
K0 = beamai:chat_client(#{}, [LogDouble]),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})).

%% Call (3 + 5 = 8, doubled after = 16)
%% (tool execution goes through the ChatClient's tool filter onion chain)
```

### Example 2: chat filter — inject a system message + audit (one around_chat handles both sides)

```erlang
%% One around_chat closure: inject a system message before, log response length after
SystemAudit = beamai:filter(<<"system_and_audit">>, #{
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
}),

K0 = beamai:chat_client(#{}, [SystemAudit]),
K1 = beamai:add_chat_model(K0, LLMConfig).

%% On each request, the chat filter chain injects a system message and audits the response.
```

### Example 3: short-circuit — around_tool does not call `Next`

```erlang
%% tool filter: short-circuit on validation failure (don't call Next, skip the real tool)
Guard = beamai:filter(<<"guard">>, #{
    around_tool => fun(#{args := #{a := A}, context := Ctx} = Req, _FCtx, Next) ->
        case A > 1000 of
            true  -> #{result => {error, <<"a exceeds limit">>}, context => Ctx};
            false -> Next(Req)
        end
    end
}),
K = beamai:chat_client(#{}, [Guard]).
```

> Short-circuit means "don't call `Next`"; the filter constructs and returns the `Response` directly. The after of any outer filter wrapping it still runs — handy for unified cleanup on the outside.

### Example 4: private context — accumulate a counter across tool-loop iterations

```erlang
%% around_chat: count LLM calls within this invoke using the private context
Counter = beamai:filter(<<"counter">>, #{
    around_chat => fun(Req, FCtx, Next) ->
        N = maps:get(calls, FCtx, 0),
        Resp = Next(Req),
        logger:info("LLM call #~B", [N + 1]),
        {Resp, FCtx#{calls => N + 1}}   %% return {Resp, NewFCtx} to write back private state
    end
}),
K = beamai:chat_client(#{}, [Counter]).
%% Each LLM call in the tool loop increments calls, isolated from other filters' private state.
```

See more in [examples/src/example_filter.erl](../examples/src/example_filter.erl).

---

## Relationship with middleware

This system uses the common **around middleware** form:

- The around closure `fun(Request, FCtx, Next) -> Response` maps to middleware's "receive request → call `next` → handle response".
- Before/after live in one closure, bridged by locals — no need to pass values through a shared context between before and after.
- Short-circuit = don't call `Next`; retry = call `Next` multiple times; no dedicated halt protocol.
- One filter bundles the arounds for several chains (`around_turn` / `around_step` / `around_chat` / `around_tool`), each chain selected independently.
- The tool loop itself is a chain link (the turn chain's innermost filter) whose `next` is one iteration — the counterpart of Spring AI's `ToolCallingAdvisor`.

---

## Relationship with the Middleware framework

The [beamai_extra](https://github.com/TTalkPro/beamai_extra) extension project provides a more advanced Middleware system (in beamai_tools) with stateful management, preset configurations, call limits, human approval, retry, and fallback. Middleware is converted into filters registered with the ChatClient, so both ultimately run in the same onion chain.

| Feature | Filter (this doc) | Middleware (beamai_extra) |
|---------|-------------------|---------------------------|
| Complexity | Lightweight, private context scoped to one invoke | Full framework, cross-call state management |
| Presets | None | production/development presets, etc. |
| Built-in features | None | Call limits, human approval, retry, fallback |
| Use cases | Simple wrapping: logging, validation, injection, caching | Complex control: rate limiting, retry, fallback |

---

## More Resources

- [beamai_core README](../apps/beamai_core/README_EN.md) - ChatClient architecture docs
- [MEMORY_EN.md](MEMORY_EN.md) - Conversation memory (Memory filter)
- [API Reference](API_REFERENCE.md) - API reference docs
