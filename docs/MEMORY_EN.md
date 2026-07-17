# Conversation Memory

English | [中文](MEMORY.md)

beamai_core fully decouples conversation-history storage and injection from the Kernel:
**the Kernel is stateless and each invoke only takes the single latest message**. History
is keyed by `conversation_id`.

Memory comes in **two layers**, solving different problems:

| Layer | Behaviour | Responsibility |
|---|---|---|
| **Storage** | `beamai_chat_memory` | Backend (ETS/DETS/your own); dumb get/add/clear, no policy |
| **Policy** | `beamai_memory_provider` | Cross-run load/persist + **pre-send transform** (window trim / summarization / RAG recall) |

On top of those layers there are **two independent entry paths**, depending on whether
you use the Kernel or the Agent:

| Path | How you wire it | For |
|---|---|---|
| **Kernel-level** | `beamai_memory_filter:memory_filter(Store)` first in the filters list | Using `beamai_kernel` / the `beamai` facade directly |
| **Agent-level** | `memory => Provider` config; the Agent orchestrates it in its tool loop | Using `beamai_agent` |

> **Do not mix the two**: `beamai_agent` **does not use** the memory filter — it calls
> `beamai_memory_provider` (history/prepare/append) explicitly in its own tool loop.
> The Agent layer has no notion of filters (filters belong to the kernel layer).

Design rationale: [design/kernel_memory_filter_redesign.md](../design/kernel_memory_filter_redesign.md).

## Table of Contents

- [Core Idea](#core-idea)
- [Components](#components)
- [delta mode vs full mode](#delta-mode-vs-full-mode)
- [Quick Start (Kernel-level)](#quick-start-kernel-level)
- [ChatMemory Behaviour (storage layer)](#chatmemory-behaviour-storage-layer)
- [Memory Provider (policy layer / Agent-level)](#memory-provider-policy-layer--agent-level)
- [Sliding Window](#sliding-window)
- [Data Flow](#data-flow)

---

## Core Idea

- **The Kernel stores no messages**. `beamai_context` no longer holds `messages` / `history`.
- **Each invoke passes only the latest message** (first round = user message, later rounds = tool results).
- Storage, injection and trimming of history are handled entirely by the **store + filter/provider**, isolated by `conversation_id`.
- Without memory, the Kernel degrades to a **stateless single-shot call** (the tool loop accumulates locally).

## Components

| Module | Layer | Responsibility |
|--------|-------|----------------|
| `beamai_chat_memory` | Storage | ChatMemory **behaviour** + dispatch API, handle `{Module, Ref}` |
| `beamai_chat_memory_ets` | Storage | Default ETS gen_server implementation (process owns the ETS table) |
| `beamai_chat_memory_dets` | Storage | DETS persistent implementation (history recovered per conversation_id after restart) |
| `beamai_memory_provider` | Policy | Provider **behaviour** + dispatch API: history/append/prepare/clear |
| `beamai_memory_provider_default` | Policy | Default provider: wraps a store; `new/2` adds a sliding window |
| `beamai_memory_filter` | Kernel entry | A single filter: around_chat stores delta + expands history (before), stores reply (after) |
| `beamai_kernel:new/2` | Kernel entry | Put the memory filter into the filters list at build time (first = outermost) |

## delta mode vs full mode

`invoke/3` picks the mode automatically based on whether memory is enabled:

| | full mode | delta mode |
|---|---|---|
| Trigger | **no** memory filter in the filters list | filters list contains a memory filter (bound to a store) |
| Passed to pipeline each round | locally accumulated **full** messages | only the **new delta** |
| Who accumulates full history | a local variable in the tool loop | the ChatMemory store |
| Uses conversation_id | no | yes (ephemeral id generated if absent, cleared at end) |
| Context continuous within tool loop | ✅ | ✅ |
| Memory across invokes | ❌ (stateless single-shot) | ✅ (persisted by conversation_id) |
| Who records the assistant message | local concatenation | the Memory Filter's around_chat stores into the store (after) |

> **Why full mode exists**: a tool-calling loop is inherently multi-round (LLM → tool →
> LLM again). Without a store, subsequent LLM calls within a single invoke must still see
> the earlier tool_call and tool results — full mode accumulates the complete conversation
> in a local variable and sends the full list each round. It is not persisted and does not
> cross invokes.

## Quick Start (Kernel-level)

```erlang
%% 1. Start a conversation store (default ETS implementation)
{ok, _Pid} = beamai_chat_memory_ets:start_link(my_mem),
Store = beamai_chat_memory_ets:handle(my_mem),   %% handle {beamai_chat_memory_ets, my_mem}

%% 2. Build the Kernel and enable memory
K0 = beamai_kernel:new(#{}, [beamai_memory_filter:memory_filter(Store)]),
K  = beamai_kernel:add_service(K0, LlmConfig),

%% 3. Identify the conversation with a conversation_id; pass only the latest message
Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"session-1">>),

{ok, R1, _} = beamai_kernel:invoke(K, [#{role => user, content => <<"My name is Alice">>}],
                                   #{context => Ctx}),
%% Same conversation_id: the second round's LLM sees the full history
{ok, R2, _} = beamai_kernel:invoke(K, [#{role => user, content => <<"What's my name?">>}],
                                   #{context => Ctx}).

%% 4. Read / clear conversation history
History = beamai_chat_memory:mem_get(Store, <<"session-1">>),
ok      = beamai_chat_memory:mem_clear(Store, <<"session-1">>).
```

> If the context has no `conversation_id`, invoke generates an ephemeral id and clears it
> after finishing (i.e. a store exists but you only want a one-off), producing no
> cross-invoke memory.

## ChatMemory Behaviour (storage layer)

Handle convention `{Module, Ref}`: Module implements this behaviour, Ref identifies the
instance (registered name / pid / config tuple). The dispatch API unpacks the handle and
forwards to the implementing module.

```erlang
-callback mem_get(Ref :: term(), ConvId :: binary()) -> [message()].
-callback mem_add(Ref :: term(), ConvId :: binary(), Msgs :: [message()]) -> ok.
-callback mem_clear(Ref :: term(), ConvId :: binary()) -> ok.

%% Dispatch API (unpacks {Module, Ref})
beamai_chat_memory:mem_get(Store, ConvId).
beamai_chat_memory:mem_add(Store, ConvId, Msgs).
beamai_chat_memory:mem_clear(Store, ConvId).
```

Implement this behaviour to provide a custom backend (e.g. a SQLite/Redis store).

### Persistent Backend (DETS)

`beamai_chat_memory_dets` mirrors the ETS implementation but persists messages to a
DETS file: after a process/node restart, reopening the same file restores every
conversation's history (each write is followed by `dets:sync`).

```erlang
{ok, _Pid} = beamai_chat_memory_dets:start_link(my_mem, #{file => "data/chat.dets"}),
Store = beamai_chat_memory_dets:handle(my_mem),   %% handle {beamai_chat_memory_dets, my_mem}
%% Same usage as the ETS backend; restart with the same file to recover history
```

## Memory Provider (policy layer / Agent-level)

The storage layer only stores and retrieves. **Policy** — how much to trim, whether to
summarize, whether to do RAG recall — belongs to the Provider. `beamai_agent` takes this
path: it calls the provider explicitly in its own tool loop, with no kernel filter involved.

Responsibility split:

- **within-run** accumulation (across tool iterations in one turn) is held by the Agent loop itself;
- **cross-run** load/persist is handled by `history/2` and `append/3`;
- the **pre-send transform** (window / summarization / recall) is handled by `prepare/3`, a pure function.

```erlang
-callback history(Ref :: term(), ConvId :: binary()) -> [message()].
-callback append(Ref :: term(), ConvId :: binary(), Msgs :: [message()]) -> ok.
-callback prepare(Ref :: term(), ConvId :: binary(), Messages :: [message()]) -> [message()].
-callback clear(Ref :: term(), ConvId :: binary()) -> ok.

%% Dispatch API (unpacks {Module, Ref})
beamai_memory_provider:history(Provider, ConvId).
beamai_memory_provider:append(Provider, ConvId, Msgs).
beamai_memory_provider:prepare(Provider, ConvId, Messages).
beamai_memory_provider:clear(Provider, ConvId).
```

> **Visibility semantics of `history/2` (read-your-writes)**: within the same calling
> process, `history` must see every message for which `append` has already returned ok —
> the Agent tool loop relies on that ordering to "load history first, then persist new
> messages". If your implementation writes asynchronously (external DB / message queue),
> it must complete the write before `append` returns, or guarantee monotonic reads per
> conversation.

### Wiring it into an Agent

The `memory` key of `beamai_agent:new/1` accepts five forms:

| Config | Effect |
|---|---|
| absent / `default` | Shared default ETS store, no window (**memory is on by default**) |
| `{window, N}` | Shared default store + an N-message sliding window |
| `{store, Handle}` | Your own store, no window |
| `{Module, Ref}` | A fully custom provider (must implement `beamai_memory_provider`) |
| `false` / `none` | No memory |

```erlang
%% Simplest: default memory with a 50-message window
{ok, Agent} = beamai_agent:new(#{llm => LlmConfig, memory => {window, 50}}),

%% Bring your own store (e.g. DETS persistence)
{ok, _} = beamai_chat_memory_dets:start_link(my_mem, #{file => "data/chat.dets"}),
Store   = beamai_chat_memory_dets:handle(my_mem),
{ok, Agent2} = beamai_agent:new(#{llm => LlmConfig, memory => {store, Store}}),

%% Fully custom policy (summarization / RAG …)
{ok, Agent3} = beamai_agent:new(#{llm => LlmConfig, memory => {my_rag_provider, Ref}}).
```

> **The default store is a shared singleton**: registered under a fixed name (avoiding
> unbounded atom growth), with each agent partitioned by its own `conversation_id`. When
> `beamai_agent` runs as an OTP application it is a permanent child of the supervision
> tree; for library-style calls / bare eunit it falls back to a lazily started orphan
> singleton.

A custom policy only needs those four callbacks. `beamai_memory_provider:default/1` is a
shorthand for `beamai_memory_provider_default:new/1`.

## Sliding Window

The window is the responsibility of the **provider's `prepare/3`** — the underlying store
always keeps the full history, and trimming happens only before sending to the LLM:

```erlang
Provider = beamai_memory_provider_default:new(Store, 20).  %% most recent 20 non-system messages
```

The window rules of `beamai_memory_provider_default` (see its `safe_window/2`):

- system messages are always kept and placed at the head;
- only the most recent N non-system messages are retained;
- orphaned leading `tool` messages are dropped after trimming (so a tool result never
  lacks its matching tool_call).

`new/1` is equivalent to a window of `infinity`: `prepare` is the identity and the context
is unbounded.

> **The Kernel-level path currently has no window**: `beamai_memory_filter` replaces
> messages with the store's full history. To trim, either take the Agent path with a
> provider, or write your own `beamai_chat_memory` store that trims inside `mem_get`.
>
> Token-based trimming and summarization are intentionally not in core; provide them from
> an upper layer by implementing `beamai_memory_provider`.

## Data Flow (Kernel-level: with tool loop, delta mode)

```
invoke(Kernel, [User msg], #{context := Ctx(conv_id=s1)})
└─ tool_calling_loop, delta = [User msg]
   └─ run_chat_pipeline(delta)
      ├─ memory.around_chat before (order -1000, outer): mem_add(s1, delta); messages := mem_get(s1) full history
      ├─ system.around_chat before (order -500, inner): messages := [SysPrompt | messages]  (not stored)
      ├─ Terminal: LLM call
      └─ memory.around_chat after (outbound): mem_add(s1, assistant reply)
   ├─ has tool_calls → run tools (tool filter chain, around_tool), delta := [tool results], loop
   └─ plain text → return {ok, Response, Ctx}
```

memory is a single filter: the same `around_chat` closure expands history (before) on the
outer layer and stores the reply (after) on the way out; the onion chain guarantees the
order with no priority simulation. system_prompts are injected by a transient filter
(around_chat only, order -500, more inner), after history expansion and before the LLM,
and are **not written to the store**.

> **Reentrancy contract**: the one hazard is an outer filter re-running the inner chain
> with the same delta (the delta would be stored twice). Retry-style filters must
> therefore sit **outside** the memory filter — which conflicts with the "memory goes
> first in the list" convention, so following that convention avoids the problem entirely
> (the filter deliberately ships no half-measure dedup).

## Data Flow (Agent-level: explicit provider orchestration)

```
beamai_agent:run(Agent, UserMsg)
└─ tool loop
   ├─ history(Provider, ConvId)              load cross-run history
   ├─ this turn's messages accumulate locally in the loop (within-run)
   ├─ prepare(Provider, ConvId, Messages)    pre-send transform (window/summary/recall)
   ├─ kernel invoke → LLM
   ├─ has tool_calls → run tools, merge results into local accumulation, loop
   └─ append(Provider, ConvId, Msgs)         persist this turn's messages (cross-run)
```

The Agent path involves no filter: kernel and memory are two **orthogonal** construction
parameters — the kernel handles LLM/tool invocation, the provider handles cross-run
history, and they compose freely.

## Key Source Files

| File | Description |
|------|-------------|
| `apps/beamai_core/src/behaviours/beamai_chat_memory.erl` | storage-layer behaviour + dispatch API |
| `apps/beamai_core/src/kernel/beamai_chat_memory_ets.erl` | ETS default implementation |
| `apps/beamai_core/src/kernel/beamai_chat_memory_dets.erl` | DETS persistent implementation |
| `apps/beamai_core/src/behaviours/beamai_memory_provider.erl` | policy-layer behaviour + dispatch API |
| `apps/beamai_core/src/kernel/beamai_memory_provider_default.erl` | default provider (optional sliding window) |
| `apps/beamai_core/src/kernel/beamai_memory_filter.erl` | Memory Filter (Kernel-level entry) |
| `apps/beamai_core/src/kernel/beamai_kernel.erl` | `new/2` (filters given once), invoke dual-mode |
| `apps/beamai_agent/src/beamai_agent_state.erl` | Agent's `memory` config resolution (`setup_memory/1`) |
| `apps/beamai_agent/src/beamai_agent_tool_loop.erl` | Agent calls history/prepare/append explicitly in the loop |
