# Conversation Memory (Memory Filter Mechanism)

English | [中文](MEMORY.md)

beamai_core's conversation memory fully decouples conversation-history storage and
injection from the Kernel: **the Kernel is stateless and each invoke only takes the
single latest message**. History is managed by a Memory Filter together with a
ChatMemory store, keyed by `conversation_id`. The Memory Filter is a single onion-style
filter (with a single around_chat hook, see the [Filter docs](FILTER_EN.md)).

Design rationale: [design/kernel_memory_filter_redesign.md](../design/kernel_memory_filter_redesign.md).

## Table of Contents

- [Core Idea](#core-idea)
- [Components](#components)
- [delta mode vs full mode](#delta-mode-vs-full-mode)
- [Quick Start](#quick-start)
- [ChatMemory Behaviour](#chatmemory-behaviour)
- [Windowed Store](#windowed-store)
- [Data Flow](#data-flow)

---

## Core Idea

- **The Kernel stores no messages**. `beamai_context` no longer holds `messages` / `history`.
- **Each invoke passes only the latest message** (first round = user message, later rounds = tool results).
- Storage, injection and trimming of history are handled entirely by the **Memory Filter + store**, isolated by `conversation_id`.
- Without memory, the Kernel degrades to a **stateless single-shot call** (the tool loop accumulates locally).

## Components

| Module | Responsibility |
|--------|----------------|
| `beamai_chat_memory` | ChatMemory **behaviour** + dispatch API, handle `{Module, Ref}` |
| `beamai_chat_memory_ets` | Default ETS gen_server implementation (process owns the ETS table) |
| `beamai_chat_memory_dets` | DETS persistent implementation (history recovered per conversation_id after restart) |
| `beamai_chat_memory_window` | Sliding-window wrapper: full history kept underneath, windowed on `mem_get` |
| `beamai_memory_filter` | A single filter: around_chat stores delta + expands history (before), stores reply (after) |
| `beamai_kernel:new/2` | Put the memory filter into the filters list at build time (first = outermost) |

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

## Quick Start

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

## ChatMemory Behaviour

Handle convention `{Module, Ref}` (same as `{beamai_store_ets, Name}`); the dispatch API
unpacks and forwards.

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

## Windowed Store

`beamai_chat_memory_window` wraps any store: full history is kept underneath, and a
count-based sliding window is applied only on `mem_get`. Rules: system messages are always
kept and placed at the head; only the most recent N non-system messages are retained;
after trimming, orphaned leading `tool` messages are dropped.

```erlang
{ok, _} = beamai_chat_memory_ets:start_link(inner),
Inner = beamai_chat_memory_ets:handle(inner),
Store = beamai_chat_memory_window:handle(Inner, 20),   %% most recent 20 non-system messages
K = beamai_kernel:new(#{}, [beamai_memory_filter:memory_filter(Store)]).
```

> Token-based trimming/summarization is intentionally not in core (to avoid a reverse
> dependency on `beamai_conversation_buffer`); it can be provided by an upper-layer
> store implementing the same behaviour.

## Data Flow (with tool loop, delta mode)

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

## Key Source Files

| File | Description |
|------|-------------|
| `apps/beamai_core/src/behaviours/beamai_chat_memory.erl` | behaviour + dispatch API |
| `apps/beamai_core/src/kernel/beamai_chat_memory_ets.erl` | ETS default implementation |
| `apps/beamai_core/src/kernel/beamai_chat_memory_dets.erl` | DETS persistent implementation |
| `apps/beamai_core/src/kernel/beamai_chat_memory_window.erl` | windowed wrapper |
| `apps/beamai_core/src/kernel/beamai_memory_filter.erl` | Memory Filter |
| `apps/beamai_core/src/kernel/beamai_kernel.erl` | `new/2` (filters given once), invoke dual-mode |
