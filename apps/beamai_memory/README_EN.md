%%%-------------------------------------------------------------------
%%% Agent Memory - Layered Architecture v4.0
%%%
%%% Unified memory management system with clear separation of responsibilities
%%% using a dual-manager layered design.
%%%-------------------------------------------------------------------

# Agent Memory

A unified memory management system that provides a unified interface for short-term memory (Checkpointer) and long-term memory (Store).

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                    beamai_memory (API Layer)                        │
│                 Unified External Interface - Coordination Layer     │
└─────────────────────────────────────────────────────────────────────┘
                            │
        ┌───────────────────┴───────────────────┐
        │                                       │
┌──────────────────────────┐        ┌──────────────────────────────┐
│  checkpoint_manager      │        │  store_manager               │
│  (Checkpoint Manager)    │        │  (Storage Manager)           │
│                          │        │                              │
│  - Checkpoint CRUD       │        │  - context_store management  │
│  - Time travel           │        │  - persistent_store mgmt     │
│  - Branch management     │        │  - Store selection logic     │
│                          │        │  - Archive functionality     │
└──────────────────────────┘        └──────────────────────────────┘
        │                                       │
        │                                       │
┌──────────────────────────┐        ┌──────────────────────────────┐
│  Extension Modules       │        │  Extension Modules           │
│  ├── time_travel         │        │  ├── store_archiver          │
│  └── branch              │        │  └── ...                     │
└──────────────────────────┘        └──────────────────────────────┘
```

## Design Principles

1. **Layered Architecture** - API layer coordinates, manager layer implements, extension layer enhances
2. **Separation of Responsibilities** - checkpoint_manager handles checkpoints, store_manager handles storage
3. **Dual Store Architecture** - Supports hot/cold data separation, can also degrade to single Store
4. **Pluggable Backends** - Store protocol supports multiple implementations (ETS, SQLite, etc.)

## Checkpoint Quantity Limits

### Configuration Options

checkpoint_manager supports configuring checkpoint quantity limits with automatic cleanup of old checkpoints:

```erlang
%% Configure quantity limit at creation time
{ok, ContextStore} = beamai_store_ets:start_link(my_context, #{}),
{ok, Memory} = beamai_memory:new(#{
    context_store => {beamai_store_ets, my_context},
    checkpoint_config => #{
        max_checkpoints => 100,    %% Keep at most 100 checkpoints
        auto_prune => true         %% Automatically clean up old checkpoints
    }
}).
```

### Quantity Limit Strategy

| Config | Default | Description |
|--------|---------|-------------|
| `max_checkpoints` | **50** | Maximum number of checkpoints (optimized for memory) |
| `auto_prune` | `true` | Automatically clean up old checkpoints when limit is exceeded |
| ETS `max_items` | **1000** | ETS storage capacity limit (optimized for memory) |
| ETS `max_namespaces` | **1000** | Namespace quantity limit |

**Default configuration optimized to reduce memory usage:**
- Checkpoint defaults to keeping at most **50** checkpoints
- ETS Store defaults to at most **1000** data items
- Old data is automatically cleaned up when limits are exceeded

### Cleanup Strategy

- **Automatic cleanup**: When saving a checkpoint, if the quantity exceeds `max_checkpoints`, the oldest checkpoints are automatically deleted
- **Manual cleanup**: Call `beamai_memory:prune_checkpoints/3` for manual cleanup
- **ETS Store**: Uses LRU eviction policy (automatically triggered, removes approximately 10% of old data)

### Statistics and Monitoring

```erlang
%% Get statistics
Stats = beamai_memory:get_checkpoint_stats(Memory),
%% => #{
%%     total_checkpoints => 42,
%%     max_checkpoints => 50,
%%     branches => 1,
%%     usage_percentage => 84.0
%% }

%% Get checkpoint count for a thread
Count = beamai_memory:get_checkpoint_count(Memory, <<"thread-1">>).

%% Manual cleanup
{ok, DeletedCount} = beamai_memory:prune_checkpoints(Memory, <<"thread-1">>, 50).

%% Clean up all threads
{ok, Result} = beamai_memory:prune_all_checkpoints(Memory, 100).
```

## Module Responsibilities

### API Layer

| Module | Responsibility | Main Functions |
|--------|----------------|----------------|
| `beamai_memory` | Unified API | Coordinates both managers, provides convenient interface |

### Manager Layer

| Manager | Responsibility | File |
|---------|----------------|------|
| **checkpoint_manager** | Checkpoint management | `checkpoint/agent_checkpoint_manager.erl` |
| **store_manager** | Storage management | `store/beamai_store_manager.erl` |

### Extension Layer

| Extension | Responsibility | File |
|-----------|----------------|------|
| **time_travel** | Time travel | `checkpoint/agent_checkpoint_time_travel.erl` |
| **branch** | Branch management | `checkpoint/agent_checkpoint_branch.erl` |
| **archiver** | Archive functionality | `store/beamai_store_archiver.erl` |

## Dual Store Architecture

### Design Motivation

| Storage Type | Purpose | Characteristics |
|--------------|---------|-----------------|
| context_store | Current session, Checkpointer | Fast read/write, in-memory storage |
| persistent_store | Historical archives, knowledge base | Persistent, large capacity |

### Data Flow

```
User conversation → context_store → [Archive] → persistent_store
                        ↑                           ↓
                 checkpoint_manager          [Load history]
                 (Time travel + Branching)
```

## Usage Examples

### Basic Usage

```erlang
%% Create Memory (Dual Store mode)
{ok, ContextStore} = beamai_store_ets:start_link(my_context, #{}),
{ok, PersistStore} = beamai_store_sqlite:start_link(my_persist, #{
    db_path => "/data/agent-memory.db"
}),
{ok, Memory} = beamai_memory:new(#{
    context_store => {beamai_store_ets, my_context},
    persistent_store => {beamai_store_sqlite, my_persist}
}).

%% Save checkpoint
Config = #{thread_id => <<"thread-1">>},
State = #{messages => [#{role => user, content => <<"Hello">>}]},
{ok, CpId} = beamai_memory:save_checkpoint(Memory, Config, State).

%% Load checkpoint
{ok, LoadedState} = beamai_memory:load_checkpoint(Memory, Config).
```

### Time Travel

```erlang
%% Go back 3 steps
{ok, State} = beamai_memory:go_back(Memory, Config, 3).

%% Go forward 1 step
{ok, State} = beamai_memory:go_forward(Memory, Config, 1).

%% Jump to a specific checkpoint
{ok, State} = beamai_memory:goto(Memory, Config, <<"cp_12345_6789">>).

%% Undo (go back 1 step)
{ok, State} = beamai_memory:undo(Memory, Config).

%% Redo (go forward 1 step)
{ok, State} = beamai_memory:redo(Memory, Config).

%% View history
{ok, History} = beamai_memory:list_history(Memory, Config).
```

### Branch Management

```erlang
%% Create a new branch
{ok, BranchId} = beamai_memory:create_branch(Memory, Config, <<"experiment">>, #{}).

%% Switch branch
{ok, State} = beamai_memory:switch_branch(Memory, Config, <<"experiment">>).

%% List all branches
{ok, Branches} = beamai_memory:list_branches(Memory).

%% Compare two branches
{ok, Diff} = beamai_memory:compare_branches(Memory, <<"main">>, <<"experiment">>).
```

### Long-term Memory

```erlang
%% Store knowledge (to persistent_store)
ok = beamai_memory:put(Memory,
    [<<"knowledge">>, <<"facts">>],
    <<"fact-1">>,
    #{content => <<"CL-Agent is an Erlang AI framework">>},
    #{persistent => true}  % Specify storage to persistent_store
).

%% Search knowledge
{ok, Results} = beamai_memory:search(Memory, [<<"knowledge">>], #{}).

%% Get knowledge
{ok, Item} = beamai_memory:get(Memory, [<<"knowledge">>, <<"facts">>], <<"fact-1">>).
```

### Archive Functionality

```erlang
%% Archive current session
{ok, SessionId} = beamai_memory:archive_session(Memory, <<"thread-1">>).

%% Archive with summary
SummarizeFn = fun(Checkpoints) ->
    <<"Session contains 5 messages">>
end,
{ok, SessionId} = beamai_memory:archive_session(Memory, <<"thread-1">>, #{
    summarize_fn => SummarizeFn,
    tags => [<<"important">>]
}).

%% List archived sessions
{ok, Archives} = beamai_memory:list_archived(Memory).

%% Filter archives by tag
{ok, Archives} = beamai_memory:list_archived(Memory, #{
    tags => [<<"important">>]
}).

%% Load archived session (read-only)
{ok, Checkpoints} = beamai_memory:load_archived(Memory, SessionId).

%% Restore archived session to Checkpointer
{ok, ThreadId} = beamai_memory:restore_archived(Memory, SessionId).

%% Delete archive
ok = beamai_memory:delete_archived(Memory, SessionId).
```

### Convenience Functions

```erlang
%% Add message
ok = beamai_memory:add_message(Memory, Config, #{
    role => user,
    content => <<"Hello">>
}).

%% Get message history
{ok, Messages} = beamai_memory:get_messages(Memory, Config).

%% Get channel value
{ok, Value} = beamai_memory:get_channel(Memory, Config, messages).

%% Set channel value
ok = beamai_memory:set_channel(Memory, Config, counter, 42).
```

## Advanced Usage

### Using Managers Directly

```erlang
%% Get Checkpointer manager
Checkpointer = beamai_memory:get_checkpointer(Memory),

%% Use all manager functions
{ok, CpId} = agent_checkpoint_manager:save(Checkpointer, Checkpoint, Config),
{ok, Cp} = agent_checkpoint_manager:load(Checkpointer, latest, Config),

%% Use time travel extension
{ok, History} = agent_checkpoint_time_travel:list_history(Checkpointer, Config),

%% Use branch management extension
{ok, BranchId} = agent_checkpoint_branch:create_branch(
    Checkpointer, Config, <<"feature-x">>, #{}
).

%% Get Store manager
StoreManager = beamai_memory:get_store_manager(Memory),

%% Use Store manager
ok = beamai_store_manager:put(StoreManager, Namespace, Key, Value, #{}),
{ok, Item} = beamai_store_manager:get(StoreManager, Namespace, Key).

%% Use archiver
{ok, SessionId} = beamai_store_archiver:archive_session(
    StoreManager, Checkpointer, ThreadId
).
```

## File Structure

```
beamai_memory/
├── README.md                              # This document
├── rebar.config                           # Build configuration
├── include/
│   ├── beamai_checkpointer.hrl             # Checkpoint record definitions
│   ├── agent_episodic_memory.hrl          # Episodic memory records
│   ├── agent_procedural_memory.hrl        # Procedural memory records
│   ├── agent_semantic_memory.hrl          # Semantic memory records
│   └── beamai_store.hrl                    # Store record definitions
├── src/
│   ├── api/
│   │   └── beamai_memory.erl               # Unified Memory API
│   ├── checkpoint/                        # Checkpoint management modules
│   │   ├── agent_checkpoint_protocol.erl  # Protocol definition
│   │   ├── agent_checkpoint_manager.erl   # Core manager
│   │   ├── agent_checkpoint_time_travel.erl # Time travel extension
│   │   └── agent_checkpoint_branch.erl    # Branch management extension
│   ├── store/                             # Storage management modules
│   │   ├── beamai_store.erl                # Store protocol
│   │   ├── beamai_store_ets.erl            # ETS backend
│   │   ├── beamai_store_sqlite.erl         # SQLite backend
│   │   ├── beamai_store_manager.erl        # Storage manager
│   │   └── beamai_store_archiver.erl       # Archiver
│   ├── memory/
│   │   ├── agent_episodic_memory.erl      # Episodic memory implementation
│   │   ├── agent_procedural_memory.erl    # Procedural memory implementation
│   │   └── agent_semantic_memory.erl      # Semantic memory implementation
│   ├── utils/
│   │   ├── beamai_memory_helpers.erl       # Helper functions
│   │   ├── beamai_memory_types.erl         # Type definitions
│   │   └── beamai_memory_utils.erl         # Utility functions
│   └── buffer/
│       └── ...                             # Buffer modules
└── test/
    └── ...                                 # Test files
```

## Comparison with Previous Versions

| Feature | Old Architecture (v3.x) | New Architecture (v4.x) |
|---------|------------------------|-------------------------|
| Layered Design | No clear layering | API + Manager + Extension |
| Code Organization | Single large file (900+ lines) | Separation of responsibilities, modular |
| Checkpointer Logic | Mixed in beamai_memory.erl | checkpoint_manager |
| Store Management | Direct calls to beamai_store | store_manager |
| Time Travel | Mixed together | agent_checkpoint_time_travel |
| Branch Management | Mixed together | agent_checkpoint_branch |
| Archive Functionality | Mixed together | beamai_store_archiver |
| Testability | Lower | Each module independently testable |
| Extensibility | Requires modifying large file | Add new extension modules |

## Version History

- **4.0.0** - Layered architecture refactoring, dual-manager design
- **3.0.0** - Dual Store architecture, Checkpointer uses Store interface
- **2.0.0** - Separated Checkpointer and Store
- **1.0.0** - Initial version
