# DeepAgent SubAgent 架构设计

English | [中文](#中文版本)

## Overview

This design analyzes the possibility of using independent SubAgents (child agent processes) as task executors in DeepAgent, instead of the current node-based execution model.

## Status

- **Status**: Proposed
- **Created**: 2026-01-20
- **Author**: Design Discussion

---

## Current Architecture (Node-based)

```
┌─────────────────────────────────────────────────────────────┐
│                    Single Process Graph Execution            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   DeepAgent Process                                         │
│   ┌─────────────────────────────────────────────────────┐  │
│   │                   Graph Engine                       │  │
│   │  ┌─────┐   ┌─────┐   ┌─────────┐   ┌─────────────┐ │  │
│   │  │ LLM │──▶│Tool │──▶│ Fan-out │──▶│TaskExecutor │ │  │
│   │  │Node │   │Node │   │         │   │  (×N)       │ │  │
│   │  └─────┘   └─────┘   └─────────┘   └──────┬──────┘ │  │
│   │      ▲                                     │        │  │
│   │      │         ┌───────────────┐           │        │  │
│   │      └─────────│  Aggregator   │◀──────────┘        │  │
│   │                └───────────────┘                    │  │
│   │                                                     │  │
│   │   Shared State: graph_state (immutable)             │  │
│   └─────────────────────────────────────────────────────┘  │
│                                                             │
│   Characteristics:                                          │
│   • Execution within same process                          │
│   • Subtasks parallel via fan_out (logical parallelism)    │
│   • State passed through immutable data structures         │
│   • Subtasks create simplified sub-graphs (LLM node only)  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Current Task Execution Flow

```erlang
%% beamai_deepagent_task_node.erl
execute_task() → do_execute_task() → build_subgraph() → graph:run()
    ↓
create_substate() → inherit from parent state
    ↓
execute with LLM node only (simplified graph)
    ↓
extract_task_result() → save_task_result()
    ↓
merge results back to parent (via subtask_results list)
```

### Key Characteristics

- **Same Process**: All execution happens in one Erlang process
- **Immutable State**: Parent and child states are immutable
- **Logical Parallelism**: fan_out creates parallel branches within graph
- **Simplified Sub-graphs**: Subtasks only have LLM node
- **Synchronous Aggregation**: Results collected by aggregator_node

---

## Proposed Architecture (SubAgent-based)

```
┌─────────────────────────────────────────────────────────────┐
│                    Multi-Process Agent Collaboration         │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   Parent Agent (Process A)                                  │
│   ┌─────────────────────────────────────────────────────┐  │
│   │  Planner LLM  →  Decision: Assign tasks to SubAgents │  │
│   │       │                                              │  │
│   │       ▼                                              │  │
│   │  ┌─────────────────────────────────────────────┐    │  │
│   │  │           SubAgent Manager                   │    │  │
│   │  │  • Create/supervise SubAgent processes       │    │  │
│   │  │  • Task distribution                         │    │  │
│   │  │  • Result collection                         │    │  │
│   │  └──────────┬──────────────┬───────────────────┘    │  │
│   └─────────────│──────────────│─────────────────────────┘  │
│                 │              │                            │
│        ┌────────▼────┐  ┌──────▼──────┐  ┌────────────┐    │
│        │ SubAgent 1  │  │ SubAgent 2  │  │ SubAgent N │    │
│        │ (Process B) │  │ (Process C) │  │ (Process..)│    │
│        │             │  │             │  │            │    │
│        │ • Own LLM   │  │ • Own LLM   │  │ • Own LLM  │    │
│        │ • Own Tools │  │ • Own Tools │  │ • Own Tools│    │
│        │ • Own State │  │ • Own State │  │ • Own State│    │
│        │ • Own Msgs  │  │ • Own Msgs  │  │ • Own Msgs │    │
│        └─────────────┘  └─────────────┘  └────────────┘    │
│                                                             │
│   Communication: Erlang Messages / A2A Protocol             │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## Architecture Change Analysis

### Components Requiring Modification

| Component | Current State | SubAgent Changes Required | Change Level |
|-----------|---------------|---------------------------|--------------|
| `beamai_deepagent.erl` | Main entry | Add SubAgent management | **Large** |
| `beamai_deepagent_task_node.erl` | Internal sub-graph | Rewrite to spawn Agent processes | **Rewrite** |
| `beamai_deepagent_aggregator_node.erl` | Sync result collection | Change to async process messages | **Large** |
| `beamai_deepagent_router.erl` | Node routing | Add inter-process routing | **Medium** |
| **New** `beamai_subagent_sup.erl` | - | Supervisor for SubAgents | **New** |
| **New** `beamai_subagent_protocol.erl` | - | Parent-child communication | **New** |
| `graph_send.erl` | fan_out logic | May need cross-process support | **Medium** |

**Estimated Change**: ~60-70% of deepagent code needs modification or rewrite

---

## Advantages

| Advantage | Description | Use Case |
|-----------|-------------|----------|
| **True Process Isolation** | SubAgent crash doesn't affect Parent (Erlang "Let it crash") | High reliability requirements |
| **True Concurrent Execution** | Multi-core parallel, full BEAM scheduler utilization | CPU-intensive tasks, multiple LLM calls |
| **Independent Configuration** | Each SubAgent can have different LLM, tools, system prompt | Heterogeneous tasks, specialization |
| **Reusable Components** | SubAgents can be independent, reusable Agents | Modular design, Agent marketplace |
| **Distributed Potential** | Can scale to multi-node Erlang cluster | Large-scale deployment |
| **Resource Isolation** | Each process has independent memory space | Memory-sensitive tasks |
| **Timeout Control** | Independent timeout per SubAgent | Unreliable tasks |
| **Monitoring/Observability** | Each process can be monitored independently | Production debugging |

### Example Use Case

```erlang
%% SubAgent approach - each subtask independently configured
spawn_subagent(#{
    task => <<"Translate document">>,
    llm => llm_client:create(deepseek, #{model => <<"deepseek-chat">>}),
    tools => [translation_tool()],
    timeout => 60000
}),

spawn_subagent(#{
    task => <<"Code review">>,
    llm => llm_client:create(anthropic, #{model => <<"claude-sonnet">>}),
    tools => [code_analysis_tool()],
    timeout => 120000
})
```

---

## Disadvantages

| Disadvantage | Description | Mitigation |
|--------------|-------------|------------|
| **Architecture Complexity** | Inter-process communication, state sync, fault handling | Good abstraction layer, clear protocol |
| **Debugging Difficulty** | Cross-process debugging, distributed tracing | Enhanced trace system, unified logging |
| **Context Passing** | Context between parent-child needs serialization | Design concise context format |
| **Performance Overhead** | Process creation, message passing, serialization | Process pool reuse, batch communication |
| **Consistency Challenges** | Multiple Agents may understand same task differently | Clear task specs, result validation |
| **Implementation Cycle** | Need to rewrite significant code | Phased implementation |
| **Testing Complexity** | Concurrent testing, race conditions | Property-based testing |

### Potential Issues

```erlang
%% Issue 1: Context Loss
%% Current: Subtask can access partial parent state
%% SubAgent: Need explicit passing of all required context

%% Issue 2: Sequential Dependencies
%% Current: Graph structure guarantees execution order
%% SubAgent: Need additional coordination mechanism

%% Issue 3: Cancellation
%% Current: Directly stop graph execution
%% SubAgent: Need to send cancel signal to all child processes
```

---

## Recommended: Hybrid Approach

```
┌─────────────────────────────────────────────────────────────┐
│                    Hybrid Architecture                       │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   DeepAgent (Main Process)                                  │
│   ┌─────────────────────────────────────────────────────┐  │
│   │              Graph Engine (unchanged)                │  │
│   │                                                     │  │
│   │   Lightweight Tasks: Node-based (existing)          │  │
│   │   ┌─────┐   ┌─────┐   ┌─────────────┐              │  │
│   │   │ LLM │──▶│Tool │──▶│TaskExecutor │              │  │
│   │   │Node │   │Node │   │  (internal) │              │  │
│   │   └─────┘   └─────┘   └─────────────┘              │  │
│   │                                                     │  │
│   │   Heavyweight Tasks: SubAgent (new)                 │  │
│   │   ┌─────────────────────────────────────────────┐  │  │
│   │   │          SubAgent Dispatcher                │  │  │
│   │   │  • Assess task complexity                   │  │  │
│   │   │  • Simple tasks → internal execution        │  │  │
│   │   │  • Complex tasks → spawn SubAgent           │  │  │
│   │   └──────────────┬──────────────────────────────┘  │  │
│   └──────────────────│──────────────────────────────────┘  │
│                      │                                      │
│            ┌─────────▼─────────┐                           │
│            │    SubAgent Pool   │  ← Independent processes  │
│            │  (create/reuse)    │                           │
│            └───────────────────┘                           │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Hybrid Implementation

```erlang
%% Task dispatch decision
dispatch_task(TaskDef, State) ->
    case classify_task(TaskDef) of
        simple ->
            %% Lightweight: use existing node-based execution
            execute_in_graph(TaskDef, State);
        complex ->
            %% Complex: spawn SubAgent
            spawn_subagent(TaskDef, State);
        specialized ->
            %% Needs special config: use predefined Agent
            delegate_to_specialized_agent(TaskDef, State)
    end.

%% Task classification criteria
classify_task(#{depth := D}) when D >= 2 -> complex;
classify_task(#{requires_tools := Tools}) when length(Tools) > 5 -> complex;
classify_task(#{estimated_tokens := T}) when T > 10000 -> complex;
classify_task(#{requires_isolation := true}) -> specialized;
classify_task(_) -> simple.
```

---

## Implementation Phases

### Phase 1: Add SubAgent Interface (Low Risk)

Keep existing architecture, add new tool for external agent execution.

```erlang
%% New spawn_external_agent tool
spawn_external_agent_tool() -> #{
    name => <<"spawn_external_agent">>,
    description => <<"Spawn independent Agent for complex task">>,
    parameters => #{
        agent_config => #{type => object},
        task => #{type => string}
    },
    handler => fun handle_spawn_external/2
}.

%% Implementation: use beamai_agent:run_once/2
handle_spawn_external(Args, _State) ->
    Config = maps:get(<<"agent_config">>, Args),
    Task = maps:get(<<"task">>, Args),
    %% Synchronous call, wait for result
    case beamai_agent:run_once(Config, Task) of
        {ok, Result} -> #{status => success, result => Result};
        {error, Reason} -> #{status => error, reason => Reason}
    end.
```

**Benefits**:
- Minimal changes to core architecture
- Backward compatible
- Can validate SubAgent value

### Phase 2: Async SubAgent Support (Medium Risk)

```erlang
%% Async spawn SubAgent
-spec spawn_async_subagent(map(), map()) -> {ok, pid()} | {error, term()}.
spawn_async_subagent(Config, TaskDef) ->
    Parent = self(),
    spawn_link(fun() ->
        Result = beamai_agent:run_once(Config, TaskDef),
        Parent ! {subagent_result, self(), Result}
    end).

%% Collect results with timeout
collect_subagent_results(Pids, Timeout) ->
    collect_results(Pids, [], Timeout).

collect_results([], Acc, _Timeout) ->
    {ok, lists:reverse(Acc)};
collect_results([Pid | Rest], Acc, Timeout) ->
    receive
        {subagent_result, Pid, Result} ->
            collect_results(Rest, [Result | Acc], Timeout)
    after Timeout ->
        {error, {timeout, Pid}}
    end.
```

**Benefits**:
- True parallel execution
- Can set per-task timeouts
- Still relatively simple

### Phase 3: Full SubAgent Architecture (High Risk, Optional)

Only consider if Phase 1/2 cannot meet requirements.

```erlang
%% Full SubAgent supervisor
-module(beamai_subagent_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [#{
        id => subagent,
        start => {beamai_subagent, start_link, []},
        restart => temporary,
        type => worker
    }],
    {ok, {SupFlags, ChildSpecs}}.

%% SubAgent process
-module(beamai_subagent).
-behaviour(gen_server).

start_link(Config, TaskDef, Parent) ->
    gen_server:start_link(?MODULE, {Config, TaskDef, Parent}, []).

init({Config, TaskDef, Parent}) ->
    %% Create agent state
    {ok, AgentState} = beamai_agent:create_state(Config),
    self() ! execute,
    {ok, #{
        agent_state => AgentState,
        task => TaskDef,
        parent => Parent
    }}.

handle_info(execute, #{agent_state := AS, task := Task, parent := Parent} = State) ->
    Result = beamai_agent:run_with_state(AS, Task, #{}),
    Parent ! {subagent_complete, self(), Result},
    {stop, normal, State}.
```

---

## Comparison Summary

| Aspect | Node-based (Current) | SubAgent | Hybrid (Recommended) |
|--------|---------------------|----------|---------------------|
| **Change Level** | None | Large (~70%) | Small (~15%) |
| **Process Model** | Single process | Multi-process | Mixed |
| **Isolation** | State isolation | Process isolation | Selective |
| **Parallelism** | Logical | True concurrent | Both |
| **Configuration** | Shared | Independent | Selective |
| **Complexity** | Low | High | Medium |
| **Debugging** | Easy | Hard | Medium |
| **Suitable For** | Simple tasks | Complex/distributed | General use |

---

## Recommendation

**For current stage, recommend Hybrid Phase 1**:

1. **Small Change** - Only add new tool, no core modifications
2. **Backward Compatible** - Existing features unaffected
3. **Validate Value** - Can verify SubAgent value first
4. **Gradual Evolution** - Decide depth based on actual needs

**Full SubAgent architecture suitable for**:
- Need distributed Agent cluster
- Subtasks need completely independent environments
- Strong fault isolation requirements
- Long-running independent subtasks

---

## Files to Modify

### Phase 1 (Minimal)

```
apps/beamai_deepagent/src/tools/
└── beamai_deepagent_external_agent.erl  (new)

apps/beamai_deepagent/src/
└── beamai_deepagent.erl  (add tool registration)
```

### Phase 2 (Medium)

```
apps/beamai_deepagent/src/
├── beamai_deepagent.erl
├── core/
│   └── beamai_deepagent_subagent.erl  (new)
└── nodes/
    └── beamai_deepagent_aggregator_node.erl  (modify for async)
```

### Phase 3 (Full)

```
apps/beamai_deepagent/src/
├── beamai_deepagent.erl  (major changes)
├── beamai_subagent_sup.erl  (new)
├── beamai_subagent.erl  (new)
├── beamai_subagent_protocol.erl  (new)
├── core/
│   ├── beamai_deepagent_router.erl  (modify)
│   └── beamai_deepagent_subagent_manager.erl  (new)
└── nodes/
    ├── beamai_deepagent_task_node.erl  (rewrite)
    └── beamai_deepagent_aggregator_node.erl  (rewrite)
```

---

## Open Questions

1. How to handle shared resources (files, databases) between SubAgents?
2. Should SubAgents be able to spawn their own SubAgents (recursive)?
3. How to implement distributed SubAgents across Erlang nodes?
4. How to handle SubAgent failures gracefully?
5. Should we support A2A protocol for SubAgent communication?

---

---

# 中文版本

## 概述

本设计分析在 DeepAgent 中使用独立的 SubAgent（子 Agent 进程）作为任务执行者的可能性，替代当前基于节点的执行模型。

## 状态

- **状态**: 提议中
- **创建日期**: 2026-01-20
- **作者**: 设计讨论

---

## 当前架构（基于节点）

```
┌─────────────────────────────────────────────────────────────┐
│                    单进程 Graph 执行                         │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   DeepAgent 进程                                            │
│   ┌─────────────────────────────────────────────────────┐  │
│   │                   Graph Engine                       │  │
│   │  ┌─────┐   ┌─────┐   ┌─────────┐   ┌─────────────┐ │  │
│   │  │ LLM │──▶│Tool │──▶│ Fan-out │──▶│TaskExecutor │ │  │
│   │  │Node │   │Node │   │         │   │  (×N)       │ │  │
│   │  └─────┘   └─────┘   └─────────┘   └──────┬──────┘ │  │
│   │      ▲                                     │        │  │
│   │      │         ┌───────────────┐           │        │  │
│   │      └─────────│  Aggregator   │◀──────────┘        │  │
│   │                └───────────────┘                    │  │
│   │                                                     │  │
│   │   共享状态: graph_state (不可变)                     │  │
│   └─────────────────────────────────────────────────────┘  │
│                                                             │
│   特点:                                                     │
│   • 同一进程内执行                                          │
│   • 子任务通过 fan_out 并行（逻辑并行）                     │
│   • 状态通过不可变数据结构传递                              │
│   • 子任务创建简化的子图（只有 LLM node）                   │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 提议的架构（基于 SubAgent）

```
┌─────────────────────────────────────────────────────────────┐
│                    多进程 Agent 协作                         │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   Parent Agent (进程 A)                                     │
│   ┌─────────────────────────────────────────────────────┐  │
│   │  Planner LLM  →  决策: 分配任务给子 Agent             │  │
│   │       │                                              │  │
│   │       ▼                                              │  │
│   │  ┌─────────────────────────────────────────────┐    │  │
│   │  │           SubAgent Manager                   │    │  │
│   │  │  • 创建/监督子 Agent 进程                    │    │  │
│   │  │  • 任务分发                                  │    │  │
│   │  │  • 结果收集                                  │    │  │
│   │  └──────────┬──────────────┬───────────────────┘    │  │
│   └─────────────│──────────────│─────────────────────────┘  │
│                 │              │                            │
│        ┌────────▼────┐  ┌──────▼──────┐  ┌────────────┐    │
│        │ SubAgent 1  │  │ SubAgent 2  │  │ SubAgent N │    │
│        │ (进程 B)    │  │ (进程 C)    │  │ (进程 ...)  │    │
│        │             │  │             │  │            │    │
│        │ • 独立 LLM  │  │ • 独立 LLM  │  │ • 独立 LLM │    │
│        │ • 独立工具  │  │ • 独立工具  │  │ • 独立工具 │    │
│        │ • 独立状态  │  │ • 独立状态  │  │ • 独立状态 │    │
│        │ • 独立历史  │  │ • 独立历史  │  │ • 独立历史 │    │
│        └─────────────┘  └─────────────┘  └────────────┘    │
│                                                             │
│   通信: Erlang 消息 / A2A 协议                              │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 架构变更分析

### 需要修改的组件

| 组件 | 当前状态 | SubAgent 方案所需变更 | 变更量 |
|------|----------|----------------------|--------|
| `beamai_deepagent.erl` | 主入口 | 添加 SubAgent 管理逻辑 | **大** |
| `beamai_deepagent_task_node.erl` | 内部子图执行 | 改为启动独立 Agent 进程 | **重写** |
| `beamai_deepagent_aggregator_node.erl` | 同步收集结果 | 改为异步等待进程消息 | **大** |
| `beamai_deepagent_router.erl` | 节点路由 | 添加进程间路由逻辑 | **中** |
| **新增** `beamai_subagent_sup.erl` | - | Supervisor 监督子 Agent | **新增** |
| **新增** `beamai_subagent_protocol.erl` | - | 父子通信协议 | **新增** |
| `graph_send.erl` | fan_out 逻辑 | 可能需要修改以支持跨进程 | **中** |

**预估变更量**: 约 60-70% 的 deepagent 代码需要修改或重写

---

## 优势分析

| 优势 | 说明 | 适用场景 |
|------|------|----------|
| **真正的进程隔离** | 子 Agent 崩溃不影响父 Agent（Erlang "Let it crash"） | 高可靠性需求 |
| **真正的并发执行** | 多核并行，充分利用 BEAM 调度器 | CPU 密集型任务、多 LLM 调用 |
| **独立配置** | 每个子 Agent 可有不同 LLM、工具、系统提示 | 异构任务、专业化分工 |
| **可复用组件** | 子 Agent 可以是独立的、可复用的 Agent | 模块化设计、Agent 市场 |
| **分布式潜力** | 可扩展到多节点 Erlang 集群 | 大规模部署 |
| **资源隔离** | 每个进程有独立的内存空间 | 内存敏感任务 |
| **超时控制** | 可以对每个子 Agent 设置独立超时 | 不可靠任务 |
| **监控可观测** | 每个进程可独立监控 | 生产环境调试 |

---

## 劣势分析

| 劣势 | 说明 | 缓解措施 |
|------|------|----------|
| **架构复杂度** | 进程间通信、状态同步、故障处理 | 良好的抽象层、清晰的协议 |
| **调试困难** | 跨进程调试、分布式追踪 | 增强 trace 系统、统一日志 |
| **上下文传递** | 父子 Agent 间的上下文需要序列化传递 | 设计精简的上下文格式 |
| **性能开销** | 进程创建、消息传递、序列化 | 进程池复用、批量通信 |
| **一致性挑战** | 多个 Agent 可能对同一任务有不同理解 | 明确的任务规范、结果验证 |
| **实现周期** | 需要重写大量代码 | 分阶段实施 |
| **测试复杂** | 并发测试、竞态条件 | Property-based testing |

---

## 推荐方案：混合架构

```
┌─────────────────────────────────────────────────────────────┐
│                    混合架构                                  │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   DeepAgent (主进程)                                        │
│   ┌─────────────────────────────────────────────────────┐  │
│   │              Graph Engine (保持不变)                  │  │
│   │                                                     │  │
│   │   轻量任务: Node-based 执行 (现有方式)              │  │
│   │   ┌─────┐   ┌─────┐   ┌─────────────┐              │  │
│   │   │ LLM │──▶│Tool │──▶│TaskExecutor │              │  │
│   │   │Node │   │Node │   │  (内部)     │              │  │
│   │   └─────┘   └─────┘   └─────────────┘              │  │
│   │                                                     │  │
│   │   重量任务: SubAgent 执行 (新增)                    │  │
│   │   ┌─────────────────────────────────────────────┐  │  │
│   │   │          SubAgent Dispatcher                │  │  │
│   │   │  • 判断任务复杂度                           │  │  │
│   │   │  • 简单任务 → 内部执行                      │  │  │
│   │   │  • 复杂任务 → 启动 SubAgent                 │  │  │
│   │   └──────────────┬──────────────────────────────┘  │  │
│   └──────────────────│──────────────────────────────────┘  │
│                      │                                      │
│            ┌─────────▼─────────┐                           │
│            │    SubAgent Pool   │  ← 独立进程池             │
│            │  (按需创建/复用)    │                           │
│            └───────────────────┘                           │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 实施阶段

### 阶段 1: 添加 SubAgent 接口（低风险）

保持现有架构，添加新工具用于外部 Agent 执行。

```erlang
%% 新增 spawn_external_agent 工具
spawn_external_agent_tool() -> #{
    name => <<"spawn_external_agent">>,
    description => <<"启动独立 Agent 执行复杂任务">>,
    parameters => #{
        agent_config => #{type => object},
        task => #{type => string}
    },
    handler => fun handle_spawn_external/2
}.

%% 实现: 使用 beamai_agent:run_once/2
handle_spawn_external(Args, _State) ->
    Config = maps:get(<<"agent_config">>, Args),
    Task = maps:get(<<"task">>, Args),
    case beamai_agent:run_once(Config, Task) of
        {ok, Result} -> #{status => success, result => Result};
        {error, Reason} -> #{status => error, reason => Reason}
    end.
```

### 阶段 2: 异步 SubAgent 支持（中等风险）

```erlang
%% 异步启动子 Agent
spawn_async_subagent(Config, TaskDef) ->
    Parent = self(),
    spawn_link(fun() ->
        Result = beamai_agent:run_once(Config, TaskDef),
        Parent ! {subagent_result, self(), Result}
    end).
```

### 阶段 3: 完整 SubAgent 架构（高风险，可选）

只有在阶段 1/2 无法满足需求时才考虑。

---

## 对比总结

| 方面 | Node-based (当前) | SubAgent | 混合 (推荐) |
|------|------------------|----------|------------|
| **变更量** | 无 | 大 (~70%) | 小 (~15%) |
| **进程模型** | 单进程 | 多进程 | 混合 |
| **隔离性** | 状态隔离 | 进程隔离 | 可选 |
| **并行性** | 逻辑并行 | 真正并发 | 两者兼有 |
| **配置** | 共享 | 独立 | 可选 |
| **复杂度** | 低 | 高 | 中 |
| **调试** | 容易 | 困难 | 中等 |
| **适用场景** | 简单任务 | 复杂/分布式 | 通用 |

---

## 建议

**对于当前阶段，推荐采用混合方案的阶段 1**：

1. **变更量小** - 只需添加新工具，不修改核心架构
2. **向后兼容** - 现有功能不受影响
3. **验证价值** - 可以先验证 SubAgent 的实际价值
4. **渐进演进** - 根据实际需求决定是否深入

**完整的 SubAgent 架构适合以下场景**：
- 需要支持分布式 Agent 集群
- 子任务需要完全独立的环境（不同 LLM、工具）
- 对故障隔离有强需求
- 需要长时间运行的独立子任务

---

## 待解决问题

1. 如何处理 SubAgent 之间的共享资源（文件、数据库）？
2. SubAgent 是否可以生成自己的 SubAgent（递归）？
3. 如何实现跨 Erlang 节点的分布式 SubAgent？
4. 如何优雅地处理 SubAgent 失败？
5. 是否应该支持 A2A 协议用于 SubAgent 通信？

---

## 相关设计

- [双 LLM 架构设计](DUAL_LLM_DEEPAGENT.md) - 可与 SubAgent 架构结合使用
