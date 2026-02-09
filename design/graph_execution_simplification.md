# 图执行引擎简化设计：Poolboy 统一架构

## 背景

当前图执行引擎使用了 Pregel/BSP（批量同步并行）模型，该模型最初是为百万级顶点的分布式图计算（如 PageRank）而设计的。然而 BeamAI 的实际使用场景是 AI Agent 工作流图，通常只有 5-50 个节点，且每个节点的计算量很重（LLM 调用）。这种不匹配导致了不必要的复杂性。

## 问题分析

### 当前架构的复杂性

```
graph_runner
  -> pregel_master (gen_server, 全局协调者)
    -> pregel_partition (hash 分区)
      -> pregel_worker[1..N] (gen_server, 每个管理一个分区)
        -> dispatch 时: 额外从 poolboy dispatch_pool checkout
    -> pregel_barrier (等待 N 个 worker)
    -> pregel_superstep (合并逻辑)
  -> field_reducers 合并
```

### 模型不匹配

| Pregel 设计假设             | BeamAI 实际场景                  |
|-----------------------------|----------------------------------|
| 百万级顶点（PageRank）       | 5-50 个节点（AI Agent 工作流）   |
| 每个顶点计算极轻（加法）     | 每个节点计算极重（LLM 调用）     |
| 需要分区到多机分布式计算     | 在单个 Erlang VM 内运行          |
| 顶点间大量消息传递           | 数据沿边线性/扇出传递            |

**核心问题**：5 个节点被 hash 分区到 4 个 worker，每个 worker 管 1-2 个节点，然后用 barrier 同步——这就是过度复杂的根源。

## 核心洞察

图执行的本质是**边遍历**：

```
数据从 __start__ 出发
  -> 沿边流向下一个顶点
  -> 顶点计算，产生 delta
  -> 解析出边，确定下一步激活哪些顶点
  -> 单出度：串行执行
  -> 多出度（含 dispatch fan-out）：并行执行
  -> 并行结果通过 barrier 同步
  -> field_reducers 合并 delta
  -> 更新 global_state
  -> 进入下一轮
  -> 到达 __end__ 或无激活顶点 -> 结束
```

这是一个**"边遍历 + 扇出并行 + barrier 同步"**的模式，完全不需要 master/worker 分离和分区。

## 新架构设计

### 统一执行流程

```
graph_runner（单协调进程）
  |
  +- 维护 global_state
  +- 维护 field_reducers
  +- 维护图拓扑（edges, nodes）
  |
  |  每一轮（superstep）:
  |
  +- 1. 获取当前 activations（待执行的顶点列表）
  |
  +- 2. 判断并行度
  |     +- len(activations) == 1 -> 直接在协调进程内执行（零开销）
  |     +- len(activations) > 1  -> 提交到 poolboy 并行执行
  |
  +- 3. Barrier：收集所有结果（并行时等待所有 poolboy 任务完成）
  |
  +- 4. 合并：field_reducers 将所有 delta 合并到 global_state
  |
  +- 5. 路由：解析每个已完成顶点的 routing_edges
  |     +- direct edge -> 目标直接加入下一轮 activations
  |     +- conditional edge -> 调用 router_fn(state) 决定目标
  |     +- fan-out edge -> 多个目标加入 activations
  |     +- dispatch -> 带 vertex_input 的目标加入 activations
  |
  +- 6. 检查终止条件
  |     +- activations 为空 -> 结束
  |     +- 到达 __end__ -> 结束
  |     +- 超过 max_supersteps -> 结束
  |
  +- 7. 循环到步骤 1
```

### 与当前架构的对比

| 维度         | 当前（Pregel）                          | 新方案（Poolboy 统一）                  |
|--------------|----------------------------------------|----------------------------------------|
| 协调进程     | master (gen_server)                     | graph_runner 单进程                     |
| 计算进程     | N 个 worker (gen_server) + dispatch pool | 统一 poolboy                           |
| 分区         | hash 分区到 worker                      | 不需要分区                              |
| Barrier      | pregel_barrier 记录 N 个 worker         | 简单的 receive 收集结果循环             |
| 状态广播     | master -> 所有 worker cast              | 不需要广播（单进程持有状态）             |
| Dispatch 并行 | 额外的 poolboy                         | 同一个 poolboy                          |
| 串行节点     | 仍经过 worker                           | 直接在协调进程内执行                     |
| 代码复杂度   | 7+ 模块                                | 2-3 模块                                |

## 组件处置方案

### 需要移除的组件

| 组件               | 原因                                  |
|--------------------|---------------------------------------|
| `pregel_master`    | 协调逻辑归入 graph_runner             |
| `pregel_worker`    | 被 poolboy worker 替代                |
| `pregel_barrier`   | 简化为 receive N 个结果的循环         |
| `pregel_partition` | 无需分区                              |

### 需要简化/复用的组件

| 组件                       | 处理方式                                                        |
|----------------------------|----------------------------------------------------------------|
| `pregel_dispatch_worker`   | 重命名为通用 `graph_worker`，同时处理普通计算和 dispatch 计算    |
| `pregel_superstep`         | 简化：保留合并逻辑，去掉 worker 分组                            |
| `pregel_vertex`            | 简化：保留 flattened 模式，去掉 halted 投票机制                  |

### 需要保留的组件

| 组件                   | 原因                                   |
|------------------------|----------------------------------------|
| `graph_state`          | 不可变状态管理，设计良好                |
| `graph_state_reducer`  | delta 合并机制，核心能力                |
| `graph_dispatch`       | fan-out API，简洁好用                   |
| `graph_edge`           | 边路由逻辑（direct/conditional/fanout） |
| `graph_compute`        | compute_fn 工厂，可简化但保留           |
| 快照机制               | 错误恢复和人工介入                      |

## 设计要点

### 1. 同一顶点的多次 Dispatch 执行

当 dispatch 产生对同一个节点的多次调用时（如 `fan_out(worker, Items)`）：

```
activations = [
  {worker, #{item => 1}},
  {worker, #{item => 2}},
  {worker, #{item => 3}}
]
-> 3 个 poolboy 任务并行执行同一个 worker 节点的 compute_fn
-> 各自产生 delta
-> field_reducers 合并所有 delta
```

同一顶点的 dispatch 执行和不同顶点的并行执行处理方式完全一致：提交 poolboy、收集 delta、用 reducers 合并。**统一是自然的。**

### 2. 错误处理与延迟 Delta

延迟 delta 机制（出错时不应用 delta，等待重试）可以直接保留在协调进程中，不依赖 master/worker 分离。

### 3. Halted/Activation 语义变化

Pregel 的 halted 投票机制（顶点执行后标记为 halted，除非被再次激活）在工作流图中是多余的。工作流的语义是：**执行完一个节点后，沿边传递到下一个节点**，不存在"顶点自发苏醒"的场景。activation 列表完全可以替代 halted 机制。

### 4. Poolboy 大小配置

AI Agent 节点主要是 I/O bound（等待 LLM 响应），所以 poolboy 的 size 可以设大一些：

```erlang
{size, 8},
{max_overflow, 32}
```

### 5. 单节点优化

当只有一个顶点需要执行时，绕过 poolboy 直接在协调进程内执行——串行路径零开销。

## 预期收益

1. **模型匹配**：Poolboy + 边遍历更自然地匹配工作流图（对比 Pregel 为大规模图设计）
2. **代码量减少 60%+**：移除 master、worker、partition、复杂 barrier，核心执行逻辑从 7+ 模块变成 2-3 模块
3. **性能提升**：去掉不必要的进程间通信（global_state 广播、worker 启动）和 hash 分区开销
4. **统一并行机制**：单一 pool 同时处理普通并行执行和 dispatch fan-out，不再有两套并行机制
5. **保留所有核心能力**：field_reducers、快照、错误恢复、条件路由、动态 dispatch
