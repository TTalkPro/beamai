# 已完成任务

## P0: pregel_worker 错误处理重构 ✅

**完成日期**: 2026-01-21

### 任务概述

为 `pregel_worker` 增加错误处理能力，使其能够识别顶点计算的成功/失败状态，并将失败信息上报给 Master。

**设计原则**：pregel_worker 不做异常处理和重试，只约定成功/失败的数据结构契约，根据结果决定下一步动作。异常处理和重试逻辑由 graph_compute 层负责。

---

### 阶段 1：pregel_worker 修改 ✅

#### 1.1 定义数据结构契约 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_worker.erl`

- [x] 添加类型定义

```erlang
%% 计算结果状态
-type compute_status() :: ok | {error, term()}.

%% 计算结果（计算函数必须返回此结构）
-type compute_result() :: #{
    vertex := vertex(),
    outbox := [{vertex_id(), term()}],
    status := compute_status()
}.
```

- [x] 导出类型

```erlang
-export_type([compute_result/0, compute_status/0]).
```

#### 1.2 修改 compute_vertices 函数 ✅

- [x] 修改函数签名，返回值增加 FailedVertices
- [x] 修改 fold 初始累加器为 `{AllVertices, [], []}`
- [x] 添加 status 检查逻辑
- [x] 新增 `process_compute_result/3` 辅助函数

#### 1.3 修改 execute_superstep 函数 ✅

- [x] 接收 compute_vertices 的三元组返回值
- [x] 传递 FailedVertices 给 notify_master_done

#### 1.4 修改 notify_master_done 函数 ✅

- [x] 修改函数签名，增加 FailedVertices 参数
- [x] 添加 failed_count 和 failed_vertices 到 Result

---

### 阶段 2：graph_compute 修改 ✅

#### 2.1 修改 compute_fn 返回值 ✅

- [x] 成功时添加 `status => ok`
- [x] 失败时抛出异常，由 try-catch 捕获

#### 2.2 添加异常处理包装 ✅

- [x] 在 compute_fn 中添加 try-catch
- [x] 新增 `execute_node/1` 内部函数
- [x] 新增 `make_error_result/2` 构造错误结果

---

### 验收结果 ✅

#### 功能验收

- [x] 顶点计算成功时，正常更新顶点和发送消息
- [x] 顶点计算失败时，不更新顶点，不发送消息
- [x] Worker 正确上报 failed_count 和 failed_vertices

#### 代码验收

- [x] 新增类型定义并导出
- [x] 函数签名更新
- [x] 单元测试通过
- [x] status 字段为必需（无向后兼容）

#### 测试用例

- [x] 所有顶点成功的场景
- [x] 部分顶点失败的场景
- [x] 所有顶点失败的场景

---

### 修改的文件

| 文件 | 修改类型 |
|------|----------|
| `apps/beamai_core/src/graph/pregel/pregel_worker.erl` | 类型定义、函数修改 |
| `apps/beamai_core/src/graph/pregel/graph_compute.erl` | 异常处理包装 |

### 新增的测试文件

| 文件 | 测试数量 |
|------|----------|
| `apps/beamai_core/test/pregel_worker_tests.erl` | 9 tests |
| `apps/beamai_core/test/graph_compute_tests.erl` | 4 tests |

---

### 相关文档

- `info/pregel_worker_error_handling_design.md` - 详细设计文档
- `info/pregel_error_reporting.md` - 错误上报机制分析
- `info/pregel_superstep_state_structure.md` - 备选方案参考

---

## P0: pregel_worker 中断支持 ✅

**完成日期**: 2026-01-21

### 任务概述

为 `pregel_worker` 增加中断（interrupt）支持，用于 human-in-the-loop 场景。顶点计算可以返回 `{interrupt, Reason}` 状态，表示需要暂停等待人工输入。

**设计原则**：
- 中断与错误分开处理，各自有独立的收集和上报机制
- 中断时不更新顶点状态，不发送消息
- 使用四元组累加器保持高内聚

---

### 修改内容 ✅

#### 类型定义扩展

```erlang
%% 计算结果状态（新增 interrupt）
-type compute_status() :: ok | {error, term()} | {interrupt, term()}.

%% 计算结果累加器（内部使用）
-type compute_acc() :: {
    Vertices :: #{vertex_id() => vertex()},
    Outbox :: [{vertex_id(), term()}],
    FailedVertices :: [{vertex_id(), term()}],
    InterruptedVertices :: [{vertex_id(), term()}]
}.
```

#### process_compute_result 新增 interrupt 处理

```erlang
process_compute_result(Id, #{status := {interrupt, Reason}},
                       {VAcc, OAcc, FailedAcc, InterruptedAcc}) ->
    %% 中断：记录中断信息，不更新顶点，不收集消息
    {VAcc, OAcc, FailedAcc, [{Id, Reason} | InterruptedAcc]}.
```

#### notify_master_done 上报中断信息

```erlang
Result = #{
    worker_id => WorkerId,
    active_count => ...,
    message_count => ...,
    failed_count => length(FailedVertices),
    failed_vertices => FailedVertices,
    interrupted_count => length(InterruptedVertices),
    interrupted_vertices => InterruptedVertices
}.
```

---

### 验收结果 ✅

#### 功能验收

- [x] 顶点计算返回 `{interrupt, Reason}` 时，记录到中断列表
- [x] 中断顶点不更新状态，不发送消息
- [x] Worker 正确上报 interrupted_count 和 interrupted_vertices
- [x] 支持混合状态（ok/error/interrupt 同时存在）

#### 代码验收

- [x] 类型定义扩展并导出
- [x] 函数式设计，使用模式匹配处理不同状态
- [x] 中文注释清晰准确
- [x] 单元测试通过 (48 tests, 0 failures)

#### 新增测试用例

- [x] compute_result_interrupt_structure_test - 中断结构验证
- [x] compute_vertices_all_interrupt_test - 所有顶点中断
- [x] compute_vertices_mixed_status_test - 混合状态测试
- [x] worker_done_with_interrupts_test - Worker 上报中断
- [x] worker_done_mixed_failures_and_interrupts_test - 混合失败和中断

---

### 修改的文件

| 文件 | 修改类型 |
|------|----------|
| `apps/beamai_core/src/graph/pregel/pregel_worker.erl` | 类型扩展、函数修改 |
| `apps/beamai_core/test/pregel_worker_tests.erl` | 新增 5 个测试用例 |

---

### 相关文档

- `info/pregel_callback_checkpoint_analysis.md` - 回调与 Checkpoint 分析（含 interrupt 分析）

---

## P0: pregel_barrier 汇总失败和中断信息 ✅

**完成日期**: 2026-01-21

### 任务概述

修改 `pregel_barrier` 模块，使其能够汇总所有 Worker 的失败和中断信息。

---

### 修改内容 ✅

#### 新增类型定义

```erlang
%% 超步结果汇总类型
-type superstep_results() :: #{
    active_count := non_neg_integer(),
    message_count := non_neg_integer(),
    failed_count := non_neg_integer(),
    failed_vertices := [{term(), term()}],
    interrupted_count := non_neg_integer(),
    interrupted_vertices := [{term(), term()}]
}.
```

#### 修改 aggregate_results 返回 map 格式

```erlang
-spec aggregate_results([map()]) -> superstep_results().
aggregate_results(Results) ->
    InitAcc = empty_results(),
    lists:foldl(fun merge_worker_result/2, InitAcc, Results).
```

---

### 验收结果 ✅

- [x] `aggregate_results/1` 返回 map 格式
- [x] 正确汇总 failed_count 和 failed_vertices
- [x] 正确汇总 interrupted_count 和 interrupted_vertices
- [x] 支持缺少字段时使用默认值（向后兼容）
- [x] 单元测试通过 (9 tests)

---

### 修改的文件

| 文件 | 修改类型 |
|------|----------|
| `apps/beamai_core/src/graph/pregel/pregel_barrier.erl` | 类型定义、函数修改 |
| `apps/beamai_core/src/graph/pregel/pregel_master.erl` | 适配新接口 |
| `apps/beamai_core/test/pregel_barrier_tests.erl` | 新增测试文件 |

---

## P0: pregel_master 回调机制 ✅

**完成日期**: 2026-01-21

### 任务概述

为 `pregel_master` 实现统一的回调机制，支持 checkpoint 保存、失败处理和 human-in-the-loop。

**设计原则**：Pregel 层只负责调用回调和执行返回的决策，不包含业务逻辑。

---

### 修改内容 ✅

#### 4.1 新增类型定义

```erlang
%% 回调类型定义
-type callback_type() :: initial | step | final.

%% Checkpoint 数据（按需获取）
-type checkpoint_data() :: #{
    superstep := non_neg_integer(),
    vertices := #{vertex_id() => vertex()},
    pending_messages := [{vertex_id(), term()}]
}.

%% 超步完成回调信息
-type superstep_complete_info() :: #{
    type := callback_type(),
    superstep := non_neg_integer(),
    active_count := non_neg_integer(),
    message_count := non_neg_integer(),
    failed_count := non_neg_integer(),
    failed_vertices := [{vertex_id(), term()}],
    interrupted_count := non_neg_integer(),
    interrupted_vertices := [{vertex_id(), term()}],
    get_checkpoint_data := fun(() -> checkpoint_data())
}.

%% 回调返回值
-type superstep_complete_result() :: continue | {stop, term()}.
```

#### 4.2 实现 checkpoint 数据收集

```erlang
%% 创建按需获取 checkpoint 数据的函数
make_get_checkpoint_data(Superstep, Workers, PendingMessages) ->
    fun() ->
        Vertices = collect_vertices_from_workers(Workers),
        Messages = collect_pending_messages_list(PendingMessages),
        #{
            superstep => Superstep,
            vertices => Vertices,
            pending_messages => Messages
        }
    end.
```

#### 4.3 修改回调调用逻辑

- `handle_call(start_execution)` - 启动后调用 initial 回调
- `complete_superstep/1` - 调用 step/final 回调
- `call_superstep_complete/3` - 统一的回调调用
- `determine_callback_type/2` - 判断回调类型

---

### 验收结果 ✅

#### 功能验收

- [x] initial 回调在执行前被调用
- [x] step 回调在超步完成后被调用
- [x] final 回调在终止时被调用
- [x] 回调信息包含所有必要字段
- [x] get_checkpoint_data 返回正确的数据
- [x] 回调返回 continue 时继续执行
- [x] 回调返回 {stop, Reason} 时停止执行
- [x] 回调收到 failed_vertices 和 interrupted_vertices

#### 代码验收

- [x] 类型定义清晰并导出
- [x] 函数命名适当，层级不超过 3 层
- [x] 函数式设计模式
- [x] 中文注释清晰准确
- [x] 单元测试通过 (11 tests)

---

### 修改的文件

| 文件 | 修改类型 |
|------|----------|
| `apps/beamai_core/src/graph/pregel/pregel_master.erl` | 类型定义、回调机制 |
| `apps/beamai_core/test/pregel_master_callback_tests.erl` | 新增测试文件 |

---

### 测试覆盖

| 测试文件 | 测试数量 |
|----------|----------|
| `pregel_worker_tests.erl` | 13 tests |
| `pregel_barrier_tests.erl` | 9 tests |
| `pregel_master_callback_tests.erl` | 11 tests |
| `graph_compute_tests.erl` | 4 tests |
| **总计** | **37 tests** |

---

## P0: 移除 pending_messages，实现 BSP 集中路由 ✅

**完成日期**: 2026-01-21

### 任务概述

根据 `info/pregel_message_reliability_analysis.md` 的分析，移除不合理的 `pending_messages` 机制，改用 BSP 模型的集中路由方式。

**问题分析**：
- `pending_messages` 职责模糊：既处理 Worker 不存在的情况，又处理消息路由
- 与 BSP 模型冲突：BSP 模型中消息应在超步结束时统一投递，而非实时发送
- 可靠性问题：Worker 使用 cast 实时发送消息，跨机器时可能丢失

**解决方案**：
- Worker 不再实时发送消息，改为上报 outbox 给 Master
- Master 在超步完成后统一路由所有消息
- 移除 Master 的 pending_messages 机制

---

### 修改内容 ✅

#### pregel_worker.erl

- [x] 移除 `route_messages/2` 函数（不再实时发送消息）
- [x] 移除 `group_by_target_worker/2` 函数
- [x] 移除 `send_to_worker/5` 函数
- [x] 修改 `execute_superstep/1`，不再调用 route_messages
- [x] 修改 `notify_master_done/5`，上报 outbox 给 Master

```erlang
%% notify_master_done 新增 outbox 上报
Result = #{
    worker_id => WorkerId,
    active_count => ...,
    message_count => length(Outbox),
    outbox => Outbox,  %% 新增：上报 outbox 给 Master
    failed_count => ...,
    failed_vertices => ...,
    interrupted_count => ...,
    interrupted_vertices => ...
}.
```

#### pregel_barrier.erl

- [x] 扩展 `superstep_results()` 类型，增加 `outbox` 字段
- [x] 修改 `empty_results/0`，初始化空 outbox
- [x] 修改 `merge_worker_result/2`，汇总所有 Worker 的 outbox

```erlang
%% 扩展后的超步结果类型
-type superstep_results() :: #{
    active_count := non_neg_integer(),
    message_count := non_neg_integer(),
    outbox := [{term(), term()}],  %% 新增：汇总所有 Worker 的 outbox
    failed_count := non_neg_integer(),
    failed_vertices := [{term(), term()}],
    interrupted_count := non_neg_integer(),
    interrupted_vertices := [{term(), term()}]
}.
```

#### pregel_master.erl

- [x] 从 `#state` 记录中移除 `pending_messages` 字段
- [x] 移除 `handle_route_messages/3` 函数
- [x] 移除 `collect_pending_messages_list/1` 函数
- [x] 新增 `route_all_messages/3` 函数（集中路由）
- [x] 新增 `group_messages_by_worker/2` 函数
- [x] 修改 `complete_superstep/1`，从汇总结果获取 outbox 并路由
- [x] 修改 `make_get_checkpoint_data/3`，使用 outbox 替代 pending_messages

```erlang
%% 新增：统一路由所有消息
-spec route_all_messages([{term(), term()}],
                          #{non_neg_integer() => pid()},
                          pos_integer()) -> ok.
route_all_messages([], _Workers, _NumWorkers) -> ok;
route_all_messages(Messages, Workers, NumWorkers) ->
    GroupedMessages = group_messages_by_worker(Messages, NumWorkers),
    maps:foreach(
        fun(TargetWorkerId, Msgs) ->
            case maps:get(TargetWorkerId, Workers, undefined) of
                undefined -> ok;
                Pid -> pregel_worker:receive_messages(Pid, Msgs)
            end
        end,
        GroupedMessages
    ).
```

---

### 新架构

```
┌─────────────────────────────────────────────────────────────────┐
│                    BSP 消息路由架构                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  超步执行阶段:                                                   │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Worker 执行顶点计算                                     │   │
│  │  • 发出的消息保存在 Worker 的 outbox                     │   │
│  │  • 不实时发送消息（BSP 模型）                            │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                              ▼                                  │
│  超步完成阶段:                                                   │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Worker 上报 Master                                      │   │
│  │  • 汇总结果（active_count, failed_vertices 等）          │   │
│  │  • outbox 内容（待路由的消息）                           │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                              ▼                                  │
│  消息路由阶段:                                                   │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Master 统一路由                                         │   │
│  │  • 收集所有 Worker 的 outbox（通过 barrier 汇总）        │   │
│  │  • 按目标 Worker 分组                                    │   │
│  │  • 投递到各 Worker 的 inbox                             │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

### 验收结果 ✅

#### 功能验收

- [x] Worker 不再实时发送消息
- [x] Worker 正确上报 outbox 给 Master
- [x] Barrier 正确汇总所有 Worker 的 outbox
- [x] Master 在超步完成后统一路由消息
- [x] 消息在下一超步开始前送达目标 Worker
- [x] 链式消息传递（v1 → v2 → v3）正常工作

#### 代码验收

- [x] 移除未使用的函数（route_messages, group_by_target_worker, send_to_worker）
- [x] 移除 pending_messages 相关代码
- [x] 新增集中路由函数
- [x] 中文注释清晰准确
- [x] 单元测试通过 (45 tests)

---

### 新增测试文件

**pregel_message_routing_tests.erl** - 8 tests:

| 测试名称 | 描述 |
|----------|------|
| `master_routes_messages_test` | Master 正确路由消息到目标 Worker |
| `multi_worker_message_routing_test` | 多 Worker 场景下消息正确路由 |
| `chain_message_routing_test` | 链式消息传递（v1 → v2 → v3） |
| `worker_reports_outbox_test` | Worker 上报的结果包含 outbox |
| `no_pending_messages_dependency_test` | 执行过程不依赖 pending_messages |
| `messages_delivered_at_superstep_end_test` | 消息在超步结束时统一投递 |
| `empty_outbox_test` | 空 outbox 不影响执行 |
| `all_vertices_send_messages_test` | 所有顶点同时发送消息 |

---

### 修改的文件

| 文件 | 修改类型 |
|------|----------|
| `apps/beamai_core/src/graph/pregel/pregel_worker.erl` | 移除实时路由，上报 outbox |
| `apps/beamai_core/src/graph/pregel/pregel_barrier.erl` | 汇总 outbox |
| `apps/beamai_core/src/graph/pregel/pregel_master.erl` | 移除 pending_messages，集中路由 |
| `apps/beamai_core/test/pregel_message_routing_tests.erl` | 新增测试文件 |

---

### 测试覆盖（更新后）

| 测试文件 | 测试数量 |
|----------|----------|
| `pregel_worker_tests.erl` | 13 tests |
| `pregel_barrier_tests.erl` | 9 tests |
| `pregel_master_callback_tests.erl` | 11 tests |
| `pregel_message_routing_tests.erl` | 8 tests |
| `graph_compute_tests.erl` | 4 tests |
| **总计** | **45 tests** |

---

### 相关文档

- `info/pregel_message_reliability_analysis.md` - 消息可靠性分析
- `info/pregel_outbox_vs_pending_writes.md` - Outbox 与 pending_writes 对比

---

## P0: Graph 层全局状态模式重构 ✅

**完成日期**: 2026-01-22

### 任务概述

将 graph 层从"消息传递模式"重构为"全局状态 + Delta 增量更新模式"。

**核心变更**：
- Master 持有全局状态（global_state），Worker 只负责计算
- 节点发送 Delta（`#{field => value}`），而不是完整状态
- 使用 field_reducers 按字段合并 Delta
- 延迟提交：出错时暂存 Delta，不 apply
- 简化 vertex：移除 value，只保留 id 和 edges

---

### Phase 1: Pregel 层核心重构 ✅

#### 1.1 重构 pregel_master.erl ✅

- [x] 添加 `global_state`、`field_reducers`、`pending_deltas` 字段
- [x] 实现 `apply_deltas/3`：`lists:foldl(apply_delta, GlobalState, Deltas)`
- [x] 实现 `broadcast_global_state/2`：广播全局状态给所有 Worker
- [x] 添加 `get_global_state/1` API
- [x] 处理 binary/atom 键标准化

```erlang
%% Delta 合并核心逻辑
apply_deltas(State, Deltas, FieldReducers) ->
    lists:foldl(fun(Delta, AccState) ->
        apply_delta(Delta, AccState, FieldReducers)
    end, State, Deltas).
```

#### 1.2 重构 pregel_worker.erl ✅

- [x] 接收 global_state（通过 cast 或 init）
- [x] 计算函数返回 `#{delta => Map, outbox => List, status => ok}`
- [x] 上报 deltas 列表给 Master
- [x] 顶点计算完成后自动 halt

#### 1.3 简化 pregel_vertex.erl ✅

- [x] 移除 `value` 字段
- [x] 类型简化为纯拓扑结构：`#{id, edges, halted}`

#### 1.4 更新 pregel_barrier.erl ✅

- [x] 汇总所有 Worker 的 deltas

---

### Phase 2: Graph 层适配 ✅

- [x] `graph_runner.erl`：构建 global_state 和 field_reducers
- [x] `graph_builder.erl`：创建纯拓扑顶点（无 value）
- [x] `graph_compute.erl`：适配新模型，返回 delta 格式

---

### Phase 3: beamai_agent 层迁移 ✅

- [x] `beamai_agent_runner.erl`：构建初始 global_state
- [x] `beamai_nodes.erl`：返回 delta 而非完整状态

---

### Phase 4: 测试更新 ✅

- [x] 更新所有测试文件适配新 API
- [x] 添加 `pregel:get_result_global_state/1` 实现

---

### Phase 5: 清理 ✅

- [x] 更新 `pregel.erl` 文档注释
- [x] 更新 `examples/src/example_pregel.erl` 示例
- [x] 更新 `info/pregel_graph_compute_fn.md` 文档
- [x] 保留向后兼容的废弃函数

---

### 架构说明

#### Delta 格式

```erlang
%% Delta 是简单的 field => value 映射
-type delta() :: #{atom() | binary() => term()}.

%% 示例
#{
    messages => [NewMessage],      %% append_reducer 处理
    context => #{key => value},    %% merge_reducer 处理
    last_response => Response      %% last_write_win 处理
}
```

#### Field Reducers

```erlang
%% 配置（键使用 binary 格式，因为 graph_state 内部标准化为 binary）
#{
    <<"messages">> => fun graph_state_reducer:append_reducer/2,
    <<"context">> => fun graph_state_reducer:merge_reducer/2
    %% 未配置的字段默认 last_write_win
}
```

#### 全局状态同步模式

当前使用 **Push 模式**：
```
Master                          Worker
  │  1. apply_deltas()            │
  │     NewGlobalState            │
  │  2. cast {global_state, GS}   │
  │ ─────────────────────────────>│
  │  3. cast {start_superstep, N} │
  │ ─────────────────────────────>│
```

可选的 **Pull 模式**（已验证可行，不会死锁）：
```
Master                          Worker
  │  1. apply_deltas()            │
  │     NewGlobalState (保留)      │
  │  2. cast {start_superstep, N} │
  │ ─────────────────────────────>│
  │  3. call get_global_state     │
  │ <─────────────────────────────│
  │ ─────────────────────────────>│
```

---

### 验收结果 ✅

#### 功能验收

- [x] global_state 正确初始化和传递
- [x] Delta 正确合并（使用 field_reducers）
- [x] 顶点简化为纯拓扑结构
- [x] 向后兼容的废弃函数工作正常

#### 测试覆盖

| 测试文件 | 测试数量 |
|----------|----------|
| `pregel_master_tests.erl` | 9 tests |
| `pregel_worker_tests.erl` | 13 tests |
| `pregel_barrier_tests.erl` | 9 tests |
| `graph_compute_tests.erl` | 8 tests |
| **总计** | **39 tests, 0 failures** |

---

### 修改的文件

| 文件 | 修改类型 |
|------|----------|
| `pregel_master.erl` | 添加 global_state、field_reducers、delta 合并逻辑 |
| `pregel_worker.erl` | 接收 global_state、返回 delta |
| `pregel_vertex.erl` | 简化为纯拓扑结构 |
| `pregel_barrier.erl` | 汇总 deltas |
| `pregel.erl` | 添加 get_result_global_state/1 API |
| `graph_runner.erl` | 构建 global_state 和 config |
| `graph_compute.erl` | 适配新模型 |
| `graph_builder.erl` | 创建纯拓扑顶点 |
| `example_pregel.erl` | 更新示例代码 |
| `pregel_graph_compute_fn.md` | 更新文档 |

---

### 相关文档

- `info/graph_global_state_redesign.md` - 设计文档
- `info/pregel_graph_compute_fn.md` - 计算函数文档（已更新）
- `TASK.md` - 任务跟踪（已标记完成）
