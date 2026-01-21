# Pregel 错误处理与 Checkpoint 支持任务

## 任务概述

为 Pregel 设计统一的回调机制，支持：
1. Graph 层保存 checkpoint
2. Graph 层处理失败并决策是否继续
3. Human-in-the-loop（中断与恢复）

**设计原则**: Pregel 层只负责调用回调和执行返回的决策，不包含业务逻辑。

---

## P0: 回调函数设计与实现 ✅

**优先级**: P0 (必须)

**前置条件**: Worker 错误上报已完成

### 设计概述

```
  超级步编号:  0(initial)    1          2          3(final)
              │            │          │          │
  回调时机:   执行前      超步0后    超步1后     结束时
              │            │          │          │
  type:     initial      step       step       final

  注意：error 和 interrupt 不是 type，而是通过独立字段报告
```

### 统一回调设计

```erlang
%% opts 中的回调配置
-type opts() :: #{
    ...
    on_superstep_complete => fun((superstep_complete_info()) -> superstep_complete_result())
}.

%% 回调信息
-type superstep_complete_info() :: #{
    type := initial | step | final,
    superstep := non_neg_integer(),
    active_count := non_neg_integer(),
    message_count := non_neg_integer(),
    %% 错误信息
    failed_count := non_neg_integer(),
    failed_vertices := [{vertex_id(), term()}],
    %% 中断信息（human-in-the-loop）
    interrupted_count := non_neg_integer(),
    interrupted_vertices := [{vertex_id(), term()}],
    %% Checkpoint 支持
    get_checkpoint_data := fun(() -> checkpoint_data())
}.

%% Checkpoint 数据（按需获取）
-type checkpoint_data() :: #{
    superstep := non_neg_integer(),
    vertices := #{vertex_id() => vertex()},
    pending_messages := [{vertex_id(), term()}]  %% 来自汇总的 outbox
}.

%% 回调返回值
-type superstep_complete_result() :: continue | {stop, term()}.
```

---

### 4.0 扩展 pregel_worker 支持 interrupt ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_worker.erl`

**目的**: 支持 human-in-the-loop 场景，顶点计算可返回 interrupt 状态

- [x] 修改 `compute_status()` 类型，增加 `{interrupt, term()}`
- [x] 修改 `process_compute_result/3`，处理 interrupt 状态
- [x] 修改 `execute_superstep/1`，收集 interrupted_vertices
- [x] 修改 `notify_master_done/5`，上报 interrupted_count 和 interrupted_vertices

---

### 4.1 定义回调类型 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [x] 定义 `superstep_complete_info()` 类型（含 interrupted 字段）
- [x] 定义 `checkpoint_data()` 类型
- [x] 定义 `superstep_complete_result()` 类型
- [x] 导出类型

---

### 4.2 实现 get_checkpoint_data 函数 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [x] 实现 `collect_vertices_from_workers/1` - 从 Workers 收集顶点状态
- [x] 实现 `make_get_checkpoint_data/3` - 创建按需获取函数

---

### 4.3 修改 pregel_master 调用回调 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [x] 添加 `on_superstep_complete` 到 #state
- [x] 修改 `handle_call(start_execution)` - 启动后调用 initial 回调
- [x] 修改 `complete_superstep/1` - 调用 step/final 回调
- [x] 实现 `call_superstep_complete/3` - 统一的回调调用
- [x] 实现 `determine_callback_type/2` - 判断回调类型
- [x] 实现 `build_superstep_complete_info/3` - 构建回调信息
- [x] 实现 `empty_superstep_results/0` - 创建空结果（用于 initial 回调）

---

### 4.4 修改 pregel_barrier 汇总失败和中断信息 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_barrier.erl`

- [x] 修改 `aggregate_results/1` 返回 map 格式
- [x] 添加 failed_count 和 failed_vertices 汇总
- [x] 添加 interrupted_count 和 interrupted_vertices 汇总
- [x] 添加 outbox 汇总（用于 Master 集中路由）

---

### 4.5 添加测试 ✅

- [x] `pregel_worker_tests.erl` - 13 tests
- [x] `pregel_barrier_tests.erl` - 9 tests
- [x] `pregel_master_callback_tests.erl` - 11 tests
- [x] `graph_compute_tests.erl` - 4 tests

---

### 4.6 移除 pending_messages，实现 BSP 集中路由 ✅

详见 `TASK_DONE.md`

---

## 职责分离

```
┌─────────────────────────────────────────────────────────────────┐
│                    分层职责                                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Graph 层                                                       │
│  ─────────                                                      │
│  • 实现 on_superstep_complete 回调                              │
│  • 决定是否保存 checkpoint                                       │
│  • 决定失败/中断时是否继续                                       │
│  • 处理 human-in-the-loop（中断恢复）                            │
│  • 返回 continue | {stop, Reason}                               │
│                                                                 │
│  ─────────────────────────────────────────────────────────────  │
│                                                                 │
│  Pregel 层 (pregel_master)                                      │
│  ─────────────────────────                                      │
│  • 在合适时机调用回调（initial/step/final）                      │
│  • 提供 get_checkpoint_data 函数                                │
│  • 汇总并传递 failed/interrupted 信息                           │
│  • 集中路由所有消息（从 Worker outbox 汇总后路由）              │
│  • 根据回调返回值执行（继续或停止）                              │
│  • 不做业务决策                                                  │
│                                                                 │
│  ─────────────────────────────────────────────────────────────  │
│                                                                 │
│  Pregel 层 (pregel_barrier)                                     │
│  ─────────────────────────                                      │
│  • 汇总所有 Worker 的结果                                        │
│  • 包含 failed_count/failed_vertices                            │
│  • 包含 interrupted_count/interrupted_vertices                  │
│  • 包含 outbox（所有 Worker 的输出消息）                        │
│                                                                 │
│  ─────────────────────────────────────────────────────────────  │
│                                                                 │
│  Pregel 层 (pregel_worker)                                      │
│  ─────────────────────────                                      │
│  • 处理 compute_status: ok | {error, R} | {interrupt, R}       │
│  • 上报 failed_count/failed_vertices                            │
│  • 上报 interrupted_count/interrupted_vertices                  │
│  • 上报 outbox（不再直接路由）                                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 测试覆盖

| 测试文件 | 测试数量 |
|----------|----------|
| `pregel_worker_tests.erl` | 13 tests |
| `pregel_barrier_tests.erl` | 9 tests |
| `pregel_master_callback_tests.erl` | 11 tests |
| `pregel_message_routing_tests.erl` | 8 tests |
| `pregel_checkpoint_restore_tests.erl` | 13 tests |
| `graph_compute_tests.erl` | 4 tests |
| **总计** | **58 tests** |

---

## 相关文档

- `info/pregel_callback_checkpoint_analysis.md` - 回调与 Checkpoint 分析
- `info/pregel_master_failure_handling.md` - Master 失败处理设计
- `info/pregel_worker_error_handling_design.md` - Worker 错误处理设计
- `info/pregel_message_reliability_analysis.md` - 消息可靠性分析
- `info/pregel_outbox_vs_pending_writes.md` - Outbox 与 pending_writes 对比

---

## P1: Checkpoint 恢复功能 ✅

**优先级**: P1 (高)

**前置条件**: P0 回调机制已完成 ✅

**目标**: 支持从 checkpoint 恢复执行，实现完整的保存-恢复流程

### 设计概述

```
保存 checkpoint（已实现）:
┌─────────────────────────────────────────────────────────────┐
│  on_superstep_complete 回调                                  │
│         │                                                    │
│         ▼                                                    │
│  get_checkpoint_data() ──► checkpoint_data()                │
│                              │                               │
│                              ├── superstep: 2                │
│                              ├── vertices: #{v1 => ...}      │
│                              └── pending_messages: [...]     │
└─────────────────────────────────────────────────────────────┘

恢复 checkpoint（已实现）:
┌─────────────────────────────────────────────────────────────┐
│  pregel_master:run(Graph, ComputeFn, Opts)                   │
│                                      │                       │
│                                      ▼                       │
│  Opts = #{                                                   │
│      restore_from => #{              %% 恢复选项             │
│          superstep => 2,             %% 从超步 2 开始        │
│          vertices => #{...},         %% 恢复的顶点状态       │
│          messages => [...]           %% 待投递的消息         │
│      }                                                       │
│  }                                                           │
└─────────────────────────────────────────────────────────────┘
```

### 类型设计

```erlang
%% 恢复选项
-type restore_opts() :: #{
    superstep := non_neg_integer(),        %% 起始超步
    vertices := #{vertex_id() => vertex()}, %% 顶点状态
    messages => [{vertex_id(), term()}]     %% 待投递消息（可选）
}.

%% 扩展 opts()
-type opts() :: #{
    combiner => pregel_combiner:spec(),
    max_supersteps => pos_integer(),
    num_workers => pos_integer(),
    on_superstep => fun((non_neg_integer(), graph()) -> ok),
    on_superstep_complete => superstep_complete_callback(),
    restore_from => restore_opts()  %% 从 checkpoint 恢复
}.
```

---

### 5.1 扩展 opts() 类型支持恢复选项 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [x] 定义 `restore_opts()` 类型
- [x] 修改 `opts()` 类型，添加 `restore_from` 选项
- [x] 导出类型

---

### 5.2 修改 pregel_master init 支持初始状态 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [x] 修改 `init/1`，解析 `restore_from` 选项
- [x] 从 `restore_from.superstep` 初始化超步号
- [x] 添加 `get_restore_superstep/1` 辅助函数

---

### 5.3 修改 start_workers 支持顶点状态恢复 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [x] 修改 `start_workers/1`，支持传入恢复的顶点状态
- [x] 修改 `start_worker_processes/6`，合并恢复的顶点
- [x] 添加 `get_restore_vertices/1` 和 `merge_restored_vertices/2` 辅助函数

---

### 5.4 实现初始消息注入 ✅

**文件**: `apps/beamai_core/src/graph/pregel/pregel_master.erl`

- [x] 修改 `handle_call(start_execution)`，在启动超步前注入消息
- [x] 添加 `inject_restore_messages/1` 函数
- [x] 使用现有的 `route_all_messages/3` 将消息路由到 Workers

---

### 5.5 添加测试 ✅

**文件**: `apps/beamai_core/test/pregel_checkpoint_restore_tests.erl`

- [x] 测试从指定超步恢复（restore_from_superstep_2_test）
- [x] 测试顶点状态恢复（restored_vertex_values_test, final_graph_has_restored_vertices_test）
- [x] 测试消息注入（injected_messages_received_test）
- [x] 测试完整的保存-恢复流程（save_and_restore_consistency_test）
- [x] 测试边界情况（empty_messages_restore_test, partial_vertices_restore_test 等）

**测试统计**: 13 tests

---

### 5.6 更新文档

- [ ] 更新 `info/pregel_callback_checkpoint_analysis.md` 添加恢复流程说明

---

## 已完成任务

见 `TASK_DONE.md`
