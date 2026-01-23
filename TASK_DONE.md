# 已完成任务

## P0: 新增 Dispatch / Command / Graph LLM 示例 ✅

**完成日期**: 2026-01-23

新增三个示例，展示 graph 层核心能力和两种 Provider 配置（GLM-4.6 zhipu_provider / GLM-4.7 anthropic_provider）。

**新增文件**：
- `examples/src/example_dispatch.erl` - 动态并行分发（fan_out + append_reducer）
- `examples/src/example_command.erl` - Command 模式（goto 动态路由 + 审查重试循环）
- `examples/src/example_graph_llm.erl` - 纯图执行 LLM + Tool 循环（条件边 + 工具调用）

**覆盖的核心特性**：
- `graph_dispatch:fan_out/3` - 动态创建并行分支
- `graph_command:goto/2` - 同时指定 delta 和路由目标
- `graph:add_conditional_edge/3` - 条件边路由
- `field_reducers` (append_reducer) - 并行分支结果合并
- 两种 Provider: `llm_client:create(zhipu, ...)` / `llm_client:create(anthropic, ...)`

**测试**: Mock 模式和真实 LLM 调用均通过

---

## P0: Graph 层全局状态模式重构 ✅

**完成日期**: 2026-01-22

将 graph 层从"消息传递模式"重构为"全局状态 + Delta 增量更新模式"。

**核心变更**：
- Master 持有全局状态（global_state），Worker 只负责计算
- 节点发送 Delta（`#{field => value}`），而不是完整状态
- 使用 field_reducers 按字段合并 Delta（append_reducer / merge_reducer / last_write_win_reducer）
- 延迟提交：出错时暂存 Delta，不 apply
- 简化 vertex：移除 value，只保留 id 和 edges

**修改的核心文件**：
- `pregel_master.erl` - global_state、field_reducers、delta 合并逻辑
- `pregel_worker.erl` - 接收 global_state、返回 delta
- `pregel_vertex.erl` - 简化为纯拓扑结构
- `pregel_barrier.erl` - 汇总 deltas
- `pregel.erl` - get_result_global_state/1 API
- `graph_runner.erl` - 构建 global_state 和 config
- `graph_compute.erl` - 适配新模型
- `graph_builder.erl` - 创建纯拓扑顶点

**测试**: 39 tests, 0 failures

---

## P0: 移除 pending_messages，实现 BSP 集中路由 ✅

**完成日期**: 2026-01-21

移除不合理的 `pending_messages` 机制，改用 BSP 模型的集中路由方式。

**核心变更**：
- Worker 不再实时发送消息，改为上报 outbox 给 Master
- Master 在超步完成后统一路由所有消息
- 移除 Master 的 pending_messages 机制

**修改的文件**：
- `pregel_worker.erl` - 移除实时路由，上报 outbox
- `pregel_barrier.erl` - 汇总 outbox
- `pregel_master.erl` - 移除 pending_messages，集中路由

**新增测试**: `pregel_message_routing_tests.erl` (8 tests)

---

## P0: pregel_master 回调机制 ✅

**完成日期**: 2026-01-21

为 `pregel_master` 实现统一的回调机制，支持 checkpoint 保存、失败处理和 human-in-the-loop。

**核心变更**：
- 支持 initial / step / final 三种回调类型
- Checkpoint 数据按需获取（get_checkpoint_data 闭包）
- 回调返回 continue | {stop, Reason} 控制执行流

**修改的文件**：
- `pregel_master.erl` - 类型定义、回调机制

**新增测试**: `pregel_master_callback_tests.erl` (11 tests)

---

## P0: pregel_barrier 汇总失败和中断信息 ✅

**完成日期**: 2026-01-21

修改 `pregel_barrier` 模块，汇总所有 Worker 的失败和中断信息。

**核心变更**：
- `aggregate_results/1` 返回 map 格式
- 汇总 failed_count / failed_vertices / interrupted_count / interrupted_vertices

**修改的文件**：
- `pregel_barrier.erl` - 类型定义、函数修改
- `pregel_master.erl` - 适配新接口

**新增测试**: `pregel_barrier_tests.erl` (9 tests)

---

## P0: pregel_worker 中断支持 ✅

**完成日期**: 2026-01-21

为 `pregel_worker` 增加中断（interrupt）支持，用于 human-in-the-loop 场景。

**核心变更**：
- 新增 `{interrupt, Reason}` 计算状态
- 中断与错误分开处理，各自有独立的收集和上报机制
- 中断时不更新顶点状态，不发送消息

**修改的文件**：
- `pregel_worker.erl` - 类型扩展、函数修改

**新增测试**: 5 个测试用例 in `pregel_worker_tests.erl`

---

## P0: pregel_worker 错误处理重构 ✅

**完成日期**: 2026-01-21

为 `pregel_worker` 增加错误处理能力，识别顶点计算的成功/失败状态并上报 Master。

**核心变更**：
- 定义 `compute_status()` 和 `compute_result()` 类型契约
- Worker 上报 failed_count 和 failed_vertices
- graph_compute 层负责异常捕获和包装

**修改的文件**：
- `pregel_worker.erl` - 类型定义、函数修改
- `graph_compute.erl` - 异常处理包装

**新增测试**：
- `pregel_worker_tests.erl` (9 tests)
- `graph_compute_tests.erl` (4 tests)

---

## 测试覆盖总览

| 测试文件 | 测试数量 |
|----------|----------|
| `pregel_master_tests.erl` | 9 tests |
| `pregel_worker_tests.erl` | 13 tests |
| `pregel_barrier_tests.erl` | 9 tests |
| `pregel_message_routing_tests.erl` | 8 tests |
| `graph_compute_tests.erl` | 8 tests |
| `graph_command_tests.erl` | 相关测试 |
| `graph_command_integration_tests.erl` | 相关测试 |
| **Pregel + Graph 核心** | **47+ tests** |
