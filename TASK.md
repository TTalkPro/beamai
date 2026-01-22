# Graph 层全局状态模式重构任务

> 设计文档: `info/graph_global_state_redesign.md`

## 概述

将 graph 层从"消息传递模式"重构为"全局状态 + Delta 增量更新模式"。

### 核心变更

- Master 持有全局状态，Worker 只负责计算
- 节点发送 Delta（`#{field => value}`），而不是完整状态
- 使用 field_reducers 按字段合并 Delta
- 延迟提交：出错时暂存 Delta，不 apply
- 简化 vertex：移除 value，只保留 id 和 edges

---

## 状态：已完成 ✅

**完成日期**: 2026-01-22

所有 5 个阶段已完成，39 个测试全部通过。

---

## Phase 1: Pregel 层核心重构 ✅

### 1.1 重构 pregel_master.erl ✅

- [x] 添加 `global_state` 字段到 `#state{}` record
- [x] 添加 `field_reducers` 字段到 `#state{}` record
- [x] 添加 `pending_deltas` 字段（延迟提交）
- [x] 修改 `init/1`：初始化 global_state 和 field_reducers
- [x] 修改 `opts()` 类型：
  - [x] 添加 `global_state` 选项
  - [x] 添加 `field_reducers` 选项
  - [x] 删除 `state_reducer` 选项
- [x] 修改 `complete_superstep/1`：
  - [x] 收集所有 Worker 的 deltas
  - [x] 检查是否有失败/中断
  - [x] 成功时：apply field_reducers，广播新 global_state
  - [x] 失败时：暂存 pending_deltas，不 apply
- [x] 实现 `apply_deltas/3`：使用 field_reducers 合并 deltas
- [x] 实现 `broadcast_global_state/2`：广播全局状态给所有 Worker
- [x] 修改 `execute_retry/2`：处理延迟提交的重试逻辑
- [x] 添加 `get_global_state/1` API
- [x] 删除旧的 `state_reducer` 和消息合并相关代码

### 1.2 重构 pregel_worker.erl ✅

- [x] 添加 `global_state` 字段到 `#state{}` record
- [x] 修改 `init/1`：接收初始 global_state
- [x] 添加 `handle_cast({global_state, State}, ...)`：接收广播的全局状态
- [x] 修改 `compute_context` 类型
- [x] 修改 `compute_result` 类型（返回 delta）
- [x] 修改 `make_context/4`：传入 global_state 而不是 vertex
- [x] 修改 `process_compute_result/3`：从结果中提取 delta
- [x] 修改 `notify_master_done/6`：上报 deltas 列表
- [x] 删除对 `pregel_vertex:value/1` 的依赖

### 1.3 简化 pregel_vertex.erl ✅

- [x] 删除 `value` 字段从类型定义
- [x] 删除 `value/1` 函数
- [x] 删除 `set_value/2` 函数
- [x] 简化 `new/1,2`：
  - [x] `new(Id)` - 只需 id
  - [x] `new(Id, Edges)` - id + edges
- [x] 更新类型定义为纯拓扑结构

### 1.4 更新 pregel_barrier.erl ✅

- [x] 修改 `superstep_results()` 类型：增加 `deltas` 字段
- [x] 修改 `aggregate_results/1`：收集 deltas

---

## Phase 2: Graph 层适配 ✅

### 2.1 重构 graph_runner.erl ✅

- [x] 修改 `run_options` 类型
- [x] 修改 `run/3`：传递 global_state 和 field_reducers 给 pregel
- [x] 新增 `build_compute_config/3`：传递 nodes 配置
- [x] 更新 `checkpoint_data` 结构

### 2.2 重构 graph_state_reducer.erl ✅

- [x] 保留并增强内置 reducer：
  - [x] `append_reducer/2` - 列表追加
  - [x] `merge_reducer/2` - Map 合并
  - [x] `last_write_win_reducer/2` - 后值覆盖
- [x] 实现新函数：
  - [x] `apply_delta/3` - 对单个 delta 按字段应用 reducer
  - [x] `apply_deltas/3` - 批量应用 deltas

### 2.3 更新 graph_compute.erl ✅

- [x] 修改 compute 函数以适配新模型
- [x] 从 config 获取 nodes 配置
- [x] 返回 `#{delta, outbox, status}` 格式

### 2.4 更新 graph_send.erl ✅

- [x] 更新消息发送逻辑

---

## Phase 3: beamai_agent 层迁移 ✅

### 3.1 更新 beamai_agent_runner.erl ✅

- [x] 修改 `build_run_options/2`：构建初始 `global_state`
- [x] 传入 `field_reducers` 配置
- [x] 修改 `handle_graph_result/2`：从结果中提取新的 global_state

### 3.2 重构 beamai_nodes.erl ✅

- [x] 所有节点改为从 `global_state` 读取数据
- [x] 返回 `delta` 而不是更新 vertex value

### 3.3 更新 beamai_node_registry.erl ✅

- [x] 更新节点构建逻辑

---

## Phase 4: 测试更新 ✅

### 4.1 Pregel 层测试 ✅

- [x] 更新 `pregel_master_tests.erl`
  - [x] 测试 global_state 初始化
  - [x] 测试 delta 合并
  - [x] 测试 field_reducers
  - [x] 测试 global_state 广播
- [x] 更新 `pregel_worker_tests.erl`
  - [x] 测试接收 global_state
  - [x] 测试返回 delta
- [x] 更新 `pregel_barrier_tests.erl`
  - [x] 测试 delta 汇总

### 4.2 Graph 层测试 ✅

- [x] 更新 `graph_compute_tests.erl`
  - [x] 测试 from_pregel_result 提取 global_state

---

## Phase 5: 清理 ✅

### 5.1 清理废弃代码 ✅

- [x] `pregel_vertex.erl`: 已移除 `value` 字段和相关函数
- [x] `pregel_master.erl`: 已移除 `state_reducer` 相关代码
- [x] `pregel_worker.erl`: 已移除 vertex value 操作
- [x] 保留向后兼容的废弃函数（返回 undefined/空值）

### 5.2 更新文档 ✅

- [x] 更新 `pregel.erl` 文档注释
- [x] 更新 `examples/src/example_pregel.erl` 示例代码
- [x] 更新 `info/pregel_graph_compute_fn.md` 文档

---

## 测试覆盖

| 测试文件 | 测试数量 |
|----------|----------|
| `pregel_master_tests.erl` | 9 tests |
| `pregel_worker_tests.erl` | 13 tests |
| `pregel_barrier_tests.erl` | 9 tests |
| `graph_compute_tests.erl` | 8 tests |
| **总计** | **39 tests, 0 failures** |

---

## Delta 格式规范

```erlang
%% Delta 是简单的 field => value 映射
-type delta() :: #{atom() | binary() => term()}.

%% 示例
#{
    messages => [NewMessage],      %% append_reducer 处理
    context => #{key => value},    %% merge_reducer 处理
    last_response => Response      %% last_write_win 处理
}

%% field_reducers 配置（注意：键使用 binary 格式）
#{
    <<"messages">> => fun graph_state_reducer:append_reducer/2,
    <<"context">> => fun graph_state_reducer:merge_reducer/2
    %% 未配置的字段默认 last_write_win
}
```

---

## 架构说明

### apply_deltas 流程

```erlang
%% Delta 合并过程
apply_deltas(GlobalState, Deltas, FieldReducers) ->
    lists:foldl(fun(Delta, AccState) ->
        apply_delta(Delta, AccState, FieldReducers)
    end, GlobalState, Deltas).
```

### 全局状态同步模式

当前使用 **Push 模式**：
1. Master 完成超步后 apply_deltas 得到 NewGlobalState
2. Master 广播 NewGlobalState 给所有 Worker
3. Master 发送 start_superstep 开始下一超步

可选的 **Pull 模式**（未实现，但已验证可行）：
1. Master 完成超步后 apply_deltas 得到 NewGlobalState（保留在 Master）
2. Master 发送 start_superstep 开始下一超步
3. Worker 主动调用 get_global_state 同步状态
4. **不会死锁**：因为同步调用发生在计算开始前

---

## 修改的核心文件

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
