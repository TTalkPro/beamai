# Graph Dispatch 设计方案

## 概述

实现 LangGraph 风格的动态并行分发（Send），在我们的 BSP 图引擎中命名为 `dispatch`。
允许条件边在运行时创建任意数量的并行执行分支，每个分支有独立的输入参数。

## 核心概念

- **Dispatch**: 待执行的节点调用指令（包含目标节点和输入参数）
- **Fan-out**: 条件边返回多个 Dispatch 创建并行分支
- **Fan-in**: 并行分支完成后 delta 通过 field_reducers 自动聚合到 global_state
- **VertexInputs**: Master 向 Worker 传递的按顶点分组的 dispatch 输入表

## 设计决策

### 1. 命名方案

| 当前 | 新命名 | 说明 |
|------|--------|------|
| `graph_send` 模块 | `graph_dispatch` | 模块重命名 |
| `send_to/1,2,3` | `dispatch/1,2,3` | API 函数 |
| `is_send/1` | `is_dispatch/1` | 谓词 |
| `is_sends/1` | `is_dispatches/1` | 列表谓词 |
| `__send__` 标记 | `__dispatch__` | 内部标记 |
| `SendInputs` | `VertexInputs` | Master→Worker 传递的数据 |
| `fan_out/2,3` | 保留 | fan_out 语义清晰 |

### 2. 执行模型：VertexInputs 表（非虚拟顶点）

由于当前分区模型中顶点数据（Fun/routing_edges）只存在于固定的一个 Worker 上，
虚拟顶点无法实现跨 Worker 并行。因此采用 VertexInputs 表方案：

- Master 将 dispatch 按目标 vertex_id 分组为 VertexInputs
- 发送到该顶点所在的 Worker
- Worker 对同一顶点按每个 input 串行执行 Fun

```
VertexInputs = #{
    task_executor => [Dispatch1, Dispatch2, Dispatch3]
}
```

### 3. 节点函数签名：显式双参数

节点计算函数从 `Fun/1` 改为 `Fun/2`，第二个参数为 VertexInput：

```erlang
Fun(GlobalState, VertexInput) -> {ok, NewState} | {error, Reason} | {interrupt, Reason, NewState}
%% 普通激活时: VertexInput = undefined
%% Dispatch 时: VertexInput = #{task => ..., depth => ...}
```

优势：
- 无字段冲突风险（Input 不与 GlobalState 合并）
- 节点明确知道哪些是 dispatch 输入
- Delta 计算天然清晰：`compute_delta(GlobalState, NewState)`
- 节点可按需区分普通/dispatch 调用

### 4. 向后兼容策略

强制迁移所有节点函数为 `Fun/2`。不关心 dispatch 的节点加 `_` 参数：

```erlang
my_node(State, _VertexInput) -> {ok, State#{...}}.
```

## 数据结构

### Dispatch 指令

```erlang
-type dispatch() :: #{
    '__dispatch__' := true,
    node := node_id(),           % 目标节点
    input := map(),              % 分支输入参数
    id := dispatch_id(),         % 唯一标识符
    metadata := map()            % 额外元数据
}.
```

### VertexInputs（Master → Worker）

```erlang
-type vertex_inputs() :: #{vertex_id() => [dispatch()]}.
%% 例: #{task_executor => [D1, D2, D3]}
```

### Context 扩展

```erlang
-type context() :: #{
    vertex_id := vertex_id(),
    vertex := vertex(),
    global_state := graph_state:state(),
    vertex_input := map() | undefined,     %% 新增
    superstep := non_neg_integer(),
    num_vertices := non_neg_integer()
}.
```

## 数据流

```
超步 S: Planner 节点计算完成
    │
    │ build_activations 返回:
    │   [llm_node, {dispatch, D1}, {dispatch, D2}, {dispatch, D3}]
    │
    ▼
Worker 汇报 → Master Barrier 聚合
    │
    │ activations = [llm_node, {dispatch, D1}, {dispatch, D2}, {dispatch, D3}]
    │
    ▼
Master: separate_dispatches →
    │   NormalActivations = [llm_node, task_executor]
    │   VertexInputs = #{task_executor => [D1, D2, D3]}
    │
    │ group_by_worker (按 vertex_id 哈希):
    │   Worker1: activations=[llm_node], vertex_inputs=#{}
    │   Worker2: activations=[task_executor], vertex_inputs=#{task_executor => [D1,D2,D3]}
    │
    ▼
超步 S+1:
  Worker2 收到:
    Activations = [task_executor]
    VertexInputs = #{task_executor => [D1, D2, D3]}

    对 task_executor 执行 3 次:
      Fun(GlobalState, #{task => "cats"})  → Delta1, Activations1
      Fun(GlobalState, #{task => "dogs"})  → Delta2, Activations2
      Fun(GlobalState, #{task => "birds"}) → Delta3, Activations3

    汇报: deltas=[Delta1,Delta2,Delta3], activations=[合并]
    │
    ▼
Master: apply_deltas([Delta1,Delta2,Delta3], FieldReducers)
  → 全局状态合并完成（Fan-in）
```

## 模块改动清单

### 1. `graph_dispatch.erl`（重命名自 `graph_send.erl`）

- 模块名: `graph_send` → `graph_dispatch`
- 函数: `send_to` → `dispatch`
- 标记: `__send__` → `__dispatch__`
- 谓词: `is_send` → `is_dispatch`, `is_sends` → `is_dispatches`
- fan_out 系列函数保留不变

### 2. `graph_edge.erl`

- `graph_send:is_send` → `graph_dispatch:is_dispatch`
- `{sends, Result}` → `{dispatches, Result}`

### 3. `graph_compute.erl`

- `build_activations`: 新增 `{ok, {dispatches, List}}` 分支，返回 `[{dispatch, D}]`
- `execute_and_route`: 从 Context 取 `vertex_input`，传给 Fun
- `execute_fun_and_route`: `Fun(GlobalState)` → `Fun(GlobalState, VertexInput)`

### 4. `pregel_superstep.erl`

- 新增: `separate_dispatches/1` — 从激活列表分离 dispatch 项
- 新增: `build_vertex_inputs/1` — dispatch 列表转 VertexInputs map
- 新增: `group_vertex_inputs_by_worker/2` — VertexInputs 按 Worker 分组

### 5. `pregel_master.erl`

- `broadcast_start_superstep`: 分离 dispatch，构建 VertexInputs，分发给 Worker
- `#state{}`: `pending_activations` 类型扩展支持 dispatch 项

### 6. `pregel_worker.erl`

- `start_superstep/3` → `start_superstep/4`（新增 VertexInputs 参数）
- `execute_superstep`: 接收 VertexInputs
- `compute_vertices`: 按 VertexInputs 决定执行次数
- `make_context`: 新增 `vertex_input` 字段
- Context 类型扩展

### 7. `pregel_barrier.erl`

- 无需改动（activations 仍为列表拼接）

### 8. 上游消费者

- `beamai_deepagent_router.erl`: `graph_send:` → `graph_dispatch:`
- `graph_dsl.erl`: 类型引用更新

## 边界情况处理

### 1. Dispatch 节点同时被普通激活

有 VertexInputs 时只走 dispatch 路径，忽略普通执行。
避免额外执行一次无 input 的计算。

### 2. 多次执行产生重复 activations

同一节点执行 N 次，每次都可能产出相同的下游激活。
在 Worker 结果中对 activations 去重（`lists:usort`）。

### 3. Checkpoint 中保存 dispatch 信息

`pending_activations` 类型从 `[vertex_id()]` 扩展为 `[vertex_id() | {dispatch, dispatch()}]`。
确保失败重试时 dispatch 信息不丢失。

### 4. Delta 计算

Delta 始终基于原始 GlobalState 计算：
```erlang
Delta = compute_delta(GlobalState, NewState)
```
VertexInput 中的字段不参与 delta 计算，不会泄漏到全局状态。
节点如需将 input 中的数据写入全局状态，需显式设置到 NewState 中。

## 并行度说明

同一节点的多个 dispatch 在同一 Worker 上串行执行（受分区模型限制）。
不同节点的 dispatch 天然并行（分布在不同 Worker）。
对于多数 Agent 场景（dispatch 数量 < 10），单 Worker 串行执行是可接受的。
