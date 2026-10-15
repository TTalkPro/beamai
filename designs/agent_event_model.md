# Agent Event Model 设计

## 目标

为 Agent run 建立一个统一、可排序、可多路消费的事件模型。UI stream、
telemetry handler 和审计日志共享同一事件来源，但各自保持独立消费器；
事件模型本身不改变 Agent 执行流程。

## 边界

- **Agent callbacks** 仍是业务扩展点。除 `on_tool_call` 的审批门语义外，
  callback 观察异常会被隔离，不影响 Agent 主流程。
- **Agent event** 是 run 时序的标准化表示，runtime 在关键边界发布。
- **Telemetry** 是事件模型的下游适配目标之一；runtime 不为 telemetry
  另建一套时序。

## 事件结构

```erlang
#{type := event_type(),
  sequence := pos_integer(),
  timestamp := integer(),
  agent_id := term(),
  run_id := binary() | undefined,
  conversation_id := binary(),
  data := map(),
  metadata := map()}
```

公共字段语义：

| 字段 | 语义 |
|---|---|
| `type` | 稳定事件类型，见下表。 |
| `sequence` | 同一 `run_id` 内单调递增的正整数。并行工具完成乱序到达时，消费端以该字段排序。 |
| `timestamp` | 事件发生时间的 wall clock 毫秒值，用于展示和跨 run 粗排序；精确定序以 `sequence` 为准。 |
| `agent_id` | 发布事件的 Agent 标识。 |
| `run_id` | 当前 run 标识；同一次 run / resume continuation 保持不变。 |
| `conversation_id` | Agent 会话标识。 |
| `data` | 事件类型相关的保守负载数据，不携带 prompt、tool result 全文、API key 或 URL。 |
| `metadata` | 其他关联字段，例如 `request_id`、`message_id`、`batch_id`、`tool_call_id`、`turn_count`。 |

`agent_id`、`run_id`、`conversation_id` 提升为顶层字段，方便消费器过滤；
它们仍会在 telemetry adapter 中合并回 telemetry metadata。

## 事件类型

```erlang
-type event_type() ::
    run_started | run_finished |
    turn_started | turn_finished |
    chat_started | chat_finished |
    tool_queued | tool_started | tool_finished |
    interrupt_requested | approval_requested |
    resumed | state_changed |
    error_occurred.
```

语义约束：

- `run_started` / `run_finished` 覆盖一次用户发起的 run 或 resume。
- `turn_started` / `turn_finished` 表示一次 Agent turn 边界；resume 续跑
  同样发出 turn 事件。
- `chat_started` / `chat_finished` 表示 tool loop 中一次 LLM 调用边界，
  一个 turn 可有多次。
- `tool_queued` 在工具进入批次时发出；`tool_started` / `tool_finished`
  以 `tool_call_id` 配对。
- `interrupt_requested` 表示已进入暂停；`approval_requested` 是面向 HITL
  审批的细分事件；`resumed` 表示用户决策已被接受并开始续跑。
- `error_occurred` 记录 turn / chat / tool 归一化后的错误分类，不携带敏感
  原文。

## 排序

事件模型提供按 `{run_id, sequence, timestamp}` 升序排序的稳定辅助函数。

单进程发布点在事件创建时分配 sequence；并行工具完成时允许物理到达顺序
乱序，但发布点分配的逻辑 sequence 不变。跨 run 事件不应强制合并为一条
全局序列，消费端先按 `run_id` 分组。

## 消费接口

事件消费器是单参函数：

```erlang
-type consumer() :: fun((event()) -> term()).
```

runtime 可同时注册多个 consumer。事件分发器逐个调用 consumer，并隔离单个
consumer 的异常；consumer 返回值被忽略。事件模型不是业务控制通道，不能
通过事件修改 Agent 决策。

## Telemetry 适配

统一事件可映射为 `beamai_telemetry` 事件：

- `name` 由事件类型映射到既有或新增的 `[beamai, ...]` 事件名。
- `level` 按事件语义映射；错误为 `error`，中断为 `warning`，其他默认
  `info`。
- `data` 保持事件 `data`。
- `metadata` 合并公共关联字段、`sequence` 和事件 `timestamp`。

该映射保证 telemetry handler 继续获得现有 telemetry 事件形状，同时可以
借助 `sequence` 重建同一次 run 的事件顺序。

## Runtime 接入约束

Phase 4 批次 Q 接入 runtime 时必须满足：

1. run 入口创建或延续同一 `run_id`，并初始化 per-run sequence。
2. 事件发布不改变 run 返回值和控制流。
3. callback / filter 仍按现有语义执行，事件发布点只做观测。
4. 默认事件负载与 telemetry 约束一致，不输出敏感内容。
5. UI stream consumer 与 telemetry consumer 可以并行注册。
