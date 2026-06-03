# Kernel 重新设计：消息存储下沉到 Memory Filter

> 状态：设计已定稿，待实现
> 参考：`~/workspace/clj-agent/modules/`（clj-agent 的 memory filter 机制）

## 1. 目标

- **Kernel / Context 不再记录 messages**。每次 `invoke` 永远只传入「单条最新消息」（首轮=用户消息，后续轮=工具结果 delta）。
- 会话历史的存储、注入、裁剪全部由 **Memory Filter + ChatMemory store** 承担。
- Kernel 回归无状态编排器；不挂 memory 时退化为单次无状态调用。

## 2. 现状（待拆除）

- `beamai_context` 持有 `messages`（工作缓冲）+ `history`（追加日志）两个字段。
- `beamai_kernel:invoke/3` 用 `ExistingMsgs ++ Messages` 拼接累积，并 `track_message/track_new_messages` 写回 context。
- blast radius：消息累积 API 仅被 `beamai_kernel.erl` 与 `beamai_chat_completion_tests.erl` 使用。`beamai_memory:get_history` 是快照历史，无关。无任何 `conversation_id` 使用点。

## 3. 已定决策

1. **彻底移除** `beamai_context` 的 `messages` / `history` 字段及相关 API。
2. **无状态透传默认**：未挂 memory 时，`invoke` 不存不展开，delta 直接送 LLM；会话记忆需显式 `with_memory`。
3. **core 默认 + 行为可插拔**：`beamai_core` 提供 ETS 默认实现；通过 `beamai_chat_memory` behaviour 允许 `beamai_memory` 之后提供 SQLite 等持久化后端。

## 4. 组件设计

### 4.1 behaviour `beamai_chat_memory`（beamai_core）

句柄沿用 `{Module, Ref}` 约定（与 `{beamai_store_ets, Name}` 一致）。

```erlang
-type handle() :: {module(), term()}.

-callback mem_get(Ref :: term(), ConvId :: binary()) -> [beamai_message:message()].
-callback mem_add(Ref :: term(), ConvId :: binary(), Msgs :: [beamai_message:message()]) -> ok.
-callback mem_clear(Ref :: term(), ConvId :: binary()) -> ok.

%% 调度 API（对 {Module,Ref} 解包转发）
-export([mem_get/2, mem_add/3, mem_clear/2]).
mem_get({M, Ref}, ConvId)        -> M:mem_get(Ref, ConvId).
mem_add({M, Ref}, ConvId, Msgs)  -> M:mem_add(Ref, ConvId, Msgs).
mem_clear({M, Ref}, ConvId)      -> M:mem_clear(Ref, ConvId).
```

### 4.2 默认实现 `beamai_chat_memory_ets`（beamai_core）

- 轻量 gen_server 持有 ETS 表：`ConvId => [message()]`（追加语义；`mem_get` 返回正序全量）。
- `start_link(Name) -> {ok, Pid}`，句柄为 `{beamai_chat_memory_ets, Name}`。
- 必须放在 beamai_core（kernel 不可依赖 beamai_memory）。

### 4.3 可选包装 `beamai_chat_memory_window`（beamai_core）

- 包装 inner store，`mem_get` 时套滑动窗口 / Token 裁剪 / 摘要——复用现有 `beamai_conversation_buffer` 的 `apply_window`、`trim_to_tokens`、`summarize_messages`。
- 底层保留全量，仅在读取时裁剪（同 clj 的 WindowedStore；保护：丢弃落单的 head `tool` 消息）。

### 4.4 `beamai_memory_filter`（beamai_core）

```erlang
-export([memory_filters/1]).

%% 返回 [PreChat, PostChat]，闭包绑定 StoreHandle
memory_filters(StoreHandle) ->
    [pre_chat_filter(StoreHandle), post_chat_filter(StoreHandle)].
```

- **pre_chat，priority = -1000**：从 filter_context 读 `conversation_id`；
  - 有 conv_id：`mem_add(Store, Cid, Delta)`，然后 `messages := mem_get(Store, Cid)`（完整历史替换）。
  - 无 conv_id：原样透传。
- **post_chat，priority = +1000**：有 conv_id 时 `mem_add(Store, Cid, [response_to_message(Resp)])`。
  - `response_to_message/1`：`beamai_llm_response` → assistant 消息（有 tool_calls 用 `beamai_message:tool_calls/1`，否则 `beamai_message:assistant/1`）。

### 4.5 System prompt 注入（不进存储）

- 系统提示作为独立 pre_chat filter，**priority = -500**（在 memory-pre 展开历史之后、LLM 之前），把 system 消息前置到 messages，但**不写入 store**。
- kernel 只把会话 delta 交给 pipeline，保证 memory-pre 存的永远是纯净对话（无 system prompt）。
- 实现：`beamai_kernel:with_system_prompt/2` 或在 invoke opts 里带 `system_prompts`，由 kernel 内部挂一个临时 pre_chat filter（priority -500）。

### 4.6 `beamai_kernel` 改动

- Kernel state 增加 `memory => handle() | undefined`。
- 新增 `with_memory(Kernel, StoreHandle) -> Kernel`：挂载 `beamai_memory_filter:memory_filters/1` 的两个 filter。
- **重写 `invoke/3`**：
  - 删除 `ExistingMsgs ++ Messages`、`track_message/2`、`track_new_messages/2`。
  - 解析 `conversation_id`：取 context 里的；无则生成临时 `<<"conv-", UUID>>` 并标记 ephemeral。
  - `tool_calling_loop` 每轮只把**新 delta** 传给 `run_chat_pipeline`（首轮=入参 Messages，后续=工具结果）。
  - 结束（`try...after`）若 ephemeral 则 `mem_clear`。
- `invoke_chat/3` 同样只接收 delta。

### 4.7 `beamai_context` 改动

- 移除字段 `messages`、`history`；移除 `get_messages/1`、`set_messages/2`、`append_message/2`、`get_history/1`、`add_history/2`。
- 新增：`with_conversation_id/2`、`conversation_id/1`（存为保留 key，如 `<<"__conversation_id__">>`）。
- context = 纯「变量 + 元数据 + conv_id」载体。

## 5. 数据流（带工具循环）

```
invoke(Kernel, [User消息], #{context := Ctx(conv_id=s1)})
└─ tool_calling_loop, delta = [User消息]
   └─ run_chat_pipeline(delta)
      ├─ [pre_chat -1000] memory : mem_add(s1, delta); messages := mem_get(s1) 全量历史
      ├─ [pre_chat  -500] system : messages := [SysPrompt | messages]   (不存储)
      ├─ LLM 调用
      └─ [post_chat +1000] memory: mem_add(s1, assistant回复)
   ├─ 有 tool_calls → 执行工具(走 pre/post_invocation filter)，delta := [tool_results]，回到循环
   └─ 纯文本 → 返回 {ok, Response, Ctx}
```

## 6. 受影响文件清单

| 文件 | 改动 |
|------|------|
| `beamai_core/src/behaviours/beamai_chat_memory.erl` | 新增 behaviour + 调度 API |
| `beamai_core/src/kernel/beamai_chat_memory_ets.erl` | 新增 ETS gen_server 默认实现 |
| `beamai_core/src/kernel/beamai_chat_memory_window.erl` | 新增窗口/裁剪包装（复用 conversation_buffer） |
| `beamai_core/src/kernel/beamai_memory_filter.erl` | 新增 memory pre/post_chat filters |
| `beamai_core/src/kernel/beamai_kernel.erl` | 加 memory 字段 + with_memory；重写 invoke；删 track_* |
| `beamai_core/src/kernel/beamai_context.erl` | 删 messages/history；加 conversation_id |
| `beamai_core/src/beamai_core.app.src` | 注册新模块 |
| `beamai_core/test/beamai_chat_completion_tests.erl` | 删除/改写 messages/history 相关用例 |
| 新增测试 | memory filter 累积、工具循环、ephemeral 清理、窗口裁剪 |

## 7. 验证

- 单测：无 memory 时单次调用透传；挂 memory 后多轮累积；工具循环每轮只传 delta；ephemeral 会话自动清理；窗口 store 读取裁剪。
- 集成：`beamai_llm_integration_SUITE` 增加一个「memory 多轮对话」用例（沿用 Zhipu 兼容 API，未设环境变量则 skip）。
