# 会话记忆（Memory Filter 机制）

[English](MEMORY_EN.md) | 中文

beamai_core 的会话记忆把「对话历史的存储与注入」从 Kernel 中彻底剥离：
**Kernel 无状态，每次 invoke 只传入单条最新消息**，会话历史由 Memory Filter
配合 ChatMemory store 按 `conversation_id` 管理。Memory Filter 是一个洋葱式
filter（含 pre_chat/post_chat 一对 hook，详见 [Filter 文档](FILTER.md)）。

设计背景见 [design/kernel_memory_filter_redesign.md](../design/kernel_memory_filter_redesign.md)。

## 目录

- [核心理念](#核心理念)
- [组件](#组件)
- [delta 模式 vs full 模式](#delta-模式-vs-full-模式)
- [快速开始](#快速开始)
- [ChatMemory Behaviour](#chatmemory-behaviour)
- [窗口包装](#窗口包装)
- [数据流](#数据流)

---

## 核心理念

- **Kernel 不记录消息**。`beamai_context` 不再持有 `messages` / `history`。
- **每次 invoke 只传单条最新消息**（首轮=用户消息，后续轮=工具结果）。
- 会话历史的存储、注入、裁剪全部由 **Memory Filter + store** 承担，按 `conversation_id` 隔离。
- 不挂记忆时 Kernel 退化为**单次无状态调用**（工具循环内部本地累积）。

## 组件

| 模块 | 职责 |
|------|------|
| `beamai_chat_memory` | ChatMemory **behaviour** + 调度 API，句柄 `{Module, Ref}` |
| `beamai_chat_memory_ets` | 默认 ETS gen_server 实现（进程持有 ETS 表） |
| `beamai_chat_memory_window` | 滑动窗口包装：底层保留全量，`mem_get` 时套条数窗口 |
| `beamai_memory_filter` | 单个 filter：pre_chat 存 delta + 展开历史；post_chat 存回复 |
| `beamai_kernel:with_memory/2` | 绑定 store 并挂载该 Memory Filter |

## delta 模式 vs full 模式

`invoke/3` 根据是否启用记忆自动选择模式：

| | full 模式 | delta 模式 |
|---|---|---|
| 触发条件 | **未** `with_memory` | 已 `with_memory` 绑定 store |
| 每轮传给 pipeline | 本地累积的**全量**消息 | 只传**新 delta** |
| 谁攒完整历史 | 工具循环里的本地变量 | ChatMemory store |
| 是否用 conversation_id | 否 | 是（context 无则生成临时 id，结束清理） |
| 工具循环内上下文连续 | ✅ | ✅ |
| 跨 invoke 记忆 | ❌（单次无状态） | ✅（按 conversation_id 持久） |
| assistant 消息谁记录 | 本地拼接 | Memory Filter 的 post_chat 存入 store |

> **full 模式存在的意义**：工具调用循环天生多轮（LLM→工具→再 LLM）。无 store 时，
> 仍需在单次 invoke 内让后续 LLM 调用看到先前的 tool_call 与工具结果——full 模式用
> 循环里的本地变量累积完整对话，每轮传全量。它不持久、不跨 invoke。

## 快速开始

```erlang
%% 1. 启动会话存储（ETS 默认实现）
{ok, _Pid} = beamai_chat_memory_ets:start_link(my_mem),
Store = beamai_chat_memory_ets:handle(my_mem),   %% 句柄 {beamai_chat_memory_ets, my_mem}

%% 2. 构建 Kernel 并启用记忆
K0 = beamai_kernel:new(),
K1 = beamai_kernel:add_service(K0, LlmConfig),
K  = beamai_kernel:with_memory(K1, Store),       %% 或 beamai:with_memory/2

%% 3. 用 conversation_id 标识会话，每次只传最新消息
Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"session-1">>),

{ok, R1, _} = beamai_kernel:invoke(K, [#{role => user, content => <<"我叫张三">>}],
                                   #{context => Ctx}),
%% 同一 conversation_id，第二轮 LLM 能看到完整历史
{ok, R2, _} = beamai_kernel:invoke(K, [#{role => user, content => <<"我叫什么？">>}],
                                   #{context => Ctx}).

%% 4. 读取/清理会话历史
History = beamai_chat_memory:mem_get(Store, <<"session-1">>),
ok      = beamai_chat_memory:mem_clear(Store, <<"session-1">>).
```

> 若 context 未设置 `conversation_id`，invoke 会生成临时会话 id 并在结束后清理
> （即：有 store 但只想一次性使用），不产生跨 invoke 记忆。

## ChatMemory Behaviour

句柄约定 `{Module, Ref}`（与 `{beamai_store_ets, Name}` 一致），调度 API 解包转发。

```erlang
-callback mem_get(Ref :: term(), ConvId :: binary()) -> [message()].
-callback mem_add(Ref :: term(), ConvId :: binary(), Msgs :: [message()]) -> ok.
-callback mem_clear(Ref :: term(), ConvId :: binary()) -> ok.

%% 调度 API（对 {Module, Ref} 解包）
beamai_chat_memory:mem_get(Store, ConvId).
beamai_chat_memory:mem_add(Store, ConvId, Msgs).
beamai_chat_memory:mem_clear(Store, ConvId).
```

实现本 behaviour 即可提供自定义后端（如持久化 SQLite store）。

## 窗口包装

`beamai_chat_memory_window` 包装任意 store：底层保留全量历史，仅在 `mem_get` 读取时
套用条数滑动窗口。规则：system 消息始终保留并置于头部；非系统消息只保留最近 N 条；
裁剪后丢弃落在头部的孤立 `tool` 消息。

```erlang
{ok, _} = beamai_chat_memory_ets:start_link(inner),
Inner = beamai_chat_memory_ets:handle(inner),
Store = beamai_chat_memory_window:handle(Inner, 20),   %% 最近 20 条非系统消息
K = beamai_kernel:with_memory(K1, Store).
```

> 基于 Token 的裁剪/摘要不在 core 内（避免反向依赖 `beamai_conversation_buffer`），
> 可由 beamai_cognition 提供实现同 behaviour 的 store。

## 数据流（带工具循环，delta 模式）

```
invoke(Kernel, [User消息], #{context := Ctx(conv_id=s1)})
└─ tool_calling_loop，delta = [User消息]
   └─ run_chat_pipeline(delta)
      ├─ memory.pre_chat (order -1000，外层): mem_add(s1, delta)；messages := mem_get(s1) 全量历史
      ├─ system.pre_chat (order -500，内层): messages := [SysPrompt | messages]  （不存储）
      ├─ Terminal: LLM 调用
      └─ memory.post_chat (回程): mem_add(s1, assistant 回复)
   ├─ 有 tool_calls → 执行工具（走 tool filter 链，pre_tool/post_tool），delta := [工具结果]，回到循环
   └─ 纯文本 → 返回 {ok, Response, Ctx}
```

memory 是单个 filter：`pre_chat` 展开历史在外层、`post_chat` 存回复在回程；洋葱链天然
保证顺序，无需优先级模拟。system_prompts 作为临时 filter（仅 pre_chat，order -500，更内层）
注入，位于历史展开之后、LLM 之前，**不写入存储**。

## 关键源文件

| 文件 | 说明 |
|------|------|
| `apps/beamai_core/src/behaviours/beamai_chat_memory.erl` | behaviour + 调度 API |
| `apps/beamai_core/src/kernel/beamai_chat_memory_ets.erl` | ETS 默认实现 |
| `apps/beamai_core/src/kernel/beamai_chat_memory_window.erl` | 窗口包装 |
| `apps/beamai_core/src/kernel/beamai_memory_filter.erl` | Memory Filter |
| `apps/beamai_core/src/kernel/beamai_kernel.erl` | `with_memory/2`、invoke 双模式 |
