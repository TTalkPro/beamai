# 会话记忆

[English](MEMORY_EN.md) | 中文

beamai_core 把「对话历史的存储与注入」从 Kernel 中彻底剥离：**Kernel 无状态，
每次 invoke 只传入单条最新消息**，会话历史按 `conversation_id` 管理。

记忆分为**两层**，各自解决不同问题：

| 层 | Behaviour | 职责 |
|---|---|---|
| **存储层** | `beamai_chat_memory` | 后端（ETS/DETS/自建），只管 get/add/clear，不含策略 |
| **策略层** | `beamai_memory_provider` | 跨轮加载/持久化 + **发送前变换**（窗口裁剪/摘要/RAG 召回） |

上面这两层之上，有**两条互不影响的接入路径**——取决于你用 Kernel 还是 Agent：

| 路径 | 接入方式 | 适用 |
|---|---|---|
| **Kernel 级** | `beamai_memory_filter:memory_filter(Store)` 放进 filters 列表首位 | 直接用 `beamai_kernel` / `beamai` facade |
| **Agent 级** | `memory => Provider` 配置，Agent 在 tool loop 里显式编排 | 用 `beamai_agent` |

> **两条路径不要混用**：`beamai_agent` **不使用** memory filter——它在自己的
> tool loop 里通过 `beamai_memory_provider` 显式调 history/prepare/append。
> Agent 层没有 filter 概念（filter 属于 kernel 层）。

设计背景见 [design/kernel_memory_filter_redesign.md](../design/kernel_memory_filter_redesign.md)。

## 目录

- [核心理念](#核心理念)
- [组件](#组件)
- [delta 模式 vs full 模式](#delta-模式-vs-full-模式)
- [快速开始（Kernel 级）](#快速开始kernel-级)
- [ChatMemory Behaviour（存储层）](#chatmemory-behaviour存储层)
- [Memory Provider（策略层 / Agent 级）](#memory-provider策略层--agent-级)
- [滑动窗口](#滑动窗口)
- [数据流](#数据流)

---

## 核心理念

- **Kernel 不记录消息**。`beamai_context` 不再持有 `messages` / `history`。
- **每次 invoke 只传单条最新消息**（首轮=用户消息，后续轮=工具结果）。
- 会话历史的存储、注入、裁剪全部由 **store + filter/provider** 承担，按 `conversation_id` 隔离。
- 不挂记忆时 Kernel 退化为**单次无状态调用**（工具循环内部本地累积）。

## 组件

| 模块 | 层 | 职责 |
|------|---|------|
| `beamai_chat_memory` | 存储 | ChatMemory **behaviour** + 调度 API，句柄 `{Module, Ref}` |
| `beamai_chat_memory_ets` | 存储 | 默认 ETS gen_server 实现（进程持有 ETS 表） |
| `beamai_chat_memory_dets` | 存储 | DETS 持久化实现（重启后按 conversation_id 恢复历史） |
| `beamai_memory_provider` | 策略 | Provider **behaviour** + 调度 API：history/append/prepare/clear |
| `beamai_memory_provider_default` | 策略 | 默认 provider：包一个 store，`new/2` 可带滑动窗口 |
| `beamai_memory_filter` | Kernel 接入 | 单个 filter：around_chat 前置存 delta + 展开历史、后置存回复 |
| `beamai_kernel:new/2` | Kernel 接入 | 构建时把 memory filter 放进 filters 列表（首位 = 最外层） |

## delta 模式 vs full 模式

`invoke/3` 根据是否启用记忆自动选择模式：

| | full 模式 | delta 模式 |
|---|---|---|
| 触发条件 | filters 列表**无** memory filter | filters 列表含 memory filter（绑定 store） |
| 每轮传给 pipeline | 本地累积的**全量**消息 | 只传**新 delta** |
| 谁攒完整历史 | 工具循环里的本地变量 | ChatMemory store |
| 是否用 conversation_id | 否 | 是（context 无则生成临时 id，结束清理） |
| 工具循环内上下文连续 | ✅ | ✅ |
| 跨 invoke 记忆 | ❌（单次无状态） | ✅（按 conversation_id 持久） |
| assistant 消息谁记录 | 本地拼接 | Memory Filter 的 around_chat 后置存入 store |

> **full 模式存在的意义**：工具调用循环天生多轮（LLM→工具→再 LLM）。无 store 时，
> 仍需在单次 invoke 内让后续 LLM 调用看到先前的 tool_call 与工具结果——full 模式用
> 循环里的本地变量累积完整对话，每轮传全量。它不持久、不跨 invoke。

## 快速开始（Kernel 级）

```erlang
%% 1. 启动会话存储（ETS 默认实现）
{ok, _Pid} = beamai_chat_memory_ets:start_link(my_mem),
Store = beamai_chat_memory_ets:handle(my_mem),   %% 句柄 {beamai_chat_memory_ets, my_mem}

%% 2. 构建 Kernel 并启用记忆（memory filter 放 filters 列表首位 = 最外层）
K0 = beamai_kernel:new(#{}, [beamai_memory_filter:memory_filter(Store)]),
K  = beamai_kernel:add_service(K0, LlmConfig),

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

## ChatMemory Behaviour（存储层）

句柄约定 `{Module, Ref}`：Module 实现本 behaviour，Ref 是实例标识（注册名/pid/
配置元组）。调度 API 对句柄解包后转发到实现模块。

```erlang
-callback mem_get(Ref :: term(), ConvId :: binary()) -> [message()].
-callback mem_add(Ref :: term(), ConvId :: binary(), Msgs :: [message()]) -> ok.
-callback mem_clear(Ref :: term(), ConvId :: binary()) -> ok.

%% 调度 API（对 {Module, Ref} 解包）
beamai_chat_memory:mem_get(Store, ConvId).
beamai_chat_memory:mem_add(Store, ConvId, Msgs).
beamai_chat_memory:mem_clear(Store, ConvId).
```

实现本 behaviour 即可提供自定义后端（如 SQLite/Redis store）。

### 持久化后端（DETS）

`beamai_chat_memory_dets` 与 ETS 实现同构，但把消息落盘到 DETS 文件：
进程/节点重启后，同一文件即可恢复各会话历史（每次写操作后 `dets:sync` 落盘）。

```erlang
{ok, _Pid} = beamai_chat_memory_dets:start_link(my_mem, #{file => "data/chat.dets"}),
Store = beamai_chat_memory_dets:handle(my_mem),   %% 句柄 {beamai_chat_memory_dets, my_mem}
%% 用法与 ETS 后端完全一致；重启后用同一 file 重新 start_link 即恢复历史
```

## Memory Provider（策略层 / Agent 级）

存储层只管存取，**策略**（裁剪多少、要不要摘要、要不要 RAG 召回）属于 Provider。
`beamai_agent` 走的是这条路径：它在自己的 tool loop 里显式调用 provider，不经任何
kernel filter。

职责切分：

- **within-run**（一轮内跨工具迭代）的消息累积由 Agent loop 自己持有；
- **cross-run**（跨轮）的加载/持久化由 `history/2`、`append/3` 负责；
- **发送前变换**（窗口/摘要/召回）由 `prepare/3` 负责，是纯函数。

```erlang
-callback history(Ref :: term(), ConvId :: binary()) -> [message()].
-callback append(Ref :: term(), ConvId :: binary(), Msgs :: [message()]) -> ok.
-callback prepare(Ref :: term(), ConvId :: binary(), Messages :: [message()]) -> [message()].
-callback clear(Ref :: term(), ConvId :: binary()) -> ok.

%% 调度 API（对 {Module, Ref} 解包）
beamai_memory_provider:history(Provider, ConvId).
beamai_memory_provider:append(Provider, ConvId, Msgs).
beamai_memory_provider:prepare(Provider, ConvId, Messages).
beamai_memory_provider:clear(Provider, ConvId).
```

> **`history/2` 的可见性语义（read-your-writes）**：同一调用进程内，`history` 必须
> 能看到此前 `append` 已返回 ok 的全部消息——Agent tool loop 依赖该时序编排「先 load
> 历史、再 persist 新消息」。若实现为异步写（外部 DB / 消息队列），必须在 `append`
> 返回前完成写入，或保证同会话的单调读。

### 接入 Agent

`beamai_agent:new/1` 的 `memory` 键支持 5 种形态：

| 配置 | 效果 |
|---|---|
| 缺省 / `default` | 共享默认 ETS store + 无窗口（**默认就有记忆**） |
| `{window, N}` | 共享默认 store + N 条滑动窗口 |
| `{store, Handle}` | 你自己的 store + 无窗口 |
| `{Module, Ref}` | 完全自定义 provider（须实现 `beamai_memory_provider`） |
| `false` / `none` | 不启用记忆 |

```erlang
%% 最简：默认记忆，带 50 条窗口
{ok, Agent} = beamai_agent:new(#{llm => LlmConfig, memory => {window, 50}}),

%% 用自己的 store（如 DETS 持久化）
{ok, _} = beamai_chat_memory_dets:start_link(my_mem, #{file => "data/chat.dets"}),
Store   = beamai_chat_memory_dets:handle(my_mem),
{ok, Agent2} = beamai_agent:new(#{llm => LlmConfig, memory => {store, Store}}),

%% 完全自定义策略（摘要 / RAG…）
{ok, Agent3} = beamai_agent:new(#{llm => LlmConfig, memory => {my_rag_provider, Ref}}).
```

> **默认 store 是共享单例**：注册在固定名下（避免动态原子增长），各 agent 以各自的
> `conversation_id` 在其中分区。`beamai_agent` 作为 OTP 应用启动时它是监督树下的
> permanent 子进程；库式直接调用 / 裸 eunit 时回退为懒启动的孤儿单例。

自定义记忆策略只需实现那 4 个 callback。`beamai_memory_provider:default/1` 是
`beamai_memory_provider_default:new/1` 的快捷构造。

## 滑动窗口

窗口是 **provider 的 `prepare/3`** 的职责——底层存储始终保留全量历史，只在发送给 LLM
前裁剪：

```erlang
Provider = beamai_memory_provider_default:new(Store, 20),  %% 最近 20 条非系统消息
```

`beamai_memory_provider_default` 的窗口规则（见其 `safe_window/2`）：

- system 消息始终保留并置于头部；
- 非系统消息只保留最近 N 条；
- 裁剪后丢弃落在头部的孤立 `tool` 消息（避免 tool 结果找不到对应的 tool_call）。

`new/1` 等价于窗口 `infinity`，即 `prepare` 恒等返回、上下文无界。

> **Kernel 级路径当前没有窗口**：`beamai_memory_filter` 直接用 store 的全量历史替换
> messages。需要裁剪的话，要么走 Agent 路径用 provider，要么自建一个实现
> `beamai_chat_memory` 的 store 在 `mem_get` 时自行裁剪。
>
> 基于 Token 的裁剪与摘要不在 core 内，由上层实现 `beamai_memory_provider` 提供。

## 数据流（Kernel 级：带工具循环，delta 模式）

```
invoke(Kernel, [User消息], #{context := Ctx(conv_id=s1)})
└─ tool_calling_loop，delta = [User消息]
   └─ run_chat_pipeline(delta)
      ├─ memory.around_chat 前置 (order -1000，外层): mem_add(s1, delta)；messages := mem_get(s1) 全量历史
      ├─ system.around_chat 前置 (order -500，内层): messages := [SysPrompt | messages]  （不存储）
      ├─ Terminal: LLM 调用
      └─ memory.around_chat 后置 (回程): mem_add(s1, assistant 回复)
   ├─ 有 tool_calls → 执行工具（走 tool filter 链，around_tool），delta := [工具结果]，回到循环
   └─ 纯文本 → 返回 {ok, Response, Ctx}
```

memory 是单个 filter：同一个 `around_chat` 闭包前置在外层展开历史、后置在回程存回复；
洋葱链天然保证顺序，无需优先级模拟。system_prompts 作为临时 filter（仅 around_chat，
order -500，更内层）注入，位于历史展开之后、LLM 之前，**不写入存储**。

> **重入约定**：唯一隐患是外层 filter 拿同一 delta 重跑内层（同一 delta 会被存两次）。
> 因此重试类 filter 必须放在 memory filter **之外**——这与「memory 放列表首位」的约定
> 相悖，故按约定放首位即无此问题（filter 本身不设半吊子查重防护）。

## 数据流（Agent 级：provider 显式编排）

```
beamai_agent:run(Agent, UserMsg)
└─ tool loop
   ├─ history(Provider, ConvId)              读跨轮历史
   ├─ 本轮消息在 loop 内本地累积（within-run）
   ├─ prepare(Provider, ConvId, Messages)    发送前变换（窗口/摘要/召回）
   ├─ kernel invoke → LLM
   ├─ 有 tool_calls → 执行工具，结果并入本地累积，回到循环
   └─ append(Provider, ConvId, Msgs)         持久化本轮消息（cross-run）
```

Agent 路径不经任何 filter：kernel 与 memory 是两个**正交**的创建参数，kernel 管
LLM/工具调用，provider 管跨轮历史，二者任意组合。

## 关键源文件

| 文件 | 说明 |
|------|------|
| `apps/beamai_core/src/behaviours/beamai_chat_memory.erl` | 存储层 behaviour + 调度 API |
| `apps/beamai_core/src/kernel/beamai_chat_memory_ets.erl` | ETS 默认实现 |
| `apps/beamai_core/src/kernel/beamai_chat_memory_dets.erl` | DETS 持久化实现 |
| `apps/beamai_core/src/behaviours/beamai_memory_provider.erl` | 策略层 behaviour + 调度 API |
| `apps/beamai_core/src/kernel/beamai_memory_provider_default.erl` | 默认 provider（可选滑动窗口） |
| `apps/beamai_core/src/kernel/beamai_memory_filter.erl` | Memory Filter（Kernel 级接入） |
| `apps/beamai_core/src/kernel/beamai_kernel.erl` | `new/2`（filters 一次性给出）、invoke 双模式 |
| `apps/beamai_agent/src/beamai_agent_state.erl` | Agent 的 `memory` 配置解析（`setup_memory/1`） |
| `apps/beamai_agent/src/beamai_agent_tool_loop.erl` | Agent 在 loop 里显式调 history/prepare/append |
