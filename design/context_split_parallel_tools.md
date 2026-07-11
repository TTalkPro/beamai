# Context 拆分设计：运行环境与用户状态分离，并行工具 writes 归并

> **状态：✅ 已实施（2026-07-11）。** 全套 `rebar3 eunit` 314 tests / 0 failures、
> `rebar3 dialyzer` 零警告。实施与逐模块计划（§6）一致；一处踩坑记录见文末 §9。
> 来源：对照 clj-agent `docs/agent-loop-concurrency-design.md`（Tool 阶段
> MapReduce 设计定稿）对 beamai 的 rethink。

---

## 0. 结论速览

1. **现状盘点**：`beamai_context` 一个 map 混装五类身份不同的东西
   （kernel 引用、filter 私有状态、conversation_id、用户变量、死字段
   trace/metadata），靠 `normalize_key` + `__` 保留 key 在同一命名空间里
   强行区分。
2. **可写通道零真实写者**：工具可返回 `{ok, Value, NewCtx}` 改写 context，
   但全链（并行/串行/tool loop/chat loop）都丢弃返回的 Ctx。与 clj-agent
   盘点结论一致——破坏性变更实际无痛。
3. **拆分为两半**：**env（只读运行环境）**= kernel + conversation_id +
   调用方注入变量，框架自持、不可序列化也无需序列化；**state（用户状态槽）**
   = 显式声明的槽 + reducer，纯数据、可序列化，是工具间共享状态的唯一通道。
4. **向并行工具靠拢**：工具写出即数据（`{ok, Value, Writes}`），并行批次
   在屏障处按 tool_call 原始序纯折叠 writes 进 state——竞态被执行模型消灭，
   不是被管理。beamai 现有并行骨架（spawn_monitor + 原序 gather +
   collect-all）已就位，本设计补上它缺失的状态语义：现状是"丢弃写"，
   目标是"归并写"。
5. **失败工具的 writes 不参与折叠** = 免费的单工具事务性。错误/超时/
   skipped/中断合成结果一律零 writes。
6. **顺带修复两个现存缺口**：
   - filter 私有状态"跨轮存活"承诺不成立（tool loop 每轮丢弃返回 context，
     filter_states 每轮被重置）——loop 改为跨轮穿线 context 后自动修复；
   - 中断/resume 丢状态（中断上下文只带 messages 不带 state）——中断上下文
     增带 state 分区，resume 恢复。
7. **顺序教训（clj-agent 血泪）**：先定状态语义，再动 context 结构。
   本文一次把两者定完：拆分是 writes 归并语义的产物，不是独立重构。

---

## 1. 现状盘点（证据基础）

### 1.1 `beamai_context:t()` 里装了什么

| 内容 | key 形式 | 性质 | 实际消费者 |
|---|---|---|---|
| `'__context__' => true` | atom | 类型标记 | 框架自检 |
| `kernel` | atom | 框架运行时引用，invoke_chat/invoke_tool 入口自动绑定 | filter/工具经 `get_kernel` 组合调用（子 agent、agentic tool） |
| `'__filter_states__'` | atom | filter 私有状态槽（按 filter 名隔离） | 仅 `beamai_filter_chain` 投影/合并 |
| `trace`、`metadata` | atom | 调试跟踪/元数据 | **src 零调用者**（只有 context.erl 自身访问器），死字段 |
| `<<"__conversation_id__">>` | 保留 binary | 身份/环境数据（只读性质） | memory_filter 定位会话；delegate `owner/1` 归属判定 |
| 任意顶层 binary key | binary | 用户变量 | `new/1` 注入；工具 handler `fun/2` 只读 |

命名空间挤压的适配成本：`normalize_key`（atom→binary 区分用户变量与内部
字段）、`__` 前缀保留 key、"内部字段不经 normalize_key"特判。拆分后全部消失。

### 1.2 消费者盘点：可写通道零真实写者（2026-07-11 已完成）

- `beamai_tool` handler 可返回 `{ok, Value, NewCtx}`（kernel
  `tool_terminal` 认这个形状），但：
  - agent 并行/串行路径 `run_one_tool` 丢弃 `invoke_tool` 返回的 `_Ctx`
    （docstring 明言"工具对 context 的修改不回写"）；
  - tool loop 的 chat 侧同样 `{ok, Response, _Ctx}` 丢弃；
  - 串行路径 `execute_sequential` 也不穿线（每个工具拿同一个 Context）。
- 内置 filter 全部只读：memory_filter 只读 `conversation_id`（`_FCtx`）；
  system_prompt filter 只改 messages；
- delegate 工具 handler 只读 Ctx（owner/seed）；
- src/examples 中**没有任何**工具真的写 context。

结论：收紧写通道的破坏面 ≈ 0。

### 1.3 现存缺口（本设计顺带修复）

1. **filter 私有状态跨轮丢失**：`beamai_filter_chain` 注释承诺"私有状态随
   共享 context 透传，跨工具循环各轮存活"，但 `beamai_agent_tool_loop:iterate`
   丢弃 `invoke_llm` 返回的 context，下一轮又从 `chat_opts` 取**原始**
   context——filter_states 每轮被重置，承诺不成立。
2. **中断/resume 丢状态**：`build_interrupt_context` 携带 messages 但不带
   context——一旦 state 有内容（本设计落地后），中断前累积的槽会在 resume
   时静默丢失（clj-agent §11.2 同款缺口）。
3. **工具间共享状态只能绕道 LLM**：并行工具靠"丢弃写"消灭竞态，代价是
   工具之间没有任何带外状态通道（只能把数据塞进 tool 结果让模型转述）。

---

## 2. 对照（姊妹项目与业界）

| 参照 | 要点 | beamai 采纳 |
|---|---|---|
| clj-agent S1（已实施） | map on snapshot / 写出即数据 `:writes` / 屏障按原序纯折叠 / 槽级 reducer / ToolContext 回归只读 | 全部采纳，形状按 Erlang 惯例调整 |
| Spring AI ToolContext | 只读环境数据（tenantId 类），不进模型、工具间不传状态 | env 分区语义与之对齐 |
| LangGraph channels/reducers | 合并语义落在状态槽上，不落在批次上 | state_slots + reducer |
| beamai 现状 | 并行骨架（spawn_monitor、原序 gather、collect-all、gate 批前串行）已就位 | 保留，只补 writes 通道 |

---

## 3. 目标形状

### 3.1 新 `beamai_context:t()`：三分区

```erlang
-type t() :: #{
    '__context__' := true,
    %% ── env：只读运行环境（框架自持，不序列化）─────────────
    env := #{
        kernel := beamai_kernel:kernel() | undefined,
        conversation_id := binary() | undefined,
        vars := #{binary() => term()}      %% 调用方注入，只读
    },
    %% ── state：用户状态槽（纯数据，可序列化）───────────────
    state := #{binary() => term()},
    %% ── 框架内部：filter 私有状态（框架自管，用户不可见）───
    '__filter_states__' := #{binary() => map()}
}.
```

- **env**：`with_kernel`/`get_kernel`、`with_conversation_id`/`conversation_id`
  迁入 env 分区；`new/1` 注入的变量收进 `env.vars`（`get/2,3` 读它，语义
  改为**只读环境**——`set/set_many/delete/update` 从公开 API 移除或仅限
  构造期使用）。
- **state**：新 API `state_get/2,3`；**工具不直接写**，写意图经 `Writes`
  返回（§4）。跨轮由 loop 穿线；可整体取出/放回
  （`get_state/1`、`with_state/2`），为暂停持久化提供序列化边界。
- **filter_states**：保留于 context（filter_chain 继续投影/合并），但归类
  为框架分区；跨轮存活由 loop 穿线兑现（修复缺口 1.3-1）。
- **删除**：`trace`、`metadata`（死字段）、`normalize_key` 公开导出、
  `<<"__conversation_id__">>` 保留 key 把戏。

### 3.2 状态槽声明（reducer 落 kernel）

```erlang
%% kernel 构造期声明（示意 API，实施时定稿）
Kernel = beamai_kernel:new(#{
    state_slots => #{
        <<"notes">>  => #{init => [], reduce => fun(Acc, V) -> [V | Acc] end},
        <<"budget">> => #{init => 0,  reduce => fun erlang:'+'/2}
    }
}).
```

- 未声明槽默认 **last-writer**（按 tool_call index 序，确定性）；同批多工具
  写同一未声明槽 → conflict，`logger:warning` 告警；
- 绝大多数工具不写 state → 零声明；
- agent 层 `beamai_agent:new` 配置透传 `state_slots` 给内部 kernel。

### 3.3 折叠纯函数

```erlang
%% beamai_context 内实现，core 保持零依赖
-spec apply_writes(t(), [{Index :: pos_integer(), Writes :: map()}],
                   Slots :: map()) ->
    {t(), Conflicts :: [binary()]}.
```

按 Index 升序 fold；同样的 LLM 输出 + 同样的工具结果 ⇒ 同样的新 state
（reducer 不需要交换律，折叠序钉死即确定性）。

---

## 4. 执行模型：并行工具的 writes 归并

### 4.1 流程（对现有骨架的增量）

```
tool_calls [tc1 tc2 tc3 ...]（LLM 一轮返回）
     │
     ├─ ① 中断检测（现状保留：interrupt tool + on_tool_call 回调，批前串行）
     ├─ ② map：每 tool 一个 spawn_monitor worker（现状保留）
     │      worker 拿轮初 Context 快照（env + state 冻结，现状已如此）
     │      worker 回传 {Msg, CallRecord, Writes}          ← 新增 Writes
     ├─ ③ 屏障：collect-all + 全局 deadline + DOWN/超时合成（现状保留）
     │      合成结果（crash/timeout/skipped）Writes 恒为 #{}  ← 事务性白拿
     ├─ ④ reduce：按 tool_call 原始 index 折叠 writes → 新 state
     │      apply_writes(Ctx, OrderedWrites, StateSlots)     ← 新增
     └─ ⑤ messages/records 按原序排回（现状保留，形状不变）
```

- **跨轮穿线（新增）**：④ 的新 context 由 loop 穿回下一轮 `chat_opts`
  （同时兑现 filter_states 跨轮存活）。删除的只是"批内工具改 context"
  这个从未被使用的语义。
- **串行路径同语义**：`parallel_tools => false` 只改执行时序（顺序 map），
  状态语义统一为「快照 + 屏障折叠」——一个 API 只有一种状态语义，不引入
  批内穿线（clj-agent §9.2 决策 4 同款）。
- **`on_tool_result` 回调**：维持现状（整批收齐后按原序触发），与 clj-agent
  的实时触发取舍不同；如需实时性另行立项，不混入本变更。

### 4.2 工具返回形状

```erlang
%% handler 返回（binary 结果为 Value 本体）：
{ok, Value}                    %% 不写 state（绝大多数工具）
{ok, Value, Writes :: map()}   %% 写意图是纯数据；同 key 多次写自己先合并
{error, Reason}                %% 照旧；error 恒零 writes
```

- 三元组 **arity 不变、第三元语义变**：从"改好的 NewCtx"变为"writes map"。
  盘点显示第三元现无任何消费者（全链丢弃），实际破坏面为零；
- `fun/1`（不收 context）的工具**同样可以**返回 writes——读环境与写状态
  正交，写状态不再被迫声明收 context；
- kernel `tool_terminal` 归一化：`{ok, V}` → `#{result => V, writes => #{}}`；
  `{ok, V, W}` → `#{result => V, writes => W}`。

### 4.3 filter 链契约收紧

- tool 链 Request `#{tool, args, context}` 保留：filter 的合法职责是**控制**
  （日志/超时/审批/限流——盘点显示没有一个需要改状态），`args` 可改写、
  可短路；
- tool 链 Response 从 `#{result, context}` 改为 `#{result, writes, context}`，
  其中 `context` 仅承载 filter_states 合并（框架用），**filter 不得注入
  state 写**——filter 私有状态走 FCtx，用户状态只属于工具 writes；
- chat 链 `#{messages, opts, context}` → `#{response, context}` 形状不变，
  chat 侧无 writes 通道（Plan 阶段天然串行，无归并问题）。

### 4.4 中断路径

- `handle_interrupt` 中 SafeCalls 的执行同样走 ②-④：安全子集的 writes
  正常折叠，skipped 合成结果零 writes；
- 中断上下文新增 `state` 字段（`get_state(Ctx1)` 的纯数据快照）；
  resume 时 `with_state` 恢复进新 context——修复缺口 1.3-2；
- state 分区纯数据 + messages 纯数据 ⇒ 中断上下文整体可
  `term_to_binary`/JSON 序列化，为将来暂停持久化（跨进程 resume）打底，
  但持久化本身**不在本设计范围**。

---

## 5. 契约变更清单（破坏面）

| 变更 | 旧 | 新 | 实际影响 |
|---|---|---|---|
| `beamai_context:t()` 结构 | 五类平铺一个 map | env/state/filter_states 三分区 | 直接 maps 操作 context 的代码需改（src 内无，仅测试） |
| 工具 handler 三元组返回 | `{ok, V, NewCtx}` | `{ok, V, Writes}` | 零真实写者；直调用户需改 |
| `beamai_kernel:invoke_tool` 返回 | `{ok, V, Ctx}` | `{ok, V, Writes}` | 现有调用点全部丢弃第三元，无感 |
| `beamai_kernel:invoke_chat(_stream)` 返回 | `{ok, Resp, Ctx}` | 不变（Ctx 含更新后 filter_states） | 无 |
| tool 链 Response | `#{result, context}` | `#{result, writes, context}` | 自写 around_tool filter 若构造 Response 需补 `writes`（可由链兜底默认） |
| `beamai_context:set/set_many/delete/update` | 公开 API | 移除（或仅构造期） | src 内零调用者 |
| `trace/metadata` API | 存在 | 删除 | 零调用者 |
| `execute_tools` 返回 | `{Msgs, Records}` | `{Msgs, Records, NewCtx}` | agent 内部两个调用点同步改 |
| tool loop 跨轮 context | 每轮取原始 context | 穿线上一轮返回 context | 行为修复（filter_states 存活、state 可见） |
| `run/messages/records` 消息形状 | — | 不变 | 无 |

## 6. 逐模块修改计划

1. `beamai_core/src/kernel/beamai_context.erl`：三分区重构；
   `+apply_writes/3`（默认 last-writer + conflict 收集）、
   `+state_get`、`+get_state/with_state`；env 访问器迁移；删 trace/metadata。
2. `beamai_core/src/kernel/beamai_tool.erl`：`invoke` 返回归一
   `{ok,V}|{ok,V,Writes}`；docstring 改只读 env 语义。
3. `beamai_core/src/kernel/beamai_kernel.erl`：`tool_terminal` 归一 writes；
   `run_tool` Response 新形状；`invoke_tool` 返回 `{ok,V,Writes}`；
   `new/1` 收 `state_slots` 入 settings。
4. `beamai_core/src/kernel/beamai_filter_chain.erl`：Response 缺 `writes`
   时兜底 `#{}`（宽容自写 filter）。
5. `beamai_agent/src/beamai_agent_utils.erl`：`run_one_tool` 透出 writes；
   `execute_concurrent/execute_sequential` 屏障折叠、返回三元组；
   `synth_result` 零 writes；顺带修 136 行注释（默认 2 分钟，非 5 分钟）。
6. `beamai_agent/src/beamai_agent_tool_loop.erl`：`execute_and_continue`/
   `handle_interrupt` 接新返回、context 跨轮穿线（写回 `chat_opts`）；
   中断上下文 `+state`。
7. `beamai_agent/src/beamai_agent.erl` / `beamai_agent_state.erl`：config 收
   `state_slots` 透传；resume 恢复 state。
8. `beamai_core/src/beamai.erl` facade：context 构造 API 对齐。
9. 文档：README（context 章节）、本文件状态更新。

## 7. 测试清单

- 并行互等证明（现有测试保留）；快照隔离：同批 B 看不到 A 的写（显式钉新语义）；
- reducer 折叠：声明 conj 型槽两工具写全收；last-writer 按 index 序而非完成序
  （故意让 index 大者先完成）；
- 错误/超时/DOWN 合成/skipped 的 writes 归零；conflict 告警（未声明槽双写）；
- 跨轮可见：本轮工具写、下轮工具读（state_get）；
- filter_states 跨轮存活回归（修复前失败、修复后通过）；
- 中断-resume：中断前累积的 state 恢复后可读（note-writer/note-reader 同款）；
- `parallel_tools => false` 串行路径与并行路径状态语义一致；
- `fun/1` 工具返回 writes 可用；messages/records 原序不变。

## 8. 待决问题

- [ ] `state_slots` 声明的最终形状：kernel opt（本文倾向）还是独立注册表；
- [ ] conflict 的策略：warn（本文倾向）还是可配置 error；
- [ ] `env.vars` 是否保留 `set` 构造期写入，还是只允许 `new/1` 一次性注入；
- [ ] per-tool `:serial` 标注（副作用工具整批退化串行）：本设计不含，
  留待后续——现状已有 agent 级 `parallel_tools` 总开关兜底；
- [ ] 工具错误分层路由（瞬态重试/环境类暂停，clj-agent S2 对应物）：
  独立立项，不混入本变更；
- [ ] 暂停持久化（state + messages 已可序列化，store 协议另议）。

## 参考

- clj-agent `docs/agent-loop-concurrency-design.md`（Tool 阶段 MapReduce
  设计定稿 + S1/S2/HITL 实施记录）
- Spring AI Reference, [Tool Calling](https://docs.spring.io/spring-ai/reference/api/tools.html)（ToolContext 只读语义）
- LangChain Docs, [LangGraph runtime (Pregel)](https://docs.langchain.com/oss/python/langgraph/pregel)（channels + 槽级 reducer）
- 本仓库：`design/kernel_memory_filter_redesign.md`（context 不再记录消息/历史的前次收紧）

---

## 9. 实施记录（2026-07-11）

与 §6 逐模块计划一致落地。几处与设计稿的偏差/补充：

1. **beamai_prompt 顺带修复**：`beamai_prompt:render(Template, Context)` 原先 fold 整个
   context map 取变量，三分区后用户变量移入 `env.vars`，读不到 → 新增
   `beamai_context:variables/1`，render 经它取 env.vars（保持旧语义：模板变量 = 只读环境变量）。
2. **filter_chain 无需改动**：短路 filter 构造的无 `writes` Response 由 `run_tool` 的
   `maps:get(writes, Resp, #{})` 兜底，B3 自动满足。
3. **中断字段命名**：中断上下文（tool_loop 侧）用 `state` 键；存入 `interrupt_state` 时命名
   `saved_state`（避免与其它语义混淆），resume 读 `saved_state` 恢复。
4. **dialyzer 踩坑（重要）**：给 `interrupt_state()` map 加 `saved_state` 字段时，忘了同步
   `beamai_agent_state:interrupt_state()` 的 **-type 声明**。Erlang typespec 里 `#{k := t}` 是
   **封闭**的（不容未列出的键），于是构造出的 IntState 不再匹配 `interrupt_state()` →
   污染 `agent_state()` → dialyzer 逐层推断 `beamai_agent:run/2` 不再返回 `{interrupt,...}` →
   在 `beamai_subagent_manager` 报 interrupt 分支「can never match」。**教训：往受 -type 约束
   的 map 加字段，必须同步类型声明**；这类"远处报错、近处改动"的 dialyzer 警告要顺着
   类型流向回溯，而非只看报错点。
5. **未做项**（与 §6/§8 一致，各自独立立项）：per-tool `:serial`、错误分层路由、暂停持久化、
   `on_tool_result` 实时触发。

测试补充：`beamai_context_tests`（三分区 + apply_writes 折叠/last-writer/conflict/reducer 累积）、
`beamai_agent_writes_tests`（并行/串行折叠、快照隔离、last-writer 按 index 非完成序、错误
writes 归零、fun/1 writes、跨轮可见、中断-resume state 恢复）。
