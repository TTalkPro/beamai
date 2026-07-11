# HITL / Timeline / 串行标注 / 错误分层：beamai 落地设计

> **状态：✅ 已实施（2026-07-11）。** 全套 `rebar3 eunit` 347 tests / 0 failures、
> `rebar3 dialyzer` EXIT=0。实施与本文一致；逐批次记录与踩坑见 `TASK.md` 完成情况段。
> 来源：对照 clj-agent `docs/agent-loop-concurrency-design.md`（§9–§13）与
> `docs/hitl-timeline-design.md`，把单 Agent 的 HITL/Timeline/错误分层/串行标注
> 移植到 beamai。承接已实施的 `design/context_split_parallel_tools.md`
> （Tool 阶段 MapReduce：writes 折叠 + 跨轮穿线 + 中断 saved_state）。
>
> **实施要点/偏差**：① 错误分类 `beamai_tool_error` 放 core，结构匹配 llm_error
> 形状（`#{'__llm_error__':=true,...}`）避免依赖 beamai_llm app；② 瞬态重试仍在
> `beamai_tool:invoke`（handler 级，不重跑 around_tool 链——无重复日志，优于 clj 的
> 重跑整链）；③ Errors 不改 execute_tools 返回 arity，改为把 error class 塞进
> CallRecord，tool_loop 从 records 提取环境失败；④ "approved 执行被中断工具"
> 语义：对 on_tool_call gate 拦截的真实工具成立，对 interrupt_tools（提问型）用
> reply/文本；⑤ ETS 为进程内存储——"跨进程 HITL"在同节点跨 agent 实例成立，真跨
> OS 进程需另实现 behaviour（如 SQLite）。

---

## 0. 术语映射（clj-agent → beamai）

| clj-agent | beamai | 说明 |
|---|---|---|
| pause / `:paused` | interrupt / `{interrupt, Info, Agent}` | beamai 已有中断机制（interrupt_tools + on_tool_call gate） |
| gate 审批暂停 | callback 中断（on_tool_call `{interrupt,Reason}`） | 已实施 |
| `:env-retry` phase | 中断 phase `env_retry` | **本次新增**：环境类失败在屏障处暂停 |
| PauseStore | `beamai_pause_store`（behaviour）+ ETS 实现 | **本次新增**：跨进程持久化 interrupt_state |
| LineageStore / timeline | `beamai_branch_store` + `beamai_timeline` | **本次新增**：fork/rollback/lineage，ETS 优先 |
| `:state-slots` / writes 折叠 | 已实施（`beamai_context:apply_writes`） | — |
| resume payload | resume 决策 + payload | **本次新增**：approved+args / reject+reason / reply |

beamai 的 ChatMemory 协议 = clj 的 ChatMemory；`beamai_chat_memory_ets` = 会话历史 ETS store。

---

## 1. per-tool `:serial`（批 A）

- **tool_spec 加 `serial => boolean()`**（缺省 false）；`beamai_kernel:serial_tool/2` 查询。
- `beamai_agent_utils:execute_tools/4`：并行判定改为
  `Parallel andalso length>1 andalso 批内无 serial 工具`；命中 serial → 整批退化串行
  （clj §9.2.4：副作用要顺序，整批退化是唯一无需解释的语义）。
- **状态语义不变**：串行仍走「快照 + 屏障折叠」，不恢复批内穿线。
- 默认全并行；交互式审批放 gate（on_tool_call，批前串行），勿放 tool filter。

## 2. on_tool_result 实时触发（批 B）

- 现状：`fire_tool_results` 在整批收齐后按原序触发。
- 改为：每个工具**完成即触发**（进度 UX 实时性 > 顺序性，clj §9.2.5）；批内触发
  顺序不确定，需确定顺序读 CallRecords（`tool_calls_made`）。
- 实现：`execute_tools` 收一个可选 `on_result` fun（`fun((Name, Result) -> ok)`），
  并发路径在 collector 收到每个 worker 结果时调、串行路径逐个调。回调仍走
  `beamai_agent_callbacks:invoke`（异常不逃逸）。

## 3. 工具错误分层路由（批 C）

四类（clj §5/§10）：语义（缺省，errors-are-data 已就位）、瞬态（工具级幂等重试）、
环境（屏障暂停等人）、策略（gate 拒绝，已就位）。

### 3.1 分类通道 `beamai_tool_error:classify/1`

判定顺序（数据驱动）：
1. 错误 map 显式 `error_class`（工具作者 `error(#{error_class => environment,...})`
   或返回 `{error, #{error_class => ...}}`）——最高优先级；
2. canonical llm_error（`beamai_llm_error:is_error/1`）：`retryable=true` →
   `transient`；`type=auth` → `environment`；其余 → `semantic`——工具内调 provider
   失败自动获正确路由，零标注；
3. Erlang 超时/网络（`timeout` / `{request_failed,_}` / worker DOWN）→ `transient`；
4. 缺省 `semantic`。

返回 `semantic | transient | environment`。

### 3.2 瞬态重试（map 任务内，对模型透明）

- tool_spec `retry => true | #{max_retries, initial_delay_ms}`（缺省
  `#{max_retries=>2, initial_delay_ms=>200}`）；**opt-in 即承诺幂等**。
- 现 `beamai_tool:invoke_with_retry` 无条件重试任何 error → 改为**仅** `transient`
  类重试，指数退避（delay, 2x, 4x…）；`retry` 未开则不重试。
- 位置：`beamai_tool:invoke` 内，per-task（并发时在 worker 内），永不"重试整批"。

### 3.3 环境类屏障暂停

- `execute_tools` 返回新增第 4 元 `Errors :: [#{id,name,class,message}]`（分类后仍
  失败的）。返回签名变 `{Msgs, Records, NewCtx, Errors}`。
- tool_loop 屏障处（execute_and_continue）：若 Errors 含 `environment` 且策略
  `on_env_error=pause` → 造中断，phase=`env_retry`，中断上下文带 `batch_messages`
  （本批已折叠结果）+ `failed_calls` + 当前 messages/state。暂停发生在"结果交给
  模型之前"，一致快照。
- **缺省策略**：agent 层 `on_env_error` 缺省 `proceed`（无人值守不收意外中断）；
  配了 `on_tool_call`（HITL 启用）的 agent 自动升 `pause`，可显式覆盖。
- **resume（env phase）**：`retry`/`approved` → 只重跑失败调用，新结果按
  tool_call_id **替换**进原批次消息（历史无重复 tool_result；再失败再暂停）；
  其余 → `proceed`（原错误结果交模型）；`reply` 显式拒收（答复语义只属审批暂停）。

## 4. resume 带 payload（批 D 内）

resume 从"只接受人类输入文本"扩展为"决策 + payload"（clj §13 / hitl §2）：

- 2/3-arity：`resume(Agent, Decision)` / `resume(Agent, Decision, Payload)`。
- 审批暂停：`approved`（原参数执行）/ `approved`+`#{args=>新参}`（编辑后批准）/
  拒绝（`#{message=>理由}` → 结果「已拒绝执行：<理由>」）/ `reply`+`#{message=>答复}`
  （工具不执行，答复即结果，解锁 ask-user 模式）。
- 决策词汇下沉：中断/gate 契约用 `proceed | reject | {reject,理由} | {reply,结果}`。
- 兼容：旧 `resume(Agent, <<"文本">>)` 等价于当前"人类输入作为被中断工具结果"路径。

## 5. 暂停持久化（批 D）

- **behaviour `beamai_pause_store`**：`pause_save/3`（ConvId, Snapshot）、
  `pause_load/2`、`pause_clear/2`；句柄 `{Module, Ref}`（同 chat_memory 约定）。
  每 conversation-id 至多一份（再暂停覆盖）。
- **ETS 实现 `beamai_pause_store_ets`**（gen_server，工程学同 chat_memory_ets）。
  **第一个适配 ETS**。
- **快照**（纯数据，`version=1`）：`#{version, conversation_id, paused_at,
  pause_reason, pending_tool, interrupt_state}`。interrupt_state 已是纯 EDN
  （messages/saved_state/tool_calls_made/iteration…，无函数）。**不存** kernel/
  tools/callbacks/memory（代码侧 resume 重建）、不存对话历史（ChatMemory 已管）。
  存档前 term_to_binary 往返校验，不可序列化则 warn（防御）。
- **Agent 集成（opt-in 全自动）**：config `pause_store`；中断时自动 save；任何终态
  （完成/错误）、新 chat、resume 成功后自动 clear（store 始终镜像"是否有未决暂停"）；
  `is_interrupted`/`resume` 在 state 无中断态时**透明回落 store**（跨重启同
  conversation-id + 同 store 重建 agent，API 不变）。
- **出界**：批次执行中途进程崩溃恢复（durable execution）不做——只保证"暂停点"
  这个一致快照跨进程存活。

## 6. Timeline 与多分支（批 E）

### 6.1 前提：Agent 持久状态 = 对话历史（唯一真相）

对话历史（ChatMemory append-only）是唯一持久状态；state 槽是 turn 级草稿（暂停恢复
是唯一跨越点）；暂停态至多一份。**日志本身就是 timeline**，无需快照版本链。

### 6.2 一致性不变量

> 合法的 fork/rollback 点只有两种：**turn 边界、暂停点**。

并发工具失败封口：批内语义失败 collect-all 全进历史、失败 writes 丢弃（事务性），
turn 边界历史完整；环境暂停是唯一 mid-turn 逃逸点，整个封在暂停快照——fork 暂停中
的全量会话须连带复制暂停快照。

### 6.3 分支模型：fork-as-new-conversation

- **behaviour `beamai_branch_store`** + ETS 实现（血缘记录）。**第一个适配 ETS**。
- 模块 `beamai_timeline`，deps 显式传 `#{memory := Mem, branch := BS, pause_store => PS}`：
  - `fork/3`：前缀复制（mem_get 前缀 + mem_add 新 conv-id）+ 血缘记录；全量 fork 且
    源暂停 → 连带复制暂停快照；
  - `rollback/3`：破坏性截断到前 N 条（clear + 重写前缀）；清该会话暂停快照；
  - `lineage/2` / `ancestry/2`：查血缘 / 沿 parent 回溯到根；
  - `prune/2`：删分支（历史 + 暂停快照 + 血缘）；有子分支拒绝。
- 现有 ChatMemory 协议零改动；memory/pause/循环全按 conversation_id 工作——无组件
  感知"树"，换分支 = 换 conv-id 建 agent。没有"当前位置"可变状态，
  switch/goto/back/forward 全退化为换 conv-id。
- 白送场景：HITL 决策分支（暂停点 fork 两支各自 resume 不同决策）、编辑重试
  （fork 前缀不含要改的 user 消息 + 分支重发 = regenerate）。

### 6.4 writes 进历史（event-sourcing 伏笔）

- tool-result 消息带可选 `writes` 元数据（该工具的写意图）：**只进存储、不发 LLM**
  （message_adapter 白名单构建 wire 消息，`writes` 天然剥落——已确认）；
- 失败/超时/被拒调用无 writes——历史自然缺席（与事务性同真同假一致）；
- 立即价值：审计（"这个槽的值哪来的"）；升级路径：将来 slots 要跨 turn，context 可
  定义为 fold(reducers, 历史 writes)，历史仍唯一真相、时间旅行=截断重折。**不另造快照店**。

---

## 7. 逐批次实施顺序

A（serial）→ B（on_tool_result 实时）→ C（错误分层：分类+瞬态重试+环境暂停）→
D（暂停持久化 PauseStore ETS + resume payload）→ E（Timeline LineageStore ETS +
writes 进历史）。每批跑全量 `rebar3 eunit` + `rebar3 dialyzer`。不 commit（待 review）。

## 8. 明确出界

- durable execution（批中崩溃恢复）：不做；
- 多 Agent 编排 / BSP-actor 决策：另议（单 Agent 优先）；
- 跨 turn 状态槽：如需走 §6.4 fold-from-history，勿建快照店。

## 参考

- clj-agent `docs/agent-loop-concurrency-design.md`（§5 错误分层 / §9 S1 / §10 S2 /
  §11 持久化 / §12 Timeline / §13 resume payload）
- clj-agent `docs/hitl-timeline-design.md`（HITL+Timeline 整合权威参考）
- 本仓库 `design/context_split_parallel_tools.md`（前置：writes 折叠 + 中断 saved_state）
