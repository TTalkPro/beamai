# Turn 级 filter 链 + HITL 加固：beamai 落地设计

> **状态：✅ 已实施（2026-07-11）。** 全套 `rebar3 eunit` 363 tests / 0 failures、
> `rebar3 dialyzer` EXIT=0。实施与本文一致，逐批次与踩坑见 `TASK.md` 完成情况段。
> 来源：对照 clj-agent `docs/filter-chain-design.md`（filter 三链 :tool/:chat/:turn）、
> `docs/hitl-timeline-design.md`（heal-dangling / resume×turn 链）、
> `docs/agent-loop-concurrency-design.md` §14（turn 链推导）。
> 承接已实施的 `design/context_split_parallel_tools.md`、`design/hitl_timeline_serial_errors.md`。
>
> **实施要点/偏差**：① turn 链复用现有 `beamai_filter_chain`（Phase=around_turn），
> TurnResult 为工具循环结果 tuple（不转 map），filter 直接模式匹配、`{error,_}`(2-tuple)/
> `{interrupt,_,_}`/`{ok,_,_,_}` 均落 filter_chain 的 `Resp->Resp` 分支（turn filter 不走
> FCtx 合并，用闭包持状态）；② resume 一次性分派用 `atomics` CAS；③ `sensitive_tool/2`
> 未加——approval_filter 直接读 tool spec 的 is_sensitive，无需 kernel 查询。

---

## 0. 现状盘点与缺口

beamai 现有 filter 两条链（`around_chat` / `around_tool`，洋葱式，FCtx 私有状态），
tool/chat 契约已收紧（context 只读、tool 返回 writes）。**缺口**：

1. **无 `around_turn` 链**（最大缺口）——用户无法在"整个 turn"外包 around 逻辑：
   每 turn 一次的 RAG 注入、最终答案 guardrail/校验重试、turn 级预算、evaluator 递归。
2. **无内置 filter**——logging / timeout / approval（tool 链）、validation（turn 链）全缺；
   用户从零手写。
3. **resume 不经 turn 链**——被人打断过的 turn，其最终答案对 guardrail/校验类 filter 是盲区。
4. **无 heal-dangling**——中断态下放弃 resume、直接开新 chat：历史尾部悬空
   assistant(tool_calls) 无对应结果，provider 拒绝残缺历史。
5. **无 `sensitive` 工具标注**——approval 类控制无声明入口。

---

## 1. `around_turn` 链（批 A）

一个机制三个粒度（对照 clj filter-chain §0）：

| 链 | 包什么 | 频次 | 典型职责 |
|---|---|---|---|
| `around_tool` | 单次工具执行 | 每 tool call（并行任务内） | 超时/审批短路/限流/日志 |
| `around_chat` | 单次 LLM 调用 | 循环内每轮 | memory/请求改写 |
| **`around_turn`** | **整个工具循环** | **每 turn 一次** | RAG 注入/最终答案 guardrail/turn 预算/evaluator 递归 |

- **hook**：`beamai_filter` 加第三个 hook `around_turn`，同洋葱机制（复用
  `beamai_filter_chain:run/4`，Phase=around_turn）。
- **契约**：TurnRequest `#{messages := 入口 delta, context := 初始 ctx, resume := bool}`
  （可改写 messages/context、短路、多次 `Next` 重入）；TurnResult = tool loop 结果
  tuple `{ok, Response, ToolCallsMade, Iterations} | {interrupt, Type, Ctx} | {error, Reason}`
  （tagged tuple，filter 直接模式匹配）。
- **组装点**：`beamai_agent:run/3` 把整个 `run_loop` 作 terminal 包一层 turn 洋葱
  （循环本体不动——循环与 gate/HITL/暂停深度耦合，为优雅重构不值）。
- **递归重入**：turn filter 可多次 `Next(Req)`（校验重试/evaluator）。重入 messages 应为
  **新 delta**，完整上下文由 memory filter 拼接（递归类 turn filter 依赖 memory 在位）；
  每次重入获全新 max_iterations 预算。
- **硬规则**：`{interrupt,_,_}` / `{error,_}` 结果**必须透传、不得重入**（暂停态上重试
  破坏 HITL 语义）。文档 + 内置 validation filter 遵守。
- 执行顺序 = filters 注册顺序（order 越小越外层），与现有一致。

## 2. resume 经 turn 链（一次性分派终端，批 B）

暂停发生时原 turn 链调用已随 `{interrupt,...}` 透传退栈；resume 是全新调用。若 resume
不进 turn 链，guardrail/校验对"被打断过的 turn"最终答案盲区。

- `resume/3` 重组同一条 turn 洋葱，终端**一次性分派**（`atomics` CAS 标志）：
  - **首次进入**：暂停 turn 的延续——消费 interrupt_state 跑 resume 逻辑
    （resume_approval / resume_env，内部走 raw run_loop）；messages 忽略（延续无入口消息）；
  - **递归重入**：turn 已完成一次后的反馈轮——TurnReq.messages 为新 delta，跑全新循环。
- TurnRequest 扩展：resume 进入带 `resume := true`；请求侧改写类 filter 应 `resume=true` 时
  跳过首次改写（延续无入口消息），重入方构造重入 req 时置 `resume => false`。
- 短路（不调 Next）合法：延续不发生，interrupt_state 保持未消费（可再次 resume）。

## 3. 内置 filter（批 C）

新模块 `beamai_filters`（core），对标 clj filter-chain §3：

| filter | 链 | 说明 |
|---|---|---|
| `logging_filter/0` | tool | 调用前后日志（debug） |
| `timeout_filter/1` | tool | 超时短路，结果标 `error{class=transient}`（声明 retry 的幂等工具可自动重试） |
| `approval_filter/1` | tool | 敏感工具审批，拒绝短路（交互式改用 gate/on_tool_call） |
| `validation_turn_filter/2` | turn | 最终答案校验：不合格把原因作反馈消息重入循环；耗尽原样返回；非 completed 透传 |

## 4. heal-dangling（批 D）

中断态下开新 chat（放弃 resume）：run/3 入口若检测到 agent 处于中断态，先对历史尾部悬空的
assistant(tool_calls) **补合成"已取消"tool 结果**（每个未决 tool_call_id 一条），清中断态 +
清 pause_store，再正常开新 turn。保证 provider 不见残缺历史。

## 5. `sensitive` 工具标注（批 E）

- tool_spec 加 `sensitive => boolean()`（缺省 false）；`beamai_tool:is_sensitive/1`、
  `beamai_kernel:sensitive_tool/2`。
- `approval_filter(ApproveFun)`：仅对 `sensitive` 工具调 ApproveFun，返回 false → 拒绝短路
  （结果「已拒绝执行（未获批准）」，无 writes）。非交互式；交互式审批仍走 gate。

---

## 6. 逐批次顺序

A（turn 链 + run 包裹）→ B（resume 经 turn 链一次性分派）→ C（内置 filter）→
D（heal-dangling）→ E（sensitive + approval）。每批全量 eunit + dialyzer。不 commit。

## 7. 明确出界

- RAG advisor 本体（需 vector store）——留 turn 挂点，不做本体；
- advisor context map（跨 filter 状态）——请求 map 透传 + 闭包 FCtx 已够；
- 多 Agent 编排——另议。

## 参考

- clj-agent `docs/filter-chain-design.md`（filter 三链契约、递归重入、resume×turn 一次性分派）
- clj-agent `docs/hitl-timeline-design.md`（heal-dangling、resume payload）
- 本仓库 `design/hitl_timeline_serial_errors.md`、`design/context_split_parallel_tools.md`
