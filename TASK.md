# TASK: Context 拆分（env/state/filter_states 三分区）+ 并行工具 writes 归并

> 来源：`design/context_split_parallel_tools.md`（2026-07-11 设计稿）。
> 目标：`beamai_context` 从五类混装拆为三分区（env 只读运行环境 / state 用户
> 状态槽 / filter_states 框架私有）；工具返回 `{ok, V, Writes}`（写出即数据），
> 并行批次屏障处按 tool_call 原始序折叠 writes 进 state（槽级 reducer）。
> 顺带修复 filter_states 跨轮丢失、中断 resume 丢 state 两个现存缺口。
> 顺序：A（core context 基础）→ B（core writes 通道）→ C（agent 折叠+穿线）→
> D（facade+测试+文档）。每批次跑全量 eunit。破坏面盘点见设计 §5（≈0 真实写者）。

## 批次 A：core context 三分区（基础，不引入并发语义）

- [x] A1 `beamai_core/src/kernel/beamai_context.erl` — 重构 `t()` 为三分区
      `#{env := #{kernel, conversation_id, vars}, state := #{}, '__filter_states__' := #{}}`；
      `new/0,1`、`get/2,3`、`set/3`、`set_many/2`、`delete/2`、`update/3`、`keys/1`、
      `has_key/2` 全部重定向到 `env.vars`（语义：只读运行环境注入，仅构造期写）；
      `with_kernel/get_kernel`、`with_conversation_id/conversation_id` 迁入 env；
      `filter_state/set_filter_state` 保持（框架私有分区不变）
- [x] A2 `beamai_context.erl` — **删除死字段** `trace`/`metadata` 及其 4 个访问器
      （`add_trace/get_trace/get_metadata/set_metadata`，src 零调用者；测试里的
      metadata 均属 `beamai_llm_response`，不受影响）
- [x] A3 `beamai_context.erl` — **新增 state API**：`state_get/2,3`、`get_state/1`、
      `with_state/2`；`apply_writes/3`（按 index 升序折叠 writes → 新 state：声明槽
      过 reducer、未声明槽 last-writer + 同批双写 conflict 收集，返回
      `{t(), Conflicts :: [binary()]}`）；core 保持零外部依赖
- [x] A4 全量 `rebar3 eunit`：现有 context/kernel/chat_completion 测试零回归
      （`set`→`env.vars`、`get` 读回、`get_kernel`、`conversation_id`、`filter_state` 等价）

## 批次 B：core writes 通道（tool / kernel / filter_chain）

- [x] B1 `beamai_core/src/kernel/beamai_tool.erl` — `invoke/2,3` 返回归一
      `{ok, V} | {ok, V, Writes :: map()} | {error, R}`；handler 三元组
      `{ok, V, NewCtx}` 语义变为 `{ok, V, Writes}`（arity 不变）；docstring 改
      "context 为只读运行环境，写状态经 Writes 返回"
- [x] B2 `beamai_core/src/kernel/beamai_kernel.erl` — `tool_terminal` 归一
      `{ok,V}→writes #{}`、`{ok,V,W}→writes W`；`run_tool` Response 形状
      `#{result, writes, context}`；`invoke_tool/4` 返回 `{ok, V, Writes}`
      （原第三元 Ctx 全链丢弃，无感）；`new/1` 收 `state_slots` 入 kernel settings；
      `+ state_slots/1` 查询
- [x] B3 `beamai_core/src/kernel/beamai_filter_chain.erl` — tool 链 Response 缺
      `writes` 时兜底 `#{}`（宽容自写 around_tool filter，不强制它构造 writes）
- [x] B4 全量 `rebar3 eunit`：kernel invoke_tool 测试第三元由 Ctx 变 Writes
      （`?assertMatch({ok,_,_})` 仍通过）；filter 链测试零回归

## 批次 C：agent 折叠 + 跨轮穿线 + 中断 state

- [x] C1 `beamai_agent/src/beamai_agent_utils.erl` — `run_one_tool/3` 透出
      `{Msg, CallRecord, Writes}`；`execute_concurrent`/`execute_sequential` 屏障处
      调 `beamai_context:apply_writes/3` 折叠、返回 `{Msgs, Records, NewCtx}`；
      `synth_result`（crash/timeout）与 skipped 合成结果 Writes 恒 `#{}`；
      顺带修 `:136` 注释（gather 默认 **2 分钟** 而非 5 分钟）
- [x] C2 `beamai_agent/src/beamai_agent_tool_loop.erl` — `execute_and_continue`/
      `handle_interrupt` 接 `execute_tools` 新返回；**context 跨轮穿线**（把折叠后
      NewCtx 写回下一轮 `chat_opts` 的 `context`，兑现 filter_states 跨轮存活 +
      state 跨轮可见）；中断上下文 `build_interrupt_context` 增带 `state`
- [x] C3 `beamai_agent/src/beamai_agent.erl` + `beamai_agent_state.erl` — config
      收 `state_slots` 透传给内部 kernel；resume 从中断上下文 `with_state` 恢复
      state 进 context
- [x] C4 全量 `rebar3 eunit`：现有 agent/并行/中断测试零回归

## 批次 D：facade + 新测试 + 文档

- [x] D1 `beamai_core/src/beamai.erl` — facade context 构造 API 与新结构对齐
      （`context/0,1` 走 `beamai_context:new`，预计零改动，确认即可）
- [x] D2 新增测试（设计 §7）：快照隔离（同批 B 看不到 A 写）、reducer 折叠
      （conj 槽两写全收）、last-writer 按 index 序而非完成序、错误/超时/skipped
      writes 归零、conflict 告警、跨轮可见（本轮写下轮读）、filter_states 跨轮
      存活回归、中断-resume state 恢复、串行/并行状态语义一致、`fun/1` 工具可返 writes
- [x] D3 文档：`README`（context 章节）、`design/context_split_parallel_tools.md`
      状态更新为已实施

## 明确不做（本 TASK 范围外，各自独立立项）

- per-tool `:serial` 标注（副作用工具整批退化串行）：现有 agent 级 `parallel_tools`
  总开关兜底，暂不细化
- 工具错误分层路由（瞬态重试 / 环境类暂停，clj-agent S2 对应物）
- 暂停持久化（state + messages 已可序列化，PauseStore 协议另议）
- `on_tool_result` 改实时触发：维持整批收齐后按原序触发

## 验证

- 每批次：`rebar3 eunit`（全量，当前基线约 217+ 测试）+ `rebar3 dialyzer`（如已配置）
- 不 commit：只改代码，待用户 review 后由其决定提交（遵 `no-commit-without-review`）

## 完成情况（2026-07-11 全部实施）

- **全绿**：`rebar3 eunit` 314 tests, 0 failures（基线 295 +19 新测试）；`rebar3 dialyzer` EXIT=0 零警告。
- **A**：beamai_context 三分区（env/state/'__filter_states__'），删 trace/metadata 死字段，
  新增 state_get/get_state/with_state/apply_writes/variables。顺带修 beamai_prompt：渲染
  context 时经 variables/1 取 env.vars（否则读不到用户变量）。
- **B**：beamai_tool tool_result 三元语义改 writes；beamai_kernel tool_terminal 归一 writes、
  run_tool Response `#{result,writes,context}`、invoke_tool 返回 `{ok,V,Writes}`、加 state_slots/1；
  filter_chain 由 run_tool 的 `maps:get(writes,_,#{})` 兜底（无需改动）。
- **C**：beamai_agent_utils execute_tools 返回三元组、finalize 屏障折叠、synth_result 零 writes、
  修 :136 注释（2 分钟）；tool_loop 跨轮穿线 context（with_ctx）+ 中断上下文带 state；
  beamai_agent resume 恢复 saved_state；beamai_agent_state build_kernel 收 state_slots。
- **D**：新增 beamai_context_tests（12）、beamai_agent_writes_tests（7，含跨轮/中断-resume）。
- **踩坑记录**：给 interrupt_state 加 saved_state 字段后 dialyzer 报 subagent_manager 的
  interrupt 分支「can never match」——根因是 `interrupt_state()` 是**封闭 map 类型**
  （Erlang typespec `#{k:=t}` 不容额外键），新字段未同步进类型声明即污染 agent_state()
  逐层收窄 run 返回类型。修法：把 `saved_state := map()` 加进
  `beamai_agent_state:interrupt_state()` 类型。教训：往受类型约束的 map 加字段必须同步 -type。
