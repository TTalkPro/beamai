# TASK: Turn 级 filter 链 + HITL 加固（filter 三链 + resume×turn + 内置 filter + heal-dangling + sensitive）

> 来源：`design/turn_filter_hitl_hardening.md`（对照 clj-agent
> `docs/filter-chain-design.md` + `docs/hitl-timeline-design.md`）。
> 承接 `design/hitl_timeline_serial_errors.md`、`design/context_split_parallel_tools.md`。
> 顺序 A→B→C→D→E，每批全量 eunit + dialyzer。不 commit：待用户 review。

## 批次 A：around_turn 链（wrap 整个工具循环）

- [x] A1 `beamai_filter` 加第三 hook `around_turn`（hook_type/hooks 类型 + 文档）
- [x] A2 `beamai_agent:run/3` 把 `run_loop`(raw) 作 terminal 包 turn 洋葱
      （复用 `beamai_filter_chain:run/4`，Phase=around_turn）；TurnRequest
      `#{messages,context,resume}` 可改写/短路/重入；TurnResult=tool loop 结果 tuple
- [x] A3 硬规则：`{interrupt,_,_}`/`{error,_}` 透传（filter 不得重入——文档约定）；
      turn filter 请求侧改写 messages（RAG 前置）、around（guardrail/预算）、重入
- [x] A4 测试：RAG 式前置 system 消息、around 观测最终结果、重入跑全新循环、
      注册顺序=层序、无 turn filter 时零开销
- [x] A5 全量 eunit + dialyzer

## 批次 B：resume 经 turn 链（一次性分派终端）

- [x] B1 `resume/3` 重组 turn 洋葱，终端 `atomics` CAS 一次性分派：
      首次=延续（消费 interrupt_state 跑 resume 逻辑）、重入=全新循环（TurnReq.messages）
- [x] B2 TurnRequest `resume => true` 标记；请求侧改写类 filter `resume` 时跳过首次改写；
      重入方置 `resume => false`
- [x] B3 测试：暂停→resume→不合格答案→校验反馈重入→合格（恰好 N 次 LLM）；
      resume 标记探针；短路保留暂停态
- [x] B4 全量 eunit + dialyzer

## 批次 C：内置 filter 模块

- [x] C1 新模块 `beamai_filters`（core）：`logging_filter/0`、`timeout_filter/1`（tool，
      超时结果标 error class=transient）、`approval_filter/1`（tool，敏感工具审批拒绝短路）
- [x] C2 `validation_turn_filter/2`（turn）：校验 completed 结果，不合格反馈重入，
      耗尽原样返回，非 completed 透传
- [x] C3 测试：logging 不改结果；timeout 短路+transient 标记（+幂等工具自动重试）；
      approval 拒绝短路；validation 重入直到合格/耗尽
- [x] C4 全量 eunit + dialyzer

## 批次 D：heal-dangling

- [x] D1 `run/3` 入口检测中断态：对历史尾部悬空 assistant(tool_calls) 的每个未决
      tool_call_id 补合成「已取消」tool 结果；清中断态 + pause_store；再正常开新 turn
- [x] D2 测试：中断后直接开新 chat → 历史无悬空 tool_calls（补齐取消结果）→ 新 turn 正常

## 批次 E：sensitive 工具标注 + approval

- [x] E1 tool_spec `sensitive => boolean()`；`beamai_tool:is_sensitive/1`、
      `beamai_kernel:sensitive_tool/2`
- [x] E2 `beamai_filters:approval_filter/1` 仅对 sensitive 工具调 ApproveFun，
      false → 拒绝短路（结果「已拒绝执行（未获批准）」，无 writes）
- [x] E3 测试：sensitive 工具被拒短路、非 sensitive 工具照常执行
- [x] E4 全量 eunit + dialyzer

## 明确出界

- RAG advisor 本体（留 turn 挂点）、advisor context map、多 Agent 编排——各自另议

## 验证

- 每批：`rebar3 eunit`（全量，基线 347）+ `rebar3 dialyzer`（EXIT=0）
- 不 commit：待用户 review 后由其决定

## 完成情况（2026-07-11 全部实施）

- **全绿**：`rebar3 eunit` 363 tests / 0 failures（基线 347 +16）；`rebar3 dialyzer` EXIT=0。
- **A around_turn 链**：`beamai_filter` 加第三 hook；`beamai_agent:run/3` 把整个 `run_loop`
  作 terminal 复用 `beamai_filter_chain:run/4`(Phase=around_turn) 包一层；TurnRequest
  `#{messages,context,resume}`、TurnResult=工具循环结果 tuple（filter 模式匹配，硬规则
  interrupt/error 透传）；`build_chat_opts` 支持 turn filter 改写的 `turn_context`。
- **B resume 经 turn 链**：`resume/3` 重组 turn 洋葱，`atomics` CAS 一次性分派终端
  （首次=延续消费 interrupt_state，重入=全新循环）；resume 逻辑重构为返回**原始** TurnResult
  tuple 的 `resume_approval_raw`/`resume_env_raw`（含 env 再暂停返回 raw interrupt tuple），
  `dispatch_turn_result` 统一分派；删 continue_resume。
- **C 内置 filter**：新模块 `beamai_filters`（core）——logging/timeout(超时 throw→transient)/
  approval(仅拦 sensitive，拒绝短路) tool 链；validation_turn_filter(校验重入) turn 链。
- **D heal-dangling**：`run/3` 入口 `heal_dangling`——处于中断态（本地或 store）时，对持久
  历史里悬空 tool_call_id 补「已取消」结果 + 清中断态/暂停快照，再开新 turn。
- **E sensitive**：tool_spec `sensitive` + `beamai_tool:is_sensitive/1`；approval_filter 直接读
  tool spec，故 `beamai_kernel:sensitive_tool/2` 未加（无消费者，approval_filter 已够）。
- **踩坑**：`atomics:new/2` 选项是 proplist `[{signed,false}]` 非 map 语法（`=>`）→ 语法错误
  连锁报一堆"函数未使用"；eunit 同进程跨用例邮箱残留（探针消息）→ 测试加 flush 隔离；
  中文字符串跨文件字节比对不可靠 → 断言 ASCII 字段（not_approved）。
- **未做**：RAG advisor 本体（留 turn 挂点）、advisor context map、多 Agent 编排。
