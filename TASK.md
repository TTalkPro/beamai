# TASK: HITL / Timeline / 串行标注 / 错误分层（单 Agent，移植自 clj-agent）

> 来源：`design/hitl_timeline_serial_errors.md`（对照 clj-agent
> `agent-loop-concurrency-design.md` §9–§13 + `hitl-timeline-design.md`）。
> 承接 `design/context_split_parallel_tools.md`（已实施：writes 折叠 + 跨轮穿线 +
> 中断 saved_state）。顺序 A→B→C→D→E，每批跑全量 eunit + dialyzer。
> 不 commit：只改代码，待用户 review。ETS 为 pause_store / lineage_store 的首个适配。

## 批次 A：per-tool `:serial` 标注

- [x] A1 `beamai_tool` tool_spec 加 `serial => boolean()`（缺省 false）；docstring 说明
- [x] A2 `beamai_kernel` 加 `serial_tool/2`（按工具名查 tool_spec 的 serial 标志）
- [x] A3 `beamai_agent_utils:execute_tools/4` 并行判定改为
      `Parallel andalso length>1 andalso 批内无 serial 工具`；命中则整批退化串行
      （状态语义仍「快照+屏障折叠」不变）
- [x] A4 测试：批内含 serial → 整批串行（可观测：串行工具间时序）；全非 serial → 并行；
      serial 工具的 writes 仍正常折叠
- [x] A5 全量 eunit + dialyzer

## 批次 B：on_tool_result 实时触发

- [x] B1 `beamai_agent_utils:execute_tools` 增可选 `on_result` fun（`fun((Name,Result)->ok)`）：
      并发 collector 收到每个结果即调、串行逐个调（异常经 callbacks:invoke 不逃逸）
- [x] B2 `beamai_agent_tool_loop`：把 `on_tool_result` 触发从批后 `fire_tool_results`
      移为传入 `on_result`（实时）；保留 CallRecords 供确定顺序消费
- [x] B3 测试：实时触发（每工具完成即回调）、批内顺序不保证但 records 原序
- [x] B4 全量 eunit + dialyzer

## 批次 C：工具错误分层路由

- [x] C1 新模块 `beamai_tool_error:classify/1` → `semantic|transient|environment`
      （error_class 显式 > llm_error retryable/auth > erlang timeout/network > semantic）
- [x] C2 `beamai_tool:invoke_with_retry` 改为**仅** transient 类重试 + 指数退避；
      `retry => true | #{max_retries,initial_delay_ms}`（缺省 max_retries=2/delay=200）；
      未开 retry 不重试（幂等 opt-in）
- [x] C3 `beamai_agent_utils:execute_tools` 返回增第 4 元 `Errors`（分类后仍失败的
      `[#{id,name,class,message}]`）；run_one_tool 透出 error class
- [x] C4 `beamai_agent_tool_loop` 屏障处：Errors 含 environment 且策略 pause →
      造中断 phase=`env_retry`，中断上下文带 batch_messages/failed_calls；
      `on_env_error` 缺省 proceed，HITL agent 自动升 pause
- [x] C5 resume（env phase）：`retry`/`approved` 只重跑失败调用、按 tool_call_id 替换；
      其余 proceed；`reply` 拒收
- [x] C6 `beamai_agent_state` config 收 `on_env_error`；interrupt_state 类型加 phase 等字段
- [x] C7 测试：三类分类；transient 重试成功/耗尽；环境暂停+resume retry 替换；
      proceed 缺省；语义类照旧 errors-are-data
- [x] C8 全量 eunit + dialyzer

## 批次 D：暂停持久化（PauseStore ETS）+ resume payload

- [x] D1 behaviour `beamai_pause_store`（pause_save/3, pause_load/2, pause_clear/2；
      句柄 {Module,Ref}）
- [x] D2 ETS 实现 `beamai_pause_store_ets`（gen_server，工程学同 chat_memory_ets）
- [x] D3 快照构造（version=1，纯数据：conversation_id/paused_at/pause_reason/
      pending_tool/interrupt_state）；存档前 term_to_binary 往返校验 + warn
- [x] D4 Agent 集成：config `pause_store`；中断自动 save；终态/新 chat/resume 成功
      自动 clear；`is_interrupted`/`resume` 无本地态时透明回落 store
- [x] D5 resume payload：2/3-arity `resume(Agent,Decision[,Payload])`；审批决策词汇
      `proceed|reject|{reject,理由}|{reply,结果}`；approved+args 编辑后批准、
      reply 答复即结果（ask-user）；兼容旧文本 resume
- [x] D6 测试：PauseStore ETS 存/取/覆盖/清；跨"重启"端到端（新 agent 实例+共享 store）；
      resume payload 三形态；saved_state 跨暂停恢复
- [x] D7 全量 eunit + dialyzer

## 批次 E：Timeline（LineageStore ETS）+ writes 进历史

- [x] E1 behaviour `beamai_lineage_store`（record/2, get/2, children/2, delete/2）+ ETS 实现
- [x] E2 模块 `beamai_timeline`：`fork/3`（前缀复制+血缘；全量+源暂停→连带暂停快照）、
      `rollback/3`（截断+清暂停快照）、`lineage/2`、`ancestry/2`、`prune/2`（有子拒绝）
- [x] E3 writes 进历史：tool-result 消息带可选 `writes` 元数据（只存不发；已确认
      message_adapter 白名单剥落）；execute_tools 落消息时附带；失败工具无 writes
- [x] E4 测试：fork 前缀+血缘、暂停 fork 带快照+双分支各自 resume、rollback 截断清暂停、
      prune 拒绝有子、ancestry 回溯；writes 落历史（错误工具无 writes）
- [x] E5 全量 eunit + dialyzer

## 明确出界（各自另议）

- durable execution（批中崩溃恢复）：不做
- 多 Agent 编排 / BSP-actor：单 Agent 优先，另议
- 跨 turn 状态槽：如需走 fold-from-history，勿建快照店

## 验证

- 每批：`rebar3 eunit`（全量，基线 314）+ `rebar3 dialyzer`（EXIT=0）
- 不 commit：待用户 review 后由其决定

## 完成情况（2026-07-11 全部实施）

- **全绿**：`rebar3 eunit` 347 tests / 0 failures（基线 314 +33）；`rebar3 dialyzer` EXIT=0。
- **A serial**：tool_spec `serial`；kernel `serial_tool/2`；execute_tools 批内含 serial → 整批退化串行（状态语义仍快照+屏障折叠）。
- **B 实时回调**：execute_tools 增 `on_result` fun，并发 collector/串行逐个即时触发；tool_loop 以 `tool_result_cb` 传入（替代批后 fire）。
- **C 错误分层**：新 `beamai_tool_error:classify/1`（core，结构匹配 llm_error 免依赖 llm app）；beamai_tool retry 仅 transient + 指数退避（`retry => true|#{max_retries,initial_delay_ms}`）；run_one_tool/synth_result 把 error class 塞进 CallRecord；tool_loop 屏障 `env_pause` → 中断 phase=env_retry；resume 按 phase 分派，env retry 重跑失败调用按 id 替换（`replace_results_by_id`）；`on_env_error` 缺省 proceed、HITL 自动升 pause。
- **D 暂停持久化 + payload**：behaviour `beamai_pause_store` + ETS `beamai_pause_store_ets`；集成 `beamai_agent_pause`（save/load/clear，快照 term 往返校验）；中断自动 save、终态/resume 自动 clear、`is_interrupted`/`resume` 透明回落 store；resume/3 payload：approved(+args 执行被中断工具)/reply(答复即结果 ask-user)/rejected(+理由)，兼容旧文本 resume。
- **E Timeline**：behaviour `beamai_lineage_store` + ETS；`beamai_timeline` fork/rollback/lineage/ancestry(根在前)/prune(有子拒绝)，全量 fork+源暂停连带复制暂停快照；writes 进历史（tool 结果消息带 `writes`，message_adapter 白名单剥落，失败工具无 writes）。
- **新增模块**：beamai_tool_error、beamai_pause_store(+_ets)、beamai_agent_pause、beamai_lineage_store(+_ets)、beamai_timeline。
- **踩坑**：execute_tools/5 新 arity 忘记 export → 全链 `undef` 19 failures（教训：新 arity 必须同步 -export）。中文字符串跨文件字节比对不可靠（reject_text 前缀）→ 测试改断言 reason 子串。
- **未做（各自另议）**：durable execution、多 Agent 编排、跨 turn 状态槽。
