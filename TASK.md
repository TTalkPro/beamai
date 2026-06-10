# TASK: 全项目代码审查修复（Erlang 惯用法 + 设计优化）

> 来源：2026-06-10 三 app 全量代码审查。整体架构健康（洋葱 filter / 显式 context / provider 分层），
> 问题集中在：错误处理一致性、并发防护、provider 重复代码、若干惯用法小毛病。
> 顺序：A（小修正，低风险）→ B（生产健壮性）→ C（结构调整）。每批次跑全量 eunit。

## 批次 A：惯用法小修正（低风险，先清完）

- [x] A1 `beamai_core/src/utils/beamai_utils.erl:190-193` — `to_binary/1` 列表分支两个 case 完全相同（死代码）；
      改为 `unicode:characters_to_binary/1`，顺带修正非 latin1 列表编码
- [x] A2 `beamai_core/src/kernel/beamai_chat_memory_ets.erl:92` — `Existing ++ NewMsgs` 长会话 O(n) 复制；
      内部改倒序存储（写 `lists:reverse(New, Stored)`，读 reverse）
- [x] A3 `error_logger` → `logger`（已弃用）：`beamai_llm_http_client.erl:121,125`、`beamai_http_pool.erl:476`
- [x] A4 `beamai_core/src/http/beamai_http.erl:181-182` — `request/6` 的 `_Attempt` 被忽略硬编码 0；删重载或真传
- [x] A5 `beamai_core/src/kernel/beamai_result.erl:69-91` — `pipe` 与 `pipe_while` 90% 重复；pipe 委托统一实现
- [x] A6 `beamai_core/src/kernel/beamai_prompt.erl:75-87` — 模板渲染对每个变量全量扫描 `binary:replace`；
      改 `re:run` 单遍捕获 `{{var}}` 再替换
- [x] A7 `beamai_agent/src/beamai_agent_interrupt.erl:155-158` — 畸形 tool_call 静默返回 `<<>>` 使中断匹配失效；
      加 `logger:warning` 痕迹

## 批次 B：生产健壮性

- [x] B1 `beamai_agent/src/beamai_agent_utils.erl:149-168` — `collect_tools/2` receive 无 `after`，
      一个工具卡死整个 loop 冻结 + worker 泄漏。加全局 deadline（默认可配），超时项合成 error 结果，
      kill 未完成 worker
- [x] B2 `beamai_agent/src/beamai_agent_callbacks.erl:66` — `catch _:_ -> ok` 静默吞回调异常；
      改为捕获 + `logger:warning`（含类名/原因/栈），仍不打断主流程
- [x] B3 beamai_llm 各 provider 裸 error tuple 漏网（如 zhipu `async_chat` 的 `{unexpected_response,_}`）；
      扫描全部 provider，error 路径统一过 `beamai_llm_error:from_reason`
      （注：新增 4 个 classify 分支对应的 reason 均为**已存在**的 provider 返回值——
      zhipu async_chat `{unexpected_response,_}`、zhipu 异步结果 `{task_failed,_}`、
      ollama validate_config `missing_model`、mock `not_supported`；它们经任何调用方的
      from_reason 归一时此前落入 unknown，现正确分类，非预留）
- [x] B4 `beamai_agent/src/beamai_agent_state.erl:212-216` — 孤儿 store unlink 后无监督，崩溃丢历史；
      懒启动时 `logger:warning` 提示生产应启动 OTP app
- [x] B5 `beamai_agent/src/beamai_agent_delegate.erl:74-81` — fanout 全失败仍返回 `{ok,_}`；
      partition 后全失败返回 `{error, #{all_failed => ...}}`

## 批次 C：结构调整（逐项评估，单独提交）

- [x] C1 流式 finalizer 强制配对（实现与原计划有偏差：provider 的 finalizer 是带 config 上下文的
      闭包，behaviour 回调不合适；改为 http_client stream_request/6 必填 finalizer、缺失 fail fast。
      zhipu/ollama/bailian 在 06-08 一轮已统一格式，审查时记忆过时，无需再改）
- [x] C2 `beamai_agent_tool_loop.erl:125-163` — 中断语义不对称（interrupt tool 先执行其他工具、
      callback 中断直接断）；统一前置 `find_first_interrupt`，明确策略并文档化
      （顺带修复隐性 bug：callback 中断原先不执行任何工具，resume 后 assistant 的其余 tool_call
      没有结果，形成 provider 拒绝的残缺历史；现安全工具执行、被拦截的合成 skipped 占位结果）
- [x] C3 `beamai_agent.erl:430-446` — LoopOpts 塞完整 `agent => State` 与已展开字段重复；
      只传所需字段（parallel_tools 等），解除 tool_loop 对 state 结构的耦合
- [x] C4 provider 重复代码上提：`extract_system_prompt/1`（anthropic/zhipu 各一份）→ message_adapter；
      各 provider 超时默认值 → common 集中
- [x] C5 记忆 load/persist 分散在 `beamai_agent:run/3` 与 tool_loop 两层；初始加载下沉 tool_loop，
      run 只组装 LoopOpts（对齐"loop 自管编排"决策）

## 暂不做（记录原因）

- provider_common `append_delta_text` binary 拼接 → iolist：ERTS 对 append 有优化，实测有性能问题再改
- `beamai_llm_response` 的 `'__struct__'` 标记风格统一：改动面宽、收益低
- maybe 表达式扁平化嵌套 case：随各批次顺带做，不单列

## 验证

- 每批次：`rebar3 eunit`（全量 408+ 测试）+ `rebar3 dialyzer`（如已配置）
- 批次 C 每项单独 commit，便于回滚
