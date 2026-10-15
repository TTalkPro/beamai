# TASK: BeamAI 项目优化路线图

> 设计文档：[`designs/project_optimization_roadmap.md`](designs/project_optimization_roadmap.md)  
> 执行清单：[`tasks/project_optimization_roadmap.md`](tasks/project_optimization_roadmap.md)  
> 状态：Phase 0 / Phase 1 / Phase 2 实施完成；Phase 3 批次 N 实施完成（远端 CI 待确认）  
> 创建日期：2026-10-15

## 当前目标

在不重写 ChatClient / Filter / Tool Calling Manager 语义的前提下，分阶段提升项目的工程质量、运行时安全性、可观测性和可演进性。

## 活跃阶段

### Phase 0：工程止血 — 实施完成

目标：清理仓库、同步 application metadata、收敛依赖和 xref warning、补齐 CI。

- [x] 删除 `rebar3.crashdump` 并更新 `.gitignore`。
- [x] 补齐三个 app 的 `.app.src` `modules` 清单，并增加一致性检查脚本。
- [x] 审计并移除 `uuid`、`esqlite`、`poolboy` 中确认无用的根依赖。
- [x] 分类并处理 384 个 xref warning，最终让 `rebar3 xref` 输出 0 warning。
- [x] 新增 OTP 27 / OTP 28 CI，覆盖 `xref`、`dialyzer`、`eunit`、`ct`。
- [x] 增加 examples 独立编译 job。
- [x] 修正 ex_doc `source_url` 和 app links 元数据。

本地验证：`xref` 0 warning，`dialyzer` 0 warning，`eunit` 780 tests / 0 failures，`ct` 通过，examples 编译通过。  
剩余确认项：推送后由 GitHub Actions 验证 OTP 27 / OTP 28 矩阵。

详细批次见 [`tasks/project_optimization_roadmap.md`](tasks/project_optimization_roadmap.md#phase-0工程止血)。

### Phase 1：运行时安全 — 实施完成

目标：移除危险默认值，增强 Tool 安全边界。

- [x] `beamai_http_pool` 默认强制 `verify_peer`。
- [x] 禁止系统 CA 加载失败时静默 fallback 到 `verify_none`。
- [x] 如需 insecure TLS，必须显式配置并输出 warning。
- [x] 补充 tool schema 校验、result size limit 和 permission metadata 设计。

本地验证：`rebar3 xref` 0 warning，`rebar3 dialyzer` 0 warning，
`rebar3 eunit` 796 tests / 0 failures。

### Phase 2：可观测性 — 实施完成

目标：建立轻量 telemetry dispatch 与默认日志后端，覆盖 LLM、Tool、Agent、
HTTP Pool 关键路径。

- [x] 新增 telemetry dispatch 模块、公共 event map 和默认 logger backend。
- [x] 支持 handler 注入、crash 隔离、按级别开启 / 关闭。
- [x] 增加 LLM request / retry / stream 埋点。
- [x] 增加 Tool queue / start / finish / timeout / crash 埋点。
- [x] 增加 Agent run / turn / interrupt / resume / state 埋点。
- [x] 增加 HTTP Pool acquire / reuse / failure / timeout / reap 埋点。
- [x] 确保默认日志不输出 prompt、tool result 全文和 API key。

本地验证：application modules 一致，`rebar3 dialyzer` 0 warning，
`rebar3 xref` 0 warning，`rebar3 eunit` 817 tests / 0 failures。  
HTTP Pool 变更提交：`04ef802`。

### Phase 3：Provider contract test — 批次 N 实施完成

目标：建立统一离线 contract harness，先约束 Provider 请求、tool、SSE、
finish reason、usage、错误和 multimodal 行为。

- [x] 建立离线 Provider contract harness，覆盖全部 11 个内置 chat provider。
- [x] 覆盖请求体构造、tool 定义、tool call 转换、SSE 分片重组、
      finish reason 归一化、usage 归一化、错误/Retry-After 和 multimodal content。
- [x] 修复 SSE parser 丢弃不完整 data 行的问题。
- [x] 修复 Ollama OpenAI 兼容流末尾独立 usage chunk 被忽略的问题。

本地验证：application modules 一致，`rebar3 dialyzer` 0 warning，
`rebar3 xref` 0 warning，`rebar3 eunit` 843 tests / 0 failures。  
Contract harness 提交：`9ee3799`。

## 后续阶段

1. **Phase 2：可观测性**  
   增加 telemetry dispatch、默认 logger backend，以及 LLM / Tool / Agent / HTTP Pool 关键事件。
2. **Phase 3：Provider contract test 批次 O**  
   将各内置 Provider 明确接入 harness；后续补充差异配置和 CI 注册校验。
3. **Phase 4：Agent event model**  
   建立统一 Agent 事件模型，让 UI stream、telemetry、审计日志共享同一时序来源。
4. **Phase 5：性能基准**  
   用可重复 benchmark 覆盖 filter、tool dispatch、memory、stream 和 HTTP pool，再决定是否做性能重构。

## 全局验收

- [ ] `rebar3 xref`：0 warning。
- [ ] `rebar3 dialyzer`：0 warning。
- [ ] `rebar3 eunit`：全部通过。
- [ ] `rebar3 ct`：全部通过。
- [ ] OTP 27 / OTP 28 CI 全绿。
- [ ] 根项目无未用依赖。
- [x] TLS 默认严格校验。
- [x] Agent 关键路径具备结构化 telemetry。
- [ ] 所有非 experimental chat provider 接入 contract test。
- [ ] 存在可重复运行的性能基准与基线数据。

## 已归档任务

前序 ToolCallingManager PR1 / PR2 的执行记录已移动到：

- [`tasks/archive/tool_calling_manager_pr1_pr2.md`](tasks/archive/tool_calling_manager_pr1_pr2.md)
