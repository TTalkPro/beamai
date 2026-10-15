# TASK: BeamAI 项目优化路线图

> 设计文档：[`designs/project_optimization_roadmap.md`](designs/project_optimization_roadmap.md)  
> 执行清单：[`tasks/project_optimization_roadmap.md`](tasks/project_optimization_roadmap.md)  
> 状态：待实施  
> 创建日期：2026-10-15

## 当前目标

在不重写 ChatClient / Filter / Tool Calling Manager 语义的前提下，分阶段提升项目的工程质量、运行时安全性、可观测性和可演进性。

## 活跃阶段

### Phase 0：工程止血 — 当前优先

目标：清理仓库、同步 application metadata、收敛依赖和 xref warning、补齐 CI。

- [ ] 删除 `rebar3.crashdump` 并更新 `.gitignore`。
- [ ] 补齐三个 app 的 `.app.src` `modules` 清单。
- [ ] 审计并移除 `uuid`、`esqlite`、`poolboy` 中确认无用的根依赖。
- [ ] 分类并处理 384 个 xref warning，最终让 `rebar3 xref` 输出 0 warning。
- [ ] 新增 OTP 27 / OTP 28 CI，覆盖 `xref`、`dialyzer`、`eunit`、`ct`。
- [ ] 增加 examples 独立检查 job。
- [ ] 修正 ex_doc `source_url` 和 app links 元数据。

详细批次见 [`tasks/project_optimization_roadmap.md`](tasks/project_optimization_roadmap.md#phase-0工程止血)。

### Phase 1：运行时安全

目标：移除危险默认值，增强 Tool 安全边界。

- [ ] `beamai_http_pool` 默认强制 `verify_peer`。
- [ ] 禁止系统 CA 加载失败时静默 fallback 到 `verify_none`。
- [ ] 如需 insecure TLS，必须显式配置并输出 warning。
- [ ] 补充 tool schema 校验、result size limit 和 permission metadata 设计。

## 后续阶段

1. **Phase 2：可观测性**  
   增加 telemetry dispatch、默认 logger backend，以及 LLM / Tool / Agent / HTTP Pool 关键事件。
2. **Phase 3：Provider contract test**  
   用统一离线 contract harness 保证多 Provider 的请求、tool call、SSE、usage 和错误行为一致。
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
- [ ] TLS 默认严格校验。
- [ ] Agent 关键路径具备结构化 telemetry。
- [ ] 所有非 experimental chat provider 接入 contract test。
- [ ] 存在可重复运行的性能基准与基线数据。

## 已归档任务

前序 ToolCallingManager PR1 / PR2 的执行记录已移动到：

- [`tasks/archive/tool_calling_manager_pr1_pr2.md`](tasks/archive/tool_calling_manager_pr1_pr2.md)
