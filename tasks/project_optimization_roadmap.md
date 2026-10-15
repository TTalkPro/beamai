# TASK: BeamAI 项目优化路线图

> 设计文档：[`designs/project_optimization_roadmap.md`](../designs/project_optimization_roadmap.md)  
> 状态：Phase 0 / Phase 1 实施完成；Phase 2 实施完成，等待远端 CI 确认 OTP 27 / OTP 28 矩阵  
> 原则：先建立质量门槛和安全默认值，再做可观测性与性能优化。所有阶段保持公共 API 兼容。

## 当前基线

- [x] 记录分析基线：`rebar3 eunit` 780 tests / 0 failures。
- [x] 记录分析基线：`rebar3 dialyzer` 0 warning。
- [x] 记录分析基线：`rebar3 xref` 384 warnings。
- [x] 记录分析基线：无 CI workflow。
- [x] 记录分析基线：`.app.src` modules 与实际模块不同步。
- [x] 记录分析基线：根项目存在疑似未用依赖。
- [x] 记录分析基线：`beamai_http_pool` 存在 `verify_none` fallback。

## Phase 0：工程止血

### 批次 A：仓库清理

- [x] A1 删除 Git 跟踪的 `rebar3.crashdump`。
- [x] A2 在 `.gitignore` 增加 `rebar3.crashdump`。
- [x] A3 确认工作区没有其他误提交构建产物。

### 批次 B：application metadata 修正

- [x] B1 补齐 `apps/beamai_agent/src/beamai_agent.app.src` 的 `modules`：
      至少包括 pause、branch、timeline、tool calling manager、batch worker、
      concurrent/sequential manager 相关模块。
- [x] B2 补齐 `apps/beamai_core/src/beamai_core.app.src` 的 `modules`：
      至少包括 `beamai_filters`、`beamai_json_schema`、`beamai_token_stream`、
      `beamai_tool_error`、`beamai_tool_index*`、`beamai_tool_search`。
- [x] B3 补齐 `apps/beamai_llm/src/beamai_llm.app.src` 的 `modules`：
      至少包括 parser、error、mock provider、HTTP provider 相关模块。
- [x] B4 增加或明确一个本地检查方式，验证 `.app.src` modules 与 `src/**/*.erl` 一致。

### 批次 C：依赖审计

- [x] C1 确认 `uuid` 在 core、llm、agent、examples 中无运行时依赖；本地 `beamai_extra` 自行声明该依赖。
- [x] C2 确认 `esqlite` 在当前核心项目中无运行时依赖。
- [x] C3 确认 `poolboy` 在当前核心项目和 examples 中无运行时依赖。
- [x] C4 从根 `rebar.config` 和 `rebar.lock` 移除确认无用的依赖。
- [x] C5 同步更新各 app `.app.src`、依赖文档和中英文 README。

### 批次 D：xref 清零

- [x] D1 对 384 个 xref warning 分类：
      intentional public facade API / internal helper / stale export。
- [x] D2 对确认的公共 facade API 采用显式 xref 策略：框架导出 API 不开启 `exports_not_used`。
- [x] D3 保留未定义调用/函数、未用局部函数和 deprecated 检查，避免掩盖真实内部问题。
- [x] D4 让 `rebar3 xref` 输出 0 warning。

### 批次 E：CI

- [x] E1 新增 GitHub Actions workflow。
- [x] E2 workflow checkout 后执行 compile。
- [x] E3 执行 `rebar3 xref`。
- [x] E4 执行 `rebar3 dialyzer`。
- [x] E5 执行 `rebar3 eunit`。
- [x] E6 执行 `rebar3 ct`。
- [x] E7 增加 OTP 27 / OTP 28 矩阵。
- [x] E8 增加独立 examples 编译 job。
- [x] E9 live provider 测试保持可选，不依赖普通 PR 的真实 API key。

### 批次 F：项目元数据

- [x] F1 将 `rebar.config` 中 ex_doc 的 `source_url` 从 example 地址改为真实仓库。
- [x] F2 补充各 app `.app.src` 的 `links`。
- [x] F3 检查 README 徽章、OTP 版本说明与 `minimum_otp_vsn` 一致。

### Phase 0 验收

- [ ] P0-V1 `rebar3 xref`：0 warning。
- [ ] P0-V2 `rebar3 dialyzer`：0 warning。
- [ ] P0-V3 `rebar3 eunit`：全部通过。
- [ ] P0-V4 `rebar3 ct`：全部通过。
- [ ] P0-V5 OTP 27 / OTP 28 CI 通过。
- [ ] P0-V6 examples job 通过。
- [ ] P0-V7 根项目无未用依赖。

本地已完成验证：application modules 一致、`rebar3 xref` 0 warning、
`rebar3 dialyzer` 0 warning、`rebar3 eunit` 780 tests / 0 failures、
`rebar3 ct` 通过、examples 编译通过。P0-V5 需推送后由 GitHub Actions 矩阵确认。

## Phase 1：运行时安全

### 批次 G：TLS 默认值

- [x] G1 修改 `beamai_http_pool` TLS 配置入口，接收显式配置。
- [x] G2 默认使用 `verify_peer` 和系统 CA。
- [x] G3 系统 CA 加载失败时返回错误，不再静默 fallback 到 `verify_none`。
- [x] G4 增加显式 `tls_insecure => true` 支持。
- [x] G5 使用 insecure 模式时输出 warning。
- [x] G6 增加测试：
  - [x] secure 默认配置。
  - [x] 显式 insecure 配置。
  - [x] CA 加载失败。
  - [x] pool config 传播。

### 批次 H：Tool 安全扩展点

- [x] H1 梳理现有 tool schema 校验入口和默认行为。
- [x] H2 提供 tool 参数 schema 强校验配置。
- [x] H3 增加 tool result size limit 约定。
- [x] H4 增加敏感结果 redaction 测试。
- [x] H5 在 tool metadata 中预留 permission 字段约定：
      filesystem / network / process / env。
- [x] H6 输出 tool 安全配置文档。

### Phase 1 验收

- [x] P1-V1 默认 TLS 配置不可能静默变成 `verify_none`。
- [x] P1-V2 显式 insecure 配置有 warning 和测试覆盖。
- [x] P1-V3 非法 tool 参数返回统一错误结构。
- [x] P1-V4 超大 tool result 有明确上限或文档化处理策略。
- [x] P1-V5 现有 eunit / dialyzer / xref 全绿。

Phase 1 本地验证：`rebar3 xref` 0 warning，`rebar3 dialyzer` 0 warning，
`rebar3 eunit` 796 tests / 0 failures。TLS 变更提交：`2e288ce`，
Tool 安全变更提交：`4b5a632`。

## Phase 2：可观测性

### 批次 I：telemetry 基础设施

- [x] I1 新增轻量 telemetry behaviour 或等价 dispatch 模块。
- [x] I2 提供默认 logger backend。
- [x] I3 支持用户注入 handler。
- [x] I4 handler crash 不影响业务路径。
- [x] I5 支持按事件级别开启 / 关闭。
- [x] I6 定义公共 event map 结构。

批次 I 本地验证：`check_app_modules` 通过，`beamai_telemetry_tests` 6 tests /
0 failures，`dialyzer` 0 warning，`xref` 0 warning。

### 批次 J：LLM 埋点

- [x] J1 `llm_request_started`。
- [x] J2 `llm_request_finished`。
- [x] J3 `llm_retry`。
- [x] J4 `llm_stream_started`。
- [x] J5 `llm_stream_finished`。
- [x] J6 统一记录 latency、status、usage、finish reason、error class。

批次 J 本地验证：`beamai_chat_model_telemetry_tests` 4 tests / 0 failures，
`dialyzer` 0 warning，`xref` 0 warning，application modules 一致。

### 批次 K：Tool 埋点

- [x] K1 `tool_queued`。
- [x] K2 `tool_started`。
- [x] K3 `tool_finished`。
- [x] K4 `tool_timeout`。
- [x] K5 `tool_crash`。
- [x] K6 记录 batch id、attempt、duration、result size。

批次 K 本地验证：`beamai_tool_telemetry_tests` 4 tests / 0 failures，
`dialyzer` 0 warning，`xref` 0 warning，application modules 一致。

### 批次 L：Agent 埋点

- [x] L1 `agent_run_started` / `agent_run_finished`。
- [x] L2 `agent_turn_started` / `agent_turn_finished`。
- [x] L3 `agent_interrupted`。
- [x] L4 `agent_resumed`。
- [x] L5 `agent_state_changed`。
- [x] L6 事件链路携带 request id、run id、conversation id。

批次 L 本地验证：`beamai_agent_telemetry_tests` 3 tests / 0 failures，
`dialyzer` 0 warning，`xref` 0 warning，application modules 一致。

### 批次 M：HTTP Pool 埋点

- [x] M1 connection acquired 事件和等待时间。
- [x] M2 connection reused 事件。
- [x] M3 connection failed 事件。
- [x] M4 wait timeout 事件。
- [x] M5 idle reap 事件。
- [x] M6 保持 `beamai_http_pool:stats/1` 可用且信息不回退。

批次 M 本地验证：`beamai_http_pool_telemetry_tests` 4 tests / 0 failures，
`dialyzer` 0 warning，`xref` 0 warning，`eunit` 817 tests / 0 failures，
application modules 一致。

### Phase 2 验收

- [ ] P2-V1 一次 Agent run 可通过事件还原完整时序。
- [x] P2-V2 telemetry handler crash 不影响 Agent。
- [x] P2-V3 默认日志不输出 prompt、tool result 全文和 API key。
- [x] P2-V4 高频事件可关闭。
- [x] P2-V5 eunit / dialyzer / xref 全绿。

## Phase 3：Provider contract test

### 批次 N：公共 contract harness

- [ ] N1 设计 provider 差异配置格式。
- [ ] N2 建立请求体构造公共断言。
- [ ] N3 建立 tool schema / tool call 转换公共断言。
- [ ] N4 建立 SSE 分片重组公共断言。
- [ ] N5 建立 finish reason 归一化断言。
- [ ] N6 建立 usage 归一化断言。
- [ ] N7 建立错误结构与 retryable 断言。
- [ ] N8 建立 Retry-After 与 rate limit headers 断言。
- [ ] N9 建立 multimodal content 断言。

### 批次 O：内置 Provider 接入

- [ ] O1 OpenAI Chat Completions。
- [ ] O2 OpenAI Responses。
- [ ] O3 Anthropic。
- [ ] O4 DeepSeek。
- [ ] O5 Zhipu。
- [ ] O6 DashScope。
- [ ] O7 Ollama。
- [ ] O8 Moonshot。
- [ ] O9 OpenRouter。
- [ ] O10 SiliconFlow。
- [ ] O11 xAI。

### Phase 3 验收

- [ ] P3-V1 所有非 experimental chat provider 接入 contract test。
- [ ] P3-V2 provider 间公共行为差异有显式配置或文档。
- [ ] P3-V3 新增 provider 缺少 contract test 时 CI 可拦截。
- [ ] P3-V4 全部测试离线可跑。

## Phase 4：Agent event model

### 批次 P：事件模型

- [ ] P1 定义 event type。
- [ ] P2 定义 event metadata 与公共字段。
- [ ] P3 定义 event ordering 与 run id 关系。
- [ ] P4 定义 stream consumer 接口。
- [ ] P5 telemetry 消费同一事件模型。
- [ ] P6 callback / filter 与 event model 的边界写入文档。

### 批次 Q：Agent runtime 接入

- [ ] Q1 run started / finished。
- [ ] Q2 turn started / finished。
- [ ] Q3 chat started / finished。
- [ ] Q4 tool queued / started / finished。
- [ ] Q5 interrupt / approval / resume。
- [ ] Q6 state changed。
- [ ] Q7 error occurred。

### Phase 4 验收

- [ ] P4-V1 同一次 run 的事件序列稳定。
- [ ] P4-V2 UI stream 与 telemetry 可并行消费。
- [ ] P4-V3 callback crash 不影响 Agent 主流程。
- [ ] P4-V4 事件文档包含每个字段语义。

## Phase 5：性能基准

### 批次 R：benchmark harness

- [ ] R1 建立可重复运行的 benchmark 入口。
- [ ] R2 使用 mock provider 避免外部网络抖动。
- [ ] R3 输出文本表格基线。
- [ ] R4 记录运行环境：OTP 版本、CPU、Erlang scheduler、时间戳。
- [ ] R5 支持单场景运行，避免每次全量耗时过长。

### 批次 S：核心场景

- [ ] S1 filter chain 0 / 1 / 4 / 16 层。
- [ ] S2 tool dispatch 1 / 8 / 64 / 256 个。
- [ ] S3 concurrent manager 与 sequential manager。
- [ ] S4 memory history append / read。
- [ ] S5 stream event fanout。
- [ ] S6 HTTP pool acquire / release。
- [ ] S7 provider adapter encode / decode。

### Phase 5 验收

- [ ] P5-V1 基准可重复运行。
- [ ] P5-V2 输出 p50 / p95 / max 或等价统计。
- [ ] P5-V3 基线结果入库或记录到文档。
- [ ] P5-V4 后续性能修改必须对比基线。

## 执行约束

1. 每个 Phase 可拆多个 PR，不要求一次完成。
2. Phase 0 与 Phase 1 优先，其他阶段不得阻塞基础工程修复。
3. 任何行为变化必须先更新设计文档，再改代码。
4. 删除公共 API 前必须显式评审。
5. 所有新模块保持完整 `-spec`，不引入新的 dialyzer warning。
6. 不引入新的根依赖，除非能证明无法用 OTP 标准库或现有依赖解决。
7. live provider 测试不作为普通 PR 的合并条件。

## 最终验证命令

```bash
rebar3 xref
rebar3 dialyzer
rebar3 eunit
rebar3 ct

# examples 目录按其自身 rebar 配置执行
cd examples && rebar3 eunit
```
