# BeamAI 项目优化路线图

> 状态：设计稿  
> 日期：2026-10-15  
> 范围：工程化基线、运行时可观测性、安全默认值、API 演进、性能验证  
> 执行清单：[`tasks/project_optimization_roadmap.md`](../tasks/project_optimization_roadmap.md)

## 1. 背景与基线

BeamAI 当前已经具备较清晰的三层结构：

| App | 核心职责 |
|---|---|
| `beamai_core` | ChatClient、Context、Filter、Tool、HTTP/SSE 基础能力 |
| `beamai_llm` | 多 Provider Chat / Embedding / Rerank / Decision / Output Parser |
| `beamai_agent` | SimpleAgent、ReAct loop、工具执行、中断恢复、子 Agent |

本轮分析时基线：

- `rebar3 eunit`：780 tests，0 failures。
- `rebar3 dialyzer`：0 warning。
- `rebar3 xref`：384 个 warning，主要集中在公共 facade 和有意导出的 API。
- 没有 CI workflow。
- 三个 `.app.src` 的 `modules` 元数据与实际源码模块不同步。
- 根项目声明了 `uuid`、`esqlite`、`poolboy`，但源码内没有直接使用。
- `rebar3.crashdump` 被 Git 跟踪。
- `beamai_http_pool` 在系统 CA 加载失败时会降级为 `verify_none`。

因此，下一阶段不建议优先进行大规模架构重写，而应先把工程质量门槛、运行时可观测性和安全边界补齐。

## 2. 目标

### 必达目标

1. 所有核心检查可以在 CI 中稳定执行。
2. `rebar3 xref` 变成有意义的强制检查，而不是被 384 个 warning 淹没。
3. application metadata 与实际模块保持一致。
4. 移除已确认无用的根依赖和误提交的构建产物。
5. 生产默认 TLS 策略不允许静默降级为 `verify_none`。
6. LLM、Tool、Agent、HTTP Pool 的关键路径具备结构化事件和指标埋点。

### 后续目标

1. Provider 行为通过统一 contract test 保证。
2. Agent 生命周期具备统一事件模型。
3. Tool 执行具备更完整的权限、脱敏和取消能力。
4. Memory 支持更丰富的压缩、摘要和长期记忆演进路径。
5. 建立稳定的 benchmark harness，用数据驱动后续性能优化。

## 3. 非目标

本轮不改变以下语义：

- 不重写 ChatClient / Filter 洋葱模型。
- 不改变 Tool Calling Manager 的默认并发或隔离模型。
- 不替换 Gun / DETS / OTP supervision 结构。
- 不把 HTTP、MCP backend 引入 `beamai_tool`。
- 不在核心库强制绑定特定 telemetry 或 tracing 实现。
- 不引入需要破坏现有公共 API 的大版本迁移。

## 4. 总体设计

### 4.1 工程质量基线

新增 CI workflow，按最小必要维度划分：

```text
ci:
  - compile
  - xref
  - dialyzer
  - eunit
  - ct

examples:
  - examples/rebar3 eunit

live:
  - optional / scheduled / manually triggered
```

建议使用 OTP 27 与 OTP 28 矩阵。live provider 测试不应在普通 push 中依赖真实 API key，应通过显式 env 或 workflow dispatch 启用。

`rebar.config` 增加 xref ignore 策略。区分两类导出：

1. **公共 facade API**：如 `beamai`、`beamai_agent`、`beamai_chat_model` 顶层入口。  
   使用显式 ignore，保留 API。
2. **内部 helper 意外导出**：删除或收敛到 internal module。

最终目标是让 `rebar3 xref` 默认输出干净，CI 将新增 warning 视为失败。

### 4.2 Application metadata 修正

`.app.src` 的 `modules` 清单必须反映实际发布模块。建议以源码扫描结果为输入，人工确认后补齐：

| App | 问题 |
|---|---|
| `beamai_agent` | 缺少 pause、branch、timeline、tool manager、batch worker 等模块 |
| `beamai_core` | 缺少 filters、json schema、tool error、tool index 等模块 |
| `beamai_llm` | 缺少 parser、mock provider、HTTP provider、error 等模块 |

短期内手工补齐。若后续模块增长频繁，可以引入构建前检查脚本，禁止 `.app.src` 与 `src/**/*.erl` 集合不一致。

### 4.3 依赖瘦身

根 `rebar.config` 中 `uuid`、`esqlite`、`poolboy` 需要逐一确认：

| 依赖 | 当前情况 | 处理 |
|---|---|---|
| `uuid` | `beamai_id` 已使用 `crypto:strong_rand_bytes/1` | 从核心库移除，extension 如需自行声明 |
| `esqlite` | 当前源码无直接引用 | 从根项目移除；若未来做 SQLite backend，再按 feature 引入 |
| `poolboy` | 当前源码无直接引用；HTTP pool 使用 Gun + 自研 gen_server | 从根项目移除；extension 如需自行声明 |

同步更新：

- `rebar.config`
- `rebar.lock`
- `docs/DEPENDENCIES.md`
- `docs/DEPENDENCIES_EN.md`
- 各 app 的 `.app.src`
- README 中依赖说明

### 4.4 安全默认值

#### TLS

`beamai_http_pool:get_tls_opts/0` 修改为失败关闭：

```erlang
get_tls_opts(Config) ->
    case maps:get(tls_insecure, Config, false) of
        true  -> [{verify, verify_none}];
        false -> verify_peer_with_system_cacerts()
    end.
```

规则：

1. 默认 `verify_peer`。
2. 系统 CA 加载失败时返回错误，而不是静默 `verify_none`。
3. `tls_insecure => true` 只允许显式配置。
4. 使用 insecure 模式时输出 warning。
5. 增加测试覆盖 secure 默认、显式 insecure、CA 加载失败三类路径。

#### Tool 执行

现有 timeout 和 batch isolation 保留。后续增强：

```erlang
#{
    name => <<"shell">>,
    handler => Handler,
    permissions => #{network => false, filesystem => true, process => true},
    max_result_bytes => 262144,
    redaction => [credential, token, private_key],
    cancellation => cooperative
}
```

第一阶段只引入 schema 校验和 metadata 约定，不强制所有工具立刻改造。核心运行时逐步支持：

1. 参数 schema 强校验。
2. 结果大小上限。
3. 敏感字段 redaction。
4. cooperative cancellation。
5. permission-aware filter。

### 4.5 Observability 设计

不直接绑定 OpenTelemetry。新增轻量事件分发层：

```erlang
-module(beamai_telemetry).

-callback emit(EventName, Measurements, Metadata) -> ok.
```

事件统一采用 map：

```erlang
#{
    type => tool_finished,
    run_id => RunId,
    conversation_id => ConvId,
    turn => Turn,
    tool => ToolName,
    status => ok | error | timeout,
    duration_ms => Duration,
    attempt => Attempt,
    error_class => transient | permanent | semantic | infrastructure
}
```

默认实现写 `logger`，用户可注入自定义 handler。核心埋点分为四层：

#### LLM 层

| 事件 | 关键数据 |
|---|---|
| `llm_request_started` | provider、model、request id |
| `llm_request_finished` | status、latency、usage、finish reason |
| `llm_retry` | attempt、delay、error class |
| `llm_stream_started` | provider、model、request id |
| `llm_stream_finished` | chunks、duration、usage |

#### Tool 层

| 事件 | 关键数据 |
|---|---|
| `tool_queued` | tool、batch id、position |
| `tool_started` | tool、attempt、timeout |
| `tool_finished` | status、duration、result size |
| `tool_timeout` | tool、timeout、duration |
| `tool_crash` | class、reason |

#### Agent 层

| 事件 | 关键数据 |
|---|---|
| `agent_run_started` | run id、conversation id |
| `agent_turn_started` / `agent_turn_finished` | turn、status |
| `agent_interrupted` | interrupt reason、pending tool calls |
| `agent_resumed` | resume source |
| `agent_state_changed` | state slot、writer |

#### HTTP Pool 层

| 事件 | 关键数据 |
|---|---|
| `http_pool_connection_acquired` | host、wait time |
| `http_pool_connection_reused` | host |
| `http_pool_wait_timeout` | host、timeout |
| `http_pool_connection_failed` | host、reason |
| `http_pool_idle_reaped` | count |

默认只输出低频关键事件，debug 级别才输出高频细节，避免 token stream 和 tool result 内容污染日志。

## 5. 分阶段实施

### Phase 0：工程止血

目标：清理明显问题，建立可持续检查门槛。

- 删除 `rebar3.crashdump`。
- 更新 `.gitignore`。
- 补齐 `.app.src` modules。
- 清理根依赖。
- 修复 ex_doc `source_url`。
- 增加 CI。

验收：

- `rebar3 eunit` 全绿。
- `rebar3 dialyzer` 0 warning。
- `rebar3 xref` 0 warning。
- CI 在 OTP 27 / OTP 28 通过。

### Phase 1：运行时安全

目标：移除危险默认值，建立工具安全扩展点。

- TLS 默认 `verify_peer`。
- `verify_none` 必须显式启用。
- tool schema 校验可配置并补测试。
- 引入 tool result size limit 约定。
- 补充脱敏 filter 测试。

验收：

- 默认配置无法静默关闭 TLS peer verification。
- 参数非法的工具调用产生统一错误结构。
- 现有测试零语义破坏。

### Phase 2：可观测性

目标：为 AI Agent 关键路径建立结构化事件。

- 新增 `beamai_telemetry` behaviour 与默认 logger backend。
- 埋点覆盖 LLM、Tool、Agent、HTTP Pool。
- 请求/运行链路携带 `request_id`、`conversation_id`、`run_id`。
- 错误统一携带 class 和 retryable 信息。

验收：

- 一次完整 Agent run 可以通过事件还原 turn / chat / tool / error 时序。
- telemetry 关闭或 handler crash 不会影响业务流程。
- 日志默认不输出 prompt、tool result 全文和 API key。

### Phase 3：Provider contract test

目标：降低多 Provider parser / adapter 漂移风险。

建立统一测试套件，覆盖：

- 请求体构造。
- tool schema 与 tool call 转换。
- SSE 分片重组。
- finish reason 归一化。
- usage 归一化。
- 错误结构。
- Retry-After。
- rate limit headers。
- multimodal content。

每个 Provider 只需注册差异配置，公共断言由 contract harness 执行。

验收：

- 现有 chat provider 接入 contract test。
- 新增 provider 时缺少 contract test 会导致 CI 失败或明确标记 experimental。

### Phase 4：Agent event model

目标：给上层 UI、审计、监控提供统一事件流。

定义事件类型：

```erlang
-type event_type() ::
    run_started | run_finished |
    turn_started | turn_finished |
    chat_started | chat_finished |
    tool_queued | tool_started | tool_finished |
    interrupt_requested | approval_requested |
    resumed | state_changed |
    error_occurred.
```

事件由 Agent runtime 发布，filter/callback 仍是业务扩展点。telemetry、UI stream、审计日志都消费同一事件模型。

验收：

- 同一次 run 的事件序列稳定。
- stream consumer 与 telemetry handler 可以并行消费。
- callback crash 不影响 Agent 主流程。

### Phase 5：性能与容量验证

目标：用 benchmark 决定是否需要性能重构。

基准项：

| 场景 | 指标 |
|---|---|
| filter chain 0/1/4/16 层 | 每次调用 overhead |
| tool dispatch 1/8/64/256 个 | latency、throughput |
| concurrent / sequential manager | 批次延迟 |
| memory history 追加/读取 | p50 / p95 |
| stream event fanout | events/sec |
| HTTP pool acquire/release | wait time、reuse rate |
| provider adapter encode/decode | CPU time、GC |

输出建议使用文本表格先落地，后续再决定是否生成 HTML report。

## 6. 风险与回滚

| 风险 | 缓解 |
|---|---|
| xref 清理误删公共 API | 仅对确认 helper 删除；facade API 使用 ignore 保留 |
| 依赖移除影响 extension 项目 | 先搜索 core / extra / examples；extension 依赖显式声明 |
| TLS strict 默认影响本地环境 | 增加显式 insecure 开关与清晰错误信息 |
| telemetry 引入性能损耗 | 事件结构保持轻量；高频事件可关闭；handler 隔离 crash |
| contract test 覆盖差异过大 | 区分 common mandatory 与 provider optional assertions |
| benchmark 数据环境差异大 | 固定 mock provider、固定输入、多轮取中位数 |

## 7. 推荐执行顺序

优先级从高到低：

1. Phase 0：工程止血。
2. Phase 1：TLS 安全默认值。
3. Phase 2：telemetry 与关键埋点。
4. Phase 3：Provider contract test。
5. Phase 4：Agent event model。
6. Phase 5：benchmark harness。

Phase 0 和 Phase 1 可拆成小 PR 快速合入。Phase 2 以后建议按 app 拆分，避免一次提交跨层过大。

## 8. 成功标准

三个月后的最低完成标准：

1. CI 全绿成为默认约束。
2. `xref`、`dialyzer`、`eunit`、`ct` 都可重复执行。
3. application metadata 自动或手动校验保持一致。
4. 根项目没有无用原生或工具类依赖。
5. TLS 默认严格校验。
6. Agent 关键路径有结构化 telemetry。
7. Provider contract test 至少覆盖所有内置 chat provider 的公共行为。
8. 存在可重复运行的性能基准脚本和基线数据。
