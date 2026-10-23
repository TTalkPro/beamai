# Changelog

本项目遵循 [Keep a Changelog](https://keepachangelog.com/zh-CN/1.1.0/) 格式。

## [2.0.1] - 2026-10-23

### Fixed

- 同一 assistant 回合同时包含文本与 tool calls 时，会话历史不再丢弃文本；Anthropic 历史回放会在 `tool_use` 前插入对应的 `text` 块，避免刷新后的线程重绘和下一轮模型上下文缺少该段内容。

### Changed

- `beamai_llm_provider_deepseek` 按官方文档升级：
  - 模型更名：`deepseek-chat` / `deepseek-reasoner` → `deepseek-flash`（默认）/ `deepseek-v4-pro`
  - 思考模式改由 `thinking`（`#{type => enabled | disabled}`）与 `reasoning_effort`
    （`none | low | high | max`，兼容 `minimal`/`medium`/`xhigh` 别名）控制，替代按模型名判断
  - 思考模式下 `tool_choice` 的 `required` 与指定具体 tool 降级为 `auto`（API 返回 400）
  - `frequency_penalty` / `presence_penalty` 官方已废弃，不再发送；新增 `user_id`
  - `response_format` 归一化为 `text` / `json_object`，`json_schema` 被剔除
  - 新增 `finish_reason` 取值 `insufficient_resource` / `aborted`
  - usage.details 新增 `cached_tokens`（等价 `prompt_tokens_details.cached_tokens`）
- FIM 补全（Beta）保留：`fim/2` 与 `stream_fim/3`，端点等价于官方要求的 beta base_url

## [2.0.0] - 2026-10-19

### Changed

- 全部 OTP 应用（`beamai_core`、`beamai_agent`、`beamai_llm`、`beamai_examples`）版本统一升级为 `2.0.0`。
- 重试逻辑从 filter 链下沉到 `beamai_chat_model` 内部，filter 看到的是一次逻辑调用；单次尝试可通过 chat opts 的 `on_retry` 回调观测。
- HTTP/SSE 传输统一走 Gun 后端（HTTP/2，三个用途分池：短请求 / 流式 / 异步长轮询），遗留 `http_pool` 配置键兼容但已废弃。

### Added

- **LLM Provider 扩展**：新增 `openai_responses`（Responses API）、`xai`、`moonshot`/`kimi`、`openrouter`、`siliconflow`，内置 Provider 共 11 家，同步/流式统一接入。
- **Embedding / Rerank / Decision**：新增 embedding（OpenAI、DashScope、Zhipu、SiliconFlow、Ollama、mock）、rerank（Jina、Cohere、Voyage、DashScope、SiliconFlow、mock）与 TypeSafe Decision 客户端。
- **Filter 链**：洋葱式 filter（`around_turn` / `around_step` / `around_chat` / `around_tool` + 流式 `token_transform`），工具循环可整体替换。
- **Agent 事件模型**：统一的 Agent runtime 事件（run/turn/tool/state），`on_llm_event` 原始流事件通道、工具结果回调、`stream_resume` 与跨节点 DETS 暂停快照。
- **可观测性**：telemetry dispatch 与默认日志后端，覆盖 LLM（request/retry/stream）、Tool、Agent、HTTP Pool 关键路径；默认日志不输出 prompt、工具结果全文与 API key。
- **Tool 安全**：工具 schema 校验、结果大小限制与 permission metadata 扩展点。
- **基准测试**：可复现的 benchmark harness 与核心性能场景。
- **CI**：OTP 27 / OTP 28 矩阵质量门（xref、dialyzer、eunit、ct），examples 独立编译。

### Security

- TLS 对端校验默认强制 `verify_peer`，系统 CA 加载失败时直接失败而非静默回退 `verify_none`；insecure TLS 需显式配置并输出 warning。

### Removed

- Process Framework 流程编排引擎与存储/快照引擎（原 `beamai_process` / `beamai_memory`）从本仓库移除。
- 移除未使用的根依赖（`uuid`、`esqlite`、`poolboy`）。

## [1.0.0]

- 初始发布：ChatClient / Tool 架构、Filter、会话记忆（ETS / DETS）、SimpleAgent（ReAct）、Output Parser、OpenAI / Anthropic / DeepSeek / Zhipu / DashScope / Ollama 六家 Provider。
