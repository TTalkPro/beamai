# Changelog

本项目遵循 [Keep a Changelog](https://keepachangelog.com/zh-CN/1.1.0/) 格式。

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
