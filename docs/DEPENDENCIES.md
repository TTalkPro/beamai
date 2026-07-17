# BeamAI Framework Dependencies

[English](DEPENDENCIES_EN.md) | 中文

本文档描述 BeamAI Framework 的依赖关系，包括外部依赖和内部模块间的依赖。

## 外部依赖

### 运行时依赖

| 依赖包 | 版本 | 用途 |
|-------|------|------|
| [gun](https://github.com/ninenines/gun) | 2.1.0 | HTTP/1.1, HTTP/2, WebSocket 客户端（唯一内置 HTTP 后端） |
| [uuid](https://github.com/okeuday/uuid) | 2.0.6 | UUID 生成（包名：uuid_erl） |
| [esqlite](https://github.com/mmzeeman/esqlite3) | 0.8.8 | SQLite 数据库支持（用于持久化存储） |

### HTTP 后端配置

HTTP 后端经 `beamai_http_behaviour` 可插拔，内置实现只有 `beamai_http_gun`
（HTTP/2、异步、配合 `beamai_http_pool` 的三个用途分池），即默认后端，无需配置。

抽象层保留是为了让调用方替换后端，主要用于测试：

```erlang
%% 例：beamai_llm 测试用的 fake 后端
application:set_env(beamai_core, http_backend, beamai_llm_fake_backend).
```

自定义后端需实现 `beamai_http_behaviour`。注意连接池语义（`http_pool_short` 等池名）
是 Gun 后端专有的，非 Gun 后端下不会自动注入 —— 详见 [HTTP.md](HTTP.md)。

> 早期曾并存 `beamai_http_hackney`（HTTP/1.1）后端，现已删除，全线统一到 Gun。

### 测试依赖

| 依赖包 | 版本 | 用途 |
|-------|------|------|
| [meck](https://github.com/eproxus/meck) | 0.9.2 | Mock 库，用于单元测试 |

### 开发工具

| 插件 | 用途 |
|------|------|
| rebar3_proper | 属性测试支持 |
| rebar3_ex_doc | 文档生成 |

## 内部应用依赖

### 依赖层级图

本仓库（beamai）只含三个核心 app；`beamai_tools` / `beamai_mcp` / `beamai_a2a` /
`beamai_rag` 属于扩展项目 [beamai_extra](https://github.com/TTalkPro/beamai_extra)。
下图为两仓合并视角，依赖关系取自各 app 的 `.app.src`：

```
                     扩展项目 beamai_extra
    ┌───────────────┐  ┌───────────────┐  ┌───────────────┐  ┌───────────────────┐
    │  beamai_mcp   │  │  beamai_rag   │  │  beamai_a2a   │  │    beamai_tools   │
    │  (MCP 协议)    │  │   (RAG)       │  │  (A2A 协议)    │  │ (工具+Middleware) │
    └───────┬───────┘  └───────┬───────┘  └───────┬───────┘  └─────────┬─────────┘
            │                  │                  │                    │
            │                  │                  ▼                    │
            │                  │        ┌──────────────────┐           │
            │                  │        │   beamai_agent   │           │
            │                  │        │ (SimpleAgent)    │           │
            │                  │        └────────┬─────────┘           │
            │                  │                 │                     │
            │                  │                 ▼                     │
            │                  │        ┌──────────────────┐           │
            │                  │        │    beamai_llm    │◄──────────┘
            │                  │        │      (LLM)       │
            │                  │        └────────┬─────────┘
            │                  │                 │ 实现 beamai_chat_behaviour
            └──────────────────┴─────────────────┤
                                                 ▼
                              ┌───────────────────────────────────┐
                              │           beamai_core             │  ← 基础层
                              │  类型 / Kernel / Filter / HTTP     │
                              │  Behaviour: chat_behaviour,       │
                              │  chat_memory, memory_provider,    │
                              │  http_behaviour, tool_behaviour   │
                              └─────────────────┬─────────────────┘
                                                ▼
                              ┌───────────────────────────────────┐
                              │  Erlang/OTP 27+ + 外部依赖         │
                              │  (gun / uuid / esqlite / poolboy) │
                              └───────────────────────────────────┘
```

**依赖方向说明**：
- 箭头(→)表示**编译时依赖**（`.app.src` 的 `applications`）
- `beamai_agent` 只依赖 `beamai_core` + `beamai_llm`——**不依赖** `beamai_tools`
- `beamai_a2a` 依赖 `beamai_agent`（用于 Agent 执行）
- `beamai_mcp`、`beamai_rag` 只依赖 `beamai_core`；`beamai_tools` 依赖 `beamai_core` + `beamai_llm`
- `beamai_core` 定义 Behaviour 接口，上层模块实现；`beamai_core` 不感知具体实现，
  通过 `{Module, Ref}` 句柄动态分发实现解耦

### 各应用依赖详情

#### beamai_core（核心库）

**依赖**: 无内部依赖

**提供功能**:
- 类型定义（beamai_types）
- 通用工具函数（beamai_utils）
- JSON-RPC 支持（beamai_jsonrpc）
- SSE 支持（beamai_sse）
- **HTTP 客户端**（可插拔后端）
  - `beamai_http` - 统一 API
  - `beamai_http_gun` - Gun 后端（HTTP/2，唯一内置实现）
  - `beamai_http_pool` - Gun 连接池管理
- **Behaviour 定义**（用于解耦依赖）
  - `beamai_chat_behaviour` - LLM 聊天接口（原 beamai_llm_behaviour）
  - `beamai_chat_memory` - 会话存储接口（存储层）
  - `beamai_memory_provider` - Agent 记忆策略接口（策略层）
  - `beamai_http_behaviour` - HTTP 客户端接口
  - `beamai_tool_behaviour` - 工具模块接口

#### beamai_tools（工具系统 + 中间件系统）

**依赖**:
- beamai_core（Behaviour 定义、类型定义）
- beamai_llm

**提供功能**:
- 入口（beamai_tools）
- 工具安全（beamai_tool_security）
- 内置工具
  - 文件工具（beamai_tool_file）
  - Shell 工具（beamai_tool_shell）
  - Todo 工具（beamai_tool_todo）
  - 人机交互工具（beamai_tool_human）
- Middleware 系统（经 `beamai_middleware_runner:to_filters/1` 桥接到 core 的 filter 洋葱链）
  - Middleware 行为定义（beamai_middleware）
  - Middleware 运行器（beamai_middleware_runner）
  - 预设 Middleware（beamai_middleware_presets）
  - 内置 Middleware（middleware_call_limit、middleware_tool_retry、middleware_model_retry、
    middleware_model_fallback、middleware_human_approval）

#### beamai_llm（LLM 集成）

**依赖**:
- beamai_core

**提供功能**:
- LLM 客户端（llm_client）
- 多提供商支持
  - OpenAI（beamai_llm_provider_openai）
  - Anthropic（beamai_llm_provider_anthropic）
  - DeepSeek（beamai_llm_provider_deepseek）- OpenAI 兼容 API
  - Ollama（beamai_llm_provider_ollama）
  - 智谱 AI（beamai_llm_provider_zhipu）
  - 阿里云百炼（beamai_llm_provider_dashscope）- DashScope 原生 API
- 消息适配器（beamai_llm_message_adapter）
- 工具适配器（beamai_llm_tool_adapter）
- 响应解析器（beamai_llm_response_parser）

#### beamai_rag（RAG 系统）

**依赖**:
- beamai_core

**提供功能**:
- RAG 管道（beamai_rag）
- 文档分割（beamai_rag_splitter）
- 向量存储（beamai_vector_store）
- 嵌入支持（beamai_embeddings）

#### beamai_agent（Agent 系统）

**依赖**:
- beamai_core（Kernel、Filter、类型、记忆 behaviour）
- beamai_llm（LLM 调用）

> **注意**: beamai_agent 是核心编排层，**不依赖** beamai_tools，也不依赖 beamai_mcp。
> 工具以 map/`beamai_tool` 形式注册进 kernel；MCP 工具通过 beamai_mcp 的适配器层
> 集成到 Agent，而非反向依赖。

**提供功能**:
- Agent 生命周期（beamai_agent：new/run/stream/resume；beamai_agent_state：状态与配置解析）
- ReAct 工具循环（beamai_agent_tool_loop）——不是图引擎；Agent 无 filter 概念
- ToolCallingManager（工具批次执行策略，behaviour + `{Mod, Ref}` 分派）
  - 行为定义（beamai_tool_calling_manager）
  - 并发实现（beamai_concurrent_tool_calling_manager，默认）
  - 串行实现（beamai_sequential_tool_calling_manager）
  - 批次工作进程（beamai_tool_batch_worker）
- 子 Agent（beamai_subagent_manager、beamai_agent_delegate）
- 回调系统（beamai_agent_callbacks）——Agent 唯一的观察扩展点，由 tool loop 直接触发
- HITL 中断/恢复（beamai_agent_interrupt、beamai_agent_pause、beamai_pause_store[_ets]）
- 分支与时间线（beamai_branch_store[_ets]、beamai_timeline）

> **记忆不在此列**：Agent 的跨轮记忆由 `beamai_core` 的 `beamai_memory_provider` 承担，
> Agent 在 tool loop 里显式调用（`memory` 是与 `kernel` 正交的创建参数）。详见
> [MEMORY.md](MEMORY.md)。

#### beamai_a2a（Agent-to-Agent 协议）

**依赖**:
- beamai_core
- beamai_agent

**提供功能**:
- A2A 服务器（beamai_a2a_server）
- A2A 客户端（beamai_a2a_client）
- Agent Card 管理（beamai_a2a_card）
- 认证（beamai_a2a_auth）
- 任务管理（beamai_a2a_task）
- HTTP 处理器（beamai_a2a_http_handler）

#### beamai_mcp（MCP 协议）

**依赖**:
- beamai_core（JSON-RPC、SSE 支持）
- gun（HTTP/SSE 传输）、cowboy（**可选**，仅 server 端 handler 需要）

**提供功能**:
- MCP 服务器（beamai_mcp_server、beamai_mcp_handler、beamai_mcp_session_registry；
  beamai_mcp_cowboy_handler 为 cowboy 接入层）
- MCP 客户端（beamai_mcp_client、beamai_mcp_client_registry）
- 传输层（beamai_mcp_transport 分派）
  - HTTP 传输（beamai_mcp_transport_http_gun）
  - SSE 传输（beamai_mcp_transport_sse_gun）
  - Stdio 传输（beamai_mcp_transport_stdio）
- 协议类型与 JSON-RPC（beamai_mcp_types、beamai_mcp_jsonrpc）
- Agent 适配器（beamai_mcp_adapter）- 将 MCP 工具转换为 Agent 工具

**传输层后端配置**:

```erlang
%% 使用 Gun 后端（默认，支持 HTTP/2）
Config = #{
    transport => http,  %% 或 sse
    url => <<"https://example.com/mcp">>
}.
```

> HTTP/SSE 传输统一走 Gun。早期的 `backend => hackney` 配置键已废弃。

> **注意**: MCP 核心功能可独立运行，不依赖 beamai_agent。
> 仅在需要将 MCP 工具集成到 Agent 系统时，才使用适配器层。

#### beamai_examples（示例，位于 `examples/`）

**依赖**: poolboy（beamai_core / beamai_llm / beamai_agent 经 `ERL_LIBS` 提供，
见 `examples/rebar.config`）

**提供功能**:
- LLM 配置助手（example_llm_config）
- Kernel 对话（example_kernel_chat）
- 流式响应（example_streaming）
- Filter 示例（example_filter）
- 工具示例（example_tool_refactored）

## Erlang/OTP 依赖

BeamAI Framework 使用以下 Erlang/OTP 标准库：

| 模块 | 用途 |
|------|------|
| gen_server | 进程管理 |
| gen_statem | 状态机 |
| supervisor | 监督树 |
| ets | 内存存储 |
| persistent_term | 持久化术语存储 |
| logger | 日志记录 |
| crypto | 加密功能 |
| ssl | SSL/TLS 支持 |
| json | JSON 编解码（OTP 27+，取代原 jsx 依赖） |

## 安装依赖

```bash
# 获取所有依赖
rebar3 get-deps

# 编译项目
rebar3 compile

# 运行测试
rebar3 eunit
rebar3 ct
```

## 版本兼容性

- **Erlang/OTP**: 27.0 或更高版本（JSON 编解码依赖 OTP 27 起 stdlib 内置的 `json` 模块）
- **rebar3**: 3.20.0 或更高版本

## 可选依赖

以下依赖是可选的，根据使用场景按需启用：

| 依赖 | 用途 | 启用条件 |
|------|------|----------|
| cowboy | HTTP 服务器 | A2A/MCP 服务端功能 |
| gun | HTTP/2 客户端 | 高性能 HTTP 需求 |
| eredis | Redis 客户端 | Redis 存储后端 |

## 依赖更新

更新依赖前请检查兼容性：

```bash
# 检查过时的依赖
rebar3 upgrade --all

# 更新 lock 文件
rebar3 lock
```
