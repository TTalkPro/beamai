# BeamAI Framework Dependencies

[English](DEPENDENCIES_EN.md) | 中文

本文档描述 BeamAI Framework 的依赖关系，包括外部依赖和内部模块间的依赖。

## 外部依赖

### 运行时依赖

| 依赖包 | 版本 | 用途 |
|-------|------|------|
| [jsx](https://github.com/talentdeficit/jsx) | 3.1.0 | JSON 编码/解码 |
| [hackney](https://github.com/benoitc/hackney) | 1.20.1 | HTTP 客户端（默认后端） |
| [gun](https://github.com/ninenines/gun) | 2.1.0 | HTTP/1.1, HTTP/2, WebSocket 客户端（可选后端） |
| [uuid](https://github.com/okeuday/uuid) | 2.0.6 | UUID 生成（包名：uuid_erl） |
| [esqlite](https://github.com/mmzeeman/esqlite3) | 0.8.8 | SQLite 数据库支持（用于持久化存储） |

### HTTP 后端配置

BeamAI 支持两种 HTTP 客户端后端，可通过配置切换：

```erlang
%% 使用 Gun（默认，支持 HTTP/2）
application:set_env(beamai_core, http_backend, beamai_http_gun).

%% 使用 Hackney（稳定成熟）
application:set_env(beamai_core, http_backend, beamai_http_hackney).
```

| 后端 | 特点 | 适用场景 |
|------|------|----------|
| gun | HTTP/2 支持、异步、现代设计 | 默认后端，高并发场景 |
| hackney | 同步 API、内置连接池、稳定 | 兼容旧代码 |

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

```
                         ┌─────────────────┐
                         │ beamai_examples │
                         └────────┬────────┘
                                  │ 依赖所有模块
        ┌─────────────────────────┼─────────────────────────┐
        │                         │                         │
        ▼                         ▼                         ▼
┌───────────────┐       ┌─────────────────┐       ┌───────────────┐
│  beamai_mcp   │       │beamai_deepagent │       │  beamai_a2a   │
│  (协议扩展)    │       │   (高级Agent)    │       │ (A2A协议)     │
└───────┬───────┘       └────────┬────────┘       └───────┬───────┘
        │                        │                        │
        │                        │                        ▼
        │                        │               ┌───────────────┐
        │                        │               │ beamai_agent  │
        │                        │               │   (Agent)     │
        │                        │               └───────┬───────┘
        │                        │                       │
        └────────────────────────┼───────────────────────┘
                                 │
    ┌────────────────────────────┼────────────────────────────┐
    │                 ┌──────────┼──────────┐                 │
    ▼                 ▼          ▼          ▼                 ▼
┌───────────┐   ┌───────────┐  ┌───────────┐  ┌───────────────────┐
│beamai_rag │   │beamai_llm │  │ beamai_   │  │    beamai_tools   │
│  (RAG)    │   │  (LLM)    │  │  memory   │  │ (工具+Middleware) │
└─────┬─────┘   └─────┬─────┘  └─────┬─────┘  └─────────┬─────────┘
      │               │              │                  │
      │               │              │  ┌────────────────┐
      │               │              │  │beamai_cognition │
      │               │              │  │   (认知架构)     │
      │               │              │  │ 依赖: memory     │
      │               │              │  │ 可选: llm        │
      │               │              │  └──┬──────┬──┬───┘
      │               │              │     │      │  │
      │               │ 实现         │     │      │  │(可选)
      │               │ Behaviour   │◄────┘      │  └───┐
      │               └──────┬──────┘────────────┘      │
      │                      │                          │
      │                      ▼                          │
      │         ┌────────────────────────┐              │
      │         │ Behaviour 接口定义      │              │
      │         │ (beamai_chat_behaviour,│              │
      │         │  beamai_process_store_  │              │
      │         │  behaviour)            │              │
      │         └────────────┬───────────┘              │
      │                      │                          │
      └──────────────────────┤──────────────────────────┘
                             ▼
                  ┌───────────────────────────┐
                  │       beamai_core         │  ← 基础层
                  │ (类型、Graph、Behaviour)   │
                  └───────────────────────────┘
                                 │
                                 ▼
                  ┌───────────────────────────┐
                  │   Erlang/OTP + 外部依赖    │
                  └───────────────────────────┘

        - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        运行时可选依赖（通过 Adapter 注入，非编译依赖）：
        beamai_tools ···> beamai_llm (llm_client)
        beamai_tools ···> beamai_cognition (beamai_conversation_buffer)
```

**依赖方向说明**：
- 实线箭头(→)表示**编译时依赖**（从上到下）
- 虚线箭头(···>)表示**运行时可选依赖**（通过 Adapter 注入）
- `beamai_deepagent` **不依赖** `beamai_agent`，它们是平行的实现
- `beamai_a2a` 依赖 `beamai_agent`（用于 Agent 执行）
- `beamai_mcp` 在适配器层**可选依赖** `beamai_agent`（用于工具转换）
- `beamai_tools`、`beamai_llm`、`beamai_memory` 同层级，都只依赖 `beamai_core`
- `beamai_cognition` 依赖 `beamai_core` + `beamai_memory`，可选依赖 `beamai_llm`
- `beamai_core` 定义 Behaviour 接口（`beamai_chat_behaviour`、`beamai_process_store_behaviour` 等），上层模块实现这些接口
- `beamai_core` 不依赖 `beamai_memory`，通过 `{Module, Ref}` 动态分发实现解耦
- `beamai_tools` 通过 Adapter 模式在**运行时**使用 beamai_llm/context，无编译依赖

### 各应用依赖详情

#### beamai_core（核心库）

**依赖**: 无内部依赖

**提供功能**:
- Graph 执行引擎（Pregel 模型）
- 状态管理（graph_state）
- 类型定义（beamai_types）
- 通用工具函数（beamai_utils）
- JSON-RPC 支持（beamai_jsonrpc）
- SSE 支持（beamai_sse）
- **HTTP 客户端**（可插拔后端）
  - `beamai_http` - 统一 API
  - `beamai_http_hackney` - Hackney 后端（默认）
  - `beamai_http_gun` - Gun 后端（HTTP/2 支持）
  - `beamai_http_pool` - Gun 连接池管理
- **Behaviour 定义**（用于解耦依赖）
  - `beamai_chat_behaviour` - LLM 聊天接口（原 beamai_llm_behaviour）
  - `beamai_process_store_behaviour` - 流程存储接口（含分支/时间旅行可选回调）
  - `beamai_http_behaviour` - HTTP 客户端接口

#### beamai_memory（纯存储引擎）

**依赖**: beamai_core

**提供功能**:
- 存储后端
  - ETS 存储（beamai_store_ets）
  - SQLite 存储（beamai_store_sqlite）
- 快照管理（beamai_process_snapshot）
- 流程存储（beamai_process_memory_store）— 实现 beamai_process_store_behaviour
- 状态存储（beamai_state_store）

#### beamai_cognition（认知架构）

**依赖**: beamai_core, beamai_memory

**可选依赖**: beamai_llm

**提供功能**:
- 语义记忆（beamai_semantic_memory）
- 情景记忆（beamai_episodic_memory）
- 程序记忆（beamai_procedural_memory）
- 技能记忆（beamai_skill_memory）
- 记忆检索与整合算法
- 对话缓冲（beamai_conversation_buffer）
- 上下文摘要（beamai_context_summarizer）

#### beamai_tools（工具系统 + 中间件系统）

**依赖**:
- beamai_core（Behaviour 定义、类型定义）

**可选依赖**（通过 Adapter 模式解耦）:
- beamai_llm（LLM 客户端，用于智能工具筛选/模拟 Middleware）
- beamai_memory（对话缓冲，用于摘要 Middleware）

**Adapter 模块**:
- `beamai_llm_adapter` - 封装 LLM 调用，默认使用 `llm_client`
- `beamai_buffer_adapter` - 封装对话缓冲，默认使用 `beamai_conversation_buffer`

**提供功能**:
- 工具定义与注册（beamai_tool, beamai_tool_registry）
- 工具提供者（beamai_tool_provider）
- 工具安全（beamai_tool_security）
- 内置工具
  - 文件工具（beamai_tools_file）
  - Shell 工具（beamai_tools_shell）
  - Todo 工具（beamai_tools_todo）
  - 人机交互工具（beamai_tools_human）
- Middleware 系统
  - Middleware 行为定义（beamai_middleware）
  - Middleware 运行器（beamai_middleware_runner）
  - 预设 Middleware（beamai_middleware_presets）
  - 内置 Middleware（middleware_call_limit, middleware_summarization 等）

#### beamai_llm（LLM 集成）

**依赖**:
- beamai_core

**提供功能**:
- LLM 客户端（llm_client）
- 多提供商支持
  - OpenAI（llm_provider_openai）
  - Anthropic（llm_provider_anthropic）
  - DeepSeek（llm_provider_deepseek）- OpenAI 兼容 API
  - Ollama（llm_provider_ollama）
  - 智谱 AI（llm_provider_zhipu）
  - 阿里云百炼（llm_provider_bailian）- DashScope 原生 API
- 消息适配器（llm_message_adapter）
- 工具适配器（llm_tool_adapter）
- 响应解析器（llm_response_parser）

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
- beamai_core（头文件：类型定义）
- beamai_llm（LLM 调用）
- beamai_memory（检查点、记忆管理）
- beamai_tools（头文件：工具定义）

> **注意**: beamai_agent 是核心编排层，**不依赖** beamai_mcp。
> MCP 工具通过 beamai_mcp 的适配器层集成到 Agent，而非反向依赖。

**提供功能**:
- Agent 生命周期管理（beamai_agent）
- Agent 初始化（beamai_agent_init）
- Agent 运行器（beamai_agent_runner）
- 图节点
  - LLM 节点（beamai_llm_node）
  - 工具节点（beamai_tool_node）
  - Middleware 集成节点（beamai_middleware_nodes）
- 回调系统（beamai_callback_utils）

#### beamai_deepagent（Deep Agent 系统）

**依赖**:
- beamai_core
- beamai_llm
- beamai_memory
- beamai_tools（包含 Middleware 系统）

**提供功能**:
- Deep Agent 核心（beamai_deepagent）
- 计划系统（beamai_deepagent_plan）
- 工具系统
  - 文件系统工具（beamai_deepagent_fs_*）
  - Todo 工具（beamai_deepagent_todo_*）
  - 人机交互工具（beamai_deepagent_human_*）
- 执行追踪（beamai_deepagent_trace）
- CLI UI（beamai_deepagent_cli_ui）

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
- beamai_tools（头文件：工具定义）
- beamai_agent（**可选**，仅适配器层使用）

**提供功能**:
- MCP 服务器（beamai_mcp_server）
- MCP 客户端（beamai_mcp_client）
- 传输层
  - HTTP 传输
    - beamai_mcp_transport_http（Hackney 后端，默认）
    - beamai_mcp_transport_http_gun（Gun 后端，HTTP/2 支持）
  - SSE 传输
    - beamai_mcp_transport_sse（Hackney 后端，默认）
    - beamai_mcp_transport_sse_gun（Gun 后端，HTTP/2 支持）
  - Stdio 传输（beamai_mcp_transport_stdio）
- 工具代理（beamai_mcp_tool_proxy）
- Agent 适配器（beamai_mcp_adapter）- 将 MCP 工具转换为 Agent 工具

**传输层后端配置**:

```erlang
%% 使用 Gun 后端（默认，支持 HTTP/2）
Config = #{
    transport => http,  %% 或 sse
    url => <<"https://example.com/mcp">>
}.

%% 使用 Hackney 后端
Config = #{
    transport => http,  %% 或 sse
    backend => hackney,
    url => <<"https://example.com/mcp">>
}.
```

> **注意**: MCP 核心功能可独立运行，不依赖 beamai_agent。
> 仅在需要将 MCP 工具集成到 Agent 系统时，才使用适配器层。

#### beamai_examples（示例）

**依赖**:
- beamai_core
- beamai_llm
- beamai_memory
- beamai_tools
- beamai_agent
- beamai_deepagent
- beamai_a2a
- beamai_mcp

**提供功能**:
- 简单 Agent 示例（example_agent_simple）
- Deep Agent 示例（example_agent_deep）
- 交互式 Agent 示例（example_agent_interactive）

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
| inets/httpc | HTTP 客户端（备用） |

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

- **Erlang/OTP**: 25.0 或更高版本
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
