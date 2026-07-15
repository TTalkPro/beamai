# BeamAI - Erlang Agent Framework

[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-26%2B-red.svg)](https://www.erlang.org/)
[![Build](https://img.shields.io/badge/build-rebar3-brightgreen.svg)](https://rebar3.org/)

[English](README_EN.md) | 中文

基于 Erlang/OTP 的高性能 AI Agent 应用框架核心库，提供构建 Agent 的基础能力。

> **项目说明**: 本项目是 BeamAI 框架的核心库，提供 Kernel、Filter（含会话记忆）、LLM 客户端和 SimpleAgent 等核心功能。
>
> 高级功能（Deep Agent、Process Framework 流程编排、存储/快照引擎、Tools 库、RAG、A2A/MCP 协议等）已迁移到 [beamai_extra](https://github.com/TTalkPro/beamai_extra) 扩展项目中。

## 核心功能与扩展

### 核心项目 (本项目)
提供构建 AI Agent 的基础设施（三大核心职责）：
- **beamai_core** - Kernel 基座：Context、Filter（洋葱式 around 模型）、Tool 的构建与调用
- **beamai_agent** - SimpleAgent：以 ReAct 为主的 Agent 框架（上下文记忆由 filter-memory 实现、多轮对话、回调、中断/恢复）
- **beamai_llm** - 统一的 LLM 客户端（支持 OpenAI、Anthropic、DeepSeek、Zhipu、DashScope、Ollama）

### 扩展项目 ([beamai_extra](https://github.com/TTalkPro/beamai_extra))
基于核心库构建的高级功能：
- **Deep Agent** - 基于 SubAgent 架构的递归规划 Agent
- **Tools 库** - 文件、Shell、HTTP 等常用工具集合
- **RAG** - 检索增强生成
- **协议支持** - A2A (Agent-to-Agent)、MCP (Model Context Protocol)

## 特性

- **Kernel/Tool 架构**: 语义化的工具注册和调用系统
  - 基于 Semantic Kernel 理念的 Kernel 核心（无状态，不记录消息）
  - 统一的 Tool 定义和管理
  - Filter 洋葱式拦截和安全验证

- **会话记忆 (Memory Filter)**: 对话历史与 Kernel 解耦
  - 每次 invoke 只传单条最新消息，历史由 Memory Filter 按 `conversation_id` 管理
  - 可插拔存储后端（ETS 默认实现 / 滑动窗口包装 / 自定义 behaviour）
  - 详见 [docs/MEMORY.md](docs/MEMORY.md)

- **统一 LLM 客户端**: 6 家 Provider 统一同步/流式
  - OpenAI、Anthropic、DeepSeek、Zhipu、DashScope、Ollama
  - 多模态输入、Anthropic 缓存/Web Search/引用、速率限制头、Retry-After 重试、统一错误结构

- **Output Parser**: 结构化输出
  - JSON/XML/CSV 解析
  - 自动重试机制

## 快速开始

### 1. 启动 Shell

```bash
export ZHIPU_API_KEY=your_key_here
rebar3 shell
```

### 2. LLM 调用

```erlang
%% 创建 LLM 配置
LLM = beamai_chat_completion:create(zhipu, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY"))
}),

%% 发送聊天请求
{ok, Response} = beamai_chat_completion:chat(LLM, [
    {role, user, content, <<"你好！"/utf8>>}
]),
```

### 3. Kernel + Tool（工具注册）

```erlang
%% 创建 Kernel
Kernel = beamai_kernel:new(),

%% 定义 Tool
SearchTool = #{
    name => <<"search">>,
    description => <<"搜索信息"/utf8>>,
    parameters => #{
        <<"query">> => #{type => string, required => true, description => <<"搜索关键词"/utf8>>}
    },
    handler => fun(#{<<"query">> := Query}, _Context) ->
        {ok, <<"搜索结果: ", Query/binary>>}
    end
},

%% 注册工具
Kernel1 = beamai_kernel:add_tool(Kernel, SearchTool),

%% 调用单个工具
{ok, Result, _NewCtx} = beamai_kernel:invoke_tool(Kernel1, <<"search">>, #{
    <<"query">> => <<"Erlang"/utf8>>
}, beamai_context:new()).
```

### 4. Filter（洋葱式拦截）

```erlang
%% 一个 filter 含 3 个可选 around hook（around_chat/around_tool/around_turn）
%% 每个 around 用单个闭包 fun(Req, FCtx, Next) -> Resp 包裹同一次调用，
%% 前置/后置同处一处，不调 Next 即短路。
%% filter 在构建 kernel 时一次性给出，注册顺序即层序（列表靠前 = 外层）。

%% 一个 around_tool：参数校验（短路）+ 结果翻倍
ValidateTransform = beamai:filter(<<"validate_transform">>, #{
    around_tool => fun(#{args := #{a := A}, context := Ctx} = Req, _FCtx, Next) ->
        case A > 1000 of
            true ->
                %% 参数超限：不调 Next，短路跳过工具执行
                #{result => {error, <<"a exceeds limit">>}, context => Ctx};
            false ->
                %% 正常：进入内层后把结果翻倍
                #{result := Result} = Resp = Next(Req),
                case is_number(Result) of
                    true  -> Resp#{result => Result * 2};
                    false -> Resp
                end
        end
    end
}),

K0 = beamai:kernel(#{}, [ValidateTransform]),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})),

%% 调用（3 + 5 = 8，后置翻倍后 = 16）
{ok, 16, _} = beamai:invoke_tool(K1, <<"add">>, #{a => 3, b => 5}, beamai:context()).
```

详见 [Filter 文档](docs/FILTER.md)。

> **流程编排 / 状态快照** 已迁移到 [beamai_extra](https://github.com/TTalkPro/beamai_extra)（Process Framework、存储/快照引擎）。

### 5. Output Parser（结构化输出）

```erlang
%% 创建 JSON 解析器
Parser = beamai_output_parser:json(#{
    schema => #{
        type => object,
        properties => #{
            <<"title">> => #{type => string},
            <<"count">> => #{type => integer}
        },
        required => [<<"title">>, <<"count">>]
    }
}),

%% 解析 LLM 响应
{ok, Parsed} = beamai_output_parser:parse(Parser, LLMResponse).
```

## 架构

### 应用结构

```
apps/
├── beamai_core/        # 核心框架
│   ├── Kernel         # beamai_kernel, beamai_tool, beamai_context,
│   │                  # beamai_filter, beamai_prompt, beamai_result
│   ├── Memory Filter  # beamai_memory_filter（会话历史按 conversation_id 管理）
│   ├── HTTP           # beamai_http, beamai_http_gun, beamai_http_hackney,
│   │                  # beamai_http_pool
│   ├── Behaviours     # beamai_chat_behaviour, beamai_http_behaviour
│   └── Utils          # beamai_id, beamai_jsonrpc, beamai_sse, beamai_utils
│
├── beamai_llm/         # LLM 客户端
│   ├── Chat           # beamai_chat_completion, beamai_llm_error
│   ├── Parser         # beamai_output_parser, beamai_parser_json
│   ├── Adapters       # beamai_llm_message_adapter, beamai_llm_response_parser, beamai_llm_tool_adapter
│   └── Providers      # OpenAI, Anthropic, DeepSeek, Zhipu, DashScope, Ollama
│
└── beamai_agent/       # SimpleAgent（ReAct）
    └── Agent          # beamai_agent, beamai_agent_state, beamai_agent_tool_loop,
                       # beamai_agent_callbacks, beamai_agent_interrupt
```

> **流程编排引擎与存储/快照引擎**（原 beamai_process / beamai_memory）已迁移到
> [beamai_extra](https://github.com/TTalkPro/beamai_extra)，不再属于本项目。

### 依赖关系

```
┌───────────────────────┐ ┌───────────────────────┐
│   Agent 层            │ │   LLM 层              │
│  (beamai_agent)       │ │  (beamai_llm)         │
└───────────┬───────────┘ └───────────┬───────────┘
            │                         │
┌───────────┴─────────────────────────┴───────────┐
│   核心层                                         │
│  (beamai_core)                                   │
└─────────────────────────────────────────────────┘
```

> beamai_core 通过 Behaviour 接口和 `{Module, Ref}` 动态分发模式解耦，
> 不依赖上层应用。beamai_llm 和 beamai_agent 平级，互不依赖。

## 核心概念

### 1. Kernel 架构

Kernel 是 BeamAI 的核心抽象，管理 Tool 的注册与调用：

```erlang
%% 创建 Kernel 实例
Kernel = beamai_kernel:new(),

%% 从 Tool 模块加载工具
Kernel1 = beamai_kernel:add_tool_module(Kernel, beamai_tool_file),

%% 或添加单个工具
Tool = #{
    name => <<"read_file">>,
    description => <<"读取文件内容"/utf8>>,
    parameters => #{
        <<"path">> => #{type => string, required => true}
    },
    handler => fun(#{<<"path">> := Path}, _Ctx) ->
        file:read_file(Path)
    end
},
Kernel2 = beamai_kernel:add_tool(Kernel1, Tool),

%% 调用注册的工具
{ok, Result, _NewCtx} = beamai_kernel:invoke_tool(Kernel2, <<"read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}, beamai_context:new()).
```

### 2. 会话记忆 (Memory Filter)

Kernel 本身无状态、不记录消息；多轮对话历史由 **Memory Filter**（`beamai_memory_filter`）以 `conversation_id` 为单位管理。每次 invoke 只携带单条最新消息，filter 负责注入历史与持久化增量。

- 可插拔存储后端（ETS 默认实现 / 滑动窗口包装 / 自定义 behaviour）
- SimpleAgent 的跨轮记忆即基于此实现
- 详见 [docs/MEMORY.md](docs/MEMORY.md)

## 配置

### LLM 配置

LLM 配置使用 `beamai_chat_completion:create/2` 创建：

```erlang
%% 创建 LLM 配置
LLM = beamai_chat_completion:create(zhipu, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    temperature => 0.7
}).

%% 发送请求
{ok, Response} = beamai_chat_completion:chat(LLM, [
    {role, user, content, <<"你好"/utf8>>}
]).
```

**支持的 Provider：**

| Provider | 模块 | API 模式 | 说明 |
|----------|------|----------|------|
| `anthropic` | beamai_llm_provider_anthropic | Anthropic | Anthropic Claude API |
| `openai` | beamai_llm_provider_openai | OpenAI | OpenAI API |
| `deepseek` | beamai_llm_provider_deepseek | OpenAI 兼容 | DeepSeek API |
| `zhipu` | beamai_llm_provider_zhipu | OpenAI 兼容 | 智谱 AI (GLM 系列) |
| `dashscope` | beamai_llm_provider_dashscope | DashScope 原生 | 阿里云百炼 (通义千问系列) |
| `ollama` | beamai_llm_provider_ollama | OpenAI 兼容 | Ollama 本地模型 |

### HTTP 后端配置

BeamAI 支持 Gun 和 Hackney 两种 HTTP 后端，默认使用 Gun（支持 HTTP/2）。

```erlang
%% 在 sys.config 中配置（可选）
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pool, #{
        max_connections => 100,
        connection_timeout => 30000
    }}
]}.
```

| 特性 | Gun（默认） | Hackney |
|------|-------------|---------|
| HTTP/2 | 支持 | 不支持 |
| 连接池 | 内置 beamai_http_pool | 依赖 hackney 池 |
| TLS | 自动使用系统 CA 证书 | hackney 默认配置 |
| 适用场景 | 推荐生产环境 | 兼容旧系统 |

## 文档

### 核心文档

- **[docs/API_REFERENCE.md](docs/API_REFERENCE.md)** - API 参考文档
- **[docs/FILTER.md](docs/FILTER.md)** - Filter 洋葱系统文档
- **[docs/MEMORY.md](docs/MEMORY.md)** - 会话记忆（Memory Filter）文档
- **[docs/OUTPUT_PARSER.md](docs/OUTPUT_PARSER.md)** - Output Parser 指南
- **[docs/DEPENDENCIES.md](docs/DEPENDENCIES.md)** - 依赖关系详解

### 模块文档

| 模块 | 说明 | 文档 |
|------|------|------|
| **beamai_core** | 核心框架：Kernel、Context、Filter、Tool、HTTP、Behaviours | [README](apps/beamai_core/README.md) |
| **beamai_agent** | SimpleAgent：ReAct Agent 框架（多轮对话、回调、中断/恢复） | [README](apps/beamai_agent/README.md) |
| **beamai_llm** | LLM 客户端：6 家 Provider 统一同步/流式；多模态输入、Anthropic 缓存/Web Search/引用、速率限制头、Retry-After 重试、统一错误结构 | [README](apps/beamai_llm/README.md) |

## 运行示例

```bash
# 编译
rebar3 compile

# 启动 Shell
rebar3 shell
```

## 项目统计

| 指标 | 数量 |
|------|------|
| **OTP 应用** | 3 个（beamai_core、beamai_agent、beamai_llm） |
| **源代码模块** | ~73 个 |
| **测试文件** | ~38 个 |
| **单元测试** | ~380 个 |

### 测试运行

```bash
# 运行所有测试
rebar3 eunit

# 运行特定应用的测试
rebar3 eunit --app=beamai_llm

# 运行代码检查
rebar3 dialyzer
```

## 性能

- 基于 Erlang/OTP 轻量级进程
- 并发工具调用
- HTTP 连接池（Gun，支持 HTTP/2）
- ETS 高速存储

## 设计原则

- **简单**: 清晰的 API，易于理解
- **模块化**: 每个模块职责单一
- **可扩展**: Behaviour 设计，易于自定义
- **高性能**: 利用 Erlang 并发特性
- **可观测**: 完善的日志、追踪、监控

## 许可证

Apache-2.0

## 贡献

欢迎提交 Issue 和 Pull Request！
