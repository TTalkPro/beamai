# BeamAI - Erlang Agent Framework

[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-26%2B-red.svg)](https://www.erlang.org/)
[![Build](https://img.shields.io/badge/build-rebar3-brightgreen.svg)](https://rebar3.org/)

[English](README_EN.md) | 中文

基于 Erlang/OTP 的高性能 AI Agent 应用框架核心库，提供构建 Agent 的基础能力。

> **项目说明**: 本项目是 BeamAI 框架的核心库，提供 Kernel、Process Framework、Graph 引擎、LLM 客户端和 Memory 管理等核心功能。
>
> 高级功能（Simple Agent、Deep Agent、Tools 库、RAG、A2A/MCP 协议等）已迁移到 [beamai_extra](https://github.com/TTalkPro/beamai_extra) 扩展项目中。

## 核心功能与扩展

### 核心项目 (本项目)
提供构建 AI Agent 的基础设施：
- **beamai_core** - Kernel/Tool 架构、Process Framework、Graph 引擎、HTTP 客户端
- **beamai_llm** - 统一的 LLM 客户端（支持 OpenAI、Anthropic、DeepSeek、Zhipu、Bailian、Ollama）
- **beamai_memory** - 纯存储引擎（快照、Store、状态存储）
- **beamai_cognition** - 认知架构（语义/情景/程序记忆 + 算法）
- **beamai_context** - LLM 上下文管理（对话缓冲、摘要）

### 扩展项目 ([beamai_extra](https://github.com/TTalkPro/beamai_extra))
基于核心库构建的高级功能：
- **Simple Agent** - 基于 ReAct 模式的对话 Agent
- **Deep Agent** - 基于 SubAgent 架构的递归规划 Agent
- **Tools 库** - 文件、Shell、HTTP 等常用工具集合
- **RAG** - 检索增强生成
- **协议支持** - A2A (Agent-to-Agent)、MCP (Model Context Protocol)

## 特性

- **Kernel/Tool 架构**: 语义化的工具注册和调用系统
  - 基于 Semantic Kernel 理念的 Kernel 核心
  - 统一的 Tool 定义和管理
  - Filter 过滤器和安全验证

- **Process Framework**: 可编排的流程引擎
  - 支持步骤定义、条件分支、并行执行
  - 时间旅行和分支回溯
  - 事件驱动和状态快照

- **Graph 引擎**: 基于 LangGraph 的图计算
  - Graph Builder/DSL 构建器
  - Pregel 分布式计算模型
  - 状态快照和条件边

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

%% 调用工具
{ok, Result, _NewCtx} = beamai_kernel:invoke(Kernel1, <<"search">>, #{
    <<"query">> => <<"Erlang"/utf8>>
}, beamai_context:new()).
```

### 4. Filter 过滤器

```erlang
%% 在 Kernel 上注册前置/后置过滤器
K0 = beamai:kernel(),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})),

%% 前置过滤器：参数验证
K2 = beamai:add_filter(K1, <<"validate">>, pre_invocation,
    fun(#{args := #{a := A}} = _Ctx) when A > 1000 ->
        {error, {validation_failed, <<"a exceeds limit">>}};
       (Ctx) ->
        {continue, Ctx}
    end),

%% 后置过滤器：结果转换
K3 = beamai:add_filter(K2, <<"transform">>, post_invocation,
    fun(#{result := Result} = Ctx) ->
        {continue, Ctx#{result => Result * 2}}
    end),

%% 调用（3 + 5 = 8，过滤器翻倍后 = 16）
{ok, 16, _} = beamai:invoke_tool(K3, <<"add">>, #{a => 3, b => 5}, beamai:context()).
```

详见 [Filter 文档](docs/FILTER.md)。

### 5. Process Framework（流程编排）

```erlang
%% 使用 Process Builder 构建流程
{ok, Process} = beamai_process_builder:new(<<"pipeline">>)
    |> beamai_process_builder:add_step(<<"step1">>, #{
        handler => fun(Input, _Ctx) -> {ok, Input#{step => 1}} end
    })
    |> beamai_process_builder:add_step(<<"step2">>, #{
        handler => fun(Input, _Ctx) -> {ok, Input#{step => 2}} end
    })
    |> beamai_process_builder:build(),

%% 执行流程
{ok, Result} = beamai_process_executor:run(Process, #{data => <<"test"/utf8>>}).
```

### 6. Graph 引擎

```erlang
%% 创建图
Builder = graph_builder:new(),
Builder1 = graph_builder:add_node(Builder, start, fun(State) ->
    {ok, State#{step => 1}}
end),
Builder2 = graph_builder:add_node(Builder1, finish, fun(State) ->
    {ok, State}
end),
Builder3 = graph_builder:add_edge(Builder2, start, finish),
Builder4 = graph_builder:set_entry_point(Builder3, start),
Builder5 = graph_builder:set_finish_point(Builder4, finish),

{ok, Graph} = graph_builder:compile(Builder5),
{ok, Result} = graph_runner:run(Graph, #{}).
```

### 7. Memory 持久化

```erlang
%% 创建存储后端
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% 保存和加载状态
ok = beamai_memory:save(Memory, <<"key1">>, #{value => 123}),
{ok, #{value := 123}} = beamai_memory:load(Memory, <<"key1">>).
```

### 8. Output Parser（结构化输出）

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
│   ├── Process        # beamai_process, beamai_process_builder,
│   │                  # beamai_process_runtime, beamai_process_step,
│   │                  # beamai_process_executor, beamai_process_event
│   ├── HTTP           # beamai_http, beamai_http_gun, beamai_http_hackney,
│   │                  # beamai_http_pool
│   ├── Behaviours     # beamai_chat_behaviour, beamai_http_behaviour,
│   │                  # beamai_step_behaviour, beamai_process_store_behaviour
│   ├── Graph          # graph, graph_node, graph_edge, graph_builder, graph_dsl,
│   │                  # graph_runner, graph_snapshot, graph_state, graph_command
│   ├── Pregel         # pregel, pregel_master, pregel_worker, pregel_vertex,
│   │                  # pregel_dispatch_worker
│   └── Utils          # beamai_id, beamai_jsonrpc, beamai_sse, beamai_utils
│
├── beamai_llm/         # LLM 客户端
│   ├── Chat           # beamai_chat_completion
│   ├── Parser         # beamai_output_parser, beamai_parser_json
│   ├── Adapters       # llm_message_adapter, llm_response_parser, llm_tool_adapter
│   └── Providers      # OpenAI, Anthropic, DeepSeek, Zhipu, Bailian, Ollama
│
├── beamai_memory/      # 纯存储引擎
│   ├── Store          # ETS/SQLite 存储后端
│   └── Snapshot       # 快照、分支、时间旅行
│
├── beamai_cognition/   # 认知架构
│   ├── Memory         # 语义/情景/程序记忆
│   └── Algorithms     # 记忆检索与整合算法
│
└── beamai_context/     # LLM 上下文管理
    ├── Buffer         # 对话缓冲
    └── Summarizer     # 上下文摘要
```

### 依赖关系

```
┌─────────────────────────────────────────────────┐
│   服务层                                         │
│  (beamai_llm)                                    │
└────────────────────────┬────────────────────────┘
                         │
┌────────────────────────┴────────────────────────┐
│   存储/认知层                                     │
│  (beamai_memory, beamai_cognition, beamai_context)│
└────────────────────────┬────────────────────────┘
                         │
┌────────────────────────┴────────────────────────┐
│   核心层                                         │
│  (beamai_core)                                   │
└─────────────────────────────────────────────────┘
```

> beamai_core 通过 Behaviour 接口和 `{Module, Ref}` 动态分发模式解耦，
> 不依赖 beamai_memory/cognition/context。

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
{ok, Result, _NewCtx} = beamai_kernel:invoke(Kernel2, <<"read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}, beamai_context:new()).
```

### 2. Process Framework

可编排的流程引擎，支持步骤定义、分支、并行和时间旅行：

```erlang
%% 构建流程
Process = beamai_process_builder:new(<<"my_process">>),
Process1 = beamai_process_builder:add_step(Process, <<"step1">>, #{
    handler => fun(Input, Ctx) -> {ok, transform(Input)} end
}),
{ok, Built} = beamai_process_builder:build(Process1),

%% 执行
{ok, Result} = beamai_process_executor:run(Built, InitialInput).
```

### 3. Graph 执行引擎

基于 LangGraph 理念的图计算引擎（已整合到 beamai_core）：

```erlang
%% 创建图
Builder = graph_builder:new(),
Builder1 = graph_builder:add_node(Builder, start, fun(State) ->
    {ok, State#{step => 1}}
end),
Builder2 = graph_builder:add_node(Builder1, finish, fun(State) ->
    {ok, State}
end),
Builder3 = graph_builder:add_edge(Builder2, start, finish),
Builder4 = graph_builder:set_entry_point(Builder3, start),
Builder5 = graph_builder:set_finish_point(Builder4, finish),

{ok, Graph} = graph_builder:compile(Builder5),
{ok, Result} = graph_runner:run(Graph, #{}).
```

### 4. Memory 持久化

使用 beamai_memory 实现状态持久化和时间旅行：

```erlang
%% 创建 Memory
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% 保存和加载状态
ok = beamai_memory:save(Memory, <<"session1">>, #{data => <<"test"/utf8>>}),
{ok, #{data := <<"test"/utf8>>}} = beamai_memory:load(Memory, <<"session1">>).

%% 创建快照和分支
{ok, SnapshotId} = beamai_memory:create_snapshot(Memory),
{ok, _} = beamai_memory:create_branch(Memory, SnapshotId, <<"experiment">>).
```

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
| `anthropic` | llm_provider_anthropic | Anthropic | Anthropic Claude API |
| `openai` | llm_provider_openai | OpenAI | OpenAI API |
| `deepseek` | llm_provider_deepseek | OpenAI 兼容 | DeepSeek API |
| `zhipu` | llm_provider_zhipu | OpenAI 兼容 | 智谱 AI (GLM 系列) |
| `bailian` | llm_provider_bailian | DashScope 原生 | 阿里云百炼 (通义千问系列) |
| `ollama` | llm_provider_ollama | OpenAI 兼容 | Ollama 本地模型 |

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
- **[docs/FILTER.md](docs/FILTER.md)** - Filter 过滤器系统文档
- **[docs/OUTPUT_PARSER.md](docs/OUTPUT_PARSER.md)** - Output Parser 指南
- **[docs/DEPENDENCIES.md](docs/DEPENDENCIES.md)** - 依赖关系详解

### 模块文档

| 模块 | 说明 | 文档 |
|------|------|------|
| **beamai_core** | 核心框架：Kernel、Process Framework、Graph 引擎、HTTP、Behaviours | [README](apps/beamai_core/README.md) |
| **beamai_llm** | LLM 客户端：支持 OpenAI、Anthropic、DeepSeek、Zhipu、Bailian、Ollama | [README](apps/beamai_llm/README.md) |
| **beamai_memory** | 纯存储引擎：快照管理、Store 后端、状态存储 | [README](apps/beamai_memory/README.md) |
| **beamai_cognition** | 认知架构：语义/情景/程序记忆、检索与整合算法 | - |
| **beamai_context** | LLM 上下文管理：对话缓冲、上下文摘要 | - |

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
| **OTP 应用** | 5 个 |
| **源代码模块** | ~60 个 |
| **测试文件** | ~20 个 |
| **代码行数 | ~20,000 行 |
| **单元测试** | ~200 个 |

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
- Graph 引擎优化执行路径
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
