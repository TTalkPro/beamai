# BeamAI - Erlang Agent Framework

[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-26%2B-red.svg)](https://www.erlang.org/)
[![Build](https://img.shields.io/badge/build-rebar3-brightgreen.svg)](https://rebar3.org/)

[English](README_EN.md) | 中文

基于 Erlang/OTP 的高性能 AI Agent 应用框架，提供完整的 Agent 开发工具链。

## 特性

- **Kernel/Tool 架构**: 语义化的工具注册和调用系统
  - 基于 Semantic Kernel 理念的 Kernel 核心
  - 统一的 Tool 定义和管理
  - Filter 过滤器和安全验证

- **Process Framework**: 可编排的流程引擎
  - 支持步骤定义、条件分支、并行执行
  - 时间旅行和分支回溯
  - 事件驱动和状态快照

- **Simple Agent**: 基于工具循环的 ReAct Agent
  - 支持自定义工具和系统提示词
  - 内置 Memory 持久化
  - 完整的回调系统
  - 中断和恢复支持

- **Deep Agent**: 基于 SubAgent 架构的递归规划 Agent
  - Planner（规划器）→ Executor（执行器）→ Reflector（反思器）
  - 支持并行子任务执行
  - Coordinator 多 Agent 协调

- **Graph 引擎**: 基于 LangGraph 的图计算
  - Graph Builder/DSL 构建器
  - Pregel 分布式计算模型
  - 状态快照和条件边

- **Output Parser**: 结构化输出
  - JSON/XML/CSV 解析
  - 自动重试机制

- **协议支持**: A2A 和 MCP
  - Agent-to-Agent 通信协议
  - Model Context Protocol 集成

- **RAG**: 检索增强生成
  - 向量嵌入和相似度搜索
  - 文本分割

## 快速开始

### 1. 启动 Shell

```bash
export ZHIPU_API_KEY=your_key_here
rebar3 shell
```

### 2. Simple Agent（基本用法）

```erlang
%% 创建 LLM 配置（使用 beamai_chat_completion:create/2）
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

%% 创建 Agent 状态（纯函数 API）
{ok, State} = beamai_agent:new(#{
    system_prompt => <<"你是一个有帮助的助手。"/utf8>>,
    llm => LLM
}),

%% 运行 Agent
{ok, Result, _NewState} = beamai_agent:run(State, <<"你好！"/utf8>>),

%% 查看结果
Response = maps:get(final_response, Result).
```

### 3. Simple Agent（多轮对话）

```erlang
%% 多轮对话通过状态传递实现
{ok, State0} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"你是一个记忆助手。"/utf8>>
}),
{ok, _, State1} = beamai_agent:run(State0, <<"我叫张三"/utf8>>),
{ok, Result, _State2} = beamai_agent:run(State1, <<"我叫什么名字？"/utf8>>).
%% Result 中 Agent 会记得用户叫张三
```

### 4. Simple Agent（使用 Kernel + Tool 注册工具）

```erlang
%% 创建 Kernel
Kernel = beamai_kernel:new(),

%% 从 Tool 模块加载工具
Kernel1 = beamai_kernel:add_tool_module(Kernel, beamai_tool_shell),

%% 或直接定义 Tool
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

Kernel2 = beamai_kernel:add_tool(Kernel1, SearchTool),

%% 获取工具规格供 Agent 使用
Tools = beamai_kernel:get_tool_specs(Kernel2),

%% 创建带工具的 Agent
{ok, State} = beamai_agent:new(#{
    system_prompt => <<"你是搜索助手。"/utf8>>,
    tools => Tools,
    llm => LLM
}),

{ok, Result, _} = beamai_agent:run(State, <<"搜索 Erlang 教程"/utf8>>).
```

### 5. Simple Agent（带 Memory 持久化）

```erlang
%% 创建存储后端
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% 创建带 Memory 的 Agent（checkpoint 自动保存）
{ok, State0} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"你是持久化助手。"/utf8>>,
    storage => Memory
}),

%% 对话（checkpoint 自动保存）
{ok, _, State1} = beamai_agent:run(State0, <<"记住：密码是 12345"/utf8>>),
{ok, _, _State2} = beamai_agent:run(State1, <<"好的"/utf8>>),

%% 稍后恢复会话
{ok, RestoredState} = beamai_agent:restore_from_memory(#{llm => LLM}, Memory),
{ok, Result, _} = beamai_agent:run(RestoredState, <<"密码是多少？"/utf8>>).
%% Agent 会记得密码是 12345
```

### 6. Deep Agent（SubAgent 编排）

```erlang
%% 创建 Deep Agent 配置（new/1 直接返回 config map）
Config = beamai_deepagent:new(#{
    llm => LLM,
    max_depth => 3,
    planning_enabled => true,
    reflection_enabled => true,
    system_prompt => <<"你是一个研究专家。"/utf8>>,
    %% 使用 Tool 模块提供工具
    plugins => [beamai_tool_file, beamai_tool_shell]
}),

%% 运行复杂任务（Planner → Executor → Reflector）
{ok, Result} = beamai_deepagent:run(Config,
    <<"分析这个代码库的架构并给出优化建议。"/utf8>>),

%% 查看执行计划和轨迹
Plan = beamai_deepagent:get_plan(Result),
Trace = beamai_deepagent:get_trace(Result).
```

### 7. Process Framework（流程编排）

```erlang
%% 使用 Process Builder 构建流程
{ok, Process} = beamai_process_builder:new(<<"research_pipeline">>)
    |> beamai_process_builder:add_step(<<"research">>, #{
        handler => fun(Input, _Ctx) -> {ok, do_research(Input)} end
    })
    |> beamai_process_builder:add_step(<<"write">>, #{
        handler => fun(Input, _Ctx) -> {ok, do_write(Input)} end
    })
    |> beamai_process_builder:add_step(<<"review">>, #{
        handler => fun(Input, _Ctx) -> {ok, do_review(Input)} end
    })
    |> beamai_process_builder:build(),

%% 执行流程
{ok, Result} = beamai_process_executor:run(Process, #{
    task => <<"研究 Erlang 并发模型"/utf8>>
}).
```

### 8. Output Parser（结构化输出）

```erlang
%% 创建 JSON 解析器
Parser = beamai_output_parser:json(#{
    schema => #{
        type => object,
        properties => #{
            <<"title">> => #{type => string},
            <<"count">> => #{type => integer},
            <<"items">> => #{type => array, items => #{type => string}}
        },
        required => [<<"title">>, <<"count">>]
    }
}),

%% 解析 LLM 响应
{ok, Parsed} = beamai_output_parser:parse(Parser, LLMResponse).

%% 带重试的解析
{ok, Parsed} = beamai_output_parser:parse_with_retry(Parser, LLMResponse, #{
    max_retries => 3
}).
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
│   ├── Behaviours     # beamai_llm_behaviour, beamai_http_behaviour,
│   │                  # beamai_step_behaviour, beamai_process_store_behaviour
│   └── Utils          # beamai_id, beamai_jsonrpc, beamai_sse, beamai_utils
│
├── beamai_graph/       # Graph 计算引擎
│   ├── Core           # graph, graph_node, graph_edge
│   ├── Builder        # graph_builder, graph_dsl
│   ├── Runner         # graph_runner, graph_snapshot
│   ├── State          # graph_state, graph_state_reducer, graph_command
│   └── Pregel         # pregel, pregel_master, pregel_worker, pregel_vertex
│
├── beamai_plugin/      # 工具和中间件系统
│   ├── Core           # beamai_plugins, beamai_tool_behaviour
│   ├── Middleware     # beamai_middleware, beamai_middleware_runner,
│   │                  # middleware_call_limit, middleware_tool_retry
│   ├── Security       # beamai_tool_security
│   └── Tools          # beamai_tool_file, beamai_tool_shell,
│                      # beamai_tool_human, beamai_tool_todo
│
├── beamai_llm/         # LLM 客户端
│   ├── Chat           # beamai_chat_completion
│   ├── Parser         # beamai_output_parser, beamai_parser_json
│   ├── Adapters       # llm_message_adapter, llm_response_adapter, llm_tool_adapter
│   └── Providers      # OpenAI, Anthropic, DeepSeek, Zhipu, Bailian, Ollama
│
├── beamai_agent/       # Agent 实现
│   ├── Core           # beamai_agent, beamai_agent_state, beamai_agent_callbacks
│   ├── Memory         # beamai_agent_memory
│   ├── Execution      # beamai_agent_tool_loop, beamai_agent_interrupt
│   └── Process Agent  # beamai_process_agent, beamai_process_agent_llm_step,
│                      # beamai_process_agent_tool_step
│
├── beamai_deepagent/   # Deep Agent（SubAgent 架构）
│   ├── Core           # beamai_deepagent, beamai_deepagent_plan,
│   │                  # beamai_deepagent_dependencies, beamai_deepagent_trace
│   └── SubAgents      # beamai_deepagent_planner, beamai_deepagent_executor,
│                      # beamai_deepagent_reflector, beamai_deepagent_parallel,
│                      # beamai_deepagent_coordinator
│
├── beamai_memory/      # 内存和上下文存储
│   ├── Context        # 上下文管理
│   ├── Store          # ETS/SQLite 存储后端
│   └── Snapshot       # 快照、分支、时间旅行
│
├── beamai_a2a/         # A2A 协议实现
│   ├── Server         # A2A 服务端
│   └── Client         # A2A 客户端
│
├── beamai_mcp/         # MCP 协议实现
│   ├── Server         # MCP 服务端
│   └── Client         # MCP 客户端
│
└── beamai_rag/         # RAG 功能
    ├── Embeddings     # 向量嵌入
    └── Vector Store   # 向量存储
```

### 依赖关系

```
┌─────────────────────────────────────┐
│   Agent 实现层                       │
│  (beamai_agent, beamai_deepagent)   │
└────────────────┬────────────────────┘
                 │
┌────────────────┴────────────────────┐
│   服务层                             │
│  (beamai_llm, beamai_plugin,        │
│   beamai_rag, beamai_a2a, beamai_mcp)│
└────────────────┬────────────────────┘
                 │
┌────────────────┴────────────────────┐
│   核心层                             │
│  (beamai_core, beamai_graph,        │
│   beamai_memory)                     │
└─────────────────────────────────────┘
```

详见 [DEPENDENCIES.md](doc/DEPENDENCIES.md)

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

基于 LangGraph 理念的图计算引擎（位于 beamai_graph 应用）：

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

使用 beamai_memory 实现会话持久化和时间旅行：

```erlang
%% 创建 Memory
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% 创建带 storage 的 Agent（checkpoint 自动保存）
{ok, State} = beamai_agent:new(#{llm => LLM, storage => Memory}),
{ok, _, NewState} = beamai_agent:run(State, <<"你好"/utf8>>),

%% 从 Memory 恢复会话
{ok, RestoredState} = beamai_agent:restore_from_memory(#{llm => LLM}, Memory).
```

### 5. Callbacks（回调系统）

监听 Agent 执行事件：

```erlang
{ok, State} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"你是助手"/utf8>>,
    callbacks => #{
        on_llm_start => fun(Prompts, Meta) ->
            io:format("LLM 调用开始~n")
        end,
        on_tool_start => fun(ToolName, Args, Meta) ->
            io:format("执行工具: ~ts~n", [ToolName])
        end,
        on_agent_finish => fun(Result, Meta) ->
            io:format("Agent 完成~n")
        end
    }
}).
```

## 配置

### LLM 配置

LLM 配置使用 `beamai_chat_completion:create/2` 创建：

```erlang
%% 创建 LLM 配置
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    temperature => 0.7
}),

%% 配置可在多个 Agent 间复用
{ok, State1} = beamai_agent:new(#{llm => LLM, system_prompt => <<"研究助手"/utf8>>}),
{ok, State2} = beamai_agent:new(#{llm => LLM, system_prompt => <<"写作助手"/utf8>>}).
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

- **[doc/API_REFERENCE.md](doc/API_REFERENCE.md)** - API 参考文档
- **[doc/MIDDLEWARE.md](doc/MIDDLEWARE.md)** - Middleware 系统文档
- **[doc/CALLBACKS.md](doc/CALLBACKS.md)** - Callback 回调系统文档
- **[doc/ARCHITECTURE.md](doc/ARCHITECTURE.md)** - 架构设计
- **[DEPENDENCIES.md](doc/DEPENDENCIES.md)** - 依赖关系详解

### 模块文档

| 模块 | 说明 | 文档 |
|------|------|------|
| **beamai_core** | 核心框架：Kernel、Process Framework、HTTP、Behaviours | [README](apps/beamai_core/README.md) |
| **beamai_graph** | Graph 引擎：图构建、执行、Pregel 分布式计算 | [README](apps/beamai_graph/README.md) |
| **beamai_plugin** | 工具和中间件系统：Tool 模块、Middleware、安全验证 | [README](apps/beamai_plugin/README.md) |
| **beamai_llm** | LLM 客户端：支持 OpenAI、Anthropic、DeepSeek、Zhipu、Bailian、Ollama | [README](apps/beamai_llm/README.md) |
| **beamai_agent** | Agent 实现：ReAct 模式、回调系统、Process Agent | [README](apps/beamai_agent/README.md) |
| **beamai_deepagent** | Deep Agent：SubAgent 编排、任务规划、并行执行、自我反思 | [README](apps/beamai_deepagent/README.md) |
| **beamai_memory** | 记忆管理：Checkpoint、Store、时间旅行、分支 | [README](apps/beamai_memory/README.md) |
| **beamai_a2a** | A2A 协议：Agent 间通信、服务端/客户端 | [README](apps/beamai_a2a/README.md) |
| **beamai_mcp** | MCP 协议：Model Context Protocol 实现 | [README](apps/beamai_mcp/README.md) |
| **beamai_rag** | RAG 功能：向量嵌入、相似度搜索 | [README](apps/beamai_rag/README.md) |

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
| **OTP 应用** | 10 个 |
| **源代码模块** | 186 个 |
| **测试文件** | 49 个 |
| **代码行数** | ~63,000 行 |
| **单元测试** | 722+ 个 |

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
