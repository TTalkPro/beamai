# BeamAI Examples

本目录包含使用 BeamAI Framework 的示例代码。

## 📁 示例列表

### Agent 示例

#### 🌟 example_agent_interactive.erl
**交互式 Deep Agent** - 持续对话的智能助手
- **功能**: 支持多轮对话、Planning、Reflection、子任务派生
- **工具**: 搜索、计算、时间查询、笔记
- **命令**: `quit`/`exit`、`trace`、`plan`
- **适用**: 交互式应用、对话系统、AI 助手

详细文档: [INTERACTIVE_DEEP_AGENT_GUIDE.md](INTERACTIVE_DEEP_AGENT_GUIDE.md)

#### 📱 example_agent_simple.erl
Simple Agent 基础示例：
- **calculator_example/0**: 计算器 Agent（数学工具）
- **weather_example/0**: 天气查询 Agent（模拟数据）

#### 🧠 example_agent_deep.erl
Deep Agent 高级功能示例：
- **research_example/0**: 研究型 Agent（Planning + Reflection）
- **code_analyzer_example/0**: 代码分析 Agent（子任务派生）

#### 📊 example_agent_graph.erl
基于 Graph 引擎的 Deep Agent 示例

### LLM 示例

#### 💬 example_llm_chat.erl
使用智谱 AI (GLM-4) 的聊天示例，展示不同的对话方式：
- **simple_chat/0**: 最简单的单轮对话
- **chat_with_messages/0**: 使用自定义消息列表
- **chat_with_system_prompt/0**: 带系统提示词的对话
- **multi_turn/0**: 多轮对话示例

#### 🔌 example_llm_anthropic.erl
使用智谱 GLM-4.7 通过 Anthropic API 兼容接口的示例

#### 🔄 example_output_parser.erl
Output Parser 结构化输出示例：
- **json_parse_example/0**: JSON Schema 解析
- **retry_example/0**: 自动重试机制

### 图计算示例

#### 🔮 example_pregel.erl
Pregel 分布式计算算法示例

#### 💾 example_checkpoint.erl
Checkpoint 状态持久化示例

#### ⚡ example_graph_parallel.erl
并行图计算示例

### 集成示例

#### 🌐 example_a2a_server.erl / example_a2a_handler.erl
A2A (Agent-to-Agent) 协议服务器示例

#### 🔧 example_mcp_tools.erl / example_mcp_proxy.erl
MCP (Model Context Protocol) 工具集成示例

## 🚀 使用方法

### 快速开始

```bash
# 1. 设置环境变量
export ZHIPU_API_KEY=your_key_here

# 2. 编译项目
rebar3 compile

# 3. 启动 Shell
rebar3 shell

# 4. 运行示例（在 shell 中）
%% 交互式 Deep Agent
example_agent_interactive:run().

%% Simple Agent
example_agent_simple:calculator_example().

%% Deep Agent
example_agent_deep:research_example().

%% Zhipu 聊天
example_llm_chat:simple_chat().

%% MCP 工具集成
example_mcp_tools:run().
```

### 编译并运行单个示例

```bash
# 编译示例
erlc -I apps/beamai_llm/include -I apps/beamai_core/include \
     examples/beamai_examples/src/example_llm_chat.erl

# 运行
erl -pa apps/beamai_llm/ebin -pa apps/beamai_core/ebin \
    -eval "example_llm_chat:simple_chat()" \
    -s init stop \
    -noshell
```

## 📝 代码示例

### Simple Agent

```erlang
%% 创建带工具的 Agent
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    system_prompt => <<"你是一个有帮助的助手。"/utf8>>,
    tools => [
        #{
            name => <<"calculator">>,
            description => <<"执行数学计算"/utf8>>,
            input_schema => #{
                type => object,
                properties => #{
                    <<"expression">> => #{
                        type => string,
                        description => <<"数学表达式，如 2+3*4">>
                    }
                },
                required => [<<"expression">>]
            },
            handler => fun(#{<<"expression">> := Expr}) ->
                {ok, calculate(Expr)}
            end
        }
    ],
    llm => #{
        provider => zhipu,
        model => <<"glm-4-flash">>,
        api_key => list_to_binary(os:getenv("ZHIPU_API_KEY"))
    }
}).

%% 运行
{ok, Result} = beamai_agent:run(Agent, <<"2+3*4等于多少？"/utf8>>).
```

### Multi 模式协调器

```erlang
%% 创建研究团队
{ok, Team} = beamai_agent:start_multi(<<"research_team">>, #{
    agents => [
        #{
            name => <<"researcher">>,
            role => <<"研究员"/utf8>>,
            system_prompt => <<"你是研究员，负责收集资料。"/utf8>>
        },
        #{
            name => <<"writer">>,
            role => <<"写作者"/utf8>>,
            system_prompt => <<"你是写作者，负责撰写文章。"/utf8>>
        }
    ],
    llm => LLMConfig
}).

%% 运行
{ok, Result} = beamai_agent:run(Team,
    <<"研究并撰写一篇关于 Erlang 的介绍。"/utf8>>).
```

### Deep Agent

```erlang
%% 创建带规划的 Deep Agent
{ok, Agent} = beamai_deepagent:start_link(<<"deep_agent">>, #{
    max_depth => 3,
    planning_enabled => true,
    reflection_enabled => true,
    tools => [...],
    llm => LLMConfig
}).

%% 运行复杂任务
{ok, Result} = beamai_deepagent:run(Agent,
    <<"分析代码库架构并给出优化建议。"/utf8>>).

%% 查看执行计划
{ok, Plan} = beamai_deepagent:get_plan(Agent).
```

## ⚙️ 环境要求

- **Erlang/OTP**: 26+
- **依赖**: beamai_core, beamai_llm
- **环境变量**: `ZHIPU_API_KEY`（或使用的其他 provider）

## 📚 相关文档

- **[README.md](../README.md)** - 项目主页
- **[DEPENDENCIES.md](../DEPENDENCIES.md)** - 依赖关系
- **[doc/QUICK_START.md](../doc/QUICK_START.md)** - 快速开始
- **[doc/ARCHITECTURE.md](../doc/ARCHITECTURE.md)** - 架构设计

## 💡 提示

1. **选择合适的示例**：
   - 初学者：从 `example_agent_simple` 开始
   - 进阶：尝试 `example_agent_deep` 和 `example_output_parser`
   - 实战：运行 `example_agent_interactive`

2. **API Key 配置**：
   ```bash
   export ZHIPU_API_KEY=your_key
   ```

3. **调试技巧**：
   - 使用 `sys:trace(Agent, true)` 启用追踪
   - 使用回调系统监听执行事件
   - 查看 Scratchpad 了解执行步骤

---

**开始探索 BeamAI Framework 的功能吧！** 🚀
