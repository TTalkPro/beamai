# beamai_agent

简单的 ReAct Agent 实现，支持工具调用、Middleware、检查点和多轮对话。

## 特性

- **ReAct 模式**: Reasoning + Acting 循环执行
- **工具调用**: 支持自定义工具和内置工具
- **Middleware 系统**: 可扩展的拦截器机制
- **检查点持久化**: 保存和恢复 Agent 状态
- **回调系统**: 监听执行事件
- **多轮对话**: 自动管理对话历史

## 模块概览

### 核心模块
- **beamai_agent** - 主模块，gen_server 实现
- **beamai_agent_init** - 初始化逻辑
- **beamai_agent_runner** - 执行器
- **beamai_agent_callbacks** - 回调处理
- **beamai_agent_checkpoint** - 检查点管理

### Middleware 模块
- **beamai_middleware** - Middleware 行为定义
- **beamai_middleware_runner** - Middleware 执行器
- **beamai_middleware_presets** - 预设配置
- **middleware_call_limit** - 调用限制
- **middleware_summarization** - 上下文摘要
- **middleware_human_approval** - 人工审批
- **middleware_tool_retry** - 工具重试
- **middleware_model_retry** - 模型重试
- **middleware_model_fallback** - 模型降级
- **middleware_pii_detection** - PII 检测

### 协调器模块
- **beamai_coordinator** - 多 Agent 协调
- **beamai_coordinator_common** - 协调器公共函数
- **beamai_nodes** - 节点定义

## API 文档

### beamai_agent

```erlang
%% 启动 Agent
beamai_agent:start_link(AgentId, Config) -> {ok, Pid} | {error, Reason}.

%% 停止 Agent
beamai_agent:stop(Pid) -> ok.

%% 发送消息
beamai_agent:chat(Pid, Message) -> {ok, Response} | {error, Reason}.
beamai_agent:chat(Pid, Message, Timeout) -> {ok, Response} | {error, Reason}.

%% 检查点操作
beamai_agent:save_checkpoint(Pid) -> {ok, CheckpointId} | {error, Reason}.
beamai_agent:save_checkpoint(Pid, Metadata) -> {ok, CheckpointId} | {error, Reason}.
beamai_agent:load_checkpoint(Pid, CheckpointId) -> {ok, Checkpoint} | {error, Reason}.
beamai_agent:load_latest_checkpoint(Pid) -> {ok, Checkpoint} | {error, Reason}.
beamai_agent:list_checkpoints(Pid) -> {ok, [Checkpoint]} | {error, Reason}.
beamai_agent:restore_from_checkpoint(Pid, CheckpointId) -> ok | {error, Reason}.

%% 获取状态
beamai_agent:get_state(Pid) -> {ok, State}.
beamai_agent:get_messages(Pid) -> {ok, Messages}.
```

### 配置结构

```erlang
Config = #{
    %% 必需
    system_prompt => <<"You are a helpful assistant.">>,

    %% LLM 配置
    llm => #{
        provider => openai,
        model => <<"gpt-4">>,
        api_key => <<"sk-xxx">>
    },

    %% 工具列表（可选，推荐使用 beamai_tool_registry 构建）
    tools => beamai_tool_registry:from_config(#{
        tools => [MyCustomTool],
        providers => [beamai_tool_provider_builtin]
    }),

    %% 检查点存储（可选）
    storage => Memory,  %% beamai_memory 实例

    %% 自动检查点（可选）
    auto_checkpoint => true,

    %% 最大迭代次数（可选）
    max_iterations => 10,

    %% Middleware 配置（可选）
    middlewares => [
        {middleware_call_limit, #{max_model_calls => 20}},
        {middleware_summarization, #{window_size => 20}}
    ],

    %% 回调函数（可选）
    callbacks => #{
        on_llm_start => fun(Prompts, Meta) -> ok end,
        on_llm_end => fun(Response, Meta) -> ok end,
        on_tool_use => fun(ToolName, Args, Meta) -> ok end
    }
}.
```

## Middleware 系统

Middleware 是 Agent 执行过程中的拦截器，可以在各个阶段进行干预。

### 生命周期钩子

```
before_agent → [before_model → LLM → after_model → before_tools → Tools → after_tools]* → after_agent
```

| 钩子 | 触发时机 | 典型用途 |
|------|----------|----------|
| `before_agent` | Agent 开始前 | 初始化 |
| `before_model` | LLM 调用前 | 检查限制、修改消息 |
| `after_model` | LLM 返回后 | 处理响应 |
| `before_tools` | 工具执行前 | 人工审批 |
| `after_tools` | 工具执行后 | 结果验证 |
| `after_agent` | Agent 结束后 | 清理、日志 |

### 使用预设配置

```erlang
%% 默认配置
Config = #{
    middlewares => beamai_middleware_presets:default()
}.

%% 生产环境配置
Config = #{
    middlewares => beamai_middleware_presets:production()
}.

%% 人工审批配置
Config = #{
    middlewares => beamai_middleware_presets:human_in_loop()
}.
```

### 内置 Middleware

| Middleware | 说明 |
|------------|------|
| `middleware_call_limit` | 限制模型/工具调用次数 |
| `middleware_summarization` | 自动压缩长对话 |
| `middleware_human_approval` | 工具执行前人工确认 |
| `middleware_tool_retry` | 工具失败自动重试 |
| `middleware_model_retry` | LLM 失败自动重试 |
| `middleware_model_fallback` | 主模型失败切换备用 |
| `middleware_pii_detection` | 检测敏感信息 |

### 自定义 Middleware

```erlang
-module(my_middleware).
-behaviour(beamai_middleware).

-export([init/1, before_model/2]).

%% 初始化
init(Opts) ->
    #{max_calls => maps:get(max_calls, Opts, 10)}.

%% LLM 调用前检查
before_model(State, #{max_calls := Max} = _MwState) ->
    Count = graph_state:get(State, call_count, 0),
    case Count >= Max of
        true -> {halt, call_limit_exceeded};
        false -> {update, #{call_count => Count + 1}}
    end.
```

### Middleware 返回值

```erlang
ok                              %% 继续执行
{update, #{key => value}}       %% 更新状态
{goto, model | tools | '__end__'}  %% 跳转
{halt, Reason}                  %% 中止
{interrupt, Action}             %% 中断等待确认
```

详细文档：[Middleware 系统](../../doc/MIDDLEWARE.md)

## 使用示例

### 基本使用

```erlang
%% 配置
Config = #{
    system_prompt => <<"You are a helpful assistant.">>,
    llm => #{
        provider => openai,
        model => <<"gpt-4">>,
        api_key => os:getenv("OPENAI_API_KEY")
    }
},

%% 启动 Agent
{ok, Agent} = beamai_agent:start_link(<<"my-agent">>, Config),

%% 对话
{ok, Response1} = beamai_agent:chat(Agent, <<"Hello!">>),
{ok, Response2} = beamai_agent:chat(Agent, <<"What can you do?">>),

%% 停止
beamai_agent:stop(Agent).
```

### 使用工具

```erlang
%% 定义计算器工具
%% 定义计算器工具
CalculatorTool = #{
    name => <<"calculator">>,
    description => <<"Perform mathematical calculations">>,
    parameters => #{
        type => object,
        properties => #{
            <<"expression">> => #{type => string}
        },
        required => [<<"expression">>]
    },
    handler => fun(#{<<"expression">> := Expr}) ->
        %% 简单求值（实际应用中需要安全处理）
        {ok, Tokens, _} = erl_scan:string(binary_to_list(Expr)),
        {ok, Parsed} = erl_parse:parse_exprs(Tokens),
        {value, Result, _} = erl_eval:exprs(Parsed, []),
        {ok, #{result => Result}}
    end
},

%% 使用 Registry 构建工具列表
Tools = beamai_tool_registry:from_config(#{
    tools => [CalculatorTool]
}),

Config = #{
    system_prompt => <<"You are an assistant that can do math.">>,
    llm => #{...},
    tools => Tools
},

{ok, Agent} = beamai_agent:start_link(<<"calc-agent">>, Config),
{ok, Response} = beamai_agent:chat(Agent, <<"What is 123 * 456?">>).
```

### 使用检查点

```erlang
%% 创建存储
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% 启动带存储的 Agent
Config = #{
    system_prompt => <<"You are a helpful assistant.">>,
    llm => #{...},
    storage => Memory
},

{ok, Agent} = beamai_agent:start_link(<<"persistent-agent">>, Config),

%% 对话
{ok, _} = beamai_agent:chat(Agent, <<"Remember: my name is Alice.">>),

%% 保存检查点
{ok, CpId} = beamai_agent:save_checkpoint(Agent, #{tag => <<"after_intro">>}),

%% 更多对话...
{ok, _} = beamai_agent:chat(Agent, <<"What's the weather?">>),

%% 恢复到之前的检查点
ok = beamai_agent:restore_from_checkpoint(Agent, CpId),

%% 现在 Agent 只记得 "my name is Alice"
{ok, Messages} = beamai_agent:get_messages(Agent).
```

### 使用智谱 AI

```erlang
Config = #{
    system_prompt => <<"你是一个乐于助人的 AI 助手。">>,
    llm => #{
        provider => anthropic,
        model => <<"glm-4">>,
        api_key => os:getenv("ZHIPU_API_KEY"),
        base_url => <<"https://open.bigmodel.cn/api/anthropic/v1">>
    }
},

{ok, Agent} = beamai_agent:start_link(<<"zhipu-agent">>, Config),
{ok, Response} = beamai_agent:chat(Agent, <<"你好！介绍一下你自己。">>).
```

## 依赖

- beamai_core
- beamai_llm
- beamai_memory

## 许可证

Apache-2.0
