# beamai_agent

[English](README_EN.md) | 中文

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
- **beamai_agent** - 主模块，无状态函数实现
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

### beamai_coordinator API

协调器用于管理多个 Agent 协同工作，支持 Pipeline（顺序）和 Orchestrator（编排）两种模式。

```erlang
%% 启动协调器
beamai_coordinator:start_link(Id, Opts) -> {ok, Pid} | {error, Reason}.
beamai_coordinator:start_pipeline(Id, Opts) -> {ok, Pid} | {error, Reason}.
beamai_coordinator:start_orchestrator(Id, Opts) -> {ok, Pid} | {error, Reason}.

%% 停止协调器（注意：参数是 Pid 而非 Id）
beamai_coordinator:stop(CoordinatorPid) -> ok | {error, Reason}.

%% 获取 Workers（参数是 Pid）
beamai_coordinator:get_workers(CoordinatorPid) -> {ok, #{Name => WorkerPid}} | {error, Reason}.
beamai_coordinator:get_worker(CoordinatorPid, WorkerName) -> {ok, WorkerPid} | {error, Reason}.

%% 委托任务（参数是 Pid）
beamai_coordinator:delegate(CoordinatorPid, WorkerName, Task) -> {ok, Result} | {error, Reason}.
beamai_coordinator:delegate_parallel(CoordinatorPid, [WorkerName], Task) -> {ok, #{Name => Result}}.
```

**注意：** 从 v2.1 开始，`beamai_coordinator` 的 API 参数从使用 `Id` 改为使用 `CoordinatorPid`。协调器元数据存储在 Agent 的 `meta` 字段中，而非共享的 ETS 表。

## API 文档

### beamai_agent（纯函数 API）

```erlang
%% 核心 API
beamai_agent:new(Config) -> {ok, State} | {error, Reason}.
beamai_agent:run(State, Message) -> {ok, Result, NewState} | {error, Reason}.
beamai_agent:run(State, Message, Opts) -> {ok, Result, NewState} | {error, Reason}.
beamai_agent:restore_from_memory(Config, Memory) -> {ok, State} | {error, Reason}.

%% 状态查询
beamai_agent:get_messages(State) -> [Message].
beamai_agent:get_full_messages(State) -> [Message].
beamai_agent:get_scratchpad(State) -> [Step].
beamai_agent:get_context(State) -> Context.
beamai_agent:get_context(State, Key) -> Value | undefined.
beamai_agent:get_context(State, Key, Default) -> Value.

%% 状态修改
beamai_agent:set_context(State, Context) -> NewState.
beamai_agent:update_context(State, Updates) -> NewState.
beamai_agent:put_context(State, Key, Value) -> NewState.
beamai_agent:clear_messages(State) -> NewState.
beamai_agent:clear_scratchpad(State) -> NewState.
```

### 配置结构

```erlang
%% 首先创建 LLM 配置（必须使用 llm_client:create/2）
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% Agent 配置
Config = #{
    %% 必需
    system_prompt => <<"You are a helpful assistant.">>,

    %% LLM 配置（必须使用 llm_client:create/2 创建）
    llm => LLM,

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
%% 创建 LLM 配置
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% 配置
Config = #{
    system_prompt => <<"You are a helpful assistant.">>,
    llm => LLM
},

%% 创建 Agent 状态
{ok, State0} = beamai_agent:new(Config),

%% 多轮对话
{ok, _Result1, State1} = beamai_agent:run(State0, <<"Hello!">>),
{ok, _Result2, _State2} = beamai_agent:run(State1, <<"What can you do?">>).
```

### 使用工具

```erlang
%% 创建 LLM 配置
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

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
    llm => LLM,
    tools => Tools
},

{ok, State} = beamai_agent:new(Config),
{ok, Result, _NewState} = beamai_agent:run(State, <<"What is 123 * 456?">>).
```

### 使用 Memory 持久化

```erlang
%% 创建 LLM 配置
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% 创建存储
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

%% 创建带存储的 Agent
Config = #{
    system_prompt => <<"You are a helpful assistant.">>,
    llm => LLM,
    storage => Memory
},

{ok, State0} = beamai_agent:new(Config),

%% 对话（checkpoint 自动保存）
{ok, _, State1} = beamai_agent:run(State0, <<"Remember: my name is Alice.">>),
{ok, _, _State2} = beamai_agent:run(State1, <<"What's the weather?">>),

%% 从 Memory 恢复会话
{ok, RestoredState} = beamai_agent:restore_from_memory(#{llm => LLM}, Memory),
Messages = beamai_agent:get_messages(RestoredState).
```

### 使用阿里云百炼

```erlang
%% 创建百炼配置（通义千问）
LLM = llm_client:create(bailian, #{
    model => <<"qwen3-max">>,
    api_key => list_to_binary(os:getenv("BAILIAN_API_KEY"))
}),

Config = #{
    system_prompt => <<"你是一个乐于助人的 AI 助手。">>,
    llm => LLM
},

{ok, State} = beamai_agent:new(Config),
{ok, Result, _} = beamai_agent:run(State, <<"你好！介绍一下你自己。">>).
```

### 使用智谱 AI

```erlang
%% 创建智谱 AI 配置（使用 Anthropic 兼容接口）
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

Config = #{
    system_prompt => <<"你是一个乐于助人的 AI 助手。">>,
    llm => LLM
},

{ok, State} = beamai_agent:new(Config),
{ok, Result, _} = beamai_agent:run(State, <<"你好！介绍一下你自己。">>).
```

### 使用协调器 (Pipeline 模式)

```erlang
%% 创建 LLM 配置
LLM = llm_client:create(bailian, #{
    model => <<"qwen-plus">>,
    api_key => list_to_binary(os:getenv("BAILIAN_API_KEY"))
}),

%% 定义 Pipeline 中的 Agents（翻译流水线）
Agents = [
    #{
        name => <<"translator">>,
        system_prompt => <<"你是翻译专家，将中文翻译成英文。">>
    },
    #{
        name => <<"polisher">>,
        system_prompt => <<"你是英文润色专家，优化英文表达。">>
    }
],

%% 启动 Pipeline 协调器
{ok, Coordinator} = beamai_coordinator:start_pipeline(<<"translation_pipeline">>, #{
    agents => Agents,
    llm => LLM
}),

%% 直接委托给指定 worker
{ok, Result} = beamai_coordinator:delegate(Coordinator, <<"translator">>, <<"你好世界">>),

%% 获取所有 workers
{ok, Workers} = beamai_coordinator:get_workers(Coordinator),

%% 停止协调器
beamai_coordinator:stop(Coordinator).
```

### 使用协调器 (Orchestrator 模式)

```erlang
%% 定义多专家 Agents
Agents = [
    #{name => <<"tech_expert">>, system_prompt => <<"你是技术专家。">>},
    #{name => <<"business_expert">>, system_prompt => <<"你是商业专家。">>}
],

%% 启动 Orchestrator 协调器
{ok, Coordinator} = beamai_coordinator:start_orchestrator(<<"expert_panel">>, #{
    agents => Agents,
    llm => LLM
}),

%% 并行委托给多个 workers
{ok, Results} = beamai_coordinator:delegate_parallel(
    Coordinator,
    [<<"tech_expert">>, <<"business_expert">>],
    <<"分析 AI 对行业的影响">>
),

%% Results = #{<<"tech_expert">> => {ok, "..."}, <<"business_expert">> => {ok, "..."}}

beamai_coordinator:stop(Coordinator).
```

## 依赖

- beamai_core
- beamai_llm
- beamai_memory

## 许可证

Apache-2.0
