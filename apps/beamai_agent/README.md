# Agent Simple

简单的 ReAct Agent 实现，支持工具调用、检查点和多轮对话。

## 特性

- ReAct（Reasoning + Acting）模式
- 工具调用支持
- 检查点持久化
- 多轮对话
- 可配置的 LLM 后端

## 模块概览

- **beamai_agent** - 主模块，gen_server 实现
- **beamai_agent_init** - 初始化逻辑
- **beamai_agent_runner** - 执行器
- **beamai_agent_callbacks** - 回调处理
- **beamai_agent_checkpoint** - 检查点管理
- **agent_coordinator** - 多 Agent 协调
- **agent_coordinator_common** - 协调器公共函数
- **agent_nodes** - 节点定义

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

    %% 工具列表（可选）
    tools => [
        #{
            name => <<"calculator">>,
            description => <<"Perform calculations">>,
            input_schema => #{...},
            handler => fun(Args) -> {ok, Result} end
        }
    ],

    %% 检查点存储（可选）
    storage => Memory,  %% beamai_memory 实例

    %% 自动检查点（可选）
    auto_checkpoint => true,

    %% 最大迭代次数（可选）
    max_iterations => 10
}.
```

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
CalculatorTool = #{
    name => <<"calculator">>,
    description => <<"Perform mathematical calculations">>,
    input_schema => #{
        type => object,
        properties => #{
            expression => #{type => string}
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

Config = #{
    system_prompt => <<"You are an assistant that can do math.">>,
    llm => #{...},
    tools => [CalculatorTool]
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
