# BeamAI Agent

[English](README_EN.md) | 中文

Agent 实现模块，提供基于工具循环的 ReAct Agent 和基于 Process Framework 的 Process Agent。

## 特性

- ReAct 模式的工具循环（Tool Loop）
- 完整的回调系统（Callbacks）
- 内存持久化（Memory）
- 中断和恢复（Interrupt）
- 多轮对话支持
- Process Agent（基于 Process Framework 的 Agent）

## 模块概览

### 核心模块

- **beamai_agent** - 主 API（new/1, run/2, restore_from_memory/2）
- **beamai_agent_state** - Agent 状态管理
- **beamai_agent_callbacks** - 回调系统实现

### 执行模块

- **beamai_agent_tool_loop** - 工具循环执行器（ReAct loop）
- **beamai_agent_interrupt** - 中断和恢复机制
- **beamai_agent_memory** - Agent 内存管理

### Process Agent

- **beamai_process_agent** - 基于 Process Framework 的 Agent
- **beamai_process_agent_llm_step** - LLM 调用步骤
- **beamai_process_agent_tool_step** - 工具调用步骤

### 辅助模块

- **beamai_agent_utils** - Agent 工具函数

## API 文档

### beamai_agent

```erlang
%% 创建 Agent 状态
beamai_agent:new(Opts) -> {ok, State} | {error, Reason}.

%% 运行 Agent
beamai_agent:run(State, Input) -> {ok, Result, NewState} | {error, Reason}.

%% 从 Memory 恢复
beamai_agent:restore_from_memory(Opts, Memory) -> {ok, State} | {error, Reason}.
```

### 配置选项

```erlang
{ok, State} = beamai_agent:new(#{
    %% LLM 配置（必填，使用 beamai_chat_completion:create/2 创建）
    llm => LLM,

    %% 系统提示词
    system_prompt => <<"你是一个助手"/utf8>>,

    %% 工具列表（通过 beamai_kernel:get_tool_specs/1 获取）
    tools => Tools,

    %% Agent ID
    id => <<"my_agent">>,

    %% 最大迭代次数（默认 10）
    max_iterations => 10,

    %% 超时时间（毫秒）
    timeout => 300000,

    %% 存储（用于 checkpoint 持久化）
    storage => Memory,

    %% 回调配置
    callbacks => #{
        on_llm_start => fun(Prompts, Meta) -> ... end,
        on_llm_end => fun(Response, Meta) -> ... end,
        on_tool_start => fun(ToolName, Args, Meta) -> ... end,
        on_tool_end => fun(ToolName, Result, Meta) -> ... end,
        on_agent_finish => fun(Result, Meta) -> ... end
    }
}).
```

## 使用示例

### 基本 Agent

```erlang
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

{ok, State} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"你是一个有帮助的助手。"/utf8>>
}),

{ok, Result, _NewState} = beamai_agent:run(State, <<"你好！"/utf8>>).
```

### 带工具的 Agent

```erlang
%% 使用 Kernel 注册工具
Kernel = beamai_kernel:new(),
Kernel1 = beamai_kernel:add_plugin_from_module(Kernel, beamai_plugin_file),
Tools = beamai_kernel:get_tool_specs(Kernel1),

{ok, State} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"你是文件助手。"/utf8>>,
    tools => Tools
}),

{ok, Result, _} = beamai_agent:run(State, <<"读取 /tmp/test.txt 文件"/utf8>>).
```

### 多轮对话

```erlang
{ok, State0} = beamai_agent:new(#{llm => LLM}),
{ok, _, State1} = beamai_agent:run(State0, <<"我叫张三"/utf8>>),
{ok, Result, _} = beamai_agent:run(State1, <<"我叫什么？"/utf8>>).
```

### 带 Memory 持久化

```erlang
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
{ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, my_store}}),

{ok, State} = beamai_agent:new(#{
    llm => LLM,
    storage => Memory
}),

{ok, _, NewState} = beamai_agent:run(State, <<"记住密码是 12345"/utf8>>),

%% 稍后恢复
{ok, Restored} = beamai_agent:restore_from_memory(#{llm => LLM}, Memory).
```

### Process Agent

```erlang
%% 使用 Process Framework 构建 Agent
%% beamai_process_agent 提供预定义的 LLM + Tool 步骤
Process = beamai_process_agent:build(#{
    llm => LLM,
    tools => Tools,
    system_prompt => <<"你是助手"/utf8>>
}),

{ok, Result} = beamai_process_executor:run(Process, #{
    input => <<"你好"/utf8>>
}).
```

## 回调类型

| 回调 | 触发时机 | 参数 |
|------|----------|------|
| `on_llm_start` | LLM 调用开始 | `(Prompts, Meta)` |
| `on_llm_end` | LLM 响应收到 | `(Response, Meta)` |
| `on_tool_start` | 工具调用开始 | `(ToolName, Args, Meta)` |
| `on_tool_end` | 工具调用结束 | `(ToolName, Result, Meta)` |
| `on_agent_finish` | Agent 执行完成 | `(Result, Meta)` |

## 依赖

- beamai_core（Kernel、Process Framework）
- beamai_llm（LLM 调用）
- beamai_plugin（Plugin 工具）
- beamai_memory（持久化）

## 许可证

Apache-2.0
