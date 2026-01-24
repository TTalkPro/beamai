# BeamAI Deep Agent

[English](README_EN.md) | 中文

基于 SubAgent 架构的深度 Agent 实现，支持复杂任务规划、并行执行和反思机制。

## 特性

- SubAgent 编排架构（Planner → Executor → Reflector）
- 任务规划与分解
- 并行子任务执行
- 依赖关系管理
- 反思与自我纠正
- Coordinator 多 Agent 协调
- Plugin 工具集成

## 架构

```
beamai_deepagent
├── beamai_deepagent          # 主 API（new/1, run/2, run/3）
├── beamai_deepagent_planner  # 规划器 SubAgent
├── beamai_deepagent_executor # 执行器 SubAgent
├── beamai_deepagent_reflector # 反思器 SubAgent
├── beamai_deepagent_parallel # 并行执行管理
├── beamai_deepagent_coordinator # 多 Agent 协调
├── beamai_deepagent_plan_plugin # 规划工具插件
├── beamai_deepagent_utils    # 工具函数
└── core/
    ├── beamai_deepagent_plan          # 计划数据结构
    ├── beamai_deepagent_dependencies  # 依赖管理
    └── beamai_deepagent_trace         # 执行追踪
```

## 模块概览

### 核心 API

- **beamai_deepagent** - 主模块，提供 `new/1`、`run/2`、`run/3`、`get_plan/1`、`get_trace/1`

### SubAgent 模块

- **beamai_deepagent_planner** - 规划器：分析任务，生成执行计划
- **beamai_deepagent_executor** - 执行器：执行计划中的各个步骤
- **beamai_deepagent_reflector** - 反思器：评估执行结果，提供改进建议
- **beamai_deepagent_parallel** - 并行管理：管理多个子任务的并行执行
- **beamai_deepagent_coordinator** - 协调器：多 Agent 间的任务分配和协调

### 工具模块

- **beamai_deepagent_plan_plugin** - 规划工具插件（实现 beamai_plugin_behaviour）

### 核心数据结构

- **beamai_deepagent_plan** - 计划数据结构和操作
- **beamai_deepagent_dependencies** - 任务依赖图管理
- **beamai_deepagent_trace** - 执行轨迹记录

### 辅助模块

- **beamai_deepagent_utils** - 通用工具函数

## API 文档

### beamai_deepagent

```erlang
%% 创建配置（直接返回 config map，不是 {ok, Config}）
-spec new(map()) -> config().
beamai_deepagent:new(Opts) -> Config.

%% 也可以创建空配置
beamai_deepagent:new() -> Config.

%% 执行任务
beamai_deepagent:run(Config, Task) -> {ok, Result} | {error, Reason}.
beamai_deepagent:run(Config, Task, Opts) -> {ok, Result} | {error, Reason}.

%% 获取计划和轨迹
beamai_deepagent:get_plan(Result) -> Plan.
beamai_deepagent:get_trace(Result) -> Trace.
```

### 配置选项

```erlang
Config = beamai_deepagent:new(#{
    %% LLM 配置（必填，使用 beamai_chat_completion:create/2 创建）
    llm => LLM,

    %% Plugin 列表（可选，实现 beamai_plugin_behaviour 的模块）
    plugins => [beamai_plugin_file, beamai_plugin_shell],

    %% 自定义工具（可选，工具 map 列表）
    custom_tools => [#{name => ..., handler => ...}],

    %% 最大递归深度（默认 3）
    max_depth => 3,

    %% 最大并行数（默认 5）
    max_parallel => 5,

    %% 是否启用规划（默认 true）
    planning_enabled => true,

    %% 是否启用反思（默认 true）
    reflection_enabled => true,

    %% 每个子代理最大工具迭代次数（默认 10）
    max_tool_iterations => 10,

    %% 每步超时毫秒数（默认 300000）
    timeout => 300000,

    %% 系统提示词
    system_prompt => <<"你是专家"/utf8>>,

    %% 各 SubAgent 专用提示词
    planner_prompt => <<"...">>,
    executor_prompt => <<"...">>,
    reflector_prompt => <<"...">>,

    %% 事件回调
    callbacks => #{
        on_plan_created => fun(Plan) -> ... end,
        on_step_start => fun(Step) -> ... end,
        on_step_end => fun(Step, Result) -> ... end,
        on_reflection => fun(Reflection) -> ... end
    }
}).
```

## 使用示例

### 基本使用

```erlang
%% 创建 LLM 配置
LLM = beamai_chat_completion:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% 创建 DeepAgent 配置
Config = beamai_deepagent:new(#{llm => LLM}),

%% 执行复杂任务
{ok, Result} = beamai_deepagent:run(Config,
    <<"分析当前目录下的 Erlang 代码，找出所有导出的函数，并生成文档。"/utf8>>),

%% 查看计划
Plan = beamai_deepagent:get_plan(Result),
Trace = beamai_deepagent:get_trace(Result).
```

### 使用 Plugin 工具

```erlang
Config = beamai_deepagent:new(#{
    llm => LLM,
    plugins => [beamai_plugin_file, beamai_plugin_shell],
    max_depth => 2
}),

{ok, Result} = beamai_deepagent:run(Config,
    <<"读取 src/ 目录下的所有 .erl 文件并统计代码行数"/utf8>>).
```

### 带回调的执行

```erlang
Config = beamai_deepagent:new(#{
    llm => LLM,
    plugins => [beamai_plugin_file],
    callbacks => #{
        on_plan_created => fun(Plan) ->
            io:format("计划创建: ~p~n", [Plan])
        end,
        on_step_start => fun(Step) ->
            io:format("开始步骤: ~p~n", [Step])
        end,
        on_reflection => fun(Reflection) ->
            io:format("反思: ~ts~n", [Reflection])
        end
    }
}),

{ok, Result} = beamai_deepagent:run(Config, <<"分析项目架构"/utf8>>).
```

### 使用智谱 AI

```erlang
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

Config = beamai_deepagent:new(#{
    llm => LLM,
    plugins => [beamai_plugin_file]
}),

{ok, Result} = beamai_deepagent:run(Config, <<"分析项目结构并生成报告"/utf8>>).
```

## 执行流程

```
1. 接收任务
   ↓
2. Planner 分析任务，生成执行计划
   ↓
3. 按依赖关系排序步骤
   ↓
4. 对于每个步骤：
   a. Executor 执行步骤（使用 Plugin 工具）
   b. 支持并行执行无依赖的步骤
   ↓
5. Reflector 评估执行结果
   ↓
6. 如需改进，更新计划并重新执行
   ↓
7. 返回最终结果
```

## 依赖

- beamai_core（Kernel、Process Framework）
- beamai_llm（LLM 调用）
- beamai_plugin（Plugin 系统）
- beamai_memory（状态持久化）

## 许可证

Apache-2.0
