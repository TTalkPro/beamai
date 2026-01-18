# Agent Deep

深度 Agent 实现，支持复杂任务规划、并行执行和反思机制。

## 特性

- 任务规划与分解
- 并行执行
- 依赖关系管理
- 反思与自我纠正
- 工具集成（文件系统、TODO 管理等）
- 检查点与恢复

## 模块概览

### 核心模块

- **beamai_deepagent** - 主模块
- **beamai_deepagent_plan** - 任务规划
- **beamai_deepagent_router** - 路由决策
- **beamai_deepagent_dependencies** - 依赖管理
- **beamai_deepbeamai_result_analyzer** - 结果分析

### 执行模块

- **beamai_deepagent_nodes** - 节点定义
- **beamai_deepbeamai_llm_node** - LLM 节点
- **beamai_deepagent_tool_executor** - 工具执行器
- **beamai_deepagent_trace** - 执行追踪

### 工具模块

- **beamai_deepagent_tool_provider** - 工具提供者（实现 beamai_tool_provider 行为）
- **beamai_deepagent_fs_tools** - 文件系统工具
- **beamai_deepagent_fs_handlers** - 文件处理器
- **beamai_deepagent_fs_backend** - 文件后端
- **beamai_deepagent_todo_tools** - TODO 管理工具
- **beamai_deepagent_todo_handlers** - TODO 处理器
- **beamai_deepagent_human_tools** - Human-in-the-loop 工具
- **beamai_deepagent_base_tools** - 基础工具（checkpoint, get_trace）
- **beamai_deepagent_plan_tools** - 计划和子任务工具

### 辅助模块

- **beamai_deepbeamai_messages** - 消息处理
- **beamai_deepbeamai_utils** - 工具函数

## API 文档

### beamai_deepagent

```erlang
%% 启动 Deep Agent
beamai_deepagent:start_link(Config) -> {ok, Pid} | {error, Reason}.

%% 执行任务
beamai_deepagent:run(Pid, Task) -> {ok, Result} | {error, Reason}.
beamai_deepagent:run(Pid, Task, Options) -> {ok, Result} | {error, Reason}.

%% 流式执行
beamai_deepagent:run_stream(Pid, Task, Callback) -> {ok, Result} | {error, Reason}.

%% 停止
beamai_deepagent:stop(Pid) -> ok.
```

### 配置结构

```erlang
%% 首先创建 LLM 配置（必须使用 llm_client:create/2）
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% DeepAgent 配置
Config = beamai_deepagent:new(#{
    %% LLM 配置（必须使用 llm_client:create/2 创建）
    llm => LLM,

    %% 工具配置（可选）
    tools => [
        beamai_deepagent_fs_tools,      %% 文件系统工具
        beamai_deepagent_todo_tools     %% TODO 管理工具
    ],

    %% 工作目录（可选）
    workspace => <<"/tmp/agent_workspace">>,

    %% 最大深度（可选）
    max_depth => 3,

    %% 最大迭代次数（可选）
    max_iterations => 10,

    %% 启用反思（可选）
    enable_reflection => true
}).
```

## 使用示例

### 基本使用

```erlang
%% 创建 LLM 配置
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% 创建 DeepAgent 配置
Config = beamai_deepagent:new(#{llm => LLM}),

%% 执行复杂任务
Task = <<"分析当前目录下的 Erlang 代码，找出所有导出的函数，并生成文档。">>,
{ok, Result} = beamai_deepagent:run(Config, Task).
```

### 使用文件系统工具

```erlang
%% 创建 LLM 配置
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

Config = beamai_deepagent:new(#{
    llm => LLM,
    tools => [beamai_deepagent_fs_tools],
    workspace => <<"/tmp/my_workspace">>
}),

%% Agent 可以读写文件
Task = <<"创建一个名为 hello.txt 的文件，内容是 'Hello, World!'">>,
{ok, Result} = beamai_deepagent:run(Config, Task).
```

### 使用 TODO 管理

```erlang
Config = beamai_deepagent:new(#{
    llm => LLM,  %% 复用之前创建的 LLM 配置
    tools => [beamai_deepagent_todo_tools]
}),

%% Agent 可以管理任务列表
Task = <<"创建一个项目计划，包含以下任务：1. 设计架构 2. 实现核心功能 3. 编写测试">>,
{ok, Result} = beamai_deepagent:run(Config, Task).
```

### 流式执行

```erlang
Callback = fun
    ({step, Step}) ->
        io:format("执行步骤: ~p~n", [Step]);
    ({tool_call, Tool, Args}) ->
        io:format("调用工具: ~s~n", [Tool]);
    ({tool_result, Tool, Result}) ->
        io:format("工具结果: ~p~n", [Result]);
    ({thinking, Thought}) ->
        io:format("思考: ~s~n", [Thought]);
    ({done, Result}) ->
        io:format("完成: ~p~n", [Result])
end,

beamai_deepagent:run_stream(Agent, Task, Callback).
```

### 使用智谱 AI

```erlang
%% 创建智谱 AI 配置（使用 Anthropic 兼容接口）
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

Config = beamai_deepagent:new(#{
    llm => LLM,
    tools => [beamai_deepagent_fs_tools]
}),

{ok, Result} = beamai_deepagent:run(Config, <<"分析项目结构并生成报告">>).
```

## 工具管理

DeepAgent 使用 `beamai_tool_provider` 机制管理工具，通过 `beamai_deepagent_tool_provider` 模块提供工具。

### 使用 Tool Provider

```erlang
%% 通过 beamai_tool_registry 获取工具
Config = #{depth => 0, planning_mode => full},
Tools = beamai_tool_registry:from_config(#{
    providers => [{beamai_deepagent_tool_provider, Config}]
}).

%% 直接访问工具集合
BaseTools = beamai_deepagent_tool_provider:base_tools().
PlanTools = beamai_deepagent_tool_provider:plan_tools().
FsTools = beamai_deepagent_tool_provider:filesystem_tools().
```

### 工具条件判断

工具的可用性根据配置动态决定：

| 工具集 | 条件 |
|--------|------|
| 基础工具 | 始终可用 |
| 计划工具 | `planning_mode=full` 且 `depth=0` |
| TodoList 工具 | `planning_mode=simple` |
| 子任务工具 | `depth < max_depth` |
| 反思工具 | `reflection_enabled=true` |
| 文件系统工具 | `filesystem_enabled=true` 或有 `filesystem` 配置 |
| Human 工具 | `human_in_loop.enabled=true`（默认启用） |

### 与其他 Provider 组合

```erlang
%% 组合 DeepAgent 工具和 MCP 工具
Tools = beamai_tool_registry:from_config(#{
    providers => [
        {beamai_deepagent_tool_provider, Config},
        {beamai_tool_provider_mcp, #{server => my_mcp_server}}
    ]
}).
```

## 工具列表

### 文件系统工具 (beamai_deepagent_fs_tools)

| 工具名 | 说明 |
|--------|------|
| `read_file` | 读取文件内容 |
| `write_file` | 写入文件 |
| `list_directory` | 列出目录内容 |
| `create_directory` | 创建目录 |
| `delete_file` | 删除文件 |
| `file_exists` | 检查文件是否存在 |

### TODO 管理工具 (beamai_deepagent_todo_tools)

| 工具名 | 说明 |
|--------|------|
| `write_todos` | 写入待办事项列表 |
| `read_todos` | 读取待办事项列表 |

### 基础工具 (beamai_deepagent_base_tools)

| 工具名 | 说明 |
|--------|------|
| `checkpoint` | 创建执行检查点 |
| `get_trace` | 获取执行轨迹 |

### 计划工具 (beamai_deepagent_plan_tools)

| 工具名 | 说明 |
|--------|------|
| `create_plan` | 创建任务计划 |
| `update_plan` | 更新任务计划 |
| `spawn_subtask` | 创建子任务 |
| `reflect` | 反思当前进度 |

### Human 交互工具 (beamai_deepagent_human_tools)

| 工具名 | 说明 |
|--------|------|
| `ask_human` | 向用户提问 |
| `confirm_action` | 请求用户确认 |

## 依赖

- beamai_core
- beamai_llm
- beamai_memory
- beamai_tools

## 许可证

Apache-2.0
