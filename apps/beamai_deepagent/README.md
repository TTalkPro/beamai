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

- **beamai_deepbeamai_tools** - 工具注册
- **beamai_deepagent_fs_tools** - 文件系统工具
- **beamai_deepagent_fs_handlers** - 文件处理器
- **beamai_deepagent_fs_backend** - 文件后端
- **beamai_deepagent_todo_tools** - TODO 管理工具
- **beamai_deepagent_todo_handlers** - TODO 处理器

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
Config = #{
    %% LLM 配置
    llm => #{
        provider => openai,
        model => <<"gpt-4">>,
        api_key => <<"sk-xxx">>
    },

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
}.
```

## 使用示例

### 基本使用

```erlang
Config = #{
    llm => #{
        provider => openai,
        model => <<"gpt-4">>,
        api_key => os:getenv("OPENAI_API_KEY")
    }
},

{ok, Agent} = beamai_deepagent:start_link(Config),

%% 执行复杂任务
Task = <<"分析当前目录下的 Erlang 代码，找出所有导出的函数，并生成文档。">>,
{ok, Result} = beamai_deepagent:run(Agent, Task),

beamai_deepagent:stop(Agent).
```

### 使用文件系统工具

```erlang
Config = #{
    llm => #{...},
    tools => [beamai_deepagent_fs_tools],
    workspace => <<"/tmp/my_workspace">>
},

{ok, Agent} = beamai_deepagent:start_link(Config),

%% Agent 可以读写文件
Task = <<"创建一个名为 hello.txt 的文件，内容是 'Hello, World!'">>,
{ok, Result} = beamai_deepagent:run(Agent, Task).
```

### 使用 TODO 管理

```erlang
Config = #{
    llm => #{...},
    tools => [beamai_deepagent_todo_tools]
},

{ok, Agent} = beamai_deepagent:start_link(Config),

%% Agent 可以管理任务列表
Task = <<"创建一个项目计划，包含以下任务：1. 设计架构 2. 实现核心功能 3. 编写测试">>,
{ok, Result} = beamai_deepagent:run(Agent, Task).
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
Config = #{
    llm => #{
        provider => anthropic,
        model => <<"glm-4">>,
        api_key => os:getenv("ZHIPU_API_KEY"),
        base_url => <<"https://open.bigmodel.cn/api/anthropic/v1">>
    },
    tools => [beamai_deepagent_fs_tools]
},

{ok, Agent} = beamai_deepagent:start_link(Config),
{ok, Result} = beamai_deepagent:run(Agent, <<"分析项目结构并生成报告">>).
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
| `add_todo` | 添加待办事项 |
| `list_todos` | 列出待办事项 |
| `complete_todo` | 完成待办事项 |
| `delete_todo` | 删除待办事项 |

## 依赖

- beamai_core
- beamai_llm
- beamai_memory

## 许可证

Apache-2.0
