# beamai_tools - 公共工具库和中间件系统

[English](README_EN.md) | 中文

Agent 工具的统一定义、注册和管理模块，以及 Agent 执行的中间件系统。

## 核心功能

### 工具系统
- **工具管理**: 统一的工具定义格式和获取接口
- **Provider 机制**: 支持多种工具来源（内置、自定义、MCP）
- **工具注册表**: 构建器模式，支持冲突解决策略
- **工具执行**: 统一的工具调用接口
- **LLM 适配**: 转换为 OpenAI/Anthropic 函数调用格式

### 中间件系统
- **执行拦截**: 在 Agent 执行的各个阶段进行拦截和修改
- **流程控制**: 支持跳转、中止、中断等控制流
- **工具增强**: 工具筛选、重试、模拟等功能
- **上下文管理**: 对话摘要、PII 检测等

## 模块结构

```
beamai_tools/
├── beamai_tools.erl              # 主 API 模块
├── beamai_tool_registry.erl      # 工具注册表
├── beamai_tool_provider.erl      # Provider 行为定义
├── beamai_tool_security.erl      # 工具安全检查
├── providers/
│   ├── beamai_tool_provider_builtin.erl  # 内置 Provider
│   └── beamai_tool_provider_custom.erl   # 自定义 Provider
├── tools/
│   ├── beamai_tools_file.erl     # 文件操作工具
│   ├── beamai_tools_shell.erl    # Shell 命令工具
│   ├── beamai_tools_todo.erl     # TODO 管理工具
│   └── beamai_tools_human.erl    # 人机交互工具
└── middleware/
    ├── beamai_middleware.erl          # Middleware 行为定义
    ├── beamai_middleware_runner.erl   # Middleware 链运行器
    ├── beamai_middleware_presets.erl  # 预设配置
    ├── middleware_call_limit.erl      # 调用次数限制
    ├── middleware_context_editing.erl # 上下文编辑
    ├── middleware_file_search.erl     # 文件搜索增强
    ├── middleware_human_approval.erl  # 人工审批
    ├── middleware_model_fallback.erl  # 模型降级
    ├── middleware_model_retry.erl     # 模型重试
    ├── middleware_pii_detection.erl   # PII 检测
    ├── middleware_shell_tool.erl      # Shell 工具增强
    ├── middleware_summarization.erl   # 对话摘要
    ├── middleware_todo_list.erl       # TODO 管理
    ├── middleware_tool_emulator.erl   # 工具模拟
    ├── middleware_tool_retry.erl      # 工具重试
    └── middleware_tool_selector.erl   # 智能工具筛选
```

## 工具系统

### 获取工具

```erlang
%% 获取指定分类的工具
FileTools = beamai_tools:get_tools(file),

%% 获取多个分类
Tools = beamai_tools:get_tools([file, shell, todo]),

%% 获取所有工具
AllTools = beamai_tools:get_all_tools().
```

### 使用工具注册表

```erlang
%% 构建器模式
Registry = beamai_tool_registry:new(),
R1 = beamai_tool_registry:add_tools(Registry, [MyTool1, MyTool2]),
R2 = beamai_tool_registry:add_provider(R1, beamai_tool_provider_builtin),
Tools = beamai_tool_registry:build(R2).

%% 便捷函数
Tools = beamai_tool_registry:from_config(#{
    tools => [MyTool],
    providers => [
        {beamai_tool_provider_mcp, #{mcp_tag => file}},
        beamai_tool_provider_builtin
    ]
}).
```

### 执行工具

```erlang
%% 按名称执行工具
{ok, Result} = beamai_tools:execute(<<"file_read">>, #{
    path => <<"/tmp/test.txt">>
}).

%% 带上下文执行
{ok, Result} = beamai_tools:execute(<<"shell_exec">>, #{
    command => <<"ls -la">>
}, #{context => #{workspace => <<"/project">>}}).
```

### 转换为 LLM 格式

```erlang
%% 单个工具
Spec = beamai_tools:to_llm_spec(Tool),

%% 批量转换
Specs = beamai_tools:to_llm_specs(Tools).
```

### 工具定义格式

```erlang
#{
    name => <<"tool_name">>,
    description => <<"工具描述"/utf8>>,
    parameters => #{
        type => object,
        properties => #{
            <<"param1">> => #{
                type => string,
                description => <<"参数描述"/utf8>>
            }
        },
        required => [<<"param1">>]
    },
    handler => fun(Args, Context) ->
        %% 执行逻辑
        {ok, Result}
    end
}
```

### 内置工具分类

| 分类 | 模块 | 工具 |
|------|------|------|
| `file` | beamai_tools_file | file_read, file_write, file_list, file_search |
| `shell` | beamai_tools_shell | shell_exec |
| `todo` | beamai_tools_todo | todo_add, todo_list, todo_complete |
| `human` | beamai_tools_human | ask_human, confirm_action |

## 中间件系统

### 生命周期钩子

```
                   ┌─────────────────┐
                   │  before_agent   │
                   └────────┬────────┘
                            │
           ┌────────────────┼────────────────┐
           │                ▼                │
           │    ┌─────────────────────┐      │
           │    │    before_model     │      │
           │    └──────────┬──────────┘      │
           │               │                 │
           │               ▼                 │
           │    ┌─────────────────────┐      │
           │    │     LLM Call        │      │
           │    └──────────┬──────────┘      │
           │               │                 │
           │               ▼                 │
           │    ┌─────────────────────┐      │
           │    │     after_model     │      │
           │    └──────────┬──────────┘      │
 Agent     │               │                 │
 Loop      │               ▼                 │
           │    ┌─────────────────────┐      │
           │    │    before_tools     │      │
           │    └──────────┬──────────┘      │
           │               │                 │
           │               ▼                 │
           │    ┌─────────────────────┐      │
           │    │    Tool Execution   │      │
           │    └──────────┬──────────┘      │
           │               │                 │
           │               ▼                 │
           │    ┌─────────────────────┐      │
           │    │     after_tools     │      │
           │    └──────────┬──────────┘      │
           │               │                 │
           └───────────────┴─────────────────┘
                           │
                           ▼
                   ┌─────────────────┐
                   │   after_agent   │
                   └─────────────────┘
```

### 使用中间件

```erlang
%% 配置中间件列表
Middlewares = [
    %% 调用次数限制
    {middleware_call_limit, #{max_calls => 10}},

    %% 模型重试
    {middleware_model_retry, #{max_retries => 3}},

    %% 工具重试
    {middleware_tool_retry, #{max_retries => 2}},

    %% 对话摘要
    {middleware_summarization, #{
        window_size => 20,
        max_tokens => 4000
    }},

    %% 人工审批
    {middleware_human_approval, #{
        require_approval => [<<"shell_exec">>, <<"file_write">>]
    }}
],

%% 在 Agent 配置中使用
AgentConfig = #{
    middlewares => Middlewares,
    llm => LLMConfig,
    tools => Tools
}.
```

### 创建自定义中间件

```erlang
-module(my_middleware).
-behaviour(beamai_middleware).

-export([init/1, before_model/2, after_model/2]).

init(Opts) ->
    #{max_calls => maps:get(max_calls, Opts, 10)}.

before_model(State, #{max_calls := MaxCalls} = _MwState) ->
    Count = maps:get(model_call_count, State, 0),
    case Count >= MaxCalls of
        true -> {halt, model_call_limit_exceeded};
        false -> {update, #{model_call_count => Count + 1}}
    end.

after_model(State, _MwState) ->
    ok.
```

### Middleware 返回值

| 返回值 | 说明 |
|--------|------|
| `ok` | 无修改，继续执行 |
| `{update, StateUpdates}` | 更新图状态 |
| `{goto, Node}` | 跳转到指定节点 (model \| tools \| '__end__') |
| `{update_goto, StateUpdates, Node}` | 更新状态并跳转 |
| `{halt, Reason}` | 中止执行并返回错误 |
| `{interrupt, Action}` | 中断等待用户确认 |

### 内置中间件

| 中间件 | 说明 |
|--------|------|
| `middleware_call_limit` | 限制 LLM/工具调用次数 |
| `middleware_model_retry` | LLM 调用失败自动重试 |
| `middleware_model_fallback` | LLM 模型降级策略 |
| `middleware_tool_retry` | 工具执行失败重试 |
| `middleware_tool_selector` | 智能工具筛选（减少 token） |
| `middleware_tool_emulator` | 工具调用模拟（测试用） |
| `middleware_summarization` | 对话历史摘要压缩 |
| `middleware_pii_detection` | PII 敏感信息检测 |
| `middleware_human_approval` | 危险操作人工审批 |
| `middleware_context_editing` | 上下文动态编辑 |
| `middleware_file_search` | 文件搜索增强 |
| `middleware_shell_tool` | 持久 Shell 会话 |
| `middleware_todo_list` | TODO 任务管理 |

### 使用预设

```erlang
%% 获取预设配置
SafePreset = beamai_middleware_presets:safe(),
DevPreset = beamai_middleware_presets:development(),

%% 组合预设
Middlewares = beamai_middleware_presets:combine([SafePreset, MyMiddlewares]).
```

## Provider 机制

### 创建自定义 Provider

```erlang
-module(my_tool_provider).
-behaviour(beamai_tool_provider).

-export([info/0, is_available/0, list_tools/1, find_tool/2]).

info() ->
    #{name => <<"My Provider">>, version => <<"1.0.0">>}.

is_available() -> true.

list_tools(Opts) ->
    Categories = maps:get(categories, Opts, all),
    {ok, filter_by_category(my_tools(), Categories)}.

find_tool(Name, _Opts) ->
    case lists:keyfind(Name, 1, my_tools()) of
        {Name, Tool} -> {ok, Tool};
        false -> {error, not_found}
    end.
```

### 使用多个 Provider

```erlang
Tools = beamai_tools:get_tools([file, custom], #{
    providers => [
        beamai_tool_provider_builtin,
        beamai_tool_provider_custom,
        beamai_tool_provider_mcp
    ]
}).
```

## 冲突解决策略

工具注册表支持多种冲突解决策略：

```erlang
%% 先添加的优先（默认）
Tools = beamai_tool_registry:build(Registry, fun strategy_first_wins/2),

%% 后添加的优先
Tools = beamai_tool_registry:build(Registry, fun strategy_last_wins/2),

%% 冲突时报错
Tools = beamai_tool_registry:build(Registry, fun strategy_error/2).
```

## API 参考

### beamai_tools

| 函数 | 说明 |
|------|------|
| `get_tools/1,2` | 获取指定分类的工具 |
| `get_all_tools/0,1` | 获取所有工具 |
| `find_tool/1,2` | 按名称查找工具 |
| `execute/2,3` | 执行工具 |
| `to_llm_spec/1` | 转换为 LLM 格式 |
| `to_llm_specs/1` | 批量转换 |

### beamai_tool_registry

| 函数 | 说明 |
|------|------|
| `new/0` | 创建空注册表 |
| `add_tools/2` | 添加工具列表 |
| `add_provider/2,3` | 添加 Provider |
| `build/1,2` | 构建工具列表 |
| `from_config/1` | 从配置构建 |
| `from_providers/1,2` | 从 Provider 列表构建 |

### beamai_middleware_runner

| 函数 | 说明 |
|------|------|
| `init/1` | 初始化中间件链 |
| `run_hook/3,4` | 执行指定钩子 |
| `get_middleware_state/2` | 获取中间件状态 |
| `set_middleware_state/3` | 设置中间件状态 |

## Adapter 模式（依赖解耦）

beamai_tools 通过 Adapter 模式解耦对 `beamai_llm` 和 `beamai_memory` 的依赖。

### 架构图

```
┌─────────────────────────────────────────────────────────────┐
│                      beamai_core                            │
│  ┌─────────────────────┐  ┌─────────────────────────────┐   │
│  │beamai_llm_behaviour │  │beamai_buffer_behaviour      │   │
│  │  - chat/2           │  │  - new/1                    │   │
│  │  - is_valid_config/1│  │  - count_tokens/2           │   │
│  └─────────────────────┘  │  - build_context/2          │   │
│            ▲              └─────────────────────────────┘   │
│            │ implements                 ▲ implements        │
└────────────┼────────────────────────────┼───────────────────┘
┌────────────┼────────────┐  ┌────────────┼───────────────────┐
│  beamai_llm│            │  │ beamai_memory                  │
│  ┌─────────┴─────────┐  │  │  ┌─────────┴─────────────────┐ │
│  │   llm_client      │  │  │  │beamai_conversation_buffer │ │
│  └───────────────────┘  │  │  └───────────────────────────┘ │
└─────────────────────────┘  └────────────────────────────────┘
             ▲ uses (via adapter)         ▲ uses (via adapter)
┌────────────┴────────────────────────────┴───────────────────┐
│                      beamai_tools                           │
│  ┌───────────────────┐  ┌─────────────────────────────────┐ │
│  │beamai_llm_adapter │  │beamai_buffer_adapter            │ │
│  └───────────────────┘  └─────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### 使用默认实现

默认情况下，Adapter 使用以下实现：
- `beamai_llm_adapter` → `llm_client`
- `beamai_buffer_adapter` → `beamai_conversation_buffer`

### 自定义实现

可以通过 Application 环境变量替换默认实现：

```erlang
%% sys.config 中配置
{beamai_tools, [
    {llm_module, my_llm_client},
    {buffer_module, my_buffer}
]}
```

或者在调用时传入：

```erlang
%% LLM Adapter
beamai_llm_adapter:chat(Config, Request, #{llm_module => my_llm}).

%% Buffer Adapter
beamai_buffer_adapter:new(Opts, #{buffer_module => my_buffer}).
```

### 实现自定义模块

自定义模块需要实现对应的 Behaviour：

```erlang
%% 自定义 LLM 客户端
-module(my_llm_client).
-behaviour(beamai_llm_behaviour).

-export([chat/2, is_valid_config/1]).

chat(Config, Request) ->
    %% 实现 LLM 调用逻辑
    {ok, Response}.

is_valid_config(Config) ->
    %% 验证配置
    true.
```

```erlang
%% 自定义 Buffer
-module(my_buffer).
-behaviour(beamai_buffer_behaviour).

-export([new/1, count_tokens/2, build_context/2]).

new(Opts) -> #{...}.
count_tokens(Config, Messages) -> 0.
build_context(Config, Messages) -> {ok, #{messages => Messages}}.
```

## 依赖

**必需依赖**:
- `beamai_core`: Behaviour 定义、基础类型
- `jsx`: JSON 编解码

**可选依赖**（通过 Adapter 解耦）:
- `beamai_llm`: 提供 `llm_client`（默认 LLM 实现）
- `beamai_memory`: 提供 `beamai_conversation_buffer`（默认 Buffer 实现）

## 许可证

Apache-2.0
