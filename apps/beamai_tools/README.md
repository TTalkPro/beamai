# beamai_tools - 公共工具库

Agent 工具的统一定义、注册和管理模块。

## 核心功能

- **工具管理**: 统一的工具定义格式和获取接口
- **Provider 机制**: 支持多种工具来源（内置、自定义、MCP）
- **工具注册表**: 构建器模式，支持冲突解决策略
- **工具执行**: 统一的工具调用接口
- **LLM 适配**: 转换为 OpenAI/Anthropic 函数调用格式

## 模块结构

```
beamai_tools/
├── beamai_tools.erl           # 主 API 模块
├── beamai_tool_registry.erl   # 工具注册表
├── beamai_tool_provider.erl   # Provider 行为定义
├── beamai_tool_security.erl   # 工具安全检查
├── providers/
│   ├── beamai_tool_provider_builtin.erl  # 内置 Provider
│   └── beamai_tool_provider_custom.erl   # 自定义 Provider
└── tools/
    ├── beamai_tools_file.erl   # 文件操作工具
    ├── beamai_tools_shell.erl  # Shell 命令工具
    ├── beamai_tools_todo.erl   # TODO 管理工具
    └── beamai_tools_human.erl  # 人机交互工具
```

## 快速开始

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

## 工具定义格式

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

## 内置工具分类

| 分类 | 模块 | 工具 |
|------|------|------|
| `file` | beamai_tools_file | file_read, file_write, file_list, file_search |
| `shell` | beamai_tools_shell | shell_exec |
| `todo` | beamai_tools_todo | todo_add, todo_list, todo_complete |
| `human` | beamai_tools_human | ask_human, confirm_action |

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

### 示例：DeepAgent Tool Provider

`beamai_deepagent` 模块提供了一个完整的 Provider 实现示例：

```erlang
%% beamai_deepagent_tool_provider 根据配置动态返回工具
Config = #{
    depth => 0,
    planning_mode => full,
    reflection_enabled => true
},

%% 通过 beamai_tool_registry 使用
Tools = beamai_tool_registry:from_config(#{
    providers => [{beamai_deepagent_tool_provider, Config}]
}).

%% 与其他 Provider 组合
AllTools = beamai_tool_registry:from_config(#{
    providers => [
        {beamai_deepagent_tool_provider, Config},
        {beamai_tool_provider_mcp, #{server => my_server}}
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

## 依赖

- `beamai_core`: 基础类型定义
- `jsx`: JSON 编解码

## 许可证

Apache-2.0
