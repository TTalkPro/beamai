# Tool 系统（原 Plugin 系统）

> **注意**：Plugin 层已在重构中移除，工具现在直接注册到 Kernel。本文档描述当前的 Tool 架构。

## 架构变更

### 旧架构（已废弃）

```
Kernel
  └── plugins: #{Name => Plugin}
        └── Plugin
              └── functions: [function_def()]
```

### 新架构

```
Kernel
  └── tools: #{Name => tool_spec()}
        └── tool_spec (handler, parameters, tag, ...)
```

## 核心类型

Tool 是一个独立的可调用单元：

```erlang
-type tool_spec() :: #{
    name := binary(),                    % 必填：工具名称
    handler := handler(),                % 必填：处理器
    description => binary(),             % 可选：描述
    parameters => parameters_schema(),   % 可选：参数定义
    tag => binary() | [binary()],        % 可选：分类标签
    timeout => pos_integer(),            % 可选：超时
    retry => #{max => integer(), delay => integer()},
    metadata => map()
}.
```

## 创建方式

### 1. 直接定义 Map

```erlang
Tool = #{
    name => <<"get_weather">>,
    description => <<"Get weather for a city">>,
    tag => <<"weather">>,
    parameters => #{
        <<"city">> => #{type => string, required => true}
    },
    handler => fun(Args, _Ctx) ->
        City = maps:get(<<"city">>, Args),
        {ok, #{city => City, temp => 25}}
    end
}.
```

### 2. 使用 beamai_tool:new/2,3

```erlang
Tool = beamai_tool:new(<<"my_tool">>, fun handle/2, #{
    description => <<"Tool description">>,
    parameters => #{...}
}).
```

### 3. 从模块加载 (Tool Behaviour)

模块需实现 `beamai_tool_behaviour`，导出 `tools/0`（必需）和 `tool_info/0`（可选）：

```erlang
-module(my_tools).
-behaviour(beamai_tool_behaviour).
-export([tool_info/0, tools/0]).

tool_info() ->
    #{description => <<"My tools">>,
      tags => [<<"custom">>]}.

tools() ->
    [
        #{name => <<"tool_a">>, handler => fun ?MODULE:handle_a/2, ...},
        #{name => <<"tool_b">>, handler => fun ?MODULE:handle_b/2, ...}
    ].
```

## 注册到 Kernel

工具直接注册到 Kernel 的 `tools` map 中：

```erlang
%% 添加单个工具
K1 = beamai_kernel:add_tool(K0, ToolSpec).

%% 批量添加
K2 = beamai_kernel:add_tools(K1, [Tool1, Tool2]).

%% 从模块加载
K3 = beamai_kernel:add_tool_module(K2, my_tools).
```

## 工具调用流程

调用 `beamai_kernel:invoke(Kernel, <<"get_weather">>, Args, Ctx)` 时：

### 1. 查找工具

直接在 `tools` map 中按名称查找（O(1) 复杂度）：

```erlang
find_tool(#{tools := Tools}, Name) ->
    maps:find(Name, Tools).
```

### 2. 执行过滤器 + 调用工具

```
pre_invocation filter -> tool:invoke -> post_invocation filter -> 返回结果
```

## Tool Schema 生成

工具可导出为 LLM 可用的 tool schema：

```erlang
%% 获取所有工具的 schema
ToolSpecs = beamai_kernel:get_tool_specs(Kernel).

%% 转换为 OpenAI 格式
OpenAISchema = beamai_tool:to_tool_schema(ToolSpec, openai).

%% 转换为 Anthropic 格式
AnthropicSchema = beamai_tool:to_tool_schema(ToolSpec, anthropic).
```

## Tag 分组

工具可通过 tag 分组和过滤：

```erlang
%% 按 tag 查询
WeatherTools = beamai_kernel:tools_by_tag(Kernel, <<"weather">>).

%% 检查工具是否有指定 tag
HasTag = beamai_tool:has_tag(ToolSpec, <<"io">>).
```

## 整体流程图

```
beamai_kernel:add_tool(Kernel, Tool)
    |
    v
Kernel#{tools => #{<<"get_weather">> => ToolSpec}}
    |
    |-- invoke(K, <<"get_weather">>, Args, Ctx)
    |     -> find_tool -> pre_filter -> tool:invoke -> post_filter -> {ok, Result}
    |
    +-- chat_with_tools(K, Messages, Opts)
          -> get_tool_specs -> LLM 返回 tool_calls -> invoke 对应工具 -> 循环直到无 tool_call
```

## 内置工具模块

| 模块 | 描述 | 工具 |
|------|------|------|
| `beamai_tool_file` | 文件操作 | file_read, file_write, file_list, file_glob |
| `beamai_tool_shell` | Shell 命令 | shell_exec |
| `beamai_tool_todo` | 任务管理 | todo_add, todo_list, todo_update |
| `beamai_tool_human` | 人工交互 | human_input |

## 关键源文件

| 文件 | 职责 |
|------|------|
| `apps/beamai_core/src/kernel/beamai_tool.erl` | 工具定义、调用、schema 生成 |
| `apps/beamai_core/src/kernel/beamai_tool_behaviour.erl` | 工具模块行为定义 |
| `apps/beamai_core/src/kernel/beamai_kernel.erl` | Kernel 核心，管理工具和调用循环 |
| `apps/beamai_plugin/src/beamai_plugins.erl` | 工具模块加载辅助 |

## 迁移指南（从 Plugin 到 Tool）

### 旧 API（已废弃）

```erlang
%% 旧方式
K = beamai:add_plugin(Kernel, Plugin).
{ok, Result} = beamai:function(Kernel, <<"plugin.func">>, Args).
```

### 新 API

```erlang
%% 新方式
K = beamai_kernel:add_tool(Kernel, ToolSpec).
{ok, Result, Ctx} = beamai_kernel:invoke(Kernel, <<"tool_name">>, Args, Ctx).
```

### 模块迁移

| 旧模块 | 新模块 |
|--------|--------|
| `beamai_plugin_file` | `beamai_tool_file` |
| `beamai_plugin_shell` | `beamai_tool_shell` |
| `beamai_plugin_todo` | `beamai_tool_todo` |
| `beamai_plugin_human` | `beamai_tool_human` |
