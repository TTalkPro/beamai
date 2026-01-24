# BeamAI Plugin

[English](README_EN.md) | 中文

BeamAI 的插件系统，提供工具定义、Middleware 管道、安全验证和内置插件。

## 特性

- Plugin Behaviour 插件接口定义
- 工具定义和管理（beamai_tool）
- Middleware 管道（前置/后置处理）
- 安全验证（路径遍历、命令注入防护）
- 内置插件（文件、Shell、Human、TODO）
- Middleware 预设（重试、限流、降级、人工审批）

## 模块概览

### 核心模块

- **beamai_plugins** - 插件系统公共 API（加载、注册、查询）
- **beamai_plugin_behaviour** - 插件行为接口定义
- **beamai_tool** - 工具定义和工具 map 操作
- **beamai_tool_security** - 安全验证（路径遍历、命令注入检测）

### Middleware 系统

- **beamai_middleware** - Middleware 定义和类型
- **beamai_middleware_runner** - Middleware 管道执行器
- **beamai_middleware_presets** - 预设 Middleware 配置
- **middleware_call_limit** - 调用次数限制
- **middleware_tool_retry** - 工具调用重试
- **middleware_model_retry** - 模型调用重试
- **middleware_model_fallback** - 模型降级
- **middleware_human_approval** - 人工审批

### 内置插件

- **beamai_plugin_file** - 文件操作插件（读、写、列目录、删除）
- **beamai_plugin_shell** - Shell 命令执行插件
- **beamai_plugin_human** - Human-in-the-loop 插件（提问、确认）
- **beamai_plugin_todo** - TODO 任务管理插件

## API 文档

### beamai_plugins

```erlang
%% 加载插件到 Kernel
beamai_plugins:load(Kernel, PluginModule) -> kernel().

%% 批量加载插件
beamai_plugins:load_all(Kernel, [PluginModule]) -> kernel().

%% 列出可用插件
beamai_plugins:available() -> [module()].

%% 添加 Middleware
beamai_plugins:with_middleware(Kernel, MiddlewareList) -> kernel().

%% 获取预设配置
beamai_plugins:presets(PresetName) -> [middleware()].

%% 快速定义工具
beamai_plugins:define_tool(Name, Description, Handler) -> tool().
beamai_plugins:define_tool(Name, Description, Handler, Schema) -> tool().
beamai_plugins:define_tool(Name, Description, Handler, Schema, Opts) -> tool().
```

### beamai_plugin_behaviour

实现此 behaviour 以创建自定义插件：

```erlang
-callback name() -> binary().
-callback description() -> binary().
-callback functions() -> [beamai_function:function()].
```

### beamai_tool

```erlang
%% 工具定义格式
#{
    name => <<"tool_name">>,
    description => <<"工具描述"/utf8>>,
    parameters => #{
        type => object,
        properties => #{
            <<"param1">> => #{type => string, description => <<"参数描述"/utf8>>}
        },
        required => [<<"param1">>]
    },
    handler => fun(Args, Context) -> {ok, Result} | {error, Reason} end
}
```

### beamai_tool_security

```erlang
%% 验证文件路径安全性
beamai_tool_security:validate_path(Path, AllowedDirs) -> ok | {error, Reason}.

%% 验证 Shell 命令安全性
beamai_tool_security:validate_command(Command, AllowedCommands) -> ok | {error, Reason}.
```

## 使用示例

### 创建自定义插件

```erlang
-module(my_plugin).
-behaviour(beamai_plugin_behaviour).

-export([name/0, description/0, functions/0]).

name() -> <<"my_plugin">>.

description() -> <<"我的自定义插件"/utf8>>.

functions() ->
    [
        beamai_function:new(
            <<"greet">>,
            <<"打招呼"/utf8>>,
            fun(#{<<"name">> := Name}, _Ctx) ->
                {ok, <<"你好，", Name/binary, "！"/utf8>>}
            end,
            #{parameters => #{
                type => object,
                properties => #{
                    <<"name">> => #{type => string}
                },
                required => [<<"name">>]
            }}
        )
    ].
```

### 加载插件到 Kernel

```erlang
Kernel = beamai_kernel:new(),

%% 加载单个插件
Kernel1 = beamai_kernel:add_plugin_from_module(Kernel, beamai_plugin_file),

%% 或使用 beamai_plugins 批量加载
Kernel2 = beamai_plugins:load_all(Kernel, [
    beamai_plugin_file,
    beamai_plugin_shell,
    my_plugin
]),

%% 获取工具规格
Tools = beamai_kernel:get_tool_specs(Kernel2).
```

### 使用 Middleware

```erlang
%% 添加重试和限流 Middleware
Kernel = beamai_kernel:new(),
Kernel1 = beamai_plugins:load_all(Kernel, [beamai_plugin_file]),

%% 使用预设
Kernel2 = beamai_plugins:with_middleware(Kernel1,
    beamai_plugins:presets(safe_execution)),

%% 或手动配置
Kernel3 = beamai_plugins:with_middleware(Kernel1, [
    {middleware_tool_retry, #{max_retries => 3}},
    {middleware_call_limit, #{max_calls => 100}},
    {middleware_human_approval, #{tools => [<<"delete_file">>]}}
]).
```

### 快速定义工具

```erlang
%% 使用 beamai_plugins:define_tool 快速创建
Tool = beamai_plugins:define_tool(
    <<"calculate">>,
    <<"简单计算"/utf8>>,
    fun(#{<<"expression">> := Expr}, _Ctx) ->
        {ok, evaluate(Expr)}
    end,
    #{
        type => object,
        properties => #{
            <<"expression">> => #{type => string}
        },
        required => [<<"expression">>]
    }
).
```

## 内置插件列表

### beamai_plugin_file

| 函数名 | 说明 |
|--------|------|
| `read_file` | 读取文件内容 |
| `write_file` | 写入文件 |
| `list_directory` | 列出目录内容 |
| `create_directory` | 创建目录 |
| `delete_file` | 删除文件 |
| `file_exists` | 检查文件是否存在 |

### beamai_plugin_shell

| 函数名 | 说明 |
|--------|------|
| `execute_command` | 执行 Shell 命令 |

### beamai_plugin_human

| 函数名 | 说明 |
|--------|------|
| `ask_human` | 向用户提问 |
| `confirm_action` | 请求用户确认 |

### beamai_plugin_todo

| 函数名 | 说明 |
|--------|------|
| `write_todos` | 写入待办事项列表 |
| `read_todos` | 读取待办事项列表 |

## 安全机制

beamai_tool_security 提供以下安全检查：

- **路径遍历防护**: 阻止 `../` 等路径遍历攻击
- **命令注入防护**: 检测 Shell 命令中的危险字符和命令
- **白名单验证**: 支持目录白名单和命令白名单

## 依赖

- beamai_core

## 许可证

Apache-2.0
