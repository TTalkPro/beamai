# Tool 重构设计文档

## 背景

当前架构中 `plugin` 层是冗余的，增加了不必要的复杂性。本次重构目标是去掉 plugin 层，让 tools 直接注册到 kernel 中。

## 当前架构

```
Kernel
  └── plugins: #{Name => Plugin}
        └── Plugin
              └── functions: [function_def()]
                    └── function_def (handler, parameters, ...)
```

**问题**：
1. `plugin` 作为中间层是冗余的
2. `beamai_function.erl` 的 `function_def` 和 `beamai_tool.erl` 的 `tool_def` 概念重叠
3. 查找函数时需要遍历所有 plugins
4. 函数名支持 `plugin.func` 格式增加了复杂性

## 目标架构

```
Kernel
  └── tools: #{Name => tool_spec()}
        └── tool_spec (handler, parameters, tag, ...)
```

## 关键变更

| 模块 | 当前 | 目标 | 变更 |
|------|------|------|------|
| `beamai_kernel.erl` | `plugins: #{Name => plugin()}` | `tools: #{Name => tool_spec()}` | 重构存储和查找逻辑 |
| `beamai_plugin.erl` | 插件容器 | **删除** | 功能合并到 kernel |
| `beamai_function.erl` | `function_def()` | **删除** | 合并到 `beamai_tool.erl` |
| `beamai_plugin_behaviour.erl` | `plugin_info/0, functions/0` | **重命名** | → `beamai_tool_behaviour` |
| `beamai.erl` | `add_plugin/2,3, function/2,3` | `add_tool/2, tool/2,3` | API 变更 |

## tool_spec 类型定义

```erlang
-type tool_spec() :: #{
    name := binary(),                    % 必填：工具名称
    handler := handler(),                % 必填：处理器
    description => binary(),             % 可选：描述
    parameters => parameters_schema(),   % 可选：参数定义
    tag => binary() | [binary()],        % 新增：分类标签
    timeout => pos_integer(),            % 可选：超时
    retry => retry_config(),             % 可选：重试策略
    metadata => map()                    % 可选：元数据
}.
```

## 实施阶段

### 阶段 1：定义新的 tool_spec 类型和 behaviour ✅ 完成
- 创建 `beamai_tool_behaviour.erl`（在 beamai_core 中）
- 统一 `tool_spec()` 类型定义

### 阶段 2：重构 beamai_kernel ✅ 完成
- 修改 kernel 类型定义，使用 `tools` 替代 `plugins`
- 新增 `add_tool/2`, `add_tools/2`, `add_tool_module/2` API
- 简化工具查找逻辑

### 阶段 3：迁移现有模块 ✅ 完成
- 将 `beamai_function.erl` 核心逻辑合并到 `beamai_tool.erl`
- 迁移现有 plugin 模块到新的 behaviour：
  - `beamai_plugin_file` → `beamai_tool_file`
  - `beamai_plugin_shell` → `beamai_tool_shell`
  - `beamai_plugin_todo` → `beamai_tool_todo`
  - `beamai_plugin_human` → `beamai_tool_human`

### 阶段 4：更新 Facade API (beamai.erl) ✅ 完成
- 添加新 API：`tool/2,3`, `add_tool/2`, `add_tools/2`, `tools_by_tag/2`
- 移除旧 API：`plugin/2,3`, `add_plugin/2,3`, `function/2,3`

### 阶段 5：清理 ✅ 完成
- 删除 `beamai_plugin.erl`
- 删除 `beamai_function.erl`
- 删除 `beamai_plugin_behaviour.erl`
- 删除旧的 plugin 模块文件
- 删除旧的 `beamai_tool.erl`（beamai_plugin 中的冗余版本）
- 更新 kernel 测试文件
- 删除旧的测试文件

## 新 API 使用示例

```erlang
%% 创建 Kernel
K0 = beamai:kernel(),

%% 方式 1：直接添加工具
K1 = beamai:add_tool(K0, #{
    name => <<"get_weather">>,
    handler => fun get_weather/1,
    description => <<"Get weather for a city">>,
    tag => <<"weather">>,
    parameters => #{
        city => #{type => string, required => true}
    }
}),

%% 方式 2：批量添加
K2 = beamai:add_tools(K1, [Tool1, Tool2, Tool3]),

%% 方式 3：从模块加载
K3 = beamai:add_tool_module(K2, beamai_tool_file),

%% 按 tag 查询
FileTools = beamai:tools_by_tag(K3, <<"io">>),

%% 使用
{ok, Resp, Ctx} = beamai:chat_with_tools(K3, Messages).
```

## Tool Module 定义示例

```erlang
-module(beamai_tool_file).
-behaviour(beamai_tool_behaviour).

-export([tool_info/0, tools/0]).

tool_info() ->
    #{description => <<"File system operations">>,
      tags => [<<"io">>, <<"file">>]}.

tools() ->
    [
        #{name => <<"file_read">>,
          handler => fun ?MODULE:handle_read/2,
          description => <<"Read file content">>,
          tag => <<"io">>,
          parameters => #{...}},
        ...
    ].
```
