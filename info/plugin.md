# Plugin 系统

## 核心概念

Plugin 是一组相关函数的逻辑分组，本质上是一个 map：

```erlang
-type plugin() :: #{
    name := binary(),
    description => binary(),
    functions := [beamai_function:function_def()],
    metadata => map()
}.
```

## 创建方式

### 1. 手动创建 (`beamai_plugin:new/2,3`)

```erlang
Plugin = beamai:plugin(<<"math">>, [
    beamai:function(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end)
])
```

`new/2` 内部会给每个 function 打上 `plugin => Name` 标签：

```erlang
TaggedFunctions = [F#{plugin => Name} || F <- Functions]
```

### 2. 从模块加载 (`beamai_plugin:from_module/1`)

模块需导出 `plugin_info/0` 和 `functions/0`：

```erlang
from_module(Module) ->
    Info = Module:plugin_info(),       %% 返回 #{name => ..., description => ...}
    RawFunctions = Module:functions(), %% 返回 [function_def()]
    ...
```

## 注册到 Kernel

Plugin 注册到 Kernel 的 `plugins` map 中，key 是 plugin name：

```erlang
add_plugin(#{plugins := Plugins} = Kernel, #{name := Name} = Plugin) ->
    Kernel#{plugins => Plugins#{Name => Plugin}}.
```

## 函数调用流程

调用 `beamai:invoke(Kernel, <<"math.add">>, #{a => 1, b => 2})` 时：

### 1. 解析函数名

- 如果包含 `.`，按 `PluginName.FuncName` 拆分，定向查找
- 如果不含 `.`，遍历所有 plugin 搜索

```erlang
get_function(#{plugins := Plugins}, FuncName) ->
    case binary:split(FuncName, <<".">>) of
        [PluginName, LocalName] ->
            case maps:find(PluginName, Plugins) of
                {ok, Plugin} -> beamai_plugin:get_function(Plugin, LocalName);
                ...
            end;
        [_Name] ->
            search_all_plugins(Plugins, FuncName)
    end.
```

### 2. 执行过滤器 + 调用函数

```
pre_filter -> function:invoke -> post_filter -> 返回结果
```

## Tool Schema 生成

Plugin 可以导出为 LLM 可用的 tool schema：

```erlang
get_tool_specs(Kernel) ->
    Functions = list_functions(Kernel),
    [beamai_function:to_tool_spec(F) || F <- Functions].
```

每个 function 的 `parameters` schema 会被转换为 JSON Schema 格式（OpenAI 或 Anthropic），
供 `chat_with_tools` 工具调用循环使用。

## 整体流程图

```
beamai:add_plugin(Kernel, Plugin)
    |
    v
Kernel#{plugins => #{<<"math">> => Plugin}}
    |
    |-- invoke(K, <<"math.add">>, Args)
    |     -> get_function -> pre_filter -> function:invoke -> post_filter -> {ok, Result}
    |
    +-- chat_with_tools(K, Messages)
          -> get_tool_specs -> LLM 返回 tool_calls -> invoke 对应函数 -> 循环直到无 tool_call
```

## 关键源文件

| 文件 | 职责 |
|------|------|
| `apps/beamai_core/src/kernel/beamai_plugin.erl` | 插件创建、查找、合并 |
| `apps/beamai_core/src/kernel/beamai_function.erl` | 函数定义、调用、schema 生成 |
| `apps/beamai_core/src/kernel/beamai_kernel.erl` | Kernel 核心，管理插件和调用循环 |
| `apps/beamai_core/src/beamai.erl` | Facade 入口 API |
