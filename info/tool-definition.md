# Tool 定义指南

本文档描述 BeamAI 中工具（Tool）的定义方式和使用方法。

## 核心类型定义

### tool_spec()

工具的完整定义结构：

```erlang
-type tool_spec() :: #{
    name := binary(),                    % 必填：工具名称
    handler := handler(),                % 必填：处理器
    description => binary(),             % 可选：描述（供 LLM 理解工具用途）
    parameters => parameters_schema(),   % 可选：参数定义
    tag => binary() | [binary()],        % 可选：分类标签（用于工具分组）
    timeout => pos_integer(),            % 可选：超时时间（毫秒，默认 30000）
    retry => #{max => integer(), delay => integer()},  % 可选：重试策略
    filters => [filter_ref()],           % 可选：过滤器引用
    metadata => map()                    % 可选：自定义元数据
}.
```

### handler()

处理器支持四种形式：

```erlang
-type handler() ::
    fun((args()) -> tool_result())                        % fun/1：仅接收参数
    | fun((args(), beamai_context:t()) -> tool_result())  % fun/2：参数 + 上下文
    | {module(), atom()}                                  % {M, F}：模块函数
    | {module(), atom(), [term()]}.                       % {M, F, ExtraArgs}：带额外参数
```

### tool_result()

工具执行结果：

```erlang
-type tool_result() ::
    {ok, term()}                          % 成功，返回结果
    | {ok, term(), beamai_context:t()}    % 成功，返回结果和更新的上下文
    | {error, term()}.                    % 失败
```

### parameters_schema()

参数定义（会自动转换为 JSON Schema）：

```erlang
-type parameters_schema() :: #{
    atom() | binary() => param_spec()
}.

-type param_spec() :: #{
    type := string | integer | float | boolean | array | object,
    description => binary(),       % 参数描述
    required => boolean(),         % 是否必填
    default => term(),             % 默认值
    enum => [term()],              % 枚举值
    items => param_spec(),         % array 元素类型
    properties => parameters_schema()  % object 嵌套属性
}.
```

## 使用示例

### 方式 1：直接定义 Map

最常用的方式，直接构造 tool_spec map：

```erlang
WeatherTool = #{
    name => <<"get_weather">>,
    description => <<"Get the current weather for a city. Returns temperature and conditions.">>,
    tag => <<"weather">>,
    parameters => #{
        <<"city">> => #{
            type => string,
            required => true,
            description => <<"City name, e.g. Beijing, Tokyo">>
        },
        <<"unit">> => #{
            type => string,
            enum => [<<"celsius">>, <<"fahrenheit">>],
            description => <<"Temperature unit">>
        }
    },
    handler => fun(Args, _Ctx) ->
        City = maps:get(<<"city">>, Args, <<"unknown">>),
        {ok, #{city => City, temp => 25, condition => <<"Sunny">>}}
    end
}.

%% 注册到 Kernel
K1 = beamai_kernel:add_tool(K0, WeatherTool).
```

### 方式 2：使用 beamai_tool:new/2,3

使用辅助函数创建：

```erlang
%% 最小形式
Tool1 = beamai_tool:new(<<"my_tool">>, fun handle/1).

%% 带选项
Tool2 = beamai_tool:new(<<"my_tool">>, fun handle/2, #{
    description => <<"Tool description">>,
    tag => <<"category">>,
    parameters => #{
        <<"arg1">> => #{type => string, required => true}
    },
    timeout => 60000,
    retry => #{max => 3, delay => 1000}
}).
```

### 方式 3：Tool Module（behaviour 模式）

适用于定义一组相关工具：

```erlang
-module(my_tools).
-behaviour(beamai_tool_behaviour).

-export([tool_info/0, tools/0]).

%% 模块级元信息（可选）
tool_info() ->
    #{
        description => <<"My custom tool collection">>,
        tags => [<<"custom">>, <<"utility">>]  % 默认 tags，会应用到没有 tag 的工具
    }.

%% 返回工具列表
tools() ->
    [
        #{
            name => <<"tool_a">>,
            description => <<"First tool">>,
            handler => fun ?MODULE:handle_a/2,
            parameters => #{...}
        },
        #{
            name => <<"tool_b">>,
            description => <<"Second tool">>,
            tag => <<"special">>,  % 覆盖默认 tag
            handler => fun ?MODULE:handle_b/2
        }
    ].

%% Handler 实现
handle_a(Args, Ctx) -> {ok, <<"result_a">>}.
handle_b(Args, Ctx) -> {ok, <<"result_b">>}.
```

加载到 Kernel：

```erlang
K1 = beamai_kernel:add_tool_module(K0, my_tools).
```

## Kernel API

### 添加工具

```erlang
%% 添加单个工具
K1 = beamai_kernel:add_tool(K0, ToolSpec).

%% 批量添加
K2 = beamai_kernel:add_tools(K1, [Tool1, Tool2, Tool3]).

%% 从模块加载
K3 = beamai_kernel:add_tool_module(K2, my_tool_module).
```

### 查询工具

```erlang
%% 获取所有工具的 schema（用于发送给 LLM）
ToolSpecs = beamai_kernel:get_tool_specs(Kernel).

%% 按名称查找
{ok, Tool} = beamai_kernel:find_tool(Kernel, <<"get_weather">>).

%% 按 tag 过滤
WeatherTools = beamai_kernel:tools_by_tag(Kernel, <<"weather">>).
```

### 调用工具

```erlang
%% 通过 Kernel 调用（会应用 filters）
{ok, Result, NewCtx} = beamai_kernel:invoke(Kernel, <<"get_weather">>, Args, Ctx).

%% 直接调用 tool_spec
{ok, Result} = beamai_tool:invoke(ToolSpec, Args).
{ok, Result, NewCtx} = beamai_tool:invoke(ToolSpec, Args, Ctx).
```

## Handler 参数说明

### Args

LLM 传递的参数，为 map 类型：

```erlang
%% 示例 Args
#{
    <<"city">> => <<"Beijing">>,
    <<"unit">> => <<"celsius">>
}
```

### Context

执行上下文，包含运行时信息：

```erlang
%% 可从 Context 获取的信息
RunId = beamai_context:get(run_id, Ctx),
AgentId = beamai_context:get(agent_id, Ctx),
Custom = beamai_context:get(my_key, Ctx).

%% 更新 Context（需要返回 {ok, Result, NewCtx}）
NewCtx = beamai_context:set(my_key, my_value, Ctx),
{ok, Result, NewCtx}.
```

## Schema 转换

工具定义会自动转换为 LLM 所需的格式：

```erlang
%% 转换为 OpenAI 格式
OpenAISchema = beamai_tool:to_tool_schema(ToolSpec, openai).
%% 结果：
%% #{
%%     <<"type">> => <<"function">>,
%%     <<"function">> => #{
%%         <<"name">> => <<"get_weather">>,
%%         <<"description">> => <<"...">>,
%%         <<"parameters">> => #{type => object, properties => ..., required => [...]}
%%     }
%% }

%% 转换为 Anthropic 格式
AnthropicSchema = beamai_tool:to_tool_schema(ToolSpec, anthropic).
%% 结果：
%% #{
%%     <<"name">> => <<"get_weather">>,
%%     <<"description">> => <<"...">>,
%%     <<"input_schema">> => #{type => object, properties => ..., required => [...]}
%% }
```

## 内置工具模块

BeamAI 提供以下内置工具模块：

| 模块 | 描述 | 工具 |
|------|------|------|
| `beamai_tool_file` | 文件操作 | file_read, file_write, file_list, file_glob |
| `beamai_tool_shell` | Shell 命令 | shell_exec |
| `beamai_tool_todo` | 任务管理 | todo_add, todo_list, todo_update |
| `beamai_tool_human` | 人工交互 | human_input |

使用示例：

```erlang
{ok, Agent} = beamai_agent:new(#{
    llm => LLMConfig,
    plugins => [beamai_tool_file, beamai_tool_shell]
}).
```

## 最佳实践

1. **描述要清晰**：description 是 LLM 决定是否调用工具的依据，应当准确描述工具功能和适用场景

2. **参数定义完整**：为每个参数提供 type、description 和 required 字段

3. **使用 tag 分组**：便于按类别管理和过滤工具

4. **错误处理**：Handler 应返回 `{error, Reason}` 而非抛出异常

5. **超时设置**：长时间操作应设置合适的 timeout

```erlang
%% 好的工具定义示例
#{
    name => <<"search_database">>,
    description => <<"Search the product database by keyword. "
                    "Returns matching products with name, price, and stock.">>,
    tag => [<<"database">>, <<"search">>],
    timeout => 10000,
    parameters => #{
        <<"keyword">> => #{
            type => string,
            required => true,
            description => <<"Search keyword (product name or category)">>
        },
        <<"limit">> => #{
            type => integer,
            description => <<"Maximum results to return (default: 10)">>
        }
    },
    handler => fun(Args, _Ctx) ->
        Keyword = maps:get(<<"keyword">>, Args),
        Limit = maps:get(<<"limit">>, Args, 10),
        case db:search(Keyword, Limit) of
            {ok, Results} -> {ok, Results};
            {error, Reason} -> {error, {db_error, Reason}}
        end
    end
}
```
