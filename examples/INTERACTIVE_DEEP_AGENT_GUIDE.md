# 交互式 Deep Agent 使用指南

## 简介

`example_agent_interactive.erl` 是一个完整的交互式 Deep Agent 示例，展示了如何创建一个可以持续对话的智能助手。

## 特性

### 核心功能
- **持续对话** - Agent 保持运行状态，支持多轮对话
- **Planning（计划）** - Agent 可以制定并跟踪执行计划
- **Reflection（反思）** - Agent 可以反思进展并调整策略
- **子任务派生** - 支持将复杂任务分解为子任务
- **工具调用** - 内置多个实用工具
- **命令系统** - 支持特殊命令查看状态

### 内置工具
1. **search_web** - 搜索网络信息
2. **calculate** - 执行数学计算
3. **get_current_time** - 查询当前时间
4. **save_note** - 保存笔记

### 特殊命令
- `quit` / `exit` - 退出交互
- `trace` - 查看执行轨迹
- `plan` - 查看当前计划

## 使用方法

### 前置条件

1. 设置环境变量：
```bash
export ZHIPU_API_KEY=your-api-key
```

2. 先编译主项目：
```bash
cd /path/to/beamai
rebar3 compile
```

### 方法 1: 使用 Makefile（推荐）

```bash
cd examples

# 编译
make compile

# 启动 shell
make shell
```

然后在 shell 中：

```erlang
example_agent_interactive:run().
```

### 方法 2: 手动设置环境变量

```bash
cd examples
ERL_LIBS=../_build/default/lib rebar3 shell
```

然后在 shell 中：

```erlang
example_agent_interactive:run().
```

## 使用示例

### 示例 1: 简单对话

```
>>> 你好

[思考中...]

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
你好！我是你的智能助手。我可以帮你：
- 搜索网络信息
- 进行数学计算
- 查询时间
- 保存笔记
- 制定计划并执行复杂任务

有什么我可以帮助你的吗？
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

>>>
```

### 示例 2: 使用工具

```
>>> 帮我计算 25 * 4 + 100

[思考中...]

  [计算] 25 * 4 + 100

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
让我来帮你计算：

25 * 4 = 100
100 + 100 = 200

答案是 200
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[统计: 迭代 1 次，工具调用 1 次]
```

### 示例 3: 复杂任务（带计划）

```
>>> 帮我规划一次去北京的旅行

[思考中...]

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
让我为你制定一个北京旅行计划！

我会先搜索相关信息，然后给你一个详细的计划。
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[统计: 迭代 3 次，工具调用 2 次]
```

### 示例 4: 查看计划

```
>>> plan

当前计划:
  目标: 规划北京旅行
  [1] completed: 搜索北京景点信息
  [2] in_progress: 制定行程安排
  [3] pending: 计算预算
```

### 示例 5: 查看执行轨迹

```
>>> trace

执行轨迹 (5 条):
  - #{type => tool_call, name => <<"search_web">>, ...}
  - #{type => llm_response, content => ...}
  - #{type => checkpoint, label => ...}
  ...
```

## Agent 配置

### LLM 配置

使用 `llm_client:create/2` 创建 LLM 配置：

```erlang
%% 使用智谱 GLM-4.7（通过 Anthropic 兼容 API）
LLMConfig = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => ApiKey,
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    timeout => 120000,
    max_tokens => 2048
}).

%% 或使用智谱原生 API
LLMConfig = llm_client:create(zhipu, #{
    model => <<"glm-4.6">>,
    api_key => ApiKey,
    timeout => 120000
}).
```

### 创建 Deep Agent

```erlang
%% 创建 Agent 配置
Config = beamai_deepagent:new(#{
    llm => LLMConfig,
    max_depth => 2,                 %% 允许子任务嵌套深度
    planning_enabled => true,        %% 启用计划
    reflection_enabled => true,      %% 启用反思
    tools => Tools,                  %% 工具列表
    max_iterations => 15,            %% 最大迭代次数
    system_prompt => Prompt          %% 系统提示词
}).

%% 运行任务
{ok, Result} = beamai_deepagent:run(Config, <<"你的任务描述">>).
```

### 自定义工具

```erlang
#{
    name => <<"my_tool">>,
    description => <<"工具描述">>,
    parameters => #{
        type => object,
        properties => #{
            <<"param">> => #{
                type => string,
                description => <<"参数描述">>
            }
        },
        required => [<<"param">>]
    },
    handler => fun(Args, _State) ->
        %% 工具逻辑
        #{result => <<"返回值">>}
    end
}
```

## 高级功能

### 1. 对话历史

Agent 自动维护对话历史，支持多轮对话：

```
>>> 我叫 David

好的，David！很高兴认识你。

>>> 我叫什么名字？

你的名字是 David。
```

### 2. 计划更新

Agent 可以动态更新计划：

```
>>> plan

当前计划:
  [1] completed: 步骤1
  [2] in_progress: 步骤2  <-- 当前进行中
  [3] pending: 步骤3
```

### 3. 反思调整

启用反思后，Agent 会：
- 分析当前进展
- 识别问题和障碍
- 调整执行策略
- 决定下一步行动

### 4. 子任务派生

对于复杂任务，Agent 可以：
- 派生子 Agent 处理子任务
- 并行执行多个子任务
- 汇总子任务结果
- 继续主任务流程

## 调试和监控

### 查看执行轨迹

```
>>> trace
```

显示最近的执行记录，包括：
- 工具调用
- LLM 响应
- 检查点
- 反思记录
- 子任务派生

### 查看当前计划

```
>>> plan
```

显示：
- 计划目标
- 步骤列表
- 每个步骤的状态

## 扩展建议

### 添加更多工具

```erlang
%% 文件操作工具
#{
    name => <<"read_file">>,
    description => <<"读取文件内容">>,
    parameters => #{
        type => object,
        properties => #{
            <<"path">> => #{type => string}
        },
        required => [<<"path">>]
    },
    handler => fun(Args, _State) ->
        Path = maps:get(<<"path">>, Args),
        case file:read_file(binary_to_list(Path)) of
            {ok, Content} ->
                #{content => Content};
            {error, Reason} ->
                #{error => atom_to_list(Reason)}
        end
    end
}
```

### 集成真实 API

```erlang
%% 使用真实的天气 API
handler => fun(Args, _State) ->
    City = maps:get(<<"city">>, Args),
    %% 调用实际 API
    case httpc:request(get, {WeatherApiUrl, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            jsx:decode(list_to_binary(Body), [return_maps]);
        {error, Reason} ->
            #{error => Reason}
    end
end
```

## 注意事项

1. **API Key 配置**
   - 确保设置了环境变量 `ZHIPU_API_KEY`
   - 使用 `example_utils:get_llm_config()` 自动获取配置

2. **超时设置**
   - GLM 模型响应可能较慢，建议设置 `timeout => 120000` (120秒)

3. **并发限制**
   - 智谱 API 有并发限制，避免同时发送过多请求

4. **成本控制**
   - 设置 `max_iterations` 防止无限循环
   - 设置 `max_depth` 防止过深的子任务嵌套

## 相关文件

- [example_agent_interactive.erl](src/example_agent_interactive.erl) - 交互式 Agent 示例
- [example_agent_deep.erl](src/example_agent_deep.erl) - Deep Agent 示例
- [example_agent_simple.erl](src/example_agent_simple.erl) - Simple Agent 示例
- [example_utils.erl](src/example_utils.erl) - 公共工具模块

## 故障排除

### Agent 没有响应

检查：
1. API Key 是否正确设置
2. 网络连接是否正常
3. LLM 配置是否使用 `llm_client:create/2` 创建

### 工具调用失败

检查：
1. 工具参数是否正确
2. 工具处理器是否抛出异常
3. 查看完整的错误信息

### Agent 循环执行

检查：
1. 是否设置了 `max_iterations`
2. LLM 是否能够理解并完成任务
3. 使用 `trace` 命令查看执行过程

### 编译错误

确保：
1. 先编译主项目：`cd .. && rebar3 compile`
2. 使用 `make compile` 编译 examples
3. 或手动设置 `ERL_LIBS=../_build/default/lib`

## 反馈和贡献

如有问题或建议，请：
- 提交 Issue
- 发起 Pull Request
