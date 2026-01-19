# BeamAI Framework 设计模式

本文档详细描述 BeamAI Framework 中使用的核心设计模式及其实现。

## 目录

- [概述](#概述)
- [策略模式](#策略模式)
- [工厂模式](#工厂模式)
- [观察者模式](#观察者模式)
- [装饰器/中间件模式](#装饰器中间件模式)
- [模板方法模式](#模板方法模式)
- [门面模式](#门面模式)
- [协调器模式](#协调器模式)
- [组合模式](#组合模式)
- [Provider 模式](#provider-模式)

---

## 概述

BeamAI Framework 充分利用了 Erlang/OTP 的特性，结合经典设计模式构建了一个可扩展、可维护的 AI Agent 框架。本文档介绍框架中的核心设计模式，帮助开发者理解系统架构并进行扩展开发。

### 设计原则

1. **高内聚低耦合**: 模块职责单一，接口清晰
2. **可插拔**: 通过 Provider 和 Middleware 机制支持灵活扩展
3. **容错性**: 利用 OTP 监督树实现故障隔离和恢复
4. **可测试性**: 模块化设计便于单元测试和集成测试

---

## 策略模式

策略模式允许在运行时选择不同的算法或行为。

### 1. Output Parser 策略

**位置**: `apps/beamai_llm/src/parser/`

Output Parser 使用策略模式支持多种输出格式的解析：

```erlang
%% beamai_output_parser.erl
%% 策略分派：根据格式类型选择具体解析器
do_parse(json, Text, Opts) -> beamai_parser_json:parse(Text, Opts);
do_parse(xml, _Text, _Opts) -> {error, {not_implemented, xml}};
do_parse(csv, _Text, _Opts) -> {error, {not_implemented, csv}};
do_parse(raw, Text, _Opts) -> {ok, Text}.
```

支持的策略：
- **JSON Parser**: 容错解析 LLM 输出的 JSON
- **XML Parser**: XML 格式解析（预留）
- **CSV Parser**: CSV 格式解析（预留）
- **Raw Parser**: 原始文本直接返回

### 2. 重试退避策略

**位置**: `apps/beamai_llm/src/parser/beamai_parser_retry.erl`

重试机制支持多种退避策略：

```erlang
%% 策略选择
backoff_function(linear) -> fun linear_backoff/1;
backoff_function(exponential) -> fun exponential_backoff/1;
backoff_function(fibonacci) -> fun fibonacci_backoff/1.

%% 线性退避: attempt * 100ms
linear_backoff(Attempt) -> Attempt * 100.

%% 指数退避: 100ms * 2^(attempt-1)，最大 5 秒
exponential_backoff(Attempt) ->
    BaseDelay = 100 bsl (Attempt - 1),
    min(BaseDelay, 5000).

%% 斐波那契退避
fibonacci_backoff(Attempt) -> fibonacci(Attempt) * 100.
```

### 3. LLM Provider 策略

**位置**: `apps/beamai_llm/src/`

支持多种 LLM 服务商的统一接口：

```erlang
%% llm_provider 行为定义
-behaviour(llm_provider).

-callback chat(Config, Messages, Opts) -> {ok, Response} | {error, term()}.
-callback stream_chat(Config, Messages, Callback, Opts) -> {ok, Response}.
-callback info() -> map().
```

支持的 Provider：
- `beamai_llm_anthropic` - Anthropic Claude
- `beamai_llm_openai` - OpenAI GPT
- `beamai_llm_zhipu` - 智谱 AI
- `beamai_llm_bailian` - 阿里云百炼
- `beamai_llm_ollama` - Ollama 本地模型

---

## 工厂模式

工厂模式用于创建复杂对象，隐藏创建逻辑。

### 1. Worker 工厂

**位置**: `apps/beamai_agent/src/beamai_coordinator_common.erl`

协调器使用工厂模式创建多个 Agent 实例：

```erlang
%% 批量创建 Worker 进程
start_workers(Agents, BaseConfig, Opts) ->
    lists:map(
        fun(AgentConfig) ->
            create_worker(AgentConfig, BaseConfig, Opts)
        end,
        Agents
    ).
```

### 2. 工具工厂

**位置**: `apps/beamai_agent/src/beamai_coordinator_common.erl`

根据协调模式创建不同类型的工具：

```erlang
%% 创建委托工具
build_delegate_tools(Agents, Pids) ->
    [build_delegate_tool(Agent, Pids) || Agent <- Agents].

%% 创建路由工具
build_router_tool(_Agents, _Pids) ->
    #{
        name => <<"route_to_workers">>,
        description => <<"Route task to specific worker">>,
        parameters => #{...},
        handler => fun route_handler/1
    }.

%% 创建并行执行工具
build_parallel_tool(_Agents, _Pids) ->
    #{
        name => <<"execute_parallel">>,
        description => <<"Execute tasks in parallel">>,
        parameters => #{...},
        handler => fun parallel_handler/1
    }.
```

### 3. Parser 工厂

**位置**: `apps/beamai_llm/src/parser/beamai_output_parser.erl`

```erlang
%% 创建通用 Parser
new(Type, Options) ->
    #{type => Type, options => Options}.

%% 创建 JSON Parser（带默认配置）
json() -> json(#{}).

json(Options) ->
    DefaultOpts = #{
        extract_codeblock => true,
        repair_common => true,
        strip_markdown => true
    },
    #{type => json, options => maps:merge(DefaultOpts, Options)}.
```

---

## 观察者模式

观察者模式用于实现事件通知机制。

### 回调系统

**位置**: `apps/beamai_agent/src/beamai_agent_callbacks.erl`

Agent 执行过程中的事件监听：

```erlang
%% 回调事件类型
-type callback_event() ::
    on_llm_start |      %% LLM 调用开始
    on_llm_end |        %% LLM 响应收到
    on_tool_use |       %% 工具被使用
    on_chain_start |    %% 链执行开始
    on_chain_end |      %% 链执行结束
    on_error.           %% 错误发生

%% 注册回调
register_callback(AgentPid, Event, Callback) ->
    gen_server:call(AgentPid, {register_callback, Event, Callback}).

%% 触发回调
emit_event(State, Event, Data) ->
    Callbacks = get_callbacks(State, Event),
    lists:foreach(
        fun(Callback) -> Callback(Event, Data) end,
        Callbacks
    ).

%% 自定义事件
emit_custom_event(State, EventName, Data) ->
    emit_event(State, {custom, EventName}, Data).
```

### 使用示例

```erlang
%% 注册回调
beamai_agent:register_callback(Agent, on_llm_end, fun(Event, Data) ->
    Response = maps:get(response, Data),
    io:format("LLM Response: ~p~n", [Response])
end).

%% 注册工具使用监控
beamai_agent:register_callback(Agent, on_tool_use, fun(_Event, Data) ->
    ToolName = maps:get(name, Data),
    logger:info("Tool used: ~s", [ToolName])
end).
```

---

## 装饰器/中间件模式

中间件模式允许在核心逻辑周围添加横切关注点。

### Middleware 系统

**位置**: `apps/beamai_agent/src/middleware/`

```
Agent 执行流程：

┌──────────────────┐
│   before_agent   │  ← Agent 开始前
└────────┬─────────┘
         │
         ▼
┌────────────────────────────────────────────────────────┐
│                    Agent Loop                          │
│  ┌──────────────┐                                      │
│  │ before_model │  ← 检查限制、修改消息                  │
│  └──────┬───────┘                                      │
│         ▼                                              │
│  ┌──────────────┐                                      │
│  │   LLM Call   │                                      │
│  └──────┬───────┘                                      │
│         ▼                                              │
│  ┌──────────────┐                                      │
│  │ after_model  │  ← 处理响应、记录日志                  │
│  └──────┬───────┘                                      │
│         ▼                                              │
│  ┌──────────────┐                                      │
│  │ before_tools │  ← 人工审批、参数验证                  │
│  └──────┬───────┘                                      │
│         ▼                                              │
│  ┌──────────────┐                                      │
│  │Tool Execution│                                      │
│  └──────┬───────┘                                      │
│         ▼                                              │
│  ┌──────────────┐                                      │
│  │ after_tools  │  ← 结果验证、失败重试                  │
│  └──────────────┘                                      │
└────────────────────────────────────────────────────────┘
         │
         ▼
┌──────────────────┐
│   after_agent    │  ← 清理资源、记录统计
└──────────────────┘
```

### Middleware 行为定义

```erlang
-behaviour(beamai_middleware).

%% 所有回调都是可选的
-callback init(Opts :: map()) -> middleware_state().
-callback before_agent(State, MwState) -> middleware_result().
-callback after_agent(State, MwState) -> middleware_result().
-callback before_model(State, MwState) -> middleware_result().
-callback after_model(State, MwState) -> middleware_result().
-callback before_tools(State, MwState) -> middleware_result().
-callback after_tools(State, MwState) -> middleware_result().

%% 返回值类型
-type middleware_result() ::
    ok |                                 %% 继续执行
    {update, map()} |                    %% 更新状态
    {goto, model | tools | '__end__'} |  %% 跳转
    {update_goto, map(), goto_target()} |%% 更新并跳转
    {halt, term()} |                     %% 中止
    {interrupt, interrupt_action()}.     %% 中断等待确认
```

### 中间件节点实现

**位置**: `apps/beamai_agent/src/middleware/beamai_middleware_nodes.erl`

```erlang
%% LLM 节点包装：在 LLM 调用前后执行中间件
llm_node(LLMConfig, Middlewares, Opts) ->
    MwChain = beamai_middleware_runner:init(Middlewares),
    fun(State) ->
        State0 = graph:set(State, mw_chain, MwChain),
        %% 执行 before_model 中间件
        case run_before_model(State0, MwChain) of
            {ok, State1} ->
                %% 调用 LLM
                case call_llm(State1, LLMConfig) of
                    {ok, State2} ->
                        %% 执行 after_model 中间件
                        run_after_model(State2, MwChain);
                    Error -> Error
                end;
            {halt, Reason} -> {halt, Reason};
            {goto, Target} -> {goto, Target}
        end
    end.
```

---

## 模板方法模式

模板方法模式定义算法骨架，允许子类重写特定步骤。

### Agent 初始化模板

**位置**: `apps/beamai_agent/src/beamai_agent_init.erl`

```erlang
%% Agent 初始化的标准流程
init_agent(Config) ->
    %% 1. 验证配置
    {ok, ValidConfig} = validate_config(Config),

    %% 2. 创建初始状态
    State0 = create_initial_state(ValidConfig),

    %% 3. 初始化组件
    State1 = init_llm(State0, ValidConfig),
    State2 = init_tools(State1, ValidConfig),
    State3 = init_memory(State2, ValidConfig),

    %% 4. 构建执行图
    Graph = build_graph(State3, ValidConfig),

    %% 5. 返回完整状态
    {ok, State3#{graph => Graph}}.
```

### Graph 执行模板

**位置**: `apps/beamai_agent/src/beamai_agent_runner.erl`

```erlang
%% Graph 执行的标准流程
run_graph(Graph, InitialState) ->
    %% 1. 获取入口节点
    EntryNode = graph:get_entry(Graph),

    %% 2. 执行循环
    run_loop(Graph, EntryNode, InitialState).

run_loop(Graph, CurrentNode, State) ->
    %% 3. 执行当前节点
    case execute_node(CurrentNode, State) of
        {ok, NewState} ->
            %% 4. 确定下一个节点
            case determine_next_node(Graph, CurrentNode, NewState) of
                {next, NextNode} ->
                    run_loop(Graph, NextNode, NewState);
                {end, _Reason} ->
                    {ok, NewState}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

---

## 门面模式

门面模式提供统一的高层接口，简化子系统的使用。

### beamai_agent 门面

**位置**: `apps/beamai_agent/src/beamai_agent.erl`

```erlang
-module(beamai_agent).

%% 统一 API 门面
-export([
    %% 生命周期
    start_link/2, stop/1,

    %% 执行
    run/2, run/3, stream/3,

    %% 状态管理
    get_state/1, get_messages/1,

    %% 配置
    update_config/2,

    %% 回调
    register_callback/3
]).

%% 委托到内部模块
start_link(Name, Config) ->
    beamai_agent_server:start_link(Name, Config).

run(Agent, Input) ->
    beamai_agent_api:run(Agent, Input).

get_state(Agent) ->
    beamai_agent_server:get_state(Agent).
```

### 内部模块职责

| 模块 | 职责 |
|------|------|
| `beamai_agent_server` | gen_server 实现，进程管理 |
| `beamai_agent_api` | 纯函数 API 实现 |
| `beamai_agent_runner` | 图执行逻辑 |
| `beamai_agent_init` | 初始化逻辑 |
| `beamai_agent_callbacks` | 回调管理 |

---

## 协调器模式

协调器模式用于管理多个 Agent 的协作执行。

### Coordinator 实现

**位置**: `apps/beamai_agent/src/beamai_coordinator.erl`

支持两种协调模式：

### 1. Pipeline 模式（顺序协调）

```
任务流程：研究员 → 写作者 → 审核员

┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│  研究员      │ → │  写作者      │ → │  审核员      │
│ (Researcher) │    │  (Writer)   │    │ (Reviewer)  │
└─────────────┘    └─────────────┘    └─────────────┘
```

```erlang
%% Pipeline 协调配置
PipelineConfig = #{
    mode => pipeline,
    agents => [
        #{name => <<"researcher">>, config => ResearcherConfig},
        #{name => <<"writer">>, config => WriterConfig},
        #{name => <<"reviewer">>, config => ReviewerConfig}
    ]
}.
```

### 2. Orchestrator 模式（编排协调）

```
                    ┌─────────────────┐
                    │   Orchestrator  │
                    │   (编排器)       │
                    └────────┬────────┘
                             │
           ┌─────────────────┼─────────────────┐
           │                 │                 │
           ▼                 ▼                 ▼
    ┌─────────────┐   ┌─────────────┐   ┌─────────────┐
    │  Worker A   │   │  Worker B   │   │  Worker C   │
    └─────────────┘   └─────────────┘   └─────────────┘
```

```erlang
%% Orchestrator 协调配置
OrchestratorConfig = #{
    mode => orchestrator,
    agents => [
        #{name => <<"search_agent">>, skills => [search, web]},
        #{name => <<"code_agent">>, skills => [code, review]},
        #{name => <<"data_agent">>, skills => [analysis, visualization]}
    ],
    %% 支持的操作
    operations => [delegate, route, parallel]
}.
```

### 协调工具

| 工具 | 说明 |
|------|------|
| `delegate_to_<agent>` | 委托任务给特定 Agent |
| `route_to_workers` | 根据任务内容路由到合适的 Worker |
| `execute_parallel` | 并行执行多个任务 |

---

## 组合模式

组合模式用于将多个模块组合成更大的功能单元。

### Shared 核心模块

**位置**: `apps/beamai_agent/src/shared/`

提取共享核心逻辑，被多个节点和中间件组合使用：

```
shared/
├── beamai_llm_core.erl      # LLM 调用核心逻辑
├── beamai_tool_core.erl     # 工具执行核心逻辑
└── beamai_state_helpers.erl # 状态管理助手
```

### LLM 核心组合

```erlang
%% beamai_llm_core.erl - 被普通节点和中间件节点共用

%% 核心 LLM 调用逻辑
call_llm(State, LLMConfig) ->
    Messages = get_messages(State),
    Tools = get_tools(State),

    case beamai_llm:chat(LLMConfig, Messages, #{tools => Tools}) of
        {ok, Response} ->
            update_state_with_response(State, Response);
        {error, Reason} ->
            {error, Reason}
    end.
```

### 工具核心组合

```erlang
%% beamai_tool_core.erl - 被普通节点和中间件节点共用

%% 核心工具执行逻辑
execute_tools(State, ToolCalls) ->
    Results = lists:map(
        fun(ToolCall) ->
            execute_single_tool(State, ToolCall)
        end,
        ToolCalls
    ),
    update_state_with_results(State, Results).
```

### 使用示例

```erlang
%% 普通 LLM 节点使用核心模块
-module(beamai_llm_node).

execute(State) ->
    %% 直接使用核心逻辑
    beamai_llm_core:call_llm(State, get_llm_config(State)).

%% 中间件 LLM 节点也使用同样的核心模块
-module(beamai_middleware_nodes).

execute_llm_with_middleware(State, Middlewares) ->
    case run_before_model(State, Middlewares) of
        {ok, State1} ->
            %% 复用相同的核心逻辑
            beamai_llm_core:call_llm(State1, get_llm_config(State1));
        Other -> Other
    end.
```

---

## Provider 模式

Provider 模式是 BeamAI 的核心扩展机制，支持多种实现的统一接口。

### Tool Provider

**位置**: `apps/beamai_tools/src/`

```erlang
%% beamai_tool_provider 行为定义
-behaviour(beamai_tool_provider).

%% 必需回调
-callback list_tools(Opts :: map()) -> {ok, [tool_def()]} | {error, term()}.

%% 可选回调
-callback find_tool(Name :: binary(), Opts :: map()) -> {ok, tool_def()} | {error, not_found}.
-callback info() -> map().
-callback available() -> boolean().
```

### 内置 Provider

| Provider | 说明 |
|----------|------|
| `beamai_tool_provider_builtin` | 内置工具（文件、Shell、TODO 等） |
| `beamai_tool_provider_mcp` | MCP 协议工具 |
| `beamai_deepagent_tool_provider` | Deep Agent 专用工具 |

### Provider 注册

```erlang
%% beamai_tool_registry 配置
Config = #{
    providers => [
        {beamai_tool_provider_builtin, #{}},
        {beamai_tool_provider_mcp, #{server => MCPServer}},
        {my_custom_provider, #{option => value}}
    ]
}.

%% 获取所有可用工具
{ok, Tools} = beamai_tool_registry:list_tools(Config).
```

### 自定义 Provider 示例

```erlang
-module(my_tool_provider).
-behaviour(beamai_tool_provider).

-export([list_tools/1, find_tool/2, info/0, available/0]).

info() ->
    #{name => <<"my_provider">>, version => <<"1.0.0">>}.

available() -> true.

list_tools(_Opts) ->
    {ok, [
        #{
            name => <<"my_tool">>,
            description => <<"My custom tool">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"input">> => #{type => string}
                }
            },
            handler => fun my_tool_handler/1
        }
    ]}.

find_tool(<<"my_tool">>, _Opts) ->
    {ok, hd(element(2, list_tools(#{})))};
find_tool(_, _) ->
    {error, not_found}.

my_tool_handler(#{<<"input">> := Input}) ->
    {ok, <<"Processed: ", Input/binary>>}.
```

---

## 总结

BeamAI Framework 综合运用了多种设计模式，构建了一个灵活、可扩展的 AI Agent 框架：

| 模式 | 应用场景 | 主要优势 |
|------|----------|----------|
| 策略模式 | Parser、退避算法、LLM Provider | 算法可替换 |
| 工厂模式 | Worker、工具创建 | 封装创建逻辑 |
| 观察者模式 | 回调系统 | 解耦事件处理 |
| 中间件模式 | Agent 执行拦截 | 横切关注点分离 |
| 模板方法模式 | 初始化、执行流程 | 定义标准流程 |
| 门面模式 | beamai_agent API | 简化接口 |
| 协调器模式 | 多 Agent 协作 | 管理复杂协作 |
| 组合模式 | Shared 核心模块 | 代码复用 |
| Provider 模式 | 工具、LLM、存储 | 统一扩展机制 |

---

## 更多资源

- [ARCHITECTURE.md](ARCHITECTURE.md) - 架构设计文档
- [MIDDLEWARE.md](MIDDLEWARE.md) - Middleware 系统文档
- [API_REFERENCE.md](API_REFERENCE.md) - API 参考文档
- [OUTPUT_PARSER.md](OUTPUT_PARSER.md) - Output Parser 指南
