# API 参考文档

本文档提供 BeamAI Framework 各模块的主要 API 参考。

## 目录

- [beamai_agent - Simple Agent](#beamai_agent---simple-agent)
- [Middleware 系统](#middleware-系统)
- [beamai_deepagent - Deep Agent](#beamai_deepagent---deep-agent)
- [beamai_llm - LLM 客户端](#beamai_llm---llm-客户端)
- [beamai_memory - 记忆管理](#beamai_memory---记忆管理)
- [beamai_tools - 工具库](#beamai_tools---工具库)
- [beamai_core - 核心模块](#beamai_core---核心模块)
- [beamai_a2a - A2A 协议](#beamai_a2a---a2a-协议)
- [beamai_mcp - MCP 协议](#beamai_mcp---mcp-协议)
- [beamai_rag - RAG 功能](#beamai_rag---rag-功能)

---

## beamai_agent - Simple Agent

ReAct 模式的简单 Agent 实现。

### 生命周期管理

```erlang
%% 启动 Agent（进程模式）
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_agent:start_link(AgentId, Config).

%% 停止 Agent
-spec stop(pid()) -> ok.
beamai_agent:stop(Agent).
```

### 执行 API

```erlang
%% 运行 Agent（进程模式）
-spec run(pid(), binary()) -> {ok, map()} | {error, term()}.
beamai_agent:run(Agent, Input).

%% 单次执行（纯函数模式）
-spec run_once(map(), binary()) -> {ok, map()} | {error, term()}.
beamai_agent:run_once(Config, Input).

%% 使用状态执行
-spec run_with_state(state(), binary(), map()) -> {ok, map(), state()} | {error, term()}.
beamai_agent:run_with_state(State, Input, Opts).
```

### 状态管理

```erlang
%% 创建状态
-spec create_state(map()) -> {ok, state()}.
-spec create_state(binary(), map()) -> {ok, state()}.
beamai_agent:create_state(Config).
beamai_agent:create_state(AgentId, Config).

%% 导出/导入状态
-spec export_state(state()) -> map().
-spec import_state(map(), map()) -> {ok, state()}.
beamai_agent:export_state(State).
beamai_agent:import_state(ExportedData, Config).
```

### Checkpoint 管理

```erlang
%% 保存检查点
-spec save_checkpoint(pid()) -> {ok, binary()} | {error, term()}.
-spec save_checkpoint(pid(), map()) -> {ok, binary()} | {error, term()}.
beamai_agent:save_checkpoint(Agent).
beamai_agent:save_checkpoint(Agent, Metadata).

%% 加载检查点
-spec load_checkpoint(pid(), binary()) -> {ok, map()} | {error, term()}.
-spec load_latest_checkpoint(pid()) -> {ok, map()} | {error, term()}.
beamai_agent:load_checkpoint(Agent, CheckpointId).
beamai_agent:load_latest_checkpoint(Agent).

%% 从检查点恢复
-spec restore_from_checkpoint(pid(), binary()) -> ok | {error, term()}.
beamai_agent:restore_from_checkpoint(Agent, CheckpointId).

%% 列出检查点
-spec list_checkpoints(pid()) -> {ok, [map()]} | {error, term()}.
beamai_agent:list_checkpoints(Agent).
```

### 回调管理

```erlang
%% 获取/设置回调
-spec get_callbacks(pid()) -> map().
-spec set_callbacks(pid(), map()) -> ok.
beamai_agent:get_callbacks(Agent).
beamai_agent:set_callbacks(Agent, Callbacks).

%% 触发自定义事件
-spec emit_custom_event(pid(), atom(), map()) -> ok.
beamai_agent:emit_custom_event(Agent, EventName, Data).
```

### 配置选项

```erlang
Config = #{
    system_prompt => binary(),           %% 系统提示词
    tools => [tool_def()],               %% 工具列表
    llm => llm_config(),                 %% LLM 配置
    max_iterations => integer(),         %% 最大迭代次数，默认 10
    storage => beamai_memory(),          %% 可选：存储实例
    callbacks => callback_map(),         %% 可选：回调函数
    middleware => [middleware_spec()]    %% 可选：中间件
}.
```

---

## Middleware 系统

Agent 执行过程中的拦截器机制。详细文档：[MIDDLEWARE.md](MIDDLEWARE.md)

### beamai_middleware 行为

```erlang
%% 所有回调都是可选的
-callback init(Opts :: map()) -> middleware_state().
-callback before_agent(State, MwState) -> middleware_result().
-callback after_agent(State, MwState) -> middleware_result().
-callback before_model(State, MwState) -> middleware_result().
-callback after_model(State, MwState) -> middleware_result().
-callback before_tools(State, MwState) -> middleware_result().
-callback after_tools(State, MwState) -> middleware_result().
```

### 返回值类型

```erlang
-type middleware_result() ::
    ok |                              %% 无修改
    {update, map()} |                 %% 更新图状态
    {goto, model | tools | '__end__'} |  %% 跳转
    {update_goto, map(), goto_target()} |  %% 更新并跳转
    {halt, term()} |                  %% 中止执行
    {interrupt, interrupt_action()}.  %% 中断等待确认
```

### beamai_middleware_runner

```erlang
%% 初始化 Middleware 链
-spec init([middleware_spec()]) -> middleware_chain().
beamai_middleware_runner:init(Specs).

%% Middleware 规格格式
Specs = [
    {middleware_module, Opts},           %% 模块 + 选项
    {middleware_module, Opts, Priority}, %% 模块 + 选项 + 优先级
    middleware_module                    %% 仅模块名
].

%% 执行钩子
-spec run_hook(hook_name(), graph_state(), middleware_chain()) -> run_result().
beamai_middleware_runner:run_hook(HookName, State, Middlewares).

%% 获取 Middleware 状态
-spec get_middleware_state(module(), middleware_chain()) -> {ok, state()} | {error, not_found}.
beamai_middleware_runner:get_middleware_state(Module, Chain).
```

### beamai_middleware_presets

```erlang
%% 预设配置
-spec default() -> [middleware_spec()].
-spec minimal() -> [middleware_spec()].
-spec production() -> [middleware_spec()].
-spec development() -> [middleware_spec()].
-spec human_in_loop() -> [middleware_spec()].

%% 带选项的预设
-spec default(map()) -> [middleware_spec()].
beamai_middleware_presets:default(#{
    call_limit => #{max_model_calls => 30},
    summarization => #{window_size => 25}
}).

%% 单独 Middleware 配置
-spec call_limit(map()) -> middleware_spec().
-spec summarization(map()) -> middleware_spec().
-spec human_approval(map()) -> middleware_spec().
-spec tool_retry(map()) -> middleware_spec().
```

### 内置 Middleware

| Middleware | 模块 | 主要配置 |
|------------|------|----------|
| 调用限制 | `middleware_call_limit` | `max_model_calls`, `max_tool_calls`, `max_iterations` |
| 上下文摘要 | `middleware_summarization` | `window_size`, `max_tokens`, `summarize` |
| 人工审批 | `middleware_human_approval` | `mode`, `timeout`, `tools` |
| 工具重试 | `middleware_tool_retry` | `max_retries`, `backoff` |
| 模型重试 | `middleware_model_retry` | `max_retries`, `retryable_errors` |
| 模型降级 | `middleware_model_fallback` | `fallback_models`, `trigger_errors` |
| PII 检测 | `middleware_pii_detection` | `action`, `types` |
| 工具选择 | `middleware_tool_selector` | `strategy`, `whitelist` |

### 自定义 Middleware 示例

```erlang
-module(my_logging_middleware).
-behaviour(beamai_middleware).

-export([init/1, before_model/2, after_model/2]).

init(Opts) ->
    #{log_level => maps:get(log_level, Opts, info)}.

before_model(State, #{log_level := Level}) ->
    Messages = graph_state:get(State, messages, []),
    log(Level, "LLM Request: ~p messages", [length(Messages)]),
    {update, #{request_start => erlang:system_time(millisecond)}}.

after_model(State, #{log_level := Level}) ->
    Start = graph_state:get(State, request_start, 0),
    Duration = erlang:system_time(millisecond) - Start,
    log(Level, "LLM Response: ~pms", [Duration]),
    ok.

log(info, Fmt, Args) -> logger:info(Fmt, Args);
log(debug, Fmt, Args) -> logger:debug(Fmt, Args).
```

---

## beamai_deepagent - Deep Agent

支持规划和并行执行的深度 Agent。

### 创建和执行

```erlang
%% 创建配置
-spec new() -> config().
-spec new(map()) -> config().
beamai_deepagent:new().
beamai_deepagent:new(Opts).

%% 运行 Agent
-spec run(config(), binary()) -> {ok, result()} | {error, term()}.
beamai_deepagent:run(Config, Task).
```

### 结果查询

```erlang
%% 获取计划
-spec get_plan(result()) -> plan() | undefined.
beamai_deepagent:get_plan(Result).

%% 获取执行轨迹
-spec get_trace(result()) -> trace().
beamai_deepagent:get_trace(Result).
```

### 配置选项

```erlang
Config = #{
    llm => llm_config(),                 %% LLM 配置
    tools => [tool_def()],               %% 自定义工具
    system_prompt => binary(),           %% 系统提示词
    max_depth => integer(),              %% 最大递归深度，默认 3
    max_iterations => integer(),         %% 最大迭代次数，默认 50
    planning_enabled => boolean(),       %% 启用规划，默认 true
    planning_mode => full | simple,      %% 规划模式，默认 full
    reflection_enabled => boolean(),     %% 启用反思，默认 true
    filesystem_enabled => boolean(),     %% 启用文件系统工具
    filesystem => filesystem_config(),   %% 文件系统配置
    human_in_loop => #{enabled => boolean()}  %% Human-in-loop 配置
}.
```

### 工具提供者

DeepAgent 通过 `beamai_deepagent_tool_provider` 提供工具，实现 `beamai_tool_provider` 行为。

```erlang
%% 通过 beamai_tool_registry 获取工具
Config = #{depth => 0, planning_mode => full},
Tools = beamai_tool_registry:from_config(#{
    providers => [{beamai_deepagent_tool_provider, Config}]
}).

%% 直接访问工具集合
beamai_deepagent_tool_provider:base_tools().       %% 基础工具
beamai_deepagent_tool_provider:plan_tools().       %% 计划工具
beamai_deepagent_tool_provider:subtask_tools().    %% 子任务工具
beamai_deepagent_tool_provider:reflect_tools().    %% 反思工具
beamai_deepagent_tool_provider:filesystem_tools(). %% 文件系统工具
beamai_deepagent_tool_provider:todo_tools().       %% TodoList 工具
beamai_deepagent_tool_provider:human_tools().      %% Human 交互工具

%% Provider 接口
beamai_deepagent_tool_provider:info().             %% 获取 Provider 信息
beamai_deepagent_tool_provider:available().        %% 检查是否可用
beamai_deepagent_tool_provider:list_tools(Opts).   %% 获取工具列表
beamai_deepagent_tool_provider:find_tool(Name, Opts). %% 查找工具
```

### 工具条件判断

| 工具集 | 条件 |
|--------|------|
| 基础工具 | 始终可用 |
| 计划工具 | `planning_mode=full` 且 `depth=0` |
| TodoList 工具 | `planning_mode=simple` |
| 子任务工具 | `depth < max_depth` |
| 反思工具 | `reflection_enabled=true` |
| 文件系统工具 | `filesystem_enabled=true` 或有 `filesystem` 配置 |
| Human 工具 | `human_in_loop.enabled=true` |

---

## beamai_llm - LLM 客户端

多 Provider 支持的 LLM 客户端。

### 配置和聊天

```erlang
%% 创建配置
-spec config(provider(), map()) -> llm_config().
llm_client:config(Provider, Opts).

%% 聊天
-spec chat(llm_config(), [message()]) -> {ok, response()} | {error, term()}.
llm_client:chat(Config, Messages).

%% 流式聊天
-spec stream_chat(llm_config(), [message()], callback()) -> {ok, response()} | {error, term()}.
llm_client:stream_chat(Config, Messages, Callback).

%% 带工具的聊天
-spec with_tools(llm_config(), [message()], [tool()]) -> {ok, response()} | {error, term()}.
llm_client:with_tools(Config, Messages, Tools).
```

### Provider 管理

```erlang
%% 列出 Provider
-spec list_providers() -> [atom()].
llm_client:list_providers().

%% Provider 信息
-spec provider_info(atom()) -> map().
llm_client:provider_info(Provider).
```

### 支持的 Provider

| Provider | 模块 | 特性 |
|----------|------|------|
| `openai` | llm_provider_openai | 聊天、流式、工具调用 |
| `anthropic` | llm_provider_anthropic | 聊天、流式、工具调用 |
| `zhipu` | llm_provider_zhipu | 聊天、流式、工具调用、异步 |
| `ollama` | llm_provider_ollama | 聊天、流式 |

### LLM 配置

```erlang
LLMConfig = #{
    provider => atom(),                  %% openai | anthropic | zhipu | ollama
    model => binary(),                   %% 模型名称
    api_key => binary(),                 %% API Key
    base_url => binary(),                %% 可选：自定义 URL
    timeout => integer(),                %% 可选：超时时间
    max_tokens => integer(),             %% 可选：最大 token
    temperature => float()               %% 可选：温度参数
}.
```

---

## beamai_memory - 记忆管理

统一的记忆和检查点管理系统。

### 创建和配置

```erlang
%% 创建 Memory 实例
-spec new(map()) -> {ok, memory()} | {error, term()}.
beamai_memory:new(Config).

Config = #{
    checkpointer => #{backend => ets | sqlite},
    store => #{backend => ets | sqlite},
    context_store => {module(), term()}
}.
```

### Checkpoint 操作

```erlang
%% 保存检查点
-spec save_checkpoint(memory(), config(), state_data()) -> {ok, memory()}.
beamai_memory:save_checkpoint(Memory, Config, StateData).

%% 加载检查点
-spec load_checkpoint(memory(), config()) -> {ok, state_data()} | {error, not_found}.
-spec load_latest_checkpoint(memory(), config()) -> {ok, state_data()} | {error, not_found}.
beamai_memory:load_checkpoint(Memory, Config).
beamai_memory:load_latest_checkpoint(Memory, Config).

%% 列出检查点
-spec list_checkpoints(memory(), config()) -> {ok, [checkpoint_info()]}.
beamai_memory:list_checkpoints(Memory, Config).

%% 检查点计数
-spec checkpoint_count(memory(), config()) -> non_neg_integer().
beamai_memory:checkpoint_count(Memory, Config).
```

### Store 操作

```erlang
%% 存储数据
-spec put(memory(), namespace(), key(), value()) -> {ok, memory()}.
beamai_memory:put(Memory, Namespace, Key, Value).

%% 搜索数据
-spec search(memory(), namespace(), filter()) -> {ok, [item()]}.
beamai_memory:search(Memory, Namespace, Filter).
```

---

## beamai_tools - 工具库

统一的工具定义和管理。

### 工具获取

```erlang
%% 获取工具
-spec get_tools(category() | [category()]) -> [tool_def()].
-spec get_tools(category(), map()) -> [tool_def()].
beamai_tools:get_tools(Categories).
beamai_tools:get_tools(Categories, Opts).

%% 获取所有工具
-spec get_all_tools() -> [tool_def()].
beamai_tools:get_all_tools().

%% 查找工具
-spec find_tool(binary()) -> {ok, tool_def()} | {error, not_found}.
beamai_tools:find_tool(Name).
```

### 工具执行

```erlang
%% 执行工具
-spec execute(binary(), map()) -> {ok, term()} | {error, term()}.
-spec execute(binary(), map(), map()) -> {ok, term()} | {error, term()}.
beamai_tools:execute(ToolName, Args).
beamai_tools:execute(ToolName, Args, Opts).
```

### 工具转换

```erlang
%% 转换为 LLM 格式
-spec to_llm_spec(tool_def()) -> map().
-spec to_llm_specs([tool_def()]) -> [map()].
beamai_tools:to_llm_spec(Tool).
beamai_tools:to_llm_specs(Tools).
```

### 工具注册表

```erlang
%% 构建工具列表
Registry = beamai_tool_registry:new(),
R1 = beamai_tool_registry:add_tools(Registry, Tools),
R2 = beamai_tool_registry:add_provider(R1, Provider),
Tools = beamai_tool_registry:build(R2).

%% 便捷函数
Tools = beamai_tool_registry:from_config(#{
    tools => [Tool1, Tool2],
    providers => [Provider1, Provider2]
}).
```

---

## beamai_core - 核心模块

### Graph 执行引擎

```erlang
%% 构建 Graph
Graph = graph_builder:new()
    |> graph_builder:add_node(NodeName, {Module, Opts})
    |> graph_builder:add_edge(From, To, Condition)
    |> graph_builder:set_entry(EntryNode)
    |> graph_builder:build().

%% 执行 Graph
-spec run(graph(), state()) -> {ok, state()} | {error, term()}.
graph_runner:run(Graph, InitialState).

%% Graph DSL
Graph = graph_dsl:compile(#{
    nodes => #{...},
    edges => [...],
    entry => atom()
}).
```

### Graph State

```erlang
%% 创建状态
-spec new(map()) -> state().
graph_state:new(Data).

%% 读写状态
-spec get(state(), key()) -> value().
-spec set(state(), key(), value()) -> state().
graph_state:get(State, Key).
graph_state:set(State, Key, Value).
```

### Pregel 分布式计算

```erlang
%% 创建 Pregel 图
{ok, Graph} = pregel_graph:new(Config).

%% 添加顶点和边
pregel_graph:add_vertex(Graph, VertexId, Data).
pregel_graph:add_edge(Graph, From, To, Weight).

%% 运行计算
{ok, Result} = pregel:run(Graph, ComputeFn, MaxIterations).
```

---

## beamai_a2a - A2A 协议

Agent-to-Agent 通信协议实现。

### 服务端

```erlang
%% 启动服务器
-spec start_link(map()) -> {ok, pid()}.
beamai_a2a_server:start_link(Config).

Config = #{
    handler => module(),                 %% 请求处理器
    port => integer(),                   %% HTTP 端口
    auth => auth_config()                %% 认证配置
}.
```

### 客户端

```erlang
%% 发现 Agent
-spec discover(binary()) -> {ok, agent_card()} | {error, term()}.
beamai_a2a_client:discover(AgentUrl).

%% 发送消息
-spec send_message(binary(), message()) -> {ok, response()} | {error, term()}.
beamai_a2a_client:send_message(AgentUrl, Message).
```

### Agent Card

```erlang
%% 创建 Card
-spec new(map()) -> agent_card().
beamai_a2a_card:new(#{
    name => binary(),
    description => binary(),
    url => binary(),
    capabilities => [binary()]
}).

%% 验证 Card
-spec validate(agent_card()) -> ok | {error, term()}.
beamai_a2a_card:validate(Card).
```

---

## beamai_mcp - MCP 协议

Model Context Protocol 实现。

### 客户端

```erlang
%% 连接 MCP 服务器
-spec connect(config()) -> {ok, client()} | {error, term()}.
beamai_mcp_client:connect(Config).

Config = #{
    transport => stdio | http | sse,
    command => binary(),                 %% stdio: 命令
    url => binary()                      %% http/sse: URL
}.

%% 列出工具
-spec list_tools(client()) -> {ok, [tool()]} | {error, term()}.
beamai_mcp_client:list_tools(Client).

%% 调用工具
-spec call_tool(client(), binary(), map()) -> {ok, result()} | {error, term()}.
beamai_mcp_client:call_tool(Client, ToolName, Args).

%% 列出资源
-spec list_resources(client()) -> {ok, [resource()]} | {error, term()}.
beamai_mcp_client:list_resources(Client).
```

### 服务端

```erlang
%% 启动服务器
-spec start_link(config()) -> {ok, pid()}.
beamai_mcp_server:start_link(Config).

%% 注册工具
-spec register_tool(pid(), tool_def()) -> ok.
beamai_mcp_server:register_tool(Server, Tool).

%% 注册资源
-spec register_resource(pid(), resource_def()) -> ok.
beamai_mcp_server:register_resource(Server, Resource).
```

---

## beamai_rag - RAG 功能

检索增强生成模块。

### 向量嵌入

```erlang
%% 生成嵌入
-spec embed(binary(), config()) -> {ok, [float()]} | {error, term()}.
-spec embed_batch([binary()], config()) -> {ok, [[float()]]} | {error, term()}.
beamai_embeddings:embed(Text, Config).
beamai_embeddings:embed_batch(Texts, Config).
```

### 向量存储

```erlang
%% 创建存储
-spec new(config()) -> {ok, store()}.
beamai_vector_store:new(Config).

%% 添加向量
-spec add(store(), binary(), [float()], map()) -> {ok, store()}.
beamai_vector_store:add(Store, Id, Vector, Metadata).

%% 相似度搜索
-spec search(store(), [float()], integer()) -> {ok, [result()]}.
beamai_vector_store:search(Store, QueryVector, TopK).
```

### RAG 流程

```erlang
%% 初始化
-spec init(config()) -> {ok, rag_state()}.
beamai_rag:init(Config).

%% 添加文档
-spec add_documents(rag_state(), [document()]) -> {ok, rag_state()}.
beamai_rag:add_documents(State, Documents).

%% 检索
-spec retrieve(rag_state(), binary(), integer()) -> {ok, [chunk()]}.
beamai_rag:retrieve(State, Query, TopK).

%% RAG 查询（检索 + 生成）
-spec query(rag_state(), binary()) -> {ok, response()}.
beamai_rag:query(State, Question).
```

---

## 通用类型

### 工具定义

```erlang
-type tool_def() :: #{
    name := binary(),
    description := binary(),
    parameters := json_schema(),
    handler := fun((map()) -> {ok, term()} | {error, term()})
                | fun((map(), map()) -> {ok, term()} | {error, term()})
}.
```

### 消息类型

```erlang
-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    tool_call_id => binary()
}.
```

### LLM 响应

```erlang
-type llm_response() :: #{
    id := binary(),
    model := binary(),
    content := binary() | null,
    tool_calls := [tool_call()],
    finish_reason := binary(),
    usage => usage_info()
}.
```

---

## 错误处理

所有 API 返回 `{ok, Result}` 或 `{error, Reason}` 格式。常见错误类型：

| 错误 | 说明 |
|------|------|
| `{error, missing_api_key}` | API Key 未配置 |
| `{error, timeout}` | 请求超时 |
| `{error, {http_error, Code, Body}}` | HTTP 错误 |
| `{error, {api_error, Details}}` | API 返回错误 |
| `{error, not_found}` | 资源未找到 |
| `{error, storage_not_enabled}` | 存储未启用 |

---

## 更多文档

- [README.md](../README.md) - 项目概述
- [ARCHITECTURE.md](ARCHITECTURE.md) - 架构设计
- [QUICK_START.md](QUICK_START.md) - 快速开始指南
- 各模块 README：
  - [beamai_core](../apps/beamai_core/README.md)
  - [beamai_llm](../apps/beamai_llm/README.md)
  - [beamai_agent](../apps/beamai_agent/README.md)
  - [beamai_deepagent](../apps/beamai_deepagent/README.md)
  - [beamai_memory](../apps/beamai_memory/README.md)
  - [beamai_tools](../apps/beamai_tools/README.md)
  - [beamai_a2a](../apps/beamai_a2a/README.md)
  - [beamai_mcp](../apps/beamai_mcp/README.md)
  - [beamai_rag](../apps/beamai_rag/README.md)
