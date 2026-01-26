# API 参考文档

[English](API_REFERENCE_EN.md) | 中文

本文档提供 BeamAI Framework 各模块的主要 API 参考。

## 目录

- [beamai_agent - Simple Agent](#beamai_agent---simple-agent)
- [beamai_coordinator - 多 Agent 协调器](#beamai_coordinator---多-agent-协调器)
- [Middleware 系统](#middleware-系统)
- [beamai_deepagent - Deep Agent](#beamai_deepagent---deep-agent)
- [beamai_llm - LLM 客户端](#beamai_llm---llm-客户端)
- [beamai_memory - 记忆管理](#beamai_memory---记忆管理)
- [beamai_tools - 工具库](#beamai_tools---工具库)
- [beamai_core - 核心模块](#beamai_core---核心模块)
  - [HTTP 客户端](#http-客户端)
  - [HTTP 后端配置](#http-后端配置)
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

### Context API（用户自定义上下文）

Context 用于存储用户自定义数据，会参与对话和检查点持久化。

```erlang
%% 获取完整 Context
-spec get_context(pid()) -> map().
beamai_agent:get_context(Agent).

%% 获取 Context 值
-spec get_context(pid(), atom() | binary()) -> term() | undefined.
-spec get_context(pid(), atom() | binary(), term()) -> term().
beamai_agent:get_context(Agent, Key).
beamai_agent:get_context(Agent, Key, Default).

%% 设置 Context
-spec set_context(pid(), map()) -> ok.
-spec update_context(pid(), map()) -> ok.
-spec put_context(pid(), atom() | binary(), term()) -> ok.
beamai_agent:set_context(Agent, NewContext).
beamai_agent:update_context(Agent, Updates).
beamai_agent:put_context(Agent, Key, Value).
```

### Meta API（进程级元数据）

Meta 用于存储进程级元数据，**不参与对话**，适用于存储协调器信息等运行时数据。

```erlang
%% 获取完整 Meta
-spec get_meta(pid()) -> map().
beamai_agent:get_meta(Agent).

%% 获取 Meta 值
-spec get_meta(pid(), atom() | binary()) -> term() | undefined.
-spec get_meta(pid(), atom() | binary(), term()) -> term().
beamai_agent:get_meta(Agent, Key).
beamai_agent:get_meta(Agent, Key, Default).

%% 设置 Meta
-spec set_meta(pid(), map()) -> ok.
-spec put_meta(pid(), atom() | binary(), term()) -> ok.
beamai_agent:set_meta(Agent, NewMeta).
beamai_agent:put_meta(Agent, Key, Value).
```

**Context vs Meta 对比：**

| 特性 | Context | Meta |
|------|---------|------|
| 参与对话 | 是 | 否 |
| 检查点持久化 | 是 | 否 |
| 典型用途 | 用户数据、对话状态 | 协调器信息、运行时配置 |

### 配置选项

```erlang
Config = #{
    system_prompt => binary(),           %% 系统提示词
    tools => [tool_def()],               %% 工具列表
    llm => llm_config(),                 %% LLM 配置
    max_iterations => integer(),         %% 最大迭代次数，默认 10
    storage => beamai_memory(),          %% 可选：存储实例
    callbacks => callback_map(),         %% 可选：回调函数
    middleware => [middleware_spec()],   %% 可选：中间件
    context => map(),                    %% 可选：用户上下文初始值
    context_reducers => field_reducers() %% 可选：上下文字段 Reducer 配置
}.
```

### Context Reducers 配置

Context Reducers 允许为用户上下文的字段配置自定义合并策略。

```erlang
%% Reducer 类型
-type field_reducer() ::
    fun((Old :: term(), New :: term()) -> Merged :: term())  %% 普通 reducer
    | {transform, TargetKey :: binary(), ReducerFun :: function()}.  %% 转换型 reducer

%% 配置示例
context_reducers => #{
    %% 普通 reducer：items 字段使用追加策略
    <<"items">> => fun graph_state_reducer:append_reducer/2,

    %% 转换型 reducer：counter_incr 累加到 counter，counter_incr 不保留
    <<"counter_incr">> => {transform, <<"counter">>, fun graph_state_reducer:increment_reducer/2}
}.
```

**内置 Reducer：**

| Reducer | 行为 |
|---------|------|
| `append_reducer` | 列表追加 |
| `merge_reducer` | Map 深度合并 |
| `increment_reducer` | 数值累加 |
| `last_write_win_reducer` | 新值覆盖旧值（默认） |

---

## beamai_coordinator - 多 Agent 协调器

协调器用于管理多个 Agent 协同工作，支持 Pipeline（顺序执行）和 Orchestrator（编排执行）两种模式。

**重要变更（v2.1）：** 协调器 API 参数从使用 `Id` 改为使用 `CoordinatorPid`。协调器元数据存储在 Agent 的 `meta` 字段中，解决了原 ETS 表的进程所有权问题。

### 启动协调器

```erlang
%% 通用启动接口
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_link(Id, Opts).

%% Pipeline 模式：任务在 workers 间顺序传递
-spec start_pipeline(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_pipeline(Id, Opts).

%% Orchestrator 模式：协调器编排多个 workers
-spec start_orchestrator(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_orchestrator(Id, Opts).
```

### 配置选项

```erlang
Opts = #{
    agents => [agent_def()],           %% Worker Agent 定义列表
    llm => llm_config(),               %% LLM 配置
    system_prompt => binary(),         %% 可选：协调器系统提示词
    max_iterations => integer()        %% 可选：最大迭代次数，默认 10
}.

%% Agent 定义
agent_def() = #{
    name := binary(),                  %% Worker 名称（必需）
    system_prompt := binary()          %% Worker 系统提示词（必需）
}.
```

### 停止协调器

```erlang
%% 停止协调器及其所有 workers
-spec stop(pid()) -> ok | {error, term()}.
beamai_coordinator:stop(CoordinatorPid).
```

### Workers 管理

```erlang
%% 获取所有 workers
-spec get_workers(pid()) -> {ok, #{binary() => pid()}} | {error, term()}.
beamai_coordinator:get_workers(CoordinatorPid).

%% 获取指定 worker
-spec get_worker(pid(), binary()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:get_worker(CoordinatorPid, WorkerName).
```

### 任务委托

```erlang
%% 委托任务给指定 worker
-spec delegate(pid(), binary(), binary()) -> {ok, binary()} | {error, term()}.
beamai_coordinator:delegate(CoordinatorPid, WorkerName, Task).

%% 并行委托任务给多个 workers
-spec delegate_parallel(pid(), [binary()], binary()) -> {ok, map()} | {error, term()}.
beamai_coordinator:delegate_parallel(CoordinatorPid, WorkerNames, Task).
%% 返回: {ok, #{WorkerName => {ok, Result} | {error, Reason}}}
```

### 使用示例

```erlang
%% Pipeline 示例：翻译流水线
LLM = llm_client:create(bailian, #{model => <<"qwen-plus">>, api_key => ApiKey}),

{ok, Pipeline} = beamai_coordinator:start_pipeline(<<"translator">>, #{
    agents => [
        #{name => <<"cn_to_en">>, system_prompt => <<"翻译成英文">>},
        #{name => <<"polisher">>, system_prompt => <<"润色英文">>}
    ],
    llm => LLM
}),

%% 直接调用某个 worker
{ok, Result} = beamai_coordinator:delegate(Pipeline, <<"cn_to_en">>, <<"你好世界">>),

%% 停止
beamai_coordinator:stop(Pipeline).
```

```erlang
%% Orchestrator 示例：多专家咨询
{ok, Panel} = beamai_coordinator:start_orchestrator(<<"experts">>, #{
    agents => [
        #{name => <<"tech">>, system_prompt => <<"技术专家">>},
        #{name => <<"biz">>, system_prompt => <<"商业专家">>}
    ],
    llm => LLM
}),

%% 并行咨询多个专家
{ok, Results} = beamai_coordinator:delegate_parallel(
    Panel, [<<"tech">>, <<"biz">>], <<"分析 AI 趋势">>
),

beamai_coordinator:stop(Panel).
```

---

## Middleware 系统

Agent 执行过程中的拦截器机制。Middleware 系统位于 `beamai_tools` 模块中，被 `beamai_agent` 和 `beamai_deepagent` 共享使用。

详细文档：[MIDDLEWARE.md](MIDDLEWARE.md)

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

### LLM 配置管理

LLM 配置必须使用 `llm_client:create/2` 创建，实现配置与 Agent 分离：

```erlang
%% 创建 LLM 配置（必须使用 llm_client:create/2）
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    temperature => 0.7
}).

%% 配置复用：多个 Agent 共享同一配置
{ok, Agent1} = beamai_agent:start_link(<<"agent1">>, #{llm => LLM, ...}),
{ok, Agent2} = beamai_agent:start_link(<<"agent2">>, #{llm => LLM, ...}).

%% 配置合并：基于现有配置创建新配置
HighTempLLM = llm_client:merge_config(LLM, #{temperature => 0.9}).

%% 验证配置有效性
true = llm_client:is_valid_config(LLM).
```

**优势：**
- 配置复用：多个 Agent 共享同一 LLM 配置
- 集中管理：API Key、模型参数统一配置
- 类型安全：Agent 启动时验证配置有效性
- 易于测试：可独立验证 LLM 配置

### 配置和聊天

```erlang
%% 创建配置
-spec create(provider(), map()) -> llm_config().
llm_client:create(Provider, Opts).

%% 验证配置
-spec is_valid_config(term()) -> boolean().
llm_client:is_valid_config(Config).

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

| Provider | 模块 | API 模式 | 特性 |
|----------|------|----------|------|
| `openai` | llm_provider_openai | OpenAI | 聊天、流式、工具调用 |
| `anthropic` | llm_provider_anthropic | Anthropic | 聊天、流式、工具调用 |
| `deepseek` | llm_provider_deepseek | OpenAI 兼容 | 聊天、流式、工具调用 |
| `zhipu` | llm_provider_zhipu | OpenAI 兼容 | 聊天、流式、工具调用、异步 |
| `bailian` | llm_provider_bailian | DashScope 原生 | 聊天、流式、工具调用、联网搜索 |
| `ollama` | llm_provider_ollama | OpenAI 兼容 | 聊天、流式 |

### Provider 公共模块 (llm_provider_common)

所有 Provider 共享的通用函数：

```erlang
%% URL 构建
llm_provider_common:build_url(Config, DefaultEndpoint, DefaultBaseUrl) -> binary().

%% Bearer 认证头构建
llm_provider_common:build_bearer_auth_headers(Config) -> [{binary(), binary()}].

%% 可选参数添加
llm_provider_common:maybe_add_stream(Body, Request) -> map().
llm_provider_common:maybe_add_tools(Body, Request) -> map().
llm_provider_common:maybe_add_top_p(Body, Request) -> map().

%% OpenAI 格式流式事件累加
llm_provider_common:accumulate_openai_event(Event, Acc) -> map().

%% 工具调用解析
llm_provider_common:parse_tool_calls(Message) -> [map()].
llm_provider_common:parse_single_tool_call(Call) -> map().

%% 使用统计解析
llm_provider_common:parse_usage(Usage) -> #{prompt_tokens, completion_tokens, total_tokens}.
```

### DeepSeek 详细说明

DeepSeek Provider 使用 OpenAI 兼容 API，支持 `deepseek-chat` 和 `deepseek-reasoner` 模型。

**支持的模型：**
- `deepseek-chat`：通用对话模型（默认）
- `deepseek-reasoner`：推理增强模型

**配置示例：**
```erlang
LLM = llm_client:create(deepseek, #{
    model => <<"deepseek-chat">>,
    api_key => list_to_binary(os:getenv("DEEPSEEK_API_KEY")),
    max_tokens => 4096,
    temperature => 1.0
}).
```

### 阿里云百炼 (DashScope) 详细说明

百炼 Provider 使用 DashScope 原生 API，自动根据模型类型选择端点：
- **文本生成模型** (`qwen-plus`, `qwen-max`, `qwen-turbo`)：使用 `/api/v1/services/aigc/text-generation/generation`
- **多模态模型** (`qwen-vl-plus`, `qwen-audio` 等)：使用 `/api/v1/services/aigc/multimodal-generation/generation`

**特有参数：**
- `enable_search => true`：启用联网搜索功能
- `tool_choice => <<"required">>`：强制工具调用

**流式输出：**
- 请求头：`X-DashScope-SSE: enable`
- 参数：`parameters.incremental_output: true`

### LLM 配置参数

`llm_client:create/2` 支持以下参数：

```erlang
LLM = llm_client:create(Provider, #{
    model => binary(),                   %% 模型名称（必需）
    api_key => binary(),                 %% API Key（必需，ollama 除外）
    base_url => binary(),                %% 可选：自定义 URL
    timeout => integer(),                %% 可选：超时时间（毫秒）
    max_tokens => integer(),             %% 可选：最大 token
    temperature => float()               %% 可选：温度参数 (0.0 - 2.0)
}).
```

**Provider 类型：** `openai | anthropic | deepseek | zhipu | bailian | ollama`

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

## beamai_tools - 工具库和中间件系统

统一的工具定义和管理，以及 Agent 执行中间件系统。

beamai_tools 包含两大核心功能：
- **工具系统**：工具定义、注册、Provider 机制
- **中间件系统**：Agent 执行拦截、增强和控制（详见 [Middleware 系统](#middleware-系统)）

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

### Kernel 和 Tool 系统

Kernel 是 BeamAI 的核心抽象，管理 Tool 的注册与调用。

```erlang
%% 创建 Kernel
beamai_kernel:new() -> kernel().
beamai_kernel:new(Opts) -> kernel().

%% 添加工具
beamai_kernel:add_tool(Kernel, ToolSpec) -> kernel().
beamai_kernel:add_tools(Kernel, [ToolSpec]) -> kernel().
beamai_kernel:add_tool_module(Kernel, Module) -> kernel().

%% 添加服务和过滤器
beamai_kernel:add_service(Kernel, Service) -> kernel().
beamai_kernel:add_filter(Kernel, Filter) -> kernel().

%% 调用工具
beamai_kernel:invoke(Kernel, ToolName, Args, Context) -> {ok, Result, NewContext} | {error, Reason}.
beamai_kernel:invoke_chat(Kernel, Messages, Opts) -> {ok, Response} | {error, Reason}.
beamai_kernel:invoke_chat_with_tools(Kernel, Messages, Opts) -> {ok, Response} | {error, Reason}.

%% 查询工具
beamai_kernel:find_tool(Kernel, Name) -> {ok, ToolSpec} | error.
beamai_kernel:get_tool_specs(Kernel) -> [ToolSpec].
beamai_kernel:tools_by_tag(Kernel, Tag) -> [ToolSpec].
```

**Tool 定义示例：**

```erlang
%% 直接定义 Map
Tool = #{
    name => <<"get_weather">>,
    description => <<"获取城市天气"/utf8>>,
    tag => <<"weather">>,
    parameters => #{
        <<"city">> => #{type => string, required => true, description => <<"城市名称"/utf8>>}
    },
    handler => fun(#{<<"city">> := City}, _Ctx) ->
        {ok, #{city => City, temp => 25}}
    end
}.

%% 使用 beamai_tool:new
Tool = beamai_tool:new(<<"my_tool">>, fun handle/2, #{
    description => <<"工具描述"/utf8>>,
    parameters => #{...}
}).

%% 注册到 Kernel
Kernel1 = beamai_kernel:add_tool(Kernel, Tool).
```

**Tool Module（behaviour 模式）：**

```erlang
-module(my_tools).
-behaviour(beamai_tool_behaviour).
-export([tool_info/0, tools/0]).

tool_info() ->
    #{description => <<"我的工具集"/utf8>>, tags => [<<"custom">>]}.

tools() ->
    [
        #{name => <<"tool_a">>, handler => fun ?MODULE:handle_a/2, ...},
        #{name => <<"tool_b">>, handler => fun ?MODULE:handle_b/2, ...}
    ].

%% 加载到 Kernel
Kernel1 = beamai_kernel:add_tool_module(Kernel, my_tools).
```

**内置 Tool 模块：**

| 模块 | 描述 | 工具 |
|------|------|------|
| `beamai_tool_file` | 文件操作 | file_read, file_write, file_list, file_glob |
| `beamai_tool_shell` | Shell 命令 | shell_exec |
| `beamai_tool_todo` | 任务管理 | todo_add, todo_list, todo_update |
| `beamai_tool_human` | 人工交互 | human_input |

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

%% 用户上下文操作
-spec get_context(state()) -> map().
-spec get_context(state(), key()) -> value() | undefined.
-spec set_context(state(), map()) -> state().
-spec update_context(state(), map()) -> state().
graph_state:get_context(State).
graph_state:get_context(State, Key).
graph_state:set_context(State, Context).
graph_state:update_context(State, Updates).
```

### Graph State Reducer

字段级 Reducer，用于合并节点返回的 delta 到全局状态。

```erlang
%% 应用 delta
-spec apply_delta(state(), delta(), field_reducers()) -> state().
-spec apply_deltas(state(), [delta()], field_reducers()) -> state().
graph_state_reducer:apply_delta(State, Delta, FieldReducers).
graph_state_reducer:apply_deltas(State, Deltas, FieldReducers).

%% 内置 Reducer
graph_state_reducer:append_reducer(Old, New) -> list().
graph_state_reducer:merge_reducer(Old, New) -> map().
graph_state_reducer:increment_reducer(Old, Delta) -> number().
graph_state_reducer:last_write_win_reducer(Old, New) -> term().
```

**Reducer 配置格式：**

```erlang
FieldReducers = #{
    %% 普通 reducer
    <<"messages">> => fun graph_state_reducer:append_reducer/2,

    %% 转换型 reducer：从 counter_incr 累加到 counter
    <<"counter_incr">> => {transform, <<"counter">>, fun graph_state_reducer:increment_reducer/2}
}.
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

### HTTP 客户端

BeamAI 提供统一的 HTTP 客户端接口，支持 Gun 和 Hackney 两种后端。

```erlang
%% 发送请求（自动使用配置的后端）
-spec request(method(), url(), headers(), body(), opts()) -> {ok, response()} | {error, term()}.
beamai_http:request(Method, Url, Headers, Body, Opts).

%% 便捷函数
-spec get(url(), headers()) -> {ok, response()} | {error, term()}.
-spec post(url(), headers(), body()) -> {ok, response()} | {error, term()}.
beamai_http:get(Url, Headers).
beamai_http:post(Url, Headers, Body).

%% 流式请求（SSE）
-spec stream_request(url(), headers(), body(), callback(), opts()) -> {ok, term()} | {error, term()}.
beamai_http:stream_request(Url, Headers, Body, Callback, Opts).
```

### HTTP 后端配置

```erlang
%% 应用配置（sys.config）
{beamai_core, [
    %% 选择 HTTP 后端：beamai_http_gun（默认）或 beamai_http_hackney
    {http_backend, beamai_http_gun},

    %% Gun 连接池配置
    {http_pool, #{
        max_connections => 100,        %% 最大连接数
        connection_timeout => 30000,   %% 连接超时（毫秒）
        idle_timeout => 60000          %% 空闲超时（毫秒）
    }}
]}.
```

**后端对比：**

| 特性 | Gun（默认） | Hackney |
|------|-------------|---------|
| HTTP/2 | 支持 | 不支持 |
| 连接池 | beamai_http_pool | hackney 内置池 |
| TLS | 自动使用系统 CA 证书（OTP 25+） | hackney 默认配置 |
| 依赖 | gun 2.1.0 | hackney |
| 推荐场景 | 生产环境、需要 HTTP/2 | 兼容旧系统 |

### HTTP 连接池 (Gun 后端)

当使用 Gun 后端时，beamai_http_pool 会作为 beamai_core 应用的子进程自动启动。

```erlang
%% 连接池 API
-spec checkout(host(), port(), protocol()) -> {ok, connection()} | {error, term()}.
beamai_http_pool:checkout(Host, Port, Protocol).

-spec checkin(connection()) -> ok.
beamai_http_pool:checkin(Conn).

%% 查看连接池状态
-spec get_stats() -> map().
beamai_http_pool:get_stats().
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

### 工具定义（tool_spec）

```erlang
-type tool_spec() :: #{
    name := binary(),                    % 必填：工具名称
    handler := handler(),                % 必填：处理器
    description => binary(),             % 可选：描述（供 LLM 理解）
    parameters => parameters_schema(),   % 可选：参数定义
    tag => binary() | [binary()],        % 可选：分类标签
    timeout => pos_integer(),            % 可选：超时时间（毫秒）
    retry => #{max => integer(), delay => integer()},  % 可选：重试策略
    metadata => map()                    % 可选：自定义元数据
}.

-type handler() ::
    fun((args()) -> tool_result())                        % fun/1：仅接收参数
    | fun((args(), beamai_context:t()) -> tool_result())  % fun/2：参数 + 上下文
    | {module(), atom()}                                  % {M, F}：模块函数
    | {module(), atom(), [term()]}.                       % {M, F, ExtraArgs}

-type tool_result() ::
    {ok, term()}                          % 成功，返回结果
    | {ok, term(), beamai_context:t()}    % 成功，返回结果和更新的上下文
    | {error, term()}.                    % 失败

-type parameters_schema() :: #{
    atom() | binary() => #{
        type := string | integer | float | boolean | array | object,
        description => binary(),
        required => boolean(),
        default => term(),
        enum => [term()],
        items => param_spec(),           % array 元素类型
        properties => parameters_schema() % object 嵌套属性
    }
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
