# API 参考文档

[English](API_REFERENCE_EN.md) | 中文

本文档提供 BeamAI Framework 核心模块的主要 API 参考。

## 目录

- [beamai_core - 核心模块](#beamai_core---核心模块)
  - [HTTP 客户端](#http-客户端)
  - [HTTP 后端配置](#http-后端配置)
- [beamai_llm - LLM 客户端](#beamai_llm---llm-客户端)
- [beamai_memory - 记忆管理](#beamai_memory---记忆管理)

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

### Process 分支与时间旅行 API

Process 框架通过可插拔的 Store 后端提供分支和时间旅行能力。
所有操作使用 `{Module, Ref}` 动态分发模式，通过 `beamai_process_store_behaviour` 可选回调实现。

```erlang
-type store() :: {module(), term()}.

%% 分支 API
-spec branch_from(store(), BranchName :: binary(), Opts :: map()) ->
    {ok, #{branch_thread_id := binary(), snapshot_id := binary()}} | {error, term()}.
-spec restore_branch(store(), BranchThreadId :: binary(),
                     ProcessSpec, Opts :: map()) -> {ok, pid()} | {error, term()}.
-spec list_branches(store(), Opts :: map()) -> {ok, [map()]} | {error, term()}.
-spec get_lineage(store(), Opts :: map()) -> {ok, [map()]} | {error, term()}.

%% 时间旅行 API
-spec go_back(store(), Steps :: pos_integer(), ProcessSpec) ->
    {ok, pid()} | {error, term()}.
-spec go_back(store(), Steps :: pos_integer(), ProcessSpec, Opts :: map()) ->
    {ok, pid()} | {error, term()}.
-spec go_forward(store(), Steps :: pos_integer(), ProcessSpec) ->
    {ok, pid()} | {error, term()}.
-spec go_forward(store(), Steps :: pos_integer(), ProcessSpec, Opts :: map()) ->
    {ok, pid()} | {error, term()}.
-spec goto_snapshot(store(), SnapshotId :: binary(), ProcessSpec) ->
    {ok, pid()} | {error, term()}.
-spec goto_snapshot(store(), SnapshotId :: binary(), ProcessSpec, Opts :: map()) ->
    {ok, pid()} | {error, term()}.
-spec list_history(store()) -> {ok, [map()]} | {error, term()}.
```

**使用示例：**

```erlang
%% 创建 Store 引用
Store = {beamai_process_memory_store, {Mgr, #{thread_id => ThreadId}}},

%% 创建分支
{ok, #{branch_thread_id := BranchId}} =
    beamai_process:branch_from(Store, <<"experiment">>, #{}),

%% 时间旅行：回退 2 步
{ok, Pid} = beamai_process:go_back(Store, 2, ProcessSpec),

%% 列出执行历史
{ok, History} = beamai_process:list_history(Store).
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
  - [beamai_memory](../apps/beamai_memory/README.md)
