# BeamAI Core

[English](README_EN.md) | 中文

BeamAI 框架的核心模块，提供 Kernel 架构、Process Framework、HTTP 客户端和行为定义。

## 模块概览

### Kernel 子系统

基于 Semantic Kernel 理念的核心抽象，管理 Tool 的注册与调用：

- **beamai_kernel** - Kernel 核心，管理 Tool 注册和调用（无状态，不记录消息）
- **beamai_tool** - 工具定义，封装可调用的工具
- **beamai_tool_behaviour** - 工具模块行为接口
- **beamai_context** - 上下文：携带 agent 状态变量、会话标识、kernel 引用、trace（不记录消息/历史）
- **beamai_filter** / **beamai_filter_chain** - 洋葱式 filter（一个 filter 含 around_chat/around_tool 两个可选 around hook，带按名字隔离的私有上下文），包裹工具执行与 LLM 调用（详见 [docs/FILTER.md](../../docs/FILTER.md)）
- **beamai_prompt** - 提示词模板管理
- **beamai_result** - 工具调用结果类型

### 会话记忆子系统

会话历史的存储与注入，从 Kernel 中剥离，按 `conversation_id` 管理（详见 [docs/MEMORY.md](../../docs/MEMORY.md)）：

- **beamai_chat_memory** - ChatMemory 行为接口 + 调度 API（句柄 `{Module, Ref}`）
- **beamai_chat_memory_ets** - 默认 ETS 会话存储实现
- **beamai_chat_memory_window** - 滑动窗口包装（读取时按条数裁剪）
- **beamai_memory_filter** - Memory Filter（单个 filter：around_chat 前置存 delta+展开历史、后置存回复）

### LLM 子系统

LLM 响应的统一抽象层：

- **beamai_llm_response** - LLM 统一响应访问器，抽象不同 Provider 的响应差异

### Process Framework 子系统

可编排的流程引擎，支持步骤定义、条件分支、并行执行和时间旅行：

- **beamai_process** - 统一 Facade API（Builder + Runtime + Time Travel + Branch）
- **beamai_process_builder** - 流程构建器（Builder 模式）
- **beamai_process_engine** - 流程执行引擎
- **beamai_process_runtime** - 流程运行时
- **beamai_process_step** - 步骤定义
- **beamai_process_step_transform** - 步骤转换
- **beamai_process_executor** - 流程执行器
- **beamai_process_event** - 事件系统
- **beamai_process_state** - 流程状态管理
- **beamai_process_worker** - 流程工作进程
- **beamai_process_sup** - 流程监督树

### HTTP 子系统

可插拔的 HTTP 客户端，支持 Gun 和 Hackney 后端：

- **beamai_http** - HTTP 客户端统一接口
- **beamai_http_gun** - Gun HTTP/2 后端实现
- **beamai_http_hackney** - Hackney HTTP/1.1 后端实现
- **beamai_http_pool** - HTTP 连接池管理

### Behaviour 定义

框架的行为接口定义：

- **beamai_chat_behaviour** - LLM 聊天接口（原 beamai_llm_behaviour）
- **beamai_http_behaviour** - HTTP 后端行为接口
- **beamai_step_behaviour** - 流程步骤行为接口
- **beamai_process_store_behaviour** - 流程存储行为接口（含分支/时间旅行可选回调）
- **beamai_tool_behaviour** - 工具模块行为接口

### 工具与协议

- **beamai_id** - 唯一 ID 生成（UUID）
- **beamai_jsonrpc** - JSON-RPC 2.0 编解码
- **beamai_sse** - Server-Sent Events (SSE) 支持
- **beamai_utils** - 通用工具函数

### 应用入口

- **beamai** - 主入口模块
- **beamai_core_app** - OTP 应用回调
- **beamai_core_sup** - 顶级监督树

## API 文档

### beamai_kernel

```erlang
%% 创建 Kernel 实例
beamai_kernel:new() -> kernel().
beamai_kernel:new(Opts) -> kernel().

%% 添加 Tool
beamai_kernel:add_tool(Kernel, ToolSpec) -> kernel().
beamai_kernel:add_tools(Kernel, [ToolSpec]) -> kernel().
beamai_kernel:add_tool_module(Kernel, Module) -> kernel().

%% 添加服务、filter、会话记忆
beamai_kernel:add_service(Kernel, Service) -> kernel().
beamai_kernel:add_filter(Kernel, Filter) -> kernel().    %% 洋葱式 filter，详见 docs/FILTER.md
beamai_kernel:with_memory(Kernel, Store) -> kernel().    %% 启用会话记忆，详见 docs/MEMORY.md

%% 调用（kernel 只提供单次能力；ReAct 工具调用循环属于 Agent 层，见 beamai_agent）
%% invoke_chat/3：单次 Chat Completion（经 around_chat 链）。Messages 为本轮新消息；
%% 若 context 带 conversation_id 且挂了 Memory Filter，则按 id 存储并展开历史。
beamai_kernel:invoke_chat(Kernel, Messages, Opts) -> {ok, Response, Context} | {error, Reason}.
beamai_kernel:invoke_tool(Kernel, ToolName, Args, Context) -> {ok, Result, Context} | {error, Reason}.

%% 查询工具
beamai_kernel:get_tool(Kernel, Name) -> {ok, ToolSpec} | error.
beamai_kernel:get_tool_specs(Kernel) -> [ToolSpec].
beamai_kernel:get_tools_by_tag(Kernel, Tag) -> [ToolSpec].
```

### beamai_tool

```erlang
%% 创建工具
beamai_tool:new(Name, Handler) -> tool_spec().
beamai_tool:new(Name, Handler, Opts) -> tool_spec().

%% 或直接定义 Map
ToolSpec = #{
    name := binary(),                    % 必填：工具名称
    handler := handler(),                % 必填：处理器
    description => binary(),             % 可选：描述
    parameters => parameters_schema(),   % 可选：参数定义
    tag => binary() | [binary()]         % 可选：分类标签
}.

%% Handler 形式
%% fun/1：fun(Args) -> {ok, Result} | {error, Reason}
%% fun/2：fun(Args, Context) -> {ok, Result} | {ok, Result, NewContext} | {error, Reason}
%% {M, F}：模块函数
%% {M, F, ExtraArgs}：带额外参数

%% 转换为 LLM schema
beamai_tool:to_tool_schema(ToolSpec, openai | anthropic) -> map().
```

### beamai_process（统一 Facade API）

```erlang
%% Builder API
beamai_process:builder(Name) -> spec().
beamai_process:add_step(Spec, StepName, Module, Config) -> spec().
beamai_process:on_event(Spec, StepName, EventName, TargetStep) -> spec().
beamai_process:set_initial_event(Spec, StepName, Data) -> spec().
beamai_process:set_execution_mode(Spec, Mode) -> spec().
beamai_process:build(Spec) -> {ok, ProcessSpec} | {error, Reason}.

%% Runtime API
beamai_process:start(ProcessSpec) -> {ok, pid()} | {error, Reason}.
beamai_process:run_sync(ProcessSpec) -> {ok, Result} | {paused, Reason, Snapshot} | {error, Reason}.
beamai_process:run_sync(ProcessSpec, Opts) -> {ok, Result} | {paused, Reason, Snapshot} | {error, Reason}.
beamai_process:resume(Pid, Data) -> ok.
beamai_process:snapshot(Pid) -> {ok, Snapshot}.
beamai_process:restore(Snapshot) -> {ok, pid()}.

%% Time Travel & Branch API
beamai_process:go_back(Store, Steps, ProcessSpec) -> {ok, pid()} | {error, Reason}.
beamai_process:branch_from(Store, BranchName, Opts) -> {ok, Info} | {error, Reason}.
beamai_process:list_history(Store) -> {ok, [map()]} | {error, Reason}.
```

## 使用示例

### Kernel + Tool

```erlang
%% 创建 Kernel
Kernel = beamai_kernel:new(),

%% 定义工具
ReadFile = #{
    name => <<"read_file">>,
    description => <<"读取文件内容"/utf8>>,
    parameters => #{
        <<"path">> => #{
            type => string,
            required => true,
            description => <<"文件路径"/utf8>>
        }
    },
    handler => fun(#{<<"path">> := Path}, _Ctx) ->
        case file:read_file(Path) of
            {ok, Content} -> {ok, Content};
            {error, Reason} -> {error, Reason}
        end
    end
},

%% 注册到 Kernel
Kernel1 = beamai_kernel:add_tool(Kernel, ReadFile),

%% 调用单个工具
{ok, Content, _NewCtx} = beamai_kernel:invoke_tool(Kernel1, <<"read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}, beamai_context:new()).
```

### Process Framework

```erlang
%% 构建多步流程
Spec = beamai_process:builder(<<"data_pipeline">>),
Spec1 = beamai_process:add_step(Spec, <<"fetch">>, my_step_module, #{type => fetch}),
Spec2 = beamai_process:add_step(Spec1, <<"transform">>, my_step_module, #{type => transform}),
Spec3 = beamai_process:add_step(Spec2, <<"save">>, my_step_module, #{type => save}),

%% 设置事件驱动链路
Spec4 = beamai_process:on_event(Spec3, <<"fetch">>, <<"fetch_done">>, <<"transform">>),
Spec5 = beamai_process:on_event(Spec4, <<"transform">>, <<"transform_done">>, <<"save">>),

%% 设置初始事件和构建
Spec6 = beamai_process:set_initial_event(Spec5, <<"fetch">>, #{}),
{ok, Built} = beamai_process:build(Spec6),

%% 同步执行
{ok, Result} = beamai_process:run_sync(Built, #{timeout => 30000}).
```

### 加载工具模块

```erlang
%% 加载实现 beamai_tool_behaviour 的工具模块
Kernel = beamai_kernel:new(),
Kernel1 = beamai_kernel:add_tool_module(Kernel, beamai_tool_file),

%% 列出已注册的工具
Tools = beamai_kernel:get_tool_specs(Kernel1).
```

### 会话记忆（多轮对话）

Kernel 无状态，每次 invoke 只传单条最新消息；历史由 Memory Filter 按
`conversation_id` 管理。详见 [docs/MEMORY.md](../../docs/MEMORY.md)。

```erlang
%% 启动会话存储并启用记忆
{ok, _} = beamai_chat_memory_ets:start_link(my_mem),
K = beamai_kernel:with_memory(Kernel1, beamai_chat_memory_ets:handle(my_mem)),

%% 用 conversation_id 标识会话，每次只传最新消息
Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"session-1">>),
{ok, R1, _} = beamai_kernel:invoke_chat(K, [#{role => user, content => <<"我叫张三">>}], #{context => Ctx}),
{ok, R2, _} = beamai_kernel:invoke_chat(K, [#{role => user, content => <<"我叫什么？">>}], #{context => Ctx}).
%% 第二轮 LLM 能看到完整历史；未挂 memory 则为单次无状态调用
%% 需要"自动执行工具并多轮循环"请用 beamai_agent（ReAct）
```

## 依赖

- jsx - JSON 编解码
- uuid - UUID 生成
- gun - HTTP/2 客户端
- hackney - HTTP/1.1 客户端
- poolboy - 连接池

## 许可证

Apache-2.0
