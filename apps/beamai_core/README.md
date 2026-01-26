# BeamAI Core

[English](README_EN.md) | 中文

BeamAI 框架的核心模块，提供 Kernel 架构、Process Framework、Graph Engine、HTTP 客户端和行为定义。

## 模块概览

### Kernel 子系统

基于 Semantic Kernel 理念的核心抽象，管理 Tool 的注册与调用：

- **beamai_kernel** - Kernel 核心，管理 Tool 注册和调用
- **beamai_tool** - 工具定义，封装可调用的工具
- **beamai_tool_behaviour** - 工具模块行为接口
- **beamai_context** - 上下文管理，传递执行环境信息
- **beamai_filter** - 过滤器，用于工具调用前后的拦截
- **beamai_prompt** - 提示词模板管理
- **beamai_result** - 工具调用结果类型

### Process Framework 子系统

可编排的流程引擎，支持步骤定义、条件分支、并行执行和时间旅行：

- **beamai_process** - 流程定义和核心数据结构
- **beamai_process_builder** - 流程构建器（Builder 模式）
- **beamai_process_runtime** - 流程运行时
- **beamai_process_step** - 步骤定义
- **beamai_process_step_transform** - 步骤转换
- **beamai_process_executor** - 流程执行器
- **beamai_process_event** - 事件系统
- **beamai_process_state** - 流程状态管理
- **beamai_process_memory_store** - 流程状态持久化
- **beamai_process_worker** - 流程工作进程
- **beamai_process_sup** - 流程监督树

### Graph Engine 子系统

基于 LangGraph 理念的声明式图执行引擎：

- **graph** - 图定义和构建器
- **graph_node** - 节点定义
- **graph_edge** - 边定义
- **graph_builder** - 图构建器
- **graph_dsl** - 声明式 DSL
- **graph_runner** - 图执行引擎
- **graph_state** - 图状态管理
- **graph_snapshot** - 图状态快照

### Pregel 子系统

基于 Pregel 模型的分布式图计算：

- **pregel** - Pregel 主节点和工作节点
- **pregel_master** - 主节点
- **pregel_worker** - 工作节点
- **pregel_vertex** - 顶点计算
- **pregel_dispatch_worker** - 分发池工作进程

### HTTP 子系统

可插拔的 HTTP 客户端，支持 Gun 和 Hackney 后端：

- **beamai_http** - HTTP 客户端统一接口
- **beamai_http_gun** - Gun HTTP/2 后端实现
- **beamai_http_hackney** - Hackney HTTP/1.1 后端实现
- **beamai_http_pool** - HTTP 连接池管理

### Behaviour 定义

框架的行为接口定义：

- **beamai_llm_behaviour** - LLM 提供者行为接口
- **beamai_http_behaviour** - HTTP 后端行为接口
- **beamai_step_behaviour** - 流程步骤行为接口
- **beamai_process_store_behaviour** - 流程存储行为接口
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

### beamai_process_builder

```erlang
%% 创建流程构建器
beamai_process_builder:new(Name) -> builder().

%% 添加步骤
beamai_process_builder:add_step(Builder, StepName, StepOpts) -> builder().

%% 构建流程
beamai_process_builder:build(Builder) -> {ok, Process} | {error, Reason}.
```

### beamai_process_executor

```erlang
%% 执行流程
beamai_process_executor:run(Process, Input) -> {ok, Result} | {error, Reason}.
beamai_process_executor:run(Process, Input, Opts) -> {ok, Result} | {error, Reason}.
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

%% 调用
{ok, Content, _NewCtx} = beamai_kernel:invoke(Kernel1, <<"read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}, beamai_context:new()).
```

### Process Framework

```erlang
%% 构建多步流程
Builder = beamai_process_builder:new(<<"data_pipeline">>),

Builder1 = beamai_process_builder:add_step(Builder, <<"fetch">>, #{
    handler => fun(Input, _Ctx) ->
        {ok, Input#{data => fetch_data()}}
    end
}),

Builder2 = beamai_process_builder:add_step(Builder1, <<"transform">>, #{
    handler => fun(#{data := Data} = Input, _Ctx) ->
        {ok, Input#{data => transform(Data)}}
    end
}),

Builder3 = beamai_process_builder:add_step(Builder2, <<"save">>, #{
    handler => fun(#{data := Data} = Input, _Ctx) ->
        ok = save_data(Data),
        {ok, Input#{saved => true}}
    end
}),

{ok, Process} = beamai_process_builder:build(Builder3),
{ok, Result} = beamai_process_executor:run(Process, #{}).
```

### Graph Engine

```erlang
%% 使用 DSL 构建简单图
{ok, Graph} = graph:build([
    {node, greeting, fun(State, _Ctx) ->
        Name = graph_state:get(State, name, <<"World">>),
        Message = <<"Hello, ", Name/binary, "!">>,
        {ok, graph_state:set(State, message, Message)}
    end},
    {node, uppercase, fun(State, _Ctx) ->
        Message = graph_state:get(State, message, <<>>),
        Upper = string:uppercase(Message),
        {ok, graph_state:set(State, message, Upper)}
    end},
    {edge, greeting, uppercase},
    {edge, uppercase, '__end__'},
    {entry, greeting}
]),

%% 运行图
InitialState = graph:state(#{name => <<"Erlang">>}),
Result = graph:run(Graph, InitialState).
```

### 加载工具模块

```erlang
%% 加载实现 beamai_tool_behaviour 的工具模块
Kernel = beamai_kernel:new(),
Kernel1 = beamai_kernel:add_tool_module(Kernel, beamai_tool_file),

%% 列出已注册的工具
Tools = beamai_kernel:get_tool_specs(Kernel1).
```

## 依赖

- jsx - JSON 编解码
- uuid - UUID 生成
- gun - HTTP/2 客户端
- hackney - HTTP/1.1 客户端
- poolboy - 连接池

## 许可证

Apache-2.0
