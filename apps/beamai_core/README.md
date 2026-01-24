# BeamAI Core

[English](README_EN.md) | 中文

BeamAI 框架的核心模块，提供 Kernel 架构、Process Framework、HTTP 客户端和行为定义。

## 模块概览

### Kernel 子系统

基于 Semantic Kernel 理念的核心抽象，管理 Plugin 和 Function 的注册与调用：

- **beamai_kernel** - Kernel 核心，管理 Plugin/Function 注册和调用
- **beamai_function** - 函数定义，封装可调用的工具函数
- **beamai_context** - 上下文管理，传递执行环境信息
- **beamai_filter** - 过滤器，用于函数调用前后的拦截
- **beamai_plugin** - 插件定义（Kernel 内部使用）
- **beamai_prompt** - 提示词模板管理
- **beamai_result** - 函数调用结果类型

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

%% 添加 Plugin
beamai_kernel:add_plugin(Kernel, Name, Functions) -> kernel().
beamai_kernel:add_plugin(Kernel, Name, Functions, Opts) -> kernel().
beamai_kernel:add_plugin_from_module(Kernel, Module) -> kernel().

%% 添加服务和过滤器
beamai_kernel:add_service(Kernel, Service) -> kernel().
beamai_kernel:add_filter(Kernel, Filter) -> kernel().

%% 调用函数
beamai_kernel:invoke(Kernel, FunctionName, Args) -> {ok, Result} | {error, Reason}.
beamai_kernel:invoke_tool(Kernel, ToolName, Args, Context) -> {ok, Result} | {error, Reason}.
beamai_kernel:invoke_chat(Kernel, Messages, Opts) -> {ok, Response} | {error, Reason}.

%% 查询函数
beamai_kernel:get_function(Kernel, Name) -> {ok, Function} | error.
beamai_kernel:list_functions(Kernel) -> [Function].
beamai_kernel:get_tool_specs(Kernel) -> [ToolSpec].
beamai_kernel:get_tool_schemas(Kernel) -> [Schema].
```

### beamai_function

```erlang
%% 创建函数
beamai_function:new(Name, Description, Handler, Opts) -> function().

%% Name: 函数名（binary）
%% Description: 函数描述（binary）
%% Handler: fun(Args, Context) -> {ok, Result} | {error, Reason}
%% Opts: #{parameters => Schema, ...}
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

### Kernel + Function

```erlang
%% 创建 Kernel
Kernel = beamai_kernel:new(),

%% 定义函数
ReadFile = beamai_function:new(
    <<"read_file">>,
    <<"读取文件内容"/utf8>>,
    fun(#{<<"path">> := Path}, _Ctx) ->
        case file:read_file(Path) of
            {ok, Content} -> {ok, Content};
            {error, Reason} -> {error, Reason}
        end
    end,
    #{parameters => #{
        type => object,
        properties => #{
            <<"path">> => #{type => string, description => <<"文件路径"/utf8>>}
        },
        required => [<<"path">>]
    }}
),

%% 注册到 Kernel
Kernel1 = beamai_kernel:add_plugin(Kernel, <<"file_ops">>, [ReadFile]),

%% 调用
{ok, Content} = beamai_kernel:invoke(Kernel1, <<"file_ops-read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}).
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

## 依赖

- jsx - JSON 编解码
- uuid - UUID 生成
- gun - HTTP/2 客户端
- hackney - HTTP/1.1 客户端

## 许可证

Apache-2.0
