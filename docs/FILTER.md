# Filter 过滤器系统文档

[English](FILTER_EN.md) | 中文

beamai_core 的 Filter 系统提供了轻量级的管道拦截机制，用于在工具调用和 LLM 请求的前后进行拦截、修改和控制。

## 目录

- [概述](#概述)
- [过滤器类型](#过滤器类型)
- [API 参考](#api-参考)
- [使用方法](#使用方法)
- [完整示例](#完整示例)
- [与 Middleware 的关系](#与-middleware-的关系)

---

## 概述

Filter 是 Kernel 工具调用和 Chat 请求的拦截器，可以：

- **修改参数/结果**: 在工具调用前后修改参数和返回值
- **修改消息**: 在 LLM 调用前后修改消息列表和响应
- **拦截执行**: 跳过工具执行直接返回结果，或中止管道返回错误
- **日志/审计**: 记录调用日志、统计响应长度等

### 执行流程

```
┌──────────────────────────────────────────────────────┐
│                  Kernel 调用流程                       │
├──────────────────────────────────────────────────────┤
│                                                       │
│  工具调用 (invoke_tool):                              │
│  ┌─────────────────┐                                 │
│  │ pre_invocation   │  ← 可修改参数、跳过、拒绝       │
│  └────────┬────────┘                                 │
│           │                                           │
│           ▼                                           │
│  ┌─────────────────┐                                 │
│  │  Tool Execution  │                                │
│  └────────┬────────┘                                 │
│           │                                           │
│           ▼                                           │
│  ┌─────────────────┐                                 │
│  │ post_invocation  │  ← 可修改结果                   │
│  └─────────────────┘                                 │
│                                                       │
│  Chat 请求 (invoke_chat):                             │
│  ┌─────────────────┐                                 │
│  │   pre_chat       │  ← 可修改消息列表               │
│  └────────┬────────┘                                 │
│           │                                           │
│           ▼                                           │
│  ┌─────────────────┐                                 │
│  │   LLM Call       │                                │
│  └────────┬────────┘                                 │
│           │                                           │
│           ▼                                           │
│  ┌─────────────────┐                                 │
│  │   post_chat      │  ← 可修改响应                   │
│  └─────────────────┘                                 │
│                                                       │
└──────────────────────────────────────────────────────┘
```

### 核心模块

| 模块 | 位置 | 说明 |
|------|------|------|
| `beamai_filter` | `apps/beamai_core/src/kernel/` | 过滤器定义和管道执行 |
| `beamai_kernel` | `apps/beamai_core/src/kernel/` | Kernel 集成（注册和调用过滤器） |
| `beamai` | `apps/beamai_core/src/` | 顶层 Facade（便捷 API） |

---

## 过滤器类型

### 四种过滤器

| 类型 | 触发时机 | 典型用途 |
|------|----------|----------|
| `pre_invocation` | 工具执行前 | 参数验证、日志记录、权限检查 |
| `post_invocation` | 工具执行后 | 结果转换、日志记录 |
| `pre_chat` | LLM 调用前 | 注入 system 消息、内容过滤、**会话历史注入** |
| `post_chat` | LLM 响应后 | 内容审计、响应转换、**会话历史存储** |

> **会话记忆**正是基于 `pre_chat` / `post_chat` 实现的：`beamai_memory_filter` 用
> pre_chat(-1000) 存 delta 并展开完整历史、post_chat(+1000) 存储回复，按
> `conversation_id` 管理对话。详见 [MEMORY.md](MEMORY.md)。

### 过滤器上下文 (filter_context)

过滤器通过 `filter_context` Map 接收和传递数据：

```erlang
%% pre_invocation / post_invocation
#{
    tool => ToolSpec,           %% 工具定义
    args => Args,               %% 调用参数（pre 可修改）
    result => Result,           %% 执行结果（仅 post 可用）
    context => Context,         %% 执行上下文
    metadata => #{}             %% 附加元数据
}

%% pre_chat
#{
    messages => [Message],      %% 消息列表（可修改）
    context => Context,         %% 执行上下文
    metadata => #{}
}

%% post_chat
#{
    result => Response,         %% LLM 响应（可修改）
    context => Context,
    metadata => #{}
}
```

### 过滤器返回值 (filter_result)

```erlang
%% 继续执行，传递修改后的上下文给下一个过滤器
{continue, UpdatedFilterCtx}

%% 跳过后续处理（包括工具执行），直接返回值
{skip, Value}

%% 中止管道，返回错误
{error, Reason}
```

---

## API 参考

### beamai_filter 模块

#### 创建过滤器

```erlang
%% 创建过滤器（默认优先级 0）
-spec new(Name :: binary(), Type :: filter_type(), Handler :: fun()) -> filter_spec().
beamai_filter:new(<<"my_filter">>, pre_invocation, fun(Ctx) -> {continue, Ctx} end).

%% 创建过滤器（指定优先级，数值越小越先执行）
-spec new(Name :: binary(), Type :: filter_type(), Handler :: fun(), Priority :: integer()) -> filter_spec().
beamai_filter:new(<<"my_filter">>, pre_invocation, Handler, 10).
```

#### 执行过滤器管道

```erlang
%% 执行前置调用过滤器
-spec apply_pre_filters(Filters, ToolSpec, Args, Context) ->
    {ok, FilteredArgs, FilteredContext} | {skip, Value} | {error, Reason}.

%% 执行后置调用过滤器
-spec apply_post_filters(Filters, ToolSpec, Result, Context) ->
    {ok, FilteredResult, FilteredContext} | {error, Reason}.

%% 执行前置 Chat 过滤器
-spec apply_pre_chat_filters(Filters, Messages, Context) ->
    {ok, FilteredMessages, FilteredContext} | {error, Reason}.

%% 执行后置 Chat 过滤器
-spec apply_post_chat_filters(Filters, Response, Context) ->
    {ok, FilteredResponse, FilteredContext} | {error, Reason}.

%% 按优先级排序过滤器
-spec sort_filters(Filters) -> SortedFilters.
```

#### 过滤器定义类型

```erlang
-type filter_spec() :: #{
    name := binary(),                                      %% 过滤器名称（调试标识）
    type := filter_type(),                                 %% 过滤器类型
    handler := fun((filter_context()) -> filter_result()), %% 处理函数
    priority => integer()                                  %% 优先级（默认 0）
}.

-type filter_type() :: pre_invocation | post_invocation | pre_chat | post_chat.

-type filter_result() ::
    {continue, filter_context()}   %% 传递给下一个过滤器
    | {skip, term()}               %% 跳过执行
    | {error, term()}.             %% 中止管道
```

### beamai_kernel 集成

```erlang
%% 注册过滤器到 Kernel
beamai_kernel:add_filter(Kernel, FilterDef) -> UpdatedKernel.

%% 从工具模块自动加载过滤器（模块需实现可选的 filters/0 回调）
beamai_kernel:add_tool_module(Kernel, Module) -> UpdatedKernel.

%% 调用工具（自动执行 pre/post invocation 过滤器管道）
beamai_kernel:invoke_tool(Kernel, ToolName, Args, Context) -> {ok, Result, Context} | {error, Reason}.

%% Chat 请求（自动执行 pre/post chat 过滤器管道）
beamai_kernel:invoke_chat(Kernel, Messages, Opts) -> {ok, Response, Context} | {error, Reason}.
```

### beamai 便捷 API

```erlang
%% 注册已构建的过滤器
beamai:add_filter(Kernel, FilterDef) -> UpdatedKernel.

%% 快捷创建并注册过滤器（自动调用 beamai_filter:new/3）
beamai:add_filter(Kernel, Name, Type, Handler) -> UpdatedKernel.
```

---

## 使用方法

### 1. 注册过滤器到 Kernel

```erlang
%% 方式一：使用 beamai 便捷 API（推荐）
K0 = beamai:kernel(),
K1 = beamai:add_filter(K0, <<"logger">>, pre_invocation,
    fun(#{tool := #{name := Name}} = Ctx) ->
        io:format("Calling tool: ~ts~n", [Name]),
        {continue, Ctx}
    end).

%% 方式二：手动创建过滤器并注册
Filter = beamai_filter:new(<<"logger">>, pre_invocation, Handler, 10),
K1 = beamai_kernel:add_filter(K0, Filter).
```

### 2. 工具模块自动注册过滤器

工具模块可以通过实现可选的 `filters/0` 回调自动注册过滤器：

```erlang
-module(my_tool_module).
-behaviour(beamai_tool_behaviour).

-export([tools/0, filters/0]).

tools() ->
    [#{name => <<"my_tool">>, handler => fun handle/2,
       description => <<"My tool">>}].

%% 可选回调：返回过滤器列表
filters() ->
    [
        beamai_filter:new(<<"audit">>, post_invocation,
            fun(#{tool := #{name := Name}, result := Result} = Ctx) ->
                logger:info("Tool ~ts returned: ~p", [Name, Result]),
                {continue, Ctx}
            end)
    ].
```

加载模块时，Kernel 会自动注册这些过滤器：

```erlang
K1 = beamai_kernel:add_tool_module(K0, my_tool_module).
%% 工具和过滤器都已注册
```

### 3. 过滤器优先级

优先级数值越小越先执行。同优先级按注册顺序执行。

```erlang
%% 验证器（优先级 -10，最先执行）
K1 = beamai_kernel:add_filter(K0,
    beamai_filter:new(<<"validator">>, pre_invocation, ValidateFn, -10)),

%% 日志器（优先级 0，默认）
K2 = beamai_kernel:add_filter(K1,
    beamai_filter:new(<<"logger">>, pre_invocation, LogFn, 0)),

%% 转换器（优先级 10，最后执行）
K3 = beamai_kernel:add_filter(K2,
    beamai_filter:new(<<"transformer">>, pre_invocation, TransformFn, 10)).

%% 执行顺序：validator → logger → transformer
```

---

## 完整示例

### 示例 1：参数验证 + 日志记录

```erlang
%% 创建 Kernel 并注册工具
K0 = beamai:kernel(),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})),

%% 前置过滤器：记录调用日志
K2 = beamai:add_filter(K1, <<"log">>, pre_invocation,
    fun(#{tool := #{name := Name}, args := Args} = Ctx) ->
        io:format("[LOG] ~ts(~p)~n", [Name, Args]),
        {continue, Ctx}
    end),

%% 前置过滤器：参数验证（拒绝过大的值）
K3 = beamai:add_filter(K2, <<"validate">>, pre_invocation,
    fun(#{args := #{a := A}} = _Ctx) when A > 1000 ->
        {error, {validation_failed, <<"a exceeds limit">>}};
       (Ctx) ->
        {continue, Ctx}
    end),

%% 后置过滤器：结果翻倍
K4 = beamai:add_filter(K3, <<"double">>, post_invocation,
    fun(#{result := Result} = Ctx) ->
        {continue, Ctx#{result => Result * 2}}
    end),

%% 调用（3 + 5 = 8，翻倍后 = 16）
{ok, 16, _} = beamai:invoke_tool(K4, <<"add">>, #{a => 3, b => 5}, beamai:context()).

%% 调用被拒绝
{error, {validation_failed, _}} = beamai:invoke_tool(K4, <<"add">>, #{a => 2000, b => 1}, beamai:context()).
```

### 示例 2：Chat 消息注入 + 响应审计

```erlang
K0 = beamai:kernel(),
K1 = beamai:add_llm(K0, LLMConfig),

%% pre_chat：自动注入 system 消息
K2 = beamai:add_filter(K1, <<"inject_system">>, pre_chat,
    fun(#{messages := Msgs} = Ctx) ->
        HasSystem = lists:any(
            fun(#{role := R}) -> R =:= system; (_) -> false end,
            Msgs),
        case HasSystem of
            true ->
                {continue, Ctx};
            false ->
                SystemMsg = #{role => system,
                              content => <<"请用简洁的中文回答。"/utf8>>},
                {continue, Ctx#{messages => [SystemMsg | Msgs]}}
        end
    end),

%% post_chat：记录响应长度
K3 = beamai:add_filter(K2, <<"audit">>, post_chat,
    fun(#{result := #{content := Content}} = Ctx) when is_binary(Content) ->
        logger:info("Response length: ~B bytes", [byte_size(Content)]),
        {continue, Ctx};
       (Ctx) ->
        {continue, Ctx}
    end),

%% 发送请求（过滤器自动注入 system 消息）
{ok, Response, _Ctx} = beamai:chat(K3, [
    #{role => user, content => <<"什么是 GenServer？"/utf8>>}
]).
```

### 示例 3：使用 skip 实现缓存

```erlang
%% pre_invocation 中跳过执行，返回缓存结果
K1 = beamai:add_filter(K0, <<"cache">>, pre_invocation,
    fun(#{tool := #{name := Name}, args := Args} = Ctx) ->
        case lookup_cache(Name, Args) of
            {ok, Cached} ->
                {skip, Cached};     %% 跳过工具执行，直接返回缓存值
            miss ->
                {continue, Ctx}     %% 缓存未命中，继续正常执行
        end
    end).
```

更多示例参见 [examples/src/example_filter.erl](../examples/src/example_filter.erl)。

---

## 与 Middleware 的关系

[beamai_extra](https://github.com/TTalkPro/beamai_extra) 扩展项目中提供了更高级的 Middleware 系统（位于 beamai_tools），支持有状态管理、预设配置、调用限制、人工审批、重试和降级等功能。

Middleware 内部通过 `beamai_middleware_runner:to_filters/1` 转换为 Filter 注册到 Kernel，两者最终在同一个过滤器管道中执行。

| 特性 | Filter（本文档） | Middleware（beamai_extra） |
|------|------------------|---------------------------|
| 复杂度 | 轻量级，无状态 | 完整框架，有状态管理 |
| 预设配置 | 无 | 提供 production/development 等预设 |
| 内置功能 | 无 | 调用限制、人工审批、重试、降级 |
| 适用场景 | 简单拦截：日志、验证、转换 | 复杂控制：限流、重试、降级 |

---

## 更多资源

- [beamai_core README](../apps/beamai_core/README.md) - Kernel 架构文档
- [API 参考](API_REFERENCE.md) - API 参考文档
- [示例代码](../examples/src/example_filter.erl) - Filter 完整示例
