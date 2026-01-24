# Middleware 系统文档

[English](MIDDLEWARE_EN.md) | 中文

beamai_plugin 的 Middleware 系统提供了一种灵活的方式来拦截、修改和控制 Kernel 函数调用的各个阶段。

## 目录

- [概述](#概述)
- [生命周期钩子](#生命周期钩子)
- [内置 Middleware](#内置-middleware)
- [预设配置](#预设配置)
- [自定义 Middleware](#自定义-middleware)
- [配置和使用](#配置和使用)
- [高级用法](#高级用法)

---

## 概述

Middleware 是 Kernel 函数调用过程中的拦截器，可以：

- **修改输入/输出**: 在 LLM 调用或工具调用前后修改上下文
- **控制流程**: 跳过、重试或中止执行
- **添加功能**: 限流、人工审批、模型降级等
- **实施限制**: 调用次数限制、错误重试等

### 架构图

```
┌─────────────────────────────────────────────────────────────┐
│                    Kernel 函数调用                            │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐                                           │
│  │   pre_chat   │  ← LLM 调用前                             │
│  └──────┬───────┘                                           │
│         │                                                    │
│         ▼                                                    │
│  ┌──────────────┐                                           │
│  │   LLM Call   │                                           │
│  └──────┬───────┘                                           │
│         │                                                    │
│         ▼                                                    │
│  ┌──────────────┐                                           │
│  │  post_chat   │  ← LLM 响应后（可重试/降级）              │
│  └──────┬───────┘                                           │
│         │                                                    │
│         ▼                                                    │
│  ┌────────────────┐                                         │
│  │pre_invocation  │  ← 工具执行前（可审批/拦截）            │
│  └──────┬─────────┘                                         │
│         │                                                    │
│         ▼                                                    │
│  ┌──────────────┐                                           │
│  │Tool Execution│                                           │
│  └──────┬───────┘                                           │
│         │                                                    │
│         ▼                                                    │
│  ┌────────────────┐                                         │
│  │post_invocation │  ← 工具执行后（可重试）                 │
│  └────────────────┘                                         │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### 核心模块

| 模块 | 位置 | 说明 |
|------|------|------|
| `beamai_middleware` | `apps/beamai_plugin/src/middleware/` | Middleware 行为定义 |
| `beamai_middleware_runner` | `apps/beamai_plugin/src/middleware/` | Middleware 链执行器 |
| `beamai_middleware_presets` | `apps/beamai_plugin/src/middleware/` | 预设配置 |

---

## 生命周期钩子

### 钩子列表

| 钩子 | 触发时机 | 典型用途 |
|------|----------|----------|
| `pre_chat` | LLM 调用前 | 检查调用限制、修改消息 |
| `post_chat` | LLM 响应后 | 错误重试、模型降级 |
| `pre_invocation` | 工具执行前 | 人工审批、参数验证 |
| `post_invocation` | 工具执行后 | 失败重试、计数更新 |

### 返回值类型

Middleware 钩子函数可以返回以下值：

```erlang
%% 无修改，继续执行
ok

%% 修改上下文后继续（传递给下一个 Middleware）
{continue, UpdatedFilterCtx}

%% 跳过后续处理，直接返回结果
{skip, Term}

%% 中止执行并返回错误
{error, Reason}
```

### FilterCtx 结构

Middleware 通过 `FilterCtx` 传递上下文信息：

```erlang
%% pre_chat / post_chat
FilterCtx = #{
    result => ok | {error, Reason}   %% LLM 调用结果（post_chat 中可用）
}

%% pre_invocation / post_invocation
FilterCtx = #{
    function => #{name => <<"tool_name">>, ...},  %% 函数定义
    args => #{<<"param">> => Value},                %% 调用参数
    result => ok | {error, Error}                   %% 执行结果（post_invocation 中可用）
}
```

---

## 内置 Middleware

### 1. middleware_call_limit - 调用限制

限制函数调用过程中的各种调用次数。

**钩子**: `pre_chat`, `pre_invocation`, `post_invocation`
**优先级**: 10（最先执行）

```erlang
{middleware_call_limit, #{
    max_model_calls => 20,           %% 最大模型调用次数
    max_tool_calls => 50,            %% 最大工具调用总次数
    max_tool_calls_per_turn => 10,   %% 每轮最大工具调用数
    max_iterations => 15,            %% 最大迭代次数
    on_limit_exceeded => halt        %% 超限行为: halt | warn_and_continue
}}
```

### 2. middleware_human_approval - 人工审批

在工具执行前请求人工确认。

**钩子**: `pre_invocation`
**优先级**: 50

```erlang
{middleware_human_approval, #{
    mode => all,                     %% all | selective | custom | none
    tools_requiring_approval => [<<"dangerous_tool">>],  %% selective 模式下需审批的工具
    approval_fn => fun(FunctionName, Ctx) -> boolean() end,  %% custom 模式下的审批函数
    approval_handler => fun(FunctionName, Ctx) -> approve | reject end,  %% 同步审批处理器
    timeout => 60000,                %% 审批超时时间(ms)
    timeout_action => reject         %% 超时行为: reject | confirm
}}
```

### 3. middleware_tool_retry - 工具重试

工具执行失败时自动重试。

**钩子**: `post_invocation`
**优先级**: 80

```erlang
{middleware_tool_retry, #{
    max_retries => 3,                %% 最大重试次数
    backoff => #{
        type => exponential,         %% 退避类型: exponential | linear | constant
        initial_delay => 1000,       %% 初始延迟(ms)
        max_delay => 30000,          %% 最大延迟(ms)
        multiplier => 2              %% 指数因子
    },
    retryable_errors => all,         %% all | [error_type]
    retry_fn => fun(Error, Ctx) -> boolean() end,  %% 自定义重试判断
    on_retry => fun(Error, RetryCount, Delay, Ctx) -> ok end,  %% 重试回调
    enable_delay => true             %% 是否启用退避延迟
}}
```

**辅助函数：**

```erlang
%% 判断错误是否可重试
middleware_tool_retry:is_retryable(Error, MwState) -> boolean().

%% 计算退避延迟
middleware_tool_retry:calculate_delay(RetryCount, BackoffConfig) -> pos_integer().
```

### 4. middleware_model_retry - 模型重试

LLM 调用失败时自动重试（支持 Jitter）。

**钩子**: `post_chat`
**优先级**: 90

```erlang
{middleware_model_retry, #{
    max_retries => 3,
    backoff => #{
        type => exponential,
        initial_delay => 1000,
        max_delay => 30000,
        multiplier => 2,
        jitter => true               %% 启用随机抖动（避免惊群效应）
    },
    retryable_errors => [timeout, rate_limit, server_error],
    retry_fn => fun(Error, Ctx) -> boolean() end,
    on_retry => fun(Error, RetryCount, Delay, Ctx) -> ok end
}}
```

### 5. middleware_model_fallback - 模型降级

主模型失败时切换到备用模型。

**钩子**: `post_chat`
**优先级**: 95（最后执行）

```erlang
{middleware_model_fallback, #{
    fallback_models => [
        #{provider => openai, model => <<"gpt-3.5-turbo">>},
        #{provider => ollama, model => <<"llama2">>}
    ],
    trigger_errors => [rate_limit, timeout],  %% 触发降级的错误类型
    on_fallback => fun(OriginalError, FallbackModel) -> ok end  %% 降级回调
}}
```

---

## 预设配置

### 使用预设

```erlang
%% 默认配置（call_limit + model_retry）
Middlewares = beamai_middleware_presets:default().

%% 最小配置（仅 call_limit）
Middlewares = beamai_middleware_presets:minimal().

%% 生产环境（call_limit + tool_retry + model_retry + model_fallback）
Middlewares = beamai_middleware_presets:production().

%% 开发调试（宽松 call_limit + tool_retry）
Middlewares = beamai_middleware_presets:development().

%% 人工审批（call_limit + human_approval）
Middlewares = beamai_middleware_presets:human_in_loop().
```

### 预设内容对比

| 预设 | call_limit | tool_retry | model_retry | model_fallback | human_approval |
|------|------------|------------|-------------|----------------|----------------|
| default | ✓ | - | ✓ | - | - |
| minimal | ✓ | - | - | - | - |
| production | ✓ (严格) | ✓ | ✓ | ✓ | - |
| development | ✓ (宽松) | ✓ | - | - | - |
| human_in_loop | ✓ | - | - | - | ✓ |

### 自定义预设选项

```erlang
%% 自定义 default 预设参数
Middlewares = beamai_middleware_presets:default(#{
    call_limit => #{max_model_calls => 30},
    model_retry => #{max_retries => 5}
}).

%% 扩展预设
Middlewares = beamai_middleware_presets:production() ++ [
    {my_custom_middleware, #{option => value}}
].
```

### 获取单个 Middleware 配置

```erlang
%% 获取单个 Middleware 的默认配置
CallLimit = beamai_middleware_presets:call_limit().
CallLimit2 = beamai_middleware_presets:call_limit(#{max_model_calls => 50}).

HumanApproval = beamai_middleware_presets:human_approval().
ToolRetry = beamai_middleware_presets:tool_retry().
ModelRetry = beamai_middleware_presets:model_retry().
ModelFallback = beamai_middleware_presets:model_fallback().
```

---

## 自定义 Middleware

### 基本结构

```erlang
-module(my_middleware).
-behaviour(beamai_middleware).

%% 导出回调函数（所有回调都是可选的）
-export([init/1, pre_chat/2, post_chat/2,
         pre_invocation/2, post_invocation/2]).

%% 初始化 Middleware 状态
init(Opts) ->
    #{
        my_option => maps:get(my_option, Opts, default_value),
        counter => 0
    }.

%% LLM 调用前
pre_chat(FilterCtx, MwState) ->
    %% FilterCtx: 过滤器上下文
    %% MwState: Middleware 内部状态
    ok.

%% LLM 响应后
post_chat(FilterCtx, MwState) ->
    case maps:get(result, FilterCtx, ok) of
        {error, _} ->
            %% 可以决定重试
            ok;
        ok ->
            ok
    end.

%% 工具执行前
pre_invocation(FilterCtx, MwState) ->
    FuncName = maps:get(name, maps:get(function, FilterCtx, #{}), <<>>),
    case is_blocked(FuncName) of
        true -> {error, {blocked_tool, FuncName}};
        false -> ok
    end.

%% 工具执行后
post_invocation(FilterCtx, MwState) ->
    ok.
```

### 完整示例：调用计数器

```erlang
-module(middleware_counter).
-behaviour(beamai_middleware).

-export([init/1, pre_chat/2, pre_invocation/2]).

init(Opts) ->
    #{
        max_calls => maps:get(max_calls, Opts, 10),
        model_count => 0,
        tool_count => 0
    }.

%% 模型调用前 - 检查并递增计数
pre_chat(_FilterCtx, #{max_calls := MaxCalls, model_count := Count} = MwState) ->
    case Count >= MaxCalls of
        true ->
            {error, {model_call_limit_exceeded, Count}};
        false ->
            %% 更新内部状态（通过 runner 的 set_middleware_state）
            ok
    end.

%% 工具调用前 - 计数
pre_invocation(_FilterCtx, #{tool_count := Count} = _MwState) ->
    logger:info("Tool call #~p", [Count + 1]),
    ok.
```

### 完整示例：请求日志

```erlang
-module(middleware_logger).
-behaviour(beamai_middleware).

-export([init/1, pre_chat/2, post_chat/2, pre_invocation/2, post_invocation/2]).

init(Opts) ->
    #{log_level => maps:get(log_level, Opts, info)}.

pre_chat(_FilterCtx, #{log_level := Level}) ->
    log(Level, ">>> LLM call starting"),
    ok.

post_chat(FilterCtx, #{log_level := Level}) ->
    case maps:get(result, FilterCtx, ok) of
        ok -> log(Level, "<<< LLM call succeeded");
        {error, Reason} -> log(Level, "<<< LLM call failed: ~p", [Reason])
    end,
    ok.

pre_invocation(FilterCtx, #{log_level := Level}) ->
    FuncName = maps:get(name, maps:get(function, FilterCtx, #{}), <<"unknown">>),
    Args = maps:get(args, FilterCtx, #{}),
    log(Level, ">>> Tool ~ts called with ~p", [FuncName, Args]),
    ok.

post_invocation(FilterCtx, #{log_level := Level}) ->
    case maps:get(result, FilterCtx, ok) of
        ok -> log(Level, "<<< Tool execution succeeded");
        {error, Reason} -> log(Level, "<<< Tool execution failed: ~p", [Reason])
    end,
    ok.

log(info, Fmt) -> logger:info(Fmt);
log(info, Fmt, Args) -> logger:info(Fmt, Args);
log(debug, Fmt) -> logger:debug(Fmt);
log(debug, Fmt, Args) -> logger:debug(Fmt, Args).
```

---

## 配置和使用

### 与 Kernel 集成

```erlang
%% 方式 1：使用 beamai_plugins 的 with_middleware
Kernel = beamai_kernel:new(),
Kernel1 = beamai_plugins:load_all(Kernel, [beamai_plugin_file, beamai_plugin_shell]),
Kernel2 = beamai_plugins:with_middleware(Kernel1,
    beamai_middleware_presets:production()),

%% 方式 2：手动初始化 Middleware 链
Chain = beamai_middleware_runner:init([
    {middleware_call_limit, #{max_model_calls => 15}},
    {middleware_tool_retry, #{max_retries => 5}},
    {my_custom_middleware, #{option => value}}
]).

%% 方式 3：转换为 Kernel Filter
Filters = beamai_middleware_runner:to_filters(Chain),
Kernel3 = beamai_kernel:add_filter(Kernel, Filters).
```

### Middleware 配置格式

```erlang
%% 完整格式：{模块, 选项, 优先级}
{middleware_call_limit, #{max_model_calls => 20}, 10}

%% 省略优先级：{模块, 选项}（使用默认优先级 100）
{middleware_call_limit, #{max_model_calls => 20}}

%% 仅模块名（使用默认选项和优先级）
middleware_call_limit
```

### 优先级说明

- 数值越小，越先执行
- 默认优先级为 100
- 内置 Middleware 预设优先级：
  - 10: call_limit（边界检查，最先执行）
  - 50: human_approval（需要交互）
  - 80: tool_retry（恢复机制）
  - 90: model_retry（恢复机制）
  - 95: model_fallback（最后降级）

---

## 高级用法

### 直接执行钩子

```erlang
%% 初始化 Middleware 链
Chain = beamai_middleware_runner:init([
    {middleware_call_limit, #{max_model_calls => 10}},
    {middleware_tool_retry, #{max_retries => 3}}
]),

%% 直接执行钩子
FilterCtx = #{function => #{name => <<"my_tool">>}, args => #{}},
Result = beamai_middleware_runner:run_hook(pre_invocation, FilterCtx, Chain).
```

### 管理 Middleware 状态

```erlang
%% 获取 Middleware 内部状态
{ok, State} = beamai_middleware_runner:get_middleware_state(middleware_call_limit, Chain).

%% 更新 Middleware 内部状态
NewChain = beamai_middleware_runner:set_middleware_state(
    middleware_call_limit,
    State#{model_call_count => 0},  %% 重置计数
    Chain
).
```

### 修改上下文传递

```erlang
%% 在 pre_invocation 中修改参数
pre_invocation(FilterCtx, _MwState) ->
    Args = maps:get(args, FilterCtx, #{}),
    %% 添加默认参数
    NewArgs = maps:merge(#{<<"timeout">> => 30000}, Args),
    {continue, FilterCtx#{args => NewArgs}}.
```

### 条件跳过

```erlang
%% 跳过工具执行，返回缓存结果
pre_invocation(FilterCtx, #{cache := Cache} = _MwState) ->
    FuncName = maps:get(name, maps:get(function, FilterCtx, #{}), <<>>),
    Args = maps:get(args, FilterCtx, #{}),
    CacheKey = {FuncName, Args},
    case maps:get(CacheKey, Cache, undefined) of
        undefined -> ok;  %% 缓存未命中，继续执行
        CachedResult -> {skip, CachedResult}  %% 返回缓存结果
    end.
```

---

## API 参考

### beamai_middleware 行为

```erlang
-type middleware_state() :: map().
-type hook_name() :: pre_chat | post_chat | pre_invocation | post_invocation.
-type middleware_result() :: ok
                           | {continue, UpdatedFilterCtx :: map()}
                           | {skip, Term :: term()}
                           | {error, Reason :: term()}.

%% 所有回调都是可选的
-callback init(Opts :: map()) -> middleware_state().
-callback pre_chat(FilterCtx :: map(), MwState :: middleware_state()) -> middleware_result().
-callback post_chat(FilterCtx :: map(), MwState :: middleware_state()) -> middleware_result().
-callback pre_invocation(FilterCtx :: map(), MwState :: middleware_state()) -> middleware_result().
-callback post_invocation(FilterCtx :: map(), MwState :: middleware_state()) -> middleware_result().
```

### beamai_middleware_runner

```erlang
%% 初始化 Middleware 链
-spec init([middleware_spec()]) -> middleware_chain().

%% 转换为 Kernel Filters
-spec to_filters(middleware_chain()) -> [beamai_filter:filter_def()].

%% 执行钩子
-spec run_hook(hook_name(), FilterCtx :: map(), middleware_chain()) ->
    ok | {continue, map()} | {skip, term()} | {error, term()}.

%% 获取/设置 Middleware 状态
-spec get_middleware_state(module(), middleware_chain()) -> {ok, state()} | {error, not_found}.
-spec set_middleware_state(module(), state(), middleware_chain()) -> middleware_chain().
```

### beamai_middleware_presets

```erlang
%% 预设配置
-spec default() -> [middleware_spec()].
-spec default(map()) -> [middleware_spec()].
-spec minimal() -> [middleware_spec()].
-spec minimal(map()) -> [middleware_spec()].
-spec production() -> [middleware_spec()].
-spec production(map()) -> [middleware_spec()].
-spec development() -> [middleware_spec()].
-spec development(map()) -> [middleware_spec()].
-spec human_in_loop() -> [middleware_spec()].
-spec human_in_loop(map()) -> [middleware_spec()].

%% 单独 Middleware 配置
-spec call_limit() -> middleware_spec().
-spec call_limit(map()) -> middleware_spec().
-spec human_approval() -> middleware_spec().
-spec human_approval(map()) -> middleware_spec().
-spec tool_retry() -> middleware_spec().
-spec tool_retry(map()) -> middleware_spec().
-spec model_retry() -> middleware_spec().
-spec model_retry(map()) -> middleware_spec().
-spec model_fallback() -> middleware_spec().
-spec model_fallback(map()) -> middleware_spec().
```

---

## 更多资源

- [beamai_plugin README](../apps/beamai_plugin/README.md) - Plugin 模块文档
- [beamai_core README](../apps/beamai_core/README.md) - Kernel 架构文档
- [API 参考](API_REFERENCE.md) - API 参考文档
