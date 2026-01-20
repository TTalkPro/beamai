# Middleware 系统文档

[English](MIDDLEWARE_EN.md) | 中文

beamai_agent 的 Middleware 系统提供了一种灵活的方式来拦截、修改和控制 Agent 执行的各个阶段。

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

Middleware 是 Agent 执行过程中的拦截器，可以：

- **修改输入/输出**: 在 LLM 调用前后修改消息
- **控制流程**: 跳过、重试或中止执行
- **添加功能**: 日志记录、监控、人工审批等
- **实施限制**: 调用次数限制、Token 限制等

### 架构图

```
┌─────────────────────────────────────────────────────────────┐
│                        Agent 执行                            │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐                                           │
│  │ before_agent │  ← Agent 开始前                            │
│  └──────┬───────┘                                           │
│         │                                                    │
│         ▼                                                    │
│  ┌────────────────────────────────────────────────────────┐ │
│  │                    Agent Loop                          │ │
│  │  ┌──────────────┐                                      │ │
│  │  │ before_model │  ← LLM 调用前                         │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  │         ▼                                              │ │
│  │  ┌──────────────┐                                      │ │
│  │  │   LLM Call   │                                      │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  │         ▼                                              │ │
│  │  ┌──────────────┐                                      │ │
│  │  │ after_model  │  ← LLM 响应后                         │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  │         ▼                                              │ │
│  │  ┌──────────────┐                                      │ │
│  │  │ before_tools │  ← 工具执行前                         │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  │         ▼                                              │ │
│  │  ┌──────────────┐                                      │ │
│  │  │Tool Execution│                                      │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  │         ▼                                              │ │
│  │  ┌──────────────┐                                      │ │
│  │  │ after_tools  │  ← 工具执行后                         │ │
│  │  └──────┬───────┘                                      │ │
│  │         │                                              │ │
│  └─────────┴──────────────────────────────────────────────┘ │
│         │                                                    │
│         ▼                                                    │
│  ┌──────────────┐                                           │
│  │ after_agent  │  ← Agent 结束后                            │
│  └──────────────┘                                           │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## 生命周期钩子

### 钩子列表

| 钩子 | 触发时机 | 典型用途 |
|------|----------|----------|
| `before_agent` | Agent 执行开始前 | 初始化计数器、记录开始时间 |
| `after_agent` | Agent 执行结束后 | 清理资源、记录结束状态 |
| `before_model` | 每次 LLM 调用前 | 检查限制、修改 messages、添加上下文 |
| `after_model` | LLM 返回后 | 处理响应、记录日志、触发后续动作 |
| `before_tools` | 工具执行前 | 人工审批、参数验证、工具过滤 |
| `after_tools` | 工具执行后 | 结果验证、失败重试、结果转换 |

### 返回值类型

Middleware 钩子函数可以返回以下值：

```erlang
%% 无修改，继续执行
ok

%% 更新图状态
{update, #{key => value}}

%% 跳转到指定节点
{goto, model | tools | '__end__'}

%% 更新状态并跳转
{update_goto, #{key => value}, model | tools | '__end__'}

%% 中止执行并返回错误
{halt, Reason}

%% 中断等待用户确认
{interrupt, #{type => tool_approval, data => Data}}
```

---

## 内置 Middleware

### 1. middleware_call_limit - 调用限制

限制 Agent 执行过程中的各种调用次数。

```erlang
{middleware_call_limit, #{
    max_model_calls => 20,           %% 最大模型调用次数
    max_tool_calls => 50,            %% 最大工具调用总次数
    max_tool_calls_per_turn => 10,   %% 每轮最大工具调用数
    max_iterations => 15,            %% 最大迭代次数
    on_limit_exceeded => halt        %% 超限行为: halt | warn_and_continue
}}
```

### 2. middleware_summarization - 上下文摘要

自动压缩长对话历史。

```erlang
{middleware_summarization, #{
    window_size => 20,               %% 保留最近 N 条消息
    max_tokens => 4000,              %% Token 上限
    summarize => true,               %% 是否生成摘要
    compress_threshold => 30         %% 触发压缩的消息数阈值
}}
```

### 3. middleware_human_approval - 人工审批

在工具执行前请求人工确认。

```erlang
{middleware_human_approval, #{
    mode => all,                     %% all | selective | custom | none
    timeout => 60000,                %% 审批超时时间(ms)
    timeout_action => reject,        %% 超时行为: reject | approve
    tools => [<<"dangerous_tool">>]  %% selective 模式下需审批的工具
}}
```

### 4. middleware_tool_retry - 工具重试

工具执行失败时自动重试。

```erlang
{middleware_tool_retry, #{
    max_retries => 3,                %% 最大重试次数
    backoff => #{
        type => exponential,         %% 退避类型: exponential | linear | constant
        initial_delay => 1000,       %% 初始延迟(ms)
        max_delay => 30000,          %% 最大延迟(ms)
        multiplier => 2              %% 指数因子
    },
    retryable_errors => all          %% all | [error_type]
}}
```

### 5. middleware_model_retry - 模型重试

LLM 调用失败时自动重试。

```erlang
{middleware_model_retry, #{
    max_retries => 3,
    backoff => #{type => exponential, initial_delay => 1000},
    retryable_errors => [timeout, rate_limit, server_error]
}}
```

### 6. middleware_model_fallback - 模型降级

主模型失败时切换到备用模型。

```erlang
{middleware_model_fallback, #{
    fallback_models => [
        #{provider => openai, model => <<"gpt-3.5-turbo">>},
        #{provider => ollama, model => <<"llama2">>}
    ],
    trigger_errors => [rate_limit, timeout]
}}
```

### 7. middleware_pii_detection - PII 检测

检测并处理个人身份信息。

```erlang
{middleware_pii_detection, #{
    action => mask,                  %% mask | warn | block
    types => [email, phone, id_card],
    mask_char => <<"*">>
}}
```

### 8. middleware_tool_selector - 工具选择器

根据上下文动态选择可用工具。

```erlang
{middleware_tool_selector, #{
    strategy => context_based,       %% all | context_based | whitelist
    whitelist => [<<"search">>, <<"calculate">>],
    max_tools => 10
}}
```

### 9. middleware_todo_list - TODO 管理

为 Agent 提供任务追踪能力。

```erlang
{middleware_todo_list, #{
    auto_create => true,             %% 自动创建 TODO
    max_items => 20
}}
```

### 10. middleware_shell_tool - Shell 工具

提供安全的 Shell 命令执行。

```erlang
{middleware_shell_tool, #{
    allowed_commands => [<<"ls">>, <<"cat">>, <<"grep">>],
    timeout => 30000,
    sandbox => true
}}
```

### 11. middleware_file_search - 文件搜索

提供文件和代码搜索能力。

```erlang
{middleware_file_search, #{
    root_path => <<"/project">>,
    max_results => 100,
    excluded_paths => [<<"node_modules">>, <<".git">>]
}}
```

### 12. middleware_context_editing - 上下文编辑

允许动态修改对话上下文。

```erlang
{middleware_context_editing, #{
    allow_message_deletion => true,
    allow_message_modification => false
}}
```

### 13. middleware_tool_emulator - 工具模拟

在测试环境中模拟工具响应。

```erlang
{middleware_tool_emulator, #{
    enabled => true,
    responses => #{
        <<"search">> => #{result => <<"mock search result">>}
    }
}}
```

---

## 预设配置

### 使用预设

```erlang
%% 默认配置
Middlewares = beamai_middleware_presets:default().

%% 最小配置
Middlewares = beamai_middleware_presets:minimal().

%% 生产环境
Middlewares = beamai_middleware_presets:production().

%% 开发调试
Middlewares = beamai_middleware_presets:development().

%% 人工审批
Middlewares = beamai_middleware_presets:human_in_loop().
```

### 预设内容对比

| 预设 | call_limit | summarization | tool_retry | human_approval |
|------|------------|---------------|------------|----------------|
| default | ✓ | ✓ | - | - |
| minimal | ✓ | - | - | - |
| production | ✓ (严格) | ✓ | ✓ | - |
| development | ✓ (宽松) | ✓ (调试) | ✓ | - |
| human_in_loop | ✓ | ✓ | - | ✓ |

### 自定义预设选项

```erlang
%% 自定义 default 预设
Middlewares = beamai_middleware_presets:default(#{
    call_limit => #{max_model_calls => 30},
    summarization => #{window_size => 30}
}).

%% 扩展预设
Middlewares = beamai_middleware_presets:default() ++ [
    {my_custom_middleware, #{option => value}}
].
```

---

## 自定义 Middleware

### 基本结构

```erlang
-module(my_middleware).
-behaviour(beamai_middleware).

%% 导出回调函数（所有回调都是可选的）
-export([init/1, before_agent/2, after_agent/2,
         before_model/2, after_model/2,
         before_tools/2, after_tools/2]).

%% 初始化 Middleware 状态
init(Opts) ->
    #{
        my_option => maps:get(my_option, Opts, default_value),
        counter => 0
    }.

%% Agent 开始前
before_agent(State, MwState) ->
    %% State: 图状态 (graph_state)
    %% MwState: Middleware 内部状态
    ok.

%% Agent 结束后
after_agent(State, MwState) ->
    ok.

%% LLM 调用前
before_model(State, MwState) ->
    %% 示例：添加系统消息
    Messages = graph_state:get(State, messages, []),
    NewMsg = #{role => system, content => <<"Be concise.">>},
    {update, #{messages => [NewMsg | Messages]}}.

%% LLM 响应后
after_model(State, MwState) ->
    ok.

%% 工具执行前
before_tools(State, MwState) ->
    %% 示例：检查危险工具
    PendingTools = graph_state:get(State, pending_tools, []),
    case contains_dangerous_tool(PendingTools) of
        true -> {halt, dangerous_tool_blocked};
        false -> ok
    end.

%% 工具执行后
after_tools(State, MwState) ->
    ok.
```

### 完整示例：调用计数器

```erlang
-module(middleware_counter).
-behaviour(beamai_middleware).

-export([init/1, before_agent/2, before_model/2, after_agent/2]).

%% 初始化
init(Opts) ->
    #{
        max_calls => maps:get(max_calls, Opts, 10),
        current_calls => 0
    }.

%% Agent 开始 - 重置计数器
before_agent(_State, MwState) ->
    %% 将计数器存储到图状态
    {update, #{middleware_counter => 0}}.

%% 模型调用前 - 检查并递增计数
before_model(State, #{max_calls := MaxCalls} = MwState) ->
    Count = graph_state:get(State, middleware_counter, 0),
    case Count >= MaxCalls of
        true ->
            logger:warning("Middleware: Call limit exceeded (~p/~p)", [Count, MaxCalls]),
            {halt, {call_limit_exceeded, Count}};
        false ->
            {update, #{middleware_counter => Count + 1}}
    end.

%% Agent 结束 - 记录统计
after_agent(State, _MwState) ->
    FinalCount = graph_state:get(State, middleware_counter, 0),
    logger:info("Middleware: Total model calls: ~p", [FinalCount]),
    ok.
```

### 完整示例：请求日志

```erlang
-module(middleware_logger).
-behaviour(beamai_middleware).

-export([init/1, before_model/2, after_model/2, before_tools/2, after_tools/2]).

init(Opts) ->
    #{
        log_level => maps:get(log_level, Opts, info),
        include_content => maps:get(include_content, Opts, false)
    }.

before_model(State, #{log_level := Level, include_content := IncludeContent}) ->
    Messages = graph_state:get(State, messages, []),
    case IncludeContent of
        true ->
            log(Level, ">>> LLM Request: ~p messages~n~p", [length(Messages), Messages]);
        false ->
            log(Level, ">>> LLM Request: ~p messages", [length(Messages)])
    end,
    %% 记录开始时间
    {update, #{mw_model_start_time => erlang:system_time(millisecond)}}.

after_model(State, #{log_level := Level}) ->
    StartTime = graph_state:get(State, mw_model_start_time, 0),
    Duration = erlang:system_time(millisecond) - StartTime,
    Response = graph_state:get(State, last_llm_response, #{}),
    Content = maps:get(content, Response, <<>>),
    log(Level, "<<< LLM Response (~pms): ~p chars", [Duration, byte_size(Content)]),
    ok.

before_tools(State, #{log_level := Level}) ->
    Tools = graph_state:get(State, pending_tools, []),
    ToolNames = [maps:get(name, T, unknown) || T <- Tools],
    log(Level, ">>> Tools to execute: ~p", [ToolNames]),
    ok.

after_tools(State, #{log_level := Level}) ->
    Results = graph_state:get(State, tool_results, []),
    log(Level, "<<< Tool results: ~p items", [length(Results)]),
    ok.

%% 内部日志函数
log(debug, Fmt, Args) -> logger:debug(Fmt, Args);
log(info, Fmt, Args) -> logger:info(Fmt, Args);
log(warning, Fmt, Args) -> logger:warning(Fmt, Args);
log(error, Fmt, Args) -> logger:error(Fmt, Args).
```

### 完整示例：敏感词过滤

```erlang
-module(middleware_content_filter).
-behaviour(beamai_middleware).

-export([init/1, after_model/2]).

init(Opts) ->
    #{
        blocked_words => maps:get(blocked_words, Opts, []),
        replacement => maps:get(replacement, Opts, <<"[FILTERED]">>),
        action => maps:get(action, Opts, replace)  %% replace | block | warn
    }.

after_model(State, #{blocked_words := BlockedWords, replacement := Replacement, action := Action}) ->
    Response = graph_state:get(State, last_llm_response, #{}),
    Content = maps:get(content, Response, <<>>),

    case check_content(Content, BlockedWords) of
        {found, Word} ->
            case Action of
                block ->
                    {halt, {blocked_content, Word}};
                warn ->
                    logger:warning("Blocked word detected: ~p", [Word]),
                    ok;
                replace ->
                    FilteredContent = filter_content(Content, BlockedWords, Replacement),
                    NewResponse = Response#{content => FilteredContent},
                    {update, #{last_llm_response => NewResponse}}
            end;
        clean ->
            ok
    end.

check_content(Content, BlockedWords) ->
    LowerContent = string:lowercase(binary_to_list(Content)),
    case lists:filter(fun(Word) ->
        string:find(LowerContent, string:lowercase(binary_to_list(Word))) =/= nomatch
    end, BlockedWords) of
        [] -> clean;
        [First|_] -> {found, First}
    end.

filter_content(Content, BlockedWords, Replacement) ->
    lists:foldl(fun(Word, Acc) ->
        binary:replace(Acc, Word, Replacement, [global])
    end, Content, BlockedWords).
```

---

## 配置和使用

### 在 Agent 配置中使用

```erlang
%% 方式 1：使用预设
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    system_prompt => <<"You are helpful.">>,
    llm => LLMConfig,
    middlewares => beamai_middleware_presets:default()
}).

%% 方式 2：手动配置
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    system_prompt => <<"You are helpful.">>,
    llm => LLMConfig,
    middlewares => [
        {middleware_call_limit, #{max_model_calls => 15}},
        {middleware_summarization, #{window_size => 20}},
        {my_custom_middleware, #{option => value}}
    ]
}).

%% 方式 3：混合配置
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    middlewares => beamai_middleware_presets:production() ++ [
        {middleware_logger, #{log_level => debug}}
    ]
}).
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
- 推荐范围：
  - 10-30: 前置检查（限制、验证）
  - 40-60: 核心功能（审批、重试）
  - 70-90: 后置处理（日志、监控）

---

## 高级用法

### 访问图状态

```erlang
before_model(State, MwState) ->
    %% 读取状态
    Messages = graph_state:get(State, messages, []),
    Context = graph_state:get(State, context, #{}),

    %% 检查自定义键
    MyData = graph_state:get(State, my_custom_key, undefined),

    %% 更新状态
    {update, #{
        messages => Messages ++ [NewMessage],
        my_custom_key => NewValue
    }}.
```

### 流程控制

```erlang
%% 跳过工具执行，直接返回 LLM
before_tools(State, _MwState) ->
    case should_skip_tools(State) of
        true -> {goto, model};
        false -> ok
    end.

%% 立即结束 Agent
after_model(State, _MwState) ->
    case is_final_answer(State) of
        true -> {goto, '__end__'};
        false -> ok
    end.
```

### 中断和恢复

```erlang
%% 请求人工确认
before_tools(State, _MwState) ->
    Tools = graph_state:get(State, pending_tools, []),
    case needs_approval(Tools) of
        true ->
            {interrupt, #{
                type => tool_approval,
                data => #{tools => Tools},
                timeout => 60000
            }};
        false ->
            ok
    end.
```

### Middleware 间通信

```erlang
%% 通过图状态共享数据
before_model(State, _MwState) ->
    %% 设置供其他 Middleware 使用的数据
    {update, #{shared_data => #{timestamp => erlang:system_time()}}}.

after_model(State, _MwState) ->
    %% 读取其他 Middleware 设置的数据
    SharedData = graph_state:get(State, shared_data, #{}),
    %% 使用 SharedData...
    ok.
```

---

## API 参考

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

### beamai_middleware_runner

```erlang
%% 初始化 Middleware 链
-spec init([middleware_spec()]) -> middleware_chain().

%% 执行钩子
-spec run_hook(hook_name(), graph_state(), middleware_chain()) -> run_result().

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
-spec production() -> [middleware_spec()].
-spec development() -> [middleware_spec()].
-spec human_in_loop() -> [middleware_spec()].

%% 单独 Middleware 配置
-spec call_limit() -> middleware_spec().
-spec call_limit(map()) -> middleware_spec().
-spec summarization() -> middleware_spec().
-spec human_approval() -> middleware_spec().
-spec tool_retry() -> middleware_spec().
```

---

## 更多资源

- [beamai_agent README](../apps/beamai_agent/README.md)
- [API 参考](API_REFERENCE.md)
- [架构设计](ARCHITECTURE.md)
