# Callback 系统文档

[English](CALLBACKS_EN.md) | 中文

BeamAI Framework 提供了类似 LangChain 的事件驱动回调系统，用于监听和响应 Agent 执行过程中的各种事件。

## 目录

- [概述](#概述)
- [回调类型](#回调类型)
- [使用方法](#使用方法)
- [回调元数据](#回调元数据)
- [API 参考](#api-参考)
- [使用示例](#使用示例)
- [最佳实践](#最佳实践)
- [扩展开发](#扩展开发)

---

## 概述

Callback 系统是 BeamAI Agent 的核心组件之一，允许开发者在 Agent 执行的关键节点注入自定义逻辑，用于：

- **监控和日志**: 记录 LLM 调用、工具执行等事件
- **调试**: 追踪 Agent 执行流程，定位问题
- **集成**: 与外部系统（监控、分析、通知）集成
- **扩展**: 在不修改核心代码的情况下添加功能

### 架构图

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           Agent 执行流程                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌──────────────────┐                                                   │
│  │  on_chain_start  │  ← Agent 执行开始                                  │
│  └────────┬─────────┘                                                   │
│           │                                                              │
│           ▼                                                              │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │                         执行循环                                    │ │
│  │  ┌──────────────────┐                                              │ │
│  │  │  on_llm_start    │  ← LLM 调用开始                               │ │
│  │  └────────┬─────────┘                                              │ │
│  │           │                                                        │ │
│  │           ▼                                                        │ │
│  │  ┌──────────────────┐     ┌──────────────────┐                     │ │
│  │  │    LLM 调用      │ ──► │  on_llm_new_token│  ← 流式 Token       │ │
│  │  └────────┬─────────┘     └──────────────────┘                     │ │
│  │           │                                                        │ │
│  │           ▼                                                        │ │
│  │  ┌──────────────────┐     ┌──────────────────┐                     │ │
│  │  │  on_llm_end      │  或 │  on_llm_error    │                     │ │
│  │  └────────┬─────────┘     └──────────────────┘                     │ │
│  │           │                                                        │ │
│  │           ▼                                                        │ │
│  │  ┌──────────────────┐                                              │ │
│  │  │    on_text       │  ← 文本内容生成                               │ │
│  │  └────────┬─────────┘                                              │ │
│  │           │                                                        │ │
│  │           ▼                                                        │ │
│  │  ┌──────────────────┐                                              │ │
│  │  │ on_agent_action  │  ← Agent 决定执行动作                         │ │
│  │  └────────┬─────────┘                                              │ │
│  │           │                                                        │ │
│  │           ▼                                                        │ │
│  │  ┌──────────────────┐                                              │ │
│  │  │  on_tool_start   │  ← 工具执行开始                               │ │
│  │  └────────┬─────────┘                                              │ │
│  │           │                                                        │ │
│  │           ▼                                                        │ │
│  │  ┌──────────────────┐     ┌──────────────────┐                     │ │
│  │  │  on_tool_end     │  或 │  on_tool_error   │                     │ │
│  │  └────────┬─────────┘     └──────────────────┘                     │ │
│  │           │                                                        │ │
│  └───────────┴────────────────────────────────────────────────────────┘ │
│           │                                                              │
│           ▼                                                              │
│  ┌──────────────────┐     ┌──────────────────┐                          │
│  │ on_agent_finish  │  或 │  on_chain_error  │                          │
│  └──────────────────┘     └──────────────────┘                          │
│           │                                                              │
│           ▼                                                              │
│  ┌──────────────────┐                                                   │
│  │  on_chain_end    │  ← Agent 执行结束                                  │
│  └──────────────────┘                                                   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### 核心模块

| 模块 | 位置 | 说明 |
|------|------|------|
| `beamai_agent_callbacks` | `apps/beamai_agent/src/` | 回调管理器 |
| `beamai_callback_utils` | `apps/beamai_core/src/utils/` | 回调工具函数 |
| `beamai_agent.hrl` | `apps/beamai_agent/include/` | 回调类型定义 |

---

## 回调类型

BeamAI 支持 18 种回调类型，覆盖完整的 Agent 生命周期。

### LLM 回调（4 种）

| 回调名称 | 触发时机 | 参数 |
|----------|----------|------|
| `on_llm_start` | LLM 调用开始 | `(Prompts, Meta)` |
| `on_llm_end` | LLM 调用成功 | `(Response, Meta)` |
| `on_llm_error` | LLM 调用失败 | `(Error, Meta)` |
| `on_llm_new_token` | 流式输出新 Token | `(Token, Meta)` |

```erlang
%% LLM 回调示例
#{
    on_llm_start => fun(Prompts, Meta) ->
        io:format("LLM 调用开始，消息数: ~p~n", [length(Prompts)])
    end,
    on_llm_end => fun(Response, Meta) ->
        Content = maps:get(content, Response, <<>>),
        io:format("LLM 响应: ~ts~n", [Content])
    end,
    on_llm_error => fun(Error, Meta) ->
        logger:error("LLM 调用失败: ~p", [Error])
    end
}
```

### Tool 回调（3 种）

| 回调名称 | 触发时机 | 参数 |
|----------|----------|------|
| `on_tool_start` | 工具执行开始 | `(ToolName, Args, Meta)` |
| `on_tool_end` | 工具执行成功 | `(ToolName, Result, Meta)` |
| `on_tool_error` | 工具执行失败 | `(ToolName, Error, Meta)` |

```erlang
%% Tool 回调示例
#{
    on_tool_start => fun(ToolName, Args, Meta) ->
        io:format("执行工具: ~ts~n参数: ~p~n", [ToolName, Args])
    end,
    on_tool_end => fun(ToolName, Result, Meta) ->
        io:format("工具 ~ts 完成: ~ts~n", [ToolName, Result])
    end,
    on_tool_error => fun(ToolName, Error, Meta) ->
        logger:warning("工具 ~ts 失败: ~p", [ToolName, Error])
    end
}
```

### Agent 回调（2 种）

| 回调名称 | 触发时机 | 参数 |
|----------|----------|------|
| `on_agent_action` | Agent 决定执行动作 | `(Action, Meta)` |
| `on_agent_finish` | Agent 完成（无工具调用） | `(Result, Meta)` |

```erlang
%% Agent 回调示例
#{
    on_agent_action => fun(Action, Meta) ->
        %% Action 包含工具调用信息
        ToolCalls = maps:get(tool_calls, Action, []),
        io:format("Agent 动作: ~p 个工具调用~n", [length(ToolCalls)])
    end,
    on_agent_finish => fun(Result, Meta) ->
        Content = maps:get(content, Result, <<>>),
        io:format("Agent 完成: ~ts~n", [Content])
    end
}
```

### Chain 回调（3 种）

| 回调名称 | 触发时机 | 参数 |
|----------|----------|------|
| `on_chain_start` | Chain/Agent 执行开始 | `(Input, Meta)` |
| `on_chain_end` | Chain/Agent 执行成功 | `(Output, Meta)` |
| `on_chain_error` | Chain/Agent 执行失败 | `(Error, Meta)` |

```erlang
%% Chain 回调示例
#{
    on_chain_start => fun(Input, Meta) ->
        io:format("开始执行，输入: ~ts~n", [Input])
    end,
    on_chain_end => fun(Output, Meta) ->
        io:format("执行完成~n")
    end,
    on_chain_error => fun(Error, Meta) ->
        logger:error("执行失败: ~p", [Error])
    end
}
```

### Retriever 回调（3 种）- RAG 相关

| 回调名称 | 触发时机 | 参数 |
|----------|----------|------|
| `on_retriever_start` | 检索开始 | `(Query, Meta)` |
| `on_retriever_end` | 检索成功 | `(Documents, Meta)` |
| `on_retriever_error` | 检索失败 | `(Error, Meta)` |

```erlang
%% Retriever 回调示例
#{
    on_retriever_start => fun(Query, Meta) ->
        io:format("开始检索: ~ts~n", [Query])
    end,
    on_retriever_end => fun(Documents, Meta) ->
        io:format("检索到 ~p 个文档~n", [length(Documents)])
    end
}
```

### 其他回调（3 种）

| 回调名称 | 触发时机 | 参数 |
|----------|----------|------|
| `on_text` | 生成文本内容 | `(Text, Meta)` |
| `on_retry` | 重试时触发 | `(RetryState, Meta)` |
| `on_custom_event` | 自定义事件 | `(EventName, Data, Meta)` |

```erlang
%% 其他回调示例
#{
    on_text => fun(Text, Meta) ->
        %% 仅当内容非空时触发
        io:format("生成文本: ~ts~n", [Text])
    end,
    on_retry => fun(RetryState, Meta) ->
        io:format("重试: ~p~n", [RetryState])
    end,
    on_custom_event => fun(EventName, Data, Meta) ->
        io:format("自定义事件 ~p: ~p~n", [EventName, Data])
    end
}
```

---

## 使用方法

### 初始化时设置回调

```erlang
{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    system_prompt => <<"你是一个助手"/utf8>>,
    llm => LLMConfig,
    callbacks => #{
        on_llm_start => fun(Prompts, Meta) ->
            io:format("LLM 开始~n")
        end,
        on_llm_end => fun(Response, Meta) ->
            io:format("LLM 结束~n")
        end,
        on_tool_start => fun(ToolName, Args, Meta) ->
            io:format("工具: ~ts~n", [ToolName])
        end
    }
}).
```

### 动态设置回调

```erlang
%% 设置新的回调
ok = beamai_agent:set_callbacks(Agent, #{
    on_llm_start => fun(Prompts, Meta) ->
        io:format("新的 LLM 回调~n")
    end
}).

%% 获取当前回调配置
CallbacksMap = beamai_agent:get_callbacks(Agent).
```

### 发送自定义事件

```erlang
%% 发送自定义事件
beamai_agent:emit_custom_event(Agent, my_event, #{value => 42}).

%% 发送带元数据的自定义事件
beamai_agent:emit_custom_event(Agent, my_event, #{value => 42}, #{
    source => <<"my_module">>
}).
```

### 向后兼容

支持旧的 `on_complete` 和 `on_error` 选项：

```erlang
%% 旧的配置方式（仍然支持）
{ok, Agent} = beamai_agent:start_link(<<"agent">>, #{
    on_complete => fun(Result) -> io:format("完成: ~p~n", [Result]) end,
    on_error => fun(Error) -> io:format("错误: ~p~n", [Error]) end
}).

%% 内部自动转换为：
%% on_complete -> on_agent_finish
%% on_error -> on_chain_error
```

---

## 回调元数据

每个回调都会收到一个 `Meta` 参数，包含执行上下文信息：

```erlang
Meta = #{
    agent_id => <<"agent_123">>,     %% Agent ID
    agent_name => <<"my_agent">>,    %% Agent 名称
    run_id => <<"uuid-...">>,        %% 当前运行 ID（UUID 格式）
    timestamp => 1705658400000       %% 毫秒级时间戳
}.
```

### 使用元数据

```erlang
#{
    on_llm_start => fun(Prompts, Meta) ->
        AgentName = maps:get(agent_name, Meta),
        RunId = maps:get(run_id, Meta),
        Timestamp = maps:get(timestamp, Meta),
        logger:info("[~ts] LLM 开始 (run: ~ts, time: ~p)",
            [AgentName, RunId, Timestamp])
    end
}
```

---

## API 参考

### beamai_agent 回调 API

```erlang
%% 设置回调处理器
-spec set_callbacks(pid(), map()) -> ok.
beamai_agent:set_callbacks(Agent, CallbackOpts).

%% 获取当前回调配置
-spec get_callbacks(pid()) -> map().
beamai_agent:get_callbacks(Agent).

%% 发送自定义事件
-spec emit_custom_event(pid(), atom() | binary(), term()) -> ok.
beamai_agent:emit_custom_event(Agent, EventName, Data).

%% 发送自定义事件（带元数据）
-spec emit_custom_event(pid(), atom() | binary(), term(), map()) -> ok.
beamai_agent:emit_custom_event(Agent, EventName, Data, Metadata).
```

### beamai_agent_callbacks 内部 API

```erlang
%% 初始化回调处理器
-spec init(map()) -> #callbacks{}.
beamai_agent_callbacks:init(Opts).

%% 更新回调处理器
-spec update(#callbacks{}, map()) -> #callbacks{}.
beamai_agent_callbacks:update(Callbacks, Opts).

%% 调用回调函数
-spec invoke(atom(), list(), #callbacks{}) -> ok.
beamai_agent_callbacks:invoke(CallbackName, Args, Callbacks).

%% 将回调记录转换为 map
-spec to_map(#callbacks{}) -> map().
beamai_agent_callbacks:to_map(Callbacks).

%% 构建回调元数据
-spec build_metadata(#state{}) -> map().
beamai_agent_callbacks:build_metadata(State).

%% 生成运行 ID
-spec generate_run_id() -> binary().
beamai_agent_callbacks:generate_run_id().
```

### beamai_callback_utils 工具函数

```erlang
%% 调用回调函数（不带元数据）
-spec invoke(atom(), list(), map()) -> ok.
beamai_callback_utils:invoke(CallbackName, Args, Callbacks).

%% 调用回调函数（带元数据）
-spec invoke(atom(), list(), map(), map()) -> ok.
beamai_callback_utils:invoke(CallbackName, Args, Callbacks, Meta).

%% 从图状态调用回调
-spec invoke_from_state(atom(), list(), map()) -> ok.
beamai_callback_utils:invoke_from_state(CallbackName, Args, State).

%% 条件调用回调
-spec maybe_invoke(boolean(), atom(), list(), map()) -> ok.
beamai_callback_utils:maybe_invoke(Condition, CallbackName, Args, State).
```

### 调用宏

```erlang
%% 在 beamai_common.hrl 中定义

%% 直接调用回调
?INVOKE_CALLBACK(Name, Args, Callbacks, Meta)

%% 从图状态调用回调
?INVOKE_CALLBACK_FROM_STATE(Name, Args, State)
```

---

## 使用示例

### 示例 1：日志记录

```erlang
%% 创建日志回调
LogCallbacks = #{
    on_llm_start => fun(Prompts, Meta) ->
        logger:info("[~ts] LLM 开始，消息数: ~p",
            [maps:get(agent_name, Meta), length(Prompts)])
    end,
    on_llm_end => fun(Response, Meta) ->
        logger:info("[~ts] LLM 完成",
            [maps:get(agent_name, Meta)])
    end,
    on_tool_start => fun(ToolName, Args, Meta) ->
        logger:info("[~ts] 工具 ~ts 开始",
            [maps:get(agent_name, Meta), ToolName])
    end,
    on_tool_end => fun(ToolName, Result, Meta) ->
        logger:info("[~ts] 工具 ~ts 完成",
            [maps:get(agent_name, Meta), ToolName])
    end
}.

{ok, Agent} = beamai_agent:start_link(<<"log_agent">>, #{
    system_prompt => <<"...">>,
    llm => LLMConfig,
    callbacks => LogCallbacks
}).
```

### 示例 2：性能监控

```erlang
%% 创建性能监控回调
PerfCallbacks = #{
    on_llm_start => fun(_Prompts, Meta) ->
        %% 记录开始时间到进程字典
        put(llm_start_time, erlang:system_time(millisecond))
    end,
    on_llm_end => fun(Response, Meta) ->
        StartTime = get(llm_start_time),
        Duration = erlang:system_time(millisecond) - StartTime,
        %% 发送到监控系统
        metrics:histogram(<<"llm.duration_ms">>, Duration),
        logger:info("LLM 耗时: ~p ms", [Duration])
    end,
    on_tool_start => fun(ToolName, _Args, _Meta) ->
        put({tool_start_time, ToolName}, erlang:system_time(millisecond))
    end,
    on_tool_end => fun(ToolName, _Result, _Meta) ->
        StartTime = get({tool_start_time, ToolName}),
        Duration = erlang:system_time(millisecond) - StartTime,
        metrics:histogram(<<"tool.duration_ms">>, Duration, #{tool => ToolName})
    end
}.
```

### 示例 3：进度通知

```erlang
%% 创建进度通知回调（如发送到 WebSocket）
NotifyCallbacks = #{
    on_chain_start => fun(Input, Meta) ->
        notify_client(maps:get(run_id, Meta), #{
            type => <<"start">>,
            input => Input
        })
    end,
    on_llm_new_token => fun(Token, Meta) ->
        notify_client(maps:get(run_id, Meta), #{
            type => <<"token">>,
            content => Token
        })
    end,
    on_tool_start => fun(ToolName, Args, Meta) ->
        notify_client(maps:get(run_id, Meta), #{
            type => <<"tool_start">>,
            tool => ToolName
        })
    end,
    on_chain_end => fun(Output, Meta) ->
        notify_client(maps:get(run_id, Meta), #{
            type => <<"end">>,
            output => Output
        })
    end
}.

notify_client(RunId, Message) ->
    websocket_handler:send(RunId, jsx:encode(Message)).
```

### 示例 4：调试追踪

```erlang
%% 创建调试回调
DebugCallbacks = #{
    on_llm_start => fun(Prompts, Meta) ->
        io:format("~n=== LLM 调用开始 ===~n"),
        io:format("Agent: ~ts~n", [maps:get(agent_name, Meta)]),
        io:format("消息数: ~p~n", [length(Prompts)]),
        lists:foreach(fun(Msg) ->
            Role = maps:get(role, Msg),
            Content = maps:get(content, Msg, <<>>),
            io:format("  [~ts] ~ts~n", [Role, truncate(Content, 100)])
        end, Prompts)
    end,
    on_llm_end => fun(Response, Meta) ->
        io:format("~n=== LLM 响应 ===~n"),
        Content = maps:get(content, Response, <<>>),
        ToolCalls = maps:get(tool_calls, Response, []),
        io:format("内容: ~ts~n", [truncate(Content, 200)]),
        io:format("工具调用: ~p 个~n", [length(ToolCalls)])
    end,
    on_tool_start => fun(ToolName, Args, _Meta) ->
        io:format("~n>>> 执行工具: ~ts~n", [ToolName]),
        io:format("    参数: ~p~n", [Args])
    end,
    on_tool_end => fun(ToolName, Result, _Meta) ->
        io:format("<<< 工具完成: ~ts~n", [ToolName]),
        io:format("    结果: ~ts~n", [truncate(Result, 100)])
    end
}.

truncate(Bin, MaxLen) when byte_size(Bin) > MaxLen ->
    <<(binary:part(Bin, 0, MaxLen))/binary, "...">>;
truncate(Bin, _) -> Bin.
```

### 示例 5：异步事件处理

```erlang
%% 使用进程消息进行异步处理
Self = self(),

AsyncCallbacks = #{
    on_llm_end => fun(Response, Meta) ->
        Self ! {llm_complete, maps:get(run_id, Meta), Response}
    end,
    on_tool_end => fun(ToolName, Result, Meta) ->
        Self ! {tool_complete, maps:get(run_id, Meta), ToolName, Result}
    end,
    on_chain_end => fun(Output, Meta) ->
        Self ! {agent_complete, maps:get(run_id, Meta), Output}
    end
}.

%% 异步接收事件
receive
    {llm_complete, RunId, Response} ->
        handle_llm_response(RunId, Response);
    {tool_complete, RunId, ToolName, Result} ->
        handle_tool_result(RunId, ToolName, Result);
    {agent_complete, RunId, Output} ->
        handle_agent_output(RunId, Output)
after 30000 ->
    timeout
end.
```

---

## 最佳实践

### 1. 保持回调轻量

回调应该快速执行，避免阻塞 Agent 主流程：

```erlang
%% 推荐：异步处理
on_llm_end => fun(Response, Meta) ->
    spawn(fun() -> process_response(Response, Meta) end)
end

%% 避免：同步阻塞操作
on_llm_end => fun(Response, Meta) ->
    %% 这会阻塞 Agent
    httpc:request(post, {Url, [], "application/json", Body}, [], [])
end
```

### 2. 处理回调异常

回调内部的异常不会影响 Agent 执行，但建议添加错误处理：

```erlang
on_llm_end => fun(Response, Meta) ->
    try
        process_response(Response)
    catch
        Class:Reason:Stack ->
            logger:warning("回调处理失败: ~p:~p~n~p",
                [Class, Reason, Stack])
    end
end
```

### 3. 使用元数据关联事件

利用 `run_id` 关联同一次执行的所有事件：

```erlang
%% 使用 ETS 存储执行上下文
on_chain_start => fun(Input, Meta) ->
    RunId = maps:get(run_id, Meta),
    ets:insert(run_context, {RunId, #{
        start_time => erlang:system_time(millisecond),
        input => Input
    }})
end,

on_chain_end => fun(Output, Meta) ->
    RunId = maps:get(run_id, Meta),
    [{_, Context}] = ets:lookup(run_context, RunId),
    Duration = erlang:system_time(millisecond) - maps:get(start_time, Context),
    %% 记录完整的执行信息
    log_execution(RunId, Context, Output, Duration),
    ets:delete(run_context, RunId)
end
```

### 4. 动态启用/禁用回调

```erlang
%% 根据配置决定是否启用回调
Callbacks = case os:getenv("DEBUG") of
    "true" -> debug_callbacks();
    _ -> #{}
end,

{ok, Agent} = beamai_agent:start_link(<<"agent">>, #{
    callbacks => Callbacks
}).
```

### 5. 组合多个回调处理器

```erlang
%% 合并多个回调配置
merge_callbacks(Callbacks1, Callbacks2) ->
    maps:fold(fun(Key, Handler2, Acc) ->
        case maps:get(Key, Acc, undefined) of
            undefined ->
                maps:put(Key, Handler2, Acc);
            Handler1 ->
                %% 创建组合处理器
                Combined = fun(Args...) ->
                    Handler1(Args...),
                    Handler2(Args...)
                end,
                maps:put(Key, Combined, Acc)
        end
    end, Callbacks1, Callbacks2).

%% 使用
AllCallbacks = merge_callbacks(
    merge_callbacks(LogCallbacks, PerfCallbacks),
    NotifyCallbacks
).
```

---

## 扩展开发

### 创建自定义回调处理器模块

```erlang
-module(my_callback_handler).
-export([callbacks/0, callbacks/1]).

%% 默认回调配置
callbacks() ->
    callbacks(#{}).

%% 带选项的回调配置
callbacks(Opts) ->
    LogLevel = maps:get(log_level, Opts, info),
    #{
        on_llm_start => fun(Prompts, Meta) ->
            log(LogLevel, "LLM 开始: ~p 条消息", [length(Prompts)])
        end,
        on_llm_end => fun(Response, Meta) ->
            log(LogLevel, "LLM 结束", [])
        end,
        on_tool_start => fun(ToolName, Args, Meta) ->
            log(LogLevel, "工具 ~ts 开始", [ToolName])
        end,
        on_tool_end => fun(ToolName, Result, Meta) ->
            log(LogLevel, "工具 ~ts 结束", [ToolName])
        end
    }.

log(debug, Fmt, Args) -> logger:debug(Fmt, Args);
log(info, Fmt, Args) -> logger:info(Fmt, Args);
log(warning, Fmt, Args) -> logger:warning(Fmt, Args).
```

### 使用自定义处理器

```erlang
{ok, Agent} = beamai_agent:start_link(<<"agent">>, #{
    callbacks => my_callback_handler:callbacks(#{log_level => debug})
}).
```

---

## 更多资源

- [ARCHITECTURE.md](ARCHITECTURE.md) - 架构设计文档
- [MIDDLEWARE.md](MIDDLEWARE.md) - Middleware 系统文档
- [API_REFERENCE.md](API_REFERENCE.md) - API 参考文档
- [beamai_agent README](../apps/beamai_agent/README.md) - Agent 模块文档
