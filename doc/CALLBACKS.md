# Callback 系统文档

[English](CALLBACKS_EN.md) | 中文

BeamAI Agent 提供事件驱动的回调系统，用于监听和响应 Agent 执行过程中的各种事件。

## 目录

- [概述](#概述)
- [回调类型](#回调类型)
- [使用方法](#使用方法)
- [回调元数据](#回调元数据)
- [API 参考](#api-参考)
- [使用示例](#使用示例)
- [最佳实践](#最佳实践)

---

## 概述

Callback 系统是 BeamAI Agent 的核心组件之一，允许开发者在 Agent 执行的关键节点注入自定义逻辑，用于：

- **监控和日志**: 记录 LLM 调用、工具执行等事件
- **调试**: 追踪 Agent 执行流程，定位问题
- **流式输出**: 实时接收 LLM 生成的 Token
- **中断控制**: 通过工具回调中断执行
- **集成**: 与外部系统（监控、通知）集成

### 架构图

```
┌─────────────────────────────────────────────────────────────────────┐
│                           Agent 执行流程                              │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌──────────────────┐                                               │
│  │  on_turn_start   │  ← 每轮开始                                   │
│  └────────┬─────────┘                                               │
│           │                                                          │
│           ▼                                                          │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │                         执行循环                                │ │
│  │  ┌──────────────────┐                                          │ │
│  │  │   on_llm_call    │  ← LLM 调用                              │ │
│  │  └────────┬─────────┘                                          │ │
│  │           │                                                    │ │
│  │           ▼                                                    │ │
│  │  ┌──────────────────┐                                          │ │
│  │  │    on_token      │  ← 流式 Token（逐个）                    │ │
│  │  └────────┬─────────┘                                          │ │
│  │           │                                                    │ │
│  │           ▼                                                    │ │
│  │  ┌──────────────────┐                                          │ │
│  │  │  on_tool_call    │  ← 工具调用（可返回 interrupt）           │ │
│  │  └────────┬─────────┘                                          │ │
│  │           │                                                    │ │
│  └───────────┴────────────────────────────────────────────────────┘ │
│           │                                                          │
│           ▼                                                          │
│  ┌──────────────────┐     ┌──────────────────┐                      │
│  │  on_turn_end     │  或 │ on_turn_error    │                      │
│  └──────────────────┘     └──────────────────┘                      │
│                                                                      │
│  ┌──────────────────┐     ┌──────────────────┐                      │
│  │  on_interrupt    │     │   on_resume      │  ← 中断/恢复事件     │
│  └──────────────────┘     └──────────────────┘                      │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### 核心模块

| 模块 | 位置 | 说明 |
|------|------|------|
| `beamai_agent_callbacks` | `apps/beamai_agent/src/` | 回调管理和调用 |

---

## 回调类型

BeamAI 支持 8 种回调类型，覆盖 Agent 执行的关键生命周期。

### 回调列表

| 回调名称 | 触发时机 | 参数 | 返回值 |
|----------|----------|------|--------|
| `on_turn_start` | 每轮执行开始 | `(Metadata)` | `ok` |
| `on_turn_end` | 每轮执行结束 | `(Metadata)` | `ok` |
| `on_turn_error` | 每轮执行出错 | `(Error, Metadata)` | `ok` |
| `on_llm_call` | LLM 调用时 | `(Messages, Metadata)` | `ok` |
| `on_tool_call` | 工具调用时 | `(FunctionName, Args)` | `ok \| {interrupt, Reason}` |
| `on_token` | 流式 Token 生成 | `(TokenText, Metadata)` | `ok` |
| `on_interrupt` | Agent 被中断 | `(InterruptState, Metadata)` | `ok` |
| `on_resume` | Agent 从中断恢复 | `(InterruptState, Metadata)` | `ok` |

### Turn 回调（3 种）

每轮 Agent 执行的生命周期事件：

```erlang
#{
    on_turn_start => fun(Meta) ->
        io:format("第 ~p 轮开始~n", [maps:get(turn_count, Meta)])
    end,
    on_turn_end => fun(Meta) ->
        io:format("第 ~p 轮结束~n", [maps:get(turn_count, Meta)])
    end,
    on_turn_error => fun(Error, Meta) ->
        logger:error("执行错误: ~p", [Error])
    end
}
```

### LLM 回调（1 种）

LLM 调用事件：

```erlang
#{
    on_llm_call => fun(Messages, Meta) ->
        io:format("LLM 调用，消息数: ~p~n", [length(Messages)])
    end
}
```

### Tool 回调（1 种）

工具调用事件。特别地，`on_tool_call` 可以返回 `{interrupt, Reason}` 来中断执行：

```erlang
#{
    on_tool_call => fun(FunctionName, Args) ->
        io:format("调用工具: ~ts~n", [FunctionName]),
        case FunctionName of
            <<"dangerous_tool">> ->
                %% 中断执行，等待人工确认
                {interrupt, #{reason => require_approval, tool => FunctionName}};
            _ ->
                ok
        end
    end
}
```

### Token 回调（1 种）

流式输出时逐个 Token 生成的事件：

```erlang
#{
    on_token => fun(TokenText, Meta) ->
        io:format("~ts", [TokenText])  %% 实时输出
    end
}
```

### 中断/恢复回调（2 种）

Agent 中断和恢复事件：

```erlang
#{
    on_interrupt => fun(InterruptState, Meta) ->
        io:format("Agent 被中断: ~p~n", [InterruptState])
    end,
    on_resume => fun(InterruptState, Meta) ->
        io:format("Agent 恢复执行~n")
    end
}
```

---

## 使用方法

### 在 Agent 创建时设置回调

```erlang
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

{ok, State} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"你是一个助手"/utf8>>,
    callbacks => #{
        on_turn_start => fun(Meta) ->
            io:format("开始执行~n")
        end,
        on_llm_call => fun(Messages, Meta) ->
            io:format("LLM 调用，~p 条消息~n", [length(Messages)])
        end,
        on_tool_call => fun(FuncName, Args) ->
            io:format("工具: ~ts~n", [FuncName]),
            ok
        end,
        on_turn_end => fun(Meta) ->
            io:format("执行完成~n")
        end
    }
}),

{ok, Result, _NewState} = beamai_agent:run(State, <<"你好"/utf8>>).
```

---

## 回调元数据

每个回调都会收到一个 `Metadata` 参数（`on_tool_call` 除外），包含执行上下文信息：

```erlang
Metadata = #{
    agent_id => <<"agent_123">>,     %% Agent ID
    agent_name => <<"my_agent">>,    %% Agent 名称
    turn_count => 1,                 %% 当前轮次
    timestamp => 1705658400000       %% 毫秒级时间戳
}.
```

### 使用元数据

```erlang
#{
    on_llm_call => fun(Messages, Meta) ->
        AgentName = maps:get(agent_name, Meta, <<"unknown">>),
        TurnCount = maps:get(turn_count, Meta, 0),
        logger:info("[~ts] 第 ~p 轮 LLM 调用，~p 条消息",
            [AgentName, TurnCount, length(Messages)])
    end
}
```

---

## API 参考

### beamai_agent_callbacks

```erlang
-type callbacks() :: #{
    on_turn_start => fun((map()) -> ok),
    on_turn_end => fun((map()) -> ok),
    on_turn_error => fun((term(), map()) -> ok),
    on_llm_call => fun((list(), map()) -> ok),
    on_tool_call => fun((binary(), map()) -> ok | {interrupt, term()}),
    on_token => fun((binary(), map()) -> ok),
    on_interrupt => fun((term(), map()) -> ok),
    on_resume => fun((term(), map()) -> ok)
}.

%% 安全调用回调（异常不影响 Agent 执行）
-spec invoke(atom(), list(), callbacks()) -> ok.
beamai_agent_callbacks:invoke(CallbackName, Args, Callbacks).

%% 构建回调元数据
-spec build_metadata(agent_state()) -> map().
beamai_agent_callbacks:build_metadata(AgentState).
```

### 回调安全机制

回调系统具备以下安全特性：

- **异常隔离**: 回调函数内的异常会被捕获，不会影响 Agent 主流程
- **未注册忽略**: 调用未注册的回调名称直接返回 `ok`
- **可选回调**: 所有回调都是可选的，只需注册需要的即可

---

## 使用示例

### 示例 1：日志记录

```erlang
LogCallbacks = #{
    on_turn_start => fun(Meta) ->
        logger:info("[~ts] 第 ~p 轮开始",
            [maps:get(agent_name, Meta, <<>>), maps:get(turn_count, Meta, 0)])
    end,
    on_llm_call => fun(Messages, Meta) ->
        logger:info("[~ts] LLM 调用，~p 条消息",
            [maps:get(agent_name, Meta, <<>>), length(Messages)])
    end,
    on_tool_call => fun(FuncName, _Args) ->
        logger:info("工具调用: ~ts", [FuncName]),
        ok
    end,
    on_turn_end => fun(Meta) ->
        logger:info("[~ts] 第 ~p 轮结束",
            [maps:get(agent_name, Meta, <<>>), maps:get(turn_count, Meta, 0)])
    end
}.

{ok, State} = beamai_agent:new(#{
    llm => LLM,
    callbacks => LogCallbacks
}).
```

### 示例 2：性能监控

```erlang
PerfCallbacks = #{
    on_turn_start => fun(_Meta) ->
        put(turn_start_time, erlang:system_time(millisecond))
    end,
    on_llm_call => fun(_Messages, _Meta) ->
        put(llm_start_time, erlang:system_time(millisecond))
    end,
    on_turn_end => fun(Meta) ->
        StartTime = get(turn_start_time),
        Duration = erlang:system_time(millisecond) - StartTime,
        logger:info("第 ~p 轮耗时: ~p ms",
            [maps:get(turn_count, Meta, 0), Duration])
    end
}.
```

### 示例 3：流式输出

```erlang
StreamCallbacks = #{
    on_token => fun(TokenText, _Meta) ->
        %% 实时输出到终端
        io:format("~ts", [TokenText])
    end,
    on_turn_end => fun(_Meta) ->
        io:format("~n")  %% 换行
    end
}.

{ok, State} = beamai_agent:new(#{
    llm => LLM,
    callbacks => StreamCallbacks
}).
```

### 示例 4：工具审批（中断机制）

```erlang
%% 使用 on_tool_call 的 interrupt 返回值实现人工审批
ApprovalCallbacks = #{
    on_tool_call => fun(FuncName, Args) ->
        DangerousTools = [<<"delete_file">>, <<"execute_command">>],
        case lists:member(FuncName, DangerousTools) of
            true ->
                io:format("工具 ~ts 需要审批，参数: ~p~n", [FuncName, Args]),
                %% 中断执行
                {interrupt, #{
                    reason => require_approval,
                    tool => FuncName,
                    args => Args
                }};
            false ->
                ok
        end
    end,
    on_interrupt => fun(InterruptState, _Meta) ->
        io:format("Agent 已中断，等待审批: ~p~n", [InterruptState])
    end,
    on_resume => fun(_InterruptState, _Meta) ->
        io:format("Agent 已恢复执行~n")
    end
}.
```

### 示例 5：WebSocket 通知

```erlang
%% 将事件推送到 WebSocket 客户端
WsCallbacks = #{
    on_turn_start => fun(Meta) ->
        ws_send(Meta, #{type => <<"turn_start">>})
    end,
    on_token => fun(TokenText, Meta) ->
        ws_send(Meta, #{type => <<"token">>, content => TokenText})
    end,
    on_tool_call => fun(FuncName, Args) ->
        ws_send(#{}, #{type => <<"tool_call">>, tool => FuncName, args => Args}),
        ok
    end,
    on_turn_end => fun(Meta) ->
        ws_send(Meta, #{type => <<"turn_end">>})
    end,
    on_turn_error => fun(Error, Meta) ->
        ws_send(Meta, #{type => <<"error">>, error => Error})
    end
}.

ws_send(Meta, Message) ->
    AgentId = maps:get(agent_id, Meta, <<"unknown">>),
    websocket_handler:send(AgentId, jsx:encode(Message)).
```

---

## 最佳实践

### 1. 保持回调轻量

回调应该快速执行，避免阻塞 Agent 主流程：

```erlang
%% 推荐：异步处理
on_llm_call => fun(Messages, Meta) ->
    spawn(fun() -> log_to_external_service(Messages, Meta) end)
end

%% 避免：同步阻塞操作
on_llm_call => fun(Messages, Meta) ->
    %% 这会阻塞 Agent
    httpc:request(post, {Url, [], "application/json", Body}, [], [])
end
```

### 2. 异常安全

回调内部的异常不会影响 Agent 执行，但建议添加错误处理：

```erlang
on_turn_end => fun(Meta) ->
    try
        process_turn_result(Meta)
    catch
        Class:Reason:Stack ->
            logger:warning("回调处理失败: ~p:~p", [Class, Reason])
    end
end
```

### 3. 合理使用 on_tool_call 中断

`on_tool_call` 是唯一可以影响执行流程的回调：

```erlang
%% 仅对危险操作使用中断
on_tool_call => fun(FuncName, Args) ->
    case requires_approval(FuncName, Args) of
        true -> {interrupt, #{tool => FuncName}};
        false -> ok  %% 大多数情况应返回 ok
    end
end
```

### 4. 利用元数据关联事件

使用 `turn_count` 和 `agent_id` 关联同一 Agent 的事件：

```erlang
on_turn_start => fun(Meta) ->
    ets:insert(agent_events, {
        {maps:get(agent_id, Meta), maps:get(turn_count, Meta)},
        #{start_time => erlang:system_time(millisecond)}
    })
end
```

---

## 更多资源

- [beamai_agent README](../apps/beamai_agent/README.md) - Agent 模块文档
- [MIDDLEWARE.md](MIDDLEWARE.md) - Middleware 系统文档
- [API_REFERENCE.md](API_REFERENCE.md) - API 参考文档
