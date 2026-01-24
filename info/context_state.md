# Context 与 Agent 状态传递

## 问题

Agent 除了 messages 外还有自己的状态（如 user_id、session、permissions 等），
需要在 filter 和 plugin 函数中访问这些状态。

## 解决方案

通过 `chat_opts` 的 `context` 字段传入 `beamai_context:t()`，
context 的 `variables` 可携带任意 agent 状态。

## beamai_context:t() 结构

```erlang
-type t() :: #{
    '__context__' := true,
    variables := #{binary() => term()},  %% 存放 agent 状态
    history := [message()],
    kernel := term() | undefined,
    trace := [trace_entry()],
    metadata := map()
}.
```

## 状态传递路径

### invoke 路径

```
beamai:invoke(Kernel, FuncName, Args, Context)
    |
    +-- pre_filter: FilterCtx#{context => Context}   <-- 可访问
    |
    +-- function:invoke(FuncDef, Args, Context)      <-- 可访问
    |       -> 可返回 {ok, Value, UpdatedCtx}
    |
    +-- post_filter: FilterCtx#{context => NewCtx}   <-- 可访问
```

### chat 路径

```
beamai:chat(Kernel, Messages, #{context => Ctx})
    |
    +-- pre_chat_filter: FilterCtx#{context => Ctx}  <-- 可访问
    |
    +-- beamai_chat_completion:chat(...)
    |
    +-- post_chat_filter: FilterCtx#{context => Ctx} <-- 可访问
```

### chat_with_tools 路径

```
beamai:chat_with_tools(Kernel, Messages, #{context => Ctx})
    |
    +-- LLM 返回 tool_calls
    |
    +-- execute_tool_calls(Kernel, TCs, Context)
    |       |
    |       +-- invoke(Kernel, Name, Args, CtxAcc)
    |       |       -> pre_filter 可访问 context
    |       |       -> function 可访问 context
    |       |       -> 返回 {ok, Value, NewCtx} 时更新 context
    |       |
    |       +-- 下一个 tool call 使用更新后的 context（链式传递）
    |
    +-- 循环直到无 tool_call
```

## 使用方式

### 传入 Agent 状态

```erlang
AgentState = #{
    <<"user_id">> => <<"u123">>,
    <<"session">> => SessionData,
    <<"permissions">> => [read, write]
},
Ctx = beamai_context:new(AgentState),

beamai:chat(Kernel, Messages, #{context => Ctx}),
beamai:chat_with_tools(Kernel, Messages, #{context => Ctx}).
```

### Filter 中访问状态

```erlang
beamai:add_filter(K, <<"auth">>, pre_chat, fun(FilterCtx) ->
    Context = maps:get(context, FilterCtx),
    Perms = beamai_context:get(Context, <<"permissions">>, []),
    case lists:member(chat, Perms) of
        true -> {continue, FilterCtx};
        false -> {error, unauthorized}
    end
end)
```

### Plugin 函数中访问状态

```erlang
beamai:function(<<"get_user_info">>, fun(_Args, Context) ->
    UserId = beamai_context:get(Context, <<"user_id">>),
    {ok, #{user_id => UserId}}
end)
```

### 函数更新状态（链式传递）

```erlang
beamai:function(<<"increment_counter">>, fun(_Args, Context) ->
    Counter = beamai_context:get(Context, <<"counter">>, 0),
    NewCtx = beamai_context:set(Context, <<"counter">>, Counter + 1),
    {ok, Counter + 1, NewCtx}  %% 返回三元组，更新 context
end)
```

多个 tool call 按顺序执行时，前一个函数更新的 context 会传给后一个。

## 关键源文件

| 文件 | 相关代码 |
|------|----------|
| `beamai_kernel.erl:50-56` | `chat_opts` 类型定义（含 context 字段） |
| `beamai_kernel.erl:136-140` | `invoke_chat` 从 Opts 取 context |
| `beamai_kernel.erl:162-173` | `invoke_chat_with_tools` 从 Opts 取 context |
| `beamai_kernel.erl:222-249` | `tool_calling_loop` / `execute_tool_calls` 传递 context |
| `beamai_context.erl` | context 的 get/set/new 操作 |
