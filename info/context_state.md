# Context 与 Agent 状态传递

## 问题

Agent 除了 messages 外还有自己的状态（如 user_id、session、permissions 等），
需要在 filter 和工具函数中访问这些状态。

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
beamai_kernel:invoke(Kernel, ToolName, Args, Context)
    |
    +-- pre_filter: FilterCtx#{context => Context}   <-- 可访问
    |
    +-- beamai_tool:invoke(ToolSpec, Args, Context)  <-- 可访问
    |       -> 可返回 {ok, Value, UpdatedCtx}
    |
    +-- post_filter: FilterCtx#{context => NewCtx}   <-- 可访问
```

### chat 路径

```
beamai_kernel:invoke_chat(Kernel, Messages, #{context => Ctx})
    |
    +-- pre_chat_filter: FilterCtx#{context => Ctx}  <-- 可访问
    |
    +-- beamai_chat_completion:chat(...)
    |
    +-- post_chat_filter: FilterCtx#{context => Ctx} <-- 可访问
```

### chat_with_tools 路径

```
beamai_kernel:invoke_chat_with_tools(Kernel, Messages, #{context => Ctx})
    |
    +-- LLM 返回 tool_calls
    |
    +-- execute_tool_calls(Kernel, TCs, Context)
    |       |
    |       +-- invoke(Kernel, Name, Args, CtxAcc)
    |       |       -> pre_filter 可访问 context
    |       |       -> tool handler 可访问 context
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

beamai_kernel:invoke_chat(Kernel, Messages, #{context => Ctx}),
beamai_kernel:invoke_chat_with_tools(Kernel, Messages, #{context => Ctx}).
```

### Filter 中访问状态

```erlang
beamai_filter:new(<<"auth">>, pre_chat, fun(FilterCtx) ->
    Context = maps:get(context, FilterCtx),
    Perms = beamai_context:get(<<"permissions">>, Context, []),
    case lists:member(chat, Perms) of
        true -> {continue, FilterCtx};
        false -> {error, unauthorized}
    end
end)
```

### Tool Handler 中访问状态

```erlang
%% fun/2 形式的 handler 可以访问 Context
Tool = #{
    name => <<"get_user_info">>,
    handler => fun(_Args, Context) ->
        UserId = beamai_context:get(<<"user_id">>, Context),
        {ok, #{user_id => UserId}}
    end
}.

%% {M, F} 形式
-module(my_tools).
-export([get_user_info/2]).

get_user_info(_Args, Context) ->
    UserId = beamai_context:get(<<"user_id">>, Context),
    {ok, #{user_id => UserId}}.
```

### Tool 更新状态（链式传递）

```erlang
Tool = #{
    name => <<"increment_counter">>,
    handler => fun(_Args, Context) ->
        Counter = beamai_context:get(<<"counter">>, Context, 0),
        NewCtx = beamai_context:set(<<"counter">>, Counter + 1, Context),
        {ok, Counter + 1, NewCtx}  %% 返回三元组，更新 context
    end
}.
```

多个 tool call 按顺序执行时，前一个工具更新的 context 会传给后一个。

## beamai_context API

```erlang
%% 创建
Ctx = beamai_context:new().
Ctx = beamai_context:new(#{<<"key">> => value}).

%% 读取
Value = beamai_context:get(Key, Ctx).
Value = beamai_context:get(Key, Ctx, Default).

%% 写入
NewCtx = beamai_context:set(Key, Value, Ctx).

%% 批量操作
NewCtx = beamai_context:merge(#{k1 => v1, k2 => v2}, Ctx).
AllVars = beamai_context:variables(Ctx).
```

## Agent 中的 Context 使用

Agent 会自动管理 context：

```erlang
%% Agent 创建时可传入初始 context
{ok, Agent} = beamai_agent:new(#{
    llm => LLMConfig,
    context => beamai_context:new(#{
        <<"user_id">> => <<"u123">>,
        <<"session">> => SessionData
    })
}).

%% Agent run 会自动传递 context 给工具
{ok, Result, NewAgent} = beamai_agent:run(Agent, <<"Hello">>).
```

## 完整示例

```erlang
%% 1. 定义需要访问 context 的工具
BalanceTool = #{
    name => <<"check_balance">>,
    description => <<"Check user's account balance">>,
    handler => fun(_Args, Context) ->
        UserId = beamai_context:get(<<"user_id">>, Context),
        case db:get_balance(UserId) of
            {ok, Balance} -> {ok, #{balance => Balance}};
            {error, Reason} -> {error, Reason}
        end
    end
}.

TransferTool = #{
    name => <<"transfer_money">>,
    description => <<"Transfer money to another user">>,
    parameters => #{
        <<"to_user">> => #{type => string, required => true},
        <<"amount">> => #{type => integer, required => true}
    },
    handler => fun(Args, Context) ->
        FromUser = beamai_context:get(<<"user_id">>, Context),
        ToUser = maps:get(<<"to_user">>, Args),
        Amount = maps:get(<<"amount">>, Args),

        case db:transfer(FromUser, ToUser, Amount) of
            {ok, TxId} ->
                %% 记录交易到 context
                NewCtx = beamai_context:set(<<"last_tx">>, TxId, Context),
                {ok, #{transaction_id => TxId}, NewCtx};
            {error, Reason} ->
                {error, Reason}
        end
    end
}.

%% 2. 构建 Kernel
K0 = beamai_kernel:new(),
K1 = beamai_kernel:add_service(K0, LLMConfig),
K2 = beamai_kernel:add_tools(K1, [BalanceTool, TransferTool]).

%% 3. 创建 Agent 并传入用户上下文
InitCtx = beamai_context:new(#{
    <<"user_id">> => <<"user_12345">>,
    <<"permissions">> => [read, transfer]
}),

{ok, Agent} = beamai_agent:new(#{
    kernel => K2,
    context => InitCtx
}).

%% 4. 运行对话
{ok, Result, _} = beamai_agent:run(Agent, <<"Transfer $100 to user_67890">>).
```

## 关键源文件

| 文件 | 相关代码 |
|------|----------|
| `beamai_context.erl` | context 的 get/set/new 操作 |
| `beamai_kernel.erl` | invoke/invoke_chat 中传递 context |
| `beamai_tool.erl` | tool invoke 支持 context 参数 |
| `beamai_agent_tool_loop.erl` | tool calling 循环中的 context 传递 |
