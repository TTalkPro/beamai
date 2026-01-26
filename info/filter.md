# Filter 系统

## 核心类型

Filter 是一个带有类型、优先级和处理函数的 map：

```erlang
-type filter_def() :: #{
    name := binary(),
    type := filter_type(),
    handler := fun((filter_context()) -> filter_result()),
    priority => integer()
}.
```

## 四种过滤器类型

```erlang
-type filter_type() :: pre_invocation | post_invocation | pre_chat | post_chat.
```

| 类型 | 触发时机 | 作用 |
|------|----------|------|
| `pre_invocation` | 工具执行前 | 可修改参数、跳过执行 |
| `post_invocation` | 工具执行后 | 可修改返回值 |
| `pre_chat` | Chat 请求前 | 可修改消息列表 |
| `post_chat` | Chat 响应后 | 可修改 LLM 响应 |

## 处理函数的返回值

```erlang
-type filter_result() ::
    {continue, filter_context()}   %% 继续执行下一个过滤器
    | {skip, term()}               %% 跳过后续执行，直接返回值
    | {error, term()}.             %% 中止，返回错误
```

## 注册到 Kernel

过滤器追加到 Kernel 的 `filters` 列表中：

```erlang
add_filter(#{filters := Filters} = Kernel, Filter) ->
    Kernel#{filters => Filters ++ [Filter]}.
```

创建方式：

```erlang
%% 使用 beamai_filter:new/3,4
Filter = beamai_filter:new(<<"log">>, pre_invocation, fun(Ctx) ->
    io:format("Calling: ~p~n", [maps:get(tool, Ctx)]),
    {continue, Ctx}
end).

K1 = beamai_kernel:add_filter(K0, Filter).

%% 带优先级
Filter2 = beamai_filter:new(<<"auth">>, pre_invocation, fun check_auth/1, 10).
```

## 执行流程

### 工具调用时

```
invoke(Kernel, ToolName, Args, Context)
    |
    +-- 1. find_tool -> 找到 ToolSpec
    |
    +-- 2. apply_pre_filters(Filters, ToolSpec, Args, Context)
    |       -> 构造 FilterCtx = #{tool, args, context, metadata}
    |       -> 按 priority 排序，依次执行 pre_invocation 类型的 handler
    |       -> 返回 {ok, FilteredArgs, FilteredCtx} | {skip, Value} | {error, ...}
    |
    +-- 3. beamai_tool:invoke(ToolSpec, FilteredArgs, FilteredCtx)
    |
    +-- 4. apply_post_filters(Filters, ToolSpec, Value, Context)
            -> 构造 FilterCtx = #{tool, result, context, metadata}
            -> 按 priority 排序，依次执行 post_invocation 类型的 handler
            -> 返回 {ok, FinalValue, FinalCtx}
```

### Chat 调用时

```
invoke_chat(Kernel, Messages, Opts)
    |
    +-- apply_pre_chat_filters(Filters, Messages, Context)
    |       -> FilterCtx = #{messages, context, metadata}
    |       -> 执行 pre_chat handler 链
    |
    +-- beamai_chat_completion:chat(LlmConfig, FilteredMsgs, Opts)
    |
    +-- apply_post_chat_filters(Filters, Response, Context)
            -> FilterCtx = #{result, context, metadata}
            -> 执行 post_chat handler 链
```

## 链式执行机制

核心是 `run_filter_chain/2`：

```erlang
run_filter_chain([], FilterCtx) ->
    {continue, FilterCtx};                     %% 全部通过
run_filter_chain([#{handler := Handler} | Rest], FilterCtx) ->
    try Handler(FilterCtx) of
        {continue, NewCtx} ->
            run_filter_chain(Rest, NewCtx);    %% 传递修改后的上下文给下一个
        {skip, Value} ->
            {skip, Value};                     %% 短路，跳过剩余过滤器
        {error, Reason} ->
            {error, Reason}                    %% 中止
    catch
        Class:Reason:Stack ->
            {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end.
```

## 优先级排序

过滤器按 `priority` 升序执行：

```erlang
sort_filters(Filters) ->
    lists:sort(fun(#{priority := P1}, #{priority := P2}) ->
        P1 =< P2
    end, [F#{priority => maps:get(priority, F, 0)} || F <- Filters]).
```

默认 priority 为 0，数值越小越先执行。

## FilterContext 字段

### pre_invocation / post_invocation

```erlang
#{
    tool := tool_spec(),       %% 当前工具定义
    args := map(),             %% 调用参数（pre）或原始参数（post）
    result := term(),          %% 仅 post：工具执行结果
    context := beamai_context:t(),
    metadata := map()
}
```

### pre_chat / post_chat

```erlang
#{
    messages := [message()],   %% 消息列表（pre）
    result := map(),           %% 仅 post：LLM 响应
    context := beamai_context:t(),
    metadata := map()
}
```

## 典型用例

### 参数校验（pre_invocation）

```erlang
beamai_filter:new(<<"validate">>, pre_invocation, fun(#{args := Args} = Ctx) ->
    case maps:is_key(<<"required_field">>, Args) of
        true -> {continue, Ctx};
        false -> {error, missing_required_field}
    end
end)
```

### 结果缓存（post_invocation）

```erlang
beamai_filter:new(<<"cache">>, post_invocation, fun(#{result := Result, args := Args} = Ctx) ->
    cache:put(Args, Result),
    {continue, Ctx}
end)
```

### 注入系统提示词（pre_chat）

```erlang
beamai_filter:new(<<"system_prompt">>, pre_chat, fun(#{messages := Msgs} = Ctx) ->
    SystemMsg = #{role => system, content => <<"You are helpful.">>},
    {continue, Ctx#{messages => [SystemMsg | Msgs]}}
end)
```

### 权限检查（pre_invocation）

```erlang
beamai_filter:new(<<"auth">>, pre_invocation, fun(#{context := Context, tool := Tool} = Ctx) ->
    ToolName = beamai_tool:get_name(Tool),
    Perms = beamai_context:get(<<"permissions">>, Context, []),
    case is_allowed(ToolName, Perms) of
        true -> {continue, Ctx};
        false -> {error, {unauthorized, ToolName}}
    end
end, 10)  %% priority 10，优先执行
```

### 日志记录（pre + post）

```erlang
%% Pre: 记录调用开始
beamai_filter:new(<<"log_start">>, pre_invocation, fun(#{tool := Tool, args := Args} = Ctx) ->
    logger:info("Tool ~s called with ~p", [beamai_tool:get_name(Tool), Args]),
    {continue, Ctx}
end).

%% Post: 记录调用结束
beamai_filter:new(<<"log_end">>, post_invocation, fun(#{tool := Tool, result := Result} = Ctx) ->
    logger:info("Tool ~s returned ~p", [beamai_tool:get_name(Tool), Result]),
    {continue, Ctx}
end).
```

## Agent 回调与 Filter

Agent 的 `on_llm_call` 和 `on_tool_call` 回调内部通过 Filter 实现：

```erlang
%% beamai_agent_state.erl 中注入的 filter
inject_callback_filters(Kernel, Callbacks) ->
    case maps:is_key(on_tool_call, Callbacks) of
        true ->
            Filter = beamai_filter:new(
                <<"agent_on_tool_call">>,
                pre_invocation,
                fun(FilterCtx) ->
                    Tool = maps:get(tool, FilterCtx),
                    Args = maps:get(args, FilterCtx),
                    beamai_agent_callbacks:invoke(on_tool_call,
                        [beamai_tool:get_name(Tool), Args], Callbacks),
                    {continue, FilterCtx}
                end,
                9999  %% 最低优先级
            ),
            beamai_kernel:add_filter(Kernel, Filter);
        false ->
            Kernel
    end.
```

## 关键源文件

| 文件 | 职责 |
|------|------|
| `apps/beamai_core/src/kernel/beamai_filter.erl` | 过滤器定义、链式执行、排序 |
| `apps/beamai_core/src/kernel/beamai_kernel.erl` | 注册过滤器、在 invoke/chat 中调用 |
| `apps/beamai_agent/src/beamai_agent_state.erl` | Agent 回调到 Filter 的转换 |
