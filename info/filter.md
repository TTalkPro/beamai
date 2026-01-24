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
| `pre_invocation` | 函数执行前 | 可修改参数、跳过执行 |
| `post_invocation` | 函数执行后 | 可修改返回值 |
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
beamai:add_filter(Kernel, <<"log">>, pre_invocation, fun(Ctx) ->
    io:format("Calling: ~p~n", [maps:get(function, Ctx)]),
    {continue, Ctx}
end)
```

## 执行流程

### 函数调用时

```
invoke(Kernel, FuncName, Args, Context)
    |
    +-- 1. get_function -> 找到 FuncDef
    |
    +-- 2. apply_pre_filters(Filters, FuncDef, Args, Context)
    |       -> 构造 FilterCtx = #{function, args, context, metadata}
    |       -> 按 priority 排序，依次执行 pre_invocation 类型的 handler
    |       -> 返回 {ok, FilteredArgs, FilteredCtx} | {skip, Value} | {error, ...}
    |
    +-- 3. beamai_function:invoke(FuncDef, FilteredArgs, FilteredCtx)
    |
    +-- 4. apply_post_filters(Filters, FuncDef, Value, Context)
            -> 构造 FilterCtx = #{function, result, context, metadata}
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

## 典型用例

```erlang
%% 参数校验（pre_invocation）
beamai:add_filter(K, <<"validate">>, pre_invocation, fun(#{args := Args} = Ctx) ->
    case maps:is_key(<<"required_field">>, Args) of
        true -> {continue, Ctx};
        false -> {error, missing_required_field}
    end
end)

%% 结果缓存（post_invocation）
beamai:add_filter(K, <<"cache">>, post_invocation, fun(#{result := Result} = Ctx) ->
    cache:put(Result),
    {continue, Ctx}
end)

%% 注入系统提示词（pre_chat）
beamai:add_filter(K, <<"system_prompt">>, pre_chat, fun(#{messages := Msgs} = Ctx) ->
    SystemMsg = #{role => system, content => <<"You are helpful.">>},
    {continue, Ctx#{messages => [SystemMsg | Msgs]}}
end)
```

## 关键源文件

| 文件 | 职责 |
|------|------|
| `apps/beamai_core/src/kernel/beamai_filter.erl` | 过滤器定义、链式执行、排序 |
| `apps/beamai_core/src/kernel/beamai_kernel.erl` | 注册过滤器、在 invoke/chat 中调用 |
| `apps/beamai_core/src/beamai.erl` | Facade API (`add_filter/2,4`) |
