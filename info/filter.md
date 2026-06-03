# Filter 系统（洋葱式拦截）

对齐 Spring AI Advisor 的 before/after 分解思路：去程改写请求、进内层、回程改写响应，回程顺序由嵌套调用栈天然逆序，无需手工指定。区别在于——beamai 的**一个 filter** 同时打包 chat 与 tool 的去程/回程 hook（最多 4 个），chat 链取 `(pre_chat, post_chat)`、tool 链取 `(pre_tool, post_tool)`。

## 核心类型

filter 是一个标记 map，最多绑定 4 个可选 hook：

```erlang
-type hook_type() :: pre_chat | post_chat | pre_tool | post_tool.
-type pre_fun()   :: fun((Request)  -> Request | {halt, Response}).
-type post_fun()  :: fun((Response) -> Response).
-type hooks() :: #{
    pre_chat  => pre_fun(),
    post_chat => post_fun(),
    pre_tool  => pre_fun(),
    post_tool => post_fun()
}.

-type filter() :: #{
    '__filter__' := true,
    name := binary(),
    order := integer(),
    hooks := hooks()        %% 4 个 hook 的任意子集
}.
```

## 4 个 hook 点 / 两条链

| hook | 含义 |
|------|------|
| `pre_chat`  | chat 去程：LLM 调用前改写请求 |
| `post_chat` | chat 回程：LLM 调用后改写响应 |
| `pre_tool`  | tool 去程：工具执行前改写参数 |
| `post_tool` | tool 回程：工具执行后改写结果 |

- **chat 链**用每个 filter 的 `(pre_chat, post_chat)` 这一对，包裹一次 LLM 调用。
- **tool 链**用每个 filter 的 `(pre_tool, post_tool)` 这一对，包裹一次工具执行。
- filter 对某条链不含相关 hook 则在该链中被跳过；缺失的单个 hook 用恒等函数补齐。

各链的 Request / Response：

| 链 | Request | Response |
|----|---------|----------|
| chat | `#{messages, context, opts}` | `#{response, context}`（response 为 beamai_llm_response） |
| tool | `#{tool, args, context}` | `#{result, context}` |

pre hook 返回 `{halt, Response}` 则短路（跳过内层），但仍执行本层对应的 post hook。

## order 语义

`order` 越小越外层：其 pre hook 越先执行、post hook 越后执行。默认 0。同 order 稳定保持注册顺序。

## 构造器（beamai_filter）

```erlang
%% 创建 filter（默认 order 0）；Hooks 含 4 个 hook 任意子集
new(Name, Hooks) -> filter().

%% 创建 filter（指定 order，越小越外层）
new(Name, Hooks, Order) -> filter().

%% 工具
sort(Filters) -> SortedFilters.        %% 按 order 升序稳定排序
hook(Filter, HookType) -> Fun | undefined.  %% 取某个 hook
```

源码（构造器极简，仅组装标记 map）：

```erlang
new(Name, Hooks) ->
    new(Name, Hooks, 0).

new(Name, Hooks, Order) when is_map(Hooks) ->
    #{'__filter__' => true, name => Name, order => Order, hooks => Hooks}.

hook(#{hooks := Hooks}, HookType) ->
    maps:get(HookType, Hooks, undefined).
```

## 洋葱链（beamai_filter_chain）

把 filter 列表按某条链的 `(pre, post)` hook compose 成嵌套调用，最内层是 terminal（真正的 LLM 调用 / 工具执行）。Phase 指定该链用哪一对 hook。

```erlang
%% Phase = {pre_chat, post_chat} 或 {pre_tool, post_tool}
run(Filters, Phase, Terminal, Request) ->
    Relevant = relevant(beamai_filter:sort(Filters), Phase),
    Run = compose(Relevant, Phase, Terminal, identity),
    try {ok, Run(Request)}
    catch throw:Reason -> {error, Reason} end.

compose([], _Phase, Terminal, _) ->
    Terminal;
compose([Filter | Rest], {PreKey, PostKey} = Phase, Terminal, _) ->
    Next = compose(Rest, Phase, Terminal, identity),
    Pre  = hook_or_identity(Filter, PreKey),
    Post = hook_or_identity(Filter, PostKey),
    fun(Req) ->
        case Pre(Req) of
            {halt, Resp} -> Post(Resp);          %% 短路：跳过 Next，仍走 post
            Req1         -> Post(Next(Req1))      %% 正常：进入内层再回程改写
        end
    end.
```

即（Phase = `{pre_chat, post_chat}`）：

```
compose([A, B], Phase, Terminal)
  = fun(Req) -> A_wrap(Req, fun(R) -> B_wrap(R, Terminal) end) end
```

执行顺序（A order 1、B order 2 在 chat 链包裹 Terminal）：

```
A.pre_chat → B.pre_chat → Terminal → B.post_chat → A.post_chat
```

- 去程 pre 按 order 升序；回程 post 自动逆序（嵌套栈展开）。
- filter 的 pre 返回 `{halt, Response}` 即短路（跳过内层），仍执行本层 post。
- terminal 出错用 `throw`；`run/4` 用 try/catch 捕获，统一返回 `{ok, Response} | {error, Reason}`。
- `relevant/2`：仅保留对该链有至少一个相关 hook 的 filter；`hook_or_identity/2`：缺失 hook 用恒等函数。

## 注册到 Kernel

filter 追加到 Kernel 的 `filters` 列表，运行时按 order 排序：

```erlang
add_filter(#{filters := Filters} = Kernel, Filter) ->
    Kernel#{filters => Filters ++ [Filter]}.
```

beamai facade 便捷 API：

```erlang
%% 注册已构建 filter
beamai:add_filter(Kernel, Filter) -> Kernel.

%% 快捷创建并注册（直接给 hook map，order 固定 0）
beamai:add_filter(Kernel, Name, Hooks) ->
    Filter = beamai_filter:new(Name, Hooks),
    beamai_kernel:add_filter(Kernel, Filter).
```

## 工具模块自动注册

工具模块实现可选回调 `filters/0`，加载时由 Kernel 自动注册：

```erlang
maybe_add_filters(Kernel, Module) ->
    case erlang:function_exported(Module, filters, 0) of
        true ->
            Filters = Module:filters(),
            lists:foldl(fun(F, K) -> add_filter(K, F) end, Kernel, Filters);
        false ->
            Kernel
    end.
```

## 典型用例

### 工具日志 + 结果翻倍（单个 filter：pre_tool + post_tool）

```erlang
beamai_filter:new(<<"log_and_double">>, #{
    pre_tool => fun(#{tool := #{name := Name}, args := Args} = Req) ->
        io:format("[LOG] ~ts(~p)~n", [Name, Args]),
        Req
    end,
    post_tool => fun(#{result := Result} = Resp) ->
        Resp#{result => Result * 2}
    end
})
```

### 注入系统提示词 + 审计（单个 filter：pre_chat + post_chat）

```erlang
beamai_filter:new(<<"system_and_audit">>, #{
    pre_chat => fun(#{messages := Msgs} = Req) ->
        SystemMsg = #{role => system, content => <<"You are helpful.">>},
        Req#{messages => [SystemMsg | Msgs]}
    end,
    post_chat => fun(#{response := Response} = Resp) ->
        logger:info("response: ~p", [Response]),
        Resp
    end
})
```

### 短路（tool / pre_tool 返回 {halt, Response}）

```erlang
beamai_filter:new(<<"guard">>, #{
    pre_tool => fun(#{args := #{a := A}, context := Ctx} = Req) ->
        case A > 1000 of
            true  -> {halt, #{result => {error, <<"a exceeds limit">>},
                              context => Ctx}};
            false -> Req
        end
    end,
    post_tool => fun(Resp) ->
        logger:info("tool finished: ~p", [Resp]),
        Resp
    end
})
```

## Memory filter（会话记忆，规范示例）

`beamai_memory_filter:memory_filter(Store)` 返回**单个** filter，前后逻辑同处一层（洋葱链保证 pre_chat 在内层之前、post_chat 在内层之后）：

```erlang
memory_filter(Store, Order) ->
    beamai_filter:new(<<"memory">>, #{
        %% pre_chat：按 conversation_id 存 delta + 用完整历史替换 messages
        pre_chat => fun(#{messages := Delta, context := Ctx} = Req) ->
            case beamai_context:conversation_id(Ctx) of
                undefined -> Req;
                ConvId ->
                    ok = beamai_chat_memory:mem_add(Store, ConvId, Delta),
                    Full = beamai_chat_memory:mem_get(Store, ConvId),
                    Req#{messages => Full}
            end
        end,
        %% post_chat：存 assistant 回复
        post_chat => fun(#{response := Response, context := Ctx} = Resp) ->
            case beamai_context:conversation_id(Ctx) of
                undefined -> ok;
                ConvId ->
                    case response_to_message(Response) of
                        undefined -> ok;
                        Msg -> ok = beamai_chat_memory:mem_add(Store, ConvId, [Msg])
                    end
            end,
            Resp
        end
    }, Order).
```

无 `conversation_id` 时原样透传（退化为单次无状态调用）。memory 用较小 order（默认 -1000）即可包在其它 filter（如 system prompt 注入）外层：先展开历史，再让内层注入系统提示。详见 docs/MEMORY.md。

## 关键源文件

| 文件 | 职责 |
|------|------|
| `apps/beamai_core/src/kernel/beamai_filter.erl` | filter 构造器、排序、取 hook |
| `apps/beamai_core/src/kernel/beamai_filter_chain.erl` | 洋葱链 compose 与 run（throw 捕获、按链筛选 relevant） |
| `apps/beamai_core/src/kernel/beamai_memory_filter.erl` | 会话记忆 filter（规范示例） |
| `apps/beamai_core/src/kernel/beamai_kernel.erl` | 注册 filter、filters/0 自动加载 |
| `apps/beamai_core/src/beamai.erl` | facade 便捷 API（add_filter/2,3） |
