# Filter 系统（洋葱式拦截）

采用 **around（环绕）middleware** 形态：每个 filter 用一个单独闭包同时承担「前置 → 调内层 → 后置」，前后逻辑同处一处、用闭包局部变量桥接，回程顺序由嵌套调用栈天然逆序，短路只需「不调内层」。beamai 的**一个 filter** 同时打包 chat 与 tool 两条链的 around（最多 2 个），chat 链取 `around_chat`、tool 链取 `around_tool`。每个 filter 另有一份按名字隔离的私有上下文。

## 核心类型

filter 是一个标记 map，最多绑定 2 个可选 around hook：

```erlang
-type hook_type()  :: around_chat | around_tool.
-type next()       :: fun((Request) -> Response).
-type around_fun() :: fun((Request, FCtx, Next) -> Response | {Response, NewFCtx}).
-type hooks() :: #{
    around_chat => around_fun(),
    around_tool => around_fun()
}.

-type filter() :: #{
    '__filter__' := true,
    name := binary(),
    order := integer(),
    hooks := hooks(),       %% 2 个 around hook 的任意子集
    init := map()           %% 私有上下文初值（缺省 #{}）
}.
```

## 2 个 around hook / 两条链

| hook | 含义 |
|------|------|
| `around_chat` | 环绕一次 LLM 调用 |
| `around_tool` | 环绕一次工具执行 |

- **chat 链**用每个 filter 的 `around_chat`，包裹一次 LLM 调用。
- **tool 链**用每个 filter 的 `around_tool`，包裹一次工具执行。
- filter 对某条链不含对应 around 则在该链中被跳过。

各链的 Request / Response：

| 链 | Request | Response |
|----|---------|----------|
| chat | `#{messages, context, opts}` | `#{response, context}`（response 为 beamai_llm_response） |
| tool | `#{tool, args, context}` | `#{result, context}` |

`context` 是贯穿全链的**共享上下文**（`beamai_context`），与 filter **私有上下文**（FCtx）分离。

around 闭包：前置改写 `Request` → `Next(Req1)` 进内层（不调即短路、多调即重试）→ 后置改写 `Response` → 返回 `Response`（私有状态不变）或 `{Response, NewFCtx}`（更新私有状态）。

## 私有上下文（FCtx）

每个 filter 一份私有上下文，按 filter 名字隔离，与共享 context 分离：

- around 经第 2 参 `FCtx` 读、经返回 `{Resp, NewFCtx}` 写。
- 不同 filter 互不可见（同内部键也不冲突）。
- 随共享 context 透传，贯穿一次 invoke（工具循环各轮、同名 filter 的 chat/tool around 之间）。
- `new/4` 第 4 参指定初值（缺省 `#{}`），首次进入时种入。
- 仅单次 invoke 内存活，不跨 invoke 持久化。

## order 语义

`order` 越小越外层：其前置越先执行、后置越后执行。默认 0。同 order 稳定保持注册顺序。

## 构造器（beamai_filter）

```erlang
%% 创建 filter（默认 order 0，私有状态初值 #{}）；Hooks 含 around_chat/around_tool 任意子集
new(Name, Hooks) -> filter().

%% 创建 filter（指定 order，越小越外层）
new(Name, Hooks, Order) -> filter().

%% 创建 filter（指定 order 与私有状态初值 Init）
new(Name, Hooks, Order, Init) -> filter().

%% 工具
sort(Filters) -> SortedFilters.                  %% 按 order 升序稳定排序
hook(Filter, HookType) -> Fun | undefined.       %% 取某个 around hook
init(Filter) -> map().                           %% 取私有上下文初值
```

源码（构造器极简，仅组装标记 map）：

```erlang
new(Name, Hooks) -> new(Name, Hooks, 0).
new(Name, Hooks, Order) -> new(Name, Hooks, Order, #{}).
new(Name, Hooks, Order, Init) when is_map(Hooks), is_map(Init) ->
    #{'__filter__' => true, name => Name, order => Order, hooks => Hooks, init => Init}.

hook(#{hooks := Hooks}, HookType) -> maps:get(HookType, Hooks, undefined).
init(#{init := Init}) -> Init.
```

## 洋葱链（beamai_filter_chain）

把 filter 列表按某条链的 around hook compose 成嵌套调用，最内层是 terminal（真正的 LLM 调用 / 工具执行）。Phase 是单个 hook atom（`around_chat` | `around_tool`）。每个 filter 进入时从共享 context 投影出其私有上下文作为 FCtx，around 返回 `{Resp, NewFCtx}` 时合并回响应的 context。

```erlang
run(Filters, Phase, Terminal, Request) ->
    Relevant = relevant(beamai_filter:sort(Filters), Phase),
    Run = compose(Relevant, Phase, Terminal),
    try {ok, Run(Request)}
    catch throw:Reason -> {error, Reason} end.

compose([], _Phase, Terminal) ->
    Terminal;
compose([Filter | Rest], Phase, Terminal) ->
    Next   = compose(Rest, Phase, Terminal),
    Around = beamai_filter:hook(Filter, Phase),
    Name   = maps:get(name, Filter),
    Init   = beamai_filter:init(Filter),
    fun(#{context := Ctx} = Req) ->
        FCtx = beamai_context:filter_state(Ctx, Name, Init),
        case Around(Req, FCtx, Next) of
            {#{context := RCtx} = Resp, NewFCtx} ->
                Resp#{context => beamai_context:set_filter_state(RCtx, Name, NewFCtx)};
            Resp ->
                Resp
        end
    end.
```

即（Phase = `around_chat`）：

```
compose([A, B], Phase, Terminal)
  = fun(Req) -> A_around(Req, fun(R) -> B_around(R, Terminal) end) end
```

执行顺序（A order 1、B order 2 在 chat 链包裹 Terminal）：

```
A 前置 → B 前置 → Terminal → B 后置 → A 后置
```

- 前置按 order 升序；后置自动逆序（嵌套栈展开）。
- around 不调 `Next` 即短路（跳过内层），直接返回 `Response`；外层后置仍执行。
- terminal 出错用 `throw`；`run/4` 用 try/catch 捕获，统一返回 `{ok, Response} | {error, Reason}`。
- `relevant/2`：仅保留对该链有对应 around 的 filter。

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

### 工具日志 + 结果翻倍（一个 around_tool）

```erlang
beamai_filter:new(<<"log_and_double">>, #{
    around_tool => fun(#{tool := #{name := Name}, args := Args} = Req, _FCtx, Next) ->
        io:format("[LOG] ~ts(~p)~n", [Name, Args]),
        #{result := Result} = Resp = Next(Req),
        Resp#{result => Result * 2}
    end
})
```

### 注入系统提示词 + 审计（一个 around_chat）

```erlang
beamai_filter:new(<<"system_and_audit">>, #{
    around_chat => fun(#{messages := Msgs} = Req, _FCtx, Next) ->
        SystemMsg = #{role => system, content => <<"You are helpful.">>},
        #{response := Response} = Resp = Next(Req#{messages => [SystemMsg | Msgs]}),
        logger:info("response: ~p", [Response]),
        Resp
    end
})
```

### 短路（around_tool 不调 Next）

```erlang
beamai_filter:new(<<"guard">>, #{
    around_tool => fun(#{args := #{a := A}, context := Ctx} = Req, _FCtx, Next) ->
        case A > 1000 of
            true  -> #{result => {error, <<"a exceeds limit">>}, context => Ctx};
            false -> Next(Req)
        end
    end
})
```

### 私有上下文（跨工具循环各轮累积）

```erlang
beamai_filter:new(<<"counter">>, #{
    around_chat => fun(Req, FCtx, Next) ->
        N = maps:get(calls, FCtx, 0),
        Resp = Next(Req),
        {Resp, FCtx#{calls => N + 1}}
    end
})
```

## Memory filter（会话记忆，规范示例）

`beamai_memory_filter:memory_filter(Store)` 返回**单个** filter，前后逻辑同处一个 around 闭包，只需查一次 `conversation_id`：

```erlang
memory_filter(Store, Order) ->
    beamai_filter:new(<<"memory">>, #{
        around_chat => fun(#{messages := Delta, context := Ctx} = Req, _FCtx, Next) ->
            case beamai_context:conversation_id(Ctx) of
                undefined ->
                    Next(Req);
                ConvId ->
                    %% 前置：存 delta + 用完整历史替换 messages
                    ok = beamai_chat_memory:mem_add(Store, ConvId, Delta),
                    Full = beamai_chat_memory:mem_get(Store, ConvId),
                    #{response := Response} = Resp = Next(Req#{messages => Full}),
                    %% 后置：存 assistant 回复
                    case response_to_message(Response) of
                        undefined -> ok;
                        Msg -> ok = beamai_chat_memory:mem_add(Store, ConvId, [Msg])
                    end,
                    Resp
            end
        end
    }, Order).
```

无 `conversation_id` 时原样透传（退化为单次无状态调用）。memory 用较小 order（默认 -1000）即可包在其它 filter（如 system prompt 注入）外层：先展开历史，再让内层注入系统提示。详见 docs/MEMORY.md。

## 关键源文件

| 文件 | 职责 |
|------|------|
| `apps/beamai_core/src/kernel/beamai_filter.erl` | filter 构造器、排序、取 hook/init |
| `apps/beamai_core/src/kernel/beamai_filter_chain.erl` | 洋葱链 compose 与 run（throw 捕获、按链筛选 relevant、私有上下文投影/合并） |
| `apps/beamai_core/src/kernel/beamai_context.erl` | 共享上下文 + filter 私有上下文槽（filter_state/3、set_filter_state/3） |
| `apps/beamai_core/src/kernel/beamai_memory_filter.erl` | 会话记忆 filter（规范示例） |
| `apps/beamai_core/src/kernel/beamai_kernel.erl` | 注册 filter、filters/0 自动加载 |
| `apps/beamai_core/src/beamai.erl` | facade 便捷 API（add_filter/2,3） |
