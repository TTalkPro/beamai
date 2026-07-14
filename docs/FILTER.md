# Filter 过滤器系统文档

[English](FILTER_EN.md) | 中文

beamai_core 的 Filter 系统提供了真正的**洋葱式（onion）**拦截机制，用于在工具执行和 LLM 调用的前后进行包裹、改写和控制。它采用 **around（环绕）** 形态：每个 filter 用一个单独的闭包同时承担「前置 → 调内层 → 后置」三段逻辑，对齐通用 middleware 模式。相比把拦截拆成 before/after 两个独立闭包，around 让前后逻辑同处一处、用闭包局部变量天然桥接，短路只需「不调内层」，无需专门的 halt 协议。

## 目录

- [概述](#概述)
- [3 个 around hook 点](#3-个-around-hook-点)
- [filter 私有上下文](#filter-私有上下文)
- [洋葱执行顺序](#洋葱执行顺序)
- [API 参考](#api-参考)
- [使用方法](#使用方法)
- [完整示例](#完整示例)
- [与 Middleware 的关系](#与-middleware-的关系)

---

## 概述

Filter 是 Kernel 工具执行和 Chat 调用的洋葱式拦截器，可以：

- **改写请求**: 前置修改参数、消息列表、调用选项
- **改写响应**: 后置修改工具结果或 LLM 响应
- **短路**: around 闭包不调用 `Next`，跳过内层（包括真正的工具执行/LLM 调用）直接返回结果
- **重试**: around 闭包可多次调用 `Next`
- **私有状态**: 每个 filter 有一份按名字隔离的私有上下文，贯穿一次 invoke（含工具循环各轮）
- **日志/审计**: 记录调用日志、统计响应长度等

每个 filter 就是**一层洋葱**——它最多绑定 3 个可选 around hook：chat 链的 `around_chat`、tool 链的 `around_tool`、turn 链的 `around_turn`（包整个工具循环，Agent 层使用）。一个 around 闭包形如：

```erlang
fun(Request, FCtx, Next) -> Response | {Response, NewFCtx} end
```

- **前置**：改写 `Request`
- **调内层**：`Next(Request1)` 拿到 `Response`（不调即短路，多调即重试）
- **后置**：改写 `Response`
- **返回**：`Response`（私有状态不变）或 `{Response, NewFCtx}`（更新私有状态）

filter 链由 `beamai_filter_chain` 合成为嵌套调用，最内层是 **terminal**（真正的 LLM 调用或工具执行）。

### 核心模块

| 模块 | 位置 | 说明 |
|------|------|------|
| `beamai_filter` | `apps/beamai_core/src/kernel/` | Filter 构造器与工具函数 |
| `beamai_filter_chain` | `apps/beamai_core/src/kernel/` | 洋葱链合成与运行 |
| `beamai_kernel` | `apps/beamai_core/src/kernel/` | Kernel 集成（注册 filter） |
| `beamai` | `apps/beamai_core/src/` | 顶层 Facade（便捷 API） |

---

## 3 个 around hook 点

一个 filter 可定义以下 3 个 hook 的任意子集：

| hook | 含义 | 形态 |
|------|------|------|
| `around_chat` | 环绕一次 LLM 调用 | `fun(Request, FCtx, Next) -> Response \| {Response, NewFCtx}` |
| `around_tool` | 环绕一次工具执行 | `fun(Request, FCtx, Next) -> Response \| {Response, NewFCtx}` |
| `around_turn` | 环绕整个工具循环（每 turn 一次，Agent 层） | 同上（Response 为工具循环结果 tuple） |

**每条链分别只用各自的 around：**

- **chat 链**用每个 filter 的 `around_chat`，包裹一次 LLM 调用。
- **tool 链**用每个 filter 的 `around_tool`，包裹一次工具执行。
- **turn 链**用每个 filter 的 `around_turn`，包裹整个工具循环（RAG 注入 / 最终答案校验 / turn 级预算）。

某 filter 若对某条链不含对应 around，则在该链中被**跳过**。

### 各链的 Request / Response

| 链 | Request | Response |
|----|---------|----------|
| chat | `#{messages, context, opts}` | `#{response, context}`（response 为 beamai_llm_response） |
| tool | `#{tool, args, context}` | `#{result, context}` |
| turn | `#{messages, context, resume}` | 工具循环结果 tuple（`{ok, Resp, TCM, Iter}` \| `{interrupt, _, _}` \| `{error, _}`；interrupt/error 必须透传、不得重入） |

其中 `context` 是贯穿全链的**共享上下文**（`beamai_context`），filter、terminal 都能读写。它与下文的 filter **私有上下文** 是两回事。

> **会话记忆**正是一个 filter：`beamai_memory_filter:memory_filter(Store)` 返回**单个** filter，其 `around_chat` 前置把本轮 delta 存入 store 并用 store 里的完整历史替换 messages（按 `conversation_id`）、后置把 assistant 回复存入 store。由于前后同处一个闭包，只需查一次 `conversation_id`。详见 [MEMORY.md](MEMORY.md)。

### 注册顺序即层序

filter 在构建 Kernel 时经 `beamai_kernel:new(Settings, Filters)` **一次性给出**，
**列表位置决定洋葱层次**：靠前 = 外层（前置先执行、后置后执行）。没有 order
字段、没有运行时排序——想调整层次，调整列表顺序即可（对齐 clj-agent 的
扁平 vector 模型）。

### Filter 规格 Map

filter 是一个标记 map：

```erlang
-type filter() :: #{
    '__filter__' := true,
    name := binary(),                  %% 名称（调试标识，也是私有上下文的隔离键）
    hooks := #{                        %% 3 个 around hook 的任意子集
        around_chat => around_fun(),
        around_tool => around_fun(),
        around_turn => around_fun()
    },
    init := map()                      %% 私有上下文初值（首次进入时种入，缺省 #{}）
}.

-type around_fun() :: fun((Request, FCtx, Next) -> Response | {Response, NewFCtx}).
-type Next :: fun((Request) -> Response).
```

---

## filter 私有上下文

每个 filter 有一份**私有上下文**（FCtx），按 filter 名字隔离，与共享的 `beamai_context` 分离：

- around 闭包通过第 2 个参数 `FCtx` **读取**私有状态，通过返回 `{Response, NewFCtx}` **写回**。
- 不同 filter 的私有上下文互不可见（即使用相同的内部键也不冲突）。
- 私有状态随共享 context 透传，**贯穿一次 invoke**——包括工具调用循环的各轮、以及同名 filter 的 `around_chat` 与 `around_tool` 之间。
- 用 `beamai_filter:new/3` 的第 3 个参数指定私有状态初值（缺省 `#{}`），首次进入该 filter 时种入。

> 私有上下文仅在一次 invoke 内存活，不跨多次 invoke 持久化。若需跨 invoke 的状态（如全局计数器），请另接外部 store。

简单 filter 无需用到私有状态时，around 直接返回 `Response` 即可（少写一层元组）。

---

## 洋葱执行顺序

对 filters 列表 `[A, B]`（A 靠前 = 外层）在 chat 链上包裹 terminal（LLM）：

```
A 前置 → B 前置 → Terminal → B 后置 → A 后置
```

合成方式（`beamai_filter_chain:compose/3`，Phase = `around_chat`）：

```
compose([A, B], Phase, Terminal)
  = fun(Req) -> A_around(Req, fun(R) -> B_around(R, Terminal) end) end
```

其中 `X_around` 即「跑 X 的前置、`Next` 进内层、回程跑 X 的后置」。

- **前置**：按列表顺序执行各层前置（A 先、B 后）。
- **terminal**：最内层执行真正的 LLM 调用 / 工具执行。
- **后置**：**自动逆序**（B 先、A 后）——这是嵌套调用栈天然的展开顺序，无需手工指定。

filter 的 around 若不调用 `Next` 即为短路（跳过所有内层），由该 filter 直接构造并返回 `Response`。外层 filter 的后置仍照常执行。

tool 链同理，把上面的 `around_chat` 换成 `around_tool`（Phase = `around_tool`）。

---

## API 参考

### beamai_filter 模块

#### 构造器

```erlang
%% 创建 filter（私有状态初值 #{}）。
%% Hooks 为 hook map，可含 around_chat/around_tool/around_turn 任意子集。
-spec new(Name :: binary(), Hooks :: hooks()) -> filter().

%% 创建 filter（指定私有状态初值 Init）
-spec new(Name :: binary(), Hooks :: hooks(), Init :: map()) -> filter().
```

其中 hook 形态：

```erlang
-type hook_type() :: around_chat | around_tool | around_turn.
-type hooks() :: #{
    around_chat => around_fun(),
    around_tool => around_fun(),
    around_turn => around_fun()
}.
-type around_fun() :: fun((Request, FCtx, Next) -> Response | {Response, NewFCtx}).
```

around 不调用 `Next` 则短路（跳过内层），直接返回 `Response`。

#### 工具函数

```erlang
%% 取 filter 的某个 hook（不存在返回 undefined）
-spec hook(filter(), hook_type()) -> around_fun() | undefined.

%% 取 filter 的私有上下文初值
-spec init(filter()) -> map().
```

### beamai_filter_chain 模块

```erlang
%% 运行某条链的 filter 洋葱。
%% Phase 指定该链用哪个 around hook：chat 链传 around_chat，tool 链传
%% around_tool。只参与该链（含对应 around）的 filter 进入洋葱，其余跳过。
%% Terminal 产出最内层响应，出错时 throw；run/4 用 try/catch 捕获，
%% 统一返回 {ok, Response} | {error, Reason}。
-spec run(Filters :: [filter()],
          Phase :: around_chat | around_tool,
          Terminal :: fun((Request) -> Response),
          Request :: map()) -> {ok, Response} | {error, Reason}.

%% 把 filter 列表与 terminal 合成为单个洋葱函数
-spec compose(Filters :: [filter()], Phase :: phase(),
              Terminal :: fun()) -> fun((Request) -> Response).
```

### beamai_context 私有上下文访问器

```erlang
%% 读取某 filter 的私有上下文（按名字隔离，缺省返回 Default）
-spec filter_state(Ctx, Name :: binary(), Default :: map()) -> map().

%% 写回某 filter 的私有上下文
-spec set_filter_state(Ctx, Name :: binary(), State :: map()) -> Ctx.
```

> 这两个访问器供洋葱链投影/合并使用；filter 代码通常通过 around 的 `FCtx` 参数读、通过返回 `{Resp, NewFCtx}` 写，无需直接调用它们。

### beamai_kernel 集成

```erlang
%% 构建 Kernel 时一次性给出全量 filter（注册顺序即层序：列表靠前 = 外层）
beamai_kernel:new(Settings, Filters) -> Kernel.
```

filter 在构建后**不可增量追加**——层次完全由这份列表的顺序决定。
需要会话记忆时把 `beamai_memory_filter:memory_filter(Store)` 放列表**首位**
（最外层：先展开完整历史，再让内层 filter 处理）。

工具模块（`beamai_kernel:add_tool_module/2`）只提供工具，不携带 filter。

> **system_prompts 注入层次**：`invoke_chat` 的 `Opts` 里给出的 `system_prompts`
> 在调用时作为**最内层**临时 filter 追加——在所有用户 filter 之后、LLM 之前
> 前置系统消息。因此用户 chat filter 看到的 messages **不含**系统提示，
> memory filter 也永远不会把系统提示存进历史。

### beamai 便捷 API

```erlang
%% 创建 Kernel（一次性给出全量 filter）
beamai:kernel(Settings, Filters) -> Kernel.

%% 快捷创建 filter（直接给 hook map；放入 kernel/2 的 Filters 列表）
beamai:filter(Name, Hooks) -> Filter.
beamai:filter(Name, Hooks, Init) -> Filter.
```

---

## 使用方法

### 1. 构建 Kernel 时一次性给出 filter

```erlang
Logger = beamai:filter(<<"logger">>, #{
    %% around_tool：前置记录工具名
    around_tool => fun(#{tool := #{name := Name}, args := Args} = Req, _FCtx, Next) ->
        io:format("Calling tool: ~ts(~p)~n", [Name, Args]),
        Next(Req)
    end
}),
K0 = beamai:kernel(#{}, [Logger]).
```

### 2. Filter 层次（注册顺序即层序）

列表靠前 = 外层：它的前置先执行、后置后执行。

```erlang
Validator   = beamai:filter(<<"validator">>, #{around_tool => ValidateFn}),
Logger      = beamai:filter(<<"logger">>, #{around_tool => LogFn}),
Transformer = beamai:filter(<<"transformer">>, #{around_tool => TransformFn}),

%% 列表顺序即洋葱层序（validator 最外层，transformer 最内层）
K = beamai:kernel(#{}, [Validator, Logger, Transformer]).

%% 前置执行顺序：validator → logger → transformer → Terminal
%% 后置执行顺序：transformer → logger → validator（自动逆序）
```

---

## 完整示例

### 示例 1：tool filter —— 日志 + 结果翻倍（一个 around_tool 同时管前后）

```erlang
%% 一个 around_tool 闭包：前置记录调用、后置改写 result
LogDouble = beamai:filter(<<"log_and_double">>, #{
    around_tool => fun(#{tool := #{name := Name}, args := Args} = Req, _FCtx, Next) ->
        io:format("[LOG] ~ts(~p)~n", [Name, Args]),
        #{result := Result} = Resp = Next(Req),
        Resp#{result => Result * 2}
    end
}),

%% 创建 Kernel（filter 一次性给出）并注册工具
K0 = beamai:kernel(#{}, [LogDouble]),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})).

%% 调用（3 + 5 = 8，后置翻倍后 = 16）
%% （工具执行经 Kernel 的 tool filter 洋葱链）
```

### 示例 2：chat filter —— 注入 system 消息 + 审计（一个 around_chat 同时管前后）

```erlang
%% 一个 around_chat 闭包：前置注入 system 消息、后置记录响应长度
SystemAudit = beamai:filter(<<"system_and_audit">>, #{
    around_chat => fun(#{messages := Msgs} = Req, _FCtx, Next) ->
        HasSystem = lists:any(
            fun(#{role := R}) -> R =:= system; (_) -> false end,
            Msgs),
        Req1 = case HasSystem of
            true ->
                Req;
            false ->
                SystemMsg = #{role => system,
                              content => <<"请用简洁的中文回答。"/utf8>>},
                Req#{messages => [SystemMsg | Msgs]}
        end,
        #{response := Response} = Resp = Next(Req1),
        case beamai_llm_response:content(Response) of
            Content when is_binary(Content) ->
                logger:info("Response length: ~B bytes", [byte_size(Content)]);
            _ ->
                ok
        end,
        Resp
    end
}),

K0 = beamai:kernel(#{}, [SystemAudit]),
K1 = beamai:add_llm(K0, LLMConfig).

%% 发送请求时，chat filter 链自动注入 system 消息并审计响应。
```

### 示例 3：短路 —— around_tool 不调 `Next`

```erlang
%% tool filter：参数校验失败时短路（不调 Next，跳过真正的工具执行）
Guard = beamai:filter(<<"guard">>, #{
    around_tool => fun(#{args := #{a := A}, context := Ctx} = Req, _FCtx, Next) ->
        case A > 1000 of
            true  -> #{result => {error, <<"a exceeds limit">>}, context => Ctx};
            false -> Next(Req)
        end
    end
}),
K = beamai:kernel(#{}, [Guard]).
```

> 短路即「不调用 `Next`」，由该 filter 直接构造并返回 `Response`。包裹它的外层 filter 后置仍会执行——便于在外层做统一收尾。

### 示例 4：私有上下文 —— 跨工具循环各轮累积计数

```erlang
%% around_chat：用私有上下文记录本次 invoke 内的 LLM 调用次数
Counter = beamai:filter(<<"counter">>, #{
    around_chat => fun(Req, FCtx, Next) ->
        N = maps:get(calls, FCtx, 0),
        Resp = Next(Req),
        logger:info("LLM call #~B", [N + 1]),
        {Resp, FCtx#{calls => N + 1}}   %% 返回 {Resp, NewFCtx} 写回私有状态
    end
}),
K = beamai:kernel(#{}, [Counter]).
%% 工具循环每轮 LLM 调用都让 calls 累加，且与其它 filter 的私有状态互不干扰。
```

更多示例参见 [examples/src/example_filter.erl](../examples/src/example_filter.erl)。

---

## 与 middleware 的对应

本系统采用通用的 **around（环绕）middleware** 形态：

- around 闭包 `fun(Request, FCtx, Next) -> Response` 对应中间件的「拿到请求 → 调 `next` → 处理响应」。
- 前置/后置同处一个闭包，用局部变量桥接，无需在 before/after 间借共享上下文传值。
- 短路 = 不调 `Next`；重试 = 多次调 `Next`；无需专门的 halt 协议。
- 一个 filter 同时打包 chat 与 tool 两条链的 around（`around_chat` / `around_tool`），各链独立选用。

---

## 与 Middleware 框架的关系

[beamai_extra](https://github.com/TTalkPro/beamai_extra) 扩展项目中提供了更高级的 Middleware 系统（位于 beamai_tools），支持有状态管理、预设配置、调用限制、人工审批、重试和降级等功能。Middleware 内部转换为 filter 注册到 Kernel，两者最终在同一条洋葱链中执行。

| 特性 | Filter（本文档） | Middleware（beamai_extra） |
|------|-------------------|---------------------------|
| 复杂度 | 轻量级，私有上下文限单次 invoke | 完整框架，跨调用状态管理 |
| 预设配置 | 无 | 提供 production/development 等预设 |
| 内置功能 | 无 | 调用限制、人工审批、重试、降级 |
| 适用场景 | 简单包裹：日志、校验、注入、缓存 | 复杂控制：限流、重试、降级 |

---

## 更多资源

- [beamai_core README](../apps/beamai_core/README.md) - Kernel 架构文档
- [MEMORY.md](MEMORY.md) - 会话记忆（Memory filter）
- [API 参考](API_REFERENCE.md) - API 参考文档
