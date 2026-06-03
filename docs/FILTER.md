# Filter 过滤器系统文档

[English](FILTER_EN.md) | 中文

beamai_core 的 Filter 系统提供了真正的**洋葱式（onion）**拦截机制，用于在工具执行和 LLM 调用的前后进行包裹、改写和控制。它对齐 [Spring AI 的 Advisor](https://docs.spring.io/spring-ai/reference/api/advisors.html) 把拦截拆成 before/after 的思路：去程改写请求、回程改写响应，回程顺序自动逆序。区别在于——beamai 的**一个 filter** 同时打包 chat 与 tool 的去程/回程 hook，最多绑定 4 个可选 hook。

## 目录

- [概述](#概述)
- [4 个 hook 点](#4-个-hook-点)
- [洋葱执行顺序](#洋葱执行顺序)
- [API 参考](#api-参考)
- [使用方法](#使用方法)
- [完整示例](#完整示例)
- [与 Middleware 的关系](#与-middleware-的关系)

---

## 概述

Filter 是 Kernel 工具执行和 Chat 调用的洋葱式拦截器，可以：

- **改写请求**: 去程修改参数、消息列表、调用选项
- **改写响应**: 回程修改工具结果或 LLM 响应
- **短路**: 在去程 hook 返回 `{halt, Response}`，跳过内层（包括真正的工具执行/LLM 调用）直接返回结果，但仍执行本层回程 hook
- **日志/审计**: 记录调用日志、统计响应长度等

每个 filter 就是**一层洋葱**——它最多绑定 4 个可选 hook：chat 链的去程/回程（`pre_chat`/`post_chat`）和 tool 链的去程/回程（`pre_tool`/`post_tool`）。同一 filter 的 pre/post 配成**同一层洋葱**包裹同一次调用：去程 pre 在内层之前执行，回程 post 在内层之后执行，回程自动逆序。filter 链由 `beamai_filter_chain` 合成为嵌套调用，最内层是 **terminal**（真正的 LLM 调用或工具执行）。

### 核心模块

| 模块 | 位置 | 说明 |
|------|------|------|
| `beamai_filter` | `apps/beamai_core/src/kernel/` | Filter 构造器与工具函数 |
| `beamai_filter_chain` | `apps/beamai_core/src/kernel/` | 洋葱链合成与运行 |
| `beamai_kernel` | `apps/beamai_core/src/kernel/` | Kernel 集成（注册 filter） |
| `beamai` | `apps/beamai_core/src/` | 顶层 Facade（便捷 API） |

---

## 4 个 hook 点

一个 filter 可定义以下 4 个 hook 的任意子集：

| hook | 含义 | 形态 |
|------|------|------|
| `pre_chat` | chat 去程：LLM 调用前改写请求 | `fun(Request) -> Request \| {halt, Response}` |
| `post_chat` | chat 回程：LLM 调用后改写响应 | `fun(Response) -> Response` |
| `pre_tool` | tool 去程：工具执行前改写参数 | `fun(Request) -> Request \| {halt, Response}` |
| `post_tool` | tool 回程：工具执行后改写结果 | `fun(Response) -> Response` |

**两条链分别只用各自的一对 hook：**

- **chat 链**用每个 filter 的 `(pre_chat, post_chat)` 这一对，包裹一次 LLM 调用。
- **tool 链**用每个 filter 的 `(pre_tool, post_tool)` 这一对，包裹一次工具执行。

某 filter 若对某条链不含相关 hook（chat 链既无 `pre_chat` 也无 `post_chat`，或 tool 链既无 `pre_tool` 也无 `post_tool`），则在该链中被**跳过**。缺失的单个 hook 用恒等函数补齐。

### 各链的 Request / Response

| 链 | Request | Response |
|----|---------|----------|
| chat | `#{messages, context, opts}` | `#{response, context}`（response 为 beamai_llm_response） |
| tool | `#{tool, args, context}` | `#{result, context}` |

> **会话记忆**正是一个 filter：`beamai_memory_filter:memory_filter(Store)` 返回**单个** filter，其 `pre_chat` 把本轮 delta 存入 store 并用 store 里的完整历史替换 messages（按 `conversation_id`）、其 `post_chat` 把 assistant 回复存入 store。前后逻辑同处一层，由洋葱链天然保证 `pre_chat` 在内层之前、`post_chat` 在内层之后。详见 [MEMORY.md](MEMORY.md)。

### order 字段

`order` 决定洋葱的层次：**越小越外层**——它的 pre hook 越先执行、post hook 越后执行。默认 `order` 为 0。同 `order` 按注册顺序排列（稳定排序）。

### Filter 规格 Map

filter 是一个标记 map：

```erlang
-type filter() :: #{
    '__filter__' := true,
    name := binary(),                                  %% 名称（调试标识）
    order := integer(),                                %% 层次（越小越外层）
    hooks := #{                                        %% 4 个 hook 的任意子集
        pre_chat  => fun((Request)  -> Request | {halt, Response}),
        post_chat => fun((Response) -> Response),
        pre_tool  => fun((Request)  -> Request | {halt, Response}),
        post_tool => fun((Response) -> Response)
    }
}.
```

---

## 洋葱执行顺序

对 filter A（order 1）和 B（order 2）在 chat 链上包裹 terminal（LLM）：

```
A.pre_chat → B.pre_chat → Terminal → B.post_chat → A.post_chat
```

合成方式（`beamai_filter_chain:compose/4`，Phase = `{pre_chat, post_chat}`）：

```
compose([A, B], Phase, Terminal)
  = fun(Req) -> A_wrap(Req, fun(R) -> B_wrap(R, Terminal) end) end
```

其中 `X_wrap` 即「先跑 X 的 pre、再进内层、回程跑 X 的 post」。

- **去程**：按 order 升序执行各层 pre hook（A 先、B 后）。
- **terminal**：最内层执行真正的 LLM 调用 / 工具执行。
- **回程**：post hook **自动逆序**（B 先、A 后）——这是嵌套调用栈天然的展开顺序，无需手工指定。

filter 的 pre hook 若返回 `{halt, Response}` 即为短路（跳过所有内层），但**仍执行本层的 post hook**。

tool 链同理，把上面的 `pre_chat/post_chat` 换成 `pre_tool/post_tool`（Phase = `{pre_tool, post_tool}`）。

---

## API 参考

### beamai_filter 模块

#### 构造器

```erlang
%% 创建 filter（默认 order 0）。
%% Hooks 为 hook map，可含 pre_chat/post_chat/pre_tool/post_tool 任意子集。
-spec new(Name :: binary(), Hooks :: hooks()) -> filter().

%% 创建 filter（指定 order，越小越外层）
-spec new(Name :: binary(), Hooks :: hooks(), Order :: integer()) -> filter().
```

其中 hook 形态：

```erlang
-type hook_type() :: pre_chat | post_chat | pre_tool | post_tool.
-type hooks() :: #{
    pre_chat  => fun((Request)  -> Request | {halt, Response}),
    post_chat => fun((Response) -> Response),
    pre_tool  => fun((Request)  -> Request | {halt, Response}),
    post_tool => fun((Response) -> Response)
}.
```

pre hook 返回 `{halt, Response}` 则短路（跳过内层），但仍执行本层对应的 post hook。

#### 工具函数

```erlang
%% 按 order 升序稳定排序（同 order 保持注册顺序）
-spec sort([filter()]) -> [filter()].

%% 取 filter 的某个 hook（不存在返回 undefined）
-spec hook(filter(), hook_type()) -> fun() | undefined.
```

### beamai_filter_chain 模块

```erlang
%% 运行某条链的 filter 洋葱。
%% Phase 指定该链用哪一对 hook：chat 链传 {pre_chat, post_chat}，
%% tool 链传 {pre_tool, post_tool}。只参与该链（含至少一个相关 hook）的
%% filter 进入洋葱，其余跳过。Terminal 产出最内层响应，出错时 throw；
%% run/4 用 try/catch 捕获，统一返回 {ok, Response} | {error, Reason}。
-spec run(Filters :: [filter()],
          Phase :: {pre_chat, post_chat} | {pre_tool, post_tool},
          Terminal :: fun((Request) -> Response),
          Request :: map()) -> {ok, Response} | {error, Reason}.

%% 把 filter 列表与 terminal 合成为单个洋葱函数（内部用，外部传 identity）
-spec compose(Filters :: [filter()], Phase :: phase(),
              Terminal :: fun(), identity) -> fun((Request) -> Response).
```

### beamai_kernel 集成

```erlang
%% 注册 filter 到 Kernel（追加到 filters 列表，运行时按 order 排序）
beamai_kernel:add_filter(Kernel, Filter) -> UpdatedKernel.

%% 从工具模块自动加载 filter（模块需实现可选的 filters/0 回调）
beamai_kernel:add_tool_module(Kernel, Module) -> UpdatedKernel.
```

### beamai 便捷 API

```erlang
%% 注册已构建的 filter
beamai:add_filter(Kernel, Filter) -> UpdatedKernel.

%% 快捷创建并注册 filter（直接给 hook map，order 固定为 0）
beamai:add_filter(Kernel, Name, Hooks) -> UpdatedKernel.
```

---

## 使用方法

### 1. 注册 filter 到 Kernel

```erlang
%% 方式一：使用 beamai 便捷 API（hook map，order 0）
K0 = beamai:kernel(),
K1 = beamai:add_filter(K0, <<"logger">>, #{
    %% pre_tool：去程记录工具名
    pre_tool => fun(#{tool := #{name := Name}, args := Args} = Req) ->
        io:format("Calling tool: ~ts(~p)~n", [Name, Args]),
        Req
    end
}).

%% 方式二：手动创建 filter 并注册（可指定 order）
Filter = beamai_filter:new(<<"logger">>, #{pre_tool => PreFn}, 10),
K1 = beamai_kernel:add_filter(K0, Filter).
```

### 2. 工具模块自动注册 filter

工具模块可通过实现可选的 `filters/0` 回调自动注册 filter：

```erlang
-module(my_tool_module).
-behaviour(beamai_tool_behaviour).

-export([tools/0, filters/0]).

tools() ->
    [#{name => <<"my_tool">>, handler => fun handle/2,
       description => <<"My tool">>}].

%% 可选回调：返回 filter 列表
filters() ->
    [
        beamai_filter:new(<<"audit">>, #{
            post_tool => fun(#{result := Result} = Resp) ->
                logger:info("Tool returned: ~p", [Result]),
                Resp
            end
        })
    ].
```

加载模块时，Kernel 会自动注册这些 filter：

```erlang
K1 = beamai_kernel:add_tool_module(K0, my_tool_module).
%% 工具和 filter 都已注册
```

### 3. Filter 层次（order）

`order` 数值越小越外层：它的 pre hook 越先执行、post hook 越后执行。同 `order` 按注册顺序排列。

```erlang
%% 校验器（order -10，最外层 → pre_tool 最先执行）
K1 = beamai_kernel:add_filter(K0,
    beamai_filter:new(<<"validator">>, #{pre_tool => ValidateFn}, -10)),

%% 日志器（order 0，默认）
K2 = beamai_kernel:add_filter(K1,
    beamai_filter:new(<<"logger">>, #{pre_tool => LogFn}, 0)),

%% 转换器（order 10，最内层 → pre_tool 最后执行、post_tool 最先执行）
K3 = beamai_kernel:add_filter(K2,
    beamai_filter:new(<<"transformer">>, #{post_tool => TransformFn}, 10)).

%% pre_tool 执行顺序：validator → logger → transformer → Terminal
```

---

## 完整示例

### 示例 1：tool filter —— 日志 + 结果翻倍（同一 filter 的 pre_tool + post_tool）

```erlang
%% 创建 Kernel 并注册工具
K0 = beamai:kernel(),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})),

%% 同一 filter 同时绑定 pre_tool（去程日志）与 post_tool（回程翻倍）
K2 = beamai:add_filter(K1, <<"log_and_double">>, #{
    %% pre_tool：去程记录调用
    pre_tool => fun(#{tool := #{name := Name}, args := Args} = Req) ->
        io:format("[LOG] ~ts(~p)~n", [Name, Args]),
        Req
    end,
    %% post_tool：回程改写 result
    post_tool => fun(#{result := Result} = Resp) ->
        Resp#{result => Result * 2}
    end
}).

%% 调用（3 + 5 = 8，回程翻倍后 = 16）
%% （工具执行经 Kernel 的 tool filter 洋葱链）
```

### 示例 2：chat filter —— 注入 system 消息 + 审计（同一 filter 的 pre_chat + post_chat）

```erlang
K0 = beamai:kernel(),
K1 = beamai:add_llm(K0, LLMConfig),

%% 同一 filter：pre_chat 去程注入 system 消息、post_chat 回程记录响应长度
K2 = beamai:add_filter(K1, <<"system_and_audit">>, #{
    %% pre_chat：自动注入 system 消息
    pre_chat => fun(#{messages := Msgs} = Req) ->
        HasSystem = lists:any(
            fun(#{role := R}) -> R =:= system; (_) -> false end,
            Msgs),
        case HasSystem of
            true ->
                Req;
            false ->
                SystemMsg = #{role => system,
                              content => <<"请用简洁的中文回答。"/utf8>>},
                Req#{messages => [SystemMsg | Msgs]}
        end
    end,
    %% post_chat：记录响应内容长度
    post_chat => fun(#{response := Response} = Resp) ->
        case beamai_llm_response:content(Response) of
            Content when is_binary(Content) ->
                logger:info("Response length: ~B bytes", [byte_size(Content)]);
            _ ->
                ok
        end,
        Resp
    end
}).

%% 发送请求时，chat filter 链自动注入 system 消息并审计响应。
```

### 示例 3：短路 —— pre_tool 返回 `{halt, Response}`

```erlang
%% tool filter：参数校验失败时短路（跳过真正的工具执行），仍走本层 post_tool
K1 = beamai:add_filter(K0, <<"guard">>, #{
    %% pre_tool：命中守卫条件则 halt，直接以错误结果短路
    pre_tool => fun(#{args := #{a := A}, context := Ctx} = Req) ->
        case A > 1000 of
            true  -> {halt, #{result => {error, <<"a exceeds limit">>},
                              context => Ctx}};
            false -> Req
        end
    end,
    %% post_tool：halt 与正常返回都会经过这里（统一收尾）
    post_tool => fun(Resp) ->
        logger:info("tool finished: ~p", [Resp]),
        Resp
    end
}).
```

> pre hook 返回 `{halt, Response}` 时跳过所有内层，但**仍执行本层的 post hook**——便于在短路路径上做统一的收尾处理（如缓存写回、计数）。

更多示例参见 [examples/src/example_filter.erl](../examples/src/example_filter.erl)。

---

## 与 Spring AI Advisor 的对应

本系统镜像 Spring AI 的 Advisor 把拦截拆成 before/after 的思路：

- Spring AI 的 `before(request, chain)` / `after(response, chain)` 分解 ⇄ beamai filter 的 pre/post hook。
- 不同点：Spring AI 一个 advisor 对应一条链；beamai 的**一个 filter** 同时打包 chat 与 tool 的去程/回程 hook（最多 4 个），chat 链取 `(pre_chat, post_chat)`、tool 链取 `(pre_tool, post_tool)`。
- 无独立的「around」形式——需要短路时由 pre hook 返回 `{halt, Response}` 实现。

两者都以「pre/post 包裹同一次调用、回程自动逆序」为核心。

---

## 与 Middleware 的关系

[beamai_extra](https://github.com/TTalkPro/beamai_extra) 扩展项目中提供了更高级的 Middleware 系统（位于 beamai_tools），支持有状态管理、预设配置、调用限制、人工审批、重试和降级等功能。Middleware 内部转换为 filter 注册到 Kernel，两者最终在同一条洋葱链中执行。

| 特性 | Filter（本文档） | Middleware（beamai_extra） |
|------|-------------------|---------------------------|
| 复杂度 | 轻量级，无状态 | 完整框架，有状态管理 |
| 预设配置 | 无 | 提供 production/development 等预设 |
| 内置功能 | 无 | 调用限制、人工审批、重试、降级 |
| 适用场景 | 简单包裹：日志、校验、注入、缓存 | 复杂控制：限流、重试、降级 |

---

## 更多资源

- [beamai_core README](../apps/beamai_core/README.md) - Kernel 架构文档
- [MEMORY.md](MEMORY.md) - 会话记忆（Memory filter）
- [API 参考](API_REFERENCE.md) - API 参考文档
