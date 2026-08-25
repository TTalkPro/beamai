# Filter 过滤器系统文档

[English](FILTER_EN.md) | 中文

beamai_core 的 Filter 系统提供了真正的**洋葱式（onion）**拦截机制，用于在工具执行和 LLM 调用的前后进行包裹、改写和控制。它采用 **around（环绕）** 形态：每个 filter 用一个单独的闭包同时承担「前置 → 调内层 → 后置」三段逻辑，对齐通用 middleware 模式。相比把拦截拆成 before/after 两个独立闭包，around 让前后逻辑同处一处、用闭包局部变量天然桥接，短路只需「不调内层」，无需专门的 halt 协议。

## 目录

- [概述](#概述)
- [4 个 around hook 点](#4-个-around-hook-点)
- [循环也是链上一环](#循环也是链上一环)
- [token_transform（token 流变换）](#token_transformtoken-流变换)
- [内置 filter](#内置-filter)
- [filter 私有上下文](#filter-私有上下文)
- [洋葱执行顺序](#洋葱执行顺序)
- [API 参考](#api-参考)
- [使用方法](#使用方法)
- [完整示例](#完整示例)
- [与 Middleware 的关系](#与-middleware-的关系)

---

## 概述

Filter 是 ChatClient 工具执行和 Chat 调用的洋葱式拦截器，可以：

- **改写请求**: 前置修改参数、消息列表、调用选项
- **改写响应**: 后置修改工具结果或 LLM 响应
- **短路**: around 闭包不调用 `Next`，跳过内层（包括真正的工具执行/LLM 调用）直接返回结果
- **重试**: around 闭包可多次调用 `Next`
- **私有状态**: 每个 filter 有一份按名字隔离的私有上下文，贯穿一次 invoke（含工具循环各轮）
- **日志/审计**: 记录调用日志、统计响应长度等

每个 filter 就是**一层洋葱**——它最多绑定 4 个可选 around hook：turn 链的 `around_turn`（包整个工具循环，Agent 层使用）、step 链的 `around_step`（包一轮 ReAct 迭代）、chat 链的 `around_chat`（包一轮的 LLM 调用）、tool 链的 `around_tool`（包一次工具执行），外加流式专用的 `token_transform`（token 流变换，不走洋葱，见专节）。一个 around 闭包形如：

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
| `beamai_filter` | `apps/beamai_core/src/core/` | Filter 构造器与工具函数 |
| `beamai_filter_chain` | `apps/beamai_core/src/core/` | 洋葱链合成与运行 |
| `beamai_chat_client` | `apps/beamai_core/src/core/` | ChatClient 集成（注册 filter） |
| `beamai` | `apps/beamai_core/src/` | 顶层 Facade（便捷 API） |

---

## 4 个 around hook 点

一个 filter 可定义以下 4 个 hook 的任意子集：

| hook | 粒度 | 形态 |
|------|------|------|
| `around_turn` | 每 turn 一次（整个工具循环，Agent 层） | 同下（Response 为工具循环结果 tuple） |
| `around_step` | 每轮 ReAct 迭代一次（含该轮工具执行） | 同下（Response 为 step 响应 map） |
| `around_chat` | 每轮迭代**恰好一次**（该轮的 LLM 调用） | `fun(Request, FCtx, Next) -> Response \| {Response, NewFCtx}` |
| `around_tool` | 每个 tool call 一次（并行任务内） | 同上 |

钩子按**进出频率**划分：turn 每 turn 一次、step / chat 每轮一次（step 还包住该轮工具执行）、
tool 每个调用一次。重试之所以不是一层钩子，见下面的层次图说明。

层次（外 → 内）：

```
around_turn            每 turn 一次           RAG 注入 / 最终答案校验 / turn 级预算
  tool_loop filter     循环驱动本身就是链上一环（可整体替换）
    around_step        每轮迭代一次           每轮预算 / 轨迹记录 / 迭代级改写
      around_chat      该轮的 LLM 调用一次    记忆 / 记账 / 审计 / 提示词注入
        provider       真正的 chat 调用       ← 重试在它**内部**，链看不见
      around_tool      每个 tool call 一次     超时 / 审批 / 参数改写
```

**每条链分别只用各自的 around：**

- **turn 链**用 `around_turn`，包裹整个工具循环；链的最内层是**循环 filter**，它的 terminal 是 step 链。
- **step 链**用 `around_step`，包裹一轮迭代——注意它**同时包住该轮的工具执行**，这是 `around_chat` 做不到的。
- **chat 链**用 `around_chat`，包裹该轮的 LLM 调用；它的 terminal 就是 provider 调用。
- **tool 链**用 `around_tool`，包裹一次工具执行。

某 filter 若对某条链不含对应 around，则在该链中被**跳过**。同一个 filter 可同时声明多个 hook，各链中的相对层序一致。

### 各链的 Request / Response

| 链 | Request | Response |
|----|---------|----------|
| step | `#{messages, context, iteration, tool_calls_made}` | `#{status, messages, context, tool_calls_made, ...}`（见下节） |
| chat | `#{messages, context, opts}`；流式额外带 `stream => true` | `#{response, context}`（response 为 beamai_llm_response） |
| tool | `#{tool, args, context}` | `#{result, context}` |
| turn | `#{messages, context, resume, load_history}` | 工具循环结果 tuple（`{ok, Resp, TCM, Iter, Messages}` \| `{interrupt, _, _}` \| `{error, _}`；interrupt/error 必须透传、不得重入） |

turn 链**重入**（校验重试 / evaluator）要用结果的第 5 元 `Messages` —— 该跑完整的消息序列
（跨轮历史 + 本轮新增 + 各轮 assistant/工具结果，直至最终答案）：

```erlang
%% 接着上一跑续走：上下文全由 filter 重建，不依赖 agent 是否开了记忆
Next(Req#{messages => Messages ++ [Feedback], load_history => false})
```

Request 的 `messages` 语义是**本轮新增消息**（不是完整历史），`load_history` 缺省 `true`
（让工具循环前接跨轮历史）。只传新增消息、指望循环载入历史把原问题带回来的写法，在
`memory => false` 时会**丢掉原始问题**——模型只收到一句「上次没通过请修正」然后胡编。
这正是 `load_history` 与第 5 元存在的理由。

其中 `context` 是贯穿全链的**共享上下文**（`beamai_context`），filter、terminal 都能读写。它与下文的 filter **私有上下文** 是两回事。

> **会话记忆**正是一个 filter：`beamai_memory_filter:memory_filter(Store)` 返回**单个** filter，其 `around_chat` 前置把本轮 delta 存入 store 并用 store 里的完整历史替换 messages（按 `conversation_id`）、后置把 assistant 回复存入 store。由于前后同处一个闭包，只需查一次 `conversation_id`。详见 [MEMORY.md](MEMORY.md)。

### 注册顺序即层序

filter 在构建 ChatClient 时经 `beamai_chat_client:new(Settings, Filters)` **一次性给出**，
**列表位置决定洋葱层次**：靠前 = 外层（前置先执行、后置后执行）。没有 order
字段、没有运行时排序——想调整层次，调整列表顺序即可（对齐 clj-agent 的
扁平 vector 模型）。

### Filter 规格 Map

filter 是一个标记 map：

```erlang
-type filter() :: #{
    '__filter__' := true,
    name := binary(),                  %% 名称（调试标识，也是私有上下文的隔离键）
    hooks := #{                        %% 5 个 hook 的任意子集
        around_chat => around_fun(),
        around_step => around_fun(),
        around_tool => around_fun(),
        around_turn => around_fun(),
        token_transform => token_transform()         %% token 流变换（见下节）
    },
    init := map()                      %% 私有上下文初值（首次进入时种入，缺省 #{}）
}.

-type around_fun() :: fun((Request, FCtx, Next) -> Response | {Response, NewFCtx}).
-type Next :: fun((Request) -> Response).
```

---

## 循环也是链上一环

ReAct 工具循环不是 agent 里写死的 while，而是 turn 链**最内层的那个 filter**
（`beamai_agent_tool_loop:loop_filter/1`）。它的 `Next` 不是别的 turn filter，而是
**step 链**——调一次 = 跑一轮迭代：

```
turn 链：[用户 turn filter ...] ++ [循环 filter]
                                      │ Next = step 链
                                      ▼
step 链：[用户 step filter ...] ++ step_terminal（一轮迭代的真正执行）
```

于是「一次对话进出各一次」（`around_turn`）与「每轮被复入一次」（`around_step`）
落在两个不同的钩子上，不再挤在同一层。

### step 契约

step 请求：

| 字段 | 含义 |
|---|---|
| `messages` | 本轮起始的完整消息序列 |
| `context` | 贯穿全链的共享上下文（逐轮穿线） |
| `iteration` | 已用迭代数（跨中断累计，从 0 开始） |
| `tool_calls_made` | 至此已发生的 tool 调用记录 |

step 响应按 `status` 分四种，循环驱动只认这四种：

| status | 含义 | 额外字段 |
|---|---|---|
| `continue` | 本轮调了工具、结果已并入，继续下一轮 | `messages` / `context` / `tool_calls_made` |
| `final` | 本轮即最终答案（纯文本回复或 return_direct） | `response` |
| `interrupt` | HITL / 环境类暂停 | `type`、`interrupt_context` |
| `error` | 本轮出错，循环终止 | `reason` |

step filter 可以改写请求里的 `messages` / `context`，也可以**不调 Next 直接合成一个
status**——那就是短路掉这一轮迭代（比如按缓存直接给出 `final`）。

```erlang
%% 每轮迭代记一条轨迹，并在超过 5 轮时强制收尾
TraceStep = beamai:filter(<<"trace_step">>, #{
    around_step => fun(#{iteration := I, context := Ctx} = Req, _FCtx, Next) ->
        case I >= 5 of
            true ->
                #{status => final, context => Ctx, messages => maps:get(messages, Req),
                  tool_calls_made => maps:get(tool_calls_made, Req),
                  response => beamai_llm_response:new(
                                #{content => <<"轮次用尽"/utf8>>, finish_reason => stop})};
            false ->
                logger:info("iteration ~p", [I]),
                Next(Req)
        end
    end
}).
```

### 换掉循环策略

循环既然是链上一环，换掉它就不用动 agent 与 ChatClient 的代码——给 agent 配一个
`loop_filter`（构造器，拿到本轮的 LoopOpts，返回一个 `around_turn` filter）：

```erlang
{ok, Agent} = beamai_agent:new(#{
    chat_client => K,
    loop_filter => fun(_LoopOpts) ->
        beamai:filter(<<"my_loop">>, #{
            around_turn => fun(Req, _FCtx, Next) ->
                %% Next 是 step 链：自己决定怎么驱动（plan-execute / reflexion / 树搜索）
                StepReq = #{messages => maps:get(messages, Req),
                            context => maps:get(context, Req),
                            iteration => 0, tool_calls_made => []},
                #{messages := Msgs, tool_calls_made := Made} = Next(StepReq),
                {ok, MyResponse, Made, 1, Msgs}   %% 返回工具循环结果 tuple
            end
        })
    end
}).
```

自定义循环只需守两条：按 step 契约驱动 `Next`，最后返回 turn 链约定的工具循环
结果 tuple（`{ok, Response, ToolCallsMade, Iterations, Messages}` / `{interrupt, _, _}` /
`{error, _}`）。迭代上限、`max_tool_calls` 这类限额是缺省循环的策略，自定义循环自己负责。

> 缺省循环还承担 resume 的一次性分派（首次进入=延续被打断的 turn，递归重入=全新循环），
> 这部分逻辑随循环 filter 走。自定义循环若要支持 HITL resume，得自己处理
> `LoopOpts` 里的 `continuation`。

---

## 重试在哪一层

**不在链上**。provider 的重试在 `beamai_chat_model:chat/3` 内部——位于整个 filter 栈
**之下**，重试重入碰不到任何 filter。于是 `around_chat` 上的记忆/记账每轮只跑一次，
无需任何层序纪律来保证。

```erlang
%% 参数三级取值：单次 Opts > provider Config > 框架默认（max_retries => 0 关闭）
LLM = beamai_chat_model:create(anthropic, #{model => M, api_key => K, max_retries => 5}),
beamai_chat_client:invoke_chat(ChatClient, Messages, #{max_retries => 0}).   %% 本次不重试
```

代价与出口：

- filter **看不到**每次真实尝试（它看到的是「一次逻辑调用」）→ 要观测用 chat opts 的
  `on_retry` 回调；
- 流式路径**不重试**（token 已投递给 sink，重跑会让下游看到重复内容）；要容错就在
  `around_turn` 层重跑整轮。

> 早先版本曾把重试做成 llm 链上的一层 filter（`around_llm`），后来撤销了：重试搬到栈底
> 之后，那条链与 chat 链的进出次数永远 1:1、契约也完全相同，差别退化成「排序」——
> 而排序在本系统里本来就由列表位置决定。详见 `design/retry_back_to_chat_model.md`。

---

## token_transform（token 流变换）

`token_transform` 是流式专用钩子（对照 clj-agent `:token-xf`，Spring AI
`StreamAdvisor` 的算子思想）：按 filters **注册顺序**组装成 token 变换链，
作用于送往 on-token sink 的**出站流**。around 链解决"改请求/改响应"，token_transform
解决"逐 token 介入"——改写、吞掉、缓冲后批量放行。

```erlang
-type token_data() :: #{token := binary(), meta := map()}.
-type token_transform() :: #{
    init  => term(),      %% 状态初值（缺省 undefined）
    step  := fun((token_data(), State) -> {[token_data()], State}),  %% 1→N
    flush => fun((State) -> [token_data()])   %% 流正常结束时冲出缓冲残留（可选）
}.
```

三条硬能力（Erlang 无 transducer，step/flush 是其等价表达）：

- **1→N**：`step` 一个 token 进、0/1/N 个出（吞掉 = 空列表，缓冲 = 攒在 State 里）；
- **跨 chunk 状态**：State 显式穿线，作用域 = **单次 LLM 流**（terminal 每次
  执行现场按 `init` 初始化，工具循环每轮各自新状态）；
- **流末 flush**：流**正常**结束后级联调用各层 `flush`（外层残留经内层 step
  传播再送 sink，之后内层自己 flush）；**错误路径不 flush**——缓冲丢弃，
  半截答案不外泄。

**硬边界：token 链只改"交付"，不改"答案"。** 变换的仅是送给 TokenCallback
的出站流；`invoke_chat_stream` 返回的归一化响应**不经过它**——memory 落库、
turn 结果、后续工具循环用的都是原始完整答案。分工：

| 要改什么 | 用哪条链 |
|---|---|
| 用户实时看到什么（脱敏/吞半截/缓冲放行） | `token_transform` |
| 这个 turn 的最终答案是什么（校验重试/改写） | `around_turn`（validation_turn_filter） |

同步路径（`invoke_chat`）完全忽略 `token_transform`；无 token_transform filter 时流式路径
零开销退化（TokenCallback 原样直通）。

内置的 `token_redact_filter` / `hold_release_filter` 见[内置 filter](#内置-filter)。

```erlang
%% 流式脱敏：sink 看到脱敏后的 token，最终响应仍是原文
K = beamai:chat_client(#{}, [
    beamai_filters:token_redact_filter(<<"sk-\\w+">>, <<"[KEY]">>)
]),
{ok, Resp, _} = beamai_chat_client:invoke_chat_stream(K, Messages, #{}, OnToken).
```

---

## 内置 filter

`beamai_filters` 里的现成 filter，均为纯构造器，建 ChatClient 时放进 `new/2` 的 filters 列表。
（大体对标 Spring AI 的 Advisor 体系，逐项取舍见 `design/spring_advisor_alignment.md`。）

| filter | 链 | 说明 |
|---|---|---|
| `beamai_agent_tool_loop:loop_filter(LoopOpts)` | turn | **缺省 ReAct 循环**，由 agent 自动追加在 turn 链最内层；agent 配 `loop_filter` 可整体替换 |
| `logging_filter()` | turn/chat/tool | 三链各记一对 debug 日志。放列表首位记全景；放在某 filter 之后则只看得到那层之内的改写 |
| `safeguard_filter(Words)` / `(Words, Opts)` | chat | 敏感词命中即短路，不调 LLM，返回 `finish_reason=content_filtered` 的答复。Opts：`failure_response`、`case_sensitive`（缺省 `false`） |
| `timeout_filter(Ms)` | tool | 单个工具执行墙钟超时 → `{error, timeout}`（归类 transient） |
| `approval_filter(ApproveFun)` | tool | 仅拦 `sensitive => true` 的工具；拒绝结果作正常工具结果回模型。非交互式——交互式审批用 callbacks 的 `on_tool_call` |
| `validation_turn_filter(ValidateFun, MaxRetries)` | turn | 最终答案校验，不合格把原因作反馈重入循环；耗尽则原样返回 |
| `schema_validation_turn_filter(Schema, MaxRetries[, Opts])` | turn | 上者的 JSON Schema 特化，见下 |
| `token_redact_filter(Pattern, Replacement)` | token | 无状态逐 token 正则脱敏。已知限制：秘密被切在两个 chunk 之间时漏检 |
| `hold_release_filter(CheckFun)` | token | 先审后放：缓冲整流不外泄，完流时全文审查 |

### safeguard_filter：能力边界

```erlang
K = beamai:chat_client(#{}, [beamai_filters:safeguard_filter([<<"敏感词"/utf8>>])]).
```

放 chat 链意味着**循环内每次 LLM 调用都过一遍**：不止拦用户输入，工具结果回灌时带出的
敏感内容同样拦得住。

但它就是子串匹配，**不是内容安全**：变形、拼音、Unicode 同形字、跨消息拼接一概拦不住。
当粗筛与兜底用，真要做内容安全请接专门的审核模型。

### schema_validation_turn_filter：结构化输出自纠

```erlang
Schema = #{type => object,
           properties => #{<<"name">> => #{type => string},
                           <<"age">> => #{type => integer, minimum => 0}},
           required => [<<"name">>, <<"age">>]},
K = beamai:chat_client(#{}, [beamai_filters:schema_validation_turn_filter(Schema, 2)]).
```

「取文本 → 解 JSON → 过 Schema」不合格则把 Schema 错误当反馈重入循环，重试 `MaxRetries`
次；耗尽则原样返回最后一次（仍不合格的）响应，**不抛错**——失败留到下游解析时才浮现。

Schema 直接复用 `beamai_tool` 的参数 Schema 形态（atom 键亦可），校验器为 `beamai_json_schema`
（零依赖，DRAFT 2020-12 的实用子集；不支持 `$ref`/`$defs`/`patternProperties`/`if-then-else`/`format`）。

Opts：`max_errors`（单次最多收集几条错误，缺省全收；字段多的 Schema 建议设 5~10，
否则错误全塞进反馈会撑爆提示词）、`code_fence`（是否剥离 ```` ```json ```` 围栏，缺省 `true`）。

配 provider 原生结构化输出（json_schema）使用效果最好——原生约束负责大多数情形，本 filter
兜住漏网的。

### 工具检索：大工具集按需揭示

`beamai_tool_search` 把「注册」（能不能执行）与「广播」（模型看不看得见）拆开：全量注册，
但首轮只广播一个 `tool_search` 工具。模型想干活就得先描述需求检索，拿回工具名后下一轮才
看得见对应工具。Spring 实测 28 个工具时省 34~64% token。

```erlang
Tools = [...],                                    %% 全量工具
{SearchTool, Filter} = beamai_tool_search:new(Tools, #{}),
K0 = beamai_chat_client:new(#{}, [Filter]),
K = beamai_chat_client:add_tools(K0, [SearchTool | Tools]).   %% 全量注册
```

Opts：`index_module`（缺省 `beamai_tool_index_keyword`，另有 `beamai_tool_index_regex`）、
`index_opts`、`max_results`（缺省 5）、`accumulate`（缺省 `true`，历次检索取并集；`false` 只
认最近一次）、`tool_name`（缺省 `<<"tool_search">>`）。

要点：

- **未索引的工具原样透传**——广播列表里 filter 没索引过的（如 agent 运行时追加的中断工具）
  一概不碰，不会被静默吃掉。
- **检索工具永远广播**，否则一轮不中就再没机会检索。
- **模型仍可调用未广播的工具**（ChatClient 执行不看广播列表），故它凭上文记忆直接调也不会失败。
- 索引后端是 behaviour（`beamai_tool_index`），向量后端留给 beamai_extra 接。
- 不自动注入 system 提示（避免双 system 消息）；需要加强引导时自行把
  `beamai_tool_search:default_system_suffix/0` 拼进 `system_prompt`。

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

其余各链同理，把上面的 `around_chat` 换成该链的 hook 名（Phase = `around_step` /
`around_tool` / `around_turn`）。

turn 链有一处特别：它的 terminal 是**内层 step 链**（循环 filter 每调一次 Next 就跑一轮迭代），
所以同一个 filter 在两条链上的相对层序保持一致（A 始终在 B 外面）。

---

## API 参考

### beamai_filter 模块

#### 构造器

```erlang
%% 创建 filter（私有状态初值 #{}）。
%% Hooks 为 hook map，可含 around_chat/around_step/around_tool/around_turn/
%% token_transform 任意子集。
-spec new(Name :: binary(), Hooks :: hooks()) -> filter().

%% 创建 filter（指定私有状态初值 Init）
-spec new(Name :: binary(), Hooks :: hooks(), Init :: map()) -> filter().
```

其中 hook 形态：

```erlang
-type hook_type() :: around_chat | around_step | around_tool | around_turn |
                     token_transform.
-type hooks() :: #{
    around_chat => around_fun(),
    around_step => around_fun(),
    around_tool => around_fun(),
    around_turn => around_fun(),
    token_transform => token_transform()
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
%% Phase 指定该链用哪个 around hook：chat 链传 around_chat，step 链传
%% around_step，tool 链传 around_tool。只参与该链（含对应 around）的 filter
%% 进入洋葱，其余跳过。
%% Terminal 产出最内层响应，出错时 throw；run/4 用 try/catch 捕获，
%% 统一返回 {ok, Response} | {error, Reason}。
-spec run(Filters :: [filter()],
          Phase :: hook_type(),
          Terminal :: fun((Request) -> Response),
          Request :: map()) -> {ok, Response} | {error, Reason}.

%% 把 filter 列表与 terminal 合成为单个洋葱函数（自行按 Phase 过滤，可直接传
%% 整份 filters 列表）。不捕获 throw——嵌套使用时（chat 链的 terminal 就是
%% llm 链）由最外层的 run/4 统一捕获。
-spec compose(Filters :: [filter()], Phase :: hook_type(),
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

### beamai_chat_client 集成

```erlang
%% 构建 ChatClient 时一次性给出全量 filter（注册顺序即层序：列表靠前 = 外层）
beamai_chat_client:new(Settings, Filters) -> ChatClient.
```

filter 在构建后**不可增量追加**——层次完全由这份列表的顺序决定。
需要会话记忆时把 `beamai_memory_filter:memory_filter(Store)` 放列表**首位**
（最外层：先展开完整历史，再让内层 filter 处理）。

工具模块（`beamai_chat_client:add_tool_module/2`）只提供工具，不携带 filter。

> **system_prompts 注入层次**：`invoke_chat` 的 `Opts` 里给出的 `system_prompts`
> 在调用时作为**最内层**临时 filter 追加——在所有用户 filter 之后、LLM 之前
> 前置系统消息。因此用户 chat filter 看到的 messages **不含**系统提示，
> memory filter 也永远不会把系统提示存进历史。

### beamai 便捷 API

```erlang
%% 创建 ChatClient（一次性给出全量 filter）
beamai:chat_client(Settings, Filters) -> ChatClient.

%% 快捷创建 filter（直接给 hook map；放入 chat_client/2 的 Filters 列表）
beamai:filter(Name, Hooks) -> Filter.
beamai:filter(Name, Hooks, Init) -> Filter.
```

---

## 使用方法

### 1. 构建 ChatClient 时一次性给出 filter

```erlang
Logger = beamai:filter(<<"logger">>, #{
    %% around_tool：前置记录工具名
    around_tool => fun(#{tool := #{name := Name}, args := Args} = Req, _FCtx, Next) ->
        io:format("Calling tool: ~ts(~p)~n", [Name, Args]),
        Next(Req)
    end
}),
K0 = beamai:chat_client(#{}, [Logger]).
```

### 2. Filter 层次（注册顺序即层序）

列表靠前 = 外层：它的前置先执行、后置后执行。

```erlang
Validator   = beamai:filter(<<"validator">>, #{around_tool => ValidateFn}),
Logger      = beamai:filter(<<"logger">>, #{around_tool => LogFn}),
Transformer = beamai:filter(<<"transformer">>, #{around_tool => TransformFn}),

%% 列表顺序即洋葱层序（validator 最外层，transformer 最内层）
K = beamai:chat_client(#{}, [Validator, Logger, Transformer]).

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

%% 创建 ChatClient（filter 一次性给出）并注册工具
K0 = beamai:chat_client(#{}, [LogDouble]),
K1 = beamai:add_tool(K0, beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }})).

%% 调用（3 + 5 = 8，后置翻倍后 = 16）
%% （工具执行经 ChatClient 的 tool filter 洋葱链）
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

K0 = beamai:chat_client(#{}, [SystemAudit]),
K1 = beamai:add_chat_model(K0, LLMConfig).

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
K = beamai:chat_client(#{}, [Guard]).
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
K = beamai:chat_client(#{}, [Counter]).
%% 工具循环每轮 LLM 调用都让 calls 累加，且与其它 filter 的私有状态互不干扰。
```

更多示例参见 [examples/src/example_filter.erl](../examples/src/example_filter.erl)。

---

## 与 middleware 的对应

本系统采用通用的 **around（环绕）middleware** 形态：

- around 闭包 `fun(Request, FCtx, Next) -> Response` 对应中间件的「拿到请求 → 调 `next` → 处理响应」。
- 前置/后置同处一个闭包，用局部变量桥接，无需在 before/after 间借共享上下文传值。
- 短路 = 不调 `Next`；重试 = 多次调 `Next`；无需专门的 halt 协议。
- 一个 filter 同时打包多条链的 around（`around_turn` / `around_step` / `around_chat` / `around_tool`），各链独立选用。
- 工具循环本身也是链上一环（turn 链最内层的 filter），它的 `next` 就是一轮迭代——对应 Spring AI 的 `ToolCallingAdvisor`。

---

## 与 Middleware 框架的关系

[beamai_extra](https://github.com/TTalkPro/beamai_extra) 扩展项目中提供了更高级的 Middleware 系统（位于 beamai_tools），支持有状态管理、预设配置、调用限制、人工审批、重试和降级等功能。Middleware 内部转换为 filter 注册到 ChatClient，两者最终在同一条洋葱链中执行。

| 特性 | Filter（本文档） | Middleware（beamai_extra） |
|------|-------------------|---------------------------|
| 复杂度 | 轻量级，私有上下文限单次 invoke | 完整框架，跨调用状态管理 |
| 预设配置 | 无 | 提供 production/development 等预设 |
| 内置功能 | 无 | 调用限制、人工审批、重试、降级 |
| 适用场景 | 简单包裹：日志、校验、注入、缓存 | 复杂控制：限流、重试、降级 |

---

## 更多资源

- [beamai_core README](../apps/beamai_core/README.md) - ChatClient 架构文档
- [MEMORY.md](MEMORY.md) - 会话记忆（Memory filter）
- [API 参考](API_REFERENCE.md) - API 参考文档
