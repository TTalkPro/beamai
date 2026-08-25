# chat / llm 分层：把重试从 chat 链下沉到 provider 层

> **状态：✅ 已实施（2026-08-25）。** 全套 `rebar3 eunit` 685 tests / 0 failures、
> `rebar3 dialyzer` EXIT=0（零警告）。
> 起因：对照「外层 advisor → 循环 advisor → 内层 advisor → provider 中间件」这套
> 四层 advisor 模型（Spring AI / clj-agent 的层次）逐层核对 beamai 的 filter 体系。

---

## 0. 现状盘点与缺口

核对结果（provider = 真正执行一次 LLM chat 的那层）：

| 四层模型 | beamai 拆分前 | 结论 |
|---|---|---|
| 外层 advisor（每对话一次） | `around_turn`（`beamai_agent:run_turn_chain/3`） | ✅ |
| 循环 advisor | `beamai_agent_tool_loop`（内置，不在链上） | ⚠️ 有行为、不可插拔（本次不动） |
| 内层 advisor（每轮复入一次） | **无独立钩子** | ❌ 缺口 |
| provider 中间件（每次 LLM 调用一次） | `around_chat`（其 terminal 就是 `Module:chat/3`） | ✅ |

**缺的是第 3 层，不是第 4 层**：`around_chat` 的洋葱最内层字面上就是 provider 调用，
它占的是最底下那格；「每轮恰好一次」这一格是空的，两层塌成了一层。

塌层的可观察后果：只要有一个外层 `around_chat` filter 重入 `Next`（重试、fallback、
自纠错、N-best 投票），**它内层所有 around_chat filter 都跟着跑 N 次**——包括语义上
「每轮只该一次」的记忆 / 记账 / 审计。`beamai_memory_filter` 的注释里已经留了化石：

> 唯一的重入隐患是外层 filter 拿同一 delta 重跑内层（同一 delta 会被存两次）。这要求
> 把重试类 filter 放在本 filter **之外**，与「memory 放列表首位」的约定相悖……

即：因为没有第 3 层，记忆（第 3 层的东西）与重试（第 4 层的东西）被迫在同一条链上争
层序，怎么排都有一边是错的。当时选择用「约定放首位」回避。

框架自带的重试当时埋在 `beamai_chat_completion:chat/3` 内部（比 `around_chat` 还低一
层），所以这个雷没被自家代码引爆——代价是重试对整条 filter 链完全不可见：数不清真实
调用次数、拿不到失败响应、也没法替换退避策略。

---

## 1. 方案

chat 侧从一条链改成**两层嵌套**的洋葱：

```
around_turn          每 turn 一次
  [工具循环]
    around_chat      每轮一次            ← 重试碰不到这层
      around_llm     每次真实请求一次    ← 重试在这层重入 Next
        provider     真正的 chat 调用
    around_tool      每个 tool call 一次
```

1. **新增 `around_llm` hook**（`beamai_filter`）。语义：包裹一次真实 LLM 请求。
   Request 与 chat 同形；流式路径额外带 `stream => true`。
2. **`beamai_kernel:run_chat/6` 与 `run_chat_stream/7`**：chat 链的 terminal 不再是
   `chat_terminal/1`，而是 `llm_chain/3` 合成出来的内层链（其 terminal 才是 provider 调用）。
   `beamai_filter_chain:compose/3` 改为自行按 Phase 过滤，可直接传整份 filters 列表；
   `run/4` 仍在最外层统一捕获 throw（内层链不重复捕获）。
3. **重试变成 filter**：新增 `beamai_llm_filters:retry_filter/0,1`（around_llm），
   内部仍复用 `beamai_llm_retry`（错误分类 / Retry-After 退避不变），只是把链的 throw
   契约与 `{ok,_}|{error,_}` 契约就地互转。
4. **`beamai_chat_completion:chat/3` 去掉重试**，退回单次请求——否则 filter 层与模块层
   会双重重试（3×3=9 次）。
5. **缺省注入**：kernel 按 settings 的 `llm_retry` 把 retry_filter 追加在 llm 链**最内层**，
   缺省即 `#{}`（框架默认参数）。经 kernel / agent 的调用行为与拆分前一致。
   `llm_retry => false` 关闭注入，使用方可把 retry_filter 放到任意层序上。
   core 不能反向依赖 llm（会成环），故用 `code:ensure_loaded/1` 运行时探测——与
   `beamai:add_llm/3 → beamai_chat_completion:create/2` 同一套约定。

### 层序取舍

缺省注入在**最内层**：使用方自己的 around_llm filter（限流、记账、mock）一律在重试
之外，看到的是「逻辑一次调用」。要观测每一次真实尝试，就 `llm_retry => false` 再自己
把 filter 排到重试之内。

### 流式不重试

`invoke_chat_stream` 的 Req 带 `stream => true`，内置 retry_filter 见此标记直接透传：
token 已经投递给 sink，重跑会让下游看到重复内容（拆分前 `stream_chat` 本就不重试，
行为不变）。llm 链本身在流式路径照常生效。

---

## 2. 破坏性变更

- **`beamai_chat_completion:chat/3` 不再重试**（单次请求）。经 kernel / agent 的调用不受
  影响（缺省注入补上了）；**直连该模块的调用方要自己包 `beamai_llm_retry:run/2`**。
  项目内唯一的直连调用方 `beamai_llm_helper` 已就地补上。
- `beamai_kernel` 私有函数 `run_chat/5`→`/6`、`run_chat_stream/6`→`/7`（多带 Settings）。
- `beamai_filter_chain:compose/3` 现在自行过滤 Phase（原先要求调用方先过滤）——传已过滤
  的列表仍然正确（幂等）。

## 3. 验证锚点

`apps/beamai_core/test/beamai_kernel_llm_chain_tests.erl`（7 例）：

- 缺省 retry_filter 被注入（2 次失败 + 1 次成功 = 3 次真实请求）；
- `llm_retry => false` 关闭注入；单次 chat opts（`max_retries => 0`）覆盖默认参数；
- **重试不重入 chat 层**：provider 调 3 次而 `{chat_in}` 只 1 次；
- 位于 retry_filter **之内**的 around_llm filter 看得到 3 次尝试、之外的只看到 1 次；
- 层序：`chat_in → llm_in → llm_call → llm_out → chat_out`；
- 流式路径 llm 链照跑但只发一次请求（不重试）。
