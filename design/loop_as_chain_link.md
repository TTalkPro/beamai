# 循环 advisor：把 tool_loop 变成 turn 链上的一环

> **状态：✅ 已实施（2026-08-25）。** 全套 `rebar3 eunit` 692 tests / 0 failures、
> `rebar3 dialyzer` EXIT=0（零警告）。
> 承接 `design/chat_llm_layer_split.md`（那次补的是第 4 层 provider 中间件 `around_llm`）。
> 目标是四层 advisor 模型里剩下的第 2 层：**循环 advisor**。

---

## 0. 缺口

拆完 chat/llm 之后，四层模型只剩循环层还是「有行为、不在链上」：

```
外层 advisor      around_turn            ✅
循环 advisor      beamai_agent_tool_loop ⚠️ 内置的 while，不是链上一环
内层 advisor      around_chat            ✅（但只包 LLM 调用，不包该轮工具执行）
provider 中间件   around_llm             ✅
```

两个具体后果：

1. **换不掉循环**。plan-execute / reflexion / 树搜索只能靠 `around_turn` 不调 `Next`
   自己从头实现，或者用工具循环结果的第 5 元 `Messages` 反复重入——那是绕过循环，
   不是替换循环。
2. **没有「一轮迭代」这个粒度**。`around_chat` 只包 LLM 调用；该轮的工具执行发生在
   它返回之后，落在 `around_tool` 上。想要「包住一整轮 ReAct 迭代（chat + 这批工具）」
   —— 每轮预算、轨迹记录、按轮短路——没有钩子。

---

## 1. 方案

循环变成 turn 链**最内层的一个 filter**，它的 `Next` 是 **step 链**（一轮迭代）：

```
turn 链：[用户 turn filter ...] ++ [循环 filter]
                                     │ Next = step 链
                                     ▼
step 链：[用户 step filter ...] ++ step_terminal（一轮迭代的真正执行）
```

1. **新增 hook `around_step`**（`beamai_filter`）：包一轮迭代，含该轮工具执行。
2. **`beamai_agent_tool_loop` 拆成「驱动」与「单步」**：
   - `loop_filter/1` → `around_turn` filter：组装起始消息（记忆载入/持久化）、判限额
     （`max_tool_iterations` / `max_tool_calls`）、按 step 响应的 `status` 决定继续或收尾、
     把最终状态折成工具循环结果 tuple。
   - `step_terminal/1` → step 链最内层：一轮迭代（prepare → on_llm_call → invoke_chat(_stream)
     → assistant 入库 → 中断检测 → 执行工具 → 结果入库 → return_direct 判定）。
3. **状态改走请求/响应**：原先靠递归参数与 `Opts#{messages => ...}` / `with_ctx/2` 穿线的
   `messages` / `context` / `tool_calls_made` / 迭代计数，现在全部在 step 请求与响应里显式
   流动——step filter 因此看得见也改得动。`chat_opts` 里的 context 退化为兜底，每轮由
   step_terminal 用请求里的 context 覆写。
4. **resume 的一次性分派下沉进循环 filter**：原先在 turn 链 terminal 里用 atomics CAS 分派
   （首次=延续被打断的 turn，递归重入=全新循环），现在成为 `LoopOpts.continuation`：
   返回 `{result, TurnResult}`（直接短路，如重跑后仍失败要再暂停）或 `{loop, OptsOverride}`
   （用续接消息跑循环）。CAS 留在循环 filter 内部。
5. **循环可替换**：agent 配置 `loop_filter => fun((LoopOpts) -> beamai_filter:filter())`；
   `beamai_agent:run_turn_chain/3` 取它构造链上那一环，agent 与 kernel 代码零改动。

### step 契约

- 请求：`#{messages, context, iteration, tool_calls_made}`（`iteration` 为已用迭代数，跨中断累计）
- 响应：`#{status := continue | final | interrupt | error, messages, context, tool_calls_made, ...}`
  - `final` 带 `response`；`interrupt` 带 `type` + `interrupt_context`；`error` 带 `reason`
  - 驱动只认这四种 status，不认识的一律 `{error, {invalid_step_response, _}}`——不把循环挂死

step filter 不调 `Next` 直接合成一个 status，就是短路掉这一轮迭代。

---

## 2. 破坏性变更

- `beamai_agent_tool_loop` 的内部结构全变（`iterate/3`、`do_iterate/3`、`with_ctx/2` 等私有函数
  没了）；公开的 `run/2` 保留且语义不变（内部用一个「continuation 返回空 override」的循环
  filter 直跑，不经 turn 链）。
- `loop_opts()` 类型里 `messages` 由必填改为可选：经 turn 链时消息来源由驱动从 turn 请求或
  continuation override 填入。
- `beamai_agent` 私有 `run_loop/4` → `loop_opts/2`；`build_chat_opts/2` 不再认
  `turn_context` / `init_state`（context 改为逐轮穿线）。
- agent state 新增 `loop_filter` 字段（缺省 `undefined`）。

行为上的一处有意统一：resume 续接循环的 context 原先取 `conversation_id + saved_state`
（忽略 turn 请求里的 context），现在与全新循环一样取 turn 请求的 context——turn filter 对
context 的改写因此对 resume 续接也生效（`resume_context/2` 本就把 saved_state 放进了 turn 请求）。

## 3. 验证锚点

`apps/beamai_agent/test/beamai_step_filter_tests.erl`（7 例）：

- 粒度：两轮迭代下 turn 进 1 次、step 进 2 次、chat 进 2 次、tool 进 1 次；
- step 请求带进度：`{iteration, tool_calls_made 长度}` 依次为 `(0,0)`、`(1,1)`；
- status 序列 `continue → final`；
- **层序断言**（本次的关键）：
  `turn_in → step_in → chat_in → chat_out → tool_in → tool_out → step_out → step_in → ... → turn_out`
  ——step 确实包住了该轮的工具执行；
- step filter 短路一轮迭代：不调 Next 合成 `final`，LLM 零调用；
- 换循环：自定义 `loop_filter` 只驱动一轮就收尾（LLM 一直要工具），对照组用缺省循环则跑到
  `max_tool_iterations`。

既有 685 例（含 HITL/resume/env_retry/timeline/return_direct/max_tool_calls 全套）无一改动即通过。
