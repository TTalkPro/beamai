# TASK: Filter → 洋葱式 Filter 重构（对齐 Spring AI before/after）

> 把分阶段扁平 filter（4 个独立 hook、优先级模拟洋葱）改成真正的洋葱式 filter。
> 最终决策（用户确认）:
> ① 名字仍叫 **filter**；② 仍是 **4 个 hook 点**（pre_chat/post_chat/pre_tool/post_tool）；
> ③ chat 与 tool 的前后**分开**；④ 一个 filter 绑定这 4 个可选 hook，
>    同一 filter 的 pre/post 配成**一层洋葱**包裹同一次调用（回程自动逆序，非优先级模拟）。

## 设计要点
- 原语:`compose([A,B], Terminal)` → `A(req, fun->B(req, fun->Terminal(req)))`,回程自动逆序。
- advisor: `#{'__advisor__', name, type(chat|tool), order, around :: fun(Req, Next) -> Resp}`,order 越小越外层。
- before/after 便捷:`around = fun(Req, Next) -> AfterFun(Next(BeforeFun(Req)))`;BeforeFun 返回 `{halt, Resp}` 可短路。
- chat:  Req `#{messages, context, opts}` → Resp `#{response, context}`,terminal = `Module:chat`。
- tool:  Req `#{tool, args, context}` → Resp `#{result, context}`,terminal = `beamai_tool:invoke`。
- 错误:terminal `throw`,`beamai_advisor_chain:run/3` try/catch 返回 `{error, Reason}`。

## 执行步骤

### 步骤 1：新增 `beamai_advisor`（behaviour + 构造器）
- [ ] `apps/beamai_core/src/kernel/beamai_advisor.erl`
- [ ] 类型 `advisor()/advisor_type()/request()/response()/next()`
- [ ] `new/4`(around)、`before_after/5`、`before/4`、`after_/4`、`sort/1`

### 步骤 2：新增 `beamai_advisor_chain`（洋葱链）
- [ ] `apps/beamai_core/src/kernel/beamai_advisor_chain.erl`
- [ ] `compose/2`、`run/3 -> {ok, Resp} | {error, Reason}`(try/catch terminal throw)

### 步骤 3：memory advisor（合并为单 advisor）
- [ ] `beamai_memory_filter.erl` → 重写为 `beamai_memory_advisor.erl`
- [ ] `memory_advisor/1` 返回单个 chat advisor(before 存 delta+展开、after 存回复)
- [ ] 保留 `response_to_message/1`

### 步骤 4：改造 `beamai_kernel`
- [ ] state `filters` → `advisors`;`add_filter/2` → `add_advisor/2`
- [ ] `maybe_add_filters`(filters/0) → `maybe_add_advisors`(advisors/0)
- [ ] `with_memory/2` 挂单个 memory advisor
- [ ] system_prompts → 单 before advisor(order 大,内层),per-invoke 追加
- [ ] `invoke_tool` 走 tool advisor 链;chat 调用走 chat advisor 链
- [ ] 删除 run_invoke_pipeline/run_chat_pipeline/execute_*_filter

### 步骤 5：`beamai_tool_behaviour`
- [ ] `filters/0` 回调 → `advisors/0`(返回 `[beamai_advisor:advisor()]`)

### 步骤 6：facade `beamai.erl`
- [ ] `add_filter/2,4` → `add_advisor/2` + 便捷 `add_advisor/5`(before_after) 或 around

### 步骤 7：删除 + 注册
- [ ] 删除 `beamai_filter.erl`
- [ ] `beamai_core.app.src`:移除 beamai_filter/beamai_memory_filter,新增 beamai_advisor/beamai_advisor_chain/beamai_memory_advisor

### 步骤 8：测试
- [ ] `beamai_filter_tests.erl` → `beamai_advisor_tests.erl`(洋葱顺序、before/after、around、halt 短路、tool 链)
- [ ] `beamai_memory_filter_tests.erl` → 改用 advisor API
- [ ] `beamai_kernel_tests.erl` 同步
- [ ] `rebar3 eunit` 全绿

### 步骤 9：文档
- [ ] `docs/FILTER.md/FILTER_EN.md` → ADVISOR(洋葱模型、before/after/around、order 语义)
- [ ] `docs/MEMORY.md/_EN`:去掉 -1000/+1000,改为单 advisor + order
- [ ] `info/filter.md`、core README、根 README 同步

## 完成情况（全部完成 ✅）

数据模型：`filter = #{'__filter__', name, order, hooks => #{pre_chat?, post_chat?, pre_tool?, post_tool?}}`
- pre hook：`fun(Req) -> Req | {halt, Resp}`；post hook：`fun(Resp) -> Resp`
- chat 链用 `(pre_chat,post_chat)`、tool 链用 `(pre_tool,post_tool)`；order 越小越外层
- chat Req `#{messages,context,opts}`→Resp `#{response,context}`；tool Req `#{tool,args,context}`→Resp `#{result,context}`

- `beamai_filter`：new/2,3（hooks map）、sort/1、hook/2 ✅
- `beamai_filter_chain`：run/4（Phase={pre,post}）、compose/4，terminal throw→{error} ✅
- `beamai_memory_filter`：单 filter（pre_chat 存 delta+展开、post_chat 存回复）+ response_to_message ✅
- `beamai_kernel`：filters 字段、add_filter、with_memory 挂单 filter、system_prompt filter(仅 pre_chat,-500)、
  invoke_tool 走 {pre_tool,post_tool}、chat 走 {pre_chat,post_chat}、删除旧 pipeline ✅
- `beamai_tool_behaviour`：filters/0 ✅；facade `beamai:add_filter/2,3` ✅
- app.src 注册 beamai_filter/beamai_filter_chain/beamai_memory_filter ✅
- 测试 `beamai_filter_tests`（洋葱顺序/链按 hook 选择/halt/tool 链/error）、`beamai_memory_filter_tests`、
  `beamai_kernel_tests`（pre_tool/post_tool/halt）；`examples/example_filter.erl` ✅
- 文档 FILTER.md/_EN（Filter 4-hook 洋葱）、MEMORY.md/_EN、info/filter.md、core README×2、根 README×2 全部同步 ✅

最终：`rebar3 compile` 4 app 通过；`rebar3 eunit` 185 tests / 0 failures；文档无旧 API 残留（仅 Spring AI 对比处提及 advisor 概念）。
