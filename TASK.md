# TASK: ToolCallingManager —— 分离工具执行（PR1：纯重构，零行为变化）

> 来源：`design/tool_calling_manager_design.md`（移植自 clj-agent 同名设计文档 v3，
> 针对 beamai/Erlang 适配）。本文为 **PR1**——把 `execute_tools` 升格为可注入
> behaviour，行为零变化。
>
> **v3 适配**（clj-agent v3 用户收敛）：
> 1. 协议方法改名 `execute_batch` → `execute_tool_calls`（Spring 对齐）
> 2. 多 impl：concurrent（默认，尊重 parallel/serial）+ sequential（强制串行）
> 3. `execute_tools/5` 和 `run_one_tool/3` 是实现内部 helper，不是协议方法

## 语义决定

1. **新增 behaviour `beamai_tool_calling_manager`**（agent 模块），定义
   `execute_tool_calls/4` callback + `execute_tool_calls/4` 分派器（与
   `beamai_memory_provider` 的 `{Mod, Ref}` + 分派 API 模式完全一致）。
2. **多 impl（v3 核心）**：
   - `beamai_concurrent_tool_calling_manager`（默认）——spawn_monitor 并发，
     尊重 `parallel` / `serial`（现状行为）
   - `beamai_sequential_tool_calling_manager`——全串行，忽略 parallel opts，
     适合调试 / 严格副作用场景
3. **`agent_state` 增 `tool_calling_manager` 字段**——`create/1` 解析，缺省 concurrent。
4. **全部 4 个 `execute_tools` 调用点**改为经 manager：
   - `beamai_agent_tool_loop:execute_and_continue/4`（:243，正常路径）
   - `beamai_agent_tool_loop:handle_interrupt/6`（:206，中断安全 tools）
   - `beamai_agent:resume_approval_raw/4`（:465，审批恢复执行）
   - `beamai_agent:resume_env_raw/3`（:492，环境重跑）
5. **`beamai_agent_utils:execute_tools/5` 完全保留不动**——concurrent/sequential manager 委托给它。
6. **`beamai_kernel` / `beamai_tool` 完全不动**——PR1 只动 agent 层。
7. **零行为变化**——全套现有测试原样通过。

## 返回值映射

现有 `execute_tools` 返回 `{ToolMsgs, Records, Ctx}` 三元组。
behaviour callback 返回 `#{messages, records, context}` map（避免 arity 爆炸）。
调用点 pattern match 解构。

## 批次 A：behaviour 定义（新模块）

- [x] A1 新模块 `beamai_agent/src/beamai_tool_calling_manager.erl`：
      `-callback execute_batch(Ref, Kernel, ToolCalls, Opts) -> Result`；
      导出类型 `manager/0`、`execute_opts/0`、`execute_result/0`；
      分派函数 `execute({Mod,Ref}, Kernel, ToolCalls, Opts) -> Result`
      （与 `beamai_memory_provider:history/2` 同款）
- [x] A2 dialyzer：新模块无 warning（-spec 完整）

## 批次 B：默认实现（新模块）

- [x] B1 新模块 `beamai_agent/src/beamai_default_tool_calling_manager.erl`：
      `-behaviour(beamai_tool_calling_manager)`；`new/0` 返回 `{?MODULE, default}`；
      `execute_batch/4` unpack opts map 后调 `beamai_agent_utils:execute_tools/5`，
      把返回的三元组包成 result map
- [x] B2 dialyzer：新模块无 warning

## 批次 C：agent_state 接入

- [x] C1 `beamai_agent_state`：`agent_state()` 类型增 `tool_calling_manager` 必填字段；
      `create/1` 解析 Config 的 `tool_calling_manager`（缺省 `default:new()`）；
      `{Mod, Ref}` 格式校验（Mod 为 atom）

## 批次 D：tool_loop 调用点改造（2 处）

- [x] D1 `beamai_agent_tool_loop`：`loop_opts()` 增 `tool_calling_manager` 字段；
      `execute_and_continue/4` 改调 `beamai_tool_calling_manager:execute/4`
      （opts map 含 context/parallel/on_result）；返回值 pattern match 解构
- [x] D2 `handle_interrupt/6` 同样改调 manager
- [x] D3 `beamai_agent:run_loop/4`（透传 `tool_calling_manager` 进 LoopOpts）

## 批次 E：agent resume 调用点改造（2 处）

- [x] E1 `beamai_agent:resume_approval_raw/4`（:463）改调 manager（parallel=false）
- [x] E2 `beamai_agent:resume_env_raw/3`（:485）改调 manager（用 agent parallel_tools）

## 批次 F：测试

- [x] F1 mock manager 注入测试：注入 `{beamai_mock_tcm_impl, Ref}` 返回固定结果，
      验证 execute 分派到 mock 而非 execute_tools
- [x] F2 边界验证：注入 default manager 时 around_tool filter 仍触发（正交性证明）
- [x] F3 额外测试：返回值 map 结构、空 ToolCalls、opts 缺省值、agent_state 注入/缺省

## 批次 G：验证

- [x] G1 `rebar3 eunit`（全量）全部通过——**593 tests, 0 failures**（基线 584 + 9 新增）
- [x] G2 `rebar3 dialyzer` 零新增 warning（基线 34 个 pre-existing 不变，EXIT=1 同基线）
- [x] G3 不 commit：待用户 review

## 明确出界

> 下列 4 条为 PR1 当时的划界。PR2 已对前 3 条作出处置，勿按原文理解——
> 详见文末 **PR2**。

- ~~backend 分派（local/http/mcp）——PR2~~ → **已否决**（PR2「一、工具定义方案」）
- ~~`beamai_tool:invoke/3` 改动——PR2~~ → PR2 已改（超时真正执行）
- ~~`beamai_kernel` / `beamai_tool` 任何改动——PR2~~ → PR2 改了 `beamai_tool`、
  `beamai_tool_error`、`beamai_context`；`beamai_kernel` 仍零改动
- execute_tools 逻辑搬家（从 utils 搬进 default manager）——PR1 不搬，只委托
  （PR2 仍未搬：manager → batch_worker → 委托 `execute_tools/5`）

## 完成记录（2026-07-15 全部实施，v3 适配）

- **全绿**：`rebar3 eunit` 597 tests / 0 failures（基线 584 + 13 新增）；
  `rebar3 dialyzer` 零新增 warning（基线 34 个 pre-existing 不变）。
- **v3 适配**：
  - 协议方法改名 `execute_batch` → `execute_tool_calls`（Spring 对齐）
  - 多 impl：concurrent（默认）+ sequential，选 manager 即选策略
  - `execute_tools/5`（批调度）和 `run_one_tool/3`（单工具）是实现内部 helper
- **A behaviour**：新模块 `beamai_tool_calling_manager`（agent 层）——
  `-callback execute_tool_calls/4` + `execute_tool_calls/4` 分派器 +
  构造器 `default/0`（→concurrent）、`concurrent/0`、`sequential/0` +
  类型 `manager/0` / `execute_opts/0` / `execute_result/0`。
- **B concurrent 实现**：新模块 `beamai_concurrent_tool_calling_manager`——
  `-behaviour(beamai_tool_calling_manager)`；`new/0` 返回 `{?MODULE, default}`；
  `execute_tool_calls/4` unpack opts map → 调 `execute_tools/5`（透传 parallel）→
  包成 result map。逻辑零重复、零改动。
- **B sequential 实现**：新模块 `beamai_sequential_tool_calling_manager`——
  与 concurrent 的唯一差异：`parallel` 永远传 `false`，忽略 opts。
  证明多 impl 真能换策略（测试验证无重叠 vs 有重叠）。
- **C agent_state**：`agent_state()` 增 `tool_calling_manager` 必填字段；
  `create/1` 增 `setup_tool_calling_manager/1`——缺省 `default()`（→concurrent）。
- **D tool_loop 改造**（2 处）：`execute_and_continue/4` + `handle_interrupt/6`
  从直调 `execute_tools/5` 改为经 `beamai_tool_calling_manager:execute_tool_calls/4`；
  `loop_opts()` 增 `tool_calling_manager` 字段；`run_loop/4` 透传。
- **E resume 改造**（2 处）：`resume_approval_raw/4`（审批恢复执行单工具）+
  `resume_env_raw/3`（环境重跑失败调用）改调 manager。
- **F 测试**（13 个）：behaviour 基础（concurrent 委托、格式、default=concurrent、
  缺省 opts）；mock 注入（独立 mock 模块）；agent_state 注入（自定义/缺省/sequential）；
  filter 正交性；**多 impl 切换**（sequential 无重叠 vs concurrent 有重叠）；
  返回值结构（map 三键、空 ToolCalls、多工具按序）。
- **不变**：`beamai_agent_utils:execute_tools/5` 完全保留不动；
  `beamai_kernel` / `beamai_tool` 零改动。
- **4 个调用点全部迁移**：
  | # | 模块 | 函数 | 行 | 场景 |
  |---|---|---|---|---|
  | 1 | beamai_agent_tool_loop | execute_and_continue/4 | :243 | 正常路径 |
  | 2 | beamai_agent_tool_loop | handle_interrupt/6 | :206 | 中断安全 tools |
  | 3 | beamai_agent | resume_approval_raw/4 | :465 | 审批恢复执行 |
  | 4 | beamai_agent | resume_env_raw/3 | :492 | 环境重跑 |

---

# PR2：工具定义方案 + 执行隔离 + 超时（2026-07-16）

> PR1 只搬了接口、行为零变化。PR2 回答两个问题：**工具怎么定义**（后端要不要
> 进 tool_spec），以及 **TCM 作为执行引擎该保证什么**（隔离 / 超时 / 谁来决定
> 何时放弃）。与 PR1 不同，本轮**有行为变化**，且动了 `beamai_core`。

## 一、工具定义方案：handler 闭包，**不引入 backend 字段**

**否决** `design/tool_calling_manager_design.md` §5（给 tool_spec 加
`backend => local | http | mcp` + `beamai_tool:invoke/3` 分派 + kernel settings
注册 `tool_backends`）。工具自己处理 MCP / HTTP，调用方不需要知道后端。

理由（按份量排序）：

1. **该方案已在生产运行**——`beamai_extra` 的
   `beamai_tool_provider_mcp.erl:191` 把 MCP 工具建成普通 tool_spec，handler 是
   闭包 `fun(Args,_Ctx) -> beamai_mcp_client:call_tool(ClientPid, ToolName, Args)`，
   来源记在 `metadata => #{source => mcp, client_pid => ...}`。无 backend 字段、
   无分派，今天就能用。
2. **§5 的「严格模式」会打死它**——该模式规定非 local backend **不允许写
   handler**，而现有 MCP provider 正是靠 handler 工作的。
3. **§5 让 `beamai_core` 感知 transport**——`resolve_backend/2` 要从 context 掏
   kernel、查 settings 里的 `tool_backends` 表。HTTP / MCP 按三 app 分层本就该
   待在 `beamai_extra`。
4. **闭包方案下 retry / timeout / serial / filters 天然正交**——只有一条执行路径。

代价：tool_spec 含 fun，不是纯数据，无法序列化或从配置文件重建。缓解：
`metadata.source` 已承载来源信息（MCP provider 即如此）。

**args 键类型**：维持 **binary key**。全项目零个 atom-key handler，所有真实工具
（shell / human / file / todo）都是 `maps:get(<<"command">>, Args)`。
`beamai_tool:parse_args/1` 走 `jsx:decode(return_maps)`，不 atom 化。
注意 `parameters` 用 **atom key 声明**、handler 用 **binary key 读**——这个不对称
已经坑过一次（见「二」）。

## 二、live 测试证伪：旧测试常绿，而工具从未成功执行过一次

旧 `beamai_tcm_live_test` 让模型算 `17+25` 再 `*3`，只断言回答含 `126`。但
handler 写的是 `fun(#{a := A, b := B})`（atom key），实际收到 binary key →
**每次调用都 function_clause** → 模型自己心算出 126 → 测试照样通过。

重写要点：

- **模型不可能猜到的数据**——内部目录价格（widget=4173、gadget=2891，答案 7064）。
  答对 ⇔ 两次查价真的执行了、且结果真的回灌了模型。算术题做不到这点。
- **spy manager**（`beamai_spy_tcm_impl`）包住真 manager，记录每批分派的工具名，
  证明 tool_calls 确实经 manager seam，而非绕过去。
- **ETS 副作用**记录每次 handler 调用及参数。
- 不断言并发/串行的重叠——模型分几批发 tool_calls 是模型自由，依赖它必然
  flaky。重叠语义由 `beamai_tool_calling_manager_tests` 确定性覆盖。

## 三、隔离：从「批次大小的意外副产品」变成 manager 级不变量

**改前实测**（探针）：

```
[probe1] 声明 timeout=100，实际耗时 2012 ms      ← timeout 字段完全没生效
[probe N=1] 调用者进程被打死：boom               ← 单工具：用户进程被工具带崩
[probe N=2] 调用者进程存活                       ← 多工具并发：隔离成立
```

根因：`beamai_agent_utils.erl:104` 的
`Concurrent = Parallel andalso length(ToolCalls) > 1 andalso not batch_has_serial(...)`
只有并发路径 spawn_monitor；串行路径 `[run_one_tool(...) || TC <- ToolCalls]`
**在调用者进程里 inline 跑**。`try/catch` 挡不住 link 传播的退出信号，也挡不住
卡死。于是没有隔离的恰恰是最常见的情况：单个 tool_call、`parallel_tools=false`、
批内有 serial 工具、**sequential manager**（卖点是「严格副作用场景」，却跑在最没
保护的路径上）。

**新语义**：新模块 `beamai_tool_batch_worker`——manager 把**整批**交给一个动态
`spawn_monitor` 的 worker，串行/并发退化为 worker **内部**的调度自由。

- **进程边界** = manager 级，恒定一层，与批次大小 / 策略无关
- **串行 / 并发** = 边界内部的调度，不影响隔离
- **spawn_monitor 而非 link**：monitor 单向观测，父进程不被 worker 退出信号波及；
  DOWN 携 Reason 可归因，是跟踪 / 重启 / 重试的结构基础。link 双向，正是它把
  工具崩溃传导给用户进程

**保住的语义**：`on_result` 仍在**调用者进程**触发（worker 内用 proxy 转发回父
进程）——整个 execute_tools 搬进 worker 会让回调跟着跑进 worker，那是语义改变。

## 四、per-tool timeout：从空转到真正执行

`beamai_tool:call_handler/4` 四个子句全部忽略 `_Timeout`，而 `invoke/3` 文档写着
「默认超时 30 秒」。改为 handler 跑在受监控子进程中，到点 kill。

- **必须起进程**：BEAM 无法中断同进程的内联调用；try/catch 与时间无关。
- **不与「manager 级隔离」冲突**：这个子进程是 timeout 的**执行机制**（故障单元 =
  单个工具），batch worker 是**隔离机制**（故障单元 = 批）。串行语义不变——
  依然是起一个、等一个、再起下一个。
- **顺带收窄故障单元**：工具内 spawn_link 崩溃现在只打死该工具的子进程，同批
  健康工具照常返回真结果。
- **handler 不得依赖调用者进程身份**（`self()` / 进程字典 / 发往调用者的消息）。
  并发路径下本就如此，这里只是让所有路径一致。

## 五、执行策略可在构造 manager 时给定

`{Mod, Ref}` 的 `Ref` 本就是留给实现携带配置的，两个内置实现此前只塞了占位原子。
现在 Ref 装 `manager_opts()`：

```erlang
beamai_tool_calling_manager:concurrent(#{tool_timeout => 300000}),
beamai_tool_calling_manager:sequential(#{tool_timeout => infinity,
                                         batch_timeout => 600000}).
```

- **优先级**：工具声明 `tool_spec.timeout` > manager 的 `tool_timeout` > infinity。
  工具最了解自己要跑多久；manager 缺省让部署方一处收紧而不必逐个改工具。
- **下发通道**：manager 与 `beamai_tool:invoke/3` 之间隔着 5 层调用，借 context
  搭车。用 **env 专用槽** `default_tool_timeout` + `with_default_tool_timeout/2` /
  `default_tool_timeout/1`（对齐 `with_kernel` / `get_kernel`），**不用 vars**——
  `beamai_prompt.erl:57` 把 `variables/1` 直接喂给提示词模板渲染，执行策略塞进去
  会污染用户的模板变量表。
- **返回前剥掉**：策略是本批的执行细节，不该随 context 跨轮飘。属**卫生**而非修
  bug——暂停快照只存 `state` 分区（`beamai_agent_interrupt.erl:84`
  `saved_state => maps:get(state, Context, #{})`），env 本就不会被持久化。
- 旧的 `{Mod, default}` 构造形态仍可用（`opts_from_ref/1` 兼容）。

## 六、缺省策略：三层全部**无限等待**

框架不替调用者判断「多久算太久」——**只有用户声明了超时元信息才真的限时**，
缺省一直等，完成情况经 `on_result` 实时汇报。

| 层 | 机制 | 缺省 |
|---|---|---|
| 工具级 | `beamai_tool` 的 `?DEFAULT_TOOL_TIMEOUT` | **infinity**——tool_spec 的 `timeout` 或 manager 的 `tool_timeout` 声明了才限时 |
| 并发收集 | `beamai_agent_utils:gather_timeout/0`（app env `tool_gather_timeout`） | **infinity**——一直等到每个 worker 交付；显式配才到点 kill 未交付者（已完成结果保留） |
| 批级 | `beamai_tool_batch_worker:batch_timeout/1`（manager 的 `batch_timeout`） | **infinity**——显式给才限时 |

- **批级隔离本身不受影响**：worker 崩溃仍被 monitor 接住、整批合成 error、
  调用者恒存活。改的只是「因为等太久而主动杀」——那才是替用户做决策的部分。
- **删除 `tool_batch_grace`**（PR2 中途引入的 5 秒宽限 app env）。它唯一的作用是
  保证批级截止不抢在 gather 截止前面响（本层计时从 spawn **前**开始、内层从
  spawn **后**起算）。缺省 infinity 后该次序天然成立，宽限成为多余概念。
  显式给 `batch_timeout` 时仍须大于 `tool_gather_timeout`，已写入文档。
- **后果**：卡死的工具会让 agent 永久挂起，直到调用方自行处置。表现从「2 分钟后
  自动降级成 error 继续跑」变成「停在那里不动」。这是刻意的。

## 七、错误分类补漏

`classify(tool_batch_crash)` 缺条目 → 掉进 `classify(_) -> semantic` 兜底，
基础设施故障被当成「模型的语义错误」路由。补为 `transient`，与同源的
`tool_worker_crash` 一致。测试断言批崩溃记录的 `class =:= transient`。

## 批次

- [x] P1 `beamai_tool_batch_worker`（新模块）：`execute_isolated/5,6`、
      proxy 转发 on_result、DOWN / 超时兜底、`batch_timeout/1`、infinity 支持
- [x] P2 两个 manager 经 worker 执行；`new/1` + Ref 携 `manager_opts()`；
      `concurrent/1`、`sequential/1`、`opts_from_ref/1`
- [x] P3 `beamai_tool`：`call_handler/4` 起受监控子进程强制 timeout；
      `apply_handler/3`（四种 handler 形态，try/catch 留子进程内）；
      `resolve_timeout/2` 优先级；`?DEFAULT_TOOL_TIMEOUT = infinity`
- [x] P4 `beamai_context`：env 增可选槽 `default_tool_timeout` +
      `with_default_tool_timeout/2` / `default_tool_timeout/1`
- [x] P5 `beamai_agent_utils`：`gather_timeout/0` 缺省 infinity；
      `deadline/1` + `remaining/1`；导出 `synth_result/3` 供批级合成复用
- [x] P6 `beamai_tool_error`：`classify(tool_batch_crash) -> transient`
- [x] P7 测试：`beamai_tcm_isolation_tests`（16）、`beamai_tool_timeout_tests`（11）、
      `beamai_spy_tcm_impl`（spy helper）、`beamai_tcm_live_test` 重写
- [x] P8 验证（见下）
- [x] P9 不 commit：待用户 review

## 验证

- `rebar3 eunit`（beamai 全量）——**537 tests, 0 failures**
- `rebar3 dialyzer`（beamai）——**零警告，EXIT=0**
- `rebar3 eunit`（beamai_extra 全量）——**425 tests, 0 failures**
- `rebar3 dialyzer`（beamai_extra）——**零警告**
- live MiniMax 端到端——2 tests, 0 failures，两个 manager 均通过：
  ```
  [concurrent] dispatches=[[lookup_price, lookup_price]] looked_up=[widget, gadget] iterations=3
    response: The total price is 7,064 cents (4,173 + 2,891).
  ```

> **beamai_extra 尤其关键**：它经 `_checkouts` 符号链接吃本地改过的
> `beamai_core`，故 shell / human / MCP / file 这些**真实工具**是在「handler 跑进
> 子进程 + timeout 语义改变 + context 多一个 env 槽」下跑的，425 个全绿。
>
> 本轮起点基线为 509 tests / dialyzer 0 warning，与 PR1 记录的
> 597 tests / 34 pre-existing warnings **不可比**——期间树已变动，原因未考证。

## 测试腐化：两次自我证伪的记录

值得单独记一笔，因为两次都是「测试仍是绿的，但测的东西已经变了」：

1. **工具层子进程一加**（四），批级隔离测试立刻变成空跑——spawn_link 崩溃在工具层
   就被接住，再也走不到批级边界。重写为：工具层用工具崩溃测（并新增「崩溃工具
   不牵连健康工具」），批层改用**在 filter 里崩溃**测（filter 跑在批 worker 中、
   在 handler 子进程之外，只有批级边界能接），并断言错误内容含 `tool_batch_crash`。
2. **缺省翻转成 infinity**（六），`hanging_tool_times_out` 立刻挂住被 eunit 取消——
   它编码的正是旧策略。这是好事：它证明默认值真的变了。改为正面表述新契约的两个
   测试（串行 + 并发）：起一个跑卡死工具的批次，等 1 秒，断言它**仍在等**、既没
   返回合成的 timeout 也没死，然后由测试自己（即调用方）`exit(Pid, kill)`——
   处置权在调用方手里。

## PR2 明确出界

- `beamai_kernel` 零改动（仍成立）
- `execute_tools/5` 逻辑仍未搬家——batch worker 委托它
- **「报告但不杀」的中间形态**（如卡了 N 秒发 warning、继续等）——现无此机制
- 编排层的 backend 分派——**已否决**，不再是待办（见「一」）
