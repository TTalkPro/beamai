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

- backend 分派（local/http/mcp）——PR2
- `beamai_tool:invoke/3` 改动——PR2
- `beamai_kernel` / `beamai_tool` 任何改动——PR2
- execute_tools 逻辑搬家（从 utils 搬进 default manager）——PR1 不搬，只委托

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
