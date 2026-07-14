# TASK: Filter 一次性构建（删 order / add_filter / with_memory / filters:0，注册顺序即层序）

> 来源：与 clj-agent `advisor.clj` 对齐（扁平 vector，注册顺序即执行顺序，
> `build-chain` 无排序）。clj-agent 的演进路径是「先有 :order、后主动删除」
> （`design/onion-filter.md` 里有 order，落地 `advisor.clj` 时删掉），本任务把
> beamai 收敛到同一终局。
> 依据（已核实）：全项目仅 memory(-1000)/system_prompt(-500) 两个非零 order；
> agent 主路径（memory→provider 重构后）从不调 add_filter；无生产模块实现
> filters/0。顺序 A→B→C→D，每批全量 eunit + dialyzer。不 commit：待用户 review。

## 语义决定（已确认）

1. **注册顺序即层序**：filters 列表靠前 = 外层（前置先执行、后置后执行）。
   删 order 字段与运行时排序。
2. **一次性构建**：filter 全量在 `beamai_kernel:new(Settings, Filters)` 给出；
   删 `add_filter`（kernel + beamai 门面）。
3. **with_memory 删除**：memory filter 由用户显式放入 Filters 列表**首位**
   （最外层：先展开历史）。`memory_filter/1` 保留，带 order 的 /2 删除。
4. **tool-module `filters/0` 特性删除**：plugin 只提供工具；filter 一律走
   顶层 Filters 列表（`maybe_add_filters` 删除）。
5. **system_prompt 注入改为最内层**（行为变化，文档写明）：invoke 时
   `Filters ++ [system_prompt_filter]`，在所有用户 filter 之后、LLM 之前
   前置系统消息。后果：用户 chat filter 看到的 messages 不含系统提示；
   memory 在列表首位时永远不会把系统提示存进历史（原语义保持）。

## 批次 A：核心（beamai_filter / beamai_filter_chain）

- [x] A1 `beamai_filter`：filter() 类型删 `order` 字段；`new/2(Name, Hooks)`、
      `new/3(Name, Hooks, Init)`（原 new/3 第三参 Order 改为 Init，new/4 删除）；
      删 `sort/1`；moduledoc 改注册顺序即层序
- [x] A2 `beamai_filter_chain:run/4`：去掉 `beamai_filter:sort` 调用，按列表
      顺序 compose；moduledoc 同步
- [x] A3 测试：`beamai_filter_tests` 删 order/sort 用例，补注册顺序=层序用例

## 批次 B：kernel 构建面（beamai_kernel / beamai.erl / memory_filter）

- [x] B1 `beamai_kernel:new/2(Settings, Filters)`；new/0,1 保持（Filters=[]）；
      删 `add_filter/2`、`with_memory/2`、`maybe_add_filters`（add_tool_module
      只装工具）；filters 相关注释改注册顺序语义
- [x] B2 `system_prompt_filter`：`beamai_filter:new/2`（无 order）；注释改
      「invoke 时追加为最内层」并写明行为变化
- [x] B3 `beamai_memory_filter`：删 `?DEFAULT_ORDER` 与 `memory_filter/2`，
      文档写明「放 Filters 列表首位（最外层）」
- [x] B4 `beamai.erl` 门面：`kernel/2(Settings, Filters)`；删 `add_filter/2,3`、
      `with_memory/2` 导出与实现
- [x] B5 测试：kernel_tests / memory_filter_tests / filters_tests 改一次性构建

## 批次 C：agent 侧收尾

- [x] C1 `beamai_agent_state:add_plugins` 注释更新（plugin 不再带 filter）；
      `beamai_agent.erl` 头注释同步
- [x] C2 agent 测试：turn_filter_tests / agent_example_tests 等 add_filter
      调用点改为一次性构建
- [x] C3 全量 eunit + dialyzer

## 批次 D：文档

- [x] D1 `docs/FILTER.md` / `FILTER_EN.md`：删 order 章节与 filters/0 章节，
      改注册顺序即层序 + 一次性构建 + system_prompt 最内层语义
- [x] D2 `README.md`（及 README_EN.md 如涉及）add_filter/with_memory 示例改 kernel/2
- [x] D3 `docs/API_REFERENCE.md`（及 _EN）同步 API 变更

## 明确出界

- agent config 增加 filters 键（用户要 filter 走预构建 kernel，不动）
- 流式 per-token filter（另议，见前次讨论）

## 验证

- 每批：`rebar3 eunit`（全量，基线 363）+ `rebar3 dialyzer`（EXIT=0）
- 不 commit：待用户 review 后由其决定

## 完成记录（2026-07-14 全部实施）

- **全绿**：`rebar3 eunit` 362 tests / 0 failures（基线 363：删 2 个 order/sort
  用例、新增 1 个注册序用例，净 -1）；`rebar3 dialyzer` EXIT=0；examples 编译通过。
- **API 变更（无兼容别名）**：
  - `beamai_filter`：删 `order` 字段与 `sort/1`；`new/3` 第三参 Order→**Init**，删 new/4
  - `beamai_kernel`：新增 `new/2(Settings, Filters)`；删 `add_filter/2`、
    `with_memory/2`、`maybe_add_filters`（tool module 的 filters/0 特性整体删除）
  - `beamai`：新增 `kernel/2`、`filter/2,3`（纯构造器）；删 `add_filter/2,3`、`with_memory/2`
  - `beamai_memory_filter`：删 `?DEFAULT_ORDER` 与 `memory_filter/2`（只留 /1，放列表首位）
- **行为变化**：system_prompts 注入层从 order -500（用户 filter 外层）改为 invoke 时
  追加**最内层**——用户 chat filter 看到的 messages 不含系统提示；memory filter
  在首位时系统提示仍然永不入历史（原语义保持）。
- **文档**：FILTER/FILTER_EN（顺带修正 2→3 hook，补 around_turn/turn 链表格）、
  MEMORY/MEMORY_EN、README/README_EN、core README/README_EN、agent README
  （删"路 B filters/0"）、examples/example_filter.erl 全部改为一次性构建写法。
- **测试改法**：add_filter 调用点统一改 `beamai_kernel:new(#{}, Filters)` 先建后加
  tool/service；plugin filters/0 回归测试反转为"导出 filters/0 也被忽略"。
