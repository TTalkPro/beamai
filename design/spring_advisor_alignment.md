# 对齐 Spring AI 2.0 Advisor：beamai 落地设计

对标 spring-projects/spring-ai `v2.0.0`（2026-06-12 GA）的 Advisor 体系，逐项盘
beamai 的对应物、缺口与取舍。**结论先行：Spring 2.0 的头号变化 beamai 早已具备，
真正的缺口是四项能力，其中 tool search 价值最大。**

## 0. 先说不对齐的部分

「全面对齐」不等于照抄。以下 Spring 概念**有意不引入**：

| Spring | 不引入的理由 |
|---|---|
| `getOrder()` 数字排序 | beamai 已在「filter 一次性构建」里删掉 order，**注册顺序即层序**。数字 order 是 Spring 的 DI 装配后果（advisor 从容器里来，位置无从谈起，只好用数字排）；beamai 的 filter 在 `beamai_kernel:new/2` 一次性列出，列表位置就是层序，更直白也更难写错。重新引入是倒退。 |
| `ToolAdvisor` / `MemoryAdvisor` 标记接口 + 自动注册 | 解决的是「容器里飘着一堆 advisor，得认出谁是工具循环」——beamai 没有容器。 |
| `BaseAdvisor` 的 `before()`/`after()` 模板 | beamai 的 around 闭包（前置 → `Next` → 后置）本就一个机制覆盖两段，无需模板方法。Spring 那套模板还带个坑：流式 `after()` 按 finish-reason 逐块触发，`MessageChatMemoryAdvisor` 得覆写 `adviseStream` 绕开。 |
| `ChatClientRequest.context()` 可变 map | beamai 的 filter 私有状态由链投影/合并（`FCtx` 参数进、返回值出），不给 filter 递一个共享可变 map。Spring 那边文档称其不可变、实际是 `HashMap` 且 `ToolSearchToolCallingAdvisor` 直接原地 `put`——正是这种口径不一致要避免的。 |

## 1. `ToolCallingAdvisor`：Spring 补的课 beamai 早修完

Spring 2.0 的头号变化是把工具循环从 `ChatModel` 内部搬进 advisor 链
（`internalToolExecutionEnabled` 连同 `ToolExecutionEligibilityPredicate` 一并删除），
并为此新增 `AdvisorChain.copy(after)` 让 advisor 能递归重入。

beamai 从来没有这个问题：

| Spring 2.0 | beamai |
|---|---|
| 工具循环搬进 advisor 链 | 循环本就在 Agent 层（`beamai_agent_tool_loop`），kernel 只提供单次 chat / 单次 tool |
| `chain.copy(this).nextCall()` 递归 | `Next(Req)` 重入（`validation_turn_filter` 早就这么用） |
| advisor 落在循环内/外靠 order 与 `+300` 比大小 | **靠选哪个 hook**：`around_turn` 包整个循环、`around_chat` 循环内每次 LLM 调用、`around_tool` 每次工具执行 |

beamai 的三链比数字 order 更严：位置是**类型**表达的，不是算出来的，写错了是选错 hook
（一眼可见），不是 order 差了 100（要对着两个类的常量算）。

**真正的缺口只有 `return_direct`**（下述），外加 `ToolExecutionEligibilityChecker`
那种可插拔的「响应算不算工具调用」判定——后者不做：beamai 的
`beamai_llm_response:has_tool_calls/1` 是统一响应上的确定判断，Spring 需要可插拔是因
为各 provider 响应形态没收敛到一个类型，那是它的历史包袱。

### 1.1 `return_direct`（已实施）

工具结果**直接作为最终答案**，不回灌模型。`beamai_tool` 加 `return_direct => boolean()`
标注，`beamai_kernel:return_direct_tool/2` 按名查，循环在屏障后判定。

两处**有意分歧**：

- **整批 AND**：批内 tool_calls 全部标注才直返（Spring 亦是 AND）。混批直返会把未标注
  工具的结果静默丢掉。
- **失败不直返**：批内任一工具失败则退回正常回灌，让模型看到错误自行补救。Spring 不
  区分成败、一律直返，会把错误 JSON 当最终答案端给用户——与 errors-are-data（错误回
  模型、模型决定怎么办）相悖。

合成的 assistant 回合照常落库，历史仍以 assistant 收尾
（`assistant(tool_calls) → tool(result) → assistant(答案)`），下一轮续接不残缺。
响应带 `metadata.return_direct = true`，turn filter 可据此区分「模型说的」与「工具直返的」。

## 2. `StructuredOutputValidationAdvisor` → `schema_validation_turn_filter`（已实施）

原有 `validation_turn_filter/2` 收的是任意 `fun((map()) -> ok | {invalid, R})`，形已具备，
缺的是开箱即用的 JSON Schema 校验。新增：

- `beamai_json_schema` —— 零依赖的 DRAFT 2020-12 **实用子集**校验器。Erlang 生态无现成
  的 2020-12 校验器（jesse 只到 draft-06），且我们只需覆盖 LLM 结构化输出实际用得上的
  关键字。不实现 `$ref` / `$defs` / `patternProperties` / `if-then-else` / `format`。
  直接吃 `beamai_tool` 的参数 Schema 形态（atom 键亦可）。
- `beamai_filters:schema_validation_turn_filter/2,3` —— 「取文本 → 解 JSON → 过 Schema」
  组成 ValidateFun，复用既有重入机制。

对照 Spring：

| | Spring | beamai |
|---|---|---|
| 校验器 | networknt json-schema-validator | 自写子集（无依赖） |
| 错误反馈 | 追加到**原 user 消息**文本后重发，错误不跨重试累积 | 反馈作为新 user 消息**重入全新循环**（沿用 `validation_turn_filter` 语义） |
| 耗尽 | 原样返回最后一次不合格响应，不抛错 | 同 |
| 流式 | `Flux.error(UnsupportedOperationException)` | turn 链本就在流式之上，不涉及 |
| 层序 | order `MAX-2000` → 落在工具循环**内**，故需 `hasToolCalls()` 守卫 | `around_turn` → 天然在循环**外**，只看得到最终答案，不需要守卫 |
| ```json 围栏 | 不处理（靠 provider 原生结构化输出） | **缺省剥离**（`code_fence` 可关）。我们没有「必然启用原生结构化输出」这个前提，模型爱裹围栏 |

## 3. `ToolSearchToolCallingAdvisor` → `beamai_tool_search`（已实施）

**本次价值最大的一项。** Spring 实测 28 个工具时省 34~64% token。

核心是把「注册」与「广播」拆开：

- **注册**（kernel 的 tools）：决定**能不能执行**——全量注册。
- **广播**（chat opts 的 tools）：决定**模型看不看得见**——`around_chat` filter 每轮现算，
  首轮只给 `tool_search` 一个。

模型想干活就得先调 `tool_search` 描述需求，拿回若干工具名；下一轮 filter 从历史里认领
这些名字，把对应工具加进广播列表。

与 Spring 的结构差异：

- **不需要 fingerprint 重建索引**。Spring 按请求解析工具、故须用 SHA-256 指纹判断工具集
  是否变了。beamai 的工具集在建 kernel 前就已知，索引一次性建好闭包进组件即可。
  （`beamai_tool_index:fingerprint/1` 仍提供，留给未来动态工具集。）
- **未索引的工具原样透传**。filter 只裁剪自己索引过的那些；广播列表里其余的（典型如
  agent 运行时追加的中断工具）一概不碰——否则会被静默吃掉。这是 beamai 特有的：Spring
  的 advisor 直接重建整个 toolCallbacks 列表。
- **检索工具永远广播**，否则一轮不中就再没机会检索，彻底卡死。
- **不自动注入 system 提示**。Spring 往 system 消息追加 `systemMessageSuffix`；beamai 的
  系统提示在最内层注入，filter 再加一条 system 消息就成了双 system 消息，各 provider 对
  此处理不一（Anthropic 的 system 是独立字段）。改为把引导写进检索工具自身的
  `description`（这本就是驱动模型的主要手段），另导出
  `beamai_tool_search:default_system_suffix/0` 供需要加强时自行拼进 `system_prompt`。

索引后端做成 behaviour（`beamai_tool_index`），内置两个纯 Erlang 实现：

| 后端 | 对应 Spring | 适用 |
|---|---|---|
| `beamai_tool_index_keyword`（缺省） | `LuceneToolIndex` | BM25 打分，含 CJK 二元切分与标识符切词（`get_weather`/`getWeather` 均切 `[get, weather]`） |
| `beamai_tool_index_regex` | `RegexToolIndex` | 正则/子串，非法正则退化为字面量匹配（查询串来自模型，不可信） |

`VectorToolIndex` 对应的向量实现**不在本仓**：向量存储属 beamai_extra。behaviour 已留好
接口，extra 侧可直接接。

## 4. `MessageChatMemoryAdvisor`：已具备，且不需要它的查重

beamai 有两套记忆编排，各司其职：

- `beamai_memory_filter`（kernel 级，`around_chat`）—— 给直接用 kernel 的调用方。
- `beamai_memory_provider`（Agent 级）—— Agent 在 tool loop 里显式编排。

Spring 2.0 的记忆变化基本不适用：`PromptChatMemoryAdvisor` 删除（beamai 从无对应物）；
默认 order 从 `+1000` 改 `+200` 好让记忆落在工具循环外（beamai 靠 hook 类型表达，不涉及）；
`conversationId` 改为必填（beamai 无 `conversation_id` 时**原样透传退化为单次无状态调用**，
比抛异常更合用，不改）。

**`isMemoryAlreadyInPrompt` 不移植**：它防的是「历史被重复**前置**进提示词」，那是 Spring
prepend 语义的产物。beamai 的 memory filter 是 **replace** 语义（用 store 的完整历史整个
替换 messages），结构上不可能重复前置。

> 实施中曾按 Spring 的思路加过一个「delta 已在历史尾部则跳过」的重入保护，写测试时发现
> 它压根不成立：一轮跑完 store 是 `[delta, assistant]`，delta 已不在尾部，第二次重入照样
> 重复落库。真要修得干净得让「存 delta + 存回复」整体幂等，不是加个查重能了事的；而唯一
> 的触发路径（把重试类 filter 放在 memory **之外**）本就与「memory 放列表首位」的约定相悖。
> 故**回退**，只在模块文档里写清缘由。半吊子防护比没有更坏——它给人已经防住了的错觉。

## 5. `SafeGuardAdvisor` → `safeguard_filter`（已实施）

`around_chat`，命中敏感词即短路、不调 LLM，合成一个 `finish_reason=content_filtered`
的答复（不带 tool_calls，故工具循环见之即正常收尾）。

放 chat 链与 Spring 层序一致（其 order=0 落在 `ToolCallingAdvisor` 之内），效果是循环内每次
LLM 调用都过一遍：不止拦用户输入，工具结果回灌时带出的敏感内容同样拦得住。

**一处分歧**：Spring 用区分大小写的 `String.contains`（"bomb" 拦不住 "BOMB"），beamai
**缺省不区分大小写**，`case_sensitive => true` 可切回。

能力边界写进了文档：这是子串匹配，不是内容安全。变形、拼音、Unicode 同形字、跨消息拼接
一概拦不住。真要做内容安全请接审核模型。Spring 的 Javadoc 把它说成防有害**输出**，实际它
只看输入——这个口径不一致我们不继承。

## 6. `SimpleLoggerAdvisor` → `logging_filter` 扩至三链（已实施）

原 `logging_filter/0` 只有 `around_tool`。Spring 的 `SimpleLoggerAdvisor` 记的是 chat 请求/
响应。既然 beamai 一个 filter 可挂三个 hook，索性三个都带上：放哪条链就记哪条。

## 7. 明确出界

| Spring | 为何不做 |
|---|---|
| `QuestionAnswerAdvisor` | RAG，需向量存储 → 属 beamai_extra（`beamai_rag`） |
| `RetrievalAugmentationAdvisor` | 同上（模块化 RAG） |
| `VectorStoreChatMemoryAdvisor` | 同上 |
| `SemanticCacheAdvisor` | 需向量存储 → 同上 |
| `VectorToolIndex` | 同上；`beamai_tool_index` behaviour 已留好接口 |
| Advisor 可观测性（`AdvisorObservationContext` 等） | Spring 把 Micrometer observation 焊进链的每一跳。beamai 无对应的 telemetry 层，值得单开一轮（`telemetry` 库），不夹带在本次。 |
| `ReReadingAdvisor` (Re2) | **Spring 根本没发布这个类**——它只存在于 `spring-ai-openai` 的**测试**目录，是份照抄用的文档示例。 |

## 8. 清单

| 项 | 落点 |
|---|---|
| `return_direct` | `beamai_tool`（标注）、`beamai_kernel:return_direct_tool/2`、`beamai_agent_tool_loop`（AND + 失败退回 + 合成落库） |
| JSON Schema 子集校验器 | `beamai_json_schema`（新） |
| Schema 校验 turn filter | `beamai_filters:schema_validation_turn_filter/2,3` |
| 工具检索 | `beamai_tool_search`（新）、`beamai_tool_index` behaviour（新）+ `_keyword` / `_regex` 两后端（新） |
| 敏感词拦截 | `beamai_filters:safeguard_filter/1,2` |
| 三链日志 | `beamai_filters:logging_filter/0` |

## 参考

- [Spring AI 2.0.0 GA](https://spring.io/blog/2026/06/12/spring-ai-2-0-0-GA-available-now/)
- [Composable Tool Calling](https://spring.io/blog/2026/06/15/spring-ai-composable-tool-calling/)
- [Self-Correcting Structured Output](https://spring.io/blog/2026/06/23/spring-ai-self-correcting-structured-output/)
- [Dynamic Tool Search](https://docs.spring.io/spring-ai/reference/guides/dynamic-tool-search.html)
- [Advisors API](https://docs.spring.io/spring-ai/reference/api/advisors.html) /
  [Recursive Advisors](https://docs.spring.io/spring-ai/reference/api/advisors-recursive.html)
- 源码读于 tag [`v2.0.0`](https://github.com/spring-projects/spring-ai/tree/v2.0.0)
