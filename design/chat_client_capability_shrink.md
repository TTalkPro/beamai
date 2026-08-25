# ChatClient 能力收缩：对齐 Spring AI 的工具体系分工

> **状态：✅ 已实施（2026-08-25）。** `rebar3 eunit` 716 tests / 0 failures、`rebar3 dialyzer`
> EXIT=0；beamai_extra 587 tests 全过。
> 依据：本地 spring-ai 的 `ChatClient.java` 与官方
> [Tool Calling](https://docs.spring.io/spring-ai/reference/api/tools.html)。
> 承接 `design/chat_model_layering.md`（Stage 1/2：ChatRequest/ChatResponse + provider 声明式回调）。

---

## 0. 缺口：一个模块干了 Spring 四个组件的活

Spring AI 的分工（tools.html §"Who executes tool calls"）：

| 组件 | 职责 |
|---|---|
| `ChatClient` | 提示词组装、默认 advisors、**声明**默认 tools、`call()` / `stream()` |
| `ToolCallback` | 工具定义 + 执行逻辑（＝我们的 `beamai_tool`） |
| `ToolDefinition` | name / description / inputSchema——**给模型看的** |
| `ToolMetadata` | returnDirect 等框架行为——**不给模型看** |
| `ToolCallbackResolver` | 按名**解析**工具（只找，不执行） |
| `ToolCallingManager` | **执行**工具、限额、错误合成、把结果拼回历史 |
| `ToolCallingAdvisor` | **循环** |

`beamai_chat_client` 导出 18 个函数，同时扮演了 ChatClient + ToolCallbackResolver +
ToolDefinition 生成 + ToolMetadata 查询 + ToolCallingManager（单次执行）。
「持有工具表」滑成了「代答一切工具问题、顺便把工具跑了」。

## 1. 收缩后的边界

```
beamai_chat_client     声明与调用：LLM 配置 / 默认工具 / 默认 filter / invoke_chat(_stream)
  ├─ beamai_tool_registry   声明侧：注册、resolve、specs/schemas、serial/return_direct
  └─ beamai_tool_executor   运行侧：单次执行（按名解析 → around_tool 洋葱 → 归一返回）
       └─ beamai_tool_calling_manager（agent 层）批量/并发/串行/限额
            └─ 循环 filter（agent 层）＝ ToolCallingAdvisor
```

导出 **18 → 11**：

| 保留 | 对应 Spring |
|---|---|
| `new/0,1,2`（Settings + Filters） | Builder 的 defaultOptions / defaultAdvisors |
| `add_tool/2`、`add_tools/2`、`add_tool_module/2` | Builder 的 defaultTools |
| `add_chat_model/2`、`chat_model/1` | ChatModel |
| `invoke_chat/3`、`invoke_chat_stream/4` | `call()` / `stream()` |
| `tools/1`、`filters/1`、`state_slots/1` | 交出自己持有的东西（≈ 把 toolCallbacks 放进 ChatOptions） |

| 搬走 | 去处 |
|---|---|
| `invoke_tool/4` | `beamai_tool_executor:invoke/4` |
| `get_tool/2` | `beamai_tool_registry:resolve/2` |
| `list_tools/1`、`get_tools_by_tag/2` | `registry:list/1`、`by_tag/2` |
| `get_tool_specs/1`、`get_tool_schemas/1,2` | `registry:specs/1`、`schemas/1,2` |
| `serial_tool/2`、`return_direct_tool/2` | `registry:serial/2`、`return_direct/2` |

注册表仍**挂在 ChatClient 上**（Spring 的 ChatClient 也持有 defaultTools），改的是
「谁回答关于它的问题、谁执行它」。取表：`beamai_chat_client:tools/1`。

### 为什么 executor 的入参是 ChatClient 而不是裸注册表

执行要同时用到**工具表**与 **around_tool filter 链**，两者都挂在 ChatClient 上——正如
Spring 的 manager 从 Prompt 携带的 ChatOptions 里取 toolCallbacks。传裸表就得再传一份
filter 列表，反而把调用点拆散。

### 没有顺手加的能力

Spring 的 ChatClient 会把 defaultTools 自动广播进 ChatOptions；我们**没有**跟进——
beamai 的「注册 ≠ 广播」是 tool search 的前提（见 `design/spring_advisor_alignment.md`），
广播仍由 agent 显式组装 `chat_opts.tools`。这次只收缩，不扩张。

## 2. 破坏性变更

`beamai_chat_client` 的 9 个函数被移除（无别名）。调用点改法：

```erlang
beamai_chat_client:invoke_tool(CC, Name, Args, Ctx)
  → beamai_tool_executor:invoke(CC, Name, Args, Ctx)

beamai_chat_client:get_tool_specs(CC)
  → beamai_tool_registry:specs(beamai_chat_client:tools(CC))
```

facade `beamai:invoke_tool/4`、`beamai:tools/1,2`、`beamai:tools_by_tag/2` **签名不变**
（内部改为委托新模块），走 facade 的代码零改动。beamai_extra 的调用点已同步。

## 3. 验证锚点

`apps/beamai_core/test/beamai_tool_registry_tests.erl`（10 例）：

- **边界守卫**：断言 `beamai_chat_client` 不再导出那 9 个函数、且保留的 11 个仍在——
  收缩后的导出面本身成为可执行的契约；
- registry 脱离 ChatClient 独立可用（new/add/add_many/from_module/resolve/list/by_tag/
  specs/schemas）；
- 元数据对未注册的名字取保守值 false（不因未知工具退化整批串行 / 误触发直返）；
- executor：正常执行、`{error, {tool_not_found, _}}`、around_tool 洋葱仍生效、writes 原样透出。

既有 706 例与 beamai_extra 的 587 例无一改动语义即通过（只改调用点）。
