# feedbacks —— 下游宿主报上来的账

一条一个文件，`<日期>-<slug>.md`。**只记「beamai 这一侧该改」的**：下游自己
绕过去了但绕法很蠢、或者根本绕不过去的那些。下游自己的 bug 不进这里。

每份的格式（症状先行，病因其次，建议最后）：

| 段 | 写什么 |
|---|---|
| **症状** | 下游看到的**那一行**（报错原文 / 或者「一个字都不报」） |
| **位置** | `文件:行`，beamai 这一侧的 |
| **怎么撞上的** | 最短复现路径 |
| **影响面** | 谁会撞、撞了会怎样、**报不报错** |
| **建议** | 可选，下游不替仓主拍板 |

## 现有

| # | 标题 | 报的人 | 严重度 | 回没回 |
|---|---|---|---|---|
| [2026-10-23](2026-10-23-assistant-text-dropped-with-tool-calls.md) | 同一回合「先说一句话、再调工具」的 assistant 文本**不进历史**：`beamai_message:from_response/1` 见到 tool_calls 就只留 `content => null`；Anthropic 适配器的 tool_calls 分支同样不发 text 块 ⇒ 刷新后那句话消失，下一轮回放给模型也缺这段 | beamai_muse | 🟡 不报错；历史与实时流不一致 | ☑ 已修复 |

> 背景：beamai_muse（OpenMuse 的 Erlang 重写）用 beamai_core / beamai_llm /
> beamai_agent 做聊天与后台任务，历史存在自己实现的 `beamai_chat_memory`
> 后端里，UI 按历史重绘线程。上面一条对照 `08aa844` 核实。
