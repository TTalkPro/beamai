# Agent Simple State 与消息管理设计文档

## State 结构

`#state{}` 记录定义在 `include/beamai_agent.hrl` 中，包含以下字段：

| 字段 | 类型 | 说明 |
|------|------|------|
| `id` | `binary()` | Agent ID |
| `name` | `binary()` | Agent 名称 |
| `system_prompt` | `binary()` | 系统提示词 |
| `tools` | `[map()]` | 工具定义列表 |
| `tool_handlers` | `#{binary() => function()}` | 工具处理器映射 |
| `llm_config` | `map()` | LLM 配置 |
| `graph` | `map() \| undefined` | 编译后的执行图 |
| `max_iterations` | `pos_integer()` | 最大迭代次数 |
| `messages` | `[map()]` | 完整对话消息列表 |
| `buffer_config` | `map() \| undefined` | 对话缓冲器配置（向后兼容） |
| `scratchpad` | `[map()]` | 中间步骤记录 |
| `pending_action` | `map() \| undefined` | 等待确认的动作 |
| `response_format` | `map() \| undefined` | 输出格式约束 |
| `callbacks` | `#callbacks{}` | 回调处理器集合 |
| `middlewares` | `[term()]` | Middleware 配置列表 |
| `middleware_chain` | `list() \| undefined` | 已初始化的 Middleware 链 |
| `storage` | `beamai_memory:memory()` | 存储后端 |
| `auto_checkpoint` | `boolean()` | 是否自动保存检查点 |
| `run_id` | `binary() \| undefined` | 当前运行 ID |

## 图执行状态（Graph State）

在图执行期间，使用独立的图状态（`graph:state/1` 创建），包含：

| 键 | 说明 |
|----|------|
| `messages` | LLM 上下文消息（可能被压缩） |
| `full_messages` | 完整对话历史（用于持久化） |
| `system_prompt` | 系统提示词 |
| `tools` | 工具规格列表 |
| `max_iterations` | 最大迭代次数 |
| `iteration` | 当前迭代计数 |
| `scratchpad` | 中间步骤记录 |
| `callbacks` | 回调处理器映射 |
| `callback_meta` | 回调元数据 |
| `last_response` | 最近的 LLM 响应 |
| `last_content` | 最近的响应内容 |
| `tool_calls` | 待执行的工具调用 |
| `tool_results` | 工具执行结果 |
| `finish_reason` | LLM 结束原因 |
| `context` | 工具执行上下文 |
| `compression_applied` | 是否已应用压缩 |
| `conversation_summary` | 对话摘要 |

## 消息管理：双轨机制

### 设计目标

1. **LLM 上下文优化**：通过压缩/摘要减少 Token 使用
2. **完整历史保留**：确保持久化时不丢失任何消息

### 两个消息字段

- **`messages`**：发送给 LLM 的上下文消息
  - 可能被 SummarizationMiddleware 压缩
  - 包含滑动窗口内的消息 + 可选摘要

- **`full_messages`**：完整的对话历史
  - 永远不被压缩
  - 用于持久化和检查点恢复
  - 包含所有原始消息

### 消息流转

```
┌─────────────────────────────────────────────────────────────────┐
│                        执行开始                                  │
├─────────────────────────────────────────────────────────────────┤
│ State.messages = [历史消息]                                      │
│                                                                  │
│ ┌──────────────────────────────────────────────────────────┐    │
│ │ 图执行初始化 (beamai_agent_runner:execute)               │    │
│ │                                                          │    │
│ │ AllMessages = State.messages ++ [新用户消息]              │    │
│ │ ContextMessages = build_llm_context(AllMessages, Buffer) │    │
│ │                                                          │    │
│ │ GraphState = #{                                          │    │
│ │   messages => ContextMessages,  %% 可能已压缩            │    │
│ │   full_messages => AllMessages  %% 完整历史              │    │
│ │ }                                                        │    │
│ └──────────────────────────────────────────────────────────┘    │
│                           ↓                                      │
│ ┌──────────────────────────────────────────────────────────┐    │
│ │ SummarizationMiddleware.before_model                     │    │
│ │                                                          │    │
│ │ if should_compress(messages):                            │    │
│ │   full_messages = full_messages  %% 保持不变             │    │
│ │   messages = compress(messages)  %% 压缩                 │    │
│ └──────────────────────────────────────────────────────────┘    │
│                           ↓                                      │
│ ┌──────────────────────────────────────────────────────────┐    │
│ │ LLM 调用 (beamai_llm_node)                                │    │
│ │                                                          │    │
│ │ messages = messages ++ [assistant_msg]                   │    │
│ │ full_messages = full_messages ++ [assistant_msg]  ←NEW   │    │
│ └──────────────────────────────────────────────────────────┘    │
│                           ↓                                      │
│ ┌──────────────────────────────────────────────────────────┐    │
│ │ 工具执行 (agent_tool_node)                               │    │
│ │                                                          │    │
│ │ messages = messages ++ [tool_results]                    │    │
│ │ full_messages = full_messages ++ [tool_results]   ←NEW   │    │
│ └──────────────────────────────────────────────────────────┘    │
│                           ↓                                      │
│ ┌──────────────────────────────────────────────────────────┐    │
│ │ 执行结束 (handle_graph_result)                           │    │
│ │                                                          │    │
│ │ FinalMessages = get_full_messages(GraphState)            │    │
│ │              = full_messages  %% 优先使用完整历史        │    │
│ │                                                          │    │
│ │ State.messages = FinalMessages  %% 持久化完整历史        │    │
│ └──────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────┘
```

### 关键原则

1. **压缩只影响 LLM 上下文**：`messages` 可被压缩，但仅用于 LLM 调用
2. **完整历史始终保留**：`full_messages` 永不压缩，包含所有消息
3. **新消息双向追加**：LLM 响应和工具结果同时追加到两个字段
4. **持久化用完整历史**：保存到 `State.messages` 的是 `full_messages`

## SummarizationMiddleware 工作原理

### 触发条件

- 消息数量超过 `compress_threshold`（默认 30）
- 或估算 Token 数超过 `token_threshold`（默认 3000）

### 压缩策略

1. **滑动窗口**：保留最近 N 条消息（`window_size`，默认 20）
2. **Token 限制**：确保不超过 `max_tokens`（默认 4000）
3. **摘要生成**：可选，对旧消息生成摘要

### 配置示例

```erlang
{middleware_summarization, #{
    window_size => 20,        %% 滑动窗口大小
    max_tokens => 4000,       %% 最大 Token 数
    summarize => true,        %% 启用摘要
    compress_threshold => 30, %% 消息数量阈值
    token_threshold => 3000,  %% Token 数量阈值
    preserve_system => true   %% 保留系统消息
}}
```

## 注意事项

1. **buffer_config vs middleware**：
   - `buffer_config` 是旧版机制，在 `execute` 入口处理
   - `middleware_summarization` 是新版机制，在 `before_model` 钩子处理
   - 建议使用 middleware 方式

2. **消息同步**：
   - 任何追加消息的操作必须同时更新 `messages` 和 `full_messages`
   - 否则会导致持久化时丢失消息

3. **检查点恢复**：
   - 恢复时使用 `State.messages`（完整历史）
   - 重新执行时会再次应用压缩
