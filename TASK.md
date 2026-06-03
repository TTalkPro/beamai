# TASK: Kernel 消息存储下沉到 Memory Filter

> 设计文档：`design/kernel_memory_filter_redesign.md`
> 目标：kernel/context 不再记录 messages；每次 invoke 只传单条最新 delta；会话存储交给 memory filter。

## 执行步骤

### 步骤 1：behaviour `beamai_chat_memory` ✅
- [x] 新建 `apps/beamai_core/src/behaviours/beamai_chat_memory.erl`
- [x] 定义回调 `mem_get/2`、`mem_add/3`、`mem_clear/2`
- [x] 调度 API：对句柄 `{Module, Ref}` 解包转发
- [x] 导出 `handle/0` 类型

### 步骤 2：默认实现 `beamai_chat_memory_ets` ✅
- [x] 新建 `apps/beamai_core/src/kernel/beamai_chat_memory_ets.erl`（gen_server）
- [x] ETS 表 `ConvId => [message()]`，追加语义，`mem_get` 正序返回
- [x] `start_link/1,2` + `handle/1` 返回句柄 `{beamai_chat_memory_ets, Name}`
- [x] 实现 behaviour 三个回调 + `stop/1`

### 步骤 3：memory filter `beamai_memory_filter` ✅
- [x] 新建 `apps/beamai_core/src/kernel/beamai_memory_filter.erl`
- [x] `memory_filters/1` 返回 `[PreChat, PostChat]`
- [x] pre_chat (-1000)：读 conv_id；存 delta + 用全量历史替换 messages；无 conv_id 透传
- [x] post_chat (+1000)：存 assistant 回复（`response_to_message/1` 转换）
- [x] 系统提示注入：放到 kernel `system_prompt_filters/1`（pre_chat -500），不存储

### 步骤 4：改造 `beamai_context` ✅
- [x] 移除 `messages`、`history` 字段
- [x] 移除 `get_messages/1`、`set_messages/2`、`append_message/2`、`get_history/1`、`add_history/2`
- [x] 新增 `with_conversation_id/2`、`conversation_id/1`（保留 key `<<"__conversation_id__">>`）
- [x] 更新模块文档注释

### 步骤 5：改造 `beamai_kernel` ✅
- [x] kernel state 加 `memory => handle() | undefined`，`new/1` 初始化
- [x] 新增 `with_memory/2`：挂载 memory filters（facade 同步暴露 `beamai:with_memory/2`）
- [x] 重写 `invoke/3`：解析/生成 conv_id；删拼接累积；delta/full 双模式；ephemeral 清理
- [x] system_prompts：改为挂临时 pre_chat(-500) filter，不再在 loop 里 `SysPrompts ++ Msgs`
- [x] 删除 `track_message/2`、`track_new_messages/2`
- [x] `tool_calling_loop` 改为 Mode 驱动（delta=只传工具结果 / full=本地累积全量）

### 步骤 6：窗口包装 `beamai_chat_memory_window` ✅
- [x] 新建 `apps/beamai_core/src/kernel/beamai_chat_memory_window.erl`
- [x] 包装 inner 句柄，`mem_get` 时套条数窗口
- [x] 保护：丢弃落单 head `tool` 消息
- ⚠️ 偏差：**未复用 `beamai_conversation_buffer`**（它在 beamai_cognition，依赖 beamai_core，
  反向依赖会成环）。core 内自实现条数窗口；Token 裁剪/摘要后端留给 beamai_cognition 之后提供。

### 步骤 7：注册 + 编译 ✅
- [x] `beamai_core.app.src` 注册 4 个新模块
- [x] `rebar3 compile` 通过

### 步骤 8：测试 ✅
- [x] 改写 `beamai_chat_completion_tests.erl`：messages/history 用例 → conversation_id 用例
- [x] 新增 `beamai_memory_filter_tests.erl`（10 用例）：ETS store、窗口裁剪/孤立 tool、
      response_to_message、pre/post_chat filter、无 conv_id 透传、跨 invoke 累积(meck)、ephemeral 清理
- [x] `rebar3 eunit` 全绿（190 tests, 0 failures）

## 完成情况

全部 8 步完成。`rebar3 compile` 4 app 通过，`rebar3 eunit` 190 tests / 0 failures。

### 一处设计偏差
窗口包装未复用 `beamai_conversation_buffer`（依赖方向会成环）。已在 core 内自实现条数窗口，
基于 Token 的裁剪/摘要可由 beamai_cognition 提供实现同 behaviour 的 store。

### 待办（可选，未做）
- `beamai_llm_integration_SUITE` 增加「memory 多轮对话」真实 LLM 用例（无环境变量则 skip）。
- 同步更新 `apps/beamai_core/README*.md`（Context/Kernel 章节关于 messages/history 的描述）。
