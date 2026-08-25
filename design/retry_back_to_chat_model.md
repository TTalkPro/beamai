# 重试回到 chat_model，删掉 around_llm

> **状态：✅ 已实施（2026-08-25）。** `rebar3 eunit` 694 tests / 0 failures、`rebar3 dialyzer` EXIT=0。
> **本文推翻 `design/chat_llm_layer_split.md`** 的结论（那次把重试抽成 llm 链上的 filter）。

---

## 0. 为什么撤

`chat_llm_layer_split.md` 的分析是对的——重试**不该**和记忆/记账挤在 chat 链上，
重入会让它们跟着重跑。但结论走过了一步：把重试提到 `around_llm` 链上，而它本来
就在 `beamai_chat_model:chat/3` 内部，也就是**整个 filter 栈之下**——那个位置本来
就满足「重入碰不到任何 filter」。

拆完之后暴露出三个实际问题：

1. **反向依赖**。core 的 ChatClient 为了注入默认重试 filter，用 `code:ensure_loaded(beamai_llm_filters)`
   运行时探测 beamai_llm——而 app 依赖是 `beamai_llm → beamai_core`，这条是反的。
2. **三个 API 不一致**。chat 的重试被抽到链上，`beamai_embedding` / `beamai_rerank`
   仍在模块内 `beamai_llm_retry:run`。同一个 app 里两套做法。
3. **直连调用方丢重试**。`beamai_chat_model:chat/3` 变成单次请求后，不经 ChatClient 的
   调用方全部失去重试；`beamai_llm_helper` 当时补了，但 beamai_extra 的
   `beamai_rag.erl:456` 是个漏网的真实回归。

更关键的是**钩子的划分标准**：turn / step / chat / tool 之所以成立，是因为**进出频率**
不同。而重试一旦回到栈底，`around_llm` 与 `around_chat` 就变成——

- 进出次数永远 1:1（chat 链的 terminal 就是 llm 链链头，中间没有任何东西会循环）
- Request / Response 契约完全相同
- 唯一差别是层序，而层序在本系统里本来就由**列表位置**决定

用 hook 类型去编码「位置」是错的抽象，这层于是没有存在理由。

## 1. 改动

| | 前 | 后 |
|---|---|---|
| 重试 | `beamai_llm_filters:retry_filter/1`（around_llm，ChatClient 缺省注入） | 回到 `beamai_chat_model:chat/3` 内部 |
| 重试参数 | ChatClient settings 的 `llm_retry` + 单次 opts | **单次 Opts > provider Config > 框架默认**（`beamai_llm_retry:opts/2`） |
| `around_llm` 钩子 | 有 | **删除**（4 个 around 钩子：turn/step/chat/tool） |
| ChatClient | `llm_chain/3` + `default_llm_filters/1` + `chat_client_settings/1` + `llm_retry` 设置 | 全删，`run_chat/5` 的 terminal 直接是 provider 调用 |
| `beamai_llm_filters` 模块 | 存在（只为那条链） | 删除 |
| embedding / rerank | `beamai_llm_retry:opts(Opts)` | `opts(Config, Opts)`，与 chat 对齐 |
| `beamai_llm_helper` | 自己包一层 `beamai_llm_retry:run` | 去掉（否则与内建重试变成 3×3 双重重试） |

流式仍不重试，但理由变干净了：`chat/3` 重试、`stream_chat/4` 不重试，**天然分开**，
不再需要往 Req 里塞 `stream => true` 让 filter 去判定（该标记保留，改作「供 chat filter
判定本次是不是流式」）。

## 2. 什么时候该把 around_llm 加回来

当出现**框架内置的、会重入 `Next` 的 llm 层中间件**时——多 provider fallback 路由、
N-best 投票、按尝试计费。那时它才有真实居民，而不是一个默认空转的钩子。

## 3. 验证锚点

`apps/beamai_llm/test/beamai_chat_model_retry_tests.erl`（9 例，配合
`beamai_flaky_provider` 经 `{custom, Module}` 接入，不用 meck）：

- 瞬态错误重试到成功（3 次真实请求）、语义错误（400）不重试、重试耗尽返回最后一次错误；
- 三级取值：Config 默认生效、单次 Opts 覆盖 Config、`max_retries => 0` 关闭；
- `on_retry` 回调按序拿到 attempt=1,2——**filter 看不到尝试，这里是唯一观测入口**；
- 流式不重试（只发一次真实请求）；
- **层次断言**：底下重试 3 次真实请求，`around_chat` filter 仍只进出 1 次。

## 4. live 验证（MiniMax-M2）

`MINIMAX_API_KEY` 走 Anthropic 兼容端点 `https://api.minimax.chat/anthropic`：

- 既有 `beamai_tcm_live_test`（2 例）、`beamai_http_pool_live_test`（1 例）全过；
- 真实链路计数：`turn=1 / step=2 / chat=2 / tool=1`，迭代 2 轮，工具结果确实回灌；
- **重试的真机证据**：`timeout => 1`（ms）触发 `{request_failed, timeout}`（可重试），
  `max_retries => 2` 下 `on_retry` 回调触发 **2 次**，而 `around_chat` filter 只进出
  **1 次**——重试确实发生在整个 filter 栈之下；单次 opts `max_retries => 0` 时 on_retry 0 次。

### live 逮到的一个 bug（已修）

Anthropic 流的 `message_start` 事件形如 `#{<<"message">> => #{<<"content">> => []}}`，
与 Ollama 的 `message.content`（binary 文本）**撞形**，被
`beamai_chat_model:extract_token_from_event/1` 当成 token 投递——每次流式开头下游都会
收到一个空 token（`[]`，还不是 `<<>>`，所以 agent 的 `emit_tokens(<<>>, ...)` 空值守卫
也拦不住）。

修法两处：该子句加 `when is_binary(Content)` 守卫；`invoke_new_token_callback/3` 收紧为
**只投递非空 binary**。回归测试 `stream_skips_non_text_events_test` 用
`beamai_flaky_provider` 回放事件序列，不走网络。

### 顺带发现（未改）

连接失败被归为不可重试：`{request_failed, {connection_failed, _}}` 落在
`beamai_llm_error:classify/2` 的 catch-all（`retryable=false`），只有 `timeout` 与
`{closed,_}` 可重试。live 跑的时候真遇到过一次连接超时直接返回错误。是否把
connection_failed 也算瞬态，值得单独决定。
