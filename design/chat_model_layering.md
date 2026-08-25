# ChatModel 分层：ChatRequest/ChatResponse + provider 只声明底层信息

> **状态：✅ Stage 1、Stage 2 已实施（2026-08-25）。** `rebar3 eunit` 706 tests / 0 failures、
> `rebar3 dialyzer` EXIT=0；MiniMax-M2 live（含 `beamai_tcm_live_test` / `beamai_http_pool_live_test`）全过。
> 参照 `~/workspace/spring-ai`：`chat/prompt/Prompt.java`、`chat/model/{ChatModel,ChatResponse}.java`
> 与各 `XxxChatModel` / `XxxApi` 的分工。

---

## 0. 缺口

对照 Spring AI 的模型层：

| Spring | 职责 | beamai（改造前） |
|---|---|---|
| `Prompt` | messages + ChatOptions | ❌ 没有类型，`(Messages, Opts)` 两个裸参数，provider 各自 `maps:get` |
| `ChatResponse` | 归一化响应 | ✅ 有（`beamai_llm_response`），但名字不成套 |
| `XxxChatModel` | 重试、observation、调用编排 | 部分：`beamai_chat_model` 只做路由 + 重试 |
| `XxxApi` | endpoint / headers / DTO | ⚠️ provider 混装了"底层信息"与"怎么发"（超时、连接池、rate-limit 各写一遍） |

「本次调用参数」与「连接凭证」混在同一个 Config 里，是最大的一处结构缺陷。

## 1. Stage 1：ChatRequest / ChatResponse

- 新增 **`beamai_chat_request`**（＝Spring 的 `Prompt`）：`messages` + `options`，
  构造 / 读取 / 纯函数改写（`with_messages`、`put_option`、`merge_options`…）。
- `beamai_llm_response` → **`beamai_chat_response`**（553 处，命名成套）。
- 三个"参数"的边界写死：

| | 是什么 | 生命周期 |
|---|---|---|
| provider **Config** | api_key / base_url / timeout / 该 provider 的默认模型参数 | 创建一次长期复用 |
| ChatRequest **options** | 这一次调用的模型参数，覆盖 Config 同名默认 | 单次 |
| filter 链的 chat **Req** | `#{messages, context, opts}` ＝ Spring 的 `ChatClientRequest` | 单次，filter 在这层改写 |

- **重试三件套不下发 provider**：`beamai_chat_model` 用 `?MODEL_LEVEL_OPTS` 过滤掉
  `max_retries` / `retry_delay` / `on_retry` / `callback_meta` / `on_llm_new_token`。

### 迁移中揪出的两个真问题

1. provider 里 11 处 `Request#{stream => true}` —— 迁移后会把 stream 写到请求顶层而不是
   options，**编译不报错、静默失效**。已全部改为 `beamai_chat_request:put_option/3`。
2. DeepSeek 的 **FIM 补全**不是 chat（prompt/suffix/echo/logprobs 自成一套），被批量脚本
   误改，已还原并加注释——它不该走 ChatRequest。

## 2. Stage 2：provider 只声明底层信息

新增 **`beamai_llm_http_provider`**：把"怎么发请求"收成一处——超时取值（Config 优先、
否则 provider 默认）、连接池路由、rate-limit 响应头解析、同步与流式两条路径的拼装。

provider behaviour 新增 7 个**声明式回调**（optional_callbacks，HTTP provider 全实现）：

```erlang
base_url(config()) -> binary().
endpoint(config(), chat_request()) -> binary().
headers(config(), chat_request()) -> [{binary(), binary()}].
body(config(), chat_request()) -> map().
parser(config()) -> fun((map()) -> {ok, map()} | {error, term()}).
stream_accumulator(config()) -> fun((term(), term()) -> term()).
stream_finalizer(config()) -> fun((term()) -> {ok, map()} | {error, term()}).
```

10 个 HTTP provider 的 `chat/2`、`stream_chat/3` 各退化为一行委托，样板代码（URL 拼装、
超时、连接池、rate-limit 回调）从每个 provider 里删除。

**回调都带 Config/Request 不是过度设计**：moonshot / siliconflow 按 `region` 选站点、
zhipu 按 `api_mode` 在 openai/anthropic 两套兼容协议间**整套**切换（端点/头/体/解析/累加器/
finalizer 六项同时变）、dashscope 流式要额外的 `X-DashScope-SSE` 头、deepseek 的 prefix
补全要换 beta 端点。

### 边界

- 非 HTTP provider（`mock`、测试用 `beamai_flaky_provider`）不实现这 7 个回调，自己写 `chat/2`。
- provider 的**非 chat 扩展 API**（deepseek FIM、zhipu 异步 chat）保留各自的传输代码——
  请求形状不是 ChatRequest，硬套只会更糟。
- URL 组装的唯一出处是 `beamai_llm_http_provider:url/3`（导出，供测试断言 region/override 逻辑）。

## 3. 验证锚点

- `apps/beamai_core/test/beamai_chat_request_tests.erl`（5 例）：构造/读取/**纯函数改写**、
  `with_options` 整体替换、以及**重试参数不进 ChatRequest**（meck provider 断言收到的 options）。
- 既有 700+ 例（含各 provider 的 body 构造断言、流式统一、缓存策略、rate-limit）全部无语义改动即通过。
- live（MiniMax-M2 经 anthropic 兼容端点）：`beamai_tcm_live_test` 2 例、
  `beamai_http_pool_live_test` 1 例，外加真机链路 `turn=1/step=2/chat=2/tool=1` 与
  重试层次（on_retry 2 次 / around_chat 1 次）复跑通过。

## 4. 未做（Stage 3）

`beamai_chat_client` 收窄：工具解析/定义/元数据 → `beamai_tool_registry`，单次执行 →
`beamai_tool_executor`，导出 18 → 11。设计见 `design/chat_client_capability_shrink.md`。
