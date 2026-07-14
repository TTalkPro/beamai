# TASK: Token 流变换链（filter 第四钩子 token_transform）

> 来源：clj-agent `docs/token-stream-filter-design.md`（:token-xf，transducer 承接）。
> Erlang 无 transducer，契约改为等价的 **step/flush map**：
> `#{init => S0, step := fun((TokenData, S) -> {[TokenData], S}), flush => fun((S) -> [TokenData])}`。
> 已核实前提：token 回调在调用方进程内**串行同步**执行（gun/hackney receive 循环，
> beamai_llm_http_client:233）；stream_chat 同步阻塞至流终；callback_meta 每请求静态；
> terminal 烤死 TokenCallback（chat filter 换不掉 sink，组装点无歧义）。
> 顺序 A→B→C→D→E，每批全量 eunit + dialyzer。不 commit：待用户 review。

## 语义决定（对照 clj 方案逐条）

1. **第四钩子 `token_transform`**，与 around_chat/around_tool/around_turn 并存于 hooks；
   同步路径（invoke_chat）完全忽略；tool/turn 链无关。
2. **TokenData** `#{token := binary(), meta := map()}`——保留 per-token meta 穿越缓冲。
3. **顺序 = filters 注册顺序**：靠前者最先见原始 token（与三链"靠前者最先见 req"一致）。
4. **状态作用域 = 单次 LLM 流**：组装器在 terminal 每次调用时现场初始化（init 值），
   工具循环每轮各自新状态；状态存组装进程 pdict（唯一 ref 键，flush 后 erase）。
5. **flush 只在正常完流**：`stream_chat` 返回 `{ok,_}` 后级联 flush（外层残留经内层
   step 传播再 sink，然后内层自己 flush——精确对齐 transducer completion 语义）；
   error/throw 不 flush（半截答案不外泄）。
6. **硬边界**：token 链只改送 sink 的出站流；最终归一化响应不经过它（结构免费成立）。
7. **零开销退化**：无 token_transform filter 时 TokenCallback 原样直通（路径与现状一致）。
8. reduced 早停不实现（无内置 take-n 需求，出界记录）。

## 批次 A：契约（beamai_filter）

- [x] A1 hooks 增可选键 `token_transform`；新类型 token_data/0、token_step/0、token_flush/0、
      token_transform/0 并导出；hook_type 不变（token_transform 不走 filter_chain 三链）；moduledoc 补第四钩子

## 批次 B：组装器（新模块 beamai_token_stream）

- [x] B1 `wrap([token_transform()], Sink) -> {TokenCallback, Flush}`：无 xf 时恒等直通；
      有 xf 时 pdict 存 [{Xf, State}]，step 链传播 1→N，Flush 级联
- [x] B2 单元测试：1→N 与 flush（3 缓冲批放，7 token → 2 批 + completion 冲尾）、
      组合顺序（先注册先见原始 token）、无状态改写、恒等退化

## 批次 C：kernel 组装点（beamai_kernel）

- [x] C1 `run_chat_stream`：收集 Filters 里的 token_transform（注册顺序）→ wrap(TokenCallback)
      → terminal 用包装后回调；`{ok,_}` 后调 Flush，error/throw 不调
- [x] C2 集成测试（meck stream_chat）：sink 收到变换后 token；最终响应不被变换；
      异常不 flush；无 token_transform 时行为与现状一致；同步 invoke_chat 带 token_transform 不受影响

## 批次 D：内置 filter（beamai_filters）

- [x] D1 `token_redact_filter(Pattern, Replacement)`：无状态逐 token 正则脱敏
      （re 构造时编译；文档写明跨 chunk 漏检限制）
- [x] D2 `hold_release_filter(CheckFun)`：缓冲整流；flush 时 CheckFun(全文) →
      `ok` 放行原序 | `{block, Text}` 只 emit 一个替换 token
- [x] D3 测试：redact 改写、reasoning/其它 meta 透传不误伤、hold-release 两分支

## 批次 E：文档

- [x] E1 FILTER.md / FILTER_EN.md：token_transform 契约 + 组装点 + 硬边界 + 内置 filter 表
- [x] E2 全量 eunit + dialyzer

## 明确出界

- reduced?/take-n 早停协议
- provider 层 reasoning token 独立通道（现 extract_token_from_event 只出 content 文本）

## 验证

- `rebar3 eunit`（全量，基线 362）+ `rebar3 dialyzer`（EXIT=0）
- 不 commit：待用户 review

## 完成记录（2026-07-14 全部实施）

- **全绿**：`rebar3 eunit` 377 tests / 0 failures（基线 362 +15：组装器单测 8 +
  kernel 集成 7）；`rebar3 dialyzer` EXIT=0。
- **A 契约**：`beamai_filter` hooks 增 `token_transform`（hook_type 第四值）；新类型
  token_data/token_step/token_flush/token_transform 并导出；hook/2 spec 放宽。
- **B 组装器**：新模块 `beamai_token_stream:wrap([Xf], Sink) -> {Callback, Flush}`——
  step 链 1→N 传播、flush 级联（外层残留经内层 step 再 sink，后内层自 flush，
  对齐 transducer completion）、状态存 pdict（唯一 ref 键，Flush erase，幂等）、
  无 xf 恒等直通。
- **C 组装点**：`run_chat_stream` 收集 token_transform（注册顺序）→ terminal **每次执行**
  现场 wrap（chat filter 重入 Next 每次流新状态）；`{ok,_}` 后 Flush，error/throw 不 flush。
- **D 内置**：`beamai_filters:token_redact_filter/2`（构造时 re:compile，逐 token
  global 替换；跨 chunk 漏检已文档化）、`hold_release_filter/1`（缓冲整流，
  CheckFun(全文) → ok 原序放行 | {block,Text} 单 token 替换）。
- **E 文档**：FILTER.md / FILTER_EN.md 新增 token_transform 专节（契约/三硬能力/硬边界
  交付 vs 答案分工表/内置 filter 表/示例）。
- **与 clj 方案的差异点**：transducer → step/flush map（`#{init, step, flush}`）；
  组装点简化——beamai terminal 烤死 TokenCallback（chat filter 换不掉 sink），
  无 clj "xf 链包链上存活的 on-token" 一说；meta 每请求静态（callback_meta），
  token_data 仍带 meta 以保穿越缓冲的一般性。
- **前提核实**：token 回调在调用方进程内串行同步执行（gun/hackney receive 循环，
  beamai_llm_http_client sse_chunk_handler → process_events → Callback 逐个调），
  pdict 状态安全；组装器文档写明"回调与 Flush 须同进程"。
