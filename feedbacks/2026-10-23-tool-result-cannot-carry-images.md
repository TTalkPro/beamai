# 工具结果只能是文本，带不了图片

报的人：beamai_muse　严重度：🟡　回没回：☑ 已修复

## 症状

一个字都不报。工具 handler 返回「文字 + 图片」时，模型只收到一段文字：

- 返回 map / list：`encode_result/1` 把它 JSON 编码成一个字符串，图片的 base64 也只是这段文字的一部分（又长又没用）；
- 没有任何返回形状能让 provider 适配器发出 Anthropic 的 `tool_result.content: [{type:text}, {type:image}]`，或 OpenAI 那种「工具消息之后跟一条带 image 的消息」。

## 位置

- `apps/beamai_core/src/core/beamai_tool.erl:346-361`：`encode_result/1 -> binary()`，所有分支都落成 binary；
- 调用点：`apps/beamai_agent/src/beamai_agent_utils.erl:154`（`run_one_tool/4`）、`beamai_agent_tool_loop.erl:462`、`beamai_agent.erl:705`，工具消息的 `content` 从这里开始就只是文本；
- `apps/beamai_llm/src/adapters/beamai_llm_message_adapter.erl:251-260`：`to_anthropic_message(#{role := tool, ...})` 把 `Content` 原样作为 `tool_result.content`。Anthropic 本来接受 text/image 块的列表，但这里拿不到；
- 对照：用户 / 助手消息已经支持 `#{type => image, source => ...}` 部件（同文件 `:30`、`format_part_openai/1`、`format_part_anthropic/1`），只是工具消息走不到这条路。

## 怎么撞上的

beamai_muse 移植 OpenMuse 的 `use_desktop`（E2B 桌面）。OpenMuse（TanStack AI）的 handler 返回：

```ts
[{ type: "text", content: JSON.stringify({status, action, receiptId, screen, screenshot: "attached as an image for this step only"}) },
 { type: "image", source: { type: "data", value: base64Jpeg, mimeType: "image/jpeg" } }]
```

模型在这一步直接看到截图，再决定下一次点击坐标。聊天历史里只保留文字部分。

在 beamai 里同样返回一个 list：模型收到的是一大段 JSON 字符串，看不到画面。下游只好退化成在文字里给出签名截图 URL，模型拿不到图，也就没法按画面操作。

## 影响面

- 所有 computer-use / 桌面 / 浏览器截图类工具：模型"看不见"自己的操作结果，只能盲操作；
- 生成图表、读取图片附件后想交给模型看的工具，也是同样情况；
- 不报错。

## 建议

- 允许 handler 返回内容部件列表，例如 `{parts, [#{type => text, text => ...}, #{type => image, source => #{type => base64, media_type => ..., data => ...}}]}`。`encode_result/1` 遇到它时保留结构（或另开一条不经过 `encode_result` 的路径），工具消息的 `content` 可以是部件列表；
- 适配器：
  - Anthropic：`tool_result.content` 直接发 text/image 块列表；
  - OpenAI chat：tool 消息只发文本部分，图片紧跟一条 `role: user` 的 `image_url` 消息（OpenAI 的通行做法）；
  - Responses：`function_call_output` 加一条 `input_image`；
- 历史 / memory：像 OpenMuse 一样只持久化文本部分（图片只在当前这一步有效），或者交给 memory 后端决定。
