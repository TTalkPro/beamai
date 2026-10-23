# 同一回合「文本 + 工具调用」的 assistant 文本不进历史

报的人：beamai_muse　严重度：🟡　回没回：☑ 已修复

## 症状

一个字都不报。模型一个回合里先说「I found the permission slip. I’ll prepare a copy…」
再调 `delegate_task`：

- 实时流（AG-UI `TEXT_MESSAGE_CONTENT`）里这句话是有的；
- 落进 memory 的 assistant 消息却是
  `#{role => assistant, content => null, tool_calls => [...]}` —— **这句话没了**；
- 宿主按历史重绘线程（刷新页面）时它消失，下一轮把历史回放给模型时也缺这段。

## 位置

- `apps/beamai_core/src/llm/beamai_message.erl:120-129` —— `base_from_response/1`：
  `has_tool_calls` 为真时只走 `tool_calls(beamai_chat_response:tool_calls(Response))`，
  `beamai_chat_response:content(Response)` 被丢掉；
- `apps/beamai_core/src/llm/beamai_message.erl:88-89` —— `tool_calls/1` 把
  `content` 写死成 `null`；
- 调用点：`apps/beamai_agent/src/beamai_agent_tool_loop.erl:327`（`record_assistant/4`，
  注释写明这里是「一条 assistant 消息到此为止的唯一权威点」）、
  `apps/beamai_core/src/core/beamai_memory_filter.erl:56`；
- 回放侧：`apps/beamai_llm/src/adapters/beamai_llm_message_adapter.erl:261-267` ——
  `to_anthropic_message/1` 的 tool_calls 分支只发 thinking 块 + tool_use 块，即使
  `content` 有文本也不会发 text 块。（OpenAI chat 的 `to_openai_message/1` 和
  Responses 的 `:550-558` 已经能同时带 content 与 tool_calls，不受影响。）

## 怎么撞上的

```erlang
%% 任意 provider（这里用自定义 provider 最短）：返回 content + tool_calls
beamai_chat_response:new(#{content => <<"I found it.">>, finish_reason => tool_use,
                           tool_calls => [#{id => <<"c1">>, name => <<"t">>,
                                            arguments => #{}, raw_arguments => <<"{}">>}]}).
%% 跑一轮 beamai_agent（带 memory），再读 memory：
%%   [#{role => user, ...},
%%    #{role => assistant, content => null, tool_calls => [...]},   <- 文本没了
%%    #{role => tool, ...}, ...]
```

beamai_muse 里：sample 模式发送「Complete the permission slip」，实时看到那句话和任务卡片，
刷新页面后只剩任务卡片。

## 影响面

- 所有「先说一句再调工具」的回复。Anthropic 与 OpenAI 的模型都经常这样回，属于常态；
- 历史与实时流不一致：宿主 UI 重绘时那句话消失；
- 回放给模型时缺少自己上一轮说过的话（Anthropic 侧即使修了 `from_response` 也还会在
  适配器里再丢一次）；
- 不报错。

下游没有干净的绕法：文本只在流事件里出现，memory 后端拿到的是已经丢了文本的消息。

## 建议

- `base_from_response/1` 有 tool_calls 时保留文本：
  `#{role => assistant, content => Content | null, tool_calls => TCs}`
  （`tool_calls/1` 可加一个 `tool_calls(Content, TCs)` 变体）；
- `to_anthropic_message/1` 的 tool_calls 分支在 `content` 为非空文本时，在 tool_use 块前
  插入 `#{<<"type">> => <<"text">>, <<"text">> => Content}`；
- 已有用 `content => null` 判断「纯工具回合」的地方，改为看 `tool_calls` 是否非空。
