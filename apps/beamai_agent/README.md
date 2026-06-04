# BeamAI Agent (SimpleAgent)

中文

以 ReAct 模式为核心的有状态多轮对话 Agent。封装 `beamai_kernel`，自实现工具循环
（Tool Loop），确保每次 LLM 调用与工具调用都经过完整的 Filter 洋葱链。

> 仅依赖 `beamai_core` 与 `beamai_llm`。持久化、middleware、Process-native Agent
> 等扩展能力位于 [beamai_extra](https://github.com/TTalkPro/beamai_extra)。

## 特性

- ReAct 模式的工具循环（Tool Loop）
- 完整的回调系统（Callbacks，观察 + 控制）
- 中断与恢复（Interrupt / Resume）
- 多轮对话（消息历史自动累积）
- 流式输出（stream）

## 模块概览

| 模块 | 说明 |
|------|------|
| `beamai_agent` | 主 API（new/run/stream/resume/查询/修改） |
| `beamai_agent_state` | Agent 状态构建与 kernel 集成（含 callback filter 注入） |
| `beamai_agent_tool_loop` | 统一的 ReAct 工具循环执行器 |
| `beamai_agent_callbacks` | 回调系统 |
| `beamai_agent_interrupt` | 中断与恢复机制 |
| `beamai_agent_utils` | 共享工具函数 |

## API

```erlang
%% 构造
beamai_agent:new(Config) -> {ok, State} | {error, Reason}.

%% 执行
beamai_agent:run(State, UserMsg)    -> {ok, Result, NewState} | {interrupt, Info, NewState} | {error, Reason}.
beamai_agent:stream(State, UserMsg) -> {ok, Result, NewState} | {interrupt, Info, NewState} | {error, Reason}.

%% 中断恢复
beamai_agent:resume(State, HumanInput) -> {ok, Result, NewState} | {interrupt, Info, NewState} | {error, Reason}.
beamai_agent:is_interrupted(State)     -> boolean().
beamai_agent:get_interrupt_info(State) -> Info | undefined.

%% 查询 / 修改
beamai_agent:messages(State), last_response(State), turn_count(State), kernel(State), id(State), name(State).
beamai_agent:set_system_prompt(State, P), add_message(State, Msg), clear_messages(State), update_metadata(State, Map).
```

### 配置选项（new/1）

```erlang
{ok, State} = beamai_agent:new(#{
    %% LLM：{Provider, Opts} 元组，或 beamai_chat_completion:create/2 的结果，
    %% 或直接传入已构建好的 kernel（kernel => K，此时忽略 llm/plugins）
    llm => {anthropic, #{model => <<"claude-sonnet-4-5">>, api_key => Key}},

    system_prompt => <<"你是一个有帮助的助手。"/utf8>>,

    %% 工具插件模块（实现 beamai_tool_behaviour 的 tools/0，可选 filters/0）
    plugins => [my_tool_module],

    %% 最大工具循环迭代次数（默认见 beamai_common.hrl）
    max_tool_iterations => 10,

    %% 中断工具（命中即暂停，等待 resume）
    interrupt_tools => [#{name => <<"ask_human">>, description => <<"Ask">>, parameters => #{}}],

    %% 回调
    callbacks => #{
        on_llm_call  => fun(Messages, Meta) -> ok end,
        on_tool_call => fun(Name, Args) -> ok end
    }
}).
```

## 使用示例

### 基本对话

```erlang
{ok, State} = beamai_agent:new(#{
    llm => {anthropic, #{model => <<"claude-sonnet-4-5">>, api_key => Key}},
    system_prompt => <<"你是一个有帮助的助手。"/utf8>>
}),
{ok, Result, _NewState} = beamai_agent:run(State, <<"你好！"/utf8>>),
io:format("~s~n", [maps:get(content, Result)]).
```

### 带工具的 Agent

```erlang
Kernel = beamai_kernel:add_tool_module(beamai_kernel:new(), my_tool_module),
{ok, State} = beamai_agent:new(#{kernel => Kernel, llm => LLM}),
{ok, Result, _} = beamai_agent:run(State, <<"帮我查一下天气"/utf8>>).
```

### 多轮对话

```erlang
{ok, S0} = beamai_agent:new(#{llm => LLM}),
{ok, _, S1} = beamai_agent:run(S0, <<"我叫张三"/utf8>>),
{ok, R, _}  = beamai_agent:run(S1, <<"我叫什么？"/utf8>>).
```

### 中断与恢复

```erlang
case beamai_agent:run(State, <<"删除所有文件"/utf8>>) of
    {interrupt, Info, S1} ->
        %% 人工确认后恢复
        {ok, Result, _S2} = beamai_agent:resume(S1, <<"确认执行"/utf8>>);
    {ok, Result, _S1} ->
        Result
end.
```

## 回调类型

| 回调 | 触发时机 | 参数 |
|------|----------|------|
| `on_turn_start` | 新 turn 开始 | `(Meta)` |
| `on_turn_end`   | turn 正常完成 | `(Meta)` |
| `on_turn_error` | turn 执行出错 | `(Reason, Meta)` |
| `on_llm_call`   | 每次 LLM 调用前（around_chat 注入） | `(Messages, Meta)` |
| `on_tool_call`  | 每次工具调用前，可返回 `{interrupt, Reason}` | `(Name, Args)` |
| `on_token`      | 流式模式每个 token | `(Token, Meta)` |
| `on_interrupt`  | 进入中断状态 | `(IntState, Meta)` |
| `on_resume`     | 从中断恢复 | `(IntState, Meta)` |

## 依赖

- `beamai_core`（Kernel、Context、Filter、Tool）
- `beamai_llm`（LLM 调用）

## 许可证

Apache-2.0
