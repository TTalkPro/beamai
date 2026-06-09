# BeamAI Agent (SimpleAgent)

中文

以 ReAct 模式为核心的有状态多轮对话 Agent。封装 `beamai_kernel`，自实现工具循环
（Tool Loop）。

**Agent 自管编排（不借道 kernel filter）**：记忆与回调都是 Agent 自己的接口，由
tool loop **显式**调用——记忆走 `beamai_memory_provider`（载入/持久/发送前变换），
回调直接触发。Agent **不**向 kernel 注入 filter；kernel 保持纯原语。tool loop 自己
持有本轮完整 messages（within-run 累积），跨轮历史由 memory provider 按
`conversation_id` 持久化。

> 仅依赖 `beamai_core` 与 `beamai_llm`。持久化、middleware、Process-native Agent
> 等扩展能力位于 [beamai_extra](https://github.com/TTalkPro/beamai_extra)。

## 特性

- ReAct 模式的工具循环（Tool Loop）
- 用 Filter 实现的上下文记忆（filter-memory，按 conversation_id 管理历史）
- **工具并发执行**（一轮多个 tool_call 默认并发，`parallel_tools` 可关）
- **真流式输出**（每次 LLM 调用走 provider streaming，token 经 `on_token` 实时透出）
- 完整的回调系统（9 个 Callbacks，观察 + 控制）
- 中断与恢复（Interrupt / Resume）
- 多轮对话（历史由 filter-memory 维护，跨轮自动携带）
- 上下文窗口管理（`memory => {window, N}` 防长对话撑爆 context window）
- 进程纳入监督树（OTP 应用：默认 store 崩溃自动重启）

## 模块概览

| 模块 | 说明 |
|------|------|
| `beamai_agent` | 主 API（new/run/stream/resume/查询/修改） |
| `beamai_agent_state` | Agent 状态构建与 kernel 集成（callback filter 注入 + filter-memory 挂载） |
| `beamai_agent_tool_loop` | ReAct 工具循环执行器（delta 模式 + filter-memory） |
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

    %% 最大工具循环迭代次数（默认 10）
    max_tool_iterations => 10,

    %% 一轮多个 tool_call 是否并发执行（默认 true；false 则串行）
    parallel_tools => true,

    %% 会话记忆（filter-memory）：详见下文「会话记忆」一节
    %%   缺省               —— 共享默认 store（单例，按 conversation_id 分区，无界增长）
    %%   {window, N}        —— 默认 store 套 N 条滑动窗口
    %%   {window, Handle, N}—— 对自管 store 套 N 条滑动窗口
    %%   {Mod, Ref}         —— 自管 store 句柄（生命周期自负责）
    %%   false | none       —— 不启用记忆（messages/1 退化为 []）
    memory => {window, 20},
    %% 复用同一会话历史可显式指定 conversation_id（缺省自动生成）
    conversation_id => <<"conv-123">>,

    %% 中断工具（命中即暂停，等待 resume）
    interrupt_tools => [#{name => <<"ask_human">>, description => <<"Ask">>, parameters => #{}}],

    %% 回调（观察 + 控制；详见「回调类型」）
    callbacks => #{
        on_llm_call    => fun(Messages, Meta) -> ok end,
        on_tool_call   => fun(Name, Args) -> ok end,
        on_tool_result => fun(Name, Result) -> ok end
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
%% plugins：自动加载模块的 tools/0（及可选 filters/0）
{ok, State} = beamai_agent:new(#{llm => LLM, plugins => [my_tool_module]}),
{ok, Result, _} = beamai_agent:run(State, <<"帮我查一下天气"/utf8>>).
```

> 若改用 `kernel => K` 传入预构建 kernel，则 `llm`/`plugins` 被忽略，需自行
> `beamai_kernel:add_service/2` 配置 LLM、`add_tool_module/2` 注册工具。

### 多轮对话

历史由 filter-memory 按 `conversation_id` 维护，跨轮自动携带（无需 agent 自累积消息）：

```erlang
{ok, S0} = beamai_agent:new(#{llm => LLM}),
{ok, _, S1} = beamai_agent:run(S0, <<"我叫张三"/utf8>>),
{ok, R, _}  = beamai_agent:run(S1, <<"我叫什么？"/utf8>>).
```

> `messages/1` 从 store 读取完整历史（正序），含工具循环中的 assistant(tool_calls)
> 与 tool 结果消息；`system_prompt` 每次动态注入、不入存储。`memory => false` 时
> `messages/1` 返回 `[]`。

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

## 会话记忆（Memory）

记忆是 Agent 的一个**可插拔接口**，由 tool loop 显式调用（不经任何 kernel filter），
分两层协议（各有默认实现、都可 DIY）：

| 层 | 协议（behaviour） | 职责 | 默认实现 |
|----|------------------|------|----------|
| **策略** | `beamai_memory_provider` | `history`(载入跨轮)/`append`(持久化)/`prepare`(发送前变换:窗口/摘要/RAG)/`clear` | `beamai_memory_provider_default`（全量持久、prepare 恒等） |
| **存储** | `beamai_chat_memory` | dumb 后端：`mem_get`/`mem_add`/`mem_clear` | `beamai_chat_memory_ets` |

职责切分：**within-run**（一轮内跨工具迭代）消息由 loop 累积；**cross-run**（跨轮）
加载/持久由 provider 的 history/append；**发送前变换**（裁剪/摘要）由 prepare。
`memory` 的各种取值统一解析为一个 provider。窗口装饰器：`beamai_memory_provider_window`。

> 注：`beamai_memory_filter` + `beamai_kernel:with_memory/2` 是**kernel 级**的 filter
> 形态记忆，给"直接用 kernel / beamai facade"的人；Agent 层**不用**它。

```erlang
%% (a) 默认：默认 provider + 共享单例 store，无界增长（缺省即此）
{ok, A} = beamai_agent:new(#{llm => LLM}),

%% (b) 关闭记忆：单次无状态调用
{ok, A} = beamai_agent:new(#{llm => LLM, memory => false}),   %% 或 none

%% (c) 滑动窗口：全量仍存底层，发给 LLM 时只留最近 N 条非系统消息（防撑爆上下文）
{ok, A} = beamai_agent:new(#{llm => LLM, memory => {window, 20}}),

%% (d) 指定存储后端句柄（默认 provider 包它）
{ok, _Pid} = beamai_chat_memory_ets:start_link(my_store),
{ok, A} = beamai_agent:new(#{llm => LLM,
                            memory => {store, beamai_chat_memory_ets:handle(my_store)}}),

%% (e) 对自管 store 套窗口
{ok, A} = beamai_agent:new(#{llm => LLM,
                            memory => {window, beamai_chat_memory_ets:handle(my_store), 20}}),

%% (f) 完全自定义策略：实现 beamai_memory_provider 的 5 个 callback（摘要/RAG/token 窗口…）
%%     memory => {YourModule, Ref}
{ok, A} = beamai_agent:new(#{llm => LLM, memory => {my_summary_memory, Ref}}).
```

**自定义记忆策略**只需实现 `beamai_memory_provider` 的 4 个 callback：

```erlang
-module(my_summary_memory).
-behaviour(beamai_memory_provider).
-export([history/2, append/3, prepare/3, clear/2]).

history(Ref, ConvId) -> [_Msg].             %% 开场载入跨轮历史
append(Ref, ConvId, Msgs) -> ok.            %% 持久化（user/assistant/tool 结果）
prepare(Ref, ConvId, Messages) -> [_Msg].   %% 发送前变换（摘要/召回/裁剪；恒等即不变换）
clear(Ref, ConvId) -> ok.
```
```erlang
{ok, A} = beamai_agent:new(#{llm => LLM, memory => {my_summary_memory, MyRef}}).
```

> 兼容：`memory => {Mod, Ref}` 若 `Mod` 实现了 `beamai_memory_provider` 即作为 provider；
> 否则按 `beamai_chat_memory` 存储句柄处理，自动用默认 provider 包装。
>
> **默认 store 的生命周期**：beamai_agent 作为 OTP 应用启动时，默认 store 纳入监督树
> （permanent，崩溃自动重启）；未启动应用（库式直接调用 / 裸 eunit）时回退为懒启动的
> 孤儿单例。需要可控生命周期/持久化的场景请用自管 store 句柄或自定义 provider。

## 自定义 Filter（chat / tool）

Filter 是 **kernel 级**机制（不是 agent 特性——agent 的记忆/回调都不走 filter）。
`new/1` **没有顶层 `filters` 键**。要在 agent 用到的 kernel 上加 filter 有两条路：

**路 A — 预构建 kernel，传 `kernel =>`**（传 kernel 时 LLM 需自行 `add_service`；
记忆仍由 `memory` 配置独立解析为 provider，与 kernel 无关）：

```erlang
%% around_chat：Req = #{messages, context, opts}，Next 返回 #{response, context}
LogChat = beamai_filter:new(<<"log_chat">>,
    #{around_chat => fun(#{messages := Msgs} = Req, _FCtx, Next) ->
        io:format("发往 LLM ~p 条消息~n", [length(Msgs)]),
        Next(Req)
    end}, 0),

%% around_tool：Req = #{tool, args, context}，Next 返回 #{result, context}
TimeTool = beamai_filter:new(<<"time_tool">>,
    #{around_tool => fun(#{tool := Spec} = Req, _FCtx, Next) ->
        T0 = erlang:monotonic_time(millisecond),
        Resp = Next(Req),
        io:format("工具 ~s 耗时 ~p ms~n",
                  [maps:get(name, Spec, <<>>), erlang:monotonic_time(millisecond) - T0]),
        Resp
    end}, 0),

K0 = beamai_kernel:new(),
K1 = beamai_kernel:add_service(K0, beamai_chat_completion:create(openai, #{model => <<"gpt-4o">>})),
K2 = beamai_kernel:add_filter(K1, LogChat),
K3 = beamai_kernel:add_filter(K2, TimeTool),
{ok, A} = beamai_agent:new(#{kernel => K3, memory => {window, 20}}).
```

**路 B — plugin 模块的 `filters/0`**（与 `tools/0` 一起被自动加载）：

```erlang
-module(my_plugin).
-export([tools/0, filters/0]).
tools()   -> [#{name => <<"echo">>, description => <<"...">>, parameters => #{},
               handler => fun(A, _C) -> {ok, A} end}].
filters() -> [beamai_filter:new(<<"audit">>,
               #{around_tool => fun(R, _F, Next) -> Next(R) end}, 0)].
```
```erlang
{ok, A} = beamai_agent:new(#{llm => LLM, plugins => [my_plugin]}).
```

**Filter hook 速查**

| hook | Req 形状 | Next 返回 |
|------|----------|-----------|
| `around_chat` | `#{messages, context, opts}` | `#{response, context}` |
| `around_tool` | `#{tool, args, context}` | `#{result, context}` |

- 闭包签名 `fun(Req, FCtx, Next) -> Resp`：前置改 `Req` 再 `Next(Req2)`；后置改 `Resp`；
  短路则不调 `Next` 直接返回。
- **order 越小越外层**。内置：memory `-1000`、system_prompt `-500`、callback filter `9999`。
  默认 order `0` 的 filter 在 memory 内层 → **能看到展开后的完整历史**；若要改"写进 memory
  的内容"，需放到比 `-1000` 更外层。
- 只做观测（日志/统计/中断）时优先用 **callbacks**，无需手写 filter。

## 回调类型

| 回调 | 触发时机 | 参数 |
|------|----------|------|
| `on_turn_start` | 新 turn 开始 | `(Meta)` |
| `on_turn_end`   | turn 正常完成 | `(Meta)` |
| `on_turn_error` | turn 执行出错 | `(Reason, Meta)` |
| `on_llm_call`   | 每次 LLM 调用前（around_chat 注入） | `(Messages, Meta)` |
| `on_tool_call`  | 每个工具调用前，可返回 `{interrupt, Reason}` | `(Name, Args)` |
| `on_tool_result`| 每个工具执行得到结果后（并发时整批收齐后） | `(Name, Result)` |
| `on_token`      | 流式模式逐 token 实时推送 | `(Token, Meta)` |
| `on_interrupt`  | 进入中断状态 | `(IntState, Meta)` |
| `on_resume`     | 从中断恢复 | `(IntState, Meta)` |

> `Meta` 含 `agent_id / agent_name / turn_count / timestamp`；`on_llm_call` 的 Meta 另含
> `conversation_id / run_id`。

## 依赖

- `beamai_core`（Kernel、Context、Filter、Tool）
- `beamai_llm`（LLM 调用）

## 许可证

Apache-2.0
