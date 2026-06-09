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
| `beamai_agent_app` / `beamai_agent_sup` | OTP 应用 + supervisor（监督默认会话 store） |
| `beamai_agent_state` | Agent 状态构建与 kernel 集成（解析 memory provider） |
| `beamai_agent_tool_loop` | ReAct 工具循环执行器（full-messages，自管编排记忆与回调） |
| `beamai_agent_callbacks` | 回调系统（10 个回调） |
| `beamai_agent_interrupt` | 中断与恢复机制 |
| `beamai_subagent_manager` | 子 Agent 管理器（异步会话注册表：spawn/list/result/kill/restart/drop） |
| `beamai_agent_delegate` | 子 Agent 委派工具（同步委派/fan-out + 异步管理工具，建于管理器之上） |
| `beamai_agent_utils` | 共享工具函数（含工具执行/并发） |

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

    %% 会话记忆（memory provider）：详见下文「会话记忆」一节
    %%   缺省          —— 默认 provider + 共享默认 store（无界增长）
    %%   {window, N}   —— 默认 store 套 N 条滑动窗口
    %%   {store, Handle}—— 默认 provider 包指定存储后端句柄
    %%   {Mod, Ref}    —— 自定义 provider（须实现 beamai_memory_provider）
    %%   false | none  —— 不启用记忆（messages/1 退化为 []）
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
| **策略** | `beamai_memory_provider` | `history`(载入跨轮)/`append`(持久化)/`prepare`(发送前变换:窗口/摘要/RAG)/`clear` | `beamai_memory_provider_default`（包存储后端，全量持久；`new/2` 可带滑动窗口） |
| **存储** | `beamai_chat_memory` | dumb 后端：`mem_get`/`mem_add`/`mem_clear` | `beamai_chat_memory_ets` |

职责切分：**within-run**（一轮内跨工具迭代）消息由 loop 累积；**cross-run**（跨轮）
加载/持久由 provider 的 history/append；**发送前变换**（裁剪/摘要）由 prepare。
`memory` 的各种取值统一解析为一个 provider；窗口即默认 provider 的一个选项
（`beamai_memory_provider_default:new(Store, N)`）。

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

%% (e) 完全自定义策略：实现 beamai_memory_provider 的 4 个 callback（摘要/RAG/token 窗口…）
{ok, A} = beamai_agent:new(#{llm => LLM, memory => {my_summary_memory, Ref}}).
```

对自管存储套窗口：用默认 provider 的 `new/2` 构造再传入（`{Mod, Ref}` 直接作为 provider）：

```erlang
W = beamai_memory_provider_default:new(beamai_chat_memory_ets:handle(my_store), 20),
{ok, A} = beamai_agent:new(#{llm => LLM, memory => W}).
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

> `memory => {Mod, Ref}` **直接**作为 provider（须实现 `beamai_memory_provider`），不做
> 任何归一/兼容包装；要用裸存储后端请显式写 `{store, Handle}`。
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
- **order 越小越外层**。agent 不再自动注入 memory/callback filter（记忆/回调由 tool loop
  自管）；invoke_chat 仅按 opts 注入临时的 system_prompt filter（order `-500`）。你加的
  filter（order `0`）即在其内层，看到的就是本轮要发送的消息。
- 只做观测（日志/统计/中断）时优先用 **callbacks**，无需手写 filter。

## 子 Agent 委派（agent-as-tool）

`beamai_agent_delegate:tool/1` 生成一个**委派工具**（opt-in，加进 kernel/plugins 才生效）。
对标 Hermes `delegate_task` / OpenClaw `sessions_spawn`：**进程隔离 + 会话隔离**（默认），
上下文**显式传入**（`context` 参数由父 LLM 填、`seed` 由程序注入），**只把子 agent 的结论**
作为工具结果回流父记忆。

- **进程隔离(默认 `isolation => process`)**：子 agent 在 spawn_monitor 的独立进程里跑;
  子崩溃/超时只回 `{error,_}`(变成 error 工具结果),**绝不炸父进程**;`timeout`(默认 60s)
  对单次委派设上限并 kill 失控子 agent。`isolation => inline` 退回当前进程同步跑(省 spawn、
  无隔离/超时,仅适合可信轻量子任务)。
- **会话隔离**：子 agent 全新会话、`memory => false`、对父历史零知识(可由 subagent 配置覆盖)。

```erlang
Delegate = beamai_agent_delegate:tool(#{
    name => <<"delegate_research">>,
    description => <<"把子任务交给 researcher 子 agent"/utf8>>,
    %% 据本次调用构造子 agent（自带角色/工具/LLM；默认 memory=false、fresh conv）
    subagent => fun(_Args, _Ctx) ->
        #{llm => LLM, system_prompt => <<"你是 researcher"/utf8>>, plugins => [researcher_tools]}
    end,
    %% 可选:程序化注入“父记忆的一部分”（闭包捕获父 provider，自定义 scope）
    seed => fun(_Args, _Ctx) ->
        format(scope(beamai_memory_provider:history(ParentMem, ParentConv)))
    end,
    isolation => process,   %% 默认;独立进程 + 超时（inline 退回当前进程）
    timeout => 60000        %% 默认 60s;超时则 kill 子 agent 返回 {error, sub_agent_timeout}
}),
K  = beamai_kernel:add_tools(beamai_kernel:add_service(beamai_kernel:new(), Llm), [Delegate]),
{ok, Parent} = beamai_agent:new(#{kernel => K, memory => ParentMem, conversation_id => ParentConv}).
```

- **工具参数**:`task`(必填)+ `context`(可选,父 LLM 自行填写要传给子的背景)。
- `context` + `seed` 与 `task` 拼成子 agent 的输入;子 agent 跑完经 `result`（默认取
  `content`）提取回传。
- **并行委派**(两种)：
  1. 父一轮发多个 `delegate` 调用 + `parallel_tools=true` → `execute_concurrent` 并发(但**依赖模型肯把多个工具塞进一轮**);
  2. **`beamai_agent_delegate:fanout_tool/1`**(推荐,确定性并发)：LLM **一次**调用传 `tasks => [...]`,
     每个子任务由独立子 agent **并发**执行,全部完成/超时后汇总返回——不依赖模型批处理。
- **程序化 fan-out**：`beamai_agent_delegate:run_many([{SubConfig, Prompt}], Timeout)` 直接并发跑
  一批子 agent(spawn_monitor 全部→gather,整批截止超时,崩溃/超时各记 error),返回按序结果列表。
- 想"跨 agent 共享知识库",用独立共享 store + 一个检索工具（OpenClaw 范式），而非继承父记忆。

### 异步管理（子 agent 管理器）

所有委派统一经 `beamai_subagent_manager`(常驻 gen_server,受监督;库式调用时懒启动)。
同步 `tool/fanout/run_many` 是它之上的"spawn→await→drop"封装;需要**长跑/监控/监督**时
用异步管理:

```erlang
%% 程序化 API
{ok, Id} = beamai_subagent_manager:spawn(#{subagent => Cfg, prompt => P, owner => O}),
beamai_subagent_manager:list(O),          %% 运行中/已完成都在
beamai_subagent_manager:result(Id),       %% {ok,Outcome} | {error, not_ready|not_found}
beamai_subagent_manager:kill(Id),         %% kill 运行中
beamai_subagent_manager:restart(Id),      %% 复用 spec 重跑（同 Id）
beamai_subagent_manager:drop(Id).         %% 移除;或 ttl 自动回收

%% LLM 工具层（让 agent 跨轮自管子 agent，按 conversation 归属隔离）
Tools = beamai_agent_delegate:management_tools(#{subagent => fun(A,C) -> #{llm=>LLM} end}),
%% => spawn_subagent / list_subagents / subagent_result / kill_subagent / restart_subagent
```

- 状态机:running → done | failed(崩溃) | killed;每个子 agent 独立监督进程,崩溃只记
  `failed`、不炸管理器/父。
- 结果回收:`drop/1` 显式,或启动时 `subagent_ttl`(app env)到点自动清。
- 同步 vs 异步:简单"做子任务给答案"用 `tool/fanout`(阻塞拿结果);"起一批长任务、监控、
  按需取/kill/restart"用管理器 API / 管理工具。

```erlang
%% fan-out 工具：一次调用并发起 N 个子 agent
Fanout = beamai_agent_delegate:fanout_tool(#{
    name => <<"research_all">>,
    subagent => fun(Args, _Ctx) -> #{llm => LLM, system_prompt => <<"researcher">>} end,
    timeout => 60000
}),
%% 或程序化并发
Results = beamai_agent_delegate:run_many(
    [{#{llm => LLM}, <<"task A">>}, {#{llm => LLM}, <<"task B">>}], 60000).
%% => [{ok, <<"...">>}, {ok, <<"...">>}]
```

## 回调类型

| 回调 | 触发时机 | 参数 |
|------|----------|------|
| `on_turn_start` | 新 turn 开始 | `(Meta)` |
| `on_turn_end`   | turn 正常完成 | `(Meta)` |
| `on_turn_error` | turn 执行出错 | `(Reason, Meta)` |
| `on_llm_call`   | 每次 LLM 调用前 | `(Messages, Meta)` |
| `on_llm_result` | 每次 LLM 返回后（含中间轮，可取各次 usage） | `(Response, Meta)` |
| `on_tool_call`  | 每个工具调用前，可返回 `{interrupt, Reason}` | `(Name, Args)` |
| `on_tool_result`| 每个工具执行得到结果后（并发时整批收齐后） | `(Name, Result)` |
| `on_token`      | 流式模式逐 token 实时推送 | `(Token, Meta)` |
| `on_interrupt`  | 进入中断状态 | `(IntState, Meta)` |
| `on_resume`     | 从中断恢复 | `(IntState, Meta)` |

> `Meta` 含 `agent_id / agent_name / conversation_id / turn_count / run_id / timestamp`。
> `on_llm_result` 的 `Response` 是原始 `beamai_llm_response`，可经访问器取
> `content / tool_calls / usage / finish_reason`——这是观测**每次**（含中间轮）token
> 用量的唯一途径（`run` 结果的 `usage` 只反映最后一次）。

## 依赖

- `beamai_core`（Kernel、Context、Filter、Tool）
- `beamai_llm`（LLM 调用）

## 许可证

Apache-2.0
