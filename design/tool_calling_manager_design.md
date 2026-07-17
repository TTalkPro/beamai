# ToolCallingManager 设计：统一工具执行管理（beamai/Erlang 移植）

> **状态：📋 设计阶段（移植自 clj-agent `tool-calling-manager-design.md`，针对 Erlang/OTP
> 与 beamai 项目实际架构适配）。待用户确认进入实施。**
>
> 本文以 clj-agent 的同名设计文档为蓝本，逐节对照 beamai 的实际代码与 Erlang 语言特性
> 做针对性移植。原设计基于 Clojure protocol + defrecord + defmulti；beamai 使用 Erlang
> behaviour（callback 模块）+ map + 模式分派。核心概念一一对应，但落地形态不同。
>
> 一旦实施，本文档顶部状态改为「已实施」并附 tests/assertions 计数。

---

## 0. TL;DR

引入 `beamai_tool_calling_manager` behaviour（**beamai_agent 模块**）作为**工具批量执行的
统一管理 seam**。现有 `beamai_agent_utils:execute_tools/5` 的逻辑升格为该 behaviour 的
默认实现模块 `beamai_default_tool_calling_manager`；未来可在同一 behaviour 下加
`execute_single` / `execute_async` 等执行 pattern。

同时扩展 `tool_spec()` map 加 `backend` 字段（`local` / `http` / `mcp`），让单个工具能
声明执行路径；分派由 `beamai_tool:invoke/3` 内部完成。**现有所有工具定义零改动**——
`backend` 缺省 `local`，老代码不需迁移。

**关键边界（防止与现有抽象重叠）**：

- `parallel_tools` / `serial` 仍是声明级执行策略，**不由 manager 决定**
- `around_tool` filter 仍是洋葱链，**不被 manager 取代**
- `writes` 屏障折叠仍走 `beamai_context:apply_writes/3`，**不被 manager 接管**
- manager 只把「批量执行入口」从 `beamai_agent_utils:execute_tools/5` 升格为**可注入 behaviour**

---

## 1. 背景：现有工具执行架构

### 1.1 分层全景

beamai 的工具执行分为三层（与 clj-agent 对应）：

| 职责 | Spring AI | clj-agent | beamai |
|---|---|---|---|
| 循环 / max-iterations | `ToolCallingAdvisor` | `react/invoke` | `beamai_agent_tool_loop:iterate/3` |
| 一批 tool-call | `ToolCallingManager` | `react/execute-batch` | `beamai_agent_utils:execute_tools/5` |
| 单个工具执行 | （manager 内部） | `kernel/invoke-tool` | `beamai_kernel:invoke_tool/4` |
| handler 调用 | `ToolCallback.call` | `tool/invoke` | `beamai_tool:invoke/3` |

### 1.2 现有 execute_tools 的完整职责

`beamai_agent_utils:execute_tools/5`（`beamai_agent_utils.erl:103`）当前承担：

1. **并发退化判定**：`parallel_tools=true` 且批内 >1 个 call 且无 `serial` 工具 → 并发；否则串行
2. **串行执行**：`execute_sequential/4`——逐个 `run_one_tool/3`、逐个触发 `OnResult`
3. **并发执行**：`execute_concurrent/4`——每工具一个 `spawn_monitor` 工作进程、
   `collect_tools/5` 按 idx 重排、`kill_pending/3` 超时兜底
4. **屏障折叠**：`finalize/3`——按 tool_call 原始序折叠 writes 进 state
   （`beamai_context:apply_writes/3`）
5. **错误归一**：`run_one_tool/3` 把 `{error, Reason}` 编码为 `#{error => #{...}}`，
   附 `error_class` 供屏障路由
6. **合成结果**：crash/timeout 合成 error tool 消息（`synth_result/3`）

这六项职责**都留在 manager 实现内部**。behaviour 只定义「执行入口」的契约。

### 1.3 调用点全景（4 处）

`execute_tools` 在代码库中有 **4 个直调点**（横跨 2 个模块）：

| # | 模块 | 函数 | 行 | 场景 |
|---|---|---|---|---|
| 1 | `beamai_agent_tool_loop` | `execute_and_continue/4` | :237 | 正常路径——执行全批、env_pause 判定、return_direct 判定、续跑 |
| 2 | `beamai_agent_tool_loop` | `handle_interrupt/6` | :204 | 中断路径——先执行同批安全 tools，被拦截的合成 skipped 结果 |
| 3 | `beamai_agent` | `resume_approval_raw/4` | :463 | 审批恢复——执行被批准的单个工具（`parallel=false`） |
| 4 | `beamai_agent` | `resume_env_raw/3` | :485 | 环境恢复——重跑失败调用（用 agent 的 `parallel_tools` 配置） |

调用点 1、2 经 tool_loop 主循环；调用点 3、4 是 resume 后的**直接工具执行**（不经
tool_loop 的 `execute_and_continue`），在 `beamai_agent.erl` 中直接调 `execute_tools`。
**全部 4 处都必须迁移到 manager**（见 §7.3）。

---

## 2. 为什么现在引入

### 2.1 新需求：tool_spec 要支持多 backend

当前所有工具只有一种执行路径——**进程内 Erlang 函数**（`beamai_tool:invoke/3` →
`call_handler/4` → `Handler(Args, Context)`）。需求是让 agent 能挂 HTTP 工具、MCP 工具
与本地工具混合使用，对 LLM 透明。

`beamai_tool:invoke/3` 直接调 handler，**没有 transport 分派 seam**——这是真实痛点。
Spring AI 用 `ToolCallback.call(input, ctx)` 协议统一 local / HTTP / MCP 三种 transport。
beamai 的等价物应落在 `tool_spec` 的 `backend` 字段 + `beamai_tool:invoke/3` 分派上（§5）。

### 2.2 可注入执行 seam（测试 + 扩展）

当前 `execute_tools` 是模块私有函数，`beamai_agent_tool_loop` 直接 hardcode 调用。
引入 manager behaviour 后：

- **测试可注入 mock**：验证循环对 manager 的依赖契约，不需 meck mock 整个 `beamai_agent_utils`
- **外部代码可拿到 manager 引用做自定义调度**：如分布式工具执行、优先级队列、自定义超时策略
- **为未来 `execute_single` / `execute_async` 等方法预留 headroom**

---

## 3. 与现有抽象的对账（核心）

### 3.1 逐条边界声明

下列抽象**本设计完全保留**，是边界，不是要重构的对象：

| 现有抽象 | 所在模块 | manager 的关系 |
|---|---|---|
| `parallel_tools` / `serial` | `beamai_agent_utils:execute_tools` | manager 读取它做批内并行退化，**不决策**——`parallel_tools` 仍是 agent 配置项，`serial` 仍是 tool_spec 声明 |
| `around_tool` filter 链 | `beamai_kernel:run_tool` → `beamai_filter_chain` | manager 包**整批调度**，filter 包**单个工具执行**。层次不同 |
| `writes` 屏障折叠 | `beamai_agent_utils:finalize/3` → `beamai_context:apply_writes/3` | 仍在 manager 实现内部，**不被 behaviour 接管** |
| 错误三分类 | `beamai_tool_error:classify/1` | manager 内部用（`run_one_tool` 仍分类），behaviour 不感知 |
| `invoke_with_retry` | `beamai_tool:invoke_with_retry/6` | 被 manager 内部的 `run_one_tool` 调用，不变 |
| gate / `on_tool_call` 中断 | `beamai_agent_tool_loop:find_first_interrupt` | **仍在 tool_loop，不进 manager** |
| `env_pause` 屏障路由 | `beamai_agent_tool_loop:env_pause` | **仍在 tool_loop，不进 manager** |
| `return_direct` 整批 AND | `beamai_agent_tool_loop:return_direct` | **仍在 tool_loop，不进 manager** |
| `eligibility` / max_iterations | `beamai_agent_tool_loop:iterate` | **仍在 tool_loop，不进 manager** |

**简言之**：manager 只负责「拿到批准的 calls 后，执行 + 收结果」这一段。
循环控制、gate、eligibility、return_direct 判定都不归它。

### 3.2 不重叠证明

| 担心 | 本设计如何避免 |
|---|---|
| manager 与 `parallel_tools`/`serial` 重叠 | `serial` 仍是**工具声明级**的批内并行退化开关，由 `default_tool_calling_manager` 在分派前读取；behaviour 本身**不知 serial 存在**。换 manager 实现仍要尊重 `parallel_tools`——这是契约 |
| manager 与 `around_tool` filter 重叠 | `around_tool` 包**单个工具执行**（在 `beamai_kernel:invoke_tool`），manager 包**整批调度**。两者层次不同 |
| concurrent manager 半管进程生命周期 | `spawn_monitor` 工作进程**写在 `beamai_default_tool_calling_manager` 内部**，**behaviour 不感知**。换 manager 实现 = 换一个模块，不持有池 |

---

## 4. Behaviour 设计

### 4.1 Erlang 适配决策：behaviour vs fun 注入

| 方案 | 形态 | 优点 | 缺点 |
|---|---|---|---|
| **A. behaviour** | `-behaviour(beamai_tool_calling_manager)` + callback 模块 `{Mod, Ref}` | 与 beamai 现有模式一致（`beamai_memory_provider`、`beamai_chat_memory`、`beamai_http` backend 都是 behaviour）；类型安全；dialyzer 可检 | 需新建模块 |
| B. fun 注入 | agent_state 持有 `fun((Kernel, ToolCalls, Opts) -> Result)` | 零新模块 | 无类型契约；dialyzer 不检；与现有 `{Mod, Ref}` 模式不一致 |

**选 A**——与 beamai 的 `beamai_memory_provider`（`{Mod, Ref}`）、`beamai_chat_memory`
（`{Mod, Ref}`）模式完全一致，符合 Erlang/OTP 惯例。

### 4.2 模块归属

| 模块 | 所在 app | 职责 |
|---|---|---|
| `beamai_tool_calling_manager`（behaviour 定义） | **beamai_agent** | 定义 callback + 类型；**不含实现** |
| `beamai_default_tool_calling_manager`（默认实现） | **beamai_agent** | 从 `beamai_agent_utils:execute_tools/5` 移植逻辑 |

> **与 clj-agent 的差异**：clj-agent 把 protocol 放 core（`core/tool_calling_manager.clj`），
> 默认实现放 client（`client/react.clj`）。beamai 的批量执行**本来就在 beamai_agent**，
> `beamai_core` 只有单次 `invoke_tool`。故 behaviour + 默认实现都在 `beamai_agent`。
> 这不破坏分层——`beamai_core` 不需要知道批量执行的存在。
>
> **Part B（backend 分派）**则不同——它在 `beamai_core/beamai_tool:invoke/3`，
> 因为单工具执行是 core 的原子能力。详见 §5。

### 4.3 Behaviour 定义

```erlang
%%%-------------------------------------------------------------------
%%% @doc ToolCallingManager behaviour：工具批量执行的统一管理 seam
%%%
%%% 定位（vs 现有抽象）：
%%% - 不夺 parallel_tools / serial 的权（声明级执行策略仍由工具声明者决定）
%%% - 不夺 around_tool filter 链的权（单工具洋葱链继续工作）
%%% - 不夺 writes 屏障的权（状态折叠仍走 beamai_context:apply_writes/3）
%%% - 只把「批量执行入口」从 beamai_agent_utils:execute_tools/5 升格为可注入 behaviour
%%%
%%% 管理器引用形态为 {Module, Ref}，与 beamai_memory_provider / beamai_chat_memory
%%% 的 {Mod, Ref} 模式一致。Module 须实现本 behaviour。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_calling_manager).

-export_type([manager/0, execute_opts/0, execute_result/0]).

-type manager() :: {module(), term()}.

-type execute_opts() :: #{
    context => beamai_context:t(),          %% 执行上下文（缺省 new/0）
    parallel => boolean(),                  %% 是否并发（缺省 false）
    on_result => fun((map()) -> ok)         %% 每工具完成回调（缺省 no-op）
}.

-type execute_result() :: #{
    messages := [map()],                    %% tool 结果消息（按原 call 序）
    records := [map()],                     %% 调用记录（含 name/args/result/error）
    context := beamai_context:t()           %% 折叠 writes 后的 context
}.

%% 唯一 callback：批量执行工具调用
%%
%% Manager 为 {Mod, Ref}；调用方经 Mod:execute_batch(Ref, Kernel, ToolCalls, Opts)
%% 路由到实现模块。Ref 为实现模块的私有状态（默认实现不用，自定义实现可携配置）。
-callback execute_batch(Ref, Kernel, ToolCalls, Opts) -> ExecuteResult when
    Ref        :: term(),
    Kernel     :: beamai_kernel:kernel(),
    ToolCalls  :: [map()],
    Opts       :: execute_opts(),
    ExecuteResult :: execute_result().

%% 可选 callback：单工具执行（未来扩展；首版不实现）
-callback execute_single(Ref, Kernel, ToolCall, Opts) -> {ok, map(), map()} when
    Ref        :: term(),
    Kernel     :: beamai_kernel:kernel(),
    ToolCall   :: map(),
    Opts       :: execute_opts().
-optional_callbacks([execute_single/4]).
```

### 4.4 默认实现

```erlang
%%%-------------------------------------------------------------------
%%% @doc 默认 ToolCallingManager：现有 execute_tools 逻辑的 behaviour 包装
%%%
%%% 行为零变化——串行/并发两条路径、屏障折叠、错误归一、合成结果全部保留。
%%% 唯一变化是从 beamai_agent_utils:execute_tools/5 的私有函数升格为
%%% 公开 behaviour callback。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_default_tool_calling_manager).
-behaviour(beamai_tool_calling_manager).

-export([new/0]).            %% 构造默认 manager 引用
-export([execute_batch/4]).  %% behaviour callback

%% 构造默认 manager（无状态，Ref 为 atom 占位）
-spec new() -> beamai_tool_calling_manager:manager().
new() -> {?MODULE, default}.

%% behaviour callback：委托给内部实现（从 beamai_agent_utils 移植）
-spec execute_batch(term(), beamai_kernel:kernel(), [map()],
                    beamai_tool_calling_manager:execute_opts()) ->
    beamai_tool_calling_manager:execute_result().
execute_batch(_Ref, Kernel, ToolCalls, Opts) ->
    Context = maps:get(context, Opts, beamai_context:new()),
    Parallel = maps:get(parallel, Opts, false),
    OnResult = maps:get(on_result, Opts, fun(_CR) -> ok end),
    %% 直接调用 beamai_agent_utils:execute_tools/5（逻辑不重复）
    {ToolMsgs, CallRecords, NewContext} =
        beamai_agent_utils:execute_tools(Kernel, ToolCalls, Context, Parallel, OnResult),
    #{messages => ToolMsgs, records => CallRecords, context => NewContext}.
```

**初版策略**：默认实现只是现有函数的转发壳。**行为零变化**——所有现有测试应原样通过。
`beamai_agent_utils:execute_tools/5` 保留不动（内部逻辑不重复，default manager 委托给它）。

> 后续可演化 manager 内部实现（如把 execute_tools 逻辑真正搬进 default manager 模块），
> 但**首版不搬**——避免 PR1 同时改逻辑 + 改接口。

### 4.5 manager 引用的注入点

```erlang
%% agent_state 增加 tool_calling_manager 字段
-type agent_state() :: #{
    ...
    tool_calling_manager := beamai_tool_calling_manager:manager(),
    ...
}.

%% beamai_agent_state:create/1 解析
create(Config) ->
    ...
    TCM = case maps:get(tool_calling_manager, Config, undefined) of
        undefined -> beamai_default_tool_calling_manager:new();
        {Mod, _Ref} = M when is_atom(Mod) -> M
    end,
    State = #{
        ...
        tool_calling_manager => TCM,
        ...
    },
    ...
```

> **为什么放 agent_state 而非 kernel**：clj-agent 把 manager 放 kernel record。
> 但 beamai 的 kernel 是 core 层的纯原语（单次 chat/tool），批量执行是 agent 层职责。
> 把 manager 放 agent_state 更符合 beamai 的分层：**core 不感知批量执行**。
>
> 若用户通过预构建 kernel 传入 agent（`kernel => K`），manager 仍从 Config 解析（agent 级配置）。
> 这与 `memory`、`callbacks` 等 agent 级配置一致。

### 4.6 未来扩展点（不在首版实施）

```erlang
%% 可能的后续 callback（仅作示意，不在首版加）：
-callback execute_single(Ref, Kernel, ToolCall, Opts) -> ...   %% 单工具，无 batch 调度
-callback execute_async(Ref, Kernel, ToolCalls, Opts) -> ...   %% 返回 {ok, Ref} + 异步回调
```

何时加？等真实需求出现。**首版只搬 `execute_batch`**。

---

## 5. tool_spec 扩展：`backend` 字段

### 5.1 设计

`tool_spec()` map 增加可选 `backend` 键（缺省 `local`），及相关 transport 配置字段。
**严格模式**：声明非 `local` backend 的工具**不允许写 handler**——`beamai_tool:validate/1`
在构造期检查。

```erlang
%% 1) 现状完全不变（backend 缺省 local）
Tool = #{
    name => <<"get_weather">>,
    description => <<"获取天气"/utf8>>,
    parameters => #{<<"city">> => #{type => string, required => true}},
    handler => fun(#{<<"city">> := City}, _Ctx) -> {ok, fetch_weather(City)} end
}.
%% 等价于显式 #{backend => local, ...}

%% 2) HTTP backend（无 handler）
Tool = #{
    name => <<"search_web">>,
    description => <<"搜索网页"/utf8>>,
    parameters => #{
        <<"query">> => #{type => string, required => true},
        <<"limit">> => #{type => integer, default => 10}
    },
    backend => http,
    endpoint => <<"https://tools.internal/search">>,
    method => post,
    headers => #{<<"Authorization">> => <<"Bearer ...">>}
}.

%% 3) MCP backend（无 handler）
Tool = #{
    name => <<"fs_read">>,
    description => <<"MCP 文件系统读"/utf8>>,
    parameters => #{<<"path">> => #{type => string, required => true}},
    backend => mcp,
    mcp_server => filesystem,       %% build-kernel 时注册的 mcp client 名
    remote_name => <<"read_file">>  %% MCP 端真实工具名（缺省取 deftool 名）
}.
```

### 5.2 tool_spec schema 扩展

| 字段 | 类型 | 缺省 | 含义 |
|---|---|---|---|
| `backend` | `local \| http \| mcp` | `local` | 分派键 |
| `endpoint` | `binary()` | — | HTTP backend：请求 URL |
| `method` | `get \| post \| ...` | `post` | HTTP backend：HTTP 方法 |
| `headers` | `map()` | `#{}` | HTTP backend：额外请求头 |
| `mcp_server` | `atom() \| binary()` | — | MCP backend：server 名（查 mcp_clients map） |
| `remote_name` | `binary()` | tool name | MCP backend：远端工具名 |

> **与 clj-agent 的差异**：clj-agent 用 `:tool/backend` + `:tool/transport` 两个元数据键
> （var metadata），因为 Clojure 的 metadata 是 namespace-qualified keyword。
> beamai 的 tool_spec 是 plain map，直接用 atom key（`backend`、`endpoint` 等），
> 与 `serial`、`sensitive`、`return_direct` 同层——beamai 现有 tool_spec opts 本就是平铺的。

### 5.3 验证（beamai_tool:validate/1 扩展）

```erlang
%% beamai_tool:validate/1 增加严格模式检查：
%%
%%   1. 非 local backend 且有 handler → 报错（transport 工具不执行本地代码）
%%   2. 非 local backend 且声明 context-sensitive handler → 报错（没有 handler，context 无所依附）
%%   3. 非 local backend 且缺 transport 必填字段 → 报错（http 缺 endpoint / mcp 缺 mcp_server）
%%   4. local backend（或缺省）且无 handler → 报错（local 工具必须有 handler）

validate(#{name := Name, backend := Backend} = Spec) when Backend =/= local ->
    Errors = lists:flatten([
        validate_name(Name),
        validate_no_handler(Spec),       %% 非 local 不许有 handler
        validate_transport(Spec, Backend) %% transport 必填字段
    ]),
    ...
```

> **与 clj-agent 的差异**：clj-agent 在**宏展开期**检查（编译期）。beamai 在
> `beamai_tool:validate/1` **运行时**检查（Erlang 无宏）。这意味着非法 tool_spec
> 只有在 validate 被调用时才发现。可以增加 `beamai_kernel:add_tool/2` 调用 validate
> 来提前发现（当前 add_tool 不 validate——可作为同步改进项）。

### 5.4 分派实现（beamai_tool:invoke/3）

> **Erlang 无 defmulti**。分派用模式匹配 + 注册的 backend handler 模块。

```erlang
%% beamai_tool:invoke/3 改造（当前直接 call_handler，改为先分派 backend）：
-spec invoke(tool_spec(), args(), beamai_context:t()) -> tool_result().
invoke(#{handler := Handler} = ToolSpec, Args, Context) ->
    %% 现有 local 路径完全不动（backend 缺省 local，有 handler）
    Timeout = maps:get(timeout, ToolSpec, 30000),
    {MaxRetries, InitialDelay} = retry_conf(maps:get(retry, ToolSpec, false)),
    invoke_with_retry(Handler, Args, Context, MaxRetries, InitialDelay, Timeout);

invoke(#{backend := Backend} = ToolSpec, Args, Context) when Backend =/= local ->
    %% 非 local 路径：dispatch 到注册的 backend handler 模块
    case resolve_backend(Backend, Context) of
        {ok, BackendMod} ->
            BackendMod:invoke(ToolSpec, Args, Context);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 按 backend 类型从 context（→ kernel settings）解析 handler 模块
%%
%% backend handler 模块注册在 kernel settings.tool_backends：
%%   #{tool_backends => #{http => beamai_tool_backend_http, mcp => beamai_tool_backend_mcp}}
%%
%% 缺省注册：beamai_agent app 启动时往默认 settings 注入（若用户未覆盖）。
%% 这与 beamai_http 的 backend 模式（app env 配置）一致。
resolve_backend(Backend, Context) ->
    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, {backend_requires_kernel, Backend}};
        Kernel ->
            Backends = beamai_kernel:tool_backends(Kernel),
            case maps:find(Backend, Backends) of
                {ok, Mod} -> {ok, Mod};
                error -> {error, {backend_not_registered, Backend}}
            end
    end.
```

**关键性质**：`beamai_tool:invoke/3` 签名不变（`invoke(ToolSpec, Args, Context)`），
返回值不变（`tool_result()`）。所有现有调用方（`beamai_kernel:tool_terminal`、
测试代码）零迁移。

### 5.5 backend handler 模块契约

非 local backend 的 handler 模块须实现：

```erlang
%% 所有 backend handler 模块的统一契约（非 behaviour，约定即可）：
-callback invoke(ToolSpec, Args, Context) -> tool_result() when
    ToolSpec :: beamai_tool:tool_spec(),
    Args :: beamai_tool:args(),
    Context :: beamai_context:t().
```

> **与 clj-agent 的差异**：clj-agent 用 `defmulti invoke-backend` + `defmethod` 注册。
> beamai 用 kernel settings 的 `tool_backends` map 做注册表。两者等价：都是
> 「backend 类型 → handler 模块」的查找表。beamai 的方式更显式（注册在 kernel，
> 不隐式依赖 ns require 副作用），也更可测（可注入 mock backend handler）。

### 5.6 backend 与现有字段的正交性

| tool_spec 字段 | `local` | `http` | `mcp` | 说明 |
|---|---|---|---|---|
| `handler` | ✅ 必填 | ❌ 禁止 | ❌ 禁止 | local 执行本地函数；remote 无 handler |
| `sensitive` | ✅ | ✅ | ✅ | gate 审批语义不变 |
| `serial` | ✅ | ✅ | ✅ | 批内退化按序语义不变 |
| `return_direct` | ✅ | ✅ | ✅ | 结果即最终答案语义不变 |
| `retry` | ✅ | ✅ | ✅ | invoke_with_retry 对 transient 类仍生效（HTTP 超时、MCP 网络错误） |
| `tag` | ✅ | ✅ | ✅ | tag 过滤语义不变 |
| `timeout` | ✅ | ✅ | ✅ | 超时语义不变（backend handler 须尊重） |
| `parameters` | ✅ | ✅ | ✅ | schema 生成不变（backend 不影响对外 schema） |

### 5.7 关于 inline / delegate 工具的边界

beamai 的 delegate 工具（`beamai_agent_delegate:tool/1`）是**进程内** map tool_spec，
有 `handler => fun(...)`。它们天然走 `local` 路径。

**结论**：

- delegate / fanout / subagent 管理工具是 inline tools，**永远走 `local`**——它们有 handler，
  没有 `backend` 字段（或缺省 `local`）
- §5 的 `backend` 扩展**只对声明了非 local backend 的 tool_spec 生效**
- 若用户想让 delegate 走 remote：必须自己写一个 `{M, F}` handler 做远程调用（仍是 local
  backend，只是 handler 内部做 HTTP）。或显式声明 `backend => http` 并去掉 handler

> **与 clj-agent 的差异**：clj-agent 有两条独立的工具执行路径（var 路径 vs inline-handler
> 路径），inline handler 绕过 `tool/invoke`。beamai **没有这个问题**——所有工具（包括
> delegate）都经 `beamai_tool:invoke/3`，因为 beamai 的 tool 是 map 不是 var，不存在
> 「绕过 invoke 直接调 handler」的路径。backend 分派覆盖所有工具，无盲区。

### 5.8 ToolSearch × backend

ToolSearch 索引的是 tool **specs/schemas**（`beamai_tool:to_tool_spec/1`），而 `backend`
存在 tool_spec map 上。所以：

- ✅ **索引正常工作**：HTTP/MCP 工具的 schema 在 index 里和 local 工具无差别
  （`to_tool_spec/1` 不输出 backend 字段）
- ✅ **执行正确**：搜索发现的工具下一轮进入 `tools` 列表后，执行时仍按 tool_spec 的
  `backend` 字段分派

---

## 6. Transport 实现细节

### 6.1 HTTP backend

**契约**：handler 模块 `beamai_tool_backend_http:invoke(ToolSpec, Args, Context)`

```erlang
invoke(#{endpoint := Url, method := Method, headers := Headers}, Args, Context) ->
    Body = beamai_utils:encode_json(#{<<"args">> => Args}),
    ReqHeaders = maps:merge(default_headers(), Headers),
    case beamai_http:request(Method, Url, ReqHeaders, Body, #{timeout => 30000}) of
        {ok, StatusCode, RespHeaders, RespBody} when StatusCode >= 200, StatusCode < 300 ->
            case json:decode(RespBody) of
                #{<<"result">> := Result} -> {ok, Result};
                #{<<"error">> := Error} -> {error, Error};
                Other -> {ok, Other}
            end;
        {ok, StatusCode, _RespHeaders, RespBody} ->
            {error, #{error_class => transient,
                      type => http_error,
                      status => StatusCode,
                      message => RespBody}};
        {error, timeout} ->
            {error, #{error_class => transient, type => timeout}};
        {error, Reason} ->
            {error, #{error_class => transient, type => network_error, reason => Reason}}
    end.
```

**首版范围**：

- 复用 `beamai_http`（Gun 默认，支持 HTTP/2 连接池）
- JSON 序列化用 OTP 27+ stdlib 内置的 `json` 模块（编码经 `beamai_utils:encode_json/1`，
  它抹平 iodata/proplist 两处差异；解码直接 `json:decode/1`）——JSON 在标准库里，
  与 clj-agent 的 cheshire 问题不同，beamai 无任何依赖障碍
- 超时：默认 30s（tool_spec `timeout` 字段透传），超时分类为 `transient`
- 鉴权：通过 `headers` 字段传入

> **与 clj-agent 的差异**：clj-agent 的 HTTP transport 必须放 client 模块（core 没有 cheshire）。
> beamai 的 JSON 来自 stdlib，`beamai_http` 也在 core——HTTP backend handler 可以放在
> `beamai_core` 或 `beamai_agent`，无模块依赖障碍。**推荐放 `beamai_agent`**（transport 是
> agent 级配置，core 不应感知 HTTP 工具这种应用层概念）。

### 6.2 MCP backend

**契约**：handler 模块 `beamai_tool_backend_mcp:invoke(ToolSpec, Args, Context)`

```erlang
invoke(#{mcp_server := Server, remote_name := RemoteName}, Args, Context) ->
    case resolve_mcp_client(Server, Context) of
        {ok, Client} ->
            %% Client 为 {Mod, Ref}（实现 MCP 协议；beamai_extra 或用户自定义）
            {Mod, Ref} = Client,
            Mod:call_tool(Ref, RemoteName, Args);
        {error, Reason} ->
            {error, Reason}
    end.

resolve_mcp_client(Server, Context) ->
    case beamai_context:get_kernel(Context) of
        undefined -> {error, no_kernel};
        Kernel ->
            Clients = beamai_kernel:mcp_clients(Kernel),
            case maps:find(Server, Clients) of
                {ok, Client} -> {ok, Client};
                error -> {error, {mcp_server_not_registered, Server}}
            end
    end.
```

**首版范围**：

- MCP client 注册在 kernel settings：`mcp_clients => #{filesystem => {mcp_client_mod, Ref}}`
- MCP 协议实现（STDIO/HTTP）属 `beamai_extra`（README 已说明 A2A/MCP 在扩展项目）
- **首版只定义 backend handler 契约 + 注册接口，不提供 MCP client 实现**——与
  `beamai_tool_index` behaviour 留接口、向量实现在 extra 的取舍一致

### 6.3 错误归一化

所有 backend 的失败必须用 beamai 现有错误模型：

- 返回 `{error, Reason}`，Reason 为 map 时可带 `error_class`
- `beamai_tool_error:classify/1` 负责把 HTTP 状态码、MCP 错误码等归一化到三分类
  （已有逻辑，backend handler 只需返回合理的 Reason 结构）

**当前 classify 已覆盖的关键模式**（`beamai_tool_error.erl`）：

- `timeout` → `transient`（HTTP 超时自动归 transient）
- `#{error_class => environment}` → `environment`（显式标注最高优先级）
- `#{'__llm_error__' => true, type => auth}` → `environment`（provider auth 错误）

HTTP backend handler 应在 Reason 里带 `error_class` 或遵循 classify 的模式匹配规则。

### 6.4 MCP client 注册

```erlang
%% kernel settings 增加 mcp_clients
Kernel = beamai_kernel:new(#{
    state_slots => #{...},
    tool_backends => #{http => beamai_tool_backend_http, mcp => beamai_tool_backend_mcp},
    mcp_clients => #{filesystem => {my_mcp_client, Ref}}
}).
```

backend handler 分派时按 `tool_spec.mcp_server` 查找。**未注册的 server 名 → 报错**
（`{error, {mcp_server_not_registered, Server}}`）。

---

## 7. 接入点（beamai_agent_tool_loop）

### 7.1 改造前（现状）

```erlang
%% execute_and_continue/4（beamai_agent_tool_loop.erl:232）
execute_and_continue(TCs, Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, callbacks := Callbacks, messages := Messages, ...} = Opts,
    Parallel = maps:get(parallel_tools, Opts, true),
    {ToolResults, NewToolCalls, NewCtx} =
        beamai_agent_utils:execute_tools(Kernel, TCs, ctx(Opts), Parallel,
                                         tool_result_cb(Callbacks)),
    ...
```

### 7.2 改造后

```erlang
execute_and_continue(TCs, Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, callbacks := Callbacks, messages := Messages, ...} = Opts,
    TCM = maps:get(tool_calling_manager, Opts, beamai_default_tool_calling_manager:new()),
    #{messages := ToolResults, records := NewToolCalls, context := NewCtx} =
        beamai_tool_calling_manager:execute(TCM, Kernel, TCs, #{
            context => ctx(Opts),
            parallel => maps:get(parallel_tools, Opts, true),
            on_result => tool_result_cb(Callbacks)
        }),
    ...
```

**新增分派函数** `beamai_tool_calling_manager:execute/4`：

```erlang
%% beamai_tool_calling_manager 模块增加 execute/4 分派器（behaviour 模块不只定义 callback，
%% 也提供调用入口——与 beamai_memory_provider:history/2 等分派函数同款）
-spec execute(manager(), beamai_kernel:kernel(), [map()], execute_opts()) -> execute_result().
execute({Mod, Ref}, Kernel, ToolCalls, Opts) ->
    Mod:execute_batch(Ref, Kernel, ToolCalls, Opts).
```

### 7.3 全部四处调用点同步改造

`execute_tools` 在代码库中有 **4 个调用点**（横跨 2 个模块），全部改为经 manager：

| # | 模块 | 函数 | 行 | 场景 | 现有调用 |
|---|---|---|---|---|---|
| 1 | `beamai_agent_tool_loop` | `execute_and_continue/4` | :237 | 正常路径：执行全批 → env_pause → return_direct → 续跑 | `execute_tools(Kernel, TCs, Ctx, Parallel, OnResult)` |
| 2 | `beamai_agent_tool_loop` | `handle_interrupt/6` | :204 | 中断路径：执行同批安全 tools（被拦截的合成 skipped） | `execute_tools(Kernel, SafeCalls, Ctx, Parallel, OnResult)` |
| 3 | `beamai_agent` | `resume_approval_raw/4` | :463 | 审批恢复：执行被批准的单个工具（parallel=false） | `execute_tools(Kernel, [ToolCall], Ctx, false)` |
| 4 | `beamai_agent` | `resume_env_raw/3` | :485 | 环境恢复：重跑失败调用（用 agent 的 parallel 配置） | `execute_tools(Kernel, FailedCalls, Ctx, Parallel)` |

**改造要点**：

- 调用点 1、2 在 `beamai_agent_tool_loop`：从 `loop_opts` 取 `tool_calling_manager`
  （由 `beamai_agent:run_loop/3` 从 `agent_state` 透传入 `loop_opts`）
- 调用点 3、4 在 `beamai_agent`：直接从 `agent_state` 取 `tool_calling_manager`
- 四处都改为 `beamai_tool_calling_manager:execute(TCM, Kernel, ToolCalls, Opts)`
- 返回值从 `{ToolMsgs, Records, Ctx}` 三元组改为 `#{messages, records, context}` map
  （pattern match 解构，改动局部）

> **为什么 resume 路径也要走 manager**：调用点 3、4 是 resume 后的**直接工具执行**
> （不经 tool_loop 的 `execute_and_continue`），如果它们不走 manager，则注入的
> 自定义 manager（如 mock / 分布式执行器）在 resume 时被绕过——seem 不完整。
> 特别是调用点 4（env retry 重跑）：它可能执行 HTTP/MCP 工具，需经 backend 分派。

### 7.4 不变的部分

下列代码**完全不动**：

- `find_first_interrupt` / `handle_interrupt` 的中断检测逻辑（只改其中 execute_tools 调用点）
- `env_pause` 环境失败暂停判定
- `return_direct` 整批 AND 判定
- `classify_tool_calls` / `partition_by_callback` callback 中断检查
- `iterate` 主循环结构
- `record_assistant` / `persist` / `finish` / `finish_direct`

这些是 `beamai_agent_tool_loop` 的循环控制职责，**不属于 manager**。

---

## 8. 迁移路径

### PR1：纯重构，抽出 ToolCallingManager（行为零变化）

**目标**：把 `execute_tools` 升格为 behaviour callback，**所有现有测试原样通过**。

**关键策略**：保留现有 `beamai_agent_utils:execute_tools/5` 函数完全不动；
`beamai_default_tool_calling_manager:execute_batch/4` 只是薄委托。**不改变现有函数签名**。

**改动文件**：

- `beamai_agent/src/beamai_tool_calling_manager.erl`（**新**——behaviour 定义 + execute/4 分派器 + 类型导出）
- `beamai_agent/src/beamai_default_tool_calling_manager.erl`（**新**——默认实现，委托给 execute_tools/5）
- `beamai_agent/src/beamai_agent_state.erl`（`agent_state` 增 `tool_calling_manager` 字段；
  `create/1` 解析 Config 的 `tool_calling_manager`，缺省 default）
- `beamai_agent/src/beamai_agent_tool_loop.erl`（调用点 1、2：`execute_and_continue/4` +
  `handle_interrupt/6` 改调 manager；`loop_opts` 增 `tool_calling_manager`）
- `beamai_agent/src/beamai_agent.erl`（调用点 3、4：`resume_approval_raw/4` +
  `resume_env_raw/3` 改调 manager；`run/stream/resume` 透传 `tool_calling_manager`
  进 loop_opts）

**不变**：

- `beamai_agent_utils:execute_tools/5` 完全保留不动（default manager 委托给它）
- `beamai_kernel` 完全不动
- `beamai_tool` 完全不动

**验收**：

- 全套 tests 通过（基线 ~380 tests / ~377 after token_stream），**零代码修改**
- 新增测试：
  - **mock manager 注入**（验证 seam 真实存在、可替换）——注入一个返回固定结果的
    `{mock_tcm, Ref}` manager，验证 tool_loop 用的是它而非 execute_tools
  - **边界验证测试**——证明 §3 的不重叠契约：
    - 注入 mock manager 时，自定义 `around_tool` filter 仍触发
    - 注入 mock manager 时，`serial => true` 仍让整批退化串行（mock 须尊重 parallel=false）
    - 注入 mock manager 时，writes 仍在屏障处折叠（mock 须调 apply_writes）

### PR2：tool_spec 扩展 `backend`（仅 local + http，mcp 留接口）

**目标**：让 tool_spec 能声明 HTTP backend，跑通一个 HTTP 工具示例。

**改动文件**：

- `beamai_core/src/kernel/beamai_tool.erl`（tool_spec 类型增 `backend`/`endpoint`/`method`/
  `headers`/`mcp_server`/`remote_name`；`validate/1` 增严格模式检查；
  `invoke/3` 增 backend 分派）
- `beamai_core/src/kernel/beamai_kernel.erl`（增 `tool_backends/1` 查询 + settings 解析；
  增 `mcp_clients/1` 查询）
- `beamai_agent/src/beamai_tool_backend_http.erl`（**新**——HTTP transport handler）
- `beamai_agent/test/beamai_tool_backend_tests.erl`（**新**——backend 分派测试）
- `examples/http_backend_test.erl`（**新**——live 验证脚本，用 mock HTTP server）

**验收**：

- 全套 tests 通过
- 新增 `beamai_tool_backend_tests`（validate 严格模式、local 路径不变、http 分派）
- live 脚本通过（mock HTTP server 验证 transport）

### PR3（可选）：MCP backend

**目标**：加 MCP backend handler + 注册接口，至少跑通一个真实 MCP server。

**何时做**：等具体 MCP 场景出现（beamai_extra 提供 MCP client 实现时）。首版只留
backend handler 契约 + kernel 注册接口占位。

---

## 9. 未决问题

### 9.1 `beamai_kernel:invoke_tool/4` 也升格为 manager 方法吗？

`beamai_kernel:invoke_tool/4` 是**单工具执行**原语（经 around_tool filter 链）。
它和 `execute_batch` 是不同粒度。

**首版不动 invoke_tool**。原因：

- 它被多处直调（kernel 是公开 API），改成协议方法会破坏现有调用方
- 它已经经 `around_tool` filter 链，形态成熟
- backend 分派在 `beamai_tool:invoke/3`（更内层）已足够

未来若加 `execute_single` callback，可作为 invoke_tool 的 behaviour 化版本——但**首版不做**。

### 9.2 execute_result 是 behaviour 类型还是 plain map？

**首版用 plain map**（`#{messages, records, context}`）。与 beamai 现有约定一致
（`beamai_tool_result`、`beamai_llm_response` 都是 map）。

### 9.3 manager 持有什么状态？

首版 `beamai_default_tool_calling_manager` 是无状态 `{?MODULE, default}`。未来可能加：

- `mcp_clients`（MCP 分派时查）——但 MCP client 在 kernel settings，manager 不需持有
- `http_client`（HTTP transport 共享连接池）——但 beamai_http 已有连接池
- `gather_timeout`（并发收集超时）——当前从 app env 读，可改成 manager 配置项

**首版不加**——避免无需求先加复杂度。

### 9.4 命名：ToolCallingManager 还是 ToolManager？

Spring 用 `ToolCallingManager`。beamai 已有 `kernel` 概念（中央编排），但 kernel 是
core 层的纯原语。`ToolCallingManager` 是 agent 层的批量执行 seam，两者不撞。

**首版用 `beamai_tool_calling_manager`**——与 Spring 对齐，迁移期文档对照方便。

### 9.5 execute_batch 的 opts map vs positional args

现状 5-arity：`execute_tools(Kernel, ToolCalls, Context, Parallel, OnResult)`。
behaviour callback 用 opts map：`execute_batch(Ref, Kernel, ToolCalls, #{context, parallel, on_result})`。

**定论**：behaviour callback 用 opts map（避免 arity 爆炸 + 未来加字段不破坏回调）。
但 `beamai_agent_utils:execute_tools/5` 保留 positional（向后兼容，default manager 内部调用）。

---

## 10. Erlang 特有考量

### 10.1 无宏——tool_spec 验证在运行时

clj-agent 的 `deftool` 宏在编译期检查非法组合（非 local backend + handler body）。
beamai 的 `beamai_tool:new/3` 是普通函数，检查只能在运行时 `validate/1`。

**缓解**：让 `beamai_kernel:add_tool/2` 在注册时调 `validate/1`，把错误前置到 build 期
（而非 invoke 期）。当前 `add_tool` 不 validate——PR2 同步改进。

### 10.2 无 multimethod——backend 分派用注册表

clj-agent 用 `defmulti invoke-backend` + `defmethod` 注册分派。beamai 用 kernel
settings 的 `tool_backends` map 做 lookup 表。两者等价，beamai 的方式更显式、可注入。

### 10.3 并发模型——spawn_monitor 已就绪

clj-agent 讨论虚拟线程池生命周期。beamai 的 `spawn_monitor` + selective receive +
`collect_tools` + `kill_pending` 已经是成熟的并发模型，无需额外抽象。

换 manager 实现（如分布式执行）时，新实现自管进程模型（可能用 gen_server / Task），
behaviour 不约束内部并发方式。

### 10.4 进程字典 vs 显式传递

clj-agent §5.4 讨论 MCP client 访问的悬而未决问题（三个候选方案）。
beamai 的 `beamai_context:get_kernel/1` 已经能从 context 拿到 kernel，kernel 的
settings 又能拿到 `mcp_clients`——**问题在 beamai 不存在**：

- `beamai_tool:invoke/3` 已接收 Context 参数
- Context 已绑定 kernel（`beamai_context:with_kernel/2` 在 `invoke_tool` 入口）
- kernel settings 的 `mcp_clients` 直接可查

无需进程字典、无需加宽签名、无需 binding 动态变量。

### 10.5 OTP 监督

当前 `beamai_agent_utils:execute_concurrent` 的 worker 进程是临时的 `spawn_monitor`，
不在监督树内。这是设计意图（worker 是瞬态计算单元，崩溃由 DOWN 消息兜底合成 error 结果）。

自定义 manager 实现若需长生命周期进程（如 MCP 连接池），应自管 gen_server 并纳入
`beamai_agent_sup` 监督树——这与 `beamai_subagent_manager`（常驻 gen_server）的模式一致。

---

## 11. 决策记录

| # | 决策 | 选择 | 理由 |
|---|---|---|---|
| 1 | manager 形态：behaviour vs fun 注入 | **behaviour** | 与 beamai 现有 `{Mod, Ref}` 模式一致（memory_provider / chat_memory / http backend） |
| 2 | manager 模块归属 | **beamai_agent** | 批量执行是 agent 层职责；core 只有单次 invoke_tool |
| 3 | manager 引用注入点 | **agent_state**（非 kernel） | beamai 的 kernel 是 core 纯原语；manager 是 agent 级配置（同 memory/callbacks） |
| 4 | `backend` 配置平铺 vs 嵌套 | **平铺** | 与 tool_spec 现有 opts 风格一致（serial/sensitive/return_direct 均平铺） |
| 5 | remote backend 是否允许 handler | **严格禁止** | 防止本地代码与远端 transport 语义混淆 |
| 6 | execute_result 形态 | **plain map** | 与 beamai 现有约定一致 |
| 7 | backend 分派机制 | **kernel settings tool_backends 注册表** | 无 multimethod；显式注册更显式可测可注入 |
| 8 | HTTP backend 模块归属 | **beamai_agent** | transport 是应用层概念；core 不应感知 HTTP 工具 |
| 9 | MCP backend 实现 | **首版只留契约** | MCP client 实现属 beamai_extra（README 已声明） |
| 10 | MCP client 访问方式 | **经 context → kernel → settings.mcp_clients** | beamai_context 已绑定 kernel，问题不存在（与 clj-agent 不同） |
| 11 | `beamai_agent_utils:execute_tools/5` 保留 | **是（deprecated alias）** | PR1 零变化承诺可证伪；PR2+ caller 迁移后再考虑移除 |

---

## 附录 A：与 clj-agent 原设计的逐项对照

| clj-agent 原设计 | beamai 适配 | 差异原因 |
|---|---|---|
| `defprotocol ToolCallingManager`（core） | `-behaviour(beamai_tool_calling_manager)`（agent） | Erlang behaviour = Clojure protocol；放 agent 因批量执行是 agent 层职责 |
| `defrecord DefaultToolCallingManager`（client） | `beamai_default_tool_calling_manager` 模块（agent） | Erlang module = Clojure record；无字段 |
| Kernel `:tool-manager` 字段 | agent_state `tool_calling_manager` 字段 | beamai kernel 是 core 纯原语 |
| `deftool` 宏 `:backend` 元数据 | tool_spec map `backend` 字段 | beamai 无宏；tool 是 map |
| 宏展开期严格模式检查 | `validate/1` 运行时检查 | Erlang 无编译期宏 |
| `defmulti invoke-backend`（core，公开） | `beamai_tool:invoke/3` 模式匹配 + tool_backends 注册表 | Erlang 无 multimethod |
| `:http` 方法 client `defmethod` 注册 | `tool_backends => #{http => beamai_tool_backend_http}` | 显式注册 vs 隐式 ns require |
| cheshire 在 client → HTTP transport 放 client | JSON 在 stdlib → 无模块依赖障碍 | OTP 27+ 内置 `json` 模块 |
| mcp-clients 访问悬而未决（三候选） | context → kernel → settings.mcp_clients（已解决） | beamai_context 已绑定 kernel |
| 虚拟线程池生命周期 | spawn_monitor 已就绪 | beamai 已有成熟并发模型 |
| inline tools 绕过 tool/invoke | **不存在** | beamai 所有工具经 invoke/3，无盲区 |
| `^:private` defmulti | 无等价物（beamai 无私有多方法） | N/A |

---

## 相关文档

- `design/spring_advisor_alignment.md`（beamai 对齐 Spring AI 2.0 Advisor 的总览——本文档
  补充其中的 ToolCallingManager 缺口）
- `design/context_split_parallel_tools.md`（writes / state_slots 屏障契约——manager 内部仍走这套）
- `design/hitl_timeline_serial_errors.md`（gate / 暂停 / resume / 错误分类与 manager 的边界分工）
- clj-agent 原设计：`docs/tool-calling-manager-design.md`（本文档的移植蓝本）

## 参考

- Spring AI 2.0 `ToolCallingManager`：
  [`ToolCallingManager.java`](https://github.com/spring-projects/spring-ai/blob/main/spring-ai-model/src/main/java/org/springframework/ai/model/tool/ToolCallingManager.java)
- Spring AI 2.0 `DefaultToolCallingManager`：
  [`DefaultToolCallingManager.java`](https://github.com/spring-projects/spring-ai/blob/main/spring-ai-model/src/main/java/org/springframework/ai/model/tool/DefaultToolCallingManager.java)
- Spring Blog：[Spring AI 2.0.0 GA Available Now](https://spring.io/blog/2026/06/12/spring-ai-2-0-0-GA-available-now/)
