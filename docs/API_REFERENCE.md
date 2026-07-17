# API 参考文档

[English](API_REFERENCE_EN.md) | 中文

本文档覆盖 BeamAI 核心库（`beamai_core` / `beamai_llm` / `beamai_agent`）的公开 API，按模块罗列函数签名、类型与关键示例。已迁移到 [beamai_extra](https://github.com/TTalkPro/beamai_extra) 的功能（Process Framework 流程编排、存储/快照引擎、Context Reducer、Deep Agent、Tools 库、RAG、A2A/MCP 协议等）不在本文档范围内。

## 目录

- [beamai_core - 核心模块](#beamai_core---核心模块)
  - [顶层 DSL：beamai](#顶层-dslbeamai)
  - [Kernel：beamai_kernel](#kernelbeamai_kernel)
  - [Context：beamai_context](#contextbeamai_context)
  - [Tool：beamai_tool](#toolbeamai_tool)
  - [Filter 系统](#filter-系统)
    - [beamai_filter](#beamai_filter)
    - [beamai_filter_chain](#beamai_filter_chain)
    - [内置 Filter 库：beamai_filters](#内置-filter-库beamai_filters)
  - [Memory Filter：beamai_memory_filter](#memory-filterbeamai_memory_filter)
  - [会话存储后端](#会话存储后端)
    - [beamai_chat_memory_ets](#beamai_chat_memory_ets)
    - [beamai_chat_memory_dets](#beamai_chat_memory_dets)
  - [Token 流组装器：beamai_token_stream](#token-流组装器beamai_token_stream)
  - [Tool 索引与检索](#tool-索引与检索)
    - [beamai_tool_index](#beamai_tool_index)
    - [beamai_tool_search](#beamai_tool_search)
  - [JSON Schema：beamai_json_schema](#json-schemabeamai_json_schema)
  - [Prompt 模板：beamai_prompt](#prompt-模板beamai_prompt)
  - [Result 单子：beamai_result](#result-单子beamai_result)
  - [工具错误分类：beamai_tool_error](#工具错误分类beamai_tool_error)
  - [Agent 记忆 Provider：beamai_memory_provider_default](#agent-记忆-providerbeamai_memory_provider_default)
  - [LLM 响应：beamai_llm_response](#llm-响应beamai_llm_response)
  - [消息构建：beamai_message](#消息构建beamai_message)
  - [HTTP 客户端：beamai_http](#http-客户端beamai_http)
  - [工具模块](#工具模块)
    - [beamai_utils](#beamai_utils)
    - [beamai_sse](#beamai_sse)
    - [beamai_jsonrpc](#beamai_jsonrpc)
    - [beamai_id](#beamai_id)
- [beamai_llm - LLM 客户端](#beamai_llm---llm-客户端)
  - [Chat Completion：beamai_chat_completion](#chat-completionbeamai_chat_completion)
  - [错误结构：beamai_llm_error](#错误结构beamai_llm_error)
  - [HTTP 客户端：beamai_llm_http_client](#http-客户端beamai_llm_http_client)
  - [Output Parser](#output-parser)
    - [beamai_output_parser](#beamai_output_parser)
    - [beamai_parser_json](#beamai_parser_json)
    - [beamai_parser_instructions](#beamai_parser_instructions)
    - [beamai_parser_retry](#beamai_parser_retry)
  - [Provider 适配器](#provider-适配器)
    - [beamai_llm_response_parser](#beamai_llm_response_parser)
    - [beamai_llm_message_adapter](#beamai_llm_message_adapter)
    - [beamai_llm_tool_adapter](#beamai_llm_tool_adapter)
    - [beamai_llm_provider_common](#beamai_llm_provider_common)
  - [支持的 Provider](#支持的-provider)
- [beamai_agent - SimpleAgent（ReAct）](#beamai_agent---simpleagentreact)
  - [Agent：beamai_agent](#agentbeamai_agent)
  - [状态：beamai_agent_state](#状态beamai_agent_state)
  - [Tool Loop：beamai_agent_tool_loop](#tool-loopbeamai_agent_tool_loop)
  - [回调：beamai_agent_callbacks](#回调beamai_agent_callbacks)
  - [中断机制：beamai_agent_interrupt](#中断机制beamai_agent_interrupt)
  - [暂停持久化：beamai_agent_pause](#暂停持久化beamai_agent_pause)
  - [Agent 工具：beamai_agent_utils](#agent-工具beamai_agent_utils)
  - [子 Agent 委派：beamai_agent_delegate](#子-agent-委派beamai_agent_delegate)
  - [子 Agent 管理器：beamai_subagent_manager](#子-agent-管理器beamai_subagent_manager)
  - [Timeline 与多分支：beamai_timeline](#timeline-与多分支beamai_timeline)
  - [分支血缘存储](#分支血缘存储)
    - [beamai_branch_store](#beamai_branch_store)
    - [beamai_branch_store_ets](#beamai_branch_store_ets)
  - [暂停快照存储](#暂停快照存储)
    - [beamai_pause_store](#beamai_pause_store)
    - [beamai_pause_store_ets](#beamai_pause_store_ets)
- [Behaviour 接口](#behaviour-接口)
  - [beamai_chat_behaviour](#beamai_chat_behaviour)
  - [beamai_memory_provider](#beamai_memory_provider)
  - [beamai_chat_memory](#beamai_chat_memory)
  - [beamai_tool_behaviour](#beamai_tool_behaviour)
  - [beamai_http_behaviour](#beamai_http_behaviour)
- [错误处理](#错误处理)
- [更多文档](#更多文档)

---

## beamai_core - 核心模块

`beamai_core` 提供 Kernel 基座、Filter 洋葱系统、Tool/Context 抽象、HTTP 客户端与各类工具模块，是 BeamAI 最底层的依赖层。

### 顶层 DSL：beamai

Facade 入口，把 Kernel、Tool、Filter、Chat 调用的常见动作收拢到一个命名空间下。日常用法直接走这里；需要更细的控制再下钻到 `beamai_kernel` / `beamai_context` 等模块。

```erlang
%% 创建空 Kernel（默认配置，无 filter）
-spec kernel() -> beamai_kernel:kernel().

%% 创建 Kernel（自定义配置，无 filter）
-spec kernel(beamai_kernel:kernel_settings()) -> beamai_kernel:kernel().

%% 创建 Kernel（自定义配置 + 全量 filter；列表靠前 = 外层）
-spec kernel(beamai_kernel:kernel_settings(), [beamai_filter:filter()]) ->
    beamai_kernel:kernel().

%% 创建工具定义（名称 + 处理器）
-spec tool(binary(), beamai_tool:handler()) -> beamai_tool:tool_spec().

%% 创建工具定义（带 description / parameters / tag / timeout / retry 等）
-spec tool(binary(), beamai_tool:handler(), map()) -> beamai_tool:tool_spec().

%% 注册单个工具
-spec add_tool(beamai_kernel:kernel(), beamai_tool:tool_spec()) ->
    beamai_kernel:kernel().

%% 批量注册工具
-spec add_tools(beamai_kernel:kernel(), [beamai_tool:tool_spec()]) ->
    beamai_kernel:kernel().

%% 从模块自动加载工具（模块须实现 beamai_tool_behaviour）
-spec add_tool_module(beamai_kernel:kernel(), module()) ->
    beamai_kernel:kernel().

%% 通过 provider 原子与选项添加 LLM 服务
-spec add_llm(beamai_kernel:kernel(), beamai_chat_behaviour:provider(), map()) ->
    beamai_kernel:kernel().

%% 使用预构建的 LLM 配置添加服务
-spec add_llm(beamai_kernel:kernel(), beamai_chat_behaviour:config()) ->
    beamai_kernel:kernel().

%% 创建 filter（私有状态初值 #{}）
-spec filter(binary(), beamai_filter:hooks()) -> beamai_filter:filter().

%% 创建 filter（指定私有上下文初值）
-spec filter(binary(), beamai_filter:hooks(), beamai_filter:fctx()) ->
    beamai_filter:filter().

%% 调用 Kernel 中注册的工具（走 tool filter 链）
-spec invoke_tool(beamai_kernel:kernel(), binary(), beamai_tool:args(),
                  beamai_context:t()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.

%% 发送 Chat Completion 请求（不含工具调用循环；带循环请用 Agent）
-spec chat(beamai_kernel:kernel(), [map()]) ->
    {ok, map(), beamai_context:t()} | {error, term()}.

%% 发送 Chat Completion 请求（带 opts）
-spec chat(beamai_kernel:kernel(), [map()], beamai_kernel:chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.

%% 渲染 {{变量}} 模板
-spec render(binary(), map()) -> {ok, binary()} | {error, term()}.

%% 取所有工具的 tool schema（默认 OpenAI 格式）
-spec tools(beamai_kernel:kernel()) -> [map()].

%% 取所有工具的 tool schema（指定 provider 格式）
-spec tools(beamai_kernel:kernel(), openai | anthropic | atom()) -> [map()].

%% 按 tag 筛选工具
-spec tools_by_tag(beamai_kernel:kernel(), binary()) ->
    [beamai_tool:tool_spec()].

%% 创建空执行上下文
-spec context() -> beamai_context:t().

%% 创建带初始 env 变量的执行上下文
-spec context(map()) -> beamai_context:t().
```

典型用法（Kernel + Tool + 一个 chat filter）：

```erlang
LLM = beamai_chat_completion:create(zhipu, #{
    model => <<"glm-4.6">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY"))
}),

System = beamai:filter(<<"system">>, #{
    around_chat => fun(#{messages := Msgs} = Req, _FCtx, Next) ->
        Sys = #{role => system, content => <<"请用简洁的中文回答。"/utf8>>},
        Next(Req#{messages => [Sys | Msgs]})
    end
}),

K = beamai:add_llm(beamai:kernel(#{}, [System]), LLM),

{ok, Resp, _Ctx} = beamai:chat(K, [
    #{role => user, content => <<"你好！"/utf8>>}
]).
```

### Kernel：beamai_kernel

Kernel 是基础设施层，负责工具注册、LLM 服务配置、Filter 编排。Kernel **本身无状态**，不记录消息；会话历史由 Memory Filter + store 承担（详见 [MEMORY.md](MEMORY.md)）。

```erlang
%% 创建空 Kernel
-spec new() -> kernel().

%% 创建 Kernel（自定义配置）
-spec new(kernel_settings()) -> kernel().

%% 创建 Kernel（配置 + 一次性给出全量 filter）
-spec new(kernel_settings(), [beamai_filter:filter()]) -> kernel().

%% 注册工具
-spec add_tool(kernel(), beamai_tool:tool_spec()) -> kernel().

%% 批量注册工具
-spec add_tools(kernel(), [beamai_tool:tool_spec()]) -> kernel().

%% 从模块自动加载并注册工具
-spec add_tool_module(kernel(), module()) -> kernel().

%% 设置 LLM 服务配置
-spec add_service(kernel(), beamai_chat_behaviour:config()) -> kernel().

%% 调用 Kernel 中注册的工具
-spec invoke_tool(kernel(), binary(), beamai_tool:args(),
                  beamai_context:t()) ->
    {ok, term(), beamai_context:writes()} | {error, term()}.

%% 发送 Chat Completion 请求（不含工具调用循环；带循环请用 Agent）
-spec invoke_chat(kernel(), [map()], chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.

%% 流式 Chat Completion（经完整 around_chat 链；sink 收到 TokenCallback）
-spec invoke_chat_stream(kernel(), [map()], chat_opts(),
                         fun((binary(), map()) -> ok)) ->
    {ok, map(), beamai_context:t()} | {error, term()}.

%% 按名称查找工具
-spec get_tool(kernel(), binary()) -> {ok, beamai_tool:tool_spec()} | error.

%% 列出 Kernel 中所有注册的工具
-spec list_tools(kernel()) -> [beamai_tool:tool_spec()].

%% 按 tag 查找工具
-spec get_tools_by_tag(kernel(), binary()) -> [beamai_tool:tool_spec()].

%% 取所有工具的统一 tool spec 列表
-spec get_tool_specs(kernel()) -> [map()].

%% 取所有工具的 tool schema（默认 OpenAI 格式）
-spec get_tool_schemas(kernel()) -> [map()].

%% 取所有工具的 tool schema（指定 provider 格式）
-spec get_tool_schemas(kernel(), openai | anthropic | atom()) -> [map()].

%% 取 LLM 服务配置
-spec get_service(kernel()) -> {ok, beamai_chat_behaviour:config()} | error.

%% 取 Kernel 的状态槽声明
-spec state_slots(kernel()) -> beamai_context:state_slots().

%% 按工具名查该工具是否标记为串行
-spec serial_tool(kernel(), binary()) -> boolean().

%% 按工具名查该工具结果是否直接作为最终答案
-spec return_direct_tool(kernel(), binary()) -> boolean().
```

类型：

```erlang
-type kernel() :: #{
    '__kernel__' := true,
    tools := #{binary() => beamai_tool:tool_spec()},
    llm_config := beamai_chat_behaviour:config() | undefined,
    filters := [beamai_filter:filter()],
    settings := kernel_settings()
}.

-type kernel_settings() :: #{
    default_timeout => pos_integer(),
    atom() => term()
}.

-type chat_opts() :: #{
    tools => [map()],
    tool_choice => auto | none | required,
    context => beamai_context:t(),
    system_prompts => [map()],
    atom() => term()
}.
```

### Context：beamai_context

执行上下文，三分区：`env`（运行环境变量）、`state`（用户状态）、`filter_states`（filter 私有上下文，按 filter 名隔离）。Kernel 与工具执行都在同一份 context 上读写。

```erlang
%% 创建空的执行上下文
-spec new() -> t().

%% 创建带初始 env 变量的执行上下文
-spec new(map()) -> t().

%% 读 env 变量（不存在返回 undefined）
-spec get(t(), atom() | binary()) -> term() | undefined.

%% 读 env 变量（带默认值）
-spec get(t(), atom() | binary(), term()) -> term().

%% 取全部 env 变量 map
-spec variables(t()) -> #{binary() => term()}.

%% 注入单个 env 变量
-spec set(t(), atom() | binary(), term()) -> t().

%% 批量注入 env 变量
-spec set_many(t(), map()) -> t().

%% 关联会话标识（与 Memory Filter 配合）
-spec with_conversation_id(t(), binary()) -> t().

%% 取会话标识
-spec conversation_id(t()) -> binary() | undefined.

%% 关联 Kernel 引用（filter 可读 Kernel）
-spec with_kernel(t(), term()) -> t().

%% 取 Kernel 引用
-spec get_kernel(t()) -> term() | undefined.

%% 读状态槽（不存在返回 undefined）
-spec state_get(t(), atom() | binary()) -> term() | undefined.

%% 读状态槽（带默认值）
-spec state_get(t(), atom() | binary(), term()) -> term().

%% 取整个 state 分区
-spec get_state(t()) -> #{binary() => term()}.

%% 整体替换 state 分区
-spec with_state(t(), #{binary() => term()}) -> t().

%% 按 tool_call 原始序折叠一批工具的 writes 进 state
-spec apply_writes(t(), [{pos_integer(), writes()}], state_slots()) ->
    {t(), [binary()]}.

%% 读某 filter 的私有上下文（缺省返回 Default）
-spec filter_state(t(), binary(), map()) -> map().

%% 写回某 filter 的私有上下文
-spec set_filter_state(t(), binary(), map()) -> t().

%% 标准化 key：atom 统一转 binary
-spec normalize_key(atom() | binary()) -> binary().

%% 列出所有 env 变量 key
-spec keys(t()) -> [binary()].

%% 删除 env 变量
-spec delete(t(), atom() | binary()) -> t().

%% 检查 env 变量是否存在
-spec has_key(t(), atom() | binary()) -> boolean().

%% 用函数更新 env 变量值
-spec update(t(), atom() | binary(), fun((term()) -> term())) -> t().
```

类型：

```erlang
-type t() :: #{
    '__context__' := true,
    env := env(),
    state := #{binary() => term()},
    '__filter_states__' := #{binary() => map()}
}.

-type env() :: #{
    kernel := term() | undefined,
    conversation_id := binary() | undefined,
    vars := #{binary() => term()}
}.

-type writes() :: #{binary() | atom() => term()}.

-type state_slots() :: #{
    binary() => #{init => term(), reduce => fun((term(), term()) -> term())}
}.

-type message() :: beamai_message:message().
-type tool_call() :: beamai_message:tool_call().
```

### Tool：beamai_tool

工具定义：处理器、参数 schema、超时、重试、tag、过滤器、返回直接标记。Tool 是 Kernel 调用的最小单位。

```erlang
%% 创建工具定义（最小形式）
-spec new(binary(), handler()) -> tool_spec().

%% 创建工具定义（带 description / parameters / tag / timeout / retry 等）
-spec new(binary(), handler(), map()) -> tool_spec().

%% 验证工具定义的合法性
-spec validate(tool_spec()) -> ok | {error, [term()]}.

%% 调用工具（空上下文）
-spec invoke(tool_spec(), args()) -> tool_result().

%% 调用工具（带上下文）
-spec invoke(tool_spec(), args(), beamai_context:t()) -> tool_result().

%% 转成统一 tool spec 格式
-spec to_tool_spec(tool_spec()) -> map().

%% 转成 OpenAI 格式 tool schema（默认）
-spec to_tool_schema(tool_spec()) -> map().

%% 转成指定 provider 格式 tool schema
-spec to_tool_schema(tool_spec(), openai | anthropic | atom()) -> map().

%% 取工具名称
-spec get_name(tool_spec()) -> binary().

%% 取工具 tag
-spec get_tag(tool_spec()) -> binary() | [binary()] | undefined.

%% 检查工具是否包含指定 tag
-spec has_tag(tool_spec(), binary()) -> boolean().

%% 是否标记为串行
-spec is_serial(tool_spec()) -> boolean().

%% 是否标记为敏感
-spec is_sensitive(tool_spec()) -> boolean().

%% 工具结果是否直接作为最终答案
-spec is_return_direct(tool_spec()) -> boolean().

%% 解析 LLM 返回的 tool_call 结构 → {Name, Id, Args}
-spec parse_tool_call(map()) -> {binary(), binary(), map()}.

%% 把工具执行结果编码为 LLM 可读的二进制
-spec encode_result(term()) -> binary().

%% 从模块自动加载工具列表（模块须实现 beamai_tool_behaviour）
-spec from_module(module()) -> {ok, [tool_spec()]} | {error, term()}.
```

类型：

```erlang
-type tool_spec() :: #{
    name := binary(),
    handler := handler(),
    description => binary(),
    parameters => parameters_schema(),
    tag => binary() | [binary()],
    timeout => pos_integer(),
    retry => boolean()
            | #{max_retries => non_neg_integer(), initial_delay_ms => non_neg_integer()}
            | #{max => integer(), delay => integer()},
    serial => boolean(),
    sensitive => boolean(),
    return_direct => boolean(),
    filters => [filter_ref()],
    metadata => map()
}.

-type handler() ::
    fun((args()) -> tool_result())
    | fun((args(), beamai_context:t()) -> tool_result())
    | {module(), atom()}
    | {module(), atom(), [term()]}.

-type tool_result() ::
    {ok, term()}
    | {ok, term(), beamai_context:writes()}
    | {error, term()}.

-type args() :: map().

-type parameters_schema() :: #{
    atom() | binary() => param_spec()
}.

-type param_spec() :: #{
    type := string | integer | float | boolean | array | object,
    description => binary(),
    required => boolean(),
    default => term(),
    enum => [term()],
    items => param_spec(),
    properties => parameters_schema()
}.
```

### Filter 系统

Filter 是 BeamAI 的**洋葱式（onion）拦截器**。采用通用 around（环绕）形态：一个闭包同时承担「前置 → 调内层 → 后置」三段逻辑，前置/后置用闭包局部变量桥接，短路只需「不调内层」。完整设计参见 [FILTER.md](FILTER.md)。

#### beamai_filter

Filter 记录与四个钩子类型。**核心钩子**：3 个 around（`around_chat` / `around_tool` / `around_turn`）+ 流式专用的 `token_transform`（token 流变换，不走洋葱）。

```erlang
%% 创建 filter（私有状态初值 #{}）
-spec new(binary(), hooks()) -> filter().

%% 创建 filter（指定私有状态初值）
-spec new(binary(), hooks(), fctx()) -> filter().

%% 取 filter 的某个 hook（不存在返回 undefined）
-spec hook(filter(), hook_type()) -> around_fun() | token_transform() | undefined.

%% 取 filter 的私有上下文初值
-spec init(filter()) -> fctx().
```

类型（**重点理解**）：

```erlang
%% 钩子类型
-type hook_type() :: around_chat | around_tool | around_turn | token_transform.

%% 一个 filter 可同时挂任意子集
-type hooks() :: #{
    around_chat => around_fun(),
    around_tool => around_fun(),
    around_turn => around_fun(),
    token_transform => token_transform()
}.

%% 三链 around 形态：闭包，前置/后置同处一处
-type around_fun() ::
    fun((request(), fctx(), next()) ->
        response() | {response(), fctx()}).

%% 第四钩子：token 流变换（1→N，跨 chunk 状态；非洋葱）
-type token_transform() :: #{
    init => term(),
    step := fun((token_data(), State :: term()) ->
        {[token_data()], NewState :: term()}),
    flush => fun((State :: term()) -> [token_data()])
}.

%% token 数据形态（送往 on-token sink 的最小单位）
-type token_data() :: #{token := binary(), meta := map()}.

%% filter 记录本身
-type filter() :: #{
    '__filter__' := true,
    name := binary(),
    hooks := hooks(),
    init := fctx()
}.
```

四条钩子的分工：

| 钩子 | 含义 | 形态 |
|------|------|------|
| `around_chat` | 环绕一次 LLM 调用 | `fun(Req, FCtx, Next) -> Resp \| {Resp, NewFCtx}` |
| `around_tool` | 环绕一次工具执行 | 同上 |
| `around_turn` | 环绕整个工具循环（Agent 层每 turn 一次） | 同上；`Resp` 是工具循环结果 tuple |
| `token_transform` | 改送往 sink 的 token 流（非洋葱） | `step/flush` 形态 |

filter 列表位置决定洋葱层序：靠前 = 外层（前置先执行、后置后执行）。Memory Filter 必须放**首位**（最外层，先展开完整历史）。

#### beamai_filter_chain

洋葱链合成与运行。最内层是 **terminal**（真正的 LLM 调用或工具执行）。

```erlang
%% 运行某条链的 filter 洋葱
%% Phase 指定该链用哪个 around hook：chat 链传 around_chat，tool 链传 around_tool
%% Terminal 产出最内层响应，run/4 用 try/catch 统一返回 {ok, Response} | {error, Reason}
-spec run([beamai_filter:filter()], phase(), terminal(), request()) ->
    {ok, response()} | {error, term()}.

%% 把 filter 列表与 terminal 合成为单个洋葱函数
-spec compose([beamai_filter:filter()], phase(), terminal()) ->
    fun((request()) -> response()).
```

#### 内置 Filter 库：beamai_filters

`beamai_filters` 提供一组现成的 filter 构造器，纯构造器，建 kernel 时放进 `new/2` 的 filters 列表。

```erlang
%% 三链日志（放首位记全景；放在某 filter 之后则只看那层之内的改写）
-spec logging_filter() -> beamai_filter:filter().

%% tool 链超时：单个工具执行超过 Ms 毫秒则超时短路
-spec timeout_filter(pos_integer()) -> beamai_filter:filter().

%% tool 链审批：ApproveFun 返回 false 则拒绝短路（非交互）
-spec approval_filter(fun((binary(), map()) -> boolean())) ->
    beamai_filter:filter().

%% chat 链敏感词拦截（命中即短路）
-spec safeguard_filter([binary()]) -> beamai_filter:filter().

%% chat 链敏感词拦截，带选项（failure_response / case_sensitive）
-spec safeguard_filter([binary()], map()) -> beamai_filter:filter().

%% turn 链最终答案校验
-spec validation_turn_filter(
    fun((map()) -> ok | {invalid, term()}),
    non_neg_integer()) ->
    beamai_filter:filter().

%% turn 链结构化输出 JSON Schema 校验
-spec schema_validation_turn_filter(
    beamai_json_schema:schema(),
    non_neg_integer()) ->
    beamai_filter:filter().

%% turn 链结构化输出 JSON Schema 校验（带选项：max_errors / code_fence）
-spec schema_validation_turn_filter(
    beamai_json_schema:schema(),
    non_neg_integer(),
    map()) ->
    beamai_filter:filter().

%% token 流脱敏（正则替换）
-spec token_redact_filter(iodata(), binary()) -> beamai_filter:filter().

%% token 流先审后放（缓冲整流不外泄，完流时全文审查）
-spec hold_release_filter(fun((binary()) -> ok | {block, binary()})) ->
    beamai_filter:filter().
```

最小用法示例（同时挂 chat 与 tool 上的一个 around）：

```erlang
K = beamai:kernel(#{}, [
    beamai_filters:logging_filter(),
    beamai_filters:timeout_filter(5000)
]).
```

### Memory Filter：beamai_memory_filter

会话历史的存储与注入。Kernel 本身不记录消息；每次 invoke 只传单条最新消息，Memory Filter 的 `around_chat` 前置存 delta 并把 store 中的完整历史替换 messages（按 `conversation_id`），后置把 assistant 回复存入 store。**必须放 filters 列表首位**（最外层）。

```erlang
%% 构造绑定 store 的 memory filter
-spec memory_filter(beamai_chat_memory:handle()) -> beamai_filter:filter().
```

```erlang
{ok, _} = beamai_chat_memory_ets:start_link(my_mem),
Store = beamai_chat_memory_ets:handle(my_mem),

K = beamai:add_llm(
      beamai:kernel(#{}, [beamai_memory_filter:memory_filter(Store)]),
      LLM),

Ctx = beamai_context:with_conversation_id(beamai_context:new(),
                                          <<"session-1">>),

{ok, _, _} = beamai:chat(K, [
    #{role => user, content => <<"我叫张三"/utf8>>}
], #{context => Ctx}),
%% 同一 conversation_id 第二轮 LLM 能看到完整历史
{ok, _, _} = beamai:chat(K, [
    #{role => user, content => <<"我叫什么？"/utf8>>}
], #{context => Ctx}).
```

详见 [MEMORY.md](MEMORY.md)。

### 会话存储后端

`beamai_chat_memory_ets` / `beamai_chat_memory_dets` 实现 `beamai_chat_memory` behaviour，对 Memory Filter 透明。句柄约定 `{Module, Ref}`，调度 API（`mem_get/2` / `mem_add/3` / `mem_clear/2`）解包转发。

#### beamai_chat_memory_ets

默认 ETS 内存实现，进程持有 ETS 表。

```erlang
%% 启动 ETS 会话存储
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.

%% 启动 ETS 会话存储（带选项）
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.

%% 停止会话存储
-spec stop(atom()) -> ok.

%% 构造 beamai_chat_memory 句柄
-spec handle(atom()) -> beamai_chat_memory:handle().

%% 取指定会话的完整消息历史
-spec mem_get(atom(), binary()) -> [message()].

%% 追加消息到指定会话末尾
-spec mem_add(atom(), binary(), [message()]) -> ok.

%% 清空指定会话
-spec mem_clear(atom(), binary()) -> ok.
```

#### beamai_chat_memory_dets

DETS 持久化实现，进程/节点重启后用同一 `file` 重新 `start_link` 即恢复历史（每次写操作后 `dets:sync` 落盘）。

```erlang
%% 启动 DETS 会话存储
-spec start_link(atom(), #{file := file:name_all(), _ => _}) ->
    {ok, pid()} | {error, term()}.

%% 停止会话存储
-spec stop(atom()) -> ok.

%% 构造 beamai_chat_memory 句柄
-spec handle(atom()) -> beamai_chat_memory:handle().

%% 取指定会话的完整消息历史
-spec mem_get(atom(), binary()) -> [message()].

%% 追加消息到指定会话末尾
-spec mem_add(atom(), binary(), [message()]) -> ok.

%% 清空指定会话
-spec mem_clear(atom(), binary()) -> ok.
```

```erlang
{ok, _} = beamai_chat_memory_dets:start_link(
    my_mem, #{file => "data/chat.dets"}),
Store = beamai_chat_memory_dets:handle(my_mem).
%% 用法与 ETS 后端完全一致
```

### Token 流组装器：beamai_token_stream

把多个 `token_transform` 与一个 sink 合成 `{TokenCallback, Flush}`。`TokenCallback` 接收 `(Token, Meta)`；`Flush` 关闭本次流，触发各层 `flush` 冲残留。

```erlang
%% 组装为 {TokenCallback, Flush}
-spec wrap([beamai_filter:token_transform()], sink()) ->
    {fun((binary(), map()) -> ok), fun(() -> ok)}.
```

`token_transform` 是流式专用的第四钩子，与三条 around 链相互独立：transform 只改「sink 看到的出站流」，不改「答案本身」。同步路径（`invoke_chat`）完全忽略 `token_transform`；无 token_transform filter 时流式路径零开销退化（`TokenCallback` 原样直通）。

### Tool 索引与检索

大工具集按需揭示：全量注册但首轮只广播检索工具，模型想干活就先描述需求检索下一轮才看得见对应工具。完整设计与选项参见 [FILTER.md](FILTER.md) 的「工具检索」一节。

#### beamai_tool_index

工具索引 Behaviour，可自定义后端（向量等由 beamai_extra 接）。默认实现见 `apps/beamai_core` 内的具体索引模块。

```erlang
%% 构建索引句柄
-spec new(module(), [beamai_tool:tool_spec()], map()) -> handle().

%% 按查询检索工具名（最佳匹配在前，至多 MaxResults 个）
-spec search(handle(), binary(), pos_integer()) -> [binary()].

%% 计算工具集指纹（SHA-256）
-spec fingerprint([beamai_tool:tool_spec()]) -> binary().
```

类型：

```erlang
-opaque handle() :: {module(), State :: term()}.
```

Behaviour 回调：

```erlang
-callback build(Tools :: [beamai_tool:tool_spec()], Opts :: map()) ->
    State :: term().
-callback search(Query :: binary(), MaxResults :: pos_integer(),
                 State :: term()) -> [binary()].
```

#### beamai_tool_search

高层检索组件，封装「检索工具 + 索引 filter」。

```erlang
%% 建检索组件：返回 {检索工具, filter}
-spec new([beamai_tool:tool_spec()]) ->
    {beamai_tool:tool_spec(), beamai_filter:filter()}.

%% 建检索组件（带选项）
-spec new([beamai_tool:tool_spec()], opts()) ->
    {beamai_tool:tool_spec(), beamai_filter:filter()}.

%% 缺省的系统提示补充文本（需要时可自行拼进 system_prompt）
-spec default_system_suffix() -> binary().
```

### JSON Schema：beamai_json_schema

零依赖 JSON Schema 子集校验器（DRAFT 2020-12 的实用子集；不支持 `$ref` / `$defs` / `patternProperties` / `if-then-else` / `format`）。

```erlang
%% 校验实例是否符合 schema（收集全部错误）
-spec validate(schema(), term()) -> ok | {error, [error()]}.

%% 校验实例（带选项，如 max_errors）
-spec validate(schema(), term(), opts()) -> ok | {error, [error()]}.

%% 把错误列表拼成一条人类可读消息
-spec error_message([error()]) -> binary().
```

类型：

```erlang
-type schema() :: map() | boolean().
-type error() :: #{
    path := binary(),
    keyword := atom(),
    message := binary()
}.
-type opts() :: #{max_errors => pos_integer()}.
```

### Prompt 模板：beamai_prompt

`{{变量}}` 替换的简易模板渲染。

```erlang
%% 创建提示词模板
-spec new(binary()) -> prompt_template().

%% 渲染模板（map 或 beamai_context:t 都可作为变量来源）
-spec render(prompt_template(), map() | beamai_context:t()) ->
    {ok, binary()} | {error, term()}.

%% 取模板中声明的变量名列表
-spec get_variables(prompt_template()) -> [binary()].
```

```erlang
-type prompt_template() :: #{
    template := binary(),
    input_variables := [binary()]
}.
```

### Result 单子：beamai_result

错误处理的 Result 单子，`{ok, T}` / `{error, E}` 形态的链式组合工具集。

```erlang
%% 构造 ok
-spec ok(T) -> {ok, T}.

%% 构造 error
-spec error(E) -> {error, E}.

%% 判断是否为 ok
-spec is_ok(result(term())) -> boolean().

%% 判断是否为 error
-spec is_error(result(term())) -> boolean().

%% 如果是 ok 则应用函数
-spec map(result(A), fun((A) -> B)) -> result(B).

%% flat_map / bind 同义：应用返回 result 的函数并展平
-spec flat_map(result(A), fun((A) -> result(B))) -> result(B).
-spec bind(result(A), fun((A) -> result(B))) -> result(B).

%% 将值通过函数列表传递
-spec pipe(A, [fun((A) -> result(B)) | fun((A) -> B)]) -> result(B).

%% 当谓词返回 true 时继续传递
-spec pipe_while(A, [{fun((A) -> boolean()), fun((A) -> result(A))}]) ->
    result(A).

%% 获取值，错误则抛异常
-spec unwrap(result(T)) -> T.

%% 获取值，错误则返回默认值
-spec unwrap_or(result(T), T) -> T.

%% 获取错误原因
-spec unwrap_error(result(term(), E)) -> E.

%% 将布尔值转换为 result
-spec from_boolean(boolean(), E) -> result(ok, E).

%% 将可能为 undefined 的值转换为 result
-spec from_maybe(T | undefined, E) -> result(T, E).

%% 将 result 列表收集为列表的 result
-spec collect([result(T)]) -> result([T]).

%% 将结果列表分区为成功和错误两部分
-spec partition([result(T, E)]) -> {[T], [E]}.

%% 对 ok 值执行副作用函数
-spec tap(result(T), fun((T) -> term())) -> result(T).

%% 对错误执行副作用函数
-spec tap_error(result(T, E), fun((E) -> term())) -> result(T, E).
```

类型：

```erlang
-type result(T) :: {ok, T} | {error, term()}.
-type result(T, E) :: {ok, T} | {error, E}.
```

### 工具错误分类：beamai_tool_error

把工具执行抛出的 Reason 归类为「语义 / 瞬态 / 环境」三类，便于上层做重试 / 退避 / 上报分流。

```erlang
%% 把工具错误 Reason 分类为 semantic | transient | environment
-spec classify(term()) -> class().

%% 从错误 Reason 提取人类可读消息
-spec message(term()) -> binary().
```

```erlang
-type class() :: semantic | transient | environment.
```

### Agent 记忆 Provider：beamai_memory_provider_default

默认 Agent 记忆 Provider，包一个存储后端，可选滑动窗口（最近 N 条非系统消息）。

```erlang
%% 无窗口（无界）默认 provider
-spec new(store()) -> beamai_memory_provider:provider().

%% 带 N 条滑动窗口的默认 provider
-spec new(store(), pos_integer()) -> beamai_memory_provider:provider().
```

### LLM 响应：beamai_llm_response

LLM 统一响应结构。所有 Provider（OpenAI / Anthropic / DeepSeek / Zhipu / DashScope / Ollama）都通过适配层归一化为这套结构，Agent 与上层只面向它编程。

```erlang
%% 从字段 map 构造标准 response() 结构
-spec new(map()) -> response().

%% 取响应 ID
-spec id(response()) -> binary().

%% 取模型名称
-spec model(response()) -> binary().

%% 取 Provider 类型
-spec provider(response()) -> provider().

%% 取文本内容（合并后的）
-spec content(response()) -> binary() | null.

%% 取原始内容块列表
-spec content_blocks(response()) -> [content_block()].

%% 取 thinking 内容
-spec thinking(response()) -> binary() | null.

%% 取推理内容（智谱 GLM-4.6+ 特有）
-spec reasoning_content(response()) -> binary() | null.

%% 取工具调用列表
-spec tool_calls(response()) -> [tool_call()].

%% 是否有工具调用
-spec has_tool_calls(response()) -> boolean().

%% 取统一的结束原因
-spec finish_reason(response()) -> finish_reason().

%% 是否正常完成
-spec is_complete(response()) -> boolean().

%% 是否需要执行工具调用
-spec needs_tool_call(response()) -> boolean().

%% 取 Token 使用统计
-spec usage(response()) -> usage().

%% 取输入 Token 数
-spec input_tokens(response()) -> non_neg_integer().

%% 取输出 Token 数
-spec output_tokens(response()) -> non_neg_integer().

%% 取总 Token 数
-spec total_tokens(response()) -> non_neg_integer().

%% 取原始响应数据
-spec raw(response()) -> map().

%% 从原始数据中获取指定路径的值
-spec raw_get(response(), [binary()] | binary()) -> term() | undefined.

%% 从原始数据中获取指定路径的值（带默认值）
-spec raw_get(response(), [binary()] | binary(), term()) -> term().

%% 取元数据
-spec metadata(response()) -> map().

%% 设置元数据字段
-spec set_metadata(response(), term(), term()) -> response().

%% 转换为普通 map
-spec to_map(response()) -> map().
```

类型：

```erlang
-type response() :: #{
    '__struct__' := ?MODULE,
    id := binary(),
    model := binary(),
    provider := provider(),
    content := binary() | null,
    content_blocks := [content_block()],
    tool_calls := [tool_call()],
    finish_reason := finish_reason(),
    usage := usage(),
    raw := map(),
    metadata := map()
}.

-type provider() :: openai | anthropic | deepseek | zhipu | ollama | dashscope | unknown.

%% 内容块四种类型
-type content_block() ::
    #{type := text, text := binary()} |
    #{type := thinking, thinking := binary(), signature := binary()} |
    #{type := redacted_thinking, data := binary()} |
    #{type := tool_use, id := binary(), name := binary(), input := map()}.

-type tool_call() :: #{
    id := binary(),
    name := binary(),
    arguments := map(),
    raw_arguments := binary()
}.

-type finish_reason() ::
    complete | tool_use | length_limit | content_filtered |
    stop_sequence | pause_turn | refusal | error | unknown.

-type usage() :: #{
    input_tokens := non_neg_integer(),
    output_tokens := non_neg_integer(),
    total_tokens := non_neg_integer(),
    details => map()
}.
```

### 消息构建：beamai_message

LLM 消息构建与基础访问器。中性消息形态，按 role 构造统一结构。

```erlang
%% 构建 system 消息
-spec system(term()) -> message().

%% 构建 user 消息
-spec user(term()) -> message().

%% 构建 assistant 消息
-spec assistant(term()) -> message().

%% 构建 assistant tool_calls 消息
-spec tool_calls([tool_call()]) -> message().

%% 构建 tool result 消息
-spec tool_result(binary(), binary(), term()) -> message().

%% 给消息附加 provider 原生内容块
-spec with_content_blocks(message(), [beamai_llm_response:content_block()]) ->
    message().

%% 把 LLM 响应转为中性 assistant 消息
-spec from_response(term()) -> message() | undefined.

%% 取消息角色
-spec role(message()) -> atom().

%% 取消息内容
-spec content(message()) -> binary() | null.

%% 取 tool_calls 列表
-spec get_tool_calls(message()) -> [tool_call()].

%% 取 tool_call_id
-spec tool_call_id(message()) -> binary() | undefined.

%% 取 tool name
-spec name(message()) -> binary() | undefined.

%% 取 provider 原生内容块
-spec content_blocks(message()) -> [beamai_llm_response:content_block()].

%% 判断是否为合法消息
-spec is_message(term()) -> boolean().

%% 判断消息是否为指定角色
-spec is_role(message(), atom()) -> boolean().

%% 从 tuple 列表快速构建消息列表
-spec messages([{atom(), term()}]) -> [message()].
```

类型：

```erlang
-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    content_blocks => [beamai_llm_response:content_block()],
    tool_call_id => binary(),
    name => binary()
}.

-type tool_call() :: #{
    id := binary(),
    type := function,
    function := #{
        name := binary(),
        arguments := binary() | map()
    }
}.
```

### HTTP 客户端：beamai_http

统一 HTTP 客户端接口。backend 经 `beamai_http_behaviour` 可插拔，内置实现只有
`beamai_http_gun`（HTTP/2）即默认值；`set_backend/1` 主要供测试替换后端使用。

```erlang
%% 简单 GET
-spec get(url()) -> response().

%% 带参数 GET
-spec get(url(), params()) -> response().

%% 带参数 + 选项 GET
-spec get(url(), params(), options()) -> response().

%% POST 请求
-spec post(url(), binary(), body()) -> response().

%% POST 请求（带选项）
-spec post(url(), binary(), body(), options()) -> response().

%% JSON POST 请求
-spec post_json(url(), map() | list()) -> response().

%% JSON POST 请求（带选项）
-spec post_json(url(), map() | list(), options()) -> response().

%% PUT 请求
-spec put(url(), binary(), body()) -> response().

%% PUT 请求（带选项）
-spec put(url(), binary(), body(), options()) -> response().

%% 简单 DELETE
-spec delete(url()) -> response().

%% 带参数 DELETE
-spec delete(url(), params()) -> response().

%% 带参数 + 选项 DELETE
-spec delete(url(), params(), options()) -> response().

%% 通用 HTTP 请求
-spec request(atom(), url(), headers(), body(), options()) -> response().

%% 通用 HTTP 请求（返回响应元信息）
-spec request_meta(atom(), url(), headers(), body(), options()) ->
    {ok, body(), beamai_http_behaviour:meta()} | {error, term()}.

%% 流式请求
-spec stream_request(atom(), url(), headers(), body(), options()) ->
    stream_response().

%% 流式请求（带自定义 chunk handler）
-spec stream_request(atom(), url(), headers(), body(), options(),
                     chunk_handler()) -> stream_response().

%% URL 编码
-spec url_encode(term()) -> binary().

%% 构建带参数的 URL
-spec build_url(url(), params()) -> binary().

%% 确保 HTTP 客户端已启动
-spec ensure_started() -> ok.

%% 取当前 HTTP backend
-spec get_backend() -> module().

%% 设置 HTTP backend（内置只有 beamai_http_gun；亦可传实现了
%% beamai_http_behaviour 的自定义/测试后端）
-spec set_backend(module()) -> ok.
```

HTTP backend 配置（sys.config）。Gun 后端使用三个用途分池
（`http_pool_short` 短请求 / `http_pool_stream` SSE 流式 /
`http_pool_longpoll` 异步长轮询），经 `http_pools` 按池配置，
只需给出要覆盖的池和键。详见 [HTTP.md](HTTP.md)：

```erlang
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pools, #{
        http_pool_stream   => #{max_connections_per_host => 20,
                                idle_timeout => 120000},
        http_pool_longpoll => #{idle_timeout => 300000}
    }}
]}.
```

遗留的 `http_pool` 键仍兼容（统一应用到三个池，启动时打废弃警告；
旧键名 `connection_timeout` 自动归一化为 `connect_timeout`）。

Gun 后端特性：

- **HTTP/2** —— 按池经 `protocols` 显式启用，见 [HTTP.md](HTTP.md)
- **连接池** —— 内置三个用途分池（`beamai_http_pool` 实例）
- **TLS** —— 自动使用系统 CA 证书（OTP 25+）

### 工具模块

#### beamai_utils

Agent 与各模块共用的通用工具函数（时间戳、二进制、JSON 编码、安全执行等）。

```erlang
%% 当前时间戳（毫秒）
-spec timestamp() -> timestamp().

%% 当前时间戳（秒）
-spec timestamp_seconds() -> pos_integer().

%% 安全获取 Map 中的值
-spec safe_get(map(), term(), term()) -> term().

%% 安全合并两个 Map
-spec safe_merge(map(), map()) -> map().

%% 列表分页
-spec paginate(list(), non_neg_integer(), pos_integer()) -> list().

%% 按时间范围过滤
-spec filter_by_time([map()], timestamp(), timestamp()) -> [map()].

%% 验证是否为有效的 Binary
-spec validate_binary(term()) -> boolean().

%% 验证是否为有效的 Map
-spec validate_map(term()) -> boolean().

%% 连接 Binary 列表
-spec binary_join(binary(), [binary()]) -> binary().

%% 转换为 Binary
-spec to_binary(term()) -> binary().

%% 确保值为 Binary（宽松版本）
-spec ensure_binary(term()) -> binary().

%% 编码 HTTP 请求体为二进制
-spec encode_body(binary() | map() | list() | term()) -> binary().

%% 尝试将 HTTP 响应体解码为 JSON
-spec decode_json_response(binary() | term()) -> map() | binary() | term().

%% 安全解析 JSON
-spec parse_json(binary() | map()) -> map().

%% 安全执行无参函数
-spec safe_execute(fun(() -> T)) -> {ok, T} | {error, {atom(), term()}}.

%% 安全执行单参函数
-spec safe_execute(fun((A) -> T), A) -> {ok, T} | {error, {atom(), term()}}.

%% 格式化错误为 Binary
-spec format_error(term()) -> binary().

%% 格式化错误为 Binary（带前缀）
-spec format_error(binary(), term()) -> binary().
```

#### beamai_sse

SSE 解析与编码。

```erlang
%% 解析 SSE 数据流 → {剩余 buffer, 事件列表}
-spec parse(binary()) -> {binary(), [map()]}.

%% 编码 SSE 事件（无 ID）
-spec encode_event(binary(), binary() | term()) -> binary().

%% 编码 SSE 事件（带 ID）
-spec encode_event(binary(), binary() | term(), binary()) -> binary().
```

#### beamai_jsonrpc

JSON-RPC 2.0 编解码与标准错误构造。

```erlang
%% 编码
-spec encode_request(term(), binary(), map()) -> binary().
-spec encode_notification(binary(), map()) -> binary().
-spec encode_response(term(), term()) -> binary().
-spec encode_error(term(), integer(), binary()) -> binary().
-spec encode_error(term(), integer(), binary(), term()) -> binary().

%% 解码
-spec decode(binary()) -> {ok, map() | {batch, [map()]}} | {error, term()}.
-spec decode_request(binary()) ->
    {ok, {term(), binary(), map()}} | {error, term()}.

%% 类型检查
-spec is_request(map()) -> boolean().
-spec is_notification(map()) -> boolean().
-spec is_response(map()) -> boolean().
-spec is_error(map()) -> boolean().
-spec is_batch(term()) -> boolean().

%% 标准错误构造（code 在 spec 中略，按 JSON-RPC 2.0 标准）
-spec parse_error(term()) -> map().
-spec invalid_request(term()) -> map().
-spec method_not_found(term(), binary()) -> map().
-spec invalid_params(term(), binary()) -> map().
-spec internal_error(term()) -> map().
-spec custom_error(term(), integer(), binary(), term()) -> map().
```

#### beamai_id

统一 ID 生成与解析（带前缀、毫秒时间戳、随机段）。

```erlang
%% 生成带前缀的唯一 ID
-spec gen_id(prefix()) -> id().

%% 解析 ID 获取其组成部分
-spec parse_id(id()) -> {ok, parsed_id()} | {error, term()}.
```

```erlang
-type id() :: binary().
-type prefix() :: binary().
-type parsed_id() :: #{
    prefix := binary(),
    timestamp := non_neg_integer(),
    random := binary()
}.
```

---

## beamai_llm - LLM 客户端

`beamai_llm` 提供统一的 LLM Chat Completion 客户端，支持 OpenAI / Anthropic / DeepSeek / Zhipu / DashScope / Ollama 六家 Provider，结构化错误、Output Parser、Provider 适配器。

### Chat Completion：beamai_chat_completion

Chat Completion 服务的统一入口。**这是最常用的 API**。

```erlang
%% 创建指定 provider 的 chat completion 配置
-spec create(provider(), map()) -> config().

%% 发送 chat completion 请求
-spec chat(config(), [map()]) -> {ok, map()} | {error, term()}.

%% 发送 chat completion 请求（带选项）
-spec chat(config(), [map()], map()) -> {ok, map()} | {error, term()}.

%% 发送流式 chat 请求
-spec stream_chat(config(), [map()], fun((term()) -> ok)) ->
    {ok, map()} | {error, term()}.

%% 发送流式 chat 请求（带选项）
-spec stream_chat(config(), [map()], fun((term()) -> ok), map()) ->
    {ok, map()} | {error, term()}.
```

类型：

```erlang
-type provider() :: openai | anthropic | ollama | zhipu | dashscope |
                    deepseek | mock | {custom, module()}.

-type config() :: #{
    provider := provider(),
    module := module(),
    '__llm_config__' := true,
    atom() => term()
}.
```

工作示例：

```erlang
LLM = beamai_chat_completion:create(zhipu, #{
    model => <<"glm-4.6">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    temperature => 0.7
}),

{ok, Resp} = beamai_chat_completion:chat(LLM, [
    #{role => user, content => <<"你好！"/utf8>>}
]),
io:format("~ts~n", [beamai_llm_response:content(Resp)]).
```

支持的 create 选项：

| 选项 | 含义 |
|------|------|
| `model` | 模型名（必填） |
| `api_key` | API Key（除 ollama 外必填） |
| `base_url` | 自定义 base URL（可选） |
| `timeout` | 请求超时（毫秒） |
| `max_tokens` | 最大输出 tokens |
| `temperature` | 采样温度（0.0-2.0） |

### 错误结构：beamai_llm_error

LLM 错误统一归一化结构。所有 Provider 抛出的错误都会经过此模块归一为 `llm_error()`，上层只需读 `type/status/retryable/retry_after_ms` 决定重试策略。

```erlang
%% 将错误 Reason 归一化为统一结构（provider 未知）
-spec from_reason(term()) -> llm_error().

%% 将错误 Reason 归一化为统一结构（指定 provider）
-spec from_reason(term(), atom() | undefined) -> llm_error().

%% 判断是否为 LLM 错误
-spec is_error(term()) -> boolean().

%% 取错误类型
-spec type(llm_error()) -> error_type().

%% 取 HTTP 状态码
-spec status(llm_error()) -> non_neg_integer() | undefined.

%% 取人类可读描述
-spec message(llm_error()) -> binary().

%% 取 Provider
-spec provider(llm_error()) -> atom() | undefined.

%% 是否可重试
-spec retryable(llm_error()) -> boolean().

%% 取建议退避时间
-spec retry_after_ms(llm_error()) -> non_neg_integer() | undefined.

%% 取原始错误 Reason
-spec raw(llm_error()) -> term().
```

类型：

```erlang
-type error_type() :: rate_limit | server_error | client_error | auth |
                      timeout | network | invalid_response | api_error | unknown.

-type llm_error() :: #{
    '__llm_error__' := true,
    type := error_type(),
    status := non_neg_integer() | undefined,
    message := binary(),
    provider := atom() | undefined,
    retryable := boolean(),
    retry_after_ms := non_neg_integer() | undefined,
    raw := term()
}.
```

`retryable` 由错误类型推导（`rate_limit` / `server_error` / `timeout` / `network` 为可重试，其余为不可重试），`retry_after_ms` 优先从响应头 `Retry-After` 解析。

### HTTP 客户端：beamai_llm_http_client

Provider 模块共用的 HTTP 客户端封装，负责发送请求、SSE 解析、流式事件累加。

```erlang
%% 发送 HTTP POST 请求（默认响应解析）
-spec request(binary(), [{binary(), binary()}], map(), request_opts()) ->
    {ok, map()} | {error, term()}.

%% 发送 HTTP POST 请求（带自定义响应解析器）
-spec request(binary(), [{binary(), binary()}], map(), request_opts(),
              response_parser()) -> {ok, map()} | {error, term()}.

%% 发送流式 HTTP 请求
-spec stream_request(binary(), [{binary(), binary()}], map(),
                     request_opts(), stream_callback()) ->
    {ok, map()} | {error, term()}.

%% 发送流式 HTTP 请求（带自定义事件累加器）
-spec stream_request(binary(), [{binary(), binary()}], map(),
                     request_opts(), stream_callback(), event_accumulator()) ->
    {ok, map()} | {error, term()}.

%% 解析 SSE 数据 → {剩余 buffer, 事件列表（map | done | skip）}
-spec parse_sse(binary()) -> {binary(), [map() | done | skip]}.

%% 解析 SSE 行列表
-spec parse_sse_lines([binary()], [map() | done | skip]) ->
    {binary(), [map() | done | skip]}.

%% 初始化流式累加器
-spec init_stream_acc() -> map().

%% 完成流式处理，生成最终结果
-spec finalize_stream(map()) -> {ok, map()}.
```

### Output Parser

结构化输出（JSON / XML / CSV）解析与重试机制。详见 [OUTPUT_PARSER.md](OUTPUT_PARSER.md)。

#### beamai_output_parser

Output Parser 的统一入口。

```erlang
%% 创建通用 Parser
-spec new(format(), map()) -> parser().

%% 创建 JSON Parser（默认选项）
-spec json() -> parser().

%% 创建 JSON Parser（带选项，如 schema）
-spec json(map()) -> parser().

%% 创建 XML Parser
-spec xml() -> parser().

%% 创建 CSV Parser
-spec csv() -> parser().

%% 解析文本（使用默认选项）
-spec parse(parser(), binary()) -> parse_result().

%% 解析文本（带覆盖选项）
-spec parse(parser(), binary(), map()) -> parse_result().

%% 带重试的解析（默认最多 3 次）
-spec parse_with_retry(parser(), binary(), non_neg_integer()) ->
    parse_result().

%% 带重试的解析（完整选项）
-spec parse_with_retry(parser(), binary(), non_neg_integer(), map()) ->
    parse_result().

%% 取格式指令（提示词拼装用）
-spec get_instructions(format()) -> binary().

%% 取格式指令（带选项）
-spec get_instructions(format(), map()) -> binary().

%% 判断是否需要重试
-spec is_retryable_error(parse_error()) -> boolean().
```

类型：

```erlang
-type format() :: json | xml | csv | raw.
-type parser() :: #{type := format(), options := map()}.
-type parse_result() :: {ok, term()} | {error, parse_error()}.
-type parse_error() ::
    {invalid_json, binary()} |
    {invalid_xml, binary()} |
    {invalid_csv, binary()} |
    {extract_failed, binary()} |
    {max_retries_exceeded, [parse_error()]}.
```

带 Schema 的 JSON Parser 工作示例：

```erlang
Parser = beamai_output_parser:json(#{
    schema => #{
        type => object,
        properties => #{
            <<"title">> => #{type => string},
            <<"count">> => #{type => integer, minimum => 0}
        },
        required => [<<"title">>, <<"count">>]
    }
}),

{ok, Parsed} = beamai_output_parser:parse(Parser, LLMText).
```

#### beamai_parser_json

JSON 解析的内部实现，提供 JSON 提取、修复、边界定位等底层能力。

```erlang
%% 解析 JSON 文本
-spec parse(binary(), parse_options()) -> parse_result().

%% 从文本中提取 JSON
-spec extract_json(binary()) -> {ok, binary()} | {error, term()}.

%% 提取 JSON 代码块（```json ... ``` 围栏）
-spec extract_json_codeblock(binary()) -> {ok, binary()} | {error, term()}.

%% 修复常见的 JSON 格式错误
-spec repair_json(binary()) -> binary().

%% 查找 JSON 边界
-spec find_json_boundaries(binary()) -> {ok, binary()} | {error, term()}.
```

#### beamai_parser_instructions

为 Output Parser 生成格式指令（用于注入到 system prompt）。

```erlang
%% 默认 JSON 格式指令
-spec json() -> binary().

%% JSON 格式指令（带选项）
-spec json(map()) -> binary().

%% XML 格式指令
-spec xml() -> binary().

%% CSV 格式指令
-spec csv() -> binary().

%% 将 JSON Schema 转换为指令
-spec json_schema_to_instruction(map()) -> binary().
```

#### beamai_parser_retry

带重试与退避的解析机制。

```erlang
%% 带重试的解析
-spec parse(beamai_output_parser:parser(), binary(),
            non_neg_integer(), map()) -> parse_result().

%% 带退避策略的重试解析
-spec parse_with_backoff(beamai_output_parser:parser(), binary(),
                         non_neg_integer(), atom(), retry_options()) ->
    parse_result().
```

### Provider 适配器

把 Provider 原生格式与 BeamAI 中性结构互转的模块层。

#### beamai_llm_response_parser

把每个 Provider 的原始响应 map 归一化为 `beamai_llm_response:response()`。

```erlang
%% 取指定 Provider 的解析器函数
-spec parser(beamai_llm_response:provider()) ->
    fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).

%% 单独命名解析器
-spec parser_openai() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_anthropic() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_ollama() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_dashscope() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_zhipu() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_deepseek() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_deepseek_fim() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).

%% 构造函数（直接吃原始 map）
-spec from_provider(map(), beamai_llm_response:provider()) ->
    {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_openai(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_anthropic(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_zhipu(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_deepseek(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_deepseek_fim(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_ollama(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_dashscope(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
```

#### beamai_llm_message_adapter

中性 `beamai_message:message()` 形态与各 Provider 原生消息形态互转。

```erlang
%% 转换为指定 Provider 格式
-spec to_provider([message()], atom()) -> [map()].

%% 从指定 Provider 格式转换
-spec from_provider([map()], atom()) -> [message()].

%% 提取系统提示（system 消息与其它分开）
-spec extract_system_prompt([message()]) -> {term() | undefined, [message()]}.

%% 单独命名
-spec to_openai([message()]) -> [map()].
-spec from_openai([map()]) -> [message()].
-spec to_anthropic([message()]) -> [map()].
-spec from_anthropic([map()]) -> [message()].
```

类型：

```erlang
-type role_atom() :: user | assistant | system | tool.
-type role() :: role_atom() | binary().
-type message() :: #{
    role := role(),
    content := content(),
    name => binary(),
    tool_call_id => binary(),
    tool_calls => [map()],
    prefix => boolean()
}.
-type content() :: binary() | null | [content_part()].
-type content_part() :: #{type := atom(), _ => _}.
-type media_source() ::
    #{type := base64, media_type := binary(), data := binary()} |
    #{type := url, url := binary()}.
```

#### beamai_llm_tool_adapter

中性 `beamai_tool:tool_spec()` 与各 Provider 工具 schema 互转（多模态输入、Anthropic 缓存 / Web Search / 引用也走此层）。

```erlang
%% 转换为指定 Provider 格式
-spec to_provider([tool() | tool_spec()], atom()) -> [map()].
-spec from_provider([map()], atom()) -> [tool()].

%% 单独命名
-spec to_openai([tool() | tool_spec()]) -> [map()].
-spec from_openai([map()]) -> [tool()].
-spec to_anthropic([tool() | tool_spec()]) -> [map()].
-spec from_anthropic([map()]) -> [tool()].
```

类型：

```erlang
-type tool() :: #{
    name := binary(),
    description := binary(),
    parameters := map()
}.
-type tool_spec() :: #{
    type := function,
    function := #{
        name := binary(),
        description := binary(),
        parameters := map()
    }
}.
```

#### beamai_llm_provider_common

Provider 模块共享的工具函数（URL 构建、请求头、流式事件累加、响应解析）。

```erlang
%% URL 与超时
-spec build_url(map(), binary(), binary()) -> binary().
-spec default_timeout(atom()) -> pos_integer().
-spec request_timeout(map(), atom()) -> timeout().

%% 请求头与请求体
-spec build_bearer_auth_headers(map()) -> [{binary(), binary()}].
-spec maybe_add_stream(map(), map()) -> map().
-spec maybe_add_tools(map(), map()) -> map().
-spec maybe_add_top_p(map(), map()) -> map().
-spec maybe_add_tool_choice(map(), map()) -> map().
-spec maybe_add_params(map(), map(), [{atom(), binary()}]) -> map().

%% 流式事件累加
-spec accumulate_openai_event(map(), map()) -> map().
-spec finalize_openai_stream(map(), atom()) -> {ok, map()} | {error, term()}.
-spec accumulate_anthropic_event(map(), map()) -> map().
-spec finalize_anthropic_stream(map()) -> {ok, map()}.
-spec accumulate_completions_event(map(), map()) -> map().
-spec finalize_completions_stream(map(), fun((map()) -> {ok, map()} | {error, term()})) ->
    {ok, map()} | {error, term()}.

%% 响应解析
-spec parse_tool_calls(map()) -> [map()].
-spec parse_single_tool_call(map()) -> map().
-spec parse_usage(map()) -> map().

%% 响应头
-spec rate_limit_metadata([{binary(), binary()}]) -> map().
-spec retry_after_ms([{binary(), binary()}]) -> non_neg_integer() | undefined.
```

### 支持的 Provider

`beamai_chat_completion:create/2` 通过 `provider` 原子选择实现，所有 Provider 统一 `{ok, response()} | {error, term()}` 形态。

| Provider | 模块 | API 模式 | 特性 |
|----------|------|----------|------|
| `openai` | `beamai_llm_provider_openai` | OpenAI | Chat、streaming、tool calling |
| `anthropic` | `beamai_llm_provider_anthropic` | Anthropic | Chat、streaming、tool calling、Prompt 缓存、Web Search、引用 |
| `deepseek` | `beamai_llm_provider_deepseek` | OpenAI 兼容 | Chat、streaming、tool calling、reasoner |
| `zhipu` | `beamai_llm_provider_zhipu` | OpenAI 兼容 | Chat、streaming、tool calling、async（GLM-4.6+ 推理内容） |
| `dashscope` | `beamai_llm_provider_dashscope` | DashScope 原生 | Chat、streaming、tool calling、文本/多模态自动选 endpoint、web search |
| `ollama` | `beamai_llm_provider_ollama` | OpenAI 兼容 | Chat、streaming（本地模型，无需 API Key） |

> 测试场景另提供 `beamai_llm_provider_mock`（provider 原子 `mock`），用于离线与单测，不建议用于生产路径。

DeepSeek 额外说明：使用 OpenAI 兼容接口，支持 `deepseek-chat`（默认）与 `deepseek-reasoner`（增强推理）两个模型。

DashScope 额外说明：根据模型类型自动选择 endpoint（文本生成走 `/services/aigc/text-generation/generation`，多模态走 `/services/aigc/multimodal-generation/generation`）。`enable_search => true` 启用 Web Search；流式请求头 `X-DashScope-SSE: enable`，参数 `parameters.incremental_output: true`。

---

## beamai_agent - SimpleAgent（ReAct）

`beamai_agent` 是有状态多轮对话 Agent，基于 ReAct 思路构建：每轮 `run` / `stream` 内部会驱动工具循环，调度 LLM 与工具直到拿到最终答案。

### Agent：beamai_agent

**核心 API**。SimpleAgent 的入口模块。所有公开 API 都围绕 `agent_state()`（不透明句柄，由 `beamai_agent_state` 创建）展开。

```erlang
%% 创建新的 Agent 实例
-spec new(map()) ->
    {ok, beamai_agent_state:agent_state()} | {error, term()}.

%% 执行一轮对话（默认选项）
-spec run(beamai_agent_state:agent_state(), binary()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.

%% 执行一轮对话（带选项）
-spec run(beamai_agent_state:agent_state(), binary(), map()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.

%% 流式执行一轮对话（默认选项）
-spec stream(beamai_agent_state:agent_state(), binary()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.

%% 流式执行一轮对话（带选项）
-spec stream(beamai_agent_state:agent_state(), binary(), map()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.

%% 恢复中断的 agent 继续执行
-spec resume(beamai_agent_state:agent_state(), term()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.

%% 从中断恢复执行（带 payload）
-spec resume(beamai_agent_state:agent_state(), term(), map()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.

%% 判断 agent 是否处于中断状态
-spec is_interrupted(beamai_agent_state:agent_state()) -> boolean().

%% 获取中断信息
-spec get_interrupt_info(beamai_agent_state:agent_state()) ->
    interrupt_info() | undefined.

%% 取对话消息历史
-spec messages(beamai_agent_state:agent_state()) -> [map()].

%% 取最后一条 assistant 文本响应
-spec last_response(beamai_agent_state:agent_state()) -> binary() | undefined.

%% 取已完成的对话 turn 数
-spec turn_count(beamai_agent_state:agent_state()) -> non_neg_integer().

%% 取 agent 内部的 kernel 实例
-spec kernel(beamai_agent_state:agent_state()) -> beamai_kernel:kernel().

%% 取 agent 唯一标识
-spec id(beamai_agent_state:agent_state()) -> binary().

%% 取 agent 名称
-spec name(beamai_agent_state:agent_state()) -> binary().

%% 设置系统提示词
-spec set_system_prompt(beamai_agent_state:agent_state(), binary()) ->
    beamai_agent_state:agent_state().

%% 手动追加消息到历史
-spec add_message(beamai_agent_state:agent_state(), map()) ->
    beamai_agent_state:agent_state().

%% 清空消息历史
-spec clear_messages(beamai_agent_state:agent_state()) ->
    beamai_agent_state:agent_state().

%% 更新用户元数据（合并方式）
-spec update_metadata(beamai_agent_state:agent_state(), map()) ->
    beamai_agent_state:agent_state().
```

类型：

```erlang
-type run_result() :: #{
    content := binary(),
    tool_calls_made => [map()],
    finish_reason := beamai_llm_response:finish_reason(),
    usage := map(),
    iterations := non_neg_integer()
}.

-type interrupt_info() :: #{
    reason := term(),
    interrupt_type := tool_request | tool_result | callback | env_retry,
    interrupted_tool_call => map(),
    completed_results => [map()],
    created_at := integer()
}.
```

**示例 1：基本 run 循环**

```erlang
LLM = beamai_chat_completion:create(zhipu, #{
    model => <<"glm-4.6">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY"))
}),

SearchTool = beamai:tool(<<"search">>,
    fun(#{<<"query">> := Q}) ->
        {ok, <<"results for ", Q/binary>>}
    end,
    #{description => <<"搜索"/utf8>>,
      parameters => #{<<"query">> => #{type => string, required => true}}}),

{ok, Agent0} = beamai_agent:new(#{
    name => <<"assistant">>,
    llm => LLM,
    tools => [SearchTool]
}),

{ok, Result, Agent1} = beamai_agent:run(Agent0, <<"今天 Erlang 有什么新闻"/utf8>>),
io:format("answer: ~ts~n", [maps:get(content, Result)]),
%% Agent1 已是更新后的状态，可继续下一轮
{ok, R2, _Agent2} = beamai_agent:run(Agent1, <<"详细说说第一条"/utf8>>).
```

**示例 2：interrupt + resume 流程**

```erlang
ApprovalTool = beamai:tool(<<"send_email">>,
    fun(#{<<"to">> := To, <<"body">> := Body}) ->
        {ok, <<"email sent to ", To/binary, ": ", Body/binary>>}
    end,
    #{description => <<"发送邮件（敏感）"/utf8>>,
      parameters => #{
          <<"to">> => #{type => string, required => true},
          <<"body">> => #{type => string, required => true}
      },
      sensitive => true,
      return_direct => true}),

{ok, Agent0} = beamai_agent:new(#{
    name => <<"assistant">>,
    llm => LLM,
    tools => [ApprovalTool]
}),

%% 第一次 run：模型请求调用 send_email，agent 进入中断
{interrupt, Info, Agent1} = beamai_agent:run(
    Agent0, <<"帮我发一封邮件给 alice@example.com 说你好"/utf8>>),
true = beamai_agent:is_interrupted(Agent1),
%% 人工审批通过 → resume（payload 是审批决定）
{ok, R, _Agent2} = beamai_agent:resume(
    Agent1, {approve, #{reason => <<"ok">>}}),
io:format("~ts~n", [maps:get(content, R)]).
```

### 状态：beamai_agent_state

Agent 状态构建与 Kernel 集成。`agent_state()` 是 Agent 内部所有函数共用的不透明句柄。

```erlang
%% 从 Config 创建完整的 agent_state
-spec create(map()) -> {ok, agent_state()} | {error, term()}.

%% 从 Config 构建 Kernel 实例
-spec build_kernel(map()) -> map().

%% 取 agent 的记忆 provider
-spec memory(agent_state()) -> beamai_memory_provider:provider() | undefined.

%% 取 agent 的会话标识
-spec conversation_id(agent_state()) -> binary().
```

类型：

```erlang
-type agent_state() :: #{
    '__agent__' := true,
    id := binary(),
    name := binary(),
    kernel := beamai_kernel:kernel(),
    memory := beamai_memory_provider:provider() | undefined,
    conversation_id := binary(),
    system_prompt := binary() | undefined,
    max_tool_iterations := pos_integer(),
    parallel_tools := boolean(),
    callbacks := beamai_agent_callbacks:callbacks(),
    turn_count := non_neg_integer(),
    metadata := map(),
    created_at := integer(),
    interrupt_state := undefined | interrupt_state(),
    run_id := binary() | undefined,
    interrupt_tools := [map()],
    on_env_error := proceed | pause,
    pause_store := beamai_pause_store:handle() | undefined
}.

-type interrupt_state() :: #{
    status := interrupted,
    reason := term(),
    messages := [map()],
    completed_tool_results := [map()],
    interrupted_tool_call := map() | undefined,
    iteration := non_neg_integer(),
    tool_calls_made := [map()],
    saved_state := map(),
    interrupt_type := tool_request | tool_result | callback | env_retry,
    created_at := integer(),
    phase => approval | env_retry,
    batch_messages => [map()],
    failed_calls => [map()]
}.
```

### Tool Loop：beamai_agent_tool_loop

Agent 自管编排的工具循环（full-messages + 显式记忆/回调）。本模块主要给框架内部使用，但导出作为公开 API 以便高级用法（自定义 loop 编排）接入。

```erlang
%% Tool loop 入口
-spec run(loop_opts(), [map()]) ->
    {ok, map(), [map()], pos_integer(), [map()]} |
    {interrupt, atom(), map()} |
    {error, term()}.

%% 构建环境类暂停（phase=env_retry）的中断上下文
-spec build_env_interrupt_context(
    non_neg_integer(), [map()], [map()], [map()], [map()],
    map(), [map()]) -> map().
```

类型：

```erlang
-type loop_opts() :: #{
    kernel := beamai_kernel:kernel(),
    messages := [map()],
    new_messages := [map()],
    load_history := boolean(),
    chat_opts := map(),
    callbacks := map(),
    max_iterations := pos_integer(),
    max_tool_iterations := pos_integer(),
    parallel_tools := boolean(),
    interrupt_tools := [map()],
    on_env_error => proceed | pause,
    memory := beamai_memory_provider:provider() | undefined,
    conversation_id := binary(),
    meta := map(),
    stream_token_handler => undefined | fun((binary()) -> ok)
}.
```

### 回调：beamai_agent_callbacks

Agent 回调系统的元数据。回调本体是用户在 Agent 配置中给出的闭包；本模块提供安全调用入口与元数据构造。

```erlang
%% 安全调用回调函数（捕获异常，避免单个 callback 崩溃影响整个 turn）
-spec invoke(atom(), [term()], callbacks()) -> ok.

%% 构建回调元数据
-spec build_metadata(map()) -> map().
```

类型（10 个回调键）：

```erlang
-type callbacks() :: #{
    on_turn_start  => fun((map()) -> ok),
    on_turn_end    => fun((map()) -> ok),
    on_turn_error  => fun((term(), map()) -> ok),
    on_llm_call    => fun(([map()], map()) -> ok),
    on_llm_result  => fun((map(), map()) -> ok),
    on_tool_call   => fun((binary(), map()) -> ok | {interrupt, term()}),
    on_tool_result => fun((binary(), binary()) -> ok),
    on_token       => fun((binary(), map()) -> ok),
    on_interrupt   => fun((map(), map()) -> ok),
    on_resume      => fun((map(), map()) -> ok)
}.
```

### 中断机制：beamai_agent_interrupt

Human-in-the-Loop 中断的工具支持。`tool_request` / `tool_result` / `callback` / `env_retry` 四类中断均在此模块管理。

```erlang
%% 从 tool_calls 列表中查找中断 tool
-spec find_interrupt_tool([map()], map()) ->
    {yes, map(), [map()]} | no.

%% 检查单个 tool_call 是否为中断 tool
-spec is_interrupt_tool(map(), [map()]) -> boolean().

%% 处理中断：构建 interrupt_state
-spec handle_interrupt(atom(), term(), map(), map()) ->
    {map(), map()}.

%% 构建恢复用的消息
-spec build_resume_messages(map(), term()) -> [map()].

%% 审批暂停下把 Decision + Payload 解析为一个动作
-spec resume_action(map(), term(), map()) ->
    {execute, map()} | {result, map()}.

%% 用重跑结果按 tool_call_id 替换原批次消息
-spec replace_results_by_id([map()], [map()]) -> [map()].

%% 验证 resume 输入是否匹配中断上下文
-spec validate_resume_input(map(), term()) -> ok | {error, term()}.

%% 取 interrupt_tools 的 tool specs
-spec get_interrupt_tool_specs(map()) -> [map()].
```

### 暂停持久化：beamai_agent_pause

Agent 暂停快照的存储集成（opt-in 全自动）。配置 `pause_store` 后，每次进入中断态自动落库，恢复时通过 `conversation_id` 自动回读。

```erlang
%% 若配置了 pause_store 且 agent 处于中断态，落库暂停快照
-spec save(map()) -> ok.

%% 从 pause_store 回读快照重建中断态
-spec load(map()) -> {ok, map()} | none.

%% 清除该会话的暂停快照
-spec clear(map()) -> ok.
```

### Agent 工具：beamai_agent_utils

Agent 内部使用的工具函数集（提取响应、构建 chat opts、串/并行执行工具、错误归一）。

```erlang
%% 从 LLM 响应中提取文本内容
-spec extract_content(map()) -> binary().

%% 从 Kernel 构建 chat 选项
-spec build_chat_opts(beamai_kernel:kernel(), map()) -> map().

%% 执行 tool calls 并收集结果（串行，新建 context）
-spec execute_tools(beamai_kernel:kernel(), [map()]) ->
    {[map()], [map()], beamai_context:t()}.

%% 执行 tool calls 并收集结果（串行，指定执行上下文）
-spec execute_tools(beamai_kernel:kernel(), [map()],
                    beamai_context:t()) ->
    {[map()], [map()], beamai_context:t()}.

%% 执行 tool calls（统一入口：可选并发）
-spec execute_tools(beamai_kernel:kernel(), [map()],
                    beamai_context:t(), boolean()) ->
    {[map()], [map()], beamai_context:t()}.

%% 执行 tool calls（带实时结果回调）
-spec execute_tools(beamai_kernel:kernel(), [map()],
                    beamai_context:t(), boolean(),
                    fun((map()) -> ok)) ->
    {[map()], [map()], beamai_context:t()}.

%% 执行单个 tool
-spec run_one_tool(beamai_kernel:kernel(), map(), beamai_context:t()) ->
    {map(), map(), beamai_context:writes()}.

%% 把工具错误原因归一为稳定、JSON 友好的结构
-spec tool_error(term()) -> #{error := map()}.
```

### 子 Agent 委派：beamai_agent_delegate

把「子 Agent 委派」作为一组工具暴露给父 Agent。建在 `beamai_subagent_manager` 之上。

```erlang
%% 单委派工具（同步：spawn → await → drop）
-spec tool(config()) -> map().

%% 并发 fan-out 工具：一次调用传 tasks 列表
-spec fanout_tool(config()) -> map().

%% 返回一组管理工具：spawn / list / result / kill / restart
-spec management_tools(config()) -> [map()].

%% 程序化并发 fan-out
-spec run_many([{map(), binary()}], timeout()) ->
    [beamai_subagent_manager:outcome() | {error, sub_agent_timeout}].

%% 程序化并发 fan-out（带自定义结果提取函数）
-spec run_many([{map(), binary()}], timeout(),
               fun((map()) -> binary())) ->
    [beamai_subagent_manager:outcome() | {error, sub_agent_timeout}].
```

类型：

```erlang
-type config() :: #{
    name := binary(),
    description => binary(),
    subagent := fun((map(), beamai_context:t()) -> map()),
    seed => fun((map(), beamai_context:t()) -> binary()),
    result => fun((map()) -> binary()),
    timeout => timeout()
}.
```

### 子 Agent 管理器：beamai_subagent_manager

异步子 Agent 注册表。负责子 Agent 的 spawn / await / list / kill / restart / drop。

```erlang
%% 启动管理器
-spec start_link() -> {ok, pid()} | {error, term()}.

%% 启动管理器（带选项）
-spec start_link(map()) -> {ok, pid()} | {error, term()}.

%% 确保管理器在线
-spec ensure_started() -> pid().

%% 异步起一个子 agent，立即返回 id
-spec spawn(spec()) -> {ok, id()}.

%% 同步等待某子 agent 完成
-spec await(id(), timeout()) ->
    outcome() | {error, timeout} | {error, not_found}.

%% 取已完成子 agent 的结果
-spec result(id()) -> {ok, outcome()} | {error, not_ready | not_found}.

%% 列出所有子 agent 信息
-spec list() -> [info()].

%% 列出指定 owner 的子 agent 信息
-spec list(term()) -> [info()].

%% kill 指定子 agent
-spec kill(id()) -> ok | {error, not_found}.

%% 用存的 spec 重跑指定子 agent
-spec restart(id()) -> {ok, id()} | {error, not_found}.

%% 从注册表移除
-spec drop(id()) -> ok.
```

类型：

```erlang
-type id() :: binary().
-type status() :: running | done | failed | killed.
-type outcome() :: {ok, binary()} | {error, term()}.
-type spec() :: #{
    subagent := map(),
    prompt := binary(),
    result => fun((map()) -> binary()),
    owner => term()
}.
-type info() :: #{
    id := id(),
    status := status(),
    owner := term(),
    started_at := integer(),
    finished_at := integer() | undefined
}.
```

### Timeline 与多分支：beamai_timeline

Fork-as-new-conversation：以「复制整段历史到新会话」的方式实现分支。每个新分支自带独立 `conversation_id`，可独立演进；通过 `branch_store` 维护血缘关系。

```erlang
%% 全量分支（复制整段历史到新会话）
-spec fork(deps(), binary()) -> {ok, binary()} | {error, fork_error()}.

%% 分支：前缀复制到新 conversation_id + 血缘记录
-spec fork(deps(), binary(), map()) -> {ok, binary()} | {error, fork_error()}.

%% 破坏性截断到前 N 条消息
-spec rollback(deps(), binary(), non_neg_integer()) -> ok.

%% 查某会话的血缘记录
-spec lineage(deps(), binary()) ->
    {ok, beamai_branch_store:branch_record()} | none.

%% 沿 parent 回溯，返回「自身在前」的血缘记录链
-spec ancestry(deps(), binary()) -> [ancestry_record()].

%% 删分支（历史 + 暂停快照 + 血缘）
-spec prune(deps(), binary()) -> ok | {error, {has_children, [binary()]}}.
```

类型：

```erlang
-type deps() :: #{
    memory := beamai_chat_memory:handle(),
    branch := beamai_branch_store:handle(),
    pause_store => beamai_pause_store:handle()
}.

-type ancestry_record() :: #{
    id := binary(),
    parent := binary() | undefined,
    fork_point := non_neg_integer() | all,
    created_at := integer()
}.

-type fork_error() :: empty_source | {invalid_at, term()} | {target_exists, binary()}.
```

### 分支血缘存储

#### beamai_branch_store

分支血缘存储 Behaviour。Timeline / 多分支功能依赖此接口。

```erlang
%% 记录某会话的血缘（覆盖既有）
-spec record(handle(), binary(), branch_record()) -> ok.

%% 读取某会话的血缘记录
-spec get(handle(), binary()) -> {ok, branch_record()} | none.

%% 列出以 ConvId 为 parent 的直接子会话 id
-spec children(handle(), binary()) -> [binary()].

%% 删除某会话的血缘记录
-spec delete(handle(), binary()) -> ok.
```

类型与回调：

```erlang
-type handle() :: {module(), term()}.
-type branch_record() :: #{
    parent := binary() | undefined,
    fork_point := non_neg_integer() | all,
    created_at := integer()
}.

-callback record(Ref :: term(), ConvId :: binary(), Record :: branch_record()) -> ok.
-callback get(Ref :: term(), ConvId :: binary()) -> {ok, branch_record()} | none.
-callback children(Ref :: term(), ConvId :: binary()) -> [binary()].
-callback delete(Ref :: term(), ConvId :: binary()) -> ok.
```

#### beamai_branch_store_ets

分支血缘存储的 ETS 内存实现。

```erlang
%% 启动 ETS 分支存储
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.

%% 停止存储
-spec stop(atom()) -> ok.

%% 构造 beamai_branch_store 句柄
-spec handle(atom()) -> beamai_branch_store:handle().

%% 记录某会话的血缘
-spec record(atom(), binary(), beamai_branch_store:branch_record()) -> ok.

%% 读取某会话的血缘记录
-spec get(atom(), binary()) -> {ok, beamai_branch_store:branch_record()} | none.

%% 列出以 ConvId 为 parent 的直接子会话
-spec children(atom(), binary()) -> [binary()].

%% 删除某会话的血缘记录
-spec delete(atom(), binary()) -> ok.
```

### 暂停快照存储

#### beamai_pause_store

暂停快照存储 Behaviour。跨进程 HITL 依赖此接口。

```erlang
%% 保存某会话的暂停快照（覆盖既有）
-spec pause_save(handle(), binary(), snapshot()) -> ok.

%% 读取某会话的暂停快照
-spec pause_load(handle(), binary()) -> {ok, snapshot()} | none.

%% 清除某会话的暂停快照
-spec pause_clear(handle(), binary()) -> ok.
```

```erlang
-type handle() :: {module(), term()}.
-type snapshot() :: map().

-callback pause_save(Ref :: term(), ConvId :: binary(), Snapshot :: snapshot()) -> ok.
-callback pause_load(Ref :: term(), ConvId :: binary()) -> {ok, snapshot()} | none.
-callback pause_clear(Ref :: term(), ConvId :: binary()) -> ok.
```

#### beamai_pause_store_ets

暂停快照存储的 ETS 内存实现。

```erlang
%% 启动 ETS 暂停存储
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.

%% 停止存储
-spec stop(atom()) -> ok.

%% 构造 beamai_pause_store 句柄
-spec handle(atom()) -> beamai_pause_store:handle().

%% 保存某会话的暂停快照
-spec pause_save(atom(), binary(), beamai_pause_store:snapshot()) -> ok.

%% 读取某会话的暂停快照
-spec pause_load(atom(), binary()) -> {ok, beamai_pause_store:snapshot()} | none.

%% 清除某会话的暂停快照
-spec pause_clear(atom(), binary()) -> ok.
```

---

## Behaviour 接口

BeamAI 通过 Behaviour 接口实现解耦，自定义后端只需实现对应 behaviour 的回调即可接入。句柄约定为 `{Module, Ref}`，调度模块负责解包转发。

### beamai_chat_behaviour

Chat Completion 行为接口，Provider 模块均实现此 behaviour。

```erlang
-callback create(Provider :: provider(), Opts :: map()) -> config().

-callback chat(Config :: config(), Messages :: [map()]) ->
    {ok, map()} | {error, term()}.

-callback chat(Config :: config(), Messages :: [map()], Opts :: map()) ->
    {ok, map()} | {error, term()}.

%% 可选：流式
-callback stream_chat(Config :: config(), Messages :: [map()], Callback :: fun()) ->
    {ok, map()} | {error, term()}.
```

类型：

```erlang
-type provider() :: openai | anthropic | ollama | zhipu | dashscope |
                    deepseek | mock | {custom, module()}.

-type config() :: #{
    provider := provider(),
    module := module(),
    '__llm_config__' := true,
    atom() => term()
}.
```

### beamai_memory_provider

Agent 记忆 Provider Behaviour。这是 Agent 的记忆**接口**，与 `beamai_chat_memory`（Kernel 层）正交。

```erlang
%% 加载某会话的跨轮历史
-spec history(provider(), binary()) -> [message()].

%% 持久化消息
-spec append(provider(), binary(), [message()]) -> ok.

%% 发送前变换
-spec prepare(provider(), binary(), [message()]) -> [message()].

%% 清空会话
-spec clear(provider(), binary()) -> ok.

%% 用默认 provider 包装一个存储后端句柄
-spec default(beamai_chat_memory:handle()) -> provider().
```

回调：

```erlang
-callback history(Ref :: term(), ConvId :: binary()) -> [message()].
-callback append(Ref :: term(), ConvId :: binary(), Msgs :: [message()]) -> ok.
-callback prepare(Ref :: term(), ConvId :: binary(),
                  Messages :: [message()]) -> [message()].
-callback clear(Ref :: term(), ConvId :: binary()) -> ok.

-type provider() :: {module(), term()}.
-type message() :: beamai_message:message().
```

### beamai_chat_memory

会话消息存储 Behaviour，Memory Filter 的依赖接口。

```erlang
%% 获取会话历史
-spec mem_get(handle(), binary()) -> [message()].

%% 追加消息到会话
-spec mem_add(handle(), binary(), [message()]) -> ok.

%% 清空会话
-spec mem_clear(handle(), binary()) -> ok.
```

回调与类型：

```erlang
-callback mem_get(Ref :: term(), ConvId :: binary()) -> [message()].
-callback mem_add(Ref :: term(), ConvId :: binary(), Msgs :: [message()]) -> ok.
-callback mem_clear(Ref :: term(), ConvId :: binary()) -> ok.

-type handle() :: {module(), term()}.
-type message() :: beamai_message:message().
```

### beamai_tool_behaviour

工具行为定义。模块实现此 behaviour 后可被 `beamai_kernel:add_tool_module/2` 自动加载。

```erlang
%% 必选：返回工具列表
-callback tools() -> [beamai_tool:tool_spec()].

%% 可选：工具元信息（描述 / tags / metadata）
-callback tool_info() -> #{
    description => binary(),
    tags => [binary()],
    metadata => map()
}.

%% 可选：与工具绑定的 filter 列表（仅在 add_tool_module 时挂上）
-callback filters() -> [beamai_filter:filter()].
```

### beamai_http_behaviour

HTTP 客户端行为定义。内置实现为 `beamai_http_gun`；测试用的
`beamai_llm_fake_backend` 亦实现此 behaviour。

```erlang
-callback request(Method :: method(), Url :: url(), Headers :: headers(),
                  Body :: body(), Opts :: opts()) -> response().

-callback stream_request(Method :: method(), Url :: url(),
                         Headers :: headers(), Body :: body(),
                         Opts :: opts(), Handler :: chunk_handler()) ->
    stream_response().

-callback ensure_started() -> ok.

%% 可选
-callback close(Ref :: term()) -> ok.

%% 可选：返回响应元信息（状态码 + headers）
-callback request_meta(Method :: method(), Url :: url(), Headers :: headers(),
                       Body :: body(), Opts :: opts()) -> response_meta().
```

类型：

```erlang
-type method() :: get | post | put | delete | head | options | patch.
-type url() :: binary() | string().
-type headers() :: [{binary(), binary()}].
-type body() :: binary() | iodata().
-type opts() :: #{
    timeout => pos_integer(),
    connect_timeout => pos_integer(),
    headers => headers(),
    pool => atom(),
    init_acc => term(),
    forward_headers => boolean()
}.
-type response() :: {ok, term()} | {error, term()}.
-type meta() :: #{status => non_neg_integer() | undefined, headers => headers()}.
-type response_meta() :: {ok, term(), meta()} | {error, term()}.
-type chunk_handler() :: fun((binary(), term()) -> {continue, term()} | {done, term()}).
-type stream_response() :: {ok, term()} | {error, term()}.
```

---

## 错误处理

BeamAI 的 API 普遍返回 `{ok, Result}` 或 `{error, Reason}` 形态。常见错误类型：

| 错误 | 含义 |
|------|------|
| `{error, missing_api_key}` | API Key 未配置 |
| `{error, timeout}` | 请求超时 |
| `{error, {http_error, Code, Body}}` | HTTP 错误（含状态码 + Body） |
| `{error, {api_error, Details}}` | API 返回错误 |
| `{error, not_found}` | 资源未找到 |
| `{error, storage_not_enabled}` | 存储未启用 |

LLM 错误现在由 `beamai_llm_error` 提供**结构化**错误（`type` / `status` / `retryable` / `retry_after_ms`），详见上文 [beamai_llm_error](#错误结构beamai_llm_error) 一节。

工具错误由 `beamai_tool_error:classify/1` 归类为 `semantic` / `transient` / `environment`，上层据此决定重试 / 退避 / 上报分流。

---

## 更多文档

- [README.md](../README.md) - 项目概述
- [FILTER.md](FILTER.md) - Filter 洋葱系统（含 `token_transform` 第四钩子详解）
- [MEMORY.md](MEMORY.md) - 会话记忆（Memory Filter）
- [OUTPUT_PARSER.md](OUTPUT_PARSER.md) - Output Parser 指南
- [DEPENDENCIES.md](DEPENDENCIES.md) - 依赖关系详解
- 各模块 README：
  - [beamai_core](../apps/beamai_core/README.md)
  - [beamai_llm](../apps/beamai_llm/README.md)
  - [beamai_agent](../apps/beamai_agent/README.md)