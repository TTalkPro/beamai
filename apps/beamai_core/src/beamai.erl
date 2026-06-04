%%%-------------------------------------------------------------------
%%% @doc Facade 入口：所有外部调用的统一入口
%%%
%%% 提供简洁的顶层 API，涵盖：
%%% - 构建 Kernel（工具 + LLM 服务）
%%% - 单次工具调用（invoke_tool）与单次 Chat Completion（chat）
%%%
%%% 注：ReAct 工具调用循环（LLM ↔ Tool 多轮编排）属于 Agent 层，见 beamai_agent。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai).

%% Kernel
-export([kernel/0, kernel/1]).

%% Tool
-export([tool/2, tool/3]).
-export([add_tool/2]).
-export([add_tools/2]).
-export([add_tool_module/2]).

%% Service (LLM)
-export([add_llm/3, add_llm/2]).

%% Filter（洋葱式过滤器）
-export([add_filter/2, add_filter/3]).

%% Memory（会话记忆）
-export([with_memory/2]).

%% Invoke
-export([invoke_tool/4]).
-export([chat/2, chat/3]).

%% Prompt
-export([render/2]).

%% Query
-export([tools/1, tools/2]).
-export([tools_by_tag/2]).

%% Context
-export([context/0, context/1]).

%%====================================================================
%% Kernel
%%====================================================================

%% @doc 创建空 Kernel（默认配置）
-spec kernel() -> beamai_kernel:kernel().
kernel() ->
    beamai_kernel:new().

%% @doc 创建 Kernel（自定义配置）
%%
%% @param Settings 配置项（如 #{max_tool_iterations => 5}）
-spec kernel(beamai_kernel:kernel_settings()) -> beamai_kernel:kernel().
kernel(Settings) ->
    beamai_kernel:new(Settings).

%%====================================================================
%% Tool
%%====================================================================

%% @doc 创建工具定义（名称 + 处理器）
-spec tool(binary(), beamai_tool:handler()) -> beamai_tool:tool_spec().
tool(Name, Handler) ->
    beamai_tool:new(Name, Handler).

%% @doc 创建工具定义（带额外选项，如 description、parameters、tag）
-spec tool(binary(), beamai_tool:handler(), map()) -> beamai_tool:tool_spec().
tool(Name, Handler, Opts) ->
    beamai_tool:new(Name, Handler, Opts).

%% @doc 注册单个工具到 Kernel
-spec add_tool(beamai_kernel:kernel(), beamai_tool:tool_spec()) -> beamai_kernel:kernel().
add_tool(Kernel, Tool) ->
    beamai_kernel:add_tool(Kernel, Tool).

%% @doc 批量注册工具到 Kernel
-spec add_tools(beamai_kernel:kernel(), [beamai_tool:tool_spec()]) -> beamai_kernel:kernel().
add_tools(Kernel, Tools) ->
    beamai_kernel:add_tools(Kernel, Tools).

%% @doc 从模块自动加载并注册工具
%%
%% 模块需实现 beamai_tool_behaviour，至少实现 tools/0 回调。
-spec add_tool_module(beamai_kernel:kernel(), module()) -> beamai_kernel:kernel().
add_tool_module(Kernel, Module) ->
    beamai_kernel:add_tool_module(Kernel, Module).

%%====================================================================
%% Service (LLM)
%%====================================================================

%% @doc 通过提供商和选项添加 LLM 服务
%%
%% 自动调用 beamai_chat_completion:create/2 创建配置并注册。
%%
%% 示例:
%%   K1 = beamai:add_llm(K0, anthropic, #{
%%       model => <<"claude-sonnet-4-20250514">>,
%%       api_key => os:getenv("ANTHROPIC_API_KEY")
%%   })
-spec add_llm(beamai_kernel:kernel(), beamai_chat_behaviour:provider(), map()) -> beamai_kernel:kernel().
add_llm(Kernel, Provider, Opts) ->
    LlmConfig = beamai_chat_completion:create(Provider, Opts),
    beamai_kernel:add_service(Kernel, LlmConfig).

%% @doc 使用预构建的 LLM 配置添加服务
%%
%% 示例:
%%   LLM = beamai_chat_completion:create(openai, #{model => <<"gpt-4">>, api_key => Key}),
%%   K1 = beamai:add_llm(K0, LLM)
-spec add_llm(beamai_kernel:kernel(), beamai_chat_behaviour:config()) -> beamai_kernel:kernel().
add_llm(Kernel, LlmConfig) ->
    beamai_kernel:add_service(Kernel, LlmConfig).

%%====================================================================
%% Filter（洋葱式过滤器）
%%====================================================================

%% @doc 注册已构建的 filter 到 Kernel
-spec add_filter(beamai_kernel:kernel(), beamai_filter:filter()) -> beamai_kernel:kernel().
add_filter(Kernel, Filter) ->
    beamai_kernel:add_filter(Kernel, Filter).

%% @doc 快捷创建并注册 filter（直接给 hook map）
%%
%% 一个 filter 含 around_chat/around_tool 任意子集，每个 around 用单个闭包
%% `fun(Req, FCtx, Next) -> Resp | {Resp, NewFCtx}` 包裹一次调用：前置改写
%% 请求、`Next(Req1)` 进入内层、后置改写响应；不调 Next 即短路。
%%
%% @param Kernel Kernel 实例
%% @param Name filter 名称
%% @param Hooks hook map（如 #{around_chat => F}）
%% @returns 更新后的 Kernel
-spec add_filter(beamai_kernel:kernel(), binary(), beamai_filter:hooks()) ->
    beamai_kernel:kernel().
add_filter(Kernel, Name, Hooks) ->
    Filter = beamai_filter:new(Name, Hooks),
    beamai_kernel:add_filter(Kernel, Filter).

%%====================================================================
%% Memory（会话记忆）
%%====================================================================

%% @doc 启用会话记忆：绑定 store 句柄并挂载 Memory 过滤器
%%
%% 启用后，凡 context 带 conversation_id 的 chat/3（invoke_chat）调用会按该 id
%% 自动存储与展开会话历史；无 conversation_id 时原样透传（单次无状态调用）。
%%
%% 示例:
%%   {ok, _} = beamai_chat_memory_ets:start_link(my_mem),
%%   K1 = beamai:with_memory(K0, beamai_chat_memory_ets:handle(my_mem))
%%
%% @param Kernel Kernel 实例
%% @param Store 会话存储句柄（beamai_chat_memory:handle/0）
%% @returns 更新后的 Kernel
-spec with_memory(beamai_kernel:kernel(), beamai_chat_memory:handle()) -> beamai_kernel:kernel().
with_memory(Kernel, Store) ->
    beamai_kernel:with_memory(Kernel, Store).

%%====================================================================
%% Invoke
%%====================================================================

%% @doc 调用 Kernel 中注册的工具
-spec invoke_tool(beamai_kernel:kernel(), binary(), beamai_tool:args(), beamai_context:t()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
invoke_tool(Kernel, ToolName, Args, Context) ->
    beamai_kernel:invoke_tool(Kernel, ToolName, Args, Context).

%% @doc 发送 Chat Completion 请求（默认选项）
-spec chat(beamai_kernel:kernel(), [map()]) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
chat(Kernel, Messages) ->
    chat(Kernel, Messages, #{}).

%% @doc 发送 Chat Completion 请求（自定义选项）
%%
%% 执行 around_chat 过滤器洋葱链（单次调用，不含工具循环）。
%% ReAct 工具循环请使用 beamai_agent。
-spec chat(beamai_kernel:kernel(), [map()], beamai_kernel:chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
chat(Kernel, Messages, Opts) ->
    beamai_kernel:invoke_chat(Kernel, Messages, Opts).

%%====================================================================
%% Prompt
%%====================================================================

%% @doc 渲染提示词模板
%%
%% 将 {{variable}} 占位符替换为 Vars 中对应的值。
%%
%% @param Template 模板字符串
%% @param Vars 变量 Map
%% @returns {ok, 渲染后的二进制} | {error, 原因}
-spec render(binary(), map()) -> {ok, binary()} | {error, term()}.
render(Template, Vars) ->
    Prompt = beamai_prompt:new(Template),
    beamai_prompt:render(Prompt, Vars).

%%====================================================================
%% Query
%%====================================================================

%% @doc 获取所有工具的 tool schema（默认 OpenAI 格式）
-spec tools(beamai_kernel:kernel()) -> [map()].
tools(Kernel) ->
    beamai_kernel:get_tool_schemas(Kernel).

%% @doc 获取所有工具的 tool schema（指定提供商格式）
-spec tools(beamai_kernel:kernel(), openai | anthropic | atom()) -> [map()].
tools(Kernel, Provider) ->
    beamai_kernel:get_tool_schemas(Kernel, Provider).

%% @doc 按标签查找工具
-spec tools_by_tag(beamai_kernel:kernel(), binary()) -> [beamai_tool:tool_spec()].
tools_by_tag(Kernel, Tag) ->
    beamai_kernel:get_tools_by_tag(Kernel, Tag).

%%====================================================================
%% Context
%%====================================================================

%% @doc 创建空执行上下文
-spec context() -> beamai_context:t().
context() ->
    beamai_context:new().

%% @doc 创建带初始变量的执行上下文
-spec context(map()) -> beamai_context:t().
context(Vars) ->
    beamai_context:new(Vars).
