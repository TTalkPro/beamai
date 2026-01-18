%%%-------------------------------------------------------------------
%%% @doc Middleware 集成节点模块
%%%
%%% 提供集成了 Middleware 的图节点，在原有节点功能基础上
%%% 添加 before/after 钩子调用。
%%%
%%% == 节点类型 ==
%%%
%%% - llm_node: LLM 调用节点（带 before_model/after_model）
%%% - tool_node: 工具执行节点（带 before_tools/after_tools）
%%% - agent_start_node: Agent 开始节点（触发 before_agent）
%%% - agent_end_node: Agent 结束节点（触发 after_agent）
%%%
%%% == 使用方式 ==
%%%
%%% ```erlang
%%% %% 创建带 Middleware 的 LLM 节点
%%% LLMNode = beamai_middleware_nodes:llm_node(LLMConfig, Middlewares),
%%%
%%% %% 创建带 Middleware 的工具节点
%%% ToolNode = beamai_middleware_nodes:tool_node(ToolHandlers, Middlewares).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware_nodes).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 节点创建 API
-export([
    llm_node/2,
    llm_node/3,
    tool_node/2,
    agent_start_node/1,
    agent_end_node/1
]).

%% Middleware 处理
-export([
    run_before_model/2,
    run_after_model/2,
    run_before_tools/2,
    run_after_tools/2,
    run_before_agent/2,
    run_after_agent/2
]).

%%====================================================================
%% 节点创建 API
%%====================================================================

%% @doc 创建带 Middleware 的 LLM 节点
%%
%% 使用默认状态键创建节点。
%%
%% @param LLMConfig LLM 配置
%% @param Middlewares Middleware 列表
%% @returns 节点函数
-spec llm_node(map(), list()) -> fun((map()) -> {ok, map()} | {error, term()}).
llm_node(LLMConfig, Middlewares) ->
    llm_node(LLMConfig, Middlewares, #{}).

%% @doc 创建带 Middleware 的 LLM 节点（带选项）
%%
%% 执行流程：
%%   1. 初始化 Middleware 链
%%   2. 运行 before_model 钩子
%%   3. 调用 LLM
%%   4. 运行 after_model 钩子
%%
%% @param LLMConfig LLM 配置
%% @param Middlewares Middleware 列表
%% @param Opts 节点选项（tools_key, messages_key, system_prompt_key）
%% @returns 节点函数
-spec llm_node(map(), list(), map()) -> fun((map()) -> {ok, map()} | {error, term()}).
llm_node(LLMConfig, Middlewares, Opts) ->
    %% 步骤 1：初始化 Middleware 链
    MwChain = beamai_middleware_runner:init(Middlewares),

    %% 步骤 2：提取配置键
    Keys = #{
        tools_key => maps:get(tools_key, Opts, tools),
        msgs_key => maps:get(messages_key, Opts, messages),
        prompt_key => maps:get(system_prompt_key, Opts, system_prompt)
    },

    fun(State) ->
        %% 步骤 3：存储 Middleware 链到状态
        State0 = graph:set(State, mw_chain, MwChain),

        %% 步骤 4：运行 before_model 钩子并处理结果
        handle_before_model_result(
            run_before_model(State0, MwChain),
            State0, LLMConfig, MwChain, Keys
        )
    end.

%% @doc 创建带 Middleware 的工具执行节点
%%
%% 执行流程：
%%   1. 运行 before_tools 钩子
%%   2. 执行工具调用
%%   3. 运行 after_tools 钩子
%%
%% @param ToolHandlers 工具处理器映射
%% @param Middlewares Middleware 列表
%% @returns 节点函数
-spec tool_node(map(), list()) -> fun((map()) -> {ok, map()}).
tool_node(ToolHandlers, Middlewares) ->
    %% 步骤 1：初始化 Middleware 链
    MwChain = beamai_middleware_runner:init(Middlewares),

    fun(State) ->
        %% 步骤 2：从状态获取或使用传入的 Middleware 链
        Chain = graph:get(State, mw_chain, MwChain),

        %% 步骤 3：运行 before_tools 钩子并处理结果
        handle_before_tools_result(
            run_before_tools(State, Chain),
            State, ToolHandlers, Chain
        )
    end.

%% @doc 创建 Agent 开始节点
%%
%% 触发 before_agent 钩子，初始化 Agent 执行。
%%
%% @param Middlewares Middleware 列表
%% @returns 节点函数
-spec agent_start_node(list()) -> fun((map()) -> {ok, map()}).
agent_start_node(Middlewares) ->
    MwChain = beamai_middleware_runner:init(Middlewares),

    fun(State) ->
        State0 = graph:set(State, mw_chain, MwChain),
        handle_agent_hook_result(run_before_agent(State0, MwChain), State0, before_agent)
    end.

%% @doc 创建 Agent 结束节点
%%
%% 触发 after_agent 钩子，完成 Agent 执行。
%%
%% @param Middlewares Middleware 列表
%% @returns 节点函数
-spec agent_end_node(list()) -> fun((map()) -> {ok, map()}).
agent_end_node(Middlewares) ->
    MwChain = beamai_middleware_runner:init(Middlewares),

    fun(State) ->
        Chain = graph:get(State, mw_chain, MwChain),
        %% after_agent 的 goto/halt 通常被忽略（已经结束了）
        case run_after_agent(State, Chain) of
            {ok, State1} -> {ok, State1};
            {_, State1} -> {ok, State1}
        end
    end.

%%====================================================================
%% Middleware 处理函数
%%====================================================================

%% @doc 运行 before_model 钩子
-spec run_before_model(map(), list()) -> beamai_middleware_runner:run_result().
run_before_model(State, MwChain) ->
    beamai_middleware_runner:run_hook(before_model, State, MwChain).

%% @doc 运行 after_model 钩子
-spec run_after_model(map(), list()) -> beamai_middleware_runner:run_result().
run_after_model(State, MwChain) ->
    beamai_middleware_runner:run_hook(after_model, State, MwChain).

%% @doc 运行 before_tools 钩子
-spec run_before_tools(map(), list()) -> beamai_middleware_runner:run_result().
run_before_tools(State, MwChain) ->
    beamai_middleware_runner:run_hook(before_tools, State, MwChain).

%% @doc 运行 after_tools 钩子
-spec run_after_tools(map(), list()) -> beamai_middleware_runner:run_result().
run_after_tools(State, MwChain) ->
    beamai_middleware_runner:run_hook(after_tools, State, MwChain).

%% @doc 运行 before_agent 钩子
-spec run_before_agent(map(), list()) -> beamai_middleware_runner:run_result().
run_before_agent(State, MwChain) ->
    beamai_middleware_runner:run_hook(before_agent, State, MwChain).

%% @doc 运行 after_agent 钩子
-spec run_after_agent(map(), list()) -> beamai_middleware_runner:run_result().
run_after_agent(State, MwChain) ->
    beamai_middleware_runner:run_hook(after_agent, State, MwChain).

%%====================================================================
%% 内部函数 - Middleware 钩子结果处理
%%====================================================================

%% @private 处理 before_model 钩子结果
%%
%% 根据钩子返回值决定下一步动作：
%%   - ok/update: 继续 LLM 调用
%%   - goto '__end__': 跳过 LLM，直接结束
%%   - goto tools: 跳过 LLM，直接执行工具
%%   - halt: 中止执行
%%   - interrupt: 暂停等待确认
-spec handle_before_model_result(term(), map(), map(), list(), map()) ->
    {ok, map()} | {error, term()}.
handle_before_model_result({ok, State1}, _State0, LLMConfig, MwChain, Keys) ->
    execute_llm_call(State1, LLMConfig, MwChain, Keys);

handle_before_model_result({goto, '__end__', State1}, _State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, graph:set(State1, finish_reason, <<"middleware_skip">>)};

handle_before_model_result({goto, tools, State1}, _State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, graph:set(State1, mw_goto_tools, true)};

handle_before_model_result({halt, Reason}, State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, set_halt_state(State0, Reason, <<"middleware_halt">>)};

handle_before_model_result({interrupt, Action, State1}, _State0, _LLMConfig, _MwChain, _Keys) ->
    {ok, set_interrupt_state(State1, Action, before_model)}.

%% @private 处理 before_tools 钩子结果
-spec handle_before_tools_result(term(), map(), map(), list()) -> {ok, map()}.
handle_before_tools_result({ok, State1}, _State, ToolHandlers, MwChain) ->
    execute_tools(State1, ToolHandlers, MwChain);

handle_before_tools_result({goto, '__end__', State1}, _State, _ToolHandlers, _MwChain) ->
    {ok, graph:set(State1, tool_calls, [])};

handle_before_tools_result({goto, model, State1}, _State, _ToolHandlers, _MwChain) ->
    {ok, graph:set(State1, mw_goto_model, true)};

handle_before_tools_result({halt, Reason}, State, _ToolHandlers, _MwChain) ->
    {ok, set_error_state(State, Reason)};

handle_before_tools_result({interrupt, Action, State1}, _State, _ToolHandlers, _MwChain) ->
    {ok, set_interrupt_state(State1, Action, before_tools)}.

%% @private 处理 after_model 钩子结果
-spec handle_after_model_result(term(), map()) -> {ok, map()}.
handle_after_model_result({ok, State1}, _State) ->
    {ok, State1};

handle_after_model_result({goto, Target, State1}, _State) ->
    {ok, graph:set(State1, mw_goto, Target)};

handle_after_model_result({halt, Reason}, State) ->
    {ok, set_error_state(State, Reason)};

handle_after_model_result({interrupt, Action, State1}, _State) ->
    {ok, set_interrupt_state(State1, Action, after_model)}.

%% @private 处理 after_tools 钩子结果
-spec handle_after_tools_result(term(), map()) -> {ok, map()}.
handle_after_tools_result({ok, State1}, _State) ->
    {ok, State1};

handle_after_tools_result({goto, tools, State1}, _State) ->
    {ok, graph:set(State1, mw_retry_tools, true)};

handle_after_tools_result({goto, Target, State1}, _State) ->
    {ok, graph:set(State1, mw_goto, Target)};

handle_after_tools_result({halt, Reason}, State) ->
    {ok, set_error_state(State, Reason)};

handle_after_tools_result({interrupt, Action, State1}, _State) ->
    {ok, set_interrupt_state(State1, Action, after_tools)}.

%% @private 处理 Agent 钩子结果（before_agent/after_agent）
%%
%% 处理的结果类型：
%%   - {ok, State}: 正常继续
%%   - {halt, Reason}: 中止执行
%%   - {interrupt, Action, State}: 暂停等待确认
%%   - {goto, Target, State}: 跳转（Agent 钩子中通常忽略）
-spec handle_agent_hook_result(term(), map(), atom()) -> {ok, map()}.
handle_agent_hook_result({ok, State1}, _State, _Hook) ->
    {ok, State1};

handle_agent_hook_result({halt, Reason}, State, _Hook) ->
    {ok, set_error_state(State, Reason)};

handle_agent_hook_result({interrupt, Action, State1}, _State, Hook) ->
    {ok, set_interrupt_state(State1, Action, Hook)};

handle_agent_hook_result({goto, _Target, State1}, _State, _Hook) ->
    %% Agent 钩子中的 goto 通常被忽略（Agent 已经在开始/结束状态）
    {ok, State1}.

%%====================================================================
%% 内部函数 - LLM 调用（最多2层嵌套）
%%====================================================================

%% @private 执行 LLM 调用（合并原 execute_llm_call 和 execute_llm_and_handle）
%%
%% 完整流程：获取数据 → 构建消息 → 调用LLM → 处理响应 → 运行钩子
-spec execute_llm_call(map(), map(), list(), map()) -> {ok, map()} | {error, term()}.
execute_llm_call(State, LLMConfig, MwChain, #{tools_key := ToolsKey,
                                               msgs_key := MsgsKey,
                                               prompt_key := PromptKey}) ->
    %% 获取数据并构建消息
    Messages = graph:get(State, MsgsKey, []),
    SystemPrompt = graph:get(State, PromptKey, <<>>),
    Tools = graph:get(State, ToolsKey, []),
    AllMsgs = beamai_message:prepend_system(SystemPrompt, Messages),

    %% 触发开始回调并调用 LLM
    invoke_callback(on_llm_start, [AllMsgs], State),
    LLMOpts = beamai_agent_utils:build_llm_opts(Tools, LLMConfig, State),

    case llm_client:chat(LLMConfig, AllMsgs, LLMOpts) of
        {ok, Response} ->
            invoke_callback(on_llm_end, [Response], State),
            State1 = process_llm_response(Response, Messages, State, MsgsKey),
            handle_after_model_result(run_after_model(State1, MwChain), State1);
        {error, Reason} ->
            invoke_callback(on_llm_error, [Reason], State),
            {ok, set_error_state(State, Reason)}
    end.

%% @private 处理 LLM 响应
%%
%% 提取响应内容，触发回调，构建 assistant 消息，更新状态。
-spec process_llm_response(map(), [map()], map(), atom()) -> map().
process_llm_response(Response, Messages, State, MsgsKey) ->
    %% 步骤 1：提取响应内容
    Content = maps:get(content, Response, null),
    ToolCalls = maps:get(tool_calls, Response, []),
    FinishReason = maps:get(finish_reason, Response, <<"stop">>),

    %% 步骤 2：触发回调
    beamai_agent_utils:invoke_text_callback(Content, State),
    beamai_agent_utils:invoke_action_callback(ToolCalls, Content, FinishReason, State),

    %% 步骤 3：构建 assistant 消息并更新状态
    AssistantMsg = beamai_agent_utils:build_assistant_message(Content, ToolCalls),
    NewMsgs = Messages ++ [AssistantMsg],

    BaseUpdates = [
        {MsgsKey, NewMsgs},
        {last_response, Response},
        {last_content, Content},
        {tool_calls, ToolCalls},
        {finish_reason, FinishReason}
    ],

    %% 步骤 4：同步 full_messages
    AllUpdates = beamai_agent_utils:append_to_full_messages(BaseUpdates, AssistantMsg, State),
    ?SET_STATE_MANY(State, AllUpdates).

%%====================================================================
%% 内部函数 - 工具执行（最多2层嵌套）
%%====================================================================

%% @private 执行工具（主入口）
-spec execute_tools(map(), map(), list()) -> {ok, map()}.
execute_tools(State, ToolHandlers, MwChain) ->
    %% 获取数据并保存原始调用
    ToolCalls = graph:get(State, tool_calls, []),
    Messages = graph:get(State, messages, []),
    Context = graph:get(State, context, #{}),
    State0 = graph:set(State, mw_original_tool_calls, ToolCalls),

    %% 执行所有工具并更新状态
    {Results, CtxUpdates} = execute_tool_calls(ToolCalls, ToolHandlers, Context, State0),
    ToolMessages = beamai_agent_utils:build_tool_messages(ToolCalls, Results),

    BaseUpdates = [
        {messages, Messages ++ ToolMessages},
        {tool_results, Results},
        {tool_calls, []},
        {context, maps:merge(Context, CtxUpdates)}
    ],
    AllUpdates = beamai_agent_utils:append_list_to_full_messages(BaseUpdates, ToolMessages, State0),
    State1 = ?SET_STATE_MANY(State0, AllUpdates),

    handle_after_tools_result(run_after_tools(State1, MwChain), State1).

%% @private 执行工具调用列表（使用内联单工具执行）
-spec execute_tool_calls([map()], map(), map(), map()) -> {[term()], map()}.
execute_tool_calls(ToolCalls, Handlers, Context, State) ->
    lists:foldl(fun(TC, {Results, CtxAcc}) ->
        %% 内联执行单个工具（减少函数调用层级）
        {Name, Args} = beamai_agent_utils:extract_tool_info(TC),
        invoke_callback(on_tool_start, [Name, Args], State),

        {Result, NewCtx} = case maps:get(Name, Handlers, undefined) of
            undefined ->
                Error = {unknown_tool, Name},
                invoke_callback(on_tool_error, [Name, Error], State),
                {{error, Error}, #{}};
            Handler ->
                ExecResult = safe_execute_tool(Handler, Args, CtxAcc),
                invoke_tool_result_callback(Name, element(1, ExecResult), State),
                ExecResult
        end,
        {Results ++ [Result], maps:merge(CtxAcc, NewCtx)}
    end, {[], Context}, ToolCalls).

%% @private 安全执行工具（内联处理器调用和结果处理）
-spec safe_execute_tool(function(), map(), map()) ->
    {{ok, binary()} | {error, term()}, map()}.
safe_execute_tool(Handler, Args, Context) ->
    try
        %% 内联调用处理器
        RawResult = case erlang:fun_info(Handler, arity) of
            {arity, 1} -> Handler(Args);
            {arity, 2} -> Handler(Args, Context);
            _ -> Handler(Args)
        end,
        %% 内联处理结果
        case RawResult of
            {Res, Ctx} when is_map(Ctx) -> {{ok, beamai_utils:to_binary(Res)}, Ctx};
            {ok, Res, Ctx} when is_map(Ctx) -> {{ok, beamai_utils:to_binary(Res)}, Ctx};
            {ok, Res} -> {{ok, beamai_utils:to_binary(Res)}, #{}};
            {error, ErrReason} -> {{error, ErrReason}, #{}};
            Res -> {{ok, beamai_utils:to_binary(Res)}, #{}}
        end
    catch
        Class:Reason:_Stack -> {{error, {Class, Reason}}, #{}}
    end.

%% @private 触发工具结果回调
-spec invoke_tool_result_callback(binary(), {ok, binary()} | {error, term()}, map()) -> ok.
invoke_tool_result_callback(Name, {ok, Result}, State) ->
    invoke_callback(on_tool_end, [Name, Result], State);
invoke_tool_result_callback(Name, {error, Reason}, State) ->
    invoke_callback(on_tool_error, [Name, Reason], State).

%%====================================================================
%% 回调辅助
%%====================================================================

%% @private 调用回调
-spec invoke_callback(atom(), list(), map()) -> ok.
invoke_callback(CallbackName, Args, State) ->
    Callbacks = graph:get(State, callbacks, #{}),
    Meta = graph:get(State, callback_meta, #{}),
    beamai_callback_utils:invoke(CallbackName, Args, Callbacks, Meta).

%%====================================================================
%% 状态更新辅助
%%====================================================================

%% @private 设置错误状态
-spec set_error_state(map(), term()) -> map().
set_error_state(State, Reason) ->
    ?SET_STATE_MANY(State, [{error, Reason}, {status, error}]).

%% @private 设置中止状态
-spec set_halt_state(map(), term(), binary()) -> map().
set_halt_state(State, Reason, FinishReason) ->
    ?SET_STATE_MANY(State, [
        {error, Reason},
        {status, error},
        {finish_reason, FinishReason}
    ]).

%% @private 设置中断状态
-spec set_interrupt_state(map(), map(), atom()) -> map().
set_interrupt_state(State, Action, InterruptPoint) ->
    ?SET_STATE_MANY(State, [
        {interrupted, true},
        {pending_action, Action},
        {interrupt_point, InterruptPoint}
    ]).
