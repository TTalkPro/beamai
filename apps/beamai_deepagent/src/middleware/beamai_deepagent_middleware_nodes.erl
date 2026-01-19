%%%-------------------------------------------------------------------
%%% @doc Deep Agent Middleware 集成节点模块
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
%%% LLMNode = beamai_deepagent_middleware_nodes:make_llm_node(Config, Middlewares),
%%%
%%% %% 创建带 Middleware 的工具节点
%%% ToolNode = beamai_deepagent_middleware_nodes:make_tool_node(Config, Middlewares).
%%% ```
%%%
%%% 复用 beamai_agent 的 Middleware 系统：
%%% - beamai_middleware: Middleware 行为定义
%%% - beamai_middleware_runner: Middleware 链运行器
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_middleware_nodes).

%% 导入工具模块
-import(beamai_deepagent_utils, [
    state_get/2, state_get/3, state_set/3,
    add_trace/3
]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 节点构造器
-export([
    make_llm_node/2,
    make_tool_node/2,
    make_agent_start_node/1,
    make_agent_end_node/1
]).

%% Middleware 运行函数
-export([
    run_before_model/2,
    run_after_model/2,
    run_before_tools/2,
    run_after_tools/2,
    run_before_agent/2,
    run_after_agent/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type node_fun() :: fun((graph_state:state()) ->
    {ok, graph_state:state()} | {error, term()}).

-type middleware_chain() :: beamai_middleware_runner:middleware_chain().

%%====================================================================
%% 节点构造器
%%====================================================================

%% @doc 创建带 Middleware 的 LLM 调用节点
%%
%% 执行流程：
%%   1. 运行 before_model 钩子
%%   2. 调用 LLM（使用 beamai_deepagent_llm_node）
%%   3. 运行 after_model 钩子
%%
%% @param Config DeepAgent 配置
%% @param Middlewares Middleware 列表
%% @returns 节点函数
-spec make_llm_node(map(), list()) -> node_fun().
make_llm_node(Config, Middlewares) ->
    %% 初始化 Middleware 链
    MwChain = beamai_middleware_runner:init(Middlewares),

    fun(State) ->
        %% 存储 Middleware 链到状态
        State0 = state_set(State, mw_chain, MwChain),

        %% 运行 before_model 钩子并处理结果
        handle_before_model_result(
            run_before_model(State0, MwChain),
            Config, MwChain
        )
    end.

%% @doc 创建带 Middleware 的工具执行节点
%%
%% 执行流程：
%%   1. 运行 before_tools 钩子
%%   2. 执行工具调用（使用 beamai_deepagent_tool_executor）
%%   3. 运行 after_tools 钩子
%%
%% @param Config DeepAgent 配置
%% @param Middlewares Middleware 列表
%% @returns 节点函数
-spec make_tool_node(map(), list()) -> node_fun().
make_tool_node(Config, Middlewares) ->
    %% 初始化 Middleware 链
    MwChain = beamai_middleware_runner:init(Middlewares),

    fun(State) ->
        %% 从状态获取或使用传入的 Middleware 链
        Chain = state_get(State, mw_chain, MwChain),

        %% 运行 before_tools 钩子并处理结果
        handle_before_tools_result(
            run_before_tools(State, Chain),
            Config, Chain
        )
    end.

%% @doc 创建 Agent 开始节点
%%
%% 触发 before_agent 钩子，初始化 Agent 执行。
%%
%% @param Middlewares Middleware 列表
%% @returns 节点函数
-spec make_agent_start_node(list()) -> node_fun().
make_agent_start_node(Middlewares) ->
    MwChain = beamai_middleware_runner:init(Middlewares),

    fun(State) ->
        State0 = state_set(State, mw_chain, MwChain),
        handle_agent_hook_result(run_before_agent(State0, MwChain), before_agent)
    end.

%% @doc 创建 Agent 结束节点
%%
%% 触发 after_agent 钩子，完成 Agent 执行。
%%
%% @param Middlewares Middleware 列表
%% @returns 节点函数
-spec make_agent_end_node(list()) -> node_fun().
make_agent_end_node(Middlewares) ->
    MwChain = beamai_middleware_runner:init(Middlewares),

    fun(State) ->
        Chain = state_get(State, mw_chain, MwChain),
        %% after_agent 的 goto/halt 通常被忽略（已经结束了）
        case run_after_agent(State, Chain) of
            {ok, State1} -> {ok, State1};
            {_, State1} -> {ok, State1}
        end
    end.

%%====================================================================
%% Middleware 运行函数
%%====================================================================

%% @doc 运行 before_model 钩子
-spec run_before_model(graph_state:state(), middleware_chain()) ->
    beamai_middleware_runner:run_result().
run_before_model(State, MwChain) ->
    beamai_middleware_runner:run_hook(before_model, to_map(State), MwChain).

%% @doc 运行 after_model 钩子
-spec run_after_model(graph_state:state(), middleware_chain()) ->
    beamai_middleware_runner:run_result().
run_after_model(State, MwChain) ->
    beamai_middleware_runner:run_hook(after_model, to_map(State), MwChain).

%% @doc 运行 before_tools 钩子
-spec run_before_tools(graph_state:state(), middleware_chain()) ->
    beamai_middleware_runner:run_result().
run_before_tools(State, MwChain) ->
    beamai_middleware_runner:run_hook(before_tools, to_map(State), MwChain).

%% @doc 运行 after_tools 钩子
-spec run_after_tools(graph_state:state(), middleware_chain()) ->
    beamai_middleware_runner:run_result().
run_after_tools(State, MwChain) ->
    beamai_middleware_runner:run_hook(after_tools, to_map(State), MwChain).

%% @doc 运行 before_agent 钩子
-spec run_before_agent(graph_state:state(), middleware_chain()) ->
    beamai_middleware_runner:run_result().
run_before_agent(State, MwChain) ->
    beamai_middleware_runner:run_hook(before_agent, to_map(State), MwChain).

%% @doc 运行 after_agent 钩子
-spec run_after_agent(graph_state:state(), middleware_chain()) ->
    beamai_middleware_runner:run_result().
run_after_agent(State, MwChain) ->
    beamai_middleware_runner:run_hook(after_agent, to_map(State), MwChain).

%%====================================================================
%% 内部函数 - 钩子结果处理
%%====================================================================

%% @private 处理 before_model 钩子结果
%%
%% 根据钩子返回值决定下一步动作：
%%   - ok/update: 继续 LLM 调用
%%   - goto '__end__': 跳过 LLM，直接结束
%%   - goto tools: 跳过 LLM，直接执行工具
%%   - halt: 中止执行
%%   - interrupt: 暂停等待确认
-spec handle_before_model_result(term(), map(), middleware_chain()) ->
    {ok, graph_state:state()} | {error, term()}.
handle_before_model_result({ok, StateMap}, Config, MwChain) ->
    %% 继续执行 LLM 调用
    execute_llm_with_middleware(from_map(StateMap), Config, MwChain);

handle_before_model_result({goto, '__end__', StateMap}, _Config, _MwChain) ->
    %% 跳过 LLM，直接结束
    State = from_map(StateMap),
    {ok, state_set(State, finish_reason, <<"middleware_skip">>)};

handle_before_model_result({goto, tools, StateMap}, _Config, _MwChain) ->
    %% 跳过 LLM，直接执行工具
    State = from_map(StateMap),
    {ok, state_set(State, mw_goto_tools, true)};

handle_before_model_result({halt, Reason}, _Config, _MwChain) ->
    %% 中止执行
    {error, {middleware_halt, Reason}};

handle_before_model_result({interrupt, Action, StateMap}, _Config, _MwChain) ->
    %% 暂停等待确认
    State = from_map(StateMap),
    {ok, set_interrupt_state(State, Action, before_model)}.

%% @private 处理 before_tools 钩子结果
-spec handle_before_tools_result(term(), map(), middleware_chain()) ->
    {ok, graph_state:state()}.
handle_before_tools_result({ok, StateMap}, Config, MwChain) ->
    %% 继续执行工具
    execute_tools_with_middleware(from_map(StateMap), Config, MwChain);

handle_before_tools_result({goto, '__end__', StateMap}, _Config, _MwChain) ->
    %% 跳过工具执行
    State = from_map(StateMap),
    {ok, state_set(State, pending_tools, [])};

handle_before_tools_result({goto, model, StateMap}, _Config, _MwChain) ->
    %% 跳转到 LLM
    State = from_map(StateMap),
    {ok, state_set(State, mw_goto_model, true)};

handle_before_tools_result({halt, Reason}, _Config, _MwChain) ->
    %% 中止执行
    {error, {middleware_halt, Reason}};

handle_before_tools_result({interrupt, Action, StateMap}, _Config, _MwChain) ->
    %% 暂停等待确认
    State = from_map(StateMap),
    {ok, set_interrupt_state(State, Action, before_tools)}.

%% @private 处理 after_model 钩子结果
-spec handle_after_model_result(term(), graph_state:state()) ->
    {ok, graph_state:state()}.
handle_after_model_result({ok, StateMap}, _State) ->
    {ok, from_map(StateMap)};

handle_after_model_result({goto, Target, StateMap}, _State) ->
    State = from_map(StateMap),
    {ok, state_set(State, mw_goto, Target)};

handle_after_model_result({halt, Reason}, State) ->
    {ok, set_error_state(State, Reason)};

handle_after_model_result({interrupt, Action, StateMap}, _State) ->
    State = from_map(StateMap),
    {ok, set_interrupt_state(State, Action, after_model)}.

%% @private 处理 after_tools 钩子结果
-spec handle_after_tools_result(term(), graph_state:state()) ->
    {ok, graph_state:state()}.
handle_after_tools_result({ok, StateMap}, _State) ->
    {ok, from_map(StateMap)};

handle_after_tools_result({goto, tools, StateMap}, _State) ->
    %% 重试工具执行
    State = from_map(StateMap),
    {ok, state_set(State, mw_retry_tools, true)};

handle_after_tools_result({goto, Target, StateMap}, _State) ->
    State = from_map(StateMap),
    {ok, state_set(State, mw_goto, Target)};

handle_after_tools_result({halt, Reason}, State) ->
    {ok, set_error_state(State, Reason)};

handle_after_tools_result({interrupt, Action, StateMap}, _State) ->
    State = from_map(StateMap),
    {ok, set_interrupt_state(State, Action, after_tools)}.

%% @private 处理 Agent 钩子结果（before_agent/after_agent）
-spec handle_agent_hook_result(term(), atom()) -> {ok, graph_state:state()}.
handle_agent_hook_result({ok, StateMap}, _Hook) ->
    {ok, from_map(StateMap)};

handle_agent_hook_result({halt, Reason}, _Hook) ->
    {error, {middleware_halt, Reason}};

handle_agent_hook_result({interrupt, Action, StateMap}, Hook) ->
    State = from_map(StateMap),
    {ok, set_interrupt_state(State, Action, Hook)};

handle_agent_hook_result({goto, _Target, StateMap}, _Hook) ->
    %% Agent 钩子中的 goto 通常被忽略
    {ok, from_map(StateMap)}.

%%====================================================================
%% 内部函数 - LLM 执行
%%====================================================================

%% @private 执行 LLM 调用（带 Middleware）
-spec execute_llm_with_middleware(graph_state:state(), map(), middleware_chain()) ->
    {ok, graph_state:state()} | {error, term()}.
execute_llm_with_middleware(State, Config, MwChain) ->
    %% 使用原有的 LLM 节点执行逻辑
    LLMNodeFun = beamai_deepagent_llm_node:create(Config),
    case LLMNodeFun(State) of
        {ok, State1} ->
            %% 运行 after_model 钩子
            handle_after_model_result(run_after_model(State1, MwChain), State1);
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数 - 工具执行
%%====================================================================

%% @private 执行工具调用（带 Middleware）
-spec execute_tools_with_middleware(graph_state:state(), map(), middleware_chain()) ->
    {ok, graph_state:state()}.
execute_tools_with_middleware(State, Config, MwChain) ->
    %% 使用原有的工具节点执行逻辑
    ToolNodeFun = beamai_deepagent_tool_executor:create(Config),
    case ToolNodeFun(State) of
        {ok, State1} ->
            %% 运行 after_tools 钩子
            handle_after_tools_result(run_after_tools(State1, MwChain), State1);
        {error, Reason} ->
            {ok, set_error_state(State, Reason)}
    end.

%%====================================================================
%% 内部函数 - 状态转换
%%====================================================================

%% @private 将 graph_state 转换为 map（供 middleware_runner 使用）
-spec to_map(graph_state:state()) -> map().
to_map(State) ->
    graph_state:to_map(State).

%% @private 将 map 转换为 graph_state
-spec from_map(map()) -> graph_state:state().
from_map(Map) ->
    graph_state:from_map(Map).

%%====================================================================
%% 内部函数 - 状态更新辅助
%%====================================================================

%% @private 设置错误状态
-spec set_error_state(graph_state:state(), term()) -> graph_state:state().
set_error_state(State, Reason) ->
    State1 = state_set(State, error, Reason),
    state_set(State1, status, error).

%% @private 设置中断状态
-spec set_interrupt_state(graph_state:state(), map(), atom()) -> graph_state:state().
set_interrupt_state(State, Action, InterruptPoint) ->
    State1 = state_set(State, interrupted, true),
    State2 = state_set(State1, pending_action, Action),
    state_set(State2, interrupt_point, InterruptPoint).
