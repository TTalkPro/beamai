%%%-------------------------------------------------------------------
%%% @doc Deep Agent 图节点函数模块
%%%
%%% 定义图执行引擎中的各类节点函数。每个节点是一个纯函数，
%%% 接收状态并返回新状态。
%%%
%%% 节点类型：
%%% - llm_node: 调用大语言模型（委托给 beamai_deepagent_llm_node）
%%% - tool_node: 执行工具调用（委托给 beamai_deepagent_tool_executor）
%%% - reflect_node: 执行反思分析（本模块实现）
%%% - aggregate_node: 聚合并行任务结果（委托给 beamai_deepagent_aggregator_node）
%%% - task_executor: 执行子任务（委托给 beamai_deepagent_task_node）
%%%
%%% 模块拆分说明：
%%% - beamai_deepagent_llm_node: LLM 调用相关逻辑
%%% - beamai_deepagent_tool_executor: 工具执行相关逻辑
%%% - beamai_deepagent_aggregator_node: 聚合节点逻辑
%%% - beamai_deepagent_task_node: 任务执行逻辑
%%% - beamai_deepagent_nodes: 反思节点 + 公共 API（本模块）
%%%
%%% 设计原则：
%%% - 纯函数：节点函数无副作用
%%% - 单一职责：每个节点只做一件事
%%% - 委托模式：复杂节点委托给专门模块
%%% - 错误隔离：异常被捕获并包装
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_nodes).

%% 导入工具模块
-import(beamai_deepagent_utils, [
    state_get/2, state_get/3, state_set/3,
    add_trace/3
]).

%% 导入消息处理模块
-import(beamai_deepagent_messages, [
    append_reflection_message/2
]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 节点构造器（LLM 和工具节点委托给子模块）
-export([
    make_llm_node/1,
    make_tool_node/1,
    make_reflect_node/1,
    make_aggregate_node/0,
    make_task_executor/1
]).

%% 辅助函数（委托给子模块）
-export([
    build_tool_specs/2,
    inject_trace_context/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type node_fun() :: fun((graph_state:state()) ->
    {ok, graph_state:state()} | {error, term()}).

%%====================================================================
%% LLM 节点（委托给 beamai_deepagent_llm_node）
%%====================================================================

%% @doc 创建 LLM 调用节点
-spec make_llm_node(map()) -> node_fun().
make_llm_node(Config) ->
    beamai_deepagent_llm_node:create(Config).

%%====================================================================
%% 工具执行节点（委托给 beamai_deepagent_tool_executor）
%%====================================================================

%% @doc 创建工具执行节点
-spec make_tool_node(map()) -> node_fun().
make_tool_node(Config) ->
    beamai_deepagent_tool_executor:create(Config).

%%====================================================================
%% 反思节点
%%====================================================================

%% @doc 创建反思节点
%%
%% 功能：分析执行进展并调整策略
%% 条件：仅当 reflection_enabled 且 should_reflect 为真时执行
-spec make_reflect_node(map()) -> node_fun().
make_reflect_node(Config) ->
    fun(State) -> execute_reflect_node(Config, State) end.

%%====================================================================
%% 聚合节点
%%====================================================================

%% @doc 创建聚合节点
%%
%% 功能：合并并行子任务的执行结果
%% 场景：接收来自 fan_out 的单个结果或聚合所有结果
%% 实现：委托给 beamai_deepagent_aggregator_node 模块
-spec make_aggregate_node() -> node_fun().
make_aggregate_node() ->
    beamai_deepagent_aggregator_node:make_aggregate_node().

%%====================================================================
%% 任务执行节点
%%====================================================================

%% @doc 创建任务执行节点
%%
%% 功能：执行单个子任务（递归调用 beamai_deepagent）
%% 限制：受 max_depth 深度限制
%% 实现：委托给 beamai_deepagent_task_node 模块
-spec make_task_executor(map()) -> node_fun().
make_task_executor(Config) ->
    beamai_deepagent_task_node:make_task_executor(Config).

%%====================================================================
%% 辅助函数（委托给子模块）
%%====================================================================

%% @doc 构建工具规格列表
-spec build_tool_specs(map(), graph_state:state()) -> [map()].
build_tool_specs(Config, State) ->
    beamai_deepagent_llm_node:build_tool_specs(Config, State).

%% @doc 注入轨迹上下文到消息
-spec inject_trace_context([map()], beamai_deepagent_trace:t()) -> [map()].
inject_trace_context(Messages, Trace) ->
    beamai_deepagent_llm_node:inject_trace_context(Messages, Trace).

%%====================================================================
%% 私有函数 - 反思节点执行
%%====================================================================

%% @private 反思节点执行逻辑
-spec execute_reflect_node(map(), graph_state:state()) -> {ok, graph_state:state()}.
execute_reflect_node(Config, State) ->
    ShouldExecute = should_execute_reflection(Config, State),
    case ShouldExecute of
        false -> {ok, State};
        true -> {ok, perform_reflection(State)}
    end.

%% @private 判断是否应执行反思
-spec should_execute_reflection(map(), graph_state:state()) -> boolean().
should_execute_reflection(Config, State) ->
    ReflectionEnabled = maps:get(reflection_enabled, Config, true),
    ShouldReflect = state_get(State, should_reflect, false),
    ReflectionEnabled andalso ShouldReflect.

%% @private 执行反思逻辑
-spec perform_reflection(graph_state:state()) -> graph_state:state().
perform_reflection(State) ->
    Trace = state_get(State, trace, beamai_deepagent_trace:new()),
    Plan = state_get(State, plan, undefined),

    Reflection = generate_reflection(Trace, Plan),
    State1 = add_trace(State, reflection, Reflection),
    State2 = state_set(State1, should_reflect, false),
    append_reflection_message(State2, Reflection).

%%====================================================================
%% 私有函数 - 反思生成
%%====================================================================

%% @private 生成反思内容
-spec generate_reflection(beamai_deepagent_trace:t(), beamai_deepagent_plan:t() | undefined) -> binary().
generate_reflection(Trace, Plan) ->
    TraceText = beamai_deepagent_trace:format(beamai_deepagent_trace:get_recent(Trace, 10)),
    PlanText = format_plan_status(Plan),
    <<"Reflection:\nRecent trace:\n", TraceText/binary,
      "\nPlan status:\n", PlanText/binary>>.

%% @private 格式化计划状态
-spec format_plan_status(beamai_deepagent_plan:t() | undefined) -> binary().
format_plan_status(undefined) ->
    <<"No plan created yet.">>;
format_plan_status(Plan) ->
    beamai_deepagent_plan:format_status(Plan).
