%%%-------------------------------------------------------------------
%%% @doc Deep Agent 路由模块
%%%
%%% 实现图执行中的条件边路由逻辑。路由函数是纯函数，
%%% 根据当前状态决定下一个执行节点。
%%%
%%% 主要功能：
%%% - after_llm: LLM 调用后的路由决策
%%% - after_tool: 工具执行后的路由决策
%%% - 依赖分析: 分析计划步骤的依赖关系
%%% - 并行分发: 创建 fan_out 消息实现并行执行
%%%
%%% 设计原则：
%%% - 纯函数：路由决策无副作用
%%% - 单一出口：每个路由函数返回单一类型
%%% - 可测试性：所有逻辑可独立测试
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_router).

%% 导入工具模块
-import(beamai_deepagent_utils, [state_get/2, state_get/3]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 路由函数
-export([
    after_llm/1,
    after_tool/1,
    should_continue/1
]).

%% 依赖分析（委托给 beamai_deepagent_dependencies）
-export([
    analyze_dependencies/1,
    topological_layers/1
]).

%% 并行分发
-export([
    fan_out_subtasks/2,
    fan_out_plan_steps/2
]).

%%====================================================================
%% LLM 后路由
%%====================================================================

%% @doc LLM 调用后的路由决策
%%
%% 决策逻辑：
%% - 有工具调用 -> tool_node（执行工具）
%% - 有最终响应 -> __end__（结束执行）
%% - 其他情况 -> llm_node（继续对话）
-spec after_llm(graph_state:state()) -> atom() | [graph_send:send()].
after_llm(State) ->
    PendingTools = state_get(State, pending_tools, []),
    FinalResponse = state_get(State, final_response, undefined),
    route_after_llm(PendingTools, FinalResponse).

%%====================================================================
%% 工具后路由
%%====================================================================

%% @doc 工具执行后的路由决策
%%
%% 决策逻辑（按优先级）：
%% 1. 创建计划 -> 可能触发并行执行
%% 2. 派生子任务 -> fan_out 并行执行
%% 3. 需要反思 -> reflect_node
%% 4. 计划完成 -> __end__
%% 5. 其他情况 -> llm_node（继续循环）
-spec after_tool(graph_state:state()) -> atom() | [graph_send:send()].
after_tool(State) ->
    ToolResults = state_get(State, tool_results, []),
    dispatch_after_tool(ToolResults, State).

%%====================================================================
%% 继续执行判断
%%====================================================================

%% @doc 检查是否应该继续执行
-spec should_continue(graph_state:state()) -> boolean().
should_continue(State) ->
    FinalResponse = state_get(State, final_response, undefined),
    case FinalResponse of
        undefined -> check_plan_incomplete(State);
        _ -> false
    end.

%%====================================================================
%% 依赖分析 API（委托给 beamai_deepagent_dependencies）
%%====================================================================

%% @doc 分析步骤依赖关系，返回可并行执行的分层
%%
%% 委托给 beamai_deepagent_dependencies:analyze/1
-spec analyze_dependencies([map()]) -> [[map()]].
analyze_dependencies(Steps) ->
    beamai_deepagent_dependencies:analyze(Steps).

%% @doc 拓扑排序分层
%%
%% 委托给 beamai_deepagent_dependencies:topological_layers/1
-spec topological_layers(map()) -> [[map()]].
topological_layers(Graph) ->
    beamai_deepagent_dependencies:topological_layers(Graph).

%%====================================================================
%% 并行分发 API
%%====================================================================

%% @doc 并行分发子任务
%%
%% 将多个子任务通过 fan_out 分发到 task_executor 节点并行执行。
-spec fan_out_subtasks(graph_state:state(), [map()]) -> [graph_send:send()].
fan_out_subtasks(State, Tasks) ->
    Depth = state_get(State, depth, 0),
    graph_send:fan_out_indexed(task_executor, Tasks, make_task_mapper(Depth)).

%% @doc 并行分发计划步骤
%%
%% 将计划中的多个步骤通过 fan_out 分发并行执行。
-spec fan_out_plan_steps(graph_state:state(), [map()]) -> [graph_send:send()].
fan_out_plan_steps(State, Steps) ->
    Depth = state_get(State, depth, 0),
    graph_send:fan_out(task_executor, Steps, make_step_mapper(Depth)).

%%====================================================================
%% 私有函数 - LLM 后路由
%%====================================================================

%% @private LLM 后路由分派
-spec route_after_llm([map()], term()) -> atom().
route_after_llm([], undefined) -> llm_node;      %% 继续对话
route_after_llm([], _Response) -> '__end__';     %% 有响应则结束
route_after_llm(_Tools, _) -> tool_node.         %% 有工具则执行

%%====================================================================
%% 私有函数 - 工具后路由
%%====================================================================

%% @private 分派工具后路由
%% 委托给 beamai_deepagent_result_analyzer 进行分析
-spec dispatch_after_tool([map()], graph_state:state()) -> atom() | [graph_send:send()].
dispatch_after_tool(Results, State) ->
    case beamai_deepagent_result_analyzer:analyze(Results) of
        {create_plan, PlanMap} -> handle_plan_created(State, PlanMap);
        {spawn_subtasks, Tasks} -> fan_out_subtasks(State, Tasks);
        needs_reflection -> reflect_node;
        continue -> check_plan_completion(State)
    end.

%%====================================================================
%% 私有函数 - 计划处理
%%====================================================================

%% @private 处理计划创建后的路由
-spec handle_plan_created(graph_state:state(), map()) -> atom() | [graph_send:send()].
handle_plan_created(State, PlanMap) ->
    Plan = beamai_deepagent_plan:from_map(PlanMap),
    Steps = beamai_deepagent_plan:get_steps(Plan),
    ParallelGroups = analyze_dependencies(Steps),
    route_by_parallel_groups(State, ParallelGroups).

%% @private 根据并行分组决定路由
-spec route_by_parallel_groups(graph_state:state(), [[map()]]) -> atom() | [graph_send:send()].
route_by_parallel_groups(_State, []) ->
    llm_node;
route_by_parallel_groups(_State, [[_Single]]) ->
    llm_node;
route_by_parallel_groups(State, [FirstGroup | _]) when length(FirstGroup) > 1 ->
    ParallelSteps = filter_parallel_steps(FirstGroup),
    case ParallelSteps of
        [] -> llm_node;
        Steps -> fan_out_plan_steps(State, Steps)
    end;
route_by_parallel_groups(_State, _) ->
    llm_node.

%% @private 筛选需要并行执行的步骤
-spec filter_parallel_steps([map()]) -> [map()].
filter_parallel_steps(Steps) ->
    [S || S <- Steps, maps:get(requires_subtask, S, false) =:= true].

%% @private 检查计划是否未完成
-spec check_plan_incomplete(graph_state:state()) -> boolean().
check_plan_incomplete(State) ->
    Plan = state_get(State, plan, undefined),
    case Plan of
        undefined -> true;
        P -> not beamai_deepagent_plan:is_complete(P)
    end.

%% @private 检查计划完成状态
-spec check_plan_completion(graph_state:state()) -> atom().
check_plan_completion(State) ->
    Plan = state_get(State, plan, undefined),
    case Plan of
        undefined -> llm_node;
        P -> plan_completion_route(beamai_deepagent_plan:is_complete(P))
    end.

%% @private 根据计划完成状态返回路由
-spec plan_completion_route(boolean()) -> atom().
plan_completion_route(true) -> '__end__';
plan_completion_route(false) -> llm_node.

%%====================================================================
%% 私有函数 - 并行分发辅助
%%====================================================================

%% @private 创建任务映射函数
-spec make_task_mapper(non_neg_integer()) -> fun((non_neg_integer(), map()) -> map()).
make_task_mapper(Depth) ->
    fun(Idx, Task) ->
        #{
            task_id => maps:get(task_id, Task, integer_to_binary(Idx)),
            task_input => extract_task_input(Task),
            task_index => Idx,
            depth => Depth
        }
    end.

%% @private 创建步骤映射函数
-spec make_step_mapper(non_neg_integer()) -> fun((map()) -> map()).
make_step_mapper(Depth) ->
    fun(Step) ->
        #{
            task_id => integer_to_binary(maps:get(id, Step)),
            task_input => maps:get(description, Step),
            step_data => Step,
            depth => Depth
        }
    end.

%% @private 提取任务输入
-spec extract_task_input(map()) -> binary().
extract_task_input(Task) ->
    maps:get(input, Task, maps:get(description, Task, <<>>)).
