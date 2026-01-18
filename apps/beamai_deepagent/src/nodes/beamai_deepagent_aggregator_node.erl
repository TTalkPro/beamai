%%%-------------------------------------------------------------------
%%% @doc Deep Agent 聚合节点模块
%%%
%%% 负责聚合并行子任务的执行结果。
%%%
%%% 核心功能：
%%% - 聚合多个子任务结果
%%% - 更新计划状态
%%% - 添加执行轨迹
%%%
%%% 设计原则：
%%% - 纯函数：无副作用
%%% - 单一职责：只负责聚合逻辑
%%% - 可测试：易于单元测试
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_aggregator_node).

%% 导入公共工具
-import(beamai_deepagent_utils, [
    state_get/2, state_get/3, state_set/3,
    add_trace/3
]).

%% 导入计划模块
-import(beamai_deepagent_plan, [
    update_subtask_status/3,
    mark_subtask_completed/2
]).

%% API
-export([make_aggregate_node/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 创建聚合节点
%%
%% 返回一个节点函数，用于聚合并行子任务的结果。
-spec make_aggregate_node() -> fun((graph_state:state()) -> {ok, graph_state:state()}).
make_aggregate_node() ->
    fun execute_aggregate/1.

%%====================================================================
%% 节点执行
%%====================================================================

%% @private 执行聚合节点
-spec execute_aggregate(graph_state:state()) -> {ok, graph_state:state()}.
execute_aggregate(State) ->
    %% 1. 获取子任务结果
    Results = state_get(State, subtask_results, []),

    %% 2. 聚合结果
    AggregatedResult = aggregate_results(Results),

    %% 3. 更新计划
    State1 = update_plan_with_results(State, Results),

    %% 4. 添加轨迹
    State2 = add_trace(State1, subtasks_aggregated, AggregatedResult),

    %% 5. 清理子任务结果
    State3 = state_set(State2, subtask_results, []),

    %% 6. 保存聚合结果
    State4 = state_set(State3, aggregated_result, AggregatedResult),

    {ok, State4}.

%%====================================================================
%% 私有函数
%%====================================================================

%% @private 聚合多个子任务结果
%%
%% 将所有子任务的结果合并为一个结构化结果。
-spec aggregate_results([map()]) -> map().
aggregate_results(Results) ->
    #{
        count => length(Results),
        successes => count_successes(Results),
        failures => count_failures(Results),
        outputs => extract_outputs(Results)
    }.

%% @private 统计成功数量
-spec count_successes([map()]) -> non_neg_integer().
count_successes(Results) ->
    length([R || R <- Results, is_success_result(R)]).

%% @private 统计失败数量
-spec count_failures([map()]) -> non_neg_integer().
count_failures(Results) ->
    length(Results) - count_successes(Results).

%% @private 判断是否为成功结果
-spec is_success_result(map()) -> boolean().
is_success_result(Result) ->
    case maps:get(success, Result, true) of
        true -> true;
        _ -> false
    end.

%% @private 提取所有输出
-spec extract_outputs([map()]) -> [term()].
extract_outputs(Results) ->
    [maps:get(output, R, undefined) || R <- Results].

%% @private 使用结果更新计划
%%
%% 将子任务结果更新到计划中。
-spec update_plan_with_results(graph_state:state(), [map()]) -> graph_state:state().
update_plan_with_results(State, Results) ->
    Plan = state_get(State, plan, undefined),
    case Plan of
        undefined ->
            State;
        _ ->
            lists:foldl(
                fun(Result, AccState) ->
                    TaskId = maps:get(task_id, Result),
                    Status = result_to_status(Result),
                    beamai_deepagent_plan:update_step_status(AccState, TaskId, Status)
                end,
                State,
                Results
            )
    end.

%% @private 将结果转换为状态
-spec result_to_status(map()) -> completed | failed.
result_to_status(#{success := true}) -> completed;
result_to_status(_) -> failed.
