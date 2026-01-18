%%%-------------------------------------------------------------------
%%% @doc 工具结果分析模块
%%%
%%% 提供工具执行结果的分析功能：
%%% - 结果类型检测
%%% - 动作类型提取
%%% - 计划/子任务/反思判断
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_result_analyzer).

%% API 导出
-export([
    analyze/1,
    find_action/2,
    has_action/2,
    get_successful_results/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type action_type() :: create_plan | spawn_subtask | reflect | continue.
-type analysis_result() ::
    {create_plan, map()} |
    {spawn_subtasks, [map()]} |
    needs_reflection |
    continue.

-export_type([action_type/0, analysis_result/0]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 分析工具执行结果，返回应采取的动作
%%
%% 按优先级分析：
%% 1. 创建计划 (create_plan)
%% 2. 派生子任务 (spawn_subtasks)
%% 3. 需要反思 (needs_reflection)
%% 4. 继续执行 (continue)
-spec analyze([map()]) -> analysis_result().
analyze(Results) ->
    SuccessResults = get_successful_results(Results),
    analyze_success_results(SuccessResults).

%% @doc 在结果中查找指定动作类型
%%
%% 返回第一个匹配的结果或 not_found。
-spec find_action([map()], action_type()) -> {ok, map()} | not_found.
find_action(Results, ActionType) ->
    Pred = make_action_predicate(ActionType),
    case lists:search(Pred, Results) of
        {value, Result} -> {ok, Result};
        false -> not_found
    end.

%% @doc 检查结果中是否包含指定动作类型
-spec has_action([map()], action_type()) -> boolean().
has_action(Results, ActionType) ->
    Pred = make_action_predicate(ActionType),
    lists:any(Pred, Results).

%% @doc 获取成功的结果列表
-spec get_successful_results([map()]) -> [map()].
get_successful_results(Results) ->
    [R || #{success := true} = R <- Results].

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 分析成功的工具结果
-spec analyze_success_results([map()]) -> analysis_result().
analyze_success_results(Results) ->
    case find_create_plan_result(Results) of
        {ok, PlanMap} -> {create_plan, PlanMap};
        not_found -> check_other_actions(Results)
    end.

%% @private 查找创建计划的结果
-spec find_create_plan_result([map()]) -> {ok, map()} | not_found.
find_create_plan_result(Results) ->
    Pred = fun(#{result := #{action := create_plan}}) -> true; (_) -> false end,
    case lists:search(Pred, Results) of
        {value, #{result := #{plan := PlanMap}}} -> {ok, PlanMap};
        false -> not_found
    end.

%% @private 检查其他动作类型
-spec check_other_actions([map()]) ->
    {spawn_subtasks, [map()]} | needs_reflection | continue.
check_other_actions(Results) ->
    SpawnResults = find_spawn_results(Results),
    case SpawnResults of
        [] -> check_reflection_action(Results);
        Tasks -> {spawn_subtasks, Tasks}
    end.

%% @private 查找派生子任务的结果
-spec find_spawn_results([map()]) -> [map()].
find_spawn_results(Results) ->
    [maps:get(result, R) || #{result := #{action := spawn_subtask}} = R <- Results].

%% @private 检查是否需要反思
-spec check_reflection_action([map()]) -> needs_reflection | continue.
check_reflection_action(Results) ->
    HasReflect = lists:any(
        fun(#{result := #{action := reflect}}) -> true; (_) -> false end,
        Results
    ),
    case HasReflect of
        true -> needs_reflection;
        false -> continue
    end.

%% @private 创建动作类型匹配谓词
-spec make_action_predicate(action_type()) -> fun((map()) -> boolean()).
make_action_predicate(ActionType) ->
    fun(#{result := #{action := Action}}) -> Action =:= ActionType;
       (_) -> false
    end.
