%%%-------------------------------------------------------------------
%%% @doc 计划工具处理器模块
%%%
%%% 实现计划相关工具的处理器函数：
%%% - handle_create_plan: 创建执行计划
%%% - handle_update_plan: 更新计划步骤
%%% - handle_spawn_subtask: 创建子任务
%%% - handle_reflect: 记录反思分析
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_plan_handlers).

%%====================================================================
%% 导出 API
%%====================================================================

-export([
    handle_create_plan/2,
    handle_update_plan/2,
    handle_spawn_subtask/2,
    handle_reflect/2
]).

%%====================================================================
%% 计划工具处理器
%%====================================================================

%% @doc 处理 create_plan 工具调用
%%
%% 解析参数创建新计划，返回 create_plan 动作供路由器处理。
-spec handle_create_plan(map(), map()) -> map().
handle_create_plan(Args, _State) ->
    Goal = maps:get(<<"goal">>, Args),
    Steps = maps:get(<<"steps">>, Args, []),
    StepDefs = convert_step_definitions(Steps),
    Plan = beamai_deepagent_plan:new(Goal, StepDefs),
    #{
        action => create_plan,
        plan => beamai_deepagent_plan:to_map(Plan),
        message => <<"计划创建成功"/utf8>>
    }.

%% @doc 处理 update_plan 工具调用
%%
%% 更新指定步骤的状态。如果计划不存在则返回错误信息。
-spec handle_update_plan(map(), map()) -> map().
handle_update_plan(Args, State) ->
    StepId = maps:get(<<"step_id">>, Args),
    StatusBin = maps:get(<<"status">>, Args),
    Result = maps:get(<<"result">>, Args, undefined),
    Status = binary_to_atom(StatusBin, utf8),
    Plan = graph_state:get(State, plan, undefined),
    execute_plan_update(Plan, StepId, Status, Result).

%%====================================================================
%% 子任务工具处理器
%%====================================================================

%% @doc 处理 spawn_subtask 工具调用
%%
%% 创建子任务定义并返回 spawn_subtask 动作。
-spec handle_spawn_subtask(map(), map()) -> map().
handle_spawn_subtask(Args, _State) ->
    #{
        action => spawn_subtask,
        task_id => maps:get(<<"task_id">>, Args),
        description => maps:get(<<"description">>, Args),
        input => maps:get(<<"input">>, Args),
        trigger_fanout => true
    }.

%%====================================================================
%% 反思工具处理器
%%====================================================================

%% @doc 处理 reflect 工具调用
%%
%% 记录反思内容并返回 reflect 动作。
-spec handle_reflect(map(), map()) -> map().
handle_reflect(Args, _State) ->
    #{
        action => reflect,
        observation => maps:get(<<"observation">>, Args),
        analysis => maps:get(<<"analysis">>, Args),
        next_action => maps:get(<<"next_action">>, Args),
        trigger_reflection => true
    }.

%%====================================================================
%% 私有函数 - 数据转换
%%====================================================================

%% @private 转换步骤定义格式
-spec convert_step_definitions([map()]) -> [map()].
convert_step_definitions(Steps) ->
    [convert_single_step(S) || S <- Steps].

%% @private 转换单个步骤
-spec convert_single_step(map()) -> map().
convert_single_step(Step) ->
    #{
        description => maps:get(<<"description">>, Step),
        dependencies => maps:get(<<"dependencies">>, Step, []),
        requires_subtask => maps:get(<<"requires_subtask">>, Step, false)
    }.

%%====================================================================
%% 私有函数 - 计划更新
%%====================================================================

%% @private 执行计划更新
-spec execute_plan_update(term(), integer(), atom(), term()) -> map().
execute_plan_update(undefined, _StepId, _Status, _Result) ->
    #{
        action => update_plan,
        success => false,
        error => <<"计划不存在"/utf8>>
    };
execute_plan_update(Plan, StepId, Status, Result) ->
    Updates = build_step_updates(Status, Result),
    _UpdatedPlan = beamai_deepagent_plan:update_step(Plan, StepId, Updates),
    #{
        action => update_plan,
        success => true,
        step_id => StepId,
        new_status => Status
    }.

%% @private 构建步骤更新 map
-spec build_step_updates(atom(), term()) -> map().
build_step_updates(Status, undefined) ->
    #{status => Status};
build_step_updates(Status, Result) ->
    #{status => Status, result => Result}.
