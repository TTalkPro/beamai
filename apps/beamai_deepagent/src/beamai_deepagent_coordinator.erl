%%%-------------------------------------------------------------------
%%% @doc DeepAgent 协调器
%%%
%%% 编排整个深度代理的执行流程：
%%%   1. 初始化状态
%%%   2. 如果启用规划：创建计划 → 分析依赖 → 逐层并行执行 → 可选反思
%%%   3. 如果未启用规划：直接执行任务
%%%   4. 最终反思（可选）
%%%   5. 返回运行结果
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_coordinator).

-export([execute/3]).

%%====================================================================
%% API
%%====================================================================

%% @doc 执行深度代理任务
%%
%% 根据配置选择直接执行或规划后执行。
%% 记录执行轨迹，在规划失败时自动降级为直接执行。
%%
%% Config: 深度代理配置
%% Task: 用户任务描述
%% Opts: 额外选项
-spec execute(map(), binary(), map()) -> {ok, beamai_deepagent:run_result()} | {error, term()}.
execute(Config, Task, Opts) ->
    State = init_state(Config, Task, Opts),
    Trace0 = beamai_deepagent_trace:new(),
    Trace1 = beamai_deepagent_trace:add(Trace0, task_started, #{task => Task}),

    case maps:get(planning_enabled, Config, true) of
        true -> execute_with_planning(State, Task, Trace1);
        false -> execute_direct(State, Task, Trace1)
    end.

%%====================================================================
%% 内部函数 - 初始化
%%====================================================================

%% @doc 初始化协调器状态
%%
%% 包含配置、任务描述、已完成结果列表和反思记录。
-spec init_state(map(), binary(), map()) -> map().
init_state(Config, Task, Opts) ->
    #{
        config => Config,
        task => Task,
        opts => Opts,
        completed_results => [],
        plan => undefined,
        reflections => []
    }.

%%====================================================================
%% 内部函数 - 直接执行（无规划）
%%====================================================================

%% @doc 直接执行任务（不经过规划阶段）
%%
%% 将整个任务作为单一步骤交给执行器处理，
%% 适用于简单任务或规划失败时的降级策略。
-spec execute_direct(map(), binary(), beamai_deepagent_trace:t()) ->
    {ok, map()} | {error, term()}.
execute_direct(State, Task, Trace) ->
    Step = #{id => 1, description => Task, dependencies => [], is_deep => false},
    Context = #{previous_results => []},
    Trace1 = beamai_deepagent_trace:add(Trace, direct_execution, #{task => Task}),

    case beamai_deepagent_executor:execute_step(Step, State, Context) of
        {ok, StepResult} ->
            Trace2 = beamai_deepagent_trace:add(Trace1, step_completed,
                #{step_id => 1, status => maps:get(status, StepResult)}),
            {ok, #{
                status => result_status(StepResult),
                response => maps:get(result, StepResult, <<>>),
                trace => Trace2,
                step_results => [StepResult]
            }};
        {error, Reason} ->
            Trace2 = beamai_deepagent_trace:add(Trace1, execution_error, #{reason => Reason}),
            {ok, #{
                status => error,
                response => beamai_deepagent_utils:format_error(Reason),
                trace => Trace2,
                step_results => []
            }}
    end.

%%====================================================================
%% 内部函数 - 规划执行
%%====================================================================

%% @doc 通过规划代理创建计划后按层执行
%%
%% 规划失败时自动降级为直接执行，不会中断流程。
-spec execute_with_planning(map(), binary(), beamai_deepagent_trace:t()) ->
    {ok, map()} | {error, term()}.
execute_with_planning(State, Task, Trace) ->
    Config = maps:get(config, State),
    Trace1 = beamai_deepagent_trace:add(Trace, planning_started, #{}),

    case beamai_deepagent_planner:create_plan(Config, Task) of
        {ok, Plan} ->
            StepCount = length(beamai_deepagent_plan:get_steps(Plan)),
            Trace2 = beamai_deepagent_trace:add(Trace1, plan_created, #{steps => StepCount}),
            execute_plan(State#{plan => Plan}, Plan, Trace2);
        {error, Reason} ->
            Trace2 = beamai_deepagent_trace:add(Trace1, planning_failed, #{reason => Reason}),
            execute_direct(State, Task, Trace2)
    end.

%% @doc 分析计划依赖并逐层执行
%%
%% 使用依赖分析器将步骤分组为可并行执行的层级，
%% 然后逐层调用并行执行模块处理。
-spec execute_plan(map(), beamai_deepagent_plan:t(), beamai_deepagent_trace:t()) ->
    {ok, map()} | {error, term()}.
execute_plan(State, Plan, Trace) ->
    StepMaps = beamai_deepagent_plan:steps_to_maps(Plan),
    Layers = beamai_deepagent_dependencies:analyze(StepMaps),
    Trace1 = beamai_deepagent_trace:add(Trace, layers_analyzed,
        #{layer_count => length(Layers)}),
    execute_layers(Layers, 1, State, Plan, Trace1, []).

%%====================================================================
%% 内部函数 - 层级执行循环
%%====================================================================

%% @doc 逐层执行步骤，收集结果
%%
%% 所有层完成后生成最终回复。
%% 每层执行后可选进行反思分析。
-spec execute_layers([[map()]], pos_integer(), map(),
    beamai_deepagent_plan:t(), beamai_deepagent_trace:t(), [map()]) ->
    {ok, map()} | {error, term()}.
execute_layers([], _LayerIdx, State, Plan, Trace, AllResults) ->
    finalize_execution(State, Plan, Trace, AllResults);
execute_layers([Layer | Rest], LayerIdx, State, Plan, Trace, AllResults) ->
    Trace1 = beamai_deepagent_trace:add(Trace, layer_started,
        #{layer => LayerIdx, steps => length(Layer)}),
    case beamai_deepagent_parallel:execute_layer(Layer, State) of
        {ok, LayerResults, NewState} ->
            Plan1 = update_plan_with_results(Plan, LayerResults),
            Trace2 = beamai_deepagent_trace:add(Trace1, layer_completed,
                #{layer => LayerIdx, results => length(LayerResults)}),
            Trace3 = maybe_reflect_layer(Plan1, LayerResults, LayerIdx, State, Trace2),
            NewAllResults = AllResults ++ LayerResults,
            execute_layers(Rest, LayerIdx + 1, NewState, Plan1, Trace3, NewAllResults);
        {error, Reason} ->
            build_layer_error_result(Plan, Trace1, AllResults, LayerIdx, Reason)
    end.

%%====================================================================
%% 内部函数 - 执行完成处理
%%====================================================================

%% @doc 所有层执行完成后的收尾处理
%%
%% 如果启用反思，生成最终反思作为回复；
%% 否则使用最后一个完成步骤的结果作为回复。
-spec finalize_execution(map(), beamai_deepagent_plan:t(),
    beamai_deepagent_trace:t(), [map()]) -> {ok, map()}.
finalize_execution(State, Plan, Trace, AllResults) ->
    Config = maps:get(config, State),
    ReflectionEnabled = maps:get(reflection_enabled, Config, true),

    {FinalResponse, Trace1} = case ReflectionEnabled of
        true ->
            case beamai_deepagent_reflector:final_reflection(Plan, AllResults, Config) of
                {ok, Reflection} ->
                    T = beamai_deepagent_trace:add(Trace, final_reflection,
                        #{length => byte_size(Reflection)}),
                    {Reflection, T};
                {error, _} ->
                    {build_default_response(AllResults), Trace}
            end;
        false ->
            {build_default_response(AllResults), Trace}
    end,

    Trace2 = beamai_deepagent_trace:add(Trace1, execution_completed, #{}),
    OverallStatus = determine_overall_status(AllResults),

    {ok, #{
        status => OverallStatus,
        response => FinalResponse,
        plan => Plan,
        trace => Trace2,
        step_results => AllResults
    }}.

%% @doc 可选执行层级反思
%%
%% 如果启用反思，对当前层结果进行分析。
%% 反思失败不影响执行，仅记录到轨迹中。
-spec maybe_reflect_layer(beamai_deepagent_plan:t(), [map()], pos_integer(),
    map(), beamai_deepagent_trace:t()) -> beamai_deepagent_trace:t().
maybe_reflect_layer(Plan, LayerResults, LayerIdx, State, Trace) ->
    Config = maps:get(config, State),
    case maps:get(reflection_enabled, Config, true) of
        true ->
            case beamai_deepagent_reflector:reflect(Plan, LayerResults, LayerIdx, Config) of
                {ok, _Reflection} ->
                    beamai_deepagent_trace:add(Trace, layer_reflection, #{layer => LayerIdx});
                {error, _} ->
                    Trace
            end;
        false ->
            Trace
    end.

%% @doc 构建层级执行错误的返回结果
-spec build_layer_error_result(beamai_deepagent_plan:t(), beamai_deepagent_trace:t(),
    [map()], pos_integer(), term()) -> {ok, map()}.
build_layer_error_result(Plan, Trace, AllResults, LayerIdx, Reason) ->
    Trace1 = beamai_deepagent_trace:add(Trace, layer_error,
        #{layer => LayerIdx, reason => Reason}),
    ErrMsg = beamai_deepagent_utils:format_error({layer_failed, LayerIdx, Reason}),
    {ok, #{
        status => error,
        response => ErrMsg,
        plan => Plan,
        trace => Trace1,
        step_results => AllResults
    }}.

%%====================================================================
%% 内部函数 - 计划状态更新
%%====================================================================

%% @doc 根据执行结果更新计划中对应步骤的状态
%%
%% 将 step_result 中的 status 映射到 plan 中的 step_status，
%% 通过 foldl 逐个更新。
-spec update_plan_with_results(beamai_deepagent_plan:t(), [map()]) ->
    beamai_deepagent_plan:t().
update_plan_with_results(Plan, Results) ->
    lists:foldl(fun(#{step_id := StepId, status := Status, result := Result}, AccPlan) ->
        PlanStatus = step_status_to_plan_status(Status),
        beamai_deepagent_plan:update_step(AccPlan, StepId,
            #{status => PlanStatus, result => Result})
    end, Plan, Results).

%% @doc 将执行结果状态映射到计划步骤状态
%%
%% crashed 和 timeout 都映射为 failed，因为计划只关注成功/失败。
-spec step_status_to_plan_status(atom()) -> atom().
step_status_to_plan_status(completed) -> completed;
step_status_to_plan_status(failed) -> failed;
step_status_to_plan_status(crashed) -> failed;
step_status_to_plan_status(timeout) -> failed;
step_status_to_plan_status(_) -> pending.

%%====================================================================
%% 内部函数 - 结果状态判定
%%====================================================================

%% @doc 将单步结果状态映射为运行结果状态
-spec result_status(map()) -> completed | timeout | error.
result_status(#{status := completed}) -> completed;
result_status(#{status := timeout}) -> timeout;
result_status(_) -> error.

%% @doc 判定所有步骤的综合执行状态
%%
%% 只要有一个步骤非 completed，整体状态为 error。
-spec determine_overall_status([map()]) -> completed | error.
determine_overall_status(Results) ->
    AllCompleted = lists:all(
        fun(#{status := S}) -> S =:= completed;
           (_) -> false
        end, Results),
    case AllCompleted of
        true -> completed;
        false -> error
    end.

%% @doc 从已完成步骤中提取默认回复
%%
%% 取最后一个成功完成步骤的结果作为回复。
%% 如果没有成功步骤，返回提示信息。
-spec build_default_response([map()]) -> binary().
build_default_response(Results) ->
    CompletedResults = [R || #{status := completed} = R <- Results],
    case CompletedResults of
        [] ->
            <<"No steps completed successfully.">>;
        _ ->
            LastResult = lists:last(CompletedResults),
            maps:get(result, LastResult, <<"Task completed.">>)
    end.
