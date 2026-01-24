%%%-------------------------------------------------------------------
%%% @doc 流程步骤执行器
%%%
%%% 负责执行已激活的流程步骤，支持顺序和并发两种模式。
%%% 从 beamai_process_runtime 中提取的纯执行逻辑，
%%% 不涉及状态机转换和事件队列管理。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_executor).

-export([execute_steps/3]).

-define(POOL_NAME, beamai_process_pool).

%%====================================================================
%% API
%%====================================================================

%% @doc 根据执行模式执行已激活步骤
%%
%% 顺序模式：依次执行各步骤，收集所有结果后统一返回。
%% 遇到 pause 或 error 时立即停止后续步骤执行。
%% 并发模式：为每个步骤生成 poolboy worker 异步执行，
%% 返回监控信息供 runtime 等待结果。
%%
%% @param ActivatedSteps 已激活的步骤 ID 列表
%% @param StepsState 当前所有步骤的运行时状态
%% @param Opts 执行选项，包含 mode（concurrent | sequential）和 context
%% @returns 顺序模式返回 {sync_done, 结果列表, 更新后步骤状态}
%%          并发模式返回 {async, 监控 Map, 更新后步骤状态}
-spec execute_steps([atom()], #{atom() => beamai_process_step:step_runtime_state()},
                    #{mode := concurrent | sequential,
                      context := beamai_context:t()}) ->
    {sync_done, [{atom(), term()}], #{atom() => beamai_process_step:step_runtime_state()}} |
    {async, #{atom() => true}, #{atom() => beamai_process_step:step_runtime_state()}}.
execute_steps(ActivatedSteps, StepsState, #{mode := Mode, context := Context}) ->
    case Mode of
        sequential -> execute_sequential(ActivatedSteps, StepsState, Context);
        concurrent -> execute_concurrent(ActivatedSteps, StepsState, Context)
    end.

%%====================================================================
%% 内部函数 - 顺序执行
%%====================================================================

%% @private 顺序执行已激活步骤
%% 依次执行每个步骤，收集结果。遇到 pause/error 立即终止。
execute_sequential(Steps, StepsState, Context) ->
    execute_sequential_loop(Steps, StepsState, Context, []).

%% @private 顺序执行循环
%% 逐步执行并累积结果，任何步骤返回 pause/error 即终止循环
execute_sequential_loop([], StepsState, _Context, AccResults) ->
    {sync_done, lists:reverse(AccResults), StepsState};
execute_sequential_loop([StepId | Rest], StepsState, Context, AccResults) ->
    StepState = maps:get(StepId, StepsState),
    #{collected_inputs := Inputs} = StepState,
    ClearedState = beamai_process_step:clear_inputs(StepState),
    StepsState1 = StepsState#{StepId => ClearedState},
    case beamai_process_step:execute(ClearedState, Inputs, Context) of
        {events, Events, NewStepState} ->
            StepsState2 = StepsState1#{StepId => NewStepState},
            Result = {StepId, {events, Events, NewStepState}},
            execute_sequential_loop(Rest, StepsState2, Context, [Result | AccResults]);
        {pause, Reason, NewStepState} ->
            StepsState2 = StepsState1#{StepId => NewStepState},
            Result = {StepId, {pause, Reason, NewStepState}},
            {sync_done, lists:reverse([Result | AccResults]), StepsState2};
        {error, _Reason} = ErrorResult ->
            Result = {StepId, ErrorResult},
            {sync_done, lists:reverse([Result | AccResults]), StepsState1}
    end.

%%====================================================================
%% 内部函数 - 并发执行
%%====================================================================

%% @private 并发执行已激活步骤
%% 为每个步骤生成 poolboy worker 进程异步执行
execute_concurrent(ActivatedSteps, StepsState, Context) ->
    Self = self(),
    {NewStepsState, Monitors} = lists:foldl(
        fun(StepId, {StAcc, MonAcc}) ->
            StepState = maps:get(StepId, StAcc),
            #{collected_inputs := Inputs} = StepState,
            ClearedState = beamai_process_step:clear_inputs(StepState),
            StAcc1 = StAcc#{StepId => ClearedState},
            spawn_step_worker(Self, StepId, ClearedState, Inputs, Context),
            {StAcc1, MonAcc#{StepId => true}}
        end,
        {StepsState, #{}},
        ActivatedSteps
    ),
    {async, Monitors, NewStepsState}.

%% @private 生成 poolboy worker 进程执行单个步骤
%% 从连接池获取 worker，执行完毕后归还，并将结果发送给父进程
spawn_step_worker(Parent, StepId, StepRuntimeState, Inputs, Context) ->
    spawn_link(fun() ->
        Result = try
            Worker = poolboy:checkout(?POOL_NAME),
            try
                beamai_process_worker:execute_step(Worker, StepRuntimeState, Inputs, Context)
            after
                poolboy:checkin(?POOL_NAME, Worker)
            end
        catch
            Class:Error ->
                {error, {worker_exception, StepId, {Class, Error}}}
        end,
        Parent ! {step_result, StepId, Result}
    end).
