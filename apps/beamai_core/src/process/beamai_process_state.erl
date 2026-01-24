%%%-------------------------------------------------------------------
%%% @doc 流程状态快照与恢复
%%%
%%% 通过序列化/反序列化运行时状态实现流程持久化。
%%% 快照包含完整的流程规格、步骤状态和事件队列。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_state).

-export([
    take_snapshot/1,
    restore_from_snapshot/1
]).

-export_type([snapshot/0]).

-type snapshot() :: #{
    '__process_snapshot__' := true,
    process_spec := beamai_process_builder:process_spec(),
    current_state := atom(),
    steps_state := #{atom() => step_snapshot()},
    event_queue := [beamai_process_event:event()],
    paused_step := atom() | undefined,
    pause_reason := term() | undefined,
    timestamp := integer()
}.

-type step_snapshot() :: #{
    state := term(),
    collected_inputs := #{atom() => term()},
    activation_count := non_neg_integer()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 生成运行时状态快照
%%
%% 将完整的运行时状态序列化为可持久化的 Map。
%% 步骤状态仅保留 state、collected_inputs 和 activation_count，
%% 去除 step_def 引用（恢复时从 process_spec 重建）。
%%
%% @param RuntimeState 运行时状态 Map（包含 process_spec、steps_state 等字段）
%% @returns 快照 Map
-spec take_snapshot(map()) -> snapshot().
take_snapshot(#{process_spec := ProcessSpec, current_state := CurrentState,
               steps_state := StepsState, event_queue := EventQueue,
               paused_step := PausedStep, pause_reason := PauseReason}) ->
    #{
        '__process_snapshot__' => true,
        process_spec => ProcessSpec,
        current_state => CurrentState,
        steps_state => snapshot_steps(StepsState),
        event_queue => EventQueue,
        paused_step => PausedStep,
        pause_reason => PauseReason,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc 从快照恢复运行时状态
%%
%% 将快照中的步骤状态与 process_spec 中的步骤定义重新关联，
%% 重建完整的运行时状态。
%%
%% @param Snapshot 快照 Map
%% @returns {ok, 恢复后的运行时状态} | {error, 原因}
-spec restore_from_snapshot(snapshot()) ->
    {ok, map()} | {error, term()}.
restore_from_snapshot(#{process_spec := ProcessSpec, current_state := CurrentState,
                        steps_state := StepsSnapshots, event_queue := EventQueue,
                        paused_step := PausedStep, pause_reason := PauseReason}) ->
    case restore_steps(StepsSnapshots, maps:get(steps, ProcessSpec)) of
        {ok, StepsState} ->
            {ok, #{
                process_spec => ProcessSpec,
                current_state => CurrentState,
                steps_state => StepsState,
                event_queue => EventQueue,
                paused_step => PausedStep,
                pause_reason => PauseReason
            }};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 将步骤运行时状态精简为快照格式（去除 step_def 引用）
snapshot_steps(StepsState) ->
    maps:map(
        fun(_StepId, #{state := State, collected_inputs := Inputs,
                       activation_count := Count}) ->
            #{
                state => State,
                collected_inputs => Inputs,
                activation_count => Count
            }
        end,
        StepsState
    ).

%% @private 从快照恢复步骤运行时状态（重建 step_def 引用）
restore_steps(StepsSnapshots, StepDefs) ->
    try
        Restored = maps:map(
            fun(StepId, #{state := State, collected_inputs := Inputs,
                         activation_count := Count}) ->
                StepDef = maps:get(StepId, StepDefs),
                #{
                    '__step_runtime__' => true,
                    step_def => StepDef,
                    state => State,
                    collected_inputs => Inputs,
                    activation_count => Count
                }
            end,
            StepsSnapshots
        ),
        {ok, Restored}
    catch
        _:Reason ->
            {error, {restore_failed, Reason}}
    end.
