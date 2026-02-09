%%%-------------------------------------------------------------------
%%% @doc 流程引擎 - 纯函数核心
%%%
%%% 实现事件驱动的步骤激活与执行逻辑，不依赖 OTP 进程。
%%% 支持顺序和并发两种执行模式。
%%%
%%% == 设计原则 ==
%%%
%%% 纯函数（无副作用）：不发消息、不 spawn 进程、不做 I/O。
%%% 副作用通过 effects 列表返回给调用者（runtime shell）处理。
%%%
%%% == 核心 API ==
%%%
%%% new/2: 创建引擎（从 process_spec）
%%% from_restored/2: 从快照恢复引擎
%%% run/1: 处理事件队列直到阻塞或完成
%%% inject_event/2: 注入外部事件
%%% apply_step_results/2: 应用并发步骤执行结果
%%% handle_resume/2: 处理暂停恢复
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_engine).

-export([
    new/2,
    from_restored/2,
    inject_event/2,
    run/1,
    apply_step_results/2,
    handle_resume/2,
    %% 访问器
    current_state/1,
    steps_state/1,
    pause_reason/1,
    paused_step/1,
    context/1,
    status/1,
    take_snapshot/1,
    build_quiescent_info/2,
    drain_effects/1
]).

-export_type([engine/0, effect/0, run_result/0, async_task/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(engine, {
    process_spec :: beamai_process_builder:process_spec(),
    steps_state = #{} :: #{atom() => beamai_process_step:step_runtime_state()},
    event_queue :: queue:queue(beamai_process_event:event()),
    context :: beamai_context:t(),
    current_state = idle :: idle | running | paused | completed | failed,
    paused_step :: atom() | undefined,
    pause_reason :: term() | undefined,
    error_handler_state = #{} :: map(),
    %% 累积副作用，run 返回后由调用者处理
    effects = [] :: [effect()]
}).

-opaque engine() :: #engine{}.

-type effect() ::
    {step_completed, atom()} |
    {paused, atom(), term()} |
    completed |
    {failed, term()} |
    {quiescent, atom()}.

-type async_task() :: {
    StepId :: atom(),
    StepState :: beamai_process_step:step_runtime_state(),
    Inputs :: #{atom() => term()},
    Context :: beamai_context:t()
}.

-type run_result() ::
    {ok, engine()} |
    {execute_async, [async_task()], engine()}.

%%====================================================================
%% API - 构造
%%====================================================================

%% @doc 从 process_spec 创建引擎实例
-spec new(beamai_process_builder:process_spec(), map()) ->
    {ok, engine()} | {error, term()}.
new(ProcessSpec, Opts) ->
    case init_steps(ProcessSpec) of
        {ok, StepsState} ->
            Context = maps:get(context, Opts, beamai_context:new()),
            InitialEvents = maps:get(initial_events, ProcessSpec, []),
            Queue = queue:from_list(InitialEvents),
            EHState = init_error_handler(ProcessSpec),
            CS = case queue:is_empty(Queue) of
                true -> idle;
                false -> running
            end,
            {ok, #engine{
                process_spec = ProcessSpec,
                steps_state = StepsState,
                event_queue = Queue,
                context = Context,
                current_state = CS,
                error_handler_state = EHState
            }};
        {error, _} = Error ->
            Error
    end.

%% @doc 从快照恢复引擎实例
%%
%% RestoredState: beamai_process_state:restore_from_snapshot/1 返回的状态 map
%% Opts: 额外选项（context 等不可序列化的数据）
-spec from_restored(map(), map()) -> {ok, engine()}.
from_restored(RestoredState, Opts) ->
    #{process_spec := PS, current_state := CS, steps_state := SS,
      event_queue := EQ, paused_step := PStep, pause_reason := PReason} = RestoredState,
    EHS = maps:get(error_handler_state, RestoredState, #{}),
    Context = maps:get(context, Opts, beamai_context:new()),
    ActualCS = determine_restored_state(CS, EQ),
    {ok, #engine{
        process_spec = PS,
        steps_state = SS,
        event_queue = queue:from_list(EQ),
        context = Context,
        current_state = ActualCS,
        paused_step = PStep,
        pause_reason = PReason,
        error_handler_state = EHS
    }}.

%%====================================================================
%% API - 事件注入
%%====================================================================

%% @doc 注入外部事件到事件队列
%%
%% idle 状态下注入事件会转为 running；
%% 其他状态仅入队不改变状态。
-spec inject_event(beamai_process_event:event(), engine()) -> engine().
inject_event(Event, #engine{event_queue = Q, current_state = CS} = E) ->
    NewQ = queue:in(Event, Q),
    NewCS = case CS of
        idle -> running;
        Other -> Other
    end,
    E#engine{event_queue = NewQ, current_state = NewCS}.

%%====================================================================
%% API - 执行
%%====================================================================

%% @doc 处理事件队列直到阻塞或完成
%%
%% 返回值：
%% - {ok, Engine}: 引擎到达终态（idle/completed/failed/paused）
%% - {execute_async, Tasks, Engine}: 需要异步执行步骤（并发模式）
-spec run(engine()) -> run_result().
run(#engine{current_state = CS} = E)
  when CS =:= completed; CS =:= failed; CS =:= idle; CS =:= paused ->
    {ok, E};
run(#engine{current_state = running} = E) ->
    process_queue(E).

%% @doc 应用并发步骤执行结果并继续处理
%%
%% 所有并发步骤结果收集完毕后调用。
%% 处理结果后继续处理事件队列。
-spec apply_step_results([{atom(), term()}], engine()) -> run_result().
apply_step_results(Results, Engine) ->
    case apply_results_loop(Results, Engine) of
        {continue, Engine1} ->
            process_queue(Engine1);
        {paused, Engine1} ->
            {ok, Engine1};
        {error, Reason, Engine1} ->
            handle_error(Reason, Engine1)
    end.

%% @doc 处理暂停恢复
%%
%% 返回值：
%% - {ok, Engine}: 恢复成功，引擎转为 running 状态
%% - {error, Reason}: 恢复被拒（非暂停状态、不支持等），引擎不变
%% - {error, Reason, Engine}: 恢复回调失败，引擎转为 failed 状态
-spec handle_resume(term(), engine()) ->
    {ok, engine()} | {error, term()} | {error, term(), engine()}.
handle_resume(_ResumeData, #engine{paused_step = undefined}) ->
    {error, no_paused_step};
handle_resume(ResumeData, #engine{paused_step = StepId, steps_state = SS,
                                   context = Ctx} = E) ->
    StepState = maps:get(StepId, SS),
    #{step_spec := #{module := Module}} = StepState,
    case erlang:function_exported(Module, on_resume, 3) of
        false ->
            {error, resume_not_supported};
        true ->
            do_resume(Module, ResumeData, StepState, StepId, Ctx, E)
    end.

%%====================================================================
%% API - 访问器
%%====================================================================

-spec current_state(engine()) -> atom().
current_state(#engine{current_state = CS}) -> CS.

-spec steps_state(engine()) -> map().
steps_state(#engine{steps_state = SS}) -> SS.

-spec pause_reason(engine()) -> term().
pause_reason(#engine{pause_reason = PR}) -> PR.

-spec paused_step(engine()) -> atom() | undefined.
paused_step(#engine{paused_step = PS}) -> PS.

-spec context(engine()) -> beamai_context:t().
context(#engine{context = Ctx}) -> Ctx.

%% @doc 获取运行时状态摘要
-spec status(engine()) -> map().
status(#engine{process_spec = #{name := Name}, current_state = CS,
               paused_step = PS, pause_reason = PR, event_queue = Q}) ->
    #{
        state => CS,
        name => Name,
        queue_length => queue:len(Q),
        paused_step => PS,
        pause_reason => PR
    }.

%% @doc 生成可序列化的状态快照
-spec take_snapshot(engine()) -> beamai_process_state:snapshot().
take_snapshot(#engine{process_spec = PS, steps_state = SS,
                       event_queue = Q, current_state = CS,
                       paused_step = PStep, pause_reason = PR,
                       error_handler_state = EHS}) ->
    beamai_process_state:take_snapshot(#{
        process_spec => PS,
        current_state => CS,
        steps_state => SS,
        event_queue => queue:to_list(Q),
        paused_step => PStep,
        pause_reason => PR,
        error_handler_state => EHS
    }).

%% @doc 构建静止点回调信息
-spec build_quiescent_info(atom(), engine()) -> map().
build_quiescent_info(Reason, #engine{process_spec = #{name := ProcessName},
                                      steps_state = StepsState,
                                      context = Context,
                                      paused_step = PausedStep,
                                      pause_reason = PauseReason}) ->
    StepStates = maps:map(
        fun(_StepId, #{state := State, activation_count := Count}) ->
            #{state => State, activation_count => Count}
        end,
        StepsState
    ),
    Status = case Reason of
        paused -> paused;
        _ -> running
    end,
    #{
        process_name => ProcessName,
        reason => Reason,
        status => Status,
        paused_step => PausedStep,
        pause_reason => PauseReason,
        step_states => StepStates,
        context => Context,
        created_at => erlang:system_time(millisecond)
    }.

%% @doc 取出并清空累积的副作用
-spec drain_effects(engine()) -> {[effect()], engine()}.
drain_effects(#engine{effects = Effects} = E) ->
    {lists:reverse(Effects), E#engine{effects = []}}.

%%====================================================================
%% 内部函数 - 事件队列处理
%%====================================================================

%% @private 处理事件队列
%% 队列为空时转入完成状态；否则取出事件进行路由和激活
process_queue(#engine{event_queue = Q} = E) ->
    case queue:out(Q) of
        {empty, _} ->
            transition_to_completed(E);
        {{value, Event}, RestQ} ->
            route_and_activate(Event, E#engine{event_queue = RestQ})
    end.

%% @private 路由事件并激活匹配的步骤
route_and_activate(Event, #engine{process_spec = #{bindings := Bindings},
                                   steps_state = SS} = E) ->
    Deliveries = beamai_process_event:route(Event, Bindings),
    NewSS = deliver_inputs(Deliveries, SS),
    E1 = E#engine{steps_state = NewSS},
    ActivatedSteps = find_activated_steps(NewSS),
    case ActivatedSteps of
        [] -> process_queue(E1);
        _ -> execute_steps(ActivatedSteps, E1)
    end.

%% @private 将事件路由结果投递到步骤输入
deliver_inputs(Deliveries, SS) ->
    lists:foldl(
        fun({StepId, InputName, Value}, Acc) ->
            beamai_process_step:collect_input(StepId, InputName, Value, Acc)
        end,
        SS, Deliveries).

%% @private 查找所有满足激活条件的步骤
find_activated_steps(SS) ->
    maps:fold(
        fun(StepId, StepState, Acc) ->
            #{step_spec := StepSpec} = StepState,
            case beamai_process_step:check_activation(StepState, StepSpec) of
                true -> [StepId | Acc];
                false -> Acc
            end
        end,
        [], SS).

%%====================================================================
%% 内部函数 - 步骤执行
%%====================================================================

%% @private 根据执行模式执行步骤
execute_steps(Steps, #engine{process_spec = #{execution_mode := Mode}} = E) ->
    case Mode of
        sequential -> execute_sequential(Steps, E);
        concurrent -> prepare_async(Steps, E)
    end.

%% @private 顺序执行步骤
execute_sequential(Steps, E) ->
    case execute_seq_loop(Steps, E) of
        {continue, E1} -> process_queue(E1);
        {paused, E1} -> {ok, E1};
        {error, Reason, E1} -> handle_error(Reason, E1)
    end.

%% @private 顺序执行循环
%% 逐步执行并累积结果，任何步骤返回 pause/error 即终止循环
execute_seq_loop([], E) ->
    {continue, E};
execute_seq_loop([StepId | Rest], #engine{steps_state = SS, context = Ctx} = E) ->
    StepState = maps:get(StepId, SS),
    #{collected_inputs := Inputs} = StepState,
    ClearedState = beamai_process_step:clear_inputs(StepState),
    SS1 = SS#{StepId => ClearedState},
    case beamai_process_step:execute(ClearedState, Inputs, Ctx) of
        {events, Events, NewStepState} ->
            SS2 = SS1#{StepId => NewStepState},
            E1 = E#engine{steps_state = SS2},
            E2 = add_effect({step_completed, StepId}, E1),
            E3 = enqueue_events(Events, E2),
            execute_seq_loop(Rest, E3);
        {pause, Reason, NewStepState} ->
            SS2 = SS1#{StepId => NewStepState},
            E1 = E#engine{
                steps_state = SS2,
                current_state = paused,
                paused_step = StepId,
                pause_reason = Reason
            },
            E2 = add_effect({quiescent, paused}, E1),
            E3 = add_effect({paused, StepId, Reason}, E2),
            {paused, E3};
        {error, Reason} ->
            {error, Reason, E#engine{steps_state = SS1}}
    end.

%% @private 准备并发执行任务（返回给 shell 去 spawn）
prepare_async(Steps, #engine{steps_state = SS, context = Ctx} = E) ->
    {Tasks, NewSS} = lists:foldl(
        fun(StepId, {TaskAcc, SSAcc}) ->
            StepState = maps:get(StepId, SSAcc),
            #{collected_inputs := Inputs} = StepState,
            ClearedState = beamai_process_step:clear_inputs(StepState),
            SSAcc1 = SSAcc#{StepId => ClearedState},
            {[{StepId, ClearedState, Inputs, Ctx} | TaskAcc], SSAcc1}
        end,
        {[], SS}, Steps),
    {execute_async, Tasks, E#engine{steps_state = NewSS}}.

%% @private 逐条处理并发执行结果
apply_results_loop([], #engine{event_queue = Q} = E) ->
    case queue:is_empty(Q) of
        false ->
            {continue, add_effect({quiescent, quiescent}, E)};
        true ->
            {continue, E}
    end;
apply_results_loop([{StepId, Result} | Rest], #engine{steps_state = SS} = E) ->
    case Result of
        {events, Events, NewStepState} ->
            SS1 = SS#{StepId => NewStepState},
            E1 = E#engine{steps_state = SS1},
            E2 = add_effect({step_completed, StepId}, E1),
            E3 = enqueue_events(Events, E2),
            apply_results_loop(Rest, E3);
        {pause, Reason, NewStepState} ->
            SS1 = SS#{StepId => NewStepState},
            E1 = E#engine{
                steps_state = SS1,
                current_state = paused,
                paused_step = StepId,
                pause_reason = Reason
            },
            E2 = add_effect({quiescent, paused}, E1),
            E3 = add_effect({paused, StepId, Reason}, E2),
            {paused, E3};
        {error, Reason} ->
            {error, Reason, E}
    end.

%%====================================================================
%% 内部函数 - 状态转换
%%====================================================================

%% @private 转入完成状态
transition_to_completed(E) ->
    E1 = E#engine{current_state = completed},
    E2 = add_effect(completed, E1),
    {ok, E2}.

%% @private 转入失败状态
transition_to_failed(Reason, E) ->
    E1 = E#engine{current_state = failed, pause_reason = Reason},
    E2 = add_effect({failed, Reason}, E1),
    {ok, E2}.

%%====================================================================
%% 内部函数 - 错误处理
%%====================================================================

%% @private 处理步骤执行错误
%% 若配置了 error_handler 则尝试恢复，否则直接转入失败状态
handle_error(Reason, #engine{process_spec = #{error_handler := undefined}} = E) ->
    transition_to_failed(Reason, E);
handle_error(Reason, #engine{process_spec = #{error_handler := Handler},
                              error_handler_state = EHS,
                              context = Ctx} = E) ->
    #{module := Module} = Handler,
    ErrorEvent = beamai_process_event:error_event(runtime, Reason),
    case Module:on_activate(#{error => ErrorEvent}, EHS, Ctx) of
        {ok, #{events := Events, state := NewEHS}} ->
            E1 = E#engine{error_handler_state = NewEHS},
            E2 = enqueue_events(Events, E1),
            process_queue(E2);
        {ok, #{events := Events}} ->
            E1 = enqueue_events(Events, E),
            process_queue(E1);
        _ ->
            transition_to_failed(Reason, E)
    end.

%%====================================================================
%% 内部函数 - 恢复
%%====================================================================

%% @private 执行恢复回调
do_resume(Module, ResumeData, StepState, StepId, Ctx,
          #engine{steps_state = SS} = E) ->
    #{state := InnerState} = StepState,
    case Module:on_resume(ResumeData, InnerState, Ctx) of
        {ok, #{events := Events, state := NewInner}} ->
            NewSS = SS#{StepId => StepState#{state => NewInner}},
            E1 = E#engine{
                steps_state = NewSS,
                paused_step = undefined,
                pause_reason = undefined,
                current_state = running
            },
            E2 = enqueue_events(Events, E1),
            {ok, E2};
        {ok, #{state := NewInner}} ->
            NewSS = SS#{StepId => StepState#{state => NewInner}},
            E1 = E#engine{
                steps_state = NewSS,
                paused_step = undefined,
                pause_reason = undefined,
                current_state = running
            },
            {ok, E1};
        {error, Reason} ->
            E1 = E#engine{current_state = failed, pause_reason = Reason},
            E2 = add_effect({failed, Reason}, E1),
            {error, Reason, E2}
    end.

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 追加副作用
add_effect(Effect, #engine{effects = Effects} = E) ->
    E#engine{effects = [Effect | Effects]}.

%% @private 将事件列表追加到事件队列
enqueue_events(Events, #engine{event_queue = Q} = E) ->
    NewQ = lists:foldl(fun(Ev, Acc) -> queue:in(Ev, Acc) end, Q, Events),
    E#engine{event_queue = NewQ}.

%% @private 初始化所有步骤的运行时状态
init_steps(#{steps := StepSpecs}) ->
    init_steps_iter(maps:to_list(StepSpecs), #{}).

%% @private 逐步初始化步骤（递归）
init_steps_iter([], Acc) -> {ok, Acc};
init_steps_iter([{StepId, StepSpec} | Rest], Acc) ->
    case beamai_process_step:init_step(StepSpec) of
        {ok, StepState} ->
            init_steps_iter(Rest, Acc#{StepId => StepState});
        {error, _} = Error ->
            Error
    end.

%% @private 初始化 error handler 状态
init_error_handler(#{error_handler := undefined}) -> #{};
init_error_handler(#{error_handler := #{module := Module, config := Config}}) ->
    case erlang:function_exported(Module, init, 1) of
        true ->
            case Module:init(Config) of
                {ok, State} -> State;
                _ -> #{}
            end;
        false -> #{}
    end;
init_error_handler(_) -> #{}.

%% @private 根据恢复状态和事件队列决定实际 FSM 状态
determine_restored_state(paused, _) -> paused;
determine_restored_state(completed, _) -> completed;
determine_restored_state(failed, _) -> failed;
determine_restored_state(_, []) -> idle;
determine_restored_state(_, _) -> running.
