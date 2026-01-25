%%%-------------------------------------------------------------------
%%% @doc 流程运行时引擎（gen_statem 状态机）
%%%
%%% 驱动事件驱动的步骤激活与执行，支持并发和顺序两种执行模式。
%%% 通过 poolboy 工作进程池实现并发步骤执行。
%%%
%%% == 状态转换 ==
%%%
%%% idle -> running -> paused/completed/failed
%%%
%%% == 初始化路径 ==
%%%
%%% - init_fresh/2: 全新启动，调用各步骤的 Module:init(Config)
%%% - init_from_restored/2: 从快照恢复，直接使用恢复的步骤状态
%%%
%%% == 自动 Checkpoint ==
%%%
%%% 通过 Opts 传入 store（{Module, Ref}）和 checkpoint_policy 配置。
%%% 在关键状态转换点异步保存快照，失败不阻塞流程执行。
%%% Module 需实现 beamai_process_store_behaviour 行为。
%%%
%%% == 静止点回调（on_quiescent）==
%%%
%%% 通过 Opts 传入 on_quiescent 回调函数。
%%% 当所有并发步骤执行完毕时触发（但流程可能尚未完成）。
%%% 用于监控、自定义持久化、进度通知等。
%%% 回调签名: fun(QuiescentInfo :: map()) -> ok
%%%
%%% 触发时机:
%%% - quiescent: 并发步骤全部完成，事件队列仍有待处理事件
%%% - paused: 步骤请求暂停
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_runtime).

-behaviour(gen_statem).

%% API
-export([
    start_link/2,
    send_event/2,
    resume/2,
    stop/1,
    get_status/1,
    snapshot/1
]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([idle/3, running/3, paused/3, completed/3, failed/3]).

-define(POOL_NAME, beamai_process_pool).

-record(data, {
    process_spec :: beamai_process_builder:process_spec(),
    steps_state :: #{atom() => beamai_process_step:step_runtime_state()},
    event_queue :: queue:queue(beamai_process_event:event()),
    context :: beamai_context:t(),
    paused_step :: atom() | undefined,
    pause_reason :: term() | undefined,
    pending_steps :: #{atom() => reference()},
    pending_results :: [{atom(), term()}],
    expected_count :: non_neg_integer(),
    caller :: pid() | undefined,
    opts :: map(),
    %% 存储后端引用：{Module, Ref}，Module 需实现 beamai_process_store_behaviour
    store :: {module(), term()} | undefined,
    %% 自动 checkpoint 策略配置
    checkpoint_policy :: map(),
    %% 静止点回调：所有并发步骤执行完毕时触发
    %% 签名: fun(QuiescentInfo :: map()) -> ok
    %% QuiescentInfo 格式见 build_quiescent_info/3
    on_quiescent :: fun((map()) -> ok) | undefined
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动流程运行时进程
%%
%% @param ProcessSpec 编译后的流程定义
%% @param Opts 启动选项（可包含 context、caller、initial_events 等）
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
start_link(ProcessSpec, Opts) ->
    gen_statem:start_link(?MODULE, {ProcessSpec, Opts}, []).

%% @doc 向运行中的流程发送事件
%%
%% @param Pid 流程运行时进程 PID
%% @param Event 事件 Map
%% @returns ok
-spec send_event(pid(), beamai_process_event:event()) -> ok.
send_event(Pid, Event) ->
    gen_statem:cast(Pid, {send_event, Event}).

%% @doc 恢复已暂停的流程
%%
%% 调用暂停步骤的 on_resume 回调，传入恢复数据。
%%
%% @param Pid 流程运行时进程 PID
%% @param Data 恢复时传入的数据
%% @returns ok | {error, not_paused}
-spec resume(pid(), term()) -> ok | {error, not_paused}.
resume(Pid, Data) ->
    gen_statem:call(Pid, {resume, Data}).

%% @doc 停止流程运行时进程
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%% @doc 获取流程当前状态信息
%%
%% @returns {ok, 状态 Map}，包含 state、name、queue_length 等字段
-spec get_status(pid()) -> {ok, map()}.
get_status(Pid) ->
    gen_statem:call(Pid, get_status).

%% @doc 获取流程状态快照（可序列化）
%%
%% @returns {ok, 快照 Map}，可用于持久化和恢复
-spec snapshot(pid()) -> {ok, beamai_process_state:snapshot()}.
snapshot(Pid) ->
    gen_statem:call(Pid, snapshot).

%%====================================================================
%% gen_statem 回调
%%====================================================================

%% @private 状态机回调模式：状态函数模式
callback_mode() -> state_functions.

%% @private 初始化流程运行时
%% 根据 Opts 中 restored 标记决定走全新初始化还是恢复路径
init({ProcessSpec, Opts}) ->
    case maps:get(restored, Opts, false) of
        true ->
            init_from_restored(ProcessSpec, Opts);
        false ->
            init_fresh(ProcessSpec, Opts)
    end.

%% @private 全新初始化路径
%% 初始化所有步骤状态，设置事件队列，根据是否有初始事件决定初始状态
init_fresh(ProcessSpec, Opts) ->
    case init_steps(ProcessSpec) of
        {ok, StepsState} ->
            Context = maps:get(context, Opts, beamai_context:new()),
            InitialEvents = maps:get(initial_events, ProcessSpec, []),
            Queue = queue:from_list(InitialEvents),
            Store = maps:get(store, Opts, undefined),
            CheckpointPolicy = maps:get(checkpoint_policy, Opts, default_checkpoint_policy()),
            OnQuiescent = maps:get(on_quiescent, Opts, undefined),
            Data = #data{
                process_spec = ProcessSpec,
                steps_state = StepsState,
                event_queue = Queue,
                context = Context,
                paused_step = undefined,
                pause_reason = undefined,
                pending_steps = #{},
                pending_results = [],
                expected_count = 0,
                caller = maps:get(caller, Opts, undefined),
                opts = Opts,
                store = Store,
                checkpoint_policy = CheckpointPolicy,
                on_quiescent = OnQuiescent
            },
            case queue:is_empty(Queue) of
                true ->
                    {ok, idle, Data};
                false ->
                    {ok, running, Data, [{state_timeout, 0, process_queue}]}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

%% @private 从快照恢复的初始化路径
%% 直接使用 Opts 中传入的已恢复步骤状态，不调用 init_steps
init_from_restored(ProcessSpec, Opts) ->
    Data = build_restored_data(ProcessSpec, Opts),
    CurrentState = maps:get(restored_current_state, Opts, idle),
    determine_initial_state(CurrentState, Data).

%% @private 构建恢复数据记录
%% 从 Opts 中提取所有恢复相关字段，构建 #data{} 记录
-spec build_restored_data(map(), map()) -> #data{}.
build_restored_data(ProcessSpec, Opts) ->
    EventQueue = maps:get(restored_event_queue, Opts, []),
    #data{
        process_spec = ProcessSpec,
        steps_state = maps:get(restored_steps_state, Opts),
        event_queue = queue:from_list(EventQueue),
        context = maps:get(context, Opts, beamai_context:new()),
        paused_step = maps:get(restored_paused_step, Opts, undefined),
        pause_reason = maps:get(restored_pause_reason, Opts, undefined),
        pending_steps = #{},
        pending_results = [],
        expected_count = 0,
        caller = maps:get(caller, Opts, undefined),
        opts = Opts,
        store = maps:get(store, Opts, undefined),
        checkpoint_policy = maps:get(checkpoint_policy, Opts, default_checkpoint_policy()),
        on_quiescent = maps:get(on_quiescent, Opts, undefined)
    }.

%% @private 根据恢复状态和事件队列决定初始 FSM 状态
%% 将复杂的嵌套条件判断拆分为独立函数
-spec determine_initial_state(atom(), #data{}) -> {ok, atom(), #data{}} | {ok, atom(), #data{}, list()}.
determine_initial_state(paused, Data) ->
    {ok, paused, Data};
determine_initial_state(completed, Data) ->
    {ok, completed, Data};
determine_initial_state(failed, Data) ->
    {ok, failed, Data};
determine_initial_state(running, Data) ->
    initial_state_from_queue(Data);
determine_initial_state(_, Data) ->
    %% idle 或其他状态，根据队列决定
    initial_state_from_queue(Data).

%% @private 根据事件队列状态决定初始状态
%% 队列为空进入 idle，否则进入 running 并触发处理
-spec initial_state_from_queue(#data{}) -> {ok, atom(), #data{}} | {ok, atom(), #data{}, list()}.
initial_state_from_queue(#data{event_queue = Queue} = Data) ->
    case queue:is_empty(Queue) of
        true -> {ok, idle, Data};
        false -> {ok, running, Data, [{state_timeout, 0, process_queue}]}
    end.

%% @private 进程终止回调
terminate(_Reason, _State, _Data) ->
    ok.

%%====================================================================
%% 状态：idle（空闲）
%%====================================================================

%% @private 空闲状态：收到事件时转入 running 状态
idle(cast, {send_event, Event}, #data{event_queue = Queue} = Data) ->
    NewQueue = queue:in(Event, Queue),
    {next_state, running, Data#data{event_queue = NewQueue},
     [{state_timeout, 0, process_queue}]};

%% @private 空闲状态：响应状态查询
idle({call, From}, get_status, Data) ->
    {keep_state, Data, [{reply, From, {ok, format_status(idle, Data)}}]};

%% @private 空闲状态：响应快照请求
idle({call, From}, snapshot, Data) ->
    {keep_state, Data, [{reply, From, {ok, do_snapshot(idle, Data)}}]};

%% @private 空闲状态：拒绝其他调用
idle({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, idle}}]}.

%%====================================================================
%% 状态：running（运行中）
%%====================================================================

%% @private 运行状态：处理事件队列超时触发
running(state_timeout, process_queue, Data) ->
    process_event_queue(Data);

%% @private 运行状态：新事件入队等待处理
running(cast, {send_event, Event}, #data{event_queue = Queue} = Data) ->
    NewQueue = queue:in(Event, Queue),
    {keep_state, Data#data{event_queue = NewQueue}};

%% @private 运行状态：接收并发步骤执行结果
running(info, {step_result, StepId, Result}, Data) ->
    handle_step_result(StepId, Result, Data);

%% @private 运行状态：响应状态查询
running({call, From}, get_status, Data) ->
    {keep_state, Data, [{reply, From, {ok, format_status(running, Data)}}]};

%% @private 运行状态：响应快照请求
running({call, From}, snapshot, Data) ->
    {keep_state, Data, [{reply, From, {ok, do_snapshot(running, Data)}}]};

%% @private 运行状态：拒绝其他调用（忙碌中）
running({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, busy}}]}.

%%====================================================================
%% 状态：paused（已暂停）
%%====================================================================

%% @private 暂停状态：处理恢复请求
%% 检查暂停步骤是否支持 on_resume 回调
paused({call, From}, {resume, ResumeData}, #data{paused_step = StepId,
                                                  steps_state = StepsState} = Data) ->
    case StepId of
        undefined ->
            {keep_state, Data, [{reply, From, {error, no_paused_step}}]};
        _ ->
            StepState = maps:get(StepId, StepsState),
            #{step_spec := #{module := Module}} = StepState,
            case erlang:function_exported(Module, on_resume, 3) of
                true ->
                    handle_resume(Module, ResumeData, StepState, StepId, From, Data);
                false ->
                    {keep_state, Data, [{reply, From, {error, resume_not_supported}}]}
            end
    end;

%% @private 暂停状态：新事件入队（等恢复后处理）
paused(cast, {send_event, Event}, #data{event_queue = Queue} = Data) ->
    NewQueue = queue:in(Event, Queue),
    {keep_state, Data#data{event_queue = NewQueue}};

%% @private 暂停状态：响应状态查询
paused({call, From}, get_status, Data) ->
    {keep_state, Data, [{reply, From, {ok, format_status(paused, Data)}}]};

%% @private 暂停状态：响应快照请求
paused({call, From}, snapshot, Data) ->
    {keep_state, Data, [{reply, From, {ok, do_snapshot(paused, Data)}}]};

%% @private 暂停状态：拒绝其他调用
paused({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, paused}}]}.

%%====================================================================
%% 状态：completed（已完成）
%%====================================================================

%% @private 完成状态：忽略新事件
completed(cast, {send_event, _Event}, _Data) ->
    keep_state_and_data;

%% @private 完成状态：响应状态查询
completed({call, From}, get_status, Data) ->
    {keep_state, Data, [{reply, From, {ok, format_status(completed, Data)}}]};

%% @private 完成状态：响应快照请求
completed({call, From}, snapshot, Data) ->
    {keep_state, Data, [{reply, From, {ok, do_snapshot(completed, Data)}}]};

%% @private 完成状态：拒绝其他调用
completed({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, completed}}]}.

%%====================================================================
%% 状态：failed（已失败）
%%====================================================================

%% @private 失败状态：忽略新事件
failed(cast, {send_event, _Event}, _Data) ->
    keep_state_and_data;

%% @private 失败状态：响应状态查询
failed({call, From}, get_status, Data) ->
    {keep_state, Data, [{reply, From, {ok, format_status(failed, Data)}}]};

%% @private 失败状态：响应快照请求
failed({call, From}, snapshot, Data) ->
    {keep_state, Data, [{reply, From, {ok, do_snapshot(failed, Data)}}]};

%% @private 失败状态：拒绝其他调用
failed({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, failed}}]}.

%%====================================================================
%% 内部函数 - 事件处理
%%====================================================================

%% @private 处理事件队列
%% 队列为空时转入完成状态；否则取出事件进行路由和激活
process_event_queue(#data{event_queue = Queue} = Data) ->
    case queue:out(Queue) of
        {empty, _} ->
            transition_to_completed(Data);
        {{value, Event}, RestQueue} ->
            Data1 = Data#data{event_queue = RestQueue},
            route_and_activate(Event, Data1)
    end.

%% @private 路由事件并激活匹配的步骤
%% 通过 bindings 将事件数据投递到目标步骤输入，然后检查激活条件
route_and_activate(Event, #data{process_spec = #{bindings := Bindings},
                                steps_state = StepsState} = Data) ->
    Deliveries = beamai_process_event:route(Event, Bindings),
    NewStepsState = deliver_inputs(Deliveries, StepsState),
    Data1 = Data#data{steps_state = NewStepsState},
    ActivatedSteps = find_activated_steps(NewStepsState),
    case ActivatedSteps of
        [] ->
            process_event_queue(Data1);
        _ ->
            execute_steps(ActivatedSteps, Data1)
    end.

%% @private 将事件路由结果投递到各步骤的输入槽
deliver_inputs(Deliveries, StepsState) ->
    lists:foldl(
        fun({StepId, InputName, Value}, Acc) ->
            beamai_process_step:collect_input(StepId, InputName, Value, Acc)
        end,
        StepsState,
        Deliveries
    ).

%% @private 查找所有已满足激活条件的步骤
find_activated_steps(StepsState) ->
    maps:fold(
        fun(StepId, StepState, Acc) ->
            #{step_spec := StepSpec} = StepState,
            case beamai_process_step:check_activation(StepState, StepSpec) of
                true -> [StepId | Acc];
                false -> Acc
            end
        end,
        [],
        StepsState
    ).

%% @private 委托执行器执行已激活步骤
%% 根据返回结果类型（同步完成或异步启动）决定状态转换
execute_steps(ActivatedSteps, #data{process_spec = #{execution_mode := Mode},
                                     steps_state = StepsState,
                                     context = Context} = Data) ->
    Opts = #{mode => Mode, context => Context},
    case beamai_process_executor:execute_steps(ActivatedSteps, StepsState, Opts) of
        {sync_done, Results, NewStepsState} ->
            apply_sequential_results(Results, Data#data{steps_state = NewStepsState});
        {async, Monitors, NewStepsState} ->
            {keep_state, Data#data{
                steps_state = NewStepsState,
                pending_steps = Monitors,
                pending_results = [],
                expected_count = map_size(Monitors)
            }}
    end.

%% @private 处理顺序执行的结果列表
%% 依次将事件入队，遇到 pause/error 立即转换状态
apply_sequential_results([], Data) ->
    process_event_queue(Data);
apply_sequential_results([{StepId, {events, Events, _NewStepState}} | Rest], Data) ->
    maybe_checkpoint(step_completed, StepId, Data),
    Data1 = enqueue_events(Events, Data),
    apply_sequential_results(Rest, Data1);
apply_sequential_results([{StepId, {pause, Reason, _NewStepState}} | _], Data) ->
    Data1 = Data#data{paused_step = StepId, pause_reason = Reason},
    notify_quiescent(paused, Data1),
    maybe_checkpoint(paused, StepId, Data1),
    {next_state, paused, Data1};
apply_sequential_results([{_StepId, {error, Reason}} | _], Data) ->
    handle_error(Reason, Data).

%% @private 处理并发步骤执行结果
%% 收集到所有步骤结果后批量应用
handle_step_result(StepId, Result, #data{pending_steps = Pending,
                                          pending_results = Results,
                                          expected_count = Expected} = Data) ->
    NewPending = maps:remove(StepId, Pending),
    NewResults = [{StepId, Result} | Results],
    Data1 = Data#data{pending_steps = NewPending, pending_results = NewResults},
    case length(NewResults) >= Expected of
        true ->
            apply_step_results(NewResults, Data1#data{
                pending_steps = #{},
                pending_results = [],
                expected_count = 0
            });
        false ->
            {keep_state, Data1}
    end.

%% @private 批量应用并发步骤的执行结果
apply_step_results(Results, Data) ->
    apply_step_results_loop(Results, Data).

%% @private 逐条处理并发执行结果
%% 事件入队继续处理，pause/error 立即转换状态
apply_step_results_loop([], #data{event_queue = Queue} = Data) ->
    %% 所有并发步骤结果已处理 — 静止点
    %% 仅在队列非空时触发（还有后续事件，非最终完成）
    case queue:is_empty(Queue) of
        false -> notify_quiescent(quiescent, Data);
        true -> ok
    end,
    process_event_queue(Data);
apply_step_results_loop([{StepId, Result} | Rest], #data{steps_state = StepsState} = Data) ->
    case Result of
        {events, Events, NewStepState} ->
            StepsState1 = StepsState#{StepId => NewStepState},
            Data1 = Data#data{steps_state = StepsState1},
            maybe_checkpoint(step_completed, StepId, Data1),
            Data2 = enqueue_events(Events, Data1),
            apply_step_results_loop(Rest, Data2);
        {pause, Reason, NewStepState} ->
            StepsState1 = StepsState#{StepId => NewStepState},
            Data1 = Data#data{
                steps_state = StepsState1,
                paused_step = StepId,
                pause_reason = Reason
            },
            notify_quiescent(paused, Data1),
            maybe_checkpoint(paused, StepId, Data1),
            {next_state, paused, Data1};
        {error, Reason} ->
            handle_error(Reason, Data)
    end.

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

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

%% @private 将事件列表追加到事件队列
enqueue_events(Events, #data{event_queue = Queue} = Data) ->
    NewQueue = lists:foldl(fun(E, Q) -> queue:in(E, Q) end, Queue, Events),
    Data#data{event_queue = NewQueue}.

%% @private 转入完成状态并通知调用者
transition_to_completed(#data{caller = Caller, steps_state = StepsState} = Data) ->
    maybe_checkpoint(completed, undefined, Data),
    case Caller of
        undefined -> ok;
        Pid -> Pid ! {process_completed, self(), StepsState}
    end,
    {next_state, completed, Data}.

%% @private 转入失败状态并通知调用者
transition_to_failed(Reason, #data{caller = Caller} = Data) ->
    maybe_checkpoint(error, undefined, Data),
    case Caller of
        undefined -> ok;
        Pid -> Pid ! {process_failed, self(), Reason}
    end,
    {next_state, failed, Data#data{pause_reason = Reason}}.

%% @private 处理步骤执行错误
%% 若配置了 error_handler 则尝试恢复，否则直接转入失败状态
handle_error(Reason, #data{process_spec = #{error_handler := undefined}} = Data) ->
    transition_to_failed(Reason, Data);
handle_error(Reason, #data{process_spec = #{error_handler := Handler},
                           context = Context} = Data) ->
    #{module := Module} = Handler,
    ErrorEvent = beamai_process_event:error_event(runtime, Reason),
    ErrorInputs = #{error => ErrorEvent},
    case Module:on_activate(ErrorInputs, #{}, Context) of
        {ok, #{events := Events}} ->
            Data1 = enqueue_events(Events, Data),
            process_event_queue(Data1);
        _ ->
            transition_to_failed(Reason, Data)
    end.

%% @private 处理步骤恢复逻辑
%% 调用步骤模块的 on_resume 回调，根据返回值决定状态转换
handle_resume(Module, ResumeData, StepState, StepId, From,
              #data{steps_state = StepsState, context = Context} = Data) ->
    #{state := InnerState} = StepState,
    case Module:on_resume(ResumeData, InnerState, Context) of
        {ok, #{events := Events, state := NewInnerState}} ->
            NewStepState = StepState#{state => NewInnerState},
            StepsState1 = StepsState#{StepId => NewStepState},
            Data1 = enqueue_events(Events, Data#data{
                steps_state = StepsState1,
                paused_step = undefined,
                pause_reason = undefined
            }),
            {next_state, running, Data1,
             [{reply, From, ok}, {state_timeout, 0, process_queue}]};
        {ok, #{state := NewInnerState}} ->
            NewStepState = StepState#{state => NewInnerState},
            StepsState1 = StepsState#{StepId => NewStepState},
            Data1 = Data#data{
                steps_state = StepsState1,
                paused_step = undefined,
                pause_reason = undefined
            },
            {next_state, running, Data1,
             [{reply, From, ok}, {state_timeout, 0, process_queue}]};
        {error, Reason} ->
            {next_state, failed, Data#data{pause_reason = Reason},
             [{reply, From, {error, Reason}}]}
    end.

%% @private 格式化运行时状态信息为可读 Map
format_status(StateName, #data{process_spec = #{name := Name},
                                paused_step = PausedStep,
                                pause_reason = PauseReason,
                                event_queue = Queue}) ->
    #{
        state => StateName,
        name => Name,
        queue_length => queue:len(Queue),
        paused_step => PausedStep,
        pause_reason => PauseReason
    }.

%% @private 生成当前运行时的可序列化快照
do_snapshot(StateName, #data{process_spec = ProcessSpec,
                              steps_state = StepsState,
                              event_queue = Queue,
                              paused_step = PausedStep,
                              pause_reason = PauseReason}) ->
    beamai_process_state:take_snapshot(#{
        process_spec => ProcessSpec,
        current_state => StateName,
        steps_state => StepsState,
        event_queue => queue:to_list(Queue),
        paused_step => PausedStep,
        pause_reason => PauseReason
    }).

%%====================================================================
%% 内部函数 - 自动 Checkpoint
%%====================================================================

%% @private 默认 checkpoint 策略
%%
%% 默认策略：暂停、完成、错误时保存快照，步骤完成时不保存。
-spec default_checkpoint_policy() -> map().
default_checkpoint_policy() ->
    #{
        on_step_completed => false,
        on_pause => true,
        on_complete => true,
        on_error => true,
        metadata => #{}
    }.

%% @private 根据策略决定是否执行 checkpoint
%%
%% 检查当前触发类型是否在策略中启用。
%% 若启用且 store 已配置，异步 spawn 调用 Module:save_snapshot/3。
%% 保存失败仅记录 warning 日志，不阻塞流程执行。
%%
%% @param Trigger 触发类型（step_completed | paused | completed | error）
%% @param StepId 触发步骤 ID（可为 undefined）
%% @param Data 当前运行时数据
-spec maybe_checkpoint(atom(), atom() | undefined, #data{}) -> ok.
maybe_checkpoint(_Trigger, _StepId, #data{store = undefined}) ->
    %% 未配置 store，跳过
    ok;
maybe_checkpoint(Trigger, StepId, #data{store = {Module, Ref},
                                         checkpoint_policy = Policy,
                                         process_spec = #{name := ProcessName}} = Data) ->
    PolicyKey = trigger_to_policy_key(Trigger),
    case maps:get(PolicyKey, Policy, false) of
        false ->
            ok;
        true ->
            %% 生成快照并异步保存
            Snapshot = do_snapshot(trigger_to_state(Trigger), Data),
            PolicyMetadata = maps:get(metadata, Policy, #{}),
            SaveOpts = #{
                process_name => ProcessName,
                trigger => Trigger,
                step_id => StepId,
                metadata => PolicyMetadata
            },
            spawn(fun() ->
                case Module:save_snapshot(Ref, Snapshot, SaveOpts) of
                    {ok, _SnapshotId} ->
                        ok;
                    {error, Reason} ->
                        logger:warning(
                            "[beamai_process_runtime] checkpoint 保存失败: "
                            "trigger=~p, step=~p, reason=~p",
                            [Trigger, StepId, Reason]
                        )
                end
            end),
            ok
    end.

%% @private 触发类型映射到策略键
-spec trigger_to_policy_key(atom()) -> atom().
trigger_to_policy_key(step_completed) -> on_step_completed;
trigger_to_policy_key(paused) -> on_pause;
trigger_to_policy_key(completed) -> on_complete;
trigger_to_policy_key(error) -> on_error.

%% @private 触发类型映射到 FSM 状态名（用于快照）
-spec trigger_to_state(atom()) -> atom().
trigger_to_state(step_completed) -> running;
trigger_to_state(paused) -> paused;
trigger_to_state(completed) -> completed;
trigger_to_state(error) -> failed.

%%====================================================================
%% 内部函数 - 静止点回调（on_quiescent）
%%====================================================================

%% @private 触发静止点回调
%%
%% 当所有并发步骤执行完毕（但流程可能尚未完成）时调用。
%% 静止点是一个一致性快照点，此时没有步骤正在执行。
%%
%% 触发时机：
%% - quiescent: 所有并发步骤结果已处理，事件队列仍有待处理事件
%% - paused: 步骤请求暂停
%%
%% 回调异常仅记录日志，不影响流程执行。
%%
%% @param Reason 触发原因（quiescent | paused）
%% @param Data 当前运行时数据
-spec notify_quiescent(atom(), #data{}) -> ok.
notify_quiescent(_Reason, #data{on_quiescent = undefined}) ->
    ok;
notify_quiescent(Reason, #data{on_quiescent = Callback} = Data) ->
    QuiescentInfo = build_quiescent_info(Reason, Data),
    try
        Callback(QuiescentInfo)
    catch
        Class:Error ->
            logger:warning(
                "[beamai_process_runtime] on_quiescent 回调异常: "
                "reason=~p, class=~p, error=~p",
                [Reason, Class, Error]
            )
    end,
    ok.

%% @private 构建静止点信息 Map
%%
%% 包含流程名称、触发原因、状态、各步骤激活计数和内部状态、
%% context 快照以及创建时间戳。
%%
%% 格式:
%% #{
%%   process_name => atom(),          %% 流程名称
%%   reason => quiescent | paused,    %% 触发原因
%%   status => running | paused,      %% 当前流程状态
%%   paused_step => atom() | undefined,   %% 暂停的步骤（仅 paused 时有效）
%%   pause_reason => term() | undefined,  %% 暂停原因
%%   step_states => #{StepId => #{    %% 各步骤快照
%%     state => term(),
%%     activation_count => integer()
%%   }},
%%   context => term(),               %% context 快照
%%   created_at => integer()          %% 时间戳（毫秒）
%% }
-spec build_quiescent_info(atom(), #data{}) -> map().
build_quiescent_info(Reason, #data{process_spec = #{name := ProcessName},
                                    steps_state = StepsState,
                                    context = Context,
                                    paused_step = PausedStep,
                                    pause_reason = PauseReason}) ->
    %% 提取各步骤的 state 和 activation_count（不包含 step_spec）
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
