%%%-------------------------------------------------------------------
%%% @doc 流程运行时引擎（gen_statem 状态机）
%%%
%%% 驱动事件驱动的步骤激活与执行，支持并发和顺序两种执行模式。
%%% 通过 poolboy 工作进程池实现并发步骤执行。
%%%
%%% 状态转换：idle -> running -> paused/completed/failed
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
    opts :: map()
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
%% 初始化所有步骤状态，设置事件队列，根据是否有初始事件决定初始状态
init({ProcessSpec, Opts}) ->
    case init_steps(ProcessSpec) of
        {ok, StepsState} ->
            Context = maps:get(context, Opts, beamai_context:new()),
            InitialEvents = maps:get(initial_events, ProcessSpec, []),
            Queue = queue:from_list(InitialEvents),
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
                opts = Opts
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
            #{step_def := #{module := Module}} = StepState,
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
            #{step_def := StepDef} = StepState,
            case beamai_process_step:check_activation(StepState, StepDef) of
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
apply_sequential_results([{_StepId, {events, Events, _NewStepState}} | Rest], Data) ->
    Data1 = enqueue_events(Events, Data),
    apply_sequential_results(Rest, Data1);
apply_sequential_results([{StepId, {pause, Reason, _NewStepState}} | _], Data) ->
    {next_state, paused, Data#data{
        paused_step = StepId,
        pause_reason = Reason
    }};
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
apply_step_results_loop([], Data) ->
    process_event_queue(Data);
apply_step_results_loop([{StepId, Result} | Rest], #data{steps_state = StepsState} = Data) ->
    case Result of
        {events, Events, NewStepState} ->
            StepsState1 = StepsState#{StepId => NewStepState},
            Data1 = enqueue_events(Events, Data#data{steps_state = StepsState1}),
            apply_step_results_loop(Rest, Data1);
        {pause, Reason, NewStepState} ->
            StepsState1 = StepsState#{StepId => NewStepState},
            {next_state, paused, Data#data{
                steps_state = StepsState1,
                paused_step = StepId,
                pause_reason = Reason
            }};
        {error, Reason} ->
            handle_error(Reason, Data)
    end.

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 初始化所有步骤的运行时状态
init_steps(#{steps := StepDefs}) ->
    init_steps_iter(maps:to_list(StepDefs), #{}).

%% @private 逐步初始化步骤（递归）
init_steps_iter([], Acc) -> {ok, Acc};
init_steps_iter([{StepId, StepDef} | Rest], Acc) ->
    case beamai_process_step:init_step(StepDef) of
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
    case Caller of
        undefined -> ok;
        Pid -> Pid ! {process_completed, self(), StepsState}
    end,
    {next_state, completed, Data}.

%% @private 转入失败状态并通知调用者
transition_to_failed(Reason, #data{caller = Caller} = Data) ->
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
