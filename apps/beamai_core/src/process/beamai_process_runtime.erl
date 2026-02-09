%%%-------------------------------------------------------------------
%%% @doc 流程运行时 - 薄 gen_server 壳
%%%
%%% 委托核心逻辑到 beamai_process_engine（纯函数），
%%% 自身仅负责：
%%% - OTP 进程生命周期管理
%%% - 消息路由（cast/call/info）
%%% - 并发步骤的 worker 进程 spawn
%%% - 副作用处理（snapshot 持久化、caller 通知、quiescent 回调）
%%%
%%% == 状态转换 ==
%%%
%%% idle -> running -> paused/completed/failed
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_runtime).

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    send_event/2,
    resume/2,
    stop/1,
    get_status/1,
    snapshot/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(POOL_NAME, beamai_process_pool).

-record(state, {
    engine :: beamai_process_engine:engine(),
    caller :: pid() | undefined,
    store :: {module(), term()} | undefined,
    snapshot_policy :: map(),
    snapshot_step_counter = 0 :: non_neg_integer(),
    on_quiescent :: fun((map()) -> ok) | undefined,
    %% 并发步骤结果收集
    pending_count = 0 :: non_neg_integer(),
    pending_results = [] :: [{atom(), term()}]
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动流程运行时进程
-spec start_link(beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
start_link(ProcessSpec, Opts) ->
    gen_server:start_link(?MODULE, {ProcessSpec, Opts}, []).

%% @doc 向运行中的流程发送事件
-spec send_event(pid(), beamai_process_event:event()) -> ok.
send_event(Pid, Event) ->
    gen_server:cast(Pid, {send_event, Event}).

%% @doc 恢复已暂停的流程
-spec resume(pid(), term()) -> ok | {error, term()}.
resume(Pid, Data) ->
    gen_server:call(Pid, {resume, Data}).

%% @doc 停止流程运行时进程
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc 获取流程当前状态信息
-spec get_status(pid()) -> {ok, map()}.
get_status(Pid) ->
    gen_server:call(Pid, get_status).

%% @doc 获取流程状态快照（可序列化）
-spec snapshot(pid()) -> {ok, beamai_process_state:snapshot()}.
snapshot(Pid) ->
    gen_server:call(Pid, snapshot).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @private 初始化流程运行时
init({ProcessSpec, Opts}) ->
    case maps:get(restored, Opts, false) of
        false ->
            init_fresh(ProcessSpec, Opts);
        RestoredState when is_map(RestoredState) ->
            init_restored(RestoredState, Opts)
    end.

%% @private 处理同步调用
handle_call(get_status, _From, #state{engine = Engine} = S) ->
    {reply, {ok, beamai_process_engine:status(Engine)}, S};

handle_call(snapshot, _From, #state{engine = Engine} = S) ->
    {reply, {ok, beamai_process_engine:take_snapshot(Engine)}, S};

handle_call({resume, ResumeData}, From, #state{engine = Engine} = S) ->
    case beamai_process_engine:current_state(Engine) of
        paused ->
            case beamai_process_engine:handle_resume(ResumeData, Engine) of
                {ok, Engine1} ->
                    gen_server:reply(From, ok),
                    S1 = S#state{engine = Engine1},
                    run_engine(S1);
                {error, Reason} ->
                    {reply, {error, Reason}, S};
                {error, Reason, Engine1} ->
                    S1 = S#state{engine = Engine1},
                    S2 = process_effects(S1),
                    {reply, {error, Reason}, S2}
            end;
        _ ->
            {reply, {error, not_paused}, S}
    end;

handle_call(_Request, _From, S) ->
    {reply, {error, unknown_request}, S}.

%% @private 处理异步消息
handle_cast({send_event, Event}, #state{engine = Engine, pending_count = PC} = S) ->
    case beamai_process_engine:current_state(Engine) of
        CS when CS =:= completed; CS =:= failed ->
            {noreply, S};
        _ ->
            Engine1 = beamai_process_engine:inject_event(Event, Engine),
            S1 = S#state{engine = Engine1},
            case PC > 0 of
                true ->
                    %% 正在等待并发结果，仅入队
                    {noreply, S1};
                false ->
                    run_engine(S1)
            end
    end.

%% @private 处理系统消息
handle_info(run, S) ->
    run_engine(S);

handle_info({step_result, StepId, Result},
            #state{pending_count = PC, pending_results = PR, engine = Engine} = S) ->
    NewPR = [{StepId, Result} | PR],
    case length(NewPR) >= PC of
        true ->
            %% 所有并发步骤结果已收集，应用到引擎
            case beamai_process_engine:apply_step_results(NewPR, Engine) of
                {ok, Engine1} ->
                    S1 = S#state{engine = Engine1, pending_count = 0, pending_results = []},
                    S2 = process_effects(S1),
                    {noreply, S2};
                {execute_async, Tasks, Engine1} ->
                    spawn_async_tasks(Tasks),
                    S1 = S#state{
                        engine = Engine1,
                        pending_count = length(Tasks),
                        pending_results = []
                    },
                    S2 = process_effects(S1),
                    {noreply, S2}
            end;
        false ->
            {noreply, S#state{pending_results = NewPR}}
    end;

handle_info(_Info, S) ->
    {noreply, S}.

%% @private 进程终止回调
terminate(_Reason, _S) ->
    ok.

%%====================================================================
%% 内部函数 - 初始化
%%====================================================================

%% @private 全新初始化路径
init_fresh(ProcessSpec, Opts) ->
    case beamai_process_engine:new(ProcessSpec, Opts) of
        {ok, Engine} ->
            State = build_state(Engine, Opts),
            maybe_trigger_run(Engine),
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

%% @private 从快照恢复的初始化路径
init_restored(RestoredState, Opts) ->
    case beamai_process_engine:from_restored(RestoredState, Opts) of
        {ok, Engine} ->
            State = build_state(Engine, Opts),
            maybe_trigger_run(Engine),
            {ok, State}
    end.

%% @private 构建 gen_server 状态
build_state(Engine, Opts) ->
    #state{
        engine = Engine,
        caller = maps:get(caller, Opts, undefined),
        store = maps:get(store, Opts, undefined),
        snapshot_policy = maps:get(snapshot_policy, Opts, default_snapshot_policy()),
        on_quiescent = maps:get(on_quiescent, Opts, undefined)
    }.

%% @private 引擎处于 running 状态时发送 run 消息触发处理
maybe_trigger_run(Engine) ->
    case beamai_process_engine:current_state(Engine) of
        running -> self() ! run;
        _ -> ok
    end.

%%====================================================================
%% 内部函数 - 引擎驱动
%%====================================================================

%% @private 运行引擎并处理结果
run_engine(#state{engine = Engine} = S) ->
    case beamai_process_engine:run(Engine) of
        {ok, Engine1} ->
            S1 = S#state{engine = Engine1},
            S2 = process_effects(S1),
            {noreply, S2};
        {execute_async, Tasks, Engine1} ->
            spawn_async_tasks(Tasks),
            S1 = S#state{
                engine = Engine1,
                pending_count = length(Tasks),
                pending_results = []
            },
            S2 = process_effects(S1),
            {noreply, S2}
    end.

%%====================================================================
%% 内部函数 - 副作用处理
%%====================================================================

%% @private 处理引擎累积的副作用
process_effects(#state{engine = Engine} = S) ->
    {Effects, Engine1} = beamai_process_engine:drain_effects(Engine),
    S1 = S#state{engine = Engine1},
    lists:foldl(fun apply_effect/2, S1, Effects).

%% @private 应用单个副作用
apply_effect({step_completed, StepId}, S) ->
    maybe_snapshot(step_completed, StepId, S);

apply_effect({paused, StepId, _Reason}, S) ->
    maybe_snapshot(paused, StepId, S);

apply_effect(completed, #state{caller = Caller, engine = Engine} = S) ->
    case Caller of
        undefined -> ok;
        Pid -> Pid ! {process_completed, self(), beamai_process_engine:steps_state(Engine)}
    end,
    maybe_snapshot(completed, undefined, S);

apply_effect({failed, Reason}, #state{caller = Caller} = S) ->
    case Caller of
        undefined -> ok;
        Pid -> Pid ! {process_failed, self(), Reason}
    end,
    maybe_snapshot(error, undefined, S);

apply_effect({quiescent, _Reason}, #state{on_quiescent = undefined} = S) ->
    S;
apply_effect({quiescent, Reason}, #state{on_quiescent = Callback, engine = Engine} = S) ->
    Info = beamai_process_engine:build_quiescent_info(Reason, Engine),
    try
        Callback(Info)
    catch
        Class:Error ->
            logger:warning(
                "[beamai_process_runtime] on_quiescent 回调异常: "
                "reason=~p, class=~p, error=~p",
                [Reason, Class, Error]
            )
    end,
    S.

%%====================================================================
%% 内部函数 - 并发步骤 spawn
%%====================================================================

%% @private 为并发步骤生成 worker 进程
spawn_async_tasks(Tasks) ->
    Self = self(),
    lists:foreach(
        fun({StepId, StepState, Inputs, Context}) ->
            spawn_link(fun() ->
                Result = try
                    Worker = poolboy:checkout(?POOL_NAME),
                    try
                        beamai_process_worker:execute_step(Worker, StepState, Inputs, Context)
                    after
                        poolboy:checkin(?POOL_NAME, Worker)
                    end
                catch
                    Class:Error ->
                        {error, {worker_exception, StepId, {Class, Error}}}
                end,
                Self ! {step_result, StepId, Result}
            end)
        end,
        Tasks).

%%====================================================================
%% 内部函数 - 自动 Snapshot
%%====================================================================

%% @private 默认 snapshot 策略
default_snapshot_policy() ->
    #{
        on_step_completed => false,
        on_pause => true,
        on_complete => true,
        on_error => true,
        every_n_steps => 0,
        metadata => #{}
    }.

%% @private 根据策略决定是否执行 snapshot
maybe_snapshot(_Trigger, _StepId, #state{store = undefined} = S) ->
    S;
maybe_snapshot(step_completed, StepId, #state{store = {Module, Ref},
                                               snapshot_policy = Policy,
                                               snapshot_step_counter = Counter,
                                               engine = Engine} = S) ->
    #{name := ProcessName} = beamai_process_engine:status(Engine),
    NewCounter = Counter + 1,
    S1 = S#state{snapshot_step_counter = NewCounter},
    ShouldSave = case maps:get(on_step_completed, Policy, false) of
        true -> true;
        false ->
            N = maps:get(every_n_steps, Policy, 0),
            N > 0 andalso NewCounter rem N =:= 0
    end,
    case ShouldSave of
        false -> S1;
        true ->
            do_save_snapshot(Module, Ref, step_completed, StepId, Policy, ProcessName, Engine),
            S1
    end;
maybe_snapshot(Trigger, StepId, #state{store = {Module, Ref},
                                        snapshot_policy = Policy,
                                        engine = Engine} = S) ->
    PolicyKey = trigger_to_policy_key(Trigger),
    case maps:get(PolicyKey, Policy, false) of
        false -> S;
        true ->
            #{name := ProcessName} = beamai_process_engine:status(Engine),
            do_save_snapshot(Module, Ref, Trigger, StepId, Policy, ProcessName, Engine),
            S
    end.

%% @private 异步保存快照
do_save_snapshot(Module, Ref, Trigger, StepId, Policy, ProcessName, Engine) ->
    Snapshot = beamai_process_engine:take_snapshot(Engine),
    PolicyMetadata = maps:get(metadata, Policy, #{}),
    SaveOpts = #{
        process_name => ProcessName,
        trigger => Trigger,
        step_id => StepId,
        metadata => PolicyMetadata
    },
    spawn(fun() ->
        case Module:save_snapshot(Ref, Snapshot, SaveOpts) of
            {ok, _} -> ok;
            {error, Reason} ->
                logger:warning(
                    "[beamai_process_runtime] snapshot 保存失败: "
                    "trigger=~p, step=~p, reason=~p",
                    [Trigger, StepId, Reason]
                )
        end
    end),
    ok.

%% @private 触发类型映射到策略键
trigger_to_policy_key(step_completed) -> on_step_completed;
trigger_to_policy_key(paused) -> on_pause;
trigger_to_policy_key(completed) -> on_complete;
trigger_to_policy_key(error) -> on_error.
