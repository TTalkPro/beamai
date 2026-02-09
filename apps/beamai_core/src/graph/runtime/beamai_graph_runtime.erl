%%%-------------------------------------------------------------------
%%% @doc 图运行时 - 薄 gen_server 壳
%%%
%%% 委托核心逻辑到 beamai_graph_engine（纯函数），
%%% 自身仅负责：
%%% - OTP 进程生命周期管理
%%% - 自动运行循环（do_step 循环）
%%% - 副作用处理（snapshot 持久化、caller 通知）
%%% - interrupt/error 时暂停等待 resume/retry
%%%
%%% == 状态转换 ==
%%%
%%% running -> interrupted/completed/error
%%%       interrupted -> running (via resume/retry)
%%%       error -> running (via retry)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_runtime).

-behaviour(gen_server).

%% API
-export([start_link/3, resume/2, retry/2,
         get_status/1, snapshot/1, stop/1]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    engine          :: beamai_graph_engine:engine(),
    runtime_state   :: running | interrupted | completed | error,
    caller          :: pid() | undefined,
    store           :: {module(), term()} | undefined,
    snapshot_policy :: map(),
    on_quiescent    :: fun((map()) -> ok) | undefined
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动图运行时进程
-spec start_link(beamai_graph_builder:graph(), beamai_graph_engine:state(), map()) ->
    {ok, pid()} | {error, term()}.
start_link(Graph, InitialState, Opts) ->
    gen_server:start_link(?MODULE, {Graph, InitialState, Opts}, []).

%% @doc 恢复已中断的图执行
-spec resume(pid(), #{beamai_graph_engine:vertex_id() => term()}) ->
    ok | {error, term()}.
resume(Pid, ResumeData) ->
    gen_server:call(Pid, {resume, ResumeData}, infinity).

%% @doc 重试失败的顶点
-spec retry(pid(), [beamai_graph_engine:vertex_id()]) ->
    ok | {error, term()}.
retry(Pid, VertexIds) ->
    gen_server:call(Pid, {retry, VertexIds}, infinity).

%% @doc 获取图执行当前状态信息
-spec get_status(pid()) -> {ok, map()}.
get_status(Pid) ->
    gen_server:call(Pid, get_status, infinity).

%% @doc 获取图执行状态快照
-spec snapshot(pid()) -> {ok, beamai_graph_state:snapshot()}.
snapshot(Pid) ->
    gen_server:call(Pid, snapshot, infinity).

%% @doc 停止图运行时进程
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @private 初始化
init({Graph, InitialState, Opts}) ->
    case maps:get(restore_from, Opts, undefined) of
        undefined ->
            init_fresh(Graph, InitialState, Opts);
        Restored ->
            init_restored(Graph, Restored, Opts)
    end.

%% @private 处理同步调用
handle_call(get_status, _From, #state{engine = Engine, runtime_state = RS} = S) ->
    Status = #{
        current_state => RS,
        superstep => beamai_graph_engine:superstep(Engine)
    },
    {reply, {ok, Status}, S};

handle_call(snapshot, _From, #state{engine = Engine} = S) ->
    {reply, {ok, beamai_graph_engine:take_snapshot(Engine)}, S};

handle_call({resume, ResumeData}, From, #state{engine = Engine,
                                                runtime_state = interrupted} = S) ->
    {_StepResult, Engine1} = beamai_graph_engine:do_resume(ResumeData, Engine),
    gen_server:reply(From, ok),
    S1 = S#state{engine = Engine1, runtime_state = running},
    self() ! run_step,
    {noreply, S1};

handle_call({resume, _ResumeData}, _From, S) ->
    {reply, {error, not_interrupted}, S};

handle_call({retry, VertexIds}, From, #state{engine = Engine,
                                              runtime_state = RS} = S)
    when RS =:= interrupted; RS =:= error ->
    {_StepResult, Engine1} = beamai_graph_engine:do_retry(VertexIds, Engine),
    gen_server:reply(From, ok),
    S1 = S#state{engine = Engine1, runtime_state = running},
    self() ! run_step,
    {noreply, S1};

handle_call({retry, _VertexIds}, _From, S) ->
    {reply, {error, not_retryable}, S};

handle_call(_Request, _From, S) ->
    {reply, {error, unknown_request}, S}.

%% @private 处理异步消息
handle_cast(_Msg, S) ->
    {noreply, S}.

%% @private 处理系统消息
handle_info(run_step, #state{engine = Engine, runtime_state = running} = S) ->
    {StepResult, Engine1} = beamai_graph_engine:do_step(Engine),
    S1 = S#state{engine = Engine1},
    handle_step_result(StepResult, S1);

handle_info(run_step, S) ->
    %% 不在 running 状态，忽略
    {noreply, S};

handle_info(_Info, S) ->
    {noreply, S}.

%% @private 进程终止回调
terminate(_Reason, _S) ->
    ok.

%%====================================================================
%% 内部函数 - 初始化
%%====================================================================

%% @private 全新初始化路径
init_fresh(Graph, InitialState, Opts) ->
    #{pregel_graph := PregelGraph} = Graph,
    ComputeFn = beamai_graph_compute:compute_fn(),
    FieldReducers = maps:get(field_reducers, Opts, #{}),

    EngineOpts = #{
        max_supersteps => maps:get(max_supersteps, Opts, 100),
        global_state => InitialState,
        field_reducers => FieldReducers
    },

    {ok, Engine} = beamai_graph_engine:new(PregelGraph, ComputeFn, EngineOpts),
    State = build_state(Engine, running, Opts),
    self() ! run_step,
    {ok, State}.

%% @private 从恢复数据初始化
init_restored(Graph, Restored, Opts) ->
    #{pregel_graph := PregelGraph} = Graph,
    ComputeFn = beamai_graph_compute:compute_fn(),
    FieldReducers = maps:get(field_reducers, Opts, #{}),

    {ok, Engine} = beamai_graph_engine:from_restored(Restored,
        #{pregel_graph => PregelGraph,
          compute_fn => ComputeFn,
          field_reducers => FieldReducers}),

    State = build_state(Engine, running, Opts),
    self() ! run_step,
    {ok, State}.

%% @private 构建 gen_server 状态
build_state(Engine, RuntimeState, Opts) ->
    #state{
        engine = Engine,
        runtime_state = RuntimeState,
        caller = maps:get(caller, Opts, undefined),
        store = maps:get(store, Opts, undefined),
        snapshot_policy = maps:get(snapshot_policy, Opts, default_snapshot_policy()),
        on_quiescent = maps:get(on_quiescent, Opts, undefined)
    }.

%%====================================================================
%% 内部函数 - 步骤结果处理
%%====================================================================

%% @private 处理单步结果
handle_step_result({continue, #{type := interrupt} = Info}, S) ->
    S1 = maybe_snapshot(interrupted, S),
    notify_caller({graph_interrupted, self(), Info}, S1),
    {noreply, S1#state{runtime_state = interrupted}};

handle_step_result({continue, #{type := error} = Info}, S) ->
    S1 = maybe_snapshot(error, S),
    notify_caller({graph_error, self(), Info}, S1),
    {noreply, S1#state{runtime_state = error}};

handle_step_result({continue, _Info}, S) ->
    S1 = maybe_snapshot(superstep_completed, S),
    self() ! run_step,
    {noreply, S1};

handle_step_result({done, _Reason, _Info}, #state{engine = Engine} = S) ->
    S1 = maybe_snapshot(completed, S),
    FinalState = beamai_graph_engine:global_state(Engine),
    notify_caller({graph_completed, self(), FinalState}, S1),
    {noreply, S1#state{runtime_state = completed}}.

%%====================================================================
%% 内部函数 - 通知
%%====================================================================

%% @private 通知 caller
notify_caller(_Msg, #state{caller = undefined}) -> ok;
notify_caller(Msg, #state{caller = Pid}) -> Pid ! Msg.

%%====================================================================
%% 内部函数 - 快照策略
%%====================================================================

%% @private 默认 snapshot 策略
default_snapshot_policy() ->
    #{
        on_superstep => false,
        on_interrupt => true,
        on_complete => true,
        on_error => true,
        every_n_steps => 0,
        metadata => #{}
    }.

%% @private 根据策略决定是否执行 snapshot
maybe_snapshot(_Trigger, #state{store = undefined} = S) -> S;
maybe_snapshot(Trigger, #state{store = {Module, Ref},
                                snapshot_policy = Policy,
                                engine = Engine} = S) ->
    PolicyKey = trigger_to_policy_key(Trigger),
    case maps:get(PolicyKey, Policy, false) of
        false -> S;
        true ->
            Snapshot = beamai_graph_engine:take_snapshot(Engine),
            SaveOpts = #{
                trigger => Trigger,
                metadata => maps:get(metadata, Policy, #{})
            },
            spawn(fun() ->
                case Module:save_snapshot(Ref, Snapshot, SaveOpts) of
                    {ok, _} -> ok;
                    {error, Reason} ->
                        logger:warning(
                            "[beamai_graph_runtime] snapshot 保存失败: "
                            "trigger=~p, reason=~p",
                            [Trigger, Reason]
                        )
                end
            end),
            S
    end.

%% @private 触发类型映射到策略键
trigger_to_policy_key(superstep_completed) -> on_superstep;
trigger_to_policy_key(interrupted) -> on_interrupt;
trigger_to_policy_key(completed) -> on_complete;
trigger_to_policy_key(error) -> on_error.
