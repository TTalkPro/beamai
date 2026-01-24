%%%-------------------------------------------------------------------
%%% @doc 流程框架公共 API 门面
%%%
%%% 提供统一的流程构建和运行接口。
%%% 作为 facade 模式实现，委托到具体的 builder/runtime 模块。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process).

-export([
    %% Builder API
    builder/1,
    add_step/3,
    add_step/4,
    on_event/4,
    on_event/5,
    set_initial_event/2,
    set_initial_event/3,
    set_execution_mode/2,
    build/1,

    %% Runtime API
    start/1,
    start/2,
    send_event/2,
    resume/2,
    stop/1,
    get_status/1,
    snapshot/1,
    restore/1,
    restore/2,

    %% Sync API
    run_sync/1,
    run_sync/2
]).

%%====================================================================
%% Builder API
%%====================================================================

%% @doc 创建新的流程构建器
-spec builder(atom()) -> beamai_process_builder:builder().
builder(Name) ->
    beamai_process_builder:new(Name).

%% @doc 添加步骤（使用默认配置）
-spec add_step(beamai_process_builder:builder(), atom(), module()) ->
    beamai_process_builder:builder().
add_step(Builder, StepId, Module) ->
    beamai_process_builder:add_step(Builder, StepId, Module).

%% @doc 添加步骤（使用自定义配置）
-spec add_step(beamai_process_builder:builder(), atom(), module(), map()) ->
    beamai_process_builder:builder().
add_step(Builder, StepId, Module, Config) ->
    beamai_process_builder:add_step(Builder, StepId, Module, Config).

%% @doc 绑定事件到步骤输入
-spec on_event(beamai_process_builder:builder(), atom(), atom(), atom()) ->
    beamai_process_builder:builder().
on_event(Builder, EventName, TargetStep, TargetInput) ->
    Binding = beamai_process_event:binding(EventName, TargetStep, TargetInput),
    beamai_process_builder:add_binding(Builder, Binding).

%% @doc 绑定事件到步骤输入（带转换函数）
-spec on_event(beamai_process_builder:builder(), atom(), atom(), atom(),
               beamai_process_event:transform_fun()) ->
    beamai_process_builder:builder().
on_event(Builder, EventName, TargetStep, TargetInput, Transform) ->
    Binding = beamai_process_event:binding(EventName, TargetStep, TargetInput, Transform),
    beamai_process_builder:add_binding(Builder, Binding).

%% @doc 设置初始事件（仅指定事件名）
-spec set_initial_event(beamai_process_builder:builder(), atom()) ->
    beamai_process_builder:builder().
set_initial_event(Builder, EventName) ->
    set_initial_event(Builder, EventName, #{}).

%% @doc 设置初始事件（指定事件名和数据）
-spec set_initial_event(beamai_process_builder:builder(), atom(), term()) ->
    beamai_process_builder:builder().
set_initial_event(Builder, EventName, Data) ->
    Event = beamai_process_event:new(EventName, Data),
    beamai_process_builder:add_initial_event(Builder, Event).

%% @doc 设置执行模式（concurrent 并发 | sequential 顺序）
-spec set_execution_mode(beamai_process_builder:builder(), concurrent | sequential) ->
    beamai_process_builder:builder().
set_execution_mode(Builder, Mode) ->
    beamai_process_builder:set_execution_mode(Builder, Mode).

%% @doc 编译构建器为流程定义
-spec build(beamai_process_builder:builder()) ->
    {ok, beamai_process_builder:process_spec()} | {error, [term()]}.
build(Builder) ->
    beamai_process_builder:compile(Builder).

%%====================================================================
%% Runtime API
%%====================================================================

%% @doc 从编译后的流程定义启动流程
-spec start(beamai_process_builder:process_spec()) ->
    {ok, pid()} | {error, term()}.
start(ProcessSpec) ->
    start(ProcessSpec, #{}).

%% @doc 从编译后的流程定义启动流程（带选项）
-spec start(beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
start(ProcessSpec, Opts) ->
    beamai_process_sup:start_runtime(ProcessSpec, Opts).

%% @doc 向运行中的流程发送事件
-spec send_event(pid(), beamai_process_event:event()) -> ok.
send_event(Pid, Event) ->
    beamai_process_runtime:send_event(Pid, Event).

%% @doc 恢复已暂停的流程
-spec resume(pid(), term()) -> ok | {error, term()}.
resume(Pid, Data) ->
    beamai_process_runtime:resume(Pid, Data).

%% @doc 停止运行中的流程
-spec stop(pid()) -> ok.
stop(Pid) ->
    beamai_process_runtime:stop(Pid).

%% @doc 获取流程状态
-spec get_status(pid()) -> {ok, map()}.
get_status(Pid) ->
    beamai_process_runtime:get_status(Pid).

%% @doc 获取流程状态快照
-spec snapshot(pid()) -> {ok, beamai_process_state:snapshot()}.
snapshot(Pid) ->
    beamai_process_runtime:snapshot(Pid).

%% @doc 从快照恢复流程（默认选项）
-spec restore(beamai_process_state:snapshot()) ->
    {ok, pid()} | {error, term()}.
restore(Snapshot) ->
    restore(Snapshot, #{}).

%% @doc 从快照恢复流程（自定义选项）
-spec restore(beamai_process_state:snapshot(), map()) ->
    {ok, pid()} | {error, term()}.
restore(Snapshot, Opts) ->
    case beamai_process_state:restore_from_snapshot(Snapshot) of
        {ok, #{process_spec := ProcessSpec, event_queue := EventQueue,
               current_state := _CurrentState} = _Restored} ->
            RestoreOpts = Opts#{restored => true},
            ProcessSpecWithQueue = ProcessSpec#{
                initial_events => EventQueue
            },
            start(ProcessSpecWithQueue, RestoreOpts);
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Sync API
%%====================================================================

%% @doc 启动流程并同步等待完成
-spec run_sync(beamai_process_builder:process_spec()) ->
    {ok, map()} | {error, term()}.
run_sync(ProcessSpec) ->
    run_sync(ProcessSpec, #{}).

%% @doc 启动流程并同步等待完成（带选项，可设置超时）
-spec run_sync(beamai_process_builder:process_spec(), map()) ->
    {ok, map()} | {error, term()}.
run_sync(ProcessSpec, Opts) ->
    Timeout = maps:get(timeout, Opts, 30000),
    OptsWithCaller = Opts#{caller => self()},
    case start(ProcessSpec, OptsWithCaller) of
        {ok, Pid} ->
            MonRef = monitor(process, Pid),
            receive
                {process_completed, Pid, StepsState} ->
                    demonitor(MonRef, [flush]),
                    {ok, StepsState};
                {process_failed, Pid, Reason} ->
                    demonitor(MonRef, [flush]),
                    {error, Reason};
                {'DOWN', MonRef, process, Pid, Reason} ->
                    {error, {process_died, Reason}}
            after Timeout ->
                demonitor(MonRef, [flush]),
                stop(Pid),
                {error, timeout}
            end;
        {error, _} = Error ->
            Error
    end.
