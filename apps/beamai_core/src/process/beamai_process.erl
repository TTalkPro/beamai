%%%-------------------------------------------------------------------
%%% @doc 流程框架公共 API 门面
%%%
%%% 提供统一的流程构建和运行接口。
%%% 作为 facade 模式实现，委托到具体的 builder/runtime 模块。
%%%
%%% == Builder API ==
%%% 创建构建器 -> 添加步骤和绑定 -> 编译为 process_spec
%%%
%%% == Runtime API ==
%%% 启动/停止流程、发送事件、恢复暂停、获取状态/快照
%%%
%%% == 快照恢复 ==
%%% restore/1,2 从快照恢复流程，将恢复的步骤状态通过 Opts 传入
%%% runtime 的 init_from_restored 路径，避免 init_steps 覆盖已恢复状态。
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
    run_sync/2,

    %% Branch API
    branch_from/3,
    restore_branch/4,
    list_branches/2,
    get_lineage/2,

    %% Time Travel API
    go_back/3,
    go_back/4,
    go_forward/3,
    go_forward/4,
    goto_snapshot/3,
    goto_snapshot/4,
    list_history/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 存储后端引用：{实现模块, 不透明引用}
-type store() :: {module(), term()}.

-export_type([store/0]).

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
%%
%% Opts 支持的选项：
%% - context: beamai_context:t()，共享上下文
%% - caller: pid()，完成/失败时通知的进程
%% - store: {Module, Ref}，存储后端（用于自动 snapshot）
%% - snapshot_policy: map()，snapshot 策略配置
%% - on_quiescent: fun(QuiescentInfo :: map()) -> ok
%%   静止点回调，当所有并发步骤执行完毕时触发。
%%   QuiescentInfo 包含 process_name、reason、status、step_states 等字段。
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
%%
%% 将恢复的步骤状态、流程状态、暂停信息通过 Opts 传入 runtime，
%% 避免 init_steps 重新初始化覆盖已恢复的状态。
-spec restore(beamai_process_state:snapshot(), map()) ->
    {ok, pid()} | {error, term()}.
restore(Snapshot, Opts) ->
    case beamai_process_state:restore_from_snapshot(Snapshot) of
        {ok, #{process_spec := ProcessSpec, event_queue := EventQueue,
               current_state := CurrentState, steps_state := StepsState,
               paused_step := PausedStep, pause_reason := PauseReason}} ->
            RestoreOpts = Opts#{
                restored => true,
                restored_steps_state => StepsState,
                restored_current_state => CurrentState,
                restored_paused_step => PausedStep,
                restored_pause_reason => PauseReason,
                restored_event_queue => EventQueue
            },
            start(ProcessSpec, RestoreOpts);
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

%%====================================================================
%% Branch API
%%====================================================================

%% @doc 从当前快照创建分支
%%
%% 委托到 Store 实现模块的 branch/3 回调。
%%
%% Store: {Module, Ref}，存储后端引用
%% BranchName: 分支名称
%% BranchOpts: 分支选项（如自定义 thread_id）
-spec branch_from(store(), binary(), map()) ->
    {ok, #{branch_thread_id := binary(), snapshot_id := binary()}} | {error, term()}.
branch_from({Module, Ref}, BranchName, BranchOpts) ->
    Module:branch(Ref, BranchName, BranchOpts).

%% @doc 从分支恢复执行（启动新 runtime）
%%
%% 从分支的最新快照恢复流程执行状态，用提供的 ProcessSpec 替换
%% 快照中的 process_spec（因为函数引用无法序列化），然后启动新的 runtime。
%%
%% Store: {Module, Ref}，存储后端引用
%% BranchThreadId: 分支线程 ID
%% ProcessSpec: 编译后的流程定义（包含函数引用）
%% Opts: runtime 启动选项
-spec restore_branch(store(), binary(),
                     beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
restore_branch({Module, Ref}, BranchThreadId, ProcessSpec, Opts) ->
    case Module:load_branch(Ref, BranchThreadId, #{}) of
        {ok, State} ->
            restore_from_memory_state(State, ProcessSpec, Opts);
        {error, _} = Error -> Error
    end.

%% @doc 列出所有分支
-spec list_branches(store(), map()) ->
    {ok, [map()]} | {error, term()}.
list_branches({Module, Ref}, Opts) ->
    Module:list_branches(Ref, Opts).

%% @doc 获取执行谱系（从当前快照回溯到根）
-spec get_lineage(store(), map()) ->
    {ok, [map()]} | {error, term()}.
get_lineage({Module, Ref}, Opts) ->
    Module:get_lineage(Ref, Opts).

%%====================================================================
%% Time Travel API
%%====================================================================

%% @doc 回退 N 步并恢复执行（默认选项）
-spec go_back(store(), pos_integer(),
              beamai_process_builder:process_spec()) ->
    {ok, pid()} | {error, term()}.
go_back(Store, Steps, ProcessSpec) ->
    go_back(Store, Steps, ProcessSpec, #{}).

%% @doc 回退 N 步并恢复执行（自定义选项）
-spec go_back(store(), pos_integer(),
              beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
go_back({Module, Ref}, Steps, ProcessSpec, Opts) ->
    case Module:go_back(Ref, Steps) of
        {ok, PastState} ->
            restore_from_memory_state(PastState, ProcessSpec, Opts);
        {error, _} = Error -> Error
    end.

%% @doc 前进 N 步并恢复执行（默认选项）
-spec go_forward(store(), pos_integer(),
                 beamai_process_builder:process_spec()) ->
    {ok, pid()} | {error, term()}.
go_forward(Store, Steps, ProcessSpec) ->
    go_forward(Store, Steps, ProcessSpec, #{}).

%% @doc 前进 N 步并恢复执行（自定义选项）
-spec go_forward(store(), pos_integer(),
                 beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
go_forward({Module, Ref}, Steps, ProcessSpec, Opts) ->
    case Module:go_forward(Ref, Steps) of
        {ok, FutureState} ->
            restore_from_memory_state(FutureState, ProcessSpec, Opts);
        {error, _} = Error -> Error
    end.

%% @doc 跳转到指定快照并恢复执行（默认选项）
-spec goto_snapshot(store(), binary(),
                    beamai_process_builder:process_spec()) ->
    {ok, pid()} | {error, term()}.
goto_snapshot(Store, SnapshotId, ProcessSpec) ->
    goto_snapshot(Store, SnapshotId, ProcessSpec, #{}).

%% @doc 跳转到指定快照并恢复执行（自定义选项）
-spec goto_snapshot(store(), binary(),
                    beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
goto_snapshot({Module, Ref}, SnapshotId, ProcessSpec, Opts) ->
    case Module:goto(Ref, SnapshotId) of
        {ok, TargetState} ->
            restore_from_memory_state(TargetState, ProcessSpec, Opts);
        {error, _} = Error -> Error
    end.

%% @doc 列出执行历史
-spec list_history(store()) ->
    {ok, [map()]} | {error, term()}.
list_history({Module, Ref}) ->
    Module:list_history(Ref).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从 memory 状态恢复流程执行
%%
%% 将 memory 返回的状态 map 作为 process snapshot，
%% 替换 process_spec（函数引用不可序列化）后启动新 runtime。
-spec restore_from_memory_state(map(), beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
restore_from_memory_state(State, ProcessSpec, Opts) ->
    Snapshot = State#{process_spec => ProcessSpec},
    restore(Snapshot, Opts).
