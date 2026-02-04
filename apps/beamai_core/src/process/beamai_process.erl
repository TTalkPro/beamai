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
    diff_branches/3,

    %% Time Travel API
    go_back/4,
    go_back/5,
    go_forward/4,
    go_forward/5,
    goto_snapshot/4,
    goto_snapshot/5,
    list_history/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type branch_info() :: #{
    thread_id := binary(),
    snapshot_id := binary(),
    branch_name => binary(),
    process_name => atom(),
    process_state => atom(),
    timestamp := integer()
}.

-type snapshot_info() :: #{
    snapshot_id := binary(),
    thread_id := binary(),
    process_name => atom(),
    process_state => atom(),
    step_id => atom(),
    timestamp := integer()
}.

-export_type([branch_info/0, snapshot_info/0]).

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
%% - store: {Module, Ref}，存储后端（用于自动 checkpoint）
%% - checkpoint_policy: map()，checkpoint 策略配置
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

%% @doc 从指定快照创建分支
%%
%% 在 Memory 中从指定 thread 的最新快照创建一个新分支，
%% 用于探索替代执行路径。
%%
%% Memory: beamai_memory 实例
%% SnapshotConfig: #{thread_id => binary()}，标识源快照所在线程
%% BranchOpts: #{branch_name => binary(), thread_id => binary()}
%%
%% 返回新分支的 thread_id 和初始 snapshot_id。
-spec branch_from(beamai_memory:memory(), map(), map()) ->
    {ok, #{branch_thread_id := binary(), snapshot_id := binary()}} | {error, term()}.
branch_from(Memory, SnapshotConfig, BranchOpts) ->
    ThreadId = maps:get(thread_id, SnapshotConfig),
    BranchName = maps:get(branch_name, BranchOpts, <<"branch">>),
    BranchThreadId = maps:get(thread_id, BranchOpts,
                              generate_branch_thread_id(ThreadId, BranchName)),

    %% 获取源线程的最新快照
    case beamai_memory:get_latest_snapshot(Memory, ThreadId) of
        {ok, Snapshot} ->
            SnapshotId = beamai_snapshot:get_id(Snapshot),
            %% 从快照创建分支
            case beamai_memory:snapshot_fork(Memory, SnapshotId, BranchName, BranchThreadId) of
                {ok, BranchSnapshot, _NewMemory} ->
                    BranchSnapshotId = beamai_snapshot:get_id(BranchSnapshot),
                    {ok, #{branch_thread_id => BranchThreadId,
                           snapshot_id => BranchSnapshotId}};
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.

%% @doc 从分支恢复执行（启动新 runtime）
%%
%% 从分支的最新快照恢复流程执行状态，用提供的 ProcessSpec 替换
%% 快照中的 process_spec（因为函数引用无法序列化），然后启动新的 runtime。
%%
%% Memory: beamai_memory 实例
%% BranchConfig: #{thread_id => binary()}，标识分支线程
%% ProcessSpec: 编译后的流程定义（包含函数引用）
%% Opts: runtime 启动选项
-spec restore_branch(beamai_memory:memory(), map(),
                     beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
restore_branch(Memory, BranchConfig, ProcessSpec, Opts) ->
    ThreadId = maps:get(thread_id, BranchConfig),
    Config = #{thread_id => ThreadId},
    case beamai_memory:load_snapshot(Memory, Config) of
        {ok, Values} ->
            %% Values 即 process snapshot map
            %% 替换 process_spec（传入的版本包含函数引用）
            Snapshot = Values#{process_spec => ProcessSpec},
            restore(Snapshot, Opts);
        {error, _} = Error -> Error
    end.

%% @doc 列出指定 Memory 中所有分支
-spec list_branches(beamai_memory:memory(), map()) ->
    {ok, [branch_info()]} | {error, term()}.
list_branches(Memory, _Opts) ->
    case beamai_memory:snapshot_branches(Memory) of
        {ok, Branches} ->
            BranchInfos = [memory_branch_to_info(B) || B <- Branches],
            {ok, BranchInfos};
        {error, _} = Error -> Error
    end.

%% @doc 获取执行谱系（从当前快照回溯到根）
-spec get_lineage(beamai_memory:memory(), map()) ->
    {ok, [snapshot_info()]} | {error, term()}.
get_lineage(Memory, Config) ->
    ThreadId = maps:get(thread_id, Config),
    case beamai_memory:get_latest_snapshot(Memory, ThreadId) of
        {ok, Snapshot} ->
            SnapshotId = beamai_snapshot:get_id(Snapshot),
            case beamai_memory:snapshot_lineage(Memory, SnapshotId) of
                {ok, States} ->
                    Infos = [state_to_snapshot_info(S) || S <- States],
                    {ok, Infos};
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.

%% @doc 比较两个分支/线程的快照差异
%%
%% Config1, Config2 各包含 #{thread_id => binary()}，
%% 比较两个线程最新快照的状态差异。
-spec diff_branches(beamai_memory:memory(), map(), map()) ->
    {ok, map()} | {error, term()}.
diff_branches(Memory, Config1, Config2) ->
    SnapshotManager = beamai_memory:get_snapshot_manager(Memory),
    beamai_snapshot_manager:diff(SnapshotManager, Config1, Config2).

%%====================================================================
%% Time Travel API
%%====================================================================

%% @doc 回退 N 步并恢复执行（默认选项）
%%
%% 在 Memory 中回退 N 个快照，加载过去的流程状态，
%% 替换 process_spec 后启动新的 runtime。
-spec go_back(beamai_memory:memory(), map(), pos_integer(),
              beamai_process_builder:process_spec()) ->
    {ok, pid()} | {error, term()}.
go_back(Memory, Config, Steps, ProcessSpec) ->
    go_back(Memory, Config, Steps, ProcessSpec, #{}).

%% @doc 回退 N 步并恢复执行（自定义选项）
-spec go_back(beamai_memory:memory(), map(), pos_integer(),
              beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
go_back(Memory, Config, Steps, ProcessSpec, Opts) ->
    case beamai_memory:snapshot_go_back(Memory, Config, Steps) of
        {ok, PastState} ->
            restore_from_memory_state(PastState, ProcessSpec, Opts);
        {error, _} = Error -> Error
    end.

%% @doc 前进 N 步并恢复执行（默认选项）
%%
%% 在 Memory 中前进 N 个快照（从当前位置向未来方向），
%% 加载目标状态后启动新的 runtime。
-spec go_forward(beamai_memory:memory(), map(), pos_integer(),
                 beamai_process_builder:process_spec()) ->
    {ok, pid()} | {error, term()}.
go_forward(Memory, Config, Steps, ProcessSpec) ->
    go_forward(Memory, Config, Steps, ProcessSpec, #{}).

%% @doc 前进 N 步并恢复执行（自定义选项）
-spec go_forward(beamai_memory:memory(), map(), pos_integer(),
                 beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
go_forward(Memory, Config, Steps, ProcessSpec, Opts) ->
    case beamai_memory:snapshot_go_forward(Memory, Config, Steps) of
        {ok, FutureState} ->
            restore_from_memory_state(FutureState, ProcessSpec, Opts);
        {error, _} = Error -> Error
    end.

%% @doc 跳转到指定快照并恢复执行（默认选项）
%%
%% 跳转到指定 SnapshotId 的快照，加载其状态后启动新的 runtime。
-spec goto_snapshot(beamai_memory:memory(), map(), binary(),
                    beamai_process_builder:process_spec()) ->
    {ok, pid()} | {error, term()}.
goto_snapshot(Memory, Config, SnapshotId, ProcessSpec) ->
    goto_snapshot(Memory, Config, SnapshotId, ProcessSpec, #{}).

%% @doc 跳转到指定快照并恢复执行（自定义选项）
-spec goto_snapshot(beamai_memory:memory(), map(), binary(),
                    beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
goto_snapshot(Memory, Config, SnapshotId, ProcessSpec, Opts) ->
    case beamai_memory:snapshot_goto(Memory, Config, SnapshotId) of
        {ok, TargetState} ->
            restore_from_memory_state(TargetState, ProcessSpec, Opts);
        {error, _} = Error -> Error
    end.

%% @doc 列出执行历史
%%
%% 返回指定线程的快照历史列表，可用于选择要跳转到的目标。
-spec list_history(beamai_memory:memory(), map()) ->
    {ok, [map()]} | {error, term()}.
list_history(Memory, Config) ->
    beamai_memory:snapshot_history(Memory, Config).

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

%%====================================================================
%% Branch 内部函数
%%====================================================================

%% @private 生成分支 thread_id
-spec generate_branch_thread_id(binary(), binary()) -> binary().
generate_branch_thread_id(ParentThreadId, BranchName) ->
    Ts = integer_to_binary(erlang:system_time(microsecond)),
    <<ParentThreadId/binary, "_branch_", BranchName/binary, "_", Ts/binary>>.

%% @private 将 memory 分支信息转换为 branch_info()
-spec memory_branch_to_info(map()) -> branch_info().
memory_branch_to_info(MemoryBranch) ->
    #{
        thread_id => maps:get(branch_id, MemoryBranch, <<>>),
        snapshot_id => maps:get(head_snapshot_id, MemoryBranch, <<>>),
        branch_name => maps:get(branch_id, MemoryBranch, <<>>),
        timestamp => maps:get(created_at, MemoryBranch, 0)
    }.

%% @private 将 memory state 转换为 snapshot_info()
-spec state_to_snapshot_info(map()) -> snapshot_info().
state_to_snapshot_info(State) ->
    #{
        snapshot_id => maps:get(snapshot_id, State, undefined),
        thread_id => maps:get(thread_id, State, undefined),
        process_name => maps:get(process_name, State, undefined),
        process_state => maps:get(current_state, State, undefined),
        step_id => maps:get(paused_step, State, undefined),
        timestamp => maps:get(timestamp, State, 0)
    }.
