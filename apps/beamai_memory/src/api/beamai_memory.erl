%%%-------------------------------------------------------------------
%%% @doc Agent Memory 统一 API - 分层架构 v5.0
%%%
%%% 统一 API，提供对 Snapshot (Process) 和 Checkpoint (Graph) 的访问。
%%%
%%% == 架构 ==
%%%
%%% ```
%%% beamai_memory (统一 API)
%%%   │
%%%   ├── beamai_snapshot (Process Framework 快照)
%%%   │   └── beamai_timeline (时间线抽象)
%%%   │       └── beamai_state_store (状态存储)
%%%   │
%%%   ├── beamai_checkpoint (Graph Engine 检查点)
%%%   │   └── beamai_timeline (时间线抽象)
%%%   │       └── beamai_state_store (状态存储)
%%%   │
%%%   └── beamai_store (长期存储)
%%% '''
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 Memory
%%% {ok, Store} = beamai_store_ets:start_link(my_store, #{}),
%%% Backend = {beamai_store_ets, my_store},
%%% {ok, Memory} = beamai_memory:new(#{backend => Backend}),
%%%
%%% %% 使用 Snapshot API (Process)
%%% {ok, Snapshot, Memory2} = beamai_memory:save_snapshot(Memory, ThreadId, ProcessState),
%%% {ok, Snapshot2, Memory3} = beamai_memory:go_back(Memory2, ThreadId, 1),
%%%
%%% %% 使用 Checkpoint API (Graph)
%%% {ok, Checkpoint, Memory4} = beamai_memory:save_checkpoint(Memory3, RunId, PregelState),
%%% {ok, Checkpoint2, Memory5} = beamai_memory:go_back_checkpoint(Memory4, RunId, 1).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory).

-include_lib("beamai_memory/include/beamai_snapshot.hrl").
-include_lib("beamai_memory/include/beamai_checkpoint.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").
-include_lib("beamai_memory/include/beamai_store.hrl").

%% 类型导出
-export_type([memory/0]).

%% 构造函数
-export([new/1, get_thread_id/1]).

%% Agent interrupt 辅助函数
-export([has_pending_interrupt/2, get_interrupt_context/2]).

%%====================================================================
%% Snapshot API (Process Framework)
%%====================================================================

-export([
    %% 核心操作
    save_snapshot/3,
    save_snapshot/4,
    load_snapshot/2,
    delete_snapshot/2,
    get_latest_snapshot/2,

    %% 时间旅行
    snapshot_go_back/3,
    snapshot_go_forward/3,
    snapshot_goto/3,
    snapshot_undo/2,
    snapshot_redo/2,
    snapshot_position/2,

    %% 分支管理
    snapshot_fork/4,
    snapshot_branches/1,
    snapshot_switch_branch/2,

    %% 历史查询
    snapshot_history/2,
    snapshot_history/3,
    snapshot_lineage/2
]).

%%====================================================================
%% Checkpoint API (Graph Engine)
%%====================================================================

-export([
    %% 核心操作
    save_checkpoint/3,
    save_checkpoint/4,
    load_checkpoint/2,
    delete_checkpoint/2,
    get_latest_checkpoint/2,

    %% 时间旅行
    checkpoint_go_back/3,
    checkpoint_go_forward/3,
    checkpoint_goto/3,
    checkpoint_undo/2,
    checkpoint_redo/2,
    checkpoint_position/2,

    %% 分支管理
    checkpoint_fork/4,
    checkpoint_branches/1,
    checkpoint_switch_branch/2,

    %% 历史查询
    checkpoint_history/2,
    checkpoint_history/3,
    checkpoint_lineage/2,

    %% Graph 专用
    retry_vertex/3,
    inject_resume_data/3
]).

%%====================================================================
%% Store API (长期存储)
%%====================================================================

-export([
    put/5,
    get/3,
    delete/3,
    search/3,
    list_namespaces/3
]).

%%====================================================================
%% 内部访问器
%%====================================================================

-export([
    get_snapshot_manager/1,
    get_checkpoint_manager/1,
    get_store/1,
    update_snapshot_manager/2,
    update_checkpoint_manager/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(memory, {
    snapshot_manager :: beamai_snapshot:manager(),
    checkpoint_manager :: beamai_checkpoint:manager(),
    store :: beamai_store:store(),
    thread_id :: binary() | undefined
}).

-opaque memory() :: #memory{}.

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 Memory 实例
-spec new(map()) -> {ok, memory()} | {error, term()}.
new(Opts) ->
    %% 支持 context_store 和 backend 两种键名，优先使用 context_store
    Backend = case maps:find(context_store, Opts) of
        {ok, Store} -> Store;
        error -> maps:get(backend, Opts)
    end,

    %% 创建 State Store
    StateStore = beamai_state_store:new(Backend, #{
        namespace => maps:get(state_namespace, Opts, <<"state">>)
    }),

    %% 创建 Snapshot Manager
    SnapshotMgr = beamai_snapshot:new(StateStore, #{
        max_entries => maps:get(max_snapshots, Opts, 100),
        auto_prune => maps:get(auto_prune, Opts, true)
    }),

    %% 创建 Checkpoint Manager
    CheckpointMgr = beamai_checkpoint:new(StateStore, #{
        max_entries => maps:get(max_checkpoints, Opts, 100),
        auto_prune => maps:get(auto_prune, Opts, true)
    }),

    ThreadId = maps:get(thread_id, Opts, undefined),

    Memory = #memory{
        snapshot_manager = SnapshotMgr,
        checkpoint_manager = CheckpointMgr,
        store = Backend,
        thread_id = ThreadId
    },

    {ok, Memory}.

%% @doc 获取 Memory 实例关联的 thread_id
-spec get_thread_id(memory()) -> binary() | undefined.
get_thread_id(#memory{thread_id = ThreadId}) ->
    ThreadId.

%%====================================================================
%% Snapshot API 实现
%%====================================================================

%% @doc 保存快照
-spec save_snapshot(memory(), binary(), map()) ->
    {ok, beamai_snapshot:snapshot(), memory()} | {error, term()}.
save_snapshot(Memory, ThreadId, ProcessState) ->
    save_snapshot(Memory, ThreadId, ProcessState, #{}).

-spec save_snapshot(memory(), binary(), map(), map()) ->
    {ok, beamai_snapshot:snapshot(), memory()} | {error, term()}.
save_snapshot(#memory{snapshot_manager = Mgr} = Memory, ThreadId, ProcessState, Opts) ->
    case beamai_snapshot:save_from_state(Mgr, ThreadId, ProcessState, Opts) of
        {ok, Snapshot, NewMgr} ->
            {ok, Snapshot, Memory#memory{snapshot_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 加载快照
-spec load_snapshot(memory(), binary()) ->
    {ok, beamai_snapshot:snapshot()} | {error, term()}.
load_snapshot(#memory{snapshot_manager = Mgr}, SnapshotId) ->
    beamai_snapshot:load(Mgr, SnapshotId).

%% @doc 删除快照
-spec delete_snapshot(memory(), binary()) -> {ok, memory()} | {error, term()}.
delete_snapshot(#memory{snapshot_manager = Mgr} = Memory, SnapshotId) ->
    case beamai_snapshot:delete(Mgr, SnapshotId) of
        {ok, NewMgr} ->
            {ok, Memory#memory{snapshot_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取最新快照
-spec get_latest_snapshot(memory(), binary()) ->
    {ok, beamai_snapshot:snapshot()} | {error, term()}.
get_latest_snapshot(#memory{snapshot_manager = Mgr}, ThreadId) ->
    beamai_snapshot:get_latest(Mgr, ThreadId).

%% @doc 回退 N 个快照
-spec snapshot_go_back(memory(), binary(), pos_integer()) ->
    {ok, beamai_snapshot:snapshot(), memory()} | {error, term()}.
snapshot_go_back(#memory{snapshot_manager = Mgr} = Memory, ThreadId, Steps) ->
    case beamai_snapshot:go_back(Mgr, ThreadId, Steps) of
        {ok, Snapshot, NewMgr} ->
            {ok, Snapshot, Memory#memory{snapshot_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 前进 N 个快照
-spec snapshot_go_forward(memory(), binary(), pos_integer()) ->
    {ok, beamai_snapshot:snapshot(), memory()} | {error, term()}.
snapshot_go_forward(#memory{snapshot_manager = Mgr} = Memory, ThreadId, Steps) ->
    case beamai_snapshot:go_forward(Mgr, ThreadId, Steps) of
        {ok, Snapshot, NewMgr} ->
            {ok, Snapshot, Memory#memory{snapshot_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 跳转到指定快照
-spec snapshot_goto(memory(), binary(), binary()) ->
    {ok, beamai_snapshot:snapshot(), memory()} | {error, term()}.
snapshot_goto(#memory{snapshot_manager = Mgr} = Memory, ThreadId, SnapshotId) ->
    case beamai_snapshot:goto(Mgr, ThreadId, SnapshotId) of
        {ok, Snapshot, NewMgr} ->
            {ok, Snapshot, Memory#memory{snapshot_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 撤销
-spec snapshot_undo(memory(), binary()) ->
    {ok, beamai_snapshot:snapshot(), memory()} | {error, term()}.
snapshot_undo(#memory{snapshot_manager = Mgr} = Memory, ThreadId) ->
    case beamai_snapshot:undo(Mgr, ThreadId) of
        {ok, Snapshot, NewMgr} ->
            {ok, Snapshot, Memory#memory{snapshot_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 重做
-spec snapshot_redo(memory(), binary()) ->
    {ok, beamai_snapshot:snapshot(), memory()} | {error, term()}.
snapshot_redo(#memory{snapshot_manager = Mgr} = Memory, ThreadId) ->
    case beamai_snapshot:redo(Mgr, ThreadId) of
        {ok, Snapshot, NewMgr} ->
            {ok, Snapshot, Memory#memory{snapshot_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取当前位置
-spec snapshot_position(memory(), binary()) ->
    {ok, beamai_timeline:position()} | {error, term()}.
snapshot_position(#memory{snapshot_manager = Mgr}, ThreadId) ->
    beamai_snapshot:get_current_position(Mgr, ThreadId).

%% @doc 从快照创建分支
-spec snapshot_fork(memory(), binary(), binary(), binary()) ->
    {ok, beamai_snapshot:snapshot(), memory()} | {error, term()}.
snapshot_fork(#memory{snapshot_manager = Mgr} = Memory, SnapshotId, BranchName, ThreadId) ->
    case beamai_snapshot:fork_from(Mgr, SnapshotId, BranchName, ThreadId) of
        {ok, Snapshot, NewMgr} ->
            {ok, Snapshot, Memory#memory{snapshot_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 列出所有分支
-spec snapshot_branches(memory()) -> [beamai_timeline:branch()].
snapshot_branches(#memory{snapshot_manager = Mgr}) ->
    beamai_snapshot:list_branches(Mgr).

%% @doc 切换分支
-spec snapshot_switch_branch(memory(), binary()) -> {ok, memory()} | {error, term()}.
snapshot_switch_branch(#memory{snapshot_manager = Mgr} = Memory, BranchId) ->
    case beamai_snapshot:switch_branch(Mgr, BranchId) of
        {ok, NewMgr} ->
            {ok, Memory#memory{snapshot_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取历史记录
-spec snapshot_history(memory(), binary()) ->
    {ok, [beamai_snapshot:snapshot()]} | {error, term()}.
snapshot_history(#memory{snapshot_manager = Mgr}, ThreadId) ->
    beamai_snapshot:get_history(Mgr, ThreadId).

-spec snapshot_history(memory(), binary(), map()) ->
    {ok, [beamai_snapshot:snapshot()]} | {error, term()}.
snapshot_history(#memory{snapshot_manager = Mgr}, ThreadId, Opts) ->
    beamai_snapshot:get_history(Mgr, ThreadId, Opts).

%% @doc 获取血统
-spec snapshot_lineage(memory(), binary()) ->
    {ok, [beamai_snapshot:snapshot()]} | {error, term()}.
snapshot_lineage(#memory{snapshot_manager = Mgr}, SnapshotId) ->
    beamai_snapshot:get_lineage(Mgr, SnapshotId).

%%====================================================================
%% Checkpoint API 实现
%%====================================================================

%% @doc 保存检查点
-spec save_checkpoint(memory(), binary(), map()) ->
    {ok, beamai_checkpoint:checkpoint(), memory()} | {error, term()}.
save_checkpoint(Memory, RunId, PregelState) ->
    save_checkpoint(Memory, RunId, PregelState, #{}).

-spec save_checkpoint(memory(), binary(), map(), map()) ->
    {ok, beamai_checkpoint:checkpoint(), memory()} | {error, term()}.
save_checkpoint(#memory{checkpoint_manager = Mgr} = Memory, RunId, PregelState, Opts) ->
    case beamai_checkpoint:save_from_pregel(Mgr, RunId, PregelState, Opts) of
        {ok, Checkpoint, NewMgr} ->
            {ok, Checkpoint, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 加载检查点
-spec load_checkpoint(memory(), binary()) ->
    {ok, beamai_checkpoint:checkpoint()} | {error, term()}.
load_checkpoint(#memory{checkpoint_manager = Mgr}, CheckpointId) ->
    beamai_checkpoint:load(Mgr, CheckpointId).

%% @doc 删除检查点
-spec delete_checkpoint(memory(), binary()) -> {ok, memory()} | {error, term()}.
delete_checkpoint(#memory{checkpoint_manager = Mgr} = Memory, CheckpointId) ->
    case beamai_checkpoint:delete(Mgr, CheckpointId) of
        {ok, NewMgr} ->
            {ok, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取最新检查点
-spec get_latest_checkpoint(memory(), binary()) ->
    {ok, beamai_checkpoint:checkpoint()} | {error, term()}.
get_latest_checkpoint(#memory{checkpoint_manager = Mgr}, RunId) ->
    beamai_checkpoint:get_latest(Mgr, RunId).

%% @doc 回退 N 个检查点
-spec checkpoint_go_back(memory(), binary(), pos_integer()) ->
    {ok, beamai_checkpoint:checkpoint(), memory()} | {error, term()}.
checkpoint_go_back(#memory{checkpoint_manager = Mgr} = Memory, RunId, Steps) ->
    case beamai_checkpoint:go_back(Mgr, RunId, Steps) of
        {ok, Checkpoint, NewMgr} ->
            {ok, Checkpoint, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 前进 N 个检查点
-spec checkpoint_go_forward(memory(), binary(), pos_integer()) ->
    {ok, beamai_checkpoint:checkpoint(), memory()} | {error, term()}.
checkpoint_go_forward(#memory{checkpoint_manager = Mgr} = Memory, RunId, Steps) ->
    case beamai_checkpoint:go_forward(Mgr, RunId, Steps) of
        {ok, Checkpoint, NewMgr} ->
            {ok, Checkpoint, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 跳转到指定检查点
-spec checkpoint_goto(memory(), binary(), binary()) ->
    {ok, beamai_checkpoint:checkpoint(), memory()} | {error, term()}.
checkpoint_goto(#memory{checkpoint_manager = Mgr} = Memory, RunId, CheckpointId) ->
    case beamai_checkpoint:goto(Mgr, RunId, CheckpointId) of
        {ok, Checkpoint, NewMgr} ->
            {ok, Checkpoint, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 撤销
-spec checkpoint_undo(memory(), binary()) ->
    {ok, beamai_checkpoint:checkpoint(), memory()} | {error, term()}.
checkpoint_undo(#memory{checkpoint_manager = Mgr} = Memory, RunId) ->
    case beamai_checkpoint:undo(Mgr, RunId) of
        {ok, Checkpoint, NewMgr} ->
            {ok, Checkpoint, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 重做
-spec checkpoint_redo(memory(), binary()) ->
    {ok, beamai_checkpoint:checkpoint(), memory()} | {error, term()}.
checkpoint_redo(#memory{checkpoint_manager = Mgr} = Memory, RunId) ->
    case beamai_checkpoint:redo(Mgr, RunId) of
        {ok, Checkpoint, NewMgr} ->
            {ok, Checkpoint, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取当前位置
-spec checkpoint_position(memory(), binary()) ->
    {ok, beamai_timeline:position()} | {error, term()}.
checkpoint_position(#memory{checkpoint_manager = Mgr}, RunId) ->
    beamai_checkpoint:get_current_position(Mgr, RunId).

%% @doc 从检查点创建分支
-spec checkpoint_fork(memory(), binary(), binary(), binary()) ->
    {ok, beamai_checkpoint:checkpoint(), memory()} | {error, term()}.
checkpoint_fork(#memory{checkpoint_manager = Mgr} = Memory, CheckpointId, BranchName, RunId) ->
    case beamai_checkpoint:fork_from(Mgr, CheckpointId, BranchName, RunId) of
        {ok, Checkpoint, NewMgr} ->
            {ok, Checkpoint, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 列出所有分支
-spec checkpoint_branches(memory()) -> [beamai_timeline:branch()].
checkpoint_branches(#memory{checkpoint_manager = Mgr}) ->
    beamai_checkpoint:list_branches(Mgr).

%% @doc 切换分支
-spec checkpoint_switch_branch(memory(), binary()) -> {ok, memory()} | {error, term()}.
checkpoint_switch_branch(#memory{checkpoint_manager = Mgr} = Memory, BranchId) ->
    case beamai_checkpoint:switch_branch(Mgr, BranchId) of
        {ok, NewMgr} ->
            {ok, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取执行路径（历史记录）
-spec checkpoint_history(memory(), binary()) ->
    {ok, [beamai_checkpoint:checkpoint()]} | {error, term()}.
checkpoint_history(#memory{checkpoint_manager = Mgr}, RunId) ->
    beamai_checkpoint:get_history(Mgr, RunId).

-spec checkpoint_history(memory(), binary(), map()) ->
    {ok, [beamai_checkpoint:checkpoint()]} | {error, term()}.
checkpoint_history(#memory{checkpoint_manager = Mgr}, RunId, Opts) ->
    beamai_checkpoint:get_history(Mgr, RunId, Opts).

%% @doc 获取血统
-spec checkpoint_lineage(memory(), binary()) ->
    {ok, [beamai_checkpoint:checkpoint()]} | {error, term()}.
checkpoint_lineage(#memory{checkpoint_manager = Mgr}, CheckpointId) ->
    beamai_checkpoint:get_lineage(Mgr, CheckpointId).

%% @doc 重试失败顶点
-spec retry_vertex(memory(), binary(), atom()) ->
    {ok, beamai_checkpoint:checkpoint(), memory()} | {error, term()}.
retry_vertex(#memory{checkpoint_manager = Mgr} = Memory, RunId, VertexId) ->
    case beamai_checkpoint:retry_vertex(Mgr, RunId, VertexId) of
        {ok, Checkpoint, NewMgr} ->
            {ok, Checkpoint, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%% @doc 注入恢复数据
-spec inject_resume_data(memory(), binary(), map()) ->
    {ok, beamai_checkpoint:checkpoint(), memory()} | {error, term()}.
inject_resume_data(#memory{checkpoint_manager = Mgr} = Memory, RunId, Data) ->
    case beamai_checkpoint:inject_resume_data(Mgr, RunId, Data) of
        {ok, Checkpoint, NewMgr} ->
            {ok, Checkpoint, Memory#memory{checkpoint_manager = NewMgr}};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Store API 实现
%%====================================================================

%% @doc 存储数据
-spec put(memory(), [binary()], binary(), map(), map()) -> ok | {error, term()}.
put(#memory{store = Store}, Namespace, Key, Value, Opts) ->
    beamai_store:put(Store, Namespace, Key, Value, Opts).

%% @doc 获取数据
-spec get(memory(), [binary()], binary()) ->
    {ok, beamai_store:item()} | {error, term()}.
get(#memory{store = Store}, Namespace, Key) ->
    beamai_store:get(Store, Namespace, Key).

%% @doc 删除数据
-spec delete(memory(), [binary()], binary()) -> ok | {error, term()}.
delete(#memory{store = Store}, Namespace, Key) ->
    beamai_store:delete(Store, Namespace, Key).

%% @doc 搜索数据
-spec search(memory(), [binary()], map()) ->
    {ok, [beamai_store:search_result()]} | {error, term()}.
search(#memory{store = Store}, NamespacePrefix, Opts) ->
    beamai_store:search(Store, NamespacePrefix, Opts).

%% @doc 列出命名空间
-spec list_namespaces(memory(), [binary()], map()) ->
    {ok, [[binary()]]} | {error, term()}.
list_namespaces(#memory{store = Store}, Prefix, Opts) ->
    beamai_store:list_namespaces(Store, Prefix, Opts).

%%====================================================================
%% 内部访问器
%%====================================================================

%% @doc 获取 Snapshot Manager
-spec get_snapshot_manager(memory()) -> beamai_snapshot:manager().
get_snapshot_manager(#memory{snapshot_manager = Mgr}) ->
    Mgr.

%% @doc 获取 Checkpoint Manager
-spec get_checkpoint_manager(memory()) -> beamai_checkpoint:manager().
get_checkpoint_manager(#memory{checkpoint_manager = Mgr}) ->
    Mgr.

%% @doc 获取 Store
-spec get_store(memory()) -> beamai_store:store().
get_store(#memory{store = Store}) ->
    Store.

%% @doc 更新 Snapshot Manager
-spec update_snapshot_manager(memory(), beamai_snapshot:manager()) -> memory().
update_snapshot_manager(Memory, Mgr) ->
    Memory#memory{snapshot_manager = Mgr}.

%% @doc 更新 Checkpoint Manager
-spec update_checkpoint_manager(memory(), beamai_checkpoint:manager()) -> memory().
update_checkpoint_manager(Memory, Mgr) ->
    Memory#memory{checkpoint_manager = Mgr}.

%%====================================================================
%% Agent Interrupt 辅助函数
%%====================================================================

%% @doc 检查是否有待处理的中断
%%
%% 检查最新的 snapshot 中是否包含中断状态。
-spec has_pending_interrupt(memory(), map()) -> boolean().
has_pending_interrupt(Memory, Config) ->
    ThreadId = case maps:find(thread_id, Config) of
        {ok, TId} -> TId;
        error -> get_thread_id(Memory)
    end,
    case get_latest_snapshot(Memory, ThreadId) of
        {ok, Snapshot} ->
            StepsState = beamai_snapshot:get_steps_state(Snapshot),
            case maps:find(agent_state, StepsState) of
                {ok, #{state := SavedData}} ->
                    case maps:find(interrupt_state, SavedData) of
                        {ok, IntState} when is_map(IntState) ->
                            maps:get(status, IntState, undefined) =:= interrupted;
                        _ ->
                            false
                    end;
                _ ->
                    false
            end;
        {error, _} ->
            false
    end.

%% @doc 获取中断上下文
%%
%% 从最新的 snapshot 中提取中断状态信息。
-spec get_interrupt_context(memory(), map()) ->
    {ok, map()} | {error, not_interrupted} | {error, term()}.
get_interrupt_context(Memory, Config) ->
    ThreadId = case maps:find(thread_id, Config) of
        {ok, TId} -> TId;
        error -> get_thread_id(Memory)
    end,
    case get_latest_snapshot(Memory, ThreadId) of
        {ok, Snapshot} ->
            StepsState = beamai_snapshot:get_steps_state(Snapshot),
            case maps:find(agent_state, StepsState) of
                {ok, #{state := SavedData}} ->
                    case maps:find(interrupt_state, SavedData) of
                        {ok, IntState} when is_map(IntState) ->
                            case maps:get(status, IntState, undefined) of
                                interrupted ->
                                    {ok, IntState};
                                _ ->
                                    {error, not_interrupted}
                            end;
                        _ ->
                            {error, not_interrupted}
                    end;
                _ ->
                    {error, not_interrupted}
            end;
        {error, not_found} ->
            {error, not_interrupted};
        {error, Reason} ->
            {error, Reason}
    end.
