%%%-------------------------------------------------------------------
%%% @doc Process Store 实现 - 基于 beamai_memory 后端
%%%
%%% 实现 beamai_process_store_behaviour，将流程快照存储到
%%% beamai_memory 的 Snapshot Manager 中。
%%%
%%% == Store Ref 格式 ==
%%%
%%% store_ref() = {beamai_memory:memory(), #{thread_id := binary()}}
%%%
%%% == 格式转换 ==
%%%
%%% 保存时：process snapshot map 直接作为 memory snapshot 的 values 存储
%%% 加载时：从 memory snapshot 的 values 提取 process snapshot map
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 Memory 实例
%%% {ok, Memory} = beamai_memory:new(#{
%%%     context_store => {beamai_store_ets, my_store}
%%% }),
%%%
%%% %% 配置 store ref
%%% StoreRef = {Memory, #{thread_id => <<"process-thread-1">>}},
%%%
%%% %% 在 runtime 启动时使用
%%% {ok, Pid} = beamai_process:start(ProcessSpec, #{
%%%     store => {beamai_process_memory_store, StoreRef}
%%% }).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_memory_store).

-behaviour(beamai_process_store_behaviour).

%% Behaviour 回调
-export([
    save_snapshot/3,
    load_snapshot/3,
    delete_snapshot/2,
    list_snapshots/2
]).

%% 分支 API（非 behaviour 回调）
-export([
    branch/3,
    load_branch/3,
    list_branches/2,
    get_lineage/2,
    diff/3
]).

%% 时间旅行 API（非 behaviour 回调）
-export([
    go_back/2,
    go_forward/2,
    goto/2,
    list_history/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type store_ref() :: {beamai_memory:memory(), thread_config()}.
-type thread_config() :: #{thread_id := binary(), _ => _}.

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

%% @doc 保存流程快照到 beamai_memory
%%
%% Process snapshot 直接作为 memory snapshot 的 values 存储。
%% 从 SaveOpts 中提取元数据用于 memory config。
-spec save_snapshot(store_ref(), beamai_process_state:snapshot(),
                    beamai_process_store_behaviour:save_opts()) ->
    {ok, binary()} | {error, term()}.
save_snapshot({Memory, ThreadConfig}, ProcessSnapshot, _SaveOpts) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    Config = #{thread_id => ThreadId},

    %% Process snapshot 直接作为 values 存储
    State = ProcessSnapshot,

    case beamai_memory:save_snapshot(Memory, Config, State) of
        {ok, SnapshotId} ->
            {ok, SnapshotId};
        {error, _} = Error ->
            Error
    end.

%% @doc 从 beamai_memory 加载流程快照
%%
%% 支持指定 SnapshotId 或 latest 加载最新快照。
%% 返回的 values 即为 process snapshot map。
-spec load_snapshot(store_ref(), binary() | latest,
                    beamai_process_store_behaviour:load_opts()) ->
    {ok, beamai_process_state:snapshot()} | {error, not_found | term()}.
load_snapshot({Memory, ThreadConfig}, latest, _LoadOpts) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    Config = #{thread_id => ThreadId},
    case beamai_memory:load_snapshot(Memory, Config) of
        {ok, Values} ->
            {ok, Values};
        {error, _} = Error ->
            Error
    end;
load_snapshot({Memory, ThreadConfig}, SnapshotId, _LoadOpts) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    Config = #{thread_id => ThreadId, snapshot_id => SnapshotId},
    SnapshotManager = beamai_memory:get_snapshot_manager(Memory),
    case beamai_snapshot:load(SnapshotManager, SnapshotId, Config) of
        {ok, {Snapshot, _Meta, _ParentCfg}} ->
            {ok, beamai_memory:snapshot_to_state(Snapshot)};
        {error, _} = Error ->
            Error
    end.

%% @doc 删除流程快照
-spec delete_snapshot(store_ref(), binary()) -> ok | {error, term()}.
delete_snapshot({Memory, ThreadConfig}, SnapshotId) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    Config = #{thread_id => ThreadId, snapshot_id => SnapshotId},
    beamai_memory:delete_snapshot(Memory, Config).

%% @doc 列出流程快照
%%
%% 从 beamai_memory 获取快照列表，转换为 snapshot_info() 格式。
-spec list_snapshots(store_ref(), beamai_process_store_behaviour:list_opts()) ->
    {ok, [beamai_process_store_behaviour:snapshot_info()]} | {error, term()}.
list_snapshots({Memory, ThreadConfig}, ListOpts) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    Config = #{thread_id => ThreadId},

    %% 合并分页选项
    Config2 = case maps:get(limit, ListOpts, undefined) of
        undefined -> Config;
        Limit -> Config#{limit => Limit}
    end,
    Config3 = case maps:get(offset, ListOpts, undefined) of
        undefined -> Config2;
        Offset -> Config2#{offset => Offset}
    end,

    case beamai_memory:list_snapshots(Memory, Config3) of
        {ok, Summaries} ->
            Infos = [summary_to_snapshot_info(S) || S <- Summaries],
            {ok, Infos};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 分支 API
%%====================================================================

%% @doc 从当前快照创建分支
%%
%% 调用 beamai_memory 的分支功能，在指定 thread 的最新快照上创建分支。
-spec branch(store_ref(), binary(), map()) ->
    {ok, #{branch_thread_id := binary(), snapshot_id := binary()}} | {error, term()}.
branch({Memory, ThreadConfig}, BranchName, BranchOpts) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    Config = #{thread_id => ThreadId},

    %% 生成分支 thread_id
    BranchThreadId = maps:get(thread_id, BranchOpts,
                              generate_branch_thread_id(ThreadId, BranchName)),

    case beamai_memory:snapshot_fork(Memory, Config, BranchName,
                                     #{thread_id => BranchThreadId}) of
        {ok, BranchId} ->
            {ok, #{branch_thread_id => BranchThreadId, snapshot_id => BranchId}};
        {error, _} = Error ->
            Error
    end.

%% @doc 从分支加载最新快照
%%
%% 加载分支 thread 的最新 process snapshot。
-spec load_branch(store_ref(), binary(), map()) ->
    {ok, beamai_process_state:snapshot()} | {error, term()}.
load_branch({Memory, _ThreadConfig}, BranchThreadId, _Opts) ->
    Config = #{thread_id => BranchThreadId},
    case beamai_memory:load_snapshot(Memory, Config) of
        {ok, Values} ->
            {ok, Values};
        {error, _} = Error ->
            Error
    end.

%% @doc 列出指定快照的所有分支
-spec list_branches(store_ref(), map()) ->
    {ok, [map()]} | {error, term()}.
list_branches({Memory, _ThreadConfig}, _Opts) ->
    beamai_memory:snapshot_branches(Memory).

%% @doc 获取执行谱系（从当前快照回溯到根）
-spec get_lineage(store_ref(), map()) ->
    {ok, [map()]} | {error, term()}.
get_lineage({Memory, ThreadConfig}, Opts) ->
    ThreadId = maps:get(thread_id, Opts, maps:get(thread_id, ThreadConfig)),
    Config = #{thread_id => ThreadId},
    beamai_memory:snapshot_lineage(Memory, Config).

%% @doc 比较两个快照（不同 thread）的差异
-spec diff(store_ref(), map(), map()) ->
    {ok, map()} | {error, term()}.
diff({Memory, _ThreadConfig}, Config1, Config2) ->
    SnapshotManager = beamai_memory:get_snapshot_manager(Memory),
    beamai_snapshot:diff(SnapshotManager, Config1, Config2).

%%====================================================================
%% 时间旅行 API
%%====================================================================

%% @doc 回退 N 步
%%
%% 在当前 thread 中回退 N 个快照，返回过去的 process snapshot。
-spec go_back(store_ref(), pos_integer()) ->
    {ok, beamai_process_state:snapshot()} | {error, term()}.
go_back({Memory, ThreadConfig}, Steps) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    Config = #{thread_id => ThreadId},
    beamai_memory:snapshot_go_back(Memory, Config, Steps).

%% @doc 前进 N 步
%%
%% 在当前 thread 中前进 N 个快照（从当前位置向未来方向）。
-spec go_forward(store_ref(), pos_integer()) ->
    {ok, beamai_process_state:snapshot()} | {error, term()}.
go_forward({Memory, ThreadConfig}, Steps) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    Config = #{thread_id => ThreadId},
    beamai_memory:snapshot_go_forward(Memory, Config, Steps).

%% @doc 跳转到指定快照
%%
%% 跳转到指定 SnapshotId，返回该快照的 process snapshot。
-spec goto(store_ref(), binary()) ->
    {ok, beamai_process_state:snapshot()} | {error, term()}.
goto({Memory, ThreadConfig}, SnapshotId) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    Config = #{thread_id => ThreadId},
    beamai_memory:snapshot_goto(Memory, Config, SnapshotId).

%% @doc 列出执行历史
%%
%% 返回当前 thread 的快照历史列表。
-spec list_history(store_ref()) ->
    {ok, [map()]} | {error, term()}.
list_history({Memory, ThreadConfig}) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    Config = #{thread_id => ThreadId},
    beamai_memory:snapshot_history(Memory, Config).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 将 memory 快照摘要转换为 process store 的 snapshot_info
-spec summary_to_snapshot_info(map()) -> beamai_process_store_behaviour:snapshot_info().
summary_to_snapshot_info(Summary) ->
    SnapshotId = maps:get(snapshot_id, Summary, undefined),
    Timestamp = maps:get(timestamp, Summary, 0),

    #{
        id => SnapshotId,
        timestamp => Timestamp,
        trigger => manual,
        process_name => undefined,
        step_id => undefined
    }.

%% @private 生成分支 thread_id
-spec generate_branch_thread_id(binary(), binary()) -> binary().
generate_branch_thread_id(ParentThreadId, BranchName) ->
    Ts = integer_to_binary(erlang:system_time(microsecond)),
    <<ParentThreadId/binary, "_branch_", BranchName/binary, "_", Ts/binary>>.
