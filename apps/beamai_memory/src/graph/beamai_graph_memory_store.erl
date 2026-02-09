%%%-------------------------------------------------------------------
%%% @doc Graph Store 实现 - 基于 beamai_graph_snapshot 后端
%%%
%%% 实现 beamai_graph_store_behaviour，将图快照存储到
%%% beamai_graph_snapshot Manager 中。
%%%
%%% == Store Ref 格式 ==
%%%
%%% store_ref() = {beamai_graph_snapshot:manager(), #{run_id => binary()}}
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 Graph Snapshot Manager
%%% Mgr = beamai_graph_snapshot:new(StateStore, #{}),
%%%
%%% %% 配置 store ref
%%% StoreRef = {Mgr, #{run_id => <<"graph-run-1">>}},
%%%
%%% %% 在 graph_runner 中使用
%%% Options = #{
%%%     store => {beamai_graph_memory_store, StoreRef},
%%%     snapshot_strategy => every_superstep,
%%%     graph_name => my_graph
%%% },
%%% graph_runner:run(Graph, State, Options).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_memory_store).

-behaviour(beamai_graph_store_behaviour).

%% Behaviour 回调
-export([
    save_snapshot/3,
    load_snapshot/3,
    delete_snapshot/2,
    list_snapshots/2
]).

%% 运行管理 API
-export([
    load_run_latest/2,
    delete_run/2,
    list_runs/2
]).

%% 分支 API
-export([
    branch/3,
    load_branch/2,
    list_branches/2
]).

%% 时间旅行 API
-export([
    go_back/3,
    go_forward/3,
    goto_superstep/3,
    list_history/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type store_ref() :: {beamai_graph_snapshot:manager(), run_config()}.
-type run_config() :: #{run_id => binary(), _ => _}.

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

%% @doc 保存图快照到 beamai_graph_snapshot
%%
%% 快照数据直接作为 snapshot 的 values 存储。
-spec save_snapshot(store_ref(), beamai_graph_store_behaviour:graph_snapshot(),
                    beamai_graph_store_behaviour:save_opts()) ->
    {ok, binary()} | {error, term()}.
save_snapshot({Mgr, RunConfig}, Snapshot, SaveOpts) ->
    %% 从快照或 config 中获取 run_id
    RunId = maps:get(run_id, Snapshot, maps:get(run_id, RunConfig, generate_run_id())),

    %% 构建存储的 state（包含完整快照信息）
    State = Snapshot#{
        graph_name => maps:get(graph_name, SaveOpts, undefined),
        trigger => maps:get(trigger, SaveOpts, superstep_completed),
        saved_at => erlang:system_time(millisecond)
    },

    case beamai_graph_snapshot:save_from_state(Mgr, RunId, State, #{}) of
        {ok, SnapshotRecord, _NewMgr} ->
            {ok, SnapshotRecord};
        {error, _} = Error ->
            Error
    end.

%% @doc 从 beamai_graph_snapshot 加载图快照
%%
%% 支持指定 SnapshotId 或 latest 加载最新快照。
-spec load_snapshot(store_ref(), binary() | latest,
                    beamai_graph_store_behaviour:load_opts()) ->
    {ok, beamai_graph_store_behaviour:graph_snapshot()} | {error, not_found | term()}.
load_snapshot({Mgr, RunConfig}, latest, LoadOpts) ->
    %% 获取 run_id
    RunId = maps:get(run_id, LoadOpts, maps:get(run_id, RunConfig, undefined)),
    case RunId of
        undefined ->
            {error, run_id_required};
        _ ->
            case beamai_graph_snapshot:get_latest(Mgr, RunId) of
                {ok, Snapshot} ->
                    {ok, Snapshot};
                {error, _} = Error ->
                    Error
            end
    end;
load_snapshot({Mgr, _RunConfig}, SnapshotId, _LoadOpts) ->
    case beamai_graph_snapshot:load(Mgr, SnapshotId) of
        {ok, Snapshot} ->
            {ok, Snapshot};
        {error, _} = Error ->
            Error
    end.

%% @doc 删除图快照
-spec delete_snapshot(store_ref(), binary()) -> ok | {error, term()}.
delete_snapshot({Mgr, _RunConfig}, SnapshotId) ->
    case beamai_graph_snapshot:delete(Mgr, SnapshotId) of
        {ok, _NewMgr} -> ok;
        {error, _} = Error -> Error
    end.

%% @doc 列出图快照
%%
%% 从 beamai_graph_snapshot 获取快照列表，转换为 snapshot_info() 格式。
-spec list_snapshots(store_ref(), beamai_graph_store_behaviour:list_opts()) ->
    {ok, [beamai_graph_store_behaviour:snapshot_info()]} | {error, term()}.
list_snapshots({Mgr, RunConfig}, ListOpts) ->
    RunId = maps:get(run_id, ListOpts, maps:get(run_id, RunConfig, undefined)),

    Opts = case maps:get(limit, ListOpts, undefined) of
        undefined -> #{};
        Limit -> #{limit => Limit}
    end,
    Opts2 = case maps:get(offset, ListOpts, undefined) of
        undefined -> Opts;
        Offset -> Opts#{offset => Offset}
    end,

    case RunId of
        undefined ->
            %% 没有指定 run_id，列出所有
            case beamai_graph_snapshot:list_all(Mgr, Opts2) of
                {ok, Snapshots} ->
                    Infos = [snapshot_to_info(S) || S <- Snapshots],
                    {ok, Infos};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            case beamai_graph_snapshot:get_history(Mgr, RunId, Opts2) of
                {ok, Snapshots} ->
                    Infos = [snapshot_to_info(S) || S <- Snapshots],
                    {ok, Infos};
                {error, _} = Error ->
                    Error
            end
    end.

%%====================================================================
%% 运行管理 API
%%====================================================================

%% @doc 按 run_id 加载最新快照
-spec load_run_latest(store_ref(), binary()) ->
    {ok, beamai_graph_store_behaviour:graph_snapshot()} | {error, not_found | term()}.
load_run_latest({Mgr, _RunConfig}, RunId) ->
    beamai_graph_snapshot:get_latest(Mgr, RunId).

%% @doc 删除运行的所有快照
-spec delete_run(store_ref(), binary()) -> ok | {error, term()}.
delete_run({Mgr, _RunConfig}, RunId) ->
    case beamai_graph_snapshot:delete_thread(Mgr, RunId) of
        {ok, _NewMgr} -> ok;
        {error, _} = Error -> Error
    end.

%% @doc 列出所有运行
-spec list_runs(store_ref(), beamai_graph_store_behaviour:list_opts()) ->
    {ok, [#{run_id := binary(), snapshot_count := non_neg_integer(), latest_superstep := non_neg_integer()}]} |
    {error, term()}.
list_runs({Mgr, _RunConfig}, _Opts) ->
    case beamai_graph_snapshot:list_threads(Mgr) of
        {ok, Threads} ->
            Runs = [thread_to_run_info(Mgr, T) || T <- Threads],
            {ok, Runs};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 分支 API
%%====================================================================

%% @doc 从当前快照创建分支
-spec branch(store_ref(), binary(), binary()) ->
    {ok, #{branch_id := binary(), snapshot_id := binary()}} | {error, term()}.
branch({Mgr, _RunConfig}, SnapshotId, BranchName) ->
    BranchId = generate_branch_id(BranchName),
    case beamai_graph_snapshot:fork_from(Mgr, SnapshotId, BranchName, BranchId) of
        {ok, ForkedSnapshot, _NewMgr} ->
            ForkedId = beamai_graph_snapshot:get_id(ForkedSnapshot),
            {ok, #{branch_id => BranchId, snapshot_id => ForkedId}};
        {error, _} = Error ->
            Error
    end.

%% @doc 加载分支的最新快照
-spec load_branch(store_ref(), binary()) ->
    {ok, beamai_graph_store_behaviour:graph_snapshot()} | {error, term()}.
load_branch({Mgr, _RunConfig}, BranchId) ->
    beamai_graph_snapshot:get_latest(Mgr, BranchId).

%% @doc 列出运行的所有分支
-spec list_branches(store_ref(), binary()) ->
    {ok, [#{branch_id := binary(), name := binary(), snapshot_count := non_neg_integer()}]} |
    {error, term()}.
list_branches({Mgr, _RunConfig}, RunId) ->
    %% 获取该 run 下的所有分支
    Branches = beamai_graph_snapshot:list_branches(Mgr),
    %% 过滤属于该 run 的分支
    FilteredBranches = [B || B <- Branches, is_branch_of_run(B, RunId)],
    {ok, [branch_to_info(B) || B <- FilteredBranches]}.

%%====================================================================
%% 时间旅行 API
%%====================================================================

%% @doc 回退 N 个超步
-spec go_back(store_ref(), binary(), pos_integer()) ->
    {ok, beamai_graph_store_behaviour:graph_snapshot()} | {error, term()}.
go_back({Mgr, _RunConfig}, RunId, Steps) ->
    case beamai_graph_snapshot:go_back(Mgr, RunId, Steps) of
        {ok, Snapshot, _NewMgr} -> {ok, Snapshot};
        {error, _} = Error -> Error
    end.

%% @doc 前进 N 个超步
-spec go_forward(store_ref(), binary(), pos_integer()) ->
    {ok, beamai_graph_store_behaviour:graph_snapshot()} | {error, term()}.
go_forward({Mgr, _RunConfig}, RunId, Steps) ->
    case beamai_graph_snapshot:go_forward(Mgr, RunId, Steps) of
        {ok, Snapshot, _NewMgr} -> {ok, Snapshot};
        {error, _} = Error -> Error
    end.

%% @doc 跳转到指定超步
-spec goto_superstep(store_ref(), binary(), non_neg_integer()) ->
    {ok, beamai_graph_store_behaviour:graph_snapshot()} | {error, term()}.
goto_superstep({Mgr, _RunConfig}, RunId, Superstep) ->
    %% 需要找到该超步对应的快照 id
    case beamai_graph_snapshot:get_history(Mgr, RunId, #{}) of
        {ok, Snapshots} ->
            case find_by_superstep(Snapshots, Superstep) of
                {ok, Snapshot} ->
                    {ok, Snapshot};
                not_found ->
                    {error, {superstep_not_found, Superstep}}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 列出运行的执行历史
-spec list_history(store_ref(), binary()) ->
    {ok, [beamai_graph_store_behaviour:snapshot_info()]} | {error, term()}.
list_history({Mgr, _RunConfig}, RunId) ->
    case beamai_graph_snapshot:get_history(Mgr, RunId, #{}) of
        {ok, Snapshots} ->
            Infos = [snapshot_to_info(S) || S <- Snapshots],
            {ok, Infos};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 将快照转换为 snapshot_info
-spec snapshot_to_info(map()) -> beamai_graph_store_behaviour:snapshot_info().
snapshot_to_info(Snapshot) ->
    #{
        id => beamai_graph_snapshot:get_id(Snapshot),
        run_id => maps:get(run_id, Snapshot, <<>>),
        superstep => maps:get(superstep, Snapshot, 0),
        iteration => maps:get(iteration, Snapshot, 0),
        timestamp => maps:get(saved_at, Snapshot, 0),
        trigger => maps:get(trigger, Snapshot, superstep_completed),
        graph_name => maps:get(graph_name, Snapshot, undefined),
        type => maps:get(type, Snapshot, step),
        active_count => length(maps:get(active_vertices, Snapshot, [])),
        completed_count => length(maps:get(completed_vertices, Snapshot, []))
    }.

%% @private 将 thread 信息转换为 run info
-spec thread_to_run_info(beamai_graph_snapshot:manager(), map()) ->
    #{run_id := binary(), snapshot_count := non_neg_integer(), latest_superstep := non_neg_integer()}.
thread_to_run_info(Mgr, #{thread_id := ThreadId} = _Thread) ->
    case beamai_graph_snapshot:get_history(Mgr, ThreadId, #{}) of
        {ok, Snapshots} ->
            LatestSuperstep = case Snapshots of
                [] -> 0;
                [Latest | _] -> maps:get(superstep, Latest, 0)
            end,
            #{
                run_id => ThreadId,
                snapshot_count => length(Snapshots),
                latest_superstep => LatestSuperstep
            };
        _ ->
            #{run_id => ThreadId, snapshot_count => 0, latest_superstep => 0}
    end.

%% @private 将分支信息转换为 branch info
-spec branch_to_info(map()) -> #{branch_id := binary(), name := binary(), snapshot_count := non_neg_integer()}.
branch_to_info(Branch) ->
    #{
        branch_id => maps:get(thread_id, Branch, <<>>),
        name => maps:get(name, Branch, <<>>),
        snapshot_count => maps:get(count, Branch, 0)
    }.

%% @private 检查分支是否属于指定 run
-spec is_branch_of_run(map(), binary()) -> boolean().
is_branch_of_run(#{parent_run_id := ParentRunId}, RunId) ->
    ParentRunId =:= RunId;
is_branch_of_run(_, _) ->
    false.

%% @private 按超步查找快照
-spec find_by_superstep([map()], non_neg_integer()) -> {ok, map()} | not_found.
find_by_superstep([], _Superstep) ->
    not_found;
find_by_superstep([Snapshot | Rest], Superstep) ->
    case maps:get(superstep, Snapshot, -1) of
        Superstep -> {ok, Snapshot};
        _ -> find_by_superstep(Rest, Superstep)
    end.

%% @private 生成 run_id
-spec generate_run_id() -> binary().
generate_run_id() ->
    Ts = integer_to_binary(erlang:system_time(microsecond)),
    Rand = integer_to_binary(rand:uniform(1000000)),
    <<"run_", Ts/binary, "_", Rand/binary>>.

%% @private 生成 branch_id
-spec generate_branch_id(binary()) -> binary().
generate_branch_id(BranchName) ->
    Ts = integer_to_binary(erlang:system_time(microsecond)),
    <<"branch_", BranchName/binary, "_", Ts/binary>>.
