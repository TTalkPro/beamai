%%%-------------------------------------------------------------------
%%% @doc 图存储行为定义（Graph Store Behaviour）
%%%
%%% 定义图快照持久化的统一接口。
%%% 实现此行为的模块可用于 graph_runner 自动快照功能。
%%%
%%% == 设计原则 ==
%%%
%%% - 存储引用（store_ref）为不透明类型，由实现模块自行定义
%%% - Snapshot 格式遵循 graph_runner:snapshot_data() 类型
%%% - 所有操作均为同步调用，异步化由调用方负责
%%% - 错误不会阻塞图执行，仅用于日志记录
%%%
%%% == 与 Process Store 的对比 ==
%%%
%%% | 维度 | Process | Graph |
%%% |------|---------|-------|
%%% | 触发时机 | Step 完成 | Superstep 完成 |
%%% | 状态内容 | steps_state, event_queue | vertices, global_state, superstep |
%%% | 核心标识 | process_name + step_id | graph_name + run_id + superstep |
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 实现模块
%%% -module(my_graph_store).
%%% -behaviour(beamai_graph_store_behaviour).
%%%
%%% save_snapshot(Ref, Snapshot, Opts) ->
%%%     %% 将快照持久化到数据库
%%%     ...
%%%
%%% %% 在 graph_runner 中使用
%%% Options = #{
%%%     store => {my_graph_store, StoreRef},
%%%     snapshot_strategy => every_superstep
%%% },
%%% graph_runner:run(Graph, State, Options).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_store_behaviour).

%%====================================================================
%% 类型定义
%%====================================================================

%% 存储引用 - 不透明类型，由实现模块定义
%% 可以是 pid、注册名、连接句柄等
-type store_ref() :: term().

%% Snapshot 唯一标识
-type snapshot_id() :: binary().

%% 运行标识（一次完整执行的 ID）
-type run_id() :: binary().

%% 图快照数据，遵循 graph_runner:snapshot_data() 格式
-type graph_snapshot() :: #{
    type := snapshot_type(),
    pregel_snapshot := graph_executor:snapshot_data(),
    global_state := graph_state:state(),
    iteration := non_neg_integer(),
    run_id := run_id(),
    active_vertices := [atom()],
    completed_vertices := [atom()],
    superstep := non_neg_integer()
}.

%% Snapshot 类型 - 标识快照产生的原因
-type snapshot_type() :: initial | step | error | interrupt | final.

%% 触发类型 - 标识快照产生的时机
-type trigger_type() :: superstep_completed    %% 超步完成
                      | interrupted            %% 中断（需要用户输入）
                      | error_occurred         %% 发生错误
                      | completed              %% 图执行完成
                      | manual.                %% 手动触发

%% 保存选项
-type save_opts() :: #{
    %% 图名称（用于分类存储）
    graph_name => atom() | binary(),
    %% 触发保存的原因
    trigger => trigger_type(),
    %% 当前超步
    superstep => non_neg_integer(),
    %% 附加元数据
    metadata => map()
}.

%% 加载选项
-type load_opts() :: #{
    %% 按图名称过滤
    graph_name => atom() | binary(),
    %% 按超步过滤
    superstep => non_neg_integer()
}.

%% 列表查询选项
-type list_opts() :: #{
    %% 按图名称过滤
    graph_name => atom() | binary(),
    %% 按 run_id 过滤
    run_id => run_id(),
    %% 分页：每页数量
    limit => pos_integer(),
    %% 分页：偏移量
    offset => non_neg_integer()
}.

%% Snapshot 摘要信息（用于列表展示）
-type snapshot_info() :: #{
    %% Snapshot 唯一标识
    id := snapshot_id(),
    %% 运行 ID
    run_id := run_id(),
    %% 超步编号
    superstep := non_neg_integer(),
    %% 迭代次数
    iteration := non_neg_integer(),
    %% 创建时间戳（毫秒）
    timestamp := integer(),
    %% 触发类型
    trigger := trigger_type(),
    %% 图名称
    graph_name => atom() | binary(),
    %% Snapshot 类型
    type := snapshot_type(),
    %% 活跃顶点数量
    active_count := non_neg_integer(),
    %% 已完成顶点数量
    completed_count := non_neg_integer()
}.

-export_type([
    store_ref/0,
    snapshot_id/0,
    run_id/0,
    graph_snapshot/0,
    snapshot_type/0,
    trigger_type/0,
    save_opts/0,
    load_opts/0,
    list_opts/0,
    snapshot_info/0
]).

%%====================================================================
%% 核心回调定义
%%====================================================================

%% @doc 保存图快照
%%
%% 将快照数据持久化到存储后端。
%% 实现模块应生成唯一的 snapshot_id 并返回。
%%
%% @param Ref 存储引用
%% @param Snapshot 快照数据
%% @param Opts 保存选项（包含 graph_name、trigger、superstep、metadata）
%% @returns {ok, 快照ID} | {error, 原因}
-callback save_snapshot(Ref :: store_ref(), Snapshot :: graph_snapshot(), Opts :: save_opts()) ->
    {ok, snapshot_id()} | {error, term()}.

%% @doc 加载图快照
%%
%% 从存储后端加载指定或最新的快照。
%% SnapshotId 为 latest 时加载最新快照。
%%
%% @param Ref 存储引用
%% @param SnapshotId 快照 ID 或 latest 原子
%% @param Opts 加载选项（包含 graph_name、superstep 过滤）
%% @returns {ok, 快照数据} | {error, not_found | 原因}
-callback load_snapshot(Ref :: store_ref(), SnapshotId :: snapshot_id() | latest, Opts :: load_opts()) ->
    {ok, graph_snapshot()} | {error, not_found | term()}.

%% @doc 删除图快照
%%
%% 从存储后端删除指定的快照。
%%
%% @param Ref 存储引用
%% @param SnapshotId 要删除的快照 ID
%% @returns ok | {error, 原因}
-callback delete_snapshot(Ref :: store_ref(), SnapshotId :: snapshot_id()) ->
    ok | {error, term()}.

%% @doc 列出图快照
%%
%% 获取快照摘要列表，支持按图名称、run_id 过滤和分页。
%% 结果按时间戳降序排列（最新在前）。
%%
%% @param Ref 存储引用
%% @param Opts 列表选项（包含 graph_name、run_id、limit、offset）
%% @returns {ok, 快照摘要列表} | {error, 原因}
-callback list_snapshots(Ref :: store_ref(), Opts :: list_opts()) ->
    {ok, [snapshot_info()]} | {error, term()}.

%%====================================================================
%% 可选回调 - 运行管理
%%====================================================================

%% @doc 按 run_id 加载最新快照
%%
%% 加载指定运行的最新快照，用于恢复执行。
-callback load_run_latest(Ref :: store_ref(), RunId :: run_id()) ->
    {ok, graph_snapshot()} | {error, not_found | term()}.

%% @doc 删除运行的所有快照
%%
%% 清理指定运行的全部快照数据。
-callback delete_run(Ref :: store_ref(), RunId :: run_id()) ->
    ok | {error, term()}.

%% @doc 列出所有运行
%%
%% 获取存储中的所有运行 ID 列表。
-callback list_runs(Ref :: store_ref(), Opts :: list_opts()) ->
    {ok, [#{run_id := run_id(), snapshot_count := non_neg_integer(), latest_superstep := non_neg_integer()}]} | {error, term()}.

%%====================================================================
%% 可选回调 - 分支管理
%%====================================================================

%% @doc 从当前快照创建分支
-callback branch(Ref :: store_ref(), SnapshotId :: snapshot_id(), BranchName :: binary()) ->
    {ok, #{branch_id := binary(), snapshot_id := snapshot_id()}} | {error, term()}.

%% @doc 加载分支的最新快照
-callback load_branch(Ref :: store_ref(), BranchId :: binary()) ->
    {ok, graph_snapshot()} | {error, term()}.

%% @doc 列出所有分支
-callback list_branches(Ref :: store_ref(), RunId :: run_id()) ->
    {ok, [#{branch_id := binary(), name := binary(), snapshot_count := non_neg_integer()}]} | {error, term()}.

%%====================================================================
%% 可选回调 - 时间旅行
%%====================================================================

%% @doc 回退 N 个超步
-callback go_back(Ref :: store_ref(), RunId :: run_id(), Steps :: pos_integer()) ->
    {ok, graph_snapshot()} | {error, term()}.

%% @doc 前进 N 个超步
-callback go_forward(Ref :: store_ref(), RunId :: run_id(), Steps :: pos_integer()) ->
    {ok, graph_snapshot()} | {error, term()}.

%% @doc 跳转到指定超步
-callback goto_superstep(Ref :: store_ref(), RunId :: run_id(), Superstep :: non_neg_integer()) ->
    {ok, graph_snapshot()} | {error, term()}.

%% @doc 列出执行历史
-callback list_history(Ref :: store_ref(), RunId :: run_id()) ->
    {ok, [snapshot_info()]} | {error, term()}.

-optional_callbacks([
    %% 运行管理
    load_run_latest/2, delete_run/2, list_runs/2,
    %% 分支管理
    branch/3, load_branch/2, list_branches/2,
    %% 时间旅行
    go_back/3, go_forward/3, goto_superstep/3, list_history/2
]).
