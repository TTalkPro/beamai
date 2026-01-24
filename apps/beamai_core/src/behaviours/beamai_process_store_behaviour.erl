%%%-------------------------------------------------------------------
%%% @doc 流程存储行为定义（Process Store Behaviour）
%%%
%%% 定义流程快照持久化的统一接口。
%%% 实现此行为的模块可用于 runtime 自动 checkpoint 功能。
%%%
%%% == 设计原则 ==
%%%
%%% - 存储引用（store_ref）为不透明类型，由实现模块自行定义
%%% - 快照格式遵循 beamai_process_state:snapshot() 类型
%%% - 所有操作均为同步调用，异步化由调用方负责
%%% - 错误不会阻塞流程执行，仅用于日志记录
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 实现模块
%%% -module(my_process_store).
%%% -behaviour(beamai_process_store_behaviour).
%%%
%%% save_snapshot(Ref, Snapshot, Opts) ->
%%%     %% 将快照持久化到数据库
%%%     ...
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_store_behaviour).

%%====================================================================
%% 类型定义
%%====================================================================

%% 存储引用 - 不透明类型，由实现模块定义
%% 可以是 pid、注册名、连接句柄等
-type store_ref() :: term().

%% 快照唯一标识
-type snapshot_id() :: binary().

%% 流程快照数据，遵循 beamai_process_state:snapshot() 格式
-type process_snapshot() :: beamai_process_state:snapshot().

%% 保存选项
-type save_opts() :: #{
    %% 流程名称（用于分类存储）
    process_name => atom(),
    %% 触发保存的原因
    trigger => trigger_type(),
    %% 触发保存的步骤 ID（如适用）
    step_id => atom() | undefined,
    %% 附加元数据
    metadata => map()
}.

%% 加载选项
-type load_opts() :: #{
    %% 按流程名称过滤
    process_name => atom()
}.

%% 列表查询选项
-type list_opts() :: #{
    %% 按流程名称过滤
    process_name => atom(),
    %% 分页：每页数量
    limit => pos_integer(),
    %% 分页：偏移量
    offset => non_neg_integer()
}.

%% 快照摘要信息（用于列表展示）
-type snapshot_info() :: #{
    %% 快照唯一标识
    id := snapshot_id(),
    %% 创建时间戳（毫秒）
    timestamp := integer(),
    %% 触发类型
    trigger := trigger_type(),
    %% 流程名称
    process_name => atom(),
    %% 步骤 ID
    step_id => atom() | undefined
}.

%% 触发类型 - 标识快照产生的原因
-type trigger_type() :: step_completed | paused | completed | error | manual.

-export_type([
    store_ref/0,
    snapshot_id/0,
    process_snapshot/0,
    save_opts/0,
    load_opts/0,
    list_opts/0,
    snapshot_info/0,
    trigger_type/0
]).

%%====================================================================
%% 回调定义
%%====================================================================

%% @doc 保存流程快照
%%
%% 将快照数据持久化到存储后端。
%% 实现模块应生成唯一的 snapshot_id 并返回。
%%
%% @param Ref 存储引用
%% @param Snapshot 快照数据
%% @param Opts 保存选项（包含 process_name、trigger、step_id、metadata）
%% @returns {ok, 快照ID} | {error, 原因}
-callback save_snapshot(Ref :: store_ref(), Snapshot :: process_snapshot(), Opts :: save_opts()) ->
    {ok, snapshot_id()} | {error, term()}.

%% @doc 加载流程快照
%%
%% 从存储后端加载指定或最新的快照。
%% SnapshotId 为 latest 时加载最新快照。
%%
%% @param Ref 存储引用
%% @param SnapshotId 快照 ID 或 latest 原子
%% @param Opts 加载选项（包含 process_name 过滤）
%% @returns {ok, 快照数据} | {error, not_found | 原因}
-callback load_snapshot(Ref :: store_ref(), SnapshotId :: snapshot_id() | latest, Opts :: load_opts()) ->
    {ok, process_snapshot()} | {error, not_found | term()}.

%% @doc 删除流程快照
%%
%% 从存储后端删除指定的快照。
%%
%% @param Ref 存储引用
%% @param SnapshotId 要删除的快照 ID
%% @returns ok | {error, 原因}
-callback delete_snapshot(Ref :: store_ref(), SnapshotId :: snapshot_id()) ->
    ok | {error, term()}.

%% @doc 列出流程快照
%%
%% 获取快照摘要列表，支持按流程名称过滤和分页。
%% 结果按时间戳降序排列（最新在前）。
%%
%% @param Ref 存储引用
%% @param Opts 列表选项（包含 process_name、limit、offset）
%% @returns {ok, 快照摘要列表} | {error, 原因}
-callback list_snapshots(Ref :: store_ref(), Opts :: list_opts()) ->
    {ok, [snapshot_info()]} | {error, term()}.
