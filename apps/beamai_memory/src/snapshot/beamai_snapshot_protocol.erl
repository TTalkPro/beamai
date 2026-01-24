%%%-------------------------------------------------------------------
%%% @doc Snapshot 行为协议
%%%
%%% 定义 Snapshot 必须实现的行为接口。
%%% 所有 Snapshot 实现都必须遵守此协议。
%%%
%%% == 设计原则 ==
%%%
%%% - Snapshot 使用 Store 进行持久化
%%% - 支持分支管理
%%% - 支持时间旅行（回退/前进/跳转）
%%% - 线程隔离（不同 thread_id 独立管理）
%%%
%%% == 命名空间设计 ==
%%%
%%% - [<<"snapshots">>, ThreadId, SnapshotId] - 检查点数据
%%% - [<<"snapshots">>, <<"_index">>, ThreadId] - 线程索引
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_snapshot_protocol).

-include("beamai_snapshot.hrl").

%% Snapshot 行为回调
-callback save(snapshot(), config()) ->
    {ok, snapshot_id()} | {error, term()}.

-callback load(snapshot_id() | latest, config()) ->
    {ok, snapshot_tuple()} | {error, not_found | term()}.

-callback list(config(), opts()) ->
    {ok, [snapshot_tuple()]} | {error, term()}.

-callback delete(snapshot_id(), config()) ->
    ok | {error, term()}.

-callback count(config()) ->
    non_neg_integer().

%% 可选的时间旅行回调
-callback go_back(config(), pos_integer()) ->
    {ok, snapshot()} | {error, term()}.

-callback go_forward(config(), pos_integer()) ->
    {ok, snapshot()} | {error, term()}.

-callback goto(snapshot_id(), config()) ->
    {ok, snapshot()} | {error, term()}.

%% 可选的分支管理回调
-callback branch(config(), branch_opts()) ->
    {ok, snapshot_id()} | {error, term()}.

-callback switch_branch(branch_id(), config()) ->
    {ok, snapshot()} | {error, term()}.

-callback list_branches(config()) ->
    {ok, [branch_info()]} | {error, term()}.

-callback get_lineage(snapshot_id(), config()) ->
    {ok, [snapshot_tuple()]} | {error, term()}.

%%====================================================================
%% 类型定义
%%====================================================================

%% 从 beamai_snapshot.hrl 导入
-type snapshot() :: #snapshot{}.
-type snapshot_metadata() :: #snapshot_metadata{}.
-type snapshot_tuple() :: {snapshot(), snapshot_metadata(), config() | undefined}.
-type snapshot_id() :: binary().
-type thread_id() :: binary().
-type config() :: map().
-type opts() :: map().

%% 分支相关类型
-type branch_id() :: binary().
-type branch_opts() :: #{
    branch_name => binary(),
    thread_id => thread_id()
}.
-type branch_info() :: #{
    branch_id := branch_id(),
    head_snapshot_id => snapshot_id() | undefined,
    snapshot_count => non_neg_integer(),
    created_at => integer()
}.

%% 导出类型
-export_type([
    snapshot/0,
    snapshot_metadata/0,
    snapshot_tuple/0,
    snapshot_id/0,
    thread_id/0,
    config/0,
    opts/0,
    branch_id/0,
    branch_opts/0,
    branch_info/0
]).
