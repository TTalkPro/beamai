%%%-------------------------------------------------------------------
%%% @doc Checkpointer 行为协议
%%%
%%% 定义 Checkpointer 必须实现的行为接口。
%%% 所有 Checkpointer 实现都必须遵守此协议。
%%%
%%% == 设计原则 ==
%%%
%%% - Checkpointer 使用 Store 进行持久化
%%% - 支持分支管理
%%% - 支持时间旅行（回退/前进/跳转）
%%% - 线程隔离（不同 thread_id 独立管理）
%%%
%%% == 命名空间设计 ==
%%%
%%% - [<<"checkpoints">>, ThreadId, CheckpointId] - 检查点数据
%%% - [<<"checkpoints">>, <<"_index">>, ThreadId] - 线程索引
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_checkpoint_protocol).

-include("beamai_checkpointer.hrl").

%% Checkpointer 行为回调
-callback save(checkpoint(), config()) ->
    {ok, checkpoint_id()} | {error, term()}.

-callback load(checkpoint_id() | latest, config()) ->
    {ok, checkpoint_tuple()} | {error, not_found | term()}.

-callback list(config(), opts()) ->
    {ok, [checkpoint_tuple()]} | {error, term()}.

-callback delete(checkpoint_id(), config()) ->
    ok | {error, term()}.

-callback count(config()) ->
    non_neg_integer().

%% 可选的时间旅行回调
-callback go_back(config(), pos_integer()) ->
    {ok, checkpoint()} | {error, term()}.

-callback go_forward(config(), pos_integer()) ->
    {ok, checkpoint()} | {error, term()}.

-callback goto(checkpoint_id(), config()) ->
    {ok, checkpoint()} | {error, term()}.

%% 可选的分支管理回调
-callback branch(config(), branch_opts()) ->
    {ok, checkpoint_id()} | {error, term()}.

-callback switch_branch(branch_id(), config()) ->
    {ok, checkpoint()} | {error, term()}.

-callback list_branches(config()) ->
    {ok, [branch_info()]} | {error, term()}.

-callback get_lineage(checkpoint_id(), config()) ->
    {ok, [checkpoint_tuple()]} | {error, term()}.

%%====================================================================
%% 类型定义
%%====================================================================

%% 从 beamai_checkpointer.hrl 导入
-type checkpoint() :: #checkpoint{}.
-type checkpoint_metadata() :: #checkpoint_metadata{}.
-type checkpoint_tuple() :: {checkpoint(), checkpoint_metadata(), config() | undefined}.
-type checkpoint_id() :: binary().
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
    head_checkpoint_id => checkpoint_id() | undefined,
    checkpoint_count => non_neg_integer(),
    created_at => integer()
}.

%% 导出类型
-export_type([
    checkpoint/0,
    checkpoint_metadata/0,
    checkpoint_tuple/0,
    checkpoint_id/0,
    thread_id/0,
    config/0,
    opts/0,
    branch_id/0,
    branch_opts/0,
    branch_info/0
]).
