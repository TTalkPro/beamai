%%%-------------------------------------------------------------------
%%% @doc Agent Memory 统一 API - 分层架构 v4.0
%%%
%%% 重构后的统一 API，职责清晰：
%%% - 协调 Checkpointer Manager 和 Store Manager
%%% - 提供便捷的高级接口
%%% - 核心逻辑委托给专门的管理器
%%%
%%% == 架构 ==
%%%
%%% ```
%%% beamai_memory (统一 API - 协调层)
%%%   │
%%%   ├── checkpoint_manager (检查点管理器)
%%%   │   ├── beamai_checkpoint_time_travel  (时间旅行扩展)
%%%   │   └── beamai_checkpoint_branch      (分支管理扩展)
%%%   │
%%%   └── store_manager (存储管理器)
%%%       ├── context_store  (内存，快速)
%%%       └── persistent_store (持久化，归档)
%%%           └── beamai_store_archiver (归档器)
%%% '''
%%%
%%% == 双 Store 架构 ==
%%%
%%% - context_store: 存储 Checkpointer 数据和当前对话（快速，内存）
%%% - persistent_store: 存储长期记忆和归档（持久化，可选）
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 Memory
%%% {ok, ContextStore} = beamai_store_ets:start_link(my_context, #{}),
%%% {ok, PersistStore} = beamai_store_sqlite:start_link(my_persist, #{
%%%     db_path => "/data/agent-memory.db"
%%% }),
%%% {ok, Memory} = beamai_memory:new(#{
%%%     context_store => {beamai_store_ets, my_context},
%%%     persistent_store => {beamai_store_sqlite, my_persist}
%%% }),
%%%
%%% %% 使用 Checkpointer API
%%% Config = #{thread_id => <<"thread-1">>},
%%% {ok, CpId} = beamai_memory:save_checkpoint(Memory, Config, #{messages => []}),
%%% {ok, State} = beamai_memory:load_checkpoint(Memory, Config).
%%%
%%% %% 使用时间旅行
%%% {ok, State} = beamai_memory:go_back(Memory, Config, 1).
%%%
%%% %% 使用分支
%%% {ok, BranchId} = beamai_memory:create_branch(Memory, Config, <<"experiment">>).
%%%
%%% %% 使用归档
%%% {ok, SessionId} = beamai_memory:archive_session(Memory, <<"thread-1">>).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory).

-include_lib("beamai_memory/include/beamai_checkpointer.hrl").
-include_lib("beamai_memory/include/beamai_store.hrl").

%% 类型导出
-export_type([memory/0]).

%% 构造函数
-export([new/1]).

%%====================================================================
%% Checkpointer 便捷 API
%%====================================================================

%% 基本操作
-export([save_checkpoint/3, load_checkpoint/2, delete_checkpoint/2, list_checkpoints/2]).

%% 时间旅行
-export([go_back/3, go_forward/3, goto/3, undo/2, redo/2]).

%% 分支管理
-export([create_branch/4, switch_branch/3, list_branches/1, compare_branches/3]).

%% 历史查询
-export([list_history/2, get_lineage/2]).

%% 便捷函数
-export([get_messages/2, add_message/3, get_channel/3, set_channel/4]).

%%====================================================================
%% Store API（长期记忆）
%%====================================================================

-export([put/5, get/3, search/3, delete/3, list_namespaces/3]).

%%====================================================================
%% 归档 API
%%====================================================================

-export([archive_session/2, archive_session/3,
         load_archived/2, restore_archived/2,
         list_archived/1, list_archived/2,
         delete_archived/2]).

%%====================================================================
%% 工具函数
%%====================================================================

-export([checkpoint_to_state/1, state_to_checkpoint/2,
         get_checkpointer/1, get_store_manager/1]).

%%====================================================================
%% 统计和清理 API
%%====================================================================

-export([
    get_checkpoint_stats/1,
    get_checkpoint_count/2,
    get_total_checkpoint_count/1,
    prune_checkpoints/3,
    prune_all_checkpoints/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(memory, {
    checkpointer :: beamai_checkpoint_manager:manager(),
    store_manager :: beamai_store_manager:store_manager()
}).

-type memory() :: #memory{}.

%% 从 beamai_checkpointer 导入类型别名
-type thread_id() :: binary().
-type checkpoint_id() :: binary().
-type config() :: map().
-type checkpoint() :: #checkpoint{}.
%% 记录类型别名
-type checkpoint_metadata() :: #checkpoint_metadata{}.
-type checkpoint_tuple() :: {checkpoint(), checkpoint_metadata(), config() | undefined}.

%% 从 beamai_store 导入类型别名
-type namespace() :: beamai_store:namespace().
-type store_key() :: beamai_store:key().

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 Memory 实例
%%
%% Opts:
%% - context_store: Store 引用（必需），格式 {Module, Ref}
%% - persistent_store: Store 引用（可选），格式 {Module, Ref}
%%
%% 示例:
%% ```
%% {ok, Memory} = beamai_memory:new(#{
%%     context_store => {beamai_store_ets, my_store}
%% }).
%% '''
-spec new(map()) -> {ok, memory()} | {error, term()}.
new(Opts) ->
    case maps:get(context_store, Opts, undefined) of
        undefined ->
            {error, context_store_required};
        {_Module, _Ref} = ContextStore ->
            PersistentStore = maps:get(persistent_store, Opts, undefined),

            %% 创建 Checkpointer 管理器（使用 context_store）
            Checkpointer = beamai_checkpoint_manager:new(ContextStore),

            %% 创建 Store 管理器
            StoreManager = beamai_store_manager:new(ContextStore, PersistentStore),

            Memory = #memory{
                checkpointer = Checkpointer,
                store_manager = StoreManager
            },
            {ok, Memory};
        Invalid ->
            {error, {invalid_context_store, Invalid}}
    end.

%%====================================================================
%% Checkpointer 便捷 API
%%====================================================================

%% @doc 保存检查点
-spec save_checkpoint(memory(), config(), map()) -> {ok, checkpoint_id()} | {error, term()}.
save_checkpoint(#memory{checkpointer = Checkpointer}, Config, State) ->
    Checkpoint = #checkpoint{
        channel_values = State,
        channel_versions = build_versions(State),
        pending_writes = []
    },
    beamai_checkpoint_manager:save(Checkpointer, Checkpoint, Config).

%% @doc 加载检查点
-spec load_checkpoint(memory(), config()) -> {ok, map()} | {error, term()}.
load_checkpoint(#memory{checkpointer = Checkpointer}, Config) ->
    case beamai_checkpoint_manager:load(Checkpointer, latest, Config) of
        {ok, {Cp, _, _}} ->
            {ok, checkpoint_to_state(Cp)};
        {error, _} = Error ->
            Error
    end.

%% @doc 删除检查点
-spec delete_checkpoint(memory(), config()) -> ok | {error, term()}.
delete_checkpoint(#memory{checkpointer = Checkpointer}, Config) ->
    beamai_checkpoint_manager:delete(Checkpointer, Config).

%% @doc 列出检查点
-spec list_checkpoints(memory(), config()) -> {ok, [map()]} | {error, term()}.
list_checkpoints(#memory{checkpointer = Checkpointer}, Config) ->
    case beamai_checkpoint_manager:list(Checkpointer, Config) of
        {ok, Checkpoints} ->
            Summaries = [checkpoint_tuple_to_summary(CpTuple) || CpTuple <- Checkpoints],
            {ok, Summaries};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 时间旅行
%%====================================================================

%% @doc 回退 N 步
-spec go_back(memory(), config(), pos_integer()) -> {ok, map()} | {error, term()}.
go_back(#memory{checkpointer = Checkpointer}, Config, Steps) ->
    case beamai_checkpoint_time_travel:go_back(Checkpointer, Config, Steps) of
        {ok, Cp} -> {ok, checkpoint_to_state(Cp)};
        {error, _} = Error -> Error
    end.

%% @doc 前进 N 步
-spec go_forward(memory(), config(), pos_integer()) -> {ok, map()} | {error, term()}.
go_forward(#memory{checkpointer = Checkpointer}, Config, Steps) ->
    case beamai_checkpoint_time_travel:go_forward(Checkpointer, Config, Steps) of
        {ok, Cp} -> {ok, checkpoint_to_state(Cp)};
        {error, _} = Error -> Error
    end.

%% @doc 跳转到指定检查点
-spec goto(memory(), config(), checkpoint_id()) -> {ok, map()} | {error, term()}.
goto(#memory{checkpointer = Checkpointer}, Config, CheckpointId) ->
    case beamai_checkpoint_time_travel:goto(Checkpointer, Config, CheckpointId) of
        {ok, Cp} -> {ok, checkpoint_to_state(Cp)};
        {error, _} = Error -> Error
    end.

%% @doc 撤销（回退 1 步）
-spec undo(memory(), config()) -> {ok, map()} | {error, term()}.
undo(Memory, Config) ->
    go_back(Memory, Config, 1).

%% @doc 重做（前进 1 步）
-spec redo(memory(), config()) -> {ok, map()} | {error, term()}.
redo(Memory, Config) ->
    go_forward(Memory, Config, 1).

%%====================================================================
%% 分支管理
%%====================================================================

%% @doc 创建分支
-spec create_branch(memory(), config(), binary(), map()) -> {ok, binary()} | {error, term()}.
create_branch(#memory{checkpointer = Checkpointer}, Config, BranchName, Opts) ->
    beamai_checkpoint_branch:create_branch(Checkpointer, Config, BranchName, Opts).

%% @doc 切换分支
-spec switch_branch(memory(), config(), binary()) -> {ok, map()} | {error, term()}.
switch_branch(#memory{checkpointer = Checkpointer}, Config, BranchId) ->
    case beamai_checkpoint_branch:switch_branch(Checkpointer, Config, BranchId) of
        {ok, Cp} -> {ok, checkpoint_to_state(Cp)};
        {error, _} = Error -> Error
    end.

%% @doc 列出所有分支
-spec list_branches(memory()) -> {ok, [map()]} | {error, term()}.
list_branches(#memory{checkpointer = Checkpointer}) ->
    beamai_checkpoint_branch:list_branches(Checkpointer).

%% @doc 比较两个分支
-spec compare_branches(memory(), binary(), binary()) -> {ok, map()} | {error, term()}.
compare_branches(#memory{checkpointer = Checkpointer}, BranchId1, BranchId2) ->
    beamai_checkpoint_branch:compare_branches(Checkpointer, BranchId1, BranchId2).

%%====================================================================
%% 历史查询
%%====================================================================

%% @doc 列出历史记录
-spec list_history(memory(), config()) -> {ok, [map()]} | {error, term()}.
list_history(#memory{checkpointer = Checkpointer}, Config) ->
    beamai_checkpoint_time_travel:list_history(Checkpointer, Config).

%% @doc 获取祖先链
-spec get_lineage(memory(), config()) -> {ok, [map()]} | {error, term()}.
get_lineage(#memory{checkpointer = Checkpointer}, Config) ->
    case beamai_checkpoint_manager:get_lineage(Checkpointer, Config) of
        {ok, Lineage} ->
            States = [checkpoint_to_state(Cp) || {Cp, _, _} <- Lineage],
            {ok, States};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 便捷函数
%%====================================================================

%% @doc 获取消息历史
-spec get_messages(memory(), config()) -> {ok, [map()]} | {error, term()}.
get_messages(Memory, Config) ->
    case load_checkpoint(Memory, Config) of
        {ok, State} ->
            Messages = maps:get(messages, State, []),
            {ok, Messages};
        {error, _} = Error ->
            Error
    end.

%% @doc 添加消息
-spec add_message(memory(), config(), map()) -> ok | {error, term()}.
add_message(Memory, Config, Message) ->
    case load_checkpoint(Memory, Config) of
        {ok, State} ->
            Messages = maps:get(messages, State, []),
            NewState = State#{messages => Messages ++ [Message]},
            save_checkpoint(Memory, Config, NewState);
        {error, not_found} ->
            NewState = #{messages => [Message]},
            save_checkpoint(Memory, Config, NewState);
        {error, _} = Error ->
            Error
    end.

%% @doc 获取通道值
-spec get_channel(memory(), config(), atom() | binary()) -> {ok, term()} | {error, term()}.
get_channel(Memory, Config, Channel) ->
    case load_checkpoint(Memory, Config) of
        {ok, State} ->
            case maps:get(Channel, State, undefined) of
                undefined -> {error, not_found};
                Value -> {ok, Value}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 设置通道值
-spec set_channel(memory(), config(), atom() | binary(), term()) -> ok | {error, term()}.
set_channel(Memory, Config, Channel, Value) ->
    case load_checkpoint(Memory, Config) of
        {ok, State} ->
            NewState = State#{Channel => Value},
            save_checkpoint(Memory, Config, NewState);
        {error, not_found} ->
            NewState = #{Channel => Value},
            save_checkpoint(Memory, Config, NewState);
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Store API（长期记忆）
%%====================================================================

%% @doc 存储值
-spec put(memory(), namespace(), store_key(), map(), map()) -> ok | {error, term()}.
put(#memory{store_manager = StoreManager}, Namespace, Key, Value, Opts) ->
    beamai_store_manager:put(StoreManager, Namespace, Key, Value, Opts).

%% @doc 获取值
-spec get(memory(), namespace(), store_key()) -> {ok, beamai_store:item()} | {error, term()}.
get(#memory{store_manager = StoreManager}, Namespace, Key) ->
    beamai_store_manager:get(StoreManager, Namespace, Key).

%% @doc 搜索
-spec search(memory(), namespace(), map()) -> {ok, [beamai_store:search_result()]} | {error, term()}.
search(#memory{store_manager = StoreManager}, Namespace, Opts) ->
    beamai_store_manager:search(StoreManager, Namespace, Opts).

%% @doc 删除
-spec delete(memory(), namespace(), store_key()) -> ok | {error, term()}.
delete(#memory{store_manager = StoreManager}, Namespace, Key) ->
    beamai_store_manager:delete(StoreManager, Namespace, Key).

%% @doc 列出命名空间
-spec list_namespaces(memory(), namespace(), map()) -> {ok, [namespace()]} | {error, term()}.
list_namespaces(#memory{store_manager = StoreManager}, Prefix, Opts) ->
    beamai_store_manager:list_namespaces(StoreManager, Prefix, Opts).

%%====================================================================
%% 归档 API
%%====================================================================

%% @doc 归档会话（默认选项）
-spec archive_session(memory(), thread_id()) -> {ok, binary()} | {error, term()}.
archive_session(#memory{} = Memory, ThreadId) ->
    archive_session(Memory, ThreadId, #{}).

%% @doc 归档会话（带选项）
-spec archive_session(memory(), thread_id(), map()) -> {ok, binary()} | {error, term()}.
archive_session(#memory{checkpointer = Checkpointer, store_manager = StoreManager},
               ThreadId, Opts) ->
    beamai_store_archiver:archive_session(StoreManager, Checkpointer, ThreadId, Opts).

%% @doc 加载已归档的会话（只读）
-spec load_archived(memory(), binary()) -> {ok, [checkpoint_tuple()]} | {error, term()}.
load_archived(#memory{checkpointer = Checkpointer, store_manager = StoreManager}, SessionId) ->
    beamai_store_archiver:load_archived(StoreManager, Checkpointer, SessionId).

%% @doc 恢复已归档的会话到 Checkpointer
-spec restore_archived(memory(), binary()) -> {ok, thread_id()} | {error, term()}.
restore_archived(#memory{checkpointer = Checkpointer, store_manager = StoreManager}, SessionId) ->
    beamai_store_archiver:restore_archived(StoreManager, Checkpointer, SessionId).

%% @doc 列出已归档的会话
-spec list_archived(memory()) -> {ok, [map()]} | {error, term()}.
list_archived(#memory{store_manager = StoreManager}) ->
    list_archived(StoreManager, #{}).

%% @doc 列出已归档的会话（带选项）
-spec list_archived(memory(), map()) -> {ok, [map()]} | {error, term()}.
list_archived(#memory{store_manager = StoreManager}, Opts) ->
    beamai_store_archiver:list_archived(StoreManager, Opts).

%% @doc 删除已归档的会话
-spec delete_archived(memory(), binary()) -> ok | {error, term()}.
delete_archived(#memory{store_manager = StoreManager}, SessionId) ->
    beamai_store_archiver:delete_archived(StoreManager, SessionId).

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 将检查点转换为图状态
-spec checkpoint_to_state(checkpoint()) -> map().
checkpoint_to_state(#checkpoint{channel_values = Values}) ->
    Values.

%% @doc 将图状态转换为检查点
-spec state_to_checkpoint(config(), map()) -> checkpoint().
state_to_checkpoint(_Config, State) when is_map(State) ->
    #checkpoint{
        channel_values = State,
        channel_versions = build_versions(State),
        pending_writes = []
    }.

%% @doc 获取 Checkpointer 管理器（高级用法）
-spec get_checkpointer(memory()) -> beamai_checkpoint_manager:manager().
get_checkpointer(#memory{checkpointer = Checkpointer}) ->
    Checkpointer.

%% @doc 获取 Store 管理器（高级用法）
-spec get_store_manager(memory()) -> beamai_store_manager:store_manager().
get_store_manager(#memory{store_manager = StoreManager}) ->
    StoreManager.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 检查点元组转摘要
-spec checkpoint_tuple_to_summary(checkpoint_tuple()) -> map().
checkpoint_tuple_to_summary({Checkpoint, _Metadata, _ParentConfig}) ->
    #{
        checkpoint_id => Checkpoint#checkpoint.id,
        thread_id => Checkpoint#checkpoint.thread_id,
        parent_id => Checkpoint#checkpoint.parent_id,
        timestamp => Checkpoint#checkpoint.timestamp,
        channel_count => maps:size(Checkpoint#checkpoint.channel_values),
        version => Checkpoint#checkpoint.version
    }.

%% @private 构建版本号映射
-spec build_versions(map()) -> map().
build_versions(State) ->
    maps:map(fun(_, _) -> 1 end, State).

%%====================================================================
%% 统计和清理 API
%%====================================================================

%% @doc 获取检查点统计信息
-spec get_checkpoint_stats(memory()) -> map().
get_checkpoint_stats(#memory{checkpointer = Checkpointer}) ->
    beamai_checkpoint_manager:get_checkpoint_stats(Checkpointer).

%% @doc 获取线程的检查点数量
-spec get_checkpoint_count(memory(), thread_id()) -> non_neg_integer().
get_checkpoint_count(#memory{checkpointer = Checkpointer}, ThreadId) ->
    beamai_checkpoint_manager:get_checkpoint_count(Checkpointer, ThreadId).

%% @doc 获取总检查点数量（所有线程和分支）
-spec get_total_checkpoint_count(memory()) -> non_neg_integer().
get_total_checkpoint_count(#memory{checkpointer = Checkpointer}) ->
    beamai_checkpoint_manager:get_total_count(Checkpointer).

%% @doc 清理线程的检查点
%%
%% 保留最新的 KeepCount 个检查点，删除其余的。
-spec prune_checkpoints(memory(), thread_id(), pos_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
prune_checkpoints(#memory{checkpointer = Checkpointer}, ThreadId, KeepCount) ->
    beamai_checkpoint_manager:prune_checkpoints(Checkpointer, ThreadId, KeepCount).

%% @doc 清理所有线程的检查点
%%
%% 对所有线程执行清理，每个线程保留指定数量的检查点。
-spec prune_all_checkpoints(memory(), pos_integer()) -> {ok, map()}.
prune_all_checkpoints(#memory{checkpointer = Checkpointer}, KeepCount) ->
    %% 获取所有线程
    case beamai_checkpoint_manager:list(Checkpointer, #{}) of
        {ok, AllCheckpoints} ->
            %% 按线程分组
            ThreadIds = lists:usort([
                Cp#checkpoint.thread_id
             || {Cp, _, _} <- AllCheckpoints]),

            %% 清理每个线程
            Results = lists:map(fun(ThreadId) ->
                case beamai_checkpoint_manager:prune_checkpoints(Checkpointer, ThreadId, KeepCount) of
                    {ok, Count} -> {ThreadId, Count};
                    {error, Reason} -> {ThreadId, {error, Reason}}
                end
            end, ThreadIds),

            {ok, #{
                total_pruned => lists:foldl(fun({_, Count}, Acc) ->
                    case Count of
                        {error, _} -> Acc;
                        _ -> Acc + Count
                    end
                end, 0, Results),
                details => Results
            }};
        {error, Reason} ->
            {error, Reason}
    end.
