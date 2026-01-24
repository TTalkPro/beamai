%%%-------------------------------------------------------------------
%%% @doc Agent Memory 统一 API - 双 Store 架构
%%%
%%% 提供统一的 Memory 操作接口，整合短期记忆和长期记忆。
%%%
%%% == 双 Store 架构 ==
%%%
%%% - context_store: 存储 Snapshot 数据和当前对话（快速，内存）
%%% - persistent_store: 存储长期记忆和归档（持久化，可选）
%%%
%%% Store 由外部创建和管理，Memory 只持有引用。
%%% 当 persistent_store 为 undefined 时，降级为单 Store 模式。
%%%
%%% == 命名空间设计 ==
%%%
%%% Snapshot 数据：
%%% - [<<"snapshots">>, ThreadId, SnapshotId] - 快照数据
%%% - [<<"snapshots">>, <<"_index">>, ThreadId] - 线程索引
%%%
%%% 长期记忆：
%%% - [<<"semantic">>, UserId, ...] - 语义记忆
%%% - [<<"procedural">>, UserId, ...] - 程序记忆
%%% - [<<"episodic">>, UserId, ...] - 情景记忆
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 1. 创建 Store 进程（可加入监督树）
%%% {ok, _} = beamai_store_ets:start_link(my_context, #{max_items => 10000}),
%%% {ok, _} = beamai_store_sqlite:start_link(my_persist, #{db_path => "/tmp/store.db"}),
%%%
%%% %% 2. 创建 Memory（传入 Store 引用）
%%% {ok, Mem} = beamai_memory:new(#{
%%%     context_store => {beamai_store_ets, my_context},
%%%     persistent_store => {beamai_store_sqlite, my_persist}
%%% }),
%%%
%%% %% 3. 使用 Memory API
%%% Config = #{thread_id => <<"thread-1">>},
%%% ok = beamai_memory:save_snapshot(Mem, Config, #{messages => []}),
%%% {ok, State} = beamai_memory:load_snapshot(Mem, Config).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory).

-include_lib("beamai_memory/include/beamai_snapshot.hrl").
-include_lib("beamai_memory/include/beamai_store.hrl").

%% 类型导出
-export_type([memory/0]).

%% 构造函数
-export([new/1]).

%% 短期记忆 (Snapshot) API
-export([
    %% 快照操作
    save_snapshot/3,
    save_snapshot/4,
    load_snapshot/2,
    load_snapshot_tuple/2,
    load_latest_snapshot/2,
    list_snapshots/2,
    delete_snapshot/2,
    snapshot_count/2,

    %% 分支管理
    branch/3,
    branch/4,
    get_lineage/2,
    get_branches/2,
    diff_snapshots/3,

    %% 便捷函数
    get_messages/2,
    add_message/3,
    get_channel/3,
    set_channel/4
]).

%% 长期记忆 (Store) API
-export([
    put/4,
    put/5,
    get/3,
    search/3,
    delete/3,
    list_namespaces/2,
    list_namespaces/3
]).

%% 归档 API
-export([
    archive_session/2,
    load_archived_session/2,
    list_archived_sessions/2
]).

%% 工具函数
-export([
    snapshot_to_state/1,
    state_to_snapshot/2,
    get_context_store/1,
    get_persistent_store/1,
    get_thread_id/1
]).

%% Agent 中断查询 API
-export([
    has_pending_interrupt/2,
    get_interrupt_context/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 双 Store 架构
-type memory() :: #{
    context_store := beamai_store:store(),
    persistent_store := beamai_store:store() | undefined,
    thread_id := binary()
}.

%% 从 beamai_snapshot 导入类型别名
-type thread_id() :: binary().
-type snapshot_id() :: binary().
-type config() :: map().
-type snapshot() :: #snapshot{}.
-type snapshot_metadata() :: #snapshot_metadata{}.
-type snapshot_tuple() :: {snapshot(), snapshot_metadata(), config() | undefined}.

%% 从 beamai_store 导入类型别名
-type namespace() :: beamai_store:namespace().
-type store_key() :: beamai_store:key().

%%====================================================================
%% 命名空间常量
%%====================================================================

-define(NS_CHECKPOINTS, <<"snapshots">>).
-define(NS_INDEX, <<"_index">>).
-define(KEY_LATEST, <<"latest">>).
-define(KEY_INDEX, <<"index">>).

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 Memory 实例
%%
%% Opts:
%% - context_store: Store 引用（必需），格式 {Module, Ref}
%% - persistent_store: Store 引用（可选），格式 {Module, Ref}
%% - thread_id: 线程 ID（可选），如果未指定则自动生成 UUID
%%
%% Store 必须事先通过对应后端的 start_link 创建：
%% ```
%% {ok, _} = beamai_store_ets:start_link(my_store, #{}),
%% {ok, Mem} = beamai_memory:new(#{
%%     context_store => {beamai_store_ets, my_store},
%%     thread_id => <<"my_session">>  %% 可选
%% }).
%% '''
%%
%% thread_id 创建后不可更改（不可变性）。
-spec new(map()) -> {ok, memory()} | {error, term()}.
new(Opts) ->
    case maps:get(context_store, Opts, undefined) of
        undefined ->
            {error, context_store_required};
        {Module, Ref} = ContextStore when is_atom(Module), (is_pid(Ref) orelse is_atom(Ref)) ->
            PersistentStore = maps:get(persistent_store, Opts, undefined),
            %% 如果未指定 thread_id，自动生成 UUID
            ThreadId = case maps:get(thread_id, Opts, undefined) of
                undefined -> generate_thread_id();
                Id when is_binary(Id) -> Id;
                Id when is_list(Id) -> list_to_binary(Id);
                _ -> generate_thread_id()
            end,
            {ok, #{
                context_store => ContextStore,
                persistent_store => PersistentStore,
                thread_id => ThreadId
            }};
        Invalid ->
            {error, {invalid_context_store, Invalid}}
    end.

%%====================================================================
%% 短期记忆 API - 快照操作
%%====================================================================

%% @doc 保存快照
%%
%% 从图状态创建快照并保存到 context_store。
-spec save_snapshot(memory(), config(), map()) -> ok | {error, term()}.
save_snapshot(Memory, Config, State) ->
    save_snapshot(Memory, Config, State, #{}).

%% @doc 保存快照（带元数据）
-spec save_snapshot(memory(), config(), map(), map()) -> ok | {error, term()}.
save_snapshot(#{context_store := Store}, Config, State, MetadataMap) ->
    ThreadId = get_thread_id_from_config(Config),
    ParentCpId = maps:get(snapshot_id, Config, undefined),

    %% 构建快照
    Checkpoint = #snapshot{
        id = generate_snapshot_id(),
        thread_id = ThreadId,
        parent_id = ParentCpId,
        values = State,
        timestamp = erlang:system_time(millisecond)
    },

    %% 构建元数据（面向 Process Framework）
    Metadata = #snapshot_metadata{
        %% 快照类型与流程信息
        snapshot_type = determine_snapshot_type(State, MetadataMap),
        process_name = maps:get(process_name, MetadataMap, undefined),
        process_state = maps:get(process_state, MetadataMap, undefined),

        %% 步骤执行信息
        step_id = maps:get(step_id, MetadataMap, undefined),
        step_activations = maps:get(step_activations, MetadataMap, #{}),

        %% 执行标识
        run_id = maps:get(run_id, Config, undefined),
        agent_id = maps:get(agent_id, Config, undefined),
        agent_name = maps:get(agent_name, Config, undefined),

        %% 用户自定义元数据
        metadata = maps:get(metadata, MetadataMap, #{})
    },

    %% 构建父配置
    ParentConfig = case ParentCpId of
        undefined -> undefined;
        _ -> #{thread_id => ThreadId, snapshot_id => ParentCpId}
    end,

    %% 序列化并存储到 context_store
    CpNs = snapshot_namespace(ThreadId),
    CpValue = snapshot_to_map(Checkpoint, Metadata, ParentConfig),

    case beamai_store:put(Store, CpNs, Checkpoint#snapshot.id, CpValue) of
        ok ->
            %% 更新线程索引
            update_thread_index(Store, ThreadId, Checkpoint);
        {error, _} = Error ->
            Error
    end.

%% @doc 加载快照
%%
%% 返回图状态格式的数据。
-spec load_snapshot(memory(), config()) ->
    {ok, map()} | {error, not_found | term()}.
load_snapshot(Memory, Config) ->
    case load_snapshot_tuple(Memory, Config) of
        {ok, {Checkpoint, _, _}} ->
            {ok, snapshot_to_state(Checkpoint)};
        {error, _} = Error ->
            Error
    end.

%% @doc 加载快照元组
%%
%% 返回 {Snapshot, Metadata, ParentConfig}。
-spec load_snapshot_tuple(memory(), config()) ->
    {ok, snapshot_tuple()} | {error, not_found | term()}.
load_snapshot_tuple(#{context_store := Store}, Config) ->
    ThreadId = get_thread_id_from_config(Config),
    CheckpointId = maps:get(snapshot_id, Config, undefined),

    case CheckpointId of
        undefined ->
            %% 获取最新快照
            get_latest_snapshot(Store, ThreadId);
        CpId ->
            %% 获取指定快照
            get_snapshot_by_id(Store, ThreadId, CpId)
    end.

%% @doc 加载最新快照
-spec load_latest_snapshot(memory(), config()) ->
    {ok, map()} | {error, not_found | term()}.
load_latest_snapshot(Memory, Config) ->
    ConfigWithoutCpId = maps:remove(snapshot_id, Config),
    load_snapshot(Memory, ConfigWithoutCpId).

%% @doc 列出快照
-spec list_snapshots(memory(), config() | map()) ->
    {ok, [snapshot_tuple()]} | {error, term()}.
list_snapshots(#{context_store := Store}, Config) ->
    ThreadId = maps:get(thread_id, Config, undefined),
    Limit = maps:get(limit, Config, 100),

    case ThreadId of
        undefined ->
            %% 列出所有快照
            list_all_snapshots(Store, Limit);
        TId ->
            %% 列出指定线程的快照
            list_thread_snapshots(Store, TId, Limit)
    end.

%% @doc 删除快照
-spec delete_snapshot(memory(), config()) -> ok | {error, term()}.
delete_snapshot(#{context_store := Store}, Config) ->
    ThreadId = get_thread_id_from_config(Config),
    CheckpointId = maps:get(snapshot_id, Config, undefined),

    case CheckpointId of
        undefined ->
            {error, snapshot_id_required};
        CpId ->
            CpNs = snapshot_namespace(ThreadId),
            beamai_store:delete(Store, CpNs, CpId)
    end.

%% @doc 获取快照数量
-spec snapshot_count(memory(), config()) -> non_neg_integer().
snapshot_count(Memory, Config) ->
    case list_snapshots(Memory, Config) of
        {ok, List} -> length(List);
        {error, _} -> 0
    end.

%%====================================================================
%% 短期记忆 API - 分支管理
%%====================================================================

%% @doc 从快照创建分支
-spec branch(memory(), config(), map()) ->
    {ok, snapshot_id()} | {error, term()}.
branch(Memory, Config, BranchOpts) ->
    branch(Memory, Config, BranchOpts, #{}).

-spec branch(memory(), config(), map(), map()) ->
    {ok, snapshot_id()} | {error, term()}.
branch(Memory, Config, BranchOpts, MetadataMap) ->
    case load_snapshot_tuple(Memory, Config) of
        {ok, {SourceCp, _SourceMeta, _ParentConfig}} ->
            %% 创建新分支
            NewCheckpointId = generate_snapshot_id(),
            SourceCpId = SourceCp#snapshot.id,

            NewThreadId = maps:get(thread_id, BranchOpts,
                maps:get(branch_name, BranchOpts,
                    generate_branch_thread_id(SourceCp#snapshot.thread_id))),

            %% 复制源快照到新分支
            BranchState = SourceCp#snapshot.values,
            BranchConfig = #{thread_id => NewThreadId, snapshot_id => SourceCpId},
            BranchMetadata = MetadataMap#{
                snapshot_type => branch,
                branch_from => SourceCpId,
                branch_name => maps:get(branch_name, BranchOpts, undefined)
            },

            case save_snapshot(Memory, BranchConfig, BranchState, BranchMetadata) of
                ok ->
                    {ok, NewCheckpointId};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 获取快照的祖先链
-spec get_lineage(memory(), config()) ->
    {ok, [snapshot_tuple()]} | {error, term()}.
get_lineage(#{context_store := Store}, Config) ->
    ThreadId = get_thread_id_from_config(Config),
    CheckpointId = maps:get(snapshot_id, Config, undefined),

    case CheckpointId of
        undefined ->
            case get_latest_snapshot(Store, ThreadId) of
                {ok, {Cp, _, _}} ->
                    get_lineage_recursive(Store, Cp#snapshot.id, ThreadId, []);
                {error, _} = Error ->
                    Error
            end;
        CpId ->
            get_lineage_recursive(Store, CpId, ThreadId, [])
    end.

%% @doc 获取快照的子分支
-spec get_branches(memory(), config()) ->
    {ok, [snapshot_tuple()]} | {error, term()}.
get_branches(#{context_store := Store}, Config) ->
    ParentCpId = maps:get(snapshot_id, Config, undefined),

    case ParentCpId of
        undefined ->
            {error, snapshot_id_required};
        _ ->
            %% 搜索所有以 ParentCpId 为父节点的快照
            case beamai_store:search(Store, [?NS_CHECKPOINTS], #{}) of
                {ok, Results} ->
                    Branches = lists:filtermap(fun(#search_result{item = Item}) ->
                        case map_to_snapshot_tuple(Item#store_item.value) of
                            {ok, {Cp, _, _} = Tuple} when Cp#snapshot.parent_id =:= ParentCpId ->
                                {true, Tuple};
                            _ ->
                                false
                        end
                    end, Results),
                    {ok, Branches};
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc 比较两个快照的差异
-spec diff_snapshots(memory(), config(), config()) ->
    {ok, map()} | {error, term()}.
diff_snapshots(Memory, Config1, Config2) ->
    case {load_snapshot_tuple(Memory, Config1), load_snapshot_tuple(Memory, Config2)} of
        {{ok, {Cp1, _, _}}, {ok, {Cp2, _, _}}} ->
            Values1 = Cp1#snapshot.values,
            Values2 = Cp2#snapshot.values,

            Keys1 = maps:keys(Values1),
            Keys2 = maps:keys(Values2),

            Added = [K || K <- Keys2, not maps:is_key(K, Values1)],
            Removed = [K || K <- Keys1, not maps:is_key(K, Values2)],
            Changed = [K || K <- Keys1,
                            maps:is_key(K, Values2),
                            maps:get(K, Values1) =/= maps:get(K, Values2)],

            DiffResult = #{
                added => [{K, maps:get(K, Values2)} || K <- Added],
                removed => [{K, maps:get(K, Values1)} || K <- Removed],
                changed => [{K, #{
                    old => maps:get(K, Values1),
                    new => maps:get(K, Values2)
                }} || K <- Changed],
                snapshot1 => #{
                    id => Cp1#snapshot.id,
                    thread_id => Cp1#snapshot.thread_id,
                    timestamp => Cp1#snapshot.timestamp
                },
                snapshot2 => #{
                    id => Cp2#snapshot.id,
                    thread_id => Cp2#snapshot.thread_id,
                    timestamp => Cp2#snapshot.timestamp
                }
            },
            {ok, DiffResult};
        {{error, _} = Error, _} ->
            Error;
        {_, {error, _} = Error} ->
            Error
    end.

%%====================================================================
%% 短期记忆 API - 便捷函数
%%====================================================================

%% @doc 获取消息历史
-spec get_messages(memory(), config()) -> {ok, [map()]} | {error, not_found}.
get_messages(Memory, Config) ->
    case load_snapshot(Memory, Config) of
        {ok, State} ->
            Messages = maps:get(messages, State, []),
            {ok, Messages};
        {error, _} = Error ->
            Error
    end.

%% @doc 添加消息
-spec add_message(memory(), config(), map()) -> ok | {error, term()}.
add_message(Memory, Config, Message) ->
    case load_snapshot(Memory, Config) of
        {ok, State} ->
            Messages = maps:get(messages, State, []),
            NewState = State#{messages => Messages ++ [Message]},
            save_snapshot(Memory, Config, NewState);
        {error, not_found} ->
            NewState = #{messages => [Message]},
            save_snapshot(Memory, Config, NewState);
        {error, _} = Error ->
            Error
    end.

%% @doc 获取通道值
-spec get_channel(memory(), config(), atom() | binary()) ->
    {ok, term()} | {error, not_found}.
get_channel(Memory, Config, Channel) ->
    case load_snapshot(Memory, Config) of
        {ok, State} ->
            case maps:get(Channel, State, undefined) of
                undefined -> {error, not_found};
                Value -> {ok, Value}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 设置通道值
-spec set_channel(memory(), config(), atom() | binary(), term()) ->
    ok | {error, term()}.
set_channel(Memory, Config, Channel, Value) ->
    case load_snapshot(Memory, Config) of
        {ok, State} ->
            NewState = State#{Channel => Value},
            save_snapshot(Memory, Config, NewState);
        {error, not_found} ->
            NewState = #{Channel => Value},
            save_snapshot(Memory, Config, NewState);
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 长期记忆 API
%%====================================================================

%% @doc 存储值到长期记忆
%%
%% 使用 persistent_store（如果可用），否则使用 context_store。
-spec put(memory(), namespace(), store_key(), map()) -> ok | {error, term()}.
put(Memory, Namespace, Key, Value) ->
    put(Memory, Namespace, Key, Value, #{}).

%% @doc 存储值到长期记忆（带选项）
-spec put(memory(), namespace(), store_key(), map(), map()) -> ok | {error, term()}.
put(Memory, Namespace, Key, Value, Opts) ->
    Store = get_long_term_store(Memory),
    beamai_store:put(Store, Namespace, Key, Value, Opts).

%% @doc 从长期记忆获取值
-spec get(memory(), namespace(), store_key()) ->
    {ok, beamai_store:item()} | {error, not_found | term()}.
get(Memory, Namespace, Key) ->
    Store = get_long_term_store(Memory),
    beamai_store:get(Store, Namespace, Key).

%% @doc 在长期记忆中搜索
-spec search(memory(), namespace(), map()) ->
    {ok, [beamai_store:search_result()]} | {error, term()}.
search(Memory, NamespacePrefix, Opts) ->
    Store = get_long_term_store(Memory),
    beamai_store:search(Store, NamespacePrefix, Opts).

%% @doc 从长期记忆删除
-spec delete(memory(), namespace(), store_key()) -> ok | {error, term()}.
delete(Memory, Namespace, Key) ->
    Store = get_long_term_store(Memory),
    beamai_store:delete(Store, Namespace, Key).

%% @doc 列出命名空间下的所有子命名空间
-spec list_namespaces(memory(), namespace()) ->
    {ok, [namespace()]} | {error, term()}.
list_namespaces(Memory, Prefix) ->
    list_namespaces(Memory, Prefix, #{}).

%% @doc 列出命名空间（带选项）
-spec list_namespaces(memory(), namespace(), map()) ->
    {ok, [namespace()]} | {error, term()}.
list_namespaces(Memory, Prefix, Opts) ->
    Store = get_long_term_store(Memory),
    beamai_store:list_namespaces(Store, Prefix, Opts).

%%====================================================================
%% 归档 API
%%====================================================================

%% @doc 归档会话
%%
%% 将 context_store 中的会话数据迁移到 persistent_store。
-spec archive_session(memory(), thread_id()) -> ok | {error, term()}.
archive_session(#{persistent_store := undefined}, _ThreadId) ->
    {error, persistent_store_not_configured};
archive_session(#{context_store := ContextStore, persistent_store := PersistentStore}, ThreadId) ->
    %% 获取线程的所有快照
    case list_thread_snapshots(ContextStore, ThreadId, 1000) of
        {ok, Snapshots} when Snapshots =/= [] ->
            %% 迁移到 persistent_store
            ArchiveNs = [<<"archives">>, <<"sessions">>, ThreadId],
            ArchiveValue = #{
                thread_id => ThreadId,
                snapshots => [snapshot_tuple_to_map(Tuple) || Tuple <- Snapshots],
                archived_at => erlang:system_time(millisecond)
            },
            beamai_store:put(PersistentStore, ArchiveNs, ThreadId, ArchiveValue);
        {ok, []} ->
            {error, no_snapshots_to_archive};
        {error, _} = Error ->
            Error
    end.

%% @doc 加载归档的会话
-spec load_archived_session(memory(), thread_id()) ->
    {ok, [snapshot_tuple()]} | {error, term()}.
load_archived_session(#{persistent_store := undefined}, _ThreadId) ->
    {error, persistent_store_not_configured};
load_archived_session(#{persistent_store := PersistentStore}, ThreadId) ->
    ArchiveNs = [<<"archives">>, <<"sessions">>, ThreadId],
    case beamai_store:get(PersistentStore, ArchiveNs, ThreadId) of
        {ok, #store_item{value = #{snapshots := SnapshotMaps}}} ->
            Snapshots = lists:filtermap(fun(CpMap) ->
                case map_to_snapshot_tuple(CpMap) of
                    {ok, Tuple} -> {true, Tuple};
                    _ -> false
                end
            end, SnapshotMaps),
            {ok, Snapshots};
        {error, _} = Error ->
            Error
    end.

%% @doc 列出归档的会话
-spec list_archived_sessions(memory(), map()) ->
    {ok, [thread_id()]} | {error, term()}.
list_archived_sessions(#{persistent_store := undefined}, _Opts) ->
    {error, persistent_store_not_configured};
list_archived_sessions(#{persistent_store := PersistentStore}, Opts) ->
    ArchivePrefix = [<<"archives">>, <<"sessions">>],
    case beamai_store:search(PersistentStore, ArchivePrefix, Opts) of
        {ok, Results} ->
            ThreadIds = [Item#store_item.key || #search_result{item = Item} <- Results],
            {ok, ThreadIds};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 将快照转换为图状态
-spec snapshot_to_state(snapshot()) -> map().
snapshot_to_state(#snapshot{values = Values}) ->
    Values.

%% @doc 将图状态转换为快照
-spec state_to_snapshot(config(), map()) -> snapshot().
state_to_snapshot(Config, State) when is_map(State) ->
    ThreadId = get_thread_id_from_config(Config),
    ParentCpId = maps:get(snapshot_id, Config, undefined),

    #snapshot{
        id = generate_snapshot_id(),
        thread_id = ThreadId,
        parent_id = ParentCpId,
        values = State,
        timestamp = erlang:system_time(millisecond)
    }.

%% @doc 获取 context_store
-spec get_context_store(memory()) -> beamai_store:store().
get_context_store(#{context_store := Store}) ->
    Store.

%% @doc 获取 persistent_store
-spec get_persistent_store(memory()) -> beamai_store:store() | undefined.
get_persistent_store(#{persistent_store := Store}) ->
    Store.

%% @doc 获取 Memory 的 thread_id
%%
%% thread_id 在 Memory 创建时设置（或自动生成），之后不可更改。
%% 使用此函数获取 Memory 实例的 thread_id 用于 snapshot 操作。
-spec get_thread_id(memory()) -> binary().
get_thread_id(#{thread_id := ThreadId}) ->
    ThreadId.

%%====================================================================
%% Agent 中断查询 API
%%====================================================================

%% @doc 查询是否有未完成的中断
%%
%% 加载最新 snapshot，检查其中是否包含 interrupt_state。
%% 用于快速判断某个 thread 是否有待恢复的中断。
%%
%% @param Memory memory 实例
%% @param Config 配置 map（需包含 thread_id）
%% @returns boolean()
-spec has_pending_interrupt(memory(), config()) -> boolean().
has_pending_interrupt(Memory, Config) ->
    case load_latest_snapshot(Memory, Config) of
        {ok, Data} ->
            case get_flex(interrupt_state, Data) of
                undefined -> false;
                null -> false;
                _ -> true
            end;
        _ -> false
    end.

%% @doc 获取中断上下文（不加载完整状态）
%%
%% 从最新 snapshot 中提取中断的原因信息，
%% 用于在恢复前展示给用户。
%%
%% @param Memory memory 实例
%% @param Config 配置 map（需包含 thread_id）
%% @returns {ok, map()} | {error, term()}
-spec get_interrupt_context(memory(), config()) -> {ok, map()} | {error, term()}.
get_interrupt_context(Memory, Config) ->
    case load_latest_snapshot(Memory, Config) of
        {ok, Data} ->
            case get_flex(interrupt_state, Data) of
                undefined -> {error, not_interrupted};
                null -> {error, not_interrupted};
                IntState when is_map(IntState) ->
                    {ok, #{
                        reason => get_flex(reason, IntState),
                        interrupt_type => get_flex(interrupt_type, IntState),
                        interrupted_tool_call => get_flex(interrupted_tool_call, IntState),
                        created_at => get_flex(created_at, IntState)
                    }}
            end;
        {error, not_found} -> {error, not_interrupted};
        {error, _} = Err -> Err
    end.

%%====================================================================
%% 内部函数 - 命名空间
%%====================================================================

%% @private 快照命名空间
-spec snapshot_namespace(thread_id()) -> namespace().
snapshot_namespace(ThreadId) ->
    [?NS_CHECKPOINTS, ThreadId].

%% @private 索引命名空间
-spec index_namespace(thread_id()) -> namespace().
index_namespace(ThreadId) ->
    [?NS_CHECKPOINTS, ?NS_INDEX, ThreadId].

%%====================================================================
%% 内部函数 - 快照操作
%%====================================================================

%% @private 获取指定快照
-spec get_snapshot_by_id(beamai_store:store(), thread_id(), snapshot_id()) ->
    {ok, snapshot_tuple()} | {error, not_found}.
get_snapshot_by_id(Store, ThreadId, CheckpointId) ->
    CpNs = snapshot_namespace(ThreadId),
    case beamai_store:get(Store, CpNs, CheckpointId) of
        {ok, #store_item{value = Value}} ->
            map_to_snapshot_tuple(Value);
        {error, not_found} ->
            %% 尝试在所有线程中查找
            find_snapshot_in_all_threads(Store, CheckpointId);
        {error, _} = Error ->
            Error
    end.

%% @private 获取最新快照
-spec get_latest_snapshot(beamai_store:store(), thread_id()) ->
    {ok, snapshot_tuple()} | {error, not_found}.
get_latest_snapshot(Store, ThreadId) ->
    IdxNs = index_namespace(ThreadId),
    case beamai_store:get(Store, IdxNs, ?KEY_LATEST) of
        {ok, #store_item{value = Value}} ->
            case get_flex(snapshot_id, Value) of
                undefined -> {error, not_found};
                LatestCpId -> get_snapshot_by_id(Store, ThreadId, LatestCpId)
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%% @private 更新线程索引
-spec update_thread_index(beamai_store:store(), thread_id(), snapshot()) ->
    ok | {error, term()}.
update_thread_index(Store, ThreadId, Checkpoint) ->
    IdxNs = index_namespace(ThreadId),

    %% 更新 latest 指针
    LatestValue = #{
        snapshot_id => Checkpoint#snapshot.id,
        timestamp => Checkpoint#snapshot.timestamp
    },
    case beamai_store:put(Store, IdxNs, ?KEY_LATEST, LatestValue) of
        ok ->
            %% 更新索引列表
            NewEntry = #{
                snapshot_id => Checkpoint#snapshot.id,
                timestamp => Checkpoint#snapshot.timestamp
            },
            case beamai_store:get(Store, IdxNs, ?KEY_INDEX) of
                {ok, #store_item{value = Value}} ->
                    Entries = get_flex(entries, Value, []),
                    NewEntries = [NewEntry | Entries],
                    beamai_store:put(Store, IdxNs, ?KEY_INDEX, #{entries => NewEntries});
                {error, not_found} ->
                    beamai_store:put(Store, IdxNs, ?KEY_INDEX, #{entries => [NewEntry]})
            end;
        {error, _} = Error ->
            Error
    end.

%% @private 列出所有快照
-spec list_all_snapshots(beamai_store:store(), pos_integer()) ->
    {ok, [snapshot_tuple()]} | {error, term()}.
list_all_snapshots(Store, Limit) ->
    case beamai_store:search(Store, [?NS_CHECKPOINTS], #{limit => Limit}) of
        {ok, Results} ->
            Snapshots = lists:filtermap(fun(#search_result{item = Item}) ->
                %% 排除索引项
                case Item#store_item.namespace of
                    [?NS_CHECKPOINTS, ?NS_INDEX | _] -> false;
                    _ ->
                        case map_to_snapshot_tuple(Item#store_item.value) of
                            {ok, Tuple} -> {true, Tuple};
                            _ -> false
                        end
                end
            end, Results),
            {ok, Snapshots};
        {error, _} = Error ->
            Error
    end.

%% @private 列出线程快照
-spec list_thread_snapshots(beamai_store:store(), thread_id(), pos_integer()) ->
    {ok, [snapshot_tuple()]} | {error, term()}.
list_thread_snapshots(Store, ThreadId, Limit) ->
    CpNs = snapshot_namespace(ThreadId),
    case beamai_store:search(Store, CpNs, #{limit => Limit}) of
        {ok, Results} ->
            Snapshots = lists:filtermap(fun(#search_result{item = Item}) ->
                case map_to_snapshot_tuple(Item#store_item.value) of
                    {ok, Tuple} -> {true, Tuple};
                    _ -> false
                end
            end, Results),
            %% 按时间戳降序排序
            Sorted = lists:sort(fun({Cp1, _, _}, {Cp2, _, _}) ->
                Cp1#snapshot.timestamp > Cp2#snapshot.timestamp
            end, Snapshots),
            {ok, Sorted};
        {error, _} = Error ->
            Error
    end.

%% @private 在所有线程中查找快照
-spec find_snapshot_in_all_threads(beamai_store:store(), snapshot_id()) ->
    {ok, snapshot_tuple()} | {error, not_found}.
find_snapshot_in_all_threads(Store, CheckpointId) ->
    case beamai_store:search(Store, [?NS_CHECKPOINTS], #{}) of
        {ok, Results} ->
            find_in_results(Results, CheckpointId);
        {error, _} ->
            {error, not_found}
    end.

%% @private 在搜索结果中查找
-spec find_in_results([beamai_store:search_result()], snapshot_id()) ->
    {ok, snapshot_tuple()} | {error, not_found}.
find_in_results([], _CheckpointId) ->
    {error, not_found};
find_in_results([#search_result{item = Item} | Rest], CheckpointId) ->
    case Item#store_item.key of
        CheckpointId ->
            map_to_snapshot_tuple(Item#store_item.value);
        _ ->
            find_in_results(Rest, CheckpointId)
    end.

%% @private 递归获取祖先链
-spec get_lineage_recursive(beamai_store:store(), snapshot_id(), thread_id(), [snapshot_tuple()]) ->
    {ok, [snapshot_tuple()]} | {error, term()}.
get_lineage_recursive(Store, CheckpointId, ThreadId, Acc) ->
    case get_snapshot_by_id(Store, ThreadId, CheckpointId) of
        {ok, {Checkpoint, _, _} = Tuple} ->
            NewAcc = [Tuple | Acc],
            case Checkpoint#snapshot.parent_id of
                undefined ->
                    {ok, lists:reverse(NewAcc)};
                ParentId ->
                    get_lineage_recursive(Store, ParentId, ThreadId, NewAcc)
            end;
        {error, not_found} when Acc =/= [] ->
            {ok, lists:reverse(Acc)};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 内部函数 - 序列化/反序列化
%%====================================================================

%% @private 快照转 Map
-spec snapshot_to_map(snapshot(), snapshot_metadata(), config() | undefined) -> map().
snapshot_to_map(Checkpoint, Metadata, ParentConfig) ->
    #{
        snapshot => #{
            id => Checkpoint#snapshot.id,
            thread_id => Checkpoint#snapshot.thread_id,
            parent_id => Checkpoint#snapshot.parent_id,
            values => Checkpoint#snapshot.values,
            timestamp => Checkpoint#snapshot.timestamp
        },
        metadata => #{
            %% 快照类型与流程信息
            snapshot_type => Metadata#snapshot_metadata.snapshot_type,
            process_name => Metadata#snapshot_metadata.process_name,
            process_state => Metadata#snapshot_metadata.process_state,
            %% 步骤执行信息
            step_id => Metadata#snapshot_metadata.step_id,
            step_activations => Metadata#snapshot_metadata.step_activations,
            %% 执行标识
            run_id => Metadata#snapshot_metadata.run_id,
            agent_id => Metadata#snapshot_metadata.agent_id,
            agent_name => Metadata#snapshot_metadata.agent_name,
            %% 用户自定义元数据
            metadata => Metadata#snapshot_metadata.metadata
        },
        parent_config => ParentConfig
    }.

%% @private snapshot_tuple 转 Map（用于归档）
-spec snapshot_tuple_to_map(snapshot_tuple()) -> map().
snapshot_tuple_to_map({Checkpoint, Metadata, ParentConfig}) ->
    snapshot_to_map(Checkpoint, Metadata, ParentConfig).

%% @private Map 转快照元组
%% 支持 atom 和 binary 键（JSON 序列化后键变成 binary）
-spec map_to_snapshot_tuple(map()) -> {ok, snapshot_tuple()} | {error, invalid_format}.
map_to_snapshot_tuple(Map) when is_map(Map) ->
    CpMap = get_flex(snapshot, Map),
    MetaMap = get_flex(metadata, Map),
    ParentConfig = get_flex(parent_config, Map),
    case {CpMap, MetaMap} of
        {undefined, _} -> {error, invalid_format};
        {_, undefined} -> {error, invalid_format};
        _ ->
            Checkpoint = #snapshot{
                id = get_flex(id, CpMap),
                thread_id = get_flex(thread_id, CpMap),
                parent_id = get_flex(parent_id, CpMap, undefined),
                values = get_flex(values, CpMap, #{}),
                timestamp = get_flex(timestamp, CpMap, 0)
            },
            Metadata = #snapshot_metadata{
                %% 快照类型与流程信息
                snapshot_type = get_flex(snapshot_type, MetaMap, undefined),
                process_name = get_flex(process_name, MetaMap, undefined),
                process_state = get_flex(process_state, MetaMap, undefined),
                %% 步骤执行信息
                step_id = get_flex(step_id, MetaMap, undefined),
                step_activations = get_flex(step_activations, MetaMap, #{}),
                %% 执行标识
                run_id = get_flex(run_id, MetaMap, undefined),
                agent_id = get_flex(agent_id, MetaMap, undefined),
                agent_name = get_flex(agent_name, MetaMap, undefined),
                %% 用户自定义元数据
                metadata = get_flex(metadata, MetaMap, #{})
            },
            {ok, {Checkpoint, Metadata, ParentConfig}}
    end;
map_to_snapshot_tuple(_) ->
    {error, invalid_format}.

%%====================================================================
%% 内部函数 - 工具
%%====================================================================

%% @private 获取长期记忆使用的 Store
-spec get_long_term_store(memory()) -> beamai_store:store().
get_long_term_store(#{persistent_store := undefined, context_store := ContextStore}) ->
    ContextStore;
get_long_term_store(#{persistent_store := PersistentStore}) ->
    PersistentStore.

%% @private 从 Config 获取 thread_id
-spec get_thread_id_from_config(config()) -> thread_id().
get_thread_id_from_config(Config) when is_map(Config) ->
    maps:get(thread_id, Config, maps:get(<<"thread_id">>, Config, undefined)).

%% @private 生成快照 ID
-spec generate_snapshot_id() -> snapshot_id().
generate_snapshot_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("cp_~16.16.0b_~4.16.0b", [Ts, Rand])).

%% @private 生成线程 ID（UUID 格式）
%%
%% thread_id 创建后不可更改（不可变性）。
-spec generate_thread_id() -> thread_id().
generate_thread_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFFFFFF),
    list_to_binary(io_lib:format("thread_~16.16.0b_~8.16.0b", [Ts, Rand])).

%% @private 生成分支线程 ID
-spec generate_branch_thread_id(thread_id()) -> thread_id().
generate_branch_thread_id(SourceThreadId) ->
    Rand = rand:uniform(16#FFFF),
    <<SourceThreadId/binary, "_branch_", (integer_to_binary(Rand))/binary>>.

%% @private 获取 map 值（同时支持 atom 和 binary 键）
%% JSON 序列化后键会变成 binary，此函数兼容两种格式
-spec get_flex(atom(), map()) -> term().
get_flex(Key, Map) when is_atom(Key), is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined -> maps:get(atom_to_binary(Key), Map, undefined);
        Value -> Value
    end.

%% @private 获取 map 值（带默认值）
-spec get_flex(atom(), map(), term()) -> term().
get_flex(Key, Map, Default) when is_atom(Key), is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined -> maps:get(atom_to_binary(Key), Map, Default);
        Value -> Value
    end.

%%====================================================================
%% 内部函数 - 元数据处理
%%====================================================================

%% @private 确定快照类型
%%
%% snapshot_type 从 MetadataMap 获取（State 现在只包含 global_state）
-spec determine_snapshot_type(map(), map()) -> atom() | undefined.
determine_snapshot_type(_State, MetadataMap) ->
    maps:get(snapshot_type, MetadataMap, undefined).
