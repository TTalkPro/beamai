%%%-------------------------------------------------------------------
%%% @doc Store 归档器 - 会话归档和恢复
%%%
%%% 负责在 context_store 和 persistent_store 之间迁移数据。
%%%
%%% == 功能 ==
%%%
%%% - 将 context_store 中的会话归档到 persistent_store
%%% - 从 persistent_store 加载已归档的会话到 context_store
%%% - 列出和管理已归档的会话
%%% - 支持会话总结（可选）
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% StoreManager = beamai_store_manager:new(ContextStore, PersistStore),
%%% Snapshotter = beamai_snapshot_manager:new(ContextStore),
%%%
%%% %% 归档当前会话
%%% {ok, SessionId} = beamai_store_archiver:archive_session(
%%%     StoreManager,
%%%     Snapshotter,
%%%     <<"thread-1">>
%%% ).
%%%
%%% %% 列出已归档的会话
%%% {ok, Sessions} = beamai_store_archiver:list_archived(StoreManager).
%%%
%%% %% 加载已归档的会话
%%% {ok, Snapshots} = beamai_store_archiver:load_archived(
%%%     StoreManager,
%%%     Snapshotter,
%%%     <<"session-123">>
%%% ).
%%%
%%% %% 归档并总结
%%% SummarizeFn = fun(Snapshots) ->
%%%     %% 生成总结
%%%     <<"会话包含 5 条消息">
%%% end,
%%% {ok, SessionId} = beamai_store_archiver:archive_session(
%%%     StoreManager,
%%%     Snapshotter,
%%%     <<"thread-1">>,
%%%     #{summarize_fn => SummarizeFn}
%%% ).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_store_archiver).

-include_lib("beamai_memory/include/beamai_snapshot.hrl").
-include_lib("beamai_memory/include/beamai_store.hrl").

%% 归档操作
-export([archive_session/3, archive_session/4]).

%% 加载操作
-export([load_archived/3, restore_archived/3]).

%% 查询操作
-export([list_archived/1, list_archived/2, get_archive_info/2]).

%% 删除操作
-export([delete_archived/2, delete_all_archived/1]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 基本类型别名
-type snapshot() :: #snapshot{}.
-type snapshot_metadata() :: #snapshot_metadata{}.
-type config() :: map().
-type snapshot_tuple() :: {snapshot(), snapshot_metadata(), config() | undefined}.

-type archive_opts() :: #{
    summarize_fn => fun(([snapshot_tuple()]) -> binary()),
    metadata => map(),
    tags => [binary()]
}.

-type archive_info() :: #{
    session_id := binary(),
    thread_id := binary(),
    archived_at := integer(),
    snapshot_count := non_neg_integer(),
    summary => binary(),
    metadata => map(),
    tags => [binary()]
}.

-export_type([archive_opts/0, archive_info/0]).

%%====================================================================
%% 命名空间常量
%%====================================================================

-define(NS_ARCHIVES, [<<"archives">>, <<"sessions">>]).

%%====================================================================
%% 归档操作
%%====================================================================

%% @doc 归档会话（默认选项）
-spec archive_session(
    beamai_store_manager:store_manager(),
    beamai_snapshot_manager:manager(),
    binary()
) -> {ok, binary()} | {error, term()}.
archive_session(StoreManager, Snapshotter, ThreadId) ->
    archive_session(StoreManager, Snapshotter, ThreadId, #{}).

%% @doc 归档会话（带选项）
-spec archive_session(
    beamai_store_manager:store_manager(),
    beamai_snapshot_manager:manager(),
    binary(),
    archive_opts()
) -> {ok, binary()} | {error, term()}.
archive_session(StoreManager, Snapshotter, ThreadId, Opts) ->
    %% 检查是否有 persistent_store
    case beamai_store_manager:has_persistent(StoreManager) of
        false ->
            {error, persistent_store_not_configured};
        true ->
            %% 获取线程的所有快照
            Config = #{thread_id => ThreadId},
            case beamai_snapshot_manager:list(Snapshotter, Config) of
                {ok, Snapshots} when Snapshots =/= [] ->
                    do_archive(StoreManager, ThreadId, Snapshots, Opts);
                {ok, []} ->
                    {error, no_snapshots_to_archive};
                {error, _} = Error ->
                    Error
            end
    end.

%% @private 执行归档
-spec do_archive(
    beamai_store_manager:store_manager(),
    binary(),
    [snapshot_tuple()],
    archive_opts()
) -> {ok, binary()}.
do_archive(StoreManager, ThreadId, Snapshots, Opts) ->
    %% 生成会话 ID
    SessionId = generate_session_id(ThreadId),

    %% 调用总结函数（如果提供）
    Summary = case maps_get(summarize_fn, Opts) of
        undefined -> undefined;
        SummarizeFn -> SummarizeFn(Snapshots)
    end,

    %% 构建归档数据
    ArchiveData = #{
        session_id => SessionId,
        thread_id => ThreadId,
        archived_at => erlang:system_time(millisecond),
        snapshot_count => length(Snapshots),
        snapshots => [snapshot_tuple_to_map(CpTuple) || CpTuple <- Snapshots],
        summary => Summary,
        metadata => maps_get(metadata, Opts, #{}),
        tags => maps_get(tags, Opts, [])
    },

    %% 存储到 persistent_store
    Namespace = ?NS_ARCHIVES,
    Key = SessionId,

    case beamai_store_manager:put(StoreManager, Namespace, Key, ArchiveData, #{persistent => true}) of
        ok ->
            %% 清空 context_store 中的快照
            %% TODO: 需要从 Snapshotter 清空
            {ok, SessionId};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 加载操作
%%====================================================================

%% @doc 加载已归档的会话（只读）
-spec load_archived(
    beamai_store_manager:store_manager(),
    beamai_snapshot_manager:manager(),
    binary()
) -> {ok, [snapshot_tuple()]} | {error, term()}.
load_archived(StoreManager, _Snapshotter, SessionId) ->
    case beamai_store_manager:get(StoreManager, ?NS_ARCHIVES, SessionId) of
        {ok, #store_item{value = ArchiveData}} ->
            SnapshotMaps = maps_get(snapshots, ArchiveData, []),
            Snapshots = lists:filtermap(fun(CpMap) ->
                map_to_snapshot_tuple(CpMap)
            end, SnapshotMaps),
            {ok, Snapshots};
        {error, _} = Error ->
            Error
    end.

%% @doc 恢复已归档的会话到 Snapshotter
-spec restore_archived(
    beamai_store_manager:store_manager(),
    beamai_snapshot_manager:manager(),
    binary()
) -> {ok, binary()} | {error, term()}.
restore_archived(StoreManager, Snapshotter, SessionId) ->
    case load_archived(StoreManager, Snapshotter, SessionId) of
        {ok, Snapshots} when Snapshots =/= [] ->
            %% 恢复到 context_store
            %% 使用第一个快照的 thread_id
            {FirstCp, _, _} = hd(Snapshots),
            ThreadId = FirstCp#snapshot.thread_id,

            %% 逐个保存快照
            lists:foreach(fun({Cp, _Meta, _ParentCfg}) ->
                NewCp = Cp#snapshot{
                    id = generate_snapshot_id(),  % 生成新 ID
                    thread_id = ThreadId
                },
                Config = #{thread_id => ThreadId},
                beamai_snapshot_manager:save(Snapshotter, NewCp, Config)
            end, Snapshots),

            {ok, ThreadId};
        {ok, []} ->
            {error, empty_archive};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 查询操作
%%====================================================================

%% @doc 列出所有已归档的会话
-spec list_archived(beamai_store_manager:store_manager()) ->
    {ok, [archive_info()]} | {error, term()}.
list_archived(StoreManager) ->
    list_archived(StoreManager, #{}).

%% @doc 列出已归档的会话（带选项）
-spec list_archived(beamai_store_manager:store_manager(), map()) ->
    {ok, [archive_info()]} | {error, term()}.
list_archived(StoreManager, Opts) ->
    Limit = maps_get(limit, Opts, 100),
    Tags = maps_get(tags, Opts, []),

    case beamai_store_manager:search(StoreManager, ?NS_ARCHIVES, #{limit => Limit}) of
        {ok, Results} ->
            Archives = [extract_archive_info(Item) || #search_result{item = Item} <- Results],

            %% 按标签过滤
            FilteredArchives = case Tags of
                [] -> Archives;
                _ -> filter_by_tags(Archives, Tags)
            end,

            {ok, FilteredArchives};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取归档信息
-spec get_archive_info(beamai_store_manager:store_manager(), binary()) ->
    {ok, archive_info()} | {error, term()}.
get_archive_info(StoreManager, SessionId) ->
    case beamai_store_manager:get(StoreManager, ?NS_ARCHIVES, SessionId) of
        {ok, #store_item{value = ArchiveData}} ->
            Info = #{
                session_id => maps_get(session_id, ArchiveData),
                thread_id => maps_get(thread_id, ArchiveData),
                archived_at => maps_get(archived_at, ArchiveData),
                snapshot_count => maps_get(snapshot_count, ArchiveData),
                summary => maps_get(summary, ArchiveData, undefined),
                metadata => maps_get(metadata, ArchiveData, #{}),
                tags => maps_get(tags, ArchiveData, [])
            },
            {ok, Info};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 删除操作
%%====================================================================

%% @doc 删除已归档的会话
-spec delete_archived(beamai_store_manager:store_manager(), binary()) ->
    ok | {error, term()}.
delete_archived(StoreManager, SessionId) ->
    beamai_store_manager:delete(StoreManager, ?NS_ARCHIVES, SessionId).

%% @doc 删除所有已归档的会话
-spec delete_all_archived(beamai_store_manager:store_manager()) ->
    ok | {error, term()}.
delete_all_archived(StoreManager) ->
    case list_archived(StoreManager, #{limit => 10000}) of
        {ok, Archives} ->
            lists:foreach(fun(Info) ->
                SessionId = maps_get(session_id, Info),
                delete_archived(StoreManager, SessionId)
            end, Archives),
            ok;
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 生成会话 ID
-spec generate_session_id(binary()) -> binary().
generate_session_id(ThreadId) ->
    Ts = erlang:system_time(millisecond),
    Rand = rand:uniform(16#FFFF),
    <<ThreadId/binary, "-", (integer_to_binary(Ts))/binary, "-",
      (integer_to_binary(Rand))/binary>>.

%% @private 生成快照 ID
-spec generate_snapshot_id() -> binary().
generate_snapshot_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("cp_~16.16.0b_~4.16.0b", [Ts, Rand])).

%% @private 快照元组转 Map
-spec snapshot_tuple_to_map(snapshot_tuple()) -> map().
snapshot_tuple_to_map({Snapshot, Metadata, ParentConfig}) ->
    #{
        <<"snapshot">> => #{
            <<"id">> => Snapshot#snapshot.id,
            <<"thread_id">> => Snapshot#snapshot.thread_id,
            <<"parent_id">> => Snapshot#snapshot.parent_id,
            <<"values">> => Snapshot#snapshot.values,
            <<"timestamp">> => Snapshot#snapshot.timestamp
        },
        <<"metadata">> => #{
            %% 快照类型与流程信息
            <<"snapshot_type">> => Metadata#snapshot_metadata.snapshot_type,
            <<"process_name">> => Metadata#snapshot_metadata.process_name,
            <<"process_state">> => Metadata#snapshot_metadata.process_state,
            %% 步骤执行信息
            <<"step_id">> => Metadata#snapshot_metadata.step_id,
            <<"step_activations">> => Metadata#snapshot_metadata.step_activations,
            %% 执行标识
            <<"run_id">> => Metadata#snapshot_metadata.run_id,
            <<"agent_id">> => Metadata#snapshot_metadata.agent_id,
            <<"agent_name">> => Metadata#snapshot_metadata.agent_name,
            %% 用户自定义元数据
            <<"metadata">> => Metadata#snapshot_metadata.metadata
        },
        <<"parent_config">> => ParentConfig
    }.

%% @private Map 转快照元组
-spec map_to_snapshot_tuple(map()) -> {ok, snapshot_tuple()} | {error, invalid_format}.
map_to_snapshot_tuple(Map) when is_map(Map) ->
    CpMap = maps_get(<<"snapshot">>, Map),
    MetaMap = maps_get(<<"metadata">>, Map),
    ParentConfig = maps_get(<<"parent_config">>, Map),

    case {CpMap, MetaMap} of
        {undefined, _} -> {error, invalid_format};
        {_, undefined} -> {error, invalid_format};
        _ ->
            Snapshot = #snapshot{
                id = maps_get(<<"id">>, CpMap),
                thread_id = maps_get(<<"thread_id">>, CpMap),
                parent_id = maps_get(<<"parent_id">>, CpMap, undefined),
                values = maps_get(<<"values">>, CpMap, #{}),
                timestamp = maps_get(<<"timestamp">>, CpMap, 0)
            },
            Metadata = #snapshot_metadata{
                %% 快照类型与流程信息
                snapshot_type = maps_get(<<"snapshot_type">>, MetaMap, undefined),
                process_name = maps_get(<<"process_name">>, MetaMap, undefined),
                process_state = maps_get(<<"process_state">>, MetaMap, undefined),
                %% 步骤执行信息
                step_id = maps_get(<<"step_id">>, MetaMap, undefined),
                step_activations = maps_get(<<"step_activations">>, MetaMap, #{}),
                %% 执行标识
                run_id = maps_get(<<"run_id">>, MetaMap, undefined),
                agent_id = maps_get(<<"agent_id">>, MetaMap, undefined),
                agent_name = maps_get(<<"agent_name">>, MetaMap, undefined),
                %% 用户自定义元数据
                metadata = maps_get(<<"metadata">>, MetaMap, #{})
            },
            {ok, {Snapshot, Metadata, ParentConfig}}
    end;
map_to_snapshot_tuple(_) ->
    {error, invalid_format}.

%% @private 提取归档信息
-spec extract_archive_info(beamai_store:search_result()) -> archive_info().
extract_archive_info(#search_result{item = #store_item{value = ArchiveData}}) ->
    #{
        session_id => maps_get(session_id, ArchiveData),
        thread_id => maps_get(thread_id, ArchiveData),
        archived_at => maps_get(archived_at, ArchiveData),
        snapshot_count => maps_get(snapshot_count, ArchiveData),
        summary => maps_get(summary, ArchiveData, undefined),
        metadata => maps_get(metadata, ArchiveData, #{}),
        tags => maps_get(tags, ArchiveData, [])
    }.

%% @private 按标签过滤
-spec filter_by_tags([archive_info()], [binary()]) -> [archive_info()].
filter_by_tags(Archives, Tags) ->
    lists:filter(fun(Archive) ->
        ArchiveTags = maps_get(tags, Archive, []),
        lists:any(fun(Tag) -> lists:member(Tag, ArchiveTags) end, Tags)
    end, Archives).

%% @private 安全获取 map 值
-spec maps_get(binary(), map()) -> term().
maps_get(Key, Map) ->
    maps_get(Key, Map, undefined).

-spec maps_get(binary(), map(), term()) -> term().
maps_get(Key, Map, Default) ->
    maps:get(Key, Map, Default).
