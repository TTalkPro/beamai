%%%-------------------------------------------------------------------
%%% @doc Snapshot 时间旅行扩展模块
%%%
%%% 提供高级时间旅行功能，建立在 beamai_snapshot_manager 基础之上。
%%%
%%% == 功能 ==
%%%
%%% - go_back/go_forward: 回退/前进到历史状态
%%% - goto: 跳转到指定快照
%%% - list_history: 列出历史记录
%%% - diff: 比较两个快照的差异
%%% - undo/redo: 撤销/重做操作
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% Manager = beamai_snapshot_manager:new(Store),
%%%
%%% %% 回退 3 步
%%% {ok, Cp} = beamai_snapshot_time_travel:go_back(Manager, Config, 3).
%%%
%%% %% 前进 1 步
%%% {ok, Cp} = beamai_snapshot_time_travel:go_forward(Manager, Config, 1).
%%%
%%% %% 跳转到指定快照
%%% {ok, Cp} = beamai_snapshot_time_travel:goto(Manager, Config, <<"cp-123">>).
%%%
%%% %% 查看历史
%%% {ok, History} = beamai_snapshot_time_travel:list_history(Manager, Config).
%%%
%%% %% 撤销（回退 1 步）
%%% {ok, Cp} = beamai_snapshot_time_travel:undo(Manager, Config).
%%%
%%% %% 重做（前进 1 步）
%%% {ok, Cp} = beamai_snapshot_time_travel:redo(Manager, Config).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_snapshot_time_travel).

-include_lib("beamai_memory/include/beamai_snapshot.hrl").

%% 时间旅行操作
-export([go_back/3, go_forward/3, goto/3, undo/2, redo/2]).

%% 历史查询
-export([list_history/2, get_history_summary/2]).

%% 差异比较
-export([diff/3, format_diff/1]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 基本类型别名
-type snapshot() :: #snapshot{}.
-type snapshot_metadata() :: #snapshot_metadata{}.
-type config() :: map().

-type snapshot_tuple() :: {snapshot(), snapshot_metadata(), config() | undefined}.
-type snapshot_summary() :: #{
    snapshot_id := binary(),
    thread_id := binary(),
    parent_id => binary() | undefined,
    timestamp := integer(),
    channel_count := non_neg_integer()
}.

-export_type([snapshot_summary/0]).

%%====================================================================
%% 时间旅行操作
%%====================================================================

%% @doc 回退 N 步
%% @see beamai_snapshot_manager:go_back/3
-spec go_back(beamai_snapshot_manager:manager(), map(), pos_integer()) ->
    {ok, snapshot()} | {error, term()}.
go_back(Manager, Config, Steps) ->
    beamai_snapshot_manager:go_back(Manager, Config, Steps).

%% @doc 前进 N 步
%% @see beamai_snapshot_manager:go_forward/3
-spec go_forward(beamai_snapshot_manager:manager(), map(), pos_integer()) ->
    {ok, snapshot()} | {error, term()}.
go_forward(Manager, Config, Steps) ->
    beamai_snapshot_manager:go_forward(Manager, Config, Steps).

%% @doc 跳转到指定快照
%% @see beamai_snapshot_manager:goto/3
-spec goto(beamai_snapshot_manager:manager(), map(), binary()) ->
    {ok, snapshot()} | {error, term()}.
goto(Manager, Config, CheckpointId) ->
    beamai_snapshot_manager:goto(Manager, Config, CheckpointId).

%% @doc 撤销（回退 1 步）
-spec undo(beamai_snapshot_manager:manager(), map()) ->
    {ok, snapshot()} | {error, term()}.
undo(Manager, Config) ->
    go_back(Manager, Config, 1).

%% @doc 重做（前进 1 步）
-spec redo(beamai_snapshot_manager:manager(), map()) ->
    {ok, snapshot()} | {error, term()}.
redo(Manager, Config) ->
    go_forward(Manager, Config, 1).

%%====================================================================
%% 历史查询
%%====================================================================

%% @doc 列出历史记录
-spec list_history(beamai_snapshot_manager:manager(), map()) ->
    {ok, [snapshot_summary()]} | {error, term()}.
list_history(Manager, Config) ->
    case beamai_snapshot_manager:list(Manager, Config) of
        {ok, Checkpoints} ->
            Summaries = [snapshot_to_summary(CpTuple) || CpTuple <- Checkpoints],
            {ok, Summaries};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取历史摘要
-spec get_history_summary(beamai_snapshot_manager:manager(), map()) ->
    {ok, map()} | {error, term()}.
get_history_summary(Manager, Config) ->
    case list_history(Manager, Config) of
        {ok, Summaries} ->
            TotalCount = length(Summaries),
            FirstTimestamp = case Summaries of
                [] -> undefined;
                [Last | _] -> maps:get(timestamp, Last)
            end,
            LastTimestamp = case Summaries of
                [] -> undefined;
                Rest -> maps:get(timestamp, lists:last(Rest))
            end,

            Summary = #{
                total_count => TotalCount,
                first_timestamp => FirstTimestamp,
                last_timestamp => LastTimestamp,
                snapshots => Summaries
            },
            {ok, Summary};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 差异比较
%%====================================================================

%% @doc 比较两个快照的差异
%% @see beamai_snapshot_manager:diff/3
-spec diff(beamai_snapshot_manager:manager(), map(), map()) ->
    {ok, map()} | {error, term()}.
diff(Manager, Config1, Config2) ->
    beamai_snapshot_manager:diff(Manager, Config1, Config2).

%% @doc 格式化差异为可读字符串
-spec format_diff({ok, map()}) -> iolist().
format_diff({ok, DiffResult}) ->
    #{
        added := Added,
        removed := Removed,
        changed := Changed,
        snapshot1 := Cp1,
        snapshot2 := Cp2
    } = DiffResult,

    [
        io_lib:format("=== Snapshot Diff ===~n", []),
        io_lib:format("~nSnapshot 1: ~s (~p)~n", [
            maps_get(id, Cp1), maps_get(timestamp, Cp1)
        ]),
        io_lib:format("Snapshot 2: ~s (~p)~n", [
            maps_get(id, Cp2), maps_get(timestamp, Cp2)
        ]),
        format_added(Added),
        format_removed(Removed),
        format_changed(Changed)
    ];
format_diff({error, _}) ->
    "Error formatting diff".

%% @private
format_added([]) -> [];
format_added(Added) ->
    [
        io_lib:format("~nAdded (~p):~n", [length(Added)]),
        [io_lib:format("  + ~p: ~p~n", [K, V]) || {K, V} <- Added]
    ].

%% @private
format_removed([]) -> [];
format_removed(Removed) ->
    [
        io_lib:format("~nRemoved (~p):~n", [length(Removed)]),
        [io_lib:format("  - ~p: ~p~n", [K, V]) || {K, V} <- Removed]
    ].

%% @private
format_changed([]) -> [];
format_changed(Changed) ->
    [
        io_lib:format("~nChanged (~p):~n", [length(Changed)]),
        [io_lib:format("  * ~p:~n    old: ~p~n    new: ~p~n", [
            K, maps_get(old, V), maps_get(new, V)
        ]) || {K, V} <- Changed]
    ].

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 快照转摘要
-spec snapshot_to_summary(snapshot_tuple()) -> snapshot_summary().
snapshot_to_summary({Checkpoint, _Metadata, _ParentConfig}) ->
    #{
        snapshot_id => Checkpoint#snapshot.id,
        thread_id => Checkpoint#snapshot.thread_id,
        parent_id => Checkpoint#snapshot.parent_id,
        timestamp => Checkpoint#snapshot.timestamp,
        channel_count => maps:size(Checkpoint#snapshot.values)
    }.

%% @private 安全获取 map 值
-spec maps_get(atom(), map()) -> term().
maps_get(Key, Map) ->
    maps:get(Key, Map, undefined).
