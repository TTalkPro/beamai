%%%-------------------------------------------------------------------
%%% @doc Agent Event Memory - 交互事件记忆模块
%%%
%%% 管理 Agent 的交互事件记忆，包括：
%%% - 事件记录和获取
%%% - 事件搜索和删除
%%% - 对话片段事件获取
%%%
%%% 从 beamai_episodic_memory 拆分出来的独立模块。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_event_memory).

-include_lib("beamai_memory/include/beamai_store.hrl").
-include_lib("beamai_memory/include/beamai_episodic_memory.hrl").

%% 类型别名
-type memory() :: beamai_memory:memory().
-type user_id() :: binary().
-type episode_id() :: binary().
-type event_id() :: binary().

%% 类型导出
-export_type([user_id/0, episode_id/0, event_id/0]).

%% 交互事件 API
-export([
    record_event/3,
    get_event/3,
    get_episode_events/3,
    get_episode_events/4,
    find_events/3,
    delete_event/3
]).

%% 工具函数
-export([
    get_event_namespace/1
]).

%% 内部函数（供门面模块使用）
-export([
    event_to_map/1,
    map_to_event/1
]).

%%====================================================================
%% 交互事件 API
%%====================================================================

%% @doc 记录交互事件
%%
%% EventData 支持：
%% - id: 事件 ID（可选，自动生成）
%% - episode_id: 所属对话片段 ID（必需）
%% - type: 事件类型 (message | tool_call | error | milestone)
%% - description: 事件描述（必需）
%% - data: 事件数据
%% - importance: 重要性评分
%% - tags: 标签列表
-spec record_event(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
record_event(Memory, UserId, EventData) ->
    Namespace = get_event_namespace(UserId),
    EventId = maps:get(id, EventData, beamai_memory_utils:generate_id(<<"evt">>)),
    Timestamp = beamai_memory_utils:current_timestamp(),

    Event = #event{
        id = EventId,
        episode_id = maps:get(episode_id, EventData),
        type = maps:get(type, EventData, ?EVENT_MESSAGE),
        description = maps:get(description, EventData),
        data = maps:get(data, EventData, #{}),
        timestamp = maps:get(timestamp, EventData, Timestamp),
        importance = maps:get(importance, EventData, ?DEFAULT_IMPORTANCE),
        tags = maps:get(tags, EventData, []),
        metadata = maps:get(metadata, EventData, #{})
    },

    Value = event_to_map(Event),
    beamai_memory:put(Memory, Namespace, EventId, Value).

%% @doc 获取单个事件
-spec get_event(memory(), user_id(), event_id()) ->
    {ok, #event{}} | {error, not_found | term()}.
get_event(Memory, UserId, EventId) ->
    Namespace = get_event_namespace(UserId),
    case beamai_memory:get(Memory, Namespace, EventId) of
        {ok, #store_item{value = Value}} ->
            {ok, map_to_event(Value)};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取对话片段的所有事件
-spec get_episode_events(memory(), user_id(), episode_id()) ->
    {ok, [#event{}]} | {error, term()}.
get_episode_events(Memory, UserId, EpisodeId) ->
    get_episode_events(Memory, UserId, EpisodeId, #{}).

-spec get_episode_events(memory(), user_id(), episode_id(), map()) ->
    {ok, [#event{}]} | {error, term()}.
get_episode_events(Memory, UserId, EpisodeId, Opts) ->
    Namespace = get_event_namespace(UserId),
    Filter = #{<<"episode_id">> => EpisodeId},
    Limit = maps:get(limit, Opts, ?DEFAULT_EVENT_LIMIT),
    SearchOpts = #{filter => Filter, limit => Limit},
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Events = [map_to_event(Item#store_item.value)
                      || #search_result{item = Item} <- Results],
            %% 按时间排序
            Sorted = lists:sort(fun(A, B) ->
                A#event.timestamp =< B#event.timestamp
            end, Events),
            {ok, Sorted};
        {error, _} = Error ->
            Error
    end.

%% @doc 查找事件
-spec find_events(memory(), user_id(), map()) ->
    {ok, [#event{}]} | {error, term()}.
find_events(Memory, UserId, Opts) ->
    Namespace = get_event_namespace(UserId),
    SearchOpts = build_event_filter(Opts),
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Events = [map_to_event(Item#store_item.value)
                      || #search_result{item = Item} <- Results],
            {ok, Events};
        {error, _} = Error ->
            Error
    end.

%% @doc 删除事件
-spec delete_event(memory(), user_id(), event_id()) ->
    {ok, memory()} | {error, term()}.
delete_event(Memory, UserId, EventId) ->
    Namespace = get_event_namespace(UserId),
    beamai_memory:delete(Memory, Namespace, EventId).

%%====================================================================
%% 命名空间工具函数
%%====================================================================

%% @doc 获取用户事件命名空间
-spec get_event_namespace(user_id()) -> [binary()].
get_event_namespace(UserId) ->
    [?NS_EPISODIC, UserId, ?NS_EVENTS].

%%====================================================================
%% 内部函数 - 记录与 Map 转换
%%====================================================================

%% @doc Event 记录转 Map
-spec event_to_map(#event{}) -> map().
event_to_map(#event{} = E) ->
    #{
        <<"id">> => E#event.id,
        <<"episode_id">> => E#event.episode_id,
        <<"type">> => beamai_memory_types:event_type_to_binary(E#event.type),
        <<"description">> => E#event.description,
        <<"data">> => E#event.data,
        <<"timestamp">> => E#event.timestamp,
        <<"importance">> => E#event.importance,
        <<"tags">> => E#event.tags,
        <<"metadata">> => E#event.metadata
    }.

%% @doc Map 转 Event 记录
-spec map_to_event(map()) -> #event{}.
map_to_event(M) ->
    TypeBin = maps:get(<<"type">>, M, <<"message">>),
    #event{
        id = maps:get(<<"id">>, M),
        episode_id = maps:get(<<"episode_id">>, M),
        type = beamai_memory_types:binary_to_event_type(TypeBin),
        description = maps:get(<<"description">>, M),
        data = maps:get(<<"data">>, M, #{}),
        timestamp = maps:get(<<"timestamp">>, M),
        importance = maps:get(<<"importance">>, M, ?DEFAULT_IMPORTANCE),
        tags = maps:get(<<"tags">>, M, []),
        metadata = maps:get(<<"metadata">>, M, #{})
    }.

%%====================================================================
%% 内部函数 - 搜索过滤
%%====================================================================

%% @private 构建 Event 搜索过滤条件
-spec build_event_filter(map()) -> map().
build_event_filter(Opts) ->
    FilterSpecs = [
        {episode_id, direct},
        {type, fun atom_to_binary/1}
    ],
    beamai_memory_utils:build_search_opts(Opts, FilterSpecs).
