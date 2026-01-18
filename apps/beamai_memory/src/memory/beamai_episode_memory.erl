%%%-------------------------------------------------------------------
%%% @doc Agent Episode Memory - 对话片段记忆模块
%%%
%%% 管理 Agent 的对话片段记忆，包括：
%%% - 对话片段创建和获取
%%% - 对话片段更新和完成
%%% - 对话片段搜索和删除
%%%
%%% 从 beamai_episodic_memory 拆分出来的独立模块。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_episode_memory).

-include_lib("beamai_memory/include/beamai_store.hrl").
-include_lib("beamai_memory/include/beamai_episodic_memory.hrl").

%% 类型别名
-type memory() :: beamai_memory:memory().
-type user_id() :: binary().
-type episode_id() :: binary().

%% 类型导出
-export_type([user_id/0, episode_id/0]).

%% 对话片段 API
-export([
    create_episode/3,
    update_episode/4,
    get_episode/3,
    find_episodes/3,
    find_episodes/2,
    complete_episode/3,
    complete_episode/4,
    delete_episode/3
]).

%% 工具函数
-export([
    get_episode_namespace/1
]).

%% 内部函数（供门面模块使用）
-export([
    episode_to_map/1,
    map_to_episode/1
]).

%%====================================================================
%% 对话片段 API
%%====================================================================

%% @doc 创建对话片段
%%
%% EpisodeData 支持：
%% - id: 片段 ID（可选，默认使用 thread_id）
%% - thread_id: 关联的会话 ID（必需）
%% - title: 对话标题
%% - summary: 对话摘要
%% - participants: 参与者列表
%% - topics: 话题标签
%% - importance: 重要性评分
-spec create_episode(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
create_episode(Memory, UserId, EpisodeData) ->
    Namespace = get_episode_namespace(UserId),
    ThreadId = maps:get(thread_id, EpisodeData),
    EpisodeId = maps:get(id, EpisodeData, ThreadId),
    Timestamp = beamai_memory_utils:current_timestamp(),

    Episode = #episode{
        id = EpisodeId,
        thread_id = ThreadId,
        title = maps:get(title, EpisodeData, undefined),
        summary = maps:get(summary, EpisodeData, undefined),
        participants = maps:get(participants, EpisodeData, []),
        message_count = maps:get(message_count, EpisodeData, 0),
        topics = maps:get(topics, EpisodeData, []),
        sentiment = maps:get(sentiment, EpisodeData, undefined),
        outcome = maps:get(outcome, EpisodeData, ?OUTCOME_ONGOING),
        importance = maps:get(importance, EpisodeData, ?DEFAULT_IMPORTANCE),
        started_at = maps:get(started_at, EpisodeData, Timestamp),
        ended_at = maps:get(ended_at, EpisodeData, undefined),
        created_at = Timestamp,
        updated_at = Timestamp,
        embedding = maps:get(embedding, EpisodeData, undefined)
    },

    Value = episode_to_map(Episode),
    StoreOpts = case Episode#episode.embedding of
        undefined -> #{};
        Emb -> #{embedding => Emb}
    end,

    beamai_memory:put(Memory, Namespace, EpisodeId, Value, StoreOpts).

%% @doc 更新对话片段
-spec update_episode(memory(), user_id(), episode_id(), map()) ->
    {ok, memory()} | {error, term()}.
update_episode(Memory, UserId, EpisodeId, Updates) ->
    case get_episode(Memory, UserId, EpisodeId) of
        {ok, Episode} ->
            Timestamp = beamai_memory_utils:current_timestamp(),
            UpdatedEpisode = apply_episode_updates(Episode, Updates, Timestamp),
            Namespace = get_episode_namespace(UserId),
            Value = episode_to_map(UpdatedEpisode),
            StoreOpts = case UpdatedEpisode#episode.embedding of
                undefined -> #{};
                Emb -> #{embedding => Emb}
            end,
            beamai_memory:put(Memory, Namespace, EpisodeId, Value, StoreOpts);
        {error, _} = Error ->
            Error
    end.

%% @doc 获取对话片段
-spec get_episode(memory(), user_id(), episode_id()) ->
    {ok, #episode{}} | {error, not_found | term()}.
get_episode(Memory, UserId, EpisodeId) ->
    Namespace = get_episode_namespace(UserId),
    case beamai_memory:get(Memory, Namespace, EpisodeId) of
        {ok, #store_item{value = Value, embedding = Emb}} ->
            E = map_to_episode(Value),
            {ok, E#episode{embedding = Emb}};
        {error, _} = Error ->
            Error
    end.

%% @doc 查找对话片段
%%
%% Opts 支持：
%% - outcome: 按状态过滤
%% - topic: 按话题过滤
%% - since: 开始时间之后
%% - until: 结束时间之前
%% - limit: 返回数量限制
-spec find_episodes(memory(), user_id(), map()) ->
    {ok, [#episode{}]} | {error, term()}.
find_episodes(Memory, UserId, Opts) ->
    Namespace = get_episode_namespace(UserId),
    SearchOpts = build_episode_filter(Opts),
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Episodes = [begin
                E = map_to_episode(Item#store_item.value),
                E#episode{embedding = Item#store_item.embedding}
            end || #search_result{item = Item} <- Results],
            %% 应用时间过滤（Store 可能不支持）
            Filtered = filter_episodes_by_time(Episodes, Opts),
            {ok, Filtered};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取所有对话片段
-spec find_episodes(memory(), user_id()) ->
    {ok, [#episode{}]} | {error, term()}.
find_episodes(Memory, UserId) ->
    find_episodes(Memory, UserId, #{}).

%% @doc 完成对话片段
-spec complete_episode(memory(), user_id(), episode_id()) ->
    {ok, memory()} | {error, term()}.
complete_episode(Memory, UserId, EpisodeId) ->
    complete_episode(Memory, UserId, EpisodeId, #{}).

-spec complete_episode(memory(), user_id(), episode_id(), map()) ->
    {ok, memory()} | {error, term()}.
complete_episode(Memory, UserId, EpisodeId, FinalData) ->
    Timestamp = beamai_memory_utils:current_timestamp(),
    Updates = FinalData#{
        outcome => ?OUTCOME_COMPLETED,
        ended_at => Timestamp
    },
    update_episode(Memory, UserId, EpisodeId, Updates).

%% @doc 删除对话片段
-spec delete_episode(memory(), user_id(), episode_id()) ->
    {ok, memory()} | {error, term()}.
delete_episode(Memory, UserId, EpisodeId) ->
    Namespace = get_episode_namespace(UserId),
    beamai_memory:delete(Memory, Namespace, EpisodeId).

%%====================================================================
%% 命名空间工具函数
%%====================================================================

%% @doc 获取用户对话片段命名空间
-spec get_episode_namespace(user_id()) -> [binary()].
get_episode_namespace(UserId) ->
    [?NS_EPISODIC, UserId, ?NS_EPISODES].

%%====================================================================
%% 内部函数 - 记录与 Map 转换
%%====================================================================

%% @doc Episode 记录转 Map
-spec episode_to_map(#episode{}) -> map().
episode_to_map(#episode{} = E) ->
    #{
        <<"id">> => E#episode.id,
        <<"thread_id">> => E#episode.thread_id,
        <<"title">> => E#episode.title,
        <<"summary">> => E#episode.summary,
        <<"participants">> => E#episode.participants,
        <<"message_count">> => E#episode.message_count,
        <<"topics">> => E#episode.topics,
        <<"sentiment">> => beamai_memory_types:sentiment_to_binary(E#episode.sentiment),
        <<"outcome">> => beamai_memory_types:outcome_to_binary(E#episode.outcome),
        <<"importance">> => E#episode.importance,
        <<"started_at">> => E#episode.started_at,
        <<"ended_at">> => E#episode.ended_at,
        <<"created_at">> => E#episode.created_at,
        <<"updated_at">> => E#episode.updated_at
    }.

%% @doc Map 转 Episode 记录
-spec map_to_episode(map()) -> #episode{}.
map_to_episode(M) ->
    SentimentBin = maps:get(<<"sentiment">>, M, undefined),
    OutcomeBin = maps:get(<<"outcome">>, M, <<"ongoing">>),
    #episode{
        id = maps:get(<<"id">>, M),
        thread_id = maps:get(<<"thread_id">>, M),
        title = maps:get(<<"title">>, M, undefined),
        summary = maps:get(<<"summary">>, M, undefined),
        participants = maps:get(<<"participants">>, M, []),
        message_count = maps:get(<<"message_count">>, M, 0),
        topics = maps:get(<<"topics">>, M, []),
        sentiment = beamai_memory_types:binary_to_sentiment(SentimentBin),
        outcome = beamai_memory_types:binary_to_outcome(OutcomeBin),
        importance = maps:get(<<"importance">>, M, ?DEFAULT_IMPORTANCE),
        started_at = maps:get(<<"started_at">>, M),
        ended_at = maps:get(<<"ended_at">>, M, undefined),
        created_at = maps:get(<<"created_at">>, M),
        updated_at = maps:get(<<"updated_at">>, M),
        embedding = undefined
    }.

%%====================================================================
%% 内部函数 - 更新应用
%%====================================================================

%% @private 应用 Episode 更新
-spec apply_episode_updates(#episode{}, map(), integer()) -> #episode{}.
apply_episode_updates(Episode, Updates, Timestamp) ->
    Episode1 = maps:fold(fun
        (title, V, E) -> E#episode{title = V};
        (summary, V, E) -> E#episode{summary = V};
        (topics, V, E) -> E#episode{topics = V};
        (sentiment, V, E) -> E#episode{sentiment = V};
        (outcome, V, E) -> E#episode{outcome = V};
        (importance, V, E) -> E#episode{importance = V};
        (message_count, V, E) -> E#episode{message_count = V};
        (ended_at, V, E) -> E#episode{ended_at = V};
        (embedding, V, E) -> E#episode{embedding = V};
        (_, _, E) -> E
    end, Episode, Updates),
    Episode1#episode{updated_at = Timestamp}.

%%====================================================================
%% 内部函数 - 搜索过滤
%%====================================================================

%% @private 构建 Episode 搜索过滤条件
-spec build_episode_filter(map()) -> map().
build_episode_filter(Opts) ->
    FilterSpecs = [
        {outcome, fun atom_to_binary/1}
    ],
    beamai_memory_utils:build_search_opts(Opts, FilterSpecs).

%% @private 按时间过滤 Episodes
-spec filter_episodes_by_time([#episode{}], map()) -> [#episode{}].
filter_episodes_by_time(Episodes, Opts) ->
    Since = maps:get(since, Opts, 0),
    Until = maps:get(until, Opts, beamai_memory_utils:current_timestamp()),
    Topic = maps:get(topic, Opts, undefined),

    lists:filter(fun(E) ->
        TimeOk = E#episode.started_at >= Since andalso E#episode.started_at =< Until,
        TopicOk = case Topic of
            undefined -> true;
            T -> lists:member(T, E#episode.topics)
        end,
        TimeOk andalso TopicOk
    end, Episodes).
