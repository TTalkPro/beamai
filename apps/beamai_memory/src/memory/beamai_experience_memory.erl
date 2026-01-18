%%%-------------------------------------------------------------------
%%% @doc Agent Experience Memory - 经验总结记忆模块
%%%
%%% 管理 Agent 的经验总结记忆，包括：
%%% - 经验添加和获取
%%% - 经验验证和更新
%%% - 经验搜索和删除
%%%
%%% 从 beamai_episodic_memory 拆分出来的独立模块。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_experience_memory).

-include_lib("beamai_memory/include/beamai_store.hrl").
-include_lib("beamai_memory/include/beamai_episodic_memory.hrl").

%% 类型别名
-type memory() :: beamai_memory:memory().
-type user_id() :: binary().
-type experience_id() :: binary().

%% 类型导出
-export_type([user_id/0, experience_id/0]).

%% 经验总结 API
-export([
    add_experience/3,
    get_experience/3,
    find_experiences/3,
    find_experiences/2,
    validate_experience/3,
    update_experience/4,
    delete_experience/3
]).

%% 工具函数
-export([
    get_experience_namespace/1
]).

%% 内部函数（供门面模块使用）
-export([
    experience_to_map/1,
    map_to_experience/1
]).

%%====================================================================
%% 经验总结 API
%%====================================================================

%% @doc 添加经验
%%
%% ExperienceData 支持：
%% - id: 经验 ID（可选，自动生成）
%% - type: 经验类型 (lesson | pattern | preference | insight)
%% - title: 经验标题（必需）
%% - content: 经验内容（必需）
%% - source_episodes: 来源对话片段列表
%% - topics: 相关主题
%% - confidence: 置信度
%% - embedding: 向量嵌入
-spec add_experience(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
add_experience(Memory, UserId, ExperienceData) ->
    Namespace = get_experience_namespace(UserId),
    ExperienceId = maps:get(id, ExperienceData, beamai_memory_utils:generate_id(<<"exp">>)),
    Timestamp = beamai_memory_utils:current_timestamp(),

    Experience = #experience{
        id = ExperienceId,
        type = maps:get(type, ExperienceData, ?EXP_LESSON),
        title = maps:get(title, ExperienceData),
        content = maps:get(content, ExperienceData),
        source_episodes = maps:get(source_episodes, ExperienceData, []),
        topics = maps:get(topics, ExperienceData, []),
        confidence = maps:get(confidence, ExperienceData, ?DEFAULT_CONFIDENCE),
        validation_count = 1,
        last_validated_at = Timestamp,
        created_at = Timestamp,
        updated_at = Timestamp,
        expires_at = maps:get(expires_at, ExperienceData, undefined),
        embedding = maps:get(embedding, ExperienceData, undefined)
    },

    Value = experience_to_map(Experience),
    StoreOpts = case Experience#experience.embedding of
        undefined -> #{};
        Emb -> #{embedding => Emb}
    end,

    beamai_memory:put(Memory, Namespace, ExperienceId, Value, StoreOpts).

%% @doc 获取经验
-spec get_experience(memory(), user_id(), experience_id()) ->
    {ok, #experience{}} | {error, not_found | term()}.
get_experience(Memory, UserId, ExperienceId) ->
    Namespace = get_experience_namespace(UserId),
    case beamai_memory:get(Memory, Namespace, ExperienceId) of
        {ok, #store_item{value = Value, embedding = Emb}} ->
            E = map_to_experience(Value),
            {ok, E#experience{embedding = Emb}};
        {error, _} = Error ->
            Error
    end.

%% @doc 查找经验
-spec find_experiences(memory(), user_id(), map()) ->
    {ok, [#experience{}]} | {error, term()}.
find_experiences(Memory, UserId, Opts) ->
    Namespace = get_experience_namespace(UserId),
    SearchOpts = build_experience_filter(Opts),
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Experiences = [begin
                E = map_to_experience(Item#store_item.value),
                E#experience{embedding = Item#store_item.embedding}
            end || #search_result{item = Item} <- Results],
            {ok, Experiences};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取所有经验
-spec find_experiences(memory(), user_id()) ->
    {ok, [#experience{}]} | {error, term()}.
find_experiences(Memory, UserId) ->
    find_experiences(Memory, UserId, #{}).

%% @doc 验证经验（增加置信度和验证次数）
-spec validate_experience(memory(), user_id(), experience_id()) ->
    {ok, memory()} | {error, term()}.
validate_experience(Memory, UserId, ExperienceId) ->
    case get_experience(Memory, UserId, ExperienceId) of
        {ok, Experience} ->
            Timestamp = beamai_memory_utils:current_timestamp(),
            NewCount = Experience#experience.validation_count + 1,
            %% 置信度随验证次数增加而增加（使用对数增长）
            NewConfidence = min(1.0, 0.5 + 0.1 * math:log(NewCount + 1)),
            Updates = #{
                validation_count => NewCount,
                confidence => NewConfidence,
                last_validated_at => Timestamp
            },
            update_experience(Memory, UserId, ExperienceId, Updates);
        {error, _} = Error ->
            Error
    end.

%% @doc 更新经验
-spec update_experience(memory(), user_id(), experience_id(), map()) ->
    {ok, memory()} | {error, term()}.
update_experience(Memory, UserId, ExperienceId, Updates) ->
    case get_experience(Memory, UserId, ExperienceId) of
        {ok, Experience} ->
            Timestamp = beamai_memory_utils:current_timestamp(),
            UpdatedExperience = apply_experience_updates(Experience, Updates, Timestamp),
            Namespace = get_experience_namespace(UserId),
            Value = experience_to_map(UpdatedExperience),
            StoreOpts = case UpdatedExperience#experience.embedding of
                undefined -> #{};
                Emb -> #{embedding => Emb}
            end,
            beamai_memory:put(Memory, Namespace, ExperienceId, Value, StoreOpts);
        {error, _} = Error ->
            Error
    end.

%% @doc 删除经验
-spec delete_experience(memory(), user_id(), experience_id()) ->
    {ok, memory()} | {error, term()}.
delete_experience(Memory, UserId, ExperienceId) ->
    Namespace = get_experience_namespace(UserId),
    beamai_memory:delete(Memory, Namespace, ExperienceId).

%%====================================================================
%% 命名空间工具函数
%%====================================================================

%% @doc 获取用户经验命名空间
-spec get_experience_namespace(user_id()) -> [binary()].
get_experience_namespace(UserId) ->
    [?NS_EPISODIC, UserId, ?NS_EXPERIENCES].

%%====================================================================
%% 内部函数 - 记录与 Map 转换
%%====================================================================

%% @doc Experience 记录转 Map
-spec experience_to_map(#experience{}) -> map().
experience_to_map(#experience{} = E) ->
    #{
        <<"id">> => E#experience.id,
        <<"type">> => atom_to_binary(E#experience.type),
        <<"title">> => E#experience.title,
        <<"content">> => E#experience.content,
        <<"source_episodes">> => E#experience.source_episodes,
        <<"topics">> => E#experience.topics,
        <<"confidence">> => E#experience.confidence,
        <<"validation_count">> => E#experience.validation_count,
        <<"last_validated_at">> => E#experience.last_validated_at,
        <<"created_at">> => E#experience.created_at,
        <<"updated_at">> => E#experience.updated_at,
        <<"expires_at">> => E#experience.expires_at
    }.

%% @doc Map 转 Experience 记录
-spec map_to_experience(map()) -> #experience{}.
map_to_experience(M) ->
    #experience{
        id = maps:get(<<"id">>, M),
        type = beamai_memory_utils:safe_binary_to_atom(maps:get(<<"type">>, M, <<"lesson">>)),
        title = maps:get(<<"title">>, M),
        content = maps:get(<<"content">>, M),
        source_episodes = maps:get(<<"source_episodes">>, M, []),
        topics = maps:get(<<"topics">>, M, []),
        confidence = maps:get(<<"confidence">>, M, ?DEFAULT_CONFIDENCE),
        validation_count = maps:get(<<"validation_count">>, M, 1),
        last_validated_at = maps:get(<<"last_validated_at">>, M),
        created_at = maps:get(<<"created_at">>, M),
        updated_at = maps:get(<<"updated_at">>, M),
        expires_at = maps:get(<<"expires_at">>, M, undefined),
        embedding = undefined
    }.

%%====================================================================
%% 内部函数 - 更新应用
%%====================================================================

%% @private 应用 Experience 更新
-spec apply_experience_updates(#experience{}, map(), integer()) -> #experience{}.
apply_experience_updates(Experience, Updates, Timestamp) ->
    Exp1 = maps:fold(fun
        (title, V, E) -> E#experience{title = V};
        (content, V, E) -> E#experience{content = V};
        (topics, V, E) -> E#experience{topics = V};
        (confidence, V, E) -> E#experience{confidence = V};
        (validation_count, V, E) -> E#experience{validation_count = V};
        (last_validated_at, V, E) -> E#experience{last_validated_at = V};
        (expires_at, V, E) -> E#experience{expires_at = V};
        (embedding, V, E) -> E#experience{embedding = V};
        (source_episodes, V, E) -> E#experience{source_episodes = V};
        (_, _, E) -> E
    end, Experience, Updates),
    Exp1#experience{updated_at = Timestamp}.

%%====================================================================
%% 内部函数 - 搜索过滤
%%====================================================================

%% @private 构建 Experience 搜索过滤条件
-spec build_experience_filter(map()) -> map().
build_experience_filter(Opts) ->
    FilterSpecs = [
        {type, fun atom_to_binary/1}
    ],
    beamai_memory_utils:build_search_opts(Opts, FilterSpecs).
