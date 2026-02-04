%%%-------------------------------------------------------------------
%%% @doc Agent Semantic Memory - 语义记忆模块
%%%
%%% 语义记忆存储事实性知识，不依赖于特定的时间或情境。
%%% 包括三类数据：
%%% - 用户偏好 (Preferences)
%%% - 实体信息 (Entities)
%%% - 知识片段 (Knowledge)
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 Memory 实例
%%% {ok, Mem} = beamai_memory:new(#{store => #{backend => ets}}),
%%%
%%% %% 设置用户偏好
%%% {ok, Mem1} = beamai_semantic_memory:set_preference(Mem, UserId, <<"theme">>, #{
%%%     value => <<"dark">>,
%%%     source => explicit
%%% }),
%%%
%%% %% 获取偏好
%%% {ok, Pref} = beamai_semantic_memory:get_preference(Mem1, UserId, <<"theme">>),
%%%
%%% %% 存储实体
%%% {ok, Mem2} = beamai_semantic_memory:upsert_entity(Mem1, UserId, #{
%%%     id => <<"entity-1">>,
%%%     type => person,
%%%     name => <<"Alice">>,
%%%     attributes => #{<<"role">> => <<"developer">>}
%%% }),
%%%
%%% %% 添加知识
%%% {ok, Mem3} = beamai_semantic_memory:add_knowledge(Mem2, UserId, #{
%%%     type => fact,
%%%     subject => <<"Erlang">>,
%%%     content => <<"Erlang is a functional programming language">>
%%% }),
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_semantic_memory).

-include_lib("beamai_memory/include/beamai_store.hrl").
-include_lib("beamai_memory/include/beamai_semantic_memory.hrl").

%% 类型别名
-type memory() :: beamai_memory:memory().
-type user_id() :: binary().
-type preference_key() :: binary().
-type entity_id() :: binary().
-type knowledge_id() :: binary().

%% 类型导出
-export_type([user_id/0, preference_key/0, entity_id/0, knowledge_id/0]).

%% 偏好管理 API
-export([
    set_preference/4,
    set_preference/5,
    get_preference/3,
    get_preferences/2,
    get_preferences/3,
    delete_preference/3
]).

%% 实体管理 API
-export([
    upsert_entity/3,
    get_entity/3,
    find_entities/3,
    find_entities/2,
    delete_entity/3
]).

%% 知识管理 API
-export([
    add_knowledge/3,
    get_knowledge/3,
    query_knowledge/3,
    query_knowledge/2,
    delete_knowledge/3
]).

%% 共享知识 API
-export([
    add_shared_knowledge/2,
    query_shared_knowledge/1,
    query_shared_knowledge/2
]).

%% 工具函数
-export([
    get_preference_namespace/1,
    get_entity_namespace/1,
    get_knowledge_namespace/1,
    get_shared_knowledge_namespace/0
]).

%%====================================================================
%% 偏好管理 API
%%====================================================================

%% @doc 设置用户偏好
%%
%% Opts 支持（支持 atom 和 binary 键）：
%% - <<"value">> / value: 偏好值（必需）
%% - <<"source">> / source: 来源 (<<"explicit">> | <<"inferred">> | <<"default">>)
%% - <<"confidence">> / confidence: 置信度 (0.0 - 1.0)
%% - <<"source_thread">> / source_thread: 来源会话 ID
%% - <<"expires_at">> / expires_at: 过期时间
-spec set_preference(memory(), user_id(), preference_key(), map()) ->
    {ok, memory()} | {error, term()}.
set_preference(Memory, UserId, Key, Opts) ->
    set_preference(Memory, UserId, Key, Opts, #{}).

-spec set_preference(memory(), user_id(), preference_key(), map(), map()) ->
    {ok, memory()} | {error, term()}.
set_preference(Memory, UserId, Key, Opts, StoreOpts) ->
    Namespace = get_preference_namespace(UserId),
    Timestamp = beamai_memory_utils:current_timestamp(),

    %% 检查是否已存在
    CreatedAt = case beamai_memory:get(Memory, Namespace, Key) of
        {ok, #store_item{created_at = T}} -> T;
        {error, not_found} -> Timestamp
    end,

    %% 从 Opts 中提取值（支持 atom 和 binary 键）
    Value = get_opt(value, Opts),
    Confidence = get_opt(confidence, Opts, 1.0),
    SourceBin = get_opt(<<"source">>, Opts, <<"explicit">>),
    SourceThread = get_opt(<<"source_thread">>, Opts, undefined),
    ExpiresAt = get_opt(<<"expires_at">>, Opts, undefined),

    %% 安全转换 binary 到 atom
    Source = beamai_memory_types:binary_to_source(SourceBin),

    %% 构建偏好记录
    Preference = #preference{
        key = Key,
        value = Value,
        confidence = Confidence,
        source = Source,
        source_thread = SourceThread,
        created_at = CreatedAt,
        updated_at = Timestamp,
        expires_at = ExpiresAt
    },

    %% 转换为存储格式
    MapValue = preference_to_map(Preference),

    beamai_memory:put(Memory, Namespace, Key, MapValue, StoreOpts).

%% @doc 获取单个偏好
-spec get_preference(memory(), user_id(), preference_key()) ->
    {ok, #preference{}} | {error, not_found | term()}.
get_preference(Memory, UserId, Key) ->
    Namespace = get_preference_namespace(UserId),
    case beamai_memory:get(Memory, Namespace, Key) of
        {ok, #store_item{value = Value}} ->
            {ok, map_to_preference(Key, Value)};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取用户的所有偏好
-spec get_preferences(memory(), user_id()) ->
    {ok, [#preference{}]} | {error, term()}.
get_preferences(Memory, UserId) ->
    get_preferences(Memory, UserId, #{}).

-spec get_preferences(memory(), user_id(), map()) ->
    {ok, [#preference{}]} | {error, term()}.
get_preferences(Memory, UserId, Opts) ->
    Namespace = get_preference_namespace(UserId),
    case beamai_memory:search(Memory, Namespace, Opts) of
        {ok, Results} ->
            Preferences = [map_to_preference(
                Item#store_item.key,
                Item#store_item.value
            ) || #search_result{item = Item} <- Results],
            {ok, Preferences};
        {error, _} = Error ->
            Error
    end.

%% @doc 删除偏好
-spec delete_preference(memory(), user_id(), preference_key()) ->
    {ok, memory()} | {error, term()}.
delete_preference(Memory, UserId, Key) ->
    Namespace = get_preference_namespace(UserId),
    beamai_memory:delete(Memory, Namespace, Key).

%%====================================================================
%% 实体管理 API
%%====================================================================

%% @doc 创建或更新实体
%%
%% EntityData 支持（支持 atom 和 binary 键）：
%% - <<"id">> / id: 实体 ID（必需）
%% - <<"type">> / type: 实体类型 (<<"person">> | <<"organization">> | ...)
%% - <<"name">> / name: 实体名称（必需）
%% - <<"aliases">> / aliases: 别名列表
%% - <<"attributes">> / attributes: 属性 map
%% - <<"relations">> / relations: 关系列表
%% - <<"confidence">> / confidence: 置信度
%% - <<"source">> / source: 来源 (<<"explicit">> | <<"extracted">> | ...)
-spec upsert_entity(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
upsert_entity(Memory, UserId, EntityData) ->
    Namespace = get_entity_namespace(UserId),
    EntityId = get_opt(<<"id">>, EntityData),
    Timestamp = beamai_memory_utils:current_timestamp(),

    %% 检查是否已存在
    CreatedAt = case beamai_memory:get(Memory, Namespace, EntityId) of
        {ok, #store_item{created_at = T}} -> T;
        {error, not_found} -> Timestamp
    end,

    %% 从 EntityData 中提取值（支持 atom 和 binary 键）
    TypeBin = get_opt(<<"type">>, EntityData, <<"custom">>),
    SourceBin = get_opt(<<"source">>, EntityData, <<"explicit">>),

    %% 安全转换 binary 到 atom
    Type = beamai_memory_types:binary_to_entity_type(TypeBin),
    Source = beamai_memory_types:binary_to_source(SourceBin),

    %% 构建实体记录
    Entity = #entity{
        id = EntityId,
        type = Type,
        name = get_opt(<<"name">>, EntityData),
        aliases = get_opt(<<"aliases">>, EntityData, []),
        attributes = get_opt(<<"attributes">>, EntityData, #{}),
        relations = get_opt(<<"relations">>, EntityData, []),
        confidence = get_opt(<<"confidence">>, EntityData, 1.0),
        source = Source,
        created_at = CreatedAt,
        updated_at = Timestamp
    },

    %% 转换为存储格式
    Value = entity_to_map(Entity),

    beamai_memory:put(Memory, Namespace, EntityId, Value, #{}).

%% @doc 获取单个实体
-spec get_entity(memory(), user_id(), entity_id()) ->
    {ok, #entity{}} | {error, not_found | term()}.
get_entity(Memory, UserId, EntityId) ->
    Namespace = get_entity_namespace(UserId),
    case beamai_memory:get(Memory, Namespace, EntityId) of
        {ok, #store_item{value = Value}} ->
            {ok, map_to_entity(Value)};
        {error, _} = Error ->
            Error
    end.

%% @doc 查找实体
%%
%% Opts 支持：
%% - type: 按类型过滤
%% - name: 按名称过滤（精确匹配）
%% - limit: 返回数量限制
-spec find_entities(memory(), user_id(), map()) ->
    {ok, [#entity{}]} | {error, term()}.
find_entities(Memory, UserId, Opts) ->
    Namespace = get_entity_namespace(UserId),
    SearchOpts = build_entity_filter(Opts),
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Entities = [map_to_entity(Item#store_item.value)
                        || #search_result{item = Item} <- Results],
            {ok, Entities};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取用户的所有实体
-spec find_entities(memory(), user_id()) ->
    {ok, [#entity{}]} | {error, term()}.
find_entities(Memory, UserId) ->
    find_entities(Memory, UserId, #{}).

%% @doc 删除实体
-spec delete_entity(memory(), user_id(), entity_id()) ->
    {ok, memory()} | {error, term()}.
delete_entity(Memory, UserId, EntityId) ->
    Namespace = get_entity_namespace(UserId),
    beamai_memory:delete(Memory, Namespace, EntityId).

%%====================================================================
%% 知识管理 API
%%====================================================================

%% @doc 添加知识
%%
%% KnowledgeData 支持（支持 atom 和 binary 键）：
%% - <<"id">> / id: 知识 ID（可选，自动生成）
%% - <<"type">> / type: 知识类型 (<<"fact">> | <<"rule">> | ...)
%% - <<"subject">> / subject: 主题（必需）
%% - <<"content">> / content: 内容（必需）
%% - <<"tags">> / tags: 标签列表
%% - <<"related_entities">> / related_entities: 关联实体 ID 列表
%% - <<"confidence">> / confidence: 置信度
%% - <<"source">> / source: 来源 (<<"explicit">> | ...)
%% - <<"source_ref">> / source_ref: 来源引用
%% - <<"embedding">> / embedding: 向量嵌入
-spec add_knowledge(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
add_knowledge(Memory, UserId, KnowledgeData) ->
    Namespace = get_knowledge_namespace(UserId),
    KnowledgeId = get_opt(<<"id">>, KnowledgeData, beamai_id:gen_id(<<"k">>)),
    Timestamp = beamai_memory_utils:current_timestamp(),

    %% 从 KnowledgeData 中提取值（支持 atom 和 binary 键）
    TypeBin = get_opt(<<"type">>, KnowledgeData, <<"fact">>),
    SourceBin = get_opt(<<"source">>, KnowledgeData, <<"explicit">>),

    %% 安全转换 binary 到 atom
    Type = beamai_memory_types:binary_to_knowledge_type(TypeBin),
    Source = beamai_memory_types:binary_to_source(SourceBin),

    %% 构建知识记录
    Knowledge = #knowledge{
        id = KnowledgeId,
        type = Type,
        subject = get_opt(<<"subject">>, KnowledgeData),
        content = get_opt(<<"content">>, KnowledgeData),
        tags = get_opt(<<"tags">>, KnowledgeData, []),
        related_entities = get_opt(<<"related_entities">>, KnowledgeData, []),
        confidence = get_opt(<<"confidence">>, KnowledgeData, 1.0),
        source = Source,
        source_ref = get_opt(<<"source_ref">>, KnowledgeData, undefined),
        embedding = get_opt(<<"embedding">>, KnowledgeData, undefined),
        created_at = Timestamp,
        updated_at = Timestamp,
        expires_at = get_opt(<<"expires_at">>, KnowledgeData, undefined)
    },

    %% 转换为存储格式
    Value = knowledge_to_map(Knowledge),

    %% 如果有 embedding，添加到 put 选项
    StoreOpts = case Knowledge#knowledge.embedding of
        undefined -> #{};
        Emb -> #{embedding => Emb}
    end,

    beamai_memory:put(Memory, Namespace, KnowledgeId, Value, StoreOpts).

%% @doc 获取单个知识
-spec get_knowledge(memory(), user_id(), knowledge_id()) ->
    {ok, #knowledge{}} | {error, not_found | term()}.
get_knowledge(Memory, UserId, KnowledgeId) ->
    Namespace = get_knowledge_namespace(UserId),
    case beamai_memory:get(Memory, Namespace, KnowledgeId) of
        {ok, #store_item{value = Value, embedding = Emb}} ->
            K = map_to_knowledge(Value),
            {ok, K#knowledge{embedding = Emb}};
        {error, _} = Error ->
            Error
    end.

%% @doc 查询知识
%%
%% Opts 支持：
%% - type: 按类型过滤
%% - subject: 按主题过滤
%% - tags: 按标签过滤（任一匹配）
%% - query: 语义搜索查询（需要后端支持）
%% - limit: 返回数量限制
-spec query_knowledge(memory(), user_id(), map()) ->
    {ok, [#knowledge{}]} | {error, term()}.
query_knowledge(Memory, UserId, Opts) ->
    Namespace = get_knowledge_namespace(UserId),
    SearchOpts = build_knowledge_filter(Opts),
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Knowledges = [begin
                K = map_to_knowledge(Item#store_item.value),
                K#knowledge{embedding = Item#store_item.embedding}
            end || #search_result{item = Item} <- Results],
            {ok, Knowledges};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取用户的所有知识
-spec query_knowledge(memory(), user_id()) ->
    {ok, [#knowledge{}]} | {error, term()}.
query_knowledge(Memory, UserId) ->
    query_knowledge(Memory, UserId, #{}).

%% @doc 删除知识
-spec delete_knowledge(memory(), user_id(), knowledge_id()) ->
    {ok, memory()} | {error, term()}.
delete_knowledge(Memory, UserId, KnowledgeId) ->
    Namespace = get_knowledge_namespace(UserId),
    beamai_memory:delete(Memory, Namespace, KnowledgeId).

%%====================================================================
%% 共享知识 API
%%====================================================================

%% @doc 添加共享知识
-spec add_shared_knowledge(memory(), map()) ->
    {ok, memory()} | {error, term()}.
add_shared_knowledge(Memory, KnowledgeData) ->
    Namespace = get_shared_knowledge_namespace(),
    KnowledgeId = maps:get(id, KnowledgeData, beamai_id:gen_id(<<"k">>)),
    Timestamp = beamai_memory_utils:current_timestamp(),

    Knowledge = #knowledge{
        id = KnowledgeId,
        type = maps:get(type, KnowledgeData, fact),
        subject = maps:get(subject, KnowledgeData),
        content = maps:get(content, KnowledgeData),
        tags = maps:get(tags, KnowledgeData, []),
        related_entities = maps:get(related_entities, KnowledgeData, []),
        confidence = maps:get(confidence, KnowledgeData, 1.0),
        source = maps:get(source, KnowledgeData, ?SOURCE_EXPLICIT),
        source_ref = maps:get(source_ref, KnowledgeData, undefined),
        embedding = maps:get(embedding, KnowledgeData, undefined),
        created_at = Timestamp,
        updated_at = Timestamp,
        expires_at = maps:get(expires_at, KnowledgeData, undefined)
    },

    Value = knowledge_to_map(Knowledge),
    StoreOpts = case Knowledge#knowledge.embedding of
        undefined -> #{};
        Emb -> #{embedding => Emb}
    end,

    beamai_memory:put(Memory, Namespace, KnowledgeId, Value, StoreOpts).

%% @doc 查询共享知识
-spec query_shared_knowledge(memory()) ->
    {ok, [#knowledge{}]} | {error, term()}.
query_shared_knowledge(Memory) ->
    query_shared_knowledge(Memory, #{}).

-spec query_shared_knowledge(memory(), map()) ->
    {ok, [#knowledge{}]} | {error, term()}.
query_shared_knowledge(Memory, Opts) ->
    Namespace = get_shared_knowledge_namespace(),
    SearchOpts = build_knowledge_filter(Opts),
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Knowledges = [begin
                K = map_to_knowledge(Item#store_item.value),
                K#knowledge{embedding = Item#store_item.embedding}
            end || #search_result{item = Item} <- Results],
            {ok, Knowledges};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 命名空间工具函数
%%====================================================================

%% @doc 获取用户偏好命名空间
-spec get_preference_namespace(user_id()) -> [binary()].
get_preference_namespace(UserId) ->
    [?NS_SEMANTIC, UserId, ?NS_PREFERENCES].

%% @doc 获取用户实体命名空间
-spec get_entity_namespace(user_id()) -> [binary()].
get_entity_namespace(UserId) ->
    [?NS_SEMANTIC, UserId, ?NS_ENTITIES].

%% @doc 获取用户知识命名空间
-spec get_knowledge_namespace(user_id()) -> [binary()].
get_knowledge_namespace(UserId) ->
    [?NS_SEMANTIC, UserId, ?NS_KNOWLEDGE].

%% @doc 获取共享知识命名空间
-spec get_shared_knowledge_namespace() -> [binary()].
get_shared_knowledge_namespace() ->
    [?NS_SEMANTIC, ?NS_SEM_SHARED, ?NS_KNOWLEDGE].

%%====================================================================
%% 内部函数 - 记录与 Map 转换
%%====================================================================

%% @private 偏好记录转 Map
-spec preference_to_map(#preference{}) -> map().
preference_to_map(#preference{} = P) ->
    #{
        <<"key">> => P#preference.key,
        <<"value">> => P#preference.value,
        <<"confidence">> => P#preference.confidence,
        <<"source">> => beamai_memory_types:source_to_binary(P#preference.source),
        <<"source_thread">> => P#preference.source_thread,
        <<"created_at">> => P#preference.created_at,
        <<"updated_at">> => P#preference.updated_at,
        <<"expires_at">> => P#preference.expires_at
    }.

%% @private Map 转偏好记录
-spec map_to_preference(binary(), map()) -> #preference{}.
map_to_preference(Key, M) ->
    SourceBin = maps:get(<<"source">>, M, <<"explicit">>),
    #preference{
        key = Key,
        value = maps:get(<<"value">>, M),
        confidence = maps:get(<<"confidence">>, M, 1.0),
        source = beamai_memory_types:binary_to_source(SourceBin),
        source_thread = maps:get(<<"source_thread">>, M, undefined),
        created_at = maps:get(<<"created_at">>, M),
        updated_at = maps:get(<<"updated_at">>, M),
        expires_at = maps:get(<<"expires_at">>, M, undefined)
    }.

%% @private 实体记录转 Map
-spec entity_to_map(#entity{}) -> map().
entity_to_map(#entity{} = E) ->
    #{
        <<"id">> => E#entity.id,
        <<"type">> => beamai_memory_types:entity_type_to_binary(E#entity.type),
        <<"name">> => E#entity.name,
        <<"aliases">> => E#entity.aliases,
        <<"attributes">> => E#entity.attributes,
        <<"relations">> => encode_relations(E#entity.relations),
        <<"confidence">> => E#entity.confidence,
        <<"source">> => beamai_memory_types:source_to_binary(E#entity.source),
        <<"created_at">> => E#entity.created_at,
        <<"updated_at">> => E#entity.updated_at
    }.

%% @private Map 转实体记录
-spec map_to_entity(map()) -> #entity{}.
map_to_entity(M) ->
    TypeBin = maps:get(<<"type">>, M, <<"custom">>),
    SourceBin = maps:get(<<"source">>, M, <<"explicit">>),
    #entity{
        id = maps:get(<<"id">>, M),
        type = beamai_memory_types:binary_to_entity_type(TypeBin),
        name = maps:get(<<"name">>, M),
        aliases = maps:get(<<"aliases">>, M, []),
        attributes = maps:get(<<"attributes">>, M, #{}),
        relations = decode_relations(maps:get(<<"relations">>, M, [])),
        confidence = maps:get(<<"confidence">>, M, 1.0),
        source = beamai_memory_types:binary_to_source(SourceBin),
        created_at = maps:get(<<"created_at">>, M),
        updated_at = maps:get(<<"updated_at">>, M)
    }.

%% @private 知识记录转 Map
-spec knowledge_to_map(#knowledge{}) -> map().
knowledge_to_map(#knowledge{} = K) ->
    #{
        <<"id">> => K#knowledge.id,
        <<"type">> => beamai_memory_types:knowledge_type_to_binary(K#knowledge.type),
        <<"subject">> => K#knowledge.subject,
        <<"content">> => K#knowledge.content,
        <<"tags">> => K#knowledge.tags,
        <<"related_entities">> => K#knowledge.related_entities,
        <<"confidence">> => K#knowledge.confidence,
        <<"source">> => beamai_memory_types:source_to_binary(K#knowledge.source),
        <<"source_ref">> => K#knowledge.source_ref,
        <<"created_at">> => K#knowledge.created_at,
        <<"updated_at">> => K#knowledge.updated_at,
        <<"expires_at">> => K#knowledge.expires_at
    }.

%% @private Map 转知识记录
-spec map_to_knowledge(map()) -> #knowledge{}.
map_to_knowledge(M) ->
    TypeBin = maps:get(<<"type">>, M, <<"fact">>),
    SourceBin = maps:get(<<"source">>, M, <<"explicit">>),
    #knowledge{
        id = maps:get(<<"id">>, M),
        type = beamai_memory_types:binary_to_knowledge_type(TypeBin),
        subject = maps:get(<<"subject">>, M),
        content = maps:get(<<"content">>, M),
        tags = maps:get(<<"tags">>, M, []),
        related_entities = maps:get(<<"related_entities">>, M, []),
        confidence = maps:get(<<"confidence">>, M, 1.0),
        source = beamai_memory_types:binary_to_source(SourceBin),
        source_ref = maps:get(<<"source_ref">>, M, undefined),
        embedding = undefined,  %% embedding 单独存储
        created_at = maps:get(<<"created_at">>, M),
        updated_at = maps:get(<<"updated_at">>, M),
        expires_at = maps:get(<<"expires_at">>, M, undefined)
    }.

%%====================================================================
%% 内部函数 - 搜索过滤
%%====================================================================

%% @private 构建实体搜索过滤条件
%%
%% 使用通用搜索选项构建器简化代码
-spec build_entity_filter(map()) -> map().
build_entity_filter(Opts) ->
    FilterSpecs = [
        {type, fun atom_to_binary/1},
        {name, direct}
    ],
    beamai_memory_utils:build_search_opts(Opts, FilterSpecs).

%% @private 构建知识搜索过滤条件
%%
%% 使用通用搜索选项构建器简化代码
-spec build_knowledge_filter(map()) -> map().
build_knowledge_filter(Opts) ->
    FilterSpecs = [
        {type, fun atom_to_binary/1},
        {subject, direct}
    ],
    beamai_memory_utils:build_search_opts(Opts, FilterSpecs).

%%====================================================================
%% 内部函数 - 工具
%%====================================================================

%% @private 编码关系列表
-spec encode_relations([{binary(), binary()}]) -> [map()].
encode_relations(Relations) ->
    [#{<<"type">> => Type, <<"target">> => Target}
     || {Type, Target} <- Relations].

%% @private 解码关系列表
-spec decode_relations([map()]) -> [{binary(), binary()}].
decode_relations(Relations) ->
    [{maps:get(<<"type">>, R), maps:get(<<"target">>, R)}
     || R <- Relations].

%% @private 从 Map 中获取值（支持 atom 和 binary 键）
%%
%% 优先使用 binary 键（外部 API），fallback 到 atom 键（向后兼容）。
-spec get_opt(binary() | atom(), map()) -> term().
get_opt(Key, Map) when is_atom(Key) ->
    %% 尝试 atom 键
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error ->
            %% Fallback 到 binary 键
            BinKey = atom_to_binary(Key, utf8),
            maps:get(BinKey, Map)
    end;
get_opt(Key, Map) when is_binary(Key) ->
    %% 尝试 binary 键
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error ->
            %% Fallback 到 atom 键
            try
                AtomKey = binary_to_existing_atom(Key, utf8),
                maps:get(AtomKey, Map)
            catch
                error:badarg ->
                    error({key_not_found, Key})
            end
    end.

%% @private 从 Map 中获取值，支持默认值（支持 atom 和 binary 键）
-spec get_opt(binary() | atom(), map(), term()) -> term().
get_opt(Key, Map, Default) ->
    try
        get_opt(Key, Map)
    catch
        error:{key_not_found, _} ->
            Default
    end.
