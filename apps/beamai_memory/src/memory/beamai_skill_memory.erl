%%%-------------------------------------------------------------------
%%% @doc Agent Skill Memory - 技能记忆模块
%%%
%%% 管理 Agent 的技能记忆，包括：
%%% - 技能注册和获取
%%% - 技能使用记录和熟练度追踪
%%% - 共享技能管理
%%%
%%% 从 beamai_procedural_memory 拆分出来的独立模块。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_skill_memory).

-include_lib("beamai_memory/include/beamai_store.hrl").
-include_lib("beamai_memory/include/beamai_procedural_memory.hrl").

%% 类型别名
-type memory() :: beamai_memory:memory().
-type user_id() :: binary().
-type skill_id() :: binary().

%% 类型导出
-export_type([user_id/0, skill_id/0]).

%% 技能管理 API
-export([
    register_skill/3,
    get_skill/3,
    find_skills/3,
    find_skills/2,
    update_skill/4,
    record_skill_usage/4,
    deprecate_skill/3,
    delete_skill/3
]).

%% 共享技能 API
-export([
    register_shared_skill/2,
    find_shared_skills/2,
    find_shared_skills/1
]).

%% 高级查询 API
-export([
    get_proficient_skills/2
]).

%% 工具函数
-export([
    get_skill_namespace/1,
    get_shared_skill_namespace/0,
    calculate_proficiency/2
]).

%% 内部函数（供门面模块使用）
-export([
    skill_to_map/1,
    map_to_skill/1
]).

%%====================================================================
%% 技能管理 API
%%====================================================================

%% @doc 注册技能
%%
%% SkillData 支持：
%% - id: 技能 ID（可选，自动生成）
%% - name: 技能名称（必需）
%% - description: 技能描述（必需）
%% - category: 技能类别
%% - instructions: 执行步骤列表
%% - preconditions: 前置条件列表
%% - tools: 相关工具列表
%% - examples: 示例列表
%% - tags: 标签列表
%% - source: 来源 (learned | predefined | imported)
-spec register_skill(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
register_skill(Memory, UserId, SkillData) ->
    Namespace = get_skill_namespace(UserId),
    SkillId = maps:get(id, SkillData, beamai_memory_utils:generate_id(<<"skill">>)),
    Timestamp = beamai_memory_utils:current_timestamp(),

    Skill = #skill{
        id = SkillId,
        name = maps:get(name, SkillData),
        description = maps:get(description, SkillData),
        category = maps:get(category, SkillData, ?CAT_TOOL_USAGE),
        status = maps:get(status, SkillData, ?SKILL_LEARNING),
        proficiency = maps:get(proficiency, SkillData, ?DEFAULT_PROFICIENCY),
        usage_count = 0,
        success_count = 0,
        instructions = maps:get(instructions, SkillData, []),
        preconditions = maps:get(preconditions, SkillData, []),
        tools = maps:get(tools, SkillData, []),
        examples = maps:get(examples, SkillData, []),
        tags = maps:get(tags, SkillData, []),
        source = maps:get(source, SkillData, ?SKILL_SOURCE_LEARNED),
        source_ref = maps:get(source_ref, SkillData, undefined),
        last_used_at = undefined,
        created_at = Timestamp,
        updated_at = Timestamp,
        embedding = maps:get(embedding, SkillData, undefined)
    },

    Value = skill_to_map(Skill),
    StoreOpts = case Skill#skill.embedding of
        undefined -> #{};
        Emb -> #{embedding => Emb}
    end,

    beamai_memory:put(Memory, Namespace, SkillId, Value, StoreOpts).

%% @doc 获取技能
-spec get_skill(memory(), user_id(), skill_id()) ->
    {ok, #skill{}} | {error, not_found | term()}.
get_skill(Memory, UserId, SkillId) ->
    Namespace = get_skill_namespace(UserId),
    case beamai_memory:get(Memory, Namespace, SkillId) of
        {ok, #store_item{value = Value, embedding = Emb}} ->
            S = map_to_skill(Value),
            {ok, S#skill{embedding = Emb}};
        {error, _} = Error ->
            Error
    end.

%% @doc 查找技能
%%
%% Opts 支持：
%% - category: 按类别过滤
%% - status: 按状态过滤
%% - min_proficiency: 最小熟练度
%% - query: 语义搜索查询
%% - limit: 返回数量限制
-spec find_skills(memory(), user_id(), map()) ->
    {ok, [#skill{}]} | {error, term()}.
find_skills(Memory, UserId, Opts) ->
    Namespace = get_skill_namespace(UserId),
    SearchOpts = build_skill_filter(Opts),
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Skills = [begin
                S = map_to_skill(Item#store_item.value),
                S#skill{embedding = Item#store_item.embedding}
            end || #search_result{item = Item} <- Results],
            %% 应用额外过滤（Store 可能不支持）
            Filtered = filter_skills(Skills, Opts),
            {ok, Filtered};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取所有技能
-spec find_skills(memory(), user_id()) ->
    {ok, [#skill{}]} | {error, term()}.
find_skills(Memory, UserId) ->
    find_skills(Memory, UserId, #{}).

%% @doc 更新技能
-spec update_skill(memory(), user_id(), skill_id(), map()) ->
    {ok, memory()} | {error, term()}.
update_skill(Memory, UserId, SkillId, Updates) ->
    case get_skill(Memory, UserId, SkillId) of
        {ok, Skill} ->
            Timestamp = beamai_memory_utils:current_timestamp(),
            UpdatedSkill = apply_skill_updates(Skill, Updates, Timestamp),
            Namespace = get_skill_namespace(UserId),
            Value = skill_to_map(UpdatedSkill),
            StoreOpts = case UpdatedSkill#skill.embedding of
                undefined -> #{};
                Emb -> #{embedding => Emb}
            end,
            beamai_memory:put(Memory, Namespace, SkillId, Value, StoreOpts);
        {error, _} = Error ->
            Error
    end.

%% @doc 记录技能使用
%%
%% Result 支持：
%% - success: 是否成功（必需）
%% - duration: 执行时长
%% - notes: 备注
-spec record_skill_usage(memory(), user_id(), skill_id(), map()) ->
    {ok, memory()} | {error, term()}.
record_skill_usage(Memory, UserId, SkillId, Result) ->
    case get_skill(Memory, UserId, SkillId) of
        {ok, Skill} ->
            Timestamp = beamai_memory_utils:current_timestamp(),
            Success = maps:get(success, Result, true),

            NewUsageCount = Skill#skill.usage_count + 1,
            NewSuccessCount = case Success of
                true -> Skill#skill.success_count + 1;
                false -> Skill#skill.success_count
            end,

            %% 计算新的熟练度
            NewProficiency = calculate_proficiency(NewSuccessCount, NewUsageCount),

            %% 根据熟练度更新状态
            NewStatus = proficiency_to_status(NewProficiency),

            Updates = #{
                usage_count => NewUsageCount,
                success_count => NewSuccessCount,
                proficiency => NewProficiency,
                status => NewStatus,
                last_used_at => Timestamp
            },

            update_skill(Memory, UserId, SkillId, Updates);
        {error, _} = Error ->
            Error
    end.

%% @doc 弃用技能
-spec deprecate_skill(memory(), user_id(), skill_id()) ->
    {ok, memory()} | {error, term()}.
deprecate_skill(Memory, UserId, SkillId) ->
    update_skill(Memory, UserId, SkillId, #{status => ?SKILL_DEPRECATED}).

%% @doc 删除技能
-spec delete_skill(memory(), user_id(), skill_id()) ->
    {ok, memory()} | {error, term()}.
delete_skill(Memory, UserId, SkillId) ->
    Namespace = get_skill_namespace(UserId),
    beamai_memory:delete(Memory, Namespace, SkillId).

%%====================================================================
%% 共享技能 API
%%====================================================================

%% @doc 注册共享技能
-spec register_shared_skill(memory(), map()) ->
    {ok, memory()} | {error, term()}.
register_shared_skill(Memory, SkillData) ->
    Namespace = get_shared_skill_namespace(),
    SkillId = maps:get(id, SkillData, beamai_memory_utils:generate_id(<<"skill">>)),
    Timestamp = beamai_memory_utils:current_timestamp(),

    Skill = #skill{
        id = SkillId,
        name = maps:get(name, SkillData),
        description = maps:get(description, SkillData),
        category = maps:get(category, SkillData, ?CAT_TOOL_USAGE),
        status = maps:get(status, SkillData, ?SKILL_PROFICIENT),
        proficiency = maps:get(proficiency, SkillData, 1.0),
        usage_count = 0,
        success_count = 0,
        instructions = maps:get(instructions, SkillData, []),
        preconditions = maps:get(preconditions, SkillData, []),
        tools = maps:get(tools, SkillData, []),
        examples = maps:get(examples, SkillData, []),
        tags = maps:get(tags, SkillData, []),
        source = ?SKILL_SOURCE_PREDEFINED,
        source_ref = maps:get(source_ref, SkillData, undefined),
        last_used_at = undefined,
        created_at = Timestamp,
        updated_at = Timestamp,
        embedding = maps:get(embedding, SkillData, undefined)
    },

    Value = skill_to_map(Skill),
    StoreOpts = case Skill#skill.embedding of
        undefined -> #{};
        Emb -> #{embedding => Emb}
    end,

    beamai_memory:put(Memory, Namespace, SkillId, Value, StoreOpts).

%% @doc 查找共享技能
-spec find_shared_skills(memory(), map()) ->
    {ok, [#skill{}]} | {error, term()}.
find_shared_skills(Memory, Opts) ->
    Namespace = get_shared_skill_namespace(),
    SearchOpts = build_skill_filter(Opts),
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Skills = [begin
                S = map_to_skill(Item#store_item.value),
                S#skill{embedding = Item#store_item.embedding}
            end || #search_result{item = Item} <- Results],
            {ok, Skills};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取所有共享技能
-spec find_shared_skills(memory()) ->
    {ok, [#skill{}]} | {error, term()}.
find_shared_skills(Memory) ->
    find_shared_skills(Memory, #{}).

%%====================================================================
%% 高级查询 API
%%====================================================================

%% @doc 获取熟练的技能
-spec get_proficient_skills(memory(), user_id()) ->
    {ok, [#skill{}]} | {error, term()}.
get_proficient_skills(Memory, UserId) ->
    find_skills(Memory, UserId, #{min_proficiency => ?PROFICIENCY_PROFICIENT}).

%%====================================================================
%% 命名空间工具函数
%%====================================================================

%% @doc 获取用户技能命名空间
-spec get_skill_namespace(user_id()) -> [binary()].
get_skill_namespace(UserId) ->
    [?NS_PROCEDURAL, UserId, ?NS_SKILLS].

%% @doc 获取共享技能命名空间
-spec get_shared_skill_namespace() -> [binary()].
get_shared_skill_namespace() ->
    [?NS_PROCEDURAL, ?NS_PROC_SHARED, ?NS_SKILLS].

%% @doc 计算熟练度
-spec calculate_proficiency(non_neg_integer(), non_neg_integer()) -> float().
calculate_proficiency(_, 0) -> ?DEFAULT_PROFICIENCY;
calculate_proficiency(SuccessCount, UsageCount) ->
    %% 基于成功率和使用次数计算熟练度
    %% 使用次数越多，熟练度越稳定
    SuccessRate = SuccessCount / UsageCount,
    %% 使用对数增长来考虑经验积累
    ExperienceFactor = min(1.0, math:log(UsageCount + 1) / math:log(100)),
    %% 熟练度 = 基础成功率 * 经验因子的调整
    BaseProf = ?DEFAULT_PROFICIENCY,
    SuccessRate * ExperienceFactor + BaseProf * (1 - ExperienceFactor).

%%====================================================================
%% 内部函数 - 记录与 Map 转换
%%====================================================================

%% @doc Skill 记录转 Map
-spec skill_to_map(#skill{}) -> map().
skill_to_map(#skill{} = S) ->
    #{
        <<"id">> => S#skill.id,
        <<"name">> => S#skill.name,
        <<"description">> => S#skill.description,
        <<"category">> => S#skill.category,
        <<"status">> => beamai_memory_types:skill_status_to_binary(S#skill.status),
        <<"proficiency">> => S#skill.proficiency,
        <<"usage_count">> => S#skill.usage_count,
        <<"success_count">> => S#skill.success_count,
        <<"instructions">> => S#skill.instructions,
        <<"preconditions">> => S#skill.preconditions,
        <<"tools">> => S#skill.tools,
        <<"examples">> => S#skill.examples,
        <<"tags">> => S#skill.tags,
        <<"source">> => beamai_memory_types:skill_source_to_binary(S#skill.source),
        <<"source_ref">> => S#skill.source_ref,
        <<"last_used_at">> => S#skill.last_used_at,
        <<"created_at">> => S#skill.created_at,
        <<"updated_at">> => S#skill.updated_at
    }.

%% @doc Map 转 Skill 记录
-spec map_to_skill(map()) -> #skill{}.
map_to_skill(M) ->
    StatusBin = maps:get(<<"status">>, M, <<"learning">>),
    SourceBin = maps:get(<<"source">>, M, <<"learned">>),
    #skill{
        id = maps:get(<<"id">>, M),
        name = maps:get(<<"name">>, M),
        description = maps:get(<<"description">>, M),
        category = maps:get(<<"category">>, M, ?CAT_TOOL_USAGE),
        status = beamai_memory_types:binary_to_skill_status(StatusBin),
        proficiency = maps:get(<<"proficiency">>, M, ?DEFAULT_PROFICIENCY),
        usage_count = maps:get(<<"usage_count">>, M, 0),
        success_count = maps:get(<<"success_count">>, M, 0),
        instructions = maps:get(<<"instructions">>, M, []),
        preconditions = maps:get(<<"preconditions">>, M, []),
        tools = maps:get(<<"tools">>, M, []),
        examples = maps:get(<<"examples">>, M, []),
        tags = maps:get(<<"tags">>, M, []),
        source = beamai_memory_types:binary_to_skill_source(SourceBin),
        source_ref = maps:get(<<"source_ref">>, M, undefined),
        last_used_at = maps:get(<<"last_used_at">>, M, undefined),
        created_at = maps:get(<<"created_at">>, M),
        updated_at = maps:get(<<"updated_at">>, M),
        embedding = undefined
    }.

%%====================================================================
%% 内部函数 - 更新应用
%%====================================================================

%% @private 应用 Skill 更新
-spec apply_skill_updates(#skill{}, map(), integer()) -> #skill{}.
apply_skill_updates(Skill, Updates, Timestamp) ->
    S1 = maps:fold(fun
        (name, V, S) -> S#skill{name = V};
        (description, V, S) -> S#skill{description = V};
        (category, V, S) -> S#skill{category = V};
        (status, V, S) -> S#skill{status = V};
        (proficiency, V, S) -> S#skill{proficiency = V};
        (usage_count, V, S) -> S#skill{usage_count = V};
        (success_count, V, S) -> S#skill{success_count = V};
        (instructions, V, S) -> S#skill{instructions = V};
        (preconditions, V, S) -> S#skill{preconditions = V};
        (tools, V, S) -> S#skill{tools = V};
        (examples, V, S) -> S#skill{examples = V};
        (tags, V, S) -> S#skill{tags = V};
        (last_used_at, V, S) -> S#skill{last_used_at = V};
        (embedding, V, S) -> S#skill{embedding = V};
        (_, _, S) -> S
    end, Skill, Updates),
    S1#skill{updated_at = Timestamp}.

%%====================================================================
%% 内部函数 - 搜索过滤
%%====================================================================

%% @private 构建 Skill 搜索过滤条件
-spec build_skill_filter(map()) -> map().
build_skill_filter(Opts) ->
    FilterSpecs = [
        {category, direct},
        {status, fun atom_to_binary/1}
    ],
    beamai_memory_utils:build_search_opts(Opts, FilterSpecs).

%% @private 过滤技能
%%
%% 在搜索结果上应用熟练度过滤（Store 可能不支持数值比较）
-spec filter_skills([#skill{}], map()) -> [#skill{}].
filter_skills(Skills, Opts) ->
    MinProf = maps:get(min_proficiency, Opts, 0.0),
    [S || S <- Skills, S#skill.proficiency >= MinProf].

%% @private 熟练度转状态
-spec proficiency_to_status(float()) -> skill_status().
proficiency_to_status(P) when P < ?PROFICIENCY_LEARNING -> ?SKILL_LEARNING;
proficiency_to_status(P) when P < ?PROFICIENCY_PROFICIENT -> ?SKILL_LEARNING;
proficiency_to_status(P) when P < ?PROFICIENCY_EXPERT -> ?SKILL_PROFICIENT;
proficiency_to_status(_) -> ?SKILL_EXPERT.
