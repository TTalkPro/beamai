%%%-------------------------------------------------------------------
%%% @doc Agent Procedural Memory - 程序记忆门面模块
%%%
%%% 程序记忆存储"如何做"的知识，包括技能、工作流和工具使用模式。
%%% 本模块作为门面，统一提供所有程序记忆的 API。
%%%
%%% 内部实现委托给专门的子模块：
%%% - beamai_skill_memory: 技能管理
%%% - beamai_workflow_memory: 工作流管理
%%%
%%% 工具模式管理由本模块直接处理（使用频率较低）。
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 Memory 实例
%%% {ok, Mem} = beamai_memory:new(#{store => #{backend => ets}}),
%%%
%%% %% 注册技能
%%% {ok, Mem1} = beamai_procedural_memory:register_skill(Mem, UserId, #{
%%%     name => <<"代码审查">>,
%%%     description => <<"审查代码质量、安全性和最佳实践">>,
%%%     category => <<"coding">>
%%% }),
%%%
%%% %% 创建工作流
%%% {ok, Mem2} = beamai_procedural_memory:create_workflow(Mem1, UserId, #{
%%%     name => <<"Bug 修复流程">>,
%%%     description => <<"标准的 bug 修复工作流程">>,
%%%     trigger => <<"当用户报告 bug 时">>
%%% }),
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_procedural_memory).

-include_lib("beamai_memory/include/beamai_store.hrl").
-include_lib("beamai_memory/include/beamai_procedural_memory.hrl").

%% 类型别名
-type memory() :: beamai_memory:memory().
-type user_id() :: binary().
-type skill_id() :: binary().
-type workflow_id() :: binary().
-type pattern_id() :: binary().

%% 类型导出
-export_type([user_id/0, skill_id/0, workflow_id/0, pattern_id/0]).

%% 技能管理 API（委托给 beamai_skill_memory）
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

%% 工作流管理 API（委托给 beamai_workflow_memory）
-export([
    create_workflow/3,
    get_workflow/3,
    find_workflows/3,
    find_workflows/2,
    update_workflow/4,
    record_workflow_execution/4,
    enable_workflow/3,
    disable_workflow/3,
    delete_workflow/3
]).

%% 工具模式管理 API（本模块直接处理）
-export([
    register_tool_pattern/3,
    get_tool_pattern/3,
    find_tool_patterns/3,
    find_patterns_for_tool/3,
    update_tool_pattern/4,
    record_pattern_usage/4,
    delete_tool_pattern/3
]).

%% 共享技能 API（委托给 beamai_skill_memory）
-export([
    register_shared_skill/2,
    find_shared_skills/2,
    find_shared_skills/1
]).

%% 高级查询 API
-export([
    get_proficient_skills/2,
    get_applicable_workflows/3,
    suggest_tools/3
]).

%% 工具函数
-export([
    get_skill_namespace/1,
    get_workflow_namespace/1,
    get_tool_pattern_namespace/1,
    get_shared_skill_namespace/0,
    calculate_proficiency/2
]).

%%====================================================================
%% 技能管理 API（委托给 beamai_skill_memory）
%%====================================================================

%% @doc 注册技能
-spec register_skill(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
register_skill(Memory, UserId, SkillData) ->
    beamai_skill_memory:register_skill(Memory, UserId, SkillData).

%% @doc 获取技能
-spec get_skill(memory(), user_id(), skill_id()) ->
    {ok, #skill{}} | {error, not_found | term()}.
get_skill(Memory, UserId, SkillId) ->
    beamai_skill_memory:get_skill(Memory, UserId, SkillId).

%% @doc 查找技能
-spec find_skills(memory(), user_id(), map()) ->
    {ok, [#skill{}]} | {error, term()}.
find_skills(Memory, UserId, Opts) ->
    beamai_skill_memory:find_skills(Memory, UserId, Opts).

%% @doc 获取所有技能
-spec find_skills(memory(), user_id()) ->
    {ok, [#skill{}]} | {error, term()}.
find_skills(Memory, UserId) ->
    beamai_skill_memory:find_skills(Memory, UserId).

%% @doc 更新技能
-spec update_skill(memory(), user_id(), skill_id(), map()) ->
    {ok, memory()} | {error, term()}.
update_skill(Memory, UserId, SkillId, Updates) ->
    beamai_skill_memory:update_skill(Memory, UserId, SkillId, Updates).

%% @doc 记录技能使用
-spec record_skill_usage(memory(), user_id(), skill_id(), map()) ->
    {ok, memory()} | {error, term()}.
record_skill_usage(Memory, UserId, SkillId, Result) ->
    beamai_skill_memory:record_skill_usage(Memory, UserId, SkillId, Result).

%% @doc 弃用技能
-spec deprecate_skill(memory(), user_id(), skill_id()) ->
    {ok, memory()} | {error, term()}.
deprecate_skill(Memory, UserId, SkillId) ->
    beamai_skill_memory:deprecate_skill(Memory, UserId, SkillId).

%% @doc 删除技能
-spec delete_skill(memory(), user_id(), skill_id()) ->
    {ok, memory()} | {error, term()}.
delete_skill(Memory, UserId, SkillId) ->
    beamai_skill_memory:delete_skill(Memory, UserId, SkillId).

%%====================================================================
%% 工作流管理 API（委托给 beamai_workflow_memory）
%%====================================================================

%% @doc 创建工作流
-spec create_workflow(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
create_workflow(Memory, UserId, WorkflowData) ->
    beamai_workflow_memory:create_workflow(Memory, UserId, WorkflowData).

%% @doc 获取工作流
-spec get_workflow(memory(), user_id(), workflow_id()) ->
    {ok, #workflow{}} | {error, not_found | term()}.
get_workflow(Memory, UserId, WorkflowId) ->
    beamai_workflow_memory:get_workflow(Memory, UserId, WorkflowId).

%% @doc 查找工作流
-spec find_workflows(memory(), user_id(), map()) ->
    {ok, [#workflow{}]} | {error, term()}.
find_workflows(Memory, UserId, Opts) ->
    beamai_workflow_memory:find_workflows(Memory, UserId, Opts).

%% @doc 获取所有工作流
-spec find_workflows(memory(), user_id()) ->
    {ok, [#workflow{}]} | {error, term()}.
find_workflows(Memory, UserId) ->
    beamai_workflow_memory:find_workflows(Memory, UserId).

%% @doc 更新工作流
-spec update_workflow(memory(), user_id(), workflow_id(), map()) ->
    {ok, memory()} | {error, term()}.
update_workflow(Memory, UserId, WorkflowId, Updates) ->
    beamai_workflow_memory:update_workflow(Memory, UserId, WorkflowId, Updates).

%% @doc 记录工作流执行
-spec record_workflow_execution(memory(), user_id(), workflow_id(), map()) ->
    {ok, memory()} | {error, term()}.
record_workflow_execution(Memory, UserId, WorkflowId, Result) ->
    beamai_workflow_memory:record_workflow_execution(Memory, UserId, WorkflowId, Result).

%% @doc 启用工作流
-spec enable_workflow(memory(), user_id(), workflow_id()) ->
    {ok, memory()} | {error, term()}.
enable_workflow(Memory, UserId, WorkflowId) ->
    beamai_workflow_memory:enable_workflow(Memory, UserId, WorkflowId).

%% @doc 禁用工作流
-spec disable_workflow(memory(), user_id(), workflow_id()) ->
    {ok, memory()} | {error, term()}.
disable_workflow(Memory, UserId, WorkflowId) ->
    beamai_workflow_memory:disable_workflow(Memory, UserId, WorkflowId).

%% @doc 删除工作流
-spec delete_workflow(memory(), user_id(), workflow_id()) ->
    {ok, memory()} | {error, term()}.
delete_workflow(Memory, UserId, WorkflowId) ->
    beamai_workflow_memory:delete_workflow(Memory, UserId, WorkflowId).

%%====================================================================
%% 工具模式管理 API（本模块直接处理）
%%====================================================================

%% @doc 注册工具使用模式
%%
%% PatternData 支持：
%% - id: 模式 ID（可选，自动生成）
%% - tool_name: 工具名称（必需）
%% - pattern_name: 模式名称（必需）
%% - description: 模式描述（必需）
%% - use_case: 适用场景（必需）
%% - arg_template: 参数模板
%% - examples: 使用示例
%% - best_practices: 最佳实践
%% - pitfalls: 常见错误
%% - tags: 标签列表
-spec register_tool_pattern(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
register_tool_pattern(Memory, UserId, PatternData) ->
    Namespace = get_tool_pattern_namespace(UserId),
    PatternId = maps:get(id, PatternData, beamai_memory_utils:generate_id(<<"pat">>)),
    Timestamp = beamai_memory_utils:current_timestamp(),

    Pattern = #tool_pattern{
        id = PatternId,
        tool_name = maps:get(tool_name, PatternData),
        pattern_name = maps:get(pattern_name, PatternData),
        description = maps:get(description, PatternData),
        use_case = maps:get(use_case, PatternData),
        arg_template = maps:get(arg_template, PatternData, #{}),
        examples = maps:get(examples, PatternData, []),
        best_practices = maps:get(best_practices, PatternData, []),
        pitfalls = maps:get(pitfalls, PatternData, []),
        usage_count = 0,
        success_rate = 1.0,
        tags = maps:get(tags, PatternData, []),
        created_at = Timestamp,
        updated_at = Timestamp,
        embedding = maps:get(embedding, PatternData, undefined)
    },

    Value = tool_pattern_to_map(Pattern),
    StoreOpts = case Pattern#tool_pattern.embedding of
        undefined -> #{};
        Emb -> #{embedding => Emb}
    end,

    beamai_memory:put(Memory, Namespace, PatternId, Value, StoreOpts).

%% @doc 获取工具模式
-spec get_tool_pattern(memory(), user_id(), pattern_id()) ->
    {ok, #tool_pattern{}} | {error, not_found | term()}.
get_tool_pattern(Memory, UserId, PatternId) ->
    Namespace = get_tool_pattern_namespace(UserId),
    case beamai_memory:get(Memory, Namespace, PatternId) of
        {ok, #store_item{value = Value, embedding = Emb}} ->
            P = map_to_tool_pattern(Value),
            {ok, P#tool_pattern{embedding = Emb}};
        {error, _} = Error ->
            Error
    end.

%% @doc 查找工具模式
-spec find_tool_patterns(memory(), user_id(), map()) ->
    {ok, [#tool_pattern{}]} | {error, term()}.
find_tool_patterns(Memory, UserId, Opts) ->
    Namespace = get_tool_pattern_namespace(UserId),
    SearchOpts = build_pattern_filter(Opts),
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Patterns = [begin
                P = map_to_tool_pattern(Item#store_item.value),
                P#tool_pattern{embedding = Item#store_item.embedding}
            end || #search_result{item = Item} <- Results],
            {ok, Patterns};
        {error, _} = Error ->
            Error
    end.

%% @doc 查找特定工具的模式
-spec find_patterns_for_tool(memory(), user_id(), binary()) ->
    {ok, [#tool_pattern{}]} | {error, term()}.
find_patterns_for_tool(Memory, UserId, ToolName) ->
    find_tool_patterns(Memory, UserId, #{tool_name => ToolName}).

%% @doc 更新工具模式
-spec update_tool_pattern(memory(), user_id(), pattern_id(), map()) ->
    {ok, memory()} | {error, term()}.
update_tool_pattern(Memory, UserId, PatternId, Updates) ->
    case get_tool_pattern(Memory, UserId, PatternId) of
        {ok, Pattern} ->
            Timestamp = beamai_memory_utils:current_timestamp(),
            UpdatedPattern = apply_pattern_updates(Pattern, Updates, Timestamp),
            Namespace = get_tool_pattern_namespace(UserId),
            Value = tool_pattern_to_map(UpdatedPattern),
            StoreOpts = case UpdatedPattern#tool_pattern.embedding of
                undefined -> #{};
                Emb -> #{embedding => Emb}
            end,
            beamai_memory:put(Memory, Namespace, PatternId, Value, StoreOpts);
        {error, _} = Error ->
            Error
    end.

%% @doc 记录模式使用
-spec record_pattern_usage(memory(), user_id(), pattern_id(), map()) ->
    {ok, memory()} | {error, term()}.
record_pattern_usage(Memory, UserId, PatternId, Result) ->
    case get_tool_pattern(Memory, UserId, PatternId) of
        {ok, Pattern} ->
            Success = maps:get(success, Result, true),

            NewUsageCount = Pattern#tool_pattern.usage_count + 1,
            OldSuccessRate = Pattern#tool_pattern.success_rate,

            %% 使用指数移动平均更新成功率
            SuccessValue = case Success of true -> 1.0; false -> 0.0 end,
            NewSuccessRate = OldSuccessRate * 0.9 + SuccessValue * 0.1,

            Updates = #{
                usage_count => NewUsageCount,
                success_rate => NewSuccessRate
            },

            update_tool_pattern(Memory, UserId, PatternId, Updates);
        {error, _} = Error ->
            Error
    end.

%% @doc 删除工具模式
-spec delete_tool_pattern(memory(), user_id(), pattern_id()) ->
    {ok, memory()} | {error, term()}.
delete_tool_pattern(Memory, UserId, PatternId) ->
    Namespace = get_tool_pattern_namespace(UserId),
    beamai_memory:delete(Memory, Namespace, PatternId).

%%====================================================================
%% 共享技能 API（委托给 beamai_skill_memory）
%%====================================================================

%% @doc 注册共享技能
-spec register_shared_skill(memory(), map()) ->
    {ok, memory()} | {error, term()}.
register_shared_skill(Memory, SkillData) ->
    beamai_skill_memory:register_shared_skill(Memory, SkillData).

%% @doc 查找共享技能
-spec find_shared_skills(memory(), map()) ->
    {ok, [#skill{}]} | {error, term()}.
find_shared_skills(Memory, Opts) ->
    beamai_skill_memory:find_shared_skills(Memory, Opts).

%% @doc 获取所有共享技能
-spec find_shared_skills(memory()) ->
    {ok, [#skill{}]} | {error, term()}.
find_shared_skills(Memory) ->
    beamai_skill_memory:find_shared_skills(Memory).

%%====================================================================
%% 高级查询 API
%%====================================================================

%% @doc 获取熟练的技能
-spec get_proficient_skills(memory(), user_id()) ->
    {ok, [#skill{}]} | {error, term()}.
get_proficient_skills(Memory, UserId) ->
    beamai_skill_memory:get_proficient_skills(Memory, UserId).

%% @doc 获取适用的工作流
-spec get_applicable_workflows(memory(), user_id(), binary()) ->
    {ok, [#workflow{}]} | {error, term()}.
get_applicable_workflows(Memory, UserId, TriggerDesc) ->
    beamai_workflow_memory:get_applicable_workflows(Memory, UserId, TriggerDesc).

%% @doc 建议工具使用
%%
%% 根据使用场景建议工具模式
-spec suggest_tools(memory(), user_id(), binary()) ->
    {ok, [#tool_pattern{}]} | {error, term()}.
suggest_tools(Memory, UserId, UseCase) ->
    find_tool_patterns(Memory, UserId, #{query => UseCase}).

%%====================================================================
%% 命名空间工具函数
%%====================================================================

%% @doc 获取用户技能命名空间
-spec get_skill_namespace(user_id()) -> [binary()].
get_skill_namespace(UserId) ->
    beamai_skill_memory:get_skill_namespace(UserId).

%% @doc 获取用户工作流命名空间
-spec get_workflow_namespace(user_id()) -> [binary()].
get_workflow_namespace(UserId) ->
    beamai_workflow_memory:get_workflow_namespace(UserId).

%% @doc 获取用户工具模式命名空间
-spec get_tool_pattern_namespace(user_id()) -> [binary()].
get_tool_pattern_namespace(UserId) ->
    [?NS_PROCEDURAL, UserId, ?NS_TOOLS].

%% @doc 获取共享技能命名空间
-spec get_shared_skill_namespace() -> [binary()].
get_shared_skill_namespace() ->
    beamai_skill_memory:get_shared_skill_namespace().

%% @doc 计算熟练度
-spec calculate_proficiency(non_neg_integer(), non_neg_integer()) -> float().
calculate_proficiency(SuccessCount, UsageCount) ->
    beamai_skill_memory:calculate_proficiency(SuccessCount, UsageCount).

%%====================================================================
%% 内部函数 - 工具模式记录与 Map 转换
%%====================================================================

%% @private ToolPattern 记录转 Map
-spec tool_pattern_to_map(#tool_pattern{}) -> map().
tool_pattern_to_map(#tool_pattern{} = P) ->
    #{
        <<"id">> => P#tool_pattern.id,
        <<"tool_name">> => P#tool_pattern.tool_name,
        <<"pattern_name">> => P#tool_pattern.pattern_name,
        <<"description">> => P#tool_pattern.description,
        <<"use_case">> => P#tool_pattern.use_case,
        <<"arg_template">> => P#tool_pattern.arg_template,
        <<"examples">> => P#tool_pattern.examples,
        <<"best_practices">> => P#tool_pattern.best_practices,
        <<"pitfalls">> => P#tool_pattern.pitfalls,
        <<"usage_count">> => P#tool_pattern.usage_count,
        <<"success_rate">> => P#tool_pattern.success_rate,
        <<"tags">> => P#tool_pattern.tags,
        <<"created_at">> => P#tool_pattern.created_at,
        <<"updated_at">> => P#tool_pattern.updated_at
    }.

%% @private Map 转 ToolPattern 记录
-spec map_to_tool_pattern(map()) -> #tool_pattern{}.
map_to_tool_pattern(M) ->
    #tool_pattern{
        id = maps:get(<<"id">>, M),
        tool_name = maps:get(<<"tool_name">>, M),
        pattern_name = maps:get(<<"pattern_name">>, M),
        description = maps:get(<<"description">>, M),
        use_case = maps:get(<<"use_case">>, M),
        arg_template = maps:get(<<"arg_template">>, M, #{}),
        examples = maps:get(<<"examples">>, M, []),
        best_practices = maps:get(<<"best_practices">>, M, []),
        pitfalls = maps:get(<<"pitfalls">>, M, []),
        usage_count = maps:get(<<"usage_count">>, M, 0),
        success_rate = maps:get(<<"success_rate">>, M, 1.0),
        tags = maps:get(<<"tags">>, M, []),
        created_at = maps:get(<<"created_at">>, M),
        updated_at = maps:get(<<"updated_at">>, M),
        embedding = undefined
    }.

%%====================================================================
%% 内部函数 - 工具模式更新应用
%%====================================================================

%% @private 应用 Pattern 更新
-spec apply_pattern_updates(#tool_pattern{}, map(), integer()) -> #tool_pattern{}.
apply_pattern_updates(Pattern, Updates, Timestamp) ->
    P1 = maps:fold(fun
        (pattern_name, V, P) -> P#tool_pattern{pattern_name = V};
        (description, V, P) -> P#tool_pattern{description = V};
        (use_case, V, P) -> P#tool_pattern{use_case = V};
        (arg_template, V, P) -> P#tool_pattern{arg_template = V};
        (examples, V, P) -> P#tool_pattern{examples = V};
        (best_practices, V, P) -> P#tool_pattern{best_practices = V};
        (pitfalls, V, P) -> P#tool_pattern{pitfalls = V};
        (usage_count, V, P) -> P#tool_pattern{usage_count = V};
        (success_rate, V, P) -> P#tool_pattern{success_rate = V};
        (tags, V, P) -> P#tool_pattern{tags = V};
        (embedding, V, P) -> P#tool_pattern{embedding = V};
        (_, _, P) -> P
    end, Pattern, Updates),
    P1#tool_pattern{updated_at = Timestamp}.

%%====================================================================
%% 内部函数 - 搜索过滤
%%====================================================================

%% @private 构建 Pattern 搜索过滤条件
-spec build_pattern_filter(map()) -> map().
build_pattern_filter(Opts) ->
    FilterSpecs = [
        {tool_name, direct}
    ],
    beamai_memory_utils:build_search_opts(Opts, FilterSpecs).
