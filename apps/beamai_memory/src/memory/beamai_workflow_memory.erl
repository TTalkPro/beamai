%%%-------------------------------------------------------------------
%%% @doc Agent Workflow Memory - 工作流记忆模块
%%%
%%% 管理 Agent 的工作流记忆，包括：
%%% - 工作流创建和获取
%%% - 工作流执行记录
%%% - 工作流启用/禁用管理
%%%
%%% 从 beamai_procedural_memory 拆分出来的独立模块。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_workflow_memory).

-include_lib("beamai_memory/include/beamai_store.hrl").
-include_lib("beamai_memory/include/beamai_procedural_memory.hrl").

%% 类型别名
-type memory() :: beamai_memory:memory().
-type user_id() :: binary().
-type workflow_id() :: binary().

%% 类型导出
-export_type([user_id/0, workflow_id/0]).

%% 工作流管理 API
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

%% 高级查询 API
-export([
    get_applicable_workflows/3
]).

%% 工具函数
-export([
    get_workflow_namespace/1
]).

%% 内部函数（供门面模块使用）
-export([
    workflow_to_map/1,
    map_to_workflow/1,
    step_to_map/1,
    map_to_step/1
]).

%%====================================================================
%% 工作流管理 API
%%====================================================================

%% @doc 创建工作流
%%
%% WorkflowData 支持：
%% - id: 工作流 ID（可选，自动生成）
%% - name: 工作流名称（必需）
%% - description: 工作流描述（必需）
%% - trigger: 触发条件（必需）
%% - steps: 步骤列表
%% - start_step: 起始步骤 ID
%% - input_schema: 输入参数定义
%% - output_schema: 输出定义
%% - tags: 标签列表
-spec create_workflow(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
create_workflow(Memory, UserId, WorkflowData) ->
    Namespace = get_workflow_namespace(UserId),
    WorkflowId = maps:get(id, WorkflowData, beamai_memory_utils:generate_id(<<"wf">>)),
    Timestamp = beamai_memory_utils:current_timestamp(),

    Steps = maps:get(steps, WorkflowData, []),
    ParsedSteps = parse_workflow_steps(Steps),

    Workflow = #workflow{
        id = WorkflowId,
        name = maps:get(name, WorkflowData),
        description = maps:get(description, WorkflowData),
        trigger = maps:get(trigger, WorkflowData),
        steps = ParsedSteps,
        start_step = maps:get(start_step, WorkflowData, get_first_step_id(ParsedSteps)),
        input_schema = maps:get(input_schema, WorkflowData, #{}),
        output_schema = maps:get(output_schema, WorkflowData, #{}),
        usage_count = 0,
        success_count = 0,
        avg_duration = undefined,
        tags = maps:get(tags, WorkflowData, []),
        version = 1,
        enabled = maps:get(enabled, WorkflowData, true),
        created_at = Timestamp,
        updated_at = Timestamp,
        embedding = maps:get(embedding, WorkflowData, undefined)
    },

    Value = workflow_to_map(Workflow),
    StoreOpts = case Workflow#workflow.embedding of
        undefined -> #{};
        Emb -> #{embedding => Emb}
    end,

    beamai_memory:put(Memory, Namespace, WorkflowId, Value, StoreOpts).

%% @doc 获取工作流
-spec get_workflow(memory(), user_id(), workflow_id()) ->
    {ok, #workflow{}} | {error, not_found | term()}.
get_workflow(Memory, UserId, WorkflowId) ->
    Namespace = get_workflow_namespace(UserId),
    case beamai_memory:get(Memory, Namespace, WorkflowId) of
        {ok, #store_item{value = Value, embedding = Emb}} ->
            W = map_to_workflow(Value),
            {ok, W#workflow{embedding = Emb}};
        {error, _} = Error ->
            Error
    end.

%% @doc 查找工作流
-spec find_workflows(memory(), user_id(), map()) ->
    {ok, [#workflow{}]} | {error, term()}.
find_workflows(Memory, UserId, Opts) ->
    Namespace = get_workflow_namespace(UserId),
    SearchOpts = build_workflow_filter(Opts),
    case beamai_memory:search(Memory, Namespace, SearchOpts) of
        {ok, Results} ->
            Workflows = [begin
                W = map_to_workflow(Item#store_item.value),
                W#workflow{embedding = Item#store_item.embedding}
            end || #search_result{item = Item} <- Results],
            %% 过滤禁用的工作流（除非明确请求）
            Filtered = case maps:get(include_disabled, Opts, false) of
                true -> Workflows;
                false -> [W || W <- Workflows, W#workflow.enabled]
            end,
            {ok, Filtered};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取所有工作流
-spec find_workflows(memory(), user_id()) ->
    {ok, [#workflow{}]} | {error, term()}.
find_workflows(Memory, UserId) ->
    find_workflows(Memory, UserId, #{}).

%% @doc 更新工作流
-spec update_workflow(memory(), user_id(), workflow_id(), map()) ->
    {ok, memory()} | {error, term()}.
update_workflow(Memory, UserId, WorkflowId, Updates) ->
    case get_workflow(Memory, UserId, WorkflowId) of
        {ok, Workflow} ->
            Timestamp = beamai_memory_utils:current_timestamp(),
            UpdatedWorkflow = apply_workflow_updates(Workflow, Updates, Timestamp),
            Namespace = get_workflow_namespace(UserId),
            Value = workflow_to_map(UpdatedWorkflow),
            StoreOpts = case UpdatedWorkflow#workflow.embedding of
                undefined -> #{};
                Emb -> #{embedding => Emb}
            end,
            beamai_memory:put(Memory, Namespace, WorkflowId, Value, StoreOpts);
        {error, _} = Error ->
            Error
    end.

%% @doc 记录工作流执行
-spec record_workflow_execution(memory(), user_id(), workflow_id(), map()) ->
    {ok, memory()} | {error, term()}.
record_workflow_execution(Memory, UserId, WorkflowId, Result) ->
    case get_workflow(Memory, UserId, WorkflowId) of
        {ok, Workflow} ->
            Success = maps:get(success, Result, true),
            Duration = maps:get(duration, Result, undefined),

            NewUsageCount = Workflow#workflow.usage_count + 1,
            NewSuccessCount = case Success of
                true -> Workflow#workflow.success_count + 1;
                false -> Workflow#workflow.success_count
            end,

            %% 更新平均执行时间
            NewAvgDuration = case {Duration, Workflow#workflow.avg_duration} of
                {undefined, Avg} -> Avg;
                {D, undefined} -> D;
                {D, OldAvg} ->
                    %% 使用指数移动平均
                    round(OldAvg * 0.8 + D * 0.2)
            end,

            Updates = #{
                usage_count => NewUsageCount,
                success_count => NewSuccessCount,
                avg_duration => NewAvgDuration
            },

            update_workflow(Memory, UserId, WorkflowId, Updates);
        {error, _} = Error ->
            Error
    end.

%% @doc 启用工作流
-spec enable_workflow(memory(), user_id(), workflow_id()) ->
    {ok, memory()} | {error, term()}.
enable_workflow(Memory, UserId, WorkflowId) ->
    update_workflow(Memory, UserId, WorkflowId, #{enabled => true}).

%% @doc 禁用工作流
-spec disable_workflow(memory(), user_id(), workflow_id()) ->
    {ok, memory()} | {error, term()}.
disable_workflow(Memory, UserId, WorkflowId) ->
    update_workflow(Memory, UserId, WorkflowId, #{enabled => false}).

%% @doc 删除工作流
-spec delete_workflow(memory(), user_id(), workflow_id()) ->
    {ok, memory()} | {error, term()}.
delete_workflow(Memory, UserId, WorkflowId) ->
    Namespace = get_workflow_namespace(UserId),
    beamai_memory:delete(Memory, Namespace, WorkflowId).

%%====================================================================
%% 高级查询 API
%%====================================================================

%% @doc 获取适用的工作流
%%
%% 根据触发条件描述查找适用的工作流
-spec get_applicable_workflows(memory(), user_id(), binary()) ->
    {ok, [#workflow{}]} | {error, term()}.
get_applicable_workflows(Memory, UserId, TriggerDesc) ->
    %% 使用语义搜索查找匹配的工作流
    find_workflows(Memory, UserId, #{query => TriggerDesc}).

%%====================================================================
%% 命名空间工具函数
%%====================================================================

%% @doc 获取用户工作流命名空间
-spec get_workflow_namespace(user_id()) -> [binary()].
get_workflow_namespace(UserId) ->
    [?NS_PROCEDURAL, UserId, ?NS_WORKFLOWS].

%%====================================================================
%% 内部函数 - 记录与 Map 转换
%%====================================================================

%% @doc Workflow 记录转 Map
-spec workflow_to_map(#workflow{}) -> map().
workflow_to_map(#workflow{} = W) ->
    #{
        <<"id">> => W#workflow.id,
        <<"name">> => W#workflow.name,
        <<"description">> => W#workflow.description,
        <<"trigger">> => W#workflow.trigger,
        <<"steps">> => [step_to_map(S) || S <- W#workflow.steps],
        <<"start_step">> => W#workflow.start_step,
        <<"input_schema">> => W#workflow.input_schema,
        <<"output_schema">> => W#workflow.output_schema,
        <<"usage_count">> => W#workflow.usage_count,
        <<"success_count">> => W#workflow.success_count,
        <<"avg_duration">> => W#workflow.avg_duration,
        <<"tags">> => W#workflow.tags,
        <<"version">> => W#workflow.version,
        <<"enabled">> => W#workflow.enabled,
        <<"created_at">> => W#workflow.created_at,
        <<"updated_at">> => W#workflow.updated_at
    }.

%% @doc Map 转 Workflow 记录
-spec map_to_workflow(map()) -> #workflow{}.
map_to_workflow(M) ->
    #workflow{
        id = maps:get(<<"id">>, M),
        name = maps:get(<<"name">>, M),
        description = maps:get(<<"description">>, M),
        trigger = maps:get(<<"trigger">>, M),
        steps = [map_to_step(S) || S <- maps:get(<<"steps">>, M, [])],
        start_step = maps:get(<<"start_step">>, M, undefined),
        input_schema = maps:get(<<"input_schema">>, M, #{}),
        output_schema = maps:get(<<"output_schema">>, M, #{}),
        usage_count = maps:get(<<"usage_count">>, M, 0),
        success_count = maps:get(<<"success_count">>, M, 0),
        avg_duration = maps:get(<<"avg_duration">>, M, undefined),
        tags = maps:get(<<"tags">>, M, []),
        version = maps:get(<<"version">>, M, 1),
        enabled = maps:get(<<"enabled">>, M, true),
        created_at = maps:get(<<"created_at">>, M),
        updated_at = maps:get(<<"updated_at">>, M),
        embedding = undefined
    }.

%% @doc Step 记录转 Map
-spec step_to_map(#workflow_step{}) -> map().
step_to_map(#workflow_step{} = S) ->
    #{
        <<"id">> => S#workflow_step.id,
        <<"name">> => S#workflow_step.name,
        <<"type">> => atom_to_binary(S#workflow_step.type),
        <<"instruction">> => S#workflow_step.instruction,
        <<"tool">> => S#workflow_step.tool,
        <<"tool_args">> => S#workflow_step.tool_args,
        <<"condition">> => S#workflow_step.condition,
        <<"substeps">> => [step_to_map(Sub) || Sub <- S#workflow_step.substeps],
        <<"next">> => S#workflow_step.next,
        <<"on_failure">> => S#workflow_step.on_failure,
        <<"metadata">> => S#workflow_step.metadata
    }.

%% @doc Map 转 Step 记录
-spec map_to_step(map()) -> #workflow_step{}.
map_to_step(M) ->
    #workflow_step{
        id = maps:get(<<"id">>, M),
        name = maps:get(<<"name">>, M),
        type = beamai_memory_utils:safe_binary_to_atom(maps:get(<<"type">>, M, <<"action">>)),
        instruction = maps:get(<<"instruction">>, M),
        tool = maps:get(<<"tool">>, M, undefined),
        tool_args = maps:get(<<"tool_args">>, M, undefined),
        condition = maps:get(<<"condition">>, M, undefined),
        substeps = [map_to_step(Sub) || Sub <- maps:get(<<"substeps">>, M, [])],
        next = maps:get(<<"next">>, M, undefined),
        on_failure = maps:get(<<"on_failure">>, M, undefined),
        metadata = maps:get(<<"metadata">>, M, #{})
    }.

%%====================================================================
%% 内部函数 - 更新应用
%%====================================================================

%% @private 应用 Workflow 更新
-spec apply_workflow_updates(#workflow{}, map(), integer()) -> #workflow{}.
apply_workflow_updates(Workflow, Updates, Timestamp) ->
    W1 = maps:fold(fun
        (name, V, W) -> W#workflow{name = V};
        (description, V, W) -> W#workflow{description = V};
        (trigger, V, W) -> W#workflow{trigger = V};
        (steps, V, W) -> W#workflow{steps = parse_workflow_steps(V)};
        (start_step, V, W) -> W#workflow{start_step = V};
        (input_schema, V, W) -> W#workflow{input_schema = V};
        (output_schema, V, W) -> W#workflow{output_schema = V};
        (usage_count, V, W) -> W#workflow{usage_count = V};
        (success_count, V, W) -> W#workflow{success_count = V};
        (avg_duration, V, W) -> W#workflow{avg_duration = V};
        (tags, V, W) -> W#workflow{tags = V};
        (enabled, V, W) -> W#workflow{enabled = V};
        (embedding, V, W) -> W#workflow{embedding = V};
        (_, _, W) -> W
    end, Workflow, Updates),
    %% 版本号递增
    W1#workflow{updated_at = Timestamp, version = Workflow#workflow.version + 1}.

%%====================================================================
%% 内部函数 - 搜索过滤
%%====================================================================

%% @private 构建 Workflow 搜索过滤条件
-spec build_workflow_filter(map()) -> map().
build_workflow_filter(Opts) ->
    beamai_memory_utils:build_search_opts(Opts, []).

%%====================================================================
%% 内部函数 - 工具
%%====================================================================

%% @private 解析工作流步骤
-spec parse_workflow_steps([map() | #workflow_step{}]) -> [#workflow_step{}].
parse_workflow_steps(Steps) ->
    lists:map(fun
        (#workflow_step{} = S) -> S;
        (M) when is_map(M) -> map_to_step(M)
    end, Steps).

%% @private 获取第一个步骤的 ID
-spec get_first_step_id([#workflow_step{}]) -> binary() | undefined.
get_first_step_id([]) -> undefined;
get_first_step_id([#workflow_step{id = Id} | _]) -> Id.
