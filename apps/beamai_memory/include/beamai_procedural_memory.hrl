%%%-------------------------------------------------------------------
%%% @doc Agent Procedural Memory 记录和常量定义
%%%
%%% Procedural Memory（程序记忆）存储技能和程序性知识：
%%% - 技能 (Skills) - 学习到的能力和程序
%%% - 工作流 (Workflows) - 完成任务的步骤序列
%%% - 工具模式 (Tool Patterns) - 工具使用模式和配置
%%%
%%% == 认知科学背景 ==
%%%
%%% 程序记忆是人类长期记忆的一部分，存储"如何做"的知识，
%%% 如骑自行车、打字等技能。与语义记忆（知道什么）和
%%% 情景记忆（经历了什么）形成对比。
%%%
%%% == 数据组织 ==
%%%
%%% 使用层级化命名空间组织数据：
%%% - [<<"procedural">>, UserId, <<"skills">>]       技能
%%% - [<<"procedural">>, UserId, <<"workflows">>]    工作流
%%% - [<<"procedural">>, UserId, <<"tools">>]        工具模式
%%% - [<<"procedural">>, <<"shared">>, <<"skills">>] 共享技能
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AGENT_PROCEDURAL_MEMORY_HRL).
-define(AGENT_PROCEDURAL_MEMORY_HRL, true).

%%====================================================================
%% 技能记录
%%====================================================================

%% 技能状态
-type skill_status() :: learning | proficient | expert | deprecated.

%% 技能 - 学习到的能力
-record(skill, {
    %% 技能 ID
    id :: binary(),

    %% 技能名称
    name :: binary(),

    %% 技能描述
    description :: binary(),

    %% 技能类型（如 coding, analysis, communication）
    category :: binary(),

    %% 技能状态
    status :: skill_status(),

    %% 熟练度 (0.0 - 1.0)
    proficiency = 0.5 :: float(),

    %% 使用次数
    usage_count = 0 :: non_neg_integer(),

    %% 成功次数
    success_count = 0 :: non_neg_integer(),

    %% 执行步骤/指令
    %% 描述如何执行该技能的步骤列表
    instructions = [] :: [binary()],

    %% 前置条件
    %% 执行该技能需要满足的条件
    preconditions = [] :: [binary()],

    %% 相关工具
    %% 执行该技能可能用到的工具
    tools = [] :: [binary()],

    %% 示例
    %% 技能应用的示例
    examples = [] :: [map()],

    %% 标签
    tags = [] :: [binary()],

    %% 来源（学习自哪里）
    source :: learned | predefined | imported,

    %% 来源引用
    source_ref :: binary() | undefined,

    %% 最后使用时间
    last_used_at :: integer() | undefined,

    %% 创建时间
    created_at :: integer(),

    %% 更新时间
    updated_at :: integer(),

    %% 向量嵌入
    embedding :: [float()] | undefined
}).

%%====================================================================
%% 工作流记录
%%====================================================================

%% 步骤类型
-type step_type() :: action | decision | parallel | loop | wait.

%% 工作流步骤
-record(workflow_step, {
    %% 步骤 ID
    id :: binary(),

    %% 步骤名称
    name :: binary(),

    %% 步骤类型
    type :: step_type(),

    %% 步骤描述/指令
    instruction :: binary(),

    %% 使用的工具（如果有）
    tool :: binary() | undefined,

    %% 工具参数模板
    tool_args :: map() | undefined,

    %% 条件（用于 decision 类型）
    condition :: binary() | undefined,

    %% 子步骤（用于 parallel/loop 类型）
    substeps = [] :: [#workflow_step{}],

    %% 下一步骤 ID（undefined 表示结束）
    next :: binary() | undefined,

    %% 失败时的步骤 ID
    on_failure :: binary() | undefined,

    %% 元数据
    metadata = #{} :: map()
}).

%% 工作流 - 完成任务的步骤序列
-record(workflow, {
    %% 工作流 ID
    id :: binary(),

    %% 工作流名称
    name :: binary(),

    %% 工作流描述
    description :: binary(),

    %% 触发条件/适用场景
    trigger :: binary(),

    %% 工作流步骤
    steps = [] :: [#workflow_step{}],

    %% 起始步骤 ID
    start_step :: binary(),

    %% 输入参数定义
    %% #{param_name => #{type, required, default, description}}
    input_schema = #{} :: map(),

    %% 输出定义
    output_schema = #{} :: map(),

    %% 使用次数
    usage_count = 0 :: non_neg_integer(),

    %% 成功次数
    success_count = 0 :: non_neg_integer(),

    %% 平均执行时间（毫秒）
    avg_duration :: integer() | undefined,

    %% 标签
    tags = [] :: [binary()],

    %% 版本号
    version = 1 :: pos_integer(),

    %% 是否启用
    enabled = true :: boolean(),

    %% 创建时间
    created_at :: integer(),

    %% 更新时间
    updated_at :: integer(),

    %% 向量嵌入
    embedding :: [float()] | undefined
}).

%%====================================================================
%% 工具模式记录
%%====================================================================

%% 工具模式 - 工具使用的最佳实践
-record(tool_pattern, {
    %% 模式 ID
    id :: binary(),

    %% 工具名称
    tool_name :: binary(),

    %% 模式名称（如 "文件搜索模式", "API 调用模式"）
    pattern_name :: binary(),

    %% 模式描述
    description :: binary(),

    %% 适用场景
    use_case :: binary(),

    %% 参数模板
    %% 常用的参数组合
    arg_template :: map(),

    %% 使用示例
    examples = [] :: [map()],

    %% 注意事项/最佳实践
    best_practices = [] :: [binary()],

    %% 常见错误/避免事项
    pitfalls = [] :: [binary()],

    %% 使用次数
    usage_count = 0 :: non_neg_integer(),

    %% 成功率
    success_rate = 1.0 :: float(),

    %% 标签
    tags = [] :: [binary()],

    %% 创建时间
    created_at :: integer(),

    %% 更新时间
    updated_at :: integer(),

    %% 向量嵌入
    embedding :: [float()] | undefined
}).

%%====================================================================
%% 常量
%%====================================================================

%% 命名空间前缀
-define(NS_PROCEDURAL, <<"procedural">>).
-define(NS_SKILLS, <<"skills">>).
-define(NS_WORKFLOWS, <<"workflows">>).
-define(NS_TOOLS, <<"tools">>).
-define(NS_PROC_SHARED, <<"shared">>).

%% 技能状态常量
-define(SKILL_LEARNING, learning).
-define(SKILL_PROFICIENT, proficient).
-define(SKILL_EXPERT, expert).
-define(SKILL_DEPRECATED, deprecated).

%% 技能来源常量
-define(SKILL_SOURCE_LEARNED, learned).
-define(SKILL_SOURCE_PREDEFINED, predefined).
-define(SKILL_SOURCE_IMPORTED, imported).

%% 步骤类型常量
-define(STEP_ACTION, action).
-define(STEP_DECISION, decision).
-define(STEP_PARALLEL, parallel).
-define(STEP_LOOP, loop).
-define(STEP_WAIT, wait).

%% 技能类别常量
-define(CAT_CODING, <<"coding">>).
-define(CAT_ANALYSIS, <<"analysis">>).
-define(CAT_COMMUNICATION, <<"communication">>).
-define(CAT_PROBLEM_SOLVING, <<"problem_solving">>).
-define(CAT_TOOL_USAGE, <<"tool_usage">>).

%% 默认值
-define(DEFAULT_PROFICIENCY, 0.5).
-define(DEFAULT_SKILL_LIMIT, 100).
-define(DEFAULT_WORKFLOW_LIMIT, 50).

%% 熟练度阈值
-define(PROFICIENCY_LEARNING, 0.3).
-define(PROFICIENCY_PROFICIENT, 0.6).
-define(PROFICIENCY_EXPERT, 0.9).

-endif.
