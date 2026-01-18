%%%-------------------------------------------------------------------
%%% @doc Agent Episodic Memory 记录和常量定义
%%%
%%% Episodic Memory（情景记忆）存储个人经历和事件：
%%% - 对话片段 (Episodes) - 完整对话的摘要
%%% - 交互事件 (Events) - 具体的交互记录
%%% - 经验总结 (Experiences) - 从多次交互中提炼的经验
%%%
%%% == 认知科学背景 ==
%%%
%%% 情景记忆是人类长期记忆的一部分，存储个人经历的"什么、何时、何地"，
%%% 具有时间性和情境性，与语义记忆（一般性知识）形成对比。
%%%
%%% == 数据组织 ==
%%%
%%% 使用层级化命名空间组织数据：
%%% - [<<"episodic">>, UserId, <<"episodes">>]      对话片段
%%% - [<<"episodic">>, UserId, <<"events">>]        交互事件
%%% - [<<"episodic">>, UserId, <<"experiences">>]   经验总结
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AGENT_EPISODIC_MEMORY_HRL).
-define(AGENT_EPISODIC_MEMORY_HRL, true).

%%====================================================================
%% 对话片段记录
%%====================================================================

%% 对话片段 - 一次完整对话的摘要
-record(episode, {
    %% 片段 ID（通常与 thread_id 对应）
    id :: binary(),

    %% 关联的 thread_id
    thread_id :: binary(),

    %% 对话标题/主题（可由 LLM 生成）
    title :: binary() | undefined,

    %% 对话摘要（可由 LLM 生成）
    summary :: binary() | undefined,

    %% 参与者（如用户 ID、agent ID）
    participants = [] :: [binary()],

    %% 消息数量
    message_count = 0 :: non_neg_integer(),

    %% 关键话题/标签
    topics = [] :: [binary()],

    %% 情感基调 (positive | negative | neutral | mixed)
    sentiment :: atom() | undefined,

    %% 对话结果/状态 (completed | abandoned | ongoing)
    outcome :: atom(),

    %% 重要性评分 (0.0 - 1.0)
    importance = 0.5 :: float(),

    %% 开始时间
    started_at :: integer(),

    %% 结束时间（ongoing 时为 undefined）
    ended_at :: integer() | undefined,

    %% 创建时间
    created_at :: integer(),

    %% 更新时间
    updated_at :: integer(),

    %% 向量嵌入（用于语义搜索）
    embedding :: [float()] | undefined
}).

%%====================================================================
%% 交互事件记录
%%====================================================================

%% 事件类型
-type event_type() :: message | tool_call | error | milestone | custom.

%% 交互事件 - 对话中的具体事件
-record(event, {
    %% 事件 ID
    id :: binary(),

    %% 所属对话片段 ID
    episode_id :: binary(),

    %% 事件类型
    type :: event_type(),

    %% 事件描述
    description :: binary(),

    %% 事件数据（根据类型不同）
    %% message: #{role, content}
    %% tool_call: #{tool, args, result}
    %% error: #{error_type, message}
    data :: map(),

    %% 事件发生时间
    timestamp :: integer(),

    %% 重要性评分
    importance = 0.5 :: float(),

    %% 标签
    tags = [] :: [binary()],

    %% 元数据
    metadata = #{} :: map()
}).

%%====================================================================
%% 经验总结记录
%%====================================================================

%% 经验类型
-type experience_type() :: lesson | pattern | preference | insight | custom.

%% 经验总结 - 从多次交互中提炼的经验
-record(experience, {
    %% 经验 ID
    id :: binary(),

    %% 经验类型
    type :: experience_type(),

    %% 经验标题
    title :: binary(),

    %% 经验内容
    content :: binary(),

    %% 来源对话片段
    source_episodes = [] :: [binary()],

    %% 相关主题
    topics = [] :: [binary()],

    %% 置信度 (0.0 - 1.0)
    %% 基于经验被验证的次数
    confidence = 0.5 :: float(),

    %% 验证次数（经验被再次确认的次数）
    validation_count = 1 :: pos_integer(),

    %% 最后验证时间
    last_validated_at :: integer(),

    %% 创建时间
    created_at :: integer(),

    %% 更新时间
    updated_at :: integer(),

    %% 过期时间（可选）
    expires_at :: integer() | undefined,

    %% 向量嵌入
    embedding :: [float()] | undefined
}).

%%====================================================================
%% 常量
%%====================================================================

%% 命名空间前缀
-define(NS_EPISODIC, <<"episodic">>).
-define(NS_EPISODES, <<"episodes">>).
-define(NS_EVENTS, <<"events">>).
-define(NS_EXPERIENCES, <<"experiences">>).

%% 事件类型常量
-define(EVENT_MESSAGE, message).
-define(EVENT_TOOL_CALL, tool_call).
-define(EVENT_ERROR, error).
-define(EVENT_MILESTONE, milestone).

%% 对话结果常量
-define(OUTCOME_COMPLETED, completed).
-define(OUTCOME_ABANDONED, abandoned).
-define(OUTCOME_ONGOING, ongoing).

%% 情感基调常量
-define(SENTIMENT_POSITIVE, positive).
-define(SENTIMENT_NEGATIVE, negative).
-define(SENTIMENT_NEUTRAL, neutral).
-define(SENTIMENT_MIXED, mixed).

%% 经验类型常量
-define(EXP_LESSON, lesson).
-define(EXP_PATTERN, pattern).
-define(EXP_PREFERENCE, preference).
-define(EXP_INSIGHT, insight).

%% 默认值
-define(DEFAULT_IMPORTANCE, 0.5).
-define(DEFAULT_CONFIDENCE, 0.5).
-define(DEFAULT_EVENT_LIMIT, 100).
-define(DEFAULT_EPISODE_LIMIT, 50).

-endif.
