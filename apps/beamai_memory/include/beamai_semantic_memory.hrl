%%%-------------------------------------------------------------------
%%% @doc Agent Semantic Memory 记录和常量定义
%%%
%%% Semantic Memory（语义记忆）存储事实与知识：
%%% - 用户偏好 (Preferences)
%%% - 实体信息 (Entities)
%%% - 知识片段 (Knowledge)
%%%
%%% == 认知科学背景 ==
%%%
%%% 语义记忆是人类长期记忆的一部分，存储一般性知识和事实，
%%% 与个人经历无关（区别于情景记忆）。
%%%
%%% == 数据组织 ==
%%%
%%% 使用层级化命名空间组织数据：
%%% - [<<"semantic">>, UserId, <<"preferences">>]  用户偏好
%%% - [<<"semantic">>, UserId, <<"entities">>]     实体信息
%%% - [<<"semantic">>, UserId, <<"knowledge">>]    知识片段
%%% - [<<"semantic">>, <<"shared">>, <<"knowledge">>] 共享知识
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AGENT_SEMANTIC_MEMORY_HRL).
-define(AGENT_SEMANTIC_MEMORY_HRL, true).

%%====================================================================
%% 偏好记录
%%====================================================================

%% 用户偏好
-record(preference, {
    %% 偏好键（如 "theme", "language", "response_style"）
    key :: binary(),

    %% 偏好值
    value :: term(),

    %% 置信度 (0.0 - 1.0)
    %% 显式设置的偏好置信度高，推断的偏好置信度低
    confidence = 1.0 :: float(),

    %% 来源
    source :: explicit | inferred | default,

    %% 来源对话（如果是从对话中提取的）
    source_thread :: binary() | undefined,

    %% 创建时间
    created_at :: integer(),

    %% 更新时间
    updated_at :: integer(),

    %% 过期时间（可选）
    expires_at :: integer() | undefined
}).

%%====================================================================
%% 实体记录
%%====================================================================

%% 实体类型
-type entity_type() :: person | organization | location | product |
                       concept | event | custom.

%% 实体信息
-record(entity, {
    %% 实体 ID（唯一标识）
    id :: binary(),

    %% 实体类型
    type :: entity_type(),

    %% 实体名称
    name :: binary(),

    %% 别名列表
    aliases = [] :: [binary()],

    %% 属性 map
    %% 如 #{<<"email">> => <<"...">>}，#{<<"role">> => <<"developer">>}
    attributes = #{} :: map(),

    %% 关系列表
    %% [{relation_type, target_entity_id}]
    relations = [] :: [{binary(), binary()}],

    %% 置信度
    confidence = 1.0 :: float(),

    %% 来源
    source :: explicit | extracted | inferred,

    %% 创建时间
    created_at :: integer(),

    %% 更新时间
    updated_at :: integer()
}).

%%====================================================================
%% 知识记录
%%====================================================================

%% 知识类型
-type knowledge_type() :: fact | rule | definition | relation | custom.

%% 知识片段
-record(knowledge, {
    %% 知识 ID
    id :: binary(),

    %% 知识类型
    type :: knowledge_type(),

    %% 主题/标题
    subject :: binary(),

    %% 内容
    content :: binary() | map(),

    %% 标签
    tags = [] :: [binary()],

    %% 关联实体
    related_entities = [] :: [binary()],

    %% 置信度
    confidence = 1.0 :: float(),

    %% 来源
    source :: explicit | extracted | inferred,

    %% 来源引用（如文档、对话等）
    source_ref :: binary() | undefined,

    %% 向量嵌入（用于语义搜索）
    embedding :: [float()] | undefined,

    %% 创建时间
    created_at :: integer(),

    %% 更新时间
    updated_at :: integer(),

    %% 过期时间
    expires_at :: integer() | undefined
}).

%%====================================================================
%% 常量
%%====================================================================

%% 命名空间前缀
-define(NS_SEMANTIC, <<"semantic">>).
-define(NS_PREFERENCES, <<"preferences">>).
-define(NS_ENTITIES, <<"entities">>).
-define(NS_KNOWLEDGE, <<"knowledge">>).
-define(NS_SEM_SHARED, <<"shared">>).  %% 避免与 beamai_store.hrl 中的 NS_SHARED 冲突

%% 默认置信度阈值
-define(DEFAULT_CONFIDENCE_THRESHOLD, 0.5).

%% 偏好来源
-define(SOURCE_EXPLICIT, explicit).
-define(SOURCE_INFERRED, inferred).
-define(SOURCE_EXTRACTED, extracted).
-define(SOURCE_DEFAULT, default).

%% 常用偏好键
-define(PREF_LANGUAGE, <<"language">>).
-define(PREF_TIMEZONE, <<"timezone">>).
-define(PREF_RESPONSE_STYLE, <<"response_style">>).
-define(PREF_VERBOSITY, <<"verbosity">>).
-define(PREF_CODE_STYLE, <<"code_style">>).

%% 实体关系类型
-define(REL_WORKS_AT, <<"works_at">>).
-define(REL_KNOWS, <<"knows">>).
-define(REL_MEMBER_OF, <<"member_of">>).
-define(REL_LOCATED_IN, <<"located_in">>).
-define(REL_RELATED_TO, <<"related_to">>).

-endif.
