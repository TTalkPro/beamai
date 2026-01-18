%%%-------------------------------------------------------------------
%%% @doc Agent Store 记录和常量定义
%%%
%%% 定义 Store（长期记忆）相关的记录和常量。
%%% 参考 LangGraph Store 设计。
%%%
%%% == 核心概念 ==
%%%
%%% - Namespace: 层级化的命名空间，类似文件夹路径
%%% - Item: 存储单元，包含 key、value 和元数据
%%% - Index: 可选的向量索引，支持语义搜索
%%%
%%% == Namespace 示例 ==
%%%
%%% ```
%%% [<<"user">>, <<"123">>]                    %% 用户级别
%%% [<<"user">>, <<"123">>, <<"preferences">>] %% 用户偏好
%%% [<<"org">>, <<"abc">>, <<"shared">>]       %% 组织共享
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AGENT_STORE_HRL).
-define(AGENT_STORE_HRL, true).

%%====================================================================
%% 存储项数据结构
%%====================================================================

%% 存储项 - Store 的基本单元
-record(store_item, {
    %% 命名空间（层级化路径）
    namespace :: [binary()],

    %% 键名
    key :: binary(),

    %% 值（JSON 可序列化的 map）
    value :: map(),

    %% 向量嵌入（用于语义搜索，可选）
    embedding :: [float()] | undefined,

    %% 创建时间戳
    created_at :: integer(),

    %% 更新时间戳
    updated_at :: integer(),

    %% 用户自定义元数据
    metadata = #{} :: map()
}).

%%====================================================================
%% 操作记录
%%====================================================================

%% Put 操作
-record(put_op, {
    namespace :: [binary()],
    key :: binary(),
    value :: map(),
    index :: boolean() | [binary()]  %% false | true | [field_paths]
}).

%% Get 操作
-record(get_op, {
    namespace :: [binary()],
    key :: binary()
}).

%% Delete 操作
-record(delete_op, {
    namespace :: [binary()],
    key :: binary()
}).

%% Search 操作
-record(search_op, {
    namespace_prefix :: [binary()],
    query :: binary() | undefined,
    filter :: map() | undefined,
    limit :: pos_integer(),
    offset :: non_neg_integer()
}).

%% List 操作
-record(list_op, {
    namespace_prefix :: [binary()],
    limit :: pos_integer(),
    offset :: non_neg_integer()
}).

%%====================================================================
%% 搜索结果
%%====================================================================

%% 搜索结果项
-record(search_result, {
    item :: #store_item{},
    score :: float() | undefined  %% 相似度分数（语义搜索时）
}).

%%====================================================================
%% 配置
%%====================================================================

%% Store 配置
-record(store_config, {
    %% 最大项数量（全局）
    max_items :: pos_integer() | undefined,

    %% 最大命名空间数量
    max_namespaces :: pos_integer() | undefined,

    %% 是否启用向量索引
    enable_indexing :: boolean(),

    %% 向量维度（启用索引时必需）
    embedding_dim :: pos_integer() | undefined,

    %% TTL（秒，可选）
    ttl :: pos_integer() | undefined,

    %% 自定义配置
    custom :: map()
}).

%%====================================================================
%% 常量
%%====================================================================

%% 默认配置
-define(DEFAULT_MAX_ITEMS, 10000).
-define(DEFAULT_MAX_NAMESPACES, 1000).
-define(DEFAULT_SEARCH_LIMIT, 10).
-define(DEFAULT_LIST_LIMIT, 100).

%% 命名空间分隔符
-define(NS_SEPARATOR, <<"/">>).

%% 特殊命名空间
-define(NS_ROOT, []).
-define(NS_SYSTEM, [<<"__system__">>]).
-define(NS_USERS, [<<"users">>]).
-define(NS_SHARED, [<<"shared">>]).

%% 元数据键
-define(META_CREATED_BY, <<"created_by">>).
-define(META_UPDATED_BY, <<"updated_by">>).
-define(META_VERSION, <<"version">>).
-define(META_TAGS, <<"tags">>).

-endif.
