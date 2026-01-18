%%%-------------------------------------------------------------------
%%% @doc Agent Checkpointer 记录和常量定义
%%%
%%% 定义 Checkpointer（短期记忆）相关的记录和常量。
%%% 类型定义在 beamai_checkpointer 模块中。
%%%
%%% == 核心概念 ==
%%%
%%% - Thread: 单个对话会话，由 thread_id 标识
%%% - Checkpoint: 某一时刻的状态快照
%%% - Channel: 图节点的输出值和版本
%%% - Pending Writes: 待处理的状态写入
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AGENT_CHECKPOINTER_HRL).
-define(AGENT_CHECKPOINTER_HRL, true).

%%====================================================================
%% 检查点数据结构
%%====================================================================

%% 检查点 - 状态快照
-record(checkpoint, {
    %% 唯一标识
    id :: binary(),

    %% 所属线程
    thread_id :: binary(),

    %% 父检查点 ID（用于分支/回溯）
    parent_id :: binary() | undefined,

    %% 通道值 - 图节点的输出
    %% #{channel_name => value}
    channel_values = #{} :: map(),

    %% 通道版本 - 用于冲突检测
    %% #{channel_name => version}
    channel_versions = #{} :: map(),

    %% 待处理写入 - 当前步骤的写入操作
    %% [{task_id, channel_name, value}]
    pending_writes = [] :: list(),

    %% 创建时间戳
    timestamp :: integer(),

    %% 版本号 - 同一线程内的序列号
    version = 0 :: non_neg_integer()
}).

%% 检查点元数据
-record(checkpoint_metadata, {
    %% 来源 - input | loop | update
    source :: input | loop | update | undefined,

    %% 当前步骤编号
    step = 0 :: non_neg_integer(),

    %% 父节点（来源节点）
    parents = #{} :: map(),

    %% 写入此检查点的任务
    writes = [] :: list(),

    %% 用户自定义元数据
    metadata = #{} :: map()
}).

%%====================================================================
%% 配置
%%====================================================================

%% Checkpointer 配置
-record(checkpointer_config, {
    %% 线程 ID（必需）
    thread_id :: binary(),

    %% 检查点 ID（可选，用于恢复特定检查点）
    checkpoint_id :: binary() | undefined,

    %% 命名空间（可选）
    namespace :: binary() | [binary()] | undefined,

    %% 最大检查点数量
    max_checkpoints :: pos_integer(),

    %% 自定义配置
    configurable = #{} :: map()
}).

%%====================================================================
%% 搜索和过滤选项
%%====================================================================

%% 列表选项
-record(list_opts, {
    %% 线程 ID 过滤
    thread_id :: binary() | undefined,

    %% 在此检查点之前（时间顺序）
    before :: binary() | undefined,

    %% 在此检查点之后（时间顺序）
    after_cp :: binary() | undefined,

    %% 时间范围
    from_timestamp :: integer() | undefined,
    to_timestamp :: integer() | undefined,

    %% 分页
    offset = 0 :: non_neg_integer(),
    limit = 100 :: pos_integer(),

    %% 过滤条件
    filter :: map() | undefined
}).

%%====================================================================
%% 常量
%%====================================================================

%% 默认最大检查点数量
-define(DEFAULT_MAX_CHECKPOINTS, 100).

%% 默认每线程最大检查点
-define(DEFAULT_MAX_CHECKPOINTS_PER_THREAD, 50).

%% 根检查点 ID
-define(ROOT_CHECKPOINT_ID, <<>>).

%% 通道名称
-define(CHANNEL_MESSAGES, messages).
-define(CHANNEL_CONTEXT, context).
-define(CHANNEL_VALUES, values).

-endif.
