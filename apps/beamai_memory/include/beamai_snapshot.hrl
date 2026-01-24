%%%-------------------------------------------------------------------
%%% @doc Snapshot 记录和常量定义
%%%
%%% 定义 Snapshot（短期记忆）相关的记录和常量。
%%% 面向 Process Framework 设计，支持流程快照的持久化和恢复。
%%%
%%% == 核心概念 ==
%%%
%%% - Thread: 单个执行会话，由 thread_id 标识
%%% - Snapshot: 某一时刻的状态快照（包含 values 数据）
%%% - Metadata: 快照的执行上下文元信息（流程状态、步骤信息等）
%%%
%%% == snapshot_metadata 设计 ==
%%%
%%% 面向 Process Framework 的元数据结构，记录：
%%% - 流程信息：process_name、process_state（FSM 状态）
%%% - 步骤信息：step_id（触发步骤）、step_activations（激活计数）
%%% - 执行标识：run_id、agent_id、agent_name
%%% - 扩展元数据：metadata map（用于存储业务自定义数据）
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BEAMAI_SNAPSHOT_HRL).
-define(BEAMAI_SNAPSHOT_HRL, true).

%%====================================================================
%% 快照数据结构
%%====================================================================

%% 快照 - 状态快照
-record(snapshot, {
    %% 唯一标识
    id :: binary(),

    %% 所属线程
    thread_id :: binary(),

    %% 父快照 ID（用于分支/回溯）
    parent_id :: binary() | undefined,

    %% 状态数据
    %% #{messages => [...], full_messages => [...], scratchpad => [...], context => #{}, ...}
    values = #{} :: map(),

    %% 创建时间戳
    timestamp :: integer()
}).

%% 快照元数据
%%
%% 记录快照的执行上下文，面向 Process Framework 设计。
%% 与 snapshot 记录配合使用，snapshot 存储状态数据，
%% snapshot_metadata 存储执行过程的元信息。
%%
%% == 字段说明 ==
%%
%% 1. 快照类型与流程信息：
%%    - snapshot_type: 快照产生阶段（initial/step_completed/paused/completed/error/manual/branch）
%%    - process_name: 流程名称
%%    - process_state: 流程状态机状态（idle/running/paused/completed/failed）
%%
%% 2. 步骤执行信息：
%%    - step_id: 触发快照的步骤 ID
%%    - step_activations: 各步骤激活计数 Map
%%
%% 3. 执行标识：
%%    - run_id: 单次流程执行的唯一标识
%%    - agent_id: 执行此流程的 Agent 标识
%%    - agent_name: Agent 人类可读名称
%%
%% 4. 扩展信息：
%%    - metadata: 用户自定义的元数据
%%
-record(snapshot_metadata, {
    %%--------------------------------------------------------------------
    %% 快照类型与流程信息
    %%--------------------------------------------------------------------

    %% 快照类型
    %%
    %% 标识快照在流程执行过程中的产生阶段：
    %% - initial: 流程启动时的初始状态
    %% - step_completed: 步骤执行完成后
    %% - paused: 流程暂停时
    %% - completed: 流程执行完成后的最终状态
    %% - error: 执行出错时
    %% - manual: 手动触发的快照
    %% - branch: 从其他快照分支创建
    %% - undefined: 未指定
    snapshot_type :: initial | step_completed | paused | completed | error | manual | branch | undefined,

    %% 流程名称
    %%
    %% 产生此快照的流程定义名称（process_spec 中的 name 字段）。
    %% 用于按流程类型分类和查询快照。
    process_name :: atom() | undefined,

    %% 流程状态机状态
    %%
    %% 快照创建时流程所处的 FSM 状态：
    %% - idle: 空闲，等待事件
    %% - running: 正在处理事件队列
    %% - paused: 已暂停，等待恢复
    %% - completed: 已完成
    %% - failed: 已失败
    process_state :: idle | running | paused | completed | failed | undefined,

    %%--------------------------------------------------------------------
    %% 步骤执行信息
    %%--------------------------------------------------------------------

    %% 触发快照的步骤 ID
    %%
    %% 记录导致此快照产生的步骤标识。
    %% 对于 step_completed 和 paused 类型的快照尤为重要。
    %% 其他类型快照此字段可能为 undefined。
    step_id :: atom() | undefined,

    %% 各步骤激活计数
    %%
    %% 记录快照创建时各步骤已被激活的次数。
    %% 用于：
    %% - 恢复时还原步骤执行进度
    %% - 监控步骤执行频率
    %% - 调试循环和重复激活问题
    %%
    %% 格式：#{步骤ID => 激活次数}
    step_activations = #{} :: #{atom() => non_neg_integer()},

    %%--------------------------------------------------------------------
    %% 执行标识
    %%--------------------------------------------------------------------

    %% 流程执行唯一标识（Run ID）
    %%
    %% 每次流程启动时生成的唯一标识。
    %% 用于：
    %% - 区分同一流程定义的不同执行实例
    %% - 关联同一次执行产生的多个快照
    %% - 日志追踪和调试
    run_id :: binary() | undefined,

    %% Agent 标识
    %%
    %% 执行此流程的 Agent 的唯一标识。
    %% 用于多 Agent 系统中追踪执行来源。
    agent_id :: binary() | undefined,

    %% Agent 名称
    %%
    %% 执行此流程的 Agent 的人类可读名称。
    %% 用于日志、调试和 UI 展示。
    agent_name :: binary() | undefined,

    %%--------------------------------------------------------------------
    %% 扩展信息
    %%--------------------------------------------------------------------

    %% 用户自定义元数据
    %%
    %% 允许用户存储任意键值对，用于：
    %% - 业务相关的标签和分类
    %% - 调试信息
    %% - 与外部系统集成的数据
    metadata = #{} :: map()
}).

%%====================================================================
%% 配置
%%====================================================================

%% Snapshot 配置
-record(snapshot_config, {
    %% 线程 ID（必需）
    thread_id :: binary(),

    %% 快照 ID（可选，用于恢复特定快照）
    snapshot_id :: binary() | undefined,

    %% 命名空间（可选）
    namespace :: binary() | [binary()] | undefined,

    %% 最大快照数量
    max_snapshots :: pos_integer(),

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

    %% 在此快照之前（时间顺序）
    before :: binary() | undefined,

    %% 在此快照之后（时间顺序）
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

%% 默认最大快照数量
-define(DEFAULT_MAX_SNAPSHOTS, 100).

%% 默认每线程最大快照
-define(DEFAULT_MAX_SNAPSHOTS_PER_THREAD, 50).

%% 根快照 ID
-define(ROOT_SNAPSHOT_ID, <<>>).

%% 通道名称
-define(CHANNEL_MESSAGES, messages).
-define(CHANNEL_CONTEXT, context).
-define(CHANNEL_VALUES, values).

-endif.
