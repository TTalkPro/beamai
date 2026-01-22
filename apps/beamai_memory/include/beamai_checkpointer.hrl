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
%%% - Values: 状态数据（messages, context 等）
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

    %% 状态数据
    %% #{messages => [...], full_messages => [...], scratchpad => [...], context => #{}, ...}
    values = #{} :: map(),

    %% 创建时间戳
    timestamp :: integer()
}).

%% 检查点元数据
%%
%% 记录检查点的执行上下文和图状态信息。
%% 与 checkpoint 记录配合使用，checkpoint 存储状态数据，
%% checkpoint_metadata 存储执行过程的元信息。
%%
%% == 字段说明 ==
%%
%% 1. 执行阶段信息：
%%    - checkpoint_type: 检查点类型（initial/step/error/interrupt/final 等）
%%    - step: Pregel 超步编号，表示图计算的迭代次数
%%
%% 2. 图顶点状态：
%%    - active_vertices: 当前活跃的顶点列表（正在执行或待执行）
%%    - completed_vertices: 已完成执行的顶点列表
%%
%% 3. 执行标识：
%%    - run_id: 单次图执行的唯一标识，用于区分不同的执行实例
%%    - agent_id: 执行此图的 Agent 标识
%%    - iteration: Graph 层的迭代次数（区别于 Pregel 层的 step）
%%
%% 4. 扩展信息：
%%    - metadata: 用户自定义的元数据，可存储任意键值对
%%
%% == 与 checkpoint 记录的关系 ==
%%
%% checkpoint_metadata 中不重复存储以下信息（已在 checkpoint 中）：
%% - thread_id: 使用 checkpoint.thread_id
%% - timestamp: 使用 checkpoint.timestamp
%% - superstep: 使用 step 字段
%%
-record(checkpoint_metadata, {
    %%--------------------------------------------------------------------
    %% 执行阶段信息
    %%--------------------------------------------------------------------

    %% 检查点类型
    %%
    %% 标识检查点在图执行过程中的产生阶段：
    %% - initial: 图执行开始前的初始状态
    %% - input: 接收到新输入时
    %% - step: 正常超步执行后
    %% - loop: 循环迭代中
    %% - update: 状态更新时
    %% - interrupt: 执行被中断时（如需要人工介入）
    %% - error: 执行出错时
    %% - final: 图执行完成后的最终状态
    %% - branch: 从其他检查点分支创建
    %% - undefined: 未指定
    checkpoint_type :: initial | input | step | loop | update | interrupt | error | final | branch | undefined,

    %% Pregel 超步编号
    %%
    %% 表示当前处于 Pregel 计算的第几个超步。
    %% 超步是 Pregel 模型中的基本计算单元，每个超步中：
    %% 1. 所有活跃顶点并行执行计算
    %% 2. 顶点之间通过消息传递通信
    %% 3. 超步结束时同步状态
    step = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------
    %% 图顶点状态
    %%--------------------------------------------------------------------

    %% 活跃顶点列表
    %%
    %% 记录检查点创建时正在执行或等待执行的顶点。
    %% 用于：
    %% - 从检查点恢复时确定需要继续执行的顶点
    %% - 调试和监控图执行进度
    %% - 分析图的执行路径
    active_vertices = [] :: [atom()],

    %% 已完成顶点列表
    %%
    %% 记录检查点创建时已完成执行的顶点。
    %% 用于：
    %% - 从检查点恢复时跳过已完成的顶点
    %% - 追踪图的执行历史
    %% - 计算执行进度百分比
    completed_vertices = [] :: [atom()],

    %%--------------------------------------------------------------------
    %% 执行标识
    %%--------------------------------------------------------------------

    %% 图执行唯一标识（Run ID）
    %%
    %% 每次调用 graph:run/3 时生成的唯一标识。
    %% 用于：
    %% - 区分同一线程中的不同执行实例
    %% - 关联同一次执行产生的多个检查点
    %% - 日志追踪和调试
    %%
    %% 格式：UUID v4，如 "550e8400-e29b-41d4-a716-446655440000"
    run_id :: binary() | undefined,

    %% Agent 标识
    %%
    %% 执行此图的 Agent 的唯一标识。
    %% 用于：
    %% - 多 Agent 系统中追踪执行来源
    %% - 权限控制和审计
    %% - 关联 Agent 配置和状态
    agent_id :: binary() | undefined,

    %% Agent 名称
    %%
    %% 执行此图的 Agent 的人类可读名称。
    %% 用于：
    %% - 日志和调试时的可读性
    %% - UI 展示
    %% - 多 Agent 系统中的标识
    agent_name :: binary() | undefined,

    %% Graph 层迭代次数
    %%
    %% 区别于 Pregel 层的 step（超步）：
    %% - step: Pregel 内部的超步计数
    %% - iteration: Graph 层的外部迭代计数
    %%
    %% 当图包含循环结构时，iteration 记录循环执行的次数。
    %% 例如：ReAct 模式中，每次"思考-行动-观察"循环 iteration 加 1。
    iteration = 0 :: non_neg_integer(),

    %%--------------------------------------------------------------------
    %% 扩展信息
    %%--------------------------------------------------------------------

    %% 用户自定义元数据
    %%
    %% 允许用户存储任意键值对，用于：
    %% - 业务相关的标签和分类
    %% - 调试信息
    %% - 与外部系统集成的数据
    %%
    %% 示例：
    %% #{
    %%   <<"user_id">> => <<"u123">>,
    %%   <<"request_id">> => <<"req-456">>,
    %%   <<"tags">> => [<<"important">>, <<"reviewed">>]
    %% }
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
