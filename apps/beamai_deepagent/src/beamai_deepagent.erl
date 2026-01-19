%%%-------------------------------------------------------------------
%%% @doc Graph Deep Agent - 基于图引擎的深度 Agent
%%%
%%% 实现 LangChain DeepAgents 风格的 Agent，构建在 beamai_graph 图引擎之上。
%%%
%%% 核心特性：
%%% - 图驱动执行：所有 Agent 逻辑用图节点和边表示
%%% - 动态图构建：create_plan 工具动态生成任务子图
%%% - 并行优先：使用 graph_send:fan_out 实现子任务并行
%%% - 状态即消息：使用 graph_state 替代 gen_server 状态
%%%
%%% 使用示例：
%%% <pre>
%%% %% 创建 LLM 配置
%%% LLM = llm_client:create(anthropic, #{
%%%     model =&gt; &lt;&lt;"glm-4.7"&gt;&gt;,
%%%     api_key =&gt; ApiKey,
%%%     base_url =&gt; &lt;&lt;"https://open.bigmodel.cn/api/anthropic"&gt;&gt;
%%% }),
%%%
%%% %% 创建 DeepAgent 配置
%%% Config = beamai_deepagent:new(#{
%%%     llm =&gt; LLM,
%%%     tools =&gt; [my_search_tool()],
%%%     max_depth =&gt; 3
%%% }),
%%%
%%% %% 运行 Agent
%%% {ok, Result} = beamai_deepagent:run(Config, &lt;&lt;"研究 Erlang 的历史"&gt;&gt;),
%%% Response = maps:get(response, Result).
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent).

%% 导入公共工具函数
-import(beamai_deepagent_utils, [
    extract_last_assistant_response/1,
    default_system_prompt/0
]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 创建 API
-export([new/0, new/1]).

%% 执行 API
-export([run/2, run/3]).
-export([stream/2, stream/3]).
-export([resume/2]).

%% 状态查询
-export([get_plan/1, get_trace/1]).

%% Checkpoint API
-export([save_checkpoint/1, save_checkpoint/2]).
-export([load_checkpoint/2, load_latest_checkpoint/1]).
-export([list_checkpoints/1, list_checkpoints/2]).
-export([restore_from_checkpoint/2]).
-export([export_state/1, import_state/2]).

%% 测试辅助（导出供测试使用）
-export([build_core_graph/1]).

%%====================================================================
%% 类型定义
%%====================================================================

-type config() :: #{
    llm => llm_config(),
    tools => [tool_spec()],
    max_depth => pos_integer(),
    max_iterations => pos_integer(),
    planning_enabled => boolean(),
    reflection_enabled => boolean(),
    depth => non_neg_integer(),
    system_prompt => binary(),
    %% Buffer 配置（滑动窗口、摘要压缩）
    buffer => buffer_config(),
    %% Checkpoint/存储相关
    enable_storage => boolean(),
    storage => {module(), pid()} | undefined,
    storage_opts => map(),
    auto_checkpoint => boolean(),
    restore_latest => boolean(),
    restore_checkpoint => binary()
}.

-type buffer_config() :: #{
    window_size => pos_integer(),    %% 滑动窗口大小（默认 20）
    max_tokens => pos_integer(),     %% 最大 Token 数（默认 4000）
    summarize => boolean(),          %% 是否启用摘要（默认 false）
    summarize_fn => function()       %% 自定义摘要函数
}.

-type llm_config() :: #{
    provider => atom(),
    model => binary(),
    api_key => binary(),
    timeout => pos_integer()
}.

-type tool_spec() :: #{
    name := binary(),
    description := binary(),
    parameters := map(),
    handler := function()
}.

-type run_result() :: #{
    status := completed | error | max_iterations,
    response := binary(),
    final_state := graph_state:state(),
    plan => beamai_deepagent_plan:t(),
    trace => beamai_deepagent_trace:t(),
    iterations := non_neg_integer()
}.

-export_type([config/0, llm_config/0, tool_spec/0, run_result/0]).

%%====================================================================
%% 创建 API
%%====================================================================

%% @doc 创建默认配置的 Agent
%%
%% 使用默认配置创建 Agent，适用于快速原型开发。
%% 默认配置使用 OpenAI GPT-4，启用计划和反思功能。
-spec new() -> config().
new() ->
    new(#{}).

%% @doc 创建自定义配置的 Agent
%%
%% 参数说明：
%% - llm: LLM 配置（provider, model, api_key 等）
%% - tools: 用户自定义工具列表
%% - max_depth: 最大递归深度（默认 3），用于控制子任务嵌套层级
%% - max_iterations: 最大迭代次数（默认 50），防止无限循环
%% - planning_enabled: 是否启用计划功能（默认 true）
%% - reflection_enabled: 是否启用反思功能（默认 true）
-spec new(map()) -> config().
new(Opts) ->
    DefaultConfig = default_config(),
    maps:merge(DefaultConfig, Opts).

%%====================================================================
%% 执行 API
%%====================================================================

%% @doc 运行 Agent
%%
%% 使用配置和用户消息运行 Agent。
%% 内部构建执行图，创建初始状态，然后启动图执行引擎。
-spec run(config(), binary()) -> {ok, run_result()} | {error, term()}.
run(Config, Message) ->
    run(Config, Message, #{}).

%% @doc 运行 Agent（带选项）
%%
%% 选项说明：
%% - engine: 执行引擎（默认 pregel）
%% - trace: 是否记录轨迹（默认 true）
%% - timeout: 超时时间毫秒（默认 300000）
%% - enable_storage: 是否启用存储（默认 false）
%% - auto_checkpoint: 是否自动保存 checkpoint（默认 false）
%% - restore_checkpoint: 恢复指定的检查点
%% - restore_latest: 恢复最新检查点
%%
%% 执行流程：
%% 1. 验证 LLM 配置
%% 2. 初始化存储（如果启用）
%% 3. 构建核心执行图
%% 4. 创建初始状态（包含用户消息）
%% 5. 恢复检查点（如果配置）
%% 6. 启动图执行引擎
%% 7. 自动保存 checkpoint（如果启用）
%% 8. 处理执行结果
-spec run(config(), binary(), map()) -> {ok, run_result()} | {error, term()}.
run(Config, Message, Opts) ->
    %% 1. 验证 LLM 配置
    case validate_llm_config(Config) of
        ok ->
            run_internal(Config, Message, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 运行 Agent 内部实现
-spec run_internal(config(), binary(), map()) -> {ok, run_result()} | {error, term()}.
run_internal(Config, Message, Opts) ->
    %% 1. 初始化存储（如果启用）
    Storage = beamai_deepagent_checkpoint:init_storage(<<"beamai_deepagent_temp">>, Config),

    %% 2. 构建图和创建初始状态
    {ok, Graph} = build_core_graph(Config),
    InitialState = create_initial_state(Config, Message),

    %% 3. 将存储添加到状态中
    StateWithStorage = maybe_add_storage(InitialState, Storage),

    %% 4. 恢复检查点（如果配置）
    RestoredState = beamai_deepagent_checkpoint:maybe_restore(Config, StateWithStorage),

    %% 5. 添加 auto_checkpoint 标志到状态
    FinalState = maybe_add_auto_checkpoint(RestoredState, Config),

    %% 6. 执行图
    RunOpts = build_run_options(Config, Opts),
    execute_graph_with_checkpoint(Graph, FinalState, RunOpts, Config).

%% @doc 流式运行 Agent
%%
%% 返回一个流式执行函数，可逐步获取执行结果。
%% 适用于需要实时显示执行进度的场景。
-spec stream(config(), binary()) -> fun().
stream(Config, Message) ->
    stream(Config, Message, #{}).

%% @doc 流式运行 Agent（带选项）
%%
%% 使用 graph:stream 启动流式执行。
-spec stream(config(), binary(), map()) -> fun().
stream(Config, Message, Opts) ->
    {ok, Graph} = build_core_graph(Config),
    InitialState = create_initial_state(Config, Message),
    graph:stream(Graph, InitialState, Opts).

%% @doc 恢复暂停的执行（异步模式）
%%
%% 当使用异步交互模式时，工具审批会返回 pending 状态。
%% 使用此函数提供用户响应并恢复执行。
%%
%% 参数说明：
%% - RequestId: 暂停时返回的请求 ID
%% - Response: 用户响应，如 #{approved => true}
%%
%% 注意：当前实现使用进程字典存储待处理请求，仅适用于同进程场景。
%% 生产环境应使用 ETS 或数据库持久化。
-spec resume(binary(), map()) -> {ok, resumed} | {error, term()}.
resume(RequestId, Response) ->
    case get_pending_request(RequestId) of
        {ok, _Request} ->
            %% 存储响应供后续查询
            store_response(RequestId, Response),
            {ok, resumed};
        not_found ->
            {error, {request_not_found, RequestId}}
    end.

%%====================================================================
%% 状态查询 API
%%====================================================================

%% @doc 从结果中获取执行计划
%%
%% 如果 Agent 创建了计划，返回计划数据结构；否则返回 undefined。
%% 计划包含目标和步骤列表，可用于分析 Agent 的执行策略。
-spec get_plan(run_result()) -> beamai_deepagent_plan:t() | undefined.
get_plan(#{final_state := State}) ->
    graph_state:get(State, plan, undefined);
get_plan(_) ->
    undefined.

%% @doc 从结果中获取执行轨迹
%%
%% 返回完整的执行轨迹记录。
%% 轨迹包含所有节点执行、工具调用、状态变更等信息。
-spec get_trace(run_result()) -> beamai_deepagent_trace:t().
get_trace(#{final_state := State}) ->
    graph_state:get(State, trace, beamai_deepagent_trace:new());
get_trace(_) ->
    beamai_deepagent_trace:new().

%%====================================================================
%% 图构建
%%====================================================================

%% @doc 构建核心 Agent 图
%%
%% 图结构说明：

%% __start__ → llm_node → [after_llm_router]
%%                ↑              │
%%                │     ┌────────┼────────┐
%%                │     ↓        ↓        ↓
%%                │  tool_node  end   reflect_node
%%                │     │                  │
%%                │     ↓                  │
%%                │  [after_tool_router]   │
%%                │     │                  │
%%                │  ┌──┴──┬────────┐      │
%%                │  ↓     ↓        ↓      │
%%                │ llm  fan_out   end     │
%%                │       │                │
%%                │       ↓                │
%%                │  task_executor         │
%%                │       │                │
%%                │       ↓                │
%%                │  aggregate_node        │
%%                │       │                │
%%                └───────┴────────────────┘

%%
%% 节点说明：
%% - llm_node: 调用 LLM 生成响应或工具调用
%% - tool_node: 执行工具调用
%% - reflect_node: 反思当前进展
%% - aggregate_node: 聚合并行子任务结果
%% - task_executor: 执行单个子任务
-spec build_core_graph(config()) -> {ok, graph:graph()}.
build_core_graph(Config) ->
    Nodes = create_graph_nodes(Config),
    MaxIterations = maps:get(max_iterations, Config, 50),
    graph:build(build_graph_spec(Nodes, MaxIterations)).

%%====================================================================
%% 私有函数 - 配置相关
%%====================================================================

%% @private 创建默认配置
%%
%% 默认值：
%% - LLM: 必须由用户通过 llm_client:create/2 提供
%% - 最大深度: 3 层
%% - 最大迭代: 50 次
%% - 启用计划和反思功能
%%
%% 注意：不提供默认 LLM 配置，用户必须使用 llm_client:create/2 创建配置
-spec default_config() -> config().
default_config() ->
    #{
        %% llm 必须由用户提供，通过 llm_client:create/2 创建
        tools => [],
        max_depth => 3,
        max_iterations => 50,
        planning_enabled => true,
        reflection_enabled => true,
        depth => 0,
        system_prompt => default_system_prompt()
    }.

%% @private 构建运行选项
%%
%% 合并配置和用户选项，设置图执行参数
-spec build_run_options(config(), map()) -> map().
build_run_options(Config, Opts) ->
    #{
        engine => maps:get(engine, Opts, pregel),
        trace => maps:get(trace, Opts, true),
        max_iterations => maps:get(max_iterations, Config, 50),
        timeout => maps:get(timeout, Opts, 300000)
    }.

%%====================================================================
%% 私有函数 - 图节点创建
%%====================================================================

%% @private 创建所有图节点
%%
%% 使用 beamai_deepagent_nodes 模块创建各个节点函数
-spec create_graph_nodes(config()) -> map().
create_graph_nodes(Config) ->
    #{
        llm_node => beamai_deepagent_nodes:make_llm_node(Config),
        tool_node => beamai_deepagent_nodes:make_tool_node(Config),
        reflect_node => beamai_deepagent_nodes:make_reflect_node(Config),
        aggregate_node => beamai_deepagent_nodes:make_aggregate_node(),
        task_executor => beamai_deepagent_nodes:make_task_executor(Config)
    }.

%% @private 构建图规格
%%
%% 返回图 DSL 规格列表，定义节点、边和路由规则。
%% 将长列表拆分为多个函数以提高可读性。
-spec build_graph_spec(map(), pos_integer()) -> [tuple()].
build_graph_spec(Nodes, MaxIterations) ->
    [config_section(MaxIterations)] ++
    node_definitions(Nodes) ++
    edge_definitions(Nodes) ++
    [{entry, llm_node}].

%% @private 配置段
%%
%% 设置图执行的全局配置。
-spec config_section(pos_integer()) -> tuple().
config_section(MaxIterations) ->
    {config, max_iterations, MaxIterations}.

%% @private 节点定义
%%
%% 定义所有图节点及其对应的处理函数。
-spec node_definitions(map()) -> [tuple()].
node_definitions(Nodes) ->
    [
        {node, llm_node, maps:get(llm_node, Nodes)},
        {node, tool_node, maps:get(tool_node, Nodes)},
        {node, reflect_node, maps:get(reflect_node, Nodes)},
        {node, aggregate_node, maps:get(aggregate_node, Nodes)},
        {node, task_executor, maps:get(task_executor, Nodes)}
    ].

%% @private 边定义
%%
%% 定义节点之间的连接关系和路由规则。
-spec edge_definitions(map()) -> [tuple()].
edge_definitions(_Nodes) ->
    [
        %% 条件边：LLM 后的路由决策
        %% 可能路由到 tool_node（执行工具）或 __end__（结束）
        {conditional_edge, llm_node, fun beamai_deepagent_router:after_llm/1},

        %% 条件边：工具执行后的路由决策
        %% 可能路由到 llm_node（继续）、fan_out（并行）或 __end__（结束）
        {conditional_edge, tool_node, fun beamai_deepagent_router:after_tool/1},

        %% 固定边：反思后回到 LLM 继续执行
        {edge, reflect_node, llm_node},

        %% 固定边：聚合后回到 LLM 处理汇总结果
        {edge, aggregate_node, llm_node},

        %% 固定边：任务执行完成后到聚合节点
        {edge, task_executor, aggregate_node}
    ].

%%====================================================================
%% 私有函数 - 状态创建
%%====================================================================

%% @private 创建初始状态
%%
%% 使用用户消息和配置初始化图状态
-spec create_initial_state(config(), binary()) -> graph_state:state().
create_initial_state(Config, Message) ->
    SystemPrompt = maps:get(system_prompt, Config, default_system_prompt()),
    graph_state:new(build_initial_state_map(Config, Message, SystemPrompt)).

%% @private 构建初始状态 map
%%
%% 初始化所有状态字段的默认值
-spec build_initial_state_map(config(), binary(), binary()) -> map().
build_initial_state_map(Config, Message, SystemPrompt) ->
    BaseState = #{
        %% 消息历史：以用户消息开始
        messages => [#{role => user, content => Message}],

        %% 系统提示：定义 Agent 行为
        system_prompt => SystemPrompt,

        %% 深度信息：用于控制子任务递归
        depth => maps:get(depth, Config, 0),
        max_depth => maps:get(max_depth, Config, 3),

        %% 计划和轨迹：执行过程记录
        plan => undefined,
        trace => beamai_deepagent_trace:new(),

        %% 工具相关：待执行和已执行的工具调用
        pending_tools => [],
        tool_results => [],

        %% 子任务：并行任务管理
        subtasks => [],
        subtask_results => [],

        %% 控制标志
        should_reflect => false,

        %% TodoList：简化的任务追踪
        todos => [],

        %% 配置引用：供节点函数访问
        config => Config
    },
    %% 初始化文件系统后端（如果配置了）
    maybe_init_filesystem(Config, BaseState).

%% @private 条件初始化文件系统后端
-spec maybe_init_filesystem(config(), map()) -> map().
maybe_init_filesystem(Config, State) ->
    case maps:get(filesystem, Config, undefined) of
        undefined ->
            State;
        FsConfig ->
            Backend = beamai_deepagent_fs_backend:new(FsConfig),
            State#{filesystem => Backend}
    end.

%%====================================================================
%% 私有函数 - 图执行
%%====================================================================

%% @private 执行图并处理结果
%%
%% 根据执行状态返回不同的结果：
%% - completed: 正常完成
%% - max_iterations: 达到最大迭代次数
%% - error: 执行出错
-spec execute_graph(graph:graph(), graph_state:state(), map()) ->
    {ok, run_result()} | {error, term()}.
execute_graph(Graph, InitialState, RunOpts) ->
    case graph:run(Graph, InitialState, RunOpts) of
        #{status := completed, final_state := FinalState} = Result ->
            {ok, build_result(Result, FinalState)};
        #{status := max_iterations, final_state := FinalState} = Result ->
            {ok, build_result(Result, FinalState)};
        #{status := error, error := Reason} ->
            {error, Reason};
        #{status := error} = Result ->
            %% 错误但没有 error 字段
            {error, {unknown_error, Result}};
        {error, Reason} ->
            %% 直接返回的错误元组
            {error, Reason};
        Other ->
            %% 未知格式
            {error, {unexpected_result, Other}}
    end.

%% @private 构建执行结果
%%
%% 从图执行结果和最终状态中提取所需信息
-spec build_result(map(), graph_state:state()) -> run_result().
build_result(GraphResult, FinalState) ->
    #{
        status => maps:get(status, GraphResult, completed),
        response => extract_response(FinalState),
        final_state => FinalState,
        plan => graph_state:get(FinalState, plan, undefined),
        trace => graph_state:get(FinalState, trace, beamai_deepagent_trace:new()),
        iterations => maps:get(iterations, GraphResult, 0)
    }.

%% @private 从状态中提取最终响应
%%
%% 优先使用 final_response 字段，否则从消息历史中提取最后的助手消息
-spec extract_response(graph_state:state()) -> binary().
extract_response(State) ->
    case graph_state:get(State, final_response, undefined) of
        undefined ->
            Messages = graph_state:get(State, messages, []),
            extract_last_assistant_response(Messages);
        Response ->
            Response
    end.

%%====================================================================
%% 私有函数 - 异步请求管理
%%====================================================================

%% @private 获取待处理请求
%%
%% 注意：当前使用进程字典实现，仅适用于同进程场景
-spec get_pending_request(binary()) -> {ok, map()} | not_found.
get_pending_request(RequestId) ->
    PendingRequests = get(pending_human_requests) orelse #{},
    case maps:get(RequestId, PendingRequests, not_found) of
        not_found -> not_found;
        Request -> {ok, Request}
    end.

%% @private 存储用户响应
%%
%% 存储用户对待处理请求的响应
-spec store_response(binary(), map()) -> ok.
store_response(RequestId, Response) ->
    Responses = get(human_responses) orelse #{},
    put(human_responses, maps:put(RequestId, Response, Responses)),
    ok.

%%====================================================================
%% Checkpoint API
%%====================================================================

%% @doc 保存当前状态为检查点
%%
%% 从 run_result 中提取 final_state 并保存为检查点。
-spec save_checkpoint(run_result(), map()) -> {ok, binary()} | {error, term()}.
save_checkpoint(Result, Meta) ->
    State = maps:get(final_state, Result),
    beamai_deepagent_checkpoint:save(Meta, State).

%% @doc 保存当前状态为检查点（简化版）
%%
%% 从 run_result 中提取 final_state 并保存为检查点，无需元数据。
-spec save_checkpoint(run_result()) -> {ok, binary()} | {error, term()}.
save_checkpoint(Result) ->
    save_checkpoint(Result, #{}).

%% @doc 加载指定检查点
%%
%% 从存储中加载指定的检查点数据。
-spec load_checkpoint(run_result(), binary()) ->
    {ok, beamai_deepagent_checkpoint:checkpoint_data()} | {error, term()}.
load_checkpoint(Result, CpId) ->
    State = maps:get(final_state, Result),
    beamai_deepagent_checkpoint:load(CpId, State).

%% @doc 加载最新检查点
%%
%% 从存储中加载最新的检查点数据。
-spec load_latest_checkpoint(run_result()) ->
    {ok, beamai_deepagent_checkpoint:checkpoint_data()} | {error, term()}.
load_latest_checkpoint(Result) ->
    State = maps:get(final_state, Result),
    beamai_deepagent_checkpoint:load_latest(State).

%% @doc 列出所有检查点
%%
%% 列出存储中的所有检查点。
-spec list_checkpoints(run_result()) -> {ok, [map()]} | {error, term()}.
list_checkpoints(Result) ->
    list_checkpoints(Result, #{}).

%% @doc 列出检查点（带过滤选项）
%%
%% 列出存储中的检查点，支持过滤选项。
-spec list_checkpoints(run_result(), map()) -> {ok, [map()]} | {error, term()}.
list_checkpoints(Result, Opts) ->
    State = maps:get(final_state, Result),
    beamai_deepagent_checkpoint:list(Opts, State).

%% @doc 从检查点恢复状态并继续执行
%%
%% 从指定的检查点恢复状态，并使用新的消息继续执行。
-spec restore_from_checkpoint(binary(), binary()) ->
    {ok, run_result()} | {error, term()}.
restore_from_checkpoint(CpId, Message) ->
    Config = #{
        restore_checkpoint => CpId,
        enable_storage => true
    },
    run(Config, Message).

%% @doc 导出状态（用于外部持久化）
%%
%% 将 final_state 导出为可序列化的 map。
-spec export_state(run_result()) -> map().
export_state(Result) ->
    State = maps:get(final_state, Result),
    #{
        %% 基础状态
        messages => graph_state:get(State, messages, []),
        system_prompt => graph_state:get(State, system_prompt, <<>>),

        %% Deep Agent 特有状态
        plan => graph_state:get(State, plan, undefined),
        trace => graph_state:get(State, trace, beamai_deepagent_trace:new()),
        subtasks => graph_state:get(State, subtasks, []),
        subtask_results => graph_state:get(State, subtask_results, []),

        %% 控制状态
        depth => graph_state:get(State, depth, 0),
        max_depth => graph_state:get(State, max_depth, 3),
        pending_tools => graph_state:get(State, pending_tools, []),
        tool_results => graph_state:get(State, tool_results, []),

        %% 元数据
        exported_at => erlang:system_time(millisecond)
    }.

%% @doc 从导出数据恢复状态并继续执行
%%
%% 从 export_state/1 导出的数据恢复状态，并使用新的消息继续执行。
-spec import_state(map(), binary()) -> {ok, run_result()} | {error, term()}.
import_state(ExportedData, Message) ->
    Config = #{
        system_prompt => maps:get(system_prompt, ExportedData, default_system_prompt()),
        depth => maps:get(depth, ExportedData, 0),
        max_depth => maps:get(max_depth, ExportedData, 3)
    },
    run(Config, Message).

%%====================================================================
%% 私有函数 - Checkpoint 支持
%%====================================================================

%% @private 添加存储到状态
-spec maybe_add_storage(graph_state:state(), {module(), pid()} | undefined) ->
    graph_state:state().
maybe_add_storage(State, undefined) ->
    State;
maybe_add_storage(State, Storage) ->
    graph_state:set(State, storage, Storage).

%% @private 添加自动检查点标志到状态
-spec maybe_add_auto_checkpoint(graph_state:state(), config()) -> graph_state:state().
maybe_add_auto_checkpoint(State, Config) ->
    AutoCheckpoint = maps:get(auto_checkpoint, Config, false),
    graph_state:set(State, auto_checkpoint, AutoCheckpoint).

%% @private 执行图并支持自动检查点
-spec execute_graph_with_checkpoint(graph:graph(), graph_state:state(), map(), config()) ->
    {ok, run_result()} | {error, term()}.
execute_graph_with_checkpoint(Graph, InitialState, RunOpts, _Config) ->
    case execute_graph(Graph, InitialState, RunOpts) of
        {ok, Result} ->
            %% 自动保存 checkpoint（如果启用）
            FinalState = maps:get(final_state, Result),
            _State = beamai_deepagent_checkpoint:maybe_auto_save(#{}, FinalState),
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 私有函数 - LLM 配置验证
%%====================================================================

%% @private 验证 LLM 配置
%%
%% LLM 配置必须通过 llm_client:create/2 创建，包含 '__llm_client__' => true 标记。
%% 这与 beamai_agent 的配置方式保持一致。
-spec validate_llm_config(config()) -> ok | {error, term()}.
validate_llm_config(Config) ->
    case maps:get(llm, Config, undefined) of
        undefined ->
            {error, {missing_llm_config,
                     <<"LLM config is required. "
                       "Create it using llm_client:create/2. "
                       "Example: LLM = llm_client:create(anthropic, #{model => <<\"glm-4.7\">>, ...})">>}};
        LLMConfig when is_map(LLMConfig) ->
            case llm_client:is_valid_config(LLMConfig) of
                true ->
                    ok;
                false ->
                    {error, {invalid_llm_config,
                             <<"LLM config must be created using llm_client:create/2. "
                               "Example: LLM = llm_client:create(anthropic, #{model => <<\"glm-4.7\">>, ...})">>}}
            end;
        _ ->
            {error, {invalid_llm_config, <<"llm config must be a map">>}}
    end.
