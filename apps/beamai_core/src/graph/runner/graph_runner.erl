%%%-------------------------------------------------------------------
%%% @doc 图执行引擎模块
%%%
%%% 使用 Pregel 分布式图计算引擎执行图。
%%%
%%% 全局状态模式：
%%% - Master 持有 global_state，Worker 只负责计算
%%% - 节点返回 delta（增量更新）而不是完整状态
%%% - 使用 field_reducers 按字段合并 delta
%%% - 支持延迟提交：出错时暂存 delta，不 apply
%%%
%%% 执行模式:
%%% - 简单模式: 不提供 snapshot 选项时，直接使用 graph_executor:run
%%% - Snapshot 模式: 提供 restore_from 或 store 时，使用步进式 API
%%%   - interrupt/error 自动返回给调用者
%%%   - 支持从 snapshot 恢复（restore_from + resume_data/retry_vertices）
%%%   - 支持 store 持久化（store + snapshot_strategy）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_runner).

%% API 导出
-export([run/2, run/3]).
%% 供 graph_snapshot 使用
-export([handle_pregel_result/3]).

%% 类型定义
-type graph() :: graph_builder:graph().
-type state() :: graph_state:state().

%% Snapshot 数据类型（包含执行器层状态）
%% 全局状态模式：状态在 global_state 中，不在顶点 value 中
%%
%% Snapshot 类型说明：
%% - initial: 超步 0 执行前的初始状态
%% - step: 正常超步完成
%% - error: 超步完成但有失败的顶点
%% - interrupt: 超步完成但有中断的顶点（human-in-the-loop）
%% - final: 执行结束
-type snapshot_data() :: #{
    type := graph_executor:snapshot_type(),              %% snapshot 类型
    pregel_snapshot := graph_executor:snapshot_data(), %% 执行器 snapshot
    global_state := state(),                       %% 当前全局状态
    iteration := non_neg_integer()                 %% 当前迭代次数
}.

%% Snapshot 策略类型
-type snapshot_strategy() ::
    every_superstep |                        %% 每个超步保存
    {every_n, pos_integer()} |               %% 每 N 个超步保存
    on_interrupt |                           %% 仅在中断时保存
    on_error.                                %% 仅在错误时保存

%% Store 配置类型
-type store_config() :: {module(), beamai_graph_store_behaviour:store_ref()}.

-type run_options() :: #{
    workers => pos_integer(),        %% Pregel worker 数量 (默认 1)
    max_iterations => pos_integer(), %% 最大迭代次数
    timeout => pos_integer(),        %% 超时时间 (毫秒)
    %% Snapshot 恢复选项
    restore_from => snapshot_data(),              %% 从 snapshot 恢复
    resume_data => #{graph_executor:vertex_id() => term()},  %% interrupt 恢复时注入的用户数据
    retry_vertices => [graph_executor:vertex_id()],           %% error 重试：重新激活指定顶点
    run_id => binary(),                      %% 外部传入的执行 ID
    %% 全局状态选项
    global_state => state(),                 %% 初始全局状态
    field_reducers => graph_executor:field_reducers(),   %% 字段级 Reducer 配置
    %% Store 相关选项（简化使用）
    store => store_config(),                 %% 存储后端配置 {Module, Ref}
    snapshot_strategy => snapshot_strategy(), %% Snapshot 策略
    graph_name => atom() | binary()          %% 图名称（用于存储分类）
}.

-export_type([snapshot_strategy/0, store_config/0]).

-export_type([snapshot_data/0]).

-type run_result() :: #{
    status := completed | interrupted | error | max_iterations,
    final_state := state(),
    iterations := non_neg_integer(),
    error => term(),
    done_reason => graph_executor:done_reason(),  %% snapshot 模式下的完成原因
    snapshot => graph_executor:snapshot_data(),    %% 中断/错误时的 snapshot（用于恢复）
    interrupted_vertices => [{graph_executor:vertex_id(), term()}],  %% 中断的顶点列表
    failed_vertices => [{graph_executor:vertex_id(), term()}]        %% 失败的顶点列表
}.

-export_type([run_options/0, run_result/0]).

%%====================================================================
%% 主执行 API
%%====================================================================

%% @doc 运行图，使用初始状态
-spec run(graph(), state()) -> run_result().
run(Graph, InitialState) ->
    run(Graph, InitialState, #{}).

%% @doc 运行图，使用初始状态和选项
%%
%% 使用 Pregel 引擎执行图，返回执行结果。
%%
%% 全局状态模式：
%% - global_state: 初始全局状态（如果未提供，使用 InitialState）
%% - field_reducers: 字段级 Reducer 配置
%%
%% 执行模式:
%% - 简单模式: 不提供 snapshot 选项时，直接使用 graph_executor:run
%% - Snapshot 模式: 提供 restore_from 或 store 时，使用步进式 API
-spec run(graph(), state(), run_options()) -> run_result().
run(Graph, InitialState, Options) ->
    %% 如果未提供 global_state，使用 InitialState
    OptionsWithState = ensure_global_state(Options, InitialState),
    case needs_snapshot_mode(OptionsWithState) of
        true ->
            run_with_snapshot(Graph, InitialState, OptionsWithState);
        false ->
            run_simple(Graph, InitialState, OptionsWithState)
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 确保 Options 中有 global_state
-spec ensure_global_state(run_options(), state()) -> run_options().
ensure_global_state(Options, InitialState) ->
    case maps:is_key(global_state, Options) of
        true -> Options;
        false -> Options#{global_state => InitialState}
    end.

%% @private 检查是否需要 snapshot 模式
%% 有 restore_from 或 store 选项时进入 snapshot 模式
-spec needs_snapshot_mode(run_options()) -> boolean().
needs_snapshot_mode(Options) ->
    maps:is_key(restore_from, Options) orelse
    maps:is_key(store, Options).

%% @private 简单执行模式（无 snapshot）
%%
%% 节点计算逻辑和路由边已存储在顶点中（扁平化结构），无需额外配置
-spec run_simple(graph(), state(), run_options()) -> run_result().
run_simple(Graph, InitialState, Options) ->
    #{pregel_graph := PregelGraph} = Graph,

    %% 准备执行选项（max_iterations 直接从 Graph 获取）
    MaxIterations = maps:get(max_iterations, Graph, 100),
    GlobalState = maps:get(global_state, Options, InitialState),
    FieldReducers = maps:get(field_reducers, Options, #{}),

    ExecutorOpts = #{
        max_supersteps => maps:get(max_supersteps, Options, MaxIterations),
        global_state => GlobalState,
        field_reducers => FieldReducers
    },

    %% 使用全局计算函数执行
    ComputeFn = graph_compute:compute_fn(),
    Result = graph_executor:run(PregelGraph, ComputeFn, ExecutorOpts),

    %% 提取结果
    ExecutorResult = graph_compute:from_pregel_result(Result),
    handle_pregel_result(ExecutorResult, InitialState, Options).

%% @private Snapshot 执行模式（使用步进式 API）
%%
%% 委托给 graph_snapshot 模块处理。
%% interrupt/error 时自动返回结果，无需回调。
-spec run_with_snapshot(graph(), state(), run_options()) -> run_result().
run_with_snapshot(Graph, InitialState, Options) ->
    ActualGlobalState = maps:get(global_state, Options, InitialState),
    graph_snapshot:run_with_snapshot(Graph, InitialState, ActualGlobalState, Options).

%% @private 处理 Pregel 引擎执行结果
-spec handle_pregel_result({ok, state()} | {error, term()}, state(), run_options()) -> run_result().
handle_pregel_result({ok, FinalState}, _InitialState, _Options) ->
    #{status => completed, final_state => FinalState, iterations => 0};
handle_pregel_result({error, {partial_result, PartialState, max_iterations_exceeded}}, _InitialState, Options) ->
    MaxIter = maps:get(max_iterations, Options, 100),
    #{status => max_iterations, final_state => PartialState, iterations => MaxIter};
handle_pregel_result({error, {partial_result, PartialState, Reason}}, _InitialState, _Options) ->
    #{status => error, final_state => PartialState, iterations => 0, error => Reason};
handle_pregel_result({error, max_iterations_exceeded}, InitialState, Options) ->
    MaxIter = maps:get(max_iterations, Options, 100),
    #{status => max_iterations, final_state => InitialState, iterations => MaxIter, error => max_iterations_exceeded};
handle_pregel_result({error, Reason}, InitialState, _Options) ->
    #{status => error, final_state => InitialState, iterations => 0, error => Reason}.
