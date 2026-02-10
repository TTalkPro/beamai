%%%-------------------------------------------------------------------
%%% @doc 图流程控制主模块 (LangGraph 风格，Pregel 执行引擎)
%%%
%%% 提供图执行框架的统一 API 入口。
%%% 整合构建器、执行器和状态管理。
%%%
%%% 基于 LangGraph 思想实现，使用 Pregel 作为底层执行引擎:
%%% - 节点: 转换状态的函数
%%% - 边: 控制流程走向 (直接或条件)
%%% - 状态: 在图中流动的不可变数据
%%% - 超步: 每次节点执行周期
%%%
%%% 执行模式:
%%% - run/2,3: 批量执行，使用 Pregel BSP 模型
%%% - run_sync/2,3: 同步执行，支持 HIL（中断+恢复）
%%% - start/2,3: 启动受监管的长期运行进程
%%%
%%% 使用示例:
%%%
%%% 方式一: 声明式 DSL (推荐)
%%% <pre>
%%% {ok, Graph} = graph:build([
%%%     {node, process, ProcessFun},
%%%     {edge, process, '__end__'},
%%%     {entry, process}
%%% ]),
%%% Result = graph:run(Graph, graph:context(#{input => Data})).
%%% </pre>
%%%
%%% 方式二: 命令式 Builder
%%% <pre>
%%% B0 = graph:builder(),
%%% B1 = graph:add_node(B0, process, ProcessFun),
%%% B2 = graph:add_edge(B1, process, '__end__'),
%%% B3 = graph:set_entry(B2, process),
%%% {ok, Graph} = graph:compile(B3).
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph).

%% DSL API (声明式)
-export([build/1, build/2]).

%% 构建器 API (命令式)
-export([builder/0, builder/1]).
-export([add_node/3, add_node/4]).
-export([add_edge/3]).
-export([add_fanout_edge/3]).
-export([add_conditional_edge/3, add_conditional_edge/4]).
-export([set_entry/2]).
-export([compile/1]).

%% 执行 API
-export([run/2, run/3]).

%% Sync API（HIL 支持）
-export([run_sync/2, run_sync/3]).

%% Runtime API（长期运行进程）
-export([start/2, start/3]).
-export([restore/2, restore/3]).
-export([resume/2, retry/2]).
-export([get_status/1, snapshot/1, stop/1]).

%% Context API (便捷重导出)
-export([context/0, context/1]).
-export([get/2, get/3, set/3]).

%% 便捷函数
-export([start_node/0, end_node/0]).

%% 类型定义
-type graph() :: beamai_graph_builder:graph().
-type builder() :: beamai_graph_builder:builder().
-type context() :: beamai_context:t().
-type node_id() :: beamai_graph_node:node_id().
-type node_fun() :: beamai_graph_node:node_fun().
-type router_fun() :: beamai_graph_edge:router_fun().
-type route_map() :: beamai_graph_edge:route_map().
-type run_options() :: beamai_graph_runner:run_options().
-type run_result() :: beamai_graph_runner:run_result().

-export_type([graph/0, builder/0, context/0, node_id/0, node_fun/0,
              router_fun/0, route_map/0, run_options/0, run_result/0]).

%%====================================================================
%% DSL API (声明式)
%%====================================================================

%% @doc 从 DSL 元素列表构建图
%% 支持的元素:
%% - {node, Name, Fun}           节点
%% - {edge, From, To}            直接边
%% - {fanout, From, Targets}     扇出边
%% - {conditional_edge, From, Fun} 条件边
%% - {entry, Node}               入口节点
-spec build([beamai_graph_dsl:dsl_element()]) -> {ok, graph()} | {error, term()}.
build(Specs) ->
    beamai_graph_dsl:build(Specs).

%% @doc 从 DSL 元素列表构建图 (带默认配置)
-spec build([beamai_graph_dsl:dsl_element()], map()) -> {ok, graph()} | {error, term()}.
build(Specs, Config) ->
    beamai_graph_dsl:build(Specs, Config).

%%====================================================================
%% 构建器 API (命令式)
%%====================================================================

%% @doc 创建新构建器
-spec builder() -> builder().
builder() ->
    beamai_graph_builder:new().

%% @doc 创建带配置的构建器
-spec builder(map()) -> builder().
builder(Config) ->
    beamai_graph_builder:new(Config).

%% @doc 添加节点
-spec add_node(builder(), node_id(), node_fun()) -> builder().
add_node(Builder, Id, Fun) ->
    beamai_graph_builder:add_node(Builder, Id, Fun).

%% @doc 添加带元数据的节点
-spec add_node(builder(), node_id(), node_fun(), map()) -> builder().
add_node(Builder, Id, Fun, Metadata) ->
    beamai_graph_builder:add_node(Builder, Id, Fun, Metadata).

%% @doc 添加直接边
-spec add_edge(builder(), node_id(), node_id()) -> builder().
add_edge(Builder, From, To) ->
    beamai_graph_builder:add_edge(Builder, From, To).

%% @doc 添加扇出边 (并行分发到多个目标)
-spec add_fanout_edge(builder(), node_id(), [node_id()]) -> builder().
add_fanout_edge(Builder, From, Targets) ->
    beamai_graph_builder:add_fanout_edge(Builder, From, Targets).

%% @doc 添加条件边 (路由函数)
-spec add_conditional_edge(builder(), node_id(), router_fun()) -> builder().
add_conditional_edge(Builder, From, RouterFun) ->
    beamai_graph_builder:add_conditional_edge(Builder, From, RouterFun).

%% @doc 添加条件边 (路由映射)
-spec add_conditional_edge(builder(), node_id(), router_fun(), route_map()) -> builder().
add_conditional_edge(Builder, From, RouterFun, RouteMap) ->
    beamai_graph_builder:add_conditional_edge(Builder, From, RouterFun, RouteMap).

%% @doc 设置入口节点
-spec set_entry(builder(), node_id()) -> builder().
set_entry(Builder, NodeId) ->
    beamai_graph_builder:set_entry(Builder, NodeId).

%% @doc 编译图
-spec compile(builder()) -> {ok, graph()} | {error, term()}.
compile(Builder) ->
    beamai_graph_builder:compile(Builder).

%%====================================================================
%% 执行 API
%%====================================================================

%% @doc 运行图
-spec run(graph(), context()) -> run_result().
run(Graph, InitialState) ->
    beamai_graph_runner:run_graph(Graph, InitialState).

%% @doc 运行图，带选项
-spec run(graph(), context(), run_options()) -> run_result().
run(Graph, InitialState, Options) ->
    beamai_graph_runner:run_graph(Graph, InitialState, Options).

%%====================================================================
%% Sync API（HIL 支持）
%%====================================================================

%% @doc 同步运行图，支持 HIL（中断+恢复）
%%
%% 返回值：
%% - {ok, FinalState}: 正常完成
%% - {interrupted, InterruptedVertices, Snapshot}: 中断，可恢复
%% - {error, Reason}: 错误
-spec run_sync(graph(), context()) ->
    {ok, context()} | {interrupted, term(), beamai_graph_state:snapshot()} | {error, term()}.
run_sync(Graph, InitialState) ->
    run_sync(Graph, InitialState, #{}).

%% @doc 同步运行图（带选项），支持 HIL
%%
%% 恢复执行时传入 snapshot 和 resume_data：
%%   run_sync(Graph, Ctx, #{snapshot => Snapshot, resume_data => Data})
%%
%% Opts 支持的选项：
%% - snapshot: 之前中断时返回的快照
%% - resume_data: #{VertexId => Data}，恢复中断所需的数据
%% - field_reducers: 字段 reducer 映射
%% - max_supersteps: 最大超步数
-spec run_sync(graph(), context(), map()) ->
    {ok, context()} | {interrupted, term(), beamai_graph_state:snapshot()} | {error, term()}.
run_sync(Graph, InitialState, Opts) ->
    case maps:get(snapshot, Opts, undefined) of
        undefined ->
            run_sync_fresh(Graph, InitialState, Opts);
        Snapshot ->
            ResumeData = maps:get(resume_data, Opts, #{}),
            run_sync_resume(Graph, Snapshot, ResumeData, Opts)
    end.

%%====================================================================
%% Runtime API（长期运行进程）
%%====================================================================

%% @doc 启动受监管的图执行进程
-spec start(graph(), context()) -> {ok, pid()} | {error, term()}.
start(Graph, InitialState) ->
    start(Graph, InitialState, #{}).

%% @doc 启动受监管的图执行进程（带选项）
%%
%% Opts 支持的选项：
%% - caller: pid()，完成/中断/错误时通知的进程
%% - store: {Module, Ref}，存储后端（用于自动 snapshot）
%% - snapshot_policy: map()，snapshot 策略配置
%% - field_reducers: 字段 reducer 映射
%% - max_supersteps: 最大超步数
-spec start(graph(), context(), map()) -> {ok, pid()} | {error, term()}.
start(Graph, InitialState, Opts) ->
    beamai_graph_sup:start_runtime(Graph, InitialState, Opts).

%% @doc 从快照恢复图执行（启动受监管进程）
-spec restore(beamai_graph_state:snapshot(), graph()) ->
    {ok, pid()} | {error, term()}.
restore(Snapshot, Graph) ->
    restore(Snapshot, Graph, #{}).

%% @doc 从快照恢复图执行（带选项）
-spec restore(beamai_graph_state:snapshot(), graph(), map()) ->
    {ok, pid()} | {error, term()}.
restore(Snapshot, Graph, Opts) ->
    case beamai_graph_state:restore_from_snapshot(Snapshot, Graph) of
        {ok, Restored} ->
            beamai_graph_sup:start_runtime(Graph, beamai_context:new(),
                Opts#{restore_from => Restored});
        {error, _} = Error ->
            Error
    end.

%% @doc 恢复中断的图执行
-spec resume(pid(), #{beamai_graph_engine:vertex_id() => term()}) ->
    ok | {error, term()}.
resume(Pid, ResumeData) ->
    beamai_graph_runtime:resume(Pid, ResumeData).

%% @doc 重试失败的顶点
-spec retry(pid(), [beamai_graph_engine:vertex_id()]) ->
    ok | {error, term()}.
retry(Pid, VertexIds) ->
    beamai_graph_runtime:retry(Pid, VertexIds).

%% @doc 获取图执行状态
-spec get_status(pid()) -> {ok, map()}.
get_status(Pid) ->
    beamai_graph_runtime:get_status(Pid).

%% @doc 获取图执行快照
-spec snapshot(pid()) -> {ok, beamai_graph_state:snapshot()}.
snapshot(Pid) ->
    beamai_graph_runtime:snapshot(Pid).

%% @doc 停止图执行进程
-spec stop(pid()) -> ok.
stop(Pid) ->
    beamai_graph_runtime:stop(Pid).

%%====================================================================
%% Context API (便捷重导出)
%%====================================================================

%% @doc 创建空上下文
-spec context() -> context().
context() ->
    beamai_context:new().

%% @doc 从 Map 创建上下文
-spec context(map()) -> context().
context(Map) ->
    beamai_context:new(Map).

%% @doc 获取上下文值
-spec get(context(), atom()) -> term().
get(Ctx, Key) ->
    beamai_context:get(Ctx, Key).

%% @doc 获取上下文值，带默认值
-spec get(context(), atom(), term()) -> term().
get(Ctx, Key, Default) ->
    beamai_context:get(Ctx, Key, Default).

%% @doc 设置上下文值
-spec set(context(), atom(), term()) -> context().
set(Ctx, Key, Value) ->
    beamai_context:set(Ctx, Key, Value).

%%====================================================================
%% 便捷函数
%%====================================================================

%% @doc 获取起始节点 ID
-spec start_node() -> node_id().
start_node() ->
    '__start__'.

%% @doc 获取终止节点 ID
-spec end_node() -> node_id().
end_node() ->
    '__end__'.

%%====================================================================
%% 内部函数 - run_sync
%%====================================================================

%% @private 首次执行
run_sync_fresh(Graph, InitialState, Opts) ->
    #{pregel_graph := PregelGraph} = Graph,
    ComputeFn = beamai_graph_compute:compute_fn(),
    FieldReducers = maps:get(field_reducers, Opts, #{}),

    EngineOpts = #{
        max_supersteps => maps:get(max_supersteps, Opts, 100),
        context => InitialState,
        field_reducers => FieldReducers
    },

    {ok, Engine} = beamai_graph_engine:new(PregelGraph, ComputeFn, EngineOpts),
    run_sync_engine(Engine).

%% @private 从快照恢复并执行
%%
%% 将 resume_data 注入到 restore_opts 的 context 中，
%% 并将 resume 的顶点加入 pending_activations，
%% 这样引擎初始化后就能直接重新执行这些顶点。
run_sync_resume(Graph, Snapshot, ResumeData, Opts) ->
    case beamai_graph_state:restore_from_snapshot(Snapshot, Graph) of
        {ok, Restored} ->
            #{pregel_graph := PregelGraph} = Graph,
            ComputeFn = beamai_graph_compute:compute_fn(),
            FieldReducers = maps:get(field_reducers, Opts, #{}),

            %% 将 resume 顶点加入 pending_activations
            ExistingActivations = maps:get(pending_activations, Restored, []),
            ResumeIds = maps:keys(ResumeData),
            AllActivations = lists:usort(ExistingActivations ++ ResumeIds),

            %% 重新激活需要 resume 的顶点
            Vertices0 = maps:get(vertices, Restored, #{}),
            Vertices1 = lists:foldl(
                fun(VId, Acc) ->
                    case maps:get(VId, Acc, undefined) of
                        undefined -> Acc;
                        V -> maps:put(VId, beamai_pregel_vertex:activate(V), Acc)
                    end
                end,
                Vertices0,
                ResumeIds
            ),

            Restored1 = Restored#{
                pending_activations => AllActivations,
                vertices => Vertices1,
                resume_data => ResumeData
            },

            {ok, Engine} = beamai_graph_engine:from_restored(Restored1,
                #{pregel_graph => PregelGraph,
                  compute_fn => ComputeFn,
                  field_reducers => FieldReducers}),

            run_sync_engine(Engine);
        {error, _} = Error ->
            Error
    end.

%% @private 运行引擎并处理结果
run_sync_engine(Engine) ->
    case beamai_graph_engine:run(Engine) of
        {ok, Engine1} ->
            case beamai_graph_engine:current_state(Engine1) of
                completed ->
                    {ok, beamai_graph_engine:context(Engine1)};
                interrupted ->
                    Info = beamai_graph_engine:last_info(Engine1),
                    Reason = maps:get(interrupted_vertices, Info, []),
                    Snapshot = beamai_graph_engine:take_snapshot(Engine1),
                    {interrupted, Reason, Snapshot};
                error ->
                    Info = beamai_graph_engine:last_info(Engine1),
                    {error, maps:get(failed_vertices, Info, [])};
                idle ->
                    {ok, beamai_graph_engine:context(Engine1)}
            end
    end.

