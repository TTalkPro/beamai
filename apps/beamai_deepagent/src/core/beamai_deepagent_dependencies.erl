%%%-------------------------------------------------------------------
%%% @doc Graph Deep Agent 依赖分析模块
%%%
%%% 负责计划步骤的依赖关系分析：
%%% - 构建依赖图：从步骤列表构建有向图
%%% - 拓扑分层：将步骤按依赖关系分层
%%% - 循环检测：处理循环依赖情况
%%%
%%% 算法说明：
%%% 使用 Kahn 算法变体实现拓扑排序，将步骤分为可并行的层级。
%%% 同一层的步骤之间没有依赖关系，可以安全地并行执行。
%%%
%%% 设计原则：
%%% - 纯函数：无副作用
%%% - 容错性：循环依赖回退为顺序执行
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_dependencies).

%%====================================================================
%% 导出 API
%%====================================================================

%% 主要 API
-export([
    analyze/1,
    topological_layers/1
]).

%% 辅助函数（供测试使用）
-export([
    build_graph/1,
    deps_satisfied/3
]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 依赖图节点：包含步骤数据和依赖列表
-type graph_node() :: #{step := map(), deps := [term()]}.
%% 依赖图：步骤 ID 到节点的映射
-type dep_graph() :: #{term() => graph_node()}.

-export_type([dep_graph/0]).

%%====================================================================
%% 主要 API
%%====================================================================

%% @doc 分析步骤依赖关系，返回可并行执行的分层
%%
%% 输入：步骤列表，每个步骤包含 id 和 dependencies 字段
%% 输出：层列表，同一层的步骤可以并行执行
%%
%% 示例：

%% Steps = [
%%   #{id => 1, dependencies => []},
%%   #{id => 2, dependencies => [1]},
%%   #{id => 3, dependencies => [1]},
%%   #{id => 4, dependencies => [2, 3]}
%% ],
%% analyze(Steps) -> [[Step1], [Step2, Step3], [Step4]]

-spec analyze([map()]) -> [[map()]].
analyze([]) ->
    [];
analyze(Steps) ->
    Graph = build_graph(Steps),
    topological_layers(Graph).

%% @doc 拓扑排序分层
%%
%% 将依赖图中的步骤按依赖关系分层。
%% 使用迭代方法，每次找出所有依赖已满足的步骤作为一层。
-spec topological_layers(dep_graph()) -> [[map()]].
topological_layers(Graph) ->
    AllIds = maps:keys(Graph),
    compute_layers(Graph, [], AllIds).

%%====================================================================
%% 图构建
%%====================================================================

%% @doc 从步骤列表构建依赖图
%%
%% 每个步骤被转换为图节点，包含原始步骤数据和依赖 ID 列表。
-spec build_graph([map()]) -> dep_graph().
build_graph(Steps) ->
    lists:foldl(fun add_step_to_graph/2, #{}, Steps).

%% @private 添加步骤到依赖图
-spec add_step_to_graph(map(), dep_graph()) -> dep_graph().
add_step_to_graph(Step, Graph) ->
    Id = maps:get(id, Step),
    Deps = maps:get(dependencies, Step, []),
    Graph#{Id => #{step => Step, deps => Deps}}.

%%====================================================================
%% 拓扑分层计算
%%====================================================================

%% @private 迭代计算分层
%%
%% 终止条件：没有剩余步骤
%% 迭代步骤：找出依赖已满足的步骤，加入当前层
-spec compute_layers(dep_graph(), [[map()]], [term()]) -> [[map()]].
compute_layers(_Graph, Layers, []) ->
    lists:reverse(Layers);
compute_layers(Graph, Layers, Remaining) ->
    Completed = extract_completed_ids(Layers),
    Ready = find_ready_ids(Remaining, Graph, Completed),
    process_ready_ids(Graph, Layers, Remaining, Ready).

%% @private 提取已完成的步骤 ID
-spec extract_completed_ids([[map()]]) -> [term()].
extract_completed_ids(Layers) ->
    lists:flatmap(fun extract_layer_ids/1, Layers).

%% @private 提取单层的步骤 ID
-spec extract_layer_ids([map()]) -> [term()].
extract_layer_ids(Layer) ->
    [maps:get(id, S) || S <- Layer].

%% @private 查找依赖已满足的步骤 ID
-spec find_ready_ids([term()], dep_graph(), [term()]) -> [term()].
find_ready_ids(Remaining, Graph, Completed) ->
    [Id || Id <- Remaining, deps_satisfied(Id, Graph, Completed)].

%% @private 处理就绪的步骤 ID
%%
%% 特殊情况：如果没有就绪步骤但还有剩余步骤，说明存在循环依赖。
%% 此时回退为顺序执行，强制取出第一个步骤单独成层。
-spec process_ready_ids(dep_graph(), [[map()]], [term()], [term()]) -> [[map()]].
process_ready_ids(Graph, Layers, Remaining, []) when Remaining =/= [] ->
    %% 循环依赖检测：回退为顺序执行
    handle_cyclic_dependency(Graph, Layers, Remaining);
process_ready_ids(_Graph, Layers, _Remaining, []) ->
    lists:reverse(Layers);
process_ready_ids(Graph, Layers, Remaining, Ready) ->
    ReadySteps = extract_steps(Graph, Ready),
    NewRemaining = Remaining -- Ready,
    compute_layers(Graph, [ReadySteps | Layers], NewRemaining).

%% @private 处理循环依赖
%%
%% 当检测到循环依赖时，强制取出第一个步骤单独执行。
%% 这样可以打破循环，避免死锁。
-spec handle_cyclic_dependency(dep_graph(), [[map()]], [term()]) -> [[map()]].
handle_cyclic_dependency(Graph, Layers, [FirstId | RestIds]) ->
    FirstStep = maps:get(step, maps:get(FirstId, Graph)),
    compute_layers(Graph, [[FirstStep] | Layers], RestIds).

%% @private 从图中提取步骤
-spec extract_steps(dep_graph(), [term()]) -> [map()].
extract_steps(Graph, Ids) ->
    [maps:get(step, maps:get(Id, Graph)) || Id <- Ids].

%%====================================================================
%% 依赖检查
%%====================================================================

%% @doc 检查步骤的依赖是否全部满足
%%
%% 如果步骤的所有依赖都在已完成列表中，则返回 true。
%% 如果步骤不存在于图中，视为无依赖，返回 true。
-spec deps_satisfied(term(), dep_graph(), [term()]) -> boolean().
deps_satisfied(Id, Graph, Completed) ->
    case maps:find(Id, Graph) of
        {ok, #{deps := Deps}} ->
            lists:all(fun(D) -> lists:member(D, Completed) end, Deps);
        error ->
            true
    end.
