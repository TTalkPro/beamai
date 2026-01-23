%%%-------------------------------------------------------------------
%%% @doc 图流控制单元测试 (EUnit)
%%%
%%% 针对 Pregel 风格图执行框架的简单 EUnit 测试。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试生成器
%%====================================================================

graph_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"状态测试", fun state_tests/0},
      {"节点测试", fun node_tests/0},
      {"边测试", fun edge_tests/0},
      {"构建器测试", fun builder_tests/0},
      {"引擎测试", fun engine_tests/0},
      {"集成测试", fun integration_tests/0},
      {"Pregel 集成测试", fun pregel_integration_tests/0},
      {"双引擎测试", fun dual_engine_tests/0},
      %% 新增迁移验证测试
      {"graph_compute 模块测试", fun graph_compute_tests/0},
      {"Pregel 图直接生成测试", fun pregel_graph_generation_tests/0},
      {"Fanout 并行执行测试", fun fanout_parallel_tests/0},
      {"状态合并测试", fun state_merge_tests/0},
      {"错误处理测试", fun error_handling_tests/0},
      {"复杂工作流测试", fun complex_workflow_tests/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% 状态测试
%%====================================================================

state_tests() ->
    %% 新建空状态
    S0 = graph_state:new(),
    ?assertEqual(#{}, S0),
    ?assert(graph_state:is_empty(S0)),

    %% 使用 map 新建状态
    S1 = graph_state:new(#{name => <<"test">>, count => 42}),
    ?assertEqual(<<"test">>, graph_state:get(S1, name)),
    ?assertEqual(42, graph_state:get(S1, count)),

    %% 使用默认值获取
    ?assertEqual(undefined, graph_state:get(S0, missing)),
    ?assertEqual(default, graph_state:get(S0, missing, default)),

    %% 设置和获取
    S2 = graph_state:set(S0, key, value),
    ?assertEqual(value, graph_state:get(S2, key)),

    %% 更新
    S3 = graph_state:new(#{count => 0}),
    S4 = graph_state:update(S3, count, fun(V) -> V + 1 end),
    ?assertEqual(1, graph_state:get(S4, count)),

    %% 合并
    SA = graph_state:new(#{a => 1, b => 2}),
    SB = graph_state:new(#{b => 20, c => 3}),
    SM = graph_state:merge(SA, SB),
    ?assertEqual(1, graph_state:get(SM, a)),
    ?assertEqual(20, graph_state:get(SM, b)),
    ?assertEqual(3, graph_state:get(SM, c)),

    ok.

%%====================================================================
%% 节点测试
%%====================================================================

node_tests() ->
    %% 创建节点
    Fun = fun(S, _) -> {ok, S} end,
    Node = graph_node:new(test_node, Fun),
    ?assertEqual(test_node, graph_node:id(Node)),
    ?assert(graph_node:is_valid(Node)),

    %% 执行节点
    AddOne = fun(State, _) ->
        Count = graph_state:get(State, count, 0),
        {ok, graph_state:set(State, count, Count + 1)}
    end,
    AddNode = graph_node:new(add_one, AddOne),
    State0 = graph_state:new(#{count => 5}),
    {ok, State1} = graph_node:execute(AddNode, State0),
    ?assertEqual(6, graph_state:get(State1, count)),

    %% 特殊节点
    StartNode = graph_node:start_node(),
    EndNode = graph_node:end_node(),
    ?assert(graph_node:is_start(StartNode)),
    ?assert(graph_node:is_end(EndNode)),

    %% 错误处理
    ErrorFun = fun(_S, _) -> {error, some_reason} end,
    ErrorNode = graph_node:new(error_node, ErrorFun),
    {error, {node_error, error_node, some_reason}} =
        graph_node:execute(ErrorNode, graph_state:new()),

    ok.

%%====================================================================
%% 边测试
%%====================================================================

edge_tests() ->
    %% 直接边
    Edge1 = graph_edge:direct(node_a, node_b),
    ?assertEqual(node_a, graph_edge:from(Edge1)),
    ?assertEqual(node_b, graph_edge:target(Edge1)),
    ?assert(graph_edge:is_direct(Edge1)),
    ?assertNot(graph_edge:is_conditional(Edge1)),

    %% 条件边
    Router = fun(State) ->
        case graph_state:get(State, condition) of
            true -> node_true;
            false -> node_false
        end
    end,
    Edge2 = graph_edge:conditional(node_a, Router),
    ?assert(graph_edge:is_conditional(Edge2)),

    %% 边解析
    {ok, node_b} = graph_edge:resolve(Edge1, graph_state:new()),

    StateTrue = graph_state:new(#{condition => true}),
    StateFalse = graph_state:new(#{condition => false}),
    {ok, node_true} = graph_edge:resolve(Edge2, StateTrue),
    {ok, node_false} = graph_edge:resolve(Edge2, StateFalse),

    ok.

%%====================================================================
%% 构建器测试
%%====================================================================

builder_tests() ->
    NodeFun = fun(S, _) -> {ok, S} end,

    %% 基本构建
    B0 = graph_builder:new(),
    B1 = graph_builder:add_node(B0, my_node, NodeFun),
    B2 = graph_builder:add_edge(B1, my_node, '__end__'),
    B3 = graph_builder:set_entry(B2, my_node),
    ?assertEqual(ok, graph_builder:validate(B3)),

    %% 验证错误
    {error, entry_not_set} = graph_builder:validate(graph_builder:new()),

    B4 = graph_builder:set_entry(graph_builder:new(), nonexistent),
    {error, {entry_node_not_found, nonexistent}} = graph_builder:validate(B4),

    %% 编译（简化后的图结构只包含 pregel_graph, entry, max_iterations）
    {ok, Graph} = graph_builder:compile(B3),
    ?assert(is_map(Graph)),
    ?assert(maps:is_key(pregel_graph, Graph)),
    ?assert(maps:is_key(entry, Graph)),
    ?assert(maps:is_key(max_iterations, Graph)),

    ok.

%%====================================================================
%% 引擎测试
%%====================================================================

engine_tests() ->
    %% 简单流程
    ProcessFun = fun(State, _) ->
        {ok, graph_state:set(State, processed, true)}
    end,

    B1 = graph:builder(),
    B2 = graph:add_node(B1, process, ProcessFun),
    B3 = graph:add_edge(B2, process, '__end__'),
    B4 = graph:set_entry(B3, process),
    {ok, Graph1} = graph:compile(B4),

    Result1 = graph:run(Graph1, graph:state(#{})),
    ?assertEqual(completed, maps:get(status, Result1)),
    ?assertEqual(true, graph:get(maps:get(final_state, Result1), processed)),

    %% 条件流程
    CheckFun = fun(State, _) ->
        Value = graph:get(State, value, 0),
        {ok, graph:set(State, is_high, Value > 50)}
    end,
    HighFun = fun(State, _) -> {ok, graph:set(State, result, high)} end,
    LowFun = fun(State, _) -> {ok, graph:set(State, result, low)} end,

    Router = fun(State) ->
        case graph:get(State, is_high) of
            true -> high_handler;
            false -> low_handler
        end
    end,

    C1 = graph:builder(),
    C2 = graph:add_node(C1, check, CheckFun),
    C3 = graph:add_node(C2, high_handler, HighFun),
    C4 = graph:add_node(C3, low_handler, LowFun),
    C5 = graph:add_conditional_edge(C4, check, Router),
    C6 = graph:add_edge(C5, high_handler, '__end__'),
    C7 = graph:add_edge(C6, low_handler, '__end__'),
    C8 = graph:set_entry(C7, check),
    {ok, Graph2} = graph:compile(C8),

    HighResult = graph:run(Graph2, graph:state(#{value => 100})),
    ?assertEqual(high, graph:get(maps:get(final_state, HighResult), result)),

    LowResult = graph:run(Graph2, graph:state(#{value => 10})),
    ?assertEqual(low, graph:get(maps:get(final_state, LowResult), result)),

    %% 循环流程
    LoopFun = fun(State, _) ->
        Count = graph:get(State, count, 0),
        {ok, graph:set(State, count, Count + 1)}
    end,

    LoopRouter = fun(State) ->
        case graph:get(State, count) < 5 of
            true -> loop;
            false -> '__end__'
        end
    end,

    L1 = graph:builder(),
    L2 = graph:add_node(L1, loop, LoopFun),
    L3 = graph:add_conditional_edge(L2, loop, LoopRouter),
    L4 = graph:set_entry(L3, loop),
    {ok, Graph3} = graph:compile(L4),

    LoopResult = graph:run(Graph3, graph:state(#{count => 0})),
    ?assertEqual(completed, maps:get(status, LoopResult)),
    ?assertEqual(5, graph:get(maps:get(final_state, LoopResult), count)),

    %% 最大迭代次数
    InfiniteRouter = fun(_State) -> infinite_loop end,

    M1 = graph:builder(#{max_iterations => 10}),
    M2 = graph:add_node(M1, infinite_loop, LoopFun),
    M3 = graph:add_conditional_edge(M2, infinite_loop, InfiniteRouter),
    M4 = graph:set_entry(M3, infinite_loop),
    {ok, Graph4} = graph:compile(M4),

    MaxResult = graph:run(Graph4, graph:state(#{})),
    ?assertEqual(max_iterations, maps:get(status, MaxResult)),

    ok.

%%====================================================================
%% 集成测试
%%====================================================================

integration_tests() ->
    %% 线性工作流: A -> B -> C -> END
    NodeA = fun(S, _) -> {ok, graph:set(S, history, [a | graph:get(S, history, [])])} end,
    NodeB = fun(S, _) -> {ok, graph:set(S, history, [b | graph:get(S, history, [])])} end,
    NodeC = fun(S, _) -> {ok, graph:set(S, history, [c | graph:get(S, history, [])])} end,

    I1 = graph:builder(),
    I2 = graph:add_node(I1, node_a, NodeA),
    I3 = graph:add_node(I2, node_b, NodeB),
    I4 = graph:add_node(I3, node_c, NodeC),
    I5 = graph:add_edge(I4, node_a, node_b),
    I6 = graph:add_edge(I5, node_b, node_c),
    I7 = graph:add_edge(I6, node_c, '__end__'),
    I8 = graph:set_entry(I7, node_a),
    {ok, Graph1} = graph:compile(I8),

    Result1 = graph:run(Graph1, graph:state(#{})),
    ?assertEqual(completed, maps:get(status, Result1)),
    History = graph:get(maps:get(final_state, Result1), history),
    ?assertEqual([c, b, a], History),

    %% 类 Agent 流程
    ParseFun = fun(State, _) ->
        Input = graph:get(State, input, <<>>),
        Intent = case binary:match(Input, <<"search">>) of
            nomatch -> chat;
            _ -> search
        end,
        {ok, graph:set(State, intent, Intent)}
    end,

    ToolRouter = fun(State) ->
        case graph:get(State, intent) of
            search -> use_tool;
            _ -> respond
        end
    end,

    ToolFun = fun(State, _) ->
        {ok, graph:set(State, tool_result, <<"Found results">>)}
    end,

    RespondFun = fun(State, _) ->
        ToolResult = graph:get(State, tool_result, undefined),
        Response = case ToolResult of
            undefined -> <<"Hello!">>;
            _ -> <<"Based on search: ", ToolResult/binary>>
        end,
        {ok, graph:set(State, response, Response)}
    end,

    A1 = graph:builder(),
    A2 = graph:add_node(A1, parse, ParseFun),
    A3 = graph:add_node(A2, use_tool, ToolFun),
    A4 = graph:add_node(A3, respond, RespondFun),
    A5 = graph:add_conditional_edge(A4, parse, ToolRouter),
    A6 = graph:add_edge(A5, use_tool, respond),
    A7 = graph:add_edge(A6, respond, '__end__'),
    A8 = graph:set_entry(A7, parse),
    {ok, Graph2} = graph:compile(A8),

    %% 测试使用工具的情况
    R1 = graph:run(Graph2, graph:state(#{input => <<"please search for erlang">>})),
    Response1 = graph:get(maps:get(final_state, R1), response),
    ?assert(binary:match(Response1, <<"Based on search">>) =/= nomatch),

    %% 测试不使用工具的情况
    R2 = graph:run(Graph2, graph:state(#{input => <<"hello">>})),
    Response2 = graph:get(maps:get(final_state, R2), response),
    ?assertEqual(<<"Hello!">>, Response2),

    ok.

%%====================================================================
%% Pregel 集成测试
%%====================================================================

pregel_integration_tests() ->
    %% 测试 Pregel 图直接生成
    ProcessFun = fun(State, _) ->
        Value = graph:get(State, value, 0),
        {ok, graph:set(State, value, Value * 2)}
    end,

    B1 = graph:builder(),
    B2 = graph:add_node(B1, double, ProcessFun),
    B3 = graph:add_edge(B2, double, '__end__'),
    B4 = graph:set_entry(B3, double),
    {ok, Graph} = graph:compile(B4),

    %% 验证 Pregel 图已直接生成
    #{pregel_graph := PregelGraph} = Graph,
    ?assert(is_map(PregelGraph)),

    %% 验证顶点已创建
    Vertices = pregel:vertices(PregelGraph),
    VertexIds = [pregel_vertex:id(V) || V <- Vertices],
    ?assert(lists:member('__start__', VertexIds)),
    ?assert(lists:member('__end__', VertexIds)),
    ?assert(lists:member(double, VertexIds)),

    %% 测试全局计算函数
    ComputeFn = graph_compute:compute_fn(),
    ?assert(is_function(ComputeFn, 1)),

    ok.

%%====================================================================
%% 双引擎测试
%%====================================================================

dual_engine_tests() ->
    %% 构建简单图
    DoubleFun = fun(State, _) ->
        Value = graph:get(State, value, 0),
        {ok, graph:set(State, value, Value * 2)}
    end,

    B1 = graph:builder(),
    B2 = graph:add_node(B1, double, DoubleFun),
    B3 = graph:add_edge(B2, double, '__end__'),
    B4 = graph:set_entry(B3, double),
    {ok, Graph} = graph:compile(B4),

    InitState = graph:state(#{value => 5}),

    %% 测试 Pregel 引擎
    PregelResult = graph:run(Graph, InitState),
    ?assertEqual(completed, maps:get(status, PregelResult)),
    ?assertEqual(10, graph:get(maps:get(final_state, PregelResult), value)),

    %% 测试 Pregel 引擎的条件流程
    CheckFun = fun(State, _) ->
        Value = graph:get(State, input, 0),
        {ok, graph:set(State, is_large, Value > 100)}
    end,
    LargeFun = fun(State, _) -> {ok, graph:set(State, result, large)} end,
    SmallFun = fun(State, _) -> {ok, graph:set(State, result, small)} end,

    Router = fun(State) ->
        case graph:get(State, is_large) of
            true -> handle_large;
            false -> handle_small
        end
    end,

    C1 = graph:builder(),
    C2 = graph:add_node(C1, check, CheckFun),
    C3 = graph:add_node(C2, handle_large, LargeFun),
    C4 = graph:add_node(C3, handle_small, SmallFun),
    C5 = graph:add_conditional_edge(C4, check, Router),
    C6 = graph:add_edge(C5, handle_large, '__end__'),
    C7 = graph:add_edge(C6, handle_small, '__end__'),
    C8 = graph:set_entry(C7, check),
    {ok, ConditionalGraph} = graph:compile(C8),

    %% 测试条件路由 - 大值
    PregLarge = graph:run(ConditionalGraph, graph:state(#{input => 200})),
    ?assertEqual(large, graph:get(maps:get(final_state, PregLarge), result)),

    %% 测试条件路由 - 小值
    PregSmall = graph:run(ConditionalGraph, graph:state(#{input => 50})),
    ?assertEqual(small, graph:get(maps:get(final_state, PregSmall), result)),

    ok.

%%====================================================================
%% graph_compute 模块测试
%%====================================================================

graph_compute_tests() ->
    %% 测试 compute_fn/0 返回函数
    ComputeFn = graph_compute:compute_fn(),
    ?assert(is_function(ComputeFn, 1)),

    %% 构建一个简单图用于测试
    ProcessFun = fun(State, _) ->
        Value = graph:get(State, value, 0),
        {ok, graph:set(State, value, Value + 1)}
    end,

    B1 = graph:builder(),
    B2 = graph:add_node(B1, process, ProcessFun),
    B3 = graph:add_edge(B2, process, '__end__'),
    B4 = graph:set_entry(B3, process),
    {ok, Graph} = graph:compile(B4),

    #{pregel_graph := PregelGraph} = Graph,
    InitialState = graph:state(#{value => 10}),

    %% 验证 Pregel 图结构正确
    ?assert(is_map(PregelGraph)),

    %% 验证 __start__ 顶点存在并包含扁平化结构
    StartVertex = pregel:get_graph_vertex(PregelGraph, '__start__'),
    ?assertNotEqual(undefined, StartVertex),
    %% 扁平化模式：直接访问 fun_, metadata, routing_edges
    ?assert(pregel_vertex:fun_(StartVertex) =/= undefined),
    ?assert(is_list(pregel_vertex:routing_edges(StartVertex))),

    %% 测试 from_pregel_result/1 - 通过完整执行流程验证
    Result = graph:run(Graph, InitialState),
    ?assertEqual(completed, maps:get(status, Result)),
    FinalState = maps:get(final_state, Result),
    ?assertEqual(11, graph:get(FinalState, value)),

    ok.

%%====================================================================
%% Pregel 图直接生成测试
%%====================================================================

pregel_graph_generation_tests() ->
    %% 测试 compile/1 直接生成 Pregel 图
    NodeA = fun(S, _) -> {ok, graph:set(S, a_done, true)} end,
    NodeB = fun(S, _) -> {ok, graph:set(S, b_done, true)} end,

    B1 = graph:builder(),
    B2 = graph:add_node(B1, node_a, NodeA),
    B3 = graph:add_node(B2, node_b, NodeB),
    B4 = graph:add_edge(B3, node_a, node_b),
    B5 = graph:add_edge(B4, node_b, '__end__'),
    B6 = graph:set_entry(B5, node_a),
    {ok, Graph} = graph:compile(B6),

    %% 验证 Graph 简化结构（只包含 pregel_graph, entry, max_iterations）
    ?assert(maps:is_key(pregel_graph, Graph)),
    ?assert(maps:is_key(entry, Graph)),
    ?assert(maps:is_key(max_iterations, Graph)),
    %% 不再包含 nodes, edges, config（已整合到 pregel_graph 的顶点中）

    #{pregel_graph := PregelGraph} = Graph,

    %% 验证所有顶点存在
    Vertices = pregel:vertices(PregelGraph),
    VertexIds = [pregel_vertex:id(V) || V <- Vertices],
    ?assert(lists:member('__start__', VertexIds)),
    ?assert(lists:member('__end__', VertexIds)),
    ?assert(lists:member(node_a, VertexIds)),
    ?assert(lists:member(node_b, VertexIds)),

    %% 验证扁平化顶点结构 - 直接包含 fun_, metadata, routing_edges
    NodeAVertex = pregel:get_graph_vertex(PregelGraph, node_a),
    ?assert(pregel_vertex:fun_(NodeAVertex) =/= undefined),
    ?assert(is_map(pregel_vertex:metadata(NodeAVertex))),

    %% 验证路由边已嵌入顶点
    NodeAEdges = pregel_vertex:routing_edges(NodeAVertex),
    ?assert(length(NodeAEdges) > 0),

    %% 验证 __start__ 顶点有到入口节点的路由边
    StartVertex = pregel:get_graph_vertex(PregelGraph, '__start__'),
    StartEdges = pregel_vertex:routing_edges(StartVertex),
    ?assert(length(StartEdges) > 0),
    [StartEdge | _] = StartEdges,
    ?assertEqual(node_a, graph_edge:target(StartEdge)),

    ok.

%%====================================================================
%% Fanout 并行执行测试
%%====================================================================

fanout_parallel_tests() ->
    %% 测试 fanout 边实现的并行执行
    ExpertA = fun(S, _) ->
        {ok, graph:set(S, expert_a_result, <<"A分析完成">>)}
    end,
    ExpertB = fun(S, _) ->
        {ok, graph:set(S, expert_b_result, <<"B分析完成">>)}
    end,
    ExpertC = fun(S, _) ->
        {ok, graph:set(S, expert_c_result, <<"C分析完成">>)}
    end,
    Synthesize = fun(S, _) ->
        ResultA = graph:get(S, expert_a_result, <<>>),
        ResultB = graph:get(S, expert_b_result, <<>>),
        ResultC = graph:get(S, expert_c_result, <<>>),
        Summary = <<ResultA/binary, " | ", ResultB/binary, " | ", ResultC/binary>>,
        {ok, graph:set(S, summary, Summary)}
    end,

    B1 = graph:builder(),
    B2 = graph:add_node(B1, expert_a, ExpertA),
    B3 = graph:add_node(B2, expert_b, ExpertB),
    B4 = graph:add_node(B3, expert_c, ExpertC),
    B5 = graph:add_node(B4, synthesize, Synthesize),

    %% 使用 fanout 边从 __start__ 并行分发到三个专家
    B6 = graph:add_fanout_edge(B5, '__start__', [expert_a, expert_b, expert_c]),

    %% 所有专家连接到 synthesize
    B7 = graph:add_edge(B6, expert_a, synthesize),
    B8 = graph:add_edge(B7, expert_b, synthesize),
    B9 = graph:add_edge(B8, expert_c, synthesize),
    B10 = graph:add_edge(B9, synthesize, '__end__'),

    %% 设置入口为 __start__ 的下一跳（通过 fanout）
    %% 注意：fanout 从 __start__ 出发，所以需要设置一个虚拟入口或直接用 fanout
    %% 实际上我们需要一个 dispatcher 节点
    Dispatcher = fun(S, _) -> {ok, S} end,
    B11 = graph:add_node(B10, dispatcher, Dispatcher),
    B12 = graph:set_entry(B11, dispatcher),
    B13 = graph:add_fanout_edge(B12, dispatcher, [expert_a, expert_b, expert_c]),

    {ok, Graph} = graph:compile(B13),

    %% 执行并验证
    Result = graph:run(Graph, graph:state(#{input => <<"测试输入">>})),
    ?assertEqual(completed, maps:get(status, Result)),

    FinalState = maps:get(final_state, Result),
    %% 验证所有专家结果都存在（通过状态合并）
    ?assertEqual(<<"A分析完成">>, graph:get(FinalState, expert_a_result)),
    ?assertEqual(<<"B分析完成">>, graph:get(FinalState, expert_b_result)),
    ?assertEqual(<<"C分析完成">>, graph:get(FinalState, expert_c_result)),

    %% 验证综合结果
    Summary = graph:get(FinalState, summary),
    ?assert(binary:match(Summary, <<"A分析完成">>) =/= nomatch),
    ?assert(binary:match(Summary, <<"B分析完成">>) =/= nomatch),
    ?assert(binary:match(Summary, <<"C分析完成">>) =/= nomatch),

    ok.

%%====================================================================
%% 状态合并测试
%%====================================================================

state_merge_tests() ->
    %% 测试并行分支的状态合并
    BranchA = fun(S, _) ->
        {ok, graph:set(S, branch_a, <<"result_a">>)}
    end,
    BranchB = fun(S, _) ->
        {ok, graph:set(S, branch_b, <<"result_b">>)}
    end,
    Merge = fun(S, _) ->
        A = graph:get(S, branch_a, <<"missing">>),
        B = graph:get(S, branch_b, <<"missing">>),
        {ok, graph:set(S, merged, <<A/binary, "+", B/binary>>)}
    end,

    B1 = graph:builder(),
    B2 = graph:add_node(B1, branch_a, BranchA),
    B3 = graph:add_node(B2, branch_b, BranchB),
    B4 = graph:add_node(B3, merge_node, Merge),

    %% 分发节点
    Dispatch = fun(S, _) -> {ok, S} end,
    B5 = graph:add_node(B4, dispatch, Dispatch),
    B6 = graph:set_entry(B5, dispatch),
    B7 = graph:add_fanout_edge(B6, dispatch, [branch_a, branch_b]),
    B8 = graph:add_edge(B7, branch_a, merge_node),
    B9 = graph:add_edge(B8, branch_b, merge_node),
    B10 = graph:add_edge(B9, merge_node, '__end__'),

    {ok, Graph} = graph:compile(B10),

    Result = graph:run(Graph, graph:state(#{initial => true})),
    ?assertEqual(completed, maps:get(status, Result)),

    FinalState = maps:get(final_state, Result),
    %% 两个分支的结果应该都被合并
    ?assertEqual(<<"result_a">>, graph:get(FinalState, branch_a)),
    ?assertEqual(<<"result_b">>, graph:get(FinalState, branch_b)),
    ?assertEqual(<<"result_a+result_b">>, graph:get(FinalState, merged)),

    ok.

%%====================================================================
%% 错误处理测试
%%====================================================================

error_handling_tests() ->
    %% 测试节点执行错误
    ErrorNode = fun(_S, _) ->
        {error, simulated_error}
    end,

    B1 = graph:builder(),
    B2 = graph:add_node(B1, error_node, ErrorNode),
    B3 = graph:add_edge(B2, error_node, '__end__'),
    B4 = graph:set_entry(B3, error_node),
    {ok, Graph} = graph:compile(B4),

    Result = graph:run(Graph, graph:state(#{})),
    %% 错误应该被正确捕获
    ?assertEqual(error, maps:get(status, Result)),

    %% 测试最大迭代次数限制
    LoopNode = fun(S, _) ->
        Count = graph:get(S, count, 0),
        {ok, graph:set(S, count, Count + 1)}
    end,
    InfiniteRouter = fun(_S) -> loop_node end,

    L1 = graph:builder(#{max_iterations => 5}),
    L2 = graph:add_node(L1, loop_node, LoopNode),
    L3 = graph:add_conditional_edge(L2, loop_node, InfiniteRouter),
    L4 = graph:set_entry(L3, loop_node),
    {ok, LoopGraph} = graph:compile(L4),

    LoopResult = graph:run(LoopGraph, graph:state(#{})),
    ?assertEqual(max_iterations, maps:get(status, LoopResult)),

    ok.

%%====================================================================
%% 复杂工作流测试
%%====================================================================

complex_workflow_tests() ->
    %% 模拟 ReAct Agent 工作流
    %% Parse -> Plan -> [Tool | Respond] -> Check -> [Plan | End]

    Parse = fun(S, _) ->
        Input = graph:get(S, input, <<>>),
        NeedsTool = binary:match(Input, <<"search">>) =/= nomatch,
        {ok, graph:set(S, needs_tool, NeedsTool)}
    end,

    Plan = fun(S, _) ->
        NeedsTool = graph:get(S, needs_tool, false),
        ToolCalls = graph:get(S, tool_calls, 0),
        Action = case {NeedsTool, ToolCalls} of
            {true, 0} -> use_tool;
            _ -> respond
        end,
        {ok, graph:set(S, action, Action)}
    end,

    Tool = fun(S, _) ->
        ToolCalls = graph:get(S, tool_calls, 0),
        S1 = graph:set(S, tool_result, <<"search_result">>),
        S2 = graph:set(S1, tool_calls, ToolCalls + 1),
        {ok, S2}
    end,

    Respond = fun(S, _) ->
        ToolResult = graph:get(S, tool_result, undefined),
        Response = case ToolResult of
            undefined -> <<"Hello!">>;
            Result -> <<"Based on search: ", Result/binary>>
        end,
        {ok, graph:set(S, response, Response)}
    end,

    Check = fun(S, _) ->
        ToolCalls = graph:get(S, tool_calls, 0),
        IsComplete = ToolCalls > 0 orelse graph:get(S, response) =/= undefined,
        {ok, graph:set(S, is_complete, IsComplete)}
    end,

    PlanRouter = fun(S) ->
        case graph:get(S, action) of
            use_tool -> tool;
            respond -> respond
        end
    end,

    CheckRouter = fun(S) ->
        case graph:get(S, is_complete) of
            true -> '__end__';
            false -> plan
        end
    end,

    B1 = graph:builder(#{max_iterations => 20}),
    B2 = graph:add_node(B1, parse, Parse),
    B3 = graph:add_node(B2, plan, Plan),
    B4 = graph:add_node(B3, tool, Tool),
    B5 = graph:add_node(B4, respond, Respond),
    B6 = graph:add_node(B5, check, Check),

    B7 = graph:add_edge(B6, parse, plan),
    B8 = graph:add_conditional_edge(B7, plan, PlanRouter),
    B9 = graph:add_edge(B8, tool, respond),
    B10 = graph:add_edge(B9, respond, check),
    B11 = graph:add_conditional_edge(B10, check, CheckRouter),

    B12 = graph:set_entry(B11, parse),
    {ok, Graph} = graph:compile(B12),

    %% 测试需要工具的情况
    ToolResult = graph:run(Graph, graph:state(#{input => <<"please search erlang">>})),
    ?assertEqual(completed, maps:get(status, ToolResult)),
    ToolFinalState = maps:get(final_state, ToolResult),
    ?assertEqual(1, graph:get(ToolFinalState, tool_calls)),
    ?assert(binary:match(graph:get(ToolFinalState, response), <<"Based on search">>) =/= nomatch),

    %% 测试不需要工具的情况
    NoToolResult = graph:run(Graph, graph:state(#{input => <<"hello">>})),
    ?assertEqual(completed, maps:get(status, NoToolResult)),
    NoToolFinalState = maps:get(final_state, NoToolResult),
    ?assertEqual(0, graph:get(NoToolFinalState, tool_calls, 0)),
    ?assertEqual(<<"Hello!">>, graph:get(NoToolFinalState, response)),

    ok.
