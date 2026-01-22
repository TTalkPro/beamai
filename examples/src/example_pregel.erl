%%%-------------------------------------------------------------------
%%% @doc Pregel Example - 并行 BSP/Pregel 使用示例（全局状态模式）
%%%
%%% 演示:
%%% 1. 简单消息传递测试
%%% 2. PageRank 算法
%%% 3. SSSP (单源最短路径)
%%% 4. 连通分量
%%%
%%% 全局状态模式说明：
%%% - Master 持有 global_state，Worker 是纯计算单元
%%% - 计算函数返回 #{delta => Map, outbox => List, status => ok}
%%% - delta 是增量更新，通过 field_reducers 合并到 global_state
%%% - 顶点只是拓扑结构（id + edges），不含 value
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_pregel).

-export([
    %% 示例运行
    run_all/0,
    run_pagerank/0,
    run_sssp/0,
    run_connected_components/0,

    %% 简单测试
    simple_test/0
]).

%%====================================================================
%% 简单测试
%%====================================================================

%% @doc 简单的并行执行测试（全局状态模式）
simple_test() ->
    io:format("=== Simple Pregel Test (Global State Mode) ===~n"),

    %% 创建一个简单的图: A -> B -> C
    %% 全局状态模式：顶点只包含拓扑结构（id + edges）
    G0 = pregel:new_graph(),
    G1 = pregel:add_vertex(G0, a, []),
    G2 = pregel:add_vertex(G1, b, []),
    G3 = pregel:add_vertex(G2, c, []),
    G4 = pregel:add_edge(G3, a, b),
    G5 = pregel:add_edge(G4, b, c),

    io:format("Graph created with ~p vertices~n", [pregel:vertex_count(G5)]),

    %% 定义简单的计算函数（全局状态模式）
    %% 每个顶点向邻居发送自己的值，并更新 global_state
    ComputeFn = fun(Ctx) ->
        #{vertex_id := Id, superstep := Superstep, messages := Messages,
          global_state := State, vertex := Vertex} = Ctx,

        %% 从 global_state 获取当前值
        Value = graph_state:get(State, Id, 0),
        Neighbors = pregel_vertex:neighbors(Vertex),

        io:format("  Superstep ~p: Vertex ~p, Value=~p, Messages=~p~n",
                  [Superstep, Id, Value, Messages]),

        case Superstep >= 2 of
            true ->
                %% 停止计算，不更新状态
                #{delta => #{}, outbox => [], status => ok};
            false ->
                %% 计算新值（当前值 + 收到消息之和）
                NewValue = case Messages of
                    [] -> Value;
                    _ -> Value + lists:sum(Messages)
                end,
                %% 发送消息给邻居
                Outbox = [{N, NewValue} || N <- Neighbors],
                %% 返回 delta 更新
                Delta = #{Id => NewValue},
                #{delta => Delta, outbox => Outbox, status => ok}
        end
    end,

    %% 初始全局状态
    InitialState = #{a => 1, b => 2, c => 3},

    %% 运行 Pregel
    io:format("~nStarting Pregel execution...~n"),
    Result = pregel:run(G5, ComputeFn, #{
        max_supersteps => 5,
        num_workers => 2,
        global_state => InitialState
    }),

    io:format("~nResult: ~p~n", [maps:get(status, Result)]),
    io:format("Supersteps: ~p~n", [maps:get(supersteps, Result)]),

    %% 显示最终全局状态
    FinalState = pregel:get_result_global_state(Result),
    io:format("Final global_state: ~p~n", [FinalState]),

    Result.

%%====================================================================
%% 运行所有示例
%%====================================================================

run_all() ->
    io:format("~n========================================~n"),
    io:format("    Erlang Pregel Examples~n"),
    io:format("    (Global State Mode)~n"),
    io:format("========================================~n~n"),

    simple_test(),
    io:format("~n"),

    run_pagerank(),
    io:format("~n"),

    run_sssp(),
    io:format("~n"),

    run_connected_components(),

    io:format("~n========================================~n"),
    io:format("    All examples completed!~n"),
    io:format("========================================~n"),
    ok.

%%====================================================================
%% PageRank 示例
%%====================================================================

run_pagerank() ->
    io:format("=== PageRank Example ===~n"),

    %% 创建一个小型网页图
    %%     A ----> B
    %%     |       |
    %%     v       v
    %%     C <---- D
    %%     |
    %%     v
    %%     A (cycle back)

    Edges = [
        {a, b},
        {a, c},
        {b, d},
        {d, c},
        {c, a}
    ],
    Graph = pregel:from_edges(Edges),

    io:format("Graph created with ~p vertices~n", [pregel:vertex_count(Graph)]),

    %% 运行 PageRank
    Result = pregel_algorithms:run_pagerank(Graph, #{
        max_iters => 10,
        damping => 0.85,
        num_workers => 2
    }),

    io:format("Result: ~p~n", [maps:get(status, Result)]),
    io:format("Supersteps: ~p~n", [maps:get(supersteps, Result)]),

    %% 显示 PageRank 值（从 global_state 获取）
    Values = pregel:get_result_global_state(Result),
    io:format("PageRank values:~n"),
    maps:foreach(
        fun(V, Rank) when is_number(Rank) ->
            io:format("  ~p: ~.4f~n", [V, Rank]);
           (_, _) -> ok
        end,
        Values
    ),

    Result.

%%====================================================================
%% SSSP 示例
%%====================================================================

run_sssp() ->
    io:format("=== SSSP (Shortest Path) Example ===~n"),

    %% 创建带权图
    %%     A --1--> B --2--> C
    %%     |                 ^
    %%     3                 |
    %%     |                 1
    %%     v                 |
    %%     D -------4------->+

    Edges = [
        {a, b, 1},
        {b, c, 2},
        {a, d, 3},
        {d, c, 4}
    ],
    Graph = pregel:from_edges(Edges),

    io:format("Graph created with ~p vertices~n", [pregel:vertex_count(Graph)]),
    io:format("Finding shortest paths from vertex 'a'...~n"),

    %% 从 a 出发找最短路径
    Result = pregel_algorithms:run_sssp(Graph, a),

    io:format("Result: ~p~n", [maps:get(status, Result)]),
    io:format("Supersteps: ~p~n", [maps:get(supersteps, Result)]),

    %% 显示距离（从 global_state 获取）
    Values = pregel:get_result_global_state(Result),
    io:format("Shortest distances from 'a':~n"),
    maps:foreach(
        fun(V, Dist) when is_number(Dist); Dist =:= infinity ->
            DistStr = case Dist of
                infinity -> "infinity";
                _ -> io_lib:format("~p", [Dist])
            end,
            io:format("  ~p: ~s~n", [V, DistStr]);
           (_, _) -> ok
        end,
        Values
    ),

    Result.

%%====================================================================
%% 连通分量示例
%%====================================================================

run_connected_components() ->
    io:format("=== Connected Components Example ===~n"),

    %% 创建有两个连通分量的图
    %% 分量 1: A -- B -- C
    %% 分量 2: D -- E

    Edges = [
        {a, b},
        {b, a},
        {b, c},
        {c, b},
        {d, e},
        {e, d}
    ],
    Graph = pregel:from_edges(Edges),

    io:format("Graph created with ~p vertices~n", [pregel:vertex_count(Graph)]),

    %% 运行连通分量算法
    Result = pregel_algorithms:run_connected_components(Graph),

    io:format("Result: ~p~n", [maps:get(status, Result)]),
    io:format("Supersteps: ~p~n", [maps:get(supersteps, Result)]),

    %% 显示组件（从 global_state 获取）
    Values = pregel:get_result_global_state(Result),
    io:format("Connected components:~n"),
    maps:foreach(
        fun(V, Component) when is_atom(V) ->
            io:format("  ~p: component ~p~n", [V, Component]);
           (_, _) -> ok
        end,
        Values
    ),

    Result.
