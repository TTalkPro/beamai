%%%-------------------------------------------------------------------
%%% @doc Pregel Example - 并行 BSP/Pregel 使用示例
%%%
%%% 演示:
%%% 1. PageRank 算法
%%% 2. SSSP (单源最短路径)
%%% 3. 连通分量
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

%% @doc 简单的并行执行测试
simple_test() ->
    io:format("=== Simple Pregel Test ===~n"),

    %% 创建一个简单的图: A -> B -> C
    G0 = pregel:new_graph(),
    G1 = pregel:add_vertex(G0, a, 1),
    G2 = pregel:add_vertex(G1, b, 2),
    G3 = pregel:add_vertex(G2, c, 3),
    G4 = pregel:add_edge(G3, a, b),
    G5 = pregel:add_edge(G4, b, c),

    io:format("Graph created with ~p vertices~n", [pregel:vertex_count(G5)]),

    %% 定义简单的计算函数: 每个顶点向邻居发送自己的值
    ComputeFn = fun(Ctx) ->
        Superstep = pregel:get_superstep(Ctx),
        Value = pregel:get_vertex_value(Ctx),
        Messages = pregel:get_messages(Ctx),

        io:format("  Superstep ~p: Vertex ~p, Value=~p, Messages=~p~n",
                  [Superstep, pregel:get_vertex_id(Ctx), Value, Messages]),

        case Superstep >= 2 of
            true ->
                pregel:vote_to_halt(Ctx);
            false ->
                %% 将自己的值发送给邻居
                Ctx1 = pregel:send_to_all_neighbors(Ctx, Value),
                %% 更新值为收到消息的和
                NewValue = case Messages of
                    [] -> Value;
                    _ -> Value + lists:sum(Messages)
                end,
                pregel:set_value(Ctx1, NewValue)
        end
    end,

    %% 运行 Pregel
    io:format("~nStarting Pregel execution...~n"),
    Result = pregel:run(G5, ComputeFn, #{
        max_supersteps => 5,
        num_workers => 2
    }),

    io:format("~nResult: ~p~n", [maps:get(status, Result)]),
    io:format("Supersteps: ~p~n", [maps:get(supersteps, Result)]),

    Result.

%%====================================================================
%% 运行所有示例
%%====================================================================

run_all() ->
    io:format("~n========================================~n"),
    io:format("    Erlang Pregel Examples~n"),
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

    %% 显示 PageRank 值
    Values = pregel:vertex_values(Result),
    io:format("PageRank values:~n"),
    maps:foreach(
        fun(V, Rank) ->
            io:format("  ~p: ~.4f~n", [V, Rank])
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

    %% 显示距离
    Values = pregel:vertex_values(Result),
    io:format("Shortest distances from 'a':~n"),
    maps:foreach(
        fun(V, Dist) ->
            DistStr = case Dist of
                infinity -> "infinity";
                _ -> io_lib:format("~p", [Dist])
            end,
            io:format("  ~p: ~s~n", [V, DistStr])
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

    %% 显示组件
    Values = pregel:vertex_values(Result),
    io:format("Connected components:~n"),
    maps:foreach(
        fun(V, Component) ->
            io:format("  ~p: component ~p~n", [V, Component])
        end,
        Values
    ),

    Result.
