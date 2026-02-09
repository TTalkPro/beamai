%%%-------------------------------------------------------------------
%%% @doc beamai_graph 并行扇出示例
%%%
%%% 演示图的并行执行能力：
%%% - 静态扇出(fanout)：预定义的多目标并行
%%% - 动态分发(dispatch)：运行时决定并行分支
%%%
%%% 使用方式:
%%%   make shell-graph
%%%   > example_graph_fanout:run_static_fanout().
%%%   > example_graph_fanout:run_dynamic_dispatch().
%%% @end
%%%-------------------------------------------------------------------
-module(example_graph_fanout).

-export([run_static_fanout/0, run_dynamic_dispatch/0]).

%% @doc 静态扇出：一个节点并行分发到多个处理节点
%%
%% 流程:
%%   prepare -> [analyze_sentiment, extract_keywords, count_words] -> __end__
%%
%% prepare 节点准备数据，然后并行分发到三个分析节点
run_static_fanout() ->
    PrepareFun = fun(State, _Context) ->
        Text = beamai_graph_engine:state_get(State, text, <<"hello world">>),
        {ok, beamai_graph_engine:state_set(State, prepared_text, Text)}
    end,

    SentimentFun = fun(State, _Context) ->
        Text = beamai_graph_engine:state_get(State, prepared_text, <<>>),
        %% 简单模拟情感分析
        Sentiment = case binary:match(Text, [<<"good">>, <<"great">>, <<"happy">>]) of
            nomatch -> <<"neutral">>;
            _ -> <<"positive">>
        end,
        {ok, beamai_graph_engine:state_set(State, sentiment, Sentiment)}
    end,

    KeywordsFun = fun(State, _Context) ->
        Text = beamai_graph_engine:state_get(State, prepared_text, <<>>),
        %% 简单提取：按空格分词取前3个
        Words = binary:split(Text, <<" ">>, [global]),
        Keywords = lists:sublist(Words, 3),
        {ok, beamai_graph_engine:state_set(State, keywords, Keywords)}
    end,

    WordCountFun = fun(State, _Context) ->
        Text = beamai_graph_engine:state_get(State, prepared_text, <<>>),
        Words = binary:split(Text, <<" ">>, [global]),
        Count = length(Words),
        {ok, beamai_graph_engine:state_set(State, word_count, Count)}
    end,

    {ok, Graph} = beamai_graph:build([
        {node, prepare, PrepareFun},
        {node, analyze_sentiment, SentimentFun},
        {node, extract_keywords, KeywordsFun},
        {node, count_words, WordCountFun},
        {fanout, prepare, [analyze_sentiment, extract_keywords, count_words]},
        {edge, analyze_sentiment, '__end__'},
        {edge, extract_keywords, '__end__'},
        {edge, count_words, '__end__'},
        {entry, prepare}
    ]),

    State = beamai_graph:state(#{text => <<"This is a great example of parallel processing">>}),
    Result = beamai_graph:run(Graph, State, #{workers => 3}),

    Final = maps:get(final_state, Result),
    io:format("Sentiment: ~s~n", [beamai_graph_engine:state_get(Final, sentiment, <<"unknown">>)]),
    io:format("Keywords: ~p~n", [beamai_graph_engine:state_get(Final, keywords, [])]),
    io:format("Word count: ~p~n", [beamai_graph_engine:state_get(Final, word_count, 0)]),
    Result.

%% @doc 动态分发：运行时决定并行分支数量和输入
%%
%% 流程:
%%   split -> [worker(dynamic)] -> aggregate -> __end__
%%
%% split 节点根据输入数据动态创建多个并行 worker 分支，
%% 每个 worker 处理一个数据块，最后 aggregate 汇总结果
run_dynamic_dispatch() ->
    SplitFun = fun(State, _Context) ->
        Items = beamai_graph_engine:state_get(State, items, []),
        %% 为每个 item 创建一个 dispatch
        Dispatches = lists:map(fun(Item) ->
            beamai_graph_dispatch:dispatch(worker, #{item => Item})
        end, Items),
        {ok, beamai_graph_engine:state_set(State, dispatch_count, length(Dispatches))}
    end,

    WorkerFun = fun(State, VertexInput) ->
        %% VertexInput 是 dispatch 传入的输入参数 map
        Item = case VertexInput of
            #{item := Val} -> Val;
            _ -> undefined
        end,
        %% 模拟处理: 将 item 值翻倍
        Processed = case is_integer(Item) of
            true -> Item * 2;
            false -> Item
        end,
        %% 每个 worker 返回单元素列表，由 append_reducer 合并
        {ok, beamai_graph_engine:state_set(State, results, [Processed])}
    end,

    AggregateFun = fun(State, _Context) ->
        Results = beamai_graph_engine:state_get(State, results, []),
        Sum = lists:sum([R || R <- Results, is_integer(R)]),
        {ok, beamai_graph_engine:state_set(State, total, Sum)}
    end,

    %% 路由函数返回 dispatch 列表实现动态并行
    RouterFun = fun(State) ->
        Items = beamai_graph_engine:state_get(State, items, []),
        lists:map(fun(Item) ->
            beamai_graph_dispatch:dispatch(worker, #{item => Item})
        end, Items)
    end,

    {ok, Graph} = beamai_graph:build([
        {node, split, SplitFun},
        {node, worker, WorkerFun},
        {node, aggregate, AggregateFun},
        {conditional_edge, split, RouterFun},
        {edge, worker, aggregate},
        {edge, aggregate, '__end__'},
        {entry, split}
    ]),

    State = beamai_graph:state(#{items => [10, 20, 30, 40, 50]}),
    %% 使用 append_reducer 合并多个 worker 的结果列表
    Result = beamai_graph:run(Graph, State, #{
        workers => 4,
        field_reducers => #{
            <<"results">> => fun beamai_graph_state_reducer:append_reducer/2
        }
    }),

    Final = maps:get(final_state, Result),
    io:format("Dispatch count: ~p~n", [beamai_graph_engine:state_get(Final, dispatch_count, 0)]),
    io:format("Results: ~p~n", [beamai_graph_engine:state_get(Final, results, [])]),
    io:format("Total: ~p~n", [beamai_graph_engine:state_get(Final, total, 0)]),
    Result.
