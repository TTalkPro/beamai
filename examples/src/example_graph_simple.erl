%%%-------------------------------------------------------------------
%%% @doc beamai_graph 简单线性图示例
%%%
%%% 演示最基本的图构建和执行：
%%% - 声明式 DSL 构建图
%%% - 命令式 Builder 构建图
%%% - 线性流程：输入 -> 处理 -> 输出
%%%
%%% 使用方式:
%%%   make shell-graph
%%%   > example_graph_simple:run_dsl().
%%%   > example_graph_simple:run_builder().
%%% @end
%%%-------------------------------------------------------------------
-module(example_graph_simple).

-export([run_dsl/0, run_builder/0]).

%% @doc 声明式 DSL 方式构建和执行简单线性图
%%
%% 流程: greeting -> uppercase -> __end__
%% greeting 节点生成问候语，uppercase 节点将其转为大写
run_dsl() ->
    GreetingFun = fun(State, _Context) ->
        Name = beamai_graph_engine:state_get(State, name, <<"World">>),
        Message = <<"Hello, ", Name/binary, "!">>,
        {ok, beamai_graph_engine:state_set(State, message, Message)}
    end,

    UppercaseFun = fun(State, _Context) ->
        Message = beamai_graph_engine:state_get(State, message, <<>>),
        Upper = string:uppercase(Message),
        {ok, beamai_graph_engine:state_set(State, message, Upper)}
    end,

    {ok, Graph} = beamai_graph:build([
        {node, greeting, GreetingFun},
        {node, uppercase, UppercaseFun},
        {edge, greeting, uppercase},
        {edge, uppercase, '__end__'},
        {entry, greeting}
    ]),

    InitialState = beamai_graph:state(#{name => <<"Erlang">>}),
    Result = beamai_graph:run(Graph, InitialState),

    Status = maps:get(status, Result),
    FinalState = maps:get(final_state, Result),
    Message = beamai_graph_engine:state_get(FinalState, message),

    io:format("Status: ~p~n", [Status]),
    io:format("Message: ~s~n", [Message]),
    Result.

%% @doc 命令式 Builder 方式构建和执行简单线性图
%%
%% 流程: add_timestamp -> format -> __end__
%% add_timestamp 节点添加时间戳，format 节点格式化输出
run_builder() ->
    TimestampFun = fun(State, _Context) ->
        Now = erlang:system_time(second),
        {ok, beamai_graph_engine:state_set(State, timestamp, Now)}
    end,

    FormatFun = fun(State, _Context) ->
        Name = beamai_graph_engine:state_get(State, name, <<"unknown">>),
        Ts = beamai_graph_engine:state_get(State, timestamp, 0),
        Output = io_lib:format("User ~s logged at ~p", [Name, Ts]),
        {ok, beamai_graph_engine:state_set(State, output, iolist_to_binary(Output))}
    end,

    B0 = beamai_graph:builder(),
    B1 = beamai_graph:add_node(B0, add_timestamp, TimestampFun),
    B2 = beamai_graph:add_node(B1, format, FormatFun),
    B3 = beamai_graph:add_edge(B2, add_timestamp, format),
    B4 = beamai_graph:add_edge(B3, format, '__end__'),
    B5 = beamai_graph:set_entry(B4, add_timestamp),
    {ok, Graph} = beamai_graph:compile(B5),

    InitialState = beamai_graph:state(#{name => <<"Alice">>}),
    Result = beamai_graph:run(Graph, InitialState),

    FinalState = maps:get(final_state, Result),
    Output = beamai_graph_engine:state_get(FinalState, output),

    io:format("Output: ~s~n", [Output]),
    Result.
