%%%-------------------------------------------------------------------
%%% @doc beamai_graph 条件路由示例
%%%
%%% 演示条件边(conditional_edge)的使用：
%%% - 根据状态动态决定下一个节点
%%% - 使用 Command 模式直接控制路由
%%% - 多路分支逻辑
%%%
%%% 使用方式:
%%%   make shell-graph
%%%   > example_graph_conditional:run_router().
%%%   > example_graph_conditional:run_command().
%%% @end
%%%-------------------------------------------------------------------
-module(example_graph_conditional).

-export([run_router/0, run_command/0]).

%% @doc 使用条件边路由函数实现分支
%%
%% 流程:
%%   classify -> (high_priority | normal | low_priority) -> respond -> __end__
%%
%% classify 节点根据输入的 score 分类优先级，
%% 条件边根据分类结果路由到不同处理节点
run_router() ->
    ClassifyFun = fun(State, _Context) ->
        Score = graph_state:get(State, score, 0),
        Priority = if
            Score >= 80 -> <<"high">>;
            Score >= 50 -> <<"normal">>;
            true -> <<"low">>
        end,
        {ok, graph_state:set(State, priority, Priority)}
    end,

    HighFun = fun(State, _Context) ->
        {ok, graph_state:set(State, action, <<"escalate_immediately">>)}
    end,

    NormalFun = fun(State, _Context) ->
        {ok, graph_state:set(State, action, <<"process_in_queue">>)}
    end,

    LowFun = fun(State, _Context) ->
        {ok, graph_state:set(State, action, <<"batch_later">>)}
    end,

    RespondFun = fun(State, _Context) ->
        Priority = graph_state:get(State, priority),
        Action = graph_state:get(State, action),
        Response = <<"Priority: ", Priority/binary, ", Action: ", Action/binary>>,
        {ok, graph_state:set(State, response, Response)}
    end,

    %% 路由函数：根据 priority 字段决定下一个节点
    RouterFun = fun(State) ->
        case graph_state:get(State, priority) of
            <<"high">> -> high_priority;
            <<"normal">> -> normal;
            <<"low">> -> low_priority
        end
    end,

    {ok, Graph} = graph:build([
        {node, classify, ClassifyFun},
        {node, high_priority, HighFun},
        {node, normal, NormalFun},
        {node, low_priority, LowFun},
        {node, respond, RespondFun},
        {conditional_edge, classify, RouterFun},
        {edge, high_priority, respond},
        {edge, normal, respond},
        {edge, low_priority, respond},
        {edge, respond, '__end__'},
        {entry, classify}
    ]),

    %% 测试不同分数
    lists:foreach(fun(Score) ->
        State = graph:state(#{score => Score}),
        Result = graph:run(Graph, State),
        Final = maps:get(final_state, Result),
        Response = graph_state:get(Final, response),
        io:format("Score ~p -> ~s~n", [Score, Response])
    end, [90, 65, 30]),

    ok.

%% @doc 使用 Command 模式控制路由
%%
%% 流程:
%%   evaluate -> (approve | reject) -> __end__
%%
%% evaluate 节点使用 graph_command:goto/2 同时设置状态和路由
run_command() ->
    EvaluateFun = fun(State, _Context) ->
        Amount = graph_state:get(State, amount, 0),
        case Amount =< 1000 of
            true ->
                {command, graph_command:goto(approve, #{reason => <<"within_limit">>})};
            false ->
                {command, graph_command:goto(reject, #{reason => <<"exceeds_limit">>})}
        end
    end,

    ApproveFun = fun(State, _Context) ->
        Reason = graph_state:get(State, reason),
        {ok, graph_state:set(State, result, <<"APPROVED: ", Reason/binary>>)}
    end,

    RejectFun = fun(State, _Context) ->
        Reason = graph_state:get(State, reason),
        {ok, graph_state:set(State, result, <<"REJECTED: ", Reason/binary>>)}
    end,

    {ok, Graph} = graph:build([
        {node, evaluate, EvaluateFun},
        {node, approve, ApproveFun},
        {node, reject, RejectFun},
        {edge, approve, '__end__'},
        {edge, reject, '__end__'},
        {entry, evaluate}
    ]),

    %% 测试审批流程
    lists:foreach(fun(Amount) ->
        State = graph:state(#{amount => Amount}),
        Result = graph:run(Graph, State),
        Final = maps:get(final_state, Result),
        Res = graph_state:get(Final, result),
        io:format("Amount ~p -> ~s~n", [Amount, Res])
    end, [500, 1500]),

    ok.
