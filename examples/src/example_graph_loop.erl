%%%-------------------------------------------------------------------
%%% @doc beamai_graph 循环图示例
%%%
%%% 演示如何在图中实现循环逻辑：
%%% - 使用 Command goto 实现循环
%%% - 条件退出循环
%%% - 模拟 Agent 的 "思考-行动-观察" 循环
%%%
%%% 使用方式:
%%%   make shell-graph
%%%   > example_graph_loop:run_counter().
%%%   > example_graph_loop:run_agent_loop().
%%% @end
%%%-------------------------------------------------------------------
-module(example_graph_loop).

-export([run_counter/0, run_agent_loop/0]).

%% @doc 简单计数器循环
%%
%% 流程: increment -> check -> (increment 或 __end__)
%% 每次循环 count +1，达到目标值后退出
run_counter() ->
    IncrementFun = fun(State, _Context) ->
        Count = graph_state:get(State, count, 0),
        io:format("  Incrementing: ~p -> ~p~n", [Count, Count + 1]),
        {command, graph_command:goto(check, #{count => Count + 1})}
    end,

    CheckFun = fun(State, _Context) ->
        Count = graph_state:get(State, count, 0),
        Target = graph_state:get(State, target, 5),
        case Count >= Target of
            true ->
                io:format("  Reached target ~p, stopping.~n", [Target]),
                {command, graph_command:goto('__end__', #{done => true})};
            false ->
                {command, graph_command:goto(increment)}
        end
    end,

    {ok, Graph} = graph:build([
        {node, increment, IncrementFun},
        {node, check, CheckFun},
        {entry, increment}
    ]),

    State = graph:state(#{count => 0, target => 5}),
    Result = graph:run(Graph, State),

    Final = maps:get(final_state, Result),
    io:format("Final count: ~p, done: ~p~n", [
        graph_state:get(Final, count),
        graph_state:get(Final, done)
    ]),
    Result.

%% @doc 模拟 Agent 循环: 思考 -> 行动 -> 观察 -> (继续或结束)
%%
%% 这是 AI Agent 典型的 ReAct 模式简化版：
%% - think: 决定下一步动作
%% - act: 执行动作 (此处模拟)
%% - observe: 观察结果并决定是否继续
run_agent_loop() ->
    %% 模拟的任务列表
    Tasks = [<<"fetch_data">>, <<"process_data">>, <<"save_result">>],

    ThinkFun = fun(State, _Context) ->
        Step = graph_state:get(State, step, 0),
        TaskList = graph_state:get(State, tasks, []),
        case Step < length(TaskList) of
            true ->
                Task = lists:nth(Step + 1, TaskList),
                io:format("  [Think] Next task: ~s~n", [Task]),
                {command, graph_command:goto(act, #{current_task => Task})};
            false ->
                io:format("  [Think] All tasks done.~n", []),
                {command, graph_command:goto('__end__', #{status => all_done})}
        end
    end,

    ActFun = fun(State, _Context) ->
        Task = graph_state:get(State, current_task),
        io:format("  [Act] Executing: ~s~n", [Task]),
        %% 模拟动作执行结果
        ActionResult = <<"completed_", Task/binary>>,
        {command, graph_command:goto(observe, #{action_result => ActionResult})}
    end,

    ObserveFun = fun(State, _Context) ->
        ActionResult = graph_state:get(State, action_result),
        Step = graph_state:get(State, step, 0),
        History = graph_state:get(State, history, []),
        io:format("  [Observe] Result: ~s~n", [ActionResult]),
        NewHistory = History ++ [ActionResult],
        {command, graph_command:goto(think, #{
            step => Step + 1,
            history => NewHistory
        })}
    end,

    {ok, Graph} = graph:build([
        {node, think, ThinkFun},
        {node, act, ActFun},
        {node, observe, ObserveFun},
        {entry, think}
    ]),

    State = graph:state(#{step => 0, tasks => Tasks, history => []}),
    Result = graph:run(Graph, State),

    Final = maps:get(final_state, Result),
    io:format("~nAgent completed. History: ~p~n", [graph_state:get(Final, history)]),
    Result.
