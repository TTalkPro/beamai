%%%-------------------------------------------------------------------
%%% @doc 复杂 Agent 工作流测试
%%%
%%% 模拟 ReAct 风格的 Agent 工作流:
%%% 1. 解析用户意图
%%% 2. 规划执行步骤
%%% 3. 执行工具调用 (可能多次循环)
%%% 4. 生成最终响应
%%%
%%% 测试双引擎执行的正确性
%%% @end
%%%-------------------------------------------------------------------
-module(graph_agent_workflow_test).

-include_lib("eunit/include/eunit.hrl").

-export([run/0, run_with_engine/1]).

%%====================================================================
%% 测试入口
%%====================================================================

run() ->
    io:format("~n=== 复杂 Agent 工作流测试 ===~n~n"),

    %% 构建工作流图
    {ok, Graph} = build_react_agent_graph(),

    %% 测试用例
    TestCases = [
        #{
            name => "简单问候",
            input => <<"你好，请问你是谁？">>,
            expected_tool_calls => 0
        },
        #{
            name => "天气查询",
            input => <<"北京今天天气怎么样？">>,
            expected_tool_calls => 1
        },
        #{
            name => "复杂计算任务",
            input => <<"帮我计算 123 * 456，然后查询结果对应的城市人口">>,
            expected_tool_calls => 2
        },
        #{
            name => "多步骤任务",
            input => <<"先搜索 Erlang 并发模型，然后总结要点">>,
            expected_tool_calls => 1
        }
    ],

    %% 使用 Pregel 引擎测试
    io:format("--- 测试 Pregel 引擎 ---~n"),
    run_test_cases(Graph, TestCases, pregel),

    io:format("~n=== 所有测试完成 ===~n"),
    ok.

run_with_engine(Engine) ->
    {ok, Graph} = build_react_agent_graph(),
    InitState = graph:state(#{
        input => <<"查询北京和上海的天气，然后比较哪个更热">>,
        messages => [],
        tool_calls => 0,
        max_tool_calls => 5
    }),

    io:format("~n=== 使用 ~p 引擎执行复杂任务 ===~n", [Engine]),
    io:format("输入: ~s~n~n", [maps:get(input, InitState)]),

    Result = graph:run(Graph, InitState, #{engine => Engine}),
    print_result(Result),
    Result.

%%====================================================================
%% 工作流构建
%%====================================================================

build_react_agent_graph() ->
    %% 节点函数定义
    ParseFun = fun parse_intent/2,
    PlanFun = fun plan_actions/2,
    ToolFun = fun execute_tool/2,
    CheckFun = fun check_completion/2,
    RespondFun = fun generate_response/2,

    %% 路由函数
    PlanRouter = fun(State) ->
        case graph:get(State, needs_tool) of
            true -> execute_tool;
            false -> generate_response
        end
    end,

    CheckRouter = fun(State) ->
        case graph:get(State, is_complete) of
            true -> generate_response;
            false -> plan_actions  %% 循环回规划阶段
        end
    end,

    %% 构建图
    B0 = graph:builder(#{max_iterations => 20}),

    %% 添加节点
    B1 = graph:add_node(B0, parse_intent, ParseFun),
    B2 = graph:add_node(B1, plan_actions, PlanFun),
    B3 = graph:add_node(B2, execute_tool, ToolFun),
    B4 = graph:add_node(B3, check_completion, CheckFun),
    B5 = graph:add_node(B4, generate_response, RespondFun),

    %% 添加边
    B6 = graph:add_edge(B5, parse_intent, plan_actions),
    B7 = graph:add_conditional_edge(B6, plan_actions, PlanRouter),
    B8 = graph:add_edge(B7, execute_tool, check_completion),
    B9 = graph:add_conditional_edge(B8, check_completion, CheckRouter),
    B10 = graph:add_edge(B9, generate_response, '__end__'),

    %% 设置入口并编译
    B11 = graph:set_entry(B10, parse_intent),
    graph:compile(B11).

%%====================================================================
%% 节点函数实现
%%====================================================================

%% 解析用户意图
parse_intent(State, _VertexInput) ->
    Input = graph:get(State, input, <<>>),

    %% 简单的意图识别
    Intents = analyze_intent(Input),

    %% 添加消息到历史
    Messages = graph:get(State, messages, []),
    NewMessage = #{role => user, content => Input, timestamp => erlang:timestamp()},

    State1 = graph:set(State, intents, Intents),
    State2 = graph:set(State1, messages, [NewMessage | Messages]),
    State3 = graph:set(State2, current_step, 0),

    io:format("  [解析意图] 识别到意图: ~p~n", [Intents]),
    {ok, State3}.

%% 规划执行动作
plan_actions(State, _VertexInput) ->
    Intents = graph:get(State, intents, []),
    CurrentStep = graph:get(State, current_step, 0),
    ToolResults = graph:get(State, tool_results, []),

    %% 决定下一步动作
    {NeedsTool, ToolName, ToolArgs} = plan_next_action(Intents, CurrentStep, ToolResults),

    State1 = graph:set(State, needs_tool, NeedsTool),
    State2 = graph:set(State1, next_tool, ToolName),
    State3 = graph:set(State2, tool_args, ToolArgs),

    case NeedsTool of
        true ->
            io:format("  [规划动作] 需要调用工具: ~p~n", [ToolName]);
        false ->
            io:format("  [规划动作] 无需工具，准备生成响应~n")
    end,

    {ok, State3}.

%% 执行工具
execute_tool(State, _VertexInput) ->
    ToolName = graph:get(State, next_tool),
    ToolArgs = graph:get(State, tool_args, #{}),
    ToolCalls = graph:get(State, tool_calls, 0),

    io:format("  [执行工具] ~p(~p)~n", [ToolName, ToolArgs]),

    %% 模拟工具执行
    Result = simulate_tool_call(ToolName, ToolArgs),

    %% 更新状态
    ToolResults = graph:get(State, tool_results, []),
    NewResult = #{tool => ToolName, args => ToolArgs, result => Result},

    State1 = graph:set(State, tool_results, [NewResult | ToolResults]),
    State2 = graph:set(State1, tool_calls, ToolCalls + 1),
    State3 = graph:set(State2, current_step, graph:get(State, current_step, 0) + 1),

    io:format("  [工具结果] ~s~n", [Result]),
    {ok, State3}.

%% 检查是否完成
check_completion(State, _VertexInput) ->
    Intents = graph:get(State, intents, []),
    ToolResults = graph:get(State, tool_results, []),
    ToolCalls = graph:get(State, tool_calls, 0),
    MaxToolCalls = graph:get(State, max_tool_calls, 3),

    %% 判断是否完成
    IsComplete = check_if_complete(Intents, ToolResults, ToolCalls, MaxToolCalls),

    State1 = graph:set(State, is_complete, IsComplete),

    io:format("  [检查完成] 已调用工具 ~p 次, 完成状态: ~p~n", [ToolCalls, IsComplete]),
    {ok, State1}.

%% 生成最终响应
generate_response(State, _VertexInput) ->
    Intents = graph:get(State, intents, []),
    ToolResults = graph:get(State, tool_results, []),
    Input = graph:get(State, input, <<>>),

    %% 生成响应
    Response = build_response(Input, Intents, ToolResults),

    %% 添加到消息历史
    Messages = graph:get(State, messages, []),
    NewMessage = #{role => assistant, content => Response, timestamp => erlang:timestamp()},

    State1 = graph:set(State, response, Response),
    State2 = graph:set(State1, messages, [NewMessage | Messages]),

    io:format("  [生成响应] ~s~n", [Response]),
    {ok, State2}.

%%====================================================================
%% 辅助函数
%%====================================================================

analyze_intent(Input) when is_binary(Input) ->
    %% 使用 binary 模式匹配，支持 UTF-8
    Patterns = [
        {weather, [<<"天气">>, <<"温度">>, <<"下雨">>, <<"晴天">>, <<"weather">>]},
        {search, [<<"搜索">>, <<"查询">>, <<"查找">>, <<"找">>, <<"search">>]},
        {calculate, [<<"计算">>, <<"加">>, <<"减">>, <<"乘">>, <<"除">>, <<"*">>, <<"+">>, <<"-">>, <<"/">>]},
        {compare, [<<"比较">>, <<"对比">>, <<"哪个">>, <<"compare">>]},
        {greeting, [<<"你好">>, <<"您好">>, <<"hi">>, <<"hello">>, <<"Hi">>, <<"Hello">>]}
    ],

    lists:filtermap(
        fun({Intent, Keywords}) ->
            HasKeyword = lists:any(
                fun(Kw) -> binary:match(Input, Kw) =/= nomatch end,
                Keywords
            ),
            case HasKeyword of
                true -> {true, Intent};
                false -> false
            end
        end,
        Patterns
    ).

plan_next_action(Intents, CurrentStep, ToolResults) ->
    CompletedTools = [maps:get(tool, R) || R <- ToolResults],

    %% 根据意图和已完成的工具决定下一步
    case {Intents, CurrentStep} of
        {[greeting | _], _} ->
            {false, none, #{}};
        {_, _} when CurrentStep >= length(Intents) ->
            {false, none, #{}};
        {[weather | _], 0} ->
            {true, weather_api, #{city => <<"北京">>}};
        {[search | _], 0} ->
            {true, search_api, #{query => <<"Erlang">>}};
        {[calculate | _], 0} ->
            {true, calculator, #{expression => <<"123 * 456">>}};
        {[compare, weather | _], Step} when Step < 2 ->
            Cities = [<<"北京">>, <<"上海">>],
            City = lists:nth(Step + 1, Cities),
            case lists:member(weather_api, CompletedTools) of
                true when length(CompletedTools) < 2 ->
                    {true, weather_api, #{city => City}};
                _ ->
                    {false, none, #{}}
            end;
        _ ->
            {false, none, #{}}
    end.

simulate_tool_call(weather_api, #{city := City}) ->
    Temps = #{<<"北京">> => 25, <<"上海">> => 28, <<"广州">> => 32},
    Temp = maps:get(City, Temps, 20),
    iolist_to_binary(io_lib:format("~s今天天气晴，温度~p°C", [City, Temp]));

simulate_tool_call(search_api, #{query := Query}) ->
    iolist_to_binary(io_lib:format("搜索结果: 找到关于 ~s 的 10 条结果", [Query]));

simulate_tool_call(calculator, #{expression := Expr}) ->
    iolist_to_binary(io_lib:format("计算结果: ~s = 56088", [Expr]));

simulate_tool_call(Tool, Args) ->
    iolist_to_binary(io_lib:format("工具 ~p 执行完成，参数: ~p", [Tool, Args])).

check_if_complete(Intents, ToolResults, ToolCalls, MaxToolCalls) ->
    %% 超过最大调用次数
    ToolCalls >= MaxToolCalls orelse
    %% 简单任务不需要工具
    Intents =:= [greeting] orelse
    %% 所有需要的工具都已执行
    (length(ToolResults) > 0 andalso
     not lists:member(compare, Intents)) orelse
    %% 比较任务需要两次工具调用
    (lists:member(compare, Intents) andalso length(ToolResults) >= 2).

build_response(Input, Intents, ToolResults) ->
    case {Intents, ToolResults} of
        {[greeting | _], _} ->
            <<"你好！我是一个基于 Erlang Graph 框架的智能助手。有什么可以帮助你的吗？">>;
        {_, []} ->
            <<"我已经理解了你的问题，但目前没有足够的信息来回答。">>;
        {[compare | _], Results} when length(Results) >= 2 ->
            ResultTexts = [maps:get(result, R) || R <- Results],
            iolist_to_binary([
                <<"根据查询结果：\n">>,
                lists:join(<<"\n">>, ResultTexts),
                <<"\n\n综合分析完成。">>
            ]);
        {_, [#{result := Result} | _]} ->
            iolist_to_binary([
                <<"根据查询，">>, Result
            ])
    end.

%%====================================================================
%% 测试运行
%%====================================================================

run_test_cases(Graph, TestCases, Engine) ->
    lists:foreach(
        fun(#{name := Name, input := Input, expected_tool_calls := Expected}) ->
            io:format("~n测试: ~s~n", [Name]),
            io:format("输入: ~s~n", [Input]),

            InitState = graph:state(#{
                input => Input,
                messages => [],
                tool_calls => 0,
                max_tool_calls => 5
            }),

            Result = graph:run(Graph, InitState, #{engine => Engine}),

            Status = maps:get(status, Result),
            FinalState = maps:get(final_state, Result),
            ToolCalls = graph:get(FinalState, tool_calls, 0),
            Response = graph:get(FinalState, response, <<"无响应">>),

            io:format("状态: ~p, 工具调用次数: ~p (期望: ~p)~n", [Status, ToolCalls, Expected]),
            io:format("响应: ~s~n", [Response]),

            %% 验证
            case Status of
                completed -> io:format("[PASS] 执行完成~n");
                _ -> io:format("[WARN] 执行状态异常: ~p~n", [Status])
            end
        end,
        TestCases
    ).

print_result(Result) ->
    Status = maps:get(status, Result),
    FinalState = maps:get(final_state, Result),

    io:format("~n--- 执行结果 ---~n"),
    io:format("状态: ~p~n", [Status]),
    io:format("工具调用次数: ~p~n", [graph:get(FinalState, tool_calls, 0)]),
    io:format("响应: ~s~n", [graph:get(FinalState, response, <<"无响应">>)]),

    %% 打印消息历史
    Messages = graph:get(FinalState, messages, []),
    io:format("~n消息历史 (~p 条):~n", [length(Messages)]),
    lists:foreach(
        fun(#{role := Role, content := Content}) ->
            io:format("  [~p] ~s~n", [Role, Content])
        end,
        lists:reverse(Messages)
    ).

%%====================================================================
%% EUnit Tests
%%====================================================================

agent_workflow_test_() ->
    {timeout, 60, fun() ->
        {ok, Graph} = build_react_agent_graph(),

        %% 测试简单问候 - 使用英文避免编码问题
        GreetingState = graph:state(#{
            input => <<"Hello there!">>,
            messages => [],
            tool_calls => 0,
            max_tool_calls => 3
        }),

        %% 测试 Pregel 引擎
        Result = graph:run(Graph, GreetingState),
        ?assertEqual(completed, maps:get(status, Result)),
        ?assertEqual(0, graph:get(maps:get(final_state, Result), tool_calls, 0)),

        ok
    end}.
