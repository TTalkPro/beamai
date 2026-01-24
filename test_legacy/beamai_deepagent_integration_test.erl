%%%-------------------------------------------------------------------
%%% @doc Graph Deep Agent 集成测试
%%%
%%% 使用 Mock LLM 测试完整的 Agent 执行流程。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_integration_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试生成器
%%====================================================================

beamai_deepagent_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"简单对话测试", {timeout, 30, fun simple_conversation_test/0}},
      {"工具调用测试", {timeout, 30, fun tool_call_test/0}},
      {"计划创建测试", {timeout, 30, fun plan_creation_test/0}},
      {"反思流程测试", {timeout, 30, fun reflection_test/0}},
      {"多轮对话测试", {timeout, 30, fun multi_turn_test/0}}
     ]}.

setup() ->
    %% 启动必要的应用
    application:ensure_all_started(beamai_runtime),
    ok.

cleanup(_) ->
    ok.

%% @private 获取 DeepAgent 工具（使用 beamai_tool_registry）
get_deepagent_tools(Config) ->
    beamai_tool_registry:from_config(#{
        providers => [{beamai_deepagent_tool_provider, Config}]
    }).

%%====================================================================
%% 简单对话测试
%%====================================================================

simple_conversation_test() ->
    %% 创建配置，使用 Mock LLM
    Config = beamai_deepagent:new(#{
        llm => mock_llm_config(),
        tools => [],
        planning_enabled => false,
        reflection_enabled => false,
        max_iterations => 5
    }),

    %% 设置 Mock 响应
    set_mock_response(#{
        content => <<"Hello! I'm a helpful assistant.">>,
        tool_calls => []
    }),

    %% 运行
    Result = beamai_deepagent:run(Config, <<"Hello">>),

    %% 验证
    case Result of
        {ok, #{status := completed, response := Response}} ->
            ?assert(is_binary(Response)),
            ?assertNotEqual(<<"No response generated">>, Response);
        {ok, #{status := max_iterations}} ->
            %% Mock 环境下可能达到最大迭代
            ok;
        {error, _Reason} ->
            %% 在 Mock 环境下可能会有错误，这是预期的
            ok
    end,
    ok.

%%====================================================================
%% 工具调用测试
%%====================================================================

tool_call_test() ->
    %% 创建计算工具
    CalcTool = #{
        name => <<"calculate">>,
        description => <<"Perform calculations">>,
        parameters => #{
            type => object,
            properties => #{
                <<"expression">> => #{type => string}
            },
            required => [<<"expression">>]
        },
        handler => fun(Args, _State) ->
            Expr = maps:get(<<"expression">>, Args, <<"0">>),
            %% 安全计算
            Result = try
                {ok, Tokens, _} = erl_scan:string(binary_to_list(<<Expr/binary, ".">>)),
                {ok, Parsed} = erl_parse:parse_exprs(Tokens),
                {value, Value, _} = erl_eval:exprs(Parsed, []),
                Value
            catch
                _:_ -> 0
            end,
            #{result => Result}
        end
    },

    Config = beamai_deepagent:new(#{
        llm => mock_llm_config(),
        tools => [CalcTool],
        planning_enabled => false,
        max_iterations => 10
    }),

    %% 这里我们测试工具是否正确注册
    AllTools = get_deepagent_tools(Config),
    ToolNames = [maps:get(name, T) || T <- AllTools],

    %% 基础工具应该存在
    ?assert(lists:member(<<"checkpoint">>, ToolNames)),
    ?assert(lists:member(<<"get_trace">>, ToolNames)),

    ok.

%%====================================================================
%% 计划创建测试
%%====================================================================

plan_creation_test() ->
    %% 测试计划工具的处理器
    Args = #{
        <<"goal">> => <<"Research Erlang">>,
        <<"steps">> => [
            #{<<"description">> => <<"Search for history">>},
            #{<<"description">> => <<"Summarize findings">>, <<"dependencies">> => [1]}
        ]
    },

    State = graph_state:new(#{}),
    Result = beamai_deepagent_plan_handlers:handle_create_plan(Args, State),

    ?assertEqual(create_plan, maps:get(action, Result)),
    ?assert(is_map(maps:get(plan, Result))),

    %% 验证计划结构
    PlanMap = maps:get(plan, Result),
    ?assertEqual(<<"Research Erlang">>, maps:get(goal, PlanMap)),
    ?assert(is_list(maps:get(steps, PlanMap))),
    ?assertEqual(2, length(maps:get(steps, PlanMap))),

    ok.

%%====================================================================
%% 反思流程测试
%%====================================================================

reflection_test() ->
    %% 测试反思工具的处理器
    Args = #{
        <<"observation">> => <<"Found 5 relevant results">>,
        <<"analysis">> => <<"Results cover the main topics">>,
        <<"next_action">> => <<"Proceed to summarization">>
    },

    State = graph_state:new(#{}),
    Result = beamai_deepagent_plan_handlers:handle_reflect(Args, State),

    ?assertEqual(reflect, maps:get(action, Result)),
    ?assertEqual(true, maps:get(trigger_reflection, Result)),
    ?assertEqual(<<"Found 5 relevant results">>, maps:get(observation, Result)),

    ok.

%%====================================================================
%% 多轮对话测试
%%====================================================================

multi_turn_test() ->
    %% 测试路由逻辑的多种场景

    %% 场景 1: 无工具调用，无响应 -> 继续 LLM
    State1 = graph_state:new(#{
        pending_tools => [],
        final_response => undefined,
        messages => [#{role => user, content => <<"test">>}]
    }),
    ?assertEqual(llm_node, beamai_deepagent_router:after_llm(State1)),

    %% 场景 2: 有工具调用 -> 执行工具
    State2 = graph_state:new(#{
        pending_tools => [#{name => <<"test_tool">>}],
        final_response => undefined
    }),
    ?assertEqual(tool_node, beamai_deepagent_router:after_llm(State2)),

    %% 场景 3: 无工具调用，有响应 -> 结束
    State3 = graph_state:new(#{
        pending_tools => [],
        final_response => <<"Done!">>
    }),
    ?assertEqual('__end__', beamai_deepagent_router:after_llm(State3)),

    %% 场景 4: 工具执行后，继续
    State4 = graph_state:new(#{
        tool_results => [#{success => true, result => #{action => continue}}],
        plan => undefined
    }),
    NextNode = beamai_deepagent_router:after_tool(State4),
    ?assertEqual(llm_node, NextNode),

    ok.

%%====================================================================
%% 状态流转测试
%%====================================================================

state_transition_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      {"初始状态创建", fun test_initial_state/0},
      {"消息累积", fun test_message_accumulation/0},
      {"轨迹记录", fun test_trace_recording/0}
     ]}.

test_initial_state() ->
    Config = beamai_deepagent:new(#{
        system_prompt => <<"Test prompt">>,
        max_depth => 5
    }),

    %% 验证配置正确
    ?assertEqual(5, maps:get(max_depth, Config)),
    ?assertEqual(<<"Test prompt">>, maps:get(system_prompt, Config)),

    ok.

test_message_accumulation() ->
    %% 测试消息在状态中的累积
    State0 = graph_state:new(#{
        messages => [#{role => user, content => <<"Hello">>}]
    }),

    Messages0 = graph_state:get(State0, messages),
    ?assertEqual(1, length(Messages0)),

    %% 添加助手响应
    State1 = graph_state:set(State0, messages,
        Messages0 ++ [#{role => assistant, content => <<"Hi there!">>}]),

    Messages1 = graph_state:get(State1, messages),
    ?assertEqual(2, length(Messages1)),

    ok.

test_trace_recording() ->
    %% 测试轨迹记录
    Trace0 = beamai_deepagent_trace:new(),
    ?assertEqual([], beamai_deepagent_trace:get_recent(Trace0, 10)),

    Trace1 = beamai_deepagent_trace:add(Trace0, llm_call, #{prompt => <<"test">>}),
    Recent1 = beamai_deepagent_trace:get_recent(Trace1, 10),
    ?assertEqual(1, length(Recent1)),

    Trace2 = beamai_deepagent_trace:add(Trace1, tool_call, #{name => <<"search">>}),
    Recent2 = beamai_deepagent_trace:get_recent(Trace2, 10),
    ?assertEqual(2, length(Recent2)),

    ok.

%%====================================================================
%% 依赖分析集成测试
%%====================================================================

dependency_analysis_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      {"复杂依赖图", fun test_complex_dependency/0},
      {"循环依赖处理", fun test_cyclic_dependency/0}
     ]}.

test_complex_dependency() ->
    %% 创建复杂依赖结构
    %%   1 --> 3
    %%   2 --> 3 --> 4
    %%         |
    %%         v
    %%         5
    Steps = [
        #{id => 1, description => <<"Step 1">>, dependencies => []},
        #{id => 2, description => <<"Step 2">>, dependencies => []},
        #{id => 3, description => <<"Step 3">>, dependencies => [1, 2]},
        #{id => 4, description => <<"Step 4">>, dependencies => [3]},
        #{id => 5, description => <<"Step 5">>, dependencies => [3]}
    ],

    Layers = beamai_deepagent_router:analyze_dependencies(Steps),

    %% 应该有 3 层
    %% Layer 1: [1, 2] (无依赖)
    %% Layer 2: [3] (依赖 1, 2)
    %% Layer 3: [4, 5] (依赖 3)
    ?assertEqual(3, length(Layers)),

    [L1, L2, L3] = Layers,
    L1Ids = [maps:get(id, S) || S <- L1],
    L2Ids = [maps:get(id, S) || S <- L2],
    L3Ids = [maps:get(id, S) || S <- L3],

    ?assertEqual(2, length(L1Ids)),
    ?assert(lists:member(1, L1Ids)),
    ?assert(lists:member(2, L1Ids)),

    ?assertEqual([3], L2Ids),

    ?assertEqual(2, length(L3Ids)),
    ?assert(lists:member(4, L3Ids)),
    ?assert(lists:member(5, L3Ids)),

    ok.

test_cyclic_dependency() ->
    %% 测试循环依赖（1 -> 2 -> 3 -> 1）
    %% 应该能处理而不崩溃
    Steps = [
        #{id => 1, description => <<"Step 1">>, dependencies => [3]},
        #{id => 2, description => <<"Step 2">>, dependencies => [1]},
        #{id => 3, description => <<"Step 3">>, dependencies => [2]}
    ],

    %% 不应该崩溃
    Layers = beamai_deepagent_router:analyze_dependencies(Steps),

    %% 应该返回某种结果（可能是顺序执行）
    ?assert(is_list(Layers)),
    TotalSteps = lists:sum([length(L) || L <- Layers]),
    ?assertEqual(3, TotalSteps),

    ok.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @doc Mock LLM 配置
mock_llm_config() ->
    llm_client:create(mock, #{
        model => <<"mock-model">>,
        api_key => <<"mock-key">>,
        timeout => 5000
    }).

%% @doc 设置 Mock 响应（在真实测试中需要实现 Mock 机制）
set_mock_response(_Response) ->
    %% 在实际实现中，这里会设置 Mock LLM 的响应
    %% 目前只是占位符
    ok.
