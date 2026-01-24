%%%-------------------------------------------------------------------
%%% @doc DeepAgent 单元测试
%%%
%%% 使用 meck 模拟 beamai_agent 行为来测试协调器逻辑。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试: beamai_deepagent_plan (Phase 2 - steps_to_maps)
%%====================================================================

plan_steps_to_maps_test() ->
    Plan = beamai_deepagent_plan:new(<<"Test goal">>, [
        #{<<"description">> => <<"Step 1">>, <<"dependencies">> => [], <<"requires_deep">> => false},
        #{<<"description">> => <<"Step 2">>, <<"dependencies">> => [1], <<"requires_deep">> => true},
        #{<<"description">> => <<"Step 3">>, <<"dependencies">> => [1], <<"requires_deep">> => false}
    ]),
    Maps = beamai_deepagent_plan:steps_to_maps(Plan),
    ?assertEqual(3, length(Maps)),
    [M1, M2, M3] = Maps,
    ?assertEqual(1, maps:get(id, M1)),
    ?assertEqual(<<"Step 1">>, maps:get(description, M1)),
    ?assertEqual([], maps:get(dependencies, M1)),
    ?assertEqual(false, maps:get(is_deep, M1)),
    ?assertEqual(2, maps:get(id, M2)),
    ?assertEqual([1], maps:get(dependencies, M2)),
    ?assertEqual(true, maps:get(is_deep, M2)),
    ?assertEqual(3, maps:get(id, M3)),
    ?assertEqual([1], maps:get(dependencies, M3)).

%%====================================================================
%% 测试: beamai_deepagent_plan_plugin (Phase 1)
%%====================================================================

plan_plugin_info_test() ->
    Info = beamai_deepagent_plan_plugin:plugin_info(),
    ?assertEqual(<<"deepagent_plan">>, maps:get(name, Info)).

plan_plugin_functions_test() ->
    Fns = beamai_deepagent_plan_plugin:functions(),
    ?assertEqual(1, length(Fns)),
    [FnDef] = Fns,
    ?assertEqual(<<"create_plan">>, maps:get(name, FnDef)).

plan_plugin_handler_test() ->
    Fns = beamai_deepagent_plan_plugin:functions(),
    [FnDef] = Fns,
    Args = #{
        <<"goal">> => <<"Test goal">>,
        <<"steps">> => [
            #{<<"description">> => <<"Do thing 1">>, <<"dependencies">> => []},
            #{<<"description">> => <<"Do thing 2">>, <<"dependencies">> => [1]}
        ]
    },
    {ok, ResultJson} = beamai_function:invoke(FnDef, Args),
    Result = jsx:decode(ResultJson, [return_maps]),
    ?assertEqual(<<"plan_created">>, maps:get(<<"status">>, Result)),
    ?assertEqual(<<"Test goal">>, maps:get(<<"goal">>, Result)),
    ?assertEqual(2, maps:get(<<"step_count">>, Result)).

plan_plugin_handler_missing_args_test() ->
    Fns = beamai_deepagent_plan_plugin:functions(),
    [FnDef] = Fns,
    {error, _} = beamai_function:invoke(FnDef, #{}).

%%====================================================================
%% 测试: beamai_deepagent_dependencies (依赖分析)
%%====================================================================

dependencies_simple_test() ->
    Steps = [
        #{id => 1, description => <<"A">>, dependencies => []},
        #{id => 2, description => <<"B">>, dependencies => [1]},
        #{id => 3, description => <<"C">>, dependencies => [1]}
    ],
    Layers = beamai_deepagent_dependencies:analyze(Steps),
    ?assertEqual(2, length(Layers)),
    [Layer1, Layer2] = Layers,
    ?assertEqual(1, length(Layer1)),
    ?assertEqual(2, length(Layer2)).

dependencies_parallel_test() ->
    Steps = [
        #{id => 1, description => <<"A">>, dependencies => []},
        #{id => 2, description => <<"B">>, dependencies => []},
        #{id => 3, description => <<"C">>, dependencies => [1, 2]}
    ],
    Layers = beamai_deepagent_dependencies:analyze(Steps),
    ?assertEqual(2, length(Layers)),
    [Layer1, Layer2] = Layers,
    ?assertEqual(2, length(Layer1)),
    ?assertEqual(1, length(Layer2)).

dependencies_empty_test() ->
    ?assertEqual([], beamai_deepagent_dependencies:analyze([])).

%%====================================================================
%% 测试: beamai_deepagent_parallel (并行执行 - 分批逻辑)
%%====================================================================

parallel_empty_layer_test() ->
    State = #{config => #{max_parallel => 5}, completed_results => []},
    {ok, Results, _} = beamai_deepagent_parallel:execute_layer([], State),
    ?assertEqual([], Results).

%%====================================================================
%% 测试: beamai_deepagent (公共 API)
%%====================================================================

new_default_test() ->
    Config = beamai_deepagent:new(),
    ?assertEqual(3, maps:get(max_depth, Config)),
    ?assertEqual(5, maps:get(max_parallel, Config)),
    ?assertEqual(true, maps:get(planning_enabled, Config)),
    ?assertEqual(true, maps:get(reflection_enabled, Config)).

new_with_opts_test() ->
    Config = beamai_deepagent:new(#{
        llm => beamai_chat_completion:create(mock, #{}),
        max_parallel => 3,
        planning_enabled => false
    }),
    ?assertEqual(3, maps:get(max_parallel, Config)),
    ?assertEqual(false, maps:get(planning_enabled, Config)).

run_missing_llm_test() ->
    Config = beamai_deepagent:new(),
    ?assertEqual({error, missing_llm_config}, beamai_deepagent:run(Config, <<"test">>)).

get_plan_test() ->
    ?assertEqual(undefined, beamai_deepagent:get_plan(#{})),
    Plan = beamai_deepagent_plan:new(<<"G">>, [#{<<"description">> => <<"S">>}]),
    ?assertEqual(Plan, beamai_deepagent:get_plan(#{plan => Plan})).

get_trace_test() ->
    ?assertEqual(undefined, beamai_deepagent:get_trace(#{})),
    Trace = beamai_deepagent_trace:new(),
    ?assertEqual(Trace, beamai_deepagent:get_trace(#{trace => Trace})).

%%====================================================================
%% 测试: Planner (mocked agent)
%%====================================================================

planner_test_() ->
    {setup,
     fun setup_agent_mock/0,
     fun teardown_agent_mock/1,
     [fun planner_creates_plan/0]
    }.

setup_agent_mock() ->
    meck:new(beamai_agent, [passthrough]),
    ok.

teardown_agent_mock(_) ->
    meck:unload(beamai_agent).

planner_creates_plan() ->
    %% Mock agent:new to return a fake agent state
    meck:expect(beamai_agent, new, fun(_Config) ->
        {ok, #{mock_agent => true, kernel => undefined, messages => [],
               turn_count => 0, callbacks => #{}, max_tool_iterations => 3,
               system_prompt => <<>>, name => <<"planner">>, id => <<"p1">>,
               metadata => #{}, interrupt_state => undefined, auto_save => false}}
    end),
    %% Mock agent:run to return a result with create_plan tool call
    meck:expect(beamai_agent, run, fun(_Agent, _Msg) ->
        {ok, #{
            content => <<"Plan created">>,
            tool_calls_made => [
                #{name => <<"deepagent_plan.create_plan">>,
                  args => #{
                    <<"goal">> => <<"Test goal">>,
                    <<"steps">> => [
                        #{<<"description">> => <<"First step">>, <<"dependencies">> => []},
                        #{<<"description">> => <<"Second step">>, <<"dependencies">> => [1]}
                    ]
                  }}
            ]
        }, #{}}
    end),

    LLM = beamai_chat_completion:create(mock, #{}),
    Config = #{llm => LLM},
    {ok, Plan} = beamai_deepagent_planner:create_plan(Config, <<"Do something">>),
    Steps = beamai_deepagent_plan:get_steps(Plan),
    ?assertEqual(2, length(Steps)).

%%====================================================================
%% 测试: Coordinator - 直接执行模式 (mocked agent)
%%====================================================================

coordinator_direct_test_() ->
    {setup,
     fun setup_agent_mock/0,
     fun teardown_agent_mock/1,
     [fun coordinator_direct_execution/0]
    }.

coordinator_direct_execution() ->
    %% Mock agent for direct execution (no planning)
    meck:expect(beamai_agent, new, fun(_Config) ->
        {ok, #{mock_agent => true, kernel => undefined, messages => [],
               turn_count => 0, callbacks => #{}, max_tool_iterations => 10,
               system_prompt => <<>>, name => <<"executor">>, id => <<"e1">>,
               metadata => #{}, interrupt_state => undefined, auto_save => false}}
    end),
    meck:expect(beamai_agent, run, fun(_Agent, _Msg) ->
        {ok, #{
            content => <<"Task completed successfully">>,
            tool_calls_made => []
        }, #{}}
    end),

    LLM = beamai_chat_completion:create(mock, #{}),
    Config = beamai_deepagent:new(#{
        llm => LLM,
        planning_enabled => false,
        reflection_enabled => false
    }),
    {ok, Result} = beamai_deepagent:run(Config, <<"Simple task">>),
    ?assertEqual(completed, maps:get(status, Result)),
    ?assertEqual(<<"Task completed successfully">>, maps:get(response, Result)),
    ?assertEqual(1, length(maps:get(step_results, Result))).

%%====================================================================
%% 测试: Coordinator - 规划模式 (mocked agent)
%%====================================================================

coordinator_planning_test_() ->
    {setup,
     fun setup_agent_mock/0,
     fun teardown_agent_mock/1,
     [fun coordinator_plan_and_execute/0]
    }.

coordinator_plan_and_execute() ->
    CallCount = atomics:new(1, [{signed, false}]),
    %% Mock agent:new
    meck:expect(beamai_agent, new, fun(_Config) ->
        {ok, #{mock_agent => true, kernel => undefined, messages => [],
               turn_count => 0, callbacks => #{}, max_tool_iterations => 10,
               system_prompt => <<>>, name => <<"agent">>, id => <<"a1">>,
               metadata => #{}, interrupt_state => undefined, auto_save => false}}
    end),
    %% Mock agent:run - first call is planner, subsequent calls are executors/reflector
    meck:expect(beamai_agent, run, fun(_Agent, Msg) ->
        N = atomics:add_get(CallCount, 1, 1),
        case N of
            1 ->
                %% Planner call
                {ok, #{
                    content => <<"Plan created">>,
                    tool_calls_made => [
                        #{name => <<"deepagent_plan.create_plan">>,
                          args => #{
                            <<"goal">> => <<"Test">>,
                            <<"steps">> => [
                                #{<<"description">> => <<"Step A">>, <<"dependencies">> => []},
                                #{<<"description">> => <<"Step B">>, <<"dependencies">> => []}
                            ]
                          }}
                    ]
                }, #{}};
            _ ->
                %% Executor/Reflector calls
                {ok, #{
                    content => <<"Step done: ", Msg/binary>>,
                    tool_calls_made => []
                }, #{}}
        end
    end),

    LLM = beamai_chat_completion:create(mock, #{}),
    Config = beamai_deepagent:new(#{
        llm => LLM,
        planning_enabled => true,
        reflection_enabled => false
    }),
    {ok, Result} = beamai_deepagent:run(Config, <<"Multi-step task">>),
    ?assertEqual(completed, maps:get(status, Result)),
    %% Should have 2 step results (both in same parallel layer)
    StepResults = maps:get(step_results, Result),
    ?assertEqual(2, length(StepResults)),
    %% Plan should be present
    Plan = maps:get(plan, Result),
    ?assert(beamai_deepagent_plan:is_complete(Plan)).

%%====================================================================
%% 测试: Trace 模块
%%====================================================================

trace_basic_test() ->
    T0 = beamai_deepagent_trace:new(),
    T1 = beamai_deepagent_trace:add(T0, test_event, #{data => 1}),
    T2 = beamai_deepagent_trace:add(T1, another_event, <<"hello">>),
    Recent = beamai_deepagent_trace:get_recent(T2, 1),
    ?assertEqual(1, length(Recent)),
    [#{type := another_event}] = Recent,
    Formatted = beamai_deepagent_trace:format(T2),
    ?assert(is_binary(Formatted)).
