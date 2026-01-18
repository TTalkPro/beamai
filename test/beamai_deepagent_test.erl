%%%-------------------------------------------------------------------
%%% @doc Graph Deep Agent 单元测试
%%%
%%% 测试内容：
%%% - 配置创建 (new/0, new/1)
%%% - 工具定义 (beamai_deepagent_tools)
%%% - 路由逻辑 (beamai_deepagent_router)
%%% - 初始状态创建
%%% - 图构建
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试生成器
%%====================================================================

beamai_deepagent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"配置创建测试", fun config_tests/0},
      {"工具定义测试", fun tools_tests/0},
      {"路由逻辑测试", fun router_tests/0},
      {"依赖分析测试", fun dependency_tests/0},
      {"初始状态测试", fun initial_state_tests/0},
      {"图构建测试", fun graph_build_tests/0},
      {"结果提取测试", fun result_extraction_tests/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%% @private 获取 DeepAgent 工具（使用 beamai_tool_registry）
get_deepagent_tools(Config) ->
    beamai_tool_registry:from_config(#{
        providers => [{beamai_deepagent_tool_provider, Config}]
    }).

%%====================================================================
%% 配置创建测试
%%====================================================================

config_tests() ->
    %% 默认配置
    DefaultConfig = beamai_deepagent:new(),
    ?assert(is_map(DefaultConfig)),
    ?assertEqual(3, maps:get(max_depth, DefaultConfig)),
    ?assertEqual(50, maps:get(max_iterations, DefaultConfig)),
    ?assertEqual(true, maps:get(planning_enabled, DefaultConfig)),
    ?assertEqual(true, maps:get(reflection_enabled, DefaultConfig)),
    ?assertEqual(0, maps:get(depth, DefaultConfig)),
    ?assertEqual([], maps:get(tools, DefaultConfig)),

    %% 自定义配置
    CustomConfig = beamai_deepagent:new(#{
        max_depth => 5,
        max_iterations => 100,
        planning_enabled => false,
        tools => [#{name => <<"test">>}]
    }),
    ?assertEqual(5, maps:get(max_depth, CustomConfig)),
    ?assertEqual(100, maps:get(max_iterations, CustomConfig)),
    ?assertEqual(false, maps:get(planning_enabled, CustomConfig)),
    ?assertEqual([#{name => <<"test">>}], maps:get(tools, CustomConfig)),
    %% 未覆盖的默认值应保留
    ?assertEqual(true, maps:get(reflection_enabled, CustomConfig)),

    %% LLM 配置
    LLMConfig = beamai_deepagent:new(#{
        llm => llm_client:create(zhipu, #{
            model => <<"glm-4">>,
            api_key => <<"test-key">>
        })
    }),
    LLM = maps:get(llm, LLMConfig),
    ?assertEqual(zhipu, maps:get(provider, LLM)),
    ?assertEqual(<<"glm-4">>, maps:get(model, LLM)),

    ok.

%%====================================================================
%% 工具定义测试
%%====================================================================

tools_tests() ->
    %% 基础工具（直接使用 Provider API）
    BaseTools = beamai_deepagent_tool_provider:base_tools(),
    ?assertEqual(2, length(BaseTools)),
    BaseNames = [maps:get(name, T) || T <- BaseTools],
    ?assert(lists:member(<<"checkpoint">>, BaseNames)),
    ?assert(lists:member(<<"get_trace">>, BaseNames)),

    %% 计划工具（直接使用 Provider API）
    PlanTools = beamai_deepagent_tool_provider:plan_tools(),
    ?assertEqual(2, length(PlanTools)),
    PlanNames = [maps:get(name, T) || T <- PlanTools],
    ?assert(lists:member(<<"create_plan">>, PlanNames)),
    ?assert(lists:member(<<"update_plan">>, PlanNames)),

    %% 子任务工具（直接使用 Provider API）
    SubtaskTools = beamai_deepagent_tool_provider:subtask_tools(),
    ?assertEqual(1, length(SubtaskTools)),
    ?assertEqual(<<"spawn_subtask">>, maps:get(name, hd(SubtaskTools))),

    %% 反思工具（直接使用 Provider API）
    ReflectTools = beamai_deepagent_tool_provider:reflect_tools(),
    ?assertEqual(1, length(ReflectTools)),
    ?assertEqual(<<"reflect">>, maps:get(name, hd(ReflectTools))),

    %% all_tools - 全功能配置
    AllToolsConfig = #{
        planning_enabled => true,
        reflection_enabled => true,
        depth => 0,
        max_depth => 3
    },
    AllTools = get_deepagent_tools(AllToolsConfig),
    AllNames = [maps:get(name, T) || T <- AllTools],
    ?assert(lists:member(<<"checkpoint">>, AllNames)),
    ?assert(lists:member(<<"create_plan">>, AllNames)),
    ?assert(lists:member(<<"spawn_subtask">>, AllNames)),
    ?assert(lists:member(<<"reflect">>, AllNames)),

    %% all_tools - 深度限制（depth >= max_depth 时无 spawn_subtask）
    DeepConfig = #{
        planning_enabled => true,
        reflection_enabled => true,
        depth => 3,
        max_depth => 3
    },
    DeepTools = get_deepagent_tools(DeepConfig),
    DeepNames = [maps:get(name, T) || T <- DeepTools],
    ?assertNot(lists:member(<<"spawn_subtask">>, DeepNames)),
    %% 非 depth=0 时无 plan tools
    ?assertNot(lists:member(<<"create_plan">>, DeepNames)),

    %% all_tools - 禁用功能
    MinimalConfig = #{
        planning_enabled => false,
        reflection_enabled => false,
        human_in_loop => #{enabled => false},  %% 禁用 human 工具
        depth => 1,
        max_depth => 3
    },
    MinimalTools = get_deepagent_tools(MinimalConfig),
    MinimalNames = [maps:get(name, T) || T <- MinimalTools],
    ?assertEqual(3, length(MinimalTools)), % base(2) + subtask(1)
    ?assertNot(lists:member(<<"create_plan">>, MinimalNames)),
    ?assertNot(lists:member(<<"reflect">>, MinimalNames)),
    ?assertNot(lists:member(<<"ask_human">>, MinimalNames)),
    ?assertNot(lists:member(<<"confirm_action">>, MinimalNames)),

    ok.

%%====================================================================
%% 工具处理器测试
%%====================================================================

tool_handlers_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      {"create_plan 处理器", fun test_create_plan_handler/0},
      {"checkpoint 处理器", fun test_checkpoint_handler/0},
      {"reflect 处理器", fun test_reflect_handler/0},
      {"spawn_subtask 处理器", fun test_spawn_subtask_handler/0}
     ]}.

test_create_plan_handler() ->
    Args = #{
        <<"goal">> => <<"Research Erlang history">>,
        <<"steps">> => [
            #{<<"description">> => <<"Step 1">>, <<"dependencies">> => []},
            #{<<"description">> => <<"Step 2">>, <<"dependencies">> => [1]}
        ]
    },
    State = graph_state:new(#{}),

    Result = beamai_deepagent_plan_handlers:handle_create_plan(Args, State),
    ?assertEqual(create_plan, maps:get(action, Result)),
    ?assert(is_map(maps:get(plan, Result))),
    ?assertEqual(<<"计划创建成功"/utf8>>, maps:get(message, Result)),
    ok.

test_checkpoint_handler() ->
    Args = #{
        <<"label">> => <<"milestone_1">>,
        <<"notes">> => <<"Completed phase 1">>
    },
    State = graph_state:new(#{}),

    Result = beamai_deepagent_base_handlers:handle_checkpoint(Args, State),
    ?assertEqual(checkpoint, maps:get(action, Result)),
    ?assertEqual(<<"milestone_1">>, maps:get(label, Result)),
    ?assert(is_integer(maps:get(timestamp, Result))),
    ok.

test_reflect_handler() ->
    Args = #{
        <<"observation">> => <<"Found relevant data">>,
        <<"analysis">> => <<"Need to process further">>,
        <<"next_action">> => <<"Continue with step 2">>
    },
    State = graph_state:new(#{}),

    Result = beamai_deepagent_plan_handlers:handle_reflect(Args, State),
    ?assertEqual(reflect, maps:get(action, Result)),
    ?assertEqual(true, maps:get(trigger_reflection, Result)),
    ok.

test_spawn_subtask_handler() ->
    Args = #{
        <<"task_id">> => <<"sub_1">>,
        <<"description">> => <<"Research topic A">>,
        <<"input">> => <<"Find information about A">>
    },
    State = graph_state:new(#{}),

    Result = beamai_deepagent_plan_handlers:handle_spawn_subtask(Args, State),
    ?assertEqual(spawn_subtask, maps:get(action, Result)),
    ?assertEqual(<<"sub_1">>, maps:get(task_id, Result)),
    ?assertEqual(true, maps:get(trigger_fanout, Result)),
    ok.

%%====================================================================
%% 路由逻辑测试
%%====================================================================

router_tests() ->
    %% after_llm - 无工具调用，无响应 -> 继续 LLM
    State1 = graph_state:new(#{
        pending_tools => [],
        final_response => undefined
    }),
    ?assertEqual(llm_node, beamai_deepagent_router:after_llm(State1)),

    %% after_llm - 无工具调用，有响应 -> 结束
    State2 = graph_state:new(#{
        pending_tools => [],
        final_response => <<"Done">>
    }),
    ?assertEqual('__end__', beamai_deepagent_router:after_llm(State2)),

    %% after_llm - 有工具调用 -> 执行工具
    State3 = graph_state:new(#{
        pending_tools => [#{name => <<"test">>}],
        final_response => undefined
    }),
    ?assertEqual(tool_node, beamai_deepagent_router:after_llm(State3)),

    %% should_continue - 无最终响应，无计划 -> 继续
    State4 = graph_state:new(#{
        final_response => undefined,
        plan => undefined
    }),
    ?assertEqual(true, beamai_deepagent_router:should_continue(State4)),

    %% should_continue - 有最终响应 -> 停止
    State5 = graph_state:new(#{
        final_response => <<"Answer">>
    }),
    ?assertEqual(false, beamai_deepagent_router:should_continue(State5)),

    ok.

%%====================================================================
%% 依赖分析测试
%%====================================================================

dependency_tests() ->
    %% 无依赖的步骤应在同一层
    Steps1 = [
        #{id => 1, description => <<"Step 1">>, dependencies => []},
        #{id => 2, description => <<"Step 2">>, dependencies => []},
        #{id => 3, description => <<"Step 3">>, dependencies => []}
    ],
    Layers1 = beamai_deepagent_router:analyze_dependencies(Steps1),
    ?assertEqual(1, length(Layers1)),
    ?assertEqual(3, length(hd(Layers1))),

    %% 顺序依赖应分多层
    Steps2 = [
        #{id => 1, description => <<"Step 1">>, dependencies => []},
        #{id => 2, description => <<"Step 2">>, dependencies => [1]},
        #{id => 3, description => <<"Step 3">>, dependencies => [2]}
    ],
    Layers2 = beamai_deepagent_router:analyze_dependencies(Steps2),
    ?assertEqual(3, length(Layers2)),
    [L1, L2, L3] = Layers2,
    ?assertEqual(1, length(L1)),
    ?assertEqual(1, length(L2)),
    ?assertEqual(1, length(L3)),

    %% 混合依赖
    Steps3 = [
        #{id => 1, description => <<"Step 1">>, dependencies => []},
        #{id => 2, description => <<"Step 2">>, dependencies => []},
        #{id => 3, description => <<"Step 3">>, dependencies => [1, 2]},
        #{id => 4, description => <<"Step 4">>, dependencies => [3]}
    ],
    Layers3 = beamai_deepagent_router:analyze_dependencies(Steps3),
    ?assertEqual(3, length(Layers3)),
    %% 第一层：步骤 1 和 2（无依赖）
    FirstLayer = hd(Layers3),
    FirstIds = [maps:get(id, S) || S <- FirstLayer],
    ?assert(lists:member(1, FirstIds)),
    ?assert(lists:member(2, FirstIds)),

    %% 空步骤列表
    EmptyLayers = beamai_deepagent_router:analyze_dependencies([]),
    ?assertEqual([], EmptyLayers),

    ok.

%%====================================================================
%% 并行分发测试
%%====================================================================

fan_out_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      {"fan_out_subtasks 测试", fun test_fan_out_subtasks/0},
      {"fan_out_plan_steps 测试", fun test_fan_out_plan_steps/0}
     ]}.

test_fan_out_subtasks() ->
    State = graph_state:new(#{depth => 1}),
    Tasks = [
        #{task_id => <<"task_1">>, input => <<"Input 1">>},
        #{task_id => <<"task_2">>, input => <<"Input 2">>}
    ],

    Sends = beamai_deepagent_router:fan_out_subtasks(State, Tasks),
    ?assertEqual(2, length(Sends)),

    %% 检查每个 Send 结构
    [Send1, Send2] = Sends,
    ?assertEqual(task_executor, maps:get(node, Send1)),
    ?assertEqual(task_executor, maps:get(node, Send2)),

    %% fan_out 使用 state 键存储数据
    SendState1 = maps:get(state, Send1),
    ?assertEqual(<<"task_1">>, maps:get(<<"task_id">>, SendState1)),
    ?assertEqual(1, maps:get(<<"depth">>, SendState1)),

    ok.

test_fan_out_plan_steps() ->
    State = graph_state:new(#{depth => 0}),
    Steps = [
        #{id => 1, description => <<"Research topic A">>},
        #{id => 2, description => <<"Research topic B">>}
    ],

    Sends = beamai_deepagent_router:fan_out_plan_steps(State, Steps),
    ?assertEqual(2, length(Sends)),

    [Send1 | _] = Sends,
    %% fan_out 使用 state 键存储数据
    SendState1 = maps:get(state, Send1),
    ?assertEqual(<<"1">>, maps:get(<<"task_id">>, SendState1)),
    ?assertEqual(#{id => 1, description => <<"Research topic A">>}, maps:get(<<"step_data">>, SendState1)),

    ok.

%%====================================================================
%% 初始状态测试
%%====================================================================

initial_state_tests() ->
    Config = beamai_deepagent:new(#{
        max_depth => 5,
        system_prompt => <<"Custom prompt">>
    }),

    %% 使用反射创建初始状态（测试内部函数）
    %% 由于 create_initial_state/2 是私有的，我们通过图构建来间接测试
    {ok, Graph} = beamai_deepagent:build_core_graph(Config),
    ?assert(is_map(Graph)),

    ok.

%%====================================================================
%% 图构建测试
%%====================================================================

graph_build_tests() ->
    Config = beamai_deepagent:new(),

    %% 构建核心图
    {ok, Graph} = beamai_deepagent:build_core_graph(Config),
    ?assert(is_map(Graph)),

    %% 验证图结构包含必要的键
    ?assert(maps:is_key(nodes, Graph)),
    ?assert(maps:is_key(edges, Graph)),
    ?assert(maps:is_key(entry, Graph)),

    %% 验证图包含必要的节点
    NodesMap = maps:get(nodes, Graph),
    ?assert(maps:is_key(llm_node, NodesMap)),
    ?assert(maps:is_key(tool_node, NodesMap)),
    ?assert(maps:is_key(reflect_node, NodesMap)),
    ?assert(maps:is_key(aggregate_node, NodesMap)),
    ?assert(maps:is_key(task_executor, NodesMap)),

    %% 验证入口点
    Entry = maps:get(entry, Graph),
    ?assertEqual(llm_node, Entry),

    %% 验证边存在
    Edges = maps:get(edges, Graph),
    ?assert(maps:is_key(llm_node, Edges)),
    ?assert(maps:is_key(tool_node, Edges)),

    ok.

%%====================================================================
%% 结果提取测试
%%====================================================================

result_extraction_tests() ->
    %% get_plan - 有计划
    Plan = beamai_deepagent_plan:new(<<"Goal">>, [#{description => <<"Step 1">>}]),
    StateWithPlan = graph_state:new(#{plan => Plan}),
    ResultWithPlan = #{final_state => StateWithPlan},
    ?assertEqual(Plan, beamai_deepagent:get_plan(ResultWithPlan)),

    %% get_plan - 无计划
    StateNoPlan = graph_state:new(#{}),
    ResultNoPlan = #{final_state => StateNoPlan},
    ?assertEqual(undefined, beamai_deepagent:get_plan(ResultNoPlan)),

    %% get_plan - 无效结果
    ?assertEqual(undefined, beamai_deepagent:get_plan(#{})),
    ?assertEqual(undefined, beamai_deepagent:get_plan(invalid)),

    %% get_trace - 有轨迹
    Trace = beamai_deepagent_trace:new(),
    TraceWithEntry = beamai_deepagent_trace:add(Trace, test_event, #{data => 1}),
    StateWithTrace = graph_state:new(#{trace => TraceWithEntry}),
    ResultWithTrace = #{final_state => StateWithTrace},
    ?assertEqual(TraceWithEntry, beamai_deepagent:get_trace(ResultWithTrace)),

    %% get_trace - 无轨迹（返回空轨迹）
    StateNoTrace = graph_state:new(#{}),
    ResultNoTrace = #{final_state => StateNoTrace},
    EmptyTrace = beamai_deepagent:get_trace(ResultNoTrace),
    ?assert(is_list(beamai_deepagent_trace:get_recent(EmptyTrace, 10))),

    ok.

%%====================================================================
%% 工具参数验证测试
%%====================================================================

tool_parameters_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      {"工具参数结构验证", fun test_tool_parameter_structure/0}
     ]}.

test_tool_parameter_structure() ->
    AllTools = get_deepagent_tools(#{
        planning_enabled => true,
        reflection_enabled => true,
        depth => 0,
        max_depth => 3
    }),

    %% 验证每个工具都有必需的字段
    lists:foreach(
        fun(Tool) ->
            ?assert(maps:is_key(name, Tool)),
            ?assert(maps:is_key(description, Tool)),
            ?assert(maps:is_key(parameters, Tool)),
            ?assert(maps:is_key(handler, Tool)),

            %% 验证 parameters 结构
            Params = maps:get(parameters, Tool),
            ?assertEqual(object, maps:get(type, Params)),
            ?assert(maps:is_key(properties, Params)),

            %% 验证 handler 是函数
            Handler = maps:get(handler, Tool),
            ?assert(is_function(Handler, 2))
        end,
        AllTools
    ),

    ok.
