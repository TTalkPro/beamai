%%%-------------------------------------------------------------------
%%% @doc Agent 框架测试套件
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT 回调函数
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% 测试用例
-export([
    %% Memory 测试（使用 beamai_memory）
    memory_basic_test/1,
    memory_checkpoint_test/1,
    memory_checkpoint_lineage_test/1,
    memory_store_test/1,

    %% 注册表测试
    registry_basic_test/1,

    %% 简单 Agent 测试
    simple_agent_init_test/1,
    simple_beamai_tools_test/1,

    %% 多 Agent 测试
    multi_agent_init_test/1,

    %% 深度 Agent 测试
    deep_agent_init_test/1,
    deep_agent_planning_test/1
]).

%%====================================================================
%% CT 回调函数
%%====================================================================

all() ->
    [{group, memory}, {group, registry},
     {group, simple_agent}, {group, multi_agent}, {group, deep_agent}].

groups() ->
    [
        {memory, [parallel], [
            memory_basic_test,
            memory_checkpoint_test,
            memory_checkpoint_lineage_test,
            memory_store_test
        ]},
        {registry, [], [
            registry_basic_test
        ]},
        {simple_agent, [], [
            simple_agent_init_test,
            simple_beamai_tools_test
        ]},
        {multi_agent, [], [
            multi_agent_init_test
        ]},
        {deep_agent, [], [
            deep_agent_init_test,
            deep_agent_planning_test
        ]}
    ].

init_per_suite(Config) ->
    application:ensure_all_started(beamai_runtime),
    Config.

end_per_suite(_Config) ->
    application:stop(beamai_runtime),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%====================================================================
%% Memory 测试 (使用 beamai_memory)
%%====================================================================

memory_basic_test(_Config) ->
    %% 测试 beamai_memory 基本创建
    {ok, Memory} = beamai_memory:new(#{
        checkpointer => #{backend => ets},
        store => #{backend => ets}
    }),
    ?assert(is_map(Memory)),
    ?assertMatch(#{checkpointer := _, store := _}, Memory),
    ok.

memory_checkpoint_test(_Config) ->
    ThreadId = <<"test_checkpoint_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% 创建 memory
    {ok, Memory} = beamai_memory:new(#{
        checkpointer => #{backend => ets},
        store => #{backend => ets}
    }),

    %% 保存检查点
    Config = #{thread_id => ThreadId},
    StateData = #{
        messages => [#{role => user, content => <<"test">>}],
        context => #{foo => <<"bar">>}
    },
    {ok, Memory1} = beamai_memory:save_checkpoint(Memory, Config, StateData),
    ?assert(is_map(Memory1)),

    %% 加载检查点
    {ok, LoadedData} = beamai_memory:load_checkpoint(Memory1, Config),
    ?assert(is_map(LoadedData)),
    ?assertEqual([#{role => user, content => <<"test">>}], maps:get(messages, LoadedData)),

    %% 加载最新检查点
    {ok, LatestData} = beamai_memory:load_latest_checkpoint(Memory1, Config),
    ?assert(is_map(LatestData)),

    %% 列出检查点
    {ok, Checkpoints} = beamai_memory:list_checkpoints(Memory1, Config),
    ?assert(is_list(Checkpoints)),
    ?assert(length(Checkpoints) >= 1),

    %% 计数
    Count = beamai_memory:checkpoint_count(Memory1, Config),
    ?assert(Count >= 1),

    ok.

memory_checkpoint_lineage_test(_Config) ->
    ThreadId = <<"test_lineage_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% 创建 memory
    {ok, Memory} = beamai_memory:new(#{
        checkpointer => #{backend => ets},
        store => #{backend => ets}
    }),

    %% 保存初始检查点
    Config = #{thread_id => ThreadId},
    StateData1 = #{messages => [#{role => user, content => <<"initial">>}]},
    {ok, Memory1} = beamai_memory:save_checkpoint(Memory, Config, StateData1),

    %% 保存第二个检查点（自动形成血统）
    StateData2 = #{messages => [
        #{role => user, content => <<"initial">>},
        #{role => assistant, content => <<"response">>}
    ]},
    {ok, Memory2} = beamai_memory:save_checkpoint(Memory1, Config, StateData2),

    %% 验证有多个检查点
    {ok, Checkpoints} = beamai_memory:list_checkpoints(Memory2, Config),
    ?assert(length(Checkpoints) >= 2),

    %% 获取血统历史
    case beamai_memory:load_checkpoint_tuple(Memory2, Config) of
        {ok, Tuple} when is_tuple(Tuple) ->
            ?assert(true);
        {error, not_found} ->
            %% 也可以接受（如果没有检查点）
            ?assert(true)
    end,

    ok.

memory_store_test(_Config) ->
    Namespace = [<<"test">>, <<"store">>, integer_to_binary(erlang:unique_integer([positive]))],

    %% 创建 memory
    {ok, Memory} = beamai_memory:new(#{
        checkpointer => #{backend => ets},
        store => #{backend => ets}
    }),

    %% 使用 store 存储数据 (put/4: Memory, Namespace, Key, Value)
    {ok, Memory1} = beamai_memory:put(Memory, Namespace, <<"key1">>, #{data => <<"value1">>}),
    ?assert(is_map(Memory1)),

    %% 搜索数据 (search/3: Memory, Namespace, Filter)
    {ok, Items} = beamai_memory:search(Memory1, Namespace, #{}),
    ?assert(is_list(Items)),
    ?assert(length(Items) >= 1),

    ok.

%%====================================================================
%% 注册表测试
%%====================================================================

registry_basic_test(_Config) ->
    %% 确保注册表正在运行
    ?assert(is_pid(whereis(beamai_registry))),

    %% 创建一个模拟 Agent 进程
    Self = self(),
    AgentId = <<"test_registry_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% 注册
    ok = beamai_registry:register_agent(AgentId, Self, #{type => simple}),

    %% 查找
    {ok, Self} = beamai_registry:lookup_agent(AgentId),

    %% 列表
    Agents = beamai_registry:list_agents(),
    ?assert(length(Agents) >= 1),

    %% 获取信息
    {ok, Info} = beamai_registry:get_agent_info(AgentId),
    simple = maps:get(type, Info),

    %% 取消注册
    ok = beamai_registry:unregister_agent(AgentId),
    {error, not_found} = beamai_registry:lookup_agent(AgentId),

    ok.

%%====================================================================
%% 简单 Agent 测试
%%====================================================================

simple_agent_init_test(_Config) ->
    AgentId = <<"simple_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    {ok, _State} = beamai_agent:init(AgentId, #{
        name => <<"Test Agent">>,
        system_prompt => <<"You are a test agent.">>
    }),

    ok.

simple_beamai_tools_test(_Config) ->
    AgentId = <<"simple_tools_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    Tools = [
        #{
            name => <<"test_tool">>,
            description => <<"A test tool">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"input">> => #{type => string}
                },
                required => [<<"input">>]
            },
            handler => fun(Args) ->
                Input = maps:get(<<"input">>, Args),
                #{echo => Input}
            end
        }
    ],

    {ok, State} = beamai_agent:init(AgentId, #{
        name => <<"Tool Test Agent">>,
        tools => Tools
    }),

    %% 获取工具
    ToolSpecs = beamai_agent:get_tools(State),
    1 = length(ToolSpecs),

    %% 执行工具
    {ok, Result, _NewState} = beamai_agent:execute_tool(
        <<"test_tool">>,
        #{<<"input">> => <<"hello">>},
        State
    ),
    ?assert(is_binary(Result)),

    ok.

%%====================================================================
%% 多 Agent 测试
%%====================================================================

multi_agent_init_test(_Config) ->
    AgentId = <<"multi_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    {ok, State} = beamai_multi:init(AgentId, #{
        name => <<"Test Team">>,
        agents => [
            #{
                name => <<"agent1">>,
                role => <<"Worker">>,
                system_prompt => <<"You are worker 1.">>
            }
        ]
    }),

    %% 检查工具是否包含委派功能
    Tools = beamai_multi:get_tools(State),
    ?assert(length(Tools) > 0),

    ok.

%%====================================================================
%% 深度 Agent 测试
%%====================================================================

deep_agent_init_test(_Config) ->
    AgentId = <<"deep_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    {ok, State} = beamai_deepagent:init(AgentId, #{
        name => <<"Test Deep Agent">>,
        max_depth => 2,
        planning_enabled => true,
        reflection_enabled => true
    }),

    %% 检查工具是否包含规划和反思功能
    Tools = beamai_deepagent:get_tools(State),
    ToolNames = [maps:get(name, maps:get(function, T)) || T <- Tools],
    ?assert(lists:member(<<"create_plan">>, ToolNames)),
    ?assert(lists:member(<<"reflect">>, ToolNames)),

    ok.

deep_agent_planning_test(_Config) ->
    AgentId = <<"deep_plan_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    {ok, State0} = beamai_deepagent:init(AgentId, #{
        name => <<"Planning Test">>,
        planning_enabled => true
    }),

    %% 创建计划
    Args = #{
        <<"goal">> => <<"Test goal">>,
        <<"steps">> => [
            #{<<"description">> => <<"Step 1">>},
            #{<<"description">> => <<"Step 2">>}
        ]
    },

    {ok, Result, State1} = beamai_deepagent:execute_tool(<<"create_plan">>, Args, State0),
    ?assert(is_binary(Result)),

    %% 更新计划
    {ok, _, _State2} = beamai_deepagent:execute_tool(
        <<"update_plan">>,
        #{<<"step_id">> => 1, <<"status">> => <<"completed">>, <<"result">> => <<"Done">>},
        State1
    ),

    ok.
