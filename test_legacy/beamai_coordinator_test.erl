%%%-------------------------------------------------------------------
%%% @doc beamai_coordinator 纯函数 API 测试
%%%
%%% 测试协调器的纯函数 API：
%%% - 创建协调器（Pipeline 和 Orchestrator）
%%% - 执行任务
%%% - 状态导出/导入
%%% - 便捷函数
%%%
%%% 包含 Mock 测试和 GLM-4.7 真实 API 测试。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_coordinator_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamai_agent/include/beamai_agent.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

setup() ->
    %% Ensure all required applications are started
    application:ensure_all_started(beamai_core),
    application:ensure_all_started(beamai_runtime),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 创建 Mock LLM 配置
mock_llm() ->
    llm_client:create(mock, #{}).

%% @private 获取 GLM-4.7 配置
glm_config() ->
    ApiKey = list_to_binary(os:getenv("ZHIPU_API_KEY", "")),
    case ApiKey of
        <<>> ->
            skip;
        _ ->
            llm_client:create(anthropic, #{
                model => <<"glm-4.7">>,
                api_key => ApiKey,
                base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
                timeout => 120000,
                max_tokens => 2048
            })
    end.

%% @private 检查是否应该跳过 GLM 测试
should_skip_glm() ->
    case glm_config() of
        skip -> true;
        _ -> false
    end.

%% @private 创建简单的 Agent 定义列表
simple_agents() ->
    [
        #{name => <<"assistant1">>, role => <<"助手1"/utf8>>},
        #{name => <<"assistant2">>, role => <<"助手2"/utf8>>}
    ].

%% @private 创建研究-写作团队定义
research_team_agents() ->
    [
        #{
            name => <<"researcher">>,
            role => <<"研究员"/utf8>>,
            system_prompt => <<"你是一名研究员，负责收集和分析信息。请简洁回答。"/utf8>>
        },
        #{
            name => <<"writer">>,
            role => <<"写作者"/utf8>>,
            system_prompt => <<"你是一名写作者，负责将研究结果整理成文章。请简洁回答。"/utf8>>
        }
    ].

%%====================================================================
%% 创建协调器测试（Mock）
%%====================================================================

create_coordinator_mock_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"new/1 creates pipeline coordinator by default",
           fun() ->
               {ok, Coord} = beamai_coordinator:new(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               ?assertEqual(pipeline, beamai_coordinator:get_type(Coord))
           end},

          {"new_pipeline/1 creates pipeline coordinator",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               ?assertEqual(pipeline, beamai_coordinator:get_type(Coord))
           end},

          {"new_orchestrator/1 creates orchestrator coordinator",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_orchestrator(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               ?assertEqual(orchestrator, beamai_coordinator:get_type(Coord))
           end},

          {"coordinator has workers",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               Workers = beamai_coordinator:get_workers(Coord),
               ?assertEqual(2, maps:size(Workers)),
               ?assert(maps:is_key(<<"assistant1">>, Workers)),
               ?assert(maps:is_key(<<"assistant2">>, Workers))
           end},

          {"get_worker/2 returns specific worker",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               {ok, _Worker} = beamai_coordinator:get_worker(Coord, <<"assistant1">>),
               {error, not_found} = beamai_coordinator:get_worker(Coord, <<"nonexistent">>)
           end}
         ]
     end}.

%%====================================================================
%% 运行任务测试（Mock）
%%====================================================================

run_task_mock_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"run/2 executes task and returns new state",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               {ok, Result, NewCoord} = beamai_coordinator:run(Coord, <<"测试任务"/utf8>>),

               %% 验证结果
               ?assert(maps:is_key(final_response, Result)),
               %% 验证状态更新
               ?assertNotEqual(Coord, NewCoord)
           end},

          {"run/3 accepts options",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_orchestrator(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               {ok, _Result, _NewCoord} = beamai_coordinator:run(Coord, <<"测试"/utf8>>, #{})
           end}
         ]
     end}.

%%====================================================================
%% 便捷函数测试（Mock）
%%====================================================================

convenience_functions_mock_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"delegate/3 executes task on specific worker",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               {ok, Response, NewCoord} = beamai_coordinator:delegate(
                   Coord, <<"assistant1">>, <<"任务"/utf8>>),

               ?assert(is_binary(Response)),
               ?assertNotEqual(Coord, NewCoord)
           end},

          {"delegate/3 returns error for unknown worker",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               {error, {worker_not_found, _}, _} = beamai_coordinator:delegate(
                   Coord, <<"unknown">>, <<"任务"/utf8>>)
           end},

          {"delegate_parallel/3 executes on multiple workers",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               {ok, Results, _NewCoord} = beamai_coordinator:delegate_parallel(
                   Coord, [<<"assistant1">>, <<"assistant2">>], <<"任务"/utf8>>),

               ?assertEqual(2, maps:size(Results)),
               ?assert(maps:is_key(<<"assistant1">>, Results)),
               ?assert(maps:is_key(<<"assistant2">>, Results))
           end}
         ]
     end}.

%%====================================================================
%% 状态导出/导入测试（Mock）
%%====================================================================

export_import_mock_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"export/1 returns serializable map",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),
               Exported = beamai_coordinator:export(Coord),

               ?assert(is_map(Exported)),
               ?assertEqual(pipeline, maps:get(type, Exported)),
               ?assert(is_map(maps:get(coordinator_state, Exported))),
               ?assert(is_map(maps:get(workers_states, Exported)))
           end},

          {"import/2 restores coordinator state",
           fun() ->
               {ok, Coord} = beamai_coordinator:new_orchestrator(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),

               %% 导出
               Exported = beamai_coordinator:export(Coord),

               %% 导入
               {ok, RestoredCoord} = beamai_coordinator:import(Exported, #{
                   llm => mock_llm()
               }),

               ?assertEqual(orchestrator, beamai_coordinator:get_type(RestoredCoord)),
               ?assertEqual(2, maps:size(beamai_coordinator:get_workers(RestoredCoord)))
           end},

          {"export/import preserves conversation history",
           fun() ->
               {ok, Coord0} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),

               %% 执行任务
               {ok, _, Coord1} = beamai_coordinator:run(Coord0, <<"第一个任务"/utf8>>),

               %% 导出
               Exported = beamai_coordinator:export(Coord1),

               %% 导入
               {ok, RestoredCoord} = beamai_coordinator:import(Exported, #{
                   llm => mock_llm()
               }),

               %% 验证协调器消息历史
               OrigState = beamai_coordinator:get_coordinator_state(Coord1),
               RestoredState = beamai_coordinator:get_coordinator_state(RestoredCoord),

               %% 消息数量应该相同
               ?assertEqual(
                   length(beamai_agent:get_messages(OrigState)),
                   length(beamai_agent:get_messages(RestoredState))
               )
           end}
         ]
     end}.

%%====================================================================
%% GLM-4.7 真实 API 测试
%%====================================================================

glm_coordinator_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         case should_skip_glm() of
             true ->
                 [{"SKIP: ZHIPU_API_KEY not set", fun() -> ok end}];
             false ->
                 [
                  {"GLM pipeline coordinator simple task",
                   {timeout, 180, fun() ->
                       {ok, Coord} = beamai_coordinator:new_pipeline(#{
                           agents => research_team_agents(),
                           llm => glm_config(),
                           max_iterations => 5
                       }),

                       {ok, Result, _NewCoord} = beamai_coordinator:run(
                           Coord, <<"简单回答：1+1等于多少？"/utf8>>),

                       Response = maps:get(final_response, Result, <<>>),
                       ?assert(is_binary(Response)),
                       ?assert(byte_size(Response) > 0)
                   end}},

                  {"GLM orchestrator coordinator simple task",
                   {timeout, 180, fun() ->
                       {ok, Coord} = beamai_coordinator:new_orchestrator(#{
                           agents => research_team_agents(),
                           llm => glm_config(),
                           max_iterations => 5
                       }),

                       {ok, Result, _NewCoord} = beamai_coordinator:run(
                           Coord, <<"回答：今天星期几？"/utf8>>),

                       Response = maps:get(final_response, Result, <<>>),
                       ?assert(is_binary(Response)),
                       ?assert(byte_size(Response) > 0)
                   end}},

                  {"GLM delegate to specific worker",
                   {timeout, 120, fun() ->
                       {ok, Coord} = beamai_coordinator:new_pipeline(#{
                           agents => research_team_agents(),
                           llm => glm_config()
                       }),

                       {ok, Response, _NewCoord} = beamai_coordinator:delegate(
                           Coord, <<"researcher">>, <<"你好，请简单介绍自己"/utf8>>),

                       ?assert(is_binary(Response)),
                       ?assert(byte_size(Response) > 0)
                   end}},

                  {"GLM coordinator export and import",
                   {timeout, 180, fun() ->
                       {ok, Coord0} = beamai_coordinator:new_pipeline(#{
                           agents => research_team_agents(),
                           llm => glm_config()
                       }),

                       %% 先执行一个任务
                       {ok, _, Coord1} = beamai_coordinator:delegate(
                           Coord0, <<"researcher">>, <<"记住数字 42"/utf8>>),

                       %% 导出
                       Exported = beamai_coordinator:export(Coord1),

                       %% 导入
                       {ok, RestoredCoord} = beamai_coordinator:import(Exported, #{
                           llm => glm_config()
                       }),

                       ?assertEqual(pipeline, beamai_coordinator:get_type(RestoredCoord)),
                       ?assertEqual(2, maps:size(beamai_coordinator:get_workers(RestoredCoord)))
                   end}}
                 ]
         end
     end}.

%%====================================================================
%% 纯函数特性测试
%%====================================================================

pure_function_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"original coordinator unchanged after run",
           fun() ->
               {ok, Coord0} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),

               OrigState = beamai_coordinator:get_coordinator_state(Coord0),
               OrigMessages = beamai_agent:get_messages(OrigState),

               %% 运行任务
               {ok, _, _Coord1} = beamai_coordinator:run(Coord0, <<"测试"/utf8>>),

               %% 原始协调器不变
               NewState = beamai_coordinator:get_coordinator_state(Coord0),
               ?assertEqual(OrigMessages, beamai_agent:get_messages(NewState))
           end},

          {"multiple runs accumulate state correctly",
           fun() ->
               {ok, Coord0} = beamai_coordinator:new_pipeline(#{
                   agents => simple_agents(),
                   llm => mock_llm()
               }),

               {ok, _, Coord1} = beamai_coordinator:run(Coord0, <<"任务1"/utf8>>),
               {ok, _, Coord2} = beamai_coordinator:run(Coord1, <<"任务2"/utf8>>),
               {ok, _, Coord3} = beamai_coordinator:run(Coord2, <<"任务3"/utf8>>),

               %% 每次运行后消息应该累积
               State1 = beamai_coordinator:get_coordinator_state(Coord1),
               State2 = beamai_coordinator:get_coordinator_state(Coord2),
               State3 = beamai_coordinator:get_coordinator_state(Coord3),

               Msg1 = length(beamai_agent:get_messages(State1)),
               Msg2 = length(beamai_agent:get_messages(State2)),
               Msg3 = length(beamai_agent:get_messages(State3)),

               %% 消息数量应该递增
               ?assert(Msg2 >= Msg1),
               ?assert(Msg3 >= Msg2)
           end}
         ]
     end}.
