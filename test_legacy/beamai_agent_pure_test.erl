%%%-------------------------------------------------------------------
%%% @doc beamai_agent 纯函数 API 单元测试
%%%
%%% 使用 Mock LLM Provider 测试纯函数 API。
%%% 测试内容：
%%% - 单轮对话（无 Memory）
%%% - 多轮对话（无 Memory）
%%% - 状态导入导出
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_pure_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamai_agent/include/beamai_agent.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

setup() ->
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

%% @private 从状态中获取消息数量
message_count(State) ->
    length(beamai_agent:get_messages(State)).

%%====================================================================
%% 单轮对话测试（无 Memory）
%%====================================================================

single_turn_no_memory_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"single turn conversation returns result",
           fun() ->
               Config = #{
                   llm => mock_llm(),
                   system_prompt => <<"Test assistant">>
               },
               {ok, State} = beamai_agent:new(Config),
               {ok, Result, _NewState} = beamai_agent:run(State, <<"Hello">>),

               %% 验证结果包含必要字段
               ?assert(maps:is_key(final_response, Result)),
               ?assert(maps:is_key(messages, Result)),
               ?assert(maps:is_key(status, Result))
           end},

          {"single turn with tools",
           fun() ->
               Tools = [
                   #{
                       name => <<"echo">>,
                       description => <<"Echo back the input">>,
                       parameters => #{
                           type => object,
                           properties => #{
                               <<"text">> => #{type => string}
                           },
                           required => [<<"text">>]
                       },
                       handler => fun(#{<<"text">> := Text}) ->
                           #{result => Text}
                       end
                   }
               ],
               Config = #{
                   llm => mock_llm(),
                   system_prompt => <<"Assistant with tools">>,
                   tools => Tools
               },
               {ok, State} = beamai_agent:new(Config),
               {ok, Result, _NewState} = beamai_agent:run(State, <<"Echo test">>),

               ?assert(maps:is_key(final_response, Result))
           end},

          {"single turn preserves context",
           fun() ->
               Config = #{
                   llm => mock_llm(),
                   context => #{user_id => <<"user123">>}
               },
               {ok, State} = beamai_agent:new(Config),
               {ok, _Result, NewState} = beamai_agent:run(State, <<"Hello">>),

               %% 验证 context 保留
               ?assertEqual(<<"user123">>, beamai_agent:get_context(NewState, user_id))
           end}
         ]
     end}.

%%====================================================================
%% 多轮对话测试（无 Memory）
%%====================================================================

multi_turn_no_memory_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"multi-turn conversation accumulates messages",
           fun() ->
               Config = #{
                   llm => mock_llm(),
                   system_prompt => <<"Multi-turn test">>
               },
               {ok, State0} = beamai_agent:new(Config),
               ?assertEqual(0, message_count(State0)),

               %% 第一轮
               {ok, _, State1} = beamai_agent:run(State0, <<"Hello">>),
               %% 应该有 user + assistant 消息
               ?assert(message_count(State1) >= 2),

               %% 第二轮
               {ok, _, State2} = beamai_agent:run(State1, <<"How are you?">>),
               %% 消息数量应该增加
               ?assert(message_count(State2) > message_count(State1)),

               %% 第三轮
               {ok, _, State3} = beamai_agent:run(State2, <<"Tell me a joke">>),
               ?assert(message_count(State3) > message_count(State2))
           end},

          {"multi-turn maintains conversation context",
           fun() ->
               Config = #{
                   llm => mock_llm(),
                   system_prompt => <<"Remember numbers">>
               },
               {ok, State0} = beamai_agent:new(Config),

               %% 设置上下文
               State1 = beamai_agent:put_context(State0, number, 42),

               %% 运行对话
               {ok, _, State2} = beamai_agent:run(State1, <<"Remember this">>),

               %% 验证上下文保留
               ?assertEqual(42, beamai_agent:get_context(State2, number))
           end},

          {"multi-turn with context updates",
           fun() ->
               Config = #{
                   llm => mock_llm()
               },
               {ok, State0} = beamai_agent:new(Config),

               %% 第一轮
               {ok, _, State1} = beamai_agent:run(State0, <<"First message">>),
               State1a = beamai_agent:put_context(State1, step, 1),

               %% 第二轮
               {ok, _, State2} = beamai_agent:run(State1a, <<"Second message">>),
               State2a = beamai_agent:put_context(State2, step, 2),

               %% 验证上下文已更新
               ?assertEqual(2, beamai_agent:get_context(State2a, step))
           end}
         ]
     end}.

%%====================================================================
%% 状态导入导出测试
%%====================================================================

state_io_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"export then import preserves conversation",
           fun() ->
               Config = #{
                   llm => mock_llm(),
                   system_prompt => <<"Export test">>
               },
               {ok, State0} = beamai_agent:new(Config),

               %% 运行几轮对话
               {ok, _, State1} = beamai_agent:run(State0, <<"Hello">>),
               {ok, _, State2} = beamai_agent:run(State1, <<"World">>),

               %% 导出
               Exported = beamai_agent:export_state(State2),
               ?assert(is_list(maps:get(messages, Exported))),

               %% 导入
               {ok, RestoredState} = beamai_agent:import_state(Exported, Config),

               %% 验证消息恢复
               ?assertEqual(
                   beamai_agent:get_messages(State2),
                   beamai_agent:get_messages(RestoredState)
               )
           end},

          {"export then import preserves context",
           fun() ->
               Config = #{
                   llm => mock_llm(),
                   context => #{key => <<"value">>}
               },
               {ok, State0} = beamai_agent:new(Config),
               State1 = beamai_agent:put_context(State0, extra, <<"data">>),

               %% 导出
               Exported = beamai_agent:export_state(State1),

               %% 导入
               {ok, RestoredState} = beamai_agent:import_state(Exported, Config),

               %% 验证上下文恢复
               ?assertEqual(<<"value">>, beamai_agent:get_context(RestoredState, key)),
               ?assertEqual(<<"data">>, beamai_agent:get_context(RestoredState, extra))
           end},

          {"exported state can be serialized",
           fun() ->
               Config = #{llm => mock_llm()},
               {ok, State} = beamai_agent:new(Config),
               {ok, _, State1} = beamai_agent:run(State, <<"Test">>),

               %% 导出
               Exported = beamai_agent:export_state(State1),

               %% 序列化/反序列化
               Binary = term_to_binary(Exported),
               Restored = binary_to_term(Binary),

               %% 导入
               {ok, _RestoredState} = beamai_agent:import_state(Restored, Config)
           end}
         ]
     end}.

%%====================================================================
%% 边界情况测试
%%====================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"empty message handling",
           fun() ->
               Config = #{llm => mock_llm()},
               {ok, State} = beamai_agent:new(Config),
               %% 空消息应该能处理（虽然可能不是最佳实践）
               {ok, _Result, _NewState} = beamai_agent:run(State, <<"">>)
           end},

          {"unicode message handling",
           fun() ->
               Config = #{llm => mock_llm()},
               {ok, State} = beamai_agent:new(Config),
               {ok, _Result, _NewState} = beamai_agent:run(State, <<"你好世界"/utf8>>)
           end},

          {"large context handling",
           fun() ->
               LargeContext = #{
                   key1 => lists:seq(1, 1000),
                   key2 => <<"large binary data">>,
                   key3 => #{nested => #{deep => <<"value">>}}
               },
               Config = #{
                   llm => mock_llm(),
                   context => LargeContext
               },
               {ok, State} = beamai_agent:new(Config),
               ?assertEqual(lists:seq(1, 1000), beamai_agent:get_context(State, key1))
           end}
         ]
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
          {"original state unchanged after run",
           fun() ->
               Config = #{llm => mock_llm()},
               {ok, State0} = beamai_agent:new(Config),
               OrigMessages = beamai_agent:get_messages(State0),

               %% 运行对话
               {ok, _, _State1} = beamai_agent:run(State0, <<"Test">>),

               %% 原始状态不变
               ?assertEqual(OrigMessages, beamai_agent:get_messages(State0))
           end},

          {"state modifications return new state",
           fun() ->
               Config = #{llm => mock_llm()},
               {ok, State0} = beamai_agent:new(Config),

               %% 修改返回新状态
               State1 = beamai_agent:put_context(State0, key, <<"value">>),

               %% 原状态不变
               ?assertEqual(undefined, beamai_agent:get_context(State0, key)),
               %% 新状态有值
               ?assertEqual(<<"value">>, beamai_agent:get_context(State1, key))
           end},

          {"same input produces consistent output",
           fun() ->
               Config = #{
                   llm => mock_llm(),
                   system_prompt => <<"Consistent test">>
               },
               {ok, State0} = beamai_agent:new(Config),

               %% 多次运行相同输入
               {ok, Result1, _} = beamai_agent:run(State0, <<"Hello">>),
               {ok, Result2, _} = beamai_agent:run(State0, <<"Hello">>),

               %% 结果结构应该一致（内容可能因 LLM 而异）
               ?assertEqual(
                   maps:keys(Result1),
                   maps:keys(Result2)
               )
           end}
         ]
     end}.
