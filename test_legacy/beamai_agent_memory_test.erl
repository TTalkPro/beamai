%%%-------------------------------------------------------------------
%%% @doc beamai_agent Memory 集成测试
%%%
%%% 测试 Agent 与 beamai_memory 的集成：
%%% - 单轮对话（带 Memory）
%%% - 多轮对话（带 Memory）
%%% - 会话恢复
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_memory_test).

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

%% @private 创建唯一的 ETS store 名称
unique_store_name() ->
    list_to_atom("test_store_" ++ integer_to_list(erlang:unique_integer([positive]))).

%% @private 创建 Memory 实例
create_memory() ->
    create_memory(<<"test_thread">>).

create_memory(ThreadId) ->
    StoreName = unique_store_name(),
    {ok, _} = beamai_store_ets:start_link(StoreName, #{}),
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => ThreadId
    }),
    Memory.

%% @private 从状态中获取消息数量
message_count(State) ->
    length(beamai_agent:get_messages(State)).

%%====================================================================
%% 单轮对话测试（带 Memory）
%%====================================================================

single_turn_with_memory_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"single turn with memory saves checkpoint",
           fun() ->
               Memory = create_memory(<<"single_turn_test">>),

               Config = #{
                   llm => mock_llm(),
                   system_prompt => <<"Test assistant">>,
                   storage => Memory
               },
               {ok, State} = beamai_agent:new(Config),
               {ok, _Result, _NewState} = beamai_agent:run(State, <<"Hello">>),

               %% 验证 checkpoint 已保存
               {ok, Checkpoints} = beamai_memory:list_checkpoints(Memory, #{
                   thread_id => <<"single_turn_test">>
               }),
               ?assert(length(Checkpoints) > 0)
           end},

          {"single turn checkpoint contains messages",
           fun() ->
               Memory = create_memory(<<"checkpoint_content_test">>),

               Config = #{
                   llm => mock_llm(),
                   storage => Memory
               },
               {ok, State} = beamai_agent:new(Config),
               {ok, _Result, _NewState} = beamai_agent:run(State, <<"Hello">>),

               %% 加载 checkpoint
               {ok, Data} = beamai_memory:load_latest_checkpoint(Memory, #{
                   thread_id => <<"checkpoint_content_test">>
               }),

               %% 验证包含消息
               Messages = maps:get(<<"messages">>, Data, maps:get(messages, Data, [])),
               ?assert(length(Messages) > 0)
           end}
         ]
     end}.

%%====================================================================
%% 多轮对话测试（带 Memory）
%%====================================================================

multi_turn_with_memory_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"multi-turn with memory saves multiple checkpoints",
           fun() ->
               Memory = create_memory(<<"multi_turn_test">>),

               Config = #{
                   llm => mock_llm(),
                   storage => Memory
               },
               {ok, State0} = beamai_agent:new(Config),

               %% 第一轮
               {ok, _, State1} = beamai_agent:run(State0, <<"First">>),

               %% 第二轮
               {ok, _, _State2} = beamai_agent:run(State1, <<"Second">>),

               %% 验证有 checkpoint
               {ok, Checkpoints} = beamai_memory:list_checkpoints(Memory, #{
                   thread_id => <<"multi_turn_test">>
               }),
               ?assert(length(Checkpoints) >= 1)
           end},

          {"multi-turn latest checkpoint has all messages",
           fun() ->
               Memory = create_memory(<<"latest_checkpoint_test">>),

               Config = #{
                   llm => mock_llm(),
                   storage => Memory
               },
               {ok, State0} = beamai_agent:new(Config),

               %% 运行多轮
               {ok, _, State1} = beamai_agent:run(State0, <<"Hello">>),
               {ok, _, State2} = beamai_agent:run(State1, <<"World">>),
               FinalMessageCount = message_count(State2),

               %% 加载最新 checkpoint
               {ok, Data} = beamai_memory:load_latest_checkpoint(Memory, #{
                   thread_id => <<"latest_checkpoint_test">>
               }),

               %% 验证消息数量
               Messages = maps:get(<<"messages">>, Data, maps:get(messages, Data, [])),
               ?assertEqual(FinalMessageCount, length(Messages))
           end}
         ]
     end}.

%%====================================================================
%% 会话恢复测试
%%====================================================================

restore_session_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"restore_from_memory recovers latest state",
           fun() ->
               Memory = create_memory(<<"restore_test">>),

               Config = #{
                   llm => mock_llm(),
                   system_prompt => <<"Restore test">>,
                   storage => Memory
               },

               %% 第一个会话
               {ok, State0} = beamai_agent:new(Config),
               {ok, _, State1} = beamai_agent:run(State0, <<"Remember this">>),
               {ok, _, _State2} = beamai_agent:run(State1, <<"And this">>),

               %% 恢复会话
               {ok, RestoredState} = beamai_agent:restore_from_memory(#{
                   llm => mock_llm(),
                   system_prompt => <<"Restore test">>
               }, Memory),

               %% 验证消息恢复
               ?assert(message_count(RestoredState) >= 2)
           end},

          {"restore_from_memory preserves context",
           fun() ->
               Memory = create_memory(<<"restore_context_test">>),

               Config = #{
                   llm => mock_llm(),
                   storage => Memory,
                   context => #{session_id => <<"abc123">>}
               },

               %% 原始会话
               {ok, State0} = beamai_agent:new(Config),
               State1 = beamai_agent:put_context(State0, user_data, <<"important">>),
               {ok, _, _State2} = beamai_agent:run(State1, <<"Save context">>),

               %% 恢复会话
               {ok, RestoredState} = beamai_agent:restore_from_memory(#{
                   llm => mock_llm()
               }, Memory),

               %% 验证上下文恢复
               %% 注意：session_id 来自原始配置，可能不在恢复状态中
               %% 但 user_data 应该在 checkpoint 中
               ?assertEqual(<<"important">>, beamai_agent:get_context(RestoredState, user_data))
           end},

          {"restored session can continue conversation",
           fun() ->
               Memory = create_memory(<<"continue_test">>),

               Config = #{
                   llm => mock_llm(),
                   storage => Memory
               },

               %% 原始会话
               {ok, State0} = beamai_agent:new(Config),
               {ok, _, State1} = beamai_agent:run(State0, <<"Start">>),
               OriginalCount = message_count(State1),

               %% 恢复会话
               {ok, RestoredState} = beamai_agent:restore_from_memory(#{
                   llm => mock_llm(),
                   storage => Memory
               }, Memory),

               %% 继续对话
               {ok, _, NewState} = beamai_agent:run(RestoredState, <<"Continue">>),

               %% 验证消息增加
               ?assert(message_count(NewState) > OriginalCount)
           end}
         ]
     end}.

%%====================================================================
%% 边界情况测试
%%====================================================================

memory_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"restore from empty memory returns fresh state",
           fun() ->
               Memory = create_memory(<<"empty_restore_test">>),

               %% 从空 Memory 恢复（没有任何 checkpoint）
               {ok, State} = beamai_agent:restore_from_memory(#{
                   llm => mock_llm()
               }, Memory),

               %% 应该得到空状态
               ?assertEqual([], beamai_agent:get_messages(State))
           end},

          {"new with restore_latest from empty memory",
           fun() ->
               Memory = create_memory(<<"new_restore_empty_test">>),

               Config = #{
                   llm => mock_llm(),
                   storage => Memory,
                   restore_latest => true
               },

               %% 从空 Memory 创建
               {ok, State} = beamai_agent:new(Config),
               ?assertEqual([], beamai_agent:get_messages(State))
           end},

          {"memory with multiple threads",
           fun() ->
               %% 创建共享 store
               StoreName = unique_store_name(),
               {ok, _} = beamai_store_ets:start_link(StoreName, #{}),

               %% 创建两个不同 thread 的 Memory
               {ok, Memory1} = beamai_memory:new(#{
                   context_store => {beamai_store_ets, StoreName},
                   thread_id => <<"thread_1">>
               }),
               {ok, Memory2} = beamai_memory:new(#{
                   context_store => {beamai_store_ets, StoreName},
                   thread_id => <<"thread_2">>
               }),

               %% 在 thread 1 运行
               {ok, State1} = beamai_agent:new(#{
                   llm => mock_llm(),
                   storage => Memory1
               }),
               {ok, _, _} = beamai_agent:run(State1, <<"Thread 1 message">>),

               %% 在 thread 2 运行
               {ok, State2} = beamai_agent:new(#{
                   llm => mock_llm(),
                   storage => Memory2
               }),
               {ok, _, _} = beamai_agent:run(State2, <<"Thread 2 message">>),

               %% 验证两个 thread 独立
               {ok, Data1} = beamai_memory:load_latest_checkpoint(Memory1, #{
                   thread_id => <<"thread_1">>
               }),
               {ok, Data2} = beamai_memory:load_latest_checkpoint(Memory2, #{
                   thread_id => <<"thread_2">>
               }),

               Messages1 = maps:get(<<"messages">>, Data1, maps:get(messages, Data1, [])),
               Messages2 = maps:get(<<"messages">>, Data2, maps:get(messages, Data2, [])),

               %% 两个 thread 的消息应该不同
               ?assertNotEqual(Messages1, Messages2)
           end}
         ]
     end}.
