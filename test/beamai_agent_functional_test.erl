%%%-------------------------------------------------------------------
%%% @doc beamai_agent 纯函数 API 测试
%%%
%%% 测试 run_once/2, run_with_state/3, create_state/1,2,
%%% export_state/1, import_state/2 等纯函数 API。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_functional_test).

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
%% 辅助函数 - 使用记录访问器代替 element()
%%====================================================================

%% @private 从状态中获取 Agent ID
get_agent_id(#state{config = #agent_config{id = Id}}) -> Id.

%% @private 从状态中获取系统提示词
get_system_prompt(#state{config = #agent_config{system_prompt = Prompt}}) -> Prompt.

%% @private 从状态中获取存储后端
get_storage(#state{config = #agent_config{storage = Storage}}) -> Storage.

%% @private 从状态中获取消息列表
get_messages(#state{messages = Messages}) -> Messages.

%% @private 从状态中获取 scratchpad
get_scratchpad(#state{scratchpad = Scratchpad}) -> Scratchpad.

%%====================================================================
%% create_state 测试
%%====================================================================

create_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"create_state/1 creates state with auto-generated id",
           fun() ->
               Config = #{
                   system_prompt => <<"You are helpful">>,
                   llm => llm_client:create(mock, #{})
               },
               {ok, State} = beamai_agent:create_state(Config),
               ?assert(is_binary(get_agent_id(State))),
               ?assertEqual(<<"You are helpful">>, get_system_prompt(State))
           end},

          {"create_state/2 creates state with specified id",
           fun() ->
               Config = #{
                   system_prompt => <<"Test agent">>,
                   llm => llm_client:create(mock, #{})
               },
               {ok, State} = beamai_agent:create_state(<<"my-agent-id">>, Config),
               ?assertEqual(<<"my-agent-id">>, get_agent_id(State))
           end},

          {"create_state disables storage in pure function mode",
           fun() ->
               Config = #{
                   system_prompt => <<"Test">>,
                   llm => llm_client:create(mock, #{}),
                   enable_storage => true  %% 用户尝试启用
               },
               {ok, State} = beamai_agent:create_state(Config),
               %% storage 应该是 undefined（被强制禁用）
               ?assertEqual(undefined, get_storage(State))
           end}
         ]
     end}.

%%====================================================================
%% export_state/import_state 测试
%%====================================================================

export_import_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"export_state returns serializable map with messages",
           fun() ->
               Config = #{
                   system_prompt => <<"Test prompt">>,
                   llm => llm_client:create(mock, #{model => <<"test">>}),
                   max_iterations => 5
               },
               {ok, State} = beamai_agent:create_state(<<"export-test">>, Config),
               Exported = beamai_agent:export_state(State),

               ?assert(is_map(Exported)),
               %% export_state 只导出对话相关数据，不包含配置
               ?assertEqual([], maps:get(messages, Exported)),
               ?assertEqual([], maps:get(full_messages, Exported)),
               ?assertEqual([], maps:get(scratchpad, Exported)),
               ?assert(is_map(maps:get(context, Exported)))
           end},

          {"import_state restores state from exported data",
           fun() ->
               %% 创建原始状态
               Config = #{
                   system_prompt => <<"Original prompt">>,
                   llm => llm_client:create(mock, #{})
               },
               {ok, OrigState} = beamai_agent:create_state(<<"import-test">>, Config),

               %% 导出（只包含对话数据）
               Exported = beamai_agent:export_state(OrigState),

               %% 导入时需要传入配置
               {ok, RestoredState} = beamai_agent:import_state_pure(Exported, Config),

               %% import_state 会生成新的 ID，但保留对话数据
               ?assert(is_binary(get_agent_id(RestoredState))),
               ?assertEqual(<<"Original prompt">>, get_system_prompt(RestoredState))
           end},

          {"import_state restores messages and scratchpad",
           fun() ->
               %% 模拟已有对话历史的导出数据（只包含对话数据）
               ExportedData = #{
                   messages => [
                       #{role => user, content => <<"Hello">>},
                       #{role => assistant, content => <<"Hi there!">>},
                       #{role => user, content => <<"How are you?">>}
                   ],
                   full_messages => [
                       #{role => user, content => <<"Hello">>},
                       #{role => assistant, content => <<"Hi there!">>},
                       #{role => user, content => <<"How are you?">>}
                   ],
                   scratchpad => [
                       #{type => llm_response, content => <<"Hello">>}
                   ],
                   context => #{}
               },

               %% import_state 需要配置
               Config = #{
                   system_prompt => <<"Test">>,
                   llm => llm_client:create(mock, #{})
               },
               {ok, State} = beamai_agent:import_state_pure(ExportedData, Config),

               %% 验证 messages 恢复
               Messages = get_messages(State),
               ?assertEqual(3, length(Messages)),

               %% 验证 scratchpad 恢复
               Scratchpad = get_scratchpad(State),
               ?assertEqual(1, length(Scratchpad))
           end},

          {"import_state allows config override",
           fun() ->
               ExportedData = #{
                   messages => [],
                   full_messages => [],
                   scratchpad => [],
                   context => #{}
               },

               %% 使用配置创建状态
               {ok, State} = beamai_agent:import_state_pure(ExportedData, #{
                   system_prompt => <<"New prompt">>,
                   llm => llm_client:create(mock, #{})
               }),

               ?assertEqual(<<"New prompt">>, get_system_prompt(State))
           end},

          {"export/import roundtrip preserves data",
           fun() ->
               Config = #{
                   system_prompt => <<"Roundtrip test">>,
                   llm => llm_client:create(mock, #{model => <<"gpt-4">>}),
                   max_iterations => 7,
                   tools => [
                       #{name => <<"test_tool">>, description => <<"A test tool">>}
                   ]
               },
               {ok, State1} = beamai_agent:create_state(<<"roundtrip">>, Config),

               %% 导出 -> 序列化 -> 反序列化 -> 导入
               Exported = beamai_agent:export_state(State1),
               Binary = term_to_binary(Exported),
               Restored = binary_to_term(Binary),
               {ok, State2} = beamai_agent:import_state_pure(Restored, Config),

               %% 验证对话数据保持一致
               ?assertEqual(get_messages(State1), get_messages(State2)),
               ?assertEqual(get_scratchpad(State1), get_scratchpad(State2)),
               %% 配置通过 Config 传入，应该相同
               ?assertEqual(get_system_prompt(State1), get_system_prompt(State2))
           end}
         ]
     end}.

%%====================================================================
%% 并发安全测试
%%====================================================================

concurrency_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"create_state is safe for concurrent calls",
           fun() ->
               Config = #{
                   system_prompt => <<"Concurrent test">>,
                   llm => llm_client:create(mock, #{})
               },

               %% 并发创建 10 个状态
               Parent = self(),
               Pids = [spawn(fun() ->
                   Result = beamai_agent:create_state(Config),
                   Parent ! {self(), Result}
               end) || _ <- lists:seq(1, 10)],

               %% 收集结果
               Results = [receive {Pid, R} -> R end || Pid <- Pids],

               %% 所有都应该成功
               lists:foreach(fun({ok, _State}) -> ok end, Results),

               %% 所有 ID 应该唯一
               Ids = [get_agent_id(S) || {ok, S} <- Results],
               ?assertEqual(10, length(lists:usort(Ids)))
           end}
         ]
     end}.

%%====================================================================
%% 状态不可变性测试
%%====================================================================

immutability_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"export_state does not modify original state",
           fun() ->
               Config = #{
                   system_prompt => <<"Immutable test">>,
                   llm => llm_client:create(mock, #{})
               },
               {ok, State} = beamai_agent:create_state(Config),

               %% 导出前记录 ID
               OriginalId = get_agent_id(State),

               %% 多次导出
               _Export1 = beamai_agent:export_state(State),
               _Export2 = beamai_agent:export_state(State),

               %% 原始状态不变
               ?assertEqual(OriginalId, get_agent_id(State))
           end}
         ]
     end}.
