%%%-------------------------------------------------------------------
%%% @doc beamai_agent 纯函数 API 测试
%%%
%%% 测试 new/1, run/2,3, restore_from_memory/2 等纯函数 API。
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

%%====================================================================
%% new/1 测试
%%====================================================================

new_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"new/1 creates state with auto-generated id",
           fun() ->
               Config = #{
                   system_prompt => <<"You are helpful">>,
                   llm => llm_client:create(mock, #{})
               },
               {ok, State} = beamai_agent:new(Config),
               ?assert(is_binary(get_agent_id(State))),
               ?assertEqual(<<"You are helpful">>, get_system_prompt(State))
           end},

          {"new/1 creates state with specified id",
           fun() ->
               Config = #{
                   id => <<"my-agent-id">>,
                   system_prompt => <<"Test agent">>,
                   llm => llm_client:create(mock, #{})
               },
               {ok, State} = beamai_agent:new(Config),
               ?assertEqual(<<"my-agent-id">>, get_agent_id(State))
           end},

          {"new/1 allows storage configuration",
           fun() ->
               %% 创建 Memory 实例（使用 mock store）
               {ok, _} = beamai_store_ets:start_link(test_store_new, #{}),
               {ok, Memory} = beamai_memory:new(#{
                   context_store => {beamai_store_ets, test_store_new},
                   thread_id => <<"test_thread_new">>
               }),

               Config = #{
                   system_prompt => <<"Test">>,
                   llm => llm_client:create(mock, #{}),
                   storage => Memory
               },
               {ok, State} = beamai_agent:new(Config),
               %% storage 应该已配置
               ?assertNotEqual(undefined, get_storage(State))
           end},

          {"new/1 initializes with empty messages",
           fun() ->
               Config = #{
                   llm => llm_client:create(mock, #{})
               },
               {ok, State} = beamai_agent:new(Config),
               ?assertEqual([], beamai_agent:get_messages(State)),
               ?assertEqual([], beamai_agent:get_full_messages(State)),
               ?assertEqual([], beamai_agent:get_scratchpad(State))
           end},

          {"new/1 accepts initial context",
           fun() ->
               Config = #{
                   llm => llm_client:create(mock, #{}),
                   context => #{<<"key1">> => <<"value1">>}
               },
               {ok, State} = beamai_agent:new(Config),
               ?assertEqual(<<"value1">>, beamai_agent:get_context(State, <<"key1">>))
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
          {"new/1 is safe for concurrent calls",
           fun() ->
               Config = #{
                   system_prompt => <<"Concurrent test">>,
                   llm => llm_client:create(mock, #{})
               },

               %% 并发创建 10 个状态
               Parent = self(),
               Pids = [spawn(fun() ->
                   Result = beamai_agent:new(Config),
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
          {"context operations do not modify original state",
           fun() ->
               Config = #{
                   system_prompt => <<"Immutable test">>,
                   llm => llm_client:create(mock, #{})
               },
               {ok, State} = beamai_agent:new(Config),

               %% 修改前记录 ID
               OriginalId = get_agent_id(State),

               %% 进行多次 context 操作
               _State1 = beamai_agent:put_context(State, <<"key1">>, <<"value1">>),
               _State2 = beamai_agent:update_context(State, #{<<"key2">> => <<"value2">>}),

               %% 原始状态不变
               ?assertEqual(OriginalId, get_agent_id(State)),
               ?assertEqual(undefined, beamai_agent:get_context(State, <<"key1">>))
           end}
         ]
     end}.

%%====================================================================
%% 辅助 API 测试
%%====================================================================

helper_api_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"get_context/set_context works correctly",
           fun() ->
               Config = #{
                   llm => llm_client:create(mock, #{}),
                   context => #{<<"key1">> => <<"value1">>}
               },
               {ok, State0} = beamai_agent:new(Config),

               %% 获取上下文
               ?assertEqual(#{<<"key1">> => <<"value1">>}, beamai_agent:get_context(State0)),
               ?assertEqual(<<"value1">>, beamai_agent:get_context(State0, <<"key1">>)),
               ?assertEqual(undefined, beamai_agent:get_context(State0, <<"key2">>)),
               ?assertEqual(<<"default">>, beamai_agent:get_context(State0, <<"key2">>, <<"default">>)),

               %% 设置上下文
               State1 = beamai_agent:set_context(State0, #{<<"key2">> => <<"value2">>}),
               ?assertEqual(#{<<"key2">> => <<"value2">>}, beamai_agent:get_context(State1))
           end},

          {"update_context merges context",
           fun() ->
               Config = #{
                   llm => llm_client:create(mock, #{}),
                   context => #{<<"key1">> => <<"value1">>}
               },
               {ok, State0} = beamai_agent:new(Config),

               %% 更新上下文
               State1 = beamai_agent:update_context(State0, #{<<"key2">> => <<"value2">>}),
               ?assertEqual(<<"value1">>, beamai_agent:get_context(State1, <<"key1">>)),
               ?assertEqual(<<"value2">>, beamai_agent:get_context(State1, <<"key2">>))
           end},

          {"put_context sets single value",
           fun() ->
               Config = #{
                   llm => llm_client:create(mock, #{})
               },
               {ok, State0} = beamai_agent:new(Config),

               %% 设置单个值（使用 binary 键）
               State1 = beamai_agent:put_context(State0, <<"key1">>, <<"value1">>),
               ?assertEqual(<<"value1">>, beamai_agent:get_context(State1, <<"key1">>))
           end},

          {"clear_messages works correctly",
           fun() ->
               Config = #{llm => llm_client:create(mock, #{})},
               {ok, State0} = beamai_agent:new(Config),

               %% 初始状态消息为空
               ?assertEqual([], beamai_agent:get_messages(State0)),

               %% 清空后仍然为空
               State1 = beamai_agent:clear_messages(State0),
               ?assertEqual([], beamai_agent:get_messages(State1))
           end},

          {"clear_scratchpad works correctly",
           fun() ->
               Config = #{llm => llm_client:create(mock, #{})},
               {ok, State0} = beamai_agent:new(Config),

               %% 初始状态 scratchpad 为空
               ?assertEqual([], beamai_agent:get_scratchpad(State0)),

               %% 清空后仍然为空
               State1 = beamai_agent:clear_scratchpad(State0),
               ?assertEqual([], beamai_agent:get_scratchpad(State1))
           end}
         ]
     end}.

%%====================================================================
%% graph_state 存储测试
%%====================================================================

graph_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"state uses graph_state for storage",
           fun() ->
               Config = #{
                   llm => llm_client:create(mock, #{}),
                   context => #{<<"key">> => <<"value">>}
               },
               {ok, #state{graph_state = GS}} = beamai_agent:new(Config),

               %% 验证 graph_state 是一个 map
               ?assert(is_map(GS)),

               %% 验证使用 binary 键
               ?assertEqual([], graph_state:get(GS, <<"messages">>, [])),
               ?assertEqual([], graph_state:get(GS, <<"full_messages">>, [])),
               ?assertEqual([], graph_state:get(GS, <<"scratchpad">>, [])),
               %% 用户上下文使用专用函数访问（避免键名冲突）
               ?assertEqual(#{<<"key">> => <<"value">>}, graph_state:get_context(GS))
           end}
         ]
     end}.
