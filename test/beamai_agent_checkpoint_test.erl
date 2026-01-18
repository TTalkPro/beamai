%%%-------------------------------------------------------------------
%%% @doc beamai_agent checkpoint 功能测试
%%%
%%% 测试 beamai_agent 与 beamai_memory checkpoint 的集成
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_checkpoint_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

setup() ->
    %% 确保 beamai_runtime 应用已启动
    application:ensure_all_started(beamai_runtime),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

%% 测试：存储未启用时返回错误
storage_not_enabled_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"save_checkpoint returns error when storage disabled",
           fun() ->
               {ok, Agent} = beamai_agent:start_link(<<"test1">>, #{
                   system_prompt => <<"Test agent">>
               }),
               ?assertEqual({error, storage_not_enabled}, beamai_agent:save_checkpoint(Agent)),
               beamai_agent:stop(Agent)
           end},

          {"load_checkpoint returns error when storage disabled",
           fun() ->
               {ok, Agent} = beamai_agent:start_link(<<"test2">>, #{
                   system_prompt => <<"Test agent">>
               }),
               ?assertEqual({error, storage_not_enabled},
                           beamai_agent:load_checkpoint(Agent, <<"some_id">>)),
               beamai_agent:stop(Agent)
           end},

          {"list_checkpoints returns error when storage disabled",
           fun() ->
               {ok, Agent} = beamai_agent:start_link(<<"test3">>, #{
                   system_prompt => <<"Test agent">>
               }),
               ?assertEqual({error, storage_not_enabled},
                           beamai_agent:list_checkpoints(Agent)),
               beamai_agent:stop(Agent)
           end}
         ]
     end}.

%% 测试：启用存储后 checkpoint 操作
%%
%% 注意：需要传入实际的 storage (beamai_memory 实例)，而非 enable_storage 标志
storage_enabled_test_() ->
    {setup,
     fun() ->
         setup(),
         %% 创建 ETS Store 和 Memory 实例用于测试
         StoreName = list_to_atom("test_store_" ++ integer_to_list(erlang:unique_integer([positive]))),
         {ok, _StorePid} = beamai_store_ets:start_link(StoreName, #{}),
         {ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, StoreName}}),
         {StoreName, Memory}
     end,
     fun({StoreName, _Memory}) ->
         %% 停止 Store 进程
         catch beamai_store_ets:stop(StoreName),
         cleanup(ok)
     end,
     fun({_StoreName, Memory}) ->
         [
          {"can save and load checkpoint with storage enabled",
           fun() ->
               {ok, Agent} = beamai_agent:start_link(<<"test_storage_1">>, #{
                   system_prompt => <<"Test agent">>,
                   storage => Memory
               }),

               %% 保存检查点
               {ok, CpId} = beamai_agent:save_checkpoint(Agent, #{tag => test1}),
               ?assert(is_binary(CpId)),

               %% 加载检查点
               {ok, Checkpoint} = beamai_agent:load_checkpoint(Agent, CpId),
               ?assert(is_map(Checkpoint)),

               %% 列出检查点
               {ok, Checkpoints} = beamai_agent:list_checkpoints(Agent),
               ?assert(length(Checkpoints) >= 1),

               beamai_agent:stop(Agent)
           end},

          {"can load latest checkpoint",
           fun() ->
               %% 为第二个测试创建新的 ETS Store 和 Memory 实例
               StoreName2 = list_to_atom("test_store2_" ++ integer_to_list(erlang:unique_integer([positive]))),
               {ok, _StorePid2} = beamai_store_ets:start_link(StoreName2, #{}),
               {ok, Memory2} = beamai_memory:new(#{context_store => {beamai_store_ets, StoreName2}}),

               {ok, Agent} = beamai_agent:start_link(<<"test_storage_2">>, #{
                   system_prompt => <<"Test agent">>,
                   storage => Memory2
               }),

               %% 保存多个检查点
               {ok, _CpId1} = beamai_agent:save_checkpoint(Agent, #{tag => v1}),
               timer:sleep(10), %% 确保时间戳不同
               {ok, _CpId2} = beamai_agent:save_checkpoint(Agent, #{tag => v2}),

               %% 加载最新检查点 - 返回的是检查点内容（messages, scratchpad, metadata）
               %% 不是检查点元数据（id 等）
               {ok, Latest} = beamai_agent:load_latest_checkpoint(Agent),
               ?assert(is_map(Latest)),
               %% 验证返回了正确的 metadata（来自最新检查点）
               ?assertEqual(#{tag => v2}, maps:get(metadata, Latest, undefined)),

               beamai_agent:stop(Agent),
               catch beamai_store_ets:stop(StoreName2)
           end}
         ]
     end}.

%% 测试：恢复检查点
restore_checkpoint_test_() ->
    {setup,
     fun() ->
         setup(),
         StoreName = list_to_atom("test_restore_store_" ++ integer_to_list(erlang:unique_integer([positive]))),
         {ok, _StorePid} = beamai_store_ets:start_link(StoreName, #{}),
         {ok, Memory} = beamai_memory:new(#{context_store => {beamai_store_ets, StoreName}}),
         {StoreName, Memory}
     end,
     fun({StoreName, _Memory}) ->
         catch beamai_store_ets:stop(StoreName),
         cleanup(ok)
     end,
     fun({_StoreName, Memory}) ->
         [
          {"can restore from checkpoint",
           fun() ->
               {ok, Agent} = beamai_agent:start_link(<<"test_restore_1">>, #{
                   system_prompt => <<"Test agent">>,
                   storage => Memory
               }),

               %% 保存检查点
               {ok, CpId} = beamai_agent:save_checkpoint(Agent, #{tag => restore_test}),

               %% 恢复检查点
               ?assertEqual(ok, beamai_agent:restore_from_checkpoint(Agent, CpId)),

               beamai_agent:stop(Agent)
           end}
         ]
     end}.
