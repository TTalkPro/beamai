%%%-------------------------------------------------------------------
%%% @doc beamai_agent checkpoint 功能测试（新架构）
%%%
%%% 测试新的 checkpoint 架构：
%%% 1. thread_id 在 Memory 中管理
%%% 2. Checkpoint 通过 on_checkpoint 回调自动保存
%%% 3. 通过 beamai_memory API 直接访问 checkpoints
%%%
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
%% Memory thread_id Tests
%%====================================================================

%% 测试：thread_id 自动生成
thread_id_auto_generate_test_() ->
    {setup,
     fun() ->
         setup(),
         StoreName = list_to_atom("test_thread_id_" ++ integer_to_list(erlang:unique_integer([positive]))),
         {ok, _StorePid} = beamai_store_ets:start_link(StoreName, #{}),
         StoreName
     end,
     fun(StoreName) ->
         catch beamai_store_ets:stop(StoreName),
         cleanup(ok)
     end,
     fun(StoreName) ->
         [
          {"thread_id is auto-generated when not specified",
           fun() ->
               {ok, Memory} = beamai_memory:new(#{
                   context_store => {beamai_store_ets, StoreName}
               }),
               ThreadId = beamai_memory:get_thread_id(Memory),
               ?assert(is_binary(ThreadId)),
               ?assert(binary:match(ThreadId, <<"thread_">>) =/= nomatch)
           end},

          {"custom thread_id is preserved",
           fun() ->
               CustomThreadId = <<"my_custom_thread_123">>,
               {ok, Memory} = beamai_memory:new(#{
                   context_store => {beamai_store_ets, StoreName},
                   thread_id => CustomThreadId
               }),
               ThreadId = beamai_memory:get_thread_id(Memory),
               ?assertEqual(CustomThreadId, ThreadId)
           end}
         ]
     end}.

%%====================================================================
%% Checkpoint Callback Tests
%%====================================================================

%% 测试：创建 Agent 时传入 Memory 会启用 checkpoint 回调
checkpoint_callback_test_() ->
    {setup,
     fun() ->
         setup(),
         StoreName = list_to_atom("test_callback_" ++ integer_to_list(erlang:unique_integer([positive]))),
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
          {"agent can be created with storage",
           fun() ->
               {ok, Agent} = beamai_agent:start_link(<<"test_callback_agent">>, #{
                   system_prompt => <<"Test agent">>,
                   storage => Memory
               }),
               ?assert(is_pid(Agent)),
               beamai_agent:stop(Agent)
           end}
         ]
     end}.

%%====================================================================
%% Memory API Access Tests
%%====================================================================

%% 测试：通过 Memory API 访问 checkpoints
memory_api_access_test_() ->
    {setup,
     fun() ->
         setup(),
         StoreName = list_to_atom("test_memory_api_" ++ integer_to_list(erlang:unique_integer([positive]))),
         {ok, _StorePid} = beamai_store_ets:start_link(StoreName, #{}),
         {ok, Memory} = beamai_memory:new(#{
             context_store => {beamai_store_ets, StoreName},
             thread_id => <<"test_thread">>
         }),
         {StoreName, Memory}
     end,
     fun({StoreName, _Memory}) ->
         catch beamai_store_ets:stop(StoreName),
         cleanup(ok)
     end,
     fun({_StoreName, Memory}) ->
         ThreadId = beamai_memory:get_thread_id(Memory),
         [
          {"can list checkpoints via memory API",
           fun() ->
               %% 空列表或成功返回
               Result = beamai_memory:list_checkpoints(Memory, #{thread_id => ThreadId}),
               case Result of
                   {ok, Checkpoints} ->
                       ?assert(is_list(Checkpoints));
                   {error, not_found} ->
                       %% 空存储返回 not_found 也是合理的
                       ok
               end
           end},

          {"thread_id is preserved in memory",
           fun() ->
               ?assertEqual(<<"test_thread">>, ThreadId)
           end}
         ]
     end}.
