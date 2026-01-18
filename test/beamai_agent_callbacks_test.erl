%%%-------------------------------------------------------------------
%%% @doc beamai_agent 回调系统测试
%%%
%%% 测试 LangChain 风格的回调系统
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_callbacks_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

setup() ->
    application:ensure_all_started(beamai_runtime),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

%% 测试：初始化回调
init_callbacks_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"callbacks initialized as empty by default",
           fun() ->
               {ok, Agent} = beamai_agent:start_link(<<"test_cb_1">>, #{
                   system_prompt => <<"Test">>
               }),
               Callbacks = beamai_agent:get_callbacks(Agent),
               ?assertEqual(undefined, maps:get(on_llm_start, Callbacks)),
               ?assertEqual(undefined, maps:get(on_tool_start, Callbacks)),
               ?assertEqual(undefined, maps:get(on_chain_start, Callbacks)),
               beamai_agent:stop(Agent)
           end},

          {"callbacks can be set during initialization",
           fun() ->
               Self = self(),
               {ok, Agent} = beamai_agent:start_link(<<"test_cb_2">>, #{
                   system_prompt => <<"Test">>,
                   callbacks => #{
                       on_chain_start => fun(Input, _Meta) ->
                           Self ! {chain_start, Input}
                       end
                   }
               }),
               Callbacks = beamai_agent:get_callbacks(Agent),
               ?assert(is_function(maps:get(on_chain_start, Callbacks))),
               beamai_agent:stop(Agent)
           end}
         ]
     end}.

%% 测试：动态设置回调
set_callbacks_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"can set callbacks dynamically",
           fun() ->
               {ok, Agent} = beamai_agent:start_link(<<"test_cb_3">>, #{
                   system_prompt => <<"Test">>
               }),

               %% 初始为空
               Callbacks1 = beamai_agent:get_callbacks(Agent),
               ?assertEqual(undefined, maps:get(on_llm_start, Callbacks1)),

               %% 动态设置
               TestFun = fun(_Prompts, _Meta) -> ok end,
               ok = beamai_agent:set_callbacks(Agent, #{
                   on_llm_start => TestFun
               }),

               %% 验证已设置
               Callbacks2 = beamai_agent:get_callbacks(Agent),
               ?assert(is_function(maps:get(on_llm_start, Callbacks2))),

               beamai_agent:stop(Agent)
           end},

          {"set_callbacks preserves existing callbacks",
           fun() ->
               Fun1 = fun(_) -> ok end,
               Fun2 = fun(_, _) -> ok end,

               {ok, Agent} = beamai_agent:start_link(<<"test_cb_4">>, #{
                   system_prompt => <<"Test">>,
                   callbacks => #{
                       on_llm_start => Fun1
                   }
               }),

               %% 设置另一个回调
               ok = beamai_agent:set_callbacks(Agent, #{
                   on_tool_start => Fun2
               }),

               %% 验证两个都存在
               Callbacks = beamai_agent:get_callbacks(Agent),
               ?assert(is_function(maps:get(on_llm_start, Callbacks))),
               ?assert(is_function(maps:get(on_tool_start, Callbacks))),

               beamai_agent:stop(Agent)
           end}
         ]
     end}.

%% 测试：自定义事件
custom_event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"can emit and receive custom events",
           fun() ->
               Self = self(),
               {ok, Agent} = beamai_agent:start_link(<<"test_cb_5">>, #{
                   system_prompt => <<"Test">>,
                   callbacks => #{
                       on_custom_event => fun(Name, Data, _Meta) ->
                           Self ! {custom_event, Name, Data}
                       end
                   }
               }),

               %% 触发自定义事件
               beamai_agent:emit_custom_event(Agent, test_event, #{value => 42}),

               %% 等待接收
               receive
                   {custom_event, test_event, #{value := 42}} -> ok
               after 1000 ->
                   ?assert(false)
               end,

               beamai_agent:stop(Agent)
           end}
         ]
     end}.

%% 测试：兼容旧的 on_complete/on_error
backward_compatibility_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"on_complete maps to on_agent_finish",
           fun() ->
               TestFun = fun(_Result) -> ok end,
               {ok, Agent} = beamai_agent:start_link(<<"test_cb_6">>, #{
                   system_prompt => <<"Test">>,
                   on_complete => TestFun
               }),

               Callbacks = beamai_agent:get_callbacks(Agent),
               ?assert(is_function(maps:get(on_agent_finish, Callbacks))),

               beamai_agent:stop(Agent)
           end},

          {"on_error maps to on_chain_error",
           fun() ->
               TestFun = fun(_Error) -> ok end,
               {ok, Agent} = beamai_agent:start_link(<<"test_cb_7">>, #{
                   system_prompt => <<"Test">>,
                   on_error => TestFun
               }),

               Callbacks = beamai_agent:get_callbacks(Agent),
               ?assert(is_function(maps:get(on_chain_error, Callbacks))),

               beamai_agent:stop(Agent)
           end}
         ]
     end}.

%% 测试：所有回调类型
all_callback_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"all 18 callback types are supported",
           fun() ->
               CallbackNames = [
                   on_llm_start, on_llm_end, on_llm_error, on_llm_new_token,
                   on_tool_start, on_tool_end, on_tool_error,
                   on_agent_action, on_agent_finish,
                   on_chain_start, on_chain_end, on_chain_error,
                   on_retriever_start, on_retriever_end, on_retriever_error,
                   on_text, on_retry, on_custom_event
               ],

               {ok, Agent} = beamai_agent:start_link(<<"test_cb_8">>, #{
                   system_prompt => <<"Test">>
               }),

               Callbacks = beamai_agent:get_callbacks(Agent),

               %% 验证所有回调类型都存在于 map 中
               lists:foreach(fun(Name) ->
                   ?assert(maps:is_key(Name, Callbacks))
               end, CallbackNames),

               ?assertEqual(18, maps:size(Callbacks)),

               beamai_agent:stop(Agent)
           end}
         ]
     end}.
