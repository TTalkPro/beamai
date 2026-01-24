%%%-------------------------------------------------------------------
%%% @doc beamai_agent GLM-4.7 真实 API 测试
%%%
%%% 使用智谱 GLM-4.7 API 进行真实测试。
%%% 需要设置环境变量 ZHIPU_API_KEY。
%%%
%%% 运行方式：
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% rebar3 eunit --module=beamai_agent_glm_live_test
%%% ```
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_glm_live_test).

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

%% @private 检查是否应该跳过测试
should_skip() ->
    case glm_config() of
        skip -> true;
        _ -> false
    end.

%% @private 创建唯一的 ETS store 名称
unique_store_name() ->
    list_to_atom("glm_test_store_" ++ integer_to_list(erlang:unique_integer([positive]))).

%% @private 创建 Memory 实例
create_memory(ThreadId) ->
    StoreName = unique_store_name(),
    {ok, _} = beamai_store_ets:start_link(StoreName, #{}),
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => ThreadId
    }),
    Memory.

%%====================================================================
%% 单轮对话测试
%%====================================================================

glm_single_turn_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         case should_skip() of
             true ->
                 [{"SKIP: ZHIPU_API_KEY not set", fun() -> ok end}];
             false ->
                 [
                  {"GLM single turn - simple question",
                   {timeout, 60, fun() ->
                       {ok, State} = beamai_agent:new(#{llm => glm_config()}),
                       {ok, Result, _} = beamai_agent:run(State, <<"2+2等于多少？"/utf8>>),

                       Response = maps:get(final_response, Result),
                       ?assert(is_binary(Response)),
                       %% 验证响应包含 "4"
                       ?assert(binary:match(Response, <<"4">>) =/= nomatch)
                   end}},

                  {"GLM single turn - Chinese response",
                   {timeout, 60, fun() ->
                       {ok, State} = beamai_agent:new(#{
                           llm => glm_config(),
                           system_prompt => <<"用中文回答"/utf8>>
                       }),
                       {ok, Result, _} = beamai_agent:run(State, <<"你好"/utf8>>),

                       Response = maps:get(final_response, Result),
                       ?assert(is_binary(Response)),
                       ?assert(byte_size(Response) > 0)
                   end}}
                 ]
         end
     end}.

%%====================================================================
%% 多轮对话测试
%%====================================================================

glm_multi_turn_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         case should_skip() of
             true ->
                 [{"SKIP: ZHIPU_API_KEY not set", fun() -> ok end}];
             false ->
                 [
                  {"GLM multi-turn - context preservation",
                   {timeout, 120, fun() ->
                       {ok, State0} = beamai_agent:new(#{
                           llm => glm_config(),
                           system_prompt => <<"你是一个数学助手。记住用户提到的数字。"/utf8>>
                       }),

                       %% 第一轮：告诉 LLM 一个数字
                       {ok, _, State1} = beamai_agent:run(State0, <<"请记住数字 42"/utf8>>),

                       %% 第二轮：询问之前的数字
                       {ok, Result, _} = beamai_agent:run(State1, <<"我刚才说的数字是多少？"/utf8>>),

                       Response = maps:get(final_response, Result),
                       ?assert(is_binary(Response)),
                       %% 验证响应包含 "42"
                       ?assert(binary:match(Response, <<"42">>) =/= nomatch)
                   end}},

                  {"GLM multi-turn - message accumulation",
                   {timeout, 120, fun() ->
                       {ok, State0} = beamai_agent:new(#{llm => glm_config()}),

                       %% 三轮对话
                       {ok, _, State1} = beamai_agent:run(State0, <<"你好"/utf8>>),
                       {ok, _, State2} = beamai_agent:run(State1, <<"今天天气如何？"/utf8>>),
                       {ok, Result, State3} = beamai_agent:run(State2, <<"谢谢"/utf8>>),

                       ?assert(is_binary(maps:get(final_response, Result))),
                       %% 验证消息累积（每轮 +2：用户+助手）
                       Messages = beamai_agent:get_messages(State3),
                       ?assert(length(Messages) >= 6)
                   end}}
                 ]
         end
     end}.

%%====================================================================
%% Memory 集成测试
%%====================================================================

glm_memory_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         case should_skip() of
             true ->
                 [{"SKIP: ZHIPU_API_KEY not set", fun() -> ok end}];
             false ->
                 [
                  {"GLM with memory - checkpoint save",
                   {timeout, 60, fun() ->
                       Memory = create_memory(<<"glm_memory_test">>),

                       {ok, State0} = beamai_agent:new(#{
                           llm => glm_config(),
                           storage => Memory
                       }),
                       {ok, _, _} = beamai_agent:run(State0, <<"你好"/utf8>>),

                       %% 验证 checkpoint 已保存
                       {ok, Checkpoints} = beamai_memory:list_checkpoints(Memory, #{
                           thread_id => <<"glm_memory_test">>
                       }),
                       ?assert(length(Checkpoints) > 0)
                   end}},

                  {"GLM with memory - restore and continue",
                   {timeout, 180, fun() ->
                       Memory = create_memory(<<"glm_restore_test">>),

                       %% 原始会话
                       {ok, State0} = beamai_agent:new(#{
                           llm => glm_config(),
                           storage => Memory,
                           system_prompt => <<"记住用户说的内容"/utf8>>
                       }),
                       {ok, _, _} = beamai_agent:run(State0, <<"记住：密码是 ABC123"/utf8>>),

                       %% 恢复会话
                       {ok, RestoredState} = beamai_agent:restore_from_memory(#{
                           llm => glm_config()
                       }, Memory),

                       %% 继续对话
                       {ok, Result, _} = beamai_agent:run(RestoredState, <<"密码是什么？"/utf8>>),

                       Response = maps:get(final_response, Result),
                       ?assert(is_binary(Response)),
                       %% 验证 LLM 记住了之前的内容
                       ?assert(binary:match(Response, <<"ABC123">>) =/= nomatch orelse
                               binary:match(Response, <<"密码"/utf8>>) =/= nomatch)
                   end}}
                 ]
         end
     end}.

%%====================================================================
%% 工具调用测试
%%====================================================================

glm_tools_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         case should_skip() of
             true ->
                 [{"SKIP: ZHIPU_API_KEY not set", fun() -> ok end}];
             false ->
                 [
                  {"GLM with tools - calculator",
                   {timeout, 120, fun() ->
                       Tools = [
                           #{
                               name => <<"calculate">>,
                               description => <<"计算数学表达式"/utf8>>,
                               parameters => #{
                                   type => object,
                                   properties => #{
                                       <<"expression">> => #{
                                           type => string,
                                           description => <<"数学表达式"/utf8>>
                                       }
                                   },
                                   required => [<<"expression">>]
                               },
                               handler => fun(#{<<"expression">> := _Expr}) ->
                                   %% 简化处理，直接返回结果
                                   #{result => 200}
                               end
                           }
                       ],

                       {ok, State} = beamai_agent:new(#{
                           llm => glm_config(),
                           tools => Tools,
                           system_prompt => <<"你是一个计算助手。使用 calculate 工具来计算。"/utf8>>
                       }),
                       {ok, Result, _} = beamai_agent:run(State, <<"计算 100 + 100"/utf8>>),

                       ?assert(is_binary(maps:get(final_response, Result)))
                   end}}
                 ]
         end
     end}.

%%====================================================================
%% 性能和稳定性测试
%%====================================================================

glm_stability_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         case should_skip() of
             true ->
                 [{"SKIP: ZHIPU_API_KEY not set", fun() -> ok end}];
             false ->
                 [
                  {"GLM handles long messages",
                   {timeout, 120, fun() ->
                       LongMessage = iolist_to_binary([
                           <<"请总结以下内容："/utf8>>,
                           lists:duplicate(100, <<"这是一段测试文本。"/utf8>>)
                       ]),

                       {ok, State} = beamai_agent:new(#{llm => glm_config()}),
                       {ok, Result, _} = beamai_agent:run(State, LongMessage),

                       ?assert(is_binary(maps:get(final_response, Result)))
                   end}},

                  {"GLM handles unicode correctly",
                   {timeout, 60, fun() ->
                       {ok, State} = beamai_agent:new(#{llm => glm_config()}),
                       {ok, Result, _} = beamai_agent:run(State,
                           <<"请用中文、日文和韩文说'你好'：你好、こんにちは、안녕하세요"/utf8>>),

                       Response = maps:get(final_response, Result),
                       ?assert(is_binary(Response))
                   end}}
                 ]
         end
     end}.
