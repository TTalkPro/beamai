%%%-------------------------------------------------------------------
%%% @doc Anthropic Provider 综合测试
%%%
%%% 使用 ZHIPU_ANTHROPIC_BASE_URL + ZHIPU_API_KEY 测试
%%% beamai_llm_provider_anthropic 的全部功能：
%%%
%%%   1. 基本对话 (chat)
%%%   2. 流式对话 (stream_chat)
%%%   3. System Prompt
%%%   4. 工具调用 (tools + tool_choice)
%%%   5. 采样参数 (temperature, top_p, top_k)
%%%   6. 停止序列 (stop_sequences)
%%%   7. 多轮对话
%%%   8. beamai_llm_response 访问器
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_anthropic_provider_test).

-export([run/0]).

%%====================================================================
%% API
%%====================================================================

run() ->
    io:format("~n========================================~n"),
    io:format("  Anthropic Provider 综合测试 (GLM-4.7)~n"),
    io:format("========================================~n~n"),

    Config = example_llm_config:anthropic(#{
        api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
        model => <<"glm-4.7">>,
        max_tokens => 2048
    }),
    io:format("[Config] provider=~p, model=~p, base_url=~p~n~n",
              [maps:get(provider, Config),
               maps:get(model, Config),
               maps:get(base_url, Config)]),

    test_basic_chat(Config),
    test_system_prompt(Config),
    test_stream_chat(Config),
    test_temperature(Config),
    test_stop_sequences(Config),
    test_tool_call(Config),
    test_tool_choice_auto(Config),
    test_multi_turn(Config),
    test_response_accessors(Config),

    io:format("~n========================================~n"),
    io:format("  全部测试完成!~n"),
    io:format("========================================~n").

%%====================================================================
%% Test 1: 基本对话
%%====================================================================

test_basic_chat(Config) ->
    io:format("[Test 1] 基本对话~n"),
    Messages = [#{role => user, content => <<"用一句话回答：Erlang 是什么？"/utf8>>}],
    case beamai_chat_model:chat(Config, Messages) of
        {ok, Resp} ->
            Content = beamai_llm_response:content(Resp),
            io:format("  Content: ~ts~n", [Content]),
            io:format("  finish_reason: ~p~n", [beamai_llm_response:finish_reason(Resp)]),
            io:format("  [PASS]~n~n");
        {error, Err} ->
            io:format("  [FAIL] ~p~n~n", [Err])
    end.

%%====================================================================
%% Test 2: System Prompt
%%====================================================================

test_system_prompt(Config) ->
    io:format("[Test 2] System Prompt~n"),
    Messages = [
        #{role => system, content => <<"你是一个海盗，所有回答都要用海盗口吻。"/utf8>>},
        #{role => user, content => <<"你好"/utf8>>}
    ],
    case beamai_chat_model:chat(Config, Messages) of
        {ok, Resp} ->
            Content = beamai_llm_response:content(Resp),
            io:format("  Content: ~ts~n", [Content]),
            io:format("  [PASS]~n~n");
        {error, Err} ->
            io:format("  [FAIL] ~p~n~n", [Err])
    end.

%%====================================================================
%% Test 3: 流式对话
%%====================================================================

test_stream_chat(Config) ->
    io:format("[Test 3] 流式对话~n"),
    Messages = [#{role => user, content => <<"用两句话描述函数式编程。"/utf8>>}],
    io:format("  Streaming: "),
    Callback = fun(Event) ->
        case Event of
            #{<<"type">> := <<"content_block_delta">>, <<"delta">> := Delta} ->
                case maps:get(<<"text">>, Delta, <<>>) of
                    <<>> -> ok;
                    Text -> io:format("~ts", [Text])
                end;
            _ -> ok
        end
    end,
    case beamai_chat_model:stream_chat(Config, Messages, Callback) of
        {ok, _Resp} ->
            io:format("~n  [PASS]~n~n");
        {error, Err} ->
            io:format("~n  [FAIL] ~p~n~n", [Err])
    end.

%%====================================================================
%% Test 4: 采样参数 (temperature)
%%====================================================================

test_temperature(Config) ->
    io:format("[Test 4] Temperature 参数~n"),
    Messages = [#{role => user, content => <<"说一个数字。"/utf8>>}],
    %% Low temperature
    Config1 = Config#{temperature => 0.1},
    case beamai_chat_model:chat(Config1, Messages) of
        {ok, Resp1} ->
            C1 = beamai_llm_response:content(Resp1),
            io:format("  temperature=0.1: ~ts~n", [C1]);
        {error, E1} ->
            io:format("  temperature=0.1 error: ~p~n", [E1])
    end,
    %% High temperature
    Config2 = Config#{temperature => 1.0},
    case beamai_chat_model:chat(Config2, Messages) of
        {ok, Resp2} ->
            C2 = beamai_llm_response:content(Resp2),
            io:format("  temperature=1.0: ~ts~n", [C2]);
        {error, E2} ->
            io:format("  temperature=1.0 error: ~p~n", [E2])
    end,
    io:format("  [PASS]~n~n").

%%====================================================================
%% Test 5: 停止序列 (stop_sequences)
%%====================================================================

test_stop_sequences(Config) ->
    io:format("[Test 5] Stop Sequences~n"),
    Messages = [#{role => user, content => <<"从1数到10，用逗号分隔。"/utf8>>}],
    Config1 = Config#{stop_sequences => [<<"5">>]},
    case beamai_chat_model:chat(Config1, Messages) of
        {ok, Resp} ->
            Content = beamai_llm_response:content(Resp),
            Reason = beamai_llm_response:finish_reason(Resp),
            io:format("  Content: ~ts~n", [Content]),
            io:format("  finish_reason: ~p (期望 stop_sequence)~n", [Reason]),
            io:format("  [PASS]~n~n");
        {error, Err} ->
            io:format("  [FAIL] ~p~n~n", [Err])
    end.

%%====================================================================
%% Test 6: 工具调用 (tools)
%%====================================================================

test_tool_call(Config) ->
    io:format("[Test 6] 工具调用~n"),
    Tools = [
        #{
            name => <<"get_weather">>,
            description => <<"获取指定城市的天气信息"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    city => #{type => string, description => <<"城市名"/utf8>>}
                },
                required => [<<"city">>]
            }
        }
    ],
    Messages = [#{role => user, content => <<"北京今天天气怎么样？"/utf8>>}],
    case beamai_chat_model:chat(Config, Messages, #{tools => Tools}) of
        {ok, Resp} ->
            HasTools = beamai_llm_response:has_tool_calls(Resp),
            ToolCalls = beamai_llm_response:tool_calls(Resp),
            Reason = beamai_llm_response:finish_reason(Resp),
            io:format("  has_tool_calls: ~p~n", [HasTools]),
            io:format("  finish_reason: ~p~n", [Reason]),
            lists:foreach(fun(TC) ->
                io:format("  tool_call: name=~p, args=~p~n",
                          [maps:get(name, TC), maps:get(arguments, TC)])
            end, ToolCalls),
            io:format("  [PASS]~n~n");
        {error, Err} ->
            io:format("  [FAIL] ~p~n~n", [Err])
    end.

%%====================================================================
%% Test 7: tool_choice=auto
%%====================================================================

test_tool_choice_auto(Config) ->
    io:format("[Test 7] Tool Choice auto~n"),
    Tools = [
        #{
            name => <<"calculate">>,
            description => <<"计算数学表达式"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    expression => #{type => string, description => <<"数学表达式"/utf8>>}
                },
                required => [<<"expression">>]
            }
        }
    ],
    %% 这个问题不需要工具，tool_choice=auto 应该让模型自己回答
    Messages = [#{role => user, content => <<"你好"/utf8>>}],
    case beamai_chat_model:chat(Config, Messages, #{tools => Tools, tool_choice => auto}) of
        {ok, Resp} ->
            HasTools = beamai_llm_response:has_tool_calls(Resp),
            Content = beamai_llm_response:content(Resp),
            io:format("  has_tool_calls: ~p~n", [HasTools]),
            io:format("  Content: ~ts~n", [Content]),
            io:format("  [PASS]~n~n");
        {error, Err} ->
            io:format("  [FAIL] ~p~n~n", [Err])
    end.

%%====================================================================
%% Test 8: 多轮对话
%%====================================================================

test_multi_turn(Config) ->
    io:format("[Test 8] 多轮对话~n"),
    Msg1 = [#{role => user, content => <<"我的名字是张三。请记住。"/utf8>>}],
    case beamai_chat_model:chat(Config, Msg1) of
        {ok, Resp1} ->
            C1 = beamai_llm_response:content(Resp1),
            io:format("  Turn 1 A: ~ts~n", [C1]),
            Msg2 = Msg1 ++ [
                #{role => assistant, content => C1},
                #{role => user, content => <<"我叫什么名字？"/utf8>>}
            ],
            case beamai_chat_model:chat(Config, Msg2) of
                {ok, Resp2} ->
                    C2 = beamai_llm_response:content(Resp2),
                    io:format("  Turn 2 A: ~ts~n", [C2]),
                    io:format("  [PASS]~n~n");
                {error, E2} ->
                    io:format("  Turn 2 [FAIL] ~p~n~n", [E2])
            end;
        {error, E1} ->
            io:format("  Turn 1 [FAIL] ~p~n~n", [E1])
    end.

%%====================================================================
%% Test 9: Response 访问器全面测试
%%====================================================================

test_response_accessors(Config) ->
    io:format("[Test 9] Response 访问器~n"),
    Messages = [#{role => user, content => <<"说 hello"/utf8>>}],
    case beamai_chat_model:chat(Config, Messages) of
        {ok, Resp} ->
            io:format("  id: ~p~n", [beamai_llm_response:id(Resp)]),
            io:format("  model: ~p~n", [beamai_llm_response:model(Resp)]),
            io:format("  provider: ~p~n", [beamai_llm_response:provider(Resp)]),
            io:format("  content: ~ts~n", [beamai_llm_response:content(Resp)]),
            io:format("  content_blocks: ~p~n", [beamai_llm_response:content_blocks(Resp)]),
            io:format("  thinking: ~p~n", [beamai_llm_response:thinking(Resp)]),
            io:format("  tool_calls: ~p~n", [beamai_llm_response:tool_calls(Resp)]),
            io:format("  has_tool_calls: ~p~n", [beamai_llm_response:has_tool_calls(Resp)]),
            io:format("  finish_reason: ~p~n", [beamai_llm_response:finish_reason(Resp)]),
            io:format("  is_complete: ~p~n", [beamai_llm_response:is_complete(Resp)]),
            io:format("  needs_tool_call: ~p~n", [beamai_llm_response:needs_tool_call(Resp)]),
            Usage = beamai_llm_response:usage(Resp),
            io:format("  usage: ~p~n", [Usage]),
            io:format("  input_tokens: ~p~n", [beamai_llm_response:input_tokens(Resp)]),
            io:format("  output_tokens: ~p~n", [beamai_llm_response:output_tokens(Resp)]),
            io:format("  total_tokens: ~p~n", [beamai_llm_response:total_tokens(Resp)]),
            io:format("  metadata: ~p~n", [beamai_llm_response:metadata(Resp)]),
            io:format("  [PASS]~n~n");
        {error, Err} ->
            io:format("  [FAIL] ~p~n~n", [Err])
    end.
