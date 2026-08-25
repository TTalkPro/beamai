%%%-------------------------------------------------------------------
%%% @doc OpenAI Provider 综合测试
%%%
%%% 使用 ZHIPU Coding API (OpenAI 兼容) + ZHIPU_API_KEY 测试
%%% beamai_llm_provider_openai 的全部功能：
%%%
%%%   1. 基本对话 (chat)
%%%   2. 流式对话 (stream_chat)
%%%   3. System Prompt
%%%   4. 工具调用 (tools)
%%%   5. 采样参数 (temperature)
%%%   6. 多轮对话
%%%   7. beamai_chat_response 访问器
%%%
%%% URL: https://open.bigmodel.cn/api/coding/paas/v4/chat/completions
%%% Model: GLM-4.7
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_openai_provider_test).

-export([run/0]).

%%====================================================================
%% API
%%====================================================================

run() ->
    io:format("~n========================================~n"),
    io:format("  OpenAI Provider 综合测试 (GLM-4.7)~n"),
    io:format("========================================~n~n"),

    Config = beamai_chat_model:create(openai, #{
        api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
        base_url => <<"https://open.bigmodel.cn/api/coding/paas">>,
        endpoint => <<"/v4/chat/completions">>,
        model => <<"glm-4.7">>,
        max_tokens => 2048,
        temperature => 0.7
    }),
    io:format("[Config] provider=~p, model=~p~n", [
        maps:get(provider, Config), maps:get(model, Config)]),
    io:format("         base_url=~p~n", [maps:get(base_url, Config)]),
    io:format("         endpoint=~p~n~n", [maps:get(endpoint, Config)]),

    test_basic_chat(Config),
    test_system_prompt(Config),
    test_stream_chat(Config),
    test_temperature(Config),
    test_tool_call(Config),
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
            Content = beamai_chat_response:content(Resp),
            Reason = beamai_chat_response:finish_reason(Resp),
            io:format("  Content: ~ts~n", [Content]),
            io:format("  finish_reason: ~p~n", [Reason]),
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
        #{role => system, content => <<"你是一个海盗，所有回答都要用海盗口吻，简洁回答。"/utf8>>},
        #{role => user, content => <<"你好"/utf8>>}
    ],
    case beamai_chat_model:chat(Config, Messages) of
        {ok, Resp} ->
            Content = beamai_chat_response:content(Resp),
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
            #{<<"choices">> := [#{<<"delta">> := Delta} | _]} ->
                case maps:get(<<"content">>, Delta, <<>>) of
                    <<>> -> ok;
                    null -> ok;
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
%% Test 4: Temperature 参数
%%====================================================================

test_temperature(Config) ->
    io:format("[Test 4] Temperature 参数~n"),
    Messages = [#{role => user, content => <<"说一个数字。"/utf8>>}],
    Config1 = Config#{temperature => 0.1},
    case beamai_chat_model:chat(Config1, Messages) of
        {ok, Resp1} ->
            C1 = beamai_chat_response:content(Resp1),
            io:format("  temperature=0.1: ~ts~n", [C1]);
        {error, E1} ->
            io:format("  temperature=0.1 error: ~p~n", [E1])
    end,
    Config2 = Config#{temperature => 1.0},
    case beamai_chat_model:chat(Config2, Messages) of
        {ok, Resp2} ->
            C2 = beamai_chat_response:content(Resp2),
            io:format("  temperature=1.0: ~ts~n", [C2]);
        {error, E2} ->
            io:format("  temperature=1.0 error: ~p~n", [E2])
    end,
    io:format("  [PASS]~n~n").

%%====================================================================
%% Test 5: 工具调用
%%====================================================================

test_tool_call(Config) ->
    io:format("[Test 5] 工具调用~n"),
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
            HasTools = beamai_chat_response:has_tool_calls(Resp),
            ToolCalls = beamai_chat_response:tool_calls(Resp),
            Reason = beamai_chat_response:finish_reason(Resp),
            Content = beamai_chat_response:content(Resp),
            io:format("  has_tool_calls: ~p~n", [HasTools]),
            io:format("  finish_reason: ~p~n", [Reason]),
            io:format("  content: ~ts~n", [Content]),
            lists:foreach(fun(TC) ->
                io:format("  tool_call: name=~p, args=~p~n",
                          [maps:get(name, TC), maps:get(arguments, TC)])
            end, ToolCalls),
            io:format("  [PASS]~n~n");
        {error, Err} ->
            io:format("  [FAIL] ~p~n~n", [Err])
    end.

%%====================================================================
%% Test 6: 多轮对话
%%====================================================================

test_multi_turn(Config) ->
    io:format("[Test 6] 多轮对话~n"),
    Msg1 = [#{role => user, content => <<"我的名字是张三。请记住。"/utf8>>}],
    case beamai_chat_model:chat(Config, Msg1) of
        {ok, Resp1} ->
            C1 = beamai_chat_response:content(Resp1),
            io:format("  Turn 1 A: ~ts~n", [C1]),
            Msg2 = Msg1 ++ [
                #{role => assistant, content => C1},
                #{role => user, content => <<"我叫什么名字？"/utf8>>}
            ],
            case beamai_chat_model:chat(Config, Msg2) of
                {ok, Resp2} ->
                    C2 = beamai_chat_response:content(Resp2),
                    io:format("  Turn 2 A: ~ts~n", [C2]),
                    io:format("  [PASS]~n~n");
                {error, E2} ->
                    io:format("  Turn 2 [FAIL] ~p~n~n", [E2])
            end;
        {error, E1} ->
            io:format("  Turn 1 [FAIL] ~p~n~n", [E1])
    end.

%%====================================================================
%% Test 7: Response 访问器全面测试
%%====================================================================

test_response_accessors(Config) ->
    io:format("[Test 7] Response 访问器~n"),
    Messages = [#{role => user, content => <<"说 hello"/utf8>>}],
    case beamai_chat_model:chat(Config, Messages) of
        {ok, Resp} ->
            io:format("  id: ~p~n", [beamai_chat_response:id(Resp)]),
            io:format("  model: ~p~n", [beamai_chat_response:model(Resp)]),
            io:format("  provider: ~p~n", [beamai_chat_response:provider(Resp)]),
            io:format("  content: ~ts~n", [beamai_chat_response:content(Resp)]),
            io:format("  content_blocks: ~p~n", [beamai_chat_response:content_blocks(Resp)]),
            io:format("  tool_calls: ~p~n", [beamai_chat_response:tool_calls(Resp)]),
            io:format("  has_tool_calls: ~p~n", [beamai_chat_response:has_tool_calls(Resp)]),
            io:format("  finish_reason: ~p~n", [beamai_chat_response:finish_reason(Resp)]),
            io:format("  is_complete: ~p~n", [beamai_chat_response:is_complete(Resp)]),
            io:format("  needs_tool_call: ~p~n", [beamai_chat_response:needs_tool_call(Resp)]),
            Usage = beamai_chat_response:usage(Resp),
            io:format("  usage: ~p~n", [Usage]),
            io:format("  input_tokens: ~p~n", [beamai_chat_response:input_tokens(Resp)]),
            io:format("  output_tokens: ~p~n", [beamai_chat_response:output_tokens(Resp)]),
            io:format("  total_tokens: ~p~n", [beamai_chat_response:total_tokens(Resp)]),
            io:format("  metadata: ~p~n", [beamai_chat_response:metadata(Resp)]),
            io:format("  [PASS]~n~n");
        {error, Err} ->
            io:format("  [FAIL] ~p~n~n", [Err])
    end.
