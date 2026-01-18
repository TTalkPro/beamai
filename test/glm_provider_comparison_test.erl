%%%-------------------------------------------------------------------
%%% @doc GLM Provider 对比测试
%%%
%%% 测试两种配置：
%%% 1. GLM-4.7 + Anthropic Provider (base_url: https://open.bigmodel.cn/api/anthropic)
%%% 2. GLM-4.6 + Zhipu Provider
%%%
%%% 运行方式：
%%%   ZHIPU_API_KEY=your_key rebar3 eunit --module=glm_provider_comparison_test
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(glm_provider_comparison_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试 1: GLM-4.7 + Anthropic Provider
%%====================================================================

glm47_anthropic_test_() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            {"跳过 GLM-4.7 Anthropic 测试 (未设置 ZHIPU_API_KEY)", []};
        ApiKey ->
            {"GLM-4.7 via Anthropic Provider", {timeout, 120, [
                {"简单对话", fun() ->
                    io:format("~n=== 测试 GLM-4.7 + Anthropic Provider ===~n"),
                    Config = llm_client:create(anthropic, #{
                        api_key => list_to_binary(ApiKey),
                        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
                        model => <<"glm-4.7">>,
                        max_tokens => 100,
                        timeout => 60000
                    }),
                    io:format("Config: ~p~n", [maps:without([api_key], Config)]),

                    Messages = [#{role => user, content => <<"你好，请用一句话介绍自己"/utf8>>}],
                    io:format("发送请求...~n"),

                    Result = llm_client:chat(Config, Messages),
                    io:format("结果: ~p~n", [Result]),

                    ?assertMatch({ok, _}, Result),
                    {ok, Response} = Result,
                    Content = maps:get(content, Response),
                    ?assert(is_binary(Content)),
                    ?assert(byte_size(Content) > 0),
                    io:format("回复内容: ~ts~n", [Content]),
                    io:format("=== GLM-4.7 Anthropic 测试通过 ===~n~n")
                end},

                {"工具调用", fun() ->
                    io:format("~n=== 测试 GLM-4.7 Anthropic 工具调用 ===~n"),
                    Config = llm_client:create(anthropic, #{
                        api_key => list_to_binary(ApiKey),
                        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
                        model => <<"glm-4.7">>,
                        max_tokens => 200,
                        timeout => 60000
                    }),

                    Messages = [#{role => user, content => <<"北京现在几点了？"/utf8>>}],
                    Tools = [
                        #{
                            name => <<"get_current_time">>,
                            description => <<"获取指定城市的当前时间"/utf8>>,
                            parameters => #{
                                type => object,
                                properties => #{
                                    city => #{type => string, description => <<"城市名称"/utf8>>}
                                },
                                required => [<<"city">>]
                            }
                        }
                    ],

                    Result = llm_client:with_tools(Config, Messages, Tools),
                    io:format("工具调用结果: ~p~n", [Result]),

                    ?assertMatch({ok, _}, Result),
                    {ok, Response} = Result,
                    ToolCalls = maps:get(tool_calls, Response, []),
                    Content = maps:get(content, Response, null),
                    io:format("Tool calls: ~p, Content: ~p~n", [ToolCalls, Content]),
                    ?assert(ToolCalls =/= [] orelse Content =/= null),
                    io:format("=== GLM-4.7 Anthropic 工具调用测试通过 ===~n~n")
                end}
            ]}}
    end.

%%====================================================================
%% 测试 2: GLM-4.6 + Zhipu Provider
%%====================================================================

glm46_zhipu_test_() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            {"跳过 GLM-4.6 Zhipu 测试 (未设置 ZHIPU_API_KEY)", []};
        ApiKey ->
            {"GLM-4.6 via Zhipu Provider", {timeout, 120, [
                {"简单对话", fun() ->
                    io:format("~n=== 测试 GLM-4.6 + Zhipu Provider ===~n"),
                    Config = llm_client:create(zhipu, #{
                        api_key => list_to_binary(ApiKey),
                        model => <<"glm-4.6">>,
                        max_tokens => 100,
                        timeout => 60000
                    }),
                    io:format("Config: ~p~n", [maps:without([api_key], Config)]),

                    Messages = [#{role => user, content => <<"你好，请用一句话介绍自己"/utf8>>}],
                    io:format("发送请求...~n"),

                    Result = llm_client:chat(Config, Messages),
                    io:format("结果: ~p~n", [Result]),

                    ?assertMatch({ok, _}, Result),
                    {ok, Response} = Result,
                    Content = maps:get(content, Response),
                    ?assert(is_binary(Content)),
                    ?assert(byte_size(Content) > 0),
                    io:format("回复内容: ~ts~n", [Content]),
                    io:format("=== GLM-4.6 Zhipu 测试通过 ===~n~n")
                end},

                {"工具调用", fun() ->
                    io:format("~n=== 测试 GLM-4.6 Zhipu 工具调用 ===~n"),
                    Config = llm_client:create(zhipu, #{
                        api_key => list_to_binary(ApiKey),
                        model => <<"glm-4.6">>,
                        max_tokens => 200,
                        timeout => 60000
                    }),

                    Messages = [#{role => user, content => <<"上海现在几点了？"/utf8>>}],
                    Tools = [
                        #{
                            name => <<"get_current_time">>,
                            description => <<"获取指定城市的当前时间"/utf8>>,
                            parameters => #{
                                type => object,
                                properties => #{
                                    city => #{type => string, description => <<"城市名称"/utf8>>}
                                },
                                required => [<<"city">>]
                            }
                        }
                    ],

                    Result = llm_client:with_tools(Config, Messages, Tools),
                    io:format("工具调用结果: ~p~n", [Result]),

                    ?assertMatch({ok, _}, Result),
                    {ok, Response} = Result,
                    ToolCalls = maps:get(tool_calls, Response, []),
                    Content = maps:get(content, Response, null),
                    io:format("Tool calls: ~p, Content: ~p~n", [ToolCalls, Content]),
                    ?assert(ToolCalls =/= [] orelse Content =/= null),
                    io:format("=== GLM-4.6 Zhipu 工具调用测试通过 ===~n~n")
                end},

                {"流式输出", fun() ->
                    io:format("~n=== 测试 GLM-4.6 Zhipu 流式输出 ===~n"),
                    Config = llm_client:create(zhipu, #{
                        api_key => list_to_binary(ApiKey),
                        model => <<"glm-4.6">>,
                        max_tokens => 50,
                        timeout => 60000
                    }),

                    Messages = [#{role => user, content => <<"说'测试成功'三个字"/utf8>>}],
                    Self = self(),
                    Callback = fun(Event) ->
                        io:format("Stream event: ~p~n", [Event]),
                        Self ! {stream_event, Event}
                    end,

                    Result = llm_client:stream_chat(Config, Messages, Callback),
                    io:format("流式结果: ~p~n", [Result]),

                    ?assertMatch({ok, _}, Result),
                    {ok, Response} = Result,
                    Content = maps:get(content, Response),
                    ?assert(is_binary(Content)),
                    io:format("最终内容: ~ts~n", [Content]),
                    io:format("=== GLM-4.6 Zhipu 流式测试通过 ===~n~n")
                end}
            ]}}
    end.

%%====================================================================
%% 对比测试
%%====================================================================

comparison_test_() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            {"跳过对比测试 (未设置 ZHIPU_API_KEY)", []};
        ApiKey ->
            {"Provider 对比测试", {timeout, 180, [
                {"相同问题对比", fun() ->
                    io:format("~n=== Provider 对比测试 ===~n"),
                    Question = <<"什么是 Erlang？请用一句话回答。"/utf8>>,

                    %% GLM-4.7 via Anthropic
                    Config1 = llm_client:create(anthropic, #{
                        api_key => list_to_binary(ApiKey),
                        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
                        model => <<"glm-4.7">>,
                        max_tokens => 100,
                        timeout => 60000
                    }),

                    %% GLM-4.6 via Zhipu
                    Config2 = llm_client:create(zhipu, #{
                        api_key => list_to_binary(ApiKey),
                        model => <<"glm-4.6">>,
                        max_tokens => 100,
                        timeout => 60000
                    }),

                    Messages = [#{role => user, content => Question}],

                    io:format("问题: ~ts~n~n", [Question]),

                    %% 测试 GLM-4.7
                    io:format("GLM-4.7 (Anthropic Provider):~n"),
                    {ok, Resp1} = llm_client:chat(Config1, Messages),
                    Content1 = maps:get(content, Resp1),
                    io:format("  回复: ~ts~n~n", [Content1]),

                    %% 测试 GLM-4.6
                    io:format("GLM-4.6 (Zhipu Provider):~n"),
                    {ok, Resp2} = llm_client:chat(Config2, Messages),
                    Content2 = maps:get(content, Resp2),
                    io:format("  回复: ~ts~n~n", [Content2]),

                    ?assert(byte_size(Content1) > 0),
                    ?assert(byte_size(Content2) > 0),
                    io:format("=== 对比测试通过 ===~n")
                end}
            ]}}
    end.
