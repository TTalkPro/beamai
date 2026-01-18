%%%-------------------------------------------------------------------
%%% @doc 智谱 AI Provider 测试
%%%
%%% 包含两种测试模式:
%%% - Mock 测试: 不需要真实 API Key
%%% - 集成测试: 需要设置 ZHIPU_API_KEY 环境变量
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_zhipu_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Mock 测试 (不需要 API Key)
%%====================================================================

provider_info_test_() ->
    {"Provider 信息测试", [
        {"获取 Provider 名称", fun() ->
            ?assertEqual(<<"Zhipu AI">>, llm_provider_zhipu:name())
        end},

        {"获取默认配置", fun() ->
            Config = llm_provider_zhipu:default_config(),
            ?assertEqual(<<"https://open.bigmodel.cn">>, maps:get(base_url, Config)),
            ?assertEqual(<<"glm-4.7">>, maps:get(model, Config)),
            ?assertEqual(300000, maps:get(timeout, Config)),
            ?assertEqual(4096, maps:get(max_tokens, Config)),
            ?assertEqual(0.7, maps:get(temperature, Config))
        end},

        {"支持工具调用", fun() ->
            ?assertEqual(true, llm_provider_zhipu:supports_tools())
        end},

        {"支持流式输出", fun() ->
            ?assertEqual(true, llm_provider_zhipu:supports_streaming())
        end}
    ]}.

config_validation_test_() ->
    {"配置验证测试", [
        {"有效 API Key", fun() ->
            ?assertEqual(ok, llm_provider_zhipu:validate_config(#{api_key => <<"test-key">>}))
        end},

        {"空 API Key", fun() ->
            ?assertEqual({error, missing_api_key}, llm_provider_zhipu:validate_config(#{api_key => <<>>}))
        end},

        {"缺少 API Key", fun() ->
            ?assertEqual({error, missing_api_key}, llm_provider_zhipu:validate_config(#{}))
        end}
    ]}.

llm_client_integration_test_() ->
    {"LLM Client 集成测试", [
        {"通过 llm_client 创建配置", fun() ->
            Config = llm_client:create(zhipu, #{api_key => <<"test-key">>}),
            ?assertEqual(zhipu, maps:get(provider, Config)),
            ?assertEqual(<<"test-key">>, maps:get(api_key, Config)),
            ?assertEqual(<<"glm-4.7">>, maps:get(model, Config))
        end},

        {"自定义模型配置", fun() ->
            Config = llm_client:create(zhipu, #{
                api_key => <<"test-key">>,
                model => <<"glm-4-plus">>,
                temperature => 0.5,
                top_p => 0.9
            }),
            ?assertEqual(<<"glm-4-plus">>, maps:get(model, Config)),
            ?assertEqual(0.5, maps:get(temperature, Config)),
            ?assertEqual(0.9, maps:get(top_p, Config))
        end},

        {"Provider 在列表中", fun() ->
            Providers = llm_client:list_providers(),
            ?assert(lists:member(zhipu, Providers))
        end},

        {"获取 Provider 信息", fun() ->
            Info = llm_client:provider_info(zhipu),
            ?assertEqual(<<"Zhipu AI">>, maps:get(name, Info)),
            ?assertEqual(true, maps:get(supports_tools, Info)),
            ?assertEqual(true, maps:get(supports_streaming, Info))
        end}
    ]}.

message_adapter_test_() ->
    {"消息适配器测试", [
        {"转换为 Zhipu 格式 (使用 OpenAI 兼容)", fun() ->
            Messages = [
                #{role => system, content => <<"你是一个助手">>},
                #{role => user, content => <<"你好">>}
            ],
            Formatted = llm_message_adapter:to_provider(Messages, zhipu),
            ?assertEqual(2, length(Formatted)),
            [System, User] = Formatted,
            ?assertEqual(<<"system">>, maps:get(<<"role">>, System)),
            ?assertEqual(<<"你是一个助手">>, maps:get(<<"content">>, System)),
            ?assertEqual(<<"user">>, maps:get(<<"role">>, User)),
            ?assertEqual(<<"你好">>, maps:get(<<"content">>, User))
        end},

        {"从 Zhipu 格式转换", fun() ->
            RawMessages = [
                #{<<"role">> => <<"assistant">>, <<"content">> => <<"你好！">>}
            ],
            Messages = llm_message_adapter:from_provider(RawMessages, zhipu),
            ?assertEqual(1, length(Messages)),
            [Msg] = Messages,
            ?assertEqual(assistant, maps:get(role, Msg)),
            ?assertEqual(<<"你好！">>, maps:get(content, Msg))
        end}
    ]}.

%%====================================================================
%% 集成测试 (需要 API Key)
%%====================================================================

%% 注意：这些测试需要设置 ZHIPU_API_KEY 环境变量
%% 运行方式：ZHIPU_API_KEY=your-key rebar3 eunit --module=llm_zhipu_test

real_chat_test_() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            {"跳过真实 API 测试 (未设置 ZHIPU_API_KEY)", []};
        ApiKey ->
            {"真实 API 测试", {timeout, 60, [
                {"简单对话", fun() ->
                    Config = llm_client:create(zhipu, #{
                        api_key => list_to_binary(ApiKey),
                        model => <<"glm-4.6">>  %% 使用 GLM-4.6 模型
                    }),
                    Messages = [#{role => user, content => <<"说'你好'两个字">>}],
                    Result = llm_client:chat(Config, Messages),
                    ?assertMatch({ok, _}, Result),
                    {ok, Response} = Result,
                    Content = maps:get(content, Response),
                    ?assert(is_binary(Content)),
                    ?assert(byte_size(Content) > 0)
                end}
            ]}}
    end.

real_stream_test_() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            {"跳过流式 API 测试 (未设置 ZHIPU_API_KEY)", []};
        ApiKey ->
            {"流式 API 测试", {timeout, 60, [
                {"流式对话", fun() ->
                    Config = llm_client:create(zhipu, #{
                        api_key => list_to_binary(ApiKey),
                        model => <<"glm-4.6">>
                    }),
                    Messages = [#{role => user, content => <<"说'测试'两个字">>}],
                    Self = self(),
                    Callback = fun(Event) -> Self ! {stream_event, Event} end,
                    Result = llm_client:stream_chat(Config, Messages, Callback),
                    ?assertMatch({ok, _}, Result),
                    {ok, Response} = Result,
                    Content = maps:get(content, Response),
                    ?assert(is_binary(Content)),
                    ?assert(byte_size(Content) > 0)
                end}
            ]}}
    end.

real_tools_test_() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            {"跳过工具调用测试 (未设置 ZHIPU_API_KEY)", []};
        ApiKey ->
            {"工具调用测试", {timeout, 60, [
                {"带工具的对话", fun() ->
                    Config = llm_client:create(zhipu, #{
                        api_key => list_to_binary(ApiKey),
                        model => <<"glm-4.6">>
                    }),
                    Messages = [#{role => user, content => <<"北京今天天气怎么样？">>}],
                    Tools = [
                        #{
                            name => <<"get_weather">>,
                            description => <<"获取指定城市的天气信息">>,
                            parameters => #{
                                type => object,
                                properties => #{
                                    city => #{type => string, description => <<"城市名称">>}
                                },
                                required => [<<"city">>]
                            }
                        }
                    ],
                    Result = llm_client:with_tools(Config, Messages, Tools),
                    ?assertMatch({ok, _}, Result),
                    {ok, Response} = Result,
                    %% 可能返回工具调用或直接回答
                    ToolCalls = maps:get(tool_calls, Response, []),
                    Content = maps:get(content, Response),
                    ?assert(ToolCalls =/= [] orelse Content =/= null)
                end}
            ]}}
    end.
