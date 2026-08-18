%%%-------------------------------------------------------------------
%%% @doc 新增厂商 Provider 单元测试
%%%
%%% 覆盖 xAI / Moonshot(Kimi) / OpenRouter / SiliconFlow：
%%%   - 请求体构建（厂商特有参数、不支持参数的剔除）
%%%   - URL 与请求头（站点区域、应用标识）
%%%   - 响应解析（reasoning_content / 引用 / 上游供应商与成本）
%%%   - Provider 路由与默认配置
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_new_providers_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MESSAGES, [#{role => user, content => <<"hello">>}]).

%%====================================================================
%% Provider 路由
%%====================================================================

provider_routing_test() ->
    Cfg = fun(P) -> maps:get(model, beamai_chat_completion:create(P, #{api_key => <<"k">>})) end,
    ?assertEqual(<<"grok-4.5">>, Cfg(xai)),
    ?assertEqual(<<"kimi-k2.5">>, Cfg(moonshot)),
    %% kimi 是 moonshot 的别名，走同一模块
    ?assertEqual(<<"kimi-k2.5">>, Cfg(kimi)),
    ?assertEqual(<<"openai/gpt-4o-mini">>, Cfg(openrouter)),
    ?assertEqual(<<"deepseek-ai/DeepSeek-V3">>, Cfg(siliconflow)).

%%====================================================================
%% xAI
%%====================================================================

xai_body_basics_test() ->
    Config = beamai_chat_completion:create(xai, #{api_key => <<"k">>, max_tokens => 100}),
    Body = beamai_llm_provider_xai:build_request_body(Config, #{messages => ?MESSAGES}),
    ?assertEqual(<<"grok-4.5">>, maps:get(<<"model">>, Body)),
    ?assertEqual(100, maps:get(<<"max_tokens">>, Body)),
    ?assertEqual(1, length(maps:get(<<"messages">>, Body))).

xai_drops_unsupported_params_test() ->
    %% xAI 不支持 frequency_penalty / presence_penalty / stop / top_k
    Config = beamai_chat_completion:create(xai, #{
        api_key => <<"k">>,
        frequency_penalty => 0.5,
        presence_penalty => 0.5,
        stop => [<<"x">>],
        top_k => 10
    }),
    Body = beamai_llm_provider_xai:build_request_body(Config, #{messages => ?MESSAGES}),
    ?assertNot(maps:is_key(<<"frequency_penalty">>, Body)),
    ?assertNot(maps:is_key(<<"presence_penalty">>, Body)),
    ?assertNot(maps:is_key(<<"stop">>, Body)),
    ?assertNot(maps:is_key(<<"top_k">>, Body)).

xai_reasoning_effort_test() ->
    Config = beamai_chat_completion:create(xai, #{api_key => <<"k">>,
                                                 reasoning_effort => <<"high">>}),
    Body = beamai_llm_provider_xai:build_request_body(Config, #{messages => ?MESSAGES}),
    ?assertEqual(<<"high">>, maps:get(<<"reasoning_effort">>, Body)).

xai_reasoning_effort_dropped_for_grok_420_test() ->
    Config = beamai_chat_completion:create(xai, #{api_key => <<"k">>,
                                                 model => <<"grok-4.20-reasoning">>,
                                                 reasoning_effort => <<"none">>}),
    Body = beamai_llm_provider_xai:build_request_body(Config, #{messages => ?MESSAGES}),
    ?assertNot(maps:is_key(<<"reasoning_effort">>, Body)).

xai_supports_reasoning_effort_test() ->
    ?assertNot(beamai_llm_provider_xai:supports_reasoning_effort(<<"grok-4.20-reasoning">>)),
    ?assertNot(beamai_llm_provider_xai:supports_reasoning_effort(<<"grok-4.20-non-reasoning">>)),
    ?assertNot(beamai_llm_provider_xai:supports_reasoning_effort(<<"grok-4.20-0309-reasoning">>)),
    ?assert(beamai_llm_provider_xai:supports_reasoning_effort(<<"grok-4.3">>)),
    ?assert(beamai_llm_provider_xai:supports_reasoning_effort(<<"grok-4.20-multi-agent">>)).

xai_stream_options_test() ->
    Config = beamai_chat_completion:create(xai, #{api_key => <<"k">>}),
    Body = beamai_llm_provider_xai:build_request_body(Config, #{messages => ?MESSAGES, stream => true}),
    ?assert(maps:get(<<"stream">>, Body)),
    ?assertEqual(#{<<"include_usage">> => true}, maps:get(<<"stream_options">>, Body)).

xai_response_with_citations_test() ->
    Raw = #{
        <<"id">> => <<"1">>, <<"model">> => <<"grok-4.5">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"content">> => <<"答案"/utf8>>,
                               <<"reasoning_content">> => <<"思考"/utf8>>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"citations">> => [<<"https://x/a">>],
        <<"usage">> => #{<<"prompt_tokens">> => 3, <<"completion_tokens">> => 4,
                         <<"total_tokens">> => 7}
    },
    {ok, Resp} = beamai_llm_response_parser:from_xai(Raw),
    ?assertEqual(xai, beamai_llm_response:provider(Resp)),
    ?assertEqual(<<"答案"/utf8>>, beamai_llm_response:content(Resp)),
    ?assertEqual(complete, beamai_llm_response:finish_reason(Resp)),
    ?assertEqual(7, beamai_llm_response:total_tokens(Resp)),
    Meta = beamai_llm_response:metadata(Resp),
    ?assertEqual([<<"https://x/a">>], maps:get(citations, Meta)),
    ?assertEqual(<<"思考"/utf8>>, maps:get(reasoning_content, Meta)).

%%====================================================================
%% Moonshot / Kimi
%%====================================================================

moonshot_url_region_test() ->
    CN = beamai_chat_completion:create(moonshot, #{api_key => <<"k">>}),
    ?assertEqual(<<"https://api.moonshot.cn/v1/chat/completions">>,
                 beamai_llm_provider_moonshot:build_url(CN)),
    Global = beamai_chat_completion:create(moonshot, #{api_key => <<"k">>, region => global}),
    ?assertEqual(<<"https://api.moonshot.ai/v1/chat/completions">>,
                 beamai_llm_provider_moonshot:build_url(Global)),
    Custom = beamai_chat_completion:create(kimi, #{api_key => <<"k">>,
                                                  base_url => <<"https://proxy.local">>}),
    ?assertEqual(<<"https://proxy.local/v1/chat/completions">>,
                 beamai_llm_provider_moonshot:build_url(Custom)).

moonshot_thinking_test() ->
    Config = beamai_chat_completion:create(moonshot, #{
        api_key => <<"k">>,
        thinking => #{type => enabled, budget_tokens => 2048},
        reasoning_history => <<"interleaved">>,
        reasoning_effort => <<"max">>
    }),
    Body = beamai_llm_provider_moonshot:build_request_body(Config, #{messages => ?MESSAGES}),
    ?assertEqual(#{<<"type">> => <<"enabled">>, <<"budget_tokens">> => 2048},
                 maps:get(<<"thinking">>, Body)),
    ?assertEqual(<<"interleaved">>, maps:get(<<"reasoning_history">>, Body)),
    ?assertEqual(<<"max">>, maps:get(<<"reasoning_effort">>, Body)).

moonshot_strips_dollar_schema_test() ->
    Format = #{
        <<"type">> => <<"json_schema">>,
        <<"json_schema">> => #{
            <<"name">> => <<"result">>,
            <<"schema">> => #{
                <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
                <<"type">> => <<"object">>
            }
        }
    },
    Config = beamai_chat_completion:create(moonshot, #{api_key => <<"k">>,
                                                      response_format => Format}),
    Body = beamai_llm_provider_moonshot:build_request_body(Config, #{messages => ?MESSAGES}),
    Schema = maps:get(<<"schema">>, maps:get(<<"json_schema">>,
                                             maps:get(<<"response_format">>, Body))),
    ?assertNot(maps:is_key(<<"$schema">>, Schema)),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)).

moonshot_response_test() ->
    Raw = #{
        <<"id">> => <<"2">>, <<"model">> => <<"kimi-k2.5">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"content">> => <<"hi">>,
                               <<"reasoning_content">> => <<"think">>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{<<"prompt_tokens">> => 1, <<"completion_tokens">> => 2}
    },
    {ok, Resp} = beamai_llm_response_parser:from_moonshot(Raw),
    ?assertEqual(moonshot, beamai_llm_response:provider(Resp)),
    ?assertEqual(<<"think">>, maps:get(reasoning_content, beamai_llm_response:metadata(Resp))),
    ?assertEqual(3, beamai_llm_response:total_tokens(Resp)).

%%====================================================================
%% OpenRouter
%%====================================================================

openrouter_headers_test() ->
    Config = beamai_chat_completion:create(openrouter, #{
        api_key => <<"sk-or">>, site_url => <<"https://app.local">>, site_name => <<"beamai">>
    }),
    Headers = beamai_llm_provider_openrouter:build_headers(Config),
    ?assertEqual(<<"Bearer sk-or">>, proplists:get_value(<<"Authorization">>, Headers)),
    ?assertEqual(<<"https://app.local">>, proplists:get_value(<<"HTTP-Referer">>, Headers)),
    ?assertEqual(<<"beamai">>, proplists:get_value(<<"X-Title">>, Headers)).

openrouter_headers_without_site_test() ->
    Config = beamai_chat_completion:create(openrouter, #{api_key => <<"sk-or">>}),
    Headers = beamai_llm_provider_openrouter:build_headers(Config),
    ?assertEqual(undefined, proplists:get_value(<<"HTTP-Referer">>, Headers)),
    ?assertEqual(undefined, proplists:get_value(<<"X-Title">>, Headers)).

openrouter_routing_body_test() ->
    Config = beamai_chat_completion:create(openrouter, #{
        api_key => <<"k">>,
        model => <<"anthropic/claude-sonnet-4">>,
        models => [<<"openai/gpt-4o">>],
        route => <<"fallback">>,
        provider => #{<<"sort">> => <<"throughput">>},
        transforms => [<<"middle-out">>],
        reasoning => #{<<"effort">> => <<"high">>},
        include_usage => true
    }),
    Body = beamai_llm_provider_openrouter:build_request_body(Config, #{messages => ?MESSAGES}),
    ?assertEqual([<<"openai/gpt-4o">>], maps:get(<<"models">>, Body)),
    ?assertEqual(<<"fallback">>, maps:get(<<"route">>, Body)),
    ?assertEqual(#{<<"sort">> => <<"throughput">>}, maps:get(<<"provider">>, Body)),
    ?assertEqual([<<"middle-out">>], maps:get(<<"transforms">>, Body)),
    ?assertEqual(#{<<"effort">> => <<"high">>}, maps:get(<<"reasoning">>, Body)),
    ?assertEqual(#{<<"include">> => true}, maps:get(<<"usage">>, Body)).

openrouter_usage_off_by_default_test() ->
    Config = beamai_chat_completion:create(openrouter, #{api_key => <<"k">>}),
    Body = beamai_llm_provider_openrouter:build_request_body(Config, #{messages => ?MESSAGES}),
    ?assertNot(maps:is_key(<<"usage">>, Body)).

openrouter_response_test() ->
    Raw = #{
        <<"id">> => <<"3">>, <<"model">> => <<"anthropic/claude-sonnet-4">>,
        <<"provider">> => <<"Anthropic">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"content">> => <<"ok">>, <<"reasoning">> => <<"r">>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{<<"prompt_tokens">> => 10, <<"completion_tokens">> => 5,
                         <<"total_tokens">> => 15, <<"cost">> => 0.00042}
    },
    {ok, Resp} = beamai_llm_response_parser:from_openrouter(Raw),
    ?assertEqual(openrouter, beamai_llm_response:provider(Resp)),
    Meta = beamai_llm_response:metadata(Resp),
    ?assertEqual(<<"Anthropic">>, maps:get(upstream_provider, Meta)),
    ?assertEqual(<<"r">>, maps:get(reasoning_content, Meta)),
    Usage = beamai_llm_response:usage(Resp),
    ?assertEqual(0.00042, maps:get(cost, maps:get(details, Usage))).

%%====================================================================
%% SiliconFlow
%%====================================================================

siliconflow_url_region_test() ->
    CN = beamai_chat_completion:create(siliconflow, #{api_key => <<"k">>}),
    ?assertEqual(<<"https://api.siliconflow.cn/v1/chat/completions">>,
                 beamai_llm_provider_siliconflow:build_url(CN)),
    Global = beamai_chat_completion:create(siliconflow, #{api_key => <<"k">>, region => global}),
    ?assertEqual(<<"https://api.siliconflow.com/v1/chat/completions">>,
                 beamai_llm_provider_siliconflow:build_url(Global)).

siliconflow_thinking_params_test() ->
    Config = beamai_chat_completion:create(siliconflow, #{
        api_key => <<"k">>,
        model => <<"Qwen/Qwen3-32B">>,
        enable_thinking => true,
        thinking_budget => 4096,
        top_k => 20,
        min_p => 0.05
    }),
    Body = beamai_llm_provider_siliconflow:build_request_body(Config, #{messages => ?MESSAGES}),
    ?assert(maps:get(<<"enable_thinking">>, Body)),
    ?assertEqual(4096, maps:get(<<"thinking_budget">>, Body)),
    ?assertEqual(20, maps:get(<<"top_k">>, Body)),
    ?assertEqual(0.05, maps:get(<<"min_p">>, Body)).

siliconflow_response_test() ->
    Raw = #{
        <<"id">> => <<"4">>, <<"model">> => <<"Qwen/Qwen3-32B">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"content">> => <<"a">>, <<"reasoning_content">> => <<"b">>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{<<"prompt_tokens">> => 2, <<"completion_tokens">> => 2,
                         <<"total_tokens">> => 4}
    },
    {ok, Resp} = beamai_llm_response_parser:from_siliconflow(Raw),
    ?assertEqual(siliconflow, beamai_llm_response:provider(Resp)),
    ?assertEqual(<<"b">>, maps:get(reasoning_content, beamai_llm_response:metadata(Resp))).

%%====================================================================
%% 工具调用（OpenAI 兼容通道）
%%====================================================================

compatible_tool_calls_test() ->
    Raw = #{
        <<"id">> => <<"5">>, <<"model">> => <<"grok-4.5">>,
        <<"choices">> => [#{
            <<"message">> => #{
                <<"content">> => null,
                <<"tool_calls">> => [#{
                    <<"id">> => <<"call_1">>,
                    <<"function">> => #{<<"name">> => <<"get_weather">>,
                                        <<"arguments">> => <<"{\"city\":\"SH\"}">>}
                }]
            },
            <<"finish_reason">> => <<"tool_calls">>
        }]
    },
    {ok, Resp} = beamai_llm_response_parser:from_xai(Raw),
    ?assert(beamai_llm_response:has_tool_calls(Resp)),
    [Call] = beamai_llm_response:tool_calls(Resp),
    ?assertEqual(<<"get_weather">>, maps:get(name, Call)),
    ?assertEqual(#{<<"city">> => <<"SH">>}, maps:get(arguments, Call)),
    ?assertEqual(tool_use, beamai_llm_response:finish_reason(Resp)).

error_response_test() ->
    Raw = #{<<"error">> => #{<<"message">> => <<"bad key">>}},
    ?assertMatch({error, {api_error, _}}, beamai_llm_response_parser:from_xai(Raw)),
    ?assertMatch({error, {api_error, _}}, beamai_llm_response_parser:from_openrouter(Raw)),
    ?assertMatch({error, {invalid_response, _}},
                 beamai_llm_response_parser:from_siliconflow(#{<<"junk">> => 1})).
