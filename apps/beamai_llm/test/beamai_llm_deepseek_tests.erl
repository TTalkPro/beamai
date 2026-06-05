%%%-------------------------------------------------------------------
%%% @doc DeepSeek Provider 特有机制单元测试
%%%
%%% 覆盖：
%%%   - deepseek-reasoner 不支持参数的自动剔除
%%%   - Chat Prefix Completion（beta）的端点路由与 prefix 透传
%%%   - FIM 填空补全的请求体构建与流式累加
%%%   - 缓存统计（prompt_cache_hit/miss_tokens）提取
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_deepseek_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% reasoner 参数过滤测试
%%====================================================================

reasoner_filters_unsupported_params_test() ->
    Config = #{
        model => <<"deepseek-reasoner">>,
        temperature => 0.7,
        top_p => 0.9,
        frequency_penalty => 0.5,
        logprobs => true,
        top_logprobs => 5
    },
    Body = beamai_llm_provider_deepseek:build_request_body(Config, #{messages => []}),
    %% reasoner 不支持的参数全部剔除
    ?assertNot(maps:is_key(<<"temperature">>, Body)),
    ?assertNot(maps:is_key(<<"top_p">>, Body)),
    ?assertNot(maps:is_key(<<"frequency_penalty">>, Body)),
    ?assertNot(maps:is_key(<<"logprobs">>, Body)),
    ?assertNot(maps:is_key(<<"top_logprobs">>, Body)),
    %% 支持的参数保留
    ?assertEqual(<<"deepseek-reasoner">>, maps:get(<<"model">>, Body)),
    ?assert(maps:is_key(<<"max_tokens">>, Body)).

chat_model_keeps_params_test() ->
    Config = #{
        model => <<"deepseek-chat">>,
        temperature => 0.7,
        top_p => 0.9,
        logprobs => true
    },
    Body = beamai_llm_provider_deepseek:build_request_body(Config, #{messages => []}),
    ?assertEqual(0.7, maps:get(<<"temperature">>, Body)),
    ?assertEqual(0.9, maps:get(<<"top_p">>, Body)),
    ?assertEqual(true, maps:get(<<"logprobs">>, Body)).

%%====================================================================
%% Chat Prefix Completion（beta）测试
%%====================================================================

prefix_routes_to_beta_endpoint_test() ->
    Request = #{messages => [
        #{role => user, content => <<"写一首诗"/utf8>>},
        #{role => assistant, content => <<"床前明月光，"/utf8>>, prefix => true}
    ]},
    ?assertEqual(<<"/beta/chat/completions">>,
                 beamai_llm_provider_deepseek:chat_endpoint(Request)).

no_prefix_uses_default_endpoint_test() ->
    Request = #{messages => [#{role => user, content => <<"hi">>}]},
    ?assertEqual(<<"/chat/completions">>,
                 beamai_llm_provider_deepseek:chat_endpoint(Request)).

prefix_passthrough_in_message_adapter_test() ->
    Messages = [
        #{role => user, content => <<"q">>},
        #{role => assistant, content => <<"```python\n">>, prefix => true}
    ],
    [UserMsg, AssistantMsg] = beamai_llm_message_adapter:to_openai(Messages),
    %% 普通消息不携带 prefix 字段
    ?assertNot(maps:is_key(<<"prefix">>, UserMsg)),
    %% prefix => true 透传到 API 格式
    ?assertEqual(true, maps:get(<<"prefix">>, AssistantMsg)).

%%====================================================================
%% FIM 填空补全测试
%%====================================================================

fim_request_body_test() ->
    Config = #{model => <<"deepseek-chat">>, temperature => 0.0, max_tokens => 128},
    Request = #{
        prompt => <<"def fib(n):">>,
        suffix => <<"    return fib(n-1) + fib(n-2)">>,
        echo => false,
        logprobs => 5
    },
    Body = beamai_llm_provider_deepseek:build_fim_request_body(Config, Request),
    ?assertEqual(<<"def fib(n):">>, maps:get(<<"prompt">>, Body)),
    ?assertEqual(<<"    return fib(n-1) + fib(n-2)">>, maps:get(<<"suffix">>, Body)),
    ?assertEqual(false, maps:get(<<"echo">>, Body)),
    ?assertEqual(5, maps:get(<<"logprobs">>, Body)),
    ?assertEqual(128, maps:get(<<"max_tokens">>, Body)),
    ?assertEqual(0.0, maps:get(<<"temperature">>, Body)),
    %% 非流式时不携带 stream
    ?assertNot(maps:is_key(<<"stream">>, Body)).

fim_stream_accumulate_test() ->
    %% Completions 格式流式：文本片段直接在 choices[0].text
    Events = [
        #{<<"id">> => <<"fim-1">>, <<"model">> => <<"deepseek-chat">>,
          <<"choices">> => [#{<<"text">> => <<"    if n <= 1:">>, <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"text">> => <<" return n\n">>, <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"text">> => <<>>, <<"finish_reason">> => <<"stop">>}],
          <<"usage">> => #{<<"prompt_tokens">> => 8, <<"completion_tokens">> => 12,
                           <<"total_tokens">> => 20}}
    ],
    Acc = lists:foldl(fun beamai_llm_provider_common:accumulate_completions_event/2,
                      beamai_llm_http_client:init_stream_acc(), Events),
    {ok, Resp} = beamai_llm_provider_common:finalize_completions_stream(
                     Acc, beamai_llm_response_parser:parser_deepseek_fim()),
    ?assertEqual(deepseek, beamai_llm_response:provider(Resp)),
    ?assertEqual(<<"    if n <= 1: return n\n">>, beamai_llm_response:content(Resp)),
    ?assertEqual(complete, beamai_llm_response:finish_reason(Resp)),
    ?assertEqual(8, beamai_llm_response:input_tokens(Resp)),
    ?assertEqual(12, beamai_llm_response:output_tokens(Resp)).

%%====================================================================
%% 缓存统计测试
%%====================================================================

cache_stats_in_usage_details_test() ->
    Raw = #{
        <<"id">> => <<"ds-cache">>,
        <<"model">> => <<"deepseek-chat">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"role">> => <<"assistant">>, <<"content">> => <<"hi">>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{
            <<"prompt_tokens">> => 100,
            <<"completion_tokens">> => 10,
            <<"total_tokens">> => 110,
            <<"prompt_cache_hit_tokens">> => 64,
            <<"prompt_cache_miss_tokens">> => 36
        }
    },
    {ok, Resp} = beamai_llm_response_parser:from_deepseek(Raw),
    Usage = beamai_llm_response:usage(Resp),
    ?assertEqual(100, maps:get(input_tokens, Usage)),
    Details = maps:get(details, Usage),
    ?assertEqual(64, maps:get(prompt_cache_hit_tokens, Details)),
    ?assertEqual(36, maps:get(prompt_cache_miss_tokens, Details)).

no_cache_stats_no_details_test() ->
    Raw = #{
        <<"id">> => <<"ds-nocache">>,
        <<"model">> => <<"deepseek-chat">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"role">> => <<"assistant">>, <<"content">> => <<"hi">>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{<<"prompt_tokens">> => 5, <<"completion_tokens">> => 5,
                         <<"total_tokens">> => 10}
    },
    {ok, Resp} = beamai_llm_response_parser:from_deepseek(Raw),
    ?assertNot(maps:is_key(details, beamai_llm_response:usage(Resp))).
