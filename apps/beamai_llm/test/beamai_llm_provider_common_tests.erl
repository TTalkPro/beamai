%%%-------------------------------------------------------------------
%%% @doc beamai_llm_provider_common 模块单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_common_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% build_url/3 测试
%%====================================================================

build_url_default_test_() ->
    [
        %% 使用默认值
        ?_assertEqual(
            <<"https://api.openai.com/v1/chat/completions">>,
            beamai_llm_provider_common:build_url(#{}, <<"/v1/chat/completions">>, <<"https://api.openai.com">>)
        )
    ].

build_url_custom_base_test_() ->
    [
        %% 自定义 base_url
        ?_assertEqual(
            <<"https://custom.api.com/v1/chat/completions">>,
            beamai_llm_provider_common:build_url(
                #{base_url => <<"https://custom.api.com">>},
                <<"/v1/chat/completions">>,
                <<"https://api.openai.com">>
            )
        )
    ].

build_url_custom_endpoint_test_() ->
    [
        %% 自定义 endpoint
        ?_assertEqual(
            <<"https://api.openai.com/v2/completions">>,
            beamai_llm_provider_common:build_url(
                #{endpoint => <<"/v2/completions">>},
                <<"/v1/chat/completions">>,
                <<"https://api.openai.com">>
            )
        )
    ].

build_url_both_custom_test_() ->
    [
        %% 同时自定义 base_url 和 endpoint
        ?_assertEqual(
            <<"https://custom.api.com/custom/endpoint">>,
            beamai_llm_provider_common:build_url(
                #{base_url => <<"https://custom.api.com">>, endpoint => <<"/custom/endpoint">>},
                <<"/default/endpoint">>,
                <<"https://default.api.com">>
            )
        )
    ].

%%====================================================================
%% build_bearer_auth_headers/1 测试
%%====================================================================

build_bearer_auth_headers_test_() ->
    Headers = beamai_llm_provider_common:build_bearer_auth_headers(#{api_key => <<"sk-test-key">>}),
    [
        %% 应该返回列表
        ?_assert(is_list(Headers)),
        %% 应该包含 Authorization 头
        ?_assertMatch({<<"Authorization">>, <<"Bearer sk-test-key">>},
                      lists:keyfind(<<"Authorization">>, 1, Headers)),
        %% 应该包含 Content-Type 头
        ?_assertMatch({<<"Content-Type">>, <<"application/json">>},
                      lists:keyfind(<<"Content-Type">>, 1, Headers))
    ].

%%====================================================================
%% maybe_add_stream/2 测试
%%====================================================================

maybe_add_stream_true_test() ->
    Body = #{<<"model">> => <<"gpt-4">>},
    Request = #{stream => true},
    Result = beamai_llm_provider_common:maybe_add_stream(Body, Request),
    ?assertEqual(true, maps:get(<<"stream">>, Result)).

maybe_add_stream_false_test() ->
    Body = #{<<"model">> => <<"gpt-4">>},
    Request = #{stream => false},
    Result = beamai_llm_provider_common:maybe_add_stream(Body, Request),
    ?assertEqual(undefined, maps:get(<<"stream">>, Result, undefined)).

maybe_add_stream_absent_test() ->
    Body = #{<<"model">> => <<"gpt-4">>},
    Request = #{},
    Result = beamai_llm_provider_common:maybe_add_stream(Body, Request),
    ?assertEqual(undefined, maps:get(<<"stream">>, Result, undefined)).

%%====================================================================
%% maybe_add_top_p/2 测试
%%====================================================================

maybe_add_top_p_test_() ->
    [
        %% 有 top_p 参数
        ?_assertEqual(
            #{<<"top_p">> => 0.9},
            beamai_llm_provider_common:maybe_add_top_p(#{}, #{top_p => 0.9})
        ),
        %% 无 top_p 参数
        ?_assertEqual(
            #{},
            beamai_llm_provider_common:maybe_add_top_p(#{}, #{})
        ),
        %% top_p 为整数
        ?_assertEqual(
            #{<<"top_p">> => 1},
            beamai_llm_provider_common:maybe_add_top_p(#{}, #{top_p => 1})
        )
    ].

%%====================================================================
%% maybe_add_params/3 测试
%%====================================================================

maybe_add_params_test_() ->
    Specs = [
        {frequency_penalty, <<"frequency_penalty">>},
        {seed, <<"seed">>},
        {stop, <<"stop">>}
    ],
    [
        %% 仅添加存在的参数
        ?_assertEqual(
            #{<<"frequency_penalty">> => 0.5, <<"seed">> => 42},
            beamai_llm_provider_common:maybe_add_params(
                #{}, #{frequency_penalty => 0.5, seed => 42, other => x}, Specs)
        ),
        %% 来源为空时不变
        ?_assertEqual(
            #{<<"model">> => <<"gpt-4">>},
            beamai_llm_provider_common:maybe_add_params(
                #{<<"model">> => <<"gpt-4">>}, #{}, Specs)
        )
    ].

%%====================================================================
%% maybe_add_tool_choice/2 测试（OpenAI 格式）
%%====================================================================

maybe_add_tool_choice_test_() ->
    [
        ?_assertEqual(#{<<"tool_choice">> => <<"auto">>},
                      beamai_llm_provider_common:maybe_add_tool_choice(#{}, #{tool_choice => auto})),
        ?_assertEqual(#{<<"tool_choice">> => <<"none">>},
                      beamai_llm_provider_common:maybe_add_tool_choice(#{}, #{tool_choice => none})),
        ?_assertEqual(#{<<"tool_choice">> => <<"required">>},
                      beamai_llm_provider_common:maybe_add_tool_choice(#{}, #{tool_choice => required})),
        %% 指定函数
        ?_assertEqual(
            #{<<"tool_choice">> => #{<<"type">> => <<"function">>,
                                     <<"function">> => #{<<"name">> => <<"get_weather">>}}},
            beamai_llm_provider_common:maybe_add_tool_choice(#{}, #{tool_choice => {tool, <<"get_weather">>}})
        ),
        %% map 高级用法直接透传
        ?_assertEqual(
            #{<<"tool_choice">> => #{<<"type">> => <<"function">>}},
            beamai_llm_provider_common:maybe_add_tool_choice(#{}, #{tool_choice => #{<<"type">> => <<"function">>}})
        ),
        %% 未指定时不变
        ?_assertEqual(#{}, beamai_llm_provider_common:maybe_add_tool_choice(#{}, #{}))
    ].

%%====================================================================
%% parse_tool_calls/1 测试
%%====================================================================

parse_tool_calls_empty_test_() ->
    [
        %% 无 tool_calls 字段
        ?_assertEqual([], beamai_llm_provider_common:parse_tool_calls(#{})),
        %% tool_calls 为空列表
        ?_assertEqual([], beamai_llm_provider_common:parse_tool_calls(#{<<"tool_calls">> => []}))
    ].

parse_tool_calls_test() ->
    Message = #{
        <<"tool_calls">> => [
            #{
                <<"id">> => <<"call_abc123">>,
                <<"function">> => #{
                    <<"name">> => <<"get_weather">>,
                    <<"arguments">> => <<"{\"city\":\"Beijing\"}">>
                }
            }
        ]
    },
    Result = beamai_llm_provider_common:parse_tool_calls(Message),
    ?assertEqual(1, length(Result)),
    [Call] = Result,
    ?assertEqual(<<"call_abc123">>, maps:get(id, Call)),
    ?assertEqual(<<"get_weather">>, maps:get(name, Call)),
    ?assertEqual(<<"{\"city\":\"Beijing\"}">>, maps:get(arguments, Call)).

%%====================================================================
%% parse_usage/1 测试
%%====================================================================

parse_usage_test_() ->
    [
        %% 完整的 usage
        ?_assertEqual(
            #{prompt_tokens => 10, completion_tokens => 20, total_tokens => 30},
            beamai_llm_provider_common:parse_usage(#{
                <<"prompt_tokens">> => 10,
                <<"completion_tokens">> => 20,
                <<"total_tokens">> => 30
            })
        ),
        %% 空 usage
        ?_assertEqual(
            #{prompt_tokens => 0, completion_tokens => 0, total_tokens => 0},
            beamai_llm_provider_common:parse_usage(#{})
        )
    ].

%%====================================================================
%% accumulate_openai_event/2 测试
%%====================================================================

accumulate_openai_event_content_test() ->
    Event = #{
        <<"id">> => <<"chatcmpl-123">>,
        <<"model">> => <<"gpt-4">>,
        <<"choices">> => [
            #{
                <<"delta">> => #{<<"content">> => <<"Hello">>},
                <<"finish_reason">> => null
            }
        ]
    },
    Acc = #{content => <<>>, id => <<>>, model => <<>>},
    Result = beamai_llm_provider_common:accumulate_openai_event(Event, Acc),
    ?assertEqual(<<"Hello">>, maps:get(content, Result)),
    ?assertEqual(<<"chatcmpl-123">>, maps:get(id, Result)),
    ?assertEqual(<<"gpt-4">>, maps:get(model, Result)).

accumulate_openai_event_accumulate_test() ->
    Event1 = #{
        <<"choices">> => [#{<<"delta">> => #{<<"content">> => <<"Hello">>}}]
    },
    Event2 = #{
        <<"choices">> => [#{<<"delta">> => #{<<"content">> => <<" World">>}}]
    },
    Acc0 = #{content => <<>>},
    Acc1 = beamai_llm_provider_common:accumulate_openai_event(Event1, Acc0),
    Acc2 = beamai_llm_provider_common:accumulate_openai_event(Event2, Acc1),
    ?assertEqual(<<"Hello World">>, maps:get(content, Acc2)).

accumulate_openai_event_non_matching_test() ->
    Event = #{<<"other">> => <<"data">>},
    Acc = #{content => <<"existing">>},
    Result = beamai_llm_provider_common:accumulate_openai_event(Event, Acc),
    ?assertEqual(<<"existing">>, maps:get(content, Result)).
