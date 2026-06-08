%%%-------------------------------------------------------------------
%%% @doc 速率限制响应头提取与注入单元测试（P1-B）
%%%
%%% 覆盖：
%%%   - rate_limit_metadata/1 对各厂商响应头的归一化
%%%   - 经 beamai_http:request_meta 将响应头注入 response.metadata 的端到端路径
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_rate_limit_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% rate_limit_metadata/1 纯函数测试
%%====================================================================

anthropic_ratelimit_headers_test() ->
    Headers = [
        {<<"anthropic-ratelimit-requests-limit">>, <<"50">>},
        {<<"anthropic-ratelimit-requests-remaining">>, <<"49">>},
        {<<"anthropic-ratelimit-tokens-reset">>, <<"2026-06-08T00:00:00Z">>},
        {<<"content-type">>, <<"application/json">>}
    ],
    #{rate_limit := RL} = beamai_llm_provider_common:rate_limit_metadata(Headers),
    ?assertEqual(<<"50">>, maps:get(<<"requests-limit">>, RL)),
    ?assertEqual(<<"49">>, maps:get(<<"requests-remaining">>, RL)),
    ?assertEqual(<<"2026-06-08T00:00:00Z">>, maps:get(<<"tokens-reset">>, RL)),
    %% 非速率限制头不收集
    ?assertNot(maps:is_key(<<"content-type">>, RL)).

openai_xratelimit_headers_test() ->
    Headers = [
        {<<"x-ratelimit-limit-requests">>, <<"3500">>},
        {<<"x-ratelimit-remaining-tokens">>, <<"89000">>},
        {<<"retry-after">>, <<"1">>}
    ],
    #{rate_limit := RL} = beamai_llm_provider_common:rate_limit_metadata(Headers),
    ?assertEqual(<<"3500">>, maps:get(<<"limit-requests">>, RL)),
    ?assertEqual(<<"89000">>, maps:get(<<"remaining-tokens">>, RL)),
    ?assertEqual(<<"1">>, maps:get(<<"retry-after">>, RL)).

case_insensitive_header_names_test() ->
    Headers = [{<<"Anthropic-RateLimit-Requests-Limit">>, <<"40">>}],
    #{rate_limit := RL} = beamai_llm_provider_common:rate_limit_metadata(Headers),
    ?assertEqual(<<"40">>, maps:get(<<"requests-limit">>, RL)).

no_rate_limit_headers_returns_empty_test() ->
    Headers = [{<<"content-type">>, <<"application/json">>}, {<<"date">>, <<"...">>}],
    ?assertEqual(#{}, beamai_llm_provider_common:rate_limit_metadata(Headers)).

empty_headers_returns_empty_test() ->
    ?assertEqual(#{}, beamai_llm_provider_common:rate_limit_metadata([])).

%%====================================================================
%% 端到端：响应头注入 response.metadata
%%====================================================================

rate_limit_injected_fixture_test_() ->
    {setup,
     fun() -> beamai_llm_fake_backend:install() end,
     fun(Prev) -> beamai_llm_fake_backend:uninstall(Prev) end,
     fun(_) -> [
        ?_test(anthropic_injects_rate_limit()),
        ?_test(openai_injects_rate_limit()),
        ?_test(no_headers_no_rate_limit_key())
     ] end}.

anthropic_injects_rate_limit() ->
    beamai_llm_fake_backend:set_response(
        anthropic_body(),
        [{<<"anthropic-ratelimit-requests-remaining">>, <<"42">>},
         {<<"anthropic-ratelimit-tokens-limit">>, <<"100000">>}]),
    Config = #{api_key => <<"k">>, model => <<"claude-sonnet-4-5">>},
    {ok, Resp} = beamai_llm_provider_anthropic:chat(
        Config, #{messages => [#{role => user, content => <<"hi">>}]}),
    RL = maps:get(rate_limit, beamai_llm_response:metadata(Resp)),
    ?assertEqual(<<"42">>, maps:get(<<"requests-remaining">>, RL)),
    ?assertEqual(<<"100000">>, maps:get(<<"tokens-limit">>, RL)),
    %% 正文仍正常解析
    ?assertEqual(<<"hi there">>, beamai_llm_response:content(Resp)).

openai_injects_rate_limit() ->
    beamai_llm_fake_backend:set_response(
        openai_body(),
        [{<<"x-ratelimit-remaining-requests">>, <<"3499">>}]),
    Config = #{api_key => <<"k">>, model => <<"gpt-4o">>},
    {ok, Resp} = beamai_llm_provider_openai:chat(
        Config, #{messages => [#{role => user, content => <<"hi">>}]}),
    RL = maps:get(rate_limit, beamai_llm_response:metadata(Resp)),
    ?assertEqual(<<"3499">>, maps:get(<<"remaining-requests">>, RL)).

no_headers_no_rate_limit_key() ->
    beamai_llm_fake_backend:set_response(
        anthropic_body(),
        [{<<"content-type">>, <<"application/json">>}]),
    Config = #{api_key => <<"k">>, model => <<"claude-sonnet-4-5">>},
    {ok, Resp} = beamai_llm_provider_anthropic:chat(
        Config, #{messages => [#{role => user, content => <<"hi">>}]}),
    ?assertNot(maps:is_key(rate_limit, beamai_llm_response:metadata(Resp))).

%%====================================================================
%% 端到端：流式路径同样注入速率限制头
%%====================================================================

stream_rate_limit_fixture_test_() ->
    {setup,
     fun() -> beamai_llm_fake_backend:install() end,
     fun(Prev) -> beamai_llm_fake_backend:uninstall(Prev) end,
     fun(_) -> [?_test(anthropic_stream_injects_rate_limit())] end}.

anthropic_stream_injects_rate_limit() ->
    beamai_llm_fake_backend:set_stream(
        anthropic_stream_chunks(),
        [{<<"anthropic-ratelimit-requests-remaining">>, <<"7">>}]),
    Config = #{api_key => <<"k">>, model => <<"claude-sonnet-4-5">>},
    {ok, Resp} = beamai_llm_provider_anthropic:stream_chat(
        Config, #{messages => [#{role => user, content => <<"hi">>}]},
        fun(_Event) -> ok end),
    %% 流式累加正确
    ?assertEqual(<<"hi">>, beamai_llm_response:content(Resp)),
    ?assertEqual(complete, beamai_llm_response:finish_reason(Resp)),
    %% 速率限制头注入 metadata
    RL = maps:get(rate_limit, beamai_llm_response:metadata(Resp)),
    ?assertEqual(<<"7">>, maps:get(<<"requests-remaining">>, RL)).

anthropic_stream_chunks() ->
    [
     <<"data: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_1\",\"model\":\"claude-sonnet-4-5\",\"usage\":{\"input_tokens\":2}}}\n">>,
     <<"data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n">>,
     <<"data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"hi\"}}\n">>,
     <<"data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}\n">>
    ].

%%====================================================================
%% fixtures
%%====================================================================

anthropic_body() ->
    #{
        <<"id">> => <<"msg_1">>,
        <<"model">> => <<"claude-sonnet-4-5">>,
        <<"type">> => <<"message">>,
        <<"role">> => <<"assistant">>,
        <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"hi there">>}],
        <<"stop_reason">> => <<"end_turn">>,
        <<"usage">> => #{<<"input_tokens">> => 2, <<"output_tokens">> => 2}
    }.

openai_body() ->
    #{
        <<"id">> => <<"chatcmpl-1">>,
        <<"model">> => <<"gpt-4o">>,
        <<"object">> => <<"chat.completion">>,
        <<"choices">> => [#{
            <<"index">> => 0,
            <<"message">> => #{<<"role">> => <<"assistant">>, <<"content">> => <<"hi there">>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{<<"prompt_tokens">> => 2, <<"completion_tokens">> => 2, <<"total_tokens">> => 4}
    }.
