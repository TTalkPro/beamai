%%%-------------------------------------------------------------------
%%% @doc 统一错误结构单元测试（D）
%%%
%%% 覆盖 beamai_llm_error 对各类 provider/HTTP 错误的归一化分类。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_error_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 分类
%%====================================================================

rate_limit_3tuple_test() ->
    E = beamai_llm_error:from_reason({http_error, 429, <<"busy">>}, anthropic),
    ?assertEqual(rate_limit, beamai_llm_error:type(E)),
    ?assertEqual(429, beamai_llm_error:status(E)),
    ?assert(beamai_llm_error:retryable(E)),
    ?assertEqual(anthropic, beamai_llm_error:provider(E)),
    ?assertEqual(undefined, beamai_llm_error:retry_after_ms(E)).

rate_limit_with_retry_after_test() ->
    E = beamai_llm_error:from_reason({http_error, 429, <<"busy">>, #{retry_after_ms => 5000}}, openai),
    ?assertEqual(rate_limit, beamai_llm_error:type(E)),
    ?assert(beamai_llm_error:retryable(E)),
    ?assertEqual(5000, beamai_llm_error:retry_after_ms(E)).

server_error_test() ->
    E = beamai_llm_error:from_reason({http_error, 503, <<>>}),
    ?assertEqual(server_error, beamai_llm_error:type(E)),
    ?assert(beamai_llm_error:retryable(E)),
    ?assertEqual(<<"Service unavailable">>, beamai_llm_error:message(E)).

auth_error_test() ->
    E = beamai_llm_error:from_reason({http_error, 401, <<>>}),
    ?assertEqual(auth, beamai_llm_error:type(E)),
    ?assertNot(beamai_llm_error:retryable(E)).

client_error_not_retryable_test() ->
    E = beamai_llm_error:from_reason({http_error, 400, <<>>}),
    ?assertEqual(client_error, beamai_llm_error:type(E)),
    ?assertNot(beamai_llm_error:retryable(E)).

missing_api_key_test() ->
    E = beamai_llm_error:from_reason(missing_api_key, deepseek),
    ?assertEqual(auth, beamai_llm_error:type(E)),
    ?assertNot(beamai_llm_error:retryable(E)),
    ?assertEqual(<<"Missing API key">>, beamai_llm_error:message(E)).

timeout_test() ->
    E = beamai_llm_error:from_reason({request_failed, timeout}),
    ?assertEqual(timeout, beamai_llm_error:type(E)),
    ?assert(beamai_llm_error:retryable(E)).

connection_closed_retryable_test() ->
    E = beamai_llm_error:from_reason({request_failed, {closed, econnreset}}),
    ?assertEqual(network, beamai_llm_error:type(E)),
    ?assert(beamai_llm_error:retryable(E)).

network_other_not_retryable_test() ->
    E = beamai_llm_error:from_reason({request_failed, nxdomain}),
    ?assertEqual(network, beamai_llm_error:type(E)),
    ?assertNot(beamai_llm_error:retryable(E)).

api_error_message_extracted_test() ->
    E = beamai_llm_error:from_reason({api_error, #{<<"message">> => <<"bad model"/utf8>>}}),
    ?assertEqual(api_error, beamai_llm_error:type(E)),
    ?assertEqual(<<"bad model"/utf8>>, beamai_llm_error:message(E)),
    ?assertNot(beamai_llm_error:retryable(E)).

api_error_nested_message_test() ->
    E = beamai_llm_error:from_reason({api_error, #{<<"error">> => #{<<"message">> => <<"boom">>}}}),
    ?assertEqual(<<"boom">>, beamai_llm_error:message(E)).

invalid_response_test() ->
    E = beamai_llm_error:from_reason({invalid_response, #{}}),
    ?assertEqual(invalid_response, beamai_llm_error:type(E)),
    ?assertNot(beamai_llm_error:retryable(E)).

unknown_test() ->
    E = beamai_llm_error:from_reason(some_weird_thing),
    ?assertEqual(unknown, beamai_llm_error:type(E)),
    ?assertNot(beamai_llm_error:retryable(E)).

%%====================================================================
%% 形态与访问器
%%====================================================================

accepts_error_tuple_test() ->
    E = beamai_llm_error:from_reason({error, {http_error, 429, <<>>}}, anthropic),
    ?assertEqual(rate_limit, beamai_llm_error:type(E)).

is_error_test() ->
    E = beamai_llm_error:from_reason(missing_api_key),
    ?assert(beamai_llm_error:is_error(E)),
    ?assertNot(beamai_llm_error:is_error({error, foo})),
    ?assertNot(beamai_llm_error:is_error(#{})).

raw_preserved_test() ->
    Raw = {http_error, 500, <<"oops">>},
    E = beamai_llm_error:from_reason(Raw),
    ?assertEqual(Raw, beamai_llm_error:raw(E)).
