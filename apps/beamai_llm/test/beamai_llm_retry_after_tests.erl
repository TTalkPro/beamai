%%%-------------------------------------------------------------------
%%% @doc Retry-After 退避单元测试（C）
%%%
%%% 覆盖：
%%%   - retry_after_ms/1 对 Retry-After 头的解析
%%%   - 429 错误透传 Retry-After 到富错误（4 元组）
%%%   - is_retryable / compute_delay 对富错误的处理
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_retry_after_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% retry_after_ms/1
%%====================================================================

retry_after_integer_seconds_test() ->
    ?assertEqual(3000, beamai_llm_provider_common:retry_after_ms(
        [{<<"retry-after">>, <<"3">>}])).

retry_after_case_insensitive_test() ->
    ?assertEqual(2000, beamai_llm_provider_common:retry_after_ms(
        [{<<"Retry-After">>, <<"2">>}])).

retry_after_absent_test() ->
    ?assertEqual(undefined, beamai_llm_provider_common:retry_after_ms(
        [{<<"content-type">>, <<"application/json">>}])).

retry_after_http_date_unparsed_test() ->
    %% HTTP-date 格式暂不支持，回退 undefined
    ?assertEqual(undefined, beamai_llm_provider_common:retry_after_ms(
        [{<<"retry-after">>, <<"Wed, 21 Oct 2026 07:28:00 GMT">>}])).

%%====================================================================
%% is_retryable / compute_delay（富错误）
%%====================================================================

is_retryable_rich_errors_test() ->
    ?assert(beamai_chat_completion:is_retryable({http_error, 429, <<>>, #{retry_after_ms => 1000}})),
    ?assert(beamai_chat_completion:is_retryable({http_error, 503, <<>>, #{}})),
    %% 旧 3 元组仍可重试
    ?assert(beamai_chat_completion:is_retryable({http_error, 429, <<>>})),
    ?assert(beamai_chat_completion:is_retryable({http_error, 500, <<>>})),
    ?assertNot(beamai_chat_completion:is_retryable({http_error, 400, <<>>, #{}})).

compute_delay_honors_retry_after_test() ->
    RetryOpts = #{retry_delay => 1000, max_retries => 3},
    %% Retry-After=5s 优先于指数退避
    ?assertEqual(5000, beamai_chat_completion:compute_delay(
        RetryOpts, 0, {http_error, 429, <<>>, #{retry_after_ms => 5000}})),
    %% 上限封顶 60s
    ?assertEqual(60000, beamai_chat_completion:compute_delay(
        RetryOpts, 0, {http_error, 429, <<>>, #{retry_after_ms => 120000}})).

compute_delay_falls_back_to_exponential_test() ->
    RetryOpts = #{retry_delay => 1000, max_retries => 3},
    %% 无 Retry-After：指数退避 retry_delay * (Attempt+1)
    ?assertEqual(1000, beamai_chat_completion:compute_delay(RetryOpts, 0, {http_error, 500, <<>>})),
    ?assertEqual(3000, beamai_chat_completion:compute_delay(RetryOpts, 2, {http_error, 500, <<>>})).

%%====================================================================
%% 端到端：429 响应把 Retry-After 透传到富错误
%%====================================================================

http_error_429_carries_retry_after_test_() ->
    {setup,
     fun() -> beamai_llm_fake_backend:install() end,
     fun(Prev) -> beamai_llm_fake_backend:uninstall(Prev) end,
     fun(_) -> [?_test(error_429_enriched())] end}.

error_429_enriched() ->
    %% fake 后端在 4xx 时返回带响应头的富错误
    beamai_llm_fake_backend:set_error(429, <<"{\"error\":\"rate limited\"}">>,
                                      [{<<"retry-after">>, <<"4">>}]),
    Config = #{api_key => <<"k">>, model => <<"claude-sonnet-4-5">>},
    Result = beamai_llm_provider_anthropic:chat(
        Config, #{messages => [#{role => user, content => <<"hi">>}]}),
    ?assertMatch({error, {http_error, 429, _, #{retry_after_ms := 4000}}}, Result).
