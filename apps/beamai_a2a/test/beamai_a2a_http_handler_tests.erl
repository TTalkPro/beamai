%%%-------------------------------------------------------------------
%%% @doc beamai_a2a_http_handler 单元测试
%%%
%%% 仅测试不需要应用启动的独立功能。
%%% 完整的集成测试应该在应用级别进行。
%%%-------------------------------------------------------------------
-module(beamai_a2a_http_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试组
%%====================================================================

%% 不依赖应用启动的测试
rate_limit_headers_test_() ->
    [
     {"禁用限流时返回空头", fun test_rate_limit_disabled/0},
     {"正常限流信息", fun test_rate_limit_normal/0},
     {"被限流时的 Retry-After", fun test_rate_limit_exceeded/0}
    ].

%%====================================================================
%% Rate Limit Headers 测试
%%====================================================================

test_rate_limit_disabled() ->
    Headers = beamai_a2a_http_handler:build_rate_limit_headers(#{enabled => false}),
    ?assertEqual(#{}, Headers).

test_rate_limit_normal() ->
    RateLimitInfo = #{
        remaining => 95,
        limit => 100,
        reset_at => 1609459200,
        retry_after => 0
    },
    Headers = beamai_a2a_http_handler:build_rate_limit_headers(RateLimitInfo),
    ?assertEqual(<<"100">>, maps:get(<<"x-ratelimit-limit">>, Headers)),
    ?assertEqual(<<"95">>, maps:get(<<"x-ratelimit-remaining">>, Headers)),
    ?assertEqual(<<"1609459200">>, maps:get(<<"x-ratelimit-reset">>, Headers)),
    %% 不应该有 retry-after 头
    ?assertEqual(false, maps:is_key(<<"retry-after">>, Headers)).

test_rate_limit_exceeded() ->
    RateLimitedInfo = #{
        remaining => 0,
        limit => 100,
        reset_at => 1609459200,
        retry_after => 60000  %% 60 秒（毫秒）
    },
    Headers = beamai_a2a_http_handler:build_rate_limit_headers(RateLimitedInfo),
    ?assertEqual(<<"100">>, maps:get(<<"x-ratelimit-limit">>, Headers)),
    ?assertEqual(<<"0">>, maps:get(<<"x-ratelimit-remaining">>, Headers)),
    %% retry_after 应该被转换为秒
    ?assertEqual(<<"60">>, maps:get(<<"retry-after">>, Headers)).
