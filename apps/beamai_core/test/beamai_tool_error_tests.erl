%%%-------------------------------------------------------------------
%%% @doc beamai_tool_error 分类单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_error_tests).

-include_lib("eunit/include/eunit.hrl").

%% 显式 error_class 最高优先级
explicit_class_test() ->
    ?assertEqual(environment, beamai_tool_error:classify(#{error_class => environment})),
    ?assertEqual(transient, beamai_tool_error:classify(#{error_class => transient})),
    ?assertEqual(semantic, beamai_tool_error:classify(#{error_class => semantic})).

%% 异常包装：递归到 reason
exception_wrapper_test() ->
    Wrapped = #{class => error, reason => #{error_class => environment}, stacktrace => []},
    ?assertEqual(environment, beamai_tool_error:classify(Wrapped)).

%% canonical llm_error 结构匹配（不依赖 beamai_llm）
llm_error_auth_is_environment_test() ->
    E = #{'__llm_error__' => true, type => auth, retryable => false},
    ?assertEqual(environment, beamai_tool_error:classify(E)).

llm_error_retryable_is_transient_test() ->
    E = #{'__llm_error__' => true, type => rate_limit, retryable => true},
    ?assertEqual(transient, beamai_tool_error:classify(E)).

llm_error_other_is_semantic_test() ->
    E = #{'__llm_error__' => true, type => client_error, retryable => false},
    ?assertEqual(semantic, beamai_tool_error:classify(E)).

%% Erlang 超时 / 网络 → transient
timeout_network_transient_test() ->
    ?assertEqual(transient, beamai_tool_error:classify(timeout)),
    ?assertEqual(transient, beamai_tool_error:classify({timeout, foo})),
    ?assertEqual(transient, beamai_tool_error:classify({request_failed, econnrefused})),
    ?assertEqual(transient, beamai_tool_error:classify(tool_timeout)),
    ?assertEqual(transient, beamai_tool_error:classify(tool_worker_crash)).

%% 缺省 semantic
default_semantic_test() ->
    ?assertEqual(semantic, beamai_tool_error:classify(some_random_reason)),
    ?assertEqual(semantic, beamai_tool_error:classify({not_found, <<"x">>})),
    ?assertEqual(semantic, beamai_tool_error:classify(#{error => #{type => <<"bad_arg">>}})).

%% message 提取
message_test() ->
    ?assertEqual(<<"boom">>, beamai_tool_error:message(<<"boom">>)),
    ?assertEqual(<<"nope">>, beamai_tool_error:message(#{message => <<"nope">>})),
    ?assertEqual(<<"deep">>, beamai_tool_error:message(
        #{class => error, reason => #{message => <<"deep">>}, stacktrace => []})).
