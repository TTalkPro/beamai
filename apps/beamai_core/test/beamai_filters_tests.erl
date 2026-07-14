%%%-------------------------------------------------------------------
%%% @doc 内置 tool 链 filter 测试（logging / timeout / approval）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_filters_tests).

-include_lib("eunit/include/eunit.hrl").

kernel(Tools, Filters) ->
    lists:foldl(fun(T, K) -> beamai_kernel:add_tool(K, T) end,
                beamai_kernel:new(#{}, Filters), Tools).

%% logging：透传不改结果
logging_passthrough_test() ->
    K = kernel([#{name => <<"t">>, parameters => #{},
                  handler => fun(_) -> {ok, <<"r">>} end}],
               [beamai_filters:logging_filter()]),
    ?assertEqual({ok, <<"r">>, #{}},
                 beamai_kernel:invoke_tool(K, <<"t">>, #{}, beamai_context:new())).

%% timeout：慢工具超时 → {error, timeout}
timeout_short_circuits_test() ->
    K = kernel([#{name => <<"slow">>, parameters => #{},
                  handler => fun(_) -> timer:sleep(500), {ok, <<"late">>} end}],
               [beamai_filters:timeout_filter(50)]),
    ?assertEqual({error, timeout},
                 beamai_kernel:invoke_tool(K, <<"slow">>, #{}, beamai_context:new())).

%% timeout：快工具正常返回
timeout_fast_ok_test() ->
    K = kernel([#{name => <<"fast">>, parameters => #{},
                  handler => fun(_) -> {ok, <<"quick">>} end}],
               [beamai_filters:timeout_filter(1000)]),
    ?assertEqual({ok, <<"quick">>, #{}},
                 beamai_kernel:invoke_tool(K, <<"fast">>, #{}, beamai_context:new())).

%% timeout 结果分类为 transient
timeout_classified_transient_test() ->
    ?assertEqual(transient, beamai_tool_error:classify(timeout)).

%% approval：sensitive 工具被拒 → 拒绝结果（不执行）
approval_rejects_sensitive_test() ->
    Executed = counters:new(1, []),
    K = kernel([#{name => <<"danger">>, parameters => #{}, sensitive => true,
                  handler => fun(_) -> counters:add(Executed, 1, 1), {ok, <<"done">>} end}],
               [beamai_filters:approval_filter(fun(_N, _A) -> false end)]),
    {ok, Result, _} = beamai_kernel:invoke_tool(K, <<"danger">>, #{}, beamai_context:new()),
    %% 拒绝结果（断言 ASCII 类型，避免跨文件中文字节比对）
    ?assertNotEqual(nomatch, binary:match(Result, <<"not_approved">>)),
    ?assertEqual(0, counters:get(Executed, 1)).   %% 未执行

%% approval：非 sensitive 工具照常执行（不审批）
approval_ignores_non_sensitive_test() ->
    K = kernel([#{name => <<"safe">>, parameters => #{},
                  handler => fun(_) -> {ok, <<"ran">>} end}],
               [beamai_filters:approval_filter(fun(_N, _A) -> false end)]),
    ?assertEqual({ok, <<"ran">>, #{}},
                 beamai_kernel:invoke_tool(K, <<"safe">>, #{}, beamai_context:new())).

%% approval：sensitive 但获批 → 执行
approval_allows_when_approved_test() ->
    K = kernel([#{name => <<"danger">>, parameters => #{}, sensitive => true,
                  handler => fun(_) -> {ok, <<"done">>} end}],
               [beamai_filters:approval_filter(fun(_N, _A) -> true end)]),
    ?assertEqual({ok, <<"done">>, #{}},
                 beamai_kernel:invoke_tool(K, <<"danger">>, #{}, beamai_context:new())).
