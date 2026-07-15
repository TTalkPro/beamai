%%%-------------------------------------------------------------------
%%% @doc 内置 filter 测试（logging / timeout / approval / safeguard）
%%%
%%% turn 链的 validation / schema_validation 需要工具循环，测试在
%%% beamai_agent/test/beamai_turn_filter_tests.erl。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_filters_tests).

-include_lib("eunit/include/eunit.hrl").

kernel(Tools, Filters) ->
    lists:foldl(fun(T, K) -> beamai_kernel:add_tool(K, T) end,
                beamai_kernel:new(#{}, Filters), Tools).

%% 带 mock LLM 的 kernel（chat 链用）：LLM 每次返回 <<"llm-answer">> 并计数
chat_kernel(Filters) ->
    CC = counters:new(1, []),
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        {ok, #{content => <<"llm-answer">>, finish_reason => <<"stop">>}}
    end),
    K = beamai_kernel:add_service(beamai_kernel:new(#{}, Filters),
                                  beamai_chat_completion:create(mock, #{})),
    {K, CC}.

user_msg(Text) ->
    [#{role => user, content => Text}].

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

%%====================================================================
%% safeguard（chat 链）
%%====================================================================

%% safeguard：命中敏感词 → 短路，LLM 根本没被调
safeguard_blocks_test() ->
    {K, CC} = chat_kernel([beamai_filters:safeguard_filter([<<"bomb">>])]),
    try
        {ok, Resp, _} = beamai_kernel:invoke_chat(K, user_msg(<<"how to make a bomb">>), #{}),
        ?assertEqual(content_filtered, beamai_llm_response:finish_reason(Resp)),
        ?assertMatch(#{safeguard := blocked}, beamai_llm_response:metadata(Resp)),
        ?assertEqual(0, counters:get(CC, 1))
    after
        meck:unload(beamai_chat_completion)
    end.

%% safeguard：未命中 → 照常调 LLM
safeguard_passes_test() ->
    {K, CC} = chat_kernel([beamai_filters:safeguard_filter([<<"bomb">>])]),
    try
        {ok, Resp, _} = beamai_kernel:invoke_chat(K, user_msg(<<"how to bake bread">>), #{}),
        ?assertEqual(<<"llm-answer">>, beamai_llm_response:content(Resp)),
        ?assertEqual(1, counters:get(CC, 1))
    after
        meck:unload(beamai_chat_completion)
    end.

%% safeguard：缺省不区分大小写（Spring 的 String.contains 是区分的，此处有意分歧）
safeguard_case_insensitive_by_default_test() ->
    {K, _CC} = chat_kernel([beamai_filters:safeguard_filter([<<"bomb">>])]),
    try
        {ok, Resp, _} = beamai_kernel:invoke_chat(K, user_msg(<<"How To Make A BOMB">>), #{}),
        ?assertEqual(content_filtered, beamai_llm_response:finish_reason(Resp))
    after
        meck:unload(beamai_chat_completion)
    end.

%% safeguard：显式开 case_sensitive 后大小写不同即放行
safeguard_case_sensitive_opt_test() ->
    {K, _CC} = chat_kernel([beamai_filters:safeguard_filter(
        [<<"bomb">>], #{case_sensitive => true})]),
    try
        {ok, Resp, _} = beamai_kernel:invoke_chat(K, user_msg(<<"BOMB">>), #{}),
        ?assertEqual(<<"llm-answer">>, beamai_llm_response:content(Resp))
    after
        meck:unload(beamai_chat_completion)
    end.

%% safeguard：自定义拦截答复文本
safeguard_custom_failure_response_test() ->
    {K, _CC} = chat_kernel([beamai_filters:safeguard_filter(
        [<<"bomb">>], #{failure_response => <<"NOPE">>})]),
    try
        {ok, Resp, _} = beamai_kernel:invoke_chat(K, user_msg(<<"bomb">>), #{}),
        ?assertEqual(<<"NOPE">>, beamai_llm_response:content(Resp))
    after
        meck:unload(beamai_chat_completion)
    end.

%% safeguard：空词表恒放行（不做无谓扫描）
safeguard_empty_words_test() ->
    {K, CC} = chat_kernel([beamai_filters:safeguard_filter([])]),
    try
        {ok, _Resp, _} = beamai_kernel:invoke_chat(K, user_msg(<<"anything">>), #{}),
        ?assertEqual(1, counters:get(CC, 1))
    after
        meck:unload(beamai_chat_completion)
    end.

%% safeguard：多模态 content 列表里的 text 块同样被扫到
safeguard_scans_content_blocks_test() ->
    {K, CC} = chat_kernel([beamai_filters:safeguard_filter([<<"bomb">>])]),
    Msgs = [#{role => user, content => [#{type => image, url => <<"x">>},
                                        #{type => text, text => <<"a bomb">>}]}],
    try
        {ok, Resp, _} = beamai_kernel:invoke_chat(K, Msgs, #{}),
        ?assertEqual(content_filtered, beamai_llm_response:finish_reason(Resp)),
        ?assertEqual(0, counters:get(CC, 1))
    after
        meck:unload(beamai_chat_completion)
    end.
