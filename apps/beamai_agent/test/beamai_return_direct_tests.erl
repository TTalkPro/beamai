%%%-------------------------------------------------------------------
%%% @doc return_direct 直返测试（对标 Spring AI ToolExecutionResult.returnDirect）
%%%
%%% 覆盖：单工具直返、整批 AND 语义、混批不直返、失败不直返（与 Spring 的
%%% 有意分歧）、多工具结果拼接、直返回合落库、未注册工具名取保守值。
%%%
%%% 直返的关键证据是 **LLM 没被调第二次**——故每个用例都断言调用次数，
%%% 只看答案文本区分不出「直返」与「模型恰好复述了工具输出」。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_return_direct_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

%% 构造一个 OpenAI 嵌套格式 tool_call
tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

%% 构造 kernel（工具为 {Name, Handler, ReturnDirect} 列表）
kernel(Tools, Filters) ->
    lists:foldl(fun({Name, Handler, RD}, K) ->
        beamai_kernel:add_tool(K, #{name => Name, parameters => #{},
                                    handler => Handler, return_direct => RD})
    end, beamai_kernel:new(#{}, Filters), Tools).

%% mock LLM：首轮发 ToolCalls，之后返回 <<"llm-answer">>
mock_llm(ToolCalls) ->
    CC = counters:new(1, []),
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => ToolCalls}};
            _ -> {ok, #{content => <<"llm-answer">>, finish_reason => <<"stop">>}}
        end
    end),
    CC.

run(Tools, ToolCalls) ->
    run(Tools, ToolCalls, []).

%% @returns {RunResult, LLM 调用次数}
run(Tools, ToolCalls, Filters) ->
    CC = mock_llm(ToolCalls),
    K = beamai_kernel:add_service(kernel(Tools, Filters),
                                  beamai_chat_completion:create(mock, #{})),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K, memory => false}),
        {ok, Result, _} = beamai_agent:run(Agent, <<"go">>),
        {Result, counters:get(CC, 1)}
    after
        meck:unload(beamai_chat_completion)
    end.

%% turn filter：把工具循环的原始响应送回 Parent（run_result 不带 metadata）
capture_filter(Parent) ->
    beamai_filter:new(<<"capture">>, #{
        around_turn => fun(Req, _F, Next) ->
            Result = Next(Req),
            case Result of
                {ok, Resp, _TCM, _Iter, _Msgs} -> Parent ! {captured, Resp};
                _ -> ok
            end,
            Result
        end
    }).

flush() ->
    receive {captured, _} -> flush() after 0 -> ok end.

%%====================================================================
%% 直返基本行为
%%====================================================================

%% 单个 return_direct 工具：结果即最终答案，LLM 不再被调第二次
single_return_direct_test() ->
    {Result, Calls} = run([{<<"direct">>, fun(_) -> {ok, <<"TOOL-OUT">>} end, true}],
                          [tc(<<"c1">>, <<"direct">>)]),
    ?assertEqual(<<"TOOL-OUT">>, maps:get(content, Result)),
    ?assertEqual(complete, maps:get(finish_reason, Result)),
    ?assertEqual(1, Calls).

%% 直返响应带 metadata 标记，turn filter 据此可区分「模型说的」与「工具直返的」
return_direct_metadata_test() ->
    flush(),
    {_Result, _Calls} = run([{<<"direct">>, fun(_) -> {ok, <<"X">>} end, true}],
                            [tc(<<"c1">>, <<"direct">>)],
                            [capture_filter(self())]),
    receive
        {captured, Resp} ->
            ?assertMatch(#{return_direct := true}, beamai_llm_response:metadata(Resp))
    after 200 ->
        ?assert(false)
    end.

%% 未标注 return_direct：照常回灌模型，答案来自 LLM
no_return_direct_test() ->
    {Result, Calls} = run([{<<"plain">>, fun(_) -> {ok, <<"TOOL-OUT">>} end, false}],
                          [tc(<<"c1">>, <<"plain">>)]),
    ?assertEqual(<<"llm-answer">>, maps:get(content, Result)),
    ?assertEqual(2, Calls).

%%====================================================================
%% 整批 AND 语义
%%====================================================================

%% 整批都标注：直返，多工具结果按原始序换行拼接
all_return_direct_joined_test() ->
    {Result, Calls} = run([{<<"a">>, fun(_) -> {ok, <<"AA">>} end, true},
                           {<<"b">>, fun(_) -> {ok, <<"BB">>} end, true}],
                          [tc(<<"c1">>, <<"a">>), tc(<<"c2">>, <<"b">>)]),
    ?assertEqual(<<"AA\nBB">>, maps:get(content, Result)),
    ?assertEqual(1, Calls).

%% 混批（一个标注一个没有）：不直返——否则未标注工具的结果会被静默丢弃
mixed_batch_not_direct_test() ->
    {Result, Calls} = run([{<<"a">>, fun(_) -> {ok, <<"AA">>} end, true},
                           {<<"b">>, fun(_) -> {ok, <<"BB">>} end, false}],
                          [tc(<<"c1">>, <<"a">>), tc(<<"c2">>, <<"b">>)]),
    ?assertEqual(<<"llm-answer">>, maps:get(content, Result)),
    ?assertEqual(2, Calls).

%%====================================================================
%% 失败不直返（与 Spring 的有意分歧）
%%====================================================================

%% 标注了 return_direct 但工具失败：退回正常回灌，让模型看到错误自行补救
failed_return_direct_falls_back_test() ->
    {Result, Calls} = run([{<<"boom">>, fun(_) -> {error, <<"nope">>} end, true}],
                          [tc(<<"c1">>, <<"boom">>)]),
    ?assertEqual(<<"llm-answer">>, maps:get(content, Result)),
    ?assertEqual(2, Calls).

%% 整批标注但其中一个失败：整批不直返
partial_failure_not_direct_test() ->
    {Result, Calls} = run([{<<"ok_tool">>, fun(_) -> {ok, <<"AA">>} end, true},
                           {<<"boom">>, fun(_) -> {error, <<"nope">>} end, true}],
                          [tc(<<"c1">>, <<"ok_tool">>), tc(<<"c2">>, <<"boom">>)]),
    ?assertEqual(<<"llm-answer">>, maps:get(content, Result)),
    ?assertEqual(2, Calls).

%%====================================================================
%% kernel 查询 API
%%====================================================================

%% 已注册工具按标注返回
return_direct_tool_lookup_test() ->
    K = kernel([{<<"d">>, fun(_) -> {ok, <<>>} end, true},
                {<<"p">>, fun(_) -> {ok, <<>>} end, false}], []),
    ?assert(beamai_kernel:return_direct_tool(K, <<"d">>)),
    ?assertNot(beamai_kernel:return_direct_tool(K, <<"p">>)).

%% 未注册的工具名取保守值 false（直返会终止循环、丢弃同批其余结果）
return_direct_unknown_tool_false_test() ->
    ?assertNot(beamai_kernel:return_direct_tool(beamai_kernel:new(), <<"ghost">>)).

%% 未声明 return_direct 字段的工具 spec 默认 false
is_return_direct_default_test() ->
    ?assertNot(beamai_tool:is_return_direct(#{name => <<"t">>, handler => fun(_) -> ok end})).
