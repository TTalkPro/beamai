%%%-------------------------------------------------------------------
%%% @doc heal-dangling：放弃中断开新 chat → 补齐悬空 tool_calls
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_heal_dangling_tests).

-include_lib("eunit/include/eunit.hrl").

heal_dangling_on_new_chat_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    CC = counters:new(1, []),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [#{id => <<"c1">>, type => <<"function">>,
                                         function => #{name => <<"ask_human">>,
                                                       arguments => <<"{}">>}}]}};
            _ -> {ok, #{content => <<"answered">>, finish_reason => <<"stop">>}}
        end
    end),
    ConvId = <<"heal-conv-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Cfg = #{llm => {mock, #{}},
            conversation_id => ConvId,   %% 用持久默认 store（按 conv 隔离）
            interrupt_tools => [#{name => <<"ask_human">>, description => <<"a">>,
                                  parameters => #{type => object, properties => #{}}}]},
    try
        {ok, Agent} = beamai_agent:new(Cfg),
        %% 触发中断：历史里 assistant(ask_human) 无对应结果（悬空）
        {interrupt, _, A1} = beamai_agent:run(Agent, <<"go">>),
        ?assert(beamai_agent:is_interrupted(A1)),
        %% 放弃 resume，直接开新 chat → heal 补「已取消」结果 + 清中断态
        {ok, Result, A2} = beamai_agent:run(A1, <<"new question">>),
        ?assertEqual(<<"answered">>, maps:get(content, Result)),
        ?assertNot(beamai_agent:is_interrupted(A2)),
        %% 历史中 c1 已有 tool 结果（不再悬空）
        Msgs = beamai_agent:messages(A2),
        HasC1Result = lists:any(fun(#{role := tool, tool_call_id := <<"c1">>}) -> true;
                                   (_) -> false end, Msgs),
        ?assert(HasC1Result),
        ?assertEqual([], dangling(Msgs))
    after
        meck:unload(beamai_chat_completion)
    end.

%% 与 beamai_agent:dangling_tool_call_ids 同逻辑（测试侧独立实现）
dangling(History) ->
    CallIds = lists:flatmap(fun(#{role := assistant, tool_calls := TCs}) when is_list(TCs) ->
                                [maps:get(id, TC, maps:get(<<"id">>, TC, undefined)) || TC <- TCs];
                               (_) -> [] end, History),
    Answered = [Id || #{role := tool, tool_call_id := Id} <- History],
    [Id || Id <- CallIds, Id =/= undefined, not lists:member(Id, Answered)].
