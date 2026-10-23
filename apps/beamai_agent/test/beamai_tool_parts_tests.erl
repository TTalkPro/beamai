%%%-------------------------------------------------------------------
%%% @doc 多模态工具结果生命周期测试
%%%
%%% 图片只进入当前回合发给 provider 的 tool result；memory 持久化与
%%% return_direct 合成答案只保留文本部件。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_parts_tests).

-include_lib("eunit/include/eunit.hrl").

parts() ->
    [#{type => text, text => <<"screen attached">>},
     #{type => image,
       source => #{type => base64, media_type => <<"image/jpeg">>, data => <<"AAAA">>}}].

tc(Name) ->
    #{id => <<"c1">>, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

chat_client(Handler, ReturnDirect) ->
    K0 = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                           beamai_chat_model:create(mock, #{})),
    beamai_chat_client:add_tool(K0, #{name => <<"screenshot">>, parameters => #{},
                                      handler => Handler,
                                      return_direct => ReturnDirect}).

tool_parts_reach_provider_but_not_memory_test() ->
    Self = self(),
    meck:new(beamai_chat_model, [passthrough]),
    Count = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_C, Messages, _O) ->
        ok = counters:add(Count, 1, 1),
        case counters:get(Count, 1) of
            1 ->
                {ok, #{content => null, tool_calls => [tc(<<"screenshot">>)],
                       finish_reason => <<"tool_calls">>}};
            _ ->
                Self ! {sent, Messages},
                {ok, #{content => <<"final">>, finish_reason => <<"stop">>}}
        end
    end),
    Provider = beamai_agent_fake_memory:new(tool_parts_mem),
    try
        {ok, Agent} = beamai_agent:new(#{
            chat_client => chat_client(fun(_A) -> {ok, {parts, parts()}} end, false),
            memory => Provider, conversation_id => <<"conv">>}),
        {ok, _Resp, _Agent} = beamai_agent:run(Agent, <<"go">>),
        receive
            {sent, Sent} ->
                [ToolMsg] = [M || #{role := tool} = M <- Sent],
                ?assertEqual(parts(), maps:get(content, ToolMsg))
        after 100 ->
            ?assert(false)
        end,
        History = beamai_memory_provider:history(Provider, <<"conv">>),
        [StoredTool] = [M || #{role := tool} = M <- History],
        ?assertEqual(<<"screen attached">>, maps:get(content, StoredTool))
    after
        meck:unload(beamai_chat_model),
        catch ets:delete(tool_parts_mem)
    end.

return_direct_parts_use_text_only_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        {ok, #{content => null, tool_calls => [tc(<<"screenshot">>)],
               finish_reason => <<"tool_calls">>}}
    end),
    try
        {ok, Agent} = beamai_agent:new(#{
            chat_client => chat_client(fun(_A) -> {ok, {parts, parts()}} end, true)}),
        {ok, Resp, _Agent} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(<<"screen attached">>, maps:get(content, Resp)),
        ?assertEqual(1, meck:num_calls(beamai_chat_model, chat, 3))
    after
        meck:unload(beamai_chat_model)
    end.
