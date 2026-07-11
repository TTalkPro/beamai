%%%-------------------------------------------------------------------
%%% @doc around_turn 链测试（RAG 前置 / around 观测 / 递归重入 / 层序）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_turn_filter_tests).

-include_lib("eunit/include/eunit.hrl").

%% mock LLM：每次调用返回 <<"answer-N">>（N=调用序），可选把 messages 送回 Parent
mock_llm(Parent) ->
    CC = counters:new(1, []),
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, Messages, _O) ->
        counters:add(CC, 1, 1),
        N = counters:get(CC, 1),
        Parent ! {llm_call, N, Messages},
        {ok, #{content => <<"answer-", (integer_to_binary(N))/binary>>,
               finish_reason => <<"stop">>}}
    end),
    CC.

kernel_with_filters(Filters) ->
    K0 = beamai_kernel:add_service(beamai_kernel:new(),
                                   beamai_chat_completion:create(mock, #{})),
    lists:foldl(fun(F, K) -> beamai_kernel:add_filter(K, F) end, K0, Filters).

turn_filter(Name, Fun) ->
    beamai_filter:new(Name, #{around_turn => Fun}).

drain_llm_calls(Acc) ->
    receive {llm_call, N, Msgs} -> drain_llm_calls([{N, Msgs} | Acc])
    after 100 -> lists:reverse(Acc)
    end.

%% 清空邮箱里遗留的探针消息（隔离同进程内跨用例的残留）
flush() ->
    receive
        {llm_call, _, _} -> flush();
        {observed, _} -> flush();
        {enter, _} -> flush();
        {exit, _} -> flush()
    after 0 -> ok
    end.

%%====================================================================
%% RAG 式前置：turn filter 改写入口消息
%%====================================================================

rag_prepend_test() ->
    Parent = self(),
    flush(),
    _ = mock_llm(Parent),
    RAG = turn_filter(<<"rag">>, fun(Req, _F, Next) ->
        Msgs = maps:get(messages, Req),
        Next(Req#{messages => [#{role => system, content => <<"CONTEXT">>} | Msgs]})
    end),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => kernel_with_filters([RAG]),
                                         memory => false}),
        {ok, _Result, _} = beamai_agent:run(Agent, <<"go">>),
        [{1, Msgs}] = drain_llm_calls([]),
        %% LLM 收到的消息里有 RAG 前置的 system 消息
        ?assert(lists:any(fun(#{role := R, content := C}) ->
                              R =:= system andalso C =:= <<"CONTEXT">>;
                          (_) -> false
                      end, Msgs))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% around：观测最终结果（不改）
%%====================================================================

around_observe_test() ->
    Parent = self(),
    flush(),
    _ = mock_llm(Parent),
    Obs = turn_filter(<<"obs">>, fun(Req, _F, Next) ->
        R = Next(Req),
        Parent ! {observed, R},
        R
    end),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => kernel_with_filters([Obs]),
                                         memory => false}),
        {ok, Result, _} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(<<"answer-1">>, maps:get(content, Result)),
        receive {observed, Observed} ->
            ?assertMatch({ok, _Resp, _TCM, _Iter}, Observed)
        after 1000 -> ?assert(false) end
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 递归重入：turn filter 多次 Next（校验重试式）
%%====================================================================

reentry_runs_fresh_loop_test() ->
    Parent = self(),
    flush(),
    _ = mock_llm(Parent),
    Budget = counters:new(1, []),   %% 允许重入 1 次
    Retry = turn_filter(<<"retry">>, fun(Req, _F, Next) ->
        R1 = Next(Req),
        case counters:get(Budget, 1) of
            0 ->
                counters:add(Budget, 1, 1),
                %% 重入：全新循环，喂新 delta
                Next(Req#{messages => [#{role => user, content => <<"try again">>}]});
            _ ->
                R1
        end
    end),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => kernel_with_filters([Retry]),
                                         memory => false}),
        {ok, Result, _} = beamai_agent:run(Agent, <<"go">>),
        %% 两次 Next ⇒ 两轮完整循环 ⇒ LLM 调用两次；最终取第二轮结果
        ?assertEqual(2, length(drain_llm_calls([]))),
        ?assertEqual(<<"answer-2">>, maps:get(content, Result))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 注册顺序 = 层序（靠前者最外层：最先见 req、最后见 resp）
%%====================================================================

registration_order_is_layering_test() ->
    Parent = self(),
    flush(),
    _ = mock_llm(Parent),
    F = fun(Tag) ->
        turn_filter(Tag, fun(Req, _Ff, Next) ->
            Parent ! {enter, Tag},
            R = Next(Req),
            Parent ! {exit, Tag},
            R
        end)
    end,
    try
        {ok, Agent} = beamai_agent:new(#{
            kernel => kernel_with_filters([F(<<"outer">>), F(<<"inner">>)]),
            memory => false}),
        {ok, _, _} = beamai_agent:run(Agent, <<"go">>),
        Events = drain_events([]),
        %% outer 先 enter、后 exit（最外层）
        ?assertEqual([{enter, <<"outer">>}, {enter, <<"inner">>},
                      {exit, <<"inner">>}, {exit, <<"outer">>}], Events)
    after
        meck:unload(beamai_chat_completion)
    end.

drain_events(Acc) ->
    receive
        {enter, T} -> drain_events([{enter, T} | Acc]);
        {exit, T} -> drain_events([{exit, T} | Acc])
    after 100 -> lists:reverse(Acc)
    end.

%%====================================================================
%% resume 经 turn 链：暂停→resume→不合格答案→校验反馈重入→合格
%%====================================================================

resume_through_turn_chain_reentry_test() ->
    Parent = self(),
    flush(),
    _ = mock_llm2(fun(N) ->
        case N of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [#{id => <<"c1">>, type => <<"function">>,
                                         function => #{name => <<"ask_human">>,
                                                       arguments => <<"{}">>}}]}};
            2 -> {ok, #{content => <<"answer-1">>, finish_reason => <<"stop">>}}; %% 不合格
            _ -> {ok, #{content => <<"answer-good">>, finish_reason => <<"stop">>}}
        end
    end, Parent),
    %% 校验 turn filter：answer-1 不合格 → 反馈重入；interrupt/error 透传（硬规则）
    Validate = turn_filter(<<"validate">>, fun(Req, _F, Next) ->
        case Next(Req) of
            {ok, Resp, _, _} = R ->
                case maps:get(content, Resp, undefined) of
                    <<"answer-1">> ->
                        Next(Req#{messages => [#{role => user, content => <<"be better">>}],
                                  resume => false});
                    _ -> R
                end;
            Other -> Other   %% interrupt / error 透传，不得重入
        end
    end),
    K = beamai_kernel:add_filter(
          beamai_kernel:add_service(beamai_kernel:new(),
                                    beamai_chat_completion:create(mock, #{})),
          Validate),
    try
        {ok, Agent} = beamai_agent:new(#{
            kernel => K, memory => false,
            interrupt_tools => [#{name => <<"ask_human">>, description => <<"a">>,
                                  parameters => #{type => object, properties => #{}}}]}),
        %% run：ask_human 触发中断 → turn filter 透传（未重入）
        {interrupt, _Info, A1} = beamai_agent:run(Agent, <<"go">>),
        ?assert(beamai_agent:is_interrupted(A1)),
        %% resume：延续产出 answer-1（不合格）→ 校验重入 → answer-good
        {ok, Result, _} = beamai_agent:resume(A1, <<"reply">>, #{message => <<"hi">>}),
        ?assertEqual(<<"answer-good">>, maps:get(content, Result)),
        %% LLM 共 3 次：run(1) + resume 延续(1) + 重入(1)
        ?assertEqual(3, length(drain_llm_calls([])))
    after
        meck:unload(beamai_chat_completion)
    end.

%% mock LLM（按调用序 N 分派），把 messages 送回 Parent
mock_llm2(Fun, Parent) ->
    CC = counters:new(1, []),
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, Messages, _O) ->
        counters:add(CC, 1, 1),
        N = counters:get(CC, 1),
        Parent ! {llm_call, N, Messages},
        Fun(N)
    end),
    CC.

%%====================================================================
%% resume 标记：首次进入带 resume=true
%%====================================================================

resume_flag_probe_test() ->
    Parent = self(),
    flush(),
    _ = mock_llm2(fun(N) ->
        case N of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [#{id => <<"c1">>, type => <<"function">>,
                                         function => #{name => <<"ask_human">>,
                                                       arguments => <<"{}">>}}]}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end, Parent),
    Probe = turn_filter(<<"probe">>, fun(Req, _F, Next) ->
        Parent ! {resume_flag, maps:get(resume, Req, undefined)},
        Next(Req)
    end),
    K = beamai_kernel:add_filter(
          beamai_kernel:add_service(beamai_kernel:new(),
                                    beamai_chat_completion:create(mock, #{})),
          Probe),
    try
        {ok, Agent} = beamai_agent:new(#{
            kernel => K, memory => false,
            interrupt_tools => [#{name => <<"ask_human">>, description => <<"a">>,
                                  parameters => #{type => object, properties => #{}}}]}),
        {interrupt, _, A1} = beamai_agent:run(Agent, <<"go">>),
        {ok, _, _} = beamai_agent:resume(A1, <<"reply">>, #{message => <<"x">>}),
        %% run 进入 resume=false，resume 进入 resume=true
        Flags = drain_resume_flags([]),
        ?assertEqual([false, true], Flags)
    after
        meck:unload(beamai_chat_completion)
    end.

drain_resume_flags(Acc) ->
    receive {resume_flag, F} -> drain_resume_flags([F | Acc])
    after 100 -> lists:reverse(Acc)
    end.

%%====================================================================
%% 内置 validation_turn_filter：不合格反馈重入直到合格
%%====================================================================

validation_turn_filter_reentry_test() ->
    Parent = self(),
    flush(),
    _ = mock_llm2(fun(N) ->
        C = case N of 1 -> <<"bad">>; _ -> <<"good">> end,
        {ok, #{content => C, finish_reason => <<"stop">>}}
    end, Parent),
    Validate = beamai_filters:validation_turn_filter(
        fun(#{content := <<"good">>}) -> ok;
           (_) -> {invalid, <<"必须是 good">>}
        end, 2),
    K = beamai_kernel:add_filter(
          beamai_kernel:add_service(beamai_kernel:new(),
                                    beamai_chat_completion:create(mock, #{})),
          Validate),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K, memory => false}),
        {ok, Result, _} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(<<"good">>, maps:get(content, Result)),
        %% 首答 bad → 重入一次 → good：LLM 两次
        ?assertEqual(2, length(drain_llm_calls([])))
    after
        meck:unload(beamai_chat_completion)
    end.

validation_turn_filter_exhausts_test() ->
    Parent = self(),
    flush(),
    _ = mock_llm2(fun(_N) -> {ok, #{content => <<"bad">>, finish_reason => <<"stop">>}} end,
                  Parent),
    Validate = beamai_filters:validation_turn_filter(
        fun(#{content := <<"good">>}) -> ok; (_) -> {invalid, <<"nope">>} end, 1),
    K = beamai_kernel:add_filter(
          beamai_kernel:add_service(beamai_kernel:new(),
                                    beamai_chat_completion:create(mock, #{})),
          Validate),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K, memory => false}),
        {ok, Result, _} = beamai_agent:run(Agent, <<"go">>),
        %% 耗尽（1 次重试）后原样返回 bad；LLM 共 2 次（初 + 1 重试）
        ?assertEqual(<<"bad">>, maps:get(content, Result)),
        ?assertEqual(2, length(drain_llm_calls([])))
    after
        meck:unload(beamai_chat_completion)
    end.
