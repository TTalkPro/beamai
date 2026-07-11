%%%-------------------------------------------------------------------
%%% @doc 暂停持久化（PauseStore ETS）+ resume payload 测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_pause_persistence_tests).

-include_lib("eunit/include/eunit.hrl").

tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

tc_args(Id, Name, Args) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => Args}}.

mock_llm(Fun) ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) -> Fun() end).

kernel_with(Tools) ->
    K0 = beamai_kernel:add_service(beamai_kernel:new(),
                                   beamai_chat_completion:create(mock, #{})),
    lists:foldl(fun({Name, H}, K) ->
        beamai_kernel:add_tool(K, #{name => Name, parameters => #{}, handler => H})
    end, K0, Tools).

%%====================================================================
%% PauseStore ETS 单元
%%====================================================================

pause_store_ets_crud_test() ->
    {ok, Pid} = beamai_pause_store_ets:start_link(test_pause_store_1),
    try
        H = beamai_pause_store_ets:handle(test_pause_store_1),
        ?assertEqual(none, beamai_pause_store:pause_load(H, <<"c1">>)),
        Snap = #{version => 1, conversation_id => <<"c1">>, interrupt_state => #{a => 1}},
        ok = beamai_pause_store:pause_save(H, <<"c1">>, Snap),
        ?assertEqual({ok, Snap}, beamai_pause_store:pause_load(H, <<"c1">>)),
        %% 覆盖
        Snap2 = Snap#{interrupt_state => #{a => 2}},
        ok = beamai_pause_store:pause_save(H, <<"c1">>, Snap2),
        ?assertEqual({ok, Snap2}, beamai_pause_store:pause_load(H, <<"c1">>)),
        %% 清除
        ok = beamai_pause_store:pause_clear(H, <<"c1">>),
        ?assertEqual(none, beamai_pause_store:pause_load(H, <<"c1">>))
    after
        gen_server:stop(Pid)
    end.

%%====================================================================
%% 跨"重启"：agent1 暂停落库 → agent2（同 conv + 同 store）透明恢复
%%====================================================================

cross_restart_resume_test() ->
    {ok, Pid} = beamai_pause_store_ets:start_link(test_pause_store_2),
    Store = beamai_pause_store_ets:handle(test_pause_store_2),
    CC = counters:new(1, []),
    mock_llm(fun() ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [tc(<<"c1">>, <<"ask_human">>)]}};
            _ -> {ok, #{content => <<"resumed-done">>, finish_reason => <<"stop">>}}
        end
    end),
    Cfg = #{llm => {mock, #{}},
            memory => false,
            conversation_id => <<"order-42">>,
            pause_store => Store,
            interrupt_tools => [#{name => <<"ask_human">>, description => <<"ask">>,
                                  parameters => #{type => object, properties => #{}}}]},
    try
        {ok, Agent1} = beamai_agent:new(Cfg),
        {interrupt, _Info, _A1} = beamai_agent:run(Agent1, <<"go">>),
        %% 模拟重启：全新 agent 实例（interrupt_state=undefined），同 conv + 同 store
        {ok, Agent2} = beamai_agent:new(Cfg),
        ?assert(beamai_agent:is_interrupted(Agent2)),   %% 经 store 发现未决暂停
        {ok, Result, Agent3} = beamai_agent:resume(Agent2, <<"reply">>,
                                                   #{message => <<"人类答复">>}),
        ?assertEqual(<<"resumed-done">>, maps:get(content, Result)),
        ?assertNot(beamai_agent:is_interrupted(Agent3))  %% 快照已清
    after
        meck:unload(beamai_chat_completion),
        gen_server:stop(Pid)
    end.

%%====================================================================
%% resume payload 三形态
%%====================================================================

%% 被中断工具的结果落在"下一轮 LLM 收到的 messages"里（非 tool_calls_made）。
%% mock 在 call2 捕获收到的 messages，从中取 tool_call_id 的结果内容。
mock_capture(Parent) ->
    CC = counters:new(1, []),
    mock_llm2(fun(Messages) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [tc(<<"c1">>, <<"ask_human">>)]}};
            _ -> Parent ! {llm_msgs, Messages},
                 {ok, #{content => <<"ok">>, finish_reason => <<"stop">>}}
        end
    end).

mock_llm2(Fun) ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, Messages, _O) -> Fun(Messages) end).

tool_result_in_msgs(Id) ->
    receive {llm_msgs, Messages} ->
        case [C || #{role := tool, tool_call_id := I, content := C} <- Messages, I =:= Id] of
            [C | _] -> C;
            [] -> undefined
        end
    after 2000 -> timeout end.

%% reply：答复即结果（ask-user）
resume_reply_test() ->
    Parent = self(),
    mock_capture(Parent),
    try
        {ok, Agent} = beamai_agent:new(#{
            llm => {mock, #{}}, memory => false,
            interrupt_tools => [#{name => <<"ask_human">>, description => <<"a">>,
                                  parameters => #{type => object, properties => #{}}}]}),
        {interrupt, _, A1} = beamai_agent:run(Agent, <<"go">>),
        {ok, Result, _} = beamai_agent:resume(A1, <<"reply">>, #{message => <<"选 B">>}),
        ?assertEqual(<<"ok">>, maps:get(content, Result)),
        ?assertEqual(<<"选 B">>, tool_result_in_msgs(<<"c1">>))
    after
        meck:unload(beamai_chat_completion)
    end.

%% approved + args：编辑后批准，工具以替换参数执行
resume_approved_with_args_test() ->
    Parent = self(),
    CC = counters:new(1, []),
    mock_llm2(fun(Messages) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [tc_args(<<"c1">>, <<"echo_id">>, <<"{\"id\":1}">>)]}};
            _ -> Parent ! {llm_msgs, Messages},
                 {ok, #{content => <<"ok">>, finish_reason => <<"stop">>}}
        end
    end),
    EchoId = fun(Args, _) -> {ok, integer_to_binary(maps:get(<<"id">>, Args, 0))} end,
    Gate = fun(Name, _Args) ->
        case Name of <<"echo_id">> -> {interrupt, <<"approve?">>}; _ -> ok end
    end,
    try
        {ok, Agent} = beamai_agent:new(#{
            kernel => kernel_with([{<<"echo_id">>, EchoId}]),
            memory => false,
            callbacks => #{on_tool_call => Gate}}),
        {interrupt, _, A1} = beamai_agent:run(Agent, <<"go">>),
        %% 批准并把 id 改成 2 → 工具以 id=2 执行
        {ok, Result, _} = beamai_agent:resume(A1, <<"approved">>,
                                              #{args => #{<<"id">> => 2}}),
        ?assertEqual(<<"ok">>, maps:get(content, Result)),
        ?assertEqual(<<"2">>, tool_result_in_msgs(<<"c1">>))
    after
        meck:unload(beamai_chat_completion)
    end.

%% 拒绝 + 理由：结果「已拒绝执行：<理由>」
resume_reject_with_reason_test() ->
    Parent = self(),
    mock_capture(Parent),
    try
        {ok, Agent} = beamai_agent:new(#{
            llm => {mock, #{}}, memory => false,
            interrupt_tools => [#{name => <<"ask_human">>, description => <<"a">>,
                                  parameters => #{type => object, properties => #{}}}]}),
        {interrupt, _, A1} = beamai_agent:run(Agent, <<"go">>),
        {ok, Result, _} = beamai_agent:resume(A1, <<"rejected">>,
                                              #{message => <<"有未结算金额">>}),
        ?assertEqual(<<"ok">>, maps:get(content, Result)),
        Content = tool_result_in_msgs(<<"c1">>),
        ?assert(is_binary(Content)),
        %% 结果为「已拒绝执行：<理由>」——断言含理由（避免跨文件中文前缀字节比对）
        ?assertNotEqual(nomatch, binary:match(Content, <<"有未结算金额">>))
    after
        meck:unload(beamai_chat_completion)
    end.

