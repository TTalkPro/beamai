%%%-------------------------------------------------------------------
%%% @doc beamai_agent_delegate 测试（委派工具：父委派 → 子跑 → 结论回流父）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_delegate_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 工具 spec 形状
%%====================================================================

tool_spec_shape_test() ->
    T = beamai_agent_delegate:tool(#{
        name => <<"d">>,
        subagent => fun(_Args, _Ctx) -> #{llm => {mock, #{}}} end
    }),
    ?assertEqual(<<"d">>, maps:get(name, T)),
    ?assert(is_function(maps:get(handler, T), 2)),
    Params = maps:get(parameters, T),
    ?assertMatch(#{task := #{required := true}}, Params),
    ?assert(maps:is_key(context, Params)).

%%====================================================================
%% 端到端：父 LLM 委派 → 子 agent 跑 → 子结论作为 tool 结果回流父记忆
%%====================================================================

delegate_e2e_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, Messages, _O) ->
        HasTool = lists:any(fun(M) -> maps:get(role, M, undefined) =:= tool end, Messages),
        LastUser = last_user(Messages),
        if
            HasTool ->
                %% 父最终轮：用工具结果（子结论）作答
                {ok, #{content => <<"FINAL:", (tool_result(Messages))/binary>>,
                       finish_reason => <<"stop">>}};
            true ->
                case binary:match(LastUser, <<"SUBTASK">>) of
                    nomatch ->
                        %% 父首轮：委派（context 由“父 LLM”填写）
                        {ok, #{content => null,
                               tool_calls => [#{id => <<"d1">>, type => <<"function">>,
                                   function => #{name => <<"delegate">>,
                                       arguments => <<"{\"task\":\"SUBTASK do-it\","
                                                      "\"context\":\"BG-from-llm\"}">>}}],
                               finish_reason => <<"tool_calls">>}};
                    _ ->
                        %% 子轮：回显是否收到 context(BG-from-llm) 与 seed(SEED-slice)，证明都传入了
                        Bg   = seen(LastUser, <<"BG-from-llm">>),
                        Seed = seen(LastUser, <<"SEED-slice">>),
                        {ok, #{content => <<"ANSWER(bg=", Bg/binary, ",seed=", Seed/binary, ")">>,
                               finish_reason => <<"stop">>}}
                end
        end
    end),

    Delegate = beamai_agent_delegate:tool(#{
        name => <<"delegate">>,
        subagent => fun(_Args, _Ctx) -> #{llm => {mock, #{}}, system_prompt => <<"sub">>} end,
        seed => fun(_Args, _Ctx) -> <<"SEED-slice">> end   %% 程序化注入的“父记忆片段”
    }),

    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_service(K0, beamai_chat_completion:create(mock, #{})),
    K2 = beamai_kernel:add_tools(K1, [Delegate]),
    try
        {ok, Parent} = beamai_agent:new(#{kernel => K2, conversation_id => <<"deleg-parent">>}),
        {ok, Res, Parent1} = beamai_agent:run(Parent, <<"please research">>),

        %% 父最终回复 = 用了子结论；且 context 与 seed 都成功传入子 agent
        ?assertEqual(<<"FINAL:ANSWER(bg=yes,seed=yes)">>, maps:get(content, Res)),
        %% 委派被记为一次工具调用
        ?assertEqual(1, length(maps:get(tool_calls_made, Res, []))),
        %% 子结论作为 role=tool 消息回流进父记忆（子内部步骤不在父里）
        ParentMsgs = beamai_agent:messages(Parent1),
        ?assert(lists:any(
            fun(M) ->
                maps:get(role, M, undefined) =:= tool andalso
                binary:match(maps:get(content, M, <<>>), <<"ANSWER">>) =/= nomatch
            end, ParentMsgs))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 异步管理工具：spawn → list → result（按 conversation 归属）
%%====================================================================

management_tools_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, Messages, _O) ->
        {ok, #{content => <<"R:", (last_user(Messages))/binary>>, finish_reason => <<"stop">>}}
    end),
    Tools = beamai_agent_delegate:management_tools(#{
        subagent => fun(_A, _C) -> #{llm => {mock, #{}}, memory => false} end
    }),
    Owner = beamai_id:gen_id(<<"owner">>),
    Ctx = beamai_context:with_conversation_id(beamai_context:new(), Owner),
    Spawn  = handler(<<"spawn_subagent">>, Tools),
    List   = handler(<<"list_subagents">>, Tools),
    Result = handler(<<"subagent_result">>, Tools),
    try
        %% spawn → 拿 id
        {ok, J1} = Spawn(#{<<"task">> => <<"hi">>}, Ctx),
        #{<<"id">> := Id, <<"status">> := <<"running">>} = jsx:decode(J1, [return_maps]),
        %% list（本 owner）含 1 个
        {ok, J2} = List(#{}, Ctx),
        #{<<"agents">> := Agents} = jsx:decode(J2, [return_maps]),
        ?assertEqual(1, length(Agents)),
        %% 等完成后 result = done
        {ok, _} = beamai_subagent_manager:await(Id, 2000),
        {ok, J3} = Result(#{<<"id">> => Id}, Ctx),
        #{<<"status">> := <<"done">>, <<"result">> := <<"R:hi">>} = jsx:decode(J3, [return_maps]),
        beamai_subagent_manager:drop(Id)
    after
        meck:unload(beamai_chat_completion)
    end.

handler(Name, Tools) ->
    [Tool] = [T || T <- Tools, maps:get(name, T) =:= Name],
    maps:get(handler, Tool).

%%====================================================================
%% fan-out：一次调用并发起 N 个子 agent（总耗时 ≪ 串行之和）
%%====================================================================

fanout_concurrent_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    %% 每个子 agent 的 LLM 调用 sleep 150ms 再返回；3 个并发 → ~150ms，串行则 ~450ms
    meck:expect(beamai_chat_completion, chat, fun(_C, Messages, _O) ->
        timer:sleep(150),
        {ok, #{content => <<"ans:", (last_user(Messages))/binary>>, finish_reason => <<"stop">>}}
    end),
    SubFn = fun(_Args, _Ctx) -> #{llm => {mock, #{}}} end,
    Jobs = [{#{llm => {mock, #{}}, memory => false, conversation_id => <<"s1">>}, <<"t1">>},
            {#{llm => {mock, #{}}, memory => false, conversation_id => <<"s2">>}, <<"t2">>},
            {#{llm => {mock, #{}}, memory => false, conversation_id => <<"s3">>}, <<"t3">>}],
    _ = SubFn,
    try
        {Micros, Results} = timer:tc(fun() -> beamai_agent_delegate:run_many(Jobs, 5000) end),
        %% 三个子 agent 各自结果
        ?assertEqual([{ok, <<"ans:t1">>}, {ok, <<"ans:t2">>}, {ok, <<"ans:t3">>}], Results),
        %% 并发：远小于串行 3*150=450ms（留余量，断言 < 350ms）
        ?assert(Micros < 350000)
    after
        meck:unload(beamai_chat_completion)
    end.

%% fanout_tool：LLM 一次调用传 tasks 列表 → 并发子 agent → 汇总结果
fanout_tool_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, Messages, _O) ->
        case is_sub(Messages) of
            true ->
                {ok, #{content => <<"R:", (last_user(Messages))/binary>>, finish_reason => <<"stop">>}};
            false ->
                case lists:any(fun(M) -> maps:get(role, M, undefined) =:= tool end, Messages) of
                    true -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}};
                    false -> {ok, #{content => null,
                        tool_calls => [#{id => <<"f1">>, type => <<"function">>,
                            function => #{name => <<"fanout">>,
                                arguments => <<"{\"tasks\":[\"SUBTASK a\",\"SUBTASK b\"]}">>}}],
                        finish_reason => <<"tool_calls">>}}
                end
        end
    end),
    Fanout = beamai_agent_delegate:fanout_tool(#{
        name => <<"fanout">>,
        subagent => fun(_Args, _Ctx) -> #{llm => {mock, #{}}} end
    }),
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_service(K0, beamai_chat_completion:create(mock, #{})),
    K2 = beamai_kernel:add_tools(K1, [Fanout]),
    try
        {ok, Parent} = beamai_agent:new(#{kernel => K2,
                                          conversation_id => beamai_id:gen_id(<<"p">>)}),
        {ok, _Res, Parent1} = beamai_agent:run(Parent, <<"do both">>),
        %% 汇总结果(含两个子任务的回答)作为 tool 结果回流父记忆
        ToolMsgs = [maps:get(content, M, <<>>) || M <- beamai_agent:messages(Parent1),
                    maps:get(role, M, undefined) =:= tool],
        Combined = iolist_to_binary(ToolMsgs),
        ?assert(binary:match(Combined, <<"SUBTASK a">>) =/= nomatch),
        ?assert(binary:match(Combined, <<"SUBTASK b">>) =/= nomatch)
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 进程隔离：超时 / 崩溃都只回 error 工具结果，父进程存活
%%====================================================================

%% 子 agent 超时 → {error, sub_agent_timeout}（父不被拖死）
delegate_timeout_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, Messages, _O) ->
        case is_sub(Messages) of
            true  -> timer:sleep(500),   %% 子 agent 故意拖时
                     {ok, #{content => <<"late">>, finish_reason => <<"stop">>}};
            false -> parent_or_final(Messages)
        end
    end),
    Delegate = beamai_agent_delegate:tool(#{
        name => <<"delegate">>,
        subagent => fun(_A, _C) -> #{llm => {mock, #{}}} end,
        timeout => 80   %% 远小于 500ms
    }),
    run_parent_and_assert_error(Delegate, <<"sub_agent_timeout">>).

%% 子 agent 崩溃（error 类）→ {error, {sub_agent_crashed,_}}（父进程存活）
delegate_crash_isolation_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, Messages, _O) ->
        case is_sub(Messages) of
            true  -> error(boom);        %% 子 agent 在独立进程里崩
            false -> parent_or_final(Messages)
        end
    end),
    Delegate = beamai_agent_delegate:tool(#{
        name => <<"delegate">>,
        subagent => fun(_A, _C) -> #{llm => {mock, #{}}} end
    }),
    run_parent_and_assert_error(Delegate, <<"sub_agent_crashed">>).

%% @private 跑父 agent；断言父存活、委派工具结果里带预期错误标记
run_parent_and_assert_error(Delegate, ErrMarker) ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_service(K0, beamai_chat_completion:create(mock, #{})),
    K2 = beamai_kernel:add_tools(K1, [Delegate]),
    try
        {ok, Parent} = beamai_agent:new(#{kernel => K2,
                                          conversation_id => beamai_id:gen_id(<<"p">>)}),
        %% 关键：父 run 正常返回（进程没被子 agent 拖死/炸死）
        {ok, _Res, Parent1} = beamai_agent:run(Parent, <<"please research">>),
        %% 委派的 error 作为 tool 结果回流父记忆
        ToolMsgs = [maps:get(content, M, <<>>) || M <- beamai_agent:messages(Parent1),
                    maps:get(role, M, undefined) =:= tool],
        ?assert(lists:any(fun(C) -> binary:match(C, ErrMarker) =/= nomatch end, ToolMsgs))
    after
        meck:unload(beamai_chat_completion)
    end.

is_sub(Messages) ->
    binary:match(last_user(Messages), <<"SUBTASK">>) =/= nomatch.

%% 父首轮委派；父最终轮（已有 tool 结果）作答
parent_or_final(Messages) ->
    case lists:any(fun(M) -> maps:get(role, M, undefined) =:= tool end, Messages) of
        true  -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}};
        false -> {ok, #{content => null,
                        tool_calls => [#{id => <<"d1">>, type => <<"function">>,
                            function => #{name => <<"delegate">>,
                                arguments => <<"{\"task\":\"SUBTASK go\"}">>}}],
                        finish_reason => <<"tool_calls">>}}
    end.

%%====================================================================
%% 辅助
%%====================================================================

last_user(Messages) ->
    Cs = [maps:get(content, M, <<>>) || M <- Messages,
          maps:get(role, M, undefined) =:= user,
          is_binary(maps:get(content, M, <<>>))],
    case Cs of [] -> <<>>; _ -> lists:last(Cs) end.

tool_result(Messages) ->
    case [maps:get(content, M, <<>>) || M <- Messages,
          maps:get(role, M, undefined) =:= tool] of
        [] -> <<>>;
        L -> lists:last(L)
    end.

seen(Hay, Needle) ->
    case binary:match(Hay, Needle) of nomatch -> <<"no">>; _ -> <<"yes">> end.
