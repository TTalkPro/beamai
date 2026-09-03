%%%-------------------------------------------------------------------
%%% @doc beamai_agent 中断/恢复 单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_interrupt_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试: beamai_agent_interrupt 模块
%%====================================================================

find_interrupt_tool_test() ->
    Agent = #{interrupt_tools => [#{name => <<"ask_human">>}]},
    TC1 = #{id => <<"c1">>, type => <<"function">>,
            function => #{name => <<"normal_tool">>, arguments => <<"{}">>}},
    TC2 = #{id => <<"c2">>, type => <<"function">>,
            function => #{name => <<"ask_human">>, arguments => <<"{\"q\":\"ok?\"}">>}},
    %% 找到中断 tool
    {yes, Found, Others} = beamai_agent_interrupt:find_interrupt_tool([TC1, TC2], Agent),
    ?assertEqual(TC2, Found),
    ?assertEqual([TC1], Others).

%% 统一响应的扁平 tool_call 格式（#{id, name, arguments}，真实 provider 经
%% beamai_chat_response 解析后的形状）也必须能匹配中断工具
%% （回归：曾只认 OpenAI 嵌套 function.name，扁平格式静默不匹配并刷告警）
find_interrupt_tool_flat_format_test() ->
    Agent = #{interrupt_tools => [#{name => <<"ask_human">>}]},
    TC1 = #{id => <<"c1">>, name => <<"normal_tool">>,
            arguments => #{}, raw_arguments => <<"{}">>},
    TC2 = #{id => <<"c2">>, name => <<"ask_human">>,
            arguments => #{<<"q">> => <<"ok?">>}, raw_arguments => <<"{\"q\":\"ok?\"}">>},
    {yes, Found, Others} = beamai_agent_interrupt:find_interrupt_tool([TC1, TC2], Agent),
    ?assertEqual(TC2, Found),
    ?assertEqual([TC1], Others).

find_interrupt_tool_no_match_test() ->
    Agent = #{interrupt_tools => [#{name => <<"ask_human">>}]},
    TC1 = #{id => <<"c1">>, type => <<"function">>,
            function => #{name => <<"normal_tool">>, arguments => <<"{}">>}},
    ?assertEqual(no, beamai_agent_interrupt:find_interrupt_tool([TC1], Agent)).

find_interrupt_tool_empty_config_test() ->
    Agent = #{interrupt_tools => []},
    TC1 = #{id => <<"c1">>, type => <<"function">>,
            function => #{name => <<"ask_human">>, arguments => <<"{}">>}},
    ?assertEqual(no, beamai_agent_interrupt:find_interrupt_tool([TC1], Agent)).

find_interrupt_tool_no_config_test() ->
    Agent = #{},
    TC1 = #{id => <<"c1">>, type => <<"function">>,
            function => #{name => <<"ask_human">>, arguments => <<"{}">>}},
    ?assertEqual(no, beamai_agent_interrupt:find_interrupt_tool([TC1], Agent)).

is_interrupt_tool_test() ->
    Tools = [#{name => <<"ask_human">>}, #{name => <<"confirm">>}],
    TC1 = #{function => #{name => <<"ask_human">>}},
    TC2 = #{function => #{name => <<"other">>}},
    ?assert(beamai_agent_interrupt:is_interrupt_tool(TC1, Tools)),
    ?assertNot(beamai_agent_interrupt:is_interrupt_tool(TC2, Tools)).

handle_interrupt_test() ->
    Context = #{
        completed_tool_results => [],
        interrupted_tool_call => #{id => <<"c1">>, function => #{name => <<"ask">>}},
        iteration => 2,
        tool_calls_made => []
    },
    Agent = #{interrupt_state => undefined},
    {IntState, UpdatedAgent} = beamai_agent_interrupt:handle_interrupt(
        tool_request, #{question => <<"ok?">>}, Context, Agent),
    ?assertEqual(interrupted, maps:get(status, IntState)),
    ?assertEqual(tool_request, maps:get(interrupt_type, IntState)),
    ?assertEqual(#{question => <<"ok?">>}, maps:get(reason, IntState)),
    ?assertNotEqual(undefined, maps:get(interrupt_state, UpdatedAgent)).

build_resume_messages_test() ->
    IntState = #{
        interrupted_tool_call => #{id => <<"c1">>, function => #{name => <<"ask">>}}
    },
    Msgs = beamai_agent_interrupt:build_resume_messages(IntState, <<"yes, approved">>),
    %% Agent 自管编排：只返回[人类输入作为被中断 tool_call 的结果]，
    %% 已累积的完整 messages 由 interrupt_state.messages 携带、由调用方拼接。
    ?assertEqual(1, length(Msgs)),
    [Msg] = Msgs,
    ?assertEqual(tool, maps:get(role, Msg)),
    ?assertEqual(<<"c1">>, maps:get(tool_call_id, Msg)),
    ?assertEqual(<<"yes, approved">>, maps:get(content, Msg)).

validate_resume_input_test() ->
    IntState = #{status => interrupted},
    ?assertEqual(ok, beamai_agent_interrupt:validate_resume_input(IntState, <<"input">>)),
    ?assertEqual({error, empty_input},
                 beamai_agent_interrupt:validate_resume_input(IntState, <<>>)),
    ?assertEqual({error, empty_input},
                 beamai_agent_interrupt:validate_resume_input(IntState, undefined)),
    ?assertEqual({error, not_interrupted},
                 beamai_agent_interrupt:validate_resume_input(undefined, <<"x">>)).

get_interrupt_tool_specs_test() ->
    Agent = #{interrupt_tools => [#{
        name => <<"ask_human">>,
        description => <<"Ask human">>,
        parameters => #{type => object, properties => #{q => #{type => string}}}
    }]},
    [Spec] = beamai_agent_interrupt:get_interrupt_tool_specs(Agent),
    ?assertEqual(function, maps:get(type, Spec)),
    Func = maps:get(function, Spec),
    ?assertEqual(<<"ask_human">>, maps:get(name, Func)),
    ?assertEqual(<<"Ask human">>, maps:get(description, Func)).

get_interrupt_tool_specs_empty_test() ->
    ?assertEqual([], beamai_agent_interrupt:get_interrupt_tool_specs(#{interrupt_tools => []})),
    ?assertEqual([], beamai_agent_interrupt:get_interrupt_tool_specs(#{})).

%%====================================================================
%% 测试: Agent 中断查询 API
%%====================================================================

is_interrupted_test() ->
    ?assertNot(beamai_agent:is_interrupted(#{interrupt_state => undefined})),
    ?assert(beamai_agent:is_interrupted(#{interrupt_state => #{status => interrupted}})),
    ?assertNot(beamai_agent:is_interrupted(#{})).

get_interrupt_info_test() ->
    ?assertEqual(undefined, beamai_agent:get_interrupt_info(#{interrupt_state => undefined})),
    IntState = #{
        reason => #{question => <<"ok?">>},
        interrupt_type => tool_request,
        interrupted_tool_call => #{id => <<"c1">>},
        completed_tool_results => [],
        created_at => 12345
    },
    Info = beamai_agent:get_interrupt_info(#{interrupt_state => IntState}),
    ?assertEqual(#{question => <<"ok?">>}, maps:get(reason, Info)),
    ?assertEqual(tool_request, maps:get(interrupt_type, Info)),
    ?assertEqual(12345, maps:get(created_at, Info)).

%%====================================================================
%% 测试: Agent State 新字段初始化
%%====================================================================

state_interrupt_fields_init_test() ->
    {ok, State} = beamai_agent_state:create(#{llm => {mock, #{}}}),
    ?assertEqual(undefined, maps:get(interrupt_state, State)),
    ?assertEqual(undefined, maps:get(run_id, State)),
    ?assertEqual([], maps:get(interrupt_tools, State)).

state_interrupt_tools_from_config_test() ->
    InterruptTools = [#{name => <<"ask">>, description => <<"Ask">>}],
    {ok, State} = beamai_agent_state:create(#{
        llm => {mock, #{}},
        interrupt_tools => InterruptTools
    }),
    ?assertEqual(InterruptTools, maps:get(interrupt_tools, State)).

%%====================================================================
%% 测试: Interrupt Tool 触发中断（集成测试）
%%====================================================================

interrupt_tool_triggers_interrupt_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_Config, _Messages, _Opts) ->
        {ok, #{
            content => null,
            tool_calls => [#{
                id => <<"call_ask">>,
                type => <<"function">>,
                function => #{
                    name => <<"ask_human">>,
                    arguments => <<"{\"question\":\"Delete these files?\"}">>
                }
            }],
            finish_reason => <<"tool_calls">>
        }}
    end),
    try
        {ok, Agent} = beamai_agent:new(#{
            llm => {mock, #{}},
            interrupt_tools => [#{
                name => <<"ask_human">>,
                description => <<"Ask human">>,
                parameters => #{type => object, properties => #{
                    question => #{type => string}
                }}
            }]
        }),
        Result = beamai_agent:run(Agent, <<"Please delete temp files">>),
        ?assertMatch({interrupt, _, _}, Result),
        {interrupt, Info, Agent1} = Result,
        ?assertEqual(tool_request, maps:get(interrupt_type, Info)),
        ?assert(beamai_agent:is_interrupted(Agent1))
    after
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% 测试: Callback 触发中断
%%====================================================================

callback_triggers_interrupt_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_Config, _Messages, _Opts) ->
        {ok, #{
            content => null,
            tool_calls => [#{
                id => <<"call_sql">>,
                type => <<"function">>,
                function => #{
                    name => <<"execute_sql">>,
                    arguments => <<"{\"sql\":\"DELETE FROM users\"}">>
                }
            }],
            finish_reason => <<"tool_calls">>
        }}
    end),
    ChatClient0 = beamai_chat_client:new(),
    LlmConfig = beamai_chat_model:create(mock, #{}),
    K1 = beamai_chat_client:add_chat_model(ChatClient0, LlmConfig),
    K2 = beamai_chat_client:add_tools(K1, [
        #{name => <<"execute_sql">>,
          description => <<"Execute SQL">>,
          parameters => #{},
          handler => fun(_Args, _Ctx) -> {ok, <<"done">>} end}
    ]),
    try
        Callbacks = #{
            on_tool_call => fun(Name, Args) ->
                case Name of
                    <<"execute_sql">> ->
                        %% parse_tool_call 使用 attempt_atom，键可能是 atom 或 binary
                        SQL = case maps:get(sql, Args, undefined) of
                            undefined -> maps:get(<<"sql">>, Args, <<>>);
                            V -> V
                        end,
                        case binary:match(SQL, <<"DELETE">>) of
                            nomatch -> ok;
                            _ -> {interrupt, #{reason => write_sql, sql => SQL}}
                        end;
                    _ -> ok
                end
            end
        },
        {ok, Agent} = beamai_agent:new(#{chat_client => K2, callbacks => Callbacks}),
        Result = beamai_agent:run(Agent, <<"Delete all users">>),
        ?assertMatch({interrupt, _, _}, Result),
        {interrupt, Info, _Agent1} = Result,
        ?assertEqual(callback, maps:get(interrupt_type, Info))
    after
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% 测试: Resume 基本功能
%%====================================================================

resume_not_interrupted_test() ->
    {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}}}),
    ?assertEqual({error, not_interrupted}, beamai_agent:resume(Agent, <<"input">>)).

resume_after_interrupt_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_Config, _Messages, _Opts) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 ->
                %% 第一次调用：返回 interrupt tool
                {ok, #{
                    content => null,
                    tool_calls => [#{
                        id => <<"call_ask">>,
                        type => <<"function">>,
                        function => #{
                            name => <<"ask_human">>,
                            arguments => <<"{\"question\":\"Proceed?\"}">>
                        }
                    }],
                    finish_reason => <<"tool_calls">>
                }};
            _ ->
                %% resume 后 LLM 返回最终响应
                {ok, #{content => <<"Done! Files deleted.">>, finish_reason => <<"stop">>}}
        end
    end),
    try
        {ok, Agent} = beamai_agent:new(#{
            llm => {mock, #{}},
            interrupt_tools => [#{
                name => <<"ask_human">>,
                description => <<"Ask">>,
                parameters => #{type => object, properties => #{}}
            }]
        }),
        %% 第一次 run 触发中断
        {interrupt, _Info, Agent1} = beamai_agent:run(Agent, <<"Delete files">>),
        ?assert(beamai_agent:is_interrupted(Agent1)),
        %% Resume
        {ok, Result, Agent2} = beamai_agent:resume(Agent1, <<"Yes, go ahead">>),
        ?assertEqual(<<"Done! Files deleted.">>, maps:get(content, Result)),
        ?assertNot(beamai_agent:is_interrupted(Agent2)),
        ?assertEqual(1, beamai_agent:turn_count(Agent2))
    after
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% 测试: 流式恢复（stream_resume）
%%====================================================================

%% @private 首轮 chat 返回中断 tool，之后返回最终答案；stream_chat 逐 token 吐
mock_interrupt_then_answer() ->
    CallCount = counters:new(1, []),
    meck:new(beamai_chat_model, [passthrough]),
    AskTool = #{id => <<"call_ask">>, type => <<"function">>,
                function => #{name => <<"ask_human">>, arguments => <<"{}">>}},
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 -> {ok, #{content => null, tool_calls => [AskTool],
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"Done">>, finish_reason => <<"stop">>}}
        end
    end),
    meck:expect(beamai_chat_model, stream_chat, fun(_C, _M, _RawCb, Opts) ->
        TokenCb = maps:get(on_llm_new_token, Opts),
        _ = [TokenCb(T, #{}) || T <- [<<"Do">>, <<"ne">>]],
        {ok, beamai_chat_response:new(
            #{content => <<"Done">>, finish_reason => <<"stop">>})}
    end),
    ok.

%% @private 起一个带中断工具的 agent
interrupt_agent(Callbacks) ->
    beamai_agent:new(#{
        llm => {mock, #{}},
        callbacks => Callbacks,
        interrupt_tools => [#{name => <<"ask_human">>,
                              description => <<"Ask">>,
                              parameters => #{}}]
    }).

recv_token() ->
    receive {token, T, M} -> {T, M} after 1000 -> timeout end.

%% stream_resume：续跑那一轮走 provider streaming，token 实时到达
stream_resume_emits_tokens_test() ->
    ok = mock_interrupt_then_answer(),
    Self = self(),
    Callbacks = #{on_token => fun(T, M) -> Self ! {token, T, M} end},
    try
        {ok, Agent} = interrupt_agent(Callbacks),
        {interrupt, _Info, Agent1} = beamai_agent:run(Agent, <<"go">>),
        {ok, Result, Agent2} = beamai_agent:stream_resume(Agent1, <<"yes">>),
        ?assertEqual(<<"Done">>, maps:get(content, Result)),
        ?assertNot(beamai_agent:is_interrupted(Agent2)),
        %% 逐 token 到达，且都归属续跑产出的那条 assistant 消息
        {<<"Do">>, M1} = recv_token(),
        {<<"ne">>, M2} = recv_token(),
        MsgId = maps:get(message_id, M1),
        ?assert(is_binary(MsgId)),
        ?assertEqual(MsgId, maps:get(message_id, M2)),
        %% Meta 是完整的一份（与 on_message_start 等看到的同源），不只有 message_id
        ?assert(is_binary(maps:get(agent_id, M1))),
        ?assert(is_binary(maps:get(conversation_id, M1)))
    after
        meck:unload(beamai_chat_model)
    end.

%% 对照：resume/3 不流式，一个 token 都不产生（答案在轮末整块给出）
resume_does_not_stream_test() ->
    ok = mock_interrupt_then_answer(),
    Self = self(),
    Callbacks = #{on_token => fun(T, M) -> Self ! {token, T, M} end},
    try
        {ok, Agent} = interrupt_agent(Callbacks),
        {interrupt, _Info, Agent1} = beamai_agent:run(Agent, <<"go">>),
        {ok, Result, _Agent2} = beamai_agent:resume(Agent1, <<"yes">>),
        ?assertEqual(<<"Done">>, maps:get(content, Result)),
        ?assertEqual(timeout, recv_token())
    after
        meck:unload(beamai_chat_model)
    end.

%% resume/4 是底层入口：直接给 stream_token_handler 也能流（不经 on_token 桥接）
resume_4_accepts_stream_handler_test() ->
    ok = mock_interrupt_then_answer(),
    Self = self(),
    try
        {ok, Agent} = interrupt_agent(#{}),
        {interrupt, _Info, Agent1} = beamai_agent:run(Agent, <<"go">>),
        Handler = fun(Token) -> Self ! {raw_token, Token} end,
        {ok, Result, _Agent2} = beamai_agent:resume(
                                  Agent1, <<"yes">>, #{},
                                  #{stream_token_handler => Handler}),
        ?assertEqual(<<"Done">>, maps:get(content, Result)),
        ?assertEqual([<<"Do">>, <<"ne">>], drain_raw_tokens())
    after
        meck:unload(beamai_chat_model)
    end.

drain_raw_tokens() ->
    receive {raw_token, T} -> [T | drain_raw_tokens()] after 200 -> [] end.

%%====================================================================
%% 测试: 续跑里执行的工具照常触发 on_tool_result
%%====================================================================

recv_tool_result() ->
    receive {tool_result, N, R, Id} -> {N, R, Id} after 300 -> timeout end.

%% approved 续跑：被批准的工具真执行，结果回调带着原来的 tool_call_id
%%
%% 回归：这条路径在循环之外执行工具，曾经漏传 on_result，宿主于是看到一次
%% 有始无终的工具调用（发了调用、永远等不到结果）。env_retry 的重跑路径
%% 与本例共用同一个回调构造（beamai_agent_tool_loop:agent_result_cb/1）。
resume_approved_fires_tool_result_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CC = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null,
                        tool_calls => [#{id => <<"call_sql">>, type => <<"function">>,
                                         function => #{name => <<"execute_sql">>,
                                                       arguments => <<"{}">>}}],
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"ok">>, finish_reason => <<"stop">>}}
        end
    end),
    Self = self(),
    Callbacks = #{
        on_tool_call => fun(_N, _A) -> {interrupt, needs_approval} end,
        on_tool_result =>
            fun(Name, Result, Info) ->
                Self ! {tool_result, Name, Result, maps:get(tool_call_id, Info, undefined)}
            end
    },
    K0 = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                           beamai_chat_model:create(mock, #{})),
    K = beamai_chat_client:add_tool(K0, #{name => <<"execute_sql">>,
                                          parameters => #{},
                                          handler => fun(_A, _C) -> {ok, <<"SQL-OUT">>} end}),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K, callbacks => Callbacks}),
        {interrupt, Info, Agent1} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(callback, maps:get(interrupt_type, Info)),
        %% 中断时那次调用还没执行 → 不该有结果回调
        ?assertEqual(timeout, recv_tool_result()),
        {ok, _Result, _Agent2} = beamai_agent:resume(Agent1, <<"approved">>),
        ?assertEqual({<<"execute_sql">>, <<"SQL-OUT">>, <<"call_sql">>},
                     recv_tool_result())
    after
        meck:unload(beamai_chat_model)
    end.

%% reply / 拒绝走的是"人给的答复即结果"，不执行工具，故不触发 on_tool_result
resume_reply_does_not_fire_tool_result_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CC = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null,
                        tool_calls => [#{id => <<"call_sql">>, type => <<"function">>,
                                         function => #{name => <<"execute_sql">>,
                                                       arguments => <<"{}">>}}],
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"ok">>, finish_reason => <<"stop">>}}
        end
    end),
    Self = self(),
    Callbacks = #{
        on_tool_call => fun(_N, _A) -> {interrupt, needs_approval} end,
        on_tool_result =>
            fun(Name, Result, Info) ->
                Self ! {tool_result, Name, Result, maps:get(tool_call_id, Info, undefined)}
            end
    },
    K0 = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                           beamai_chat_model:create(mock, #{})),
    K = beamai_chat_client:add_tool(K0, #{name => <<"execute_sql">>,
                                          parameters => #{},
                                          handler => fun(_A, _C) -> {ok, <<"SQL-OUT">>} end}),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K, callbacks => Callbacks}),
        {interrupt, _Info, Agent1} = beamai_agent:run(Agent, <<"go">>),
        {ok, _Result, _Agent2} = beamai_agent:resume(Agent1, <<"rejected">>, #{}),
        ?assertEqual(timeout, recv_tool_result())
    after
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% 测试: 跨"节点重启"的 HITL（DETS pause_store）
%%====================================================================

%% @private 一次性的 DETS 暂停存储
fresh_pause_store() ->
    Unique = erlang:unique_integer([positive]),
    Name = list_to_atom(lists:concat(["hitl_restart_", Unique])),
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"), "beamai_pause_dets_tests"),
    ok = filelib:ensure_path(Dir),
    File = filename:join(Dir, lists:concat(["hitl_", Unique, ".dets"])),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    {Name, File}.

%% 暂停落 DETS → store 进程重启 → **全新的 agent** 仍能接着跑
%%
%% 这是持久化 pause_store 存在的全部理由：ETS 版进程一死表就回收，人十分钟后
%% 回话（或运维中间重启过）就只能作废这次会话。这里把那个断点真的制造出来。
hitl_survives_store_restart_test() ->
    ok = mock_interrupt_then_answer(),
    {Name, File} = fresh_pause_store(),
    Store = beamai_pause_store_dets:handle(Name),
    ConvId = <<"conv-restart">>,
    Config = #{llm => {mock, #{}},
               memory => false,
               conversation_id => ConvId,
               pause_store => Store,
               interrupt_tools => [#{name => <<"ask_human">>,
                                     description => <<"Ask">>,
                                     parameters => #{}}]},
    try
        %% 第一个 agent：停在人身上
        {ok, Agent1} = beamai_agent:new(Config),
        {interrupt, _Info, _} = beamai_agent:run(Agent1, <<"go">>),
        ?assertMatch({ok, _}, beamai_pause_store:pause_load(Store, ConvId)),

        %% 制造断点：store 进程停掉再从同一个文件开起来（≈ 节点重启）
        ok = beamai_pause_store_dets:stop(Name),
        {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),

        %% 第二个 agent：全新实例，自身没有任何中断态，只靠 conversation_id
        %% 从磁盘把中断态接回来（resume 的透明回落）
        {ok, Agent2} = beamai_agent:new(Config),
        %% 进程内什么都没有……
        ?assertEqual(undefined, maps:get(interrupt_state, Agent2)),
        %% ……但 is_interrupted/1 说有：它会去 store 里问，这正是持久化的意义
        ?assert(beamai_agent:is_interrupted(Agent2)),
        {ok, Result, Agent3} = beamai_agent:resume(Agent2, <<"yes">>),
        ?assertEqual(<<"Done">>, maps:get(content, Result)),
        ?assertNot(beamai_agent:is_interrupted(Agent3)),

        %% 恢复成功后快照被清掉——否则同一个暂停会被 resume 第二次
        ?assertEqual(none, beamai_pause_store:pause_load(Store, ConvId))
    after
        meck:unload(beamai_chat_model),
        catch beamai_pause_store_dets:stop(Name),
        file:delete(File)
    end.

%% 会话隔离：另一个会话的暂停不会被误认成本会话的
restart_does_not_cross_conversations_test() ->
    ok = mock_interrupt_then_answer(),
    {Name, File} = fresh_pause_store(),
    Store = beamai_pause_store_dets:handle(Name),
    Config = fun(ConvId) ->
        #{llm => {mock, #{}}, memory => false,
          conversation_id => ConvId, pause_store => Store,
          interrupt_tools => [#{name => <<"ask_human">>, description => <<"Ask">>,
                                parameters => #{}}]}
    end,
    try
        {ok, A} = beamai_agent:new(Config(<<"conv-a">>)),
        {interrupt, _, _} = beamai_agent:run(A, <<"go">>),
        ok = beamai_pause_store_dets:stop(Name),
        {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
        %% 另一个会话没有未决暂停 → resume 应当拒绝，而不是捡起 conv-a 的
        {ok, B} = beamai_agent:new(Config(<<"conv-b">>)),
        ?assertEqual({error, not_interrupted}, beamai_agent:resume(B, <<"yes">>)),
        ?assertMatch({ok, _}, beamai_pause_store:pause_load(Store, <<"conv-a">>))
    after
        meck:unload(beamai_chat_model),
        catch beamai_pause_store_dets:stop(Name),
        file:delete(File)
    end.
