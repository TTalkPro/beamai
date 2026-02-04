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
        pending_messages => [#{role => user, content => <<"hi">>}],
        assistant_response => #{role => assistant, content => null, tool_calls => []},
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
        pending_messages => [#{role => user, content => <<"delete files">>}],
        assistant_response => #{role => assistant, content => null,
                               tool_calls => [#{id => <<"c1">>}]},
        completed_tool_results => [#{role => tool, tool_call_id => <<"c0">>,
                                    content => <<"done">>}],
        interrupted_tool_call => #{id => <<"c1">>, function => #{name => <<"ask">>}}
    },
    Msgs = beamai_agent_interrupt:build_resume_messages(IntState, <<"yes, approved">>),
    %% [user msg] ++ [assistant] ++ [completed tool result] ++ [human tool result]
    ?assertEqual(4, length(Msgs)),
    LastMsg = lists:last(Msgs),
    ?assertEqual(tool, maps:get(role, LastMsg)),
    ?assertEqual(<<"c1">>, maps:get(tool_call_id, LastMsg)),
    ?assertEqual(<<"yes, approved">>, maps:get(content, LastMsg)).

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
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_Config, _Messages, _Opts) ->
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
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 测试: Callback 触发中断
%%====================================================================

callback_triggers_interrupt_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_Config, _Messages, _Opts) ->
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
    Kernel0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{}),
    K1 = beamai_kernel:add_service(Kernel0, LlmConfig),
    K2 = beamai_kernel:add_tools(K1, [
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
        {ok, Agent} = beamai_agent:new(#{kernel => K2, callbacks => Callbacks}),
        Result = beamai_agent:run(Agent, <<"Delete all users">>),
        ?assertMatch({interrupt, _, _}, Result),
        {interrupt, Info, _Agent1} = Result,
        ?assertEqual(callback, maps:get(interrupt_type, Info))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 测试: Resume 基本功能
%%====================================================================

resume_not_interrupted_test() ->
    {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}}}),
    ?assertEqual({error, not_interrupted}, beamai_agent:resume(Agent, <<"input">>)).

resume_after_interrupt_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_completion, chat, fun(_Config, _Messages, _Opts) ->
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
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 测试: Memory 持久化中断状态
%%====================================================================

memory_save_restore_interrupt_test() ->
    StoreName = test_interrupt_store,
    {ok, _Pid} = beamai_store_ets:start_link(StoreName, #{}),
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"interrupt-thread">>
    }),
    try
        %% 构建带中断状态的 agent
        {ok, Agent0} = beamai_agent:new(#{
            llm => {mock, #{}},
            memory => Memory
        }),
        IntState = #{
            status => interrupted,
            reason => #{question => <<"ok?">>},
            pending_messages => [#{role => user, content => <<"hi">>}],
            assistant_response => #{role => assistant, content => null, tool_calls => []},
            completed_tool_results => [],
            interrupted_tool_call => #{id => <<"c1">>, function => #{name => <<"ask">>}},
            iteration => 1,
            tool_calls_made => [],
            interrupt_type => tool_request,
            created_at => erlang:system_time(millisecond)
        },
        Agent1 = Agent0#{interrupt_state => IntState, run_id => <<"run-123">>},
        %% Save
        ok = beamai_agent:save(Agent1),
        %% Restore
        Config = #{llm => {mock, #{}}},
        {ok, Restored} = beamai_agent:restore(Config, Memory),
        ?assert(beamai_agent:is_interrupted(Restored)),
        RestoredInt = maps:get(interrupt_state, Restored),
        ?assertEqual(interrupted, maps:get(status, RestoredInt)),
        ?assertEqual(<<"run-123">>, maps:get(run_id, Restored))
    after
        beamai_store_ets:stop(StoreName)
    end.

%%====================================================================
%% 测试: beamai_memory 中断查询 API
%%====================================================================

memory_has_pending_interrupt_test() ->
    StoreName = test_interrupt_query_store,
    {ok, _Pid} = beamai_store_ets:start_link(StoreName, #{}),
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"query-thread">>
    }),
    Config = #{thread_id => <<"query-thread">>},
    try
        %% 初始状态：无中断
        ?assertNot(beamai_memory:has_pending_interrupt(Memory, Config)),
        %% 保存中断状态
        AgentData = #{
            messages => [],
            interrupt_state => #{status => interrupted, reason => test}
        },
        ProcessState = #{
            process_spec => <<"test">>,
            fsm_state => idle,
            steps_state => #{agent_state => #{state => AgentData}},
            event_queue => []
        },
        ThreadId = maps:get(thread_id, Config),
        {ok, _Snapshot, _NewMemory} = beamai_memory:save_snapshot(Memory, ThreadId, ProcessState, #{}),
        %% 现在有中断
        ?assert(beamai_memory:has_pending_interrupt(Memory, Config))
    after
        beamai_store_ets:stop(StoreName)
    end.

memory_get_interrupt_context_test() ->
    StoreName = test_interrupt_ctx_store,
    {ok, _Pid} = beamai_store_ets:start_link(StoreName, #{}),
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"ctx-thread">>
    }),
    Config = #{thread_id => <<"ctx-thread">>},
    try
        %% 无中断
        ?assertEqual({error, not_interrupted},
                     beamai_memory:get_interrupt_context(Memory, Config)),
        %% 保存中断状态
        AgentData = #{
            messages => [],
            interrupt_state => #{
                status => interrupted,
                reason => #{question => <<"approve?">>},
                interrupt_type => tool_request,
                interrupted_tool_call => #{id => <<"c1">>},
                created_at => 99999
            }
        },
        ProcessState = #{
            process_spec => <<"test">>,
            fsm_state => idle,
            steps_state => #{agent_state => #{state => AgentData}},
            event_queue => []
        },
        ThreadId = maps:get(thread_id, Config),
        {ok, _Snapshot, _NewMemory} = beamai_memory:save_snapshot(Memory, ThreadId, ProcessState, #{}),
        %% 获取上下文
        {ok, Ctx} = beamai_memory:get_interrupt_context(Memory, Config),
        ?assertEqual(#{question => <<"approve?">>}, maps:get(reason, Ctx)),
        ?assertEqual(tool_request, maps:get(interrupt_type, Ctx))
    after
        beamai_store_ets:stop(StoreName)
    end.
