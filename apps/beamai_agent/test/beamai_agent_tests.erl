%%%-------------------------------------------------------------------
%%% @doc beamai_agent 单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试: new/1
%%====================================================================

new_with_kernel_test() ->
    Kernel = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{}),
    K1 = beamai_kernel:add_service(Kernel, LlmConfig),
    {ok, Agent} = beamai_agent:new(#{kernel => K1}),
    ?assert(is_binary(beamai_agent:id(Agent))),
    ?assertEqual(<<"agent">>, beamai_agent:name(Agent)),
    ?assertEqual(0, beamai_agent:turn_count(Agent)),
    ?assertEqual([], beamai_agent:messages(Agent)).

new_with_config_test() ->
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{}},
        system_prompt => <<"You are a test agent.">>,
        name => <<"test_agent">>,
        metadata => #{role => tester}
    }),
    ?assertEqual(<<"test_agent">>, beamai_agent:name(Agent)),
    ?assertEqual(0, beamai_agent:turn_count(Agent)).

new_with_custom_id_test() ->
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{}},
        id => <<"my-agent-001">>
    }),
    ?assertEqual(<<"my-agent-001">>, beamai_agent:id(Agent)).

%%====================================================================
%% 测试: run/2
%%====================================================================

run_basic_test() ->
    {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}}}),
    {ok, Result, Agent1} = beamai_agent:run(Agent, <<"Hello">>),
    ?assertEqual(<<"This is a mock response.">>, maps:get(content, Result)),
    ?assertEqual(1, beamai_agent:turn_count(Agent1)),
    ?assertEqual(2, length(beamai_agent:messages(Agent1))),
    %% 验证消息历史
    [UserMsg, AssistantMsg] = beamai_agent:messages(Agent1),
    ?assertEqual(user, maps:get(role, UserMsg)),
    ?assertEqual(<<"Hello">>, maps:get(content, UserMsg)),
    ?assertEqual(assistant, maps:get(role, AssistantMsg)),
    ?assertEqual(<<"This is a mock response.">>, maps:get(content, AssistantMsg)).

run_with_system_prompt_test() ->
    %% 使用 meck 来验证 system prompt 被正确传递
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_Config, Messages, _Opts) ->
        %% 验证第一条消息是 system prompt
        [#{role := system, content := <<"Test system">>} | _] = Messages,
        {ok, #{content => <<"OK">>, finish_reason => stop}}
    end),
    try
        {ok, Agent} = beamai_agent:new(#{
            llm => {mock, #{}},
            system_prompt => <<"Test system">>
        }),
        {ok, Result, _} = beamai_agent:run(Agent, <<"Hi">>),
        ?assertEqual(<<"OK">>, maps:get(content, Result))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 测试: 多轮对话
%%====================================================================

multi_turn_test() ->
    {ok, Agent0} = beamai_agent:new(#{llm => {mock, #{}}}),
    {ok, _, Agent1} = beamai_agent:run(Agent0, <<"First">>),
    ?assertEqual(1, beamai_agent:turn_count(Agent1)),
    ?assertEqual(2, length(beamai_agent:messages(Agent1))),
    {ok, _, Agent2} = beamai_agent:run(Agent1, <<"Second">>),
    ?assertEqual(2, beamai_agent:turn_count(Agent2)),
    ?assertEqual(4, length(beamai_agent:messages(Agent2))),
    {ok, _, Agent3} = beamai_agent:run(Agent2, <<"Third">>),
    ?assertEqual(3, beamai_agent:turn_count(Agent3)),
    ?assertEqual(6, length(beamai_agent:messages(Agent3))).

multi_turn_history_accumulation_test() ->
    %% 验证每轮都带上完整历史
    meck:new(beamai_chat_completion, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_completion, chat, fun(_Config, Messages, _Opts) ->
        counters:add(CallCount, 1, 1),
        N = counters:get(CallCount, 1),
        %% 第 N 轮应有 2*(N-1) 条历史 + 1 条新 user msg
        ExpectedLen = 2 * (N - 1) + 1,
        ?assertEqual(ExpectedLen, length(Messages)),
        {ok, #{content => <<"Reply ", (integer_to_binary(N))/binary>>, finish_reason => stop}}
    end),
    try
        {ok, A0} = beamai_agent:new(#{llm => {mock, #{}}}),
        {ok, _, A1} = beamai_agent:run(A0, <<"Q1">>),
        {ok, _, A2} = beamai_agent:run(A1, <<"Q2">>),
        {ok, _, _A3} = beamai_agent:run(A2, <<"Q3">>),
        ok
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 测试: Tool Calling Loop
%%====================================================================

run_with_tool_calls_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_completion, chat, fun(_Config, _Messages, _Opts) ->
        counters:add(CallCount, 1, 1),
        N = counters:get(CallCount, 1),
        case N of
            1 ->
                %% 第一次返回 tool_call
                {ok, #{
                    content => null,
                    tool_calls => [#{
                        id => <<"call_1">>,
                        type => <<"function">>,
                        function => #{
                            name => <<"test_tool">>,
                            arguments => <<"{\"arg\":\"val\"}">>
                        }
                    }],
                    finish_reason => <<"tool_calls">>
                }};
            2 ->
                %% 第二次返回最终响应
                {ok, #{content => <<"Tool result processed.">>, finish_reason => <<"stop">>}}
        end
    end),
    %% 注册一个测试 tool
    Kernel0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{}),
    K1 = beamai_kernel:add_service(Kernel0, LlmConfig),
    K2 = beamai_kernel:add_plugin(K1, <<"test">>, [
        #{name => <<"test_tool">>,
          description => <<"A test tool">>,
          parameters => #{},
          handler => fun(_Args, _Ctx) -> {ok, <<"tool_output">>} end}
    ]),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K2}),
        {ok, Result, Agent1} = beamai_agent:run(Agent, <<"Use the tool">>),
        ?assertEqual(<<"Tool result processed.">>, maps:get(content, Result)),
        ?assertEqual(1, length(maps:get(tool_calls_made, Result, []))),
        ?assertEqual(1, beamai_agent:turn_count(Agent1))
    after
        meck:unload(beamai_chat_completion)
    end.

max_tool_iterations_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_Config, _Messages, _Opts) ->
        %% 总是返回 tool_call，永远不结束
        {ok, #{
            content => null,
            tool_calls => [#{
                id => <<"call_inf">>,
                type => <<"function">>,
                function => #{
                    name => <<"loop_tool">>,
                    arguments => <<"{}">>
                }
            }],
            finish_reason => <<"tool_calls">>
        }}
    end),
    Kernel0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{}),
    K1 = beamai_kernel:add_service(Kernel0, LlmConfig),
    K2 = beamai_kernel:add_plugin(K1, <<"test">>, [
        #{name => <<"loop_tool">>,
          description => <<"loops">>,
          parameters => #{},
          handler => fun(_Args, _Ctx) -> {ok, <<"again">>} end}
    ]),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K2, max_tool_iterations => 3}),
        {error, {max_tool_iterations, _}} = beamai_agent:run(Agent, <<"Loop">>)
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 测试: Callbacks
%%====================================================================

callbacks_on_turn_start_end_test() ->
    Self = self(),
    Callbacks = #{
        on_turn_start => fun(Meta) -> Self ! {turn_start, Meta} end,
        on_turn_end => fun(Meta) -> Self ! {turn_end, Meta} end
    },
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{}},
        callbacks => Callbacks
    }),
    {ok, _, _} = beamai_agent:run(Agent, <<"Test">>),
    receive {turn_start, StartMeta} ->
        ?assert(is_binary(maps:get(agent_id, StartMeta))),
        ?assertEqual(0, maps:get(turn_count, StartMeta))
    after 1000 -> ?assert(false)
    end,
    receive {turn_end, EndMeta} ->
        ?assertEqual(1, maps:get(turn_count, EndMeta))
    after 1000 -> ?assert(false)
    end.

callbacks_on_turn_error_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_Config, _Messages, _Opts) ->
        {error, connection_failed}
    end),
    Self = self(),
    Callbacks = #{
        on_turn_error => fun(Reason, _Meta) -> Self ! {turn_error, Reason} end
    },
    try
        {ok, Agent} = beamai_agent:new(#{
            llm => {mock, #{}},
            callbacks => Callbacks
        }),
        {error, connection_failed} = beamai_agent:run(Agent, <<"Test">>),
        receive {turn_error, connection_failed} -> ok
        after 1000 -> ?assert(false)
        end
    after
        meck:unload(beamai_chat_completion)
    end.

callback_filter_on_llm_call_test() ->
    Self = self(),
    Callbacks = #{
        on_llm_call => fun(Messages, _Meta) -> Self ! {llm_call, length(Messages)} end
    },
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{}},
        callbacks => Callbacks
    }),
    {ok, _, _} = beamai_agent:run(Agent, <<"Hello">>),
    receive {llm_call, MsgCount} ->
        %% 应该有 1 条消息（user msg，无 system prompt）
        ?assertEqual(1, MsgCount)
    after 1000 -> ?assert(false)
    end.

callback_filter_on_tool_call_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_completion, chat, fun(_Config, _Messages, _Opts) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 ->
                {ok, #{
                    content => null,
                    tool_calls => [#{
                        id => <<"call_x">>,
                        type => <<"function">>,
                        function => #{
                            name => <<"my_tool">>,
                            arguments => <<"{\"x\":1}">>
                        }
                    }],
                    finish_reason => <<"tool_calls">>
                }};
            _ ->
                {ok, #{content => <<"Done">>, finish_reason => <<"stop">>}}
        end
    end),
    Self = self(),
    Callbacks = #{
        on_tool_call => fun(Name, _Args) -> Self ! {tool_call, Name} end
    },
    Kernel0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{}),
    K1 = beamai_kernel:add_service(Kernel0, LlmConfig),
    K2 = beamai_kernel:add_plugin(K1, <<"test">>, [
        #{name => <<"my_tool">>,
          description => <<"test">>,
          parameters => #{},
          handler => fun(_Args, _Ctx) -> {ok, <<"result">>} end}
    ]),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K2, callbacks => Callbacks}),
        {ok, _, _} = beamai_agent:run(Agent, <<"Use tool">>),
        receive {tool_call, <<"my_tool">>} -> ok
        after 1000 -> ?assert(false)
        end
    after
        meck:unload(beamai_chat_completion)
    end.

callback_exception_ignored_test() ->
    %% 回调抛出异常不影响执行
    Callbacks = #{
        on_turn_start => fun(_) -> error(boom) end,
        on_turn_end => fun(_) -> throw(crash) end
    },
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{}},
        callbacks => Callbacks
    }),
    {ok, Result, _} = beamai_agent:run(Agent, <<"Test">>),
    ?assertEqual(<<"This is a mock response.">>, maps:get(content, Result)).

%%====================================================================
%% 测试: 状态查询与修改
%%====================================================================

state_queries_test() ->
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{}},
        name => <<"q_agent">>,
        system_prompt => <<"sys">>
    }),
    ?assertEqual(<<"q_agent">>, beamai_agent:name(Agent)),
    ?assertEqual(0, beamai_agent:turn_count(Agent)),
    ?assertEqual([], beamai_agent:messages(Agent)),
    ?assertEqual(undefined, beamai_agent:last_response(Agent)).

set_system_prompt_test() ->
    {ok, Agent0} = beamai_agent:new(#{llm => {mock, #{}}}),
    Agent1 = beamai_agent:set_system_prompt(Agent0, <<"New prompt">>),
    ?assertEqual(<<"New prompt">>, maps:get(system_prompt, Agent1)).

add_message_test() ->
    {ok, Agent0} = beamai_agent:new(#{llm => {mock, #{}}}),
    Msg = #{role => user, content => <<"Injected">>},
    Agent1 = beamai_agent:add_message(Agent0, Msg),
    ?assertEqual([Msg], beamai_agent:messages(Agent1)).

clear_messages_test() ->
    {ok, Agent0} = beamai_agent:new(#{llm => {mock, #{}}}),
    {ok, _, Agent1} = beamai_agent:run(Agent0, <<"Hi">>),
    ?assertEqual(2, length(beamai_agent:messages(Agent1))),
    Agent2 = beamai_agent:clear_messages(Agent1),
    ?assertEqual([], beamai_agent:messages(Agent2)).

update_metadata_test() ->
    {ok, Agent0} = beamai_agent:new(#{
        llm => {mock, #{}},
        metadata => #{a => 1}
    }),
    Agent1 = beamai_agent:update_metadata(Agent0, #{b => 2}),
    ?assertEqual(#{a => 1, b => 2}, maps:get(metadata, Agent1)).

last_response_test() ->
    {ok, Agent0} = beamai_agent:new(#{llm => {mock, #{}}}),
    {ok, _, Agent1} = beamai_agent:run(Agent0, <<"Hi">>),
    ?assertEqual(<<"This is a mock response.">>, beamai_agent:last_response(Agent1)).

%%====================================================================
%% 测试: Memory
%%====================================================================

memory_save_no_config_test() ->
    {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}}}),
    ?assertEqual({error, no_memory_configured}, beamai_agent:save(Agent)).

memory_save_restore_test() ->
    %% 创建一个 ETS memory store
    StoreName = test_agent_store,
    {ok, _Pid} = beamai_store_ets:start_link(StoreName, #{}),
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"test-thread">>
    }),
    Config = #{
        llm => {mock, #{}},
        memory => Memory,
        system_prompt => <<"Persist me">>
    },
    try
        {ok, Agent0} = beamai_agent:new(Config),
        {ok, _, Agent1} = beamai_agent:run(Agent0, <<"Message 1">>),
        ok = beamai_agent:save(Agent1),
        %% 恢复
        {ok, Restored} = beamai_agent:restore(Config, Memory),
        ?assertEqual(2, length(beamai_agent:messages(Restored))),
        ?assertEqual(1, beamai_agent:turn_count(Restored))
    after
        beamai_store_ets:stop(StoreName)
    end.

%%====================================================================
%% 测试: beamai_agent_callbacks 模块
%%====================================================================

callbacks_invoke_missing_test() ->
    ?assertEqual(ok, beamai_agent_callbacks:invoke(on_turn_start, [#{}], #{})).

callbacks_invoke_present_test() ->
    Self = self(),
    Cb = #{on_turn_start => fun(M) -> Self ! {got, M} end},
    beamai_agent_callbacks:invoke(on_turn_start, [hello], Cb),
    receive {got, hello} -> ok
    after 500 -> ?assert(false)
    end.

callbacks_build_metadata_test() ->
    State = #{id => <<"a1">>, name => <<"bob">>, turn_count => 5},
    Meta = beamai_agent_callbacks:build_metadata(State),
    ?assertEqual(<<"a1">>, maps:get(agent_id, Meta)),
    ?assertEqual(<<"bob">>, maps:get(agent_name, Meta)),
    ?assertEqual(5, maps:get(turn_count, Meta)),
    ?assert(is_integer(maps:get(timestamp, Meta))).

%%====================================================================
%% 测试: beamai_agent_state 模块
%%====================================================================

state_create_test() ->
    {ok, State} = beamai_agent_state:create(#{llm => {mock, #{}}}),
    ?assertEqual(true, maps:get('__agent__', State)),
    ?assert(is_binary(maps:get(id, State))),
    ?assertEqual(<<"agent">>, maps:get(name, State)),
    ?assertEqual([], maps:get(messages, State)),
    ?assertEqual(0, maps:get(turn_count, State)),
    ?assertEqual(10, maps:get(max_tool_iterations, State)).

state_build_kernel_with_existing_test() ->
    K = beamai_kernel:new(),
    ?assertEqual(K, beamai_agent_state:build_kernel(#{kernel => K})).

state_build_messages_test() ->
    State = #{
        system_prompt => <<"System">>,
        messages => [#{role => user, content => <<"Old">>},
                     #{role => assistant, content => <<"Reply">>}]
    },
    UserMsg = #{role => user, content => <<"New">>},
    Result = beamai_agent_state:build_messages(State, UserMsg),
    ?assertEqual(4, length(Result)),
    ?assertEqual(system, maps:get(role, hd(Result))),
    ?assertEqual(<<"New">>, maps:get(content, lists:last(Result))).

state_build_messages_no_system_test() ->
    State = #{system_prompt => undefined, messages => []},
    UserMsg = #{role => user, content => <<"Hi">>},
    Result = beamai_agent_state:build_messages(State, UserMsg),
    ?assertEqual([UserMsg], Result).

state_inject_callback_filters_test() ->
    K0 = beamai_kernel:new(),
    Callbacks = #{
        on_llm_call => fun(_Msgs, _Meta) -> ok end,
        on_tool_call => fun(_Name, _Args) -> ok end
    },
    K1 = beamai_agent_state:inject_callback_filters(K0, Callbacks),
    #{filters := Filters} = K1,
    ?assertEqual(2, length(Filters)),
    %% 验证 filter 名称
    Names = [maps:get(name, F) || F <- Filters],
    ?assert(lists:member(<<"agent_on_llm_call">>, Names)),
    ?assert(lists:member(<<"agent_on_tool_call">>, Names)).

state_inject_no_callbacks_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_agent_state:inject_callback_filters(K0, #{}),
    #{filters := Filters} = K1,
    ?assertEqual(0, length(Filters)).
