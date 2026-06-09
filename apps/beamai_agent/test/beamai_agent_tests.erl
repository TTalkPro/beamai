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
    K2 = beamai_kernel:add_tools(K1, [
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

%% 一轮多个 tool_call：默认并发执行，结果按原顺序、且总耗时显著小于串行之和
parallel_tool_calls_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    CallCount = counters:new(1, []),
    ThreeCalls = [tc(<<"call_a">>, <<"slow_a">>),
                  tc(<<"call_b">>, <<"slow_b">>),
                  tc(<<"call_c">>, <<"slow_c">>)],
    meck:expect(beamai_chat_completion, chat, fun(_Config, _Messages, _Opts) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 -> {ok, #{content => null, tool_calls => ThreeCalls,
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    Sleep = fun(_Args, _Ctx) -> timer:sleep(150), {ok, <<"ok">>} end,
    K = slow_tools_kernel(Sleep),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K}),  %% parallel_tools 默认 true
        {Micros, {ok, Result, _}} =
            timer:tc(fun() -> beamai_agent:run(Agent, <<"go">>) end),
        Made = maps:get(tool_calls_made, Result, []),
        Names = [maps:get(name, R) || R <- Made],
        %% 三个工具结果按原 tool_call 顺序
        ?assertEqual([<<"slow_a">>, <<"slow_b">>, <<"slow_c">>], Names),
        %% 并发：远小于串行 3*150=450ms（留余量，断言 < 350ms）
        ?assert(Micros < 350000)
    after
        meck:unload(beamai_chat_completion)
    end.

%% parallel_tools=false：串行执行，结果顺序仍正确
sequential_tool_calls_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    CallCount = counters:new(1, []),
    TwoCalls = [tc(<<"c1">>, <<"t1">>), tc(<<"c2">>, <<"t2">>)],
    meck:expect(beamai_chat_completion, chat, fun(_Config, _Messages, _Opts) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 -> {ok, #{content => null, tool_calls => TwoCalls,
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    Fast = fun(_Args, _Ctx) -> {ok, <<"ok">>} end,
    K = slow_tools_kernel(Fast),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K, parallel_tools => false}),
        {ok, Result, _} = beamai_agent:run(Agent, <<"go">>),
        Names = [maps:get(name, R) || R <- maps:get(tool_calls_made, Result, [])],
        ?assertEqual([<<"t1">>, <<"t2">>], Names)
    after
        meck:unload(beamai_chat_completion)
    end.

%% @private 构造一个 tool_call map
tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

%% @private 构造一个注册了多个同 handler 工具的 kernel
slow_tools_kernel(Handler) ->
    Kernel0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_service(Kernel0, beamai_chat_completion:create(mock, #{})),
    Names = [<<"slow_a">>, <<"slow_b">>, <<"slow_c">>, <<"t1">>, <<"t2">>],
    beamai_kernel:add_tools(K1,
        [#{name => N, description => <<"t">>, parameters => #{}, handler => Handler}
         || N <- Names]).

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
    K2 = beamai_kernel:add_tools(K1, [
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
        on_llm_call => fun(Messages, Meta) -> Self ! {llm_call, length(Messages), Meta} end
    },
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{}},
        name => <<"bob">>,
        conversation_id => <<"conv-xyz">>,
        callbacks => Callbacks
    }),
    {ok, _, _} = beamai_agent:run(Agent, <<"Hello">>),
    receive {llm_call, MsgCount, Meta} ->
        %% 应该有 1 条消息（user msg，无 system prompt）
        ?assertEqual(1, MsgCount),
        %% on_llm_call 现在收到真实 meta（不再是空 map）
        ?assertEqual(<<"bob">>, maps:get(agent_name, Meta)),
        ?assertEqual(<<"conv-xyz">>, maps:get(conversation_id, Meta)),
        ?assert(is_binary(maps:get(agent_id, Meta))),
        ?assert(is_integer(maps:get(turn_count, Meta))),
        ?assert(maps:is_key(run_id, Meta))
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
    K2 = beamai_kernel:add_tools(K1, [
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
        end,
        %% on_tool_call 对每次工具调用只应触发一次（不再有 filter 双触发）
        ?assertEqual(0, drain_tool_calls())
    after
        meck:unload(beamai_chat_completion)
    end.

%% @private 排空邮箱中剩余的 {tool_call, _} 消息，返回剩余条数
drain_tool_calls() -> drain_tool_calls(0).
drain_tool_calls(N) ->
    receive {tool_call, _} -> drain_tool_calls(N + 1)
    after 0 -> N
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
    %% 跨轮历史改由 filter-memory provider 维护，agent 状态不再持有 messages，
    %% 而是持有 memory provider 与 conversation_id。
    ?assert(is_binary(beamai_agent_state:conversation_id(State))),
    ?assertNotEqual(undefined, beamai_agent_state:memory(State)),
    ?assertEqual([], beamai_agent:messages(State)),
    ?assertEqual(0, maps:get(turn_count, State)),
    ?assertEqual(10, maps:get(max_tool_iterations, State)).

state_create_memory_disabled_test() ->
    %% memory => false 时不启用记忆：无 provider，messages 退化为 []
    {ok, State} = beamai_agent_state:create(#{llm => {mock, #{}}, memory => false}),
    ?assertEqual(undefined, beamai_agent_state:memory(State)),
    ?assertEqual([], beamai_agent:messages(State)).

state_build_kernel_with_existing_test() ->
    K = beamai_kernel:new(),
    ?assertEqual(K, beamai_agent_state:build_kernel(#{kernel => K})).

%% agent 不再向 kernel 注入 callback/memory filter：注册回调后 kernel 仍无 filter
state_no_filter_injection_test() ->
    {ok, State} = beamai_agent:new(#{
        llm => {mock, #{}},
        callbacks => #{on_llm_call => fun(_M, _Meta) -> ok end,
                       on_tool_call => fun(_N, _A) -> ok end}
    }),
    #{filters := Filters} = beamai_agent:kernel(State),
    ?assertEqual([], Filters).

%%====================================================================
%% extract_content 健壮性（#4）
%%====================================================================

extract_content_null_test() ->
    Resp = beamai_llm_response:new(#{content => null}),
    ?assertEqual(<<>>, beamai_agent_utils:extract_content(Resp)).

extract_content_binary_test() ->
    Resp = beamai_llm_response:new(#{content => <<"hello">>}),
    ?assertEqual(<<"hello">>, beamai_agent_utils:extract_content(Resp)).

extract_content_non_binary_test() ->
    %% 意外的非 binary content（如 list / map）兜底为空二进制，不崩溃
    R1 = beamai_llm_response:new(#{content => [#{type => text, text => <<"a">>}]}),
    ?assertEqual(<<>>, beamai_agent_utils:extract_content(R1)),
    R2 = beamai_llm_response:new(#{content => #{foo => <<"bar">>}}),
    ?assertEqual(<<>>, beamai_agent_utils:extract_content(R2)).

%%====================================================================
%% 真流式（#2）：每轮 LLM 调用走 provider streaming，token 实时透出
%%====================================================================

%% 单轮：token 经 on_token 实时、按序到达；最终统一响应驱动返回值
stream_real_tokens_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            TokenCb = maps:get(on_llm_new_token, Opts),
            [TokenCb(T, #{}) || T <- [<<"Hel">>, <<"lo">>, <<"!">>]],
            {ok, beamai_llm_response:new(
                #{content => <<"Hello!">>, finish_reason => <<"stop">>})}
        end),
    Self = self(),
    Callbacks = #{on_token => fun(Tok, _Meta) -> Self ! {token, Tok} end},
    try
        {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}}, callbacks => Callbacks}),
        {ok, Result, _} = beamai_agent:stream(Agent, <<"hi">>),
        ?assertEqual(<<"Hello!">>, maps:get(content, Result)),
        ?assertEqual([<<"Hel">>, <<"lo">>, <<"!">>], collect_tokens(3))
    after
        meck:unload(beamai_chat_completion)
    end.

%% 跨工具轮：tool 调用轮无文本 token，最终回合逐 token 流出
stream_with_tool_call_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_completion, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            counters:add(CallCount, 1, 1),
            TokenCb = maps:get(on_llm_new_token, Opts),
            case counters:get(CallCount, 1) of
                1 ->
                    {ok, beamai_llm_response:new(
                        #{content => null, tool_calls => [tc(<<"c1">>, <<"t1">>)],
                          finish_reason => <<"tool_calls">>})};
                _ ->
                    TokenCb(<<"final">>, #{}),
                    {ok, beamai_llm_response:new(
                        #{content => <<"final">>, finish_reason => <<"stop">>})}
            end
        end),
    Self = self(),
    Callbacks = #{on_token => fun(Tok, _M) -> Self ! {token, Tok} end},
    K = slow_tools_kernel(fun(_A, _C) -> {ok, <<"ok">>} end),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K, callbacks => Callbacks}),
        {ok, Result, _} = beamai_agent:stream(Agent, <<"go">>),
        ?assertEqual(<<"final">>, maps:get(content, Result)),
        ?assertEqual(1, length(maps:get(tool_calls_made, Result, []))),
        %% 仅最终回合产生一个 token（工具轮 content=null 不产 token）
        ?assertEqual([<<"final">>], collect_tokens(1))
    after
        meck:unload(beamai_chat_completion)
    end.

%% @private 按序收集 N 个 {token, _} 消息
collect_tokens(0) -> [];
collect_tokens(N) ->
    receive {token, T} -> [T | collect_tokens(N - 1)]
    after 1000 -> []
    end.

%%====================================================================
%% on_tool_result 回调（#7） + 工具错误结构化（#8）
%%====================================================================

%% 工具执行后触发 on_tool_result（函数名 + 编码结果）
on_tool_result_callback_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 -> {ok, #{content => null, tool_calls => [tc(<<"c1">>, <<"t1">>)],
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    Self = self(),
    Callbacks = #{on_tool_result => fun(Name, Result) -> Self ! {tool_result, Name, Result} end},
    K = slow_tools_kernel(fun(_A, _C) -> {ok, <<"the-output">>} end),
    try
        {ok, Agent} = beamai_agent:new(#{kernel => K, callbacks => Callbacks}),
        {ok, _, _} = beamai_agent:run(Agent, <<"go">>),
        receive {tool_result, <<"t1">>, <<"the-output">>} -> ok
        after 1000 -> ?assert(false)
        end
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 默认 store 纳入监督树（#5）
%%====================================================================

%% beamai_agent_sup:ensure_store/1 把 store 纳入监督树；幂等；被 kill 自动重启
supervised_store_restart_test() ->
    {ok, Started} = application:ensure_all_started(beamai_agent),
    Name = supervised_test_store,
    try
        Handle = beamai_agent_sup:ensure_store(Name),
        ?assertMatch({beamai_chat_memory_ets, Name}, Handle),
        Pid1 = whereis(Name),
        ?assert(is_pid(Pid1)),
        %% 幂等：再次调用复用同一进程
        _ = beamai_agent_sup:ensure_store(Name),
        ?assertEqual(Pid1, whereis(Name)),
        %% 是 supervisor 的子进程
        ?assert(lists:keymember(Name, 1, supervisor:which_children(beamai_agent_sup))),
        %% kill 后自动重启为新进程
        exit(Pid1, kill),
        timer:sleep(100),
        Pid2 = whereis(Name),
        ?assert(is_pid(Pid2)),
        ?assertNotEqual(Pid1, Pid2)
    after
        catch supervisor:terminate_child(beamai_agent_sup, Name),
        catch supervisor:delete_child(beamai_agent_sup, Name),
        [application:stop(A) || A <- lists:reverse(Started)]
    end.

%% app 未启动时 ensure_default_store 回退到孤儿 store（不依赖监督树）
default_store_orphan_fallback_test() ->
    %% 确保 app 未运行
    application:stop(beamai_agent),
    {ok, State} = beamai_agent:new(#{llm => {mock, #{}}}),
    %% 默认 provider 包默认 ETS store（无窗口 = infinity）
    Mem = beamai_agent_state:memory(State),
    ?assertMatch({beamai_memory_provider_default,
                  {{beamai_chat_memory_ets, beamai_agent_default_memory}, infinity}}, Mem),
    ?assert(is_pid(whereis(beamai_agent_default_memory))).

%%====================================================================
%% 上下文窗口管理（#6）：memory => {window, N}
%%====================================================================

%% {window, N}：全量持久（history 全见）；窗口只在 prepare（发送前）裁剪
windowed_memory_test() ->
    {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}}, memory => {window, 2},
                                     conversation_id => <<"win-conv">>}),
    %% memory provider 为默认 provider 带窗口 2
    Provider = beamai_agent_state:memory(Agent),
    ?assertMatch({beamai_memory_provider_default, {_, 2}}, Provider),
    %% 追加 4 条用户消息
    [beamai_agent:add_message(Agent, #{role => user, content => C})
     || C <- [<<"m1">>, <<"m2">>, <<"m3">>, <<"m4">>]],
    %% history（messages/1）保留全量 4 条
    AllContents = [maps:get(content, M) || M <- beamai_agent:messages(Agent)],
    ?assertEqual([<<"m1">>, <<"m2">>, <<"m3">>, <<"m4">>], AllContents),
    %% prepare（发送给 LLM 前）只保留最近 2 条
    Full = beamai_memory_provider:history(Provider, <<"win-conv">>),
    Sent = beamai_memory_provider:prepare(Provider, <<"win-conv">>, Full),
    ?assertEqual([<<"m3">>, <<"m4">>], [maps:get(content, M) || M <- Sent]).

%%====================================================================
%% 自定义记忆 Provider：memory => {Module, Ref}（符合 beamai_memory_provider 协议）
%%====================================================================

custom_memory_provider_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) ->
        {ok, #{content => <<"hi back">>, finish_reason => <<"stop">>}}
    end),
    Provider = beamai_agent_fake_memory:new(fake_mem_tab),
    try
        {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}},
                                         memory => Provider,
                                         conversation_id => <<"c">>}),
        %% provider 原样作为 agent 记忆（未被默认 provider 包装）
        ?assertEqual(Provider, beamai_agent_state:memory(Agent)),
        {ok, R, _} = beamai_agent:run(Agent, <<"hello">>),
        ?assertEqual(<<"hi back">>, maps:get(content, R)),
        %% 自定义 provider 真的被用到：单轮一次 prepare；append 至少 2 次(user+assistant)
        ?assertEqual(1, beamai_agent_fake_memory:count(fake_mem_tab, prepare)),
        ?assert(beamai_agent_fake_memory:count(fake_mem_tab, append) >= 2),
        %% 历史经自定义 provider 落库（user + assistant）
        ?assertEqual([<<"hello">>, <<"hi back">>],
                     [maps:get(content, M) || M <- beamai_agent:messages(Agent)])
    after
        meck:unload(beamai_chat_completion),
        catch ets:delete(fake_mem_tab)
    end.

%% 工具返回 {error, Reason}：归一为稳定的 #{error => #{type, message}} 结构
tool_error_structured_test() ->
    %% 非 map reason → #{type, message}
    E1 = beamai_agent_utils:tool_error(some_atom_reason),
    ?assertEqual(#{error => #{type => <<"tool_error">>,
                              message => <<"some_atom_reason">>}}, E1),
    %% binary reason
    E2 = beamai_agent_utils:tool_error(<<"boom">>),
    ?assertMatch(#{error := #{type := <<"tool_error">>, message := <<"boom">>}}, E2),
    %% 已结构化的 map reason 原样透传到 error 下
    E3 = beamai_agent_utils:tool_error(#{type => <<"not_found">>, message => <<"x">>}),
    ?assertEqual(#{error => #{type => <<"not_found">>, message => <<"x">>}}, E3).
