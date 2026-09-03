%%%-------------------------------------------------------------------
%%% @doc beamai_agent 单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试: new/1
%%====================================================================

new_with_chat_client_test() ->
    ChatClient = beamai_chat_client:new(),
    LlmConfig = beamai_chat_model:create(mock, #{}),
    K1 = beamai_chat_client:add_chat_model(ChatClient, LlmConfig),
    {ok, Agent} = beamai_agent:new(#{chat_client => K1}),
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
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_Config, Messages, _Opts) ->
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
        meck:unload(beamai_chat_model)
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
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_Config, Messages, _Opts) ->
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
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% 测试: Tool Calling Loop
%%====================================================================

run_with_tool_calls_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_Config, _Messages, _Opts) ->
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
    ChatClient0 = beamai_chat_client:new(),
    LlmConfig = beamai_chat_model:create(mock, #{}),
    K1 = beamai_chat_client:add_chat_model(ChatClient0, LlmConfig),
    K2 = beamai_chat_client:add_tools(K1, [
        #{name => <<"test_tool">>,
          description => <<"A test tool">>,
          parameters => #{},
          handler => fun(_Args, _Ctx) -> {ok, <<"tool_output">>} end}
    ]),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K2}),
        {ok, Result, Agent1} = beamai_agent:run(Agent, <<"Use the tool">>),
        ?assertEqual(<<"Tool result processed.">>, maps:get(content, Result)),
        ?assertEqual(1, length(maps:get(tool_calls_made, Result, []))),
        ?assertEqual(1, beamai_agent:turn_count(Agent1))
    after
        meck:unload(beamai_chat_model)
    end.

%% 一轮多个 tool_call：默认并发执行，结果按原顺序、且总耗时显著小于串行之和
parallel_tool_calls_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    ThreeCalls = [tc(<<"call_a">>, <<"slow_a">>),
                  tc(<<"call_b">>, <<"slow_b">>),
                  tc(<<"call_c">>, <<"slow_c">>)],
    meck:expect(beamai_chat_model, chat, fun(_Config, _Messages, _Opts) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 -> {ok, #{content => null, tool_calls => ThreeCalls,
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    Sleep = fun(_Args, _Ctx) -> timer:sleep(150), {ok, <<"ok">>} end,
    K = slow_tools_chat_client(Sleep),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K}),  %% parallel_tools 默认 true
        {Micros, {ok, Result, _}} =
            timer:tc(fun() -> beamai_agent:run(Agent, <<"go">>) end),
        Made = maps:get(tool_calls_made, Result, []),
        Names = [maps:get(name, R) || R <- Made],
        %% 三个工具结果按原 tool_call 顺序
        ?assertEqual([<<"slow_a">>, <<"slow_b">>, <<"slow_c">>], Names),
        %% 并发：远小于串行 3*150=450ms（留余量，断言 < 350ms）
        ?assert(Micros < 350000)
    after
        meck:unload(beamai_chat_model)
    end.

%% parallel_tools=false：串行执行，结果顺序仍正确
sequential_tool_calls_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    TwoCalls = [tc(<<"c1">>, <<"t1">>), tc(<<"c2">>, <<"t2">>)],
    meck:expect(beamai_chat_model, chat, fun(_Config, _Messages, _Opts) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 -> {ok, #{content => null, tool_calls => TwoCalls,
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    Fast = fun(_Args, _Ctx) -> {ok, <<"ok">>} end,
    K = slow_tools_chat_client(Fast),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K, parallel_tools => false}),
        {ok, Result, _} = beamai_agent:run(Agent, <<"go">>),
        Names = [maps:get(name, R) || R <- maps:get(tool_calls_made, Result, [])],
        ?assertEqual([<<"t1">>, <<"t2">>], Names)
    after
        meck:unload(beamai_chat_model)
    end.

%% @private 构造一个 tool_call map
tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

%% @private 构造一个注册了多个同 handler 工具的 ChatClient
slow_tools_chat_client(Handler) ->
    ChatClient0 = beamai_chat_client:new(),
    K1 = beamai_chat_client:add_chat_model(ChatClient0, beamai_chat_model:create(mock, #{})),
    Names = [<<"slow_a">>, <<"slow_b">>, <<"slow_c">>, <<"t1">>, <<"t2">>],
    beamai_chat_client:add_tools(K1,
        [#{name => N, description => <<"t">>, parameters => #{}, handler => Handler}
         || N <- Names]).

max_tool_iterations_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_Config, _Messages, _Opts) ->
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
    ChatClient0 = beamai_chat_client:new(),
    LlmConfig = beamai_chat_model:create(mock, #{}),
    K1 = beamai_chat_client:add_chat_model(ChatClient0, LlmConfig),
    K2 = beamai_chat_client:add_tools(K1, [
        #{name => <<"loop_tool">>,
          description => <<"loops">>,
          parameters => #{},
          handler => fun(_Args, _Ctx) -> {ok, <<"again">>} end}
    ]),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K2, max_tool_iterations => 3}),
        {error, {max_tool_iterations, _}} = beamai_agent:run(Agent, <<"Loop">>)
    after
        meck:unload(beamai_chat_model)
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
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_Config, _Messages, _Opts) ->
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
        meck:unload(beamai_chat_model)
    end.

callback_on_llm_call_test() ->
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

%% on_llm_result：每次 LLM 返回后触发；多工具回合含中间轮，可取各次 usage
callback_on_llm_result_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 -> {ok, #{content => null, tool_calls => [tc(<<"c1">>, <<"t1">>)],
                        finish_reason => <<"tool_calls">>,
                        usage => #{total_tokens => 11}}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>,
                        usage => #{total_tokens => 22}}}
        end
    end),
    Self = self(),
    Callbacks = #{on_llm_result =>
        fun(Resp, _Meta) ->
            Self ! {llm_result, beamai_chat_response:has_tool_calls(Resp),
                    maps:get(total_tokens, beamai_chat_response:usage(Resp), 0)}
        end},
    K = slow_tools_chat_client(fun(_A, _C) -> {ok, <<"ok">>} end),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K, callbacks => Callbacks}),
        {ok, _, _} = beamai_agent:run(Agent, <<"go">>),
        %% 两次 LLM 调用各触发一次（含中间工具轮），usage 各自可见
        ?assertEqual({true, 11}, recv_llm_result()),   %% 第一次：含 tool_calls
        ?assertEqual({false, 22}, recv_llm_result())   %% 第二次：最终回复
    after
        meck:unload(beamai_chat_model)
    end.

recv_llm_result() ->
    receive {llm_result, HasTC, Tokens} -> {HasTC, Tokens}
    after 1000 -> timeout
    end.

callback_on_tool_call_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_Config, _Messages, _Opts) ->
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
    ChatClient0 = beamai_chat_client:new(),
    LlmConfig = beamai_chat_model:create(mock, #{}),
    K1 = beamai_chat_client:add_chat_model(ChatClient0, LlmConfig),
    K2 = beamai_chat_client:add_tools(K1, [
        #{name => <<"my_tool">>,
          description => <<"test">>,
          parameters => #{},
          handler => fun(_Args, _Ctx) -> {ok, <<"result">>} end}
    ]),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K2, callbacks => Callbacks}),
        {ok, _, _} = beamai_agent:run(Agent, <<"Use tool">>),
        receive {tool_call, <<"my_tool">>} -> ok
        after 1000 -> ?assert(false)
        end,
        %% on_tool_call 对每次工具调用只应触发一次（不再有 filter 双触发）
        ?assertEqual(0, drain_tool_calls())
    after
        meck:unload(beamai_chat_model)
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
    %% 跨轮历史改由 memory provider 维护，agent 状态不再持有 messages，
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

state_build_chat_client_with_existing_test() ->
    K = beamai_chat_client:new(),
    ?assertEqual(K, beamai_agent_state:build_chat_client(#{chat_client => K})).

%% agent 不再向 ChatClient 注入 callback/memory filter：注册回调后 ChatClient 仍无 filter
state_no_filter_injection_test() ->
    {ok, State} = beamai_agent:new(#{
        llm => {mock, #{}},
        callbacks => #{on_llm_call => fun(_M, _Meta) -> ok end,
                       on_tool_call => fun(_N, _A) -> ok end}
    }),
    #{filters := Filters} = beamai_agent:chat_client(State),
    ?assertEqual([], Filters).

%% plugins 只提供工具：模块即使导出 filters/0 也被忽略（特性已删除，
%% filter 一律在构建 ChatClient 时经 filters 列表一次性给出）
state_plugin_filters_ignored_test() ->
    {ok, State} = beamai_agent:new(#{
        llm => {mock, #{}},
        plugins => [beamai_agent_test_plugin]
    }),
    #{filters := Filters} = beamai_agent:chat_client(State),
    ?assertEqual([], Filters),
    %% 工具正常注册
    ?assertMatch({ok, _}, beamai_tool_registry:resolve(beamai_chat_client:tools(beamai_agent:chat_client(State)), <<"plugin_tool">>)).

%%====================================================================
%% extract_content 健壮性（#4）
%%====================================================================

extract_content_null_test() ->
    Resp = beamai_chat_response:new(#{content => null}),
    ?assertEqual(<<>>, beamai_agent_utils:extract_content(Resp)).

extract_content_binary_test() ->
    Resp = beamai_chat_response:new(#{content => <<"hello">>}),
    ?assertEqual(<<"hello">>, beamai_agent_utils:extract_content(Resp)).

extract_content_non_binary_test() ->
    %% 意外的非 binary content（如 list / map）兜底为空二进制，不崩溃
    R1 = beamai_chat_response:new(#{content => [#{type => text, text => <<"a">>}]}),
    ?assertEqual(<<>>, beamai_agent_utils:extract_content(R1)),
    R2 = beamai_chat_response:new(#{content => #{foo => <<"bar">>}}),
    ?assertEqual(<<>>, beamai_agent_utils:extract_content(R2)).

%%====================================================================
%% 真流式（#2）：每轮 LLM 调用走 provider streaming，token 实时透出
%%====================================================================

%% 单轮：token 经 on_token 实时、按序到达；最终统一响应驱动返回值
stream_real_tokens_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            TokenCb = maps:get(on_llm_new_token, Opts),
            [TokenCb(T, #{}) || T <- [<<"Hel">>, <<"lo">>, <<"!">>]],
            {ok, beamai_chat_response:new(
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
        meck:unload(beamai_chat_model)
    end.

%% 跨工具轮：tool 调用轮无文本 token，最终回合逐 token 流出
stream_with_tool_call_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            counters:add(CallCount, 1, 1),
            TokenCb = maps:get(on_llm_new_token, Opts),
            case counters:get(CallCount, 1) of
                1 ->
                    {ok, beamai_chat_response:new(
                        #{content => null, tool_calls => [tc(<<"c1">>, <<"t1">>)],
                          finish_reason => <<"tool_calls">>})};
                _ ->
                    TokenCb(<<"final">>, #{}),
                    {ok, beamai_chat_response:new(
                        #{content => <<"final">>, finish_reason => <<"stop">>})}
            end
        end),
    Self = self(),
    Callbacks = #{on_token => fun(Tok, _M) -> Self ! {token, Tok} end},
    K = slow_tools_chat_client(fun(_A, _C) -> {ok, <<"ok">>} end),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K, callbacks => Callbacks}),
        {ok, Result, _} = beamai_agent:stream(Agent, <<"go">>),
        ?assertEqual(<<"final">>, maps:get(content, Result)),
        ?assertEqual(1, length(maps:get(tool_calls_made, Result, []))),
        %% 仅最终回合产生一个 token（工具轮 content=null 不产 token）
        ?assertEqual([<<"final">>], collect_tokens(1))
    after
        meck:unload(beamai_chat_model)
    end.

%% @private 按序收集 N 个 {token, _} 消息
collect_tokens(0) -> [];
collect_tokens(N) ->
    receive {token, T} -> [T | collect_tokens(N - 1)]
    after 1000 -> []
    end.

%%====================================================================
%% 消息边界（on_message_start / on_message_end）
%%====================================================================

%% 一轮 turn 里每次 LLM 调用产出一条 assistant 消息：边界成对、id 各不相同，
%% 且流式 token 的 Meta 带着自己所属消息的 id
message_boundaries_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            counters:add(CallCount, 1, 1),
            TokenCb = maps:get(on_llm_new_token, Opts),
            case counters:get(CallCount, 1) of
                1 ->
                    {ok, beamai_chat_response:new(
                        #{content => null, tool_calls => [tc(<<"c1">>, <<"t1">>)],
                          finish_reason => <<"tool_calls">>})};
                _ ->
                    TokenCb(<<"fi">>, #{}),
                    TokenCb(<<"nal">>, #{}),
                    {ok, beamai_chat_response:new(
                        #{content => <<"final">>, finish_reason => <<"stop">>})}
            end
        end),
    Self = self(),
    Callbacks = #{
        on_message_start => fun(Id, _M) -> Self ! {mstart, Id} end,
        on_message_end =>
            fun(Msg, M) -> Self ! {mend, Msg, maps:get(message_id, M)} end,
        on_token =>
            fun(T, M) -> Self ! {tok, T, maps:get(message_id, M, undefined)} end
    },
    K = slow_tools_chat_client(fun(_A, _C) -> {ok, <<"ok">>} end),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K, callbacks => Callbacks}),
        {ok, _, _} = beamai_agent:stream(Agent, <<"go">>),
        %% 第一条：只有 tool_calls 的 assistant 回合（不产文本 token）
        {mstart, Id1} = recv_boundary(),
        {mend, Msg1, Id1} = recv_boundary(),
        ?assertMatch(#{tool_calls := [_]}, Msg1),
        %% 第二条：文本回合，token 夹在这一对之间且带 Id2
        {mstart, Id2} = recv_boundary(),
        ?assertNotEqual(Id1, Id2),
        ?assertEqual([{<<"fi">>, Id2}, {<<"nal">>, Id2}],
                     [{T, I} || {tok, T, I} <- [recv_boundary(), recv_boundary()]]),
        {mend, Msg2, Id2} = recv_boundary(),
        ?assertEqual(<<"final">>, maps:get(content, Msg2))
    after
        meck:unload(beamai_chat_model)
    end.

%% LLM 出错也闭合这条消息（Message=undefined）：宿主不必为异常路径兜底
message_boundary_closed_on_error_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) -> {error, boom} end),
    Self = self(),
    Callbacks = #{
        on_message_start => fun(Id, _M) -> Self ! {mstart, Id} end,
        on_message_end =>
            fun(Msg, M) -> Self ! {mend, Msg, maps:get(message_id, M)} end
    },
    try
        {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}}, callbacks => Callbacks}),
        ?assertMatch({error, _}, beamai_agent:run(Agent, <<"hi">>)),
        {mstart, Id} = recv_boundary(),
        ?assertEqual({mend, undefined, Id}, recv_boundary())
    after
        meck:unload(beamai_chat_model)
    end.

%% 直返合成的回合没有 LLM 调用，但它同样是一条 assistant 消息：边界照发
return_direct_emits_message_boundary_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        {ok, #{content => null, tool_calls => [tc(<<"c1">>, <<"direct">>)],
               finish_reason => <<"tool_calls">>}}
    end),
    Self = self(),
    Callbacks = #{
        on_message_start => fun(Id, _M) -> Self ! {mstart, Id} end,
        on_message_end =>
            fun(Msg, M) -> Self ! {mend, Msg, maps:get(message_id, M)} end
    },
    K0 = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                           beamai_chat_model:create(mock, #{})),
    K = beamai_chat_client:add_tool(K0, #{name => <<"direct">>, parameters => #{},
                                          handler => fun(_) -> {ok, <<"TOOL-OUT">>} end,
                                          return_direct => true}),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K, callbacks => Callbacks}),
        {ok, Result, _} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(<<"TOOL-OUT">>, maps:get(content, Result)),
        %% 第一条来自 LLM（tool_calls 回合），第二条是直返合成的
        {mstart, Id1} = recv_boundary(),
        {mend, _Msg1, Id1} = recv_boundary(),
        {mstart, Id2} = recv_boundary(),
        {mend, Msg2, Id2} = recv_boundary(),
        ?assertNotEqual(Id1, Id2),
        ?assertEqual(<<"TOOL-OUT">>, maps:get(content, Msg2))
    after
        meck:unload(beamai_chat_model)
    end.

%% @private 按到达顺序收一条边界/token 消息（超时返回 timeout）
recv_boundary() ->
    receive
        M when element(1, M) =:= mstart;
               element(1, M) =:= mend;
               element(1, M) =:= tok -> M
    after 1000 -> timeout
    end.

%%====================================================================
%% 原始流事件（on_llm_event）：统一响应抹掉的东西经这条通道原样透出
%%====================================================================

%% tool_calls 的 arguments 增量既不是文本（不走 on_token）、也不在统一响应里，
%% 只有 on_llm_event 看得见
stream_raw_events_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, RawCb, Opts) ->
            TokenCb = maps:get(on_llm_new_token, Opts),
            RawCb(#{<<"choices">> =>
                        [#{<<"delta">> =>
                               #{<<"tool_calls">> =>
                                     [#{<<"index">> => 0,
                                        <<"function">> =>
                                            #{<<"arguments">> => <<"{\"x\"">>}}]}}]}),
            TokenCb(<<"hi">>, #{}),
            RawCb(#{<<"choices">> => [#{<<"delta">> => #{<<"content">> => <<"hi">>}}]}),
            {ok, beamai_chat_response:new(
                #{content => <<"hi">>, finish_reason => <<"stop">>})}
        end),
    Self = self(),
    Callbacks = #{on_token => fun(T, _M) -> Self ! {token, T} end,
                  on_llm_event => fun(Ev, Meta) -> Self ! {event, Ev, Meta} end},
    try
        {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}}, callbacks => Callbacks}),
        {ok, Result, _} = beamai_agent:stream(Agent, <<"hi">>),
        ?assertEqual(<<"hi">>, maps:get(content, Result)),
        %% 第一条 raw 事件是 tool_calls 的 arguments 增量
        {Ev1, Meta1} = recv_event(),
        ?assertMatch(#{<<"choices">> := [#{<<"delta">> := #{<<"tool_calls">> := _}}]}, Ev1),
        ?assert(is_binary(maps:get(run_id, Meta1))),
        %% 文本 chunk 两条通道都到：on_token 拿归一化文本，on_llm_event 拿原文
        {Ev2, _} = recv_event(),
        ?assertMatch(#{<<"choices">> := [#{<<"delta">> := #{<<"content">> := <<"hi">>}}]}, Ev2),
        ?assertEqual([<<"hi">>], collect_tokens(1))
    after
        meck:unload(beamai_chat_model)
    end.

%% 非流式 run/2 没有流事件：on_llm_event 一次都不触发
run_emits_no_raw_events_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
    end),
    Self = self(),
    Callbacks = #{on_llm_event => fun(Ev, Meta) -> Self ! {event, Ev, Meta} end},
    try
        {ok, Agent} = beamai_agent:new(#{llm => {mock, #{}}, callbacks => Callbacks}),
        {ok, _, _} = beamai_agent:run(Agent, <<"hi">>),
        ?assertEqual(timeout, recv_event())
    after
        meck:unload(beamai_chat_model)
    end.

%% @private 收一条 {event, Ev, Meta}（超时返回 timeout）
recv_event() ->
    receive {event, Ev, Meta} -> {Ev, Meta}
    after 300 -> timeout
    end.

%%====================================================================
%% on_tool_result 回调（#7） + 工具错误结构化（#8）
%%====================================================================

%% 工具执行后触发 on_tool_result（函数名 + 编码结果）
on_tool_result_callback_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 -> {ok, #{content => null, tool_calls => [tc(<<"c1">>, <<"t1">>)],
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    Self = self(),
    Callbacks = #{on_tool_result => fun(Name, Result) -> Self ! {tool_result, Name, Result} end},
    K = slow_tools_chat_client(fun(_A, _C) -> {ok, <<"the-output">>} end),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K, callbacks => Callbacks}),
        {ok, _, _} = beamai_agent:run(Agent, <<"go">>),
        receive {tool_result, <<"t1">>, <<"the-output">>} -> ok
        after 1000 -> ?assert(false)
        end
    after
        meck:unload(beamai_chat_model)
    end.

%% 注册 arity-3 的工具回调时，末位额外收到 Info（Meta + tool_call_id / args），
%% 并发批次下靠 tool_call_id 把结果配回对应调用
tool_callbacks_carry_tool_call_id_test() ->
    meck:new(beamai_chat_model, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 -> {ok, #{content => null,
                        tool_calls => [tc(<<"call_a">>, <<"t1">>),
                                       tc(<<"call_b">>, <<"t2">>)],
                        finish_reason => <<"tool_calls">>}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    Self = self(),
    Callbacks = #{
        on_tool_call => fun(Name, _Args, Info) -> Self ! {call, Name, Info}, ok end,
        on_tool_result =>
            fun(Name, Result, Info) -> Self ! {result, Name, Result, Info} end
    },
    K = slow_tools_chat_client(fun(_A, _C) -> {ok, <<"out">>} end),
    try
        {ok, Agent} = beamai_agent:new(#{chat_client => K, callbacks => Callbacks}),
        {ok, _, _} = beamai_agent:run(Agent, <<"go">>),
        %% 每次调用带自己的 tool_call_id（on_tool_call 沿批次逆序触发，故排序比较）
        Calls = recv_n(call, 2),
        ?assertEqual([{<<"t1">>, <<"call_a">>}, {<<"t2">>, <<"call_b">>}],
                     lists:sort([{N, maps:get(tool_call_id, I)} || {call, N, I} <- Calls])),
        %% 结果同样带 id（并发下顺序不定，靠 id 而非工具名配对）+ 原始 args
        Results = recv_n(result, 2),
        ?assertEqual([{<<"t1">>, <<"call_a">>, <<"out">>},
                      {<<"t2">>, <<"call_b">>, <<"out">>}],
                     lists:sort([{N, maps:get(tool_call_id, I), R}
                                 || {result, N, R, I} <- Results])),
        ?assert(lists:all(fun({result, _, _, I}) -> maps:is_key(args, I) end, Results)),
        %% Info 是 Meta 的超集：run_id / conversation_id 仍在
        [{call, _, Info} | _] = Calls,
        ?assert(is_binary(maps:get(run_id, Info))),
        ?assert(is_binary(maps:get(conversation_id, Info)))
    after
        meck:unload(beamai_chat_model)
    end.

%% @private 收 N 条以 Tag 开头的消息（顺序不定，调用方自行排序）
recv_n(Tag, N) -> recv_n(Tag, N, []).
recv_n(_Tag, 0, Acc) -> Acc;
recv_n(Tag, N, Acc) ->
    receive
        Msg when element(1, Msg) =:= Tag -> recv_n(Tag, N - 1, [Msg | Acc])
    after 1000 -> Acc
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
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
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
        meck:unload(beamai_chat_model),
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
