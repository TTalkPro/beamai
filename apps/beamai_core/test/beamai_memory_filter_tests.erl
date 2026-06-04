%%%-------------------------------------------------------------------
%%% @doc Memory Filter 与会话存储测试
%%%-------------------------------------------------------------------
-module(beamai_memory_filter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MOCK_MODULE, beamai_mock_mem_provider).

%%====================================================================
%% ETS Store 单元测试
%%====================================================================

ets_store_add_get_clear_test() ->
    Name = unique_name(ets_store),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Store = beamai_chat_memory_ets:handle(Name),
    %% 空会话返回 []
    ?assertEqual([], beamai_chat_memory:mem_get(Store, <<"c1">>)),
    %% 追加保持正序
    M1 = #{role => user, content => <<"a">>},
    M2 = #{role => assistant, content => <<"b">>},
    ok = beamai_chat_memory:mem_add(Store, <<"c1">>, [M1]),
    ok = beamai_chat_memory:mem_add(Store, <<"c1">>, [M2]),
    ?assertEqual([M1, M2], beamai_chat_memory:mem_get(Store, <<"c1">>)),
    %% 会话隔离
    ?assertEqual([], beamai_chat_memory:mem_get(Store, <<"c2">>)),
    %% 清理
    ok = beamai_chat_memory:mem_clear(Store, <<"c1">>),
    ?assertEqual([], beamai_chat_memory:mem_get(Store, <<"c1">>)),
    gen_server:stop(Pid).

%%====================================================================
%% Window Store 单元测试
%%====================================================================

window_store_test() ->
    Name = unique_name(win_inner),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Inner = beamai_chat_memory_ets:handle(Name),
    Win = beamai_chat_memory_window:handle(Inner, 2),
    Sys = #{role => system, content => <<"sys">>},
    U1 = #{role => user, content => <<"u1">>},
    A1 = #{role => assistant, content => <<"a1">>},
    U2 = #{role => user, content => <<"u2">>},
    ok = beamai_chat_memory:mem_add(Win, <<"c">>, [Sys, U1, A1, U2]),
    %% 底层保留全量
    ?assertEqual([Sys, U1, A1, U2], beamai_chat_memory:mem_get(Inner, <<"c">>)),
    %% 窗口：system 保留 + 最近 2 条非系统
    ?assertEqual([Sys, A1, U2], beamai_chat_memory:mem_get(Win, <<"c">>)),
    gen_server:stop(Pid).

window_drops_orphan_tool_test() ->
    Name = unique_name(win_orphan),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Inner = beamai_chat_memory_ets:handle(Name),
    Win = beamai_chat_memory_window:handle(Inner, 2),
    A1 = #{role => assistant, content => <<"a1">>},
    T1 = #{role => tool, tool_call_id => <<"x">>, name => <<"t">>, content => <<"r">>},
    U2 = #{role => user, content => <<"u2">>},
    ok = beamai_chat_memory:mem_add(Win, <<"c">>, [A1, T1, U2]),
    %% 窗口取最近 2 条 = [T1, U2]，头部孤立 tool 被丢弃
    ?assertEqual([U2], beamai_chat_memory:mem_get(Win, <<"c">>)),
    gen_server:stop(Pid).

%%====================================================================
%% response_to_message 单元测试
%%====================================================================

response_to_message_text_test() ->
    Resp = beamai_llm_response:new(#{content => <<"hello">>}),
    ?assertEqual(#{role => assistant, content => <<"hello">>},
                 beamai_memory_filter:response_to_message(Resp)).

response_to_message_null_test() ->
    Resp = beamai_llm_response:new(#{content => null}),
    ?assertEqual(undefined, beamai_memory_filter:response_to_message(Resp)).

%%====================================================================
%% Memory Filter（单 filter：around_chat 前置存 delta+展开、后置存回复）
%%====================================================================

%% 终端回显收到的 messages，并产出一个 assistant 响应
echo_terminal() ->
    fun(#{messages := Msgs, context := C}) ->
        #{response => beamai_llm_response:new(#{content => <<"reply">>}),
          context => C, seen => Msgs}
    end.

run_chat_filter(Filter, Messages, Ctx) ->
    beamai_filter_chain:run([Filter], around_chat, echo_terminal(),
                            #{messages => Messages, context => Ctx, opts => #{}}).

pre_chat_stores_and_expands_test() ->
    Name = unique_name(mem_pre),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Store = beamai_chat_memory_ets:handle(Name),
    Filter = beamai_memory_filter:memory_filter(Store),
    Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"c">>),
    Old = #{role => user, content => <<"old">>},
    New = #{role => user, content => <<"new">>},
    ok = beamai_chat_memory:mem_add(Store, <<"c">>, [Old]),
    {ok, Resp} = run_chat_filter(Filter, [New], Ctx),
    %% 前置：存 delta + 用完整历史替换 → 终端看到 [Old, New]
    ?assertEqual([Old, New], maps:get(seen, Resp)),
    %% 后置：把 assistant 回复也存入 → store 共 3 条
    ?assertEqual(3, length(beamai_chat_memory:mem_get(Store, <<"c">>))),
    gen_server:stop(Pid).

no_conv_id_passthrough_test() ->
    Name = unique_name(mem_pass),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Store = beamai_chat_memory_ets:handle(Name),
    Filter = beamai_memory_filter:memory_filter(Store),
    Ctx = beamai_context:new(),  %% 无 conversation_id
    New = #{role => user, content => <<"x">>},
    {ok, Resp} = run_chat_filter(Filter, [New], Ctx),
    %% 原样透传，终端只看到 delta，store 不变
    ?assertEqual([New], maps:get(seen, Resp)),
    ?assertEqual([], beamai_chat_memory:mem_get(Store, <<"c">>)),
    gen_server:stop(Pid).

post_chat_stores_response_test() ->
    Name = unique_name(mem_post),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Store = beamai_chat_memory_ets:handle(Name),
    Filter = beamai_memory_filter:memory_filter(Store),
    Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"c">>),
    {ok, _Resp} = run_chat_filter(Filter, [], Ctx),
    %% 前置存了空 delta（无），后置存 assistant 回复
    ?assertEqual([#{role => assistant, content => <<"reply">>}],
                 beamai_chat_memory:mem_get(Store, <<"c">>)),
    gen_server:stop(Pid).

%%====================================================================
%% 集成测试（meck）：跨 invoke 累积 + ephemeral 清理
%%====================================================================

memory_integration_test_() ->
    {setup, fun setup_mock/0, fun cleanup_mock/1, fun(_) ->
        [fun cross_invoke_accumulation/0]
    end}.

%% mock：返回 content = "saw:N"（N=LLM 收到的消息条数），不触发工具调用
setup_mock() ->
    meck:new(?MOCK_MODULE, [non_strict]),
    meck:expect(?MOCK_MODULE, name, fun() -> <<"mock">> end),
    meck:expect(?MOCK_MODULE, default_config, fun() -> #{} end),
    meck:expect(?MOCK_MODULE, validate_config, fun(_) -> ok end),
    meck:expect(?MOCK_MODULE, supports_tools, fun() -> true end),
    meck:expect(?MOCK_MODULE, supports_streaming, fun() -> false end),
    meck:expect(?MOCK_MODULE, chat, fun(_Config, Request) ->
        Messages = maps:get(messages, Request, []),
        N = integer_to_binary(length(Messages)),
        {ok, #{id => <<"r">>, model => <<"mock">>,
               content => <<"saw:", N/binary>>,
               finish_reason => <<"stop">>, usage => #{}}}
    end),
    ok.

cleanup_mock(_) ->
    meck:unload(?MOCK_MODULE).

%% 多次 invoke_chat 经 Memory filter 按 conversation_id 累积历史
cross_invoke_accumulation() ->
    Name = unique_name(integ_mem),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Store = beamai_chat_memory_ets:handle(Name),
    K = build_mock_kernel(Store),
    Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"s1">>),
    %% 第一轮：LLM 看到 1 条（user1）
    {ok, R1, _} = beamai_kernel:invoke_chat(K, [#{role => user, content => <<"hi">>}],
                                            #{context => Ctx}),
    ?assertEqual(<<"saw:1">>, maps:get(content, R1)),
    %% 第二轮：同会话，LLM 看到 3 条（user1, asst1, user2）
    {ok, R2, _} = beamai_kernel:invoke_chat(K, [#{role => user, content => <<"bye">>}],
                                            #{context => Ctx}),
    ?assertEqual(<<"saw:3">>, maps:get(content, R2)),
    %% store 累积 4 条
    Hist = beamai_chat_memory:mem_get(Store, <<"s1">>),
    ?assertEqual(4, length(Hist)),
    ?assertEqual([user, assistant, user, assistant],
                 [maps:get(role, M) || M <- Hist]),
    gen_server:stop(Pid).

%%====================================================================
%% 辅助
%%====================================================================

build_mock_kernel(Store) ->
    K0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create({custom, ?MOCK_MODULE}, #{model => <<"mock">>}),
    K1 = beamai_kernel:add_service(K0, LlmConfig),
    beamai_kernel:with_memory(K1, Store).

unique_name(Prefix) ->
    list_to_atom(lists:concat([Prefix, "_", erlang:unique_integer([positive])])).
