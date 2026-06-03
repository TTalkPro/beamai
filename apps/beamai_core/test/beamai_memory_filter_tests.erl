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
%% Memory Filter（pre/post_chat）单元测试
%%====================================================================

pre_chat_stores_and_expands_test() ->
    Name = unique_name(pre_filter),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Store = beamai_chat_memory_ets:handle(Name),
    [Pre, _Post] = beamai_memory_filter:memory_filters(Store),
    Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"c">>),
    %% 预置一条历史
    Old = #{role => user, content => <<"old">>},
    ok = beamai_chat_memory:mem_add(Store, <<"c">>, [Old]),
    %% pre_chat：存 delta + 用完整历史替换 messages
    Delta = [#{role => user, content => <<"new">>}],
    {ok, Msgs, _Ctx2} = beamai_filter:apply_pre_chat_filters([Pre], Delta, Ctx),
    ?assertEqual([Old, #{role => user, content => <<"new">>}], Msgs),
    %% store 现在含两条
    ?assertEqual(2, length(beamai_chat_memory:mem_get(Store, <<"c">>))),
    gen_server:stop(Pid).

pre_chat_no_conv_id_passthrough_test() ->
    Name = unique_name(pre_pass),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Store = beamai_chat_memory_ets:handle(Name),
    [Pre, _Post] = beamai_memory_filter:memory_filters(Store),
    Ctx = beamai_context:new(),  %% 无 conversation_id
    Delta = [#{role => user, content => <<"x">>}],
    {ok, Msgs, _} = beamai_filter:apply_pre_chat_filters([Pre], Delta, Ctx),
    %% 原样透传，store 不变
    ?assertEqual(Delta, Msgs),
    gen_server:stop(Pid).

post_chat_stores_response_test() ->
    Name = unique_name(post_filter),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Store = beamai_chat_memory_ets:handle(Name),
    [_Pre, Post] = beamai_memory_filter:memory_filters(Store),
    Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"c">>),
    Resp = beamai_llm_response:new(#{content => <<"reply">>}),
    {ok, _R, _Ctx2} = beamai_filter:apply_post_chat_filters([Post], Resp, Ctx),
    ?assertEqual([#{role => assistant, content => <<"reply">>}],
                 beamai_chat_memory:mem_get(Store, <<"c">>)),
    gen_server:stop(Pid).

%%====================================================================
%% 集成测试（meck）：跨 invoke 累积 + ephemeral 清理
%%====================================================================

memory_integration_test_() ->
    {setup, fun setup_mock/0, fun cleanup_mock/1, fun(_) ->
        [fun cross_invoke_accumulation/0, fun ephemeral_cleanup/0]
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

cross_invoke_accumulation() ->
    Name = unique_name(integ_mem),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Store = beamai_chat_memory_ets:handle(Name),
    K = build_mock_kernel(Store),
    Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"s1">>),
    %% 第一轮：LLM 看到 1 条（user1）
    {ok, R1, _} = beamai_kernel:invoke(K, [#{role => user, content => <<"hi">>}],
                                        #{context => Ctx}),
    ?assertEqual(<<"saw:1">>, maps:get(content, R1)),
    %% 第二轮：同会话，LLM 看到 3 条（user1, asst1, user2）
    {ok, R2, _} = beamai_kernel:invoke(K, [#{role => user, content => <<"bye">>}],
                                        #{context => Ctx}),
    ?assertEqual(<<"saw:3">>, maps:get(content, R2)),
    %% store 累积 4 条
    Hist = beamai_chat_memory:mem_get(Store, <<"s1">>),
    ?assertEqual(4, length(Hist)),
    ?assertEqual([user, assistant, user, assistant],
                 [maps:get(role, M) || M <- Hist]),
    gen_server:stop(Pid).

%% 有记忆但 context 无 conversation_id → 临时会话，结束后清理
ephemeral_cleanup() ->
    Name = unique_name(integ_eph),
    {ok, Pid} = beamai_chat_memory_ets:start_link(Name),
    Store = beamai_chat_memory_ets:handle(Name),
    K = build_mock_kernel(Store),
    {ok, _R, Ctx2} = beamai_kernel:invoke(K, [#{role => user, content => <<"hi">>}], #{}),
    %% 生成了临时 conv_id，但 invoke 结束已 mem_clear → 该会话为空
    ConvId = beamai_context:conversation_id(Ctx2),
    ?assertEqual([], beamai_chat_memory:mem_get(Store, ConvId)),
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
