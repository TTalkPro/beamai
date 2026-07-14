%%%-------------------------------------------------------------------
%%% @doc DETS 会话存储测试（含重启恢复）
%%%-------------------------------------------------------------------
-module(beamai_chat_memory_dets_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 基本读写（与 ETS 后端契约一致）
%%====================================================================

add_get_clear_test() ->
    {Name, File} = fresh_store(basic),
    {ok, _Pid} = beamai_chat_memory_dets:start_link(Name, #{file => File}),
    Store = beamai_chat_memory_dets:handle(Name),
    %% 空会话返回 []
    ?assertEqual([], beamai_chat_memory:mem_get(Store, <<"c1">>)),
    %% 追加保持正序（含批量追加）
    M1 = #{role => user, content => <<"a">>},
    M2 = #{role => assistant, content => <<"b">>},
    M3 = #{role => user, content => <<"c">>},
    ok = beamai_chat_memory:mem_add(Store, <<"c1">>, [M1]),
    ok = beamai_chat_memory:mem_add(Store, <<"c1">>, [M2, M3]),
    ?assertEqual([M1, M2, M3], beamai_chat_memory:mem_get(Store, <<"c1">>)),
    %% 空追加为 no-op
    ok = beamai_chat_memory:mem_add(Store, <<"c1">>, []),
    ?assertEqual([M1, M2, M3], beamai_chat_memory:mem_get(Store, <<"c1">>)),
    %% 会话隔离
    ?assertEqual([], beamai_chat_memory:mem_get(Store, <<"c2">>)),
    %% 清空只影响目标会话
    ok = beamai_chat_memory:mem_add(Store, <<"c2">>, [M1]),
    ok = beamai_chat_memory:mem_clear(Store, <<"c1">>),
    ?assertEqual([], beamai_chat_memory:mem_get(Store, <<"c1">>)),
    ?assertEqual([M1], beamai_chat_memory:mem_get(Store, <<"c2">>)),
    cleanup(Name, File).

%% 复杂消息（tool_calls / content_blocks）无损往返
complex_message_roundtrip_test() ->
    {Name, File} = fresh_store(roundtrip),
    {ok, _Pid} = beamai_chat_memory_dets:start_link(Name, #{file => File}),
    Store = beamai_chat_memory_dets:handle(Name),
    Msg = #{role => assistant, content => null,
            tool_calls => [#{id => <<"t1">>, name => <<"foo">>,
                             arguments => #{<<"x">> => 1}}],
            content_blocks => [#{type => thinking, thinking => <<"r">>,
                                 signature => <<"sig">>}]},
    ok = beamai_chat_memory:mem_add(Store, <<"c">>, [Msg]),
    ?assertEqual([Msg], beamai_chat_memory:mem_get(Store, <<"c">>)),
    cleanup(Name, File).

%%====================================================================
%% 持久化：重启后恢复历史（本后端存在的意义）
%%====================================================================

survives_restart_test() ->
    {Name, File} = fresh_store(restart),
    M1 = #{role => user, content => <<"hi">>},
    M2 = #{role => assistant, content => <<"hello">>},
    {ok, _} = beamai_chat_memory_dets:start_link(Name, #{file => File}),
    Store = beamai_chat_memory_dets:handle(Name),
    ok = beamai_chat_memory:mem_add(Store, <<"c">>, [M1, M2]),
    ok = beamai_chat_memory_dets:stop(Name),
    %% 重新打开同一文件：历史仍在，且能继续正序追加
    {ok, _} = beamai_chat_memory_dets:start_link(Name, #{file => File}),
    ?assertEqual([M1, M2], beamai_chat_memory:mem_get(Store, <<"c">>)),
    M3 = #{role => user, content => <<"again">>},
    ok = beamai_chat_memory:mem_add(Store, <<"c">>, [M3]),
    ?assertEqual([M1, M2, M3], beamai_chat_memory:mem_get(Store, <<"c">>)),
    cleanup(Name, File).

%%====================================================================
%% 与 memory filter 协作（kernel 直用路径）
%%====================================================================

memory_filter_with_dets_test() ->
    {Name, File} = fresh_store(filter),
    {ok, _} = beamai_chat_memory_dets:start_link(Name, #{file => File}),
    Store = beamai_chat_memory_dets:handle(Name),
    Filter = beamai_memory_filter:memory_filter(Store),
    Ctx = beamai_context:with_conversation_id(beamai_context:new(), <<"c">>),
    Old = #{role => user, content => <<"old">>},
    New = #{role => user, content => <<"new">>},
    ok = beamai_chat_memory:mem_add(Store, <<"c">>, [Old]),
    Terminal = fun(#{messages := Msgs, context := C}) ->
        #{response => beamai_llm_response:new(#{content => <<"reply">>}),
          context => C, seen => Msgs}
    end,
    {ok, Resp} = beamai_filter_chain:run([Filter], around_chat, Terminal,
                                         #{messages => [New], context => Ctx,
                                           opts => #{}}),
    %% 前置：存 delta + 展开完整历史；后置：存 assistant 回复
    ?assertEqual([Old, New], maps:get(seen, Resp)),
    ?assertEqual(3, length(beamai_chat_memory:mem_get(Store, <<"c">>))),
    cleanup(Name, File).

%%====================================================================
%% 辅助
%%====================================================================

fresh_store(Prefix) ->
    Unique = erlang:unique_integer([positive]),
    Name = list_to_atom(lists:concat([Prefix, "_dets_", Unique])),
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"), "beamai_dets_tests"),
    ok = filelib:ensure_path(Dir),
    File = filename:join(Dir, lists:concat([Prefix, "_", Unique, ".dets"])),
    {Name, File}.

cleanup(Name, File) ->
    catch beamai_chat_memory_dets:stop(Name),
    file:delete(File),
    ok.
