%%%-------------------------------------------------------------------
%%% @doc Timeline / 多分支测试（fork/rollback/lineage/ancestry/prune）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_timeline_tests).

-include_lib("eunit/include/eunit.hrl").

msg(N) -> #{role => user, content => N}.

setup() ->
    {ok, MPid} = beamai_chat_memory_ets:start_link(tl_mem),
    {ok, LPid} = beamai_lineage_store_ets:start_link(tl_lin),
    {ok, PPid} = beamai_pause_store_ets:start_link(tl_pause),
    Deps = #{memory => beamai_chat_memory_ets:handle(tl_mem),
             lineage => beamai_lineage_store_ets:handle(tl_lin),
             pause_store => beamai_pause_store_ets:handle(tl_pause)},
    {Deps, [MPid, LPid, PPid]}.

cleanup(Pids) ->
    [gen_server:stop(P) || P <- Pids].

timeline_test_() ->
    {setup, fun setup/0, fun({_D, Pids}) -> cleanup(Pids) end,
     fun({Deps, _}) ->
        [
         ?_test(fork_full(Deps)),
         ?_test(fork_prefix(Deps)),
         ?_test(rollback_truncate(Deps)),
         ?_test(ancestry_chain(Deps)),
         ?_test(prune_rejects_with_children(Deps)),
         ?_test(pause_fork_copies_snapshot(Deps))
        ]
     end}.

fork_full(Deps) ->
    #{memory := Mem} = Deps,
    ok = beamai_chat_memory:mem_add(Mem, <<"src1">>, [msg(<<"a">>), msg(<<"b">>)]),
    {ok, New} = beamai_timeline:fork(Deps, <<"src1">>),
    ?assertEqual([msg(<<"a">>), msg(<<"b">>)], beamai_chat_memory:mem_get(Mem, New)),
    {ok, Rec} = beamai_timeline:lineage(Deps, New),
    ?assertEqual(<<"src1">>, maps:get(parent, Rec)),
    ?assertEqual(all, maps:get(fork_point, Rec)).

fork_prefix(Deps) ->
    #{memory := Mem} = Deps,
    ok = beamai_chat_memory:mem_add(Mem, <<"src2">>,
                                    [msg(<<"1">>), msg(<<"2">>), msg(<<"3">>)]),
    {ok, New} = beamai_timeline:fork(Deps, <<"src2">>, #{at => 2}),
    ?assertEqual([msg(<<"1">>), msg(<<"2">>)], beamai_chat_memory:mem_get(Mem, New)),
    {ok, Rec} = beamai_timeline:lineage(Deps, New),
    ?assertEqual(2, maps:get(fork_point, Rec)).

rollback_truncate(Deps) ->
    #{memory := Mem} = Deps,
    ok = beamai_chat_memory:mem_add(Mem, <<"rb">>,
                                    [msg(<<"1">>), msg(<<"2">>), msg(<<"3">>), msg(<<"4">>)]),
    ok = beamai_timeline:rollback(Deps, <<"rb">>, 2),
    ?assertEqual([msg(<<"1">>), msg(<<"2">>)], beamai_chat_memory:mem_get(Mem, <<"rb">>)).

ancestry_chain(Deps) ->
    #{memory := Mem} = Deps,
    ok = beamai_chat_memory:mem_add(Mem, <<"root">>, [msg(<<"x">>)]),
    {ok, B} = beamai_timeline:fork(Deps, <<"root">>, #{as => <<"branch-b">>}),
    {ok, C} = beamai_timeline:fork(Deps, B, #{as => <<"branch-c">>}),
    ?assertEqual([<<"root">>, <<"branch-b">>, <<"branch-c">>],
                 beamai_timeline:ancestry(Deps, C)),
    %% root 无血缘记录 → ancestry 仅自身
    ?assertEqual([<<"root">>], beamai_timeline:ancestry(Deps, <<"root">>)).

prune_rejects_with_children(Deps) ->
    #{memory := Mem} = Deps,
    ok = beamai_chat_memory:mem_add(Mem, <<"proot">>, [msg(<<"x">>)]),
    {ok, P1} = beamai_timeline:fork(Deps, <<"proot">>, #{as => <<"p1">>}),
    {ok, _P2} = beamai_timeline:fork(Deps, P1, #{as => <<"p2">>}),
    %% p1 有子 p2 → 拒绝
    ?assertMatch({error, {has_children, _}}, beamai_timeline:prune(Deps, <<"p1">>)),
    %% 先删叶子 p2，再删 p1
    ?assertEqual(ok, beamai_timeline:prune(Deps, <<"p2">>)),
    ?assertEqual(ok, beamai_timeline:prune(Deps, <<"p1">>)),
    ?assertEqual([], beamai_chat_memory:mem_get(Mem, <<"p1">>)),
    ?assertEqual(none, beamai_timeline:lineage(Deps, <<"p1">>)).

pause_fork_copies_snapshot(Deps) ->
    #{memory := Mem, pause_store := PS} = Deps,
    ok = beamai_chat_memory:mem_add(Mem, <<"psrc">>, [msg(<<"a">>)]),
    Snap = #{version => 1, conversation_id => <<"psrc">>,
             interrupt_state => #{status => interrupted}},
    ok = beamai_pause_store:pause_save(PS, <<"psrc">>, Snap),
    %% 全量 fork → 连带复制暂停快照（conversation_id 更新为新会话）
    {ok, New} = beamai_timeline:fork(Deps, <<"psrc">>, #{as => <<"pfork">>}),
    ?assertEqual({ok, Snap#{conversation_id => <<"pfork">>}},
                 beamai_pause_store:pause_load(PS, New)),
    %% 部分前缀 fork 不带暂停快照
    {ok, New2} = beamai_timeline:fork(Deps, <<"psrc">>, #{as => <<"pfork2">>, at => 1}),
    ?assertEqual(none, beamai_pause_store:pause_load(PS, New2)).
