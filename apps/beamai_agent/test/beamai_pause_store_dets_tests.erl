%%%-------------------------------------------------------------------
%%% @doc DETS 暂停存储测试（含重启恢复 —— 这才是它存在的理由）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_pause_store_dets_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

fresh_store(Prefix) ->
    Unique = erlang:unique_integer([positive]),
    Name = list_to_atom(lists:concat([Prefix, "_pause_dets_", Unique])),
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"), "beamai_pause_dets_tests"),
    ok = filelib:ensure_path(Dir),
    File = filename:join(Dir, lists:concat([Prefix, "_", Unique, ".dets"])),
    {Name, File}.

cleanup(Name, File) ->
    catch beamai_pause_store_dets:stop(Name),
    file:delete(File),
    ok.

snapshot(ConvId, Reason) ->
    snapshot(ConvId, Reason, erlang:system_time(millisecond)).

snapshot(ConvId, Reason, PausedAt) ->
    #{version => 1,
      conversation_id => ConvId,
      paused_at => PausedAt,
      pause_reason => Reason,
      pending_tool => #{id => <<"c1">>, name => <<"ask_human">>},
      interrupt_state => #{status => interrupted, iteration => 2}}.

%%====================================================================
%% behaviour 契约（与 ETS 后端一致）
%%====================================================================

save_load_clear_test() ->
    {Name, File} = fresh_store(basic),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    Store = beamai_pause_store_dets:handle(Name),
    %% 没有未决暂停时是 none，不是 {ok, #{}}
    ?assertEqual(none, beamai_pause_store:pause_load(Store, <<"c1">>)),
    Snap = snapshot(<<"c1">>, needs_approval),
    ok = beamai_pause_store:pause_save(Store, <<"c1">>, Snap),
    ?assertEqual({ok, Snap}, beamai_pause_store:pause_load(Store, <<"c1">>)),
    ok = beamai_pause_store:pause_clear(Store, <<"c1">>),
    ?assertEqual(none, beamai_pause_store:pause_load(Store, <<"c1">>)),
    cleanup(Name, File).

%% 每会话至多一份：再暂停即覆盖（behaviour 的明确约定）
save_overwrites_test() ->
    {Name, File} = fresh_store(overwrite),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    Store = beamai_pause_store_dets:handle(Name),
    ok = beamai_pause_store:pause_save(Store, <<"c1">>, snapshot(<<"c1">>, first)),
    Second = snapshot(<<"c1">>, second),
    ok = beamai_pause_store:pause_save(Store, <<"c1">>, Second),
    ?assertEqual({ok, Second}, beamai_pause_store:pause_load(Store, <<"c1">>)),
    ?assertEqual([<<"c1">>], beamai_pause_store_dets:conversations(Name)),
    cleanup(Name, File).

conversations_are_isolated_test() ->
    {Name, File} = fresh_store(isolation),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    Store = beamai_pause_store_dets:handle(Name),
    ok = beamai_pause_store:pause_save(Store, <<"a">>, snapshot(<<"a">>, ra)),
    ok = beamai_pause_store:pause_save(Store, <<"b">>, snapshot(<<"b">>, rb)),
    ok = beamai_pause_store:pause_clear(Store, <<"a">>),
    ?assertEqual(none, beamai_pause_store:pause_load(Store, <<"a">>)),
    ?assertMatch({ok, #{pause_reason := rb}}, beamai_pause_store:pause_load(Store, <<"b">>)),
    ?assertEqual([<<"b">>], beamai_pause_store_dets:conversations(Name)),
    cleanup(Name, File).

%%====================================================================
%% 持久化：重启后暂停还在（ETS 版做不到的那一件事）
%%====================================================================

survives_restart_test() ->
    {Name, File} = fresh_store(restart),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    Store = beamai_pause_store_dets:handle(Name),
    Snap = snapshot(<<"c1">>, needs_approval),
    ok = beamai_pause_store:pause_save(Store, <<"c1">>, Snap),
    %% 进程停掉（相当于节点重启）
    ok = beamai_pause_store_dets:stop(Name),
    ?assertEqual(undefined, whereis(Name)),
    %% 同一个文件再开：快照原样还在
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    ?assertEqual({ok, Snap}, beamai_pause_store:pause_load(Store, <<"c1">>)),
    cleanup(Name, File).

%% 清除也要落盘：重启后不能"复活"一个已经恢复过的暂停
clear_survives_restart_test() ->
    {Name, File} = fresh_store(clear_restart),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    Store = beamai_pause_store_dets:handle(Name),
    ok = beamai_pause_store:pause_save(Store, <<"c1">>, snapshot(<<"c1">>, r)),
    ok = beamai_pause_store:pause_clear(Store, <<"c1">>),
    ok = beamai_pause_store_dets:stop(Name),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    ?assertEqual(none, beamai_pause_store:pause_load(Store, <<"c1">>)),
    cleanup(Name, File).

%% 快照里的复杂结构（中断态、pending_tool）无损往返
complex_snapshot_roundtrip_test() ->
    {Name, File} = fresh_store(roundtrip),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    Store = beamai_pause_store_dets:handle(Name),
    Snap = #{version => 1,
             conversation_id => <<"c1">>,
             paused_at => 1700000000000,
             pause_reason => #{<<"question">> => <<"确认删除？"/utf8>>},
             pending_tool => #{id => <<"c1">>, type => <<"function">>,
                               function => #{name => <<"rm">>,
                                             arguments => <<"{\"p\":\"/\"}">>}},
             interrupt_state => #{status => interrupted,
                                  iteration => 3,
                                  messages => [#{role => user, content => <<"go">>},
                                               #{role => assistant, content => null,
                                                 tool_calls => [#{id => <<"c1">>}]}],
                                  saved_state => #{<<"todos">> => [<<"a">>, <<"b">>]}}},
    ok = beamai_pause_store:pause_save(Store, <<"c1">>, Snap),
    ok = beamai_pause_store_dets:stop(Name),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    ?assertEqual({ok, Snap}, beamai_pause_store:pause_load(Store, <<"c1">>)),
    cleanup(Name, File).

%%====================================================================
%% 清理：持久化带来的新问题
%%====================================================================

%% 被放弃的暂停不会自己消失（ETS 版进程一死就干净了），得能按时间清
prune_removes_stale_only_test() ->
    {Name, File} = fresh_store(prune),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    Store = beamai_pause_store_dets:handle(Name),
    Now = erlang:system_time(millisecond),
    ok = beamai_pause_store:pause_save(Store, <<"old">>,
                                       snapshot(<<"old">>, r, Now - 60000)),
    ok = beamai_pause_store:pause_save(Store, <<"fresh">>,
                                       snapshot(<<"fresh">>, r, Now - 1000)),
    %% 清掉 10 秒前的
    ?assertEqual({ok, 1}, beamai_pause_store_dets:prune(Name, 10000)),
    ?assertEqual(none, beamai_pause_store:pause_load(Store, <<"old">>)),
    ?assertMatch({ok, _}, beamai_pause_store:pause_load(Store, <<"fresh">>)),
    ?assertEqual([<<"fresh">>], beamai_pause_store_dets:conversations(Name)),
    cleanup(Name, File).

%% 没有 paused_at 的快照当作陈旧：连什么时候存的都说不清，留着也判断不了
prune_drops_undated_test() ->
    {Name, File} = fresh_store(undated),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    Store = beamai_pause_store_dets:handle(Name),
    ok = beamai_pause_store:pause_save(Store, <<"x">>, #{version => 1}),
    ?assertEqual({ok, 1}, beamai_pause_store_dets:prune(Name, 3600000)),
    ?assertEqual([], beamai_pause_store_dets:conversations(Name)),
    cleanup(Name, File).

prune_on_empty_store_test() ->
    {Name, File} = fresh_store(prune_empty),
    {ok, _} = beamai_pause_store_dets:start_link(Name, #{file => File}),
    ?assertEqual({ok, 0}, beamai_pause_store_dets:prune(Name, 1000)),
    ?assertEqual([], beamai_pause_store_dets:conversations(Name)),
    cleanup(Name, File).
