%%%-------------------------------------------------------------------
%%% @doc beamai_subagent_manager 测试（spawn/await/result/list/kill/restart/drop/ttl）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_subagent_manager_tests).
-include_lib("eunit/include/eunit.hrl").

manager_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun spawn_await/0, fun result_lifecycle/0, fun list_by_owner/0,
      fun kill_running/0, fun restart_agent/0, fun drop_agent/0]}.

setup() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, Messages, _O) ->
        timer:sleep(100),
        {ok, #{content => <<"R:", (last_user(Messages))/binary>>, finish_reason => <<"stop">>}}
    end),
    fresh_manager(#{ttl => infinity}).

cleanup(Pid) ->
    unlink(Pid), exit(Pid, kill),
    meck:unload(beamai_chat_completion).

spawn_await() ->
    {ok, Id} = beamai_subagent_manager:spawn(spec(<<"hi">>)),
    ?assertEqual({ok, <<"R:hi">>}, beamai_subagent_manager:await(Id, 2000)).

result_lifecycle() ->
    {ok, Id} = beamai_subagent_manager:spawn(spec(<<"hi">>)),
    %% 还在跑（mock sleep 100ms）→ not_ready
    ?assertEqual({error, not_ready}, beamai_subagent_manager:result(Id)),
    {ok, _} = beamai_subagent_manager:await(Id, 2000),
    ?assertEqual({ok, {ok, <<"R:hi">>}}, beamai_subagent_manager:result(Id)).

list_by_owner() ->
    {ok, _} = beamai_subagent_manager:spawn((spec(<<"a">>))#{owner => alice}),
    {ok, _} = beamai_subagent_manager:spawn((spec(<<"b">>))#{owner => bob}),
    ?assertEqual(1, length(beamai_subagent_manager:list(alice))),
    ?assertEqual(2, length(beamai_subagent_manager:list())).

kill_running() ->
    {ok, Id} = beamai_subagent_manager:spawn(spec(<<"hi">>)),
    ok = beamai_subagent_manager:kill(Id),
    ?assertEqual({ok, {error, sub_agent_killed}}, beamai_subagent_manager:result(Id)).

restart_agent() ->
    {ok, Id} = beamai_subagent_manager:spawn(spec(<<"hi">>)),
    {ok, _} = beamai_subagent_manager:await(Id, 2000),
    {ok, Id} = beamai_subagent_manager:restart(Id),
    ?assertEqual({ok, <<"R:hi">>}, beamai_subagent_manager:await(Id, 2000)).

drop_agent() ->
    {ok, Id} = beamai_subagent_manager:spawn(spec(<<"hi">>)),
    {ok, _} = beamai_subagent_manager:await(Id, 2000),
    ok = beamai_subagent_manager:drop(Id),
    ?assertEqual({error, not_found}, beamai_subagent_manager:result(Id)).

%% TTL：完成条目在 ttl 后自动回收
ttl_auto_drop_test() ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) ->
        {ok, #{content => <<"x">>, finish_reason => <<"stop">>}}
    end),
    Pid = fresh_manager(#{ttl => 60}),
    try
        {ok, Id} = beamai_subagent_manager:spawn(spec(<<"hi">>)),
        {ok, _} = beamai_subagent_manager:await(Id, 2000),
        timer:sleep(180),   %% > ttl 60ms
        ?assertEqual({error, not_found}, beamai_subagent_manager:result(Id))
    after
        unlink(Pid), exit(Pid, kill),
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 辅助
%%====================================================================

spec(Prompt) -> #{subagent => #{llm => {mock, #{}}, memory => false}, prompt => Prompt}.

%% 杀掉现有单例（如有）并起一个全新的，返回其 pid（linked 到测试进程）
fresh_manager(Opts) ->
    case whereis(beamai_subagent_manager) of
        undefined -> ok;
        P ->
            Mon = erlang:monitor(process, P),
            catch unlink(P),
            exit(P, kill),
            receive {'DOWN', Mon, process, P, _} -> ok after 2000 -> ok end
    end,
    {ok, Pid} = beamai_subagent_manager:start_link(Opts),
    Pid.

last_user(Messages) ->
    Cs = [maps:get(content, M, <<>>) || M <- Messages,
          maps:get(role, M, undefined) =:= user,
          is_binary(maps:get(content, M, <<>>))],
    case Cs of [] -> <<>>; _ -> lists:last(Cs) end.
