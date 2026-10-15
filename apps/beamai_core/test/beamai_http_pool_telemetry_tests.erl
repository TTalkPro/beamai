%%%-------------------------------------------------------------------
%%% @doc HTTP Pool telemetry 埋点测试
%%%
%%% 用假连接进程注入池状态，避免测试依赖外部 HTTP 服务；被注入的 pid 仍必须
%%% 存活，池的存活检查才允许复用。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_pool_telemetry_tests).

-include_lib("eunit/include/eunit.hrl").

%% 与 beamai_http_pool 的 record 布局保持同步；仅测试状态注入使用。
-type state_name() :: atom().
-type state_map() :: map().
-type pool_config() :: map().

-record(state, {name :: state_name(),
                pools = #{} :: state_map(),
                conn_info = #{} :: state_map(),
                in_use = #{} :: state_map(),
                config = #{} :: pool_config()}).

telemetry_test_() ->
    {foreach, fun setup/0, fun(_) -> reset() end, [
        fun acquired_and_reused_share_one_connection/0,
        fun exhausted_host_reports_wait_timeout/0,
        fun known_connection_failure_is_reported/0,
        fun idle_reaping_is_reported/0
    ]}.

acquired_and_reused_share_one_connection() ->
    install_collector(self()),
    Pool = pool_name(),
    {ok, PoolPid} = start_pool(Pool, #{}),
    Fake = spawn(fun() -> timer:sleep(2000) end),
    ok = inject_connection(PoolPid, Fake, #{}),
    ?assertEqual({ok, Fake},
                 beamai_http_pool:get_connection(Pool, <<"http://test.local">>)),
    Names = names(2),
    %% `reused` 在 pool 进程内、`acquired` 在 caller 返回后发布。
    ?assertEqual([reused, acquired], Names),
    ?assert(whereis(Pool) =/= undefined).

exhausted_host_reports_wait_timeout() ->
    install_collector(self()),
    Pool = pool_name(),
    {ok, PoolPid} = start_pool(Pool, #{max_connections_per_host => 1}),
    Fake = spawn(fun() -> timer:sleep(2000) end),
    ok = inject_connection(PoolPid, Fake, #{Fake => true}),
    ?assertEqual({error, pool_exhausted},
                 beamai_http_pool:get_connection(Pool, <<"http://test.local">>)),
    ?assertEqual([wait_timeout], names(1)),
    ?assert(whereis(Pool) =/= undefined).

known_connection_failure_is_reported() ->
    install_collector(self()),
    Pool = pool_name(),
    {ok, PoolPid} = start_pool(Pool, #{}),
    Fake = spawn(fun() -> timer:sleep(2000) end),
    ok = inject_connection(PoolPid, Fake, #{}),
    ok = beamai_http_pool:connection_failed(Pool, Fake),
    wait_name(failed),
    ?assert(whereis(Pool) =/= undefined).

idle_reaping_is_reported() ->
    install_collector(self()),
    Pool = pool_name(),
    {ok, PoolPid} = start_pool(Pool, #{}),
    Fake = spawn(fun() -> timer:sleep(2000) end),
    ok = inject_idle_expired_connection(PoolPid, Fake),
    PoolPid ! cleanup_idle,
    wait_name(idle_reaped),
    ?assert(whereis(Pool) =/= undefined).

pool_name() ->
    list_to_atom("beamai_http_pool_telemetry_" ++
                     integer_to_list(erlang:unique_integer([positive]))).

start_pool(Name, Config) ->
    gen_server:start({local, Name}, beamai_http_pool, [Name, Config], []).

inject_connection(PoolPid, ConnPid, InUse) ->
    Now = erlang:system_time(millisecond),
    inject_state(PoolPid, fun(State) ->
        State#state{
          pools = #{host_key() => [ConnPid]},
          conn_info = #{ConnPid => connection_info(host_key(), Now)},
          in_use = InUse}
    end).

inject_idle_expired_connection(PoolPid, ConnPid) ->
    Now = erlang:system_time(millisecond),
    inject_state(PoolPid, fun(State) ->
        State#state{
          pools = #{host_key() => [ConnPid]},
          conn_info = #{ConnPid => connection_info(host_key(), Now - 120000)},
          in_use = #{}}
    end).

inject_state(PoolPid, Fun) ->
    _ = sys:replace_state(PoolPid,
        fun(#state{} = State) -> Fun(State) end, 1000),
    ok.

host_key() ->
    <<"test.local:80:tcp">>.

connection_info(HostKey, LastUsed) ->
    #{host_key => HostKey,
      created_at => LastUsed,
      last_used => LastUsed}.

install_collector(Self) ->
    ok = beamai_telemetry:set_config(#{
        enabled => true,
        enabled_levels => all,
        handlers => [#{id => test,
                       handler => fun(Event) -> Self ! {telemetry, Event} end}]
    }).

names(0, Acc) -> lists:reverse(Acc);
names(N, Acc) ->
    receive
        {telemetry, #{name := [beamai, http_pool, Kind]}} ->
            names(N - 1, [Kind | Acc])
    after 500 ->
        erlang:error({telemetry_timeout,
                      {expected, N, got, lists:reverse(Acc)}})
    end.

names(N) -> names(N, []).

wait_name(Name) ->
    receive
        {telemetry, #{name := [beamai, http_pool, Name]}} ->
            ok;
        {telemetry, _} ->
            wait_name(Name)
    after 500 ->
        erlang:error({telemetry_timeout, Name})
    end.

setup() ->
    reset().

reset() ->
    application:unset_env(beamai_core, telemetry).
