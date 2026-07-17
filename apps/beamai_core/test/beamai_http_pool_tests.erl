%%%-------------------------------------------------------------------
%%% @doc beamai_http_pool 测试（Phase 2：三个命名池实例）
%%%
%%% 覆盖：
%%% - merge_config：模块默认值、旧键名 connection_timeout 归一化（旧键优先）
%%%   （注意：池实例不读 app env，env 解析测试见下方 sup 部分）
%%% - protocols 校验（非法协议 / 非列表报错）
%%% - stats 回显池名与生效配置
%%% - 未启动池的 get_connection 返回 {error, {pool_not_started, _}}
%%% - beamai_core_sup:resolve_pool_configs：三池默认一致、
%%%   遗留 http_pool env 统一应用到三池
%%%
%%% 测试用不注册名字的 gen_server:start 启动池实例，
%%% 避免与 supervisor 管理的实例冲突。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_pool_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 池实例配置（不涉及 app env）
%%====================================================================

merge_config_test_() ->
    [
     {"默认配置：protocols=[http]，connect_timeout=30s，无旧键残留",
      fun() ->
          Config = beamai_http_pool:merge_config(#{}),
          ?assertEqual([http], maps:get(protocols, Config)),
          ?assertEqual(30000, maps:get(connect_timeout, Config)),
          ?assertEqual(60000, maps:get(idle_timeout, Config)),
          ?assertEqual(10, maps:get(max_connections_per_host, Config)),
          ?assertNot(maps:is_key(connection_timeout, Config))
      end},

     {"旧键名 connection_timeout 归一化为 connect_timeout",
      fun() ->
          Config = beamai_http_pool:merge_config(#{connection_timeout => 5000}),
          ?assertEqual(5000, maps:get(connect_timeout, Config)),
          ?assertNot(maps:is_key(connection_timeout, Config))
      end},

     {"新旧键同时设置时旧键优先（保证遗留配置行为不变）",
      fun() ->
          Config = beamai_http_pool:merge_config(#{connection_timeout => 5000,
                                                   connect_timeout => 9000}),
          ?assertEqual(5000, maps:get(connect_timeout, Config))
      end},

     {"传入配置覆盖模块默认值",
      fun() ->
          Config = beamai_http_pool:merge_config(#{protocols => [http2, http],
                                                   max_connections_per_host => 50}),
          ?assertEqual([http2, http], maps:get(protocols, Config)),
          ?assertEqual(50, maps:get(max_connections_per_host, Config))
      end},

     {"非法 protocol 报错",
      fun() ->
          ?assertError({invalid_protocol, spdy},
                       beamai_http_pool:validate_protocols([http, spdy])),
          ?assertError({invalid_protocols, []},
                       beamai_http_pool:validate_protocols([])),
          ?assertError({invalid_protocols, http},
                       beamai_http_pool:validate_protocols(http)),
          ?assertEqual(ok, beamai_http_pool:validate_protocols([http])),
          ?assertEqual(ok, beamai_http_pool:validate_protocols([http2, http]))
      end},

     {"stats 回显池名与生效配置",
      fun() ->
          {ok, Pid} = gen_server:start(beamai_http_pool,
                                       [http_pool_stream,
                                        #{connection_timeout => 1234,
                                          protocols => [http2, http]}],
                                       []),
          try
              Stats = gen_server:call(Pid, stats),
              ?assertMatch(#{name := http_pool_stream,
                             total_connections := 0, in_use := 0, idle := 0},
                           Stats),
              Config = maps:get(config, Stats),
              ?assertEqual(1234, maps:get(connect_timeout, Config)),
              ?assertEqual([http2, http], maps:get(protocols, Config))
          after
              gen_server:stop(Pid)
          end
      end},

     {"未启动的池返回 pool_not_started",
      fun() ->
          ?assertEqual({error, {pool_not_started, http_pool_never_started_xyz}},
                       beamai_http_pool:get_connection(http_pool_never_started_xyz,
                                                       <<"https://example.com">>))
      end}
    ].

%%====================================================================
%% Supervisor 侧配置解析（涉及 app env，fixture 保存/恢复）
%%====================================================================

resolve_pool_configs_test_() ->
    {foreach,
     fun() -> application:get_env(beamai_core, http_pool) end,
     fun(Saved) ->
         case Saved of
             undefined -> application:unset_env(beamai_core, http_pool);
             {ok, V} -> application:set_env(beamai_core, http_pool, V)
         end
     end,
     [
      {"无 env 时三个池均为默认配置（等于拆分前单池默认）",
       fun() ->
           application:unset_env(beamai_core, http_pool),
           Pools = beamai_core_sup:resolve_pool_configs(),
           ?assertEqual([http_pool_longpoll, http_pool_short, http_pool_stream],
                        lists:sort(maps:keys(Pools))),
           Expected = #{max_connections_per_host => 10,
                        connect_timeout => 30000,
                        idle_timeout => 60000,
                        protocols => [http]},
           maps:foreach(fun(_Name, Cfg) -> ?assertEqual(Expected, Cfg) end, Pools)
       end},

      {"遗留 http_pool env 统一应用到三个池（含旧键名，池 init 时归一化）",
       fun() ->
           application:set_env(beamai_core, http_pool,
                               #{max_connections_per_host => 50,
                                 connection_timeout => 5000}),
           Pools = beamai_core_sup:resolve_pool_configs(),
           maps:foreach(fun(_Name, Cfg) ->
               ?assertEqual(50, maps:get(max_connections_per_host, Cfg)),
               %% sup 只做 merge，旧键名归一化由池的 merge_config 负责
               Merged = beamai_http_pool:merge_config(Cfg),
               ?assertEqual(5000, maps:get(connect_timeout, Merged))
           end, Pools)
       end},

      {"遗留 env 可配置 protocols（不再硬编码）",
       fun() ->
           application:set_env(beamai_core, http_pool,
                               #{protocols => [http2, http]}),
           Pools = beamai_core_sup:resolve_pool_configs(),
           maps:foreach(fun(_Name, Cfg) ->
               ?assertEqual([http2, http], maps:get(protocols, Cfg))
           end, Pools)
       end}
     ]}.
