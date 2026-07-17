%%%-------------------------------------------------------------------
%%% @doc 连接池路由测试
%%%
%%% 覆盖 beamai_llm_http_client:maybe_inject_pool/2：
%%% - Gun 后端下按请求形态注入池名（chat/stream/async_poll 路由表）
%%% - 非 Gun 后端（如 Hackney）不注入，且移除已有 pool 键——
%%%   防止 Gun 池名泄漏为从未启动的 hackney 池名
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_pool_routing_tests).

-include_lib("eunit/include/eunit.hrl").

routing_test_() ->
    {foreach,
     fun() -> beamai_http:get_backend() end,
     fun(SavedBackend) -> beamai_http:set_backend(SavedBackend) end,
     [
      {"Gun 后端：chat -> http_pool_short",
       fun() ->
           beamai_http:set_backend(beamai_http_gun),
           Opts = beamai_llm_http_client:maybe_inject_pool(chat, #{timeout => 1}),
           ?assertEqual(http_pool_short, maps:get(pool, Opts)),
           ?assertEqual(1, maps:get(timeout, Opts))
       end},

      {"Gun 后端：stream -> http_pool_stream",
       fun() ->
           beamai_http:set_backend(beamai_http_gun),
           Opts = beamai_llm_http_client:maybe_inject_pool(stream, #{}),
           ?assertEqual(http_pool_stream, maps:get(pool, Opts))
       end},

      {"Gun 后端：async_poll -> http_pool_longpoll",
       fun() ->
           beamai_http:set_backend(beamai_http_gun),
           Opts = beamai_llm_http_client:maybe_inject_pool(async_poll, #{}),
           ?assertEqual(http_pool_longpoll, maps:get(pool, Opts))
       end},

      {"Hackney 后端：不注入 pool，且移除已有 pool 键",
       fun() ->
           beamai_http:set_backend(beamai_http_hackney),
           Opts = beamai_llm_http_client:maybe_inject_pool(
                      chat, #{timeout => 1, pool => http_pool_short}),
           ?assertNot(maps:is_key(pool, Opts)),
           ?assertEqual(1, maps:get(timeout, Opts))
       end},

      {"其他后端（如测试 fake）：同样不注入",
       fun() ->
           beamai_http:set_backend(beamai_llm_fake_backend),
           Opts = beamai_llm_http_client:maybe_inject_pool(stream, #{}),
           ?assertNot(maps:is_key(pool, Opts))
       end}
     ]}.
