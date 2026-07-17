%%%-------------------------------------------------------------------
%%% @doc 连接池路由测试
%%%
%%% 覆盖 beamai_llm_http_client:maybe_inject_pool/3：
%%% - Gun 后端下按请求形态注入默认池名（chat/stream/async_poll 路由表）
%%% - 调用方在 Opts 显式指定 pool 时优先，且任何后端下原样透传
%%%   （显式指定视为调用方对后端知情）
%%% - 未显式指定时非 Gun 后端（如测试 fake）不注入，且移除已有 pool 键——
%%%   防止 Gun 专有的池名自动泄漏给按别的语义解释它的后端
%%%
%%% 以及 Phase 4 的两级覆盖入口：
%%% - beamai_llm_provider_common:with_pool_opt/2（provider Config 的 pool）
%%% - 端到端：provider Config 带 pool 时，经 chat/stream 全链路透传到后端 Opts
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_pool_routing_tests).

-include_lib("eunit/include/eunit.hrl").

routing_test_() ->
    {foreach,
     fun() -> beamai_http:get_backend() end,
     fun(SavedBackend) -> beamai_http:set_backend(SavedBackend) end,
     [
      {"Gun 后端：chat -> http_pool_short（默认路由）",
       fun() ->
           beamai_http:set_backend(beamai_http_gun),
           Opts = beamai_llm_http_client:maybe_inject_pool(chat, #{}, #{timeout => 1}),
           ?assertEqual(http_pool_short, maps:get(pool, Opts)),
           ?assertEqual(1, maps:get(timeout, Opts))
       end},

      {"Gun 后端：stream -> http_pool_stream（默认路由）",
       fun() ->
           beamai_http:set_backend(beamai_http_gun),
           Opts = beamai_llm_http_client:maybe_inject_pool(stream, #{}, #{}),
           ?assertEqual(http_pool_stream, maps:get(pool, Opts))
       end},

      {"Gun 后端：async_poll -> http_pool_longpoll（默认路由）",
       fun() ->
           beamai_http:set_backend(beamai_http_gun),
           Opts = beamai_llm_http_client:maybe_inject_pool(async_poll, #{}, #{}),
           ?assertEqual(http_pool_longpoll, maps:get(pool, Opts))
       end},

      {"Gun 后端：Opts 显式指定 pool 覆盖默认路由",
       fun() ->
           beamai_http:set_backend(beamai_http_gun),
           Opts = beamai_llm_http_client:maybe_inject_pool(
                      chat, #{pool => http_pool_stream}, #{}),
           ?assertEqual(http_pool_stream, maps:get(pool, Opts))
       end},

      {"非 Gun 后端：未显式指定时不注入，且移除已有 pool 键",
       fun() ->
           beamai_http:set_backend(beamai_llm_fake_backend),
           Opts = beamai_llm_http_client:maybe_inject_pool(
                      chat, #{}, #{timeout => 1, pool => http_pool_short}),
           ?assertNot(maps:is_key(pool, Opts)),
           ?assertEqual(1, maps:get(timeout, Opts))
       end},

      {"非 Gun 后端：显式指定 pool 时原样透传（由该后端自行解释池名）",
       fun() ->
           beamai_http:set_backend(beamai_llm_fake_backend),
           Opts = beamai_llm_http_client:maybe_inject_pool(
                      chat, #{pool => my_own_pool}, #{}),
           ?assertEqual(my_own_pool, maps:get(pool, Opts))
       end},

      {"非 Gun 后端：stream 形态未显式指定时同样不注入",
       fun() ->
           beamai_http:set_backend(beamai_llm_fake_backend),
           Opts = beamai_llm_http_client:maybe_inject_pool(stream, #{}, #{}),
           ?assertNot(maps:is_key(pool, Opts))
       end}
     ]}.

%%====================================================================
%% Provider 级 pool 配置
%%====================================================================

with_pool_opt_test_() ->
    [
     {"Config 带 pool 时并入 Opts",
      fun() ->
          Opts = beamai_llm_provider_common:with_pool_opt(
                     #{timeout => 1}, #{pool => http_pool_stream}),
          ?assertEqual(http_pool_stream, maps:get(pool, Opts)),
          ?assertEqual(1, maps:get(timeout, Opts))
      end},
     {"Config 不带 pool 时 Opts 原样返回",
      fun() ->
          Opts = beamai_llm_provider_common:with_pool_opt(#{timeout => 1}, #{}),
          ?assertEqual(#{timeout => 1}, Opts)
      end}
    ].

%%====================================================================
%% 端到端：provider Config 的 pool 全链路透传到 HTTP 后端
%%====================================================================

provider_pool_e2e_test_() ->
    {foreach,
     fun() -> beamai_llm_fake_backend:install() end,
     fun(Prev) -> beamai_llm_fake_backend:uninstall(Prev) end,
     [
      {"chat：Config 带 pool 时透传到后端 Opts",
       fun() ->
           beamai_llm_fake_backend:set_response(#{<<"id">> => <<"x">>}, []),
           Config = #{api_key => <<"sk-test">>, model => <<"gpt-4">>,
                      pool => http_pool_stream},
           _ = beamai_llm_provider_openai:chat(Config, #{messages => []}),
           ?assertEqual(http_pool_stream,
                        maps:get(pool, beamai_llm_fake_backend:last_opts()))
       end},

      {"chat：Config 不带 pool 时后端 Opts 无 pool 键（fake 后端非 Gun，不自动注入）",
       fun() ->
           beamai_llm_fake_backend:set_response(#{<<"id">> => <<"x">>}, []),
           Config = #{api_key => <<"sk-test">>, model => <<"gpt-4">>},
           _ = beamai_llm_provider_openai:chat(Config, #{messages => []}),
           ?assertNot(maps:is_key(pool, beamai_llm_fake_backend:last_opts()))
       end},

      {"stream_chat：Config 带 pool 时透传到后端 Opts",
       fun() ->
           beamai_llm_fake_backend:set_stream([<<"data: [DONE]\n">>], []),
           Config = #{api_key => <<"sk-test">>, model => <<"gpt-4">>,
                      pool => http_pool_short},
           _ = beamai_llm_provider_openai:stream_chat(Config, #{messages => []},
                                                      fun(_) -> ok end),
           ?assertEqual(http_pool_short,
                        maps:get(pool, beamai_llm_fake_backend:last_opts()))
       end}
     ]}.
