%%%-------------------------------------------------------------------
%%% @doc HTTP 分池路由 live 集成测试（MiniMax via Anthropic 兼容接口）
%%%
%%% 需要环境变量 MINIMAX_API_KEY，未设置时整组跳过（返回空测试集，
%%% 不用 {skip, _}——eunit 会把它当 bad descriptor 取消掉导致套件报错）。
%%%
%%% 手动运行：
%%%   MINIMAX_API_KEY=xxx rebar3 eunit --module=beamai_http_pool_live_test
%%%
%%% 用真实 MiniMax 流量验证连接池按用途路由（Gun 后端）：
%%%   1. 同步 chat            -> http_pool_short（默认路由）
%%%   2. 流式 stream_chat     -> http_pool_stream（默认路由）
%%%   3. provider Config pool -> http_pool_longpoll（按 provider 覆盖）
%%%
%%% 断言用差分而非绝对值：每步前后抓三个池的 total_connections 快照，
%%% 目标池必须新增连接、其余池不得变化——这能证明流量真的进了预期的池
%%% 且没有串池，比只看响应内容严格。setup 先 close_all 三池，保证从
%%% 0 基线开始，也避免上游测试遗留连接在 60s idle 清理时把计数扰动。
%%%
%%% 建连偶发瞬态超时（gun await_up 的 TLS 握手抖动），每步带一次重试；
%%% 失败的建连不会计入池的 total_connections，重试不影响差分断言。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_pool_live_test).

-include_lib("eunit/include/eunit.hrl").

-define(POOLS, [http_pool_short, http_pool_stream, http_pool_longpoll]).
-define(MINIMAX_BASE_URL, <<"https://api.minimax.chat/anthropic">>).
-define(MINIMAX_MODEL, <<"MiniMax-M2">>).

pool_routing_live_test_() ->
    case os:getenv("MINIMAX_API_KEY") of
        false ->
            [];
        _ ->
            {setup, fun setup/0, fun(_) -> {timeout, 180, fun routing_flow/0} end}
    end.

%% @private 启动应用并清空三池（差分断言的 0 基线）
setup() ->
    {ok, _} = application:ensure_all_started(beamai_core),
    {ok, _} = application:ensure_all_started(beamai_llm),
    lists:foreach(fun(P) -> ok = beamai_http_pool:close_all(P) end, ?POOLS),
    ok.

%%====================================================================
%% Test
%%====================================================================

routing_flow() ->
    ApiKey = list_to_binary(os:getenv("MINIMAX_API_KEY")),
    Base = #{model => ?MINIMAX_MODEL, api_key => ApiKey,
             base_url => ?MINIMAX_BASE_URL},
    Msg = [#{role => user, content => <<"Reply with exactly one word: PONG">>}],
    LLM = beamai_chat_completion:create(anthropic, Base),

    %% 1) 同步 chat -> http_pool_short（默认路由）
    Snap0 = snap(),
    {ok, R1} = with_retry(fun() -> beamai_chat_completion:chat(LLM, Msg) end),
    assert_nonempty_content(R1),
    Snap1 = snap(),
    assert_only_grew(http_pool_short, Snap0, Snap1),

    %% 2) 流式 stream_chat -> http_pool_stream（默认路由）
    Counter = counters:new(1, []),
    {ok, _R2} = with_retry(fun() ->
        beamai_chat_completion:stream_chat(LLM, Msg,
            fun(_Event) -> counters:add(Counter, 1, 1) end)
    end),
    ?assert(counters:get(Counter, 1) > 0),
    Snap2 = snap(),
    assert_only_grew(http_pool_stream, Snap1, Snap2),

    %% 3) provider Config 的 pool 覆盖默认路由 -> http_pool_longpoll
    LLM3 = beamai_chat_completion:create(anthropic,
                                         Base#{pool => http_pool_longpoll}),
    {ok, R3} = with_retry(fun() -> beamai_chat_completion:chat(LLM3, Msg) end),
    assert_nonempty_content(R3),
    Snap3 = snap(),
    assert_only_grew(http_pool_longpoll, Snap2, Snap3).

%%====================================================================
%% Helpers
%%====================================================================

%% @private 三个池的连接总数快照
snap() ->
    [{P, maps:get(total_connections, beamai_http_pool:stats(P))} || P <- ?POOLS].

%% @private 断言：只有目标池的连接数增长，其余池不变
assert_only_grew(Target, Before, After) ->
    lists:foreach(fun(P) ->
        B = proplists:get_value(P, Before),
        A = proplists:get_value(P, After),
        case P =:= Target of
            true ->
                %% 失败时报出池名与前后计数
                ?assertEqual({P, B, grew, true}, {P, B, grew, A > B});
            false ->
                ?assertEqual({P, B}, {P, A})
        end
    end, ?POOLS).

%% @private 响应 content 非空（统一响应结构未受分池影响）
assert_nonempty_content(Resp) ->
    Content = maps:get(content, Resp, <<>>),
    ?assert(is_binary(Content) andalso byte_size(Content) > 0).

%% @private 瞬态错误重试一次（建连超时等；失败建连不计入池连接数）
with_retry(Fun) ->
    case Fun() of
        {ok, _} = Ok ->
            Ok;
        {error, Reason} ->
            ?debugFmt("live request transient error, retrying once: ~p", [Reason]),
            Fun()
    end.
