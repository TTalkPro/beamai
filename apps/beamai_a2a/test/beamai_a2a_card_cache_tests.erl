%%%-------------------------------------------------------------------
%%% @doc Agent Card 缓存单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_card_cache_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试设置
%%====================================================================

setup() ->
    %% 启动缓存（使用短 TTL 便于测试）
    {ok, Pid} = beamai_a2a_card_cache:start_link(#{
        default_ttl => 2,           %% 2 秒 TTL
        cleanup_interval => 100,    %% 100 毫秒清理间隔
        max_entries => 5
    }),
    Pid.

cleanup(Pid) ->
    %% 停止缓存
    case is_process_alive(Pid) of
        true -> beamai_a2a_card_cache:stop();
        false -> ok
    end.

%%====================================================================
%% 测试套件
%%====================================================================

cache_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"基本存取测试", fun test_basic_put_get/0},
        {"TTL 过期测试", fun test_ttl_expiration/0},
        {"自定义 TTL 测试", fun test_custom_ttl/0},
        {"失效测试", fun test_invalidate/0},
        {"清空缓存测试", fun test_clear/0},
        {"统计信息测试", fun test_stats/0},
        {"列出键测试", fun test_list_keys/0},
        {"最大条目测试", fun test_max_entries/0},
        {"URL 标准化测试", fun test_url_normalization/0},
        {"exists 函数测试", fun test_exists/0}
     ]}.

%%====================================================================
%% 基本功能测试
%%====================================================================

test_basic_put_get() ->
    Url = <<"https://agent.example.com">>,
    Card = #{name => <<"test-agent">>, description => <<"Test">>},

    %% 存储
    ok = beamai_a2a_card_cache:put(Url, Card),

    %% 查找
    {ok, Retrieved} = beamai_a2a_card_cache:lookup(Url),
    ?assertEqual(Card, Retrieved),

    ok.

test_ttl_expiration() ->
    Url = <<"https://expiring.example.com">>,
    Card = #{name => <<"expiring-agent">>},

    %% 存储（使用 1 秒 TTL）
    ok = beamai_a2a_card_cache:put(Url, Card, 1),

    %% 立即查找应该成功
    {ok, _} = beamai_a2a_card_cache:lookup(Url),

    %% 等待过期
    timer:sleep(1500),

    %% 应该已过期（expired）或被清理（not_found）
    Result = beamai_a2a_card_cache:lookup(Url),
    ?assert(Result =:= {error, expired} orelse Result =:= {error, not_found}),

    ok.

test_custom_ttl() ->
    Url = <<"https://long-ttl.example.com">>,
    Card = #{name => <<"long-lived">>},

    %% 存储（使用 10 秒 TTL）
    ok = beamai_a2a_card_cache:put(Url, Card, 10),

    %% 等待默认 TTL（2 秒）过期
    timer:sleep(2500),

    %% 应该仍然有效（因为自定义 TTL 是 10 秒）
    {ok, Retrieved} = beamai_a2a_card_cache:lookup(Url),
    ?assertEqual(Card, Retrieved),

    ok.

test_invalidate() ->
    Url = <<"https://to-invalidate.example.com">>,
    Card = #{name => <<"to-delete">>},

    %% 存储
    ok = beamai_a2a_card_cache:put(Url, Card),

    %% 验证存在
    {ok, _} = beamai_a2a_card_cache:lookup(Url),

    %% 失效
    ok = beamai_a2a_card_cache:invalidate(Url),

    %% 应该不存在
    {error, not_found} = beamai_a2a_card_cache:lookup(Url),

    ok.

test_clear() ->
    %% 存储多个条目
    ok = beamai_a2a_card_cache:put(<<"https://a.com">>, #{name => <<"a">>}),
    ok = beamai_a2a_card_cache:put(<<"https://b.com">>, #{name => <<"b">>}),
    ok = beamai_a2a_card_cache:put(<<"https://c.com">>, #{name => <<"c">>}),

    %% 验证存在
    ?assertEqual(3, length(beamai_a2a_card_cache:list_keys())),

    %% 清空
    ok = beamai_a2a_card_cache:clear(),

    %% 应该为空
    ?assertEqual(0, length(beamai_a2a_card_cache:list_keys())),

    ok.

test_stats() ->
    %% 存储一些条目
    ok = beamai_a2a_card_cache:put(<<"https://stats1.com">>, #{name => <<"s1">>}),
    ok = beamai_a2a_card_cache:put(<<"https://stats2.com">>, #{name => <<"s2">>}),

    %% 获取统计
    Stats = beamai_a2a_card_cache:stats(),

    ?assertEqual(2, maps:get(size, Stats)),
    ?assertEqual(2, maps:get(default_ttl, Stats)),  %% setup 中设置的
    ?assertEqual(5, maps:get(max_entries, Stats)),  %% setup 中设置的
    ?assert(maps:is_key(memory, Stats)),

    ok.

test_list_keys() ->
    %% 清空
    ok = beamai_a2a_card_cache:clear(),

    %% 存储
    Urls = [<<"https://key1.com">>, <<"https://key2.com">>, <<"https://key3.com">>],
    lists:foreach(fun(Url) ->
        ok = beamai_a2a_card_cache:put(Url, #{name => Url})
    end, Urls),

    %% 列出键
    Keys = beamai_a2a_card_cache:list_keys(),
    ?assertEqual(3, length(Keys)),

    %% 验证所有 URL 都在列表中
    lists:foreach(fun(Url) ->
        ?assert(lists:member(Url, Keys))
    end, Urls),

    ok.

test_max_entries() ->
    %% 清空
    ok = beamai_a2a_card_cache:clear(),

    %% 存储超过最大条目数（5 个）
    lists:foreach(fun(I) ->
        Url = list_to_binary("https://max" ++ integer_to_list(I) ++ ".com"),
        ok = beamai_a2a_card_cache:put(Url, #{name => Url}),
        timer:sleep(100)  %% 确保时间戳不同，以便驱逐最旧的
    end, lists:seq(1, 7)),

    %% 应该只有 5 个条目（最大限制）
    Stats = beamai_a2a_card_cache:stats(),
    ?assertEqual(5, maps:get(size, Stats)),

    %% 验证确实有 5 个条目
    Keys = beamai_a2a_card_cache:list_keys(),
    ?assertEqual(5, length(Keys)),

    ok.

test_url_normalization() ->
    Card = #{name => <<"normalized">>},

    %% 使用带尾部斜杠的 URL 存储
    ok = beamai_a2a_card_cache:put(<<"https://normalized.com/">>, Card),

    %% 使用不带尾部斜杠的 URL 查找
    {ok, Retrieved} = beamai_a2a_card_cache:lookup(<<"https://normalized.com">>),
    ?assertEqual(Card, Retrieved),

    %% 使用字符串 URL 查找
    {ok, Retrieved2} = beamai_a2a_card_cache:lookup("https://normalized.com"),
    ?assertEqual(Card, Retrieved2),

    ok.

test_exists() ->
    Url = <<"https://exists.example.com">>,
    Card = #{name => <<"exists">>},

    %% 不存在
    ?assertEqual(false, beamai_a2a_card_cache:exists(Url)),

    %% 存储
    ok = beamai_a2a_card_cache:put(Url, Card),

    %% 存在
    ?assertEqual(true, beamai_a2a_card_cache:exists(Url)),

    %% 失效后
    ok = beamai_a2a_card_cache:invalidate(Url),
    ?assertEqual(false, beamai_a2a_card_cache:exists(Url)),

    ok.

%%====================================================================
%% 自动清理测试
%%====================================================================

cleanup_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = beamai_a2a_card_cache:start_link(#{
             default_ttl => 1,
             cleanup_interval => 200,
             max_entries => 100
         }),
         Pid
     end,
     fun(Pid) ->
         case is_process_alive(Pid) of
             true -> beamai_a2a_card_cache:stop();
             false -> ok
         end
     end,
     [
        {"自动清理过期条目", fun test_auto_cleanup/0}
     ]}.

test_auto_cleanup() ->
    %% 存储多个条目（TTL 为 1 秒）
    lists:foreach(fun(I) ->
        Url = list_to_binary("https://cleanup" ++ integer_to_list(I) ++ ".com"),
        ok = beamai_a2a_card_cache:put(Url, #{name => Url})
    end, lists:seq(1, 3)),

    %% 验证存在
    InitialSize = maps:get(size, beamai_a2a_card_cache:stats()),
    ?assertEqual(3, InitialSize),

    %% 等待过期（TTL=1秒）+ 清理间隔（200ms）+ 缓冲
    timer:sleep(1800),

    %% 条目应该已被自动清理（或者在下次 lookup 时清理）
    %% 由于 cleanup 是异步的，我们可能需要触发一次查找
    _ = beamai_a2a_card_cache:lookup(<<"https://cleanup1.com">>),
    _ = beamai_a2a_card_cache:lookup(<<"https://cleanup2.com">>),
    _ = beamai_a2a_card_cache:lookup(<<"https://cleanup3.com">>),

    %% 现在应该为空
    FinalSize = maps:get(size, beamai_a2a_card_cache:stats()),
    ?assertEqual(0, FinalSize),

    ok.
