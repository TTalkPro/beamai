%%%-------------------------------------------------------------------
%%% @doc Rate Limiting 单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_rate_limit_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试设置
%%====================================================================

setup() ->
    %% 启动限流服务
    {ok, Pid} = beamai_a2a_rate_limit:start_link(#{}),
    Pid.

cleanup(Pid) ->
    %% 停止服务
    case is_process_alive(Pid) of
        true -> beamai_a2a_rate_limit:stop();
        false -> ok
    end.

%%====================================================================
%% Token Bucket 测试套件
%%====================================================================

token_bucket_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"基本限流检查", fun test_basic_check/0},
        {"检查并消费令牌", fun test_check_and_consume/0},
        {"连续请求消耗令牌", fun test_continuous_requests/0},
        {"令牌补充", fun test_token_refill/0},
        {"超过限制被拒绝", fun test_rate_limited/0},
        {"突发请求支持", fun test_burst/0}
     ]}.

test_basic_check() ->
    Key = <<"test-key-1">>,

    %% 首次检查应该允许
    {ok, Info} = beamai_a2a_rate_limit:check(Key),

    ?assert(maps:is_key(remaining, Info)),
    ?assert(maps:is_key(limit, Info)),
    ?assert(maps:is_key(reset_at, Info)),
    ?assert(maps:get(remaining, Info) > 0),

    ok.

test_check_and_consume() ->
    Key = <<"test-key-2">>,

    %% 获取初始状态
    {ok, Info1} = beamai_a2a_rate_limit:check(Key),
    Remaining1 = maps:get(remaining, Info1),

    %% 消费一个令牌
    {ok, Info2} = beamai_a2a_rate_limit:check_and_consume(Key),
    Remaining2 = maps:get(remaining, Info2),

    %% 剩余令牌应该减少
    ?assert(Remaining2 < Remaining1),

    ok.

test_continuous_requests() ->
    Key = <<"test-key-3">>,

    %% 连续发送多个请求
    Results = [beamai_a2a_rate_limit:check_and_consume(Key) || _ <- lists:seq(1, 5)],

    %% 所有请求都应该成功（在突发限制内）
    AllOk = lists:all(fun({ok, _}) -> true; (_) -> false end, Results),
    ?assert(AllOk),

    %% 检查令牌在递减
    Remainings = [maps:get(remaining, Info) || {ok, Info} <- Results],
    IsSorted = Remainings =:= lists:sort(fun(A, B) -> A > B end, Remainings),
    ?assert(IsSorted),

    ok.

test_token_refill() ->
    Key = <<"test-key-4">>,

    %% 消费一些令牌
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(Key),
    {ok, Info1} = beamai_a2a_rate_limit:check(Key),
    Remaining1 = maps:get(remaining, Info1),

    %% 等待一小段时间让令牌补充
    timer:sleep(100),

    %% 再次检查，令牌应该略有增加
    {ok, Info2} = beamai_a2a_rate_limit:check(Key),
    Remaining2 = maps:get(remaining, Info2),

    %% 令牌应该增加（或至少不减少，因为 Token Bucket 会补充）
    ?assert(Remaining2 >= Remaining1),

    ok.

test_rate_limited() ->
    Key = <<"test-key-5">>,

    %% 设置非常低的限制（固定窗口更容易测试）
    ok = beamai_a2a_rate_limit:set_rule(Key, #{
        requests_per_minute => 3,
        burst_size => 0,
        algorithm => fixed_window
    }),

    %% 快速消耗所有配额
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(Key),
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(Key),
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(Key),

    %% 下一个请求应该被限流
    Result = beamai_a2a_rate_limit:check_and_consume(Key),

    ?assertMatch({error, rate_limited, _}, Result),
    {error, rate_limited, Info} = Result,
    ?assert(maps:is_key(retry_after, Info)),
    ?assertEqual(0, maps:get(remaining, Info)),

    ok.

test_burst() ->
    Key = <<"test-key-6">>,

    %% 设置允许突发的规则
    ok = beamai_a2a_rate_limit:set_rule(Key, #{
        requests_per_minute => 10,
        burst_size => 5
    }),

    %% 突发发送 5 个请求应该全部成功
    Results = [beamai_a2a_rate_limit:check_and_consume(Key) || _ <- lists:seq(1, 5)],
    AllOk = lists:all(fun({ok, _}) -> true; (_) -> false end, Results),
    ?assert(AllOk),

    ok.

%%====================================================================
%% Fixed Window 测试套件
%%====================================================================

fixed_window_test_() ->
    {foreach,
     fun() ->
         {ok, Pid} = beamai_a2a_rate_limit:start_link(#{algorithm => fixed_window}),
         Pid
     end,
     fun cleanup/1,
     [
        {"Fixed Window 基本测试", fun test_fixed_window_basic/0},
        {"Fixed Window 窗口重置", fun test_fixed_window_reset/0}
     ]}.

test_fixed_window_basic() ->
    Key = <<"fixed-key-1">>,

    %% 设置固定窗口规则
    ok = beamai_a2a_rate_limit:set_rule(Key, #{
        requests_per_minute => 5,
        algorithm => fixed_window
    }),

    %% 发送请求
    {ok, Info} = beamai_a2a_rate_limit:check_and_consume(Key),

    ?assertEqual(fixed_window, maps:get(algorithm, Info)),
    ?assertEqual(4, maps:get(remaining, Info)),

    ok.

test_fixed_window_reset() ->
    Key = <<"fixed-key-2">>,

    %% 设置固定窗口规则
    ok = beamai_a2a_rate_limit:set_rule(Key, #{
        requests_per_minute => 100,
        algorithm => fixed_window
    }),

    %% 发送几个请求
    {ok, Info1} = beamai_a2a_rate_limit:check_and_consume(Key),
    {ok, Info2} = beamai_a2a_rate_limit:check_and_consume(Key),

    %% 检查 reset_at 一致（同一窗口）
    ResetAt1 = maps:get(reset_at, Info1),
    ResetAt2 = maps:get(reset_at, Info2),
    ?assertEqual(ResetAt1, ResetAt2),

    ok.

%%====================================================================
%% Sliding Window 测试套件
%%====================================================================

sliding_window_test_() ->
    {foreach,
     fun() ->
         {ok, Pid} = beamai_a2a_rate_limit:start_link(#{algorithm => sliding_window}),
         Pid
     end,
     fun cleanup/1,
     [
        {"Sliding Window 基本测试", fun test_sliding_window_basic/0}
     ]}.

test_sliding_window_basic() ->
    Key = <<"sliding-key-1">>,

    %% 设置滑动窗口规则
    ok = beamai_a2a_rate_limit:set_rule(Key, #{
        requests_per_minute => 10,
        algorithm => sliding_window
    }),

    %% 发送请求
    {ok, Info} = beamai_a2a_rate_limit:check_and_consume(Key),

    ?assertEqual(sliding_window, maps:get(algorithm, Info)),
    ?assert(maps:get(remaining, Info) > 0),

    ok.

%%====================================================================
%% 规则管理测试套件
%%====================================================================

rule_management_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"设置自定义规则", fun test_set_rule/0},
        {"获取规则", fun test_get_rule/0},
        {"删除规则", fun test_remove_rule/0},
        {"列出所有规则", fun test_list_rules/0}
     ]}.

test_set_rule() ->
    Key = <<"rule-key-1">>,
    Rule = #{
        requests_per_minute => 100,
        requests_per_hour => 5000,
        burst_size => 20
    },

    ok = beamai_a2a_rate_limit:set_rule(Key, Rule),

    {ok, StoredRule} = beamai_a2a_rate_limit:get_rule(Key),
    ?assertEqual(100, maps:get(requests_per_minute, StoredRule)),
    ?assertEqual(5000, maps:get(requests_per_hour, StoredRule)),
    ?assertEqual(20, maps:get(burst_size, StoredRule)),

    ok.

test_get_rule() ->
    Key = <<"rule-key-2">>,

    %% 未设置规则
    {error, not_found} = beamai_a2a_rate_limit:get_rule(Key),

    %% 设置规则
    ok = beamai_a2a_rate_limit:set_rule(Key, #{requests_per_minute => 50}),

    %% 获取规则
    {ok, Rule} = beamai_a2a_rate_limit:get_rule(Key),
    ?assertEqual(50, maps:get(requests_per_minute, Rule)),

    ok.

test_remove_rule() ->
    Key = <<"rule-key-3">>,

    %% 设置规则
    ok = beamai_a2a_rate_limit:set_rule(Key, #{requests_per_minute => 50}),
    {ok, _} = beamai_a2a_rate_limit:get_rule(Key),

    %% 删除规则
    ok = beamai_a2a_rate_limit:remove_rule(Key),

    %% 验证已删除
    {error, not_found} = beamai_a2a_rate_limit:get_rule(Key),

    ok.

test_list_rules() ->
    %% 添加多个规则
    ok = beamai_a2a_rate_limit:set_rule(<<"list-key-1">>, #{requests_per_minute => 10}),
    ok = beamai_a2a_rate_limit:set_rule(<<"list-key-2">>, #{requests_per_minute => 20}),
    ok = beamai_a2a_rate_limit:set_rule(<<"list-key-3">>, #{requests_per_minute => 30}),

    %% 列出规则
    Rules = beamai_a2a_rate_limit:list_rules(),

    ?assert(length(Rules) >= 3),

    ok.

%%====================================================================
%% 配置测试套件
%%====================================================================

config_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"获取默认配置", fun test_default_config/0},
        {"设置配置", fun test_set_config/0},
        {"检查是否启用", fun test_is_enabled/0},
        {"禁用限流", fun test_disabled/0}
     ]}.

test_default_config() ->
    Config = beamai_a2a_rate_limit:get_config(),

    ?assertEqual(true, maps:get(enabled, Config)),
    ?assertEqual(60, maps:get(default_rpm, Config)),
    ?assertEqual(1000, maps:get(default_rph, Config)),
    ?assertEqual(10, maps:get(default_burst, Config)),
    ?assertEqual(token_bucket, maps:get(algorithm, Config)),

    ok.

test_set_config() ->
    %% 设置新配置
    ok = beamai_a2a_rate_limit:set_config(#{
        default_rpm => 120,
        default_burst => 20
    }),

    Config = beamai_a2a_rate_limit:get_config(),
    ?assertEqual(120, maps:get(default_rpm, Config)),
    ?assertEqual(20, maps:get(default_burst, Config)),

    ok.

test_is_enabled() ->
    ?assertEqual(true, beamai_a2a_rate_limit:is_enabled()),

    ok = beamai_a2a_rate_limit:set_config(#{enabled => false}),
    ?assertEqual(false, beamai_a2a_rate_limit:is_enabled()),

    ok = beamai_a2a_rate_limit:set_config(#{enabled => true}),
    ?assertEqual(true, beamai_a2a_rate_limit:is_enabled()),

    ok.

test_disabled() ->
    %% 禁用限流
    ok = beamai_a2a_rate_limit:set_config(#{enabled => false}),

    %% 任何请求都应该被允许
    Key = <<"disabled-key">>,
    {ok, Info} = beamai_a2a_rate_limit:check_and_consume(Key),

    ?assertEqual(infinity, maps:get(remaining, Info)),
    ?assertEqual(false, maps:get(enabled, Info)),

    %% 恢复配置
    ok = beamai_a2a_rate_limit:set_config(#{enabled => true}),

    ok.

%%====================================================================
%% 统计和管理测试套件
%%====================================================================

stats_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"统计信息", fun test_stats/0},
        {"重置单个 Key", fun test_reset_key/0},
        {"重置所有", fun test_reset_all/0}
     ]}.

test_stats() ->
    %% 发送一些请求
    Key = <<"stats-key">>,
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(Key),
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(Key),

    %% 获取统计
    Stats = beamai_a2a_rate_limit:stats(),

    ?assert(maps:is_key(checks, Stats)),
    ?assert(maps:is_key(allowed, Stats)),
    ?assert(maps:is_key(rejected, Stats)),
    ?assert(maps:is_key(buckets_count, Stats)),
    ?assert(maps:is_key(rules_count, Stats)),

    ?assert(maps:get(checks, Stats) >= 2),
    ?assert(maps:get(allowed, Stats) >= 2),

    ok.

test_reset_key() ->
    Key = <<"reset-key-1">>,

    %% 设置低限制并消耗（使用固定窗口）
    ok = beamai_a2a_rate_limit:set_rule(Key, #{
        requests_per_minute => 2,
        burst_size => 0,
        algorithm => fixed_window
    }),
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(Key),
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(Key),

    %% 此时应该被限流
    ?assertMatch({error, rate_limited, _}, beamai_a2a_rate_limit:check_and_consume(Key)),

    %% 重置
    ok = beamai_a2a_rate_limit:reset(Key),

    %% 应该又可以请求了
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(Key),

    ok.

test_reset_all() ->
    %% 创建多个桶
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(<<"reset-all-1">>),
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(<<"reset-all-2">>),
    {ok, _} = beamai_a2a_rate_limit:check_and_consume(<<"reset-all-3">>),

    Stats1 = beamai_a2a_rate_limit:stats(),
    ?assert(maps:get(buckets_count, Stats1) >= 3),

    %% 重置所有
    ok = beamai_a2a_rate_limit:reset_all(),

    Stats2 = beamai_a2a_rate_limit:stats(),
    ?assertEqual(0, maps:get(buckets_count, Stats2)),

    ok.

%%====================================================================
%% 消费成本测试
%%====================================================================

cost_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"自定义消费成本", fun test_custom_cost/0}
     ]}.

test_custom_cost() ->
    Key = <<"cost-key-1">>,

    %% 设置规则
    ok = beamai_a2a_rate_limit:set_rule(Key, #{
        requests_per_minute => 100,
        burst_size => 50
    }),

    %% 获取初始状态
    {ok, Info1} = beamai_a2a_rate_limit:check(Key),
    Remaining1 = maps:get(remaining, Info1),

    %% 消费 5 个令牌
    {ok, Info2} = beamai_a2a_rate_limit:check_and_consume(Key, #{cost => 5}),
    Remaining2 = maps:get(remaining, Info2),

    %% 应该减少约 5 个
    ?assert(Remaining1 - Remaining2 >= 4),
    ?assert(Remaining1 - Remaining2 =< 6),

    ok.
