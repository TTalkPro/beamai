%%%-------------------------------------------------------------------
%%% @doc A2A 限流模块
%%%
%%% 提供 API 限流功能，支持：
%%% - Token Bucket 算法（令牌桶）
%%% - Fixed Window 算法（固定窗口）
%%% - Sliding Window 算法（滑动窗口）
%%% - 多种限流维度（API Key、IP、全局）
%%% - 自定义限流规则
%%% - 自动清理过期数据
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 检查是否允许请求
%%% case beamai_a2a_rate_limit:check(<<"api-key-123">>) of
%%%     {ok, Info} ->
%%%         %% Info = #{remaining => 99, limit => 100, reset_at => ...}
%%%         process_request();
%%%     {error, rate_limited, Info} ->
%%%         %% Info = #{retry_after => 5000, ...}
%%%         reject_request(429)
%%% end.
%%%
%%% %% 检查并消费令牌
%%% case beamai_a2a_rate_limit:check_and_consume(<<"api-key-123">>) of
%%%     {ok, Info} -> process_request();
%%%     {error, rate_limited, Info} -> reject_request(429)
%%% end.
%%%
%%% %% 设置自定义限流规则
%%% ok = beamai_a2a_rate_limit:set_rule(<<"premium-key">>, #{
%%%     requests_per_minute => 1000,
%%%     requests_per_hour => 50000
%%% }).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_rate_limit).

-behaviour(gen_server).

%% API 导出
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% 限流检查
    check/1,
    check/2,
    check_and_consume/1,
    check_and_consume/2,

    %% 规则管理
    set_rule/2,
    get_rule/1,
    remove_rule/1,
    list_rules/0,

    %% 配置
    set_config/1,
    get_config/0,
    is_enabled/0,

    %% 统计和管理
    stats/0,
    reset/1,
    reset_all/0
]).

%% gen_server 回调
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% 常量定义
-define(SERVER, ?MODULE).
-define(BUCKET_TABLE, beamai_a2a_rate_buckets).
-define(RULE_TABLE, beamai_a2a_rate_rules).
-define(CLEANUP_INTERVAL, 60000).  %% 1 分钟清理一次

%% 默认限流配置
-define(DEFAULT_REQUESTS_PER_MINUTE, 60).
-define(DEFAULT_REQUESTS_PER_HOUR, 1000).
-define(DEFAULT_BURST_SIZE, 10).

%% 服务器状态
-record(state, {
    config :: map(),
    stats :: map(),
    cleanup_timer :: reference() | undefined
}).

%% 令牌桶记录
-record(bucket, {
    key :: binary(),                    %% 限流 Key
    tokens :: float(),                  %% 当前令牌数
    last_refill :: non_neg_integer(),   %% 上次补充时间（毫秒）
    window_start :: non_neg_integer(),  %% 窗口开始时间
    window_count :: non_neg_integer()   %% 窗口内请求数
}).

%% 限流规则记录
-record(rule, {
    key :: binary(),
    requests_per_minute :: pos_integer(),
    requests_per_hour :: pos_integer(),
    burst_size :: pos_integer(),
    algorithm :: token_bucket | fixed_window | sliding_window
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动限流服务（使用默认配置）
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc 启动限流服务
%% 配置选项：
%% - enabled: 是否启用限流（默认 true）
%% - default_rpm: 默认每分钟请求数（默认 60）
%% - default_rph: 默认每小时请求数（默认 1000）
%% - default_burst: 默认突发大小（默认 10）
%% - algorithm: 默认算法（token_bucket | fixed_window | sliding_window）
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc 停止限流服务
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc 检查是否允许请求（不消费令牌）
%% 返回当前限流状态
-spec check(binary()) -> {ok, map()} | {error, rate_limited, map()}.
check(Key) ->
    check(Key, #{}).

%% @doc 检查是否允许请求（带选项）
-spec check(binary(), map()) -> {ok, map()} | {error, rate_limited, map()}.
check(Key, Opts) ->
    gen_server:call(?SERVER, {check, Key, Opts}).

%% @doc 检查并消费令牌
%% 如果允许，消费一个令牌
-spec check_and_consume(binary()) -> {ok, map()} | {error, rate_limited, map()}.
check_and_consume(Key) ->
    check_and_consume(Key, #{}).

%% @doc 检查并消费令牌（带选项）
%% 选项：
%% - cost: 消费的令牌数（默认 1）
%% - scope: 限流范围 (key | ip | global)
-spec check_and_consume(binary(), map()) -> {ok, map()} | {error, rate_limited, map()}.
check_and_consume(Key, Opts) ->
    gen_server:call(?SERVER, {check_and_consume, Key, Opts}).

%% @doc 设置自定义限流规则
-spec set_rule(binary(), map()) -> ok | {error, term()}.
set_rule(Key, Rule) ->
    gen_server:call(?SERVER, {set_rule, Key, Rule}).

%% @doc 获取限流规则
-spec get_rule(binary()) -> {ok, map()} | {error, not_found}.
get_rule(Key) ->
    case ets:lookup(?RULE_TABLE, Key) of
        [Rule] -> {ok, rule_to_map(Rule)};
        [] -> {error, not_found}
    end.

%% @doc 删除限流规则
-spec remove_rule(binary()) -> ok.
remove_rule(Key) ->
    gen_server:call(?SERVER, {remove_rule, Key}).

%% @doc 列出所有自定义规则
-spec list_rules() -> [map()].
list_rules() ->
    ets:foldl(fun(Rule, Acc) ->
        [rule_to_map(Rule) | Acc]
    end, [], ?RULE_TABLE).

%% @doc 设置全局配置
-spec set_config(map()) -> ok.
set_config(Config) ->
    gen_server:call(?SERVER, {set_config, Config}).

%% @doc 获取当前配置
-spec get_config() -> map().
get_config() ->
    gen_server:call(?SERVER, get_config).

%% @doc 检查是否启用限流
-spec is_enabled() -> boolean().
is_enabled() ->
    gen_server:call(?SERVER, is_enabled).

%% @doc 获取统计信息
-spec stats() -> map().
stats() ->
    gen_server:call(?SERVER, stats).

%% @doc 重置指定 Key 的限流状态
-spec reset(binary()) -> ok.
reset(Key) ->
    gen_server:call(?SERVER, {reset, Key}).

%% @doc 重置所有限流状态
-spec reset_all() -> ok.
reset_all() ->
    gen_server:call(?SERVER, reset_all).

%%====================================================================
%% gen_server 回调
%%====================================================================

init(Config) ->
    %% 创建 ETS 表
    ets:new(?BUCKET_TABLE, [
        named_table, public, set,
        {keypos, #bucket.key},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    ets:new(?RULE_TABLE, [
        named_table, public, set,
        {keypos, #rule.key},
        {read_concurrency, true}
    ]),

    %% 合并默认配置
    DefaultConfig = #{
        enabled => true,
        default_rpm => ?DEFAULT_REQUESTS_PER_MINUTE,
        default_rph => ?DEFAULT_REQUESTS_PER_HOUR,
        default_burst => ?DEFAULT_BURST_SIZE,
        algorithm => token_bucket
    },
    MergedConfig = maps:merge(DefaultConfig, Config),

    %% 启动清理定时器
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),

    State = #state{
        config = MergedConfig,
        stats = #{
            checks => 0,
            allowed => 0,
            rejected => 0,
            rules_count => 0
        },
        cleanup_timer = Timer
    },
    {ok, State}.

handle_call({check, Key, Opts}, _From, State) ->
    Result = do_check(Key, Opts, false, State),
    NewStats = update_stats(Result, State#state.stats),
    {reply, Result, State#state{stats = NewStats}};

handle_call({check_and_consume, Key, Opts}, _From, State) ->
    Result = do_check(Key, Opts, true, State),
    NewStats = update_stats(Result, State#state.stats),
    {reply, Result, State#state{stats = NewStats}};

handle_call({set_rule, Key, RuleMap}, _From, State) ->
    Rule = #rule{
        key = Key,
        requests_per_minute = maps:get(requests_per_minute, RuleMap,
            maps:get(default_rpm, State#state.config)),
        requests_per_hour = maps:get(requests_per_hour, RuleMap,
            maps:get(default_rph, State#state.config)),
        burst_size = maps:get(burst_size, RuleMap,
            maps:get(default_burst, State#state.config)),
        algorithm = maps:get(algorithm, RuleMap,
            maps:get(algorithm, State#state.config))
    },
    ets:insert(?RULE_TABLE, Rule),
    BaseStats = State#state.stats,
    NewStats = BaseStats#{rules_count => ets:info(?RULE_TABLE, size)},
    {reply, ok, State#state{stats = NewStats}};

handle_call({remove_rule, Key}, _From, State) ->
    ets:delete(?RULE_TABLE, Key),
    ets:delete(?BUCKET_TABLE, Key),
    BaseStats = State#state.stats,
    NewStats = BaseStats#{rules_count => ets:info(?RULE_TABLE, size)},
    {reply, ok, State#state{stats = NewStats}};

handle_call({set_config, NewConfig}, _From, State) ->
    MergedConfig = maps:merge(State#state.config, NewConfig),
    {reply, ok, State#state{config = MergedConfig}};

handle_call(get_config, _From, State) ->
    {reply, State#state.config, State};

handle_call(is_enabled, _From, State) ->
    Enabled = maps:get(enabled, State#state.config, true),
    {reply, Enabled, State};

handle_call(stats, _From, State) ->
    BaseStats = State#state.stats,
    FullStats = BaseStats#{
        buckets_count => ets:info(?BUCKET_TABLE, size),
        rules_count => ets:info(?RULE_TABLE, size)
    },
    {reply, FullStats, State};

handle_call({reset, Key}, _From, State) ->
    ets:delete(?BUCKET_TABLE, Key),
    {reply, ok, State};

handle_call(reset_all, _From, State) ->
    ets:delete_all_objects(?BUCKET_TABLE),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% 清理过期的桶
    cleanup_expired_buckets(),

    %% 重新设置定时器
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    {noreply, State#state{cleanup_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% 取消定时器
    case State#state.cleanup_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 执行限流检查
do_check(Key, Opts, Consume, State) ->
    Config = State#state.config,
    case maps:get(enabled, Config, true) of
        false ->
            %% 限流未启用
            {ok, #{
                remaining => infinity,
                limit => infinity,
                enabled => false
            }};
        true ->
            %% 获取限流规则
            Rule = get_effective_rule(Key, Config),
            Algorithm = Rule#rule.algorithm,

            %% 根据算法执行检查
            case Algorithm of
                token_bucket ->
                    check_token_bucket(Key, Rule, Consume, Opts);
                fixed_window ->
                    check_fixed_window(Key, Rule, Consume, Opts);
                sliding_window ->
                    check_sliding_window(Key, Rule, Consume, Opts)
            end
    end.

%% @private 获取有效的限流规则
get_effective_rule(Key, Config) ->
    case ets:lookup(?RULE_TABLE, Key) of
        [Rule] -> Rule;
        [] ->
            %% 使用默认规则
            #rule{
                key = Key,
                requests_per_minute = maps:get(default_rpm, Config),
                requests_per_hour = maps:get(default_rph, Config),
                burst_size = maps:get(default_burst, Config),
                algorithm = maps:get(algorithm, Config)
            }
    end.

%% @private Token Bucket 算法
check_token_bucket(Key, Rule, Consume, Opts) ->
    Now = erlang:system_time(millisecond),
    Cost = maps:get(cost, Opts, 1),

    %% 获取或创建桶
    Bucket = get_or_create_bucket(Key, Rule, Now),

    %% 计算令牌补充
    RefillRate = Rule#rule.requests_per_minute / 60000,  %% 每毫秒补充的令牌
    TimePassed = Now - Bucket#bucket.last_refill,
    TokensToAdd = TimePassed * RefillRate,
    MaxTokens = Rule#rule.burst_size + Rule#rule.requests_per_minute / 60,  %% 最大令牌数

    NewTokens = min(MaxTokens, Bucket#bucket.tokens + TokensToAdd),

    %% 检查是否有足够令牌
    case NewTokens >= Cost of
        true ->
            %% 允许请求
            FinalTokens = case Consume of
                true -> NewTokens - Cost;
                false -> NewTokens
            end,
            UpdatedBucket = Bucket#bucket{
                tokens = FinalTokens,
                last_refill = Now
            },
            ets:insert(?BUCKET_TABLE, UpdatedBucket),

            ResetAt = calculate_reset_time(Now, Rule),
            {ok, #{
                remaining => trunc(FinalTokens),
                limit => Rule#rule.requests_per_minute,
                reset_at => ResetAt,
                algorithm => token_bucket
            }};
        false ->
            %% 拒绝请求
            RetryAfter = calculate_retry_after(Cost - NewTokens, RefillRate),
            UpdatedBucket = Bucket#bucket{
                tokens = NewTokens,
                last_refill = Now
            },
            ets:insert(?BUCKET_TABLE, UpdatedBucket),

            {error, rate_limited, #{
                remaining => 0,
                limit => Rule#rule.requests_per_minute,
                retry_after => RetryAfter,
                reset_at => Now + RetryAfter,
                algorithm => token_bucket
            }}
    end.

%% @private Fixed Window 算法
check_fixed_window(Key, Rule, Consume, Opts) ->
    Now = erlang:system_time(millisecond),
    Cost = maps:get(cost, Opts, 1),
    WindowSize = 60000,  %% 1 分钟窗口

    %% 获取或创建桶
    Bucket = get_or_create_bucket(Key, Rule, Now),

    %% 检查是否在同一窗口
    WindowStart = (Now div WindowSize) * WindowSize,
    {CurrentCount, NewWindowStart} = case Bucket#bucket.window_start of
        WS when WS < WindowStart ->
            %% 新窗口
            {0, WindowStart};
        _ ->
            %% 同一窗口
            {Bucket#bucket.window_count, Bucket#bucket.window_start}
    end,

    Limit = Rule#rule.requests_per_minute,
    case CurrentCount + Cost =< Limit of
        true ->
            %% 允许请求
            NewCount = case Consume of
                true -> CurrentCount + Cost;
                false -> CurrentCount
            end,
            UpdatedBucket = Bucket#bucket{
                window_start = NewWindowStart,
                window_count = NewCount,
                last_refill = Now
            },
            ets:insert(?BUCKET_TABLE, UpdatedBucket),

            ResetAt = NewWindowStart + WindowSize,
            {ok, #{
                remaining => Limit - NewCount,
                limit => Limit,
                reset_at => ResetAt,
                algorithm => fixed_window
            }};
        false ->
            %% 拒绝请求
            ResetAt = NewWindowStart + WindowSize,
            RetryAfter = ResetAt - Now,
            {error, rate_limited, #{
                remaining => 0,
                limit => Limit,
                retry_after => max(0, RetryAfter),
                reset_at => ResetAt,
                algorithm => fixed_window
            }}
    end.

%% @private Sliding Window 算法（简化实现）
%% 使用加权平均的方式近似滑动窗口
check_sliding_window(Key, Rule, Consume, Opts) ->
    Now = erlang:system_time(millisecond),
    Cost = maps:get(cost, Opts, 1),
    WindowSize = 60000,  %% 1 分钟窗口

    %% 获取或创建桶
    Bucket = get_or_create_bucket(Key, Rule, Now),

    %% 计算当前窗口和上一窗口
    CurrentWindowStart = (Now div WindowSize) * WindowSize,
    PositionInWindow = Now - CurrentWindowStart,
    Weight = PositionInWindow / WindowSize,

    {PrevCount, CurrentCount, NewWindowStart} = case Bucket#bucket.window_start of
        WS when WS < CurrentWindowStart - WindowSize ->
            %% 两个窗口都过期
            {0, 0, CurrentWindowStart};
        WS when WS < CurrentWindowStart ->
            %% 上一窗口有数据
            {Bucket#bucket.window_count, 0, CurrentWindowStart};
        _ ->
            %% 同一窗口
            {0, Bucket#bucket.window_count, Bucket#bucket.window_start}
    end,

    %% 计算加权请求数
    WeightedCount = PrevCount * (1 - Weight) + CurrentCount,
    Limit = Rule#rule.requests_per_minute,

    case WeightedCount + Cost =< Limit of
        true ->
            %% 允许请求
            NewCount = case Consume of
                true -> CurrentCount + Cost;
                false -> CurrentCount
            end,
            UpdatedBucket = Bucket#bucket{
                window_start = NewWindowStart,
                window_count = NewCount,
                last_refill = Now,
                tokens = PrevCount  %% 存储上一窗口计数
            },
            ets:insert(?BUCKET_TABLE, UpdatedBucket),

            Remaining = max(0, trunc(Limit - WeightedCount - Cost)),
            {ok, #{
                remaining => Remaining,
                limit => Limit,
                reset_at => CurrentWindowStart + WindowSize,
                algorithm => sliding_window
            }};
        false ->
            %% 拒绝请求
            %% 估算重试时间
            NeededReduction = WeightedCount + Cost - Limit,
            RetryAfter = trunc(NeededReduction / (PrevCount / WindowSize + 0.001) * 1000),
            RetryAfter2 = min(RetryAfter, WindowSize),

            {error, rate_limited, #{
                remaining => 0,
                limit => Limit,
                retry_after => max(100, RetryAfter2),
                reset_at => Now + RetryAfter2,
                algorithm => sliding_window
            }}
    end.

%% @private 获取或创建令牌桶
get_or_create_bucket(Key, Rule, Now) ->
    case ets:lookup(?BUCKET_TABLE, Key) of
        [Bucket] -> Bucket;
        [] ->
            %% 创建新桶，初始令牌为突发大小
            #bucket{
                key = Key,
                tokens = float(Rule#rule.burst_size),
                last_refill = Now,
                window_start = Now,
                window_count = 0
            }
    end.

%% @private 计算重置时间
calculate_reset_time(Now, _Rule) ->
    WindowSize = 60000,
    NextWindow = ((Now div WindowSize) + 1) * WindowSize,
    NextWindow.

%% @private 计算重试等待时间
calculate_retry_after(TokensNeeded, RefillRate) ->
    %% 需要等待的毫秒数
    max(100, trunc(TokensNeeded / RefillRate)).

%% @private 清理过期的桶
cleanup_expired_buckets() ->
    Now = erlang:system_time(millisecond),
    ExpireThreshold = Now - 300000,  %% 5 分钟未活动则清理

    ets:foldl(fun(Bucket, _Acc) ->
        case Bucket#bucket.last_refill < ExpireThreshold of
            true -> ets:delete(?BUCKET_TABLE, Bucket#bucket.key);
            false -> ok
        end
    end, ok, ?BUCKET_TABLE).

%% @private 更新统计
update_stats(Result, Stats) ->
    Checks = maps:get(checks, Stats, 0) + 1,
    case Result of
        {ok, _} ->
            Allowed = maps:get(allowed, Stats, 0) + 1,
            Stats#{checks => Checks, allowed => Allowed};
        {error, rate_limited, _} ->
            Rejected = maps:get(rejected, Stats, 0) + 1,
            Stats#{checks => Checks, rejected => Rejected}
    end.

%% @private 规则转换为 map
rule_to_map(#rule{} = Rule) ->
    #{
        key => Rule#rule.key,
        requests_per_minute => Rule#rule.requests_per_minute,
        requests_per_hour => Rule#rule.requests_per_hour,
        burst_size => Rule#rule.burst_size,
        algorithm => Rule#rule.algorithm
    }.
