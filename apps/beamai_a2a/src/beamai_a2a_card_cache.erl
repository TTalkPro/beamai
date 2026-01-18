%%%-------------------------------------------------------------------
%%% @doc Agent Card 缓存模块
%%%
%%% 使用 ETS 缓存远程 Agent Card，避免重复 HTTP 请求。
%%% 支持 TTL 过期机制和手动失效。
%%%
%%% == 架构 ==
%%%
%%% ```
%%% ┌─────────────────────────────────────────────────────────┐
%%% │                  beamai_a2a_card_cache                    │
%%% │                    (gen_server)                          │
%%% ├─────────────────────────────────────────────────────────┤
%%% │  ┌─────────────────────────────────────────────────┐    │
%%% │  │              ETS Table                           │    │
%%% │  │  Key: URL (binary)                               │    │
%%% │  │  Value: {Card, InsertedAt, TTL}                  │    │
%%% │  └─────────────────────────────────────────────────┘    │
%%% │                                                         │
%%% │  ┌─────────────────────────────────────────────────┐    │
%%% │  │          TTL Cleanup Timer                       │    │
%%% │  │  定期清理过期条目                                 │    │
%%% │  └─────────────────────────────────────────────────┘    │
%%% └─────────────────────────────────────────────────────────┘
%%% ```
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 获取（带缓存）
%%% {ok, Card} = beamai_a2a_card_cache:get("https://agent.example.com").
%%%
%%% %% 手动存储
%%% ok = beamai_a2a_card_cache:put("https://agent.example.com", Card).
%%%
%%% %% 带自定义 TTL 存储（秒）
%%% ok = beamai_a2a_card_cache:put("https://agent.example.com", Card, 7200).
%%%
%%% %% 失效
%%% ok = beamai_a2a_card_cache:invalidate("https://agent.example.com").
%%%
%%% %% 清空缓存
%%% ok = beamai_a2a_card_cache:clear().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_card_cache).

-behaviour(gen_server).

%% 避免与 erlang:put/2 和 erlang:get/1 冲突
-compile({no_auto_import,[put/2, put/3, get/1, get/2]}).

%% API 导出
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% 缓存操作
    get/1,
    get/2,
    put/2,
    put/3,
    invalidate/1,
    clear/0,

    %% 查询
    lookup/1,
    exists/1,
    stats/0,
    list_keys/0
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

%%====================================================================
%% 类型定义
%%====================================================================

-type url() :: binary() | string().
-type card() :: map().
-type ttl() :: pos_integer().  %% 秒
-type cache_entry() :: {card(), InsertedAt :: integer(), TTL :: ttl()}.
-type options() :: #{
    default_ttl => ttl(),
    cleanup_interval => pos_integer(),
    max_entries => pos_integer() | infinity
}.

-export_type([url/0, card/0, ttl/0, options/0]).

%%====================================================================
%% 常量定义
%%====================================================================

-define(SERVER, ?MODULE).
-define(TABLE, beamai_a2a_card_cache_table).
-define(DEFAULT_TTL, 3600).           %% 默认 1 小时
-define(CLEANUP_INTERVAL, 60000).     %% 清理间隔 1 分钟
-define(MAX_ENTRIES, 1000).           %% 最大缓存条目

%%====================================================================
%% 状态记录
%%====================================================================

-record(state, {
    table :: ets:tid(),
    default_ttl :: ttl(),
    cleanup_interval :: pos_integer(),
    max_entries :: pos_integer() | infinity,
    cleanup_timer :: reference() | undefined
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动缓存服务器
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc 启动缓存服务器（带配置）
%%
%% 配置选项：
%% - default_ttl: 默认 TTL（秒），默认 3600
%% - cleanup_interval: 清理间隔（毫秒），默认 60000
%% - max_entries: 最大条目数，默认 1000
-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc 停止缓存服务器
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc 获取 Agent Card（带缓存）
%%
%% 如果缓存命中且未过期，返回缓存的 Card。
%% 如果缓存未命中或已过期，从远程获取并缓存。
%%
%% @param Url Agent 基础 URL
%% @returns {ok, Card} | {error, Reason}
-spec get(url()) -> {ok, card()} | {error, term()}.
get(Url) ->
    get(Url, #{}).

%% @doc 获取 Agent Card（带选项）
%%
%% 选项：
%% - force_refresh: 强制刷新（默认 false）
%% - ttl: 缓存 TTL（秒）
-spec get(url(), map()) -> {ok, card()} | {error, term()}.
get(Url, Opts) ->
    UrlBin = normalize_url(Url),
    ForceRefresh = maps:get(force_refresh, Opts, false),

    case ForceRefresh of
        true ->
            fetch_and_cache(UrlBin, Opts);
        false ->
            case lookup(UrlBin) of
                {ok, Card} ->
                    {ok, Card};
                {error, not_found} ->
                    fetch_and_cache(UrlBin, Opts);
                {error, expired} ->
                    fetch_and_cache(UrlBin, Opts)
            end
    end.

%% @doc 存储 Agent Card 到缓存
-spec put(url(), card()) -> ok.
put(Url, Card) ->
    gen_server:call(?SERVER, {put, normalize_url(Url), Card, undefined}).

%% @doc 存储 Agent Card（带自定义 TTL）
-spec put(url(), card(), ttl()) -> ok.
put(Url, Card, TTL) ->
    gen_server:call(?SERVER, {put, normalize_url(Url), Card, TTL}).

%% @doc 查找缓存中的 Agent Card
%%
%% 仅查找，不会触发远程获取。
%%
%% @returns {ok, Card} | {error, not_found} | {error, expired}
-spec lookup(url()) -> {ok, card()} | {error, not_found | expired}.
lookup(Url) ->
    gen_server:call(?SERVER, {lookup, normalize_url(Url)}).

%% @doc 检查 URL 是否在缓存中（且未过期）
-spec exists(url()) -> boolean().
exists(Url) ->
    case lookup(Url) of
        {ok, _} -> true;
        _ -> false
    end.

%% @doc 使缓存条目失效
-spec invalidate(url()) -> ok.
invalidate(Url) ->
    gen_server:call(?SERVER, {invalidate, normalize_url(Url)}).

%% @doc 清空所有缓存
-spec clear() -> ok.
clear() ->
    gen_server:call(?SERVER, clear).

%% @doc 获取缓存统计信息
-spec stats() -> map().
stats() ->
    gen_server:call(?SERVER, stats).

%% @doc 列出所有缓存的 URL
-spec list_keys() -> [binary()].
list_keys() ->
    gen_server:call(?SERVER, list_keys).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @private
init(Opts) ->
    %% 创建 ETS 表
    Table = ets:new(?TABLE, [
        set,
        protected,
        {keypos, 1},
        {read_concurrency, true}
    ]),

    %% 解析配置
    DefaultTTL = maps:get(default_ttl, Opts, ?DEFAULT_TTL),
    CleanupInterval = maps:get(cleanup_interval, Opts, ?CLEANUP_INTERVAL),
    MaxEntries = maps:get(max_entries, Opts, ?MAX_ENTRIES),

    %% 启动清理定时器
    Timer = schedule_cleanup(CleanupInterval),

    State = #state{
        table = Table,
        default_ttl = DefaultTTL,
        cleanup_interval = CleanupInterval,
        max_entries = MaxEntries,
        cleanup_timer = Timer
    },

    {ok, State}.

%% @private
handle_call({put, Url, Card, TTL}, _From, State) ->
    #state{table = Table, default_ttl = DefaultTTL, max_entries = MaxEntries} = State,

    %% 检查是否需要清理空间
    CurrentSize = ets:info(Table, size),
    case MaxEntries =/= infinity andalso CurrentSize >= MaxEntries of
        true ->
            %% 删除最旧的条目
            evict_oldest(Table);
        false ->
            ok
    end,

    %% 存储条目
    ActualTTL = case TTL of
        undefined -> DefaultTTL;
        _ -> TTL
    end,
    Now = erlang:system_time(second),
    Entry = {Url, Card, Now, ActualTTL},
    ets:insert(Table, Entry),

    {reply, ok, State};

handle_call({lookup, Url}, _From, State) ->
    #state{table = Table} = State,
    Result = do_lookup(Table, Url),
    {reply, Result, State};

handle_call({invalidate, Url}, _From, State) ->
    #state{table = Table} = State,
    ets:delete(Table, Url),
    {reply, ok, State};

handle_call(clear, _From, State) ->
    #state{table = Table} = State,
    ets:delete_all_objects(Table),
    {reply, ok, State};

handle_call(stats, _From, State) ->
    #state{table = Table, default_ttl = DefaultTTL, max_entries = MaxEntries} = State,
    Stats = #{
        size => ets:info(Table, size),
        memory => ets:info(Table, memory),
        default_ttl => DefaultTTL,
        max_entries => MaxEntries
    },
    {reply, Stats, State};

handle_call(list_keys, _From, State) ->
    #state{table = Table} = State,
    Keys = ets:foldl(fun({Url, _, _, _}, Acc) -> [Url | Acc] end, [], Table),
    {reply, Keys, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(cleanup, State) ->
    #state{table = Table, cleanup_interval = Interval} = State,

    %% 清理过期条目
    cleanup_expired(Table),

    %% 重新调度
    Timer = schedule_cleanup(Interval),
    {noreply, State#state{cleanup_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    #state{table = Table, cleanup_timer = Timer} = State,

    %% 取消定时器
    case Timer of
        undefined -> ok;
        _ -> erlang:cancel_timer(Timer)
    end,

    %% 删除 ETS 表
    ets:delete(Table),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 标准化 URL
-spec normalize_url(url()) -> binary().
normalize_url(Url) when is_binary(Url) ->
    %% 移除尾部斜杠
    case binary:last(Url) of
        $/ -> binary:part(Url, 0, byte_size(Url) - 1);
        _ -> Url
    end;
normalize_url(Url) when is_list(Url) ->
    normalize_url(list_to_binary(Url)).

%% @private 查找缓存条目
-spec do_lookup(ets:tid(), binary()) -> {ok, card()} | {error, not_found | expired}.
do_lookup(Table, Url) ->
    case ets:lookup(Table, Url) of
        [] ->
            {error, not_found};
        [{_, Card, InsertedAt, TTL}] ->
            Now = erlang:system_time(second),
            case Now - InsertedAt < TTL of
                true ->
                    {ok, Card};
                false ->
                    %% 已过期，删除并返回错误
                    ets:delete(Table, Url),
                    {error, expired}
            end
    end.

%% @private 从远程获取并缓存
-spec fetch_and_cache(binary(), map()) -> {ok, card()} | {error, term()}.
fetch_and_cache(Url, Opts) ->
    case beamai_a2a_client:discover(Url, maps:without([force_refresh, ttl], Opts)) of
        {ok, Card} ->
            %% 缓存结果
            case maps:get(ttl, Opts, undefined) of
                undefined ->
                    put(Url, Card);
                TTL ->
                    put(Url, Card, TTL)
            end,
            {ok, Card};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 清理过期条目
-spec cleanup_expired(ets:tid()) -> non_neg_integer().
cleanup_expired(Table) ->
    Now = erlang:system_time(second),
    %% 使用 ets:select_delete 高效删除过期条目
    MatchSpec = [{
        {'$1', '_', '$2', '$3'},
        [{'<', {'+', '$2', '$3'}, Now}],
        [true]
    }],
    ets:select_delete(Table, MatchSpec).

%% @private 驱逐最旧的条目
-spec evict_oldest(ets:tid()) -> ok.
evict_oldest(Table) ->
    %% 找到最旧的条目（InsertedAt 最小）
    case ets:foldl(fun({Url, _, InsertedAt, _}, Acc) ->
        case Acc of
            undefined -> {Url, InsertedAt};
            {_, OldestTime} when InsertedAt < OldestTime -> {Url, InsertedAt};
            _ -> Acc
        end
    end, undefined, Table) of
        undefined ->
            ok;
        {OldestUrl, _} ->
            ets:delete(Table, OldestUrl),
            ok
    end.

%% @private 调度清理任务
-spec schedule_cleanup(pos_integer()) -> reference().
schedule_cleanup(Interval) ->
    erlang:send_after(Interval, self(), cleanup).
