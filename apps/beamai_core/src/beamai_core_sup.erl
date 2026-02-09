%%%-------------------------------------------------------------------
%%% @doc BeamAI Core 顶层 Supervisor
%%%
%%% 管理 beamai_core 的核心进程：
%%% - beamai_http_pool: HTTP 连接池（仅当使用 Gun 后端时）
%%% - beamai_process_pool: Process step worker 池（poolboy）
%%% - beamai_graph_pool: Graph executor worker 池（poolboy）
%%% - beamai_process_sup: Process runtime 动态 supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor 回调
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor 回调
%%====================================================================

%% @doc 初始化 supervisor
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    Children = get_children(),

    {ok, {SupFlags, Children}}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取子进程规格
get_children() ->
    HttpChildren = case should_start_http_pool() of
        true -> [http_pool_spec()];
        false -> []
    end,
    ProcessChildren = case should_start_process_pool() of
        true -> [process_pool_spec()];
        false -> []
    end,
    GraphChildren = case should_start_graph_pool() of
        true -> [graph_pool_spec()];
        false -> []
    end,
    HttpChildren ++ ProcessChildren ++ GraphChildren ++ [process_sup_spec()].

%% @private 默认池大小计算（CPU * 2）
default_pool_size() ->
    erlang:system_info(schedulers) * 2.

%% @private 判断是否需要启动 HTTP 连接池
%% 当配置使用 Gun 后端或者 Gun 可用时启动
should_start_http_pool() ->
    Backend = application:get_env(beamai_core, http_backend, beamai_http_gun),
    case Backend of
        beamai_http_gun ->
            %% 检查 Gun 是否可用
            case code:which(gun) of
                non_existing -> false;
                _ -> true
            end;
        _ ->
            false
    end.

%% @private 判断是否需要启动 Process pool
should_start_process_pool() ->
    case application:get_env(beamai_core, process_pool_enabled, true) of
        false -> false;
        true -> code:which(poolboy) =/= non_existing
    end.

%% @private 判断是否需要启动 Graph pool
should_start_graph_pool() ->
    case application:get_env(beamai_core, graph_pool_enabled, true) of
        false -> false;
        true -> code:which(poolboy) =/= non_existing
    end.

%% @private HTTP 连接池子进程规格
http_pool_spec() ->
    PoolConfig = application:get_env(beamai_core, http_pool, #{}),
    #{
        id => beamai_http_pool,
        start => {beamai_http_pool, start_link, [PoolConfig]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_http_pool]
    }.

%% @private Process worker pool 规格 (poolboy)
process_pool_spec() ->
    DefaultSize = default_pool_size(),
    PoolSize = application:get_env(beamai_core, process_pool_size, DefaultSize),
    MaxOverflow = application:get_env(beamai_core, process_pool_max_overflow, DefaultSize * 2),
    PoolArgs = [
        {name, {local, beamai_process_pool}},
        {worker_module, beamai_process_worker},
        {size, PoolSize},
        {max_overflow, MaxOverflow},
        {strategy, fifo}
    ],
    poolboy:child_spec(beamai_process_pool, PoolArgs, []).

%% @private Graph executor worker pool 规格 (poolboy)
graph_pool_spec() ->
    PoolConfig = application:get_env(beamai_core, graph_pool, #{}),
    DefaultSize = default_pool_size(),
    Size = maps:get(size, PoolConfig, DefaultSize),
    MaxOverflow = maps:get(max_overflow, PoolConfig, DefaultSize * 2),
    PoolArgs = [
        {name, {local, beamai_graph_pool}},
        {worker_module, graph_pool_worker},
        {size, Size},
        {max_overflow, MaxOverflow},
        {strategy, fifo}
    ],
    poolboy:child_spec(beamai_graph_pool, PoolArgs, []).

%% @private Process runtime supervisor 规格
process_sup_spec() ->
    #{
        id => beamai_process_sup,
        start => {beamai_process_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [beamai_process_sup]
    }.
