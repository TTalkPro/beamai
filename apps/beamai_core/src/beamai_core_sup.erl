%%%-------------------------------------------------------------------
%%% @doc BeamAI Core 顶层 Supervisor
%%%
%%% 管理 beamai_core 的核心进程：
%%% - beamai_http_pool: HTTP 连接池（仅当使用 Gun 后端时）
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
%% 根据配置的 HTTP 后端决定是否启动连接池
get_children() ->
    HttpChildren = case should_start_pool() of
        true -> [http_pool_spec()];
        false -> []
    end,
    DispatchChildren = case should_start_dispatch_pool() of
        true -> [dispatch_pool_spec()];
        false -> []
    end,
    HttpChildren ++ DispatchChildren.

%% @private 判断是否需要启动连接池
%% 当配置使用 Gun 后端或者 Gun 可用时启动
should_start_pool() ->
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

%% @private 判断是否需要启动 dispatch 池
should_start_dispatch_pool() ->
    case application:get_env(beamai_core, dispatch_pool_enabled, true) of
        false -> false;
        true ->
            case code:which(poolboy) of
                non_existing -> false;
                _ -> true
            end
    end.

%% @private Dispatch 进程池子进程规格
dispatch_pool_spec() ->
    PoolConfig = application:get_env(beamai_core, dispatch_pool, #{}),
    Size = maps:get(size, PoolConfig, 20),
    MaxOverflow = maps:get(max_overflow, PoolConfig, 40),
    PoolArgs = [
        {name, {local, beamai_dispatch_pool}},
        {worker_module, pregel_dispatch_worker},
        {size, Size},
        {max_overflow, MaxOverflow},
        {strategy, fifo}
    ],
    poolboy:child_spec(beamai_dispatch_pool, PoolArgs, []).
