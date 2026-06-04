%%%-------------------------------------------------------------------
%%% @doc BeamAI Core 顶层 Supervisor
%%%
%%% 管理 beamai_core 的核心进程：
%%% - beamai_http_pool: HTTP 连接池（仅当使用 Gun 后端时）
%%%
%%% 注：Process 框架（worker 池 + runtime supervisor）已迁出至独立的
%%% beamai_process 应用，不再由本 supervisor 管理。
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
    case should_start_http_pool() of
        true -> [http_pool_spec()];
        false -> []
    end.

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
