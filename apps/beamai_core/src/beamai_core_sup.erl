%%%-------------------------------------------------------------------
%%% @doc BeamAI Core 顶层 Supervisor
%%%
%%% 管理 beamai_core 的核心进程（仅当使用 Gun 后端时）：
%%% - http_pool_short:    同步 chat / 工具调用等短请求
%%% - http_pool_stream:   SSE 流式请求
%%% - http_pool_longpoll: 异步任务长轮询（如智谱 async-result）
%%%
%%% 三个池是相互独立的 gen_server（beamai_http_pool 实例），one_for_one
%%% 保证一个池崩溃不影响其他两个。
%%%
%%% == 配置 ==
%%%
%%% 遗留的 http_pool env 键仍兼容：其值统一应用到三个池
%%% （保持旧的单池语义），并在启动时打一次废弃警告。
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

-ifdef(TEST).
-export([resolve_pool_configs/0]).
-endif.

%% 三个用途分池的注册名
-define(POOL_NAMES, [http_pool_short, http_pool_stream, http_pool_longpoll]).

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
        true -> http_pool_specs();
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

%% @private 三个用途分池的子进程规格
http_pool_specs() ->
    [pool_spec(PoolName, PoolConfig)
     || {PoolName, PoolConfig} <- maps:to_list(resolve_pool_configs())].

pool_spec(PoolName, PoolConfig) ->
    #{
        id => PoolName,
        start => {beamai_http_pool, start_link, [PoolName, PoolConfig]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_http_pool]
    }.

%% @private 解析各池配置
%% 池实例本身不读 app env；env 在这里统一解析后经 start_link/2 传入。
%% 遗留 http_pool 键：值统一应用到三个池（保持旧的单池语义），
%% connection_timeout 旧键名由 beamai_http_pool:merge_config/1 归一化。
resolve_pool_configs() ->
    Legacy = application:get_env(beamai_core, http_pool, undefined),
    case Legacy of
        undefined -> ok;
        _ ->
            logger:warning(
                "beamai_core: config key 'http_pool' is deprecated, "
                "use 'http_pools' with per-pool configs instead")
    end,
    maps:from_list([{Name, resolve_one_pool(Name, Legacy)} || Name <- ?POOL_NAMES]).

resolve_one_pool(PoolName, undefined) ->
    default_pool_config(PoolName);
resolve_one_pool(PoolName, Legacy) when is_map(Legacy) ->
    maps:merge(default_pool_config(PoolName), Legacy).

%% @private 各池默认配置
%% 三个池默认值完全一致，等于拆分前单池的默认值——无配置用户
%% 每类流量的行为与拆分前相同；分池调优（更大的 stream 预算、
%% 更长的 longpoll idle）由运维经配置显式选择。
default_pool_config(_PoolName) ->
    #{
        max_connections_per_host => 10,
        connect_timeout => 30000,
        idle_timeout => 60000,
        protocols => [http]
    }.
