%%%-------------------------------------------------------------------
%%% @doc A2A 应用 Supervisor
%%%
%%% 管理 A2A 相关进程的监督树。
%%%
%%% == 监督树结构 ==
%%%
%%% ```
%%% beamai_a2a_sup
%%% ├── beamai_a2a_card_cache (worker)
%%% ├── beamai_a2a_push (worker)
%%% ├── beamai_a2a_auth (worker)
%%% ├── beamai_a2a_rate_limit (worker)
%%% └── beamai_a2a_task_sup (simple_one_for_one)
%%%     └── beamai_a2a_task (worker) × N
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_sup).

-behaviour(supervisor).

%% API 导出
-export([start_link/0]).

%% supervisor 回调
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% supervisor 回调
%%====================================================================

%% @doc 初始化 Supervisor
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    %% Agent Card 缓存
    CardCache = #{
        id => beamai_a2a_card_cache,
        start => {beamai_a2a_card_cache, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_a2a_card_cache]
    },

    %% Push Notifications 服务
    PushService = #{
        id => beamai_a2a_push,
        start => {beamai_a2a_push, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_a2a_push]
    },

    %% 认证服务
    AuthService = #{
        id => beamai_a2a_auth,
        start => {beamai_a2a_auth, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_a2a_auth]
    },

    %% 限流服务
    RateLimitService = #{
        id => beamai_a2a_rate_limit,
        start => {beamai_a2a_rate_limit, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_a2a_rate_limit]
    },

    %% Task Supervisor（动态创建 Task 进程）
    TaskSup = #{
        id => beamai_a2a_task_sup,
        start => {beamai_a2a_task_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [beamai_a2a_task_sup]
    },

    Children = [CardCache, PushService, AuthService, RateLimitService, TaskSup],

    {ok, {SupFlags, Children}}.
