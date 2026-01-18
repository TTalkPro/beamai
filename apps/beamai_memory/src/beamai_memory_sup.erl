%%%-------------------------------------------------------------------
%%% @doc Agent Memory 监督树
%%%
%%% 管理 Agent Memory 系统的子进程：
%%% - Checkpointer 进程池（可选）
%%% - Store 进程池（可选）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor 回调
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor 回调
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    %% 暂时不启动子进程
    %% Checkpointer 和 Store 由调用方按需创建
    ChildSpecs = [],

    {ok, {SupFlags, ChildSpecs}}.
