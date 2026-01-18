%%%-------------------------------------------------------------------
%%% @doc A2A Task Supervisor
%%%
%%% 管理 Task 进程的动态创建和监督。
%%% 使用 simple_one_for_one 策略。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_task_sup).

-behaviour(supervisor).

%% API 导出
-export([start_link/0, start_task/1]).

%% supervisor 回调
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc 启动一个新的 Task 进程
%%
%% @param Opts Task 配置选项
%% @returns {ok, Pid} | {error, Reason}
-spec start_task(map()) -> {ok, pid()} | {error, term()}.
start_task(Opts) ->
    supervisor:start_child(?MODULE, [Opts]).

%%====================================================================
%% supervisor 回调
%%====================================================================

%% @doc 初始化 Supervisor
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    %% Task 子进程规范
    TaskSpec = #{
        id => beamai_a2a_task,
        start => {beamai_a2a_task, start_link, []},
        restart => temporary,  %% Task 完成后不重启
        shutdown => 5000,
        type => worker,
        modules => [beamai_a2a_task]
    },

    {ok, {SupFlags, [TaskSpec]}}.
