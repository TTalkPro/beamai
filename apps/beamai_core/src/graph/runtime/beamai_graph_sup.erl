%%%-------------------------------------------------------------------
%%% @doc 图运行时动态 Supervisor
%%%
%%% 使用 simple_one_for_one 策略管理 beamai_graph_runtime
%%% gen_server 进程，每个图执行实例作为临时子进程启动。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_runtime/3]).

%% Supervisor 回调
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Supervisor 进程（注册为本地名称）
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc 在 Supervisor 下启动新的图运行时
%%
%% @param Graph 编译后的图定义
%% @param InitialState 初始全局状态
%% @param Opts 启动选项
%% @returns {ok, Pid} | {error, Reason}
-spec start_runtime(beamai_graph_builder:graph(), beamai_context:t(), map()) ->
    {ok, pid()} | {error, term()}.
start_runtime(Graph, InitialState, Opts) ->
    supervisor:start_child(?MODULE, [Graph, InitialState, Opts]).

%%====================================================================
%% Supervisor 回调
%%====================================================================

%% @private 初始化 Supervisor 配置
%% 使用 simple_one_for_one 策略，子进程为临时进程（崩溃不重启）
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpec = #{
        id => beamai_graph_runtime,
        start => {beamai_graph_runtime, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [beamai_graph_runtime]
    },
    {ok, {SupFlags, [ChildSpec]}}.
