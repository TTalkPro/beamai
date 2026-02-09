%%%-------------------------------------------------------------------
%%% @doc 流程运行时动态 Supervisor
%%%
%%% 使用 simple_one_for_one 策略管理 beamai_process_runtime
%%% gen_server 进程，每个流程实例作为临时子进程启动。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_runtime/1, start_runtime/2]).

%% Supervisor 回调
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Supervisor 进程（注册为本地名称）
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc 在 Supervisor 下启动新的流程运行时（默认选项）
%%
%% @param ProcessSpec 编译后的流程定义
%% @returns {ok, Pid} | {error, Reason}
-spec start_runtime(beamai_process_builder:process_spec()) ->
    {ok, pid()} | {error, term()}.
start_runtime(ProcessSpec) ->
    start_runtime(ProcessSpec, #{}).

%% @doc 在 Supervisor 下启动新的流程运行时（自定义选项）
%%
%% @param ProcessSpec 编译后的流程定义
%% @param Opts 启动选项
%% @returns {ok, Pid} | {error, Reason}
-spec start_runtime(beamai_process_builder:process_spec(), map()) ->
    {ok, pid()} | {error, term()}.
start_runtime(ProcessSpec, Opts) ->
    supervisor:start_child(?MODULE, [ProcessSpec, Opts]).

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
        id => beamai_process_runtime,
        start => {beamai_process_runtime, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [beamai_process_runtime]
    },
    {ok, {SupFlags, [ChildSpec]}}.
