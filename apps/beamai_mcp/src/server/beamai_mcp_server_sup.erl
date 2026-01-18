%%%-------------------------------------------------------------------
%%% @doc MCP 服务器监督者
%%%
%%% 使用 simple_one_for_one 策略管理多个 MCP 服务器实例。
%%% 每个服务器实例处理一个客户端连接。
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 启动 SSE 服务器实例（通常由 Cowboy handler 调用）
%%% {ok, Pid} = beamai_mcp_server_sup:start_server(#{
%%%     transport => sse,
%%%     req => CowboyReq,
%%%     tools => [...],
%%%     resources => [...]
%%% }).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_server_sup).

-behaviour(supervisor).

%%====================================================================
%% API 导出
%%====================================================================

-export([
    start_link/0,
    start_server/1,
    stop_server/1
]).

%%====================================================================
%% Supervisor 回调
%%====================================================================

-export([init/1]).

%%====================================================================
%% 宏定义
%%====================================================================

-define(SERVER, ?MODULE).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动服务器监督者
%%
%% @returns {ok, Pid} | {error, Reason}
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc 启动新的 MCP 服务器实例
%%
%% @param Config 服务器配置
%%   - transport: sse | http
%%   - tools: 工具列表
%%   - resources: 资源列表
%%   - prompts: 提示列表
%% @returns {ok, Pid} | {error, Reason}
-spec start_server(map()) -> {ok, pid()} | {error, term()}.
start_server(Config) ->
    supervisor:start_child(?SERVER, [Config]).

%% @doc 停止 MCP 服务器实例
%%
%% @param Pid 服务器进程 ID
%% @returns ok | {error, Reason}
-spec stop_server(pid()) -> ok | {error, term()}.
stop_server(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

%%====================================================================
%% Supervisor 回调
%%====================================================================

%% @doc 初始化监督者
%%
%% 使用 simple_one_for_one 策略，动态创建服务器实例。
%%
%% @param Args 初始化参数（未使用）
%% @returns {ok, {SupFlags, ChildSpecs}}
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpec = #{
        id => beamai_mcp_server,
        start => {beamai_mcp_server, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [beamai_mcp_server]
    },

    {ok, {SupFlags, [ChildSpec]}}.
