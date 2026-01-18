%%%-------------------------------------------------------------------
%%% @doc MCP 客户端监督者
%%%
%%% 使用 simple_one_for_one 策略管理多个 MCP 客户端实例。
%%% 每个客户端连接一个 MCP 服务器。
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 启动 Stdio 客户端
%%% {ok, Pid} = beamai_mcp_client_sup:start_client(#{
%%%     transport => stdio,
%%%     command => "npx",
%%%     args => ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
%%% }).
%%%
%%% %% 启动 SSE 客户端
%%% {ok, Pid} = beamai_mcp_client_sup:start_client(#{
%%%     transport => sse,
%%%     url => <<"https://example.com/mcp/sse">>
%%% }).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_client_sup).

-behaviour(supervisor).

%%====================================================================
%% API 导出
%%====================================================================

-export([
    start_link/0,
    start_client/1,
    stop_client/1
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

%% @doc 启动客户端监督者
%%
%% @returns {ok, Pid} | {error, Reason}
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc 启动新的 MCP 客户端
%%
%% @param Config 客户端配置
%%   - transport: stdio | sse | http
%%   - 其他传输特定配置
%% @returns {ok, Pid} | {error, Reason}
-spec start_client(map()) -> {ok, pid()} | {error, term()}.
start_client(Config) ->
    supervisor:start_child(?SERVER, [Config]).

%% @doc 停止 MCP 客户端
%%
%% @param Pid 客户端进程 ID
%% @returns ok | {error, Reason}
-spec stop_client(pid()) -> ok | {error, term()}.
stop_client(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

%%====================================================================
%% Supervisor 回调
%%====================================================================

%% @doc 初始化监督者
%%
%% 使用 simple_one_for_one 策略，动态创建客户端。
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
        id => beamai_mcp_client,
        start => {beamai_mcp_client, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [beamai_mcp_client]
    },

    {ok, {SupFlags, [ChildSpec]}}.
