%%%-------------------------------------------------------------------
%%% @doc MCP 主监督者
%%%
%%% 监督 MCP 客户端和服务器的子监督者。
%%%
%%% == 监督树结构 ==
%%%
%%% ```
%%% beamai_mcp_sup (one_for_one)
%%% ├── beamai_mcp_client_registry (gen_server)
%%% ├── beamai_mcp_client_sup (simple_one_for_one)
%%% │   └── beamai_mcp_client (多个客户端实例)
%%% └── beamai_mcp_server_sup (simple_one_for_one)
%%%     └── beamai_mcp_server (多个服务器实例)
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_sup).

-behaviour(supervisor).

%%====================================================================
%% API 导出
%%====================================================================

-export([start_link/0]).

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

%% @doc 启动主监督者
%%
%% @returns {ok, Pid} | {error, Reason}
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor 回调
%%====================================================================

%% @doc 初始化监督者
%%
%% 配置客户端和服务器的子监督者。
%%
%% @param Args 初始化参数（未使用）
%% @returns {ok, {SupFlags, ChildSpecs}}
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    %% 客户端注册表（需要在客户端监督者之前启动）
    ClientRegistry = #{
        id => beamai_mcp_client_registry,
        start => {beamai_mcp_client_registry, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_mcp_client_registry]
    },

    %% 客户端监督者
    ClientSup = #{
        id => beamai_mcp_client_sup,
        start => {beamai_mcp_client_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [beamai_mcp_client_sup]
    },

    %% 服务器监督者
    ServerSup = #{
        id => beamai_mcp_server_sup,
        start => {beamai_mcp_server_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [beamai_mcp_server_sup]
    },

    {ok, {SupFlags, [ClientRegistry, ClientSup, ServerSup]}}.
