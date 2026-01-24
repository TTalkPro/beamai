%%%-------------------------------------------------------------------
%%% @doc MCP 远程工具代理
%%%
%%% 连接到 MCP 服务器，获取工具列表并转换为 beamai_agent 工具。
%%%
%%% == 功能特性 ==
%%%
%%% - 自动连接 MCP 服务器
%%% - 动态获取工具列表
%%% - 将 MCP 工具转换为 beamai_agent 格式
%%% - 支持工具列表刷新
%%% - 处理 tools/list_changed 通知
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 连接到文件系统 MCP 服务器
%%% {ok, Proxy} = beamai_mcp_tool_proxy:start_link(#{
%%%     transport => stdio,
%%%     command => "npx",
%%%     args => ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
%%% }),
%%%
%%% %% 获取转换后的工具列表
%%% {ok, Tools} = beamai_mcp_tool_proxy:get_tools(Proxy),
%%%
%%% %% 使用这些工具创建 agent
%%% {ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
%%%     system_prompt => <<"You can access files">>,
%%%     tools => Tools,
%%%     ...
%%% }).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_tool_proxy).

-behaviour(gen_server).

-include("beamai_mcp.hrl").

%%====================================================================
%% API 导出
%%====================================================================

-export([
    start_link/1,
    start_link/2,
    stop/1,
    get_tools/1,
    get_tools/2,
    refresh_tools/1,
    get_server_info/1,
    get_state/1
]).

%%====================================================================
%% gen_server 回调
%%====================================================================

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% 状态记录
%%====================================================================

-record(state, {
    %% MCP 客户端进程
    mcp_client :: pid() | undefined,

    %% 转换后的 beamai_agent 工具列表
    tools = [] :: [map()],

    %% 原始 MCP 工具列表（用于调试）
    mcp_tools = [] :: [map()],

    %% 服务器信息
    server_info :: map() | undefined,
    server_capabilities :: map() | undefined,

    %% 配置
    config :: map(),

    %% 状态
    status = disconnected :: disconnected | connecting | connected | error,
    error_reason :: term() | undefined,

    %% 自动刷新配置
    auto_refresh = false :: boolean(),
    refresh_interval :: pos_integer() | undefined
}).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动 MCP 工具代理
%%
%% @param Config 配置 map，包含：
%%   - transport: stdio | http | sse
%%   - command: 命令（stdio 传输）
%%   - args: 参数列表（stdio 传输）
%%   - base_url: URL（HTTP 传输）
%%   - client_info: 客户端信息（可选）
%%   - auto_refresh: 是否自动刷新工具列表（可选，默认 false）
%%   - refresh_interval: 刷新间隔毫秒数（可选，默认 30000）
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc 启动已命名的 MCP 工具代理
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(Name, Config) ->
    gen_server:start_link({local, Name}, ?MODULE, Config, []).

%% @doc 停止代理
-spec stop(pid() | atom()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc 获取转换后的工具列表
%%
%% 返回已转换为 beamai_agent 格式的工具列表。
%% 这些工具可以直接用于 beamai_agent:start_link/2。
%%
%% @param Pid 代理进程
%% @returns {ok, [AgentTool]} | {error, Reason}
-spec get_tools(pid() | atom()) -> {ok, [map()]} | {error, term()}.
get_tools(Pid) ->
    get_tools(Pid, 5000).

%% @doc 获取转换后的工具列表（带超时）
-spec get_tools(pid() | atom(), timeout()) -> {ok, [map()]} | {error, term()}.
get_tools(Pid, Timeout) ->
    gen_server:call(Pid, get_tools, Timeout).

%% @doc 刷新工具列表
%%
%% 重新从 MCP 服务器获取工具列表并转换。
%% 用于支持动态工具更新。
%%
%% @param Pid 代理进程
%% @returns ok | {error, Reason}
-spec refresh_tools(pid() | atom()) -> ok | {error, term()}.
refresh_tools(Pid) ->
    gen_server:call(Pid, refresh_tools, 10000).

%% @doc 获取服务器信息
%%
%% @param Pid 代理进程
%% @returns {ok, ServerInfo} | {error, Reason}
-spec get_server_info(pid() | atom()) -> {ok, map()} | {error, term()}.
get_server_info(Pid) ->
    gen_server:call(Pid, get_server_info).

%% @doc 获取代理状态
%%
%% @param Pid 代理进程
%% @returns {ok, State} where State = disconnected | connecting | connected | error
-spec get_state(pid() | atom()) -> {ok, atom()}.
get_state(Pid) ->
    gen_server:call(Pid, get_state).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @private
init(Config) ->
    %% 提取配置
    AutoRefresh = maps:get(auto_refresh, Config, false),
    RefreshInterval = maps:get(refresh_interval, Config, 30000),

    State = #state{
        config = Config,
        status = connecting,
        auto_refresh = AutoRefresh,
        refresh_interval = RefreshInterval
    },

    %% 异步启动连接
    self() ! connect_to_server,

    {ok, State}.

%% @private
handle_call(get_tools, _From, #state{status = connected, tools = Tools} = State) ->
    {reply, {ok, Tools}, State};

handle_call(get_tools, _From, #state{status = Status} = State) ->
    {reply, {error, {not_connected, Status}}, State};

handle_call(refresh_tools, _From, #state{status = connected} = State) ->
    case load_tools_from_server(State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(refresh_tools, _From, #state{status = Status} = State) ->
    {reply, {error, {not_connected, Status}}, State};

handle_call(get_server_info, _From, #state{server_info = Info} = State) ->
    Result = case Info of
        undefined -> {error, not_available};
        _ -> {ok, Info}
    end,
    {reply, Result, State};

handle_call(get_state, _From, #state{status = Status} = State) ->
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(connect_to_server, State) ->
    case connect_and_initialize(State) of
        {ok, NewState} ->
            %% 连接成功，加载工具
            self() ! load_tools,
            {noreply, NewState};
        {error, Reason} ->
            io:format("[beamai_mcp_tool_proxy] Connection failed: ~p~n", [Reason]),
            %% 连接失败，稍后重试
            erlang:send_after(5000, self(), connect_to_server),
            {noreply, State#state{status = error, error_reason = Reason}}
    end;

handle_info(load_tools, #state{status = connected} = State) ->
    case load_tools_from_server(State) of
        {ok, NewState} ->
            io:format("[beamai_mcp_tool_proxy] Loaded ~p tools~n",
                      [length(NewState#state.tools)]),
            %% 设置自动刷新
            maybe_schedule_refresh(NewState),
            {noreply, NewState};
        {error, Reason} ->
            io:format("[beamai_mcp_tool_proxy] Failed to load tools: ~p~n", [Reason]),
            %% 稍后重试
            erlang:send_after(5000, self(), load_tools),
            {noreply, State}
    end;

handle_info(load_tools, State) ->
    %% 未连接，忽略
    {noreply, State};

handle_info(auto_refresh, #state{status = connected} = State) ->
    %% 自动刷新工具列表
    case load_tools_from_server(State) of
        {ok, NewState} ->
            maybe_schedule_refresh(NewState),
            {noreply, NewState};
        {error, _Reason} ->
            maybe_schedule_refresh(State),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{mcp_client = undefined}) ->
    ok;
terminate(_Reason, #state{mcp_client = Client}) ->
    catch beamai_mcp_client:stop(Client),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数 - 连接管理
%%====================================================================

%% @private 连接并初始化 MCP 服务器
-spec connect_and_initialize(#state{}) -> {ok, #state{}} | {error, term()}.
connect_and_initialize(#state{config = Config} = State) ->
    %% 启动 MCP 客户端
    case beamai_mcp_client:start_link(Config) of
        {ok, Client} ->
            %% 等待初始化完成（客户端会自动初始化）
            case wait_for_connection(Client, 10) of
                ok ->
                    %% 获取服务器信息
                    ServerInfo = get_client_server_info(Client),
                    Capabilities = get_client_capabilities(Client),
                    {ok, State#state{
                        mcp_client = Client,
                        status = connected,
                        server_info = ServerInfo,
                        server_capabilities = Capabilities
                    }};
                {error, Reason} ->
                    catch beamai_mcp_client:stop(Client),
                    {error, {init_timeout, Reason}}
            end;
        {error, Reason} ->
            {error, {client_start_failed, Reason}}
    end.

%% @private 等待客户端连接
-spec wait_for_connection(pid(), non_neg_integer()) -> ok | {error, timeout}.
wait_for_connection(_Client, 0) ->
    {error, timeout};
wait_for_connection(Client, Retries) ->
    case beamai_mcp_client:get_state(Client) of
        {ok, connected} ->
            ok;
        _ ->
            timer:sleep(1000),
            wait_for_connection(Client, Retries - 1)
    end.

%% @private 获取服务器信息
-spec get_client_server_info(pid()) -> map() | undefined.
get_client_server_info(_Client) ->
    %% 预留：实际需要从客户端获取 server_info
    undefined.

%% @private 获取服务器能力
-spec get_client_capabilities(pid()) -> map() | undefined.
get_client_capabilities(Client) ->
    case beamai_mcp_client:get_capabilities(Client) of
        {ok, Caps} -> Caps;
        _ -> undefined
    end.

%%====================================================================
%% 内部函数 - 工具加载
%%====================================================================

%% @private 从 MCP 服务器加载工具
-spec load_tools_from_server(#state{}) -> {ok, #state{}} | {error, term()}.
load_tools_from_server(#state{mcp_client = Client} = State) ->
    case beamai_mcp_client:list_tools(Client) of
        {ok, Result} ->
            McpTools = extract_tools_from_result(Result),
            %% 转换为 beamai_agent 工具
            AgentTools = convert_mcp_tools_to_agent(McpTools, Client),
            {ok, State#state{
                tools = AgentTools,
                mcp_tools = McpTools
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 从结果中提取工具列表
-spec extract_tools_from_result(map()) -> [map()].
extract_tools_from_result(#{<<"tools">> := Tools}) when is_list(Tools) ->
    Tools;
extract_tools_from_result(_) ->
    [].

%% @private 转换 MCP 工具为 beamai_agent 工具
-spec convert_mcp_tools_to_agent([map()], pid()) -> [map()].
convert_mcp_tools_to_agent(McpTools, Client) ->
    lists:map(fun(McpTool) ->
        convert_single_mcp_tool(McpTool, Client)
    end, McpTools).

%% @private 转换单个 MCP 工具
-spec convert_single_mcp_tool(map(), pid()) -> map().
convert_single_mcp_tool(McpTool, Client) ->
    Name = maps:get(<<"name">>, McpTool),
    Description = maps:get(<<"description">>, McpTool, <<"No description">>),
    InputSchema = maps:get(<<"inputSchema">>, McpTool, #{}),

    %% 创建代理 handler
    Handler = make_proxy_handler(Client, Name),

    #{
        name => Name,
        description => Description,
        parameters => InputSchema,
        handler => Handler
    }.

%% @private 创建调用 MCP 工具的代理 handler
%%
%% 返回一个函数，该函数会通过 MCP Client 远程调用工具。
-spec make_proxy_handler(pid(), binary()) -> function().
make_proxy_handler(Client, ToolName) ->
    fun(Args) ->
        case beamai_mcp_client:call_tool(Client, ToolName, Args) of
            {ok, Result} ->
                %% 提取并转换内容
                beamai_mcp_adapter:extract_content(Result);
            {error, Reason} ->
                %% 格式化错误
                beamai_mcp_adapter:format_error(Reason)
        end
    end.

%%====================================================================
%% 内部函数 - 自动刷新
%%====================================================================

%% @private 可能调度自动刷新
-spec maybe_schedule_refresh(#state{}) -> ok.
maybe_schedule_refresh(#state{auto_refresh = false}) ->
    ok;
maybe_schedule_refresh(#state{auto_refresh = true, refresh_interval = Interval}) ->
    erlang:send_after(Interval, self(), auto_refresh),
    ok.
