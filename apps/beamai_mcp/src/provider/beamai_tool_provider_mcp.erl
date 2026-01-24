%%%-------------------------------------------------------------------
%%% @doc MCP 工具 Provider
%%%
%%% 从 MCP 服务器动态获取工具，实现 beamai_tool_provider 行为。
%%% 支持同时连接多个 MCP 服务器，自动聚合所有工具。
%%%
%%% == 功能特性 ==
%%%
%%% - 支持多 MCP 服务器连接
%%% - 自动从注册表聚合所有客户端的工具
%%% - 自动转换 MCP 工具为 tool_def() map 格式
%%% - 支持按标签过滤客户端
%%% - 支持工具名称前缀避免冲突
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 方式一：注册多个 MCP 客户端（推荐）
%%% {ok, FsClient} = beamai_mcp_client:start_link(FsConfig),
%%% {ok, GitClient} = beamai_mcp_client:start_link(GitConfig),
%%%
%%% ok = beamai_mcp_client_registry:register(<<"filesystem">>, FsClient, #{
%%%     tags => [file, local]
%%% }),
%%% ok = beamai_mcp_client_registry:register(<<"git">>, GitClient, #{
%%%     tags => [vcs, local]
%%% }),
%%%
%%% %% 获取所有 MCP 工具（自动聚合）
%%% Tools = beamai_tools:get_tools(all, #{
%%%     providers => [beamai_tool_provider_mcp]
%%% }).
%%%
%%% %% 方式二：按标签过滤
%%% FileTools = beamai_tools:get_tools(all, #{
%%%     providers => [beamai_tool_provider_mcp],
%%%     context => #{mcp_tag => file}
%%% }).
%%%
%%% %% 方式三：指定特定客户端
%%% Tools = beamai_tools:get_tools(all, #{
%%%     providers => [beamai_tool_provider_mcp],
%%%     context => #{mcp_clients => [FsClient, GitClient]}
%%% }).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_provider_mcp).

%% Tool 定义类型（兼容 beamai_function 格式）
-type tool_def() :: #{
    name := binary(),
    description := binary(),
    parameters => map(),
    handler := function(),
    category => atom(),
    permissions => [atom()],
    metadata => map()
}.

-export_type([tool_def/0]).

%% Provider API
-export([list_tools/1, find_tool/2, info/0, available/0]).

%% 辅助函数
-export([
    mcp_to_tool_def/2,
    mcp_to_tool_def/3,
    make_mcp_handler/2
]).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

%% @doc Provider 元信息
-spec info() -> map().
info() ->
    #{
        name => <<"mcp">>,
        version => <<"2.0.0">>,
        description => <<"MCP 远程工具 Provider，支持多服务器聚合"/utf8>>,
        categories => [mcp]
    }.

%% @doc 检查 MCP 客户端是否可用
%%
%% 如果注册表中有客户端或配置了默认客户端，返回 true。
-spec available() -> boolean().
available() ->
    %% 优先检查注册表
    case get_registered_clients() of
        [_ | _] -> true;
        [] ->
            %% 回退到默认客户端
            case get_default_client() of
                {ok, Pid} -> is_process_alive(Pid);
                error -> false
            end
    end.

%% @doc 列出所有 MCP 服务器的工具
%%
%% Context 参数：
%% - mcp_clients: 指定客户端 PID 列表（优先使用）
%% - mcp_client: 单个客户端 PID（向后兼容）
%% - mcp_tag: 按标签过滤注册表中的客户端
%% - mcp_prefix: 是否为工具名添加客户端名称前缀（默认 false）
%%
%% @returns {ok, [tool_def()]} | {error, term()}
-spec list_tools(map()) -> {ok, [tool_def()]} | {error, term()}.
list_tools(Opts) ->
    Context = maps:get(context, Opts, #{}),
    UsePrefix = maps:get(mcp_prefix, Context, false),

    case get_clients(Context) of
        [] ->
            {ok, []};
        Clients ->
            %% 从所有客户端聚合工具
            AllTools = lists:flatmap(fun({ClientName, ClientPid}) ->
                fetch_tools_from_client(ClientPid, ClientName, UsePrefix)
            end, Clients),
            {ok, AllTools}
    end.

%% @doc 按名称查找工具
%%
%% 优先从缓存查找，如果没有缓存则获取完整列表。
-spec find_tool(binary(), map()) -> {ok, tool_def()} | {error, not_found | term()}.
find_tool(Name, Opts) ->
    case list_tools(Opts) of
        {ok, Tools} ->
            find_tool_by_name(Name, Tools);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 按名称查找工具
-spec find_tool_by_name(binary(), [tool_def()]) -> {ok, tool_def()} | {error, not_found}.
find_tool_by_name(_Name, []) ->
    {error, not_found};
find_tool_by_name(Name, [#{name := Name} = Tool | _]) ->
    {ok, Tool};
find_tool_by_name(Name, [_ | Rest]) ->
    find_tool_by_name(Name, Rest).

%%====================================================================
%% 辅助函数
%%====================================================================

%% @doc 将 MCP 工具 map 转换为 tool_def()（无前缀）
%%
%% MCP 工具格式：
%% ```
%% #{
%%%     <<"name">> => <<"read_file">>,
%%%     <<"description">> => <<"Read file content">>,
%%%     <<"inputSchema">> => #{...}
%%% }
%% ```
-spec mcp_to_tool_def(map(), pid()) -> tool_def().
mcp_to_tool_def(McpTool, ClientPid) ->
    mcp_to_tool_def(McpTool, ClientPid, undefined).

%% @doc 将 MCP 工具 map 转换为 tool_def()（带可选前缀）
%%
%% 当 ClientName 不为 undefined 时，工具名称会变成 "ClientName:ToolName"
-spec mcp_to_tool_def(map(), pid(), binary() | undefined) -> tool_def().
mcp_to_tool_def(McpTool, ClientPid, ClientName) ->
    OriginalName = maps:get(<<"name">>, McpTool),
    Description = maps:get(<<"description">>, McpTool, <<"">>),
    InputSchema = maps:get(<<"inputSchema">>, McpTool, #{type => object}),

    %% 根据是否有 ClientName 决定工具名称
    Name = case ClientName of
        undefined -> OriginalName;
        _ -> <<ClientName/binary, ":", OriginalName/binary>>
    end,

    %% 如果有前缀，更新描述以说明来源
    FinalDesc = case ClientName of
        undefined -> Description;
        _ -> <<"[", ClientName/binary, "] ", Description/binary>>
    end,

    #{
        name => Name,
        description => FinalDesc,
        category => mcp,
        permissions => [network_access],
        parameters => InputSchema,
        handler => make_mcp_handler(OriginalName, ClientPid),
        metadata => #{
            source => mcp,
            client_pid => ClientPid,
            client_name => ClientName,
            original_name => OriginalName,
            original_tool => McpTool
        }
    }.

%% @doc 创建 MCP 工具处理器
%%
%% 返回一个函数，调用时会通过 MCP 客户端执行工具。
-spec make_mcp_handler(binary(), pid()) -> function().
make_mcp_handler(ToolName, ClientPid) ->
    fun(Args, _Context) ->
        case beamai_mcp_client:call_tool(ClientPid, ToolName, Args) of
            {ok, Result} ->
                %% 提取内容
                Content = beamai_mcp_adapter:extract_content(Result),
                {ok, Content};
            {error, Reason} ->
                {error, {mcp_call_failed, ToolName, Reason}}
        end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取 MCP 客户端列表
%%
%% 按优先级获取客户端：
%% 1. context 中的 mcp_clients 列表
%% 2. context 中的 mcp_client 单个客户端
%% 3. context 中的 mcp_tag 按标签过滤
%% 4. 注册表中的所有客户端
%% 5. 默认客户端
-spec get_clients(map()) -> [{binary() | undefined, pid()}].
get_clients(Context) ->
    case maps:get(mcp_clients, Context, undefined) of
        Pids when is_list(Pids) ->
            %% 直接指定的客户端列表（过滤无效的 PID）
            [{undefined, Pid} || Pid <- Pids, is_pid(Pid), is_process_alive(Pid)];
        undefined ->
            case maps:get(mcp_client, Context, undefined) of
                Pid when is_pid(Pid) ->
                    %% 向后兼容：单个客户端
                    case is_process_alive(Pid) of
                        true -> [{undefined, Pid}];
                        false -> get_clients_from_registry(Context)
                    end;
                _ ->
                    %% 从注册表获取
                    get_clients_from_registry(Context)
            end
    end.

%% @private 从注册表获取客户端
-spec get_clients_from_registry(map()) -> [{binary(), pid()}].
get_clients_from_registry(Context) ->
    case maps:get(mcp_tag, Context, undefined) of
        Tag when is_atom(Tag) ->
            %% 按标签过滤
            ClientInfos = beamai_mcp_client_registry:get_clients_by_tag(Tag),
            [{maps:get(name, Info), maps:get(pid, Info)} || Info <- ClientInfos];
        undefined ->
            %% 获取所有注册的客户端
            case get_registered_clients() of
                [] ->
                    %% 回退到默认客户端
                    case get_default_client() of
                        {ok, Pid} -> [{<<"default">>, Pid}];
                        error -> []
                    end;
                Clients ->
                    Clients
            end
    end.

%% @private 获取注册表中的所有客户端
-spec get_registered_clients() -> [{binary(), pid()}].
get_registered_clients() ->
    try
        ClientInfos = beamai_mcp_client_registry:list_clients(),
        [{maps:get(name, Info), maps:get(pid, Info)} || Info <- ClientInfos]
    catch
        exit:{noproc, _} ->
            %% 注册表服务未启动
            []
    end.

%% @private 获取默认 MCP 客户端
-spec get_default_client() -> {ok, pid()} | error.
get_default_client() ->
    case application:get_env(beamai_mcp, default_client) of
        {ok, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> {ok, Pid};
                false -> error
            end;
        _ -> error
    end.

%% @private 从单个客户端获取工具
-spec fetch_tools_from_client(pid(), binary() | undefined, boolean()) -> [tool_def()].
fetch_tools_from_client(ClientPid, ClientName, UsePrefix) ->
    case beamai_mcp_client:list_tools(ClientPid) of
        {ok, #{<<"tools">> := McpTools}} when is_list(McpTools) ->
            Prefix = case UsePrefix of
                true -> ClientName;
                false -> undefined
            end,
            [mcp_to_tool_def(T, ClientPid, Prefix) || T <- McpTools];
        {ok, Result} ->
            logger:warning("MCP list_tools 返回意外格式 (客户端: ~p): ~p",
                          [ClientName, Result]),
            [];
        {error, Reason} ->
            logger:warning("MCP list_tools 失败 (客户端: ~p): ~p",
                          [ClientName, Reason]),
            []
    end.
