%%%-------------------------------------------------------------------
%%% @doc MCP 客户端注册表
%%%
%%% 统一管理所有 MCP 客户端连接，支持：
%%% - 客户端注册和注销
%%% - 按名称或标签查找客户端
%%% - 自动清理已死亡的客户端
%%% - 列出所有活跃客户端
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 启动客户端并注册
%%% {ok, Client} = beamai_mcp_client:start_link(Config),
%%% ok = beamai_mcp_client_registry:register(<<"filesystem">>, Client, #{
%%%     server_info => #{name => <<"fs-server">>},
%%%     tags => [file, local]
%%% }),
%%%
%%% %% 按名称查找
%%% {ok, Client} = beamai_mcp_client_registry:get_client(<<"filesystem">>),
%%%
%%% %% 按标签查找
%%% Clients = beamai_mcp_client_registry:get_clients_by_tag(file),
%%%
%%% %% 列出所有客户端
%%% AllClients = beamai_mcp_client_registry:list_clients().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_client_registry).

-behaviour(gen_server).

%%====================================================================
%% API 导出
%%====================================================================

-export([
    start_link/0,
    stop/0
]).

%% 注册管理
-export([
    register/2,
    register/3,
    unregister/1,
    is_registered/1
]).

%% 查询
-export([
    get_client/1,
    get_client_info/1,
    list_clients/0,
    list_client_pids/0,
    get_clients_by_tag/1,
    count/0
]).

%% 便捷函数：启动并注册
-export([
    start_and_register/2,
    start_and_register/3
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
%% 类型定义
%%====================================================================

-type client_name() :: binary() | atom().
-type client_info() :: #{
    pid := pid(),
    name := client_name(),
    registered_at := integer(),
    server_info => map(),
    tags => [atom()],
    metadata => map()
}.

-export_type([client_name/0, client_info/0]).

%%====================================================================
%% 宏定义
%%====================================================================

-define(SERVER, ?MODULE).
-define(TABLE, beamai_mcp_clients).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动注册表服务
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 停止注册表服务
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc 注册 MCP 客户端（简单版本）
-spec register(client_name(), pid()) -> ok | {error, term()}.
register(Name, Pid) ->
    register(Name, Pid, #{}).

%% @doc 注册 MCP 客户端（带元数据）
%%
%% Opts:
%% - server_info: MCP 服务器信息
%% - tags: 标签列表，用于分类查找
%% - metadata: 其他元数据
-spec register(client_name(), pid(), map()) -> ok | {error, term()}.
register(Name, Pid, Opts) when is_pid(Pid) ->
    gen_server:call(?SERVER, {register, Name, Pid, Opts}).

%% @doc 注销 MCP 客户端
-spec unregister(client_name() | pid()) -> ok | {error, not_found}.
unregister(NameOrPid) ->
    gen_server:call(?SERVER, {unregister, NameOrPid}).

%% @doc 检查客户端是否已注册
-spec is_registered(client_name() | pid()) -> boolean().
is_registered(NameOrPid) ->
    gen_server:call(?SERVER, {is_registered, NameOrPid}).

%% @doc 按名称获取客户端 PID
-spec get_client(client_name()) -> {ok, pid()} | {error, not_found}.
get_client(Name) ->
    gen_server:call(?SERVER, {get_client, Name}).

%% @doc 获取客户端完整信息
-spec get_client_info(client_name() | pid()) -> {ok, client_info()} | {error, not_found}.
get_client_info(NameOrPid) ->
    gen_server:call(?SERVER, {get_client_info, NameOrPid}).

%% @doc 列出所有客户端信息
-spec list_clients() -> [client_info()].
list_clients() ->
    gen_server:call(?SERVER, list_clients).

%% @doc 列出所有客户端 PID
-spec list_client_pids() -> [pid()].
list_client_pids() ->
    gen_server:call(?SERVER, list_client_pids).

%% @doc 按标签获取客户端
-spec get_clients_by_tag(atom()) -> [client_info()].
get_clients_by_tag(Tag) ->
    gen_server:call(?SERVER, {get_clients_by_tag, Tag}).

%% @doc 获取注册的客户端数量
-spec count() -> non_neg_integer().
count() ->
    gen_server:call(?SERVER, count).

%% @doc 启动 MCP 客户端并自动注册
-spec start_and_register(client_name(), map()) -> {ok, pid()} | {error, term()}.
start_and_register(Name, ClientConfig) ->
    start_and_register(Name, ClientConfig, #{}).

%% @doc 启动 MCP 客户端并自动注册（带元数据）
-spec start_and_register(client_name(), map(), map()) -> {ok, pid()} | {error, term()}.
start_and_register(Name, ClientConfig, RegistryOpts) ->
    case beamai_mcp_client:start_link(ClientConfig) of
        {ok, Pid} ->
            case register(Name, Pid, RegistryOpts) of
                ok -> {ok, Pid};
                {error, Reason} ->
                    %% 注册失败，停止客户端
                    beamai_mcp_client:stop(Pid),
                    {error, {register_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {start_failed, Reason}}
    end.

%%====================================================================
%% gen_server 回调
%%====================================================================

-record(state, {
    clients :: #{client_name() => client_info()},
    pid_to_name :: #{pid() => client_name()}
}).

init([]) ->
    %% 创建 ETS 表用于快速查找（可选优化）
    State = #state{
        clients = #{},
        pid_to_name = #{}
    },
    {ok, State}.

handle_call({register, Name, Pid, Opts}, _From, State) ->
    #state{clients = Clients, pid_to_name = PidToName} = State,

    %% 检查名称是否已存在
    case maps:is_key(Name, Clients) of
        true ->
            {reply, {error, {already_registered, Name}}, State};
        false ->
            %% 监控客户端进程
            erlang:monitor(process, Pid),

            %% 构建客户端信息
            Info = #{
                pid => Pid,
                name => Name,
                registered_at => erlang:system_time(millisecond),
                server_info => maps:get(server_info, Opts, #{}),
                tags => maps:get(tags, Opts, []),
                metadata => maps:get(metadata, Opts, #{})
            },

            NewState = State#state{
                clients = Clients#{Name => Info},
                pid_to_name = PidToName#{Pid => Name}
            },
            {reply, ok, NewState}
    end;

handle_call({unregister, NameOrPid}, _From, State) ->
    case do_unregister(NameOrPid, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({is_registered, Name}, _From, #state{clients = Clients} = State)
  when is_binary(Name); is_atom(Name) ->
    {reply, maps:is_key(Name, Clients), State};

handle_call({is_registered, Pid}, _From, #state{pid_to_name = PidToName} = State)
  when is_pid(Pid) ->
    {reply, maps:is_key(Pid, PidToName), State};

handle_call({get_client, Name}, _From, #state{clients = Clients} = State) ->
    Result = case maps:find(Name, Clients) of
        {ok, #{pid := Pid}} ->
            %% 验证进程还活着
            case is_process_alive(Pid) of
                true -> {ok, Pid};
                false -> {error, not_found}
            end;
        error ->
            {error, not_found}
    end,
    {reply, Result, State};

handle_call({get_client_info, Name}, _From, #state{clients = Clients} = State)
  when is_binary(Name); is_atom(Name) ->
    Result = case maps:find(Name, Clients) of
        {ok, Info} -> {ok, Info};
        error -> {error, not_found}
    end,
    {reply, Result, State};

handle_call({get_client_info, Pid}, _From, State)
  when is_pid(Pid) ->
    #state{clients = Clients, pid_to_name = PidToName} = State,
    Result = case maps:find(Pid, PidToName) of
        {ok, Name} ->
            case maps:find(Name, Clients) of
                {ok, Info} -> {ok, Info};
                error -> {error, not_found}
            end;
        error ->
            {error, not_found}
    end,
    {reply, Result, State};

handle_call(list_clients, _From, #state{clients = Clients} = State) ->
    %% 过滤掉已死亡的客户端
    ActiveClients = maps:filter(fun(_Name, #{pid := Pid}) ->
        is_process_alive(Pid)
    end, Clients),
    {reply, maps:values(ActiveClients), State};

handle_call(list_client_pids, _From, #state{clients = Clients} = State) ->
    Pids = [Pid || #{pid := Pid} <- maps:values(Clients), is_process_alive(Pid)],
    {reply, Pids, State};

handle_call({get_clients_by_tag, Tag}, _From, #state{clients = Clients} = State) ->
    Filtered = maps:filter(fun(_Name, #{pid := Pid, tags := Tags}) ->
        is_process_alive(Pid) andalso lists:member(Tag, Tags)
    end, Clients),
    {reply, maps:values(Filtered), State};

handle_call(count, _From, #state{clients = Clients} = State) ->
    %% 只计算活跃的客户端
    Count = maps:fold(fun(_Name, #{pid := Pid}, Acc) ->
        case is_process_alive(Pid) of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Clients),
    {reply, Count, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc 处理客户端进程退出
handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    %% 自动清理已死亡的客户端
    case do_unregister(Pid, State) of
        {ok, NewState} ->
            logger:info("MCP 客户端已断开连接并自动注销: ~p", [Pid]),
            {noreply, NewState};
        {error, _} ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 执行注销操作
-spec do_unregister(client_name() | pid(), #state{}) ->
    {ok, #state{}} | {error, not_found}.
do_unregister(Name, #state{clients = Clients, pid_to_name = PidToName} = State)
  when is_binary(Name); is_atom(Name) ->
    case maps:find(Name, Clients) of
        {ok, #{pid := Pid}} ->
            NewState = State#state{
                clients = maps:remove(Name, Clients),
                pid_to_name = maps:remove(Pid, PidToName)
            },
            {ok, NewState};
        error ->
            {error, not_found}
    end;

do_unregister(Pid, #state{pid_to_name = PidToName} = State)
  when is_pid(Pid) ->
    case maps:find(Pid, PidToName) of
        {ok, Name} ->
            do_unregister(Name, State);
        error ->
            {error, not_found}
    end.
