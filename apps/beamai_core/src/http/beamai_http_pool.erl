%%%-------------------------------------------------------------------
%%% @doc HTTP 连接池管理器
%%%
%%% 为 Gun HTTP 客户端提供连接池管理，支持：
%%% - 连接复用
%%% - 自动重连
%%% - 空闲连接清理
%%% - 按主机分组管理
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 获取连接
%%% {ok, ConnPid} = beamai_http_pool:get_connection("https://api.example.com").
%%%
%%% %% 归还连接
%%% beamai_http_pool:return_connection(ConnPid).
%%%
%%% %% 标记连接失败（不再复用）
%%% beamai_http_pool:connection_failed(ConnPid).
%%% ```
%%%
%%% == 配置 ==
%%%
%%% ```erlang
%%% application:set_env(beamai_core, http_pool, #{
%%%     max_connections_per_host => 10,
%%%     connection_timeout => 5000,
%%%     idle_timeout => 60000
%%% }).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_pool).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([get_connection/1, get_connection/2]).
-export([return_connection/1]).
-export([connection_failed/1]).
-export([close_all/0]).
-export([stats/0]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(state, {
    %% 连接池: #{HostKey => [ConnPid]}
    pools = #{} :: #{binary() => [pid()]},
    %% 连接信息: #{ConnPid => #{host, created_at, last_used}}
    conn_info = #{} :: #{pid() => map()},
    %% 正在使用的连接
    in_use = #{} :: #{pid() => true},
    %% 配置
    config :: map()
}).

-type pool_config() :: #{
    max_connections_per_host => pos_integer(),
    connection_timeout => pos_integer(),
    idle_timeout => pos_integer()
}.

%%====================================================================
%% 默认配置
%%====================================================================

-define(DEFAULT_MAX_CONNECTIONS, 10).
-define(DEFAULT_CONN_TIMEOUT, 30000).  %% 增加到 30 秒，适应慢速 TLS 握手
-define(DEFAULT_IDLE_TIMEOUT, 60000).
-define(CLEANUP_INTERVAL, 30000).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动连接池
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc 启动连接池（带配置）
-spec start_link(pool_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc 获取到指定 URL 的连接
-spec get_connection(binary() | string()) -> {ok, pid()} | {error, term()}.
get_connection(Url) ->
    get_connection(Url, #{}).

%% @doc 获取连接（带选项）
-spec get_connection(binary() | string(), map()) -> {ok, pid()} | {error, term()}.
get_connection(Url, Opts) ->
    ensure_started(),
    gen_server:call(?MODULE, {get_connection, Url, Opts}, infinity).

%% @doc 归还连接到池
-spec return_connection(pid()) -> ok.
return_connection(ConnPid) ->
    gen_server:cast(?MODULE, {return_connection, ConnPid}).

%% @doc 标记连接失败
-spec connection_failed(pid()) -> ok.
connection_failed(ConnPid) ->
    gen_server:cast(?MODULE, {connection_failed, ConnPid}).

%% @doc 关闭所有连接
-spec close_all() -> ok.
close_all() ->
    gen_server:call(?MODULE, close_all).

%% @doc 获取池统计信息
-spec stats() -> map().
stats() ->
    gen_server:call(?MODULE, stats).

%%====================================================================
%% gen_server 回调
%%====================================================================

init(Config) ->
    %% 合并默认配置
    FullConfig = merge_config(Config),

    %% 启动定时清理
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_idle),

    {ok, #state{config = FullConfig}}.

handle_call({get_connection, Url, Opts}, _From, State) ->
    case do_get_connection(Url, Opts, State) of
        {ok, ConnPid, NewState} ->
            {reply, {ok, ConnPid}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(close_all, _From, State) ->
    do_close_all(State),
    {reply, ok, State#state{pools = #{}, conn_info = #{}, in_use = #{}}};

handle_call(stats, _From, #state{pools = Pools, conn_info = ConnInfo, in_use = InUse} = State) ->
    Stats = #{
        pools => maps:map(fun(_, Conns) -> length(Conns) end, Pools),
        total_connections => maps:size(ConnInfo),
        in_use => maps:size(InUse),
        idle => maps:size(ConnInfo) - maps:size(InUse)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({return_connection, ConnPid}, State) ->
    NewState = do_return_connection(ConnPid, State),
    {noreply, NewState};

handle_cast({connection_failed, ConnPid}, State) ->
    NewState = do_remove_connection(ConnPid, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_idle, State) ->
    NewState = do_cleanup_idle(State),
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_idle),
    {noreply, NewState};

handle_info({'DOWN', _Ref, process, ConnPid, _Reason}, State) ->
    %% 连接进程退出，从池中移除
    NewState = do_remove_connection(ConnPid, State),
    {noreply, NewState};

handle_info({gun_up, _ConnPid, _Protocol}, State) ->
    %% Gun 连接就绪
    {noreply, State};

handle_info({gun_down, ConnPid, _Protocol, _Reason, _KilledStreams}, State) ->
    %% Gun 连接断开
    NewState = do_remove_connection(ConnPid, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    do_close_all(State),
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 确保连接池已启动
%% 连接池应该由 beamai_core_sup 启动
%% 如果未启动，返回错误而不是尝试懒启动
ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            throw({error, {pool_not_started,
                "beamai_http_pool should be started by beamai_core_sup. "
                "Make sure beamai_core application is started."}});
        _Pid ->
            ok
    end.

%% @private 合并配置
merge_config(Config) ->
    AppConfig = application:get_env(beamai_core, http_pool, #{}),
    Defaults = #{
        max_connections_per_host => ?DEFAULT_MAX_CONNECTIONS,
        connection_timeout => ?DEFAULT_CONN_TIMEOUT,
        idle_timeout => ?DEFAULT_IDLE_TIMEOUT
    },
    maps:merge(maps:merge(Defaults, AppConfig), Config).

%% @private 获取连接
do_get_connection(Url, _Opts, #state{pools = Pools, conn_info = ConnInfo,
                                     in_use = InUse, config = Config} = State) ->
    UrlBin = to_binary(Url),
    {Host, Port, Transport} = parse_url(UrlBin),
    HostKey = make_host_key(Host, Port, Transport),

    %% 尝试从池中获取空闲连接
    case get_idle_connection(HostKey, Pools, InUse) of
        {ok, ConnPid, NewPools} ->
            %% 更新状态
            NewConnInfo = maps:update_with(ConnPid,
                fun(Info) -> Info#{last_used => erlang:system_time(millisecond)} end,
                ConnInfo),
            NewInUse = InUse#{ConnPid => true},
            {ok, ConnPid, State#state{pools = NewPools, conn_info = NewConnInfo, in_use = NewInUse}};
        none ->
            %% 检查是否可以创建新连接
            MaxConns = maps:get(max_connections_per_host, Config),
            CurrentCount = count_host_connections(HostKey, ConnInfo),
            case CurrentCount < MaxConns of
                true ->
                    %% 创建新连接
                    create_new_connection(Host, Port, Transport, HostKey, State);
                false ->
                    {error, pool_exhausted}
            end
    end.

%% @private 从池中获取空闲连接
get_idle_connection(HostKey, Pools, InUse) ->
    case maps:get(HostKey, Pools, []) of
        [] ->
            none;
        [ConnPid | Rest] ->
            case maps:is_key(ConnPid, InUse) of
                true ->
                    %% 这个连接正在使用，尝试下一个
                    case get_idle_from_list(Rest, InUse) of
                        {ok, IdleConn, NewRest} ->
                            {ok, IdleConn, Pools#{HostKey => [ConnPid | NewRest]}};
                        none ->
                            none
                    end;
                false ->
                    %% 检查连接是否存活
                    case is_process_alive(ConnPid) of
                        true ->
                            {ok, ConnPid, Pools#{HostKey => Rest}};
                        false ->
                            %% 连接已死，移除并尝试下一个
                            get_idle_connection(HostKey, Pools#{HostKey => Rest}, InUse)
                    end
            end
    end.

get_idle_from_list([], _InUse) ->
    none;
get_idle_from_list([Conn | Rest], InUse) ->
    case maps:is_key(Conn, InUse) of
        true ->
            case get_idle_from_list(Rest, InUse) of
                {ok, IdleConn, NewRest} ->
                    {ok, IdleConn, [Conn | NewRest]};
                none ->
                    none
            end;
        false ->
            case is_process_alive(Conn) of
                true ->
                    {ok, Conn, Rest};
                false ->
                    get_idle_from_list(Rest, InUse)
            end
    end.

%% @private 创建新连接
create_new_connection(Host, Port, Transport, HostKey,
                      #state{pools = _Pools, conn_info = ConnInfo,
                             in_use = InUse, config = Config} = State) ->
    ConnTimeout = maps:get(connection_timeout, Config),
    GunOpts = #{
        connect_timeout => ConnTimeout,
        transport => Transport,
        protocols => [http2, http],
        tls_opts => get_tls_opts()
    },

    case gun:open(Host, Port, GunOpts) of
        {ok, ConnPid} ->
            %% 监控连接进程
            erlang:monitor(process, ConnPid),

            %% 等待连接就绪
            case gun:await_up(ConnPid, ConnTimeout) of
                {ok, _Protocol} ->
                    Now = erlang:system_time(millisecond),
                    NewConnInfo = ConnInfo#{
                        ConnPid => #{
                            host_key => HostKey,
                            created_at => Now,
                            last_used => Now
                        }
                    },
                    NewInUse = InUse#{ConnPid => true},
                    %% 新连接不加入池，因为直接使用
                    {ok, ConnPid, State#state{conn_info = NewConnInfo, in_use = NewInUse}};
                {error, Reason} ->
                    gun:close(ConnPid),
                    {error, {connection_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {open_failed, Reason}}
    end.

%% @private 归还连接
do_return_connection(ConnPid, #state{pools = Pools, conn_info = ConnInfo,
                                      in_use = InUse} = State) ->
    case maps:get(ConnPid, ConnInfo, undefined) of
        undefined ->
            %% 未知连接
            State;
        #{host_key := HostKey} ->
            %% 从 in_use 移除
            NewInUse = maps:remove(ConnPid, InUse),
            %% 检查连接是否存活
            case is_process_alive(ConnPid) of
                true ->
                    %% 添加到池
                    HostConns = maps:get(HostKey, Pools, []),
                    NewPools = Pools#{HostKey => [ConnPid | HostConns]},
                    State#state{pools = NewPools, in_use = NewInUse};
                false ->
                    %% 连接已死，移除
                    do_remove_connection(ConnPid, State#state{in_use = NewInUse})
            end
    end.

%% @private 移除连接
do_remove_connection(ConnPid, #state{pools = Pools, conn_info = ConnInfo,
                                      in_use = InUse} = State) ->
    case maps:get(ConnPid, ConnInfo, undefined) of
        undefined ->
            State;
        #{host_key := HostKey} ->
            %% 从池中移除
            HostConns = maps:get(HostKey, Pools, []),
            NewHostConns = lists:delete(ConnPid, HostConns),
            NewPools = case NewHostConns of
                [] -> maps:remove(HostKey, Pools);
                _ -> Pools#{HostKey => NewHostConns}
            end,
            %% 关闭连接
            catch gun:close(ConnPid),
            %% 更新状态
            State#state{
                pools = NewPools,
                conn_info = maps:remove(ConnPid, ConnInfo),
                in_use = maps:remove(ConnPid, InUse)
            }
    end.

%% @private 清理空闲连接
do_cleanup_idle(#state{pools = _Pools, conn_info = ConnInfo,
                        in_use = InUse, config = Config} = State) ->
    IdleTimeout = maps:get(idle_timeout, Config),
    Now = erlang:system_time(millisecond),

    %% 找出过期的连接
    ExpiredConns = maps:fold(fun(ConnPid, #{last_used := LastUsed}, Acc) ->
        case maps:is_key(ConnPid, InUse) of
            true ->
                %% 正在使用，跳过
                Acc;
            false ->
                case Now - LastUsed > IdleTimeout of
                    true -> [ConnPid | Acc];
                    false -> Acc
                end
        end
    end, [], ConnInfo),

    %% 移除过期连接
    lists:foldl(fun(ConnPid, S) ->
        do_remove_connection(ConnPid, S)
    end, State, ExpiredConns).

%% @private 关闭所有连接
do_close_all(#state{conn_info = ConnInfo}) ->
    maps:foreach(fun(ConnPid, _Info) ->
        catch gun:close(ConnPid)
    end, ConnInfo).

%% @private 统计主机连接数
count_host_connections(HostKey, ConnInfo) ->
    maps:fold(fun(_ConnPid, #{host_key := HK}, Acc) ->
        case HK of
            HostKey -> Acc + 1;
            _ -> Acc
        end
    end, 0, ConnInfo).

%% @private 解析 URL
parse_url(Url) ->
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = Parsed ->
            Port = maps:get(port, Parsed, default_port(Scheme)),
            Transport = case Scheme of
                <<"https">> -> tls;
                "https" -> tls;
                _ -> tcp
            end,
            {to_charlist(Host), Port, Transport};
        _ ->
            throw({invalid_url, Url})
    end.

%% @private 默认端口
default_port(<<"https">>) -> 443;
default_port("https") -> 443;
default_port(<<"http">>) -> 80;
default_port("http") -> 80;
default_port(_) -> 80.

%% @private 生成主机键
make_host_key(Host, Port, Transport) ->
    iolist_to_binary([to_binary(Host), $:, integer_to_binary(Port),
                      $:, atom_to_binary(Transport)]).

%% @private 转换为二进制
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V).

%% @private 转换为字符列表
to_charlist(V) when is_list(V) -> V;
to_charlist(V) when is_binary(V) -> binary_to_list(V).

%% @private 获取 TLS 配置选项
%% 使用系统 CA 证书进行 TLS 验证（OTP 25+）
get_tls_opts() ->
    try
        %% OTP 25+ 支持 public_key:cacerts_get()
        CACerts = public_key:cacerts_get(),
        [
            {verify, verify_peer},
            {cacerts, CACerts},
            {depth, 3},
            {customize_hostname_check, [
                {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
            ]}
        ]
    catch
        _:_ ->
            %% 如果获取系统证书失败，使用 verify_none（仅用于开发/测试）
            error_logger:warning_msg("Failed to get system CA certs, using verify_none~n"),
            [{verify, verify_none}]
    end.
