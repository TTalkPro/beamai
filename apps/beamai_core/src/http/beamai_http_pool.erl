%%%-------------------------------------------------------------------
%%% @doc HTTP 连接池管理器
%%%
%%% 为 Gun HTTP 客户端提供连接池管理，支持：
%%% - 连接复用
%%% - 自动重连
%%% - 空闲连接清理
%%% - 按主机分组管理
%%%
%%% 本模块是"池实例"的实现：beamai_core_sup 按用途启动三个实例
%%% （http_pool_short / http_pool_stream / http_pool_longpoll），
%%% 分别承载短请求、SSE 流式、异步长轮询三类流量，互不争用连接预算。
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 获取连接（第一个参数是池名）
%%% {ok, ConnPid} = beamai_http_pool:get_connection(http_pool_short,
%%%                                                 "https://api.example.com").
%%%
%%% %% 归还连接
%%% beamai_http_pool:return_connection(http_pool_short, ConnPid).
%%%
%%% %% 标记连接失败（不再复用）
%%% beamai_http_pool:connection_failed(http_pool_short, ConnPid).
%%% ```
%%%
%%% == 配置 ==
%%%
%%% 池配置由 beamai_core_sup 解析后经 start_link/2 传入，
%%% 本模块不读 app env（这是按池差异化配置的关键）。
%%% 用户侧配置入口见 beamai_core_sup（遗留 http_pool 键仍兼容）：
%%%
%%% ```erlang
%%% application:set_env(beamai_core, http_pool, #{
%%%     max_connections_per_host => 10,
%%%     connect_timeout => 5000,       %% 旧键名 connection_timeout 仍兼容（优先生效）
%%%     idle_timeout => 60000,
%%%     protocols => [http]            %% 可配 [http2, http] 启用 HTTP/2（先验证，见下）
%%% }).
%%% ```
%%%
%%% 注意：Gun 2.1 的 TCP 连接路径对 protocols 做单元素匹配
%%% （`[Protocol] = maps:get(protocols, Opts, [http])`），
%%% 配置 [http2, http] 在纯 TCP（http://）下会 badmatch 崩掉连接，
%%% TLS（https://）路径正常。默认 [http] 保证两种 scheme 均可用。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_pool).

-behaviour(gen_server).

%% API（全部以池名为第一个参数）
-export([start_link/2]).
-export([get_connection/2, get_connection/3]).
-export([return_connection/2]).
-export([connection_failed/2]).
-export([close_all/1]).
-export([stats/1]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-ifdef(TEST).
-export([merge_config/1, validate_protocols/1]).
-endif.

%%====================================================================
%% 类型定义
%%====================================================================

-record(state, {
    %% 池注册名（http_pool_short 等）
    name :: atom(),
    %% 连接池: #{HostKey => [ConnPid]}
    pools = #{} :: #{binary() => [pid()]},
    %% 连接信息: #{ConnPid => #{host, created_at, last_used}}
    conn_info = #{} :: #{pid() => map()},
    %% 正在使用的连接
    in_use = #{} :: #{pid() => true},
    %% 配置
    config :: map()
}).

-type pool_name() :: http_pool_short | http_pool_stream | http_pool_longpoll.

-type pool_config() :: #{
    max_connections_per_host => pos_integer(),
    connect_timeout => pos_integer(),      %% ms，对应 gun:open 的 connect_timeout
    connection_timeout => pos_integer(),   %% 已废弃：connect_timeout 的旧键名，仍兼容
    idle_timeout => pos_integer(),
    protocols => [http | http2]            %% 传给 gun:open 的 protocols
}.

-export_type([pool_name/0, pool_config/0]).

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

%% @doc 启动指定名字的连接池实例（由 beamai_core_sup 调用）
-spec start_link(pool_name(), pool_config()) -> {ok, pid()} | {error, term()}.
start_link(PoolName, Config) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [PoolName, Config], []).

%% @doc 获取到指定 URL 的连接
-spec get_connection(pool_name(), binary() | string()) -> {ok, pid()} | {error, term()}.
get_connection(PoolName, Url) ->
    get_connection(PoolName, Url, #{}).

%% @doc 获取连接（带选项）
%% 池未启动时返回 {error, {pool_not_started, PoolName}}（不再 throw 跨进程边界）
-spec get_connection(pool_name(), binary() | string(), map()) -> {ok, pid()} | {error, term()}.
get_connection(PoolName, Url, Opts) ->
    case whereis(PoolName) of
        undefined ->
            {error, {pool_not_started, PoolName}};
        _ ->
            gen_server:call(PoolName, {get_connection, Url, Opts}, infinity)
    end.

%% @doc 归还连接到池
-spec return_connection(pool_name(), pid()) -> ok.
return_connection(PoolName, ConnPid) ->
    gen_server:cast(PoolName, {return_connection, ConnPid}).

%% @doc 标记连接失败
-spec connection_failed(pool_name(), pid()) -> ok.
connection_failed(PoolName, ConnPid) ->
    gen_server:cast(PoolName, {connection_failed, ConnPid}).

%% @doc 关闭所有连接
-spec close_all(pool_name()) -> ok.
close_all(PoolName) ->
    gen_server:call(PoolName, close_all).

%% @doc 获取池统计信息
-spec stats(pool_name()) -> map().
stats(PoolName) ->
    gen_server:call(PoolName, stats).

%%====================================================================
%% gen_server 回调
%%====================================================================

init([PoolName, Config]) ->
    %% 合并默认配置
    FullConfig = merge_config(Config),

    %% 配置错误在启动时报出，而不是等到第一次建连
    ok = validate_protocols(maps:get(protocols, FullConfig)),

    %% 启动定时清理
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_idle),

    {ok, #state{name = PoolName, config = FullConfig}}.

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

handle_call(stats, _From, #state{name = Name, pools = Pools, conn_info = ConnInfo,
                                 in_use = InUse, config = Config} = State) ->
    Stats = #{
        name => Name,
        pools => maps:map(fun(_, Conns) -> length(Conns) end, Pools),
        total_connections => maps:size(ConnInfo),
        in_use => maps:size(InUse),
        idle => maps:size(ConnInfo) - maps:size(InUse),
        %% 回显生效配置，便于运维核对 sys.config 是否真正生效
        config => Config
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

%% @private 合并配置
%% 池实例不读 app env（env 解析在 beamai_core_sup，这是按池差异化配置的关键）；
%% 这里只做模块级默认值兜底 + 旧键名归一化。
%% 旧键名 connection_timeout 归一化为 connect_timeout（旧键优先，保证遗留配置行为不变）。
merge_config(Config) ->
    Defaults = #{
        max_connections_per_host => ?DEFAULT_MAX_CONNECTIONS,
        connect_timeout => ?DEFAULT_CONN_TIMEOUT,
        idle_timeout => ?DEFAULT_IDLE_TIMEOUT,
        %% Gun 2.1 的 TCP 连接路径做 [Protocol] = maps:get(protocols, Opts, [http]) 单元素匹配，
        %% 传 [http2, http] 会 {badmatch, [http2, http]} 崩掉所有 http:// 连接，
        %% 故默认 [http]（HTTP/1.1）；需要 HTTP/2 的部署显式配置 protocols => [http2, http]。
        protocols => [http]
    },
    normalize_legacy_keys(maps:merge(Defaults, Config)).

%% @private 旧键名 connection_timeout -> connect_timeout（gun 选项名）
normalize_legacy_keys(#{connection_timeout := V} = Config) ->
    maps:remove(connection_timeout, Config#{connect_timeout => V});
normalize_legacy_keys(Config) ->
    Config.

%% @private 校验 protocols 配置，只接受 http / http2
validate_protocols([_ | _] = Protocols) ->
    lists:foreach(fun
        (http) -> ok;
        (http2) -> ok;
        (Other) -> error({invalid_protocol, Other})
    end, Protocols),
    ok;
validate_protocols(Other) ->
    error({invalid_protocols, Other}).

%% @private 获取连接
do_get_connection(Url, _Opts, #state{pools = Pools, conn_info = ConnInfo,
                                     in_use = InUse, config = Config} = State) ->
    UrlBin = beamai_utils:to_binary(Url),
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
    ConnTimeout = maps:get(connect_timeout, Config),
    GunOpts = #{
        connect_timeout => ConnTimeout,
        transport => Transport,
        protocols => maps:get(protocols, Config),
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
    iolist_to_binary([beamai_utils:to_binary(Host), $:, integer_to_binary(Port),
                      $:, atom_to_binary(Transport)]).

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
            logger:warning("Failed to get system CA certs, using verify_none"),
            [{verify, verify_none}]
    end.
