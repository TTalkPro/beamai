%%%-------------------------------------------------------------------
%%% @doc Gun HTTP 客户端适配器
%%%
%%% 使用 gun 作为后端的 HTTP 客户端实现。
%%%
%%% == 特点 ==
%%% - 支持 HTTP/2
%%% - 异步消息驱动
%%% - WebSocket 支持（未来）
%%%
%%% == 配置 ==
%%%
%%% ```erlang
%%% application:set_env(beamai_core, http_backend, beamai_http_gun).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_gun).

-behaviour(beamai_http_behaviour).

%% Behaviour 回调
-export([request/5, stream_request/6, ensure_started/0]).

%% 额外 API（用于高级场景）
-export([request_async/5, await_response/3]).

%%====================================================================
%% 默认配置
%%====================================================================

-define(DEFAULT_TIMEOUT, 30000).
-define(DEFAULT_CONNECT_TIMEOUT, 5000).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

%% @doc 确保 gun 已启动
-spec ensure_started() -> ok.
ensure_started() ->
    application:ensure_all_started(gun),
    ok.

%% @doc 发送 HTTP 请求（同步封装）
-spec request(atom(), binary() | string(), [{binary(), binary()}],
              binary(), map()) -> {ok, term()} | {error, term()}.
request(Method, Url, Headers, Body, Opts) ->
    ensure_started(),

    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),

    case request_async(Method, Url, Headers, Body, Opts) of
        {ok, ConnPid, StreamRef} ->
            Result = await_response(ConnPid, StreamRef, Timeout),
            %% 归还连接
            beamai_http_pool:return_connection(ConnPid),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 发送流式请求
-spec stream_request(atom(), binary() | string(), [{binary(), binary()}],
                     binary(), map(), fun()) -> {ok, term()} | {error, term()}.
stream_request(Method, Url, Headers, Body, Opts, Handler) ->
    ensure_started(),

    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
    InitAcc = maps:get(init_acc, Opts, <<>>),

    case request_async(Method, Url, Headers, Body, Opts) of
        {ok, ConnPid, StreamRef} ->
            Result = stream_receive_loop(ConnPid, StreamRef, InitAcc, Handler, Timeout),
            %% 归还连接
            beamai_http_pool:return_connection(ConnPid),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 高级 API
%%====================================================================

%% @doc 发送异步请求
-spec request_async(atom(), binary() | string(), [{binary(), binary()}],
                    binary(), map()) -> {ok, pid(), reference()} | {error, term()}.
request_async(Method, Url, Headers, Body, _Opts) ->
    UrlBin = beamai_utils:to_binary(Url),

    case beamai_http_pool:get_connection(UrlBin) of
        {ok, ConnPid} ->
            {Path, Query} = parse_path_and_query(UrlBin),
            FullPath = case Query of
                <<>> -> Path;
                _ -> <<Path/binary, "?", Query/binary>>
            end,

            %% 准备请求头
            ReqHeaders = prepare_headers(Headers),

            %% 发送请求
            StreamRef = case Method of
                get ->
                    gun:get(ConnPid, FullPath, ReqHeaders);
                head ->
                    gun:head(ConnPid, FullPath, ReqHeaders);
                delete ->
                    gun:delete(ConnPid, FullPath, ReqHeaders);
                post ->
                    gun:post(ConnPid, FullPath, ReqHeaders, beamai_utils:encode_body(Body));
                put ->
                    gun:put(ConnPid, FullPath, ReqHeaders, beamai_utils:encode_body(Body));
                patch ->
                    gun:patch(ConnPid, FullPath, ReqHeaders, beamai_utils:encode_body(Body));
                options ->
                    gun:options(ConnPid, FullPath, ReqHeaders)
            end,

            {ok, ConnPid, StreamRef};
        {error, Reason} ->
            {error, {connection_failed, Reason}}
    end.

%% @doc 等待响应（同步）
-spec await_response(pid(), reference(), pos_integer()) ->
    {ok, term()} | {error, term()}.
await_response(ConnPid, StreamRef, Timeout) ->
    receive_response(ConnPid, StreamRef, <<>>, Timeout).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 接收响应
receive_response(ConnPid, StreamRef, Acc, Timeout) ->
    receive
        {gun_response, ConnPid, StreamRef, fin, Status, _Headers}
          when Status >= 200, Status < 300 ->
            %% 无 body 的成功响应
            {ok, beamai_utils:decode_json_response(Acc)};
        {gun_response, ConnPid, StreamRef, fin, Status, _Headers}
          when Status >= 400 ->
            {error, {http_error, Status, Acc}};
        {gun_response, ConnPid, StreamRef, fin, _Status, _Headers} ->
            {ok, beamai_utils:decode_json_response(Acc)};

        {gun_response, ConnPid, StreamRef, nofin, Status, _Headers}
          when Status >= 200, Status < 300 ->
            %% 有 body，继续接收
            receive_response(ConnPid, StreamRef, Acc, Timeout);
        {gun_response, ConnPid, StreamRef, nofin, Status, _Headers}
          when Status >= 400 ->
            %% 错误响应，收集错误 body
            receive_error_body(ConnPid, StreamRef, Status, Acc, Timeout);
        {gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} ->
            receive_response(ConnPid, StreamRef, Acc, Timeout);

        {gun_data, ConnPid, StreamRef, fin, Data} ->
            FinalBody = <<Acc/binary, Data/binary>>,
            {ok, beamai_utils:decode_json_response(FinalBody)};
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            receive_response(ConnPid, StreamRef, <<Acc/binary, Data/binary>>, Timeout);

        {gun_error, ConnPid, StreamRef, Reason} ->
            {error, {gun_error, Reason}};
        {gun_error, ConnPid, Reason} ->
            {error, {gun_error, Reason}}
    after Timeout ->
        gun:cancel(ConnPid, StreamRef),
        {error, timeout}
    end.

%% @private 接收错误响应 body
receive_error_body(ConnPid, StreamRef, Status, Acc, Timeout) ->
    receive
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            {error, {http_error, Status, <<Acc/binary, Data/binary>>}};
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            receive_error_body(ConnPid, StreamRef, Status, <<Acc/binary, Data/binary>>, Timeout);
        {gun_error, ConnPid, StreamRef, Reason} ->
            {error, {gun_error, Reason}}
    after Timeout ->
        {error, {http_error, Status, Acc}}
    end.

%% @private 流式接收循环
stream_receive_loop(ConnPid, StreamRef, Acc, Handler, Timeout) ->
    receive
        {gun_response, ConnPid, StreamRef, fin, Status, _Headers}
          when Status >= 200, Status < 300 ->
            {ok, Acc};
        {gun_response, ConnPid, StreamRef, fin, Status, _Headers} ->
            {error, {http_error, Status}};
        {gun_response, ConnPid, StreamRef, nofin, Status, _Headers}
          when Status >= 200, Status < 300 ->
            stream_receive_loop(ConnPid, StreamRef, Acc, Handler, Timeout);
        {gun_response, ConnPid, StreamRef, nofin, Status, _Headers} ->
            {error, {http_error, Status}};

        {gun_data, ConnPid, StreamRef, fin, Data} ->
            case Handler(Data, Acc) of
                {continue, FinalAcc} -> {ok, FinalAcc};
                {done, FinalAcc} -> {ok, FinalAcc}
            end;
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            case Handler(Data, Acc) of
                {continue, NewAcc} ->
                    stream_receive_loop(ConnPid, StreamRef, NewAcc, Handler, Timeout);
                {done, FinalAcc} ->
                    gun:cancel(ConnPid, StreamRef),
                    {ok, FinalAcc}
            end;

        {gun_error, ConnPid, StreamRef, Reason} ->
            {error, {gun_error, Reason}};
        {gun_error, ConnPid, Reason} ->
            {error, {gun_error, Reason}}
    after Timeout ->
        gun:cancel(ConnPid, StreamRef),
        {error, timeout}
    end.

%% @private 解析路径和查询字符串
parse_path_and_query(Url) ->
    case uri_string:parse(Url) of
        #{path := Path} = Parsed ->
            Query = maps:get(query, Parsed, <<>>),
            PathBin = case Path of
                <<>> -> <<"/">>;
                P -> beamai_utils:to_binary(P)
            end,
            {PathBin, beamai_utils:to_binary(Query)};
        _ ->
            {<<"/">>, <<>>}
    end.

%% @private 准备请求头
prepare_headers(Headers) ->
    %% 确保有 User-Agent
    HasUA = lists:any(fun({K, _}) ->
        string:lowercase(K) =:= <<"user-agent">>
    end, Headers),
    case HasUA of
        true -> Headers;
        false -> [{<<"user-agent">>, <<"BeamAI/1.0 Gun/2.1">>} | Headers]
    end.

