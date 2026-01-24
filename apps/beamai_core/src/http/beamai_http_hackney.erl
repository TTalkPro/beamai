%%%-------------------------------------------------------------------
%%% @doc Hackney HTTP 客户端适配器
%%%
%%% 使用 hackney 作为后端的 HTTP 客户端实现。
%%% 这是默认的 HTTP 后端。
%%%
%%% == 特点 ==
%%% - 内置连接池
%%% - 同步 API
%%% - 稳定成熟
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_hackney).

-behaviour(beamai_http_behaviour).

%% Behaviour 回调
-export([request/5, stream_request/6, ensure_started/0]).

%%====================================================================
%% 默认配置
%%====================================================================

-define(DEFAULT_TIMEOUT, 30000).
-define(DEFAULT_CONNECT_TIMEOUT, 10000).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

%% @doc 确保 hackney 已启动
-spec ensure_started() -> ok.
ensure_started() ->
    application:ensure_all_started(hackney),
    ok.

%% @doc 发送 HTTP 请求
-spec request(atom(), binary() | string(), [{binary(), binary()}],
              binary(), map()) -> {ok, term()} | {error, term()}.
request(Method, Url, Headers, Body, Opts) ->
    ensure_started(),

    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
    ConnectTimeout = maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
    Pool = maps:get(pool, Opts, default),

    HackneyOpts = [
        {recv_timeout, Timeout},
        {connect_timeout, ConnectTimeout},
        {pool, Pool},
        with_body
    ],

    UrlBin = beamai_utils:to_binary(Url),
    BodyBin = beamai_utils:to_binary(Body),

    case hackney:request(Method, UrlBin, Headers, BodyBin, HackneyOpts) of
        {ok, StatusCode, _RespHeaders, RespBody} when StatusCode >= 200, StatusCode < 300 ->
            {ok, beamai_utils:decode_json_response(RespBody)};
        {ok, StatusCode, _RespHeaders, RespBody} when StatusCode >= 400 ->
            {error, {http_error, StatusCode, RespBody}};
        {ok, _StatusCode, _RespHeaders, RespBody} ->
            {ok, beamai_utils:decode_json_response(RespBody)};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% @doc 发送流式请求
-spec stream_request(atom(), binary() | string(), [{binary(), binary()}],
                     binary(), map(), fun()) -> {ok, term()} | {error, term()}.
stream_request(Method, Url, Headers, Body, Opts, Handler) ->
    ensure_started(),

    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
    ConnectTimeout = maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
    InitAcc = maps:get(init_acc, Opts, <<>>),

    HackneyOpts = [
        async,
        {recv_timeout, Timeout},
        {connect_timeout, ConnectTimeout}
    ],

    UrlBin = beamai_utils:to_binary(Url),
    BodyBin = beamai_utils:encode_body(Body),

    case hackney:request(Method, UrlBin, Headers, BodyBin, HackneyOpts) of
        {ok, ClientRef} ->
            stream_receive_loop(ClientRef, InitAcc, Handler, Timeout);
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 流式接收循环
%%
%% 处理 hackney 异步消息：
%% - {status, Code, _}: HTTP 状态码，非 2xx 时关闭连接
%% - {headers, _}: 响应头（忽略）
%% - done: 流结束
%% - Chunk (binary): 数据块，调用 Handler(Chunk, Acc)
%%   返回 {continue, NewAcc} 继续接收，{done, FinalAcc} 关闭连接
stream_receive_loop(ClientRef, Acc, Handler, Timeout) ->
    receive
        {hackney_response, ClientRef, {status, StatusCode, _Reason}} ->
            case StatusCode of
                Code when Code >= 200, Code < 300 ->
                    stream_receive_loop(ClientRef, Acc, Handler, Timeout);
                Code ->
                    hackney:close(ClientRef),
                    {error, {http_error, Code}}
            end;
        {hackney_response, ClientRef, {headers, _Headers}} ->
            stream_receive_loop(ClientRef, Acc, Handler, Timeout);
        {hackney_response, ClientRef, done} ->
            {ok, Acc};
        {hackney_response, ClientRef, Chunk} when is_binary(Chunk) ->
            case Handler(Chunk, Acc) of
                {continue, NewAcc} ->
                    stream_receive_loop(ClientRef, NewAcc, Handler, Timeout);
                {done, FinalAcc} ->
                    hackney:close(ClientRef),
                    {ok, FinalAcc}
            end;
        {hackney_response, ClientRef, {error, Reason}} ->
            {error, Reason}
    after Timeout ->
        hackney:close(ClientRef),
        {error, timeout}
    end.

