%%%-------------------------------------------------------------------
%%% @doc HTTP 客户端工具模块
%%%
%%% 提供统一的 HTTP 请求接口，支持各种 API 集成。
%%% 使用 hackney 作为底层 HTTP 客户端，提供高性能和稳定性。
%%%
%%% 功能特性:
%%%   - GET/POST/PUT/DELETE 请求
%%%   - JSON 自动编解码
%%%   - 超时和重试机制
%%%   - 请求头管理
%%%   - 连接池管理
%%%
%%% 使用示例:
%%%
%%%   简单 GET 请求:
%%%   {ok, Body} = beamai_http:get("https://api.example.com/data").
%%%
%%%   带参数的 GET 请求:
%%%   {ok, Body} = beamai_http:get("https://api.example.com/search",
%%%                               #{query => "erlang"}).
%%%
%%%   POST JSON 数据:
%%%   {ok, Response} = beamai_http:post_json("https://api.example.com/data",
%%%                                         #{key => value}).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http).

%% API 导出
-export([get/1, get/2, get/3]).
-export([post/3, post/4]).
-export([post_json/2, post_json/3]).
-export([put/3, put/4]).
-export([delete/1, delete/2, delete/3]).
-export([request/5, request/6]).

%% 流式请求 API
-export([stream_request/5, stream_request/6]).

%% 工具函数导出
-export([url_encode/1, build_url/2]).
-export([ensure_started/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type url() :: string() | binary().
-type headers() :: [{binary(), binary()}].
-type params() :: #{atom() | binary() => term()}.
-type body() :: binary() | string() | map() | list().
-type options() :: #{
    timeout => pos_integer(),       %% 请求超时（毫秒）
    connect_timeout => pos_integer(),%% 连接超时（毫秒）
    headers => headers(),           %% 额外请求头
    retry => non_neg_integer(),     %% 重试次数
    retry_delay => pos_integer(),   %% 重试延迟（毫秒）
    pool => atom(),                 %% 连接池名称
    init_acc => term()              %% 流式请求初始累加器
}.
-type response() :: {ok, body()} | {error, term()}.

%% 流式请求类型
-type stream_callback() :: fun((binary()) -> any()).
-type chunk_handler() :: fun((binary(), term()) -> {continue, term()} | {done, term()}).
-type stream_response() :: {ok, term()} | {error, term()}.

-export_type([url/0, headers/0, params/0, body/0, options/0, response/0]).
-export_type([stream_callback/0, chunk_handler/0, stream_response/0]).

%%====================================================================
%% 默认配置
%%====================================================================

-define(DEFAULT_TIMEOUT, 30000).        %% 30 秒
-define(DEFAULT_CONNECT_TIMEOUT, 10000). %% 10 秒
-define(DEFAULT_RETRY, 0).
-define(DEFAULT_RETRY_DELAY, 1000).      %% 1 秒
-define(USER_AGENT, <<"ErlangAgent/2.0">>).

%%====================================================================
%% 公共 API - GET 请求
%%====================================================================

%% @doc 发送简单 GET 请求
-spec get(url()) -> response().
get(Url) ->
    get(Url, #{}, #{}).

%% @doc 发送带参数的 GET 请求
-spec get(url(), params()) -> response().
get(Url, Params) ->
    get(Url, Params, #{}).

%% @doc 发送带参数和选项的 GET 请求
-spec get(url(), params(), options()) -> response().
get(Url, Params, Opts) ->
    do_request_with_params(get, Url, Params, Opts).

%%====================================================================
%% 公共 API - DELETE 请求
%%====================================================================

%% @doc 发送简单 DELETE 请求
-spec delete(url()) -> response().
delete(Url) ->
    delete(Url, #{}, #{}).

%% @doc 发送带参数的 DELETE 请求
-spec delete(url(), params()) -> response().
delete(Url, Params) ->
    delete(Url, Params, #{}).

%% @doc 发送带参数和选项的 DELETE 请求
-spec delete(url(), params(), options()) -> response().
delete(Url, Params, Opts) ->
    do_request_with_params(delete, Url, Params, Opts).

%%====================================================================
%% 公共 API - POST 请求
%%====================================================================

%% @doc 发送 POST 请求
-spec post(url(), binary(), body()) -> response().
post(Url, ContentType, Body) ->
    post(Url, ContentType, Body, #{}).

%% @doc 发送带选项的 POST 请求
-spec post(url(), binary(), body(), options()) -> response().
post(Url, ContentType, Body, Opts) ->
    do_request_with_body(post, Url, ContentType, Body, Opts).

%% @doc 发送 JSON POST 请求
-spec post_json(url(), map() | list()) -> response().
post_json(Url, Data) ->
    post_json(Url, Data, #{}).

%% @doc 发送带选项的 JSON POST 请求
-spec post_json(url(), map() | list(), options()) -> response().
post_json(Url, Data, Opts) ->
    Body = jsx:encode(Data),
    post(Url, <<"application/json">>, Body, Opts).

%%====================================================================
%% 公共 API - PUT 请求
%%====================================================================

%% @doc 发送 PUT 请求
-spec put(url(), binary(), body()) -> response().
put(Url, ContentType, Body) ->
    put(Url, ContentType, Body, #{}).

%% @doc 发送带选项的 PUT 请求
-spec put(url(), binary(), body(), options()) -> response().
put(Url, ContentType, Body, Opts) ->
    do_request_with_body(put, Url, ContentType, Body, Opts).

%%====================================================================
%% 公共 API - 通用请求
%%====================================================================

%% @doc 发送 HTTP 请求
-spec request(atom(), url(), headers(), body(), options()) -> response().
request(Method, Url, ExtraHeaders, Body, Opts) ->
    do_request(Method, Url, ExtraHeaders, Body, Opts, 0).

%% @doc 发送 HTTP 请求（带重试计数）
-spec request(atom(), url(), headers(), body(), options(), non_neg_integer()) -> response().
request(Method, Url, ExtraHeaders, Body, Opts, _Attempt) ->
    do_request(Method, Url, ExtraHeaders, Body, Opts, 0).

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc URL 编码
-spec url_encode(term()) -> binary().
url_encode(Value) when is_binary(Value) ->
    url_encode(binary_to_list(Value));
url_encode(Value) when is_atom(Value) ->
    url_encode(atom_to_list(Value));
url_encode(Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value));
url_encode(Value) when is_list(Value) ->
    list_to_binary(uri_string:quote(Value)).

%% @doc 构建带参数的 URL
-spec build_url(url(), params()) -> binary().
build_url(Url, Params) when map_size(Params) == 0 ->
    beamai_utils:to_binary(Url);
build_url(Url, Params) ->
    QueryString = build_query_string(Params),
    BaseUrl = beamai_utils:to_binary(Url),
    case binary:match(BaseUrl, <<"?">>) of
        nomatch -> <<BaseUrl/binary, "?", QueryString/binary>>;
        _ -> <<BaseUrl/binary, "&", QueryString/binary>>
    end.

%% @doc 确保 hackney 已启动
-spec ensure_started() -> ok.
ensure_started() ->
    application:ensure_all_started(hackney),
    ok.

%%====================================================================
%% 内部函数 - 请求执行
%%====================================================================

%% @private 发送带参数的请求（用于 GET 和 DELETE）
%%
%% 构建 URL 并发送无请求体的 HTTP 请求。
%%
%% @param Method HTTP 方法（get 或 delete）
%% @param URL 目标 URL
%% @param Params 查询参数
%% @param Opts 请求选项
%% @returns 请求响应
-spec do_request_with_params(atom(), url(), params(), options()) -> response().
do_request_with_params(Method, Url, Params, Opts) ->
    FullUrl = build_url(Url, Params),
    request(Method, FullUrl, [], <<>>, Opts).

%% @private 发送带请求体的请求（用于 POST 和 PUT）
%%
%% 构建 Content-Type 头并发送带请求体的 HTTP 请求。
%%
%% @param Method HTTP 方法（post 或 put）
%% @param URL 目标 URL
%% @param ContentType 内容类型
%% @param Body 请求体
%% @param Opts 请求选项
%% @returns 请求响应
-spec do_request_with_body(atom(), url(), binary(), body(), options()) -> response().
do_request_with_body(Method, Url, ContentType, Body, Opts) ->
    Headers = [{<<"Content-Type">>, ContentType}],
    request(Method, Url, Headers, Body, Opts).

%% @private 执行 HTTP 请求
-spec do_request(atom(), url(), headers(), body(), options(), non_neg_integer()) -> response().
do_request(Method, Url, ExtraHeaders, Body, Opts, Attempt) ->
    ensure_started(),

    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
    ConnectTimeout = maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
    Retry = maps:get(retry, Opts, ?DEFAULT_RETRY),
    RetryDelay = maps:get(retry_delay, Opts, ?DEFAULT_RETRY_DELAY),
    CustomHeaders = maps:get(headers, Opts, []),
    Pool = maps:get(pool, Opts, default),

    Headers = default_headers() ++ CustomHeaders ++ ExtraHeaders,
    UrlBin = beamai_utils:to_binary(Url),
    BodyBin = beamai_utils:to_binary(Body),

    HackneyOpts = build_hackney_opts(Timeout, ConnectTimeout, Pool),

    case execute_request(Method, UrlBin, Headers, BodyBin, HackneyOpts) of
        {ok, _} = Success ->
            Success;
        {error, _} when Attempt < Retry ->
            timer:sleep(RetryDelay * (Attempt + 1)),
            do_request(Method, Url, ExtraHeaders, Body, Opts, Attempt + 1);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 构建 hackney 选项
-spec build_hackney_opts(pos_integer(), pos_integer(), atom()) -> list().
build_hackney_opts(Timeout, ConnectTimeout, Pool) ->
    [
        {recv_timeout, Timeout},           %% 接收响应的超时时间（关键！）
        {connect_timeout, ConnectTimeout},  %% 连接超时
        {pool, Pool},
        with_body
    ].

%% @private 执行请求
-spec execute_request(atom(), binary(), headers(), binary(), list()) -> response().
execute_request(Method, Url, Headers, Body, Opts) ->
    case hackney:request(Method, Url, Headers, Body, Opts) of
        {ok, StatusCode, _, RespBody} when StatusCode >= 200, StatusCode < 300 ->
            {ok, maybe_decode_json(RespBody)};
        {ok, StatusCode, _, RespBody} when StatusCode >= 400 ->
            {error, {http_error, StatusCode, RespBody}};
        {ok, _, _, RespBody} ->
            {ok, maybe_decode_json(RespBody)};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 默认请求头
-spec default_headers() -> headers().
default_headers() ->
    [
        {<<"User-Agent">>, ?USER_AGENT},
        {<<"Accept">>, <<"application/json">>}
    ].

%% @private 构建查询字符串
-spec build_query_string(params()) -> binary().
build_query_string(Params) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        Key = url_encode(K),
        Value = url_encode(V),
        [<<Key/binary, "=", Value/binary>> | Acc]
    end, [], Params),
    iolist_to_binary(lists:join(<<"&">>, lists:reverse(Pairs))).

%% @private 尝试解析 JSON 响应
-spec maybe_decode_json(binary()) -> term().
maybe_decode_json(Body) when is_binary(Body) ->
    case jsx:is_json(Body) of
        true ->
            try jsx:decode(Body, [return_maps])
            catch
                _:Error ->
                    error_logger:warning_msg("JSON decode error: ~p~nBody: ~ts~n", [Error, Body]),
                    Body
            end;
        false -> Body
    end;
maybe_decode_json(Body) ->
    error_logger:warning_msg("maybe_decode_json received non-binary: ~p~n", [Body]),
    Body.

%%====================================================================
%% 流式请求 API
%%====================================================================

%% @doc 发送流式 POST 请求
%% Callback: 每收到数据块时调用的回调函数
%% Handler: 处理数据块并累积状态的函数
-spec stream_request(atom(), url(), headers(), body(), options()) ->
    stream_response().
stream_request(Method, Url, ExtraHeaders, Body, Opts) ->
    %% 默认处理器：简单累积所有数据块
    DefaultHandler = fun(Chunk, Acc) -> {continue, <<Acc/binary, Chunk/binary>>} end,
    stream_request(Method, Url, ExtraHeaders, Body, Opts, DefaultHandler).

%% @doc 发送流式请求（带自定义处理器）
%% Handler: fun((Chunk, Acc) -> {continue, NewAcc} | {done, FinalAcc})
-spec stream_request(atom(), url(), headers(), body(), options(), chunk_handler()) ->
    stream_response().
stream_request(Method, Url, ExtraHeaders, Body, Opts, Handler) ->
    ensure_started(),

    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
    ConnectTimeout = maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
    CustomHeaders = maps:get(headers, Opts, []),
    InitAcc = maps:get(init_acc, Opts, <<>>),

    Headers = default_headers() ++ CustomHeaders ++ ExtraHeaders,
    UrlBin = beamai_utils:to_binary(Url),
    BodyBin = encode_body(Body),

    HackneyOpts = [
        async,
        {recv_timeout, Timeout},
        {connect_timeout, ConnectTimeout}
    ],

    case hackney:request(Method, UrlBin, Headers, BodyBin, HackneyOpts) of
        {ok, ClientRef} ->
            stream_receive_loop(ClientRef, InitAcc, Handler, Timeout);
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%%====================================================================
%% 流式处理内部函数
%%====================================================================

%% @private 流式接收循环
-spec stream_receive_loop(reference(), term(), chunk_handler(), pos_integer()) ->
    stream_response().
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

%% @private 编码请求体
-spec encode_body(body()) -> binary().
encode_body(Body) when is_binary(Body) -> Body;
encode_body(Body) when is_map(Body) -> jsx:encode(Body);
encode_body(Body) when is_list(Body) -> jsx:encode(Body);
encode_body(Body) -> beamai_utils:to_binary(Body).
