%%%-------------------------------------------------------------------
%%% @doc HTTP 客户端工具模块
%%%
%%% 提供统一的 HTTP 请求接口，支持多种后端实现。
%%% 默认使用 hackney，可通过配置切换到 gun。
%%%
%%% == 后端切换 ==
%%%
%%% ```erlang
%%% %% 使用 Gun（支持 HTTP/2）
%%% application:set_env(beamai_core, http_backend, beamai_http_gun).
%%%
%%% %% 使用 Hackney（默认）
%%% application:set_env(beamai_core, http_backend, beamai_http_hackney).
%%% ```
%%%
%%% == 功能特性 ==
%%% - GET/POST/PUT/DELETE 请求
%%% - JSON 自动编解码
%%% - 超时和重试机制
%%% - 流式请求支持
%%% - 可插拔后端（hackney/gun）
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 简单 GET 请求
%%% {ok, Body} = beamai_http:get("https://api.example.com/data").
%%%
%%% %% POST JSON 数据
%%% {ok, Response} = beamai_http:post_json("https://api.example.com/data",
%%%                                        #{key => value}).
%%% ```
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
-export([get_backend/0, set_backend/1]).

%%====================================================================
%% 类型定义
%%====================================================================

-type url() :: string() | binary().
-type headers() :: [{binary(), binary()}].
-type params() :: #{atom() | binary() => term()}.
-type body() :: binary() | string() | map() | list().
-type options() :: #{
    timeout => pos_integer(),
    connect_timeout => pos_integer(),
    headers => headers(),
    retry => non_neg_integer(),
    retry_delay => pos_integer(),
    pool => atom(),
    init_acc => term()
}.
-type response() :: {ok, body()} | {error, term()}.
-type stream_callback() :: fun((binary()) -> any()).
-type chunk_handler() :: fun((binary(), term()) -> {continue, term()} | {done, term()}).
-type stream_response() :: {ok, term()} | {error, term()}.

-export_type([url/0, headers/0, params/0, body/0, options/0, response/0]).
-export_type([stream_callback/0, chunk_handler/0, stream_response/0]).

%%====================================================================
%% 默认配置
%%====================================================================

-define(DEFAULT_TIMEOUT, 30000).
-define(DEFAULT_CONNECT_TIMEOUT, 10000).
-define(DEFAULT_RETRY, 0).
-define(DEFAULT_RETRY_DELAY, 1000).
-define(USER_AGENT, <<"BeamAI/2.0">>).
-define(DEFAULT_BACKEND, beamai_http_gun).

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
%% 流式请求 API
%%====================================================================

%% @doc 发送流式 POST 请求
-spec stream_request(atom(), url(), headers(), body(), options()) -> stream_response().
stream_request(Method, Url, ExtraHeaders, Body, Opts) ->
    DefaultHandler = fun(Chunk, Acc) -> {continue, <<Acc/binary, Chunk/binary>>} end,
    stream_request(Method, Url, ExtraHeaders, Body, Opts, DefaultHandler).

%% @doc 发送流式请求（带自定义处理器）
-spec stream_request(atom(), url(), headers(), body(), options(), chunk_handler()) ->
    stream_response().
stream_request(Method, Url, ExtraHeaders, Body, Opts, Handler) ->
    Backend = get_backend(),
    Backend:ensure_started(),

    CustomHeaders = maps:get(headers, Opts, []),
    Headers = default_headers() ++ CustomHeaders ++ ExtraHeaders,
    UrlBin = beamai_utils:to_binary(Url),
    BodyBin = beamai_utils:encode_body(Body),

    Backend:stream_request(Method, UrlBin, Headers, BodyBin, Opts, Handler).

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

%% @doc 确保 HTTP 客户端已启动
-spec ensure_started() -> ok.
ensure_started() ->
    Backend = get_backend(),
    Backend:ensure_started().

%% @doc 获取当前 HTTP 后端
-spec get_backend() -> module().
get_backend() ->
    application:get_env(beamai_core, http_backend, ?DEFAULT_BACKEND).

%% @doc 设置 HTTP 后端
-spec set_backend(module()) -> ok.
set_backend(Backend) ->
    application:set_env(beamai_core, http_backend, Backend).

%%====================================================================
%% 内部函数 - 请求执行
%%====================================================================

%% @private 发送带参数的请求
-spec do_request_with_params(atom(), url(), params(), options()) -> response().
do_request_with_params(Method, Url, Params, Opts) ->
    FullUrl = build_url(Url, Params),
    request(Method, FullUrl, [], <<>>, Opts).

%% @private 发送带请求体的请求
-spec do_request_with_body(atom(), url(), binary(), body(), options()) -> response().
do_request_with_body(Method, Url, ContentType, Body, Opts) ->
    Headers = [{<<"Content-Type">>, ContentType}],
    request(Method, Url, Headers, Body, Opts).

%% @private 执行 HTTP 请求
-spec do_request(atom(), url(), headers(), body(), options(), non_neg_integer()) -> response().
do_request(Method, Url, ExtraHeaders, Body, Opts, Attempt) ->
    Backend = get_backend(),
    Backend:ensure_started(),

    Retry = maps:get(retry, Opts, ?DEFAULT_RETRY),
    RetryDelay = maps:get(retry_delay, Opts, ?DEFAULT_RETRY_DELAY),
    CustomHeaders = maps:get(headers, Opts, []),

    Headers = default_headers() ++ CustomHeaders ++ ExtraHeaders,
    UrlBin = beamai_utils:to_binary(Url),
    BodyBin = beamai_utils:encode_body(Body),

    case Backend:request(Method, UrlBin, Headers, BodyBin, Opts) of
        {ok, _} = Success ->
            Success;
        {error, _} when Attempt < Retry ->
            timer:sleep(RetryDelay * (Attempt + 1)),
            do_request(Method, Url, ExtraHeaders, Body, Opts, Attempt + 1);
        {error, Reason} ->
            {error, Reason}
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

