%%%-------------------------------------------------------------------
%%% @doc 测试用 HTTP 后端桩
%%%
%%% 实现 beamai_http_behaviour，返回可配置的响应体与响应头，
%%% 用于在不发真实网络请求的前提下验证 request_meta 与响应头注入。
%%%
%%% 响应内容通过进程字典在测试中设置：
%%%   beamai_llm_fake_backend:set_response(BodyMap, Headers)
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_fake_backend).
-behaviour(beamai_http_behaviour).

-export([request/5, request_meta/5, stream_request/6, ensure_started/0]).
-export([set_response/2, install/0, uninstall/1]).

%% 用 application env 跨进程传递响应（beamai_http 同进程调用，进程字典亦可，
%% 这里用 application env 更稳妥）。
set_response(BodyMap, Headers) ->
    application:set_env(beamai_core, fake_backend_body, BodyMap),
    application:set_env(beamai_core, fake_backend_headers, Headers).

%% 安装本后端，返回原后端供恢复
install() ->
    Prev = beamai_http:get_backend(),
    beamai_http:set_backend(?MODULE),
    Prev.

uninstall(Prev) ->
    beamai_http:set_backend(Prev).

ensure_started() -> ok.

request(_Method, _Url, _Headers, _Body, _Opts) ->
    {ok, body()}.

request_meta(_Method, _Url, _Headers, _Body, _Opts) ->
    {ok, body(), #{status => 200, headers => headers()}}.

stream_request(_Method, _Url, _Headers, _Body, _Opts, _Handler) ->
    {ok, <<>>}.

body() ->
    application:get_env(beamai_core, fake_backend_body, #{}).

headers() ->
    application:get_env(beamai_core, fake_backend_headers, []).
