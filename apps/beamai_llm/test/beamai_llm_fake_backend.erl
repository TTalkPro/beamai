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
-export([set_response/2, set_stream/2, install/0, uninstall/1]).

%% 用 application env 跨进程传递响应（beamai_http 同进程调用，进程字典亦可，
%% 这里用 application env 更稳妥）。
set_response(BodyMap, Headers) ->
    application:set_env(beamai_core, fake_backend_body, BodyMap),
    application:set_env(beamai_core, fake_backend_headers, Headers).

%% 配置流式响应：SseChunks 为 [binary()]（每个含完整 "data: ...\n" 行），
%% Headers 为响应头列表（forward_headers 开启时会回传给 Handler）。
set_stream(SseChunks, Headers) ->
    application:set_env(beamai_core, fake_backend_stream, SseChunks),
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

%% 模拟流式：可选回传响应头，再按顺序把 SSE 数据块喂给 Handler，
%% 返回最终累加状态（与真实后端的 {ok, State} 一致）。
stream_request(_Method, _Url, _Headers, _Body, Opts, Handler) ->
    InitAcc = maps:get(init_acc, Opts, <<>>),
    Acc0 = case maps:get(forward_headers, Opts, false) of
        true -> apply_handler(Handler, {http_headers, headers()}, InitAcc);
        false -> InitAcc
    end,
    Chunks = application:get_env(beamai_core, fake_backend_stream, []),
    AccN = lists:foldl(fun(Ch, A) -> apply_handler(Handler, Ch, A) end, Acc0, Chunks),
    {ok, AccN}.

apply_handler(Handler, Input, Acc) ->
    case Handler(Input, Acc) of
        {continue, NewAcc} -> NewAcc;
        {done, NewAcc} -> NewAcc
    end.

body() ->
    application:get_env(beamai_core, fake_backend_body, #{}).

headers() ->
    application:get_env(beamai_core, fake_backend_headers, []).
