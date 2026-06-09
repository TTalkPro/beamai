%%%-------------------------------------------------------------------
%%% @doc beamai_agent OTP 应用回调
%%%
%%% 启动 beamai_agent 顶层 supervisor，使默认会话 store 等进程纳入监督树、
%%% 崩溃可被自动重启（取代原先 unlink 的孤儿进程）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
    beamai_agent_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
