%%%-------------------------------------------------------------------
%%% @doc BeamAI Core 应用回调模块
%%%
%%% 负责启动 beamai_core 应用的 supervision tree。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_core_app).

-behaviour(application).

%% Application 回调
-export([start/2, stop/1]).

%%====================================================================
%% Application 回调实现
%%====================================================================

%% @doc 启动应用
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    beamai_core_sup:start_link().

%% @doc 停止应用
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
