%%%-------------------------------------------------------------------
%%% @doc Agent Memory 应用
%%%
%%% Agent Memory 系统的应用入口。
%%% 管理短期记忆（Checkpointer）和长期记忆（Store）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_app).

-behaviour(application).

%% Application 回调
-export([start/2, stop/1]).

%%====================================================================
%% Application 回调
%%====================================================================

start(_StartType, _StartArgs) ->
    beamai_memory_sup:start_link().

stop(_State) ->
    ok.
