%%%-------------------------------------------------------------------
%%% @doc BeamAI Graph Application
%%%
%%% Graph execution engine application callback module.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    beamai_graph_sup:start_link().

stop(_State) ->
    ok.
