-module(beamai_embedding_service).

%% API
-export([embed/3]).

%% Types
-export_type([embedding_opts/0]).

-type embedding_opts() :: #{
    model => binary(),
    atom() => term()
}.

%%====================================================================
%% API
%%====================================================================

-spec embed(beamai_kernel:service_config(), [binary()], embedding_opts()) ->
    {ok, [[float()]]} | {error, term()}.
embed(#{connector := Connector, config := Config}, Texts, Opts) ->
    MergedConfig = maps:merge(Config, Opts),
    case erlang:function_exported(Connector, embedding, 3) of
        true -> Connector:embedding(MergedConfig, Texts, Opts);
        false -> {error, embedding_not_supported}
    end.
