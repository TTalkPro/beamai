-module(beamai_service).

%% API
-export([new/3]).
-export([get_type/1, get_connector/1, get_config/1]).

%% Types
-export_type([service_config/0, service_type/0]).

-type service_type() :: chat_completion | embedding | memory | atom().

-type service_config() :: #{
    type := service_type(),
    connector := module(),
    config := map()
}.

%%====================================================================
%% API
%%====================================================================

-spec new(service_type(), module(), map()) -> service_config().
new(Type, Connector, Config) ->
    #{
        type => Type,
        connector => Connector,
        config => Config
    }.

-spec get_type(service_config()) -> service_type().
get_type(#{type := Type}) -> Type.

-spec get_connector(service_config()) -> module().
get_connector(#{connector := Connector}) -> Connector.

-spec get_config(service_config()) -> map().
get_config(#{config := Config}) -> Config.
