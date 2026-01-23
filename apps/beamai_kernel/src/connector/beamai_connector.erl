-module(beamai_connector).

%% Behaviour callbacks
-callback chat(Config :: map(), Messages :: [beamai_chat_completion:message()],
               Opts :: beamai_chat_completion:chat_opts()) ->
    {ok, beamai_chat_completion:chat_response()} | {error, term()}.

-callback chat_stream(Config :: map(), Messages :: [beamai_chat_completion:message()],
                      Opts :: beamai_chat_completion:chat_opts()) ->
    {ok, pid()} | {error, term()}.

-callback embedding(Config :: map(), Texts :: [binary()], Opts :: map()) ->
    {ok, [[float()]]} | {error, term()}.

-optional_callbacks([chat_stream/3, embedding/3]).

%% Utility exports
-export([default_headers/1]).
-export([post_json/3, post_json/4]).

%%====================================================================
%% Utilities
%%====================================================================

-spec default_headers(binary()) -> [{binary(), binary()}].
default_headers(ApiKey) ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Authorization">>, <<"Bearer ", ApiKey/binary>>}
    ].

-spec post_json(binary(), [{binary(), binary()}], map()) ->
    {ok, integer(), list(), binary()} | {error, term()}.
post_json(Url, Headers, Body) ->
    post_json(Url, Headers, Body, 60000).

-spec post_json(binary(), [{binary(), binary()}], map(), pos_integer()) ->
    {ok, integer(), list(), binary()} | {error, term()}.
post_json(Url, Headers, Body, Timeout) ->
    JsonBody = jsx:encode(Body),
    case hackney:request(post, Url, Headers, JsonBody, [
        {recv_timeout, Timeout},
        {connect_timeout, 10000},
        with_body
    ]) of
        {ok, StatusCode, RespHeaders, RespBody} ->
            {ok, StatusCode, RespHeaders, RespBody};
        {error, Reason} ->
            {error, {http_error, Reason}}
    end.
