-module(beamai_connector_openai).
-behaviour(beamai_connector).

%% Behaviour callbacks
-export([chat/3, embedding/3]).

-define(DEFAULT_BASE_URL, <<"https://api.openai.com/v1">>).
-define(DEFAULT_MODEL, <<"gpt-4">>).

%%====================================================================
%% Behaviour Callbacks
%%====================================================================

-spec chat(map(), [beamai_chat_completion:message()], beamai_chat_completion:chat_opts()) ->
    {ok, beamai_chat_completion:chat_response()} | {error, term()}.
chat(Config, Messages, Opts) ->
    BaseUrl = maps:get(base_url, Config, ?DEFAULT_BASE_URL),
    ApiKey = get_api_key(Config),
    Model = maps:get(model, Config, maps:get(model, Opts, ?DEFAULT_MODEL)),
    Url = <<BaseUrl/binary, "/chat/completions">>,
    Headers = beamai_connector:default_headers(ApiKey),
    Body = build_chat_body(Model, Messages, Opts),
    Timeout = maps:get(timeout, Config, 60000),
    case beamai_connector:post_json(Url, Headers, Body, Timeout) of
        {ok, 200, _Headers, RespBody} ->
            parse_chat_response(RespBody);
        {ok, StatusCode, _Headers, RespBody} ->
            {error, #{status => StatusCode, body => RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec embedding(map(), [binary()], map()) ->
    {ok, [[float()]]} | {error, term()}.
embedding(Config, Texts, Opts) ->
    BaseUrl = maps:get(base_url, Config, ?DEFAULT_BASE_URL),
    ApiKey = get_api_key(Config),
    Model = maps:get(embedding_model, Config, maps:get(model, Opts, <<"text-embedding-3-small">>)),
    Url = <<BaseUrl/binary, "/embeddings">>,
    Headers = beamai_connector:default_headers(ApiKey),
    Body = #{model => Model, input => Texts},
    case beamai_connector:post_json(Url, Headers, Body) of
        {ok, 200, _Headers, RespBody} ->
            parse_embedding_response(RespBody);
        {ok, StatusCode, _Headers, RespBody} ->
            {error, #{status => StatusCode, body => RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal
%%====================================================================

get_api_key(Config) ->
    case maps:find(api_key, Config) of
        {ok, Key} when is_binary(Key) -> Key;
        {ok, Key} when is_list(Key) -> list_to_binary(Key);
        _ -> list_to_binary(os:getenv("OPENAI_API_KEY", ""))
    end.

build_chat_body(Model, Messages, Opts) ->
    Body0 = #{
        model => Model,
        messages => format_messages(Messages)
    },
    Body1 = maybe_add(temperature, Opts, Body0),
    Body2 = maybe_add(max_tokens, Opts, Body1),
    Body3 = case maps:find(tools, Opts) of
        {ok, Tools} when is_list(Tools), Tools =/= [] ->
            B = Body2#{tools => Tools},
            case maps:find(tool_choice, Opts) of
                {ok, auto} -> B#{tool_choice => <<"auto">>};
                {ok, none} -> B#{tool_choice => <<"none">>};
                {ok, required} -> B#{tool_choice => <<"required">>};
                _ -> B
            end;
        _ -> Body2
    end,
    Body3.

format_messages(Messages) ->
    [format_message(M) || M <- Messages].

format_message(#{role := Role, content := Content} = Msg) ->
    M0 = #{role => to_binary(Role), content => Content},
    M1 = case maps:find(tool_calls, Msg) of
        {ok, TCs} -> M0#{tool_calls => format_tool_calls(TCs)};
        error -> M0
    end,
    case maps:find(tool_call_id, Msg) of
        {ok, Id} -> M1#{tool_call_id => Id};
        error -> M1
    end.

format_tool_calls(TCs) ->
    [#{
        id => maps:get(id, TC),
        type => <<"function">>,
        function => #{
            name => maps:get(name, maps:get(function, TC)),
            arguments => ensure_binary_args(maps:get(arguments, maps:get(function, TC)))
        }
    } || TC <- TCs].

ensure_binary_args(Args) when is_binary(Args) -> Args;
ensure_binary_args(Args) when is_map(Args) -> jsx:encode(Args);
ensure_binary_args(Args) -> list_to_binary(io_lib:format("~p", [Args])).

parse_chat_response(Body) ->
    Json = jsx:decode(Body, [return_maps]),
    case maps:get(<<"choices">>, Json, []) of
        [Choice | _] ->
            Msg = maps:get(<<"message">>, Choice),
            Response = #{
                role => assistant,
                content => maps:get(<<"content">>, Msg, null),
                finish_reason => parse_finish_reason(maps:get(<<"finish_reason">>, Choice, null)),
                usage => maps:get(<<"usage">>, Json, #{})
            },
            case maps:get(<<"tool_calls">>, Msg, null) of
                null -> {ok, Response};
                [] -> {ok, Response};
                ToolCalls ->
                    ParsedTCs = [parse_tool_call(TC) || TC <- ToolCalls],
                    {ok, Response#{tool_calls => ParsedTCs}}
            end;
        [] ->
            {error, empty_response}
    end.

parse_tool_call(#{<<"id">> := Id, <<"function">> := Func}) ->
    #{
        id => Id,
        type => function,
        function => #{
            name => maps:get(<<"name">>, Func),
            arguments => maps:get(<<"arguments">>, Func, <<"{}">>)
        }
    };
parse_tool_call(TC) ->
    #{id => maps:get(<<"id">>, TC, <<>>),
      type => function,
      function => #{name => <<>>, arguments => <<"{}">>}}.

parse_embedding_response(Body) ->
    Json = jsx:decode(Body, [return_maps]),
    Data = maps:get(<<"data">>, Json, []),
    Embeddings = [maps:get(<<"embedding">>, D) || D <- Data],
    {ok, Embeddings}.

parse_finish_reason(<<"stop">>) -> stop;
parse_finish_reason(<<"tool_calls">>) -> tool_calls;
parse_finish_reason(<<"length">>) -> length;
parse_finish_reason(_) -> stop.

maybe_add(Key, Map, Body) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Body#{Key => Value};
        error -> Body
    end.

to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.
