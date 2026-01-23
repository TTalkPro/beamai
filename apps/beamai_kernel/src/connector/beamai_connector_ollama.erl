-module(beamai_connector_ollama).
-behaviour(beamai_connector).

%% Behaviour callbacks
-export([chat/3, embedding/3]).

-define(DEFAULT_BASE_URL, <<"http://localhost:11434">>).
-define(DEFAULT_MODEL, <<"llama3">>).

%%====================================================================
%% Behaviour Callbacks
%%====================================================================

-spec chat(map(), [beamai_chat_completion:message()], beamai_chat_completion:chat_opts()) ->
    {ok, beamai_chat_completion:chat_response()} | {error, term()}.
chat(Config, Messages, Opts) ->
    BaseUrl = maps:get(base_url, Config, ?DEFAULT_BASE_URL),
    Model = maps:get(model, Config, maps:get(model, Opts, ?DEFAULT_MODEL)),
    Url = <<BaseUrl/binary, "/api/chat">>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Body = build_chat_body(Model, Messages, Opts),
    Timeout = maps:get(timeout, Config, 120000),
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
    Model = maps:get(embedding_model, Config, maps:get(model, Opts, <<"nomic-embed-text">>)),
    Url = <<BaseUrl/binary, "/api/embed">>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
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

build_chat_body(Model, Messages, Opts) ->
    Body0 = #{
        model => Model,
        messages => format_messages(Messages),
        stream => false
    },
    Body1 = case maps:find(temperature, Opts) of
        {ok, Temp} -> Body0#{options => #{temperature => Temp}};
        error -> Body0
    end,
    Body2 = case maps:find(tools, Opts) of
        {ok, Tools} when is_list(Tools), Tools =/= [] ->
            OllamaTools = [openai_to_ollama_tool(T) || T <- Tools],
            Body1#{tools => OllamaTools};
        _ -> Body1
    end,
    Body2.

openai_to_ollama_tool(#{type := <<"function">>, function := Func}) ->
    #{
        type => <<"function">>,
        function => Func
    };
openai_to_ollama_tool(Tool) ->
    Tool.

format_messages(Messages) ->
    [format_message(M) || M <- Messages].

format_message(#{role := tool, tool_call_id := _Id, content := Content}) ->
    #{role => <<"tool">>, content => Content};
format_message(#{role := assistant, tool_calls := TCs} = Msg) when is_list(TCs), TCs =/= [] ->
    #{
        role => <<"assistant">>,
        content => maps:get(content, Msg, <<>>),
        tool_calls => [format_tool_call(TC) || TC <- TCs]
    };
format_message(#{role := Role, content := Content}) ->
    #{role => to_binary(Role), content => ensure_content(Content)}.

format_tool_call(#{id := _Id, function := #{name := Name, arguments := Args}}) ->
    #{
        function => #{
            name => Name,
            arguments => parse_args(Args)
        }
    }.

parse_args(Args) when is_map(Args) -> Args;
parse_args(Args) when is_binary(Args) ->
    case jsx:is_json(Args) of
        true -> jsx:decode(Args, [return_maps]);
        false -> #{raw => Args}
    end;
parse_args(_) -> #{}.

ensure_content(null) -> <<>>;
ensure_content(Content) when is_binary(Content) -> Content;
ensure_content(_) -> <<>>.

parse_chat_response(Body) ->
    Json = jsx:decode(Body, [return_maps]),
    Msg = maps:get(<<"message">>, Json, #{}),
    Content = maps:get(<<"content">>, Msg, <<>>),
    Response = #{
        role => assistant,
        content => Content,
        finish_reason => parse_done_reason(maps:get(<<"done_reason">>, Json, <<"stop">>)),
        usage => build_usage(Json)
    },
    case maps:get(<<"tool_calls">>, Msg, null) of
        null -> {ok, Response};
        [] -> {ok, Response};
        ToolCalls ->
            ParsedTCs = [parse_tool_call(TC) || TC <- ToolCalls],
            {ok, Response#{tool_calls => ParsedTCs}}
    end.

parse_tool_call(#{<<"function">> := Func}) ->
    #{
        id => generate_tool_call_id(),
        type => function,
        function => #{
            name => maps:get(<<"name">>, Func),
            arguments => jsx:encode(maps:get(<<"arguments">>, Func, #{}))
        }
    };
parse_tool_call(_) ->
    #{id => generate_tool_call_id(), type => function,
      function => #{name => <<>>, arguments => <<"{}">>}}.

parse_embedding_response(Body) ->
    Json = jsx:decode(Body, [return_maps]),
    Embeddings = maps:get(<<"embeddings">>, Json, []),
    {ok, Embeddings}.

build_usage(Json) ->
    #{
        prompt_tokens => maps:get(<<"prompt_eval_count">>, Json, 0),
        completion_tokens => maps:get(<<"eval_count">>, Json, 0),
        total_tokens => maps:get(<<"prompt_eval_count">>, Json, 0) +
                       maps:get(<<"eval_count">>, Json, 0)
    }.

parse_done_reason(<<"stop">>) -> stop;
parse_done_reason(<<"tool_calls">>) -> tool_calls;
parse_done_reason(<<"length">>) -> length;
parse_done_reason(_) -> stop.

generate_tool_call_id() ->
    Bytes = crypto:strong_rand_bytes(12),
    <<"call_", (base64:encode(Bytes))/binary>>.

to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.
