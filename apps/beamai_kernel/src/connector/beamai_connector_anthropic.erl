-module(beamai_connector_anthropic).
-behaviour(beamai_connector).

%% Behaviour callbacks
-export([chat/3]).

-define(DEFAULT_BASE_URL, <<"https://api.anthropic.com/v1">>).
-define(DEFAULT_MODEL, <<"claude-sonnet-4-20250514">>).
-define(API_VERSION, <<"2023-06-01">>).

%%====================================================================
%% Behaviour Callbacks
%%====================================================================

-spec chat(map(), [beamai_chat_completion:message()], beamai_chat_completion:chat_opts()) ->
    {ok, beamai_chat_completion:chat_response()} | {error, term()}.
chat(Config, Messages, Opts) ->
    BaseUrl = maps:get(base_url, Config, ?DEFAULT_BASE_URL),
    ApiKey = get_api_key(Config),
    Model = maps:get(model, Config, maps:get(model, Opts, ?DEFAULT_MODEL)),
    Url = <<BaseUrl/binary, "/messages">>,
    Headers = build_headers(ApiKey),
    {SystemMsg, UserMessages} = extract_system(Messages),
    Body = build_chat_body(Model, SystemMsg, UserMessages, Opts),
    Timeout = maps:get(timeout, Config, 60000),
    case beamai_connector:post_json(Url, Headers, Body, Timeout) of
        {ok, 200, _Headers, RespBody} ->
            parse_chat_response(RespBody);
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
        _ -> list_to_binary(os:getenv("ANTHROPIC_API_KEY", ""))
    end.

build_headers(ApiKey) ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"x-api-key">>, ApiKey},
        {<<"anthropic-version">>, ?API_VERSION}
    ].

extract_system(Messages) ->
    case [M || #{role := system} = M <- Messages] of
        [#{content := SysContent} | _] ->
            UserMsgs = [M || #{role := R} = M <- Messages, R =/= system],
            {SysContent, UserMsgs};
        [] ->
            {undefined, Messages}
    end.

build_chat_body(Model, SystemMsg, Messages, Opts) ->
    Body0 = #{
        model => Model,
        messages => format_messages(Messages),
        max_tokens => maps:get(max_tokens, Opts, 4096)
    },
    Body1 = case SystemMsg of
        undefined -> Body0;
        Sys -> Body0#{system => Sys}
    end,
    Body2 = maybe_add(temperature, Opts, Body1),
    Body3 = case maps:find(tools, Opts) of
        {ok, Tools} when is_list(Tools), Tools =/= [] ->
            AnthropicTools = [openai_to_anthropic_tool(T) || T <- Tools],
            B = Body2#{tools => AnthropicTools},
            case maps:find(tool_choice, Opts) of
                {ok, auto} -> B#{tool_choice => #{type => <<"auto">>}};
                {ok, none} -> B;
                {ok, required} -> B#{tool_choice => #{type => <<"any">>}};
                _ -> B
            end;
        _ -> Body2
    end,
    Body3.

openai_to_anthropic_tool(#{type := <<"function">>, function := Func}) ->
    #{
        name => maps:get(name, Func),
        description => maps:get(description, Func, <<"">>),
        input_schema => maps:get(parameters, Func, #{type => <<"object">>, properties => #{}})
    };
openai_to_anthropic_tool(#{name := _} = Tool) ->
    %% Already in Anthropic format
    Tool.

format_messages(Messages) ->
    [format_message(M) || M <- Messages].

format_message(#{role := tool, tool_call_id := Id, content := Content}) ->
    #{
        role => <<"user">>,
        content => [#{
            type => <<"tool_result">>,
            tool_use_id => Id,
            content => Content
        }]
    };
format_message(#{role := assistant, tool_calls := TCs}) when is_list(TCs), TCs =/= [] ->
    Content = [#{
        type => <<"tool_use">>,
        id => maps:get(id, TC),
        name => maps:get(name, maps:get(function, TC)),
        input => parse_args(maps:get(arguments, maps:get(function, TC)))
    } || TC <- TCs],
    #{role => <<"assistant">>, content => Content};
format_message(#{role := Role, content := Content}) ->
    #{role => to_binary(Role), content => ensure_content(Content)}.

ensure_content(null) -> <<>>;
ensure_content(Content) -> Content.

parse_args(Args) when is_map(Args) -> Args;
parse_args(Args) when is_binary(Args) ->
    case jsx:is_json(Args) of
        true -> jsx:decode(Args, [return_maps]);
        false -> #{raw => Args}
    end;
parse_args(_) -> #{}.

parse_chat_response(Body) ->
    Json = jsx:decode(Body, [return_maps]),
    Content = maps:get(<<"content">>, Json, []),
    StopReason = maps:get(<<"stop_reason">>, Json, <<"end_turn">>),
    {TextParts, ToolUseParts} = partition_content(Content),
    TextContent = case TextParts of
        [] -> null;
        _ -> iolist_to_binary([maps:get(<<"text">>, P) || P <- TextParts])
    end,
    Response = #{
        role => assistant,
        content => TextContent,
        finish_reason => parse_stop_reason(StopReason),
        usage => maps:get(<<"usage">>, Json, #{})
    },
    case ToolUseParts of
        [] -> {ok, Response};
        _ ->
            TCs = [#{
                id => maps:get(<<"id">>, TU),
                type => function,
                function => #{
                    name => maps:get(<<"name">>, TU),
                    arguments => jsx:encode(maps:get(<<"input">>, TU, #{}))
                }
            } || TU <- ToolUseParts],
            {ok, Response#{tool_calls => TCs}}
    end.

partition_content(Content) ->
    lists:partition(
        fun(#{<<"type">> := T}) -> T =:= <<"text">>;
           (_) -> true
        end,
        Content
    ).

parse_stop_reason(<<"end_turn">>) -> stop;
parse_stop_reason(<<"tool_use">>) -> tool_calls;
parse_stop_reason(<<"max_tokens">>) -> length;
parse_stop_reason(_) -> stop.

maybe_add(Key, Map, Body) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Body#{Key => Value};
        error -> Body
    end.

to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.
