-module(beamai_chat_completion).

%% API
-export([chat/3]).
-export([chat_stream/3]).

%% Types
-export_type([chat_opts/0, message/0, chat_response/0, tool_call/0]).

-type chat_opts() :: #{
    model => binary(),
    temperature => float(),
    max_tokens => pos_integer(),
    tools => [map()],
    tool_choice => auto | none | required,
    stream => boolean(),
    atom() => term()
}.

-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    tool_call_id => binary(),
    name => binary()
}.

-type chat_response() :: #{
    content := binary() | null,
    role := assistant,
    tool_calls => [tool_call()],
    usage => map(),
    finish_reason => stop | tool_calls | length
}.

-type tool_call() :: #{
    id := binary(),
    type := function,
    function := #{
        name := binary(),
        arguments := binary() | map()
    }
}.

%%====================================================================
%% API
%%====================================================================

-spec chat(beamai_kernel:service_config(), [message()], chat_opts()) ->
    {ok, chat_response()} | {error, term()}.
chat(#{connector := Connector, config := Config}, Messages, Opts) ->
    MergedConfig = maps:merge(Config, Opts),
    Connector:chat(MergedConfig, Messages, Opts).

-spec chat_stream(beamai_kernel:service_config(), [message()], chat_opts()) ->
    {ok, pid()} | {error, term()}.
chat_stream(#{connector := Connector, config := Config}, Messages, Opts) ->
    MergedConfig = maps:merge(Config, Opts),
    case erlang:function_exported(Connector, chat_stream, 3) of
        true -> Connector:chat_stream(MergedConfig, Messages, Opts);
        false -> {error, stream_not_supported}
    end.
