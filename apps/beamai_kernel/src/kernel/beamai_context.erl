-module(beamai_context).

%% API
-export([new/0, new/1]).
-export([get/2, get/3]).
-export([set/3, set_many/2]).
-export([get_history/1, add_message/2]).
-export([with_kernel/2, get_kernel/1]).
-export([add_trace/2, get_trace/1]).
-export([get_metadata/1, set_metadata/3]).

%% Types
-export_type([t/0, message/0, trace_entry/0]).

-type t() :: #{
    '__context__' := true,
    variables := #{binary() => term()},
    history := [message()],
    kernel := term() | undefined,
    trace := [trace_entry()],
    metadata := map()
}.

-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    tool_call_id => binary(),
    name => binary()
}.

-type tool_call() :: #{
    id := binary(),
    type := function,
    function := #{
        name := binary(),
        arguments := binary() | map()
    }
}.

-type trace_entry() :: #{
    timestamp := integer(),
    type := atom(),
    data := term()
}.

%%====================================================================
%% API
%%====================================================================

-spec new() -> t().
new() ->
    #{
        '__context__' => true,
        variables => #{},
        history => [],
        kernel => undefined,
        trace => [],
        metadata => #{}
    }.

-spec new(map()) -> t().
new(Vars) when is_map(Vars) ->
    Ctx = new(),
    Ctx#{variables => Vars}.

-spec get(t(), binary()) -> term() | undefined.
get(#{variables := Vars}, Key) ->
    maps:get(Key, Vars, undefined).

-spec get(t(), binary(), term()) -> term().
get(#{variables := Vars}, Key, Default) ->
    maps:get(Key, Vars, Default).

-spec set(t(), binary(), term()) -> t().
set(#{variables := Vars} = Ctx, Key, Value) ->
    Ctx#{variables => Vars#{Key => Value}}.

-spec set_many(t(), map()) -> t().
set_many(#{variables := Vars} = Ctx, NewVars) ->
    Ctx#{variables => maps:merge(Vars, NewVars)}.

-spec get_history(t()) -> [message()].
get_history(#{history := History}) -> History.

-spec add_message(t(), message()) -> t().
add_message(#{history := History} = Ctx, Message) ->
    Ctx#{history => History ++ [Message]}.

-spec with_kernel(t(), term()) -> t().
with_kernel(Ctx, Kernel) ->
    Ctx#{kernel => Kernel}.

-spec get_kernel(t()) -> term() | undefined.
get_kernel(#{kernel := Kernel}) -> Kernel.

-spec add_trace(t(), trace_entry()) -> t().
add_trace(#{trace := Trace} = Ctx, Entry) ->
    Ctx#{trace => Trace ++ [Entry#{timestamp => erlang:system_time(millisecond)}]}.

-spec get_trace(t()) -> [trace_entry()].
get_trace(#{trace := Trace}) -> Trace.

-spec get_metadata(t()) -> map().
get_metadata(#{metadata := Meta}) -> Meta.

-spec set_metadata(t(), binary() | atom(), term()) -> t().
set_metadata(#{metadata := Meta} = Ctx, Key, Value) ->
    Ctx#{metadata => Meta#{Key => Value}}.
