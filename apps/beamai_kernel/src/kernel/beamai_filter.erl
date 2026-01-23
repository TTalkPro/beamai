-module(beamai_filter).

%% API
-export([new/3, new/4]).
-export([apply_pre_filters/4]).
-export([apply_post_filters/4]).
-export([apply_pre_chat_filters/3]).
-export([apply_post_chat_filters/3]).
-export([sort_filters/1]).

%% Types
-export_type([filter_def/0, filter_type/0, filter_context/0, filter_result/0]).

-type filter_def() :: #{
    name := binary(),
    type := filter_type(),
    handler := fun((filter_context()) -> filter_result()),
    priority => integer()
}.

-type filter_type() :: pre_invocation | post_invocation | pre_chat | post_chat.

-type filter_context() :: #{
    function => beamai_function:function_def(),
    args => beamai_function:args(),
    result => term(),
    messages => [beamai_context:message()],
    context => beamai_context:t(),
    metadata => map()
}.

-type filter_result() ::
    {continue, filter_context()}
    | {skip, term()}
    | {error, term()}.

%%====================================================================
%% API
%%====================================================================

-spec new(binary(), filter_type(), fun((filter_context()) -> filter_result())) -> filter_def().
new(Name, Type, Handler) ->
    new(Name, Type, Handler, 0).

-spec new(binary(), filter_type(), fun((filter_context()) -> filter_result()), integer()) -> filter_def().
new(Name, Type, Handler, Priority) ->
    #{
        name => Name,
        type => Type,
        handler => Handler,
        priority => Priority
    }.

-spec apply_pre_filters([filter_def()], beamai_function:function_def(), beamai_function:args(), beamai_context:t()) ->
    {ok, beamai_function:args(), beamai_context:t()} | {skip, term()} | {error, term()}.
apply_pre_filters(Filters, FuncDef, Args, Context) ->
    PreFilters = get_filters_by_type(Filters, pre_invocation),
    FilterCtx = #{
        function => FuncDef,
        args => Args,
        context => Context,
        metadata => #{}
    },
    case run_filter_chain(PreFilters, FilterCtx) of
        {continue, #{args := NewArgs, context := NewCtx}} ->
            {ok, NewArgs, NewCtx};
        {continue, #{args := NewArgs}} ->
            {ok, NewArgs, Context};
        {continue, FCtx} ->
            {ok, maps:get(args, FCtx, Args), maps:get(context, FCtx, Context)};
        {skip, Value} ->
            {skip, Value};
        {error, Reason} ->
            {error, Reason}
    end.

-spec apply_post_filters([filter_def()], beamai_function:function_def(), term(), beamai_context:t()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
apply_post_filters(Filters, FuncDef, Result, Context) ->
    PostFilters = get_filters_by_type(Filters, post_invocation),
    FilterCtx = #{
        function => FuncDef,
        result => Result,
        context => Context,
        metadata => #{}
    },
    case run_filter_chain(PostFilters, FilterCtx) of
        {continue, #{result := NewResult, context := NewCtx}} ->
            {ok, NewResult, NewCtx};
        {continue, #{result := NewResult}} ->
            {ok, NewResult, Context};
        {continue, FCtx} ->
            {ok, maps:get(result, FCtx, Result), maps:get(context, FCtx, Context)};
        {skip, Value} ->
            {ok, Value, Context};
        {error, Reason} ->
            {error, Reason}
    end.

-spec apply_pre_chat_filters([filter_def()], [beamai_context:message()], beamai_context:t()) ->
    {ok, [beamai_context:message()], beamai_context:t()} | {error, term()}.
apply_pre_chat_filters(Filters, Messages, Context) ->
    PreChatFilters = get_filters_by_type(Filters, pre_chat),
    FilterCtx = #{
        messages => Messages,
        context => Context,
        metadata => #{}
    },
    case run_filter_chain(PreChatFilters, FilterCtx) of
        {continue, #{messages := NewMsgs, context := NewCtx}} ->
            {ok, NewMsgs, NewCtx};
        {continue, FCtx} ->
            {ok, maps:get(messages, FCtx, Messages), maps:get(context, FCtx, Context)};
        {error, Reason} ->
            {error, Reason};
        {skip, _} ->
            {ok, Messages, Context}
    end.

-spec apply_post_chat_filters([filter_def()], term(), beamai_context:t()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
apply_post_chat_filters(Filters, Response, Context) ->
    PostChatFilters = get_filters_by_type(Filters, post_chat),
    FilterCtx = #{
        result => Response,
        context => Context,
        metadata => #{}
    },
    case run_filter_chain(PostChatFilters, FilterCtx) of
        {continue, #{result := NewResult, context := NewCtx}} ->
            {ok, NewResult, NewCtx};
        {continue, FCtx} ->
            {ok, maps:get(result, FCtx, Response), maps:get(context, FCtx, Context)};
        {error, Reason} ->
            {error, Reason};
        {skip, Value} ->
            {ok, Value, Context}
    end.

-spec sort_filters([filter_def()]) -> [filter_def()].
sort_filters(Filters) ->
    lists:sort(fun(#{priority := P1}, #{priority := P2}) ->
        P1 =< P2
    end, [F#{priority => maps:get(priority, F, 0)} || F <- Filters]).

%%====================================================================
%% Internal
%%====================================================================

get_filters_by_type(Filters, Type) ->
    Matching = [F || #{type := T} = F <- Filters, T =:= Type],
    sort_filters(Matching).

run_filter_chain([], FilterCtx) ->
    {continue, FilterCtx};
run_filter_chain([#{handler := Handler} | Rest], FilterCtx) ->
    try Handler(FilterCtx) of
        {continue, NewCtx} ->
            run_filter_chain(Rest, NewCtx);
        {skip, Value} ->
            {skip, Value};
        {error, Reason} ->
            {error, Reason}
    catch
        Class:Reason:Stack ->
            {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end.
