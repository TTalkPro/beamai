%%%-------------------------------------------------------------------
%%% @doc 过滤器管道：前置/后置拦截
%%%
%%% 提供函数调用和 Chat 请求的拦截机制：
%%% - pre_invocation: 函数执行前拦截（可修改参数或跳过）
%%% - post_invocation: 函数执行后拦截（可修改结果）
%%% - pre_chat: Chat 请求前拦截（可修改消息）
%%% - post_chat: Chat 响应后拦截（可修改响应）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_filter).

%% API
-export([new/3, new/4]).
-export([apply_pre_filters/4]).
-export([apply_post_filters/4]).
-export([apply_post_filters_result/4]).
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

%% @doc 创建过滤器（默认优先级 0）
%%
%% @param Name 过滤器名称（用于调试标识）
%% @param Type 过滤器类型（pre_invocation | post_invocation | pre_chat | post_chat）
%% @param Handler 过滤器处理函数，接收 filter_context，返回 filter_result
%% @returns 过滤器定义 Map
-spec new(binary(), filter_type(), fun((filter_context()) -> filter_result())) -> filter_def().
new(Name, Type, Handler) ->
    new(Name, Type, Handler, 0).

%% @doc 创建过滤器（指定优先级）
%%
%% 优先级数值越小越先执行。同优先级按注册顺序执行。
%%
%% @param Name 过滤器名称
%% @param Type 过滤器类型
%% @param Handler 处理函数
%% @param Priority 优先级（整数，越小越先执行）
%% @returns 过滤器定义 Map
-spec new(binary(), filter_type(), fun((filter_context()) -> filter_result()), integer()) -> filter_def().
new(Name, Type, Handler, Priority) ->
    #{
        name => Name,
        type => Type,
        handler => Handler,
        priority => Priority
    }.

%% @doc 执行前置调用过滤器管道
%%
%% 依次执行所有 pre_invocation 类型的过滤器。
%% 过滤器可修改参数（args）和上下文（context），
%% 也可通过 {skip, Value} 直接跳过函数执行返回结果，
%% 或通过 {error, Reason} 中止管道。
%%
%% @param Filters 过滤器列表
%% @param FuncDef 即将被调用的函数定义
%% @param Args 调用参数
%% @param Context 执行上下文
%% @returns {ok, 过滤后参数, 过滤后上下文} | {skip, 跳过值} | {error, 原因}
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

%% @doc 执行后置调用过滤器管道
%%
%% 依次执行所有 post_invocation 类型的过滤器。
%% 过滤器可修改函数执行结果（result）和上下文（context）。
%%
%% @param Filters 过滤器列表
%% @param FuncDef 已执行的函数定义
%% @param Result 函数执行结果
%% @param Context 执行上下文
%% @returns {ok, 过滤后结果, 过滤后上下文} | {error, 原因}
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

%% @doc 执行后置过滤器并返回最终结果
%%
%% 将 filter 内部格式 {ok, Value, Ctx} 转为调用者需要的 {ok, Value}。
%%
-spec apply_post_filters_result([filter_def()], beamai_function:function_def(),
                                 term(), beamai_context:t()) ->
    {ok, term()} | {error, term()}.
apply_post_filters_result(Filters, FuncDef, Value, Context) ->
    case apply_post_filters(Filters, FuncDef, Value, Context) of
        {ok, FinalValue, _FinalCtx} -> {ok, FinalValue};
        {error, _} = Err -> Err
    end.

%% @doc 执行前置 Chat 过滤器管道
%%
%% 依次执行所有 pre_chat 类型的过滤器。
%% 过滤器可修改消息列表（如注入 system 消息）和上下文。
%%
%% @param Filters 过滤器列表
%% @param Messages 消息列表
%% @param Context 执行上下文
%% @returns {ok, 过滤后消息列表, 过滤后上下文} | {error, 原因}
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

%% @doc 执行后置 Chat 过滤器管道
%%
%% 依次执行所有 post_chat 类型的过滤器。
%% 过滤器可修改 LLM 响应内容（如内容审计、格式转换）。
%%
%% @param Filters 过滤器列表
%% @param Response LLM 响应 Map
%% @param Context 执行上下文
%% @returns {ok, 过滤后响应, 过滤后上下文} | {error, 原因}
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

%% @doc 按优先级排序过滤器列表
%%
%% 优先级缺失时默认为 0。数值越小越先执行。
-spec sort_filters([filter_def()]) -> [filter_def()].
sort_filters(Filters) ->
    lists:sort(fun(#{priority := P1}, #{priority := P2}) ->
        P1 =< P2
    end, [F#{priority => maps:get(priority, F, 0)} || F <- Filters]).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 按类型筛选并排序过滤器
get_filters_by_type(Filters, Type) ->
    Matching = [F || #{type := T} = F <- Filters, T =:= Type],
    sort_filters(Matching).

%% @private 依次执行过滤器链
%%
%% 每个过滤器返回 {continue, NewCtx} 时传递给下一个；
%% 返回 {skip, Value} 或 {error, Reason} 时立即终止链。
%% 处理器异常时捕获并返回包含错误信息的 {error, ...}。
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
