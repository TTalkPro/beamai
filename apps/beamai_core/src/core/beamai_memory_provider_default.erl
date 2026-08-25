%%%-------------------------------------------------------------------
%%% @doc 默认 Agent 记忆 Provider（包一个存储后端，可选滑动窗口）
%%%
%%% 实现 `beamai_memory_provider`，内部委托一个 `beamai_chat_memory` 存储后端：
%%%   - history/append/clear：直通存储后端（mem_get / mem_add / mem_clear）
%%%   - prepare：无窗口时恒等（无界上下文）；带窗口 N 时只保留发送列表里 system +
%%%     最近 N 条非系统消息（全量仍持久于底层），防止长对话撑爆 context window
%%%
%%% 句柄：`{beamai_memory_provider_default, {StoreHandle, WindowOpt}}`，
%%% WindowOpt = infinity（无窗口）| pos_integer()（窗口条数）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_provider_default).

-behaviour(beamai_memory_provider).

%% 构造
-export([new/1, new/2]).
%% beamai_memory_provider 回调
-export([history/2, append/3, prepare/3, clear/2]).

-type store() :: beamai_chat_memory:handle().
-type window() :: infinity | pos_integer().
-type ref() :: {store(), window()}.
-type message() :: beamai_message:message().

%% @doc 无窗口（无界）默认 provider。
-spec new(store()) -> beamai_memory_provider:provider().
new(StoreHandle) ->
    {?MODULE, {StoreHandle, infinity}}.

%% @doc 带 N 条滑动窗口的默认 provider。
-spec new(store(), pos_integer()) -> beamai_memory_provider:provider().
new(StoreHandle, MaxMessages) when is_integer(MaxMessages), MaxMessages > 0 ->
    {?MODULE, {StoreHandle, MaxMessages}}.

%%====================================================================
%% beamai_memory_provider 回调
%%====================================================================

-spec history(ref(), binary()) -> [message()].
history({Store, _Window}, ConvId) ->
    beamai_chat_memory:mem_get(Store, ConvId).

-spec append(ref(), binary(), [message()]) -> ok.
append({Store, _Window}, ConvId, Msgs) ->
    beamai_chat_memory:mem_add(Store, ConvId, Msgs).

-spec prepare(ref(), binary(), [message()]) -> [message()].
prepare({_Store, infinity}, _ConvId, Messages) ->
    Messages;
prepare({_Store, Max}, _ConvId, Messages) when is_integer(Max) ->
    safe_window(Messages, Max).

-spec clear(ref(), binary()) -> ok.
clear({Store, _Window}, ConvId) ->
    beamai_chat_memory:mem_clear(Store, ConvId).

%%====================================================================
%% 内部函数 - 滑动窗口
%%====================================================================

%% @private 套用滑动窗口：system 全留置顶，非系统只留尾部 MaxMessages 条，
%% 并丢弃裁剪后落在头部的孤立 tool 消息（避免 tool 结果找不到对应调用）。
-spec safe_window([message()], pos_integer()) -> [message()].
safe_window(Msgs, MaxMessages) ->
    Systems = [M || M <- Msgs, is_role(M, system)],
    Body = [M || M <- Msgs, not is_role(M, system)],
    Windowed = case length(Body) > MaxMessages of
        true -> lists:nthtail(length(Body) - MaxMessages, Body);
        false -> Body
    end,
    Systems ++ drop_leading_tool(Windowed).

%% @private 丢弃头部孤立的 tool 消息
drop_leading_tool([M | Rest]) ->
    case is_role(M, tool) of
        true -> drop_leading_tool(Rest);
        false -> [M | Rest]
    end;
drop_leading_tool([]) ->
    [].

%% @private 判断消息角色（兼容 atom 与 binary）
is_role(#{role := Role}, Expect) ->
    Role =:= Expect orelse Role =:= atom_to_binary(Expect, utf8);
is_role(_, _) ->
    false.
