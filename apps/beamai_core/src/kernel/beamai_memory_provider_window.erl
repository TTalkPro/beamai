%%%-------------------------------------------------------------------
%%% @doc 滑动窗口 Agent 记忆 Provider（装饰任意 provider）
%%%
%%% 包装一个内部 provider：history/append/clear 透传；**仅在 prepare 时**对要发给
%%% LLM 的消息列表套滑动窗口（保留 system + 最近 N 条非系统消息，丢弃头部孤立
%%% tool 消息）。底层 provider 仍持久全量；窗口只影响发送，防止长对话撑爆
%%% context window。
%%%
%%% 句柄：`{beamai_memory_provider_window, {InnerProvider, MaxMessages}}`。
%%% 复用 beamai_chat_memory_window:safe_window/2（与存储层窗口同一套规则）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_provider_window).

-behaviour(beamai_memory_provider).

%% 构造
-export([new/2]).
%% beamai_memory_provider 回调
-export([history/2, append/3, prepare/3, clear/2]).

-type ref() :: {beamai_memory_provider:provider(), pos_integer()}.

%% @doc 用内部 provider + 窗口大小构造。
-spec new(beamai_memory_provider:provider(), pos_integer()) ->
    beamai_memory_provider:provider().
new(Inner, MaxMessages) ->
    {?MODULE, {Inner, MaxMessages}}.

%%====================================================================
%% beamai_memory_provider 回调
%%====================================================================

-spec history(ref(), binary()) -> [beamai_message:message()].
history({Inner, _Max}, ConvId) ->
    beamai_memory_provider:history(Inner, ConvId).

-spec append(ref(), binary(), [beamai_message:message()]) -> ok.
append({Inner, _Max}, ConvId, Msgs) ->
    beamai_memory_provider:append(Inner, ConvId, Msgs).

-spec prepare(ref(), binary(), [beamai_message:message()]) -> [beamai_message:message()].
prepare({Inner, Max}, ConvId, Messages) ->
    beamai_chat_memory_window:safe_window(
        beamai_memory_provider:prepare(Inner, ConvId, Messages), Max).

-spec clear(ref(), binary()) -> ok.
clear({Inner, _Max}, ConvId) ->
    beamai_memory_provider:clear(Inner, ConvId).
