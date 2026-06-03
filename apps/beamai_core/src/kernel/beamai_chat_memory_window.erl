%%%-------------------------------------------------------------------
%%% @doc 会话消息存储 - 滑动窗口包装
%%%
%%% 包装任意 beamai_chat_memory 实现：底层保留全量历史，仅在
%%% mem_get 读取时套用滑动窗口（保留最近 N 条非系统消息）。
%%% mem_add / mem_clear 透传到内部 store。
%%%
%%% 窗口规则（对齐 clj-agent 的 safe-window）：
%%% - system 消息始终保留，置于结果头部；
%%% - 非系统消息只保留尾部 MaxMessages 条；
%%% - 裁剪后丢弃落在头部的孤立 tool 消息（避免 tool 结果找不到对应调用）。
%%%
%%% 句柄：`{beamai_chat_memory_window, {InnerHandle, MaxMessages}}`。
%%% 注：基于条数的窗口；基于 Token 的裁剪/摘要可由 beamai_cognition
%%% 提供的 store 实现（其可依赖 beamai_conversation_buffer）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_chat_memory_window).

-behaviour(beamai_chat_memory).

%% API
-export([handle/2]).

%% beamai_chat_memory 回调
-export([mem_get/2, mem_add/3, mem_clear/2]).

-type message() :: beamai_message:message().
-type ref() :: {beamai_chat_memory:handle(), pos_integer()}.

%%====================================================================
%% API
%%====================================================================

%% @doc 构造窗口包装句柄
%%
%% @param Inner 内部 store 句柄
%% @param MaxMessages 保留的最近非系统消息条数
-spec handle(beamai_chat_memory:handle(), pos_integer()) -> beamai_chat_memory:handle().
handle(Inner, MaxMessages) ->
    {?MODULE, {Inner, MaxMessages}}.

%%====================================================================
%% beamai_chat_memory 回调
%%====================================================================

-spec mem_get(ref(), binary()) -> [message()].
mem_get({Inner, MaxMessages}, ConvId) ->
    Full = beamai_chat_memory:mem_get(Inner, ConvId),
    safe_window(Full, MaxMessages).

-spec mem_add(ref(), binary(), [message()]) -> ok.
mem_add({Inner, _MaxMessages}, ConvId, Msgs) ->
    beamai_chat_memory:mem_add(Inner, ConvId, Msgs).

-spec mem_clear(ref(), binary()) -> ok.
mem_clear({Inner, _MaxMessages}, ConvId) ->
    beamai_chat_memory:mem_clear(Inner, ConvId).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 套用滑动窗口
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
