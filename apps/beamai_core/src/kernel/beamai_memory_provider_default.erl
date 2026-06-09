%%%-------------------------------------------------------------------
%%% @doc 默认 Agent 记忆 Provider（包一个存储后端，全量持久 + prepare 恒等）
%%%
%%% 实现 `beamai_memory_provider`，内部委托一个 `beamai_chat_memory` 存储后端：
%%%   - history/append/clear：直通存储后端（mem_get / mem_add / mem_clear）
%%%   - prepare：恒等返回（不裁剪、不变换）——即默认无界上下文
%%%
%%% 需要裁剪/摘要的，用 `beamai_memory_provider_window` 装饰，或自定义 provider。
%%% 句柄：`{beamai_memory_provider_default, StoreHandle}`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_provider_default).

-behaviour(beamai_memory_provider).

%% 构造
-export([new/1]).
%% beamai_memory_provider 回调
-export([history/2, append/3, prepare/3, clear/2]).

-type store() :: beamai_chat_memory:handle().

%% @doc 用一个存储后端句柄构造默认 provider。
-spec new(store()) -> beamai_memory_provider:provider().
new(StoreHandle) ->
    {?MODULE, StoreHandle}.

%%====================================================================
%% beamai_memory_provider 回调
%%====================================================================

-spec history(store(), binary()) -> [beamai_message:message()].
history(Store, ConvId) ->
    beamai_chat_memory:mem_get(Store, ConvId).

-spec append(store(), binary(), [beamai_message:message()]) -> ok.
append(Store, ConvId, Msgs) ->
    beamai_chat_memory:mem_add(Store, ConvId, Msgs).

-spec prepare(store(), binary(), [beamai_message:message()]) -> [beamai_message:message()].
prepare(_Store, _ConvId, Messages) ->
    Messages.

-spec clear(store(), binary()) -> ok.
clear(Store, ConvId) ->
    beamai_chat_memory:mem_clear(Store, ConvId).
