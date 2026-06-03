%%%-------------------------------------------------------------------
%%% @doc 会话消息存储 Behaviour
%%%
%%% 定义对话历史存储的统一接口。Kernel 不再自己记录消息，
%%% 由实现本 Behaviour 的 store 按 conversation_id 管理会话历史。
%%%
%%% 句柄约定 `{Module, Ref}`（与 {beamai_store_ets, Name} 一致）：
%%% - Module 实现本 Behaviour
%%% - Ref 为该实现的实例标识（如注册名、pid、配置元组）
%%%
%%% 调度 API（mem_get/2、mem_add/3、mem_clear/2）对句柄解包后
%%% 转发到对应实现模块。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_chat_memory).

%% 调度 API
-export([mem_get/2, mem_add/3, mem_clear/2]).

%% Types
-export_type([handle/0]).

-type handle() :: {module(), term()}.
-type message() :: beamai_message:message().

%%====================================================================
%% Behaviour 回调
%%====================================================================

%% @doc 获取指定会话的完整消息历史（正序，无则返回 []）
-callback mem_get(Ref :: term(), ConvId :: binary()) -> [message()].

%% @doc 追加消息到指定会话末尾
-callback mem_add(Ref :: term(), ConvId :: binary(), Msgs :: [message()]) -> ok.

%% @doc 清空指定会话
-callback mem_clear(Ref :: term(), ConvId :: binary()) -> ok.

%%====================================================================
%% 调度 API
%%====================================================================

%% @doc 获取会话历史
-spec mem_get(handle(), binary()) -> [message()].
mem_get({Module, Ref}, ConvId) ->
    Module:mem_get(Ref, ConvId).

%% @doc 追加消息到会话
-spec mem_add(handle(), binary(), [message()]) -> ok.
mem_add({Module, Ref}, ConvId, Msgs) ->
    Module:mem_add(Ref, ConvId, Msgs).

%% @doc 清空会话
-spec mem_clear(handle(), binary()) -> ok.
mem_clear({Module, Ref}, ConvId) ->
    Module:mem_clear(Ref, ConvId).
