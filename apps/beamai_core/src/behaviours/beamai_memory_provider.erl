%%%-------------------------------------------------------------------
%%% @doc Agent 记忆 Provider Behaviour（Agent 的记忆**接口**）
%%%
%%% 这是 Agent 层的记忆协议——由 Agent 在自己的 tool loop 里**显式**调用（不经
%%% 任何 kernel filter）。与底层**存储**协议（`beamai_chat_memory`）分层：
%%%
%%%   - beamai_chat_memory   —— 存储后端（ETS / Redis…），dumb get/add/clear
%%%   - beamai_memory_provider —— Agent 记忆策略（持久化 + 发送前变换：窗口/摘要/RAG…）
%%%
%%% 职责切分（Agent 自管编排）：
%%%   - **within-run**（一轮内跨工具迭代）的消息累积由 Agent loop 自己持有；
%%%   - **cross-run**（跨轮）的加载/持久化由 provider 的 history/append 负责；
%%%   - **发送前变换**（裁剪窗口 / 摘要 / 召回）由 provider 的 prepare 负责（纯函数）。
%%%
%%% 句柄约定 `{Module, Ref}`。默认实现见 `beamai_memory_provider_default`
%%% （包存储后端，持久全量、prepare 恒等；new/2 可带滑动窗口）。
%%% 自定义记忆只需实现这 4 个 callback，`memory => {YourMod, Ref}` 即插。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_provider).

%% 调度 API
-export([history/2, append/3, prepare/3, clear/2]).
%% 构造
-export([default/1]).

-export_type([provider/0]).

-type provider() :: {module(), term()}.
-type message() :: beamai_message:message().

%%====================================================================
%% Behaviour 回调
%%====================================================================

%% @doc 加载某会话的跨轮历史（开场用；无则 []）。
-callback history(Ref :: term(), ConvId :: binary()) -> [message()].

%% @doc 持久化消息（user / assistant / tool 结果，供下次 history 取回）。
-callback append(Ref :: term(), ConvId :: binary(), Msgs :: [message()]) -> ok.

%% @doc 发送前变换：把本轮要发给 LLM 的完整消息列表变换为实际发送的列表
%% （窗口裁剪 / 摘要压缩 / RAG 召回…）。默认实现恒等返回。
-callback prepare(Ref :: term(), ConvId :: binary(), Messages :: [message()]) -> [message()].

%% @doc 清空会话。
-callback clear(Ref :: term(), ConvId :: binary()) -> ok.

%%====================================================================
%% 调度 API
%%====================================================================

-spec history(provider(), binary()) -> [message()].
history({Module, Ref}, ConvId) -> Module:history(Ref, ConvId).

-spec append(provider(), binary(), [message()]) -> ok.
append({Module, Ref}, ConvId, Msgs) -> Module:append(Ref, ConvId, Msgs).

-spec prepare(provider(), binary(), [message()]) -> [message()].
prepare({Module, Ref}, ConvId, Messages) -> Module:prepare(Ref, ConvId, Messages).

-spec clear(provider(), binary()) -> ok.
clear({Module, Ref}, ConvId) -> Module:clear(Ref, ConvId).

%%====================================================================
%% 构造
%%====================================================================

%% @doc 用默认 provider 包装一个存储后端句柄。
-spec default(beamai_chat_memory:handle()) -> provider().
default(StoreHandle) ->
    beamai_memory_provider_default:new(StoreHandle).
