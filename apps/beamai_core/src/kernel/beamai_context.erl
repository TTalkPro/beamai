%%%-------------------------------------------------------------------
%%% @doc 执行上下文：变量、消息、历史、跟踪
%%%
%%% 在函数调用链中传递的不可变上下文，包含：
%%% - 变量存储（key-value）
%%% - 消息缓冲（发给 LLM 的工作上下文，可被 summarize/truncate）
%%% - 消息历史（完整原始对话日志，只追加不修改）
%%% - Kernel 引用
%%% - 执行跟踪（调试用）
%%% - 元数据
%%%
%%% Key 标准化规则：
%%% - '__context__' 保持 atom（类型标记）
%%% - 其他 atom key → binary（用户变量）
%%% - binary key 保持不变
%%%
%%% 内部字段（messages, history, kernel, trace, metadata）
%%% 使用 atom key，通过专用访问器操作，不经过 normalize_key。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_context).

%% API
-export([new/0, new/1]).
-export([get/2, get/3]).
-export([set/3, set_many/2]).
-export([get_messages/1, set_messages/2, append_message/2]).
-export([get_history/1, add_history/2]).
-export([with_kernel/2, get_kernel/1]).
-export([add_trace/2, get_trace/1]).
-export([get_metadata/1, set_metadata/3]).

%% 工具函数
-export([normalize_key/1]).
-export([keys/1, delete/2, has_key/2, update/3]).

%% Types
-export_type([t/0, message/0, tool_call/0, trace_entry/0]).

-type message() :: beamai_message:message().
-type tool_call() :: beamai_message:tool_call().

-type t() :: #{
    '__context__' := true,
    messages := [message()],
    history := [message()],
    kernel := term() | undefined,
    trace := [trace_entry()],
    metadata := map(),
    binary() => term()
}.

-type trace_entry() :: #{
    timestamp := integer(),
    type := atom(),
    data := term()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 创建空的执行上下文
-spec new() -> t().
new() ->
    #{
        '__context__' => true,
        messages => [],
        history => [],
        kernel => undefined,
        trace => [],
        metadata => #{}
    }.

%% @doc 创建带初始变量的执行上下文
%%
%% @param Vars 初始变量 Map，键值对标准化后合并到上下文顶层
-spec new(map()) -> t().
new(Vars) when is_map(Vars) ->
    Normalized = maps:fold(fun(K, V, Acc) -> Acc#{normalize_key(K) => V} end, #{}, Vars),
    maps:merge(new(), Normalized).

%% @doc 标准化 key
%%
%% - '__context__' 保持 atom（类型标记）
%% - 其他 atom key → binary（用户变量）
%% - binary key 保持不变
-spec normalize_key(atom() | binary()) -> atom() | binary().
normalize_key('__context__') -> '__context__';
normalize_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
normalize_key(Key) when is_binary(Key) -> Key.

%% @doc 获取上下文中的变量值
%%
%% 键不存在时返回 undefined。
-spec get(t(), atom() | binary()) -> term() | undefined.
get(Ctx, Key) ->
    maps:get(normalize_key(Key), Ctx, undefined).

%% @doc 获取上下文中的变量值（带默认值）
-spec get(t(), atom() | binary(), term()) -> term().
get(Ctx, Key, Default) ->
    maps:get(normalize_key(Key), Ctx, Default).

%% @doc 设置上下文变量
%%
%% 返回新的上下文（不可变更新），原上下文不变。
-spec set(t(), atom() | binary(), term()) -> t().
set(Ctx, Key, Value) ->
    Ctx#{normalize_key(Key) => Value}.

%% @doc 批量设置上下文变量
%%
%% 将 NewVars 中的所有键值对标准化后合并到上下文中，已有键会被覆盖。
-spec set_many(t(), map()) -> t().
set_many(Ctx, NewVars) ->
    Normalized = maps:fold(fun(K, V, Acc) -> Acc#{normalize_key(K) => V} end, #{}, NewVars),
    maps:merge(Ctx, Normalized).

%% @doc 获取上下文所有键
-spec keys(t()) -> [atom() | binary()].
keys(Ctx) -> maps:keys(Ctx).

%% @doc 删除上下文中的变量
-spec delete(t(), atom() | binary()) -> t().
delete(Ctx, Key) -> maps:remove(normalize_key(Key), Ctx).

%% @doc 检查上下文中是否存在指定键
-spec has_key(t(), atom() | binary()) -> boolean().
has_key(Ctx, Key) -> maps:is_key(normalize_key(Key), Ctx).

%% @doc 使用函数更新上下文变量值
-spec update(t(), atom() | binary(), fun((term()) -> term())) -> t().
update(Ctx, Key, Fun) ->
    NK = normalize_key(Key),
    set(Ctx, NK, Fun(get(Ctx, NK))).

%% @doc 获取消息缓冲（发给 LLM 的工作上下文）
%%
%% 可能是经过 summarize/truncate 处理后的消息列表。
-spec get_messages(t()) -> [message()].
get_messages(#{messages := Messages}) -> Messages.

%% @doc 替换消息缓冲（用于 summarize/truncate 后重置）
%%
%% @param Ctx 上下文
%% @param Messages 新的消息列表
%% @returns 更新后的上下文
-spec set_messages(t(), [message()]) -> t().
set_messages(Ctx, Messages) ->
    Ctx#{messages => Messages}.

%% @doc 追加消息到消息缓冲末尾
%%
%% @param Ctx 上下文
%% @param Message 消息 Map
%% @returns 更新后的上下文
-spec append_message(t(), message()) -> t().
append_message(#{messages := Messages} = Ctx, Message) ->
    Ctx#{messages => Messages ++ [Message]}.

%% @doc 获取完整对话历史（只追加的原始日志）
%%
%% 返回上下文中积累的所有原始消息记录，不会被 summarize 影响。
-spec get_history(t()) -> [message()].
get_history(#{history := History}) -> History.

%% @doc 追加消息到对话历史末尾（只追加，不可修改）
%%
%% @param Ctx 上下文
%% @param Message 消息 Map，至少包含 role 和 content 字段
%% @returns 包含新消息的上下文
-spec add_history(t(), message()) -> t().
add_history(#{history := History} = Ctx, Message) ->
    Ctx#{history => History ++ [Message]}.

%% @doc 将 Kernel 引用关联到上下文
%%
%% 使得函数执行期间可以通过上下文访问 Kernel。
%%
%% @param Ctx 上下文
%% @param Kernel Kernel 实例
%% @returns 关联了 Kernel 的新上下文
-spec with_kernel(t(), term()) -> t().
with_kernel(Ctx, Kernel) ->
    Ctx#{kernel => Kernel}.

%% @doc 获取上下文关联的 Kernel 引用
%%
%% 未关联时返回 undefined。
-spec get_kernel(t()) -> term() | undefined.
get_kernel(#{kernel := Kernel}) -> Kernel.

%% @doc 添加执行跟踪记录
%%
%% 自动附加当前毫秒时间戳。用于调试和审计函数调用链。
%%
%% @param Ctx 上下文
%% @param Entry 跟踪条目，包含 type 和 data 字段
%% @returns 包含新跟踪记录的上下文
-spec add_trace(t(), trace_entry()) -> t().
add_trace(#{trace := Trace} = Ctx, Entry) ->
    Ctx#{trace => Trace ++ [Entry#{timestamp => erlang:system_time(millisecond)}]}.

%% @doc 获取所有执行跟踪记录
-spec get_trace(t()) -> [trace_entry()].
get_trace(#{trace := Trace}) -> Trace.

%% @doc 获取上下文元数据
-spec get_metadata(t()) -> map().
get_metadata(#{metadata := Meta}) -> Meta.

%% @doc 设置上下文元数据字段
%%
%% @param Ctx 上下文
%% @param Key 元数据键（binary 或 atom）
%% @param Value 元数据值
%% @returns 更新后的上下文
-spec set_metadata(t(), binary() | atom(), term()) -> t().
set_metadata(#{metadata := Meta} = Ctx, Key, Value) ->
    Ctx#{metadata => Meta#{Key => Value}}.
