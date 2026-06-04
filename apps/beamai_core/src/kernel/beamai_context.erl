%%%-------------------------------------------------------------------
%%% @doc 执行上下文：变量、会话标识、跟踪
%%%
%%% 在函数调用链中传递的不可变上下文，包含：
%%% - 变量存储（key-value）
%%% - 会话标识（conversation_id，供 Memory Filter 定位会话）
%%% - Kernel 引用
%%% - 执行跟踪（调试用）
%%% - 元数据
%%%
%%% 注意：context 不再记录消息/历史。会话历史的存储与注入由
%%% beamai_memory_filter + beamai_chat_memory store 承担，按
%%% conversation_id 管理。详见 design/kernel_memory_filter_redesign.md。
%%%
%%% Key 标准化规则：
%%% - '__context__' 保持 atom（类型标记）
%%% - 其他 atom key → binary（用户变量）
%%% - binary key 保持不变
%%%
%%% 内部字段（kernel, trace, metadata）使用 atom key，
%%% 通过专用访问器操作，不经过 normalize_key。
%%% conversation_id 存为保留 binary key `<<"__conversation_id__">>`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_context).

%% API
-export([new/0, new/1]).
-export([get/2, get/3]).
-export([set/3, set_many/2]).
-export([with_conversation_id/2, conversation_id/1]).
-export([filter_state/3, set_filter_state/3]).
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
    kernel := term() | undefined,
    trace := [trace_entry()],
    metadata := map(),
    %% filter 私有上下文槽，存于内部 atom key '__filter_states__'（见 ?FILTER_STATES_KEY）
    '__filter_states__' := #{binary() => map()},
    binary() => term()
}.

%% conversation_id 的保留存储 key
-define(CONV_ID_KEY, <<"__conversation_id__">>).

%% filter 私有上下文槽（内部 atom key，按 filter 名字隔离，不经 normalize_key）
-define(FILTER_STATES_KEY, '__filter_states__').

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
        kernel => undefined,
        trace => [],
        metadata => #{},
        ?FILTER_STATES_KEY => #{}
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

%% @doc 关联会话标识（供 Memory Filter 定位会话历史）
%%
%% @param Ctx 上下文
%% @param ConvId 会话 ID（binary）
%% @returns 关联了会话 ID 的新上下文
-spec with_conversation_id(t(), binary()) -> t().
with_conversation_id(Ctx, ConvId) ->
    Ctx#{?CONV_ID_KEY => ConvId}.

%% @doc 获取上下文的会话标识
%%
%% 未设置时返回 undefined（Memory Filter 据此退化为无状态调用）。
-spec conversation_id(t()) -> binary() | undefined.
conversation_id(Ctx) ->
    maps:get(?CONV_ID_KEY, Ctx, undefined).

%% @doc 读取某 filter 的私有上下文（按名字隔离，缺省返回 Default）
%%
%% 供 filter 洋葱链投影使用；filter 代码经 around 的 FCtx 参数间接读取，
%% 不直接调用本函数。
-spec filter_state(t(), binary(), map()) -> map().
filter_state(Ctx, Name, Default) ->
    States = maps:get(?FILTER_STATES_KEY, Ctx, #{}),
    maps:get(Name, States, Default).

%% @doc 写回某 filter 的私有上下文（按名字隔离）
%%
%% 供 filter 洋葱链合并使用；filter 代码经 around 返回 {Resp, NewFCtx} 间接写。
-spec set_filter_state(t(), binary(), map()) -> t().
set_filter_state(Ctx, Name, State) ->
    States = maps:get(?FILTER_STATES_KEY, Ctx, #{}),
    Ctx#{?FILTER_STATES_KEY => States#{Name => State}}.

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
