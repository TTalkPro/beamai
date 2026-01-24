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

%% Types
-export_type([t/0, message/0, trace_entry/0]).

-type t() :: #{
    '__context__' := true,
    variables := #{binary() => term()},
    messages := [message()],
    history := [message()],
    kernel := term() | undefined,
    trace := [trace_entry()],
    metadata := map()
}.

-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    tool_call_id => binary(),
    name => binary()
}.

-type tool_call() :: #{
    id := binary(),
    type := function,
    function := #{
        name := binary(),
        arguments := binary() | map()
    }
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
%%
%% 返回包含空变量表、空历史、空跟踪的初始上下文。
-spec new() -> t().
new() ->
    #{
        '__context__' => true,
        variables => #{},
        messages => [],
        history => [],
        kernel => undefined,
        trace => [],
        metadata => #{}
    }.

%% @doc 创建带初始变量的执行上下文
%%
%% @param Vars 初始变量 Map，键值对将存入 variables 字段
-spec new(map()) -> t().
new(Vars) when is_map(Vars) ->
    Ctx = new(),
    Ctx#{variables => Vars}.

%% @doc 获取上下文中的变量值
%%
%% 键不存在时返回 undefined。
%%
%% @param Ctx 上下文
%% @param Key 变量名
%% @returns 变量值或 undefined
-spec get(t(), binary()) -> term() | undefined.
get(#{variables := Vars}, Key) ->
    maps:get(Key, Vars, undefined).

%% @doc 获取上下文中的变量值（带默认值）
%%
%% @param Ctx 上下文
%% @param Key 变量名
%% @param Default 键不存在时的默认值
%% @returns 变量值或默认值
-spec get(t(), binary(), term()) -> term().
get(#{variables := Vars}, Key, Default) ->
    maps:get(Key, Vars, Default).

%% @doc 设置上下文变量
%%
%% 返回新的上下文（不可变更新），原上下文不变。
%%
%% @param Ctx 上下文
%% @param Key 变量名
%% @param Value 变量值
%% @returns 更新后的新上下文
-spec set(t(), binary(), term()) -> t().
set(#{variables := Vars} = Ctx, Key, Value) ->
    Ctx#{variables => Vars#{Key => Value}}.

%% @doc 批量设置上下文变量
%%
%% 将 NewVars 中的所有键值对合并到上下文变量中，已有键会被覆盖。
%%
%% @param Ctx 上下文
%% @param NewVars 要合并的变量 Map
%% @returns 更新后的新上下文
-spec set_many(t(), map()) -> t().
set_many(#{variables := Vars} = Ctx, NewVars) ->
    Ctx#{variables => maps:merge(Vars, NewVars)}.

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
