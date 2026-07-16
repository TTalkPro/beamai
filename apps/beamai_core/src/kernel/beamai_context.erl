%%%-------------------------------------------------------------------
%%% @doc 执行上下文：三分区（运行环境 / 用户状态 / filter 私有）
%%%
%%% 在函数调用链中传递的不可变上下文，拆为三个身份不同的分区：
%%%
%%%   - **env（只读运行环境，框架自持）**：kernel 引用、conversation_id、
%%%     调用方注入变量（vars）。工具与 filter **只读**；不参与序列化
%%%     （kernel 是引用/闭包）。
%%%   - **state（用户状态槽，纯数据可序列化）**：工具间共享状态的唯一通道。
%%%     工具**不直接写**，写意图经调用返回的 Writes 表达，由 tool 批次在
%%%     屏障处按 tool_call 原始序折叠进来（见 apply_writes/3）。可整体
%%%     取出/放回（get_state/1、with_state/2），为暂停持久化提供序列化边界。
%%%   - **filter 私有状态（框架自管）**：按 filter 名隔离，供 filter 洋葱链
%%%     投影/合并，用户不可见（filter_state/3、set_filter_state/3）。
%%%
%%% 注意：context 不记录消息/历史（会话历史由 memory provider / memory_filter
%%% 按 conversation_id 管理，详见 design/kernel_memory_filter_redesign.md），
%%% 也不承担工具间可变穿线（见 design/context_split_parallel_tools.md）。
%%%
%%% 变量 key 标准化：atom → binary（`get/set` 等接受 atom 或 binary，内部
%%% 统一存为 binary）。env/state/filter_states 分区键为 atom，经专用访问器
%%% 操作，不经 normalize_key。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_context).

%% 构造
-export([new/0, new/1]).
%% env：只读运行环境（vars 仅构造期注入）
-export([get/2, get/3, variables/1]).
-export([set/3, set_many/2]).
-export([with_conversation_id/2, conversation_id/1]).
-export([with_kernel/2, get_kernel/1]).
-export([with_default_tool_timeout/2, default_tool_timeout/1]).
%% state：用户状态槽（纯数据，工具经 Writes 写、屏障折叠）
-export([state_get/2, state_get/3, get_state/1, with_state/2, apply_writes/3]).
%% filter 私有状态（框架自管）
-export([filter_state/3, set_filter_state/3]).
%% 变量工具
-export([normalize_key/1]).
-export([keys/1, delete/2, has_key/2, update/3]).

%% Types
-export_type([t/0, env/0, writes/0, state_slots/0, message/0, tool_call/0]).

-type message() :: beamai_message:message().
-type tool_call() :: beamai_message:tool_call().

-type env() :: #{
    kernel := term() | undefined,
    conversation_id := binary() | undefined,
    vars := #{binary() => term()},
    %% 本轮执行的**缺省**工具超时（由 ToolCallingManager 注入）。工具自身声明的
    %% tool_spec.timeout 优先级更高。可选键——缺席即回落 beamai_tool 的内置缺省。
    %% 放 env 专用槽而非 vars：vars 是喂模板渲染的用户变量表（见 variables/1），
    %% 执行策略漏进去会污染用户的提示词变量。
    default_tool_timeout => pos_integer() | infinity
}.

%% 单个工具一轮的写意图（纯数据，屏障处折叠进 state）
-type writes() :: #{binary() | atom() => term()}.

%% 状态槽声明：init 缺省 undefined，reduce 缺省 last-writer
-type state_slots() :: #{binary() => #{init => term(), reduce => fun((term(), term()) -> term())}}.

-type t() :: #{
    '__context__' := true,
    env := env(),
    state := #{binary() => term()},
    '__filter_states__' := #{binary() => map()}
}.

%% filter 私有上下文槽（内部 atom key，按 filter 名字隔离，不经 normalize_key）
-define(FILTER_STATES_KEY, '__filter_states__').

%%====================================================================
%% 构造
%%====================================================================

%% @doc 创建空的执行上下文
-spec new() -> t().
new() ->
    #{
        '__context__' => true,
        env => #{kernel => undefined, conversation_id => undefined, vars => #{}},
        state => #{},
        ?FILTER_STATES_KEY => #{}
    }.

%% @doc 创建带初始 env 变量的执行上下文
%%
%% Vars 标准化后注入 env.vars（只读运行环境，供工具/ filter 读取）。
-spec new(map()) -> t().
new(Vars) when is_map(Vars) ->
    Normalized = normalize_map(Vars),
    Ctx = new(),
    #{env := Env} = Ctx,
    Ctx#{env => Env#{vars => Normalized}}.

%% @doc 标准化 key：atom → binary，binary 原样
-spec normalize_key(atom() | binary()) -> binary().
normalize_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
normalize_key(Key) when is_binary(Key) -> Key.

%%====================================================================
%% env：只读运行环境
%%====================================================================

%% @doc 读取 env 变量（不存在返回 undefined）
-spec get(t(), atom() | binary()) -> term() | undefined.
get(Ctx, Key) -> get(Ctx, Key, undefined).

%% @doc 读取 env 变量（带默认值）
-spec get(t(), atom() | binary(), term()) -> term().
get(#{env := #{vars := Vars}}, Key, Default) ->
    maps:get(normalize_key(Key), Vars, Default).

%% @doc 取全部 env 变量 map（供模板渲染等按变量表消费）
-spec variables(t()) -> #{binary() => term()}.
variables(#{env := #{vars := Vars}}) -> Vars.

%% @doc 注入单个 env 变量（返回新上下文；语义为构造期只读环境注入）
-spec set(t(), atom() | binary(), term()) -> t().
set(#{env := Env} = Ctx, Key, Value) ->
    #{vars := Vars} = Env,
    Ctx#{env => Env#{vars => Vars#{normalize_key(Key) => Value}}}.

%% @doc 批量注入 env 变量（已有键覆盖）
-spec set_many(t(), map()) -> t().
set_many(#{env := Env} = Ctx, NewVars) ->
    #{vars := Vars} = Env,
    Ctx#{env => Env#{vars => maps:merge(Vars, normalize_map(NewVars))}}.

%% @doc 列出所有 env 变量 key
-spec keys(t()) -> [binary()].
keys(#{env := #{vars := Vars}}) -> maps:keys(Vars).

%% @doc 删除 env 变量
-spec delete(t(), atom() | binary()) -> t().
delete(#{env := Env} = Ctx, Key) ->
    #{vars := Vars} = Env,
    Ctx#{env => Env#{vars => maps:remove(normalize_key(Key), Vars)}}.

%% @doc 检查 env 变量是否存在
-spec has_key(t(), atom() | binary()) -> boolean().
has_key(#{env := #{vars := Vars}}, Key) -> maps:is_key(normalize_key(Key), Vars).

%% @doc 用函数更新 env 变量值
-spec update(t(), atom() | binary(), fun((term()) -> term())) -> t().
update(Ctx, Key, Fun) ->
    NK = normalize_key(Key),
    set(Ctx, NK, Fun(get(Ctx, NK))).

%% @doc 关联会话标识（供 Memory Filter / provider 定位会话历史）
-spec with_conversation_id(t(), binary()) -> t().
with_conversation_id(#{env := Env} = Ctx, ConvId) ->
    Ctx#{env => Env#{conversation_id => ConvId}}.

%% @doc 获取会话标识（未设置返回 undefined）
-spec conversation_id(t()) -> binary() | undefined.
conversation_id(#{env := #{conversation_id := ConvId}}) -> ConvId;
conversation_id(_) -> undefined.

%% @doc 关联 Kernel 引用（invoke_chat/invoke_tool 入口自动绑定）
-spec with_kernel(t(), term()) -> t().
with_kernel(#{env := Env} = Ctx, Kernel) ->
    Ctx#{env => Env#{kernel => Kernel}}.

%% @doc 获取 Kernel 引用（未关联返回 undefined）
-spec get_kernel(t()) -> term() | undefined.
get_kernel(#{env := #{kernel := Kernel}}) -> Kernel;
get_kernel(_) -> undefined.

%% @doc 注入本轮执行的缺省工具超时（由 ToolCallingManager 在批执行前绑入）
%%
%% 传 undefined 表示**清除**——每批由其 manager 显式决定，不留上一批的残值
%% （context 会跨轮穿线，不清则策略会粘住）。
-spec with_default_tool_timeout(t(), pos_integer() | infinity | undefined) -> t().
with_default_tool_timeout(#{env := Env} = Ctx, undefined) ->
    Ctx#{env => maps:remove(default_tool_timeout, Env)};
with_default_tool_timeout(#{env := Env} = Ctx, Timeout) ->
    Ctx#{env => Env#{default_tool_timeout => Timeout}}.

%% @doc 读取本轮的缺省工具超时（未注入返回 undefined，由调用方回落自身缺省）
-spec default_tool_timeout(t()) -> pos_integer() | infinity | undefined.
default_tool_timeout(#{env := #{default_tool_timeout := Timeout}}) -> Timeout;
default_tool_timeout(_) -> undefined.

%%====================================================================
%% state：用户状态槽
%%====================================================================

%% @doc 读取状态槽（不存在返回 undefined）
-spec state_get(t(), atom() | binary()) -> term() | undefined.
state_get(Ctx, Key) -> state_get(Ctx, Key, undefined).

%% @doc 读取状态槽（带默认值）
-spec state_get(t(), atom() | binary(), term()) -> term().
state_get(#{state := State}, Key, Default) ->
    maps:get(normalize_key(Key), State, Default).

%% @doc 取整个 state 分区（纯数据，供序列化 / 快照）
-spec get_state(t()) -> #{binary() => term()}.
get_state(#{state := State}) -> State.

%% @doc 整体替换 state 分区（供 resume / 快照恢复）
-spec with_state(t(), #{binary() => term()}) -> t().
with_state(Ctx, State) when is_map(State) -> Ctx#{state => State}.

%% @doc 按 tool_call 原始序折叠一批工具的 writes 进 state
%%
%% IndexedWrites 为 `[{Index, Writes}]`：Index 是 tool_call 原始序（1 基）。
%% 折叠规则（确定性：折叠序钉死为 Index 升序，reducer 无需交换律）：
%%   - 声明槽（Slots 含该 key）：`Reduce(Cur, V)`，Cur 缺省取槽 init；
%%   - 未声明槽：last-writer（Index 大者胜）；同批被多个工具写 → 记为 conflict。
%%
%% 失败/超时/skipped 工具的 Writes 应为 `#{}`（不参与折叠）——单工具事务性。
%%
%% @returns `{NewCtx, Conflicts}`，Conflicts 为同批双写的未声明槽名（供告警）
-spec apply_writes(t(), [{pos_integer(), writes()}], state_slots()) ->
    {t(), [binary()]}.
apply_writes(Ctx, IndexedWrites, Slots) ->
    Ordered = lists:keysort(1, IndexedWrites),
    KVs = lists:flatmap(fun({_Idx, W}) -> normalize_writes(W) end, Ordered),
    State0 = get_state(Ctx),
    State1 = lists:foldl(fun({K, V}, St) -> fold_kv(K, V, St, Slots) end, State0, KVs),
    Conflicts = batch_conflicts(KVs, Slots),
    {with_state(Ctx, State1), Conflicts}.

%%====================================================================
%% filter 私有状态（框架自管）
%%====================================================================

%% @doc 读取某 filter 的私有上下文（按名字隔离，缺省返回 Default）
%%
%% 供 filter 洋葱链投影使用；filter 代码经 around 的 FCtx 参数间接读取。
-spec filter_state(t(), binary(), map()) -> map().
filter_state(#{?FILTER_STATES_KEY := States}, Name, Default) ->
    maps:get(Name, States, Default);
filter_state(_, _Name, Default) ->
    Default.

%% @doc 写回某 filter 的私有上下文（按名字隔离）
%%
%% 供 filter 洋葱链合并使用；filter 代码经 around 返回 {Resp, NewFCtx} 间接写。
-spec set_filter_state(t(), binary(), map()) -> t().
set_filter_state(#{?FILTER_STATES_KEY := States} = Ctx, Name, State) ->
    Ctx#{?FILTER_STATES_KEY => States#{Name => State}}.

%%====================================================================
%% 内部
%%====================================================================

%% @private 标准化变量 map 的 key（atom → binary）
normalize_map(Map) ->
    maps:fold(fun(K, V, Acc) -> Acc#{normalize_key(K) => V} end, #{}, Map).

%% @private 单个 writes map 归一为有序 [{binary_key, V}]（key 标准化）
normalize_writes(W) ->
    [{normalize_key(K), V} || {K, V} <- maps:to_list(W)].

%% @private 单条 write 折叠进 state
fold_kv(K, V, St, Slots) ->
    case maps:find(K, Slots) of
        {ok, #{reduce := Reduce} = Slot} when is_function(Reduce, 2) ->
            Cur = maps:get(K, St, maps:get(init, Slot, undefined)),
            St#{K => Reduce(Cur, V)};
        _ ->
            %% 未声明（或声明缺 reduce）：last-writer，Index 升序 fold ⇒ 末者胜
            St#{K => V}
    end.

%% @private 收集同批被多个工具写、且未声明 reducer 的槽（last-writer 冲突）
batch_conflicts(KVs, Slots) ->
    Counts = lists:foldl(fun({K, _V}, M) ->
        maps:update_with(K, fun(N) -> N + 1 end, 1, M)
    end, #{}, KVs),
    [K || {K, N} <- maps:to_list(Counts), N > 1,
          not is_declared_reducer(K, Slots)].

%% @private 是否为带 reducer 的声明槽（声明槽的多写由 reducer 处理，非冲突）
is_declared_reducer(K, Slots) ->
    case maps:find(K, Slots) of
        {ok, #{reduce := R}} when is_function(R, 2) -> true;
        _ -> false
    end.
