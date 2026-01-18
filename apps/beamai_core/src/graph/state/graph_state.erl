%%%-------------------------------------------------------------------
%%% @doc 图状态管理模块
%%%
%%% 提供图执行过程中的不可变状态操作。
%%% 状态以 Map 形式存储，支持:
%%% - 安全的读取/设置操作
%%% - 状态合并和更新
%%% - 状态验证
%%%
%%% 设计原则:
%%% - 不可变性: 所有操作返回新状态
%%% - 类型安全: 键统一为 binary 类型（避免 atom 泄漏）
%%% - 简洁性: 每个函数只做一件事
%%%
%%% == 为什么使用 Binary 键而不是 Atom 键？==
%%%
%%% Atom 在 Erlang VM 中是全局的、不可回收的。
%%% 如果动态创建 Atom（从外部输入），会导致 atom 表溢出，VM 崩溃。
%%%
%%% 使用 Binary 作为键可以：
%%% 1. 避免 atom 泄漏
%%% 2. 安全处理外部输入
%%% 3. 保持高性能（Binary 比较和哈希都很高效）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_state).

%% API 导出
-export([new/0, new/1]).
-export([get/2, get/3]).
-export([set/3, set_many/2]).
-export([update/3, merge/2]).
-export([delete/2, keys/1, values/1]).
-export([has_key/2, is_empty/1]).
-export([to_map/1, from_map/1]).

%% 类型定义
-type state() :: #{binary() => term()}.
-type key() :: binary() | atom().
-type value() :: term().

-export_type([state/0, key/0, value/0]).

%%====================================================================
%%% 状态创建
%%====================================================================

%% @doc 创建空状态
-spec new() -> state().
new() ->
    #{}.

%% @doc 从 Map 创建状态
%% 自动将键转换为 binary 类型（避免 atom 泄漏）
-spec new(map()) -> state().
new(InitialMap) when is_map(InitialMap) ->
    normalize_keys(InitialMap).

%%====================================================================
%%% 状态读取
%%====================================================================

%% @doc 获取指定键的值
%% 键不存在时返回 undefined
%% 支持 atom 和 binary 键
-spec get(state(), key()) -> value() | undefined.
get(State, Key) when is_atom(Key) ->
    maps:get(atom_to_binary(Key, utf8), State, undefined);
get(State, Key) when is_binary(Key) ->
    maps:get(Key, State, undefined).

%% @doc 获取指定键的值，支持默认值
%% 支持 atom 和 binary 键
-spec get(state(), key(), value()) -> value().
get(State, Key, Default) when is_atom(Key) ->
    maps:get(atom_to_binary(Key, utf8), State, Default);
get(State, Key, Default) when is_binary(Key) ->
    maps:get(Key, State, Default).

%% @doc 检查键是否存在
%% 支持 atom 和 binary 键
-spec has_key(state(), key()) -> boolean().
has_key(State, Key) when is_atom(Key) ->
    maps:is_key(atom_to_binary(Key, utf8), State);
has_key(State, Key) when is_binary(Key) ->
    maps:is_key(Key, State).

%% @doc 检查状态是否为空
-spec is_empty(state()) -> boolean().
is_empty(State) ->
    maps:size(State) =:= 0.

%% @doc 获取所有键
-spec keys(state()) -> [key()].
keys(State) ->
    maps:keys(State).

%% @doc 获取所有值
-spec values(state()) -> [value()].
values(State) ->
    maps:values(State).

%%====================================================================
%%% 状态修改 (不可变 - 返回新状态)
%%====================================================================

%% @doc 设置单个键值对
%% 支持 atom 和 binary 键，内部统一存储为 binary
-spec set(state(), key(), value()) -> state().
set(State, Key, Value) when is_atom(Key) ->
    State#{atom_to_binary(Key, utf8) => Value};
set(State, Key, Value) when is_binary(Key) ->
    State#{Key => Value}.

%% @doc 批量设置键值对
%% 支持列表或 Map 格式
-spec set_many(state(), [{key(), value()}] | map()) -> state().
set_many(State, Pairs) when is_list(Pairs) ->
    lists:foldl(fun({K, V}, Acc) -> set(Acc, K, V) end, State, Pairs);
set_many(State, Map) when is_map(Map) ->
    maps:merge(State, normalize_keys(Map)).

%% @doc 使用函数更新值
%% UpdateFun 接收旧值（或 undefined），返回新值
-spec update(state(), key(), fun((value() | undefined) -> value())) -> state().
update(State, Key, UpdateFun) ->
    OldValue = get(State, Key),
    NewValue = UpdateFun(OldValue),
    set(State, Key, NewValue).

%% @doc 合并两个状态
%% 冲突时右侧状态优先
-spec merge(state(), state()) -> state().
merge(State1, State2) ->
    maps:merge(State1, State2).

%% @doc 删除指定键
%% 支持 atom 和 binary 键
-spec delete(state(), key()) -> state().
delete(State, Key) when is_atom(Key) ->
    maps:remove(atom_to_binary(Key, utf8), State);
delete(State, Key) when is_binary(Key) ->
    maps:remove(Key, State).

%%====================================================================
%%% 类型转换
%%====================================================================

%% @doc 转换为普通 Map
-spec to_map(state()) -> map().
to_map(State) ->
    State.

%% @doc 从普通 Map 创建状态
-spec from_map(map()) -> state().
from_map(Map) ->
    new(Map).

%%====================================================================
%%% 内部函数
%%====================================================================

%% @doc 将 Map 的键标准化为 binary
%%
%% 避免动态创建 atom，防止 atom 表溢出。
%% 转换规则：
%% - binary: 直接使用（推荐）
%% - atom: 转换为 binary（安全，atom 是编译时已知的）
%% - list: 转换为 binary（安全，list 是内容已知的）
%%
%% 关键设计决策：
%% - 我们将所有键统一为 binary 类型（内部存储）
%% - 但接受 atom 作为输入（为了代码便利性）
%% - 永远不将任意 binary 转换为 atom（防止 atom 泄漏）
%%
%% @see https://www.erlang.org/doc/efficiency_guide/atoms.html
-spec normalize_keys(map()) -> state().
normalize_keys(Map) ->
    maps:fold(fun normalize_key/3, #{}, Map).

%% @doc 标准化单个键
%%
%% 转换规则：
%% - binary: 直接使用（推荐，避免 atom 泄漏）
%% - atom: 转换为 binary（安全的，atom 是编译时已知的）
%% - list: 转换为 binary（安全的，list 是内容已知的）
%% - 其他: 抛出错误
%%
%% 安全保证：
%% - 只将已知的 atom 转换为 binary（编译时确定）
%% - 不将任意 binary 转换为 atom（防止 atom 表溢出）
-spec normalize_key(term(), value(), state()) -> state().
normalize_key(Key, Value, Acc) when is_binary(Key) ->
    %% binary 键：直接使用（推荐方式）
    %% 不转换为 atom，避免 atom 泄漏
    Acc#{Key => Value};
normalize_key(Key, Value, Acc) when is_atom(Key) ->
    %% atom 键：转换为 binary（安全的，atom 是已知的）
    %% 这里的 atom 是代码中显式写的，不是外部输入
    Acc#{atom_to_binary(Key, utf8) => Value};
normalize_key(Key, Value, Acc) when is_list(Key) ->
    %% list 键：转换为 binary（安全的，list 内容是已知的）
    Acc#{list_to_binary(Key) => Value};
normalize_key(Key, _Value, _Acc) ->
    %% 其他类型：错误
    error({invalid_key_type, Key}).
