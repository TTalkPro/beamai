%%%-------------------------------------------------------------------
%%% @doc Store 管理器 - 双 Store 架构的核心
%%%
%%% 管理 context_store 和 persistent_store，提供统一的存储接口。
%%%
%%% == 职责 ==
%%%
%%% - 管理 context_store（短期记忆，快速）
%%% - 管理 persistent_store（长期记忆，持久化）
%%% - 提供 Store 选择逻辑（effective store）
%%% - 提供归档功能
%%%
%%% == 架构 ==
%%%
%%% ```
%%% store_manager
%%%   ├── context_store  (必需) - 内存存储，用于当前会话
%%%   └── persistent_store (可选) - 持久化存储，用于归档
%%% '''
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 Store 管理器（单 Store 模式）
%%% {ok, ContextStore} = beamai_store_ets:start_link(my_context, #{}),
%%% StoreManager = beamai_store_manager:new(ContextStore).
%%%
%%% %% 创建 Store 管理器（双 Store 模式）
%%% {ok, PersistStore} = beamai_store_sqlite:start_link(my_persist, #{
%%%     db_path => "/data/memory.db"
%%% }),
%%% StoreManager = beamai_store_manager:new(ContextStore, PersistStore).
%%%
%%% %% 存储数据（自动选择合适的 Store）
%%% ok = beamai_store_manager:put(StoreManager,
%%%     [<<"user">>, <<"123">>, <<"prefs">>],
%%%     <<"theme">>,
%%%     #{value => <<"dark">>},
%%%     #{persistent => true}  % 存到 persistent_store
%%% ).
%%%
%%% %% 获取数据
%%% {ok, Item} = beamai_store_manager:get(StoreManager,
%%%     [<<"user">>, <<"123">>, <<"prefs">>],
%%%     <<"theme">>
%%% ).
%%%
%%% %% 归档会话
%%% {ok, SessionId} = beamai_store_manager:archive_session(
%%%     StoreManager,
%%%     Checkpointer,
%%%     <<"thread-1">>
%%% ).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_store_manager).

-include_lib("beamai_memory/include/beamai_store.hrl").

%% 构造函数
-export([new/1, new/2]).

%% Store 操作
-export([put/5, get/3, search/3, delete/3, list_namespaces/3]).

%% 批量操作
-export([put_batch/3, get_batch/3, delete_batch/3]).

%% Store 查询
-export([get_context_store/1, get_persistent_store/1,
         get_effective_store/2, has_persistent/1]).

%% 统计信息
-export([count/2, stats/1, clear/2]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(store_manager, {
    context_store :: beamai_store:store(),
    persistent_store :: beamai_store:store() | undefined
}).

-type store_manager() :: #store_manager{}.
-type namespace() :: beamai_store:namespace().
-type store_key() :: beamai_store:key().

-export_type([store_manager/0]).

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 Store 管理器（单 Store 模式）
-spec new(beamai_store:store()) -> store_manager().
new(ContextStore) ->
    #store_manager{
        context_store = ContextStore,
        persistent_store = undefined
    }.

%% @doc 创建 Store 管理器（双 Store 模式）
-spec new(beamai_store:store(), beamai_store:store() | undefined) -> store_manager().
new(ContextStore, PersistentStore) ->
    #store_manager{
        context_store = ContextStore,
        persistent_store = PersistentStore
    }.

%%====================================================================
%% Store 操作
%%====================================================================

%% @doc 存储值
%%
%% Opts:
%% - persistent => true | false (是否存储到 persistent_store，默认 true)
%% - metadata => map() (元数据)
%% - embedding => [float()] (向量)
-spec put(store_manager(), namespace(), store_key(), map(), map()) ->
    ok | {error, term()}.
put(#store_manager{context_store = CtxStore, persistent_store = undefined},
    Namespace, Key, Value, Opts) ->
    %% 单 Store 模式：使用 context_store
    beamai_store:put(CtxStore, Namespace, Key, Value, Opts);
put(#store_manager{context_store = CtxStore, persistent_store = PersistStore},
    Namespace, Key, Value, Opts) ->
    %% 双 Store 模式：根据 persistent 参数选择
    case maps_get(persistent, Opts, true) of
        true ->
            beamai_store:put(PersistStore, Namespace, Key, Value, Opts);
        false ->
            beamai_store:put(CtxStore, Namespace, Key, Value, Opts)
    end.

%% @doc 获取值
%%
%% Opts:
%% - persistent => true | false (优先从 persistent_store 获取)
-spec get(store_manager(), namespace(), store_key()) ->
    {ok, beamai_store:item()} | {error, term()}.
get(#store_manager{persistent_store = undefined, context_store = CtxStore},
    Namespace, Key) ->
    beamai_store:get(CtxStore, Namespace, Key);
get(#store_manager{context_store = CtxStore, persistent_store = PersistStore},
    Namespace, Key) ->
    %% 优先从 persistent_store 获取，如果不存在则从 context_store 获取
    case beamai_store:get(PersistStore, Namespace, Key) of
        {error, not_found} ->
            beamai_store:get(CtxStore, Namespace, Key);
        Result ->
            Result
    end.

%% @doc 搜索
%%
%% Opts:
%% - persistent => true | false (从 persistent_store 搜索)
%% - query => term() (搜索查询)
%% - limit => integer() (结果限制)
-spec search(store_manager(), namespace(), map()) ->
    {ok, [beamai_store:search_result()]} | {error, term()}.
search(#store_manager{persistent_store = undefined, context_store = CtxStore},
    Namespace, Opts) ->
    beamai_store:search(CtxStore, Namespace, Opts);
search(#store_manager{context_store = CtxStore, persistent_store = PersistStore},
    Namespace, Opts) ->
    Store = case maps_get(persistent, Opts, true) of
        true -> PersistStore;
        false -> CtxStore
    end,
    beamai_store:search(Store, Namespace, Opts).

%% @doc 删除
-spec delete(store_manager(), namespace(), store_key()) ->
    ok | {error, term()}.
delete(#store_manager{persistent_store = undefined, context_store = CtxStore},
    Namespace, Key) ->
    beamai_store:delete(CtxStore, Namespace, Key);
delete(#store_manager{context_store = CtxStore, persistent_store = PersistStore},
    Namespace, Key) ->
    %% 同时从两个 Store 删除
    Res1 = beamai_store:delete(CtxStore, Namespace, Key),
    Res2 = beamai_store:delete(PersistStore, Namespace, Key),
    case {Res1, Res2} of
        {ok, ok} -> ok;
        {{error, _} = Error, _} -> Error;
        {_, {error, _} = Error} -> Error
    end.

%% @doc 列出命名空间
-spec list_namespaces(store_manager(), namespace(), map()) ->
    {ok, [namespace()]} | {error, term()}.
list_namespaces(#store_manager{persistent_store = undefined, context_store = CtxStore},
    Prefix, Opts) ->
    beamai_store:list_namespaces(CtxStore, Prefix, Opts);
list_namespaces(#store_manager{context_store = CtxStore, persistent_store = PersistStore},
    Prefix, Opts) ->
    Store = case maps_get(persistent, Opts, true) of
        true -> PersistStore;
        false -> CtxStore
    end,
    beamai_store:list_namespaces(Store, Prefix, Opts).

%%====================================================================
%% 批量操作
%%====================================================================

%% @doc 批量存储
-spec put_batch(store_manager(), [map()], map()) ->
    {ok, [ok | {error, term()}]} | {error, term()}.
put_batch(#store_manager{} = Manager, Items, GlobalOpts) ->
    Results = lists:map(fun(Item) ->
        Namespace = maps_get(namespace, Item),
        Key = maps_get(key, Item),
        Value = maps_get(value, Item),
        Opts = maps_get(opts, Item, GlobalOpts),
        put(Manager, Namespace, Key, Value, Opts)
    end, Items),
    {ok, Results}.

%% @doc 批量获取
-spec get_batch(store_manager(), [{namespace(), store_key()}], map()) ->
    {ok, [{ok, beamai_store:item()} | {error, term()}]}.
get_batch(#store_manager{} = Manager, Keys, _Opts) ->
    Results = lists:map(fun({Namespace, Key}) ->
        get(Manager, Namespace, Key)
    end, Keys),
    {ok, Results}.

%% @doc 批量删除
-spec delete_batch(store_manager(), [{namespace(), store_key()}], map()) ->
    {ok, [ok | {error, term()}]}.
delete_batch(#store_manager{} = Manager, Keys, _Opts) ->
    Results = lists:map(fun({Namespace, Key}) ->
        delete(Manager, Namespace, Key)
    end, Keys),
    {ok, Results}.

%%====================================================================
%% Store 查询
%%====================================================================

%% @doc 获取 context_store
-spec get_context_store(store_manager()) -> beamai_store:store().
get_context_store(#store_manager{context_store = Store}) ->
    Store.

%% @doc 获取 persistent_store
-spec get_persistent_store(store_manager()) -> beamai_store:store() | undefined.
get_persistent_store(#store_manager{persistent_store = Store}) ->
    Store.

%% @doc 获取有效的 Store
-spec get_effective_store(store_manager(), boolean()) -> beamai_store:store().
get_effective_store(#store_manager{context_store = CtxStore, persistent_store = undefined}, _Persistent) ->
    CtxStore;
get_effective_store(#store_manager{context_store = CtxStore, persistent_store = PersistStore}, true) ->
    PersistStore;
get_effective_store(#store_manager{context_store = CtxStore, persistent_store = _PersistStore}, false) ->
    CtxStore.

%% @doc 检查是否有 persistent_store
-spec has_persistent(store_manager()) -> boolean().
has_persistent(#store_manager{persistent_store = undefined}) ->
    false;
has_persistent(#store_manager{persistent_store = _Store}) ->
    true.

%%====================================================================
%% 统计信息
%%====================================================================

%% @doc 统计条目数
-spec count(store_manager(), namespace()) -> non_neg_integer().
count(#store_manager{} = Manager, Namespace) ->
    case search(Manager, Namespace, #{}) of
        {ok, Results} -> length(Results);
        {error, _} -> 0
    end.

%% @doc 获取统计信息
-spec stats(store_manager()) -> map().
stats(#store_manager{context_store = CtxStore, persistent_store = PersistStore}) ->
    #{
        has_persistent => has_persistent(#store_manager{persistent_store = PersistStore}),
        context_store => format_store_ref(CtxStore),
        persistent_store => case PersistStore of
            undefined -> undefined;
            _ -> format_store_ref(PersistStore)
        end
    }.

%% @doc 清空命名空间
-spec clear(store_manager(), namespace()) -> ok | {error, term()}.
clear(#store_manager{persistent_store = undefined, context_store = CtxStore}, Namespace) ->
    beamai_store:clear(CtxStore, Namespace);
clear(#store_manager{context_store = CtxStore, persistent_store = PersistStore}, Namespace) ->
    %% 清空两个 Store
    Res1 = beamai_store:clear(CtxStore, Namespace),
    Res2 = beamai_store:clear(PersistStore, Namespace),
    case {Res1, Res2} of
        {ok, ok} -> ok;
        {{error, _} = Error, _} -> Error;
        {_, {error, _} = Error} -> Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 格式化 Store 引用
-spec format_store_ref(beamai_store:store()) -> map().
format_store_ref({Module, Ref}) ->
    #{
        module => Module,
        ref => Ref,
        type => case Module of
            beamai_store_ets -> ets;
            beamai_store_sqlite -> sqlite;
            _ -> unknown
        end
    }.

%% @private 安全获取 map 值
-spec maps_get(atom(), map()) -> term().
maps_get(Key, Map) ->
    maps_get(Key, Map, undefined).

-spec maps_get(atom(), map(), term()) -> term().
maps_get(Key, Map, Default) ->
    maps:get(Key, Map, Default).
