%%%-------------------------------------------------------------------
%%% @doc Agent Store - 统一存储接口
%%%
%%% Store 作为纯代理层，统一封装对后端存储进程的调用。
%%% 所有后端（ETS、SQLite、Redis 等）都必须实现 gen_server 接口。
%%%
%%% == 设计理念 ==
%%%
%%% - 进程模式：所有 Store 后端都是 gen_server 进程
%%% - 统一接口：本模块通过 gen_server:call 代理所有操作
%%% - 职责分离：Store 创建由外部负责，本模块只负责调用
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 1. 外部创建 Store 进程（可加入监督树）
%%% {ok, _} = beamai_store_ets:start_link(my_store, #{max_items => 10000}),
%%%
%%% %% 2. 构建 Store 引用
%%% Store = {beamai_store_ets, my_store},
%%%
%%% %% 3. 使用统一接口操作
%%% ok = beamai_store:put(Store, [<<"user">>, <<"123">>], <<"theme">>, #{value => <<"dark">>}),
%%% {ok, Item} = beamai_store:get(Store, [<<"user">>, <<"123">>], <<"theme">>),
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_store).

-include_lib("beamai_memory/include/beamai_store.hrl").

%% 类型导出
-export_type([
    store/0,
    namespace/0,
    key/0,
    item/0,
    search_result/0,
    put_opts/0,
    search_opts/0,
    list_opts/0
]).

%% 核心 CRUD API
-export([
    put/4,
    put/5,
    get/3,
    delete/3
]).

%% 搜索和列表 API
-export([
    search/3,
    list_namespaces/2,
    list_namespaces/3
]).

%% 批量操作 API
-export([
    batch/2
]).

%% 管理 API
-export([
    stats/1,
    clear/1
]).

%% 工具函数
-export([
    namespace_to_string/1,
    string_to_namespace/1,
    is_prefix_of/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

%% Store 引用：{后端模块, 进程引用}
-type store() :: {Module :: module(), Ref :: pid() | atom()}.

%% 命名空间 - 层级化路径
-type namespace() :: [binary()].

%% 键名
-type key() :: binary().

%% 存储项
-type item() :: #store_item{}.

%% 搜索结果
-type search_result() :: #search_result{}.

%% Put 选项
-type put_opts() :: #{
    %% 是否创建向量索引
    index => boolean() | [binary()],
    %% 自定义元数据
    metadata => map(),
    %% 预计算的嵌入向量
    embedding => [float()]
}.

%% Search 选项
-type search_opts() :: #{
    %% 语义搜索查询
    query => binary(),
    %% 过滤条件
    filter => map(),
    %% 返回数量限制
    limit => pos_integer(),
    %% 偏移量
    offset => non_neg_integer()
}.

%% List 选项
-type list_opts() :: #{
    %% 返回数量限制
    limit => pos_integer(),
    %% 偏移量
    offset => non_neg_integer()
}.

%% 批量操作
-type batch_op() ::
    {put, namespace(), key(), map()} |
    {put, namespace(), key(), map(), put_opts()} |
    {get, namespace(), key()} |
    {delete, namespace(), key()}.

%%====================================================================
%% 核心 CRUD API
%%====================================================================

%% @doc 存储数据
%%
%% 将值存储到指定的命名空间和键下。
-spec put(store(), namespace(), key(), map()) -> ok | {error, term()}.
put(Store, Namespace, Key, Value) ->
    put(Store, Namespace, Key, Value, #{}).

%% @doc 存储数据（带选项）
-spec put(store(), namespace(), key(), map(), put_opts()) -> ok | {error, term()}.
put({_Module, Ref}, Namespace, Key, Value, Opts) ->
    case validate_namespace(Namespace) of
        ok ->
            case validate_key(Key) of
                ok ->
                    gen_server:call(Ref, {put, Namespace, Key, Value, Opts});
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 获取数据
%%
%% 从指定的命名空间和键获取值。
-spec get(store(), namespace(), key()) -> {ok, item()} | {error, not_found | term()}.
get({_Module, Ref}, Namespace, Key) ->
    gen_server:call(Ref, {get, Namespace, Key}).

%% @doc 删除数据
-spec delete(store(), namespace(), key()) -> ok | {error, term()}.
delete({_Module, Ref}, Namespace, Key) ->
    gen_server:call(Ref, {delete, Namespace, Key}).

%%====================================================================
%% 搜索和列表 API
%%====================================================================

%% @doc 搜索数据
%%
%% 在指定命名空间前缀下搜索数据。
%% 支持：
%% - 精确过滤（通过 filter 选项）
%% - 语义搜索（通过 query 选项，需要后端支持）
-spec search(store(), namespace(), search_opts()) ->
    {ok, [search_result()]} | {error, term()}.
search({_Module, Ref}, NamespacePrefix, Opts) ->
    NormalizedOpts = normalize_search_opts(Opts),
    gen_server:call(Ref, {search, NamespacePrefix, NormalizedOpts}).

%% @doc 列出命名空间下的所有子命名空间
-spec list_namespaces(store(), namespace()) ->
    {ok, [namespace()]} | {error, term()}.
list_namespaces(Store, Prefix) ->
    list_namespaces(Store, Prefix, #{}).

%% @doc 列出命名空间（带选项）
-spec list_namespaces(store(), namespace(), list_opts()) ->
    {ok, [namespace()]} | {error, term()}.
list_namespaces({_Module, Ref}, Prefix, Opts) ->
    NormalizedOpts = normalize_list_opts(Opts),
    gen_server:call(Ref, {list_namespaces, Prefix, NormalizedOpts}).

%%====================================================================
%% 批量操作 API
%%====================================================================

%% @doc 批量执行操作
%%
%% 执行多个操作，返回每个操作的结果。
-spec batch(store(), [batch_op()]) -> {ok, [term()]} | {error, term()}.
batch({_Module, Ref}, Operations) ->
    gen_server:call(Ref, {batch, Operations}).

%%====================================================================
%% 管理 API
%%====================================================================

%% @doc 获取存储统计信息
%%
%% 返回后端存储的统计信息，如项目数、命名空间数等。
-spec stats(store()) -> map().
stats({_Module, Ref}) ->
    gen_server:call(Ref, stats).

%% @doc 清空存储
%%
%% 删除所有存储的数据。
-spec clear(store()) -> ok | {error, term()}.
clear({_Module, Ref}) ->
    gen_server:call(Ref, clear).

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 将命名空间转换为字符串
%%
%% 例如: [<<"user">>, <<"123">>] -> <<"user/123">>
-spec namespace_to_string(namespace()) -> binary().
namespace_to_string([]) ->
    <<>>;
namespace_to_string(Namespace) ->
    iolist_to_binary(lists:join(?NS_SEPARATOR, Namespace)).

%% @doc 将字符串转换为命名空间
%%
%% 例如: <<"user/123">> -> [<<"user">>, <<"123">>]
-spec string_to_namespace(binary()) -> namespace().
string_to_namespace(<<>>) ->
    [];
string_to_namespace(String) ->
    binary:split(String, ?NS_SEPARATOR, [global]).

%% @doc 检查 Prefix 是否是 Namespace 的前缀
-spec is_prefix_of(namespace(), namespace()) -> boolean().
is_prefix_of([], _Namespace) ->
    true;
is_prefix_of(_Prefix, []) ->
    false;
is_prefix_of([H | T1], [H | T2]) ->
    is_prefix_of(T1, T2);
is_prefix_of(_, _) ->
    false.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 验证命名空间
-spec validate_namespace(namespace()) -> ok | {error, invalid_namespace}.
validate_namespace(Namespace) when is_list(Namespace) ->
    case lists:all(fun is_binary/1, Namespace) of
        true -> ok;
        false -> {error, invalid_namespace}
    end;
validate_namespace(_) ->
    {error, invalid_namespace}.

%% @private 验证键
-spec validate_key(key()) -> ok | {error, invalid_key}.
validate_key(Key) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_key(_) ->
    {error, invalid_key}.

%% @private 规范化搜索选项
-spec normalize_search_opts(search_opts()) -> search_opts().
normalize_search_opts(Opts) ->
    #{
        query => maps:get(query, Opts, undefined),
        filter => maps:get(filter, Opts, undefined),
        limit => maps:get(limit, Opts, ?DEFAULT_SEARCH_LIMIT),
        offset => maps:get(offset, Opts, 0)
    }.

%% @private 规范化列表选项
-spec normalize_list_opts(list_opts()) -> list_opts().
normalize_list_opts(Opts) ->
    #{
        limit => maps:get(limit, Opts, ?DEFAULT_LIST_LIMIT),
        offset => maps:get(offset, Opts, 0)
    }.
