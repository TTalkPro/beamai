%%%-------------------------------------------------------------------
%%% @doc Agent Store SQLite 后端
%%%
%%% 使用 SQLite 实现的 Store 持久化存储后端。
%%% 适用于单机部署和需要持久化的场景。
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 启动 Store 进程（可加入监督树）
%%% {ok, Pid} = beamai_store_sqlite:start_link(my_sqlite, #{db_path => "/path/to/db.sqlite"}),
%%%
%%% %% 通过 beamai_store 统一接口操作
%%% Store = {beamai_store_sqlite, my_sqlite},
%%% ok = beamai_store:put(Store, [<<"user">>, <<"123">>], <<"theme">>, #{value => <<"dark">>}),
%%% {ok, Item} = beamai_store:get(Store, [<<"user">>, <<"123">>], <<"theme">>).
%%% '''
%%%
%%% == 表结构 ==
%%%
%%% ```sql
%%% CREATE TABLE store_items (
%%%     full_key   TEXT PRIMARY KEY,   -- namespace + key 组合键
%%%     namespace  TEXT NOT NULL,      -- 命名空间（如 "user/123/prefs"）
%%%     key        TEXT NOT NULL,      -- 条目键
%%%     value      TEXT NOT NULL,      -- JSON 序列化的值
%%%     embedding  TEXT,               -- JSON 序列化的向量（可选）
%%%     metadata   TEXT,               -- JSON 序列化的元数据（可选）
%%%     created_at INTEGER NOT NULL,   -- 创建时间戳
%%%     updated_at INTEGER NOT NULL    -- 更新时间戳
%%% );
%%% '''
%%%
%%% == 配置选项 ==
%%%
%%% - db_path: 数据库文件路径（必需）
%%% - busy_timeout: 忙等待超时，毫秒（默认 5000）
%%% - cache_size: 页面缓存大小（默认 2000）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_store_sqlite).

-behaviour(gen_server).

-include_lib("beamai_memory/include/beamai_store.hrl").

%% API
-export([
    start_link/1,
    start_link/2,
    stop/1,
    stats/1,
    clear/1
]).

%% gen_server 回调
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%%====================================================================
%% 内部状态
%%====================================================================

-record(state, {
    %% 数据库连接
    db :: esqlite3:connection(),
    %% 数据库路径
    db_path :: string(),
    %% 配置
    busy_timeout :: pos_integer()
}).

%%====================================================================
%% 表定义 SQL
%%====================================================================

-define(CREATE_TABLE_SQL, "
    CREATE TABLE IF NOT EXISTS store_items (
        full_key   TEXT PRIMARY KEY,
        namespace  TEXT NOT NULL,
        key        TEXT NOT NULL,
        value      TEXT NOT NULL,
        embedding  TEXT,
        metadata   TEXT,
        created_at INTEGER NOT NULL,
        updated_at INTEGER NOT NULL
    )
").

-define(CREATE_INDEX_NS_SQL, "
    CREATE INDEX IF NOT EXISTS idx_store_namespace ON store_items(namespace)
").

-define(CREATE_INDEX_UPDATED_SQL, "
    CREATE INDEX IF NOT EXISTS idx_store_updated_at ON store_items(updated_at)
").

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Store 进程（带注册名）
%%
%% 启动后通过 beamai_store 接口操作：
%% ```
%% {ok, _} = beamai_store_sqlite:start_link(my_sqlite, #{db_path => "/tmp/store.db"}),
%% Store = {beamai_store_sqlite, my_sqlite},
%% ok = beamai_store:put(Store, Namespace, Key, Value).
%% '''
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(Name, Opts) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Opts, []).

%% @doc 启动 Store 进程（无注册名）
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc 停止 Store 进程
-spec stop(pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%% @doc 获取统计信息
-spec stats(pid() | atom()) -> map().
stats(Ref) ->
    gen_server:call(Ref, stats).

%% @doc 清空所有数据
-spec clear(pid() | atom()) -> ok.
clear(Ref) ->
    gen_server:call(Ref, clear).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @private 初始化
init(Opts) ->
    DbPath = maps:get(db_path, Opts),
    BusyTimeout = maps:get(busy_timeout, Opts, 5000),
    CacheSize = maps:get(cache_size, Opts, 2000),

    case esqlite3:open(DbPath) of
        {ok, Db} ->
            ok = configure_sqlite(Db, BusyTimeout, CacheSize),
            ok = create_schema(Db),
            State = #state{
                db = Db,
                db_path = DbPath,
                busy_timeout = BusyTimeout
            },
            {ok, State};
        {error, Reason} ->
            {stop, {open_failed, Reason}}
    end.

%% @private 处理同步调用
handle_call({put, Namespace, Key, Value, Opts}, _From, State) ->
    case do_put(Namespace, Key, Value, Opts, State) of
        ok -> {reply, ok, State};
        {error, _} = Error -> {reply, Error, State}
    end;

handle_call({get, Namespace, Key}, _From, State) ->
    Result = do_get(Namespace, Key, State),
    {reply, Result, State};

handle_call({delete, Namespace, Key}, _From, State) ->
    Result = do_delete(Namespace, Key, State),
    {reply, Result, State};

handle_call({search, NamespacePrefix, Opts}, _From, State) ->
    Result = do_search(NamespacePrefix, Opts, State),
    {reply, Result, State};

handle_call({list_namespaces, Prefix, Opts}, _From, State) ->
    Result = do_list_namespaces(Prefix, Opts, State),
    {reply, Result, State};

handle_call({batch, Operations}, _From, State) ->
    Result = do_batch(Operations, State),
    {reply, Result, State};

handle_call(stats, _From, State) ->
    {reply, do_stats(State), State};

handle_call(clear, _From, State) ->
    case do_clear(State) of
        ok -> {reply, ok, State};
        {error, _} = Error -> {reply, Error, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private 处理异步消息
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private 处理其他消息
handle_info(_Info, State) ->
    {noreply, State}.

%% @private 终止
terminate(_Reason, #state{db = Db}) ->
    esqlite3:close(Db),
    ok.

%%====================================================================
%% 内部函数 - 核心操作
%%====================================================================

%% @private 存储数据
-spec do_put(beamai_store:namespace(), beamai_store:key(), map(), map(), #state{}) ->
    ok | {error, term()}.
do_put(Namespace, Key, Value, Opts, #state{db = Db}) ->
    FullKey = make_full_key(Namespace, Key),
    NsString = namespace_to_string(Namespace),
    Timestamp = erlang:system_time(millisecond),

    %% 序列化
    ValueJson = jsx:encode(Value),
    EmbeddingJson = case maps:get(embedding, Opts, undefined) of
        undefined -> null;
        Emb -> jsx:encode(Emb)
    end,
    MetadataJson = case maps:get(metadata, Opts, #{}) of
        M when map_size(M) =:= 0 -> <<"{}">>;
        M -> jsx:encode(M)
    end,

    %% 获取 created_at（如果已存在）
    CreatedAt = case get_created_at(Db, FullKey) of
        {ok, ExistingCreatedAt} -> ExistingCreatedAt;
        {error, not_found} -> Timestamp
    end,

    %% 插入或更新（UPSERT）
    Sql = "INSERT OR REPLACE INTO store_items
           (full_key, namespace, key, value, embedding, metadata, created_at, updated_at)
           VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",

    case esqlite3:q(Db, Sql, [FullKey, NsString, Key, ValueJson,
                              EmbeddingJson, MetadataJson, CreatedAt, Timestamp]) of
        [] -> ok;
        {error, Reason} -> {error, {put_failed, Reason}}
    end.

%% @private 获取数据
-spec do_get(beamai_store:namespace(), beamai_store:key(), #state{}) ->
    {ok, #store_item{}} | {error, not_found | term()}.
do_get(Namespace, Key, #state{db = Db}) ->
    FullKey = make_full_key(Namespace, Key),
    Sql = "SELECT namespace, key, value, embedding, metadata, created_at, updated_at
           FROM store_items WHERE full_key = ?1",

    case esqlite3:q(Db, Sql, [FullKey]) of
        [Row] when is_list(Row) -> row_to_item(Row);
        [] -> {error, not_found};
        {error, Reason} -> {error, {get_failed, Reason}}
    end.

%% @private 删除数据
-spec do_delete(beamai_store:namespace(), beamai_store:key(), #state{}) ->
    ok | {error, not_found | term()}.
do_delete(Namespace, Key, #state{db = Db}) ->
    FullKey = make_full_key(Namespace, Key),
    Sql = "DELETE FROM store_items WHERE full_key = ?1",

    case esqlite3:q(Db, Sql, [FullKey]) of
        [] ->
            case esqlite3:changes(Db) of
                0 -> {error, not_found};
                _ -> ok
            end;
        {error, Reason} -> {error, {delete_failed, Reason}}
    end.

%% @private 搜索数据
-spec do_search(beamai_store:namespace(), map(), #state{}) ->
    {ok, [#search_result{}]} | {error, term()}.
do_search(NamespacePrefix, Opts, #state{db = Db}) ->
    NsPrefixString = namespace_to_string(NamespacePrefix),
    Filter = maps:get(filter, Opts, undefined),
    Limit = maps:get(limit, Opts, ?DEFAULT_SEARCH_LIMIT),
    Offset = maps:get(offset, Opts, 0),

    {Sql, Params} = build_search_query(NsPrefixString, Limit, Offset),

    case esqlite3:q(Db, Sql, Params) of
        Rows when is_list(Rows) ->
            Results = lists:filtermap(fun(Row) ->
                case row_to_item(Row) of
                    {ok, Item} ->
                        case apply_filter(Item, Filter) of
                            true -> {true, #search_result{item = Item, score = undefined}};
                            false -> false
                        end;
                    _ -> false
                end
            end, Rows),
            {ok, Results};
        {error, Reason} -> {error, {search_failed, Reason}}
    end.

%% @private 列出命名空间
-spec do_list_namespaces(beamai_store:namespace(), map(), #state{}) ->
    {ok, [beamai_store:namespace()]} | {error, term()}.
do_list_namespaces(Prefix, Opts, #state{db = Db}) ->
    PrefixString = namespace_to_string(Prefix),
    Limit = maps:get(limit, Opts, ?DEFAULT_LIST_LIMIT),
    Offset = maps:get(offset, Opts, 0),

    {Sql, Params} = case PrefixString of
        <<>> ->
            {"SELECT DISTINCT namespace FROM store_items
              ORDER BY namespace LIMIT ?1 OFFSET ?2", [Limit, Offset]};
        _ ->
            {"SELECT DISTINCT namespace FROM store_items
              WHERE namespace LIKE ?1 || '%'
              ORDER BY namespace LIMIT ?2 OFFSET ?3", [PrefixString, Limit, Offset]}
    end,

    case esqlite3:q(Db, Sql, Params) of
        Rows when is_list(Rows) ->
            Namespaces = [string_to_namespace(extract_ns(Row)) || Row <- Rows],
            {ok, Namespaces};
        {error, Reason} -> {error, {list_namespaces_failed, Reason}}
    end.

%% @private 批量操作
-spec do_batch([beamai_store:batch_op()], #state{}) ->
    {ok, [term()]} | {error, term()}.
do_batch(Operations, #state{db = Db} = State) ->
    case esqlite3:q(Db, "BEGIN TRANSACTION", []) of
        [] ->
            case batch_execute(Operations, State, []) of
                {ok, Results} ->
                    case esqlite3:q(Db, "COMMIT", []) of
                        [] -> {ok, Results};
                        {error, Reason} -> {error, {commit_failed, Reason}}
                    end;
                {error, Reason} ->
                    _ = esqlite3:q(Db, "ROLLBACK", []),
                    {error, Reason}
            end;
        {error, Reason} -> {error, {begin_failed, Reason}}
    end.

%% @private 获取统计信息
-spec do_stats(#state{}) -> map().
do_stats(#state{db = Db, db_path = DbPath}) ->
    ItemCount = case esqlite3:q(Db, "SELECT COUNT(*) FROM store_items", []) of
        [[Count]] -> Count;
        [{Count}] -> Count;
        _ -> 0
    end,
    NsCount = case esqlite3:q(Db, "SELECT COUNT(DISTINCT namespace) FROM store_items", []) of
        [[Count2]] -> Count2;
        [{Count2}] -> Count2;
        _ -> 0
    end,
    #{
        item_count => ItemCount,
        namespace_count => NsCount,
        db_path => list_to_binary(DbPath)
    }.

%% @private 清空所有数据
-spec do_clear(#state{}) -> ok | {error, term()}.
do_clear(#state{db = Db}) ->
    case esqlite3:q(Db, "DELETE FROM store_items", []) of
        [] -> ok;
        {error, Reason} -> {error, {clear_failed, Reason}}
    end.

%%====================================================================
%% 内部函数 - 初始化
%%====================================================================

%% @private 配置 SQLite
-spec configure_sqlite(esqlite3:connection(), pos_integer(), pos_integer()) -> ok.
configure_sqlite(Db, BusyTimeout, CacheSize) ->
    _ = esqlite3:q(Db, "PRAGMA journal_mode = WAL", []),
    _ = esqlite3:q(Db, "PRAGMA synchronous = NORMAL", []),
    BusyTimeoutSql = io_lib:format("PRAGMA busy_timeout = ~B", [BusyTimeout]),
    _ = esqlite3:q(Db, lists:flatten(BusyTimeoutSql), []),
    CacheSizeSql = io_lib:format("PRAGMA cache_size = ~B", [CacheSize]),
    _ = esqlite3:q(Db, lists:flatten(CacheSizeSql), []),
    _ = esqlite3:q(Db, "PRAGMA foreign_keys = ON", []),
    ok.

%% @private 创建表和索引
-spec create_schema(esqlite3:connection()) -> ok.
create_schema(Db) ->
    ok = exec_sql(Db, ?CREATE_TABLE_SQL),
    ok = exec_sql(Db, ?CREATE_INDEX_NS_SQL),
    ok = exec_sql(Db, ?CREATE_INDEX_UPDATED_SQL),
    ok.

%% @private 执行 SQL
-spec exec_sql(esqlite3:connection(), string()) -> ok | {error, term()}.
exec_sql(Db, Sql) ->
    case esqlite3:q(Db, Sql, []) of
        ok -> ok;
        [] -> ok;
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% 内部函数 - 键和命名空间
%%====================================================================

%% @private 生成完整键
-spec make_full_key(beamai_store:namespace(), beamai_store:key()) -> binary().
make_full_key(Namespace, Key) ->
    NsString = namespace_to_string(Namespace),
    <<NsString/binary, ":", Key/binary>>.

%% @private 命名空间转字符串
-spec namespace_to_string(beamai_store:namespace()) -> binary().
namespace_to_string([]) ->
    <<>>;
namespace_to_string(Namespace) ->
    iolist_to_binary(lists:join(<<"/">>, Namespace)).

%% @private 字符串转命名空间
-spec string_to_namespace(binary()) -> beamai_store:namespace().
string_to_namespace(<<>>) ->
    [];
string_to_namespace(String) ->
    binary:split(String, <<"/">>, [global]).

%% @private 从行中提取命名空间
extract_ns([Ns]) -> Ns;
extract_ns({Ns}) -> Ns;
extract_ns(Ns) when is_binary(Ns) -> Ns.

%%====================================================================
%% 内部函数 - 查询
%%====================================================================

%% @private 获取 created_at
-spec get_created_at(esqlite3:connection(), binary()) ->
    {ok, integer()} | {error, not_found}.
get_created_at(Db, FullKey) ->
    Sql = "SELECT created_at FROM store_items WHERE full_key = ?1",
    case esqlite3:q(Db, Sql, [FullKey]) of
        [[CreatedAt]] -> {ok, CreatedAt};
        [{CreatedAt}] -> {ok, CreatedAt};
        [] -> {error, not_found};
        _ -> {error, not_found}
    end.

%% @private 构建搜索查询
-spec build_search_query(binary(), pos_integer(), non_neg_integer()) ->
    {string(), list()}.
build_search_query(NsPrefixString, Limit, Offset) ->
    BaseSql = "SELECT namespace, key, value, embedding, metadata, created_at, updated_at
               FROM store_items",

    {WhereSql, Params} = case NsPrefixString of
        <<>> ->
            {"", []};
        _ ->
            {" WHERE namespace LIKE ?1 || '%'", [NsPrefixString]}
    end,

    OrderSql = " ORDER BY updated_at DESC",
    LimitSql = case Params of
        [] -> " LIMIT ?1 OFFSET ?2";
        _ -> " LIMIT ?2 OFFSET ?3"
    end,

    FullSql = BaseSql ++ WhereSql ++ OrderSql ++ LimitSql,
    AllParams = Params ++ [Limit, Offset],

    {FullSql, AllParams}.

%%====================================================================
%% 内部函数 - 序列化/反序列化
%%====================================================================

%% @private 行转 Item
-spec row_to_item(list() | tuple()) -> {ok, #store_item{}} | {error, term()}.
row_to_item([NsBin, KeyBin, ValueJson, EmbeddingJson, MetadataJson, CreatedAt, UpdatedAt]) ->
    row_to_item({NsBin, KeyBin, ValueJson, EmbeddingJson, MetadataJson, CreatedAt, UpdatedAt});
row_to_item({NsBin, KeyBin, ValueJson, EmbeddingJson, MetadataJson, CreatedAt, UpdatedAt}) ->
    try
        Item = #store_item{
            namespace = string_to_namespace(NsBin),
            key = KeyBin,
            value = jsx:decode(ValueJson, [return_maps]),
            embedding = decode_embedding(EmbeddingJson),
            metadata = decode_metadata(MetadataJson),
            created_at = CreatedAt,
            updated_at = UpdatedAt
        },
        {ok, Item}
    catch
        _:_ -> {error, decode_failed}
    end.

%% @private 解码嵌入向量
-spec decode_embedding(term()) -> [float()] | undefined.
decode_embedding(undefined) -> undefined;
decode_embedding(null) -> undefined;
decode_embedding(<<"null">>) -> undefined;
decode_embedding(<<>>) -> undefined;
decode_embedding(Json) when is_binary(Json) ->
    try
        case jsx:decode(Json, [return_maps]) of
            List when is_list(List) -> List;
            _ -> undefined
        end
    catch
        _:_ -> undefined
    end;
decode_embedding(_) -> undefined.

%% @private 解码元数据
-spec decode_metadata(term()) -> map().
decode_metadata(undefined) -> #{};
decode_metadata(null) -> #{};
decode_metadata(<<"null">>) -> #{};
decode_metadata(<<>>) -> #{};
decode_metadata(<<"{}">>) -> #{};
decode_metadata(Json) when is_binary(Json) ->
    try
        case jsx:decode(Json, [return_maps]) of
            Map when is_map(Map) -> Map;
            _ -> #{}
        end
    catch
        _:_ -> #{}
    end;
decode_metadata(_) -> #{}.

%%====================================================================
%% 内部函数 - 过滤
%%====================================================================

%% @private 应用过滤条件
-spec apply_filter(#store_item{}, map() | undefined) -> boolean().
apply_filter(_Item, undefined) ->
    true;
apply_filter(#store_item{value = Value, metadata = Meta}, Filter) when is_map(Filter) ->
    maps:fold(fun(Key, ExpectedValue, Acc) ->
        Acc andalso check_filter_key(Key, ExpectedValue, Value, Meta)
    end, true, Filter).

%% @private 检查单个过滤键
-spec check_filter_key(binary(), term(), map(), map()) -> boolean().
check_filter_key(Key, ExpectedValue, Value, Meta) ->
    case maps:get(Key, Value, undefined) of
        ExpectedValue -> true;
        undefined ->
            case maps:get(Key, Meta, undefined) of
                ExpectedValue -> true;
                _ -> false
            end;
        _ -> false
    end.

%%====================================================================
%% 内部函数 - 批量操作
%%====================================================================

%% @private 批量执行
-spec batch_execute([beamai_store:batch_op()], #state{}, [term()]) ->
    {ok, [term()]} | {error, term()}.
batch_execute([], _State, Results) ->
    {ok, lists:reverse(Results)};
batch_execute([Op | Rest], State, Results) ->
    case execute_op(Op, State) of
        {ok, Result} ->
            batch_execute(Rest, State, [Result | Results]);
        {error, _} = Error ->
            Error
    end.

%% @private 执行单个操作
-spec execute_op(beamai_store:batch_op(), #state{}) ->
    {ok, term()} | {error, term()}.
execute_op({put, Ns, Key, Value}, State) ->
    case do_put(Ns, Key, Value, #{}, State) of
        ok -> {ok, ok};
        Error -> Error
    end;
execute_op({put, Ns, Key, Value, Opts}, State) ->
    case do_put(Ns, Key, Value, Opts, State) of
        ok -> {ok, ok};
        Error -> Error
    end;
execute_op({get, Ns, Key}, State) ->
    Result = do_get(Ns, Key, State),
    {ok, Result};
execute_op({delete, Ns, Key}, State) ->
    case do_delete(Ns, Key, State) of
        ok -> {ok, ok};
        {error, not_found} -> {ok, {error, not_found}};
        Error -> Error
    end.
