%%%-------------------------------------------------------------------
%%% @doc Agent Store ETS 后端
%%%
%%% 使用 ETS 实现的 Store 存储后端。
%%% 适用于开发和单节点部署场景。
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 启动 Store 进程（可加入监督树）
%%% {ok, Pid} = beamai_store_ets:start_link(my_store, #{max_items => 10000}),
%%%
%%% %% 通过 beamai_store 统一接口操作
%%% Store = {beamai_store_ets, my_store},
%%% ok = beamai_store:put(Store, [<<"user">>, <<"123">>], <<"theme">>, #{value => <<"dark">>}),
%%% {ok, Item} = beamai_store:get(Store, [<<"user">>, <<"123">>], <<"theme">>).
%%% '''
%%%
%%% == 存储结构 ==
%%%
%%% 使用两个 ETS 表：
%%% - items_table: 存储数据项
%%%   Key: {namespace_tuple, key}
%%%   Value: #store_item{}
%%%
%%% - ns_index: 命名空间索引
%%%   Key: namespace_tuple
%%%   Value: [key]
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_store_ets).

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

%%====================================================================
%% 常量定义
%%====================================================================

%% 默认值在 beamai_store.hrl 中定义

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
    %% 数据项表
    items_table :: ets:tid(),
    %% 命名空间索引表
    ns_index :: ets:tid(),
    %% 配置
    max_items :: pos_integer(),
    max_namespaces :: pos_integer(),
    %% 统计
    item_count = 0 :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Store 进程（带注册名）
%%
%% 启动后通过 beamai_store 接口操作：
%% ```
%% {ok, _} = beamai_store_ets:start_link(my_store, #{max_items => 10000}),
%% Store = {beamai_store_ets, my_store},
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
    State = #state{
        items_table = ets:new(store_items, [set, private]),
        ns_index = ets:new(store_ns_idx, [set, private]),
        max_items = maps:get(max_items, Opts, ?DEFAULT_MAX_ITEMS),
        max_namespaces = maps:get(max_namespaces, Opts, ?DEFAULT_MAX_NAMESPACES)
    },
    {ok, State}.

%% @private 处理同步调用
handle_call({put, Namespace, Key, Value, Opts}, _From, State) ->
    case do_put(Namespace, Key, Value, Opts, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, _} = Error -> {reply, Error, State}
    end;

handle_call({get, Namespace, Key}, _From, State) ->
    Result = do_get(Namespace, Key, State),
    {reply, Result, State};

handle_call({delete, Namespace, Key}, _From, State) ->
    case do_delete(Namespace, Key, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, _} = Error -> {reply, Error, State}
    end;

handle_call({search, NamespacePrefix, Opts}, _From, State) ->
    Result = do_search(NamespacePrefix, Opts, State),
    {reply, Result, State};

handle_call({list_namespaces, Prefix, Opts}, _From, State) ->
    Result = do_list_namespaces(Prefix, Opts, State),
    {reply, Result, State};

handle_call({batch, Operations}, _From, State) ->
    case do_batch(Operations, State) of
        {ok, Results, NewState} -> {reply, {ok, Results}, NewState};
        {error, _} = Error -> {reply, Error, State}
    end;

handle_call(stats, _From, State) ->
    {reply, do_stats(State), State};

handle_call(clear, _From, State) ->
    {ok, NewState} = do_clear(State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private 处理异步消息
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private 处理其他消息
handle_info(_Info, State) ->
    {noreply, State}.

%% @private 终止
terminate(_Reason, #state{items_table = ItemsT, ns_index = NsIdx}) ->
    ets:delete(ItemsT),
    ets:delete(NsIdx),
    ok.

%%====================================================================
%% 内部函数 - 核心操作
%%====================================================================

%% @private 存储数据
-spec do_put(beamai_store:namespace(), beamai_store:key(), map(), map(), #state{}) ->
    {ok, #state{}} | {error, term()}.
do_put(Namespace, Key, Value, Opts, #state{items_table = Table, ns_index = NsIndex} = State) ->
    NsTuple = list_to_tuple(Namespace),
    ItemKey = {NsTuple, Key},
    Timestamp = erlang:system_time(millisecond),

    %% 检查是否是更新
    IsUpdate = ets:member(Table, ItemKey),

    %% 构建存储项
    Item = #store_item{
        namespace = Namespace,
        key = Key,
        value = Value,
        embedding = maps:get(embedding, Opts, undefined),
        created_at = case IsUpdate of
            true ->
                case ets:lookup(Table, ItemKey) of
                    [{_, OldItem}] -> OldItem#store_item.created_at;
                    [] -> Timestamp
                end;
            false -> Timestamp
        end,
        updated_at = Timestamp,
        metadata = maps:get(metadata, Opts, #{})
    },

    %% 检查是否超过限制（仅对新插入检查）
    NewState0 = case IsUpdate of
        true -> State;
        false ->
            %% 检查并淘汰
            CurrentSize = ets:info(Table, size),
            case CurrentSize >= State#state.max_items of
                true ->
                    evict_lru(Table, State#state.max_items);
                false ->
                    State
            end
    end,

    %% 存储数据项
    ets:insert(Table, {ItemKey, Item}),

    %% 更新命名空间索引
    update_ns_index(NsIndex, NsTuple, Key, IsUpdate),

    %% 更新计数
    NewState = case IsUpdate of
        true -> NewState0;
        false -> NewState0#state{item_count = NewState0#state.item_count + 1}
    end,

    {ok, NewState}.

%% @private 获取数据
-spec do_get(beamai_store:namespace(), beamai_store:key(), #state{}) ->
    {ok, #store_item{}} | {error, not_found}.
do_get(Namespace, Key, #state{items_table = Table}) ->
    NsTuple = list_to_tuple(Namespace),
    ItemKey = {NsTuple, Key},
    case ets:lookup(Table, ItemKey) of
        [{_, Item}] -> {ok, Item};
        [] -> {error, not_found}
    end.

%% @private 删除数据
-spec do_delete(beamai_store:namespace(), beamai_store:key(), #state{}) ->
    {ok, #state{}} | {error, not_found}.
do_delete(Namespace, Key, #state{items_table = Table, ns_index = NsIndex} = State) ->
    NsTuple = list_to_tuple(Namespace),
    ItemKey = {NsTuple, Key},
    case ets:member(Table, ItemKey) of
        true ->
            ets:delete(Table, ItemKey),
            remove_from_ns_index(NsIndex, NsTuple, Key),
            {ok, State#state{item_count = max(0, State#state.item_count - 1)}};
        false ->
            {error, not_found}
    end.

%% @private 搜索数据
-spec do_search(beamai_store:namespace(), map(), #state{}) ->
    {ok, [#search_result{}]} | {error, term()}.
do_search(NamespacePrefix, Opts, #state{items_table = Table}) ->
    PrefixTuple = list_to_tuple(NamespacePrefix),
    PrefixSize = tuple_size(PrefixTuple),
    Filter = maps:get(filter, Opts, undefined),
    Limit = maps:get(limit, Opts, ?DEFAULT_SEARCH_LIMIT),
    Offset = maps:get(offset, Opts, 0),

    %% 收集匹配的项
    MatchingItems = ets:foldl(fun({{NsTuple, _Key}, Item}, Acc) ->
        case is_prefix_match(PrefixTuple, PrefixSize, NsTuple) of
            true ->
                case apply_filter(Item, Filter) of
                    true -> [Item | Acc];
                    false -> Acc
                end;
            false ->
                Acc
        end
    end, [], Table),

    %% 排序（按更新时间降序）
    Sorted = lists:sort(fun(A, B) ->
        A#store_item.updated_at > B#store_item.updated_at
    end, MatchingItems),

    %% 分页
    Paginated = paginate(Sorted, Offset, Limit),

    %% 转换为搜索结果
    Results = [#search_result{item = Item, score = undefined} || Item <- Paginated],

    {ok, Results}.

%% @private 列出命名空间
-spec do_list_namespaces(beamai_store:namespace(), map(), #state{}) ->
    {ok, [beamai_store:namespace()]} | {error, term()}.
do_list_namespaces(Prefix, Opts, #state{ns_index = NsIndex}) ->
    PrefixTuple = list_to_tuple(Prefix),
    PrefixSize = tuple_size(PrefixTuple),
    Limit = maps:get(limit, Opts, ?DEFAULT_LIST_LIMIT),
    Offset = maps:get(offset, Opts, 0),

    %% 收集匹配的命名空间
    Namespaces = ets:foldl(fun({NsTuple, _Keys}, Acc) ->
        case is_prefix_match(PrefixTuple, PrefixSize, NsTuple) of
            true -> [tuple_to_list(NsTuple) | Acc];
            false -> Acc
        end
    end, [], NsIndex),

    %% 排序并分页
    Sorted = lists:sort(Namespaces),
    Paginated = paginate(Sorted, Offset, Limit),

    {ok, Paginated}.

%% @private 批量操作
-spec do_batch([beamai_store:batch_op()], #state{}) ->
    {ok, [term()], #state{}} | {error, term()}.
do_batch(Operations, State) ->
    batch_execute(Operations, State, []).

%% @private 获取统计信息
-spec do_stats(#state{}) -> map().
do_stats(#state{items_table = ItemsT, ns_index = NsIdx, item_count = Count}) ->
    #{
        item_count => Count,
        namespace_count => ets:info(NsIdx, size),
        memory => #{
            items => ets:info(ItemsT, memory),
            index => ets:info(NsIdx, memory)
        }
    }.

%% @private 清空所有数据
-spec do_clear(#state{}) -> {ok, #state{}}.
do_clear(#state{items_table = ItemsT, ns_index = NsIdx} = State) ->
    ets:delete_all_objects(ItemsT),
    ets:delete_all_objects(NsIdx),
    {ok, State#state{item_count = 0}}.

%%====================================================================
%% 内部函数 - 索引操作
%%====================================================================

%% @private 更新命名空间索引
-spec update_ns_index(ets:tid(), tuple(), beamai_store:key(), boolean()) -> ok.
update_ns_index(NsIndex, NsTuple, Key, IsUpdate) ->
    case IsUpdate of
        true ->
            %% 更新操作，键已存在，无需更新索引
            ok;
        false ->
            %% 新增操作，添加到索引
            case ets:lookup(NsIndex, NsTuple) of
                [{_, Keys}] ->
                    ets:insert(NsIndex, {NsTuple, [Key | Keys]});
                [] ->
                    ets:insert(NsIndex, {NsTuple, [Key]})
            end,
            ok
    end.

%% @private 从命名空间索引中移除
-spec remove_from_ns_index(ets:tid(), tuple(), beamai_store:key()) -> ok.
remove_from_ns_index(NsIndex, NsTuple, Key) ->
    case ets:lookup(NsIndex, NsTuple) of
        [{_, Keys}] ->
            NewKeys = lists:delete(Key, Keys),
            case NewKeys of
                [] -> ets:delete(NsIndex, NsTuple);
                _ -> ets:insert(NsIndex, {NsTuple, NewKeys})
            end;
        [] ->
            ok
    end,
    ok.

%%====================================================================
%% 内部函数 - 匹配和过滤
%%====================================================================

%% @private 检查前缀匹配
-spec is_prefix_match(tuple(), non_neg_integer(), tuple()) -> boolean().
is_prefix_match(_Prefix, 0, _NsTuple) ->
    true;
is_prefix_match(Prefix, PrefixSize, NsTuple) when tuple_size(NsTuple) >= PrefixSize ->
    check_prefix_elements(Prefix, NsTuple, PrefixSize, 1);
is_prefix_match(_, _, _) ->
    false.

%% @private 逐个检查前缀元素
-spec check_prefix_elements(tuple(), tuple(), non_neg_integer(), pos_integer()) -> boolean().
check_prefix_elements(_Prefix, _NsTuple, PrefixSize, Idx) when Idx > PrefixSize ->
    true;
check_prefix_elements(Prefix, NsTuple, PrefixSize, Idx) ->
    case element(Idx, Prefix) =:= element(Idx, NsTuple) of
        true -> check_prefix_elements(Prefix, NsTuple, PrefixSize, Idx + 1);
        false -> false
    end.

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
%% 内部函数 - 工具
%%====================================================================

%% @private 分页
-spec paginate([term()], non_neg_integer(), pos_integer()) -> [term()].
paginate(List, Offset, Limit) ->
    Dropped = safe_drop(List, Offset),
    lists:sublist(Dropped, Limit).

%% @private 安全跳过
-spec safe_drop([term()], non_neg_integer()) -> [term()].
safe_drop(List, N) when N >= length(List) -> [];
safe_drop(List, 0) -> List;
safe_drop(List, N) -> lists:nthtail(N, List).

%% @private 批量执行
-spec batch_execute([beamai_store:batch_op()], #state{}, [term()]) ->
    {ok, [term()], #state{}} | {error, term()}.
batch_execute([], State, Results) ->
    {ok, lists:reverse(Results), State};
batch_execute([Op | Rest], State, Results) ->
    case execute_op(Op, State) of
        {ok, Result, NewState} ->
            batch_execute(Rest, NewState, [Result | Results]);
        {error, _} = Error ->
            Error
    end.

%% @private 执行单个操作
-spec execute_op(beamai_store:batch_op(), #state{}) ->
    {ok, term(), #state{}} | {error, term()}.
execute_op({put, Ns, Key, Value}, State) ->
    case do_put(Ns, Key, Value, #{}, State) of
        {ok, NewState} -> {ok, ok, NewState};
        Error -> Error
    end;
execute_op({put, Ns, Key, Value, Opts}, State) ->
    case do_put(Ns, Key, Value, Opts, State) of
        {ok, NewState} -> {ok, ok, NewState};
        Error -> Error
    end;
execute_op({get, Ns, Key}, State) ->
    Result = do_get(Ns, Key, State),
    {ok, Result, State};
execute_op({delete, Ns, Key}, State) ->
    case do_delete(Ns, Key, State) of
        {ok, NewState} -> {ok, ok, NewState};
        {error, not_found} -> {ok, {error, not_found}, State};
        Error -> Error
    end.

%% @private 淘汰最旧的条目（LRU 策略）
%%
%% 当存储空间不足时，删除最旧的条目以释放空间。
-spec evict_lru(ets:tid(), pos_integer()) -> ok.
evict_lru(Table, MaxItems) ->
    %% 收集所有条目并按访问时间排序
    AllItems = ets:foldl(fun({{NsTuple, Key}, #store_item{updated_at = UpdatedAt}}, Acc) ->
        [{{UpdatedAt, NsTuple, Key}} | Acc]
    end, [], Table),

    %% 按更新时间排序（最旧的在前）
    SortedByTime = lists:sort(fun({Time1, _}, {Time2, _}) ->
        Time1 =< Time2
    end, AllItems),

    %% 删除最旧的条目（约 10% 或至少 1 个）
    ToEvict = max(1, MaxItems div 10),
    ToDelete = lists:sublist(SortedByTime, ToEvict),

    lists:foreach(fun({_UpdatedAt, NsTuple, Key}) ->
        ets:delete(Table, {NsTuple, Key})
    end, ToDelete),

    ok.
