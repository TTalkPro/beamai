%%%-------------------------------------------------------------------
%%% @doc Checkpointer 管理器
%%%
%%% 管理检查点，支持时间旅行和分支功能。
%%%
%%% == 功能 ==
%%%
%%% - 使用 Store 后端进行持久化
%%% - 支持分支管理
%%% - 支持时间旅行（回退/前进/跳转）
%%% - 线程隔离（不同 thread_id 独立管理）
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建管理器
%%% {ok, Store} = beamai_store_ets:start_link(my_store, #{}),
%%% Manager = beamai_checkpoint_manager:new(Store),
%%%
%%% %% 保存检查点
%%% Config = #{thread_id => <<"thread-1">>},
%%% Checkpoint = #checkpoint{
%%%     id = <<"cp-1">>,
%%%     thread_id = <<"thread-1">>,
%%%     values = #{messages => []}
%%% },
%%% {ok, CpId} = beamai_checkpoint_manager:save(Manager, Checkpoint, Config).
%%%
%%% %% 加载检查点
%%% {ok, {Cp, Meta, ParentCfg}} = beamai_checkpoint_manager:load(Manager, latest, Config).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_checkpoint_manager).

-include_lib("beamai_memory/include/beamai_checkpointer.hrl").
-include_lib("beamai_memory/include/beamai_store.hrl").

%% 构造函数
-export([new/1, new/2]).

%% 基本操作
-export([save/3, load/3, delete/2, list/2, count/1]).

%% 时间旅行
-export([go_back/3, go_forward/3, goto/3]).

%% 分支管理
-export([branch/4, switch_branch/3, list_branches/1, get_lineage/2, diff/3]).

%% 工具函数
-export([get_current_branch/1, get_store/1]).

%% 清理和统计
-export([prune_checkpoints/3, prune_branch_checkpoints/3, get_checkpoint_stats/1]).
-export([get_checkpoint_count/2, get_total_count/1]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(manager, {
    store :: beamai_store:store(),
    current_branch :: binary(),
    branches :: #{binary() => branch_info()},
    namespace_prefix :: [binary()],
    max_checkpoints :: pos_integer() | infinity,
    auto_prune :: boolean()
}).

-type manager() :: #manager{}.

%% 基本类型别名
-type checkpoint() :: #checkpoint{}.
-type checkpoint_metadata() :: #checkpoint_metadata{}.
-type config() :: map().
-type checkpoint_id() :: binary().
-type thread_id() :: binary().
-type checkpoint_tuple() :: {checkpoint(), checkpoint_metadata(), config() | undefined}.

-type branch_info() :: #{
    branch_id := binary(),
    head_checkpoint_id => binary() | undefined,
    checkpoint_count => non_neg_integer(),
    created_at => integer()
}.

-export_type([manager/0, branch_info/0]).

%%====================================================================
%% 常量定义
%%====================================================================

-define(NS_CHECKPOINTS, <<"checkpoints">>).
-define(NS_INDEX, <<"_index">>).
-define(KEY_LATEST, <<"latest">>).
-define(KEY_INDEX, <<"index">>).

%%====================================================================
%% 常量定义 - 默认配置
%%====================================================================

%% 默认检查点数量限制（优化内存使用）
%% -define(DEFAULT_MAX_CHECKPOINTS, 50). %% Defined in beamai_checkpointer.hrl

%% 默认 ETS 存储容量
-define(DEFAULT_ETS_MAX_ITEMS, 1000).
-define(DEFAULT_BRANCH, <<"main">>).

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 Checkpointer 管理器
-spec new(beamai_store:store()) -> manager().
new(Store) ->
    new(Store, #{}).

%% @doc 创建 Checkpointer 管理器（带选项）
%%
%% Opts:
%% - initial_branch => binary() - 初始分支名称（默认 "main"）
%% - namespace_prefix => [binary()] - 命名空间前缀（默认 [<<"checkpoints">>]）
%% - max_checkpoints => pos_integer() | infinity - 最大检查点数量（默认 infinity）
%% - auto_prune => boolean() - 超过限制时自动清理（默认 true）
%%
%% 示例:
%% ```
%% Manager = beamai_checkpoint_manager:new(Store, #{
%%     max_checkpoints => 100,
%%     auto_prune => true
%% }).
%% '''
-spec new(beamai_store:store(), map()) -> manager().
new(Store, Opts) ->
    InitialBranch = maps:get(initial_branch, Opts, ?DEFAULT_BRANCH),
    NamespacePrefix = maps:get(namespace_prefix, Opts, [?NS_CHECKPOINTS]),
    MaxCheckpoints = maps:get(max_checkpoints, Opts, ?DEFAULT_MAX_CHECKPOINTS),
    AutoPrune = maps:get(auto_prune, Opts, true),

    #manager{
        store = Store,
        current_branch = InitialBranch,
        branches = #{InitialBranch => #{
            branch_id => InitialBranch,
            checkpoint_count => 0,
            created_at => erlang:system_time(millisecond)
        }},
        namespace_prefix = NamespacePrefix,
        max_checkpoints = MaxCheckpoints,
        auto_prune = AutoPrune
    }.

%%====================================================================
%% 基本操作
%%====================================================================

%% @doc 保存检查点
-spec save(manager(), checkpoint(), config()) ->
    {ok, checkpoint_id()} | {error, term()}.
save(#manager{store = Store, current_branch = BranchId, branches = Branches} = Manager,
     Checkpoint, Config) ->

    ThreadId = get_thread_id(Config),
    ParentCpId = maps:get(checkpoint_id, Config, undefined),

    %% 生成 ID（如果未提供）
    CpId = case Checkpoint#checkpoint.id of
        undefined -> generate_checkpoint_id();
        Id -> Id
    end,

    %% 更新检查点
    UpdatedCp = Checkpoint#checkpoint{
        id = CpId,
        thread_id = ThreadId,
        parent_id = ParentCpId,
        timestamp = erlang:system_time(millisecond)
    },

    %% 序列化并存储
    CpNs = checkpoint_namespace(Manager, ThreadId),
    CpValue = checkpoint_to_map(UpdatedCp),

    %% 检查数量限制并自动清理（如果启用）
    case check_and_auto_prune(Manager, ThreadId) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            case beamai_store:put(Store, CpNs, CpId, CpValue) of
                ok ->
                    %% 更新分支信息
                    BranchInfo0 = maps:get(BranchId, Branches, #{
                        branch_id => BranchId,
                        checkpoint_count => 0,
                        created_at => erlang:system_time(millisecond)
                    }),
                    BranchInfo = BranchInfo0#{
                        head_checkpoint_id => CpId,
                        checkpoint_count => maps:get(checkpoint_count, BranchInfo0, 0) + 1
                    },

                    UpdatedManager = Manager#manager{
                        branches = maps:put(BranchId, BranchInfo, Branches)
                    },

                    %% 更新线程索引
                    update_thread_index(Store, ThreadId, UpdatedCp),
                    {ok, CpId};
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc 加载检查点
-spec load(manager(), checkpoint_id() | latest, config()) ->
    {ok, checkpoint_tuple()} | {error, not_found | term()}.
load(#manager{store = Store}, CheckpointId, Config) when CheckpointId =:= latest orelse CheckpointId =:= undefined ->
    ThreadId = get_thread_id(Config),
    get_latest_checkpoint(Store, checkpoint_namespace(#manager{}, ThreadId));
load(#manager{store = Store}, CheckpointId, Config) ->
    ThreadId = get_thread_id(Config),
    CpNs = [?NS_CHECKPOINTS, ThreadId],
    get_checkpoint_by_id(Store, ThreadId, CheckpointId).

%% @doc 删除检查点
-spec delete(manager(), config()) -> ok | {error, term()}.
delete(#manager{store = Store}, Config) ->
    ThreadId = get_thread_id(Config),
    CheckpointId = maps:get(checkpoint_id, Config, undefined),

    case CheckpointId of
        undefined ->
            {error, checkpoint_id_required};
        CpId ->
            CpNs = [?NS_CHECKPOINTS, ThreadId],
            beamai_store:delete(Store, CpNs, CpId)
    end.

%% @doc 列出检查点
-spec list(manager(), config()) -> {ok, [checkpoint_tuple()]} | {error, term()}.
list(#manager{store = Store}, Config) ->
    ThreadId = maps:get(thread_id, Config, undefined),
    Limit = maps:get(limit, Config, 100),

    case ThreadId of
        undefined ->
            list_all_checkpoints(Store, Limit);
        TId ->
            list_thread_checkpoints(Store, TId, Limit)
    end.

%% @doc 获取检查点数量
-spec count(manager()) -> non_neg_integer().
count(#manager{branches = Branches}) ->
    lists:foldl(fun(_, #{checkpoint_count := Count}, Acc) ->
        Acc + Count
    end, 0, Branches).

%%====================================================================
%% 时间旅行功能
%%====================================================================

%% @doc 回退 N 步
-spec go_back(manager(), config(), pos_integer()) ->
    {ok, checkpoint()} | {error, term()}.
go_back(#manager{store = Store, current_branch = BranchId, branches = Branches} = Manager,
        Config, Steps) ->

    ThreadId = get_thread_id(Config),

    case list_thread_checkpoints(Store, ThreadId, 1000) of
        {ok, Checkpoints} ->
            BranchInfo = maps:get(BranchId, Branches),
            CurrentCpId = maps:get(head_checkpoint_id, BranchInfo, undefined),

            case CurrentCpId of
                undefined ->
                    {error, no_current_checkpoint};
                _ ->
                    CurrentIndex = find_checkpoint_index(Checkpoints, CurrentCpId),
                    TargetIndex = min(CurrentIndex + Steps, length(Checkpoints) - 1),

                    if
                        TargetIndex < length(Checkpoints) ->
                            {Cp, _, _} = lists:nth(TargetIndex + 1, Checkpoints),
                            %% 更新分支 head
                            NewBranchInfo = BranchInfo#{head_checkpoint_id => Cp#checkpoint.id},
                            NewBranches = maps:put(BranchId, NewBranchInfo, Branches),
                            _ = Manager#manager{branches = NewBranches},
                            {ok, Cp};
                        true ->
                            {error, cannot_go_back_that_far}
                    end
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 前进 N 步
-spec go_forward(manager(), config(), pos_integer()) ->
    {ok, checkpoint()} | {error, term()}.
go_forward(#manager{store = Store, current_branch = BranchId, branches = Branches} = Manager,
        Config, Steps) ->

    ThreadId = get_thread_id(Config),

    case list_thread_checkpoints(Store, ThreadId, 1000) of
        {ok, Checkpoints} ->
            BranchInfo = maps:get(BranchId, Branches),
            CurrentCpId = maps:get(head_checkpoint_id, BranchInfo, undefined),

            case CurrentCpId of
                undefined ->
                    {error, no_current_checkpoint};
                _ ->
                    CurrentIndex = find_checkpoint_index(Checkpoints, CurrentCpId),
                    TargetIndex = max(CurrentIndex - Steps, 0),

                    {Cp, _, _} = lists:nth(TargetIndex + 1, Checkpoints),
                    %% 更新分支 head
                    NewBranchInfo = BranchInfo#{head_checkpoint_id => Cp#checkpoint.id},
                    NewBranches = maps:put(BranchId, NewBranchInfo, Branches),
                    _UpdatedManager = Manager#manager{branches = NewBranches},
                    {ok, Cp}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 跳转到指定检查点
-spec goto(manager(), config(), checkpoint_id()) ->
    {ok, checkpoint()} | {error, term()}.
goto(#manager{current_branch = BranchId, branches = Branches} = Manager,
     Config, CheckpointId) ->

    case load(Manager, CheckpointId, Config) of
        {ok, {Cp, _, _}} ->
            %% 更新分支 head
            BranchInfo = maps:get(BranchId, Branches),
            NewBranchInfo = BranchInfo#{head_checkpoint_id => CheckpointId},
            NewBranches = maps:put(BranchId, NewBranchInfo, Branches),
            _ = Manager#manager{branches = NewBranches},
            {ok, Cp};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 分支管理功能
%%====================================================================

%% @doc 创建分支
-spec branch(manager(), config(), binary(), map()) ->
    {ok, checkpoint_id()} | {error, term()}.
branch(#manager{store = _Store, current_branch = _CurrentBranch, branches = _Branches} = Manager,
      Config, NewThreadId, BranchOpts) ->

    case load(Manager, latest, Config) of
        {ok, {SourceCp, _SourceMeta, _ParentConfig}} ->
            SourceCpId = SourceCp#checkpoint.id,

            %% 创建新分支
            BranchName = maps:get(branch_name, BranchOpts, generate_branch_name(NewThreadId)),

            %% 创建新分支的初始检查点
            NewCpId = generate_checkpoint_id(),
            NewCp = #checkpoint{
                id = NewCpId,
                thread_id = NewThreadId,
                parent_id = SourceCpId,
                values = SourceCp#checkpoint.values,
                timestamp = erlang:system_time(millisecond)
            },

            %% 保存新检查点到新线程
            NewConfig = Config#{thread_id => NewThreadId},
            case save(Manager, NewCp, NewConfig) of
                {ok, NewCpId} ->
                    %% 记录分支信息
                    BranchInfo = #{
                        branch_id => BranchName,
                        checkpoint_count => 1,
                        created_at => erlang:system_time(millisecond)
                    },

                    %% 更新当前分支到新分支
                    {ok, NewCpId};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 切换分支
-spec switch_branch(manager(), config(), binary()) ->
    {ok, checkpoint()} | {error, term()}.
switch_branch(#manager{branches = Branches} = Manager, Config, BranchId) ->
    case maps:get(BranchId, Branches, undefined) of
        undefined ->
            {error, branch_not_found};
        _BranchInfo ->
            %% 切换当前分支
            NewManager = Manager#manager{current_branch = BranchId},
            %% 加载分支的最新检查点
            load(NewManager, latest, Config)
    end.

%% @doc 列出所有分支
-spec list_branches(manager()) -> {ok, [branch_info()]}.
list_branches(#manager{branches = Branches}) ->
    {ok, maps:values(Branches)}.

%% @doc 获取检查点的祖先链
-spec get_lineage(manager(), config()) ->
    {ok, [checkpoint_tuple()]} | {error, term()}.
get_lineage(#manager{store = Store}, Config) ->
    ThreadId = get_thread_id(Config),
    CheckpointId = maps:get(checkpoint_id, Config, undefined),

    StartId = case CheckpointId of
        undefined ->
            case get_latest_checkpoint(Store, [?NS_CHECKPOINTS, ThreadId]) of
                {ok, {Cp, _, _}} -> Cp#checkpoint.id;
                {error, _} = Error -> Error
            end;
        Id -> Id
    end,

    get_lineage_recursive(Store, StartId, ThreadId, []).

%% @doc 比较两个检查点的差异
-spec diff(manager(), config(), config()) ->
    {ok, map()} | {error, term()}.
diff(Manager, Config1, Config2) ->
    case {load(Manager, latest, Config1), load(Manager, latest, Config2)} of
        {{ok, {Cp1, _, _}}, {ok, {Cp2, _, _}}} ->
            Values1 = Cp1#checkpoint.values,
            Values2 = Cp2#checkpoint.values,

            Keys1 = maps:keys(Values1),
            Keys2 = maps:keys(Values2),

            Added = [K || K <- Keys2, not maps:is_key(K, Values1)],
            Removed = [K || K <- Keys1, not maps:is_key(K, Values2)],
            Changed = [K || K <- Keys1,
                            maps:is_key(K, Values2),
                            maps:get(K, Values1) =/= maps:get(K, Values2)],

            DiffResult = #{
                added => [{K, maps:get(K, Values2)} || K <- Added],
                removed => [{K, maps:get(K, Values1)} || K <- Removed],
                changed => [{K, #{
                    old => maps:get(K, Values1),
                    new => maps:get(K, Values2)
                }} || K <- Changed],
                checkpoint1 => #{
                    id => Cp1#checkpoint.id,
                    thread_id => Cp1#checkpoint.thread_id,
                    timestamp => Cp1#checkpoint.timestamp
                },
                checkpoint2 => #{
                    id => Cp2#checkpoint.id,
                    thread_id => Cp2#checkpoint.thread_id,
                    timestamp => Cp2#checkpoint.timestamp
                }
            },
            {ok, DiffResult};
        {{error, _} = Error, _} ->
            Error;
        {_, {error, _} = Error} ->
            Error
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 获取当前分支
-spec get_current_branch(manager()) -> binary().
get_current_branch(#manager{current_branch = BranchId}) ->
    BranchId.

%% @doc 获取 Store
-spec get_store(manager()) -> beamai_store:store().
get_store(#manager{store = Store}) ->
    Store.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取 thread_id
-spec get_thread_id(config()) -> thread_id().
get_thread_id(Config) when is_map(Config) ->
    maps:get(thread_id, Config, maps:get(<<"thread_id">>, Config, undefined)).

%% @private 生成检查点 ID
-spec generate_checkpoint_id() -> checkpoint_id().
generate_checkpoint_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("cp_~16.16.0b_~4.16.0b", [Ts, Rand])).

%% @private 生成分支名称
-spec generate_branch_name(thread_id()) -> binary().
generate_branch_name(ThreadId) ->
    Rand = rand:uniform(16#FFFF),
    <<ThreadId/binary, "_branch_", (integer_to_binary(Rand))/binary>>.

%% @private 检查点命名空间
-spec checkpoint_namespace(manager(), thread_id()) -> [binary()].
checkpoint_namespace(#manager{namespace_prefix = Prefix}, ThreadId) ->
    Prefix ++ [ThreadId].

%% @private 获取最新检查点
-spec get_latest_checkpoint(beamai_store:store(), [binary()]) ->
    {ok, checkpoint_tuple()} | {error, not_found}.
get_latest_checkpoint(Store, CpNs) ->
    IdxNs = [?NS_CHECKPOINTS, ?NS_INDEX | tl(CpNs)],
    case beamai_store:get(Store, IdxNs, ?KEY_LATEST) of
        {ok, #store_item{value = Value}} ->
            LatestCpId = get_flex(checkpoint_id, Value),
            case LatestCpId of
                undefined -> {error, not_found};
                _ ->
                    ThreadId = lists:last(CpNs),
                    get_checkpoint_by_id(Store, ThreadId, LatestCpId)
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%% @private 获取指定检查点
-spec get_checkpoint_by_id(beamai_store:store(), thread_id(), checkpoint_id()) ->
    {ok, checkpoint_tuple()} | {error, not_found}.
get_checkpoint_by_id(Store, ThreadId, CheckpointId) ->
    CpNs = [?NS_CHECKPOINTS, ThreadId],
    case beamai_store:get(Store, CpNs, CheckpointId) of
        {ok, #store_item{value = Value}} ->
            map_to_checkpoint_tuple(Value);
        {error, not_found} ->
            find_checkpoint_in_all_threads(Store, CheckpointId);
        {error, _} = Error ->
            Error
    end.

%% @private 在所有线程中查找检查点
-spec find_checkpoint_in_all_threads(beamai_store:store(), checkpoint_id()) ->
    {ok, checkpoint_tuple()} | {error, not_found}.
find_checkpoint_in_all_threads(Store, CheckpointId) ->
    case beamai_store:search(Store, [?NS_CHECKPOINTS], #{}) of
        {ok, Results} ->
            find_in_results(Results, CheckpointId);
        {error, _} ->
            {error, not_found}
    end.

%% @private 在搜索结果中查找
-spec find_in_results([beamai_store:search_result()], checkpoint_id()) ->
    {ok, checkpoint_tuple()} | {error, not_found}.
find_in_results([], _CheckpointId) ->
    {error, not_found};
find_in_results([#search_result{item = Item} | Rest], CheckpointId) ->
    case Item#store_item.key of
        CheckpointId ->
            map_to_checkpoint_tuple(Item#store_item.value);
        _ ->
            find_in_results(Rest, CheckpointId)
    end.

%% @private 列出所有检查点
-spec list_all_checkpoints(beamai_store:store(), pos_integer()) ->
    {ok, [checkpoint_tuple()]} | {error, term()}.
list_all_checkpoints(Store, Limit) ->
    case beamai_store:search(Store, [?NS_CHECKPOINTS], #{limit => Limit}) of
        {ok, Results} ->
            Checkpoints = lists:filtermap(fun(#search_result{item = Item}) ->
                case Item#store_item.namespace of
                    [?NS_CHECKPOINTS, ?NS_INDEX | _] -> false;
                    _ ->
                        case map_to_checkpoint_tuple(Item#store_item.value) of
                            {ok, Tuple} -> {true, Tuple};
                            _ -> false
                        end
                end
            end, Results),
            {ok, Checkpoints};
        {error, _} = Error ->
            Error
    end.

%% @private 列出线程检查点
-spec list_thread_checkpoints(beamai_store:store(), thread_id(), pos_integer()) ->
    {ok, [checkpoint_tuple()]} | {error, term()}.
list_thread_checkpoints(Store, ThreadId, Limit) ->
    CpNs = [?NS_CHECKPOINTS, ThreadId],
    case beamai_store:search(Store, CpNs, #{limit => Limit}) of
        {ok, Results} ->
            Checkpoints = lists:filtermap(fun(#search_result{item = Item}) ->
                case map_to_checkpoint_tuple(Item#store_item.value) of
                    {ok, Tuple} -> {true, Tuple};
                    _ -> false
                end
            end, Results),
            %% 按时间戳降序排序
            Sorted = lists:sort(fun({Cp1, _, _}, {Cp2, _, _}) ->
                Cp1#checkpoint.timestamp > Cp2#checkpoint.timestamp
            end, Checkpoints),
            {ok, Sorted};
        {error, _} = Error ->
            Error
    end.

%% @private 更新线程索引
-spec update_thread_index(beamai_store:store(), thread_id(), checkpoint()) ->
    ok | {error, term()}.
update_thread_index(Store, ThreadId, Checkpoint) ->
    IdxNs = [?NS_CHECKPOINTS, ?NS_INDEX, ThreadId],

    %% 更新 latest 指针
    LatestValue = #{
        checkpoint_id => Checkpoint#checkpoint.id,
        timestamp => Checkpoint#checkpoint.timestamp
    },
    case beamai_store:put(Store, IdxNs, ?KEY_LATEST, LatestValue) of
        ok ->
            %% 更新索引列表
            NewEntry = #{
                checkpoint_id => Checkpoint#checkpoint.id,
                timestamp => Checkpoint#checkpoint.timestamp
            },
            case beamai_store:get(Store, IdxNs, ?KEY_INDEX) of
                {ok, #store_item{value = Value}} ->
                    Entries = get_flex(entries, Value, []),
                    NewEntries = [NewEntry | Entries],
                    beamai_store:put(Store, IdxNs, ?KEY_INDEX, #{entries => NewEntries});
                {error, not_found} ->
                    beamai_store:put(Store, IdxNs, ?KEY_INDEX, #{entries => [NewEntry]})
            end;
        {error, _} = Error ->
            Error
    end.

%% @private 查找检查点索引
-spec find_checkpoint_index([checkpoint_tuple()], checkpoint_id()) -> non_neg_integer().
find_checkpoint_index(Checkpoints, CheckpointId) ->
    find_checkpoint_index(Checkpoints, CheckpointId, 0).

find_checkpoint_index([], _CheckpointId, _Index) ->
    0;
find_checkpoint_index([{Cp, _, _} | _Rest], CheckpointId, Index) when Cp#checkpoint.id =:= CheckpointId ->
    Index;
find_checkpoint_index([_ | Rest], CheckpointId, Index) ->
    find_checkpoint_index(Rest, CheckpointId, Index + 1).

%% @private 递归获取祖先链
-spec get_lineage_recursive(beamai_store:store(), checkpoint_id(), thread_id(), [checkpoint_tuple()]) ->
    {ok, [checkpoint_tuple()]} | {error, term()}.
get_lineage_recursive(Store, CheckpointId, ThreadId, Acc) ->
    case get_checkpoint_by_id(Store, ThreadId, CheckpointId) of
        {ok, {Checkpoint, _, _} = Tuple} ->
            NewAcc = [Tuple | Acc],
            case Checkpoint#checkpoint.parent_id of
                undefined ->
                    {ok, lists:reverse(NewAcc)};
                ParentId ->
                    get_lineage_recursive(Store, ParentId, ThreadId, NewAcc)
            end;
        {error, not_found} when Acc =/= [] ->
            {ok, lists:reverse(Acc)};
        {error, _} = Error ->
            Error
    end.

%% @private 检查点转 Map
-spec checkpoint_to_map(checkpoint()) -> map().
checkpoint_to_map(Checkpoint) ->
    #{
        <<"checkpoint">> => #{
            <<"id">> => Checkpoint#checkpoint.id,
            <<"thread_id">> => Checkpoint#checkpoint.thread_id,
            <<"parent_id">> => Checkpoint#checkpoint.parent_id,
            <<"values">> => Checkpoint#checkpoint.values,
            <<"timestamp">> => Checkpoint#checkpoint.timestamp
        },
        <<"metadata">> => #{
            %% 执行阶段信息
            <<"checkpoint_type">> => undefined,
            <<"step">> => 0,
            %% 图顶点状态
            <<"active_vertices">> => [],
            <<"completed_vertices">> => [],
            %% 执行标识
            <<"run_id">> => undefined,
            <<"agent_id">> => undefined,
            <<"agent_name">> => undefined,
            <<"iteration">> => 0,
            %% 用户自定义元数据
            <<"metadata">> => #{}
        },
        <<"parent_config">> => undefined
    }.

%% @private Map 转检查点元组
-spec map_to_checkpoint_tuple(map()) -> {ok, checkpoint_tuple()} | {error, invalid_format}.
map_to_checkpoint_tuple(Map) when is_map(Map) ->
    CpMap = get_flex(<<"checkpoint">>, Map),
    case CpMap of
        undefined ->
            {error, invalid_format};
        _ ->
            Checkpoint = #checkpoint{
                id = get_flex(<<"id">>, CpMap),
                thread_id = get_flex(<<"thread_id">>, CpMap),
                parent_id = get_flex(<<"parent_id">>, CpMap, undefined),
                values = get_flex(<<"values">>, CpMap, #{}),
                timestamp = get_flex(<<"timestamp">>, CpMap, 0)
            },
            Metadata = #checkpoint_metadata{
                %% 执行阶段信息
                checkpoint_type = undefined,
                step = 0,
                %% 图顶点状态
                active_vertices = [],
                completed_vertices = [],
                %% 执行标识
                run_id = undefined,
                agent_id = undefined,
                agent_name = undefined,
                iteration = 0,
                %% 用户自定义元数据
                metadata = #{}
            },
            {ok, {Checkpoint, Metadata, undefined}}
    end;
map_to_checkpoint_tuple(_) ->
    {error, invalid_format}.

%% @private 获取 map 值（支持 binary 和 atom 键）
-spec get_flex(binary() | atom(), map()) -> term().
get_flex(Key, Map) ->
    get_flex(Key, Map, undefined).

-spec get_flex(binary() | atom(), map(), term()) -> term().
get_flex(Key, Map, Default) when is_binary(Key) ->
    maps:get(Key, Map, maps:get(binary_to_existing_atom(Key, utf8), Map, Default));
get_flex(Key, Map, Default) when is_atom(Key) ->
    maps:get(Key, Map, maps:get(atom_to_binary(Key, utf8), Map, Default)).

%%====================================================================
%% 清理和统计功能
%%====================================================================

%% @doc 检查数量限制并自动清理
%%
%% 检查线程的检查点数量，如果超过 max_checkpoints 且启用了 auto_prune，
%% 则自动清理最旧的检查点。
-spec check_and_auto_prune(manager(), thread_id()) -> ok | {error, term()}.
check_and_auto_prune(#manager{auto_prune = false}, _ThreadId) ->
    ok;
check_and_auto_prune(#manager{auto_prune = true, max_checkpoints = infinity}, _ThreadId) ->
    ok;
check_and_auto_prune(#manager{store = Store, auto_prune = true, max_checkpoints = Max}, ThreadId) ->
    case list_thread_checkpoints_internal(#manager{store = Store}, ThreadId, Max + 1) of
        {ok, Checkpoints} when length(Checkpoints) >= Max ->
            %% 需要清理，保留最新的 Max 个
            ToDeleteCount = length(Checkpoints) - Max,
            ToDelete = lists:sublist(Checkpoints, length(Checkpoints) - ToDeleteCount + 1, ToDeleteCount),

            CpNs = [?NS_CHECKPOINTS, ThreadId],
            lists:foreach(fun({Cp, _, _}) ->
                beamai_store:delete(Store, CpNs, Cp#checkpoint.id)
            end, ToDelete),

            logger:info("自动清理了 ~p 个旧检查点 (thread: ~s)", [ToDeleteCount, ThreadId]),
            ok;
        _ ->
            ok
    end.

%% @doc 清理线程的检查点
%%
%% 保留最新的 KeepCount 个检查点，删除其余的。
-spec prune_checkpoints(manager(), thread_id(), pos_integer()) -> {ok, non_neg_integer()}.
prune_checkpoints(#manager{store = Store}, ThreadId, KeepCount) ->
    case list_thread_checkpoints_internal(#manager{store = Store}, ThreadId, 10000) of
        {ok, Checkpoints} ->
            TotalCount = length(Checkpoints),
            case TotalCount > KeepCount of
                true ->
                    ToDeleteCount = TotalCount - KeepCount,
                    ToDelete = lists:sublist(Checkpoints, KeepCount, ToDeleteCount),

                    CpNs = [?NS_CHECKPOINTS, ThreadId],
                    DeletedCount = lists:foldl(fun({Cp, _, _}, Acc) ->
                        case beamai_store:delete(Store, CpNs, Cp#checkpoint.id) of
                            ok -> Acc + 1;
                            {error, _} -> Acc
                        end
                    end, 0, ToDelete),

                    logger:info("清理了 ~p 个检查点 (thread: ~s, 保留: ~p)", [DeletedCount, ThreadId, KeepCount]),
                    {ok, DeletedCount};
                false ->
                    {ok, 0}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 清理分支的所有检查点
%%
%% 删除分支的所有检查点，保留分支信息但重置计数。
-spec prune_branch_checkpoints(manager(), binary(), pos_integer()) -> {ok, non_neg_integer()}.
prune_branch_checkpoints(#manager{store = _Store, branches = Branches}, BranchId, _KeepCount) ->
    case maps:get(BranchId, Branches) of
        undefined ->
            {error, branch_not_found};
        BranchInfo ->
            %% 获取该分支对应的所有线程的检查点
            %% 这里简化处理，清空分支并重置计数
            NewBranchInfo = BranchInfo#{
                checkpoint_count => 0,
                head_checkpoint_id => undefined
            },
            
            %% TODO: 实际实现需要遍历所有线程并删除该分支的检查点
            {ok, 0}
    end.

%% @doc 获取检查点统计信息
-spec get_checkpoint_stats(manager()) -> map().
get_checkpoint_stats(#manager{store = _Store, branches = Branches, max_checkpoints = Max}) ->
    TotalCount = count(#manager{branches = Branches}),
    
    %% 计算每个分支的统计
    BranchStats = maps:foldl(fun(BranchId, #{checkpoint_count := Count, created_at := Created}, Acc) ->
        Acc#{BranchId => #{
            checkpoint_count => Count,
            created_at => Created
        }}
    end, #{}, Branches),
    
    #{
        total_checkpoints => TotalCount,
        max_checkpoints => Max,
        branches => maps:size(Branches),
        branch_stats => BranchStats,
        usage_percentage => case Max of
            infinity -> 0.0;
            _ -> (TotalCount / Max) * 100
        end
    }.

%% @doc 获取线程的检查点数量
-spec get_checkpoint_count(manager(), thread_id()) -> non_neg_integer().
get_checkpoint_count(#manager{store = Store}, ThreadId) ->
    CpNs = [?NS_CHECKPOINTS, ThreadId],
    case beamai_store:search(Store, CpNs, #{limit => 100000}) of
        {ok, Results} -> length(Results);
        {error, _} -> 0
    end.

%% @doc 获取总检查点数量（所有线程和分支）
-spec get_total_count(manager()) -> non_neg_integer().
get_total_count(#manager{branches = Branches}) ->
    lists:foldl(fun(_, #{checkpoint_count := Count}, Acc) ->
        Acc + Count
    end, 0, Branches).

%% @private 列出线程检查点（内部版本）
-spec list_thread_checkpoints_internal(manager(), thread_id(), pos_integer()) ->
    {ok, [checkpoint_tuple()]} | {error, term()}.
list_thread_checkpoints_internal(#manager{store = Store}, ThreadId, Limit) ->
    CpNs = [?NS_CHECKPOINTS, ThreadId],
    case beamai_store:search(Store, CpNs, #{limit => Limit}) of
        {ok, Results} ->
            Checkpoints = lists:filtermap(fun(#search_result{item = Item}) ->
                case map_to_checkpoint_tuple(Item#store_item.value) of
                    {ok, Tuple} -> {true, Tuple};
                    _ -> false
                end
            end, Results),
            %% 按时间戳降序排序
            Sorted = lists:sort(fun({Cp1, _, _}, {Cp2, _, _}) ->
                Cp1#checkpoint.timestamp > Cp2#checkpoint.timestamp
            end, Checkpoints),
            {ok, Sorted};
        {error, _} = Error ->
            Error
    end.
