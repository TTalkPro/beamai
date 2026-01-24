%%%-------------------------------------------------------------------
%%% @doc Snapshot 分支管理扩展模块
%%%
%%% 提供高级分支管理功能，建立在 beamai_snapshot_manager 基础之上。
%%%
%%% == 功能 ==
%%%
%%% - create_branch: 创建新分支
%%% - switch_branch: 切换分支
%%% - list_branches: 列出所有分支
%%% - delete_branch: 删除分支
%%% - merge_branch: 合并分支（高级功能）
%%% - compare_branches: 比较分支
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% Manager = beamai_snapshot_manager:new(Store),
%%%
%%% %% 创建新分支（从当前状态）
%%% Config = #{thread_id => <<"main">>},
%%% {ok, BranchId} = beamai_snapshot_branch:create_branch(
%%%     Manager, Config, <<"experiment">>, #{}
%%% ).
%%%
%%% %% 列出所有分支
%%% {ok, Branches} = beamai_snapshot_branch:list_branches(Manager).
%%%
%%% %% 切换到新分支
%%% {ok, Cp} = beamai_snapshot_branch:switch_branch(
%%%     Manager, Config, <<"experiment">>
%%% ).
%%%
%%% %% 比较两个分支
%%% {ok, Diff} = beamai_snapshot_branch:compare_branches(
%%%     Manager, <<"main">>, <<"experiment">>
%%% ).
%%%
%%% %% 合并分支
%%% {ok, Result} = beamai_snapshot_branch:merge_branch(
%%%     Manager, <<"experiment">>, <<"main">>, #{strategy => fast_forward}
%%% ).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_snapshot_branch).

-include_lib("beamai_memory/include/beamai_snapshot.hrl").

%% 分支创建和管理
-export([create_branch/4, switch_branch/3, delete_branch/2]).

%% 分支查询
-export([list_branches/1, get_branch_info/2, compare_branches/3]).

%% 分支合并
-export([merge_branch/4, can_fast_forward/3]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 基本类型别名
-type snapshot() :: #snapshot{}.
-type snapshot_metadata() :: #snapshot_metadata{}.
-type config() :: map().

-type snapshot_tuple() :: {snapshot(), snapshot_metadata(), config() | undefined}.

%%====================================================================
%% 分支创建和管理
%%====================================================================

%% @doc 创建新分支
%% 从当前快照或指定快照创建新分支
-spec create_branch(beamai_snapshot_manager:manager(), map(), binary(), map()) ->
    {ok, binary()} | {error, term()}.
create_branch(Manager, Config, BranchName, Opts) ->
    NewThreadId = maps_get(thread_id, Opts, <<BranchName/binary, "-thread">>),
    beamai_snapshot_manager:branch(Manager, Config, NewThreadId, #{branch_name => BranchName}).

%% @doc 切换分支
%% 切换到指定分支并加载其最新状态
-spec switch_branch(beamai_snapshot_manager:manager(), map(), binary()) ->
    {ok, snapshot()} | {error, term()}.
switch_branch(Manager, Config, BranchId) ->
    beamai_snapshot_manager:switch_branch(Manager, Config, BranchId).

%% @doc 删除分支
%% 删除指定分支（不能删除当前分支或最后一个分支）
-spec delete_branch(beamai_snapshot_manager:manager(), binary()) ->
    ok | {error, term()}.
delete_branch(Manager, BranchId) ->
    CurrentBranch = beamai_snapshot_manager:get_current_branch(Manager),

    %% 不能删除当前分支
    if
        BranchId =:= CurrentBranch ->
            {error, cannot_delete_current_branch};
        true ->
            %% TODO: 实现删除逻辑
            %% 这需要在 manager 中添加 delete_branch 接口
            {error, not_implemented}
    end.

%%====================================================================
%% 分支查询
%%====================================================================

%% @doc 列出所有分支
%% @see beamai_snapshot_manager:list_branches/1
-spec list_branches(beamai_snapshot_manager:manager()) ->
    {ok, [map()]} | {error, term()}.
list_branches(Manager) ->
    case beamai_snapshot_manager:list_branches(Manager) of
        {ok, Branches} ->
            CurrentBranch = beamai_snapshot_manager:get_current_branch(Manager),
            %% 标记当前分支
            BranchList = lists:map(fun(B) ->
                B#{is_current => maps:get(branch_id, B) =:= CurrentBranch}
            end, Branches),
            {ok, BranchList};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取分支信息
-spec get_branch_info(beamai_snapshot_manager:manager(), binary()) ->
    {ok, map()} | {error, not_found}.
get_branch_info(Manager, BranchId) ->
    case list_branches(Manager) of
        {ok, Branches} ->
            case lists:search(fun(B) ->
                maps_get(branch_id, B) =:= BranchId
            end, Branches) of
                {value, BranchInfo} ->
                    {ok, BranchInfo};
                false ->
                    {error, not_found}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 比较两个分支
%% 返回两个分支之间的差异
-spec compare_branches(beamai_snapshot_manager:manager(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
compare_branches(Manager, BranchId1, BranchId2) ->
    case {get_branch_info(Manager, BranchId1), get_branch_info(Manager, BranchId2)} of
        {{ok, Info1}, {ok, Info2}} ->
            %% 获取两个分支的最新快照
            Config1 = #{thread_id => BranchId1},
            Config2 = #{thread_id => BranchId2},

            case {beamai_snapshot_manager:load(Manager, latest, Config1),
                  beamai_snapshot_manager:load(Manager, latest, Config2)} of
                {{ok, {Cp1, _, _}}, {ok, {Cp2, _, _}}} ->
                    Diff = #{
                        branch1 => #{
                            branch_id => BranchId1,
                            snapshot_id => Cp1#snapshot.id,
                            timestamp => Cp1#snapshot.timestamp,
                            snapshot_count => maps_get(snapshot_count, Info1, 0)
                        },
                        branch2 => #{
                            branch_id => BranchId2,
                            snapshot_id => Cp2#snapshot.id,
                            timestamp => Cp2#snapshot.timestamp,
                            snapshot_count => maps_get(snapshot_count, Info2, 0)
                        },
                        has_common_ancestor => has_common_ancestor(Cp1, Cp2),
                        diff_result => compare_snapshots(Cp1, Cp2)
                    },
                    {ok, Diff};
                {{error, _}, _} ->
                    {error, branch1_not_found};
                {_, {error, _}} ->
                    {error, branch2_not_found}
            end;
        {_, {error, _}} ->
            {error, branch_not_found}
    end.

%%====================================================================
%% 分支合并
%%====================================================================

%% @doc 合并分支
%% 支持多种合并策略
-spec merge_branch(beamai_snapshot_manager:manager(), binary(), binary(), map()) ->
    {ok, map()} | {error, term()}.
merge_branch(Manager, SourceBranch, TargetBranch, Opts) ->
    Strategy = maps_get(strategy, Opts, fast_forward),

    case compare_branches(Manager, SourceBranch, TargetBranch) of
        {ok, Comparison} ->
            case Strategy of
                fast_forward ->
                    merge_fast_forward(Manager, SourceBranch, TargetBranch, Comparison);
                squash ->
                    merge_squash(Manager, SourceBranch, TargetBranch, Comparison);
                merge ->
                    merge_three_way(Manager, SourceBranch, TargetBranch, Comparison);
                _ ->
                    {error, unknown_merge_strategy}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 检查是否可以快进合并
-spec can_fast_forward(beamai_snapshot_manager:manager(), binary(), binary()) ->
    boolean().
can_fast_forward(Manager, SourceBranch, TargetBranch) ->
    case compare_branches(Manager, SourceBranch, TargetBranch) of
        {ok, Comparison} ->
            maps_get(has_common_ancestor, Comparison, false);
        _ ->
            false
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 检查两个快照是否有共同祖先
-spec has_common_ancestor(snapshot(), snapshot()) -> boolean().
has_common_ancestor(Cp1, Cp2) ->
    %% 简单检查：Cp1 是否是 Cp2 的祖先，或反之
    is_ancestor(Cp1, Cp2) orelse is_ancestor(Cp2, Cp1).

%% @private 检查 Cp1 是否是 Ancestor 的祖先
-spec is_ancestor(snapshot(), snapshot()) -> boolean().
is_ancestor(Ancestor, Descendant) ->
    %% 沿着 parent_id 链查找
    is_ancestor_recursive(Ancestor#snapshot.id, Descendant).

%% @private 递归查找祖先
-spec is_ancestor_recursive(binary(), snapshot()) -> boolean().
is_ancestor_recursive(TargetId, Checkpoint) ->
    case Checkpoint#snapshot.parent_id of
        undefined ->
            false;
        TargetId ->
            true;
        _ParentId ->
            %% TODO: 需要加载父快照继续查找
            %% 这里简化处理，只检查直接父节点
            false
    end.

%% @private 比较两个快照的状态
-spec compare_snapshots(snapshot(), snapshot()) -> map().
compare_snapshots(Cp1, Cp2) ->
    Values1 = Cp1#snapshot.values,
    Values2 = Cp2#snapshot.values,

    Keys1 = maps:keys(Values1),
    Keys2 = maps:keys(Values2),

    Added = [K || K <- Keys2, not maps:is_key(K, Values1)],
    Removed = [K || K <- Keys1, not maps:is_key(K, Values2)],
    Changed = [K || K <- Keys1,
                    maps:is_key(K, Values2),
                    maps:get(K, Values1) =/= maps:get(K, Values2)],

    #{
        added => length(Added),
        removed => length(Removed),
        changed => length(Changed),
        added_keys => Added,
        removed_keys => Removed,
        changed_keys => Changed
    }.

%% @private 快进合并
-spec merge_fast_forward(beamai_snapshot_manager:manager(), binary(), binary(), map()) ->
    {ok, map()} | {error, term()}.
merge_fast_forward(_Manager, _SourceBranch, _TargetBranch, Comparison) ->
    case maps_get(has_common_ancestor, Comparison, false) of
        true ->
            %% 可以快进：只需将 target_branch 的 head 指向 source_branch 的 head
            #{
                branch1 := SourceInfo,
                branch2 := TargetInfo
            } = Comparison,
            Result = #{
                strategy => fast_forward,
                source_branch => SourceInfo,
                target_branch => TargetInfo,
                merged => true
            },
            {ok, Result};
        false ->
            {error, cannot_fast_forward}
    end.

%% @private Squash 合并（压缩多个提交为一个）
-spec merge_squash(beamai_snapshot_manager:manager(), binary(), binary(), map()) ->
    {ok, map()} | {error, term()}.
merge_squash(_Manager, _SourceBranch, _TargetBranch, _Comparison) ->
    %% TODO: 实现 squash 合并
    {error, not_implemented}.

%% @private 三方合并
-spec merge_three_way(beamai_snapshot_manager:manager(), binary(), binary(), map()) ->
    {ok, map()} | {error, term()}.
merge_three_way(_Manager, _SourceBranch, _TargetBranch, _Comparison) ->
    %% TODO: 实现三方合并
    {error, not_implemented}.

%% @private 安全获取 map 值
-spec maps_get(atom(), map()) -> term().
maps_get(Key, Map) ->
    maps:get(Key, Map, undefined).

-spec maps_get(atom(), map(), term()) -> term().
maps_get(Key, Map, Default) ->
    maps:get(Key, Map, Default).
