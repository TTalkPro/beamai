%%%-------------------------------------------------------------------
%%% @doc Deep Agent Snapshot 管理模块（内部使用）
%%%
%%% 负责 beamai_deepagent 状态持久化的内部逻辑：
%%% - 存储初始化
%%% - 快照恢复
%%%
%%% == 变更说明 ==
%%%
%%% 外部 Snapshot API 已移除（save、load、list 等）。
%%% Snapshot 现在通过以下方式管理：
%%% 1. 自动保存：通过 on_snapshot 回调在图执行时自动保存
%%% 2. 访问数据：用户通过 beamai_memory API 直接访问 snapshots
%%%
%%% 保留的内部功能：
%%% - init_storage/2: 从配置选项获取 Memory 实例
%%% - maybe_restore/2: 根据配置恢复快照
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_snapshot).

%% 内部 API 导出
-export([
    init_storage/2,
    maybe_restore/2
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化存储
%%
%% 根据配置选项初始化存储后端。
%% 支持两种方式：
%% 1. enable_storage => true - 自动创建存储
%% 2. storage => Memory - 使用已有 Memory 实例
-spec init_storage(binary(), map()) -> beamai_memory:memory() | undefined.
init_storage(AgentId, Config) ->
    case maps:get(storage, Config, undefined) of
        undefined ->
            maybe_create_storage(AgentId, Config);
        Memory when is_map(Memory) ->
            Memory
    end.

%% @doc 根据配置恢复快照
%%
%% 支持两种恢复方式：
%% 1. restore_snapshot => SnapshotId - 恢复指定快照
%% 2. restore_latest => true - 恢复最新快照
-spec maybe_restore(map(), graph_state:state()) -> graph_state:state().
maybe_restore(Config, State) ->
    Storage = graph_state:get(State, storage, undefined),
    case Storage of
        undefined ->
            State;
        Memory when is_map(Memory) ->
            ThreadId = beamai_memory:get_thread_id(Memory),
            case maps:get(restore_snapshot, Config, undefined) of
                undefined ->
                    maybe_restore_latest(Config, Memory, ThreadId, State);
                SnapshotId when is_binary(SnapshotId) ->
                    restore_snapshot_state(Memory, ThreadId, SnapshotId, State)
            end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 根据配置创建存储
-spec maybe_create_storage(binary(), map()) -> beamai_memory:memory() | undefined.
maybe_create_storage(AgentId, Config) ->
    case maps:get(enable_storage, Config, false) of
        true ->
            StorageOpts = maps:get(storage_opts, Config, #{}),
            %% 创建 ETS 存储后端
            StoreName = binary_to_atom(<<AgentId/binary, "_store">>, utf8),
            case beamai_store_ets:start_link(StoreName, StorageOpts) of
                {ok, _} ->
                    case beamai_memory:new(#{
                        context_store => {beamai_store_ets, StoreName},
                        thread_id => AgentId
                    }) of
                        {ok, Memory} ->
                            Memory;
                        {error, Reason} ->
                            logger:warning("创建 Memory 失败: ~p", [Reason]),
                            undefined
                    end;
                {error, Reason} ->
                    logger:warning("启动存储失败: ~p", [Reason]),
                    undefined
            end;
        false ->
            undefined
    end.

%% @private 恢复最新快照
-spec maybe_restore_latest(map(), beamai_memory:memory(), binary(), graph_state:state()) -> graph_state:state().
maybe_restore_latest(Config, Memory, ThreadId, State) ->
    case maps:get(restore_latest, Config, false) of
        true -> restore_latest_state(Memory, ThreadId, State);
        false -> State
    end.

%% @private 恢复最新快照状态
-spec restore_latest_state(beamai_memory:memory(), binary(), graph_state:state()) -> graph_state:state().
restore_latest_state(Memory, ThreadId, State) ->
    Config = #{thread_id => ThreadId},
    case beamai_memory:load_latest_snapshot(Memory, Config) of
        {ok, Data} ->
            apply_snapshot_data(Data, State);
        {error, not_found} ->
            State;
        {error, Reason} ->
            logger:warning("恢复最新快照失败: ~p", [Reason]),
            State
    end.

%% @private 恢复指定快照状态
-spec restore_snapshot_state(beamai_memory:memory(), binary(), binary(), graph_state:state()) -> graph_state:state().
restore_snapshot_state(Memory, ThreadId, SnapshotId, State) ->
    Config = #{thread_id => ThreadId, snapshot_id => SnapshotId},
    case beamai_memory:load_snapshot(Memory, Config) of
        {ok, Data} ->
            apply_snapshot_data(Data, State);
        {error, Reason} ->
            logger:warning("恢复快照 ~s 失败: ~p", [SnapshotId, Reason]),
            State
    end.

%% @private 应用快照数据到状态
%%
%% 恢复 messages、plan、trace、subtasks 等到状态。
-spec apply_snapshot_data(map(), graph_state:state()) -> graph_state:state().
apply_snapshot_data(Data, State) ->
    %% 恢复基础状态
    State1 = graph_state:set(State, messages, maps:get(messages, Data, [])),
    State2 = graph_state:set(State1, system_prompt, maps:get(system_prompt, Data, <<>>)),

    %% 恢复 Deep Agent 特有状态
    State3 = graph_state:set(State2, plan, maps:get(plan, Data, undefined)),
    State4 = graph_state:set(State3, trace, maps:get(trace, Data, beamai_deepagent_trace:new())),
    State5 = graph_state:set(State4, subtasks, maps:get(subtasks, Data, [])),
    State6 = graph_state:set(State5, subtask_results, maps:get(subtask_results, Data, [])),

    %% 恢复控制状态
    State7 = graph_state:set(State6, depth, maps:get(depth, Data, 0)),
    State8 = graph_state:set(State7, max_depth, maps:get(max_depth, Data, 3)),
    State9 = graph_state:set(State8, pending_tools, maps:get(pending_tools, Data, [])),
    State10 = graph_state:set(State9, tool_results, maps:get(tool_results, Data, [])),

    State10.
