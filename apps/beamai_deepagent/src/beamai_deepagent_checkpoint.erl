%%%-------------------------------------------------------------------
%%% @doc Deep Agent Checkpoint 管理模块
%%%
%%% 提供 beamai_deepagent 的状态持久化功能：
%%% - 存储初始化
%%% - 检查点保存/加载
%%% - 自动检查点
%%% - 检查点恢复
%%%
%%% 与 beamai_agent_checkpoint 的区别：
%%% - 适配 graph_state 而非 #state{}
%%% - 保存 plan、trace、subtasks 等 deep agent 特有状态
%%% - 支持纯函数模式和进程模式
%%%
%%% 直接使用 beamai_memory 模块进行存储操作。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_checkpoint).

%% API 导出
-export([
    init_storage/2,
    save/2,
    save/3,
    load/2,
    load_latest/1,
    list/2,
    restore/2,
    maybe_restore/2,
    maybe_auto_save/2
]).

%% 类型定义
-type config() :: #{
    enable_storage => boolean(),
    storage => beamai_memory:memory() | undefined,
    storage_opts => map(),
    restore_latest => boolean(),
    restore_checkpoint => binary()
}.
-type state() :: graph_state:state().
-type checkpoint_data() :: #{
    messages := [map()],
    plan := map() | undefined,
    trace := map(),
    subtasks := [map()],
    subtask_results := [map()],
    depth := non_neg_integer(),
    max_depth := pos_integer(),
    system_prompt := binary(),
    pending_tools := [map()],
    tool_results := [map()],
    metadata => map()
}.

-export_type([config/0, state/0, checkpoint_data/0]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化存储
%%
%% 根据配置选项初始化存储后端。
%% 支持两种方式：
%% 1. enable_storage => true - 自动创建存储
%% 2. storage => Memory - 使用已有 Memory 实例
-spec init_storage(binary(), config()) -> beamai_memory:memory() | undefined.
init_storage(AgentId, Config) ->
    case maps:get(storage, Config, undefined) of
        undefined ->
            maybe_create_storage(AgentId, Config);
        Memory when is_map(Memory) ->
            Memory
    end.

%% @doc 保存检查点
%%
%% 保存当前状态到存储，返回检查点 ID。
%% 保存内容包括：messages、plan、trace、subtasks 等 deep agent 特有状态。
-spec save(map(), state()) -> {ok, binary()} | {error, term()}.
save(Meta, State) ->
    Storage = graph_state:get(State, storage, undefined),
    AgentId = graph_state:get(State, agent_id, <<"unknown">>),
    do_save(Meta, State, Storage, AgentId).

%% @doc 保存检查点（带额外元数据）
-spec save(map(), state(), map()) -> {ok, binary()} | {error, term()}.
save(Meta, State, _Config) ->
    save(Meta, State).

%% @doc 加载检查点
%%
%% 从存储加载指定检查点。
-spec load(binary(), state()) -> {ok, checkpoint_data()} | {error, term()}.
load(CpId, State) ->
    Storage = graph_state:get(State, storage, undefined),
    AgentId = graph_state:get(State, agent_id, <<"unknown">>),
    case Storage of
        undefined ->
            {error, storage_not_enabled};
        Memory when is_map(Memory) ->
            Config = #{thread_id => AgentId, checkpoint_id => CpId},
            beamai_memory:load_checkpoint(Memory, Config)
    end.

%% @doc 加载最新检查点
-spec load_latest(state()) -> {ok, checkpoint_data()} | {error, term()}.
load_latest(State) ->
    Storage = graph_state:get(State, storage, undefined),
    AgentId = graph_state:get(State, agent_id, <<"unknown">>),
    case Storage of
        undefined ->
            {error, storage_not_enabled};
        Memory when is_map(Memory) ->
            Config = #{thread_id => AgentId},
            beamai_memory:load_latest_checkpoint(Memory, Config)
    end.

%% @doc 列出检查点
-spec list(map(), state()) -> {ok, [map()]} | {error, term()}.
list(_Opts, State) ->
    Storage = graph_state:get(State, storage, undefined),
    AgentId = graph_state:get(State, agent_id, <<"unknown">>),
    case Storage of
        undefined ->
            {error, storage_not_enabled};
        Memory when is_map(Memory) ->
            Config = #{thread_id => AgentId},
            case beamai_memory:list_checkpoints(Memory, Config) of
                {ok, Checkpoints} ->
                    SimplifiedList = [checkpoint_to_map(Cp) || Cp <- Checkpoints],
                    {ok, SimplifiedList};
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc 从检查点恢复状态
%%
%% 加载检查点并应用到当前状态。
%% 恢复内容包括：messages、plan、trace、subtasks 等。
-spec restore(binary(), state()) -> {ok, state()} | {error, term()}.
restore(CpId, State) ->
    Storage = graph_state:get(State, storage, undefined),
    AgentId = graph_state:get(State, agent_id, <<"unknown">>),
    case Storage of
        undefined ->
            {error, storage_not_enabled};
        Memory when is_map(Memory) ->
            Config = #{thread_id => AgentId, checkpoint_id => CpId},
            case beamai_memory:load_checkpoint(Memory, Config) of
                {ok, Data} ->
                    {ok, apply_checkpoint_data(Data, State)};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 根据配置恢复检查点
%%
%% 支持两种恢复方式：
%% 1. restore_checkpoint => CpId - 恢复指定检查点
%% 2. restore_latest => true - 恢复最新检查点
-spec maybe_restore(config(), state()) -> state().
maybe_restore(Config, State) ->
    Storage = graph_state:get(State, storage, undefined),
    AgentId = graph_state:get(State, agent_id, <<"unknown">>),
    case Storage of
        undefined ->
            State;
        Memory when is_map(Memory) ->
            case maps:get(restore_checkpoint, Config, undefined) of
                undefined ->
                    maybe_restore_latest(Config, Memory, AgentId, State);
                CpId when is_binary(CpId) ->
                    restore_checkpoint_state(Memory, AgentId, CpId, State)
            end
    end.

%% @doc 自动保存检查点（如果启用）
%%
%% 保存内容包括：messages、plan、trace、subtasks、result。
-spec maybe_auto_save(map(), state()) -> state().
maybe_auto_save(Result, State) ->
    AutoCheckpoint = graph_state:get(State, auto_checkpoint, false),
    Storage = graph_state:get(State, storage, undefined),
    AgentId = graph_state:get(State, agent_id, <<"unknown">>),
    case AutoCheckpoint andalso Storage =/= undefined of
        true ->
            do_auto_save(Result, State, Storage, AgentId);
        false ->
            State
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 执行保存操作
-spec do_save(map(), state(), beamai_memory:memory() | undefined, binary()) ->
    {ok, binary()} | {error, term()}.
do_save(_Meta, _State, undefined, _AgentId) ->
    {error, storage_not_enabled};
do_save(Meta, State, Memory, AgentId) ->
    Config = #{thread_id => AgentId},
    Data = build_checkpoint_data(State, Meta),
    case beamai_memory:save_checkpoint(Memory, Config, Data) of
        {ok, NewMemory} ->
            %% 从 Memory 中获取最新的 checkpoint_id
            case beamai_memory:load_checkpoint_tuple(NewMemory, Config) of
                {ok, {Checkpoint, _Meta, _Parent}} ->
                    CpId = maps:get(id, Checkpoint, <<"unknown">>),
                    {ok, CpId};
                _ ->
                    {ok, <<"saved">>}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 执行自动保存
-spec do_auto_save(map(), state(), beamai_memory:memory(), binary()) -> state().
do_auto_save(Result, State, Memory, AgentId) ->
    Config = #{thread_id => AgentId},
    Data = build_checkpoint_data(State, #{
        auto => true,
        result => Result,
        timestamp => beamai_memory_utils:current_timestamp()
    }),
    case beamai_memory:save_checkpoint(Memory, Config, Data) of
        {ok, NewMemory} ->
            graph_state:set(State, storage, NewMemory);
        {error, Reason} ->
            logger:warning("自动保存检查点失败: ~p", [Reason]),
            State
    end.

%% @private 根据配置创建存储
-spec maybe_create_storage(binary(), config()) -> beamai_memory:memory() | undefined.
maybe_create_storage(AgentId, Config) ->
    case maps:get(enable_storage, Config, false) of
        true ->
            StorageOpts = maps:get(storage_opts, Config, #{}),
            MemoryOpts = #{
                checkpointer => maps:merge(#{backend => ets}, StorageOpts),
                store => maps:merge(#{backend => ets}, StorageOpts)
            },
            case beamai_memory:new(MemoryOpts) of
                {ok, Memory} ->
                    %% 初始化空检查点以设置 thread_id
                    MemConfig = #{thread_id => AgentId},
                    case beamai_memory:save_checkpoint(Memory, MemConfig, #{}) of
                        {ok, NewMemory} -> NewMemory;
                        {error, _} -> Memory
                    end;
                {error, Reason} ->
                    logger:warning("启动存储失败: ~p", [Reason]),
                    undefined
            end;
        false ->
            undefined
    end.

%% @private 恢复最新检查点
-spec maybe_restore_latest(config(), beamai_memory:memory(), binary(), state()) -> state().
maybe_restore_latest(Config, Memory, AgentId, State) ->
    case maps:get(restore_latest, Config, false) of
        true -> restore_latest_state(Memory, AgentId, State);
        false -> State
    end.

%% @private 恢复最新检查点状态
-spec restore_latest_state(beamai_memory:memory(), binary(), state()) -> state().
restore_latest_state(Memory, AgentId, State) ->
    Config = #{thread_id => AgentId},
    case beamai_memory:load_latest_checkpoint(Memory, Config) of
        {ok, Data} ->
            apply_checkpoint_data(Data, State);
        {error, not_found} ->
            State;
        {error, Reason} ->
            logger:warning("恢复最新检查点失败: ~p", [Reason]),
            State
    end.

%% @private 恢复指定检查点状态
-spec restore_checkpoint_state(beamai_memory:memory(), binary(), binary(), state()) -> state().
restore_checkpoint_state(Memory, AgentId, CpId, State) ->
    Config = #{thread_id => AgentId, checkpoint_id => CpId},
    case beamai_memory:load_checkpoint(Memory, Config) of
        {ok, Data} ->
            apply_checkpoint_data(Data, State);
        {error, Reason} ->
            logger:warning("恢复检查点 ~s 失败: ~p", [CpId, Reason]),
            State
    end.

%% @private 构建检查点数据
%%
%% 提取 deep agent 特有的状态信息。
-spec build_checkpoint_data(state(), map()) -> map().
build_checkpoint_data(State, Meta) ->
    #{
        %% 基础状态
        messages => graph_state:get(State, messages, []),
        system_prompt => graph_state:get(State, system_prompt, <<>>),

        %% Deep Agent 特有状态
        plan => serialize_plan(graph_state:get(State, plan, undefined)),
        trace => graph_state:get(State, trace, beamai_deepagent_trace:new()),
        subtasks => graph_state:get(State, subtasks, []),
        subtask_results => graph_state:get(State, subtask_results, []),

        %% 控制状态
        depth => graph_state:get(State, depth, 0),
        max_depth => graph_state:get(State, max_depth, 3),
        pending_tools => graph_state:get(State, pending_tools, []),
        tool_results => graph_state:get(State, tool_results, []),

        %% 元数据
        metadata => Meta
    }.

%% @private 序列化 plan
-spec serialize_plan(term()) -> map() | undefined.
serialize_plan(undefined) ->
    undefined;
serialize_plan(Plan) when is_map(Plan) ->
    Plan;
serialize_plan(_) ->
    undefined.

%% @private 应用检查点数据到状态
%%
%% 恢复 messages、plan、trace、subtasks 等到状态。
-spec apply_checkpoint_data(checkpoint_data(), state()) -> state().
apply_checkpoint_data(Data, State) ->
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

%% @private 将检查点元组转换为 map
-spec checkpoint_to_map(tuple()) -> map().
checkpoint_to_map({Checkpoint, Metadata, _ParentConfig}) when is_map(Checkpoint) ->
    #{
        id => maps:get(id, Checkpoint, undefined),
        data => maps:get(channel_values, Checkpoint, #{}),
        metadata => Metadata
    };
checkpoint_to_map(_) ->
    #{}.
