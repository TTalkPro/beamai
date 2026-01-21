%%%-------------------------------------------------------------------
%%% @doc 检查点管理模块
%%%
%%% 负责 Agent 状态持久化：
%%% - 检查点保存/加载
%%% - 自动检查点
%%% - 检查点恢复
%%%
%%% 支持 LangGraph 风格的 Checkpoint 持久化。
%%% 直接使用 beamai_memory 模块进行存储操作。
%%%
%%% Memory 实例由外部创建和传入，本模块不负责创建存储。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_checkpoint).

-include("beamai_agent.hrl").
-include_lib("beamai_memory/include/beamai_checkpointer.hrl").

%% API 导出
-export([
    init_storage/2,
    save/2,
    load/2,
    load_latest/1,
    list/2,
    restore/2,
    maybe_restore/2,
    maybe_auto_save/2
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化存储
%%
%% 从配置选项获取预创建的 Memory 实例。
%% Memory 必须由外部创建后通过 storage 选项传入。
-spec init_storage(binary(), map()) -> beamai_memory:memory() | undefined.
init_storage(_AgentId, Opts) ->
    case maps:get(storage, Opts, undefined) of
        undefined ->
            undefined;
        Memory when is_map(Memory) ->
            Memory
    end.

%% @doc 保存检查点
%%
%% 保存当前状态到存储，返回检查点 ID。
%% 保存内容包括：
%% - messages: 压缩后的消息（用于 LLM 调用）
%% - full_messages: 完整对话历史（用于审计、调试、回溯）
%% - scratchpad: 中间步骤
%% - context: 用户自定义上下文数据
%% - 执行上下文信息（run_id, checkpoint_type, iteration, superstep, 顶点状态等）
-spec save(map(), #state{}) -> {ok, binary()} | {error, term()}.
save(_Meta, #state{config = #agent_config{storage = undefined}}) ->
    {error, storage_not_enabled};
save(Meta, #state{config = #agent_config{storage = Memory, id = AgentId},
                  run_id = RunId, messages = Msgs,
                  full_messages = FullMsgs, scratchpad = Pad, context = Ctx}) ->
    Config = #{
        thread_id => AgentId,
        run_id => RunId
    },
    State = #{
        messages => Msgs,
        full_messages => FullMsgs,
        scratchpad => Pad,
        context => Ctx,
        metadata => Meta,
        %% 执行上下文信息
        run_id => RunId,
        checkpoint_type => maps:get(checkpoint_type, Meta, undefined),
        iteration => maps:get(iteration, Meta, 0),
        superstep => maps:get(superstep, Meta, 0),
        active_vertices => maps:get(active_vertices, Meta, []),
        completed_vertices => maps:get(completed_vertices, Meta, [])
    },
    case beamai_memory:save_checkpoint(Memory, Config, State) of
        ok ->
            %% 从 Memory 中获取最新的 checkpoint_id
            case beamai_memory:load_checkpoint_tuple(Memory, Config) of
                {ok, {#checkpoint{id = CpId}, _Meta, _Parent}} ->
                    {ok, CpId};
                {ok, {Checkpoint, _Meta, _Parent}} when is_map(Checkpoint) ->
                    %% 兼容 map 格式
                    {ok, maps:get(id, Checkpoint, <<"saved">>)};
                _ ->
                    {ok, <<"saved">>}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 加载检查点
%%
%% 从存储加载指定检查点。
-spec load(binary(), #state{}) -> {ok, map()} | {error, term()}.
load(_CpId, #state{config = #agent_config{storage = undefined}}) ->
    {error, storage_not_enabled};
load(CpId, #state{config = #agent_config{storage = Memory, id = AgentId}}) ->
    Config = #{thread_id => AgentId, checkpoint_id => CpId},
    beamai_memory:load_checkpoint(Memory, Config).

%% @doc 加载最新检查点
-spec load_latest(#state{}) -> {ok, map()} | {error, term()}.
load_latest(#state{config = #agent_config{storage = undefined}}) ->
    {error, storage_not_enabled};
load_latest(#state{config = #agent_config{storage = Memory, id = AgentId}}) ->
    Config = #{thread_id => AgentId},
    beamai_memory:load_latest_checkpoint(Memory, Config).

%% @doc 列出检查点
-spec list(map(), #state{}) -> {ok, [map()]} | {error, term()}.
list(_Opts, #state{config = #agent_config{storage = undefined}}) ->
    {error, storage_not_enabled};
list(_Opts, #state{config = #agent_config{storage = Memory, id = AgentId}}) ->
    Config = #{thread_id => AgentId},
    case beamai_memory:list_checkpoints(Memory, Config) of
        {ok, Checkpoints} ->
            %% 转换为简化的 map 格式
            SimplifiedList = [checkpoint_to_map(Cp) || Cp <- Checkpoints],
            {ok, SimplifiedList};
        {error, _} = Error ->
            Error
    end.

%% @doc 从检查点恢复状态
%%
%% 加载检查点并应用到当前状态。
%% 恢复内容包括：messages、full_messages、scratchpad 和 context。
-spec restore(binary(), #state{}) -> {ok, #state{}} | {error, term()}.
restore(_CpId, #state{config = #agent_config{storage = undefined}} = State) ->
    {error, storage_not_enabled, State};
restore(CpId, #state{config = #agent_config{storage = Memory, id = AgentId}} = State) ->
    Config = #{thread_id => AgentId, checkpoint_id => CpId},
    case beamai_memory:load_checkpoint(Memory, Config) of
        {ok, Data} ->
            {ok, apply_checkpoint_data(Data, State)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 根据配置恢复检查点
%%
%% 支持两种恢复方式：
%% 1. restore_checkpoint => CpId - 恢复指定检查点
%% 2. restore_latest => true - 恢复最新检查点
-spec maybe_restore(map(), #state{}) -> #state{}.
maybe_restore(_Opts, #state{config = #agent_config{storage = undefined}} = State) ->
    State;
maybe_restore(Opts, #state{config = #agent_config{storage = Memory, id = AgentId}} = State) ->
    case maps:get(restore_checkpoint, Opts, undefined) of
        undefined ->
            maybe_restore_latest(Opts, Memory, AgentId, State);
        CpId when is_binary(CpId) ->
            restore_checkpoint_state(Memory, AgentId, CpId, State)
    end.

%% @doc 自动保存检查点（如果配置了 storage）
%%
%% 保存内容包括：messages、full_messages、scratchpad、result 和执行上下文。
%% 如果配置了 storage，自动保存检查点。
-spec maybe_auto_save(map(), #state{}) -> #state{}.
maybe_auto_save(_Result, #state{config = #agent_config{storage = undefined}} = State) ->
    State;
maybe_auto_save(Result, #state{config = #agent_config{storage = Memory, id = AgentId},
                               run_id = RunId, messages = Msgs, full_messages = FullMsgs,
                               scratchpad = Pad, context = Ctx} = State) ->
    Config = #{
        thread_id => AgentId,
        run_id => RunId
    },
    StateData = #{
        messages => Msgs,
        full_messages => FullMsgs,
        scratchpad => Pad,
        context => Ctx,
        result => Result,
        metadata => #{auto => true, timestamp => beamai_memory_utils:current_timestamp()},
        %% 执行上下文信息
        run_id => RunId,
        checkpoint_type => final,
        iteration => maps:get(iterations, Result, 0),
        superstep => 0,
        active_vertices => [],
        completed_vertices => []
    },
    case beamai_memory:save_checkpoint(Memory, Config, StateData) of
        ok ->
            State;
        {error, Reason} ->
            logger:warning("自动保存检查点失败: ~p", [Reason]),
            State
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 恢复最新检查点
-spec maybe_restore_latest(map(), beamai_memory:memory(), binary(), #state{}) -> #state{}.
maybe_restore_latest(Opts, Memory, AgentId, State) ->
    case maps:get(restore_latest, Opts, false) of
        true -> restore_latest_state(Memory, AgentId, State);
        false -> State
    end.

%% @private 恢复最新检查点状态
-spec restore_latest_state(beamai_memory:memory(), binary(), #state{}) -> #state{}.
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
-spec restore_checkpoint_state(beamai_memory:memory(), binary(), binary(), #state{}) -> #state{}.
restore_checkpoint_state(Memory, AgentId, CpId, State) ->
    Config = #{thread_id => AgentId, checkpoint_id => CpId},
    case beamai_memory:load_checkpoint(Memory, Config) of
        {ok, Data} ->
            apply_checkpoint_data(Data, State);
        {error, Reason} ->
            logger:warning("恢复检查点 ~s 失败: ~p", [CpId, Reason]),
            State
    end.

%% @private 应用检查点数据到状态
%%
%% 恢复 messages、full_messages、scratchpad、context 和 run_id 到状态。
%%
%% 注意：
%% - context 使用合并策略，保留当前状态中存在但检查点中不存在的键
%% - run_id 恢复确保 interrupt 前后的执行使用相同的 run_id
-spec apply_checkpoint_data(map(), #state{}) -> #state{}.
apply_checkpoint_data(Data, #state{context = CurrentCtx} = State) ->
    Messages = maps:get(messages, Data, []),
    FullMessages = maps:get(full_messages, Data, []),
    Scratchpad = maps:get(scratchpad, Data, []),
    RunId = maps:get(run_id, Data, undefined),
    %% Context 恢复：检查点数据覆盖当前值，但保留检查点中没有的当前键
    SavedCtx = maps:get(context, Data, #{}),
    NewCtx = maps:merge(CurrentCtx, SavedCtx),
    State#state{
        messages = Messages,
        full_messages = FullMessages,
        scratchpad = Scratchpad,
        context = NewCtx,
        run_id = RunId
    }.

%% @private 将检查点元组转换为 map
-spec checkpoint_to_map(tuple()) -> map().
checkpoint_to_map({#checkpoint{id = Id, values = Values}, Metadata, _ParentConfig}) ->
    #{
        id => Id,
        data => Values,
        metadata => Metadata
    };
checkpoint_to_map({Checkpoint, Metadata, _ParentConfig}) when is_map(Checkpoint) ->
    %% 兼容 map 格式
    #{
        id => maps:get(id, Checkpoint, undefined),
        data => maps:get(values, Checkpoint, maps:get(channel_values, Checkpoint, #{})),
        metadata => Metadata
    };
checkpoint_to_map(_) ->
    #{}.
