%%%-------------------------------------------------------------------
%%% @doc Agent 持久化（Memory 集成）
%%%
%%% 提供 agent 状态的保存和恢复功能，通过 beamai_memory 的
%%% snapshot 机制实现持久化。
%%%
%%% 持久化内容包括：
%%%   - 消息历史（messages）
%%%   - 对话轮数（turn_count）
%%%   - 用户元数据（metadata）
%%%   - 系统提示词（system_prompt）
%%%   - Agent ID（agent_id）
%%%
%%% 不持久化的内容（每次从 config 重建）：
%%%   - kernel（含 LLM 配置、plugins、filters）
%%%   - callbacks
%%%   - memory 实例本身
%%%
%%% 使用前提：
%%%   agent 创建时需通过 config 传入 memory 实例。
%%%   未配置 memory 时调用 save/1 会返回 {error, no_memory_configured}。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_memory).

-export([save/1, restore/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc 保存当前 agent 状态到 memory
%%
%% 将 agent 的运行时状态序列化为 snapshot 数据并持久化。
%% 使用 agent_id 关联的 thread_id 作为存储键。
%%
%% 保存的数据字段：
%%   - messages: 完整对话历史
%%   - turn_count: 已完成的对话轮数
%%   - metadata: 用户自定义元数据
%%   - agent_id: agent 唯一标识
%%   - system_prompt: 当前系统提示词
%%
%% @param State agent 状态 map（需包含 memory 配置）
%% @returns ok 保存成功
%% @returns {error, no_memory_configured} 未配置 memory
%% @returns {error, Reason} 持久化失败
-spec save(map()) -> ok | {error, term()}.
save(#{memory := undefined}) ->
    {error, no_memory_configured};
save(#{memory := Memory, messages := Messages, id := AgentId,
       turn_count := TurnCount, metadata := Meta,
       system_prompt := SysPrompt} = State) ->
    ThreadId = beamai_memory:get_thread_id(Memory),
    IntState = maps:get(interrupt_state, State, undefined),
    RunId = maps:get(run_id, State, undefined),

    %% 将 agent 数据包装为 process_snapshot 格式
    %% 使用 steps_state 存储 agent 状态
    AgentData = #{
        messages => Messages,
        turn_count => TurnCount,
        metadata => Meta,
        agent_id => AgentId,
        system_prompt => SysPrompt,
        interrupt_state => IntState,
        run_id => RunId
    },

    ProcessState = #{
        process_spec => <<"beamai_agent">>,
        fsm_state => idle,
        steps_state => #{agent_state => #{state => AgentData}},
        event_queue => []
    },

    CheckpointType = case IntState of
        undefined -> <<"agent_snapshot">>;
        _ -> <<"agent_interrupt">>
    end,

    Opts = #{
        agent_id => AgentId,
        metadata => #{type => CheckpointType}
    },

    case beamai_memory:save_snapshot(Memory, ThreadId, ProcessState, Opts) of
        {ok, _Snapshot, _NewMemory} -> ok;
        {error, _} = Error -> Error
    end.

%% @doc 从 memory 恢复 agent 状态
%%
%% 加载指定 memory 中最新的 snapshot，用其保存的数据恢复 agent 状态。
%%
%% 恢复流程：
%%   1. 从 memory 加载最新 snapshot 数据
%%   2. 使用原始 config 重建 agent（重建 kernel、注入 filters 等）
%%   3. 将 snapshot 中的消息历史、turn_count、metadata 覆盖到新 state
%%   4. 恢复 system_prompt（如果 snapshot 中有保存）
%%
%% @param Config agent 配置 map（用于重建 kernel、callbacks 等不可序列化部分）
%% @param Memory memory 实例（用于加载 snapshot）
%% @returns {ok, AgentState} 恢复成功，返回完整 agent 状态
%% @returns {error, Reason} 恢复失败（snapshot 不存在或重建失败）
-spec restore(map(), term()) -> {ok, map()} | {error, term()}.
restore(Config, Memory) ->
    ThreadId = beamai_memory:get_thread_id(Memory),
    case beamai_memory:get_latest_snapshot(Memory, ThreadId) of
        {ok, Snapshot} ->
            %% 从 snapshot 的 steps_state 中提取 agent 数据
            StepsState = beamai_snapshot:get_steps_state(Snapshot),
            #{agent_state := #{state := SavedData}} = StepsState,
            %% 用原始 config + memory 重建 agent
            case beamai_agent:new(Config#{memory => Memory}) of
                {ok, State0} ->
                    %% 用 snapshot 数据覆盖运行时状态
                    State1 = State0#{
                        messages => maps:get(messages, SavedData, []),
                        turn_count => maps:get(turn_count, SavedData, 0),
                        metadata => maps:merge(
                            maps:get(metadata, State0),
                            maps:get(metadata, SavedData, #{})
                        )
                    },
                    %% 恢复 system_prompt（如果 snapshot 中有保存）
                    State2 = case maps:get(system_prompt, SavedData, undefined) of
                        undefined -> State1;
                        SP -> State1#{system_prompt => SP}
                    end,
                    %% 恢复中断状态
                    State3 = State2#{
                        interrupt_state => maps:get(interrupt_state, SavedData, undefined),
                        run_id => maps:get(run_id, SavedData, undefined)
                    },
                    {ok, State3};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

