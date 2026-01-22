%%%-------------------------------------------------------------------
%%% @doc Agent Checkpoint 回调模块
%%%
%%% 创建 on_checkpoint 回调函数，注入给图执行层。
%%% 回调函数会在图执行的关键点被调用，自动保存检查点。
%%%
%%% == 回调签名 ==
%%%
%%% 回调函数签名: fun(Info, CheckpointData) -> continue | interrupt
%%% - Info: #{type => checkpoint_type(), superstep => integer(), ...}
%%% - CheckpointData: #{type, pregel_checkpoint, iteration, run_id, ...}
%%%
%%% == 回调类型 ==
%%%
%%% - initial: 初始化完成
%%% - step: 超步完成
%%% - final: 最终结果
%%% - error: 执行出错
%%% - interrupt: 用户中断
%%%
%%% == 使用方式 ==
%%%
%%% Agent 在构建图执行选项时调用 create_callback/2，
%%% 将返回的回调函数传给 graph:run/3 的 on_checkpoint 选项。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_checkpoint_callback).

-include("beamai_agent.hrl").

%% API 导出
-export([create_callback/2]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 创建 checkpoint 回调函数
%%
%% 回调函数签名: fun(Info, CheckpointData) -> continue | interrupt
%% - Info: pregel 超步信息 #{type, superstep, ...}
%% - CheckpointData: 检查点数据 #{type, pregel_checkpoint, iteration, run_id, ...}
%%
%% @param AgentState Agent 的当前状态（#state{}）
%% @param Memory beamai_memory 实例
%% @returns on_checkpoint 回调函数
-spec create_callback(#state{}, beamai_memory:memory()) -> function().
create_callback(#state{config = #agent_config{id = AgentId, name = AgentName}}, Memory) ->
    ThreadId = beamai_memory:get_thread_id(Memory),

    fun(Info, CheckpointData) ->
        %% 提取检查点类型
        CheckpointType = maps:get(type, CheckpointData, maps:get(type, Info, step)),

        %% 从 pregel_checkpoint 提取状态和执行上下文
        PregelCheckpoint = maps:get(pregel_checkpoint, CheckpointData, #{}),

        %% StateData = global_state（直接存储 agent 状态）
        %% 重要：使用 checkpoint_data.global_state 而不是 pregel_checkpoint.global_state
        %% checkpoint_data.global_state 是最新的全局状态（已应用 deltas）
        %% pregel_checkpoint.global_state 可能是旧的快照
        StateData = maps:get(global_state, CheckpointData, extract_graph_state(PregelCheckpoint)),

        %% DEBUG: 对比两个 global_state
        PregelGlobalState = extract_graph_state(PregelCheckpoint),
        PregelMsgs = get_messages_from_state(PregelGlobalState),
        DirectMsgs = get_messages_from_state(StateData),
        logger:info("[CHECKPOINT DEBUG] type=~p, superstep=~p",
                    [CheckpointType, maps:get(superstep, CheckpointData, 0)]),
        logger:info("[CHECKPOINT DEBUG] pregel_checkpoint.global_state.messages: ~p 条",
                    [length(PregelMsgs)]),
        logger:info("[CHECKPOINT DEBUG] checkpoint_data.global_state.messages: ~p 条",
                    [length(DirectMsgs)]),
        %% 打印消息角色
        logger:info("[CHECKPOINT DEBUG] StateData messages roles: ~p",
                    [[maps:get(role, M, maps:get(<<"role">>, M, unknown)) || M <- DirectMsgs]]),

        %% 构建配置
        Config = #{
            thread_id => ThreadId,
            run_id => maps:get(run_id, CheckpointData, undefined),
            agent_id => AgentId,
            agent_name => AgentName
        },

        %% 构建元数据
        %% 扁平字段由 beamai_memory 处理
        %% metadata 字段存储执行上下文（用于恢复）
        Superstep = maps:get(superstep, CheckpointData, maps:get(superstep, Info, 0)),
        MetadataMap = #{
            checkpoint_type => CheckpointType,
            iteration => maps:get(iteration, CheckpointData, 0),
            superstep => Superstep,
            active_vertices => maps:get(active_vertices, CheckpointData, []),
            completed_vertices => maps:get(completed_vertices, CheckpointData, []),
            %% 执行上下文（用于图恢复）
            metadata => #{
                pending_activations => maps:get(pending_activations, PregelCheckpoint, []),
                vertices => maps:get(vertices, PregelCheckpoint, #{}),
                pending_deltas => maps:get(pending_deltas, PregelCheckpoint, [])
            }
        },

        %% 保存 checkpoint
        case beamai_memory:save_checkpoint(Memory, Config, StateData, MetadataMap) of
            ok -> continue;
            {error, Reason} ->
                logger:warning("Checkpoint 保存失败: ~p", [Reason]),
                continue
        end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从 pregel checkpoint 提取图状态
%%
%% 全局状态模式：状态存储在 pregel_checkpoint.global_state 中
-spec extract_graph_state(map()) -> map().
extract_graph_state(PregelCheckpoint) ->
    maps:get(global_state, PregelCheckpoint, #{}).

%% @private 从状态中提取 messages（支持 atom 和 binary 键）
-spec get_messages_from_state(map()) -> [map()].
get_messages_from_state(State) ->
    case maps:get(<<"messages">>, State, undefined) of
        undefined -> maps:get(messages, State, []);
        Msgs -> Msgs
    end.
