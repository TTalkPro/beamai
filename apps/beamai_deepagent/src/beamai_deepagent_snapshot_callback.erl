%%%-------------------------------------------------------------------
%%% @doc Deep Agent Snapshot 回调模块
%%%
%%% 创建 on_snapshot 回调函数，注入给图执行层。
%%% 回调函数会在图执行的关键点被调用，自动保存快照。
%%%
%%% == 回调签名 ==
%%%
%%% 回调函数签名: fun(Info, SnapshotData) -> continue | interrupt
%%% - Info: #{type => snapshot_type(), superstep => integer(), ...}
%%% - SnapshotData: #{type, pregel_snapshot, iteration, run_id, ...}
%%%
%%% == 元数据适配 ==
%%%
%%% 图执行层的上下文信息（iteration、superstep、active_vertices、
%%% completed_vertices）存储在 metadata 扩展字段中，
%%% 适配 Process Framework 的 snapshot_metadata 结构。
%%%
%%% == Deep Agent 特有状态 ==
%%%
%%% 保存 plan、trace、subtasks、depth 等 Deep Agent 特有字段。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_snapshot_callback).

%% API 导出
-export([create_callback/2]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 创建 snapshot 回调函数
%%
%% 回调函数签名: fun(Info, SnapshotData) -> continue | interrupt
%% - Info: pregel 超步信息 #{type, superstep, ...}
%% - SnapshotData: 快照数据 #{type, pregel_snapshot, iteration, run_id, ...}
%%
%% @param Config Deep Agent 配置
%% @param Memory beamai_memory 实例
%% @returns on_snapshot 回调函数
-spec create_callback(map(), beamai_memory:memory()) -> function().
create_callback(Config, Memory) ->
    ThreadId = beamai_memory:get_thread_id(Memory),
    AgentId = maps:get(agent_id, Config, <<"deepagent">>),
    AgentName = maps:get(agent_name, Config, <<"DeepAgent">>),

    fun(Info, SnapshotData) ->
        %% 提取快照类型
        SnapshotType = maps:get(type, SnapshotData, maps:get(type, Info, step)),

        %% 从 pregel_snapshot 提取状态
        PregelSnapshot = maps:get(pregel_snapshot, SnapshotData, #{}),
        GraphState = extract_graph_state(PregelSnapshot),

        %% 构建要保存的状态数据（Deep Agent 特有字段）
        StateData = #{
            %% 基础状态
            messages => get_state_value(GraphState, messages, []),
            system_prompt => get_state_value(GraphState, system_prompt, <<>>),

            %% Deep Agent 特有状态
            plan => serialize_plan(get_state_value(GraphState, plan, undefined)),
            trace => get_state_value(GraphState, trace, beamai_deepagent_trace:new()),
            subtasks => get_state_value(GraphState, subtasks, []),
            subtask_results => get_state_value(GraphState, subtask_results, []),

            %% 控制状态
            depth => get_state_value(GraphState, depth, 0),
            max_depth => get_state_value(GraphState, max_depth, 3),
            pending_tools => get_state_value(GraphState, pending_tools, []),
            tool_results => get_state_value(GraphState, tool_results, [])
        },

        %% 构建配置
        SaveConfig = #{
            thread_id => ThreadId,
            run_id => maps:get(run_id, SnapshotData, undefined),
            agent_id => AgentId,
            agent_name => AgentName
        },

        %% 构建元数据（适配 Process Framework 的 snapshot_metadata 格式）
        MetadataMap = #{
            snapshot_type => SnapshotType,
            process_name => deepagent,
            step_id => undefined,
            step_activations => #{},
            %% 将图执行的上下文信息放入 metadata 扩展字段
            metadata => #{
                iteration => maps:get(iteration, SnapshotData, 0),
                superstep => maps:get(superstep, SnapshotData, maps:get(superstep, Info, 0)),
                active_vertices => maps:get(active_vertices, SnapshotData, []),
                completed_vertices => maps:get(completed_vertices, SnapshotData, [])
            }
        },

        %% 保存 snapshot
        case beamai_memory:save_snapshot(Memory, SaveConfig, StateData, MetadataMap) of
            ok -> continue;
            {error, Reason} ->
                logger:warning("Deep Agent Snapshot 保存失败: ~p", [Reason]),
                continue
        end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从 pregel snapshot 提取图状态
%%
%% 状态存储在 __start__ 顶点的 initial_state 字段中
-spec extract_graph_state(map()) -> map().
extract_graph_state(PregelSnapshot) ->
    Vertices = maps:get(vertices, PregelSnapshot, #{}),
    case maps:get('__start__', Vertices, undefined) of
        undefined ->
            #{};
        StartVertex ->
            Value = maps:get(value, StartVertex, #{}),
            maps:get(initial_state, Value, #{})
    end.

%% @private 从状态中获取值，支持二进制和原子键
-spec get_state_value(map(), atom(), term()) -> term().
get_state_value(State, Key, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:get(BinKey, State, undefined) of
        undefined -> maps:get(Key, State, Default);
        Value -> Value
    end.

%% @private 序列化 plan
-spec serialize_plan(term()) -> map() | undefined.
serialize_plan(undefined) ->
    undefined;
serialize_plan(Plan) when is_map(Plan) ->
    Plan;
serialize_plan(_) ->
    undefined.
