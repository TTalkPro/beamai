%%%-------------------------------------------------------------------
%%% @doc Agent 回调系统
%%%
%%% 提供 8 个回调，用于监控和控制 agent 执行过程：
%%%   - on_turn_start: 新 turn 开始时触发
%%%   - on_turn_end: turn 正常完成后触发
%%%   - on_turn_error: turn 执行出错时触发
%%%   - on_llm_call: 每次 LLM 调用前触发（通过 pre_chat filter 注入）
%%%   - on_tool_call: 每次 tool 调用前触发，可返回 {interrupt, Reason}
%%%   - on_token: streaming 模式下每收到一个 token 时触发
%%%   - on_interrupt: agent 进入中断状态时触发
%%%   - on_resume: agent 从中断状态恢复时触发
%%%
%%% 设计原则：
%%%   - 大部分回调为观察性用途，不影响执行流程
%%%   - on_tool_call 可返回 {interrupt, Reason} 触发中断
%%%   - 回调异常时静默忽略，确保不中断主流程
%%%   - 回调未注册时不做任何操作
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_callbacks).

-export([invoke/3, build_metadata/1]).

-export_type([callbacks/0]).

-type callbacks() :: #{
    on_turn_start  => fun((map()) -> ok),           %% 参数: 元数据 map
    on_turn_end    => fun((map()) -> ok),           %% 参数: 元数据 map
    on_turn_error  => fun((term(), map()) -> ok),   %% 参数: 错误原因, 元数据 map
    on_llm_call    => fun(([map()], map()) -> ok),  %% 参数: 消息列表, 元数据 map
    on_tool_call   => fun((binary(), map()) -> ok | {interrupt, term()}),
                                                    %% 参数: 函数名, 调用参数
                                                    %% 返回 {interrupt, Reason} 可触发中断
    on_token       => fun((binary(), map()) -> ok), %% 参数: token 文本, 元数据 map
    on_interrupt   => fun((map(), map()) -> ok),    %% 参数: interrupt_state, 元数据 map
    on_resume      => fun((map(), map()) -> ok)     %% 参数: interrupt_state, 元数据 map
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 安全调用回调函数
%%
%% 从 callbacks map 中查找指定名称的回调，找到后用提供的参数列表调用。
%% 安全保证：
%%   - 回调未注册（undefined）时直接返回 ok，不做任何操作
%%   - 回调执行中抛出任何异常均被捕获并返回 ok
%%   - 确保回调永远不会中断 agent 的主执行流程
%%
%% @param Name 回调名称（atom，如 on_turn_start, on_tool_call 等）
%% @param Args 传递给回调函数的参数列表（erlang:apply 格式）
%% @param Callbacks 回调注册表 map
%% @returns ok（总是返回 ok）
-spec invoke(atom(), [term()], callbacks()) -> ok.
invoke(Name, Args, Callbacks) ->
    case maps:get(Name, Callbacks, undefined) of
        undefined -> ok;
        Fun -> try erlang:apply(Fun, Args) catch _:_ -> ok end
    end.

%% @doc 构建回调元数据
%%
%% 从 agent state 中提取关键信息，组装成标准化的元数据 map，
%% 传递给各回调函数作为上下文信息。
%%
%% 元数据包含：
%%   - agent_id: agent 唯一标识
%%   - agent_name: agent 名称
%%   - turn_count: 当前已完成的 turn 数
%%   - timestamp: 当前时间戳（毫秒）
%%
%% @param AgentState agent 状态 map
%% @returns 元数据 map
-spec build_metadata(map()) -> map().
build_metadata(AgentState) ->
    #{agent_id => maps:get(id, AgentState),
      agent_name => maps:get(name, AgentState, <<>>),
      turn_count => maps:get(turn_count, AgentState, 0),
      timestamp => erlang:system_time(millisecond)}.
