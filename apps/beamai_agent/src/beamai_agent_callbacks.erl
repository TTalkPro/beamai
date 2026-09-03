%%%-------------------------------------------------------------------
%%% @doc Agent 回调系统
%%%
%%% 提供 11 个回调，用于监控和控制 agent 执行过程：
%%%   - on_turn_start: 新 turn 开始时触发
%%%   - on_turn_end: turn 正常完成后触发
%%%   - on_turn_error: turn 执行出错时触发
%%%   - on_llm_call: 每次 LLM 调用前触发（tool loop 内）
%%%   - on_llm_result: 每次 LLM 返回后触发（原始 response，观察用；可取各次 usage）
%%%   - on_llm_event: 流式模式下 provider 每个原始流事件触发（见下「原始流事件」）
%%%   - on_tool_call: 每次 tool 调用前触发，可返回 {interrupt, Reason}
%%%   - on_tool_result: 每个 tool 执行得到结果后触发（观察用，不影响流程）
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
%%% on_tool_call / on_tool_result 支持**两种 arity**，按注册函数自身的 arity 分派：
%%%
%%%   - 旧签名 Fun/2：on_tool_call(Name, Args) / on_tool_result(Name, Result)
%%%   - 新签名 Fun/3：末位额外收 Info —— Meta 加上本次调用的关联字段
%%%
%%% Info 的关联字段：
%%%   - on_tool_call  —— `tool_call_id`
%%%   - on_tool_result —— `tool_call_id`、`args`，工具失败时另有 `error`
%%%
%%% == 原始流事件（on_llm_event）==
%%%
%%% on_token 给的是归一化后的**文本**；on_llm_event 给的是 provider 原样解出的
%%% SSE chunk（binary 键，各家形状不同，不归一化）。tool_calls 的 arguments 增量、
%%% thinking / reasoning 增量、逐块 usage 只在后者里有——它们既不是文本、也在
%%% 汇聚成统一响应时被抹掉。要用就得按 provider 分支解读。
%%%
%%% **只在 beamai_agent:stream/2,3 下触发**：非流式的 run/2,3 根本没有流事件。
%%%
%%% == 工具回调的关联信息（Info）==
%%%
%%% `tool_call_id` 是把「哪次调用」和「哪个结果」配起来的**唯一**依据：
%%% parallel_tools 缺省开启，on_tool_result 谁先完成谁先触发，只靠工具名无法
%%% 配对（同名工具可在一批里出现多次）。需要按调用逐条上报进度的宿主
%%% （如 AG-UI 之类的前端事件流）必须用 Fun/3。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_callbacks).

-export([invoke/3, invoke/4, tool_gate/4, build_metadata/1]).

-export_type([callbacks/0]).

-type callbacks() :: #{
    on_turn_start  => fun((map()) -> ok),           %% 参数: 元数据 map
    on_turn_end    => fun((map()) -> ok),           %% 参数: 元数据 map
    on_turn_error  => fun((term(), map()) -> ok),   %% 参数: 错误原因, 元数据 map
    on_llm_call    => fun(([map()], map()) -> ok),  %% 参数: 消息列表, 元数据 map
    on_llm_result  => fun((map(), map()) -> ok),    %% 参数: 原始 response, 元数据 map
    on_llm_event   => fun((map(), map()) -> ok),    %% 参数: provider 原始流事件, 元数据 map
                                                    %% 仅 stream/2,3 触发
    %% 参数: 函数名, 调用参数[, Info]；返回 {interrupt, Reason} 可触发中断
    on_tool_call   => fun((binary(), map()) -> ok | {interrupt, term()})
                    | fun((binary(), map(), map()) -> ok | {interrupt, term()}),
    %% 参数: 函数名, 编码后的结果（binary）[, Info]
    on_tool_result => fun((binary(), binary()) -> ok)
                    | fun((binary(), binary(), map()) -> ok),
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
%%   - 回调执行中抛出任何异常均被捕获并返回 ok（记 warning 日志留痕）
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
        Fun -> safe_apply(Name, Fun, Args)
    end.

%% @doc 调用回调，按注册函数的 arity 决定是否附带关联信息
%%
%% 注册 Fun/length(Args) 按旧签名调用；注册 Fun/(length(Args)+1) 则把 Info
%% 追加为末参。旧宿主代码无需改动即可继续工作，新宿主换成多一个参数的
%% 函数就能拿到 tool_call_id 等关联字段（见模块头「工具回调的关联信息」）。
%%
%% @param Name 回调名称
%% @param Args 旧签名的参数列表
%% @param Info 关联信息 map（Meta + 本次调用的关联字段）
%% @param Callbacks 回调注册表 map
%% @returns ok（总是返回 ok）
-spec invoke(atom(), [term()], map(), callbacks()) -> ok.
invoke(Name, Args, Info, Callbacks) ->
    case maps:get(Name, Callbacks, undefined) of
        undefined -> ok;
        Fun -> safe_apply(Name, Fun, extend_args(Fun, Args, Info))
    end.

%% @doc 调用 on_tool_call 策略门并取回裁决
%%
%% 与 invoke/4 一样按 arity 兼容新旧签名，区别在于**保留返回值**：on_tool_call
%% 是唯一能影响流程的回调，返回 {interrupt, Reason} 即拦下这次调用。回调抛异常
%% 一律视为放行（ok），与「回调不打断主流程」的保证一致。
%%
%% @param Fun 已从注册表取出的 on_tool_call 回调
%% @param Name 工具名
%% @param Args 调用参数
%% @param Info 关联信息 map（Meta + tool_call_id）
%% @returns ok | {interrupt, Reason}
-spec tool_gate(function(), binary(), map(), map()) -> ok | {interrupt, term()}.
tool_gate(Fun, Name, Args, Info) ->
    try erlang:apply(Fun, extend_args(Fun, [Name, Args], Info)) of
        {interrupt, Reason} -> {interrupt, Reason};
        _ -> ok
    catch
        Class:Reason:Stack ->
            logger:warning("beamai_agent callback on_tool_call crashed: ~p:~p",
                           [Class, Reason], #{stacktrace => Stack}),
            ok
    end.

%% @private 注册的是多一个参数的函数就把 Info 追加为末参，否则按旧签名调用
extend_args(Fun, Args, Info) ->
    case erlang:is_function(Fun, length(Args) + 1) of
        true -> Args ++ [Info];
        false -> Args
    end.

%% @private 应用回调并吞掉异常（记 warning 留痕），确保不中断主流程
safe_apply(Name, Fun, Args) ->
    try
        _ = erlang:apply(Fun, Args),
        ok
    catch
        Class:Reason:Stack ->
            logger:warning("beamai_agent callback ~p crashed: ~p:~p",
                           [Name, Class, Reason], #{stacktrace => Stack}),
            ok
    end.

%% @doc 构建回调元数据
%%
%% 从 agent state 中提取关键信息，组装成标准化的元数据 map，
%% 传递给各回调函数作为上下文信息。
%%
%% 元数据包含：
%%   - agent_id: agent 唯一标识
%%   - agent_name: agent 名称
%%   - conversation_id: 当前会话标识
%%   - turn_count: 当前已完成的 turn 数
%%   - run_id: 本次 run 的唯一 ID（未在 run 中时 undefined）
%%   - timestamp: 当前时间戳（毫秒）
%%
%% @param AgentState agent 状态 map
%% @returns 元数据 map
-spec build_metadata(map()) -> map().
build_metadata(AgentState) ->
    #{agent_id => maps:get(id, AgentState),
      agent_name => maps:get(name, AgentState, <<>>),
      conversation_id => maps:get(conversation_id, AgentState, undefined),
      turn_count => maps:get(turn_count, AgentState, 0),
      run_id => maps:get(run_id, AgentState, undefined),
      timestamp => erlang:system_time(millisecond)}.
