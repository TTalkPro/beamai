%%%-------------------------------------------------------------------
%%% @doc 有状态多轮对话 Agent
%%%
%%% 封装 beamai_kernel，提供：
%%%   - 多轮对话管理（消息历史自动累积）
%%%   - 自实现 tool loop（确保 filters 完整触发）
%%%   - 6 个观察性回调（on_turn_start/end/error, on_llm_call, on_tool_call, on_token）
%%%   - 可选持久化（通过 beamai_memory）
%%%
%%% 核心设计决策：
%%%   - 不使用 kernel 的 invoke_chat_with_tools（它绕过 pre/post_chat filters）
%%%   - Agent 自己实现 tool loop，每次 LLM 调用和函数调用都经过完整 filter 管道
%%%   - 回调通过 kernel filter 注入（on_llm_call → pre_chat, on_tool_call → pre_invocation）
%%%   - Map-based 状态，无 Record 依赖，方便序列化和扩展
%%%
%%% 使用示例：
%%% ```
%%% {ok, Agent} = beamai_agent:new(#{
%%%     llm => {openai, #{model => <<"gpt-4">>}},
%%%     system_prompt => <<"You are a helpful assistant.">>
%%% }),
%%% {ok, Result, Agent1} = beamai_agent:run(Agent, <<"Hello">>),
%%% io:format("~s~n", [maps:get(content, Result)]).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent).

%% 构造
-export([new/1]).

%% 执行
-export([run/2, run/3]).
-export([stream/2, stream/3]).

%% 中断/恢复
-export([resume/2, resume_from_memory/3]).
-export([is_interrupted/1, get_interrupt_info/1]).

%% 查询
-export([messages/1, last_response/1, turn_count/1, kernel/1, id/1, name/1]).

%% 修改
-export([set_system_prompt/2, add_message/2, clear_messages/1, update_metadata/2]).

%% 持久化
-export([save/1, restore/2]).

-export_type([run_result/0, interrupt_info/0]).

-type run_result() :: #{
    content := binary(),                  %% LLM 最终回复文本
    tool_calls_made => [map()],           %% 本轮执行的所有 tool 调用记录
    finish_reason => binary(),            %% LLM 停止原因（如 <<"stop">>）
    usage => map(),                       %% token 使用统计
    iterations => non_neg_integer()       %% tool loop 迭代次数
}.

-type interrupt_info() :: #{
    reason := term(),                     %% 中断原因
    interrupt_type := tool_request | tool_result | callback,
    interrupted_tool_call => map(),       %% 触发中断的 tool_call
    completed_results => [map()],         %% 已完成的 tool 结果
    created_at := integer()
}.

%%====================================================================
%% 构造 API
%%====================================================================

%% @doc 创建新的 Agent 实例
%%
%% 从配置 map 构建完整的 agent 状态，包括 kernel 初始化、
%% callback filter 注入、默认值填充等。
%%
%% 详细配置选项参见 beamai_agent_state:create/1。
%%
%% @param Config 配置选项 map
%% @returns {ok, AgentState} 创建成功
%% @returns {error, Reason} 创建失败
-spec new(map()) -> {ok, beamai_agent_state:agent_state()} | {error, term()}.
new(Config) ->
    beamai_agent_state:create(Config).

%%====================================================================
%% 执行 API
%%====================================================================

%% @doc 执行一轮对话（默认选项）
%%
%% 将用户消息发送给 LLM，自动处理 tool calling 循环，
%% 返回最终回复和更新后的 agent 状态。
%%
%% @param State 当前 agent 状态
%% @param UserMessage 用户输入文本
%% @returns {ok, RunResult, NewState} 执行成功
%% @returns {error, Reason} 执行失败
-spec run(beamai_agent_state:agent_state(), binary()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} | {error, term()}.
run(State, UserMessage) ->
    run(State, UserMessage, #{}).

%% @doc 执行一轮对话（带选项）
%%
%% 执行流程：
%%   1. 触发 on_turn_start 回调
%%   2. 组装消息（system_prompt + 历史 + 当前用户消息）
%%   3. 进入 tool loop:
%%      a. 调用 kernel:invoke_chat 发送给 LLM（经过 pre/post_chat filters）
%%      b. 若 LLM 返回 tool_calls: 逐个 kernel:invoke 执行（经过 pre/post_invocation filters）
%%      c. 拼接 tool results 到消息，回到 (a)
%%      d. 若 LLM 返回文本: 终止循环
%%   4. 追加 user_msg + assistant_msg 到历史
%%   5. 触发 on_turn_end 回调
%%   6. 可选 auto_save
%%
%% 选项：
%%   chat_opts — 传递给 kernel invoke_chat 的额外选项
%%
%% @param State 当前 agent 状态
%% @param UserMessage 用户输入文本
%% @param Opts 执行选项 map
%% @returns {ok, RunResult, NewState} 执行成功
%% @returns {error, Reason} 执行失败
-spec run(beamai_agent_state:agent_state(), binary(), map()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} | {error, term()}.
run(State, UserMessage, Opts) ->
    #{callbacks := Callbacks, kernel := Kernel,
      max_tool_iterations := MaxIter} = State,

    %% 生成本轮 run_id
    RunId = generate_run_id(),
    State0 = State#{run_id => RunId},

    Meta = beamai_agent_callbacks:build_metadata(State0),
    beamai_agent_callbacks:invoke(on_turn_start, [Meta], Callbacks),

    UserMsg = #{role => user, content => UserMessage},
    Messages = beamai_agent_state:build_messages(State0, UserMsg),

    ChatOpts = build_chat_opts(State0, Opts),

    case tool_loop(Kernel, Messages, ChatOpts, Callbacks, Meta, MaxIter, [], State0) of
        {ok, Response, ToolCallsMade, Iterations} ->
            Content = extract_content(Response),
            AssistantMsg = #{role => assistant, content => Content},
            NewMessages = maps:get(messages, State0) ++ [UserMsg, AssistantMsg],
            NewState = State0#{
                messages => NewMessages,
                turn_count => maps:get(turn_count, State0) + 1
            },
            Result = #{
                content => Content,
                tool_calls_made => ToolCallsMade,
                finish_reason => maps:get(finish_reason, Response, <<>>),
                usage => maps:get(usage, Response, #{}),
                iterations => Iterations
            },
            EndMeta = Meta#{turn_count => maps:get(turn_count, NewState)},
            beamai_agent_callbacks:invoke(on_turn_end, [EndMeta], Callbacks),
            FinalState = maybe_auto_save(NewState),
            {ok, Result, FinalState};
        {interrupt, Type, Context} ->
            %% 中断：构建 interrupt_state 并保存
            handle_new_interrupt(State0, Type, Context, UserMsg, Callbacks, Meta);
        {error, Reason} ->
            beamai_agent_callbacks:invoke(on_turn_error, [Reason, Meta], Callbacks),
            {error, Reason}
    end.

%% @doc 流式执行一轮对话（默认选项）
%%
%% 与 run/2 功能相同，但最后一次 LLM 调用使用 streaming 模式，
%% 通过 on_token 回调逐 token 传递给用户。
%%
%% @param State 当前 agent 状态
%% @param UserMessage 用户输入文本
%% @returns {ok, RunResult, NewState} 执行成功
%% @returns {error, Reason} 执行失败
-spec stream(beamai_agent_state:agent_state(), binary()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} | {error, term()}.
stream(State, UserMessage) ->
    stream(State, UserMessage, #{}).

%% @doc 流式执行一轮对话（带选项）
%%
%% Tool-call 迭代使用普通 chat（需完整 response 解析 tool_calls），
%% 最后一次 LLM 调用（确认无更多 tool calls 后）使用 streaming。
%% Token 通过 on_token callback 传递给用户。
%%
%% @param State 当前 agent 状态
%% @param UserMessage 用户输入文本
%% @param Opts 执行选项 map
%% @returns {ok, RunResult, NewState} 执行成功
%% @returns {error, Reason} 执行失败
-spec stream(beamai_agent_state:agent_state(), binary(), map()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} | {error, term()}.
stream(State, UserMessage, Opts) ->
    #{callbacks := Callbacks, kernel := Kernel,
      max_tool_iterations := MaxIter} = State,

    RunId = generate_run_id(),
    State0 = State#{run_id => RunId},

    Meta = beamai_agent_callbacks:build_metadata(State0),
    beamai_agent_callbacks:invoke(on_turn_start, [Meta], Callbacks),

    UserMsg = #{role => user, content => UserMessage},
    Messages = beamai_agent_state:build_messages(State0, UserMsg),

    ChatOpts = build_chat_opts(State0, Opts),

    case stream_tool_loop(Kernel, Messages, ChatOpts, Callbacks, Meta, MaxIter, [], State0) of
        {ok, Response, ToolCallsMade, Iterations} ->
            Content = extract_content(Response),
            AssistantMsg = #{role => assistant, content => Content},
            NewMessages = maps:get(messages, State0) ++ [UserMsg, AssistantMsg],
            NewState = State0#{
                messages => NewMessages,
                turn_count => maps:get(turn_count, State0) + 1
            },
            Result = #{
                content => Content,
                tool_calls_made => ToolCallsMade,
                finish_reason => maps:get(finish_reason, Response, <<>>),
                usage => maps:get(usage, Response, #{}),
                iterations => Iterations
            },
            EndMeta = Meta#{turn_count => maps:get(turn_count, NewState)},
            beamai_agent_callbacks:invoke(on_turn_end, [EndMeta], Callbacks),
            FinalState = maybe_auto_save(NewState),
            {ok, Result, FinalState};
        {interrupt, Type, Context} ->
            handle_new_interrupt(State0, Type, Context, UserMsg, Callbacks, Meta);
        {error, Reason} ->
            beamai_agent_callbacks:invoke(on_turn_error, [Reason, Meta], Callbacks),
            {error, Reason}
    end.

%%====================================================================
%% 查询 API
%%====================================================================

%% @doc 获取对话消息历史
%%
%% 返回所有已累积的 user 和 assistant 消息列表。
%% 不包含 system_prompt（system_prompt 在每次调用时动态拼接）。
%%
%% @param State agent 状态
%% @returns 消息列表 [#{role => user|assistant, content => binary()}]
-spec messages(beamai_agent_state:agent_state()) -> [map()].
messages(#{messages := Msgs}) -> Msgs.

%% @doc 获取最后一条 assistant 响应
%%
%% 从消息历史末尾向前查找第一条 role=assistant 的消息内容。
%% 如果历史中没有 assistant 消息，返回 undefined。
%%
%% @param State agent 状态
%% @returns 最后回复内容（binary）或 undefined
-spec last_response(beamai_agent_state:agent_state()) -> binary() | undefined.
last_response(#{messages := Msgs}) ->
    find_last_assistant(lists:reverse(Msgs)).

%% @doc 获取已完成的对话 turn 数
%%
%% 每次 run/2 或 stream/2 成功完成后 turn_count 加 1。
%%
%% @param State agent 状态
%% @returns 非负整数
-spec turn_count(beamai_agent_state:agent_state()) -> non_neg_integer().
turn_count(#{turn_count := N}) -> N.

%% @doc 获取 agent 内部的 kernel 实例
%%
%% 可用于直接操作 kernel（如添加新 plugin、查看 tool schemas 等）。
%% 注意：修改后的 kernel 不会自动同步回 agent 状态。
%%
%% @param State agent 状态
%% @returns kernel 实例
-spec kernel(beamai_agent_state:agent_state()) -> beamai_kernel:kernel().
kernel(#{kernel := K}) -> K.

%% @doc 获取 agent 唯一标识
%%
%% 创建时自动生成或由用户通过 Config 中的 id 键指定。
%%
%% @param State agent 状态
%% @returns agent ID（binary）
-spec id(beamai_agent_state:agent_state()) -> binary().
id(#{id := Id}) -> Id.

%% @doc 获取 agent 名称
%%
%% 默认值为 <<"agent">>，可通过 Config 中的 name 键自定义。
%%
%% @param State agent 状态
%% @returns agent 名称（binary）
-spec name(beamai_agent_state:agent_state()) -> binary().
name(#{name := N}) -> N.

%%====================================================================
%% 修改 API
%%====================================================================

%% @doc 设置系统提示词
%%
%% 替换当前的系统提示词。新提示词将在下次 run/stream 调用时生效。
%% 传入 undefined 可清除系统提示词。
%%
%% @param State agent 状态
%% @param Prompt 新的系统提示词
%% @returns 更新后的 agent 状态
-spec set_system_prompt(beamai_agent_state:agent_state(), binary()) ->
    beamai_agent_state:agent_state().
set_system_prompt(State, Prompt) ->
    State#{system_prompt => Prompt}.

%% @doc 手动追加消息到历史
%%
%% 将一条消息追加到消息历史末尾。可用于注入上下文信息，
%% 如添加 assistant 角色的引导消息。
%%
%% @param State agent 状态
%% @param Msg 消息 map（需包含 role 和 content 键）
%% @returns 更新后的 agent 状态
-spec add_message(beamai_agent_state:agent_state(), map()) ->
    beamai_agent_state:agent_state().
add_message(#{messages := Msgs} = State, Msg) ->
    State#{messages => Msgs ++ [Msg]}.

%% @doc 清空消息历史
%%
%% 重置对话上下文，agent 将从全新对话开始。
%% 注意：不会重置 turn_count。
%%
%% @param State agent 状态
%% @returns 清空历史后的 agent 状态
-spec clear_messages(beamai_agent_state:agent_state()) ->
    beamai_agent_state:agent_state().
clear_messages(State) ->
    State#{messages => []}.

%% @doc 更新用户元数据（合并方式）
%%
%% 将新的元数据 map 合并到现有元数据中。
%% 使用 maps:merge/2，新值覆盖同名旧值。
%%
%% @param State agent 状态
%% @param New 要合并的新元数据 map
%% @returns 更新后的 agent 状态
-spec update_metadata(beamai_agent_state:agent_state(), map()) ->
    beamai_agent_state:agent_state().
update_metadata(#{metadata := Old} = State, New) ->
    State#{metadata => maps:merge(Old, New)}.

%%====================================================================
%% 持久化 API
%%====================================================================

%% @doc 保存 agent 状态到 memory
%%
%% 将当前消息历史、turn_count、metadata 等序列化并持久化。
%% 需要 agent 创建时配置了 memory 实例。
%%
%% @param State agent 状态
%% @returns ok | {error, Reason}
-spec save(beamai_agent_state:agent_state()) -> ok | {error, term()}.
save(State) ->
    beamai_agent_memory:save(State).

%% @doc 从 memory 恢复 agent 状态
%%
%% 使用原始 config 重建 agent，然后用 checkpoint 数据恢复运行时状态。
%%
%% @param Config agent 配置（用于重建 kernel 等）
%% @param Memory memory 实例
%% @returns {ok, AgentState} | {error, Reason}
-spec restore(map(), term()) -> {ok, beamai_agent_state:agent_state()} | {error, term()}.
restore(Config, Memory) ->
    beamai_agent_memory:restore(Config, Memory).

%%====================================================================
%% 中断/恢复 API
%%====================================================================

%% @doc 恢复中断的 agent 继续执行
%%
%% 从当前 agent 状态中的 interrupt_state 恢复执行：
%%   1. 验证输入
%%   2. 构建恢复消息（中断时的消息 + 人类输入作为 tool result）
%%   3. 清除中断状态
%%   4. 继续 tool loop
%%
%% @param Agent 带有 interrupt_state 的 agent 状态
%% @param HumanInput 人类输入（binary 或 map）
%% @returns {ok, RunResult, NewState} | {interrupt, InterruptInfo, NewState} | {error, Reason}
-spec resume(beamai_agent_state:agent_state(), term()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.
resume(#{interrupt_state := undefined}, _HumanInput) ->
    {error, not_interrupted};
resume(#{interrupt_state := IntState} = Agent, HumanInput) ->
    %% 1. 验证输入
    case beamai_agent_interrupt:validate_resume_input(IntState, HumanInput) of
        {error, _} = Err -> Err;
        ok ->
            #{callbacks := Callbacks, kernel := Kernel,
              max_tool_iterations := MaxIter} = Agent,

            Meta = beamai_agent_callbacks:build_metadata(Agent),
            beamai_agent_callbacks:invoke(on_resume, [IntState, Meta], Callbacks),

            %% 2. 构建恢复消息
            ResumeMessages = beamai_agent_interrupt:build_resume_messages(IntState, HumanInput),

            %% 3. 清除中断状态
            Agent1 = Agent#{interrupt_state => undefined},

            %% 4. 继续 tool loop
            #{iteration := Iter, tool_calls_made := PrevCalls} = IntState,
            RemainingIter = MaxIter - Iter,
            ChatOpts = build_chat_opts(Agent1, #{}),

            case tool_loop(Kernel, ResumeMessages, ChatOpts, Callbacks, Meta,
                          RemainingIter, PrevCalls, Agent1) of
                {ok, Response, AllToolCalls, Iterations} ->
                    Content = extract_content(Response),
                    AssistantMsg = #{role => assistant, content => Content},
                    NewMessages = maps:get(messages, Agent1) ++ [AssistantMsg],
                    NewState = Agent1#{
                        messages => NewMessages,
                        turn_count => maps:get(turn_count, Agent1) + 1
                    },
                    Result = #{
                        content => Content,
                        tool_calls_made => AllToolCalls,
                        finish_reason => maps:get(finish_reason, Response, <<>>),
                        usage => maps:get(usage, Response, #{}),
                        iterations => Iterations
                    },
                    EndMeta = Meta#{turn_count => maps:get(turn_count, NewState)},
                    beamai_agent_callbacks:invoke(on_turn_end, [EndMeta], Callbacks),
                    FinalState = maybe_auto_save(NewState),
                    {ok, Result, FinalState};
                {interrupt, Type, Context} ->
                    %% 再次中断
                    UserMsg = #{role => user, content => <<"[resume]">>},
                    handle_new_interrupt(Agent1, Type, Context, UserMsg, Callbacks, Meta);
                {error, Reason} ->
                    beamai_agent_callbacks:invoke(on_turn_error, [Reason, Meta], Callbacks),
                    {error, Reason}
            end
    end.

%% @doc 从 memory 加载并恢复中断的 agent
%%
%% 加载最新的 checkpoint（含 interrupt_state），然后调用 resume/2。
%%
%% @param Config agent 配置
%% @param Memory memory 实例
%% @param HumanInput 人类输入
%% @returns {ok, RunResult, NewState} | {interrupt, InterruptInfo, NewState} | {error, Reason}
-spec resume_from_memory(map(), term(), term()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.
resume_from_memory(Config, Memory, HumanInput) ->
    case beamai_agent_memory:restore(Config, Memory) of
        {ok, Agent} ->
            case is_interrupted(Agent) of
                true -> resume(Agent, HumanInput);
                false -> {error, not_interrupted}
            end;
        {error, _} = Err -> Err
    end.

%% @doc 判断 agent 是否处于中断状态
%%
%% @param State agent 状态
%% @returns boolean()
-spec is_interrupted(beamai_agent_state:agent_state()) -> boolean().
is_interrupted(#{interrupt_state := undefined}) -> false;
is_interrupted(#{interrupt_state := #{status := interrupted}}) -> true;
is_interrupted(_) -> false.

%% @doc 获取中断信息
%%
%% 从 interrupt_state 中提取用户可见的中断信息。
%%
%% @param State agent 状态
%% @returns interrupt_info() | undefined
-spec get_interrupt_info(beamai_agent_state:agent_state()) -> interrupt_info() | undefined.
get_interrupt_info(#{interrupt_state := undefined}) ->
    undefined;
get_interrupt_info(#{interrupt_state := IntState}) ->
    #{
        reason => maps:get(reason, IntState),
        interrupt_type => maps:get(interrupt_type, IntState),
        interrupted_tool_call => maps:get(interrupted_tool_call, IntState, undefined),
        completed_results => maps:get(completed_tool_results, IntState, []),
        created_at => maps:get(created_at, IntState)
    };
get_interrupt_info(_) ->
    undefined.

%%====================================================================
%% 内部函数 - Tool Loop
%%====================================================================

%% @private 自实现 tool loop（普通模式）
%%
%% 核心循环逻辑：
%%   1. 调用 invoke_chat 发送消息给 LLM（经过 pre/post_chat filters）
%%   2. 检查响应中是否包含 tool_calls
%%      - 有 tool_calls: 执行所有工具，拼接结果到消息，递归
%%      - 无 tool_calls（纯文本响应）: 返回最终结果
%%   3. 迭代次数用尽时返回 max_tool_iterations 错误
%%
%% 不使用 kernel 的 invoke_chat_with_tools，因为它内部直接调用
%% beamai_chat_completion:chat，绕过了 pre_chat/post_chat filters。
%%
%% @param Kernel kernel 实例
%% @param Msgs 当前消息列表
%% @param Opts chat 选项（含 tools spec）
%% @param Callbacks 回调表（此处未直接使用，回调通过 filter 触发）
%% @param Meta 元数据
%% @param N 剩余迭代次数
%% @param ToolCallsMade 已执行的 tool 调用记录
%% @returns {ok, Response, ToolCallsMade, Iterations} | {error, Reason}
tool_loop(_Kernel, _Msgs, _Opts, _Callbacks, _Meta, 0, ToolCallsMade, _Agent) ->
    {error, {max_tool_iterations, ToolCallsMade}};
tool_loop(Kernel, Msgs, Opts, Callbacks, Meta, N, ToolCallsMade, Agent) ->
    #{max_tool_iterations := MaxIter} = Agent,
    case beamai_kernel:invoke_chat(Kernel, Msgs, Opts) of
        {ok, #{tool_calls := TCs} = _Response, _Ctx} when is_list(TCs), TCs =/= [] ->
            %% 1. 检查是否有 interrupt tool
            case beamai_agent_interrupt:find_interrupt_tool(TCs, Agent) of
                {yes, InterruptToolCall, OtherCalls} ->
                    %% 先执行非中断 tools
                    {OtherResults, OtherCallRecords} = execute_tools(Kernel, OtherCalls),
                    AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
                    Context = #{
                        pending_messages => Msgs,
                        assistant_response => AssistantMsg,
                        completed_tool_results => OtherResults,
                        interrupted_tool_call => InterruptToolCall,
                        iteration => MaxIter - N,
                        tool_calls_made => ToolCallsMade ++ OtherCallRecords
                    },
                    %% 提取中断原因（从 tool_call 的参数中）
                    Reason = extract_interrupt_reason(InterruptToolCall),
                    {interrupt, tool_request, Context#{reason => Reason}};
                no ->
                    %% 2. 检查 callback 是否触发中断
                    case check_callback_interrupt(TCs, Callbacks, Agent) of
                        {interrupt, CallbackReason, InterruptedTC} ->
                            AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
                            Context = #{
                                pending_messages => Msgs,
                                assistant_response => AssistantMsg,
                                completed_tool_results => [],
                                interrupted_tool_call => InterruptedTC,
                                iteration => MaxIter - N,
                                tool_calls_made => ToolCallsMade,
                                reason => CallbackReason
                            },
                            {interrupt, callback, Context};
                        ok ->
                            %% 3. 执行 tools，检查结果是否有中断
                            case execute_tools_with_interrupt_check(Kernel, TCs) of
                                {ok, ToolResults, NewToolCalls} ->
                                    AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
                                    NewMsgs = Msgs ++ [AssistantMsg | ToolResults],
                                    tool_loop(Kernel, NewMsgs, Opts, Callbacks, Meta, N - 1,
                                              ToolCallsMade ++ NewToolCalls, Agent);
                                {interrupt, IntReason, PartialResults, InterruptedTC, CompletedCalls} ->
                                    AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
                                    Context = #{
                                        pending_messages => Msgs,
                                        assistant_response => AssistantMsg,
                                        completed_tool_results => PartialResults,
                                        interrupted_tool_call => InterruptedTC,
                                        iteration => MaxIter - N,
                                        tool_calls_made => ToolCallsMade ++ CompletedCalls,
                                        reason => IntReason
                                    },
                                    {interrupt, tool_result, Context}
                            end
                    end
            end;
        {ok, Response, _Ctx} ->
            %% LLM 返回纯文本响应：tool loop 终止
            Iters = case ToolCallsMade of
                [] -> 1;
                _ -> length(ToolCallsMade) + 1
            end,
            {ok, Response, ToolCallsMade, Iters};
        {error, _} = Err ->
            Err
    end.

%% @private 流式 tool loop
%%
%% Tool-call 迭代使用普通 chat（需要完整 response 才能解析 tool_calls），
%% 确认不再有 tool calls 后，最后一次 LLM 调用使用 streaming 模式。
%%
%% 流程：
%%   1. 先用普通 invoke_chat 探测是否有 tool_calls
%%   2. 有 tool_calls: 执行工具，递归（同 tool_loop）
%%   3. 无 tool_calls: 切换到 stream_final_call 进行流式调用
%%
%% @param Kernel kernel 实例
%% @param Msgs 当前消息列表
%% @param Opts chat 选项
%% @param Callbacks 回调表（on_token 在 stream_final_call 中使用）
%% @param Meta 元数据
%% @param N 剩余迭代次数
%% @param ToolCallsMade 已执行的 tool 调用记录
%% @returns {ok, Response, ToolCallsMade, Iterations} | {error, Reason}
stream_tool_loop(_Kernel, _Msgs, _Opts, _Callbacks, _Meta, 0, ToolCallsMade, _Agent) ->
    {error, {max_tool_iterations, ToolCallsMade}};
stream_tool_loop(Kernel, Msgs, Opts, Callbacks, Meta, N, ToolCallsMade, Agent) ->
    #{max_tool_iterations := MaxIter} = Agent,
    case beamai_kernel:invoke_chat(Kernel, Msgs, Opts) of
        {ok, #{tool_calls := TCs} = _Response, _Ctx} when is_list(TCs), TCs =/= [] ->
            %% 检查中断（与 tool_loop 相同逻辑）
            case beamai_agent_interrupt:find_interrupt_tool(TCs, Agent) of
                {yes, InterruptToolCall, OtherCalls} ->
                    {OtherResults, OtherCallRecords} = execute_tools(Kernel, OtherCalls),
                    AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
                    Reason = extract_interrupt_reason(InterruptToolCall),
                    Context = #{
                        pending_messages => Msgs,
                        assistant_response => AssistantMsg,
                        completed_tool_results => OtherResults,
                        interrupted_tool_call => InterruptToolCall,
                        iteration => MaxIter - N,
                        tool_calls_made => ToolCallsMade ++ OtherCallRecords,
                        reason => Reason
                    },
                    {interrupt, tool_request, Context};
                no ->
                    case check_callback_interrupt(TCs, Callbacks, Agent) of
                        {interrupt, CallbackReason, InterruptedTC} ->
                            AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
                            Context = #{
                                pending_messages => Msgs,
                                assistant_response => AssistantMsg,
                                completed_tool_results => [],
                                interrupted_tool_call => InterruptedTC,
                                iteration => MaxIter - N,
                                tool_calls_made => ToolCallsMade,
                                reason => CallbackReason
                            },
                            {interrupt, callback, Context};
                        ok ->
                            case execute_tools_with_interrupt_check(Kernel, TCs) of
                                {ok, ToolResults, NewToolCalls} ->
                                    AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
                                    NewMsgs = Msgs ++ [AssistantMsg | ToolResults],
                                    stream_tool_loop(Kernel, NewMsgs, Opts, Callbacks, Meta, N - 1,
                                                    ToolCallsMade ++ NewToolCalls, Agent);
                                {interrupt, IntReason, PartialResults, InterruptedTC, CompletedCalls} ->
                                    AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
                                    Context = #{
                                        pending_messages => Msgs,
                                        assistant_response => AssistantMsg,
                                        completed_tool_results => PartialResults,
                                        interrupted_tool_call => InterruptedTC,
                                        iteration => MaxIter - N,
                                        tool_calls_made => ToolCallsMade ++ CompletedCalls,
                                        reason => IntReason
                                    },
                                    {interrupt, tool_result, Context}
                            end
                    end
            end;
        {ok, _Response, _Ctx} ->
            %% 无更多 tool calls：切换到流式模式进行最终调用
            case stream_final_call(Kernel, Msgs, Opts, Callbacks, Meta) of
                {ok, StreamResponse} ->
                    TotalIterations = case ToolCallsMade of
                        [] -> 1;
                        _ -> length(ToolCallsMade) + 1
                    end,
                    {ok, StreamResponse, ToolCallsMade, TotalIterations};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% @private 流式最终 LLM 调用
%%
%% 使用 beamai_chat_completion:stream_chat 进行流式调用，
%% 每收到一个 token 通过 on_token 回调传递给用户。
%%
%% @param Kernel kernel 实例
%% @param Msgs 最终消息列表
%% @param Opts chat 选项
%% @param Callbacks 回调表（需包含 on_token）
%% @param Meta 元数据（传递给 on_token）
%% @returns {ok, Response} | {error, Reason}
stream_final_call(Kernel, Msgs, Opts, Callbacks, Meta) ->
    case beamai_kernel:get_service(Kernel) of
        {ok, LlmConfig} ->
            TokenCb = fun(Token) ->
                beamai_agent_callbacks:invoke(on_token, [Token, Meta], Callbacks)
            end,
            beamai_chat_completion:stream_chat(LlmConfig, Msgs, TokenCb, Opts);
        error ->
            {error, no_llm_service}
    end.

%% @private 执行 tool calls 并收集结果
%%
%% 遍历 LLM 返回的所有 tool_call，逐个：
%%   1. 解析 tool_call 获取 ID、函数名、参数
%%   2. 调用 kernel:invoke 执行函数（经过 pre/post_invocation filters）
%%   3. 将执行结果编码为 JSON 字符串
%%   4. 构建 tool 角色消息和调用记录
%%
%% @param Kernel kernel 实例
%% @param ToolCalls LLM 返回的 tool_call 列表
%% @returns {ToolResultMsgs, CallRecords}
%%   ToolResultMsgs: tool 角色消息列表，用于拼接到下一轮消息中
%%   CallRecords: 调用记录列表，用于返回给用户
execute_tools(Kernel, ToolCalls) ->
    lists:foldl(fun(TC, {ResultsAcc, CallsAcc}) ->
        {Id, Name, Args} = beamai_function:parse_tool_call(TC),
        Result = case beamai_kernel:invoke_tool(Kernel, Name, Args, beamai_context:new()) of
            {ok, Value, _Ctx} -> beamai_function:encode_result(Value);
            {error, Reason} -> beamai_function:encode_result(#{error => Reason})
        end,
        Msg = #{role => tool, tool_call_id => Id, content => Result},
        CallRecord = #{name => Name, args => Args, result => Result, tool_call_id => Id},
        {ResultsAcc ++ [Msg], CallsAcc ++ [CallRecord]}
    end, {[], []}, ToolCalls).

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 构建 chat 选项
%%
%% 从 kernel 获取所有已注册函数的 tool specs，自动添加到 chat 选项中。
%% 如果 kernel 中没有注册任何函数，不添加 tools 字段。
%%
%% @param Kernel kernel 实例
%% @param Opts 用户传入的选项（可通过 chat_opts 键传递额外参数）
%% @returns 完整的 chat 选项 map
build_chat_opts(#{kernel := Kernel} = Agent, Opts) ->
    ToolSpecs = beamai_kernel:get_tool_specs(Kernel),
    InterruptSpecs = beamai_agent_interrupt:get_interrupt_tool_specs(Agent),
    AllSpecs = ToolSpecs ++ InterruptSpecs,
    BaseChatOpts = maps:get(chat_opts, Opts, #{}),
    case AllSpecs of
        [] -> BaseChatOpts;
        _ -> BaseChatOpts#{
            tools => AllSpecs,
            tool_choice => maps:get(tool_choice, BaseChatOpts, auto)
        }
    end.

%% @private 从 LLM 响应中提取文本内容
%%
%% 处理三种情况：
%%   - content 为 binary: 直接返回
%%   - content 为 null: 返回空二进制（tool_calls 响应时常见）
%%   - 其他情况: 返回空二进制
extract_content(#{content := Content}) when is_binary(Content) -> Content;
extract_content(#{content := null}) -> <<>>;
extract_content(#{}) -> <<>>.

%% @private 从消息列表中查找最后一条 assistant 消息
%%
%% 从列表末尾（已反转）向前查找 role=assistant 的消息。
find_last_assistant([]) -> undefined;
find_last_assistant([#{role := assistant, content := C} | _]) -> C;
find_last_assistant([_ | Rest]) -> find_last_assistant(Rest).

%% @private 可选自动保存
%%
%% 如果 agent 配置了 auto_save=true，每轮结束后自动调用 memory:save。
%% 保存失败时静默忽略（不影响执行结果）。
maybe_auto_save(#{auto_save := true} = State) ->
    case beamai_agent_memory:save(State) of
        ok -> State;
        {error, _} -> State
    end;
maybe_auto_save(State) ->
    State.

%%====================================================================
%% 内部函数 - 中断支持
%%====================================================================

%% @private 处理新的中断
%%
%% 构建 interrupt_state，触发 on_interrupt callback，
%% 可选 auto_save，返回 {interrupt, Info, State}
handle_new_interrupt(Agent, Type, Context, _UserMsg, Callbacks, Meta) ->
    Reason = maps:get(reason, Context, undefined),
    {IntState, Agent1} = beamai_agent_interrupt:handle_interrupt(Type, Reason, Context, Agent),
    beamai_agent_callbacks:invoke(on_interrupt, [IntState, Meta], Callbacks),
    Agent2 = maybe_auto_save(Agent1),
    Info = #{
        reason => Reason,
        interrupt_type => Type,
        interrupted_tool_call => maps:get(interrupted_tool_call, Context, undefined),
        completed_results => maps:get(completed_tool_results, Context, []),
        created_at => maps:get(created_at, IntState)
    },
    {interrupt, Info, Agent2}.

%% @private 检查 on_tool_call callback 是否触发中断
%%
%% 遍历 tool_calls，对每个调用触发 on_tool_call callback。
%% 如果 callback 返回 {interrupt, Reason}，中断执行。
check_callback_interrupt(ToolCalls, Callbacks, _Agent) ->
    OnToolCall = maps:get(on_tool_call, Callbacks, undefined),
    case OnToolCall of
        undefined -> ok;
        Fun ->
            check_callback_interrupt_loop(ToolCalls, Fun)
    end.

check_callback_interrupt_loop([], _Fun) ->
    ok;
check_callback_interrupt_loop([TC | Rest], Fun) ->
    {_Id, Name, Args} = beamai_function:parse_tool_call(TC),
    case catch Fun(Name, Args) of
        {interrupt, Reason} ->
            {interrupt, Reason, TC};
        _ ->
            check_callback_interrupt_loop(Rest, Fun)
    end.

%% @private 执行 tools 并检查执行结果中的中断信号
%%
%% 逐个执行 tool_calls，如果某个 tool 返回 {interrupt, Reason, PartialResult}，
%% 则停止执行并返回中断信息。
execute_tools_with_interrupt_check(Kernel, ToolCalls) ->
    execute_tools_with_interrupt_check(Kernel, ToolCalls, [], []).

execute_tools_with_interrupt_check(_Kernel, [], ResultsAcc, CallsAcc) ->
    {ok, lists:reverse(ResultsAcc), lists:reverse(CallsAcc)};
execute_tools_with_interrupt_check(Kernel, [TC | Rest], ResultsAcc, CallsAcc) ->
    {Id, Name, Args} = beamai_function:parse_tool_call(TC),
    case beamai_kernel:invoke_tool(Kernel, Name, Args, beamai_context:new()) of
        {ok, Value, _Ctx} ->
            Result = beamai_function:encode_result(Value),
            Msg = #{role => tool, tool_call_id => Id, content => Result},
            CallRecord = #{name => Name, args => Args, result => Result, tool_call_id => Id},
            execute_tools_with_interrupt_check(Kernel, Rest,
                [Msg | ResultsAcc], [CallRecord | CallsAcc]);
        {interrupt, Reason, PartialResult} ->
            %% Tool 执行返回中断信号
            PartialMsg = #{role => tool, tool_call_id => Id,
                          content => beamai_function:encode_result(PartialResult)},
            {interrupt, Reason,
             lists:reverse([PartialMsg | ResultsAcc]),
             TC,
             lists:reverse(CallsAcc)};
        {error, Reason} ->
            Result = beamai_function:encode_result(#{error => Reason}),
            Msg = #{role => tool, tool_call_id => Id, content => Result},
            CallRecord = #{name => Name, args => Args, result => Result, tool_call_id => Id},
            execute_tools_with_interrupt_check(Kernel, Rest,
                [Msg | ResultsAcc], [CallRecord | CallsAcc])
    end.

%% @private 从 interrupt tool_call 中提取中断原因
extract_interrupt_reason(#{function := #{arguments := Args}}) when is_map(Args) ->
    Args;
extract_interrupt_reason(#{<<"function">> := #{<<"arguments">> := Args}}) when is_map(Args) ->
    Args;
extract_interrupt_reason(TC) ->
    {_Id, Name, Args} = beamai_function:parse_tool_call(TC),
    #{tool => Name, arguments => Args}.

%% @private 生成 run_id
generate_run_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("run_~16.16.0b_~4.16.0b", [Ts, Rand])).
