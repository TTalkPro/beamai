%%%-------------------------------------------------------------------
%%% @doc 有状态多轮对话 Agent（ReAct）
%%%
%%% 封装 beamai_kernel，提供：
%%%   - 多轮对话管理（跨轮历史由 memory provider 按 conversation_id 维护）
%%%   - 自实现 tool loop（full-messages 模式，自管编排记忆与回调）
%%%   - 9 个观察性回调（on_turn_start/end/error, on_llm_call, on_tool_call,
%%%     on_tool_result, on_token, on_interrupt, on_resume）
%%%
%%% 核心设计决策（Agent 自管编排，不借道 kernel filter）：
%%%   - **记忆是 Agent 自己的接口**（beamai_memory_provider），由 tool loop 显式调用
%%%     （history 载入跨轮 / append 持久化 / prepare 发送前变换），**不**注入 kernel
%%%     filter。within-run 消息由 loop 累积，cross-run 由 provider 持久化。
%%%   - **回调是 Agent 自己的接口**，全部在 tool loop 里直接触发，**不**注入 kernel
%%%     filter（取消了曾经的 on_llm_call around_chat 注入）。
%%%   - kernel 仍是纯原语（invoke_chat/invoke_tool + filter 给"直接用 kernel 的人"；
%%%     预构建 kernel 的 filter 在 beamai_kernel:new/2 一次性给出，注册顺序即层序）。
%%%   - Map-based 状态，无 Record 依赖，方便序列化（memory provider 为 {Mod, Ref}，
%%%     conversation_id 为 binary，均可序列化）
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
-export([resume/2, resume/3]).
-export([is_interrupted/1, get_interrupt_info/1]).

%% 查询
-export([messages/1, last_response/1, turn_count/1, kernel/1, id/1, name/1]).

%% 修改
-export([set_system_prompt/2, add_message/2, clear_messages/1, update_metadata/2]).

-export_type([run_result/0, interrupt_info/0]).

-type run_result() :: #{
    content := binary(),                  %% LLM 最终回复文本
    tool_calls_made => [map()],           %% 本轮执行的所有 tool 调用记录
    finish_reason => beamai_llm_response:finish_reason(), %% LLM 停止原因（如 complete）
    usage => map(),                       %% token 使用统计
    iterations => non_neg_integer()       %% tool loop 迭代次数
}.

-type interrupt_info() :: #{
    reason := term(),                     %% 中断原因
    interrupt_type := tool_request | tool_result | callback | env_retry,
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
%% memory provider 解析、默认值填充等。kernel 与 memory 为两个
%% 正交的创建参数；callbacks 是 agent 唯一的观察扩展点。
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
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.
run(State, UserMessage) ->
    run(State, UserMessage, #{}).

%% @doc 执行一轮对话（带选项）
%%
%% 执行流程（Agent 自管编排，记忆统一由 tool_loop 经 memory provider 处理）：
%%   1. 触发 on_turn_start 回调
%%   2. 进入 tool loop（声明本轮新增 = 用户消息，并要求载入跨轮历史）:
%%      a. tool_loop 持久化新消息、前接跨轮历史，组装本轮完整 messages
%%      b. 每轮：prepare 变换 → invoke_chat(_stream) → assistant 回合并入并持久化
%%      c. 有 tool_calls 则执行（结果并入并持久化）回到 (b)；纯文本则终止
%%   3. turn_count + 1
%%   4. 触发 on_turn_end 回调
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
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.
run(State, UserMessage, Opts) ->
    #{callbacks := Callbacks} = State,

    %% 生成本轮 run_id；若上一个中断被放弃（未 resume 直接开新 chat），先 heal
    %% 历史尾部悬空的 assistant(tool_calls)——补「已取消」结果 + 清中断态/暂停快照，
    %% 避免 provider 拒绝残缺历史。
    RunId = beamai_id:gen_id(<<"run">>),
    State0 = heal_dangling(State#{run_id => RunId}),

    Meta = beamai_agent_callbacks:build_metadata(State0),
    beamai_agent_callbacks:invoke(on_turn_start, [Meta], Callbacks),

    %% 记忆编排（载入跨轮历史 + 持久化新消息）统一由 tool_loop 负责，
    %% 这里只声明本轮新增消息并要求载入历史。整个工具循环包在 turn 洋葱内：
    %% turn filter 可改写入口消息（RAG 前置）、around（最终答案 guardrail/预算）、
    %% 多次重入（校验重试/evaluator）。terminal = 一轮完整 tool loop。
    UserMsg = #{role => user, content => UserMessage},
    TurnReq = #{messages => [UserMsg],
                context => initial_turn_context(State0),
                resume => false},
    Terminal = fun(TReq) ->
        run_loop(State0,
                 #{new => maps:get(messages, TReq),
                   load_history => maps:get(load_history, TReq, true)},
                 [], Opts#{turn_context => maps:get(context, TReq)})
    end,
    dispatch_turn_result(State0, run_turn_chain(State0, TurnReq, Terminal),
                         Callbacks, Meta).

%% @private turn 洋葱：把整个工具循环包进 around_turn filter 链
%%
%% 复用 beamai_filter_chain（Phase=around_turn）。TurnResult 为工具循环结果 tuple，
%% filter 直接模式匹配（硬规则：interrupt/error 透传不重入）。filter/terminal 抛出
%% 由链统一捕获为 {error, Reason}。
run_turn_chain(#{kernel := #{filters := Filters}}, TurnReq, Terminal) ->
    case beamai_filter_chain:run(Filters, around_turn, Terminal, TurnReq) of
        {ok, TurnResult} -> TurnResult;
        {error, Reason} -> {error, Reason}
    end.

%% @private turn 入口初始 context（只读环境：conversation_id；首轮 state 空）
initial_turn_context(State) ->
    beamai_context:with_conversation_id(
        beamai_context:new(), beamai_agent_state:conversation_id(State)).

%% @private 把工具循环结果 tuple 分派为 run/resume 的最终返回
dispatch_turn_result(State0, TurnResult, Callbacks, Meta) ->
    case TurnResult of
        %% Messages（第 5 元）只服务于 turn filter 重入，agent 的最终返回用不上
        {ok, Response, ToolCallsMade, Iterations, _Messages} ->
            finalize_turn(State0, Response, ToolCallsMade, Iterations);
        {interrupt, Type, Context} ->
            UserMsg = #{role => user, content => <<"[turn]">>},
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
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.
stream(State, UserMessage) ->
    stream(State, UserMessage, #{}).

%% @doc 流式执行一轮对话（带选项）
%%
%% 与 run/3 同样经 tool loop（full-messages 模式）完成工具循环，区别在于循环中
%% 的每次 LLM 调用走 provider 级 streaming（kernel:invoke_chat_stream）：文本
%% token 经 on_token callback **实时**逐 token 推送，而非等整轮完成后再分块。
%% 工具调用轮通常无文本内容（content=null），自然不产生 token；最终文本回合
%% 即逐 token 流出。
%%
%% 要求所选 provider 的 stream_chat 返回汇聚后的统一响应（openai/anthropic/
%% deepseek 已支持；其余 provider 视其流式 finalize 完成度）。
%%
%% @param State 当前 agent 状态
%% @param UserMessage 用户输入文本
%% @param Opts 执行选项 map
%% @returns {ok, RunResult, NewState} 执行成功
%% @returns {error, Reason} 执行失败
-spec stream(beamai_agent_state:agent_state(), binary(), map()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.
stream(State, UserMessage, Opts) ->
    #{callbacks := Callbacks} = State,
    Meta = beamai_agent_callbacks:build_metadata(State),
    %% 桥接：kernel 逐 token 回调 → agent on_token 回调
    TokenHandler = fun(Token) -> emit_tokens(Token, Meta, Callbacks) end,
    run(State, UserMessage, Opts#{stream_token_handler => TokenHandler}).

%%====================================================================
%% 查询 API
%%====================================================================

%% @doc 获取对话消息历史
%%
%% 从 memory provider 按 conversation_id 读取完整历史（正序）。
%% 历史含 user / assistant 消息，以及工具循环中的 assistant(tool_calls) 与
%% tool 结果消息。不包含 system_prompt（每次调用时动态拼接、不入存储）。
%% 未启用记忆（memory => false）时返回 []。
%%
%% @param State agent 状态
%% @returns 消息列表
-spec messages(beamai_agent_state:agent_state()) -> [map()].
messages(State) ->
    case beamai_agent_state:memory(State) of
        undefined -> [];
        Memory -> beamai_memory_provider:history(Memory, beamai_agent_state:conversation_id(State))
    end.

%% @doc 获取最后一条 assistant 文本响应
%%
%% 从历史末尾向前查找第一条 role=assistant 且 content 非 null 的消息内容
%% （跳过仅含 tool_calls 的 assistant 消息）。无则返回 undefined。
%%
%% @param State agent 状态
%% @returns 最后回复内容（binary）或 undefined
-spec last_response(beamai_agent_state:agent_state()) -> binary() | undefined.
last_response(State) ->
    find_last_assistant(lists:reverse(messages(State))).

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
%% 经 memory provider 将一条消息追加到本会话历史末尾。可用于注入上下文
%% 信息，如添加 assistant 角色的引导消息。未启用记忆时为 no-op。
%% agent 状态本身不可变，历史变更落在记忆里，故返回原 State。
%%
%% @param State agent 状态
%% @param Msg 消息 map（需包含 role 和 content 键）
%% @returns agent 状态（不变）
-spec add_message(beamai_agent_state:agent_state(), map()) ->
    beamai_agent_state:agent_state().
add_message(State, Msg) ->
    case beamai_agent_state:memory(State) of
        undefined -> ok;
        Memory -> beamai_memory_provider:append(Memory, beamai_agent_state:conversation_id(State), [Msg])
    end,
    State.

%% @doc 清空消息历史
%%
%% 经 memory provider 清空本会话历史，agent 将从全新对话开始。未启用记忆时为 no-op。
%% 注意：不会重置 turn_count；agent 状态不可变，故返回原 State。
%%
%% @param State agent 状态
%% @returns agent 状态（不变）
-spec clear_messages(beamai_agent_state:agent_state()) ->
    beamai_agent_state:agent_state().
clear_messages(State) ->
    case beamai_agent_state:memory(State) of
        undefined -> ok;
        Memory -> beamai_memory_provider:clear(Memory, beamai_agent_state:conversation_id(State))
    end,
    State.

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
resume(Agent, Decision) ->
    resume(Agent, Decision, #{}).

%% @doc 从中断恢复执行（带 payload）
%%
%% Decision + Payload 语义（见 design/hitl_timeline_serial_errors.md §4）：
%%   审批暂停：
%%     `<<"approved">>` [+ #{args=>新参}] — 原/替换参数执行被中断的工具；
%%     其它（拒绝） [+ #{message=>理由}] — 结果「已拒绝执行[：理由]」回模型；
%%     `<<"reply">>` + #{message=>答复}（必填） — 工具不执行，答复即结果（ask-user）。
%%   环境暂停（phase=env_retry）：
%%     `<<"retry">>`/`<<"approved">>` — 只重跑失败调用、按 id 替换结果（再失败再暂停）；
%%     其它 — proceed：原错误结果照常交模型；`<<"reply">>` 显式拒收。
%%   兼容：旧 `resume(Agent, <<"文本">>)` 等价于审批暂停下"人类输入作为被中断工具结果"。
-spec resume(beamai_agent_state:agent_state(), term(), map()) ->
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.
resume(#{interrupt_state := undefined} = Agent, Decision, Payload) ->
    %% 本进程无中断态：透明回落 pause_store（跨重启恢复）
    case beamai_agent_pause:load(Agent) of
        {ok, Restored} -> resume(Restored, Decision, Payload);
        none -> {error, not_interrupted}
    end;
resume(#{interrupt_state := IntState} = Agent, Decision, Payload) ->
    case beamai_agent_interrupt:validate_resume_input(IntState, Decision) of
        {error, _} = Err -> Err;
        ok ->
            Meta = beamai_agent_callbacks:build_metadata(Agent),
            #{callbacks := Callbacks} = Agent,
            beamai_agent_callbacks:invoke(on_resume, [IntState, Meta], Callbacks),
            Agent1 = clear_interrupt(Agent),
            %% resume 同样经 turn 洋葱（guardrail/校验对被打断过的 turn 最终答案也生效），
            %% 终端一次性分派：首次进入=延续暂停 turn（消费 interrupt_state），
            %% 递归重入=全新循环（TurnReq.messages）。CAS 保证延续只跑一次。
            Consumed = atomics:new(1, [{signed, false}]),  %% 0=未消费
            InitState = maps:get(saved_state, IntState, #{}),
            TurnReq = #{messages => [],
                        context => resume_context(Agent1, InitState),
                        resume => true},
            Terminal = fun(TReq) ->
                case atomics:compare_exchange(Consumed, 1, 0, 1) of
                    ok ->
                        resume_continuation(Agent1, Decision, Payload, IntState);
                    _ ->
                        %% 递归重入：全新循环
                        run_loop(Agent1,
                                 #{new => maps:get(messages, TReq),
                                   load_history => maps:get(load_history, TReq, true)},
                                 [], #{turn_context => maps:get(context, TReq)})
                end
            end,
            dispatch_turn_result(Agent1, run_turn_chain(Agent1, TurnReq, Terminal),
                                 Callbacks, Meta)
    end.

%% @private 延续暂停的 turn：按 phase 消费 interrupt_state，返回**原始**工具循环结果
%% tuple（由 dispatch_turn_result 统一分派 finalize/interrupt/error）。
resume_continuation(Agent1, Decision, Payload, IntState) ->
    case maps:get(phase, IntState, approval) of
        env_retry -> resume_env_raw(Agent1, Decision, IntState);
        _ -> resume_approval_raw(Agent1, Decision, Payload, IntState)
    end.

%% @private 审批暂停延续：按决策要么执行被中断工具、要么用其结果消息，跑续接循环
resume_approval_raw(Agent1, Decision, Payload, IntState) ->
    #{max_tool_iterations := MaxIter} = Agent1,
    Existing = maps:get(messages, IntState, []),
    #{iteration := Iter, tool_calls_made := PrevCalls} = IntState,
    InitState = maps:get(saved_state, IntState, #{}),
    ResumeMsgs = case beamai_agent_interrupt:resume_action(IntState, Decision, Payload) of
        {result, Msg} ->
            [Msg];
        {execute, ToolCall} ->
            %% approved：真正执行被中断工具（可能已替换参数），用其结果
            Kernel = maps:get(kernel, Agent1),
            Ctx = resume_context(Agent1, InitState),
            {Msgs, _Records, _Ctx2} =
                beamai_agent_utils:execute_tools(Kernel, [ToolCall], Ctx, false),
            Msgs
    end,
    run_loop(Agent1,
             #{existing => Existing, new => ResumeMsgs},
             PrevCalls,
             #{max_iterations => MaxIter - Iter, init_state => InitState}).

%% @private 环境暂停延续：retry 重跑失败调用并按 id 替换结果（仍失败则再暂停——返回
%% 原始 {interrupt, env_retry, _}），否则 proceed 原错误结果放行；跑续接循环。
resume_env_raw(Agent1, Decision, IntState) ->
    #{max_tool_iterations := MaxIter, kernel := Kernel} = Agent1,
    #{iteration := Iter, tool_calls_made := PrevCalls,
      batch_messages := BatchMsgs, failed_calls := FailedCalls} = IntState,
    InitState = maps:get(saved_state, IntState, #{}),
    Existing = maps:get(messages, IntState, []),
    RemOpts = #{max_iterations => MaxIter - Iter, init_state => InitState},
    case is_retry_decision(Decision) of
        true ->
            Ctx = resume_context(Agent1, InitState),
            Parallel = maps:get(parallel_tools, Agent1, true),
            {RetryMsgs, RetryRecords, _} =
                beamai_agent_utils:execute_tools(Kernel, FailedCalls, Ctx, Parallel),
            Corrected = beamai_agent_interrupt:replace_results_by_id(BatchMsgs, RetryMsgs),
            StillFailed = [TC || {TC, R} <- lists:zip(FailedCalls, RetryRecords),
                                 env_failed_record(R)],
            case {maps:get(on_env_error, Agent1, proceed), StillFailed} of
                {pause, [_ | _]} ->
                    %% 重跑后仍有环境类失败 → 返回原始 interrupt tuple，dispatch 统一暂停
                    {interrupt, env_retry,
                     beamai_agent_tool_loop:build_env_interrupt_context(
                       Iter, Existing, Corrected, RetryRecords, StillFailed,
                       InitState, PrevCalls)};
                _ ->
                    run_loop(Agent1, #{existing => Existing, new => Corrected},
                             PrevCalls, RemOpts)
            end;
        false ->
            %% proceed：原错误结果照常交模型
            run_loop(Agent1, #{existing => Existing, new => BatchMsgs},
                     PrevCalls, RemOpts)
    end.

%% @private 构建 resume 执行/重跑用的只读环境 context（带 conversation_id + 恢复 state）
resume_context(Agent, InitState) ->
    Ctx0 = beamai_context:with_conversation_id(
             beamai_context:new(), beamai_agent_state:conversation_id(Agent)),
    beamai_context:with_state(Ctx0, InitState).

%% @private CallRecord 是否为环境类失败
env_failed_record(#{error := #{class := environment}}) -> true;
env_failed_record(_) -> false.

%% @private retry / approved 视为"重跑失败调用"，其余为 proceed
is_retry_decision(<<"retry">>) -> true;
is_retry_decision(<<"approved">>) -> true;
is_retry_decision(retry) -> true;
is_retry_decision(approved) -> true;
is_retry_decision(_) -> false.

%% @private 清除中断态并同步 pause_store（若配置）
clear_interrupt(Agent) ->
    Agent1 = Agent#{interrupt_state => undefined},
    beamai_agent_pause:clear(Agent1),
    Agent1.

%% @private 放弃中断开新 chat 时，heal 历史尾部悬空的 assistant(tool_calls)
%%
%% 未处于中断态 → 原样返回。否则对持久历史里未获结果的每个 tool_call_id 补一条
%% 「已取消」tool 结果（保证 provider 不见残缺历史），再清中断态 + 暂停快照。
%% memory=undefined（不持久）时无历史可 heal，仅清态。
heal_dangling(Agent) ->
    case is_pending_interrupt(Agent) of
        false ->
            Agent;
        true ->
            heal_history(Agent),
            clear_interrupt(Agent)
    end.

%% @private 是否有未决中断（本地态或 pause_store）
is_pending_interrupt(#{interrupt_state := IntState}) when IntState =/= undefined -> true;
is_pending_interrupt(Agent) ->
    beamai_agent_pause:load(Agent) =/= none.

%% @private 对持久历史补齐悬空 tool_calls 的「已取消」结果
heal_history(Agent) ->
    case beamai_agent_state:memory(Agent) of
        undefined ->
            ok;
        Memory ->
            ConvId = beamai_agent_state:conversation_id(Agent),
            History = beamai_memory_provider:history(Memory, ConvId),
            case dangling_tool_call_ids(History) of
                [] -> ok;
                Ids ->
                    Cancelled = [cancelled_tool_result(Id) || Id <- Ids],
                    beamai_memory_provider:append(Memory, ConvId, Cancelled)
            end
    end.

%% @private 历史中出现在 assistant(tool_calls) 但无对应 tool 结果的 tool_call_id
dangling_tool_call_ids(History) ->
    CallIds = lists:flatmap(fun assistant_tool_call_ids/1, History),
    Answered = [Id || #{role := tool, tool_call_id := Id} <- History],
    [Id || Id <- CallIds, not lists:member(Id, Answered)].

assistant_tool_call_ids(#{role := assistant, tool_calls := TCs}) when is_list(TCs) ->
    [begin {Id, _N, _A} = beamai_tool:parse_tool_call(TC), Id end || TC <- TCs];
assistant_tool_call_ids(_) ->
    [].

%% @private 合成「已取消」tool 结果
cancelled_tool_result(Id) ->
    #{role => tool, tool_call_id => Id,
      content => beamai_tool:encode_result(
          #{error => #{type => cancelled,
                       message => <<"tool call abandoned due to interrupt; not executed">>}})}.

%% @doc 判断 agent 是否处于中断状态
%%
%% @param State agent 状态
%% @returns boolean()
-spec is_interrupted(beamai_agent_state:agent_state()) -> boolean().
is_interrupted(#{interrupt_state := #{status := interrupted}}) -> true;
is_interrupted(#{interrupt_state := undefined} = Agent) ->
    %% 本进程无中断态：透明回落 pause_store（跨重启发现未决暂停）
    case beamai_agent_pause:load(Agent) of
        {ok, _} -> true;
        none -> false
    end;
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
%% 内部函数 - 辅助
%%====================================================================

%% @private 运行一轮 tool loop（full-messages 模式）
%%
%% 统一 run / resume 的 LoopOpts 组装。MsgSpec 描述本轮消息来源：
%%   existing     — 已有上下文（resume 时为中断时携带的完整 messages）
%%   new          — 本轮新增消息（用户输入 / resume 的人类输入）
%%   load_history — 是否前接跨轮历史（首轮 true，resume false）
%% 历史载入与新消息持久化全部由 tool_loop 编排（agent 不再直接碰 memory）。
%% Opts 可含 max_iterations 覆盖（resume 用剩余迭代数）。
run_loop(State, MsgSpec, PrevCalls, Opts) ->
    #{callbacks := Callbacks, kernel := Kernel} = State,
    MaxIter = maps:get(max_iterations, Opts, maps:get(max_tool_iterations, State)),
    LoopOpts = #{
        kernel => Kernel,
        messages => maps:get(existing, MsgSpec, []),
        new_messages => maps:get(new, MsgSpec, []),
        load_history => maps:get(load_history, MsgSpec, false),
        chat_opts => build_chat_opts(State, Opts),
        callbacks => Callbacks,
        max_iterations => MaxIter,
        %% 只传 tool_loop 实际需要的字段，不传完整 agent state
        max_tool_iterations => maps:get(max_tool_iterations, State),
        parallel_tools => maps:get(parallel_tools, State, true),
        interrupt_tools => maps:get(interrupt_tools, State, []),
        on_env_error => maps:get(on_env_error, State, proceed),
        memory => beamai_agent_state:memory(State),
        conversation_id => beamai_agent_state:conversation_id(State),
        meta => beamai_agent_callbacks:build_metadata(State),
        %% 流式时透传 token 处理器；非流式为 undefined
        stream_token_handler => maps:get(stream_token_handler, Opts, undefined)
    },
    beamai_agent_tool_loop:run(LoopOpts, PrevCalls).

%% @private 完成一轮对话：构建结果、更新状态、触发回调
%%
%% 跨轮历史已由 memory provider 持久化（含本轮用户消息与 assistant 回复），
%% 此处不再向 agent 状态追加消息，仅累加 turn_count 并构建结果。
%%
%% @param State0 执行前的 agent 状态
%% @param Response LLM 最终响应
%% @param ToolCallsMade 本轮所有 tool 调用记录
%% @param Iterations 迭代次数
%% @returns {ok, RunResult, FinalState}
finalize_turn(State0, Response, ToolCallsMade, Iterations) ->
    #{callbacks := Callbacks} = State0,
    Meta = beamai_agent_callbacks:build_metadata(State0),
    Content = beamai_agent_utils:extract_content(Response),
    %% 到达终态：清除该会话未决暂停快照（store 始终镜像"是否有未决暂停"）
    beamai_agent_pause:clear(State0),
    NewState = State0#{turn_count => maps:get(turn_count, State0) + 1},
    Result = #{
        content => Content,
        tool_calls_made => ToolCallsMade,
        finish_reason => beamai_llm_response:finish_reason(Response),
        usage => beamai_llm_response:usage(Response),
        iterations => Iterations
    },
    EndMeta = Meta#{turn_count => maps:get(turn_count, NewState)},
    beamai_agent_callbacks:invoke(on_turn_end, [EndMeta], Callbacks),
    {ok, Result, NewState}.

%% @private 构建 chat 选项
%%
%% 在共享工具模块基础上，附加：
%%   - 带 conversation_id 的 context（供工具执行 / 用户自加的 kernel filter 定位会话）
%%   - system_prompts（system_prompt 包装为 system 消息，经 kernel invoke_chat 注入、不入存储）
%%   - 中断 tool specs（如有）
%%
%% 注：记忆与 on_llm_call 等回调由 tool_loop 显式编排，不再经 chat_opts 引线。
build_chat_opts(#{kernel := Kernel} = Agent, Opts) ->
    BaseOpts0 = beamai_agent_utils:build_chat_opts(Kernel, Opts),
    %% turn filter 可改写 context → 若 turn 链传入 turn_context 则用之（已含
    %% conversation_id/state）；否则按 conversation_id + init_state 构建默认。
    Ctx = case maps:get(turn_context, Opts, undefined) of
        undefined ->
            Ctx0 = beamai_context:with_conversation_id(
                     beamai_context:new(), beamai_agent_state:conversation_id(Agent)),
            beamai_context:with_state(Ctx0, maps:get(init_state, Opts, #{}));
        TurnCtx ->
            TurnCtx
    end,
    BaseOpts1 = BaseOpts0#{
        context => Ctx,
        system_prompts => system_prompts(maps:get(system_prompt, Agent, undefined))
    },
    InterruptSpecs = beamai_agent_interrupt:get_interrupt_tool_specs(Agent),
    case InterruptSpecs of
        [] -> BaseOpts1;
        _ ->
            ExistingTools = maps:get(tools, BaseOpts1, []),
            BaseOpts1#{tools => ExistingTools ++ InterruptSpecs}
    end.

%% @private 把 system_prompt 包装为 system 消息列表（空/未设返回 []）
system_prompts(undefined) -> [];
system_prompts(<<>>) -> [];
system_prompts(P) when is_binary(P) -> [#{role => system, content => P}].

%% @private 经 on_token 回调分块推送内容（用于 stream/2,3）
emit_tokens(<<>>, _Meta, _Callbacks) ->
    ok;
emit_tokens(Content, Meta, Callbacks) ->
    beamai_agent_callbacks:invoke(on_token, [Content, Meta], Callbacks).

%% @private 从消息列表中查找最后一条带文本内容的 assistant 消息
%%
%% 从列表末尾（已反转）向前查找 role=assistant 且 content 为 binary 的消息，
%% 跳过仅含 tool_calls（content=null）的 assistant 消息。
find_last_assistant([]) -> undefined;
find_last_assistant([#{role := assistant, content := C} | _]) when is_binary(C) -> C;
find_last_assistant([_ | Rest]) -> find_last_assistant(Rest).

%%====================================================================
%% 内部函数 - 中断支持
%%====================================================================

%% @private 处理新的中断
%%
%% 构建 interrupt_state，触发 on_interrupt callback，
%% 返回 {interrupt, Info, State}
handle_new_interrupt(Agent, Type, Context, _UserMsg, Callbacks, Meta) ->
    Reason = maps:get(reason, Context, undefined),
    {IntState, Agent1} = beamai_agent_interrupt:handle_interrupt(Type, Reason, Context, Agent),
    %% 暂停自动落库（配置了 pause_store 时；缺省 no-op）——跨进程 HITL
    beamai_agent_pause:save(Agent1),
    beamai_agent_callbacks:invoke(on_interrupt, [IntState, Meta], Callbacks),
    Info = #{
        reason => Reason,
        interrupt_type => Type,
        interrupted_tool_call => maps:get(interrupted_tool_call, Context, undefined),
        completed_results => maps:get(completed_tool_results, Context, []),
        created_at => maps:get(created_at, IntState)
    },
    {interrupt, Info, Agent1}.
