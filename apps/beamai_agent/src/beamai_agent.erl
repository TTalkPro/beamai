%%%-------------------------------------------------------------------
%%% @doc 有状态多轮对话 Agent（ReAct）
%%%
%%% 封装 beamai_kernel，提供：
%%%   - 多轮对话管理（跨轮历史由 filter-memory 按 conversation_id 维护）
%%%   - 自实现 tool loop（delta 模式，确保 filters 完整触发）
%%%   - 6 个观察性回调（on_turn_start/end/error, on_llm_call, on_tool_call, on_token）
%%%
%%% 核心设计决策：
%%%   - 上下文记忆用 Filter 实现：kernel 经 with_memory/2 挂载 beamai_memory_filter，
%%%     工具循环以 delta 模式运行——每轮只提交新消息（用户消息 / 工具结果），
%%%     由 Memory filter 存储 delta 并展开完整历史。agent 状态不再持有 messages，
%%%     仅持有 store 句柄与 conversation_id。
%%%   - Agent 自己实现 tool loop（每次 LLM 调用 invoke_chat、工具调用 invoke_tool
%%%     都经过完整 filter 管道），以支撑中断检测与流式。
%%%   - 回调通过 kernel filter 注入（on_llm_call → around_chat, on_tool_call → around_tool）
%%%   - Map-based 状态，无 Record 依赖，方便序列化和扩展（store 句柄为 {Mod, 注册名}，
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
-export([resume/2]).
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
    {ok, run_result(), beamai_agent_state:agent_state()} |
    {interrupt, interrupt_info(), beamai_agent_state:agent_state()} |
    {error, term()}.
run(State, UserMessage) ->
    run(State, UserMessage, #{}).

%% @doc 执行一轮对话（带选项）
%%
%% 执行流程（delta 模式 + filter-memory）：
%%   1. 触发 on_turn_start 回调
%%   2. 进入 tool loop（本轮 delta 起始 = 用户消息）:
%%      a. kernel:invoke_chat 发送本轮 delta；Memory filter 存 delta、展开完整历史、
%%         调 LLM、存回 assistant 回复（经过完整 around_chat 链）
%%      b. 若 LLM 返回 tool_calls: 逐个 kernel:invoke_tool 执行（经过 around_tool 链），
%%         工具结果作为下一轮 delta，回到 (a)
%%      c. 若 LLM 返回文本: 终止循环
%%   3. turn_count + 1（历史已由 Memory filter 写入 store，无需 agent 追加）
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

    %% 生成本轮 run_id
    RunId = beamai_id:gen_id(<<"run">>),
    State0 = State#{run_id => RunId},

    Meta = beamai_agent_callbacks:build_metadata(State0),
    beamai_agent_callbacks:invoke(on_turn_start, [Meta], Callbacks),

    %% delta 模式：本轮只提交用户消息，跨轮历史由 Memory filter 维护。
    UserMsg = #{role => user, content => UserMessage},
    case run_loop(State0, [UserMsg], [], Opts) of
        {ok, Response, ToolCallsMade, Iterations} ->
            finalize_turn(State0, Response, ToolCallsMade, Iterations);
        {interrupt, Type, Context} ->
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
%% 与 run/3 同样经过 filter-memory（delta 模式）完成工具循环，区别在于循环中
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
%% 从 filter-memory store 按 conversation_id 读取完整历史（正序）。
%% 历史含 user / assistant 消息，以及工具循环中的 assistant(tool_calls) 与
%% tool 结果消息。不包含 system_prompt（每次调用时动态拼接、不入存储）。
%% 未启用记忆（memory => false）时返回 []。
%%
%% @param State agent 状态
%% @returns 消息列表
-spec messages(beamai_agent_state:agent_state()) -> [map()].
messages(State) ->
    case beamai_agent_state:store(State) of
        undefined -> [];
        Store -> beamai_chat_memory:mem_get(Store, beamai_agent_state:conversation_id(State))
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
%% 将一条消息追加到 store 中本会话历史末尾。可用于注入上下文信息，
%% 如添加 assistant 角色的引导消息。未启用记忆时为 no-op。
%% agent 状态本身不可变，历史变更落在 store，故返回原 State。
%%
%% @param State agent 状态
%% @param Msg 消息 map（需包含 role 和 content 键）
%% @returns agent 状态（不变）
-spec add_message(beamai_agent_state:agent_state(), map()) ->
    beamai_agent_state:agent_state().
add_message(State, Msg) ->
    case beamai_agent_state:store(State) of
        undefined -> ok;
        Store -> beamai_chat_memory:mem_add(Store, beamai_agent_state:conversation_id(State), [Msg])
    end,
    State.

%% @doc 清空消息历史
%%
%% 清空 store 中本会话历史，agent 将从全新对话开始。未启用记忆时为 no-op。
%% 注意：不会重置 turn_count；agent 状态不可变，故返回原 State。
%%
%% @param State agent 状态
%% @returns agent 状态（不变）
-spec clear_messages(beamai_agent_state:agent_state()) ->
    beamai_agent_state:agent_state().
clear_messages(State) ->
    case beamai_agent_state:store(State) of
        undefined -> ok;
        Store -> beamai_chat_memory:mem_clear(Store, beamai_agent_state:conversation_id(State))
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
resume(#{interrupt_state := undefined}, _HumanInput) ->
    {error, not_interrupted};
resume(#{interrupt_state := IntState} = Agent, HumanInput) ->
    %% 1. 验证输入
    case beamai_agent_interrupt:validate_resume_input(IntState, HumanInput) of
        {error, _} = Err -> Err;
        ok ->
            #{callbacks := Callbacks, max_tool_iterations := MaxIter} = Agent,

            Meta = beamai_agent_callbacks:build_metadata(Agent),
            beamai_agent_callbacks:invoke(on_resume, [IntState, Meta], Callbacks),

            %% 2. 构建恢复 delta（已完成工具结果 + 人类输入；历史在 store）
            ResumeDelta = beamai_agent_interrupt:build_resume_messages(IntState, HumanInput),

            %% 3. 清除中断状态
            Agent1 = Agent#{interrupt_state => undefined},

            %% 4. 继续 tool loop（剩余迭代数）
            #{iteration := Iter, tool_calls_made := PrevCalls} = IntState,
            RemainingIter = MaxIter - Iter,
            case run_loop(Agent1, ResumeDelta, PrevCalls, #{max_iterations => RemainingIter}) of
                {ok, Response, AllToolCalls, Iterations} ->
                    finalize_turn(Agent1, Response, AllToolCalls, Iterations);
                {interrupt, Type, Context} ->
                    UserMsg = #{role => user, content => <<"[resume]">>},
                    handle_new_interrupt(Agent1, Type, Context, UserMsg, Callbacks, Meta);
                {error, Reason} ->
                    beamai_agent_callbacks:invoke(on_turn_error, [Reason, Meta], Callbacks),
                    {error, Reason}
            end
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
%% 内部函数 - 辅助
%%====================================================================

%% @private 运行一轮 tool loop（delta 模式）
%%
%% 统一 run / resume 的 LoopOpts 组装：注入带 conversation_id 的 context 与
%% system_prompts 的 chat_opts，交给 tool_loop 在 filter-memory 下驱动循环。
%% Opts 可含 max_iterations 覆盖（resume 用剩余迭代数）。
run_loop(State, Delta, PrevCalls, Opts) ->
    #{callbacks := Callbacks, kernel := Kernel} = State,
    MaxIter = maps:get(max_iterations, Opts, maps:get(max_tool_iterations, State)),
    LoopOpts = #{
        kernel => Kernel,
        messages => Delta,
        chat_opts => build_chat_opts(State, Opts),
        callbacks => Callbacks,
        max_iterations => MaxIter,
        agent => State,
        %% 流式时透传 token 处理器；非流式为 undefined
        stream_token_handler => maps:get(stream_token_handler, Opts, undefined)
    },
    beamai_agent_tool_loop:run(LoopOpts, PrevCalls).

%% @private 完成一轮对话：构建结果、更新状态、触发回调
%%
%% 跨轮历史已由 Memory filter 存入 store（含本轮用户消息与 assistant 回复），
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
%%   - 带 conversation_id 的 context（供 Memory / 回调等 filter 定位会话）
%%   - system_prompts（system_prompt 包装为 system 消息，经内层 filter 注入、不入存储）
%%   - callback_meta（本轮 agent 元数据，供 on_llm_call / 流式 on_token 回调读取；
%%     经引线随请求传入，filter 闭包据此拿到实时 turn_count/run_id 等）
%%   - 中断 tool specs（如有）
build_chat_opts(#{kernel := Kernel} = Agent, Opts) ->
    BaseOpts0 = beamai_agent_utils:build_chat_opts(Kernel, Opts),
    Ctx = beamai_context:with_conversation_id(
            beamai_context:new(), beamai_agent_state:conversation_id(Agent)),
    BaseOpts1 = BaseOpts0#{
        context => Ctx,
        system_prompts => system_prompts(maps:get(system_prompt, Agent, undefined)),
        callback_meta => callback_meta(Agent)
    },
    InterruptSpecs = beamai_agent_interrupt:get_interrupt_tool_specs(Agent),
    case InterruptSpecs of
        [] -> BaseOpts1;
        _ ->
            ExistingTools = maps:get(tools, BaseOpts1, []),
            BaseOpts1#{tools => ExistingTools ++ InterruptSpecs}
    end.

%% @private 本轮 agent 元数据（标准 build_metadata + conversation_id + run_id）
callback_meta(Agent) ->
    (beamai_agent_callbacks:build_metadata(Agent))#{
        conversation_id => beamai_agent_state:conversation_id(Agent),
        run_id => maps:get(run_id, Agent, undefined)
    }.

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
    beamai_agent_callbacks:invoke(on_interrupt, [IntState, Meta], Callbacks),
    Info = #{
        reason => Reason,
        interrupt_type => Type,
        interrupted_tool_call => maps:get(interrupted_tool_call, Context, undefined),
        completed_results => maps:get(completed_tool_results, Context, []),
        created_at => maps:get(created_at, IntState)
    },
    {interrupt, Info, Agent1}.
