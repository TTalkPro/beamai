%%%-------------------------------------------------------------------
%%% @doc Tool Loop 执行模块（Agent 自管编排：full-messages + 显式记忆/回调）
%%%
%%% 驱动 ReAct 工具调用循环。**记忆与回调都由本模块显式编排**，不经任何 kernel
%%% filter：
%%%
%%%   - 本模块持有本轮**完整 messages**（within-run 累积）。
%%%   - 每轮：触发 on_llm_call → 经 memory provider 的 prepare 变换(窗口/摘要) →
%%%     invoke_chat(_stream) → 把 assistant 回合并入 messages 并 append 持久化 →
%%%     有 tool_calls 则执行(触发 on_tool_call/result)、把工具结果并入并持久化 → 递归。
%%%   - 循环终止于四种情形：模型不再要工具（正常完成）、整批工具标注
%%%     return_direct（工具结果即最终答案，不回灌模型）、中断（HITL/env_retry）、
%%%     迭代耗尽。
%%%   - 跨轮历史(cross-run)由 memory provider 的 history/append 负责；本模块只负责
%%%     within-run 累积与按序持久化。memory=undefined 时仅在本轮内累积、不持久化。
%%%
%%% chat_opts 仍带 context（conversation_id，供工具执行 / 用户自加的 kernel filter）
%%% 与 system_prompts（kernel invoke_chat 内按 opts 注入）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_tool_loop).

-export([run/2]).
-export([build_env_interrupt_context/7]).

-type provider() :: beamai_memory_provider:provider() | undefined.

-type loop_opts() :: #{
    kernel := beamai_kernel:kernel(),
    messages := [map()],            %% 已有上下文（resume 时为中断时携带的完整 messages）
    new_messages => [map()],        %% 本轮新增消息（入口处持久化后并入）
    load_history => boolean(),      %% 是否前接跨轮历史（首轮 true，resume false）
    chat_opts := map(),             %% 含 context 与 system_prompts
    callbacks := map(),
    max_iterations := pos_integer(),
    max_tool_iterations := pos_integer(),  %% agent 配置的总迭代上限（中断上下文计数用）
    parallel_tools := boolean(),    %% 一轮多个 tool_call 是否并发执行
    interrupt_tools := [map()],     %% 中断 tool 定义列表
    on_env_error => proceed | pause, %% 环境类工具失败策略（缺省 proceed）
    memory := provider(),           %% 记忆 provider（undefined 则不持久/不变换）
    conversation_id := binary(),
    meta := map(),                  %% 回调元数据（on_llm_call 等）
    %% 流式 token 处理器：设置时每轮 LLM 调用走 invoke_chat_stream，
    %% 文本 token 经此回调实时透出（undefined 则非流式）。
    stream_token_handler => undefined | fun((binary()) -> ok)
}.

-export_type([loop_opts/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc tool loop 入口
%%
%% @param Opts 循环选项（kernel、完整 messages、memory、回调等）
%% @param PrevToolCalls 之前已执行的 tool 调用记录（resume 时携带）
%% @returns {ok, Response, ToolCallsMade, Iterations, Messages} |
%%          {interrupt, Type, Context} |
%%          {error, Reason}
%%
%% Messages 为本轮跑完的**完整消息序列**（含载入的跨轮历史、本轮新增、各轮
%% assistant 回合与工具结果，直至最终答案）。turn filter 要重入时全靠它：
%% 拿到它再配 `load_history => false` 续跑，才能不依赖记忆是否开启就重建出
%% 完整上下文（见 beamai_filters:validate_loop 与 design/spring_advisor_alignment.md §2）。
-spec run(loop_opts(), [map()]) ->
    {ok, map(), [map()], pos_integer(), [map()]} |
    {interrupt, atom(), map()} |
    {error, term()}.
run(Opts, PrevToolCalls) ->
    #{max_iterations := MaxIter} = Opts,
    iterate(init_messages(Opts), MaxIter, PrevToolCalls).

%% @private 组装本轮起始 messages（记忆编排统一入口）
%%
%% 先按 load_history 载入跨轮历史（必须在持久化新消息**之前**，否则新消息
%% 会经历史重复带回），再持久化新增消息（cross-run append）并拼接：
%% 首轮 [历史 ++ 新消息]，resume [中断时 messages ++ 人类输入]。
init_messages(Opts) ->
    New = maps:get(new_messages, Opts, []),
    Prior = case maps:get(load_history, Opts, false) of
        true -> load_history(Opts);
        false -> []
    end,
    persist(Opts, New),
    Existing = maps:get(messages, Opts, []),
    Opts#{messages => Prior ++ Existing ++ New}.

%% @private 载入跨轮历史（无 memory 则 []）
load_history(#{memory := undefined}) -> [];
load_history(#{memory := Provider, conversation_id := ConvId}) ->
    beamai_memory_provider:history(Provider, ConvId).

%%====================================================================
%% 内部函数 - 主循环
%%====================================================================

%% @private 迭代次数耗尽，返回错误
iterate(_Opts, 0, ToolCallsMade) ->
    {error, {max_tool_iterations, ToolCallsMade}};

%% @private 主循环体：用本轮完整 messages 调用 LLM 并根据响应分支处理
iterate(Opts, N, ToolCallsMade) ->
    #{messages := Messages, callbacks := Callbacks, meta := Meta} = Opts,
    ToSend = prepare_messages(Opts, Messages),
    beamai_agent_callbacks:invoke(on_llm_call, [ToSend, Meta], Callbacks),
    case invoke_llm(Opts, ToSend) of
        {ok, Response, ChatCtx} ->
            %% 每次 LLM 返回后触发（含中间轮，可据此累计各次 usage）
            beamai_agent_callbacks:invoke(on_llm_result, [Response, Meta], Callbacks),
            Messages1 = record_assistant(Opts, Response, Messages),
            %% 穿线 chat 返回的 context：filter 私有状态与 state 槽跨轮存活
            Opts1 = with_ctx(Opts, ChatCtx),
            case beamai_llm_response:has_tool_calls(Response) of
                true ->
                    TCs = beamai_llm_response:tool_calls(Response),
                    handle_tool_calls(TCs, Opts1#{messages => Messages1}, N, ToolCallsMade);
                false ->
                    finish(Response, ToolCallsMade, Messages1)
            end;
        {error, _} = Err ->
            Err
    end.

%% @private 经 memory provider 变换待发送消息（无 provider 则原样）
prepare_messages(#{memory := undefined}, Messages) ->
    Messages;
prepare_messages(#{memory := Provider, conversation_id := ConvId}, Messages) ->
    beamai_memory_provider:prepare(Provider, ConvId, Messages).

%% @private 把 assistant 回合并入 messages 并持久化（无可存内容则原样返回）
record_assistant(Opts, Response, Messages) ->
    case beamai_message:from_response(Response) of
        undefined ->
            Messages;
        Msg ->
            persist(Opts, [Msg]),
            Messages ++ [Msg]
    end.

%% @private 调用 LLM：有 stream_token_handler 则走流式，否则非流式
invoke_llm(#{kernel := Kernel, chat_opts := ChatOpts} = Opts, ToSend) ->
    case maps:get(stream_token_handler, Opts, undefined) of
        undefined ->
            beamai_kernel:invoke_chat(Kernel, ToSend, ChatOpts);
        Handler when is_function(Handler, 1) ->
            TokenCb = fun(Token, _Meta) -> Handler(Token) end,
            beamai_kernel:invoke_chat_stream(Kernel, ToSend, ChatOpts, TokenCb)
    end.

%% @private 无 tool_calls，返回最终响应（Messages 为跑完的完整消息序列）
finish(Response, ToolCallsMade, Messages) ->
    {ok, Response, ToolCallsMade, compute_iterations(ToolCallsMade), Messages}.

%%====================================================================
%% 内部函数 - Tool Calls 处理
%%====================================================================

%% @private 处理 LLM 返回的 tool_calls：统一前置中断检测，命中则统一处理
handle_tool_calls(TCs, Opts, N, ToolCallsMade) ->
    case find_first_interrupt(TCs, Opts) of
        {interrupt, Type, Reason, InterruptedTC, SafeCalls, SkippedCalls} ->
            handle_interrupt(Type, Reason, InterruptedTC, SafeCalls, SkippedCalls,
                             Opts, N, ToolCallsMade);
        no ->
            execute_and_continue(TCs, Opts, N, ToolCallsMade)
    end.

%% @private 统一中断检测：先查中断 tool（LLM 显式请求人类介入），
%% 再查 on_tool_call 回调（宿主侧策略拦截）；返回首个命中。
%% find_interrupt_tool 匹配 #{interrupt_tools := _}，Opts 自身即满足。
%%
%% 返回 {interrupt, Type, Reason, InterruptedTC, SafeCalls, SkippedCalls}：
%%   SafeCalls    —— 同批可安全执行的 tools；
%%   SkippedCalls —— 同样被拦截但非首个的 tools（不执行，合成 skipped 结果）。
find_first_interrupt(TCs, Opts) ->
    case beamai_agent_interrupt:find_interrupt_tool(TCs, Opts) of
        {yes, InterruptTC, OtherCalls} ->
            {interrupt, tool_request, extract_interrupt_reason(InterruptTC),
             InterruptTC, OtherCalls, []};
        no ->
            case classify_tool_calls(TCs, maps:get(callbacks, Opts)) of
                {interrupt, Reason, [Flagged | MoreFlagged], SafeCalls} ->
                    {interrupt, callback, Reason, Flagged, SafeCalls, MoreFlagged};
                ok ->
                    no
            end
    end.

%% @private 统一中断处理（两类中断同语义）
%%
%% 先执行同批安全 tools（结果并入 messages 并持久化），被拦截未执行的其余
%% tools 合成 skipped 结果，再构建中断上下文返回。这保证 resume 后消息历史
%% 中 assistant 的每个 tool_call 都有对应结果（被中断的那个由人类输入补全），
%% 不会出现 provider 拒绝的残缺历史。中断上下文携带当前完整 messages，供
%% resume 续接。
handle_interrupt(Type, Reason, InterruptedTC, SafeCalls, SkippedCalls,
                 Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, callbacks := Callbacks, messages := Messages,
      max_tool_iterations := MaxIter} = Opts,
    Parallel = maps:get(parallel_tools, Opts, true),
    {SafeResults, SafeCallRecords, NewCtx} =
        beamai_agent_utils:execute_tools(Kernel, SafeCalls, ctx(Opts), Parallel,
                                         tool_result_cb(Callbacks)),
    AllResults = SafeResults ++ [skipped_result(TC) || TC <- SkippedCalls],
    persist(Opts, AllResults),
    Context = build_interrupt_context(MaxIter - N, AllResults, InterruptedTC,
                                      ToolCallsMade ++ SafeCallRecords, Reason,
                                      Messages ++ AllResults,
                                      beamai_context:get_state(NewCtx)),
    {interrupt, Type, Context}.

%% @private 被拦截未执行的 tool_call 的占位结果（保证消息历史完整）
skipped_result(TC) ->
    {Id, _Name, _Args} = beamai_tool:parse_tool_call(TC),
    #{role => tool, tool_call_id => Id,
      content => beamai_tool:encode_result(
          #{error => #{type => skipped,
                       message => <<"This tool call was skipped because the agent was "
                                    "interrupted before execution. It was not run; "
                                    "re-issue it after the interrupt is resolved if "
                                    "still needed.">>}})}.

%% @private 执行 tools，把结果并入 messages 并持久化，继续循环
%%
%% 屏障处（工具批次执行完、结果尚未交给下一轮 LLM 之前）做环境类失败分层路由：
%% 若批内含 environment 类失败且策略 pause → 带一致快照暂停等人（phase=env_retry），
%% 批次结果尚未持久化/交模型，携带在中断上下文的 batch_messages 里，resume 时按
%% 决策重跑失败调用或原样放行（见 beamai_agent:resume）。其余情形正常续跑
%% （语义/瞬态/策略类结果照旧 errors-are-data 交模型）。
execute_and_continue(TCs, Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, callbacks := Callbacks, messages := Messages,
      max_tool_iterations := MaxIter} = Opts,
    Parallel = maps:get(parallel_tools, Opts, true),
    {ToolResults, NewToolCalls, NewCtx} =
        beamai_agent_utils:execute_tools(Kernel, TCs, ctx(Opts), Parallel,
                                         tool_result_cb(Callbacks)),
    case env_pause(Opts, TCs, NewToolCalls) of
        {pause, FailedCalls} ->
            Context = build_env_interrupt_context(
                        MaxIter - N, Messages, ToolResults, NewToolCalls, FailedCalls,
                        beamai_context:get_state(NewCtx), ToolCallsMade),
            {interrupt, env_retry, Context};
        proceed ->
            persist(Opts, ToolResults),
            %% 穿线折叠后的 context：本轮工具写下的 state 槽下一轮工具/ filter 可见
            Opts0 = with_ctx(Opts, NewCtx),
            Opts1 = Opts0#{messages => Messages ++ ToolResults},
            AllCalls = ToolCallsMade ++ NewToolCalls,
            case return_direct(Opts1, TCs, NewToolCalls) of
                true -> finish_direct(Opts1, ToolResults, AllCalls);
                false -> iterate(Opts1, N - 1, AllCalls)
            end
    end.

%% @private 整批是否直返（对标 Spring AI ToolExecutionResult.returnDirect）
%%
%% **AND 语义**：批内 tool_calls 全部标注 return_direct 才直返。混批时任一未标注
%% 即照常回灌——否则未标注工具的结果会被静默丢弃、模型再没机会用上。
%%
%% **与 Spring 的一处有意分歧**：批内任一工具**失败**则不直返，退回正常回灌，
%% 让模型看到错误后自行补救。Spring 不区分成败、一律直返，会把错误 JSON 当最终
%% 答案端给用户——那与 errors-are-data（错误回模型、模型决定怎么办）相悖。
return_direct(_Opts, [], _Records) -> false;
return_direct(#{kernel := Kernel}, TCs, Records) ->
    lists:all(fun(TC) ->
        {_Id, Name, _Args} = beamai_tool:parse_tool_call(TC),
        beamai_kernel:return_direct_tool(Kernel, Name)
    end, TCs)
        andalso not lists:any(fun is_failed/1, Records).

%% @private CallRecord 是否失败（失败时才带 error 键）
is_failed(#{error := _}) -> true;
is_failed(_) -> false.

%% @private 直返：工具结果合成最终答案，落库后结束循环（不再回灌模型）
%%
%% 合成的 assistant 回合照常持久化：历史因此仍以 assistant 收尾（形如
%% assistant(tool_calls) → tool(result) → assistant(答案)），下一轮续接不残缺。
finish_direct(#{messages := Messages} = Opts, ToolResults, ToolCallsMade) ->
    Response = direct_response(ToolResults),
    Messages1 = record_assistant(Opts, Response, Messages),
    finish(Response, ToolCallsMade, Messages1).

%% @private 由工具结果合成最终响应（多工具按原始序换行拼接）
direct_response(ToolResults) ->
    Content = iolist_to_binary(
        lists:join(<<"\n">>, [C || #{content := C} <- ToolResults])),
    beamai_llm_response:new(#{
        content => Content,
        finish_reason => complete,
        metadata => #{return_direct => true}
    }).

%% @private 环境类失败暂停判定：策略 pause 且批内有 environment 类失败 →
%% {pause, FailedTCs}（FailedTCs 为环境失败的原始 tool_call，resume retry 用）；
%% 否则 proceed。策略缺省 proceed。
env_pause(Opts, TCs, Records) ->
    case maps:get(on_env_error, Opts, proceed) of
        pause ->
            Failed = [TC || {TC, R} <- lists:zip(TCs, Records),
                            env_failed(R)],
            case Failed of
                [] -> proceed;
                _ -> {pause, Failed}
            end;
        _ ->
            proceed
    end.

%% @private CallRecord 是否为环境类失败
env_failed(#{error := #{class := environment}}) -> true;
env_failed(_) -> false.

%% @private 把消息持久化到 memory provider（无 provider 或空列表则 no-op）
persist(#{memory := undefined}, _Msgs) -> ok;
persist(_Opts, []) -> ok;
persist(#{memory := Provider, conversation_id := ConvId}, Msgs) ->
    beamai_memory_provider:append(Provider, ConvId, Msgs).

%% @private 构建实时结果回调：每个工具完成即触发 on_tool_result（进度实时性优先，
%% 并发时触发顺序不确定；需确定顺序读 CallRecords）。经 callbacks:invoke 吞异常。
tool_result_cb(Callbacks) ->
    fun(#{name := Name, result := Result}) ->
        beamai_agent_callbacks:invoke(on_tool_result, [Name, Result], Callbacks)
    end.

%% @private 取贯穿全链的 context（带 conversation_id），缺省新建
ctx(#{chat_opts := ChatOpts}) ->
    maps:get(context, ChatOpts, beamai_context:new()).

%% @private 把更新后的 context 穿线回 chat_opts，供下一轮 chat/tool 复用
%% （filter 私有状态跨轮存活、state 槽跨轮可见）
with_ctx(#{chat_opts := ChatOpts} = Opts, Ctx) ->
    Opts#{chat_opts => ChatOpts#{context => Ctx}}.

%% @private 计算迭代次数
compute_iterations([]) -> 1;
compute_iterations(ToolCallsMade) -> length(ToolCallsMade) + 1.

%%====================================================================
%% 内部函数 - 中断上下文构建
%%====================================================================

%% @private 构建中断上下文 map（携带当前完整 messages 与 state 快照供 resume 续接）
%%
%% State 为中断前累积的 state 槽（纯数据），resume 时恢复进 context，避免累积
%% 状态在跨越中断时静默丢失。
build_interrupt_context(Iteration, CompletedResults, InterruptedTC,
                        ToolCallsMade, Reason, Messages, State) ->
    #{
        completed_tool_results => CompletedResults,
        interrupted_tool_call => InterruptedTC,
        iteration => Iteration,
        tool_calls_made => ToolCallsMade,
        reason => Reason,
        messages => Messages,
        state => State
    }.

%% @private 构建环境类暂停（phase=env_retry）的中断上下文
%%
%% 与审批中断不同：批次**已执行完**（一致快照），结果在 batch_messages 里、
%% 尚未持久化/交模型；messages 为**批前**消息（含触发本批的 assistant tool_calls）；
%% failed_calls 为环境类失败的原始 tool_call（resume retry 重跑并按 id 替换结果）。
build_env_interrupt_context(Iteration, Messages, BatchMessages, Records, FailedCalls,
                            State, ToolCallsMade) ->
    #{
        phase => env_retry,
        reason => env_error,
        %% iteration 存"已用计数"（= MaxIter - 剩余），与审批路径一致：resume 以
        %% MaxIter - iteration 还原剩余迭代
        iteration => Iteration,
        tool_calls_made => ToolCallsMade ++ Records,
        interrupted_tool_call => undefined,
        completed_tool_results => BatchMessages,
        messages => Messages,
        state => State,
        batch_messages => BatchMessages,
        failed_calls => FailedCalls
    }.

%%====================================================================
%% 内部函数 - Callback 中断检查
%%====================================================================

%% @private 按 on_tool_call callback 对同批 tool_calls 分类
%%
%% 对每个 tool_call 都执行回调（既是通知也是策略门），收集全部被拦截
%% 的调用：首个作为中断点，其余作为 skipped；未拦截的为安全可执行。
%% 返回 {interrupt, Reason, FlaggedCalls, SafeCalls} | ok。
classify_tool_calls(ToolCalls, Callbacks) ->
    case maps:get(on_tool_call, Callbacks, undefined) of
        undefined ->
            ok;
        Fun ->
            {Flagged, Safe} = partition_by_callback(ToolCalls, Fun),
            case Flagged of
                [] -> ok;
                [{Reason, _TC} | _] ->
                    {interrupt, Reason, [TC || {_R, TC} <- Flagged], Safe}
            end
    end.

%% @private 按回调裁决分组：{被拦截的 [{Reason, TC}], 安全的 [TC]}（保持原顺序）
partition_by_callback(ToolCalls, Fun) ->
    lists:foldr(fun(TC, {FAcc, SAcc}) ->
        {_Id, Name, Args} = beamai_tool:parse_tool_call(TC),
        case catch Fun(Name, Args) of
            {interrupt, Reason} -> {[{Reason, TC} | FAcc], SAcc};
            _ -> {FAcc, [TC | SAcc]}
        end
    end, {[], []}, ToolCalls).

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 从 interrupt tool_call 中提取中断原因
%% 支持 OpenAI 嵌套格式与统一响应的扁平格式（与 get_tool_call_name 同理）
extract_interrupt_reason(#{function := #{arguments := Args}}) when is_map(Args) ->
    Args;
extract_interrupt_reason(#{<<"function">> := #{<<"arguments">> := Args}}) when is_map(Args) ->
    Args;
extract_interrupt_reason(#{arguments := Args}) when is_map(Args) ->
    Args;
extract_interrupt_reason(TC) ->
    {_Id, Name, Args} = beamai_tool:parse_tool_call(TC),
    #{tool => Name, arguments => Args}.
