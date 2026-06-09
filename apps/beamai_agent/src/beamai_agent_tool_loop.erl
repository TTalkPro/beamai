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

-type provider() :: beamai_memory_provider:provider() | undefined.

-type loop_opts() :: #{
    kernel := beamai_kernel:kernel(),
    messages := [map()],            %% 本轮完整 messages（含已载入的跨轮历史）
    chat_opts := map(),             %% 含 context 与 system_prompts
    callbacks := map(),
    max_iterations := pos_integer(),
    agent := map(),
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
%% @returns {ok, Response, ToolCallsMade, Iterations} |
%%          {interrupt, Type, Context} |
%%          {error, Reason}
-spec run(loop_opts(), [map()]) ->
    {ok, map(), [map()], pos_integer()} |
    {interrupt, atom(), map()} |
    {error, term()}.
run(Opts, PrevToolCalls) ->
    #{max_iterations := MaxIter} = Opts,
    iterate(Opts, MaxIter, PrevToolCalls).

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
        {ok, Response, _Ctx} ->
            %% 每次 LLM 返回后触发（含中间轮，可据此累计各次 usage）
            beamai_agent_callbacks:invoke(on_llm_result, [Response, Meta], Callbacks),
            Messages1 = record_assistant(Opts, Response, Messages),
            case beamai_llm_response:has_tool_calls(Response) of
                true ->
                    TCs = beamai_llm_response:tool_calls(Response),
                    handle_tool_calls(TCs, Opts#{messages => Messages1}, N, ToolCallsMade);
                false ->
                    finish(Response, ToolCallsMade)
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

%% @private 无 tool_calls，返回最终响应
finish(Response, ToolCallsMade) ->
    {ok, Response, ToolCallsMade, compute_iterations(ToolCallsMade)}.

%%====================================================================
%% 内部函数 - Tool Calls 处理
%%====================================================================

%% @private 处理 LLM 返回的 tool_calls
handle_tool_calls(TCs, Opts, N, ToolCallsMade) ->
    #{agent := Agent} = Opts,
    case beamai_agent_interrupt:find_interrupt_tool(TCs, Agent) of
        {yes, InterruptTC, OtherCalls} ->
            handle_interrupt_tool(InterruptTC, OtherCalls, Opts, N, ToolCallsMade);
        no ->
            handle_normal_tool_calls(TCs, Opts, N, ToolCallsMade)
    end.

%% @private 处理 interrupt tool 类型的中断
%%
%% 先执行非中断 tools（结果并入 messages 并持久化），再构建中断上下文返回。
%% 中断上下文携带当前完整 messages，供 resume 续接（与是否启用记忆无关）。
handle_interrupt_tool(InterruptTC, OtherCalls, Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, agent := Agent, callbacks := Callbacks, messages := Messages} = Opts,
    #{max_tool_iterations := MaxIter} = Agent,
    Parallel = maps:get(parallel_tools, Agent, true),
    {OtherResults, OtherCallRecords} =
        beamai_agent_utils:execute_tools(Kernel, OtherCalls, ctx(Opts), Parallel),
    fire_tool_results(Callbacks, OtherCallRecords),
    persist(Opts, OtherResults),
    Reason = extract_interrupt_reason(InterruptTC),
    Context = build_interrupt_context(MaxIter - N, OtherResults, InterruptTC,
                                      ToolCallsMade ++ OtherCallRecords, Reason,
                                      Messages ++ OtherResults),
    {interrupt, tool_request, Context}.

%% @private 处理非中断 tool calls（callback 检查 + 执行）
handle_normal_tool_calls(TCs, Opts, N, ToolCallsMade) ->
    #{callbacks := Callbacks, agent := Agent, messages := Messages} = Opts,
    case check_callback_interrupt(TCs, Callbacks) of
        {interrupt, CallbackReason, InterruptedTC} ->
            #{max_tool_iterations := MaxIter} = Agent,
            Context = build_interrupt_context(MaxIter - N, [], InterruptedTC,
                                              ToolCallsMade, CallbackReason, Messages),
            {interrupt, callback, Context};
        ok ->
            execute_and_continue(TCs, Opts, N, ToolCallsMade)
    end.

%% @private 执行 tools，把结果并入 messages 并持久化，继续循环
execute_and_continue(TCs, Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, agent := Agent, callbacks := Callbacks, messages := Messages} = Opts,
    Parallel = maps:get(parallel_tools, Agent, true),
    {ToolResults, NewToolCalls} =
        beamai_agent_utils:execute_tools(Kernel, TCs, ctx(Opts), Parallel),
    fire_tool_results(Callbacks, NewToolCalls),
    persist(Opts, ToolResults),
    iterate(Opts#{messages => Messages ++ ToolResults}, N - 1, ToolCallsMade ++ NewToolCalls).

%% @private 把消息持久化到 memory provider（无 provider 或空列表则 no-op）
persist(#{memory := undefined}, _Msgs) -> ok;
persist(_Opts, []) -> ok;
persist(#{memory := Provider, conversation_id := ConvId}, Msgs) ->
    beamai_memory_provider:append(Provider, ConvId, Msgs).

%% @private 对每个工具调用记录触发 on_tool_result 回调（并发时整批收齐后触发）
fire_tool_results(Callbacks, CallRecords) ->
    lists:foreach(fun(#{name := Name, result := Result}) ->
        beamai_agent_callbacks:invoke(on_tool_result, [Name, Result], Callbacks)
    end, CallRecords).

%% @private 取贯穿全链的 context（带 conversation_id），缺省新建
ctx(#{chat_opts := ChatOpts}) ->
    maps:get(context, ChatOpts, beamai_context:new()).

%% @private 计算迭代次数
compute_iterations([]) -> 1;
compute_iterations(ToolCallsMade) -> length(ToolCallsMade) + 1.

%%====================================================================
%% 内部函数 - 中断上下文构建
%%====================================================================

%% @private 构建中断上下文 map（携带当前完整 messages 供 resume 续接）
build_interrupt_context(Iteration, CompletedResults, InterruptedTC,
                        ToolCallsMade, Reason, Messages) ->
    #{
        completed_tool_results => CompletedResults,
        interrupted_tool_call => InterruptedTC,
        iteration => Iteration,
        tool_calls_made => ToolCallsMade,
        reason => Reason,
        messages => Messages
    }.

%%====================================================================
%% 内部函数 - Callback 中断检查
%%====================================================================

%% @private 检查 on_tool_call callback 是否触发中断
check_callback_interrupt(ToolCalls, Callbacks) ->
    case maps:get(on_tool_call, Callbacks, undefined) of
        undefined -> ok;
        Fun -> check_callback_interrupt_loop(ToolCalls, Fun)
    end.

%% @private 逐个检查 tool_call 的 callback 中断
check_callback_interrupt_loop([], _Fun) ->
    ok;
check_callback_interrupt_loop([TC | Rest], Fun) ->
    {_Id, Name, Args} = beamai_tool:parse_tool_call(TC),
    case catch Fun(Name, Args) of
        {interrupt, Reason} ->
            {interrupt, Reason, TC};
        _ ->
            check_callback_interrupt_loop(Rest, Fun)
    end.

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 从 interrupt tool_call 中提取中断原因
extract_interrupt_reason(#{function := #{arguments := Args}}) when is_map(Args) ->
    Args;
extract_interrupt_reason(#{<<"function">> := #{<<"arguments">> := Args}}) when is_map(Args) ->
    Args;
extract_interrupt_reason(TC) ->
    {_Id, Name, Args} = beamai_tool:parse_tool_call(TC),
    #{tool => Name, arguments => Args}.
