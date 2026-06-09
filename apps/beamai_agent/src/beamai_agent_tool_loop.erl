%%%-------------------------------------------------------------------
%%% @doc Tool Loop 执行模块（delta 模式 + filter-memory）
%%%
%%% 驱动 ReAct 工具调用循环。会话历史不再由本模块本地累积，而是交给
%%% kernel 的 Memory filter（with_memory/2）按 conversation_id 管理：
%%%
%%%   - 每次只把**本轮 delta** 交给 invoke_chat（首轮 = 用户消息，
%%%     后续轮 = 上一轮工具结果消息）。
%%%   - Memory filter 的 around_chat 负责存 delta、用完整历史替换 messages
%%%     发给 LLM、并存回 assistant 回复（含 tool_calls）。
%%%   - 因此本模块在 LLM 返回 tool_calls 后，下一轮 delta 只需工具结果，
%%%     assistant(tool_calls) 已由 Memory filter 存储。
%%%
%%% 贯穿全链的 context（含 conversation_id）从 chat_opts 的 context 字段透传，
%%% 同时用于工具执行，使 Memory / 回调等 filter 能定位会话。
%%%
%%% 核心逻辑：
%%%   1. invoke_chat 发送本轮 delta（经 Memory filter 展开完整历史）
%%%   2. 响应含 tool_calls：检查中断 → 执行工具 → 工具结果作为下一轮 delta → 递归
%%%   3. 响应无 tool_calls：返回最终结果
%%%   4. 迭代次数用尽返回 max_tool_iterations 错误
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_tool_loop).

-export([run/2]).

-type loop_opts() :: #{
    kernel := beamai_kernel:kernel(),
    messages := [map()],      %% 本轮 delta（非全量历史）
    chat_opts := map(),       %% 含 context（带 conversation_id）与 system_prompts
    callbacks := map(),
    max_iterations := pos_integer(),
    agent := map(),
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
%% 执行 LLM 调用循环，处理 tool calls 和中断。
%%
%% @param Opts 循环选项（kernel、本轮 delta、chat_opts、回调等）
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

%% @private 主循环体：用本轮 delta 调用 LLM 并根据响应分支处理
iterate(Opts, N, ToolCallsMade) ->
    case invoke_llm(Opts) of
        {ok, Response, _Ctx} ->
            case beamai_llm_response:has_tool_calls(Response) of
                true ->
                    TCs = beamai_llm_response:tool_calls(Response),
                    handle_tool_calls(TCs, Opts, N, ToolCallsMade);
                false ->
                    finish(Response, ToolCallsMade)
            end;
        {error, _} = Err ->
            Err
    end.

%% @private 调用 LLM：有 stream_token_handler 则走流式（token 实时透出），
%% 否则非流式。两者都返回汇聚后的统一响应，循环逻辑一致。
invoke_llm(#{kernel := Kernel, messages := Delta, chat_opts := ChatOpts} = Opts) ->
    case maps:get(stream_token_handler, Opts, undefined) of
        undefined ->
            beamai_kernel:invoke_chat(Kernel, Delta, ChatOpts);
        Handler when is_function(Handler, 1) ->
            TokenCb = fun(Token, _Meta) -> Handler(Token) end,
            beamai_kernel:invoke_chat_stream(Kernel, Delta, ChatOpts, TokenCb)
    end.

%% @private 无 tool_calls，返回最终响应
finish(Response, ToolCallsMade) ->
    {ok, Response, ToolCallsMade, compute_iterations(ToolCallsMade)}.

%%====================================================================
%% 内部函数 - Tool Calls 处理
%%====================================================================

%% @private 处理 LLM 返回的 tool_calls
%%
%% 分三个优先级检查：
%%   1. 是否包含 interrupt tool
%%   2. callback 是否触发中断
%%   3. 正常执行（执行中检查结果中断）
handle_tool_calls(TCs, Opts, N, ToolCallsMade) ->
    #{agent := Agent} = Opts,
    case beamai_agent_interrupt:find_interrupt_tool(TCs, Agent) of
        {yes, InterruptTC, OtherCalls} ->
            handle_interrupt_tool(InterruptTC, OtherCalls, TCs, Opts, N, ToolCallsMade);
        no ->
            handle_normal_tool_calls(TCs, Opts, N, ToolCallsMade)
    end.

%% @private 处理 interrupt tool 类型的中断
%%
%% 先执行非中断 tools，然后构建中断上下文返回。已执行的工具结果暂存于
%% completed_tool_results，resume 时作为 delta 的一部分由 Memory filter 存储。
handle_interrupt_tool(InterruptTC, OtherCalls, TCs, Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, agent := Agent, callbacks := Callbacks} = Opts,
    #{max_tool_iterations := MaxIter} = Agent,
    Ctx = ctx(Opts),
    Parallel = maps:get(parallel_tools, Agent, true),
    {OtherResults, OtherCallRecords} =
        beamai_agent_utils:execute_tools(Kernel, OtherCalls, Ctx, Parallel),
    fire_tool_results(Callbacks, OtherCallRecords),
    Reason = extract_interrupt_reason(InterruptTC),
    Context = build_interrupt_context(TCs, MaxIter - N,
                                      OtherResults, InterruptTC,
                                      ToolCallsMade ++ OtherCallRecords, Reason),
    {interrupt, tool_request, Context}.

%% @private 处理非中断 tool calls（callback 检查 + 执行）
handle_normal_tool_calls(TCs, Opts, N, ToolCallsMade) ->
    #{callbacks := Callbacks, agent := Agent} = Opts,
    case check_callback_interrupt(TCs, Callbacks) of
        {interrupt, CallbackReason, InterruptedTC} ->
            #{max_tool_iterations := MaxIter} = Agent,
            Context = build_interrupt_context(TCs, MaxIter - N,
                                              [], InterruptedTC,
                                              ToolCallsMade, CallbackReason),
            {interrupt, callback, Context};
        ok ->
            execute_and_continue(TCs, Opts, N, ToolCallsMade)
    end.

%% @private 执行 tools 并继续循环
%%
%% delta 模式：下一轮 delta 只含工具结果消息（assistant(tool_calls) 已由
%% Memory filter 在本次 invoke_chat 后置存储）。工具执行（含并发）统一委托给
%% beamai_agent_utils:execute_tools/4。
execute_and_continue(TCs, #{kernel := Kernel, agent := Agent, callbacks := Callbacks} = Opts,
                     N, ToolCallsMade) ->
    Parallel = maps:get(parallel_tools, Agent, true),
    {ToolResults, NewToolCalls} =
        beamai_agent_utils:execute_tools(Kernel, TCs, ctx(Opts), Parallel),
    fire_tool_results(Callbacks, NewToolCalls),
    NextOpts = Opts#{messages => ToolResults},
    iterate(NextOpts, N - 1, ToolCallsMade ++ NewToolCalls).

%% @private 对每个工具调用记录触发 on_tool_result 回调（观察用）
%%
%% 并发执行时回调在整批结果收齐后触发（非逐个完成即触发）。
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

%% @private 构建中断上下文 map
%%
%% delta 模式下会话历史在 store：触发中断的 assistant(tool_calls) 已由 Memory
%% filter 存储，resume 仅凭 completed_tool_results + 人类输入作为 delta 继续，
%% 故不再保存 pending_messages / assistant_response。TCs 仅用于定位被中断的调用。
build_interrupt_context(_TCs, Iteration, CompletedResults,
                        InterruptedTC, ToolCallsMade, Reason) ->
    #{
        completed_tool_results => CompletedResults,
        interrupted_tool_call => InterruptedTC,
        iteration => Iteration,
        tool_calls_made => ToolCallsMade,
        reason => Reason
    }.

%%====================================================================
%% 内部函数 - Callback 中断检查
%%====================================================================

%% @private 检查 on_tool_call callback 是否触发中断
%%
%% 遍历 tool_calls，对每个调用触发 on_tool_call callback。
%% 如果 callback 返回 {interrupt, Reason}，中断执行。
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
