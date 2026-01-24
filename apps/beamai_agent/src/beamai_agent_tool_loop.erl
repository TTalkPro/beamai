%%%-------------------------------------------------------------------
%%% @doc 统一的 Tool Loop 执行模块
%%%
%%% 将 beamai_agent 中原本分散的 tool_loop 和 stream_tool_loop
%%% 合并为单一入口，通过 mode 参数区分普通/流式行为。
%%%
%%% 核心逻辑：
%%%   1. 调用 invoke_chat 发送消息给 LLM
%%%   2. 检查响应中是否包含 tool_calls
%%%      - 有 tool_calls: 检查中断 → 执行工具 → 拼接结果 → 递归
%%%      - 无 tool_calls:
%%%        * normal 模式: 直接返回结果
%%%        * stream 模式: 切换到流式进行最终调用
%%%   3. 迭代次数用尽时返回 max_tool_iterations 错误
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_tool_loop).

-export([run/2]).

-type loop_opts() :: #{
    kernel := beamai_kernel:kernel(),
    messages := [map()],
    chat_opts := map(),
    callbacks := map(),
    meta := map(),
    max_iterations := pos_integer(),
    agent := map(),
    mode := normal | stream
}.

-export_type([loop_opts/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 统一的 tool loop 入口
%%
%% 执行 LLM 调用循环，处理 tool calls 和中断。
%% mode=normal 时直接返回结果；mode=stream 时最后一次调用使用流式。
%%
%% @param Opts 循环选项（包含 kernel、消息、回调等）
%% @param PrevToolCalls 之前已执行的 tool 调用记录
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

%% @private 主循环体：调用 LLM 并根据响应分支处理
iterate(Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, messages := Msgs, chat_opts := ChatOpts} = Opts,
    case beamai_kernel:invoke_chat(Kernel, Msgs, ChatOpts) of
        {ok, #{tool_calls := TCs} = _Response, _Ctx} when is_list(TCs), TCs =/= [] ->
            handle_tool_calls(TCs, Msgs, Opts, N, ToolCallsMade);
        {ok, Response, _Ctx} ->
            finish_no_tools(Opts, Response, ToolCallsMade);
        {error, _} = Err ->
            Err
    end.

%%====================================================================
%% 内部函数 - Tool Calls 处理
%%====================================================================

%% @private 处理 LLM 返回的 tool_calls
%%
%% 分三个优先级检查：
%%   1. 是否包含 interrupt tool
%%   2. callback 是否触发中断
%%   3. 正常执行（执行中检查结果中断）
handle_tool_calls(TCs, Msgs, Opts, N, ToolCallsMade) ->
    #{agent := Agent} = Opts,
    case beamai_agent_interrupt:find_interrupt_tool(TCs, Agent) of
        {yes, InterruptTC, OtherCalls} ->
            handle_interrupt_tool(InterruptTC, OtherCalls, TCs, Msgs, Opts, N, ToolCallsMade);
        no ->
            handle_normal_tool_calls(TCs, Msgs, Opts, N, ToolCallsMade)
    end.

%% @private 处理 interrupt tool 类型的中断
%%
%% 先执行非中断 tools，然后构建中断上下文返回。
handle_interrupt_tool(InterruptTC, OtherCalls, TCs, Msgs, Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, agent := Agent} = Opts,
    #{max_tool_iterations := MaxIter} = Agent,
    {OtherResults, OtherCallRecords} = beamai_agent_utils:execute_tools(Kernel, OtherCalls),
    Reason = extract_interrupt_reason(InterruptTC),
    Context = build_interrupt_context(TCs, Msgs, MaxIter - N,
                                      OtherResults, InterruptTC,
                                      ToolCallsMade ++ OtherCallRecords, Reason),
    {interrupt, tool_request, Context}.

%% @private 处理非中断 tool calls（callback 检查 + 执行）
handle_normal_tool_calls(TCs, Msgs, Opts, N, ToolCallsMade) ->
    #{callbacks := Callbacks, agent := Agent} = Opts,
    case check_callback_interrupt(TCs, Callbacks) of
        {interrupt, CallbackReason, InterruptedTC} ->
            #{max_tool_iterations := MaxIter} = Agent,
            Context = build_interrupt_context(TCs, Msgs, MaxIter - N,
                                              [], InterruptedTC,
                                              ToolCallsMade, CallbackReason),
            {interrupt, callback, Context};
        ok ->
            execute_and_continue(TCs, Msgs, Opts, N, ToolCallsMade)
    end.

%% @private 执行 tools 并继续循环（或处理执行中断）
execute_and_continue(TCs, Msgs, Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, agent := Agent} = Opts,
    case execute_tools_with_interrupt_check(Kernel, TCs) of
        {ok, ToolResults, NewToolCalls} ->
            AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
            NewMsgs = Msgs ++ [AssistantMsg | ToolResults],
            NewOpts = Opts#{messages => NewMsgs},
            iterate(NewOpts, N - 1, ToolCallsMade ++ NewToolCalls);
        {interrupt, IntReason, PartialResults, InterruptedTC, CompletedCalls} ->
            #{max_tool_iterations := MaxIter} = Agent,
            Context = build_interrupt_context(TCs, Msgs, MaxIter - N,
                                              PartialResults, InterruptedTC,
                                              ToolCallsMade ++ CompletedCalls, IntReason),
            {interrupt, tool_result, Context}
    end.

%%====================================================================
%% 内部函数 - 无 Tool Calls 结束处理
%%====================================================================

%% @private 无 tool_calls 时的结束处理
%%
%% normal 模式: 直接返回响应
%% stream 模式: 进行流式最终调用
finish_no_tools(#{mode := normal}, Response, ToolCallsMade) ->
    Iters = compute_iterations(ToolCallsMade),
    {ok, Response, ToolCallsMade, Iters};
finish_no_tools(#{mode := stream} = Opts, _Response, ToolCallsMade) ->
    #{kernel := Kernel, messages := Msgs, chat_opts := ChatOpts,
      callbacks := Callbacks, meta := Meta} = Opts,
    case stream_final_call(Kernel, Msgs, ChatOpts, Callbacks, Meta) of
        {ok, StreamResponse} ->
            Iters = compute_iterations(ToolCallsMade),
            {ok, StreamResponse, ToolCallsMade, Iters};
        {error, _} = Err ->
            Err
    end.

%% @private 计算迭代次数
compute_iterations([]) -> 1;
compute_iterations(ToolCallsMade) -> length(ToolCallsMade) + 1.

%% @private 流式最终 LLM 调用
%%
%% 使用 beamai_chat_completion:stream_chat 进行流式调用，
%% 每收到一个 token 通过 on_token 回调传递给用户。
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

%%====================================================================
%% 内部函数 - 中断上下文构建
%%====================================================================

%% @private 构建中断上下文 map
%%
%% 统一的中断上下文构建函数，消除原代码中 6 处重复。
build_interrupt_context(TCs, Msgs, Iteration, CompletedResults,
                        InterruptedTC, ToolCallsMade, Reason) ->
    AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
    #{
        pending_messages => Msgs,
        assistant_response => AssistantMsg,
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
    {_Id, Name, Args} = beamai_function:parse_tool_call(TC),
    case catch Fun(Name, Args) of
        {interrupt, Reason} ->
            {interrupt, Reason, TC};
        _ ->
            check_callback_interrupt_loop(Rest, Fun)
    end.

%%====================================================================
%% 内部函数 - 带中断检查的 Tool 执行
%%====================================================================

%% @private 逐个执行 tool_calls，检查执行结果中的中断信号
%%
%% 如果某个 tool 返回 {interrupt, Reason, PartialResult}，
%% 停止执行并返回中断信息。
execute_tools_with_interrupt_check(Kernel, ToolCalls) ->
    execute_tools_iter(Kernel, ToolCalls, [], []).

%% @private 执行循环体
execute_tools_iter(_Kernel, [], ResultsAcc, CallsAcc) ->
    {ok, lists:reverse(ResultsAcc), lists:reverse(CallsAcc)};
execute_tools_iter(Kernel, [TC | Rest], ResultsAcc, CallsAcc) ->
    {Id, Name, Args} = beamai_function:parse_tool_call(TC),
    case beamai_kernel:invoke_tool(Kernel, Name, Args, beamai_context:new()) of
        {ok, Value, _Ctx} ->
            Result = beamai_function:encode_result(Value),
            Msg = #{role => tool, tool_call_id => Id, content => Result},
            CallRecord = #{name => Name, args => Args, result => Result, tool_call_id => Id},
            execute_tools_iter(Kernel, Rest,
                [Msg | ResultsAcc], [CallRecord | CallsAcc]);
        {interrupt, Reason, PartialResult} ->
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
            execute_tools_iter(Kernel, Rest,
                [Msg | ResultsAcc], [CallRecord | CallsAcc])
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
    {_Id, Name, Args} = beamai_function:parse_tool_call(TC),
    #{tool => Name, arguments => Args}.
