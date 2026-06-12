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
    messages := [map()],            %% 已有上下文（resume 时为中断时携带的完整 messages）
    new_messages => [map()],        %% 本轮新增消息（入口处持久化后并入）
    load_history => boolean(),      %% 是否前接跨轮历史（首轮 true，resume false）
    chat_opts := map(),             %% 含 context 与 system_prompts
    callbacks := map(),
    max_iterations := pos_integer(),
    max_tool_iterations := pos_integer(),  %% agent 配置的总迭代上限（中断上下文计数用）
    parallel_tools := boolean(),    %% 一轮多个 tool_call 是否并发执行
    interrupt_tools := [map()],     %% 中断 tool 定义列表
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
    {SafeResults, SafeCallRecords} =
        beamai_agent_utils:execute_tools(Kernel, SafeCalls, ctx(Opts), Parallel),
    AllResults = SafeResults ++ [skipped_result(TC) || TC <- SkippedCalls],
    fire_tool_results(Callbacks, SafeCallRecords),
    persist(Opts, AllResults),
    Context = build_interrupt_context(MaxIter - N, AllResults, InterruptedTC,
                                      ToolCallsMade ++ SafeCallRecords, Reason,
                                      Messages ++ AllResults),
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
execute_and_continue(TCs, Opts, N, ToolCallsMade) ->
    #{kernel := Kernel, callbacks := Callbacks, messages := Messages} = Opts,
    Parallel = maps:get(parallel_tools, Opts, true),
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
