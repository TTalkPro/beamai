%%%-------------------------------------------------------------------
%%% @doc Agent 共享工具函数
%%%
%%% 提供 beamai_agent 的基础工具函数：
%%%   - LLM 响应内容提取
%%%   - Chat 选项构建（tool specs 注入）
%%%   - Tool 执行辅助
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_utils).

-export([
    extract_content/1,
    build_chat_opts/2,
    execute_tools/2,
    execute_tools/3,
    execute_tools/4,
    run_one_tool/3,
    tool_error/1
]).

%%====================================================================
%% API
%%====================================================================

%% @doc 从 LLM 响应中提取文本内容，统一归一为 binary
%%
%% 响应 content 的类型为 binary() | null（多模态是输入侧能力，响应文本恒为
%% binary/null）。binary 原样返回；其余（含 null 及任何意外形态）兜底为空
%% 二进制，绝不 function_clause 崩溃。
-spec extract_content(map()) -> binary().
extract_content(Response) ->
    case beamai_llm_response:content(Response) of
        Content when is_binary(Content) -> Content;
        _ -> <<>>
    end.

%% @doc 从 Kernel 构建 chat 选项
%%
%% 获取 kernel 中所有已注册函数的 tool specs，注入到 chat 选项中。
%% 如果 kernel 中没有注册任何函数，不添加 tools 字段。
%%
%% @param Kernel kernel 实例
%% @param Opts 用户额外选项（可包含 chat_opts 子键）
%% @returns 完整的 chat 选项 map
-spec build_chat_opts(beamai_kernel:kernel(), map()) -> map().
build_chat_opts(Kernel, Opts) ->
    ToolSpecs = beamai_kernel:get_tool_specs(Kernel),
    BaseChatOpts = maps:get(chat_opts, Opts, #{}),
    case ToolSpecs of
        [] -> BaseChatOpts;
        _ -> BaseChatOpts#{
            tools => ToolSpecs,
            tool_choice => maps:get(tool_choice, BaseChatOpts, auto)
        }
    end.

%% @doc 执行 tool calls 并收集结果（串行，新建 context）
%%
%% @returns {ToolResultMsgs, CallRecords}
-spec execute_tools(beamai_kernel:kernel(), [map()]) -> {[map()], [map()]}.
execute_tools(Kernel, ToolCalls) ->
    execute_tools(Kernel, ToolCalls, beamai_context:new()).

%% @doc 执行 tool calls 并收集结果（串行，指定执行上下文）
%%
%% 用传入的 Context（含 conversation_id 等）执行工具，使 around_tool filter
%% 能读到当轮上下文。工具对 context 的修改不回写（结果以 tool 消息累积）。
-spec execute_tools(beamai_kernel:kernel(), [map()], beamai_context:t()) -> {[map()], [map()]}.
execute_tools(Kernel, ToolCalls, Context) ->
    execute_tools(Kernel, ToolCalls, Context, false).

%% @doc 执行 tool calls（统一入口：可选并发）
%%
%% Parallel=true 且 tool_call 多于一个时，每个 tool 一个被监控的工作进程并发
%% 执行、按原顺序 gather；否则串行。两个列表（tool 消息、调用记录）均按原
%% tool_call 顺序返回。这是 agent 全链工具执行的**单一实现**（主循环与中断
%% 分支共用），避免逻辑漂移。
%%
%% @returns {ToolResultMsgs, CallRecords}
-spec execute_tools(beamai_kernel:kernel(), [map()], beamai_context:t(), boolean()) ->
    {[map()], [map()]}.
execute_tools(Kernel, ToolCalls, Context, Parallel) ->
    case Parallel andalso length(ToolCalls) > 1 of
        true -> execute_concurrent(Kernel, ToolCalls, Context);
        false -> execute_sequential(Kernel, ToolCalls, Context)
    end.

%% @doc 执行单个 tool：经完整 filter 管道调用，编码结果，构建 tool 消息与调用记录
%%
%% @returns {ToolMsg, CallRecord}
-spec run_one_tool(beamai_kernel:kernel(), map(), beamai_context:t()) -> {map(), map()}.
run_one_tool(Kernel, TC, Context) ->
    {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
    Result = case beamai_kernel:invoke_tool(Kernel, Name, Args, Context) of
        {ok, Value, _Ctx} -> beamai_tool:encode_result(Value);
        {error, Reason} -> beamai_tool:encode_result(tool_error(Reason))
    end,
    Msg = #{role => tool, tool_call_id => Id, content => Result},
    CallRecord = #{name => Name, args => Args, result => Result, tool_call_id => Id},
    {Msg, CallRecord}.

%% @doc 把工具错误原因归一为稳定、JSON 友好的结构，便于 LLM 可靠解析自我纠错
%%
%% - map（已是结构化错误）：原样置于 error 下
%% - 其它（atom/tuple/binary/...）：归一为 #{type, message}
-spec tool_error(term()) -> #{error := map()}.
tool_error(Reason) when is_map(Reason) ->
    #{error => Reason};
tool_error(Reason) ->
    #{error => #{type => <<"tool_error">>, message => reason_to_binary(Reason)}}.

%% @private 把任意 reason 转为人类可读 binary
reason_to_binary(R) when is_binary(R) -> R;
reason_to_binary(R) when is_atom(R) -> atom_to_binary(R, utf8);
reason_to_binary(R) -> iolist_to_binary(io_lib:format("~p", [R])).

%%====================================================================
%% Internal - Tool 执行（串行 / 并发）
%%====================================================================

%% @private 串行执行 tool_calls
execute_sequential(Kernel, ToolCalls, Context) ->
    {Msgs, Records} = lists:foldl(fun(TC, {MAcc, RAcc}) ->
        {Msg, CR} = run_one_tool(Kernel, TC, Context),
        {[Msg | MAcc], [CR | RAcc]}
    end, {[], []}, ToolCalls),
    {lists:reverse(Msgs), lists:reverse(Records)}.

%% @private 并发执行 tool_calls：每个 tool 一个被监控的工作进程
%%
%% 工作进程把 {tool_result, self(), {Msg, CallRecord}} 回传给父进程；进程意外
%% 崩溃（未走 invoke_tool 的 {error,_} 路径）由 'DOWN' 兜底合成 error tool 消息。
%% 结果按原 tool_call 顺序（索引）重排。
execute_concurrent(Kernel, ToolCalls, Context) ->
    Parent = self(),
    Indexed = lists:zip(lists:seq(1, length(ToolCalls)), ToolCalls),
    Workers = lists:foldl(fun({Idx, TC}, Acc) ->
        {Pid, MRef} = spawn_monitor(fun() ->
            Parent ! {tool_result, self(), run_one_tool(Kernel, TC, Context)}
        end),
        Acc#{Pid => #{idx => Idx, tc => TC, mref => MRef}}
    end, #{}, Indexed),
    ResultMap = collect_tools(Workers, #{}),
    Ordered = [maps:get(Idx, ResultMap) || {Idx, _TC} <- Indexed],
    lists:unzip(Ordered).

%% @private 收集并发工作进程结果，直到每个 tool 都有结果（idx 为键）
collect_tools(Workers, Acc) when map_size(Acc) =:= map_size(Workers) ->
    Acc;
collect_tools(Workers, Acc) ->
    receive
        {tool_result, Pid, RC} ->
            #{idx := Idx, mref := MRef} = maps:get(Pid, Workers),
            erlang:demonitor(MRef, [flush]),
            collect_tools(Workers, Acc#{Idx => RC});
        {'DOWN', _MRef, process, Pid, Reason} ->
            case maps:get(Pid, Workers, undefined) of
                undefined ->
                    collect_tools(Workers, Acc);
                #{idx := Idx, tc := TC} ->
                    case maps:is_key(Idx, Acc) of
                        true -> collect_tools(Workers, Acc);  %% 结果已先到，DOWN 忽略
                        false -> collect_tools(Workers, Acc#{Idx => crash_result(TC, Reason)})
                    end
            end
    end.

%% @private 工作进程崩溃时合成 error tool 结果（保持与正常结果同构）
crash_result(TC, Reason) ->
    {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
    Result = beamai_tool:encode_result(
        #{error => #{type => tool_worker_crash,
                     reason => iolist_to_binary(io_lib:format("~p", [Reason]))}}),
    Msg = #{role => tool, tool_call_id => Id, content => Result},
    CallRecord = #{name => Name, args => Args, result => Result, tool_call_id => Id},
    {Msg, CallRecord}.
