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
    execute_tools/5,
    run_one_tool/3,
    synth_result/3,
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
%% @returns {ToolResultMsgs, CallRecords, NewContext}
-spec execute_tools(beamai_kernel:kernel(), [map()]) -> {[map()], [map()], beamai_context:t()}.
execute_tools(Kernel, ToolCalls) ->
    execute_tools(Kernel, ToolCalls, beamai_context:new()).

%% @doc 执行 tool calls 并收集结果（串行，指定执行上下文）
%%
%% 用传入的 Context（只读运行环境，含 conversation_id 等）执行工具，使
%% around_tool filter 能读到当轮上下文。工具写状态经返回 Writes 表达，在屏障处
%% 按 tool_call 原始序折叠进 state（见 finalize/3）。
-spec execute_tools(beamai_kernel:kernel(), [map()], beamai_context:t()) ->
    {[map()], [map()], beamai_context:t()}.
execute_tools(Kernel, ToolCalls, Context) ->
    execute_tools(Kernel, ToolCalls, Context, false).

%% @doc 执行 tool calls（统一入口：可选并发）
%%
%% Parallel=true 且 tool_call 多于一个时，每个 tool 一个被监控的工作进程并发
%% 执行、按原顺序 gather；否则串行。两条路径**状态语义统一**为「每个工具拿
%% 轮初 Context 快照、屏障处按 tool_call 原始序折叠 writes」——串行只改执行
%% 时序，不引入批内穿线。返回的 NewContext 携带折叠后的 state（+ filter 私有
%% 状态合并），供调用方跨轮穿线。这是 agent 全链工具执行的**单一实现**
%% （主循环与中断分支共用），避免逻辑漂移。
%%
%% @returns {ToolResultMsgs, CallRecords, NewContext}
-spec execute_tools(beamai_kernel:kernel(), [map()], beamai_context:t(), boolean()) ->
    {[map()], [map()], beamai_context:t()}.
execute_tools(Kernel, ToolCalls, Context, Parallel) ->
    execute_tools(Kernel, ToolCalls, Context, Parallel, fun(_CR) -> ok end).

%% @doc 执行 tool calls（带实时结果回调）
%%
%% OnResult 在**每个工具完成即触发**（并发时在结果落地点、串行时逐个），传入该
%% 工具的 CallRecord。进度 UX 的实时性优先于顺序性——并发时触发顺序不确定；
%% 需确定顺序的消费返回的 CallRecords（按原始 tool_call 序）。OnResult 应自身
%% 吞异常（调用方通常传 beamai_agent_callbacks:invoke 包装）。
%%
%% @returns {ToolResultMsgs, CallRecords, NewContext}
-spec execute_tools(beamai_kernel:kernel(), [map()], beamai_context:t(), boolean(),
                    fun((map()) -> ok)) ->
    {[map()], [map()], beamai_context:t()}.
execute_tools(Kernel, ToolCalls, Context, Parallel, OnResult) ->
    Concurrent = Parallel
        andalso length(ToolCalls) > 1
        andalso not batch_has_serial(Kernel, ToolCalls),
    case Concurrent of
        true -> execute_concurrent(Kernel, ToolCalls, Context, OnResult);
        false -> execute_sequential(Kernel, ToolCalls, Context, OnResult)
    end.

%% @private 批内是否含 serial 工具（命中则整批退化串行，保住副作用顺序）
batch_has_serial(Kernel, ToolCalls) ->
    lists:any(fun(TC) ->
        {_Id, Name, _Args} = beamai_tool:parse_tool_call(TC),
        beamai_kernel:serial_tool(Kernel, Name)
    end, ToolCalls).

%% @doc 执行单个 tool：经完整 filter 管道调用，编码结果，构建 tool 消息与调用记录
%%
%% Context 为只读运行环境快照；工具写意图经 invoke_tool 返回的 Writes 透出。
%%
%% @returns {ToolMsg, CallRecord, Writes}
-spec run_one_tool(beamai_kernel:kernel(), map(), beamai_context:t()) ->
    {map(), map(), beamai_context:writes()}.
run_one_tool(Kernel, TC, Context) ->
    {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
    {Result, Writes, ErrInfo} = case beamai_kernel:invoke_tool(Kernel, Name, Args, Context) of
        {ok, Value, W} ->
            {beamai_tool:encode_result(Value), W, undefined};
        {error, Reason} ->
            %% 失败：分类为 semantic|transient|environment，供屏障处分层路由
            {beamai_tool:encode_result(tool_error(Reason)), #{},
             #{class => beamai_tool_error:classify(Reason),
               message => beamai_tool_error:message(Reason)}}
    end,
    %% tool 结果消息带可选 writes 元数据（该工具写意图）：**只进存储、不发 LLM**
    %% （message_adapter 白名单构建 wire 消息，writes 天然剥落）。event-sourcing 伏笔
    %% + 审计。失败工具 Writes=#{} → 无 writes 键（历史自然缺席，与事务性同真同假）。
    Msg = with_writes(#{role => tool, tool_call_id => Id, content => Result}, Writes),
    CallRecord0 = #{name => Name, args => Args, result => Result, tool_call_id => Id},
    {Msg, with_error(CallRecord0, ErrInfo), Writes}.

%% @private 成功无 error 字段；失败带 #{class, message}（屏障处按 class 路由）
with_error(CallRecord, undefined) -> CallRecord;
with_error(CallRecord, ErrInfo) -> CallRecord#{error => ErrInfo}.

%% @private 非空 writes 附到 tool 结果消息（存储用；wire 层剥离）
with_writes(Msg, Writes) when map_size(Writes) =:= 0 -> Msg;
with_writes(Msg, Writes) -> Msg#{writes => Writes}.

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

%% @private 串行执行 tool_calls（每个工具拿同一轮初快照，屏障处折叠）
%% 每个工具完成即触发 OnResult（实时）。
execute_sequential(Kernel, ToolCalls, Context, OnResult) ->
    Ordered = [fire_result(OnResult, run_one_tool(Kernel, TC, Context)) || TC <- ToolCalls],
    finalize(Kernel, Context, Ordered).

%% @private 触发实时结果回调并原样返回结果三元组
fire_result(OnResult, {_Msg, CallRecord, _Writes} = Result) ->
    _ = OnResult(CallRecord),
    Result.

%% @private 并发执行 tool_calls：每个 tool 一个被监控的工作进程
%%
%% 工作进程把 {tool_result, self(), {Msg, CallRecord, Writes}} 回传给父进程；
%% 进程意外崩溃（未走 invoke_tool 的 {error,_} 路径）由 'DOWN' 兜底合成 error
%% tool 消息（Writes 空）。全局收集有截止时间（app env `tool_gather_timeout`，
%% 默认 2 分钟）：超时后 kill 未完成的工作进程、为其合成 timeout error 结果，
%% 避免单个卡死工具冻结整个 tool loop 并泄漏 worker。结果按原 tool_call 顺序
%% （索引）重排，屏障处折叠 writes。
execute_concurrent(Kernel, ToolCalls, Context, OnResult) ->
    Parent = self(),
    Indexed = lists:zip(lists:seq(1, length(ToolCalls)), ToolCalls),
    Workers = lists:foldl(fun({Idx, TC}, Acc) ->
        {Pid, MRef} = spawn_monitor(fun() ->
            Parent ! {tool_result, self(), run_one_tool(Kernel, TC, Context)}
        end),
        Acc#{Pid => #{idx => Idx, tc => TC, mref => MRef}}
    end, #{}, Indexed),
    Deadline = erlang:monotonic_time(millisecond) + gather_timeout(),
    ResultMap = collect_tools(Workers, #{}, Deadline, OnResult),
    Ordered = [maps:get(Idx, ResultMap) || {Idx, _TC} <- Indexed],
    finalize(Kernel, Context, Ordered).

%% @private 屏障收尾：拆分消息/记录、按原始 index 序折叠 writes 进 state
%%
%% Ordered 为按 tool_call 原始顺序排好的 [{Msg, CallRecord, Writes}]。折叠经
%% beamai_context:apply_writes/3：声明槽过 reducer、未声明槽 last-writer；
%% 同批多工具写同一未声明槽产生 conflict，记 warning（last-writer 已应用）。
finalize(Kernel, Context, Ordered) ->
    Msgs = [M || {M, _R, _W} <- Ordered],
    Records = [R || {_M, R, _W} <- Ordered],
    IndexedWrites = lists:zip(lists:seq(1, length(Ordered)),
                              [W || {_M, _R, W} <- Ordered]),
    Slots = beamai_kernel:state_slots(Kernel),
    {NewCtx, Conflicts} = beamai_context:apply_writes(Context, IndexedWrites, Slots),
    warn_conflicts(Conflicts),
    {Msgs, Records, NewCtx}.

%% @private 状态槽冲突告警（同批多工具写未声明槽，last-writer 已按原序应用）
warn_conflicts([]) -> ok;
warn_conflicts(Conflicts) ->
    logger:warning("beamai_agent: 状态槽冲突（同批多个工具写入未声明槽，已按 "
                   "tool_call 原始序取 last-writer）：~p", [Conflicts]).

%% @private 收集并发工作进程结果，直到每个 tool 都有结果（idx 为键）或超时
%% 每个结果落地即触发 OnResult（实时；顺序不确定）。
collect_tools(Workers, Acc, _Deadline, _OnResult) when map_size(Acc) =:= map_size(Workers) ->
    Acc;
collect_tools(Workers, Acc, Deadline, OnResult) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {tool_result, Pid, RC} ->
            case maps:get(Pid, Workers, undefined) of
                undefined ->
                    %% 上一批被 kill 的 worker 在死前发出的迟到结果，忽略
                    collect_tools(Workers, Acc, Deadline, OnResult);
                #{idx := Idx, mref := MRef} ->
                    erlang:demonitor(MRef, [flush]),
                    _ = fire_result(OnResult, RC),
                    collect_tools(Workers, Acc#{Idx => RC}, Deadline, OnResult)
            end;
        {'DOWN', _MRef, process, Pid, Reason} ->
            case maps:get(Pid, Workers, undefined) of
                undefined ->
                    collect_tools(Workers, Acc, Deadline, OnResult);
                #{idx := Idx, tc := TC} ->
                    case maps:is_key(Idx, Acc) of
                        true -> collect_tools(Workers, Acc, Deadline, OnResult);  %% 结果已先到，DOWN 忽略
                        false ->
                            R = synth_result(TC, tool_worker_crash, Reason),
                            _ = fire_result(OnResult, R),
                            collect_tools(Workers, Acc#{Idx => R}, Deadline, OnResult)
                    end
            end
    after Remaining ->
        kill_pending(Workers, Acc, OnResult)
    end.

%% @private 超时收尾：kill 所有未交付结果的 worker，并合成 timeout error 结果
kill_pending(Workers, Acc, OnResult) ->
    maps:fold(fun(Pid, #{idx := Idx, tc := TC, mref := MRef}, A) ->
        case maps:is_key(Idx, A) of
            true ->
                A;
            false ->
                erlang:demonitor(MRef, [flush]),
                exit(Pid, kill),
                R = synth_result(TC, tool_timeout, gather_timeout_exceeded),
                _ = fire_result(OnResult, R),
                A#{Idx => R}
        end
    end, Acc, Workers).

%% @doc 合成 error tool 结果（保持与正常结果同构；Writes 空 → 不参与折叠）
%% crash/timeout 均归为 transient 类（重试/等待有意义），带 error 供屏障路由。
%% 并发路径的 DOWN/超时兜底用它；批级隔离失败（{@link beamai_tool_batch_worker}）
%% 也用它为整批合成结果——两处的 error 结果形状必须一致。
-spec synth_result(map(), atom(), term()) ->
    {map(), map(), beamai_context:writes()}.
synth_result(TC, Type, Reason) ->
    {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
    Msg0 = iolist_to_binary(io_lib:format("~p", [Reason])),
    Result = beamai_tool:encode_result(
        #{error => #{type => Type, reason => Msg0}}),
    Msg = #{role => tool, tool_call_id => Id, content => Result},
    CallRecord = #{name => Name, args => Args, result => Result, tool_call_id => Id,
                   error => #{class => beamai_tool_error:classify(Type), message => Msg0}},
    {Msg, CallRecord, #{}}.

%% @private 并发收集的全局截止时长（毫秒，默认 2 分钟）
%% 长耗时工具场景请通过 app env tool_gather_timeout 显式调大
gather_timeout() ->
    application:get_env(beamai_agent, tool_gather_timeout, 120000).
