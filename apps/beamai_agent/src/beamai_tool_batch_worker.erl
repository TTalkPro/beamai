%%%-------------------------------------------------------------------
%%% @doc 批级隔离 worker：ToolCallingManager 的进程边界
%%%
%%% 把「一批工具的执行」整体搬进一个**动态 spawn_monitor 的进程**跑，使工具
%%% 执行**永远**无法带崩调用者（用户的 agent 进程）。
%%%
%%% <b>为什么隔离在 manager 级、而非每工具一个进程</b>：工具存在「必须串行
%%% 执行」的场景（副作用顺序、非幂等外部 API）。隔离若做到每工具一进程，
%%% 就把「隔离」和「调度」绑死了。这里的划分是：
%%% <ul>
%%%   <li><b>进程边界</b> = manager 级，恒定一层，与批次大小/策略无关</li>
%%%   <li><b>串行 / 并发</b> = worker <b>内部</b>的调度自由（见
%%%       {@link beamai_agent_utils:execute_tools/5}），不影响隔离</li>
%%% </ul>
%%% 于是 sequential manager 同样拿到独立进程——副作用最重的工具不再跑在
%%% 最没保护的路径上。
%%%
%%% <b>为什么 spawn_monitor 而非 link</b>：monitor 是单向观测，父进程不会被
%%% worker 的退出信号波及；且 DOWN 携带 Reason 可归因，便于跟踪任务与后续的
%%% 重启 / 重试。link 是双向的——正是它把工具的崩溃传导给用户进程。
%%%
%%% <b>为什么 try/catch 不够</b>：`beamai_tool:call_handler/4' 的 try/catch 只
%%% 挡得住 handler 自身 raise 的 error/throw/exit。挡不住：
%%% <ul>
%%%   <li>工具内 <code>spawn_link</code> 的子进程崩溃 → 退出信号绕过 try/catch
%%%       直接打死宿主进程</li>
%%%   <li>工具卡死 → try/catch 不涉及时间，宿主陪着一起挂起</li>
%%% </ul>
%%% 二者都只有进程边界能挡。
%%%
%%% <b>失败语义（批粒度）</b>：worker 崩溃 → 为**整批**每个 tool_call 合成
%%% error 结果（与正常结果同构，Writes 空），Context 退回批前快照（本批工具的
%%% writes 全部丢弃）。这是 manager 级隔离的代价：故障单元是「批」而非「单个
%%% 工具」。换来的是调用者恒定存活。
%%%
%%% <b>缺省不限时</b>：本层只管隔离，不替调用者决定何时放弃——工具跑多久就等
%%% 多久（见 {@link batch_timeout/1}）。要限时须显式声明：工具级经 tool_spec 的
%%% `timeout'，批级经 manager 构造时的 `batch_timeout'。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_batch_worker).

-export([execute_isolated/5, execute_isolated/6]).
-export([batch_timeout/1]).

%% @doc 在隔离进程中执行一批工具，返回与 execute_tools/5 同构的三元组。
%%
%% Parallel 透传给 worker 内部的 {@link beamai_agent_utils:execute_tools/5}
%% ——由它决定批内串行还是并发。本函数只负责进程边界。
%%
%% OnResult 在**调用者进程**触发（worker 内用 proxy 转发回来），与隔离前的
%% 语义一致：进度回调属于用户进程，不该跑进 worker。
%%
%% @returns {ToolResultMsgs, CallRecords, NewContext}
-spec execute_isolated(beamai_kernel:kernel(), [map()], beamai_context:t(),
                       boolean(), fun((map()) -> ok)) ->
    {[map()], [map()], beamai_context:t()}.
execute_isolated(Kernel, ToolCalls, Context, Parallel, OnResult) ->
    execute_isolated(Kernel, ToolCalls, Context, Parallel, OnResult, #{}).

%% @doc 同 {@link execute_isolated/5}，并应用 manager 级执行策略
%% （见 {@link beamai_tool_calling_manager:manager_opts()}）。
-spec execute_isolated(beamai_kernel:kernel(), [map()], beamai_context:t(),
                       boolean(), fun((map()) -> ok),
                       beamai_tool_calling_manager:manager_opts()) ->
    {[map()], [map()], beamai_context:t()}.
execute_isolated(_Kernel, [], Context, _Parallel, _OnResult, _MgrOpts) ->
    %% 空批次不值得起进程
    {[], [], Context};
execute_isolated(Kernel, ToolCalls, Context0, Parallel, OnResult, MgrOpts) ->
    %% manager 的缺省工具超时经 context 下发给 beamai_tool:invoke/3（工具自身
    %% 声明的 timeout 仍优先）。恒定注入——传 undefined 即清除，使每批的策略只
    %% 由其 manager 决定，不粘上一批的残值（context 跨轮穿线）。
    Context = beamai_context:with_default_tool_timeout(
                Context0, maps:get(tool_timeout, MgrOpts, undefined)),
    Parent = self(),
    Tag = make_ref(),
    %% proxy：worker 内每工具完成即把 CallRecord 转回父进程，由父进程触发真回调
    Proxy = fun(CR) -> Parent ! {tcm_on_result, Tag, CR}, ok end,
    {Pid, MRef} = spawn_monitor(fun() ->
        Result = beamai_agent_utils:execute_tools(Kernel, ToolCalls, Context,
                                                  Parallel, Proxy),
        Parent ! {tcm_batch_result, Tag, Result}
    end),
    Deadline = deadline(batch_timeout(MgrOpts)),
    %% 失败合成用注入前的 Context0：策略是执行细节，不该渗进返回给调用方的状态
    await_batch(Tag, Pid, MRef, ToolCalls, Context0, OnResult, Deadline).

%% @private infinity 的截止即无截止
deadline(infinity) -> infinity;
deadline(Timeout) -> erlang:monotonic_time(millisecond) + Timeout.

%% @private 等待 worker 交付：转发进度回调、收批结果、崩溃/超时兜底
%%
%% 同一发送者到同一接收者的消息保序，故 worker 的 tcm_on_result 必然排在
%% tcm_batch_result 之前 —— 拿到批结果时进度回调已全部触发过，无需 drain。
await_batch(Tag, Pid, MRef, ToolCalls, Context, OnResult, Deadline) ->
    receive
        {tcm_on_result, Tag, CallRecord} ->
            _ = OnResult(CallRecord),
            await_batch(Tag, Pid, MRef, ToolCalls, Context, OnResult, Deadline);
        {tcm_batch_result, Tag, {Msgs, Records, NewCtx}} ->
            erlang:demonitor(MRef, [flush]),
            %% 剥掉下发的执行策略：它是本批的执行细节，不该随 context 穿到下一轮
            %% 或被持久化进暂停载荷
            {Msgs, Records, beamai_context:with_default_tool_timeout(NewCtx, undefined)};
        {'DOWN', MRef, process, Pid, Reason} ->
            %% worker 被工具带崩（含 link 传播的退出信号）：整批合成 error
            synth_batch(ToolCalls, Context, tool_batch_crash, Reason, OnResult)
    after remaining(Deadline) ->
        %% 仅当调用方显式配了 batch_timeout 才会走到这里（缺省 infinity）：
        %% kill worker，整批合成 timeout
        erlang:demonitor(MRef, [flush]),
        exit(Pid, kill),
        drain(Tag),
        synth_batch(ToolCalls, Context, tool_timeout, batch_timeout_exceeded, OnResult)
    end.

%% @private 距截止还剩多久（infinity 即不设限，永远等 worker 交付或 DOWN）
remaining(infinity) -> infinity;
remaining(Deadline) -> max(0, Deadline - erlang:monotonic_time(millisecond)).

%% @private 丢弃被 kill 的 worker 留在信箱里的在途进度消息（本 Tag 唯一，不误伤）
drain(Tag) ->
    receive
        {tcm_on_result, Tag, _} -> drain(Tag);
        {tcm_batch_result, Tag, _} -> drain(Tag)
    after 0 -> ok
    end.

%% @private 整批合成 error 结果：Context 退回批前快照（本批 writes 全丢）
%%
%% 注意：批内已完成的工具会再触发一次 OnResult（携 error 记录）。批失败时其
%% 结果本就作废，最终态即 error——进度 UX 可能看到该工具的重复记录。
synth_batch(ToolCalls, Context, Type, Reason, OnResult) ->
    Ordered = [begin
        {_Msg, CallRecord, _W} = R = beamai_agent_utils:synth_result(TC, Type, Reason),
        _ = OnResult(CallRecord),
        R
    end || TC <- ToolCalls],
    {[M || {M, _R, _W} <- Ordered], [R || {_M, R, _W} <- Ordered], Context}.

%% @doc 批级执行的截止时长（毫秒），**缺省 infinity —— 不设截止**。
%%
%% 本层的职责是**隔离**（调用者恒存活），不是替调用者决定何时放弃：工具跑多久
%% 就等多久，要不要杀由调用者决定。故缺省一直等 worker 交付或 DOWN。
%%
%% 只有 manager 构造时显式给了 `batch_timeout' 才限时。给的时候注意它与内层
%% `tool_gather_timeout' 的次序：本层计时从 spawn **之前**开始、内层从 spawn
%% **之后**才起算，故本层若不严格大于内层，就会抢在内层前面响，把并发路径本可
%% 保住的**部分结果**（其他工具的真结果 + 卡死者的 timeout error）整批冲成
%% 错误。缺省 infinity 时这个次序天然成立。
-spec batch_timeout(beamai_tool_calling_manager:manager_opts()) ->
    pos_integer() | infinity.
batch_timeout(MgrOpts) ->
    maps:get(batch_timeout, MgrOpts, infinity).
