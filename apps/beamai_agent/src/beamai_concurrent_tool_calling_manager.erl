%%%-------------------------------------------------------------------
%%% @doc 并发 ToolCallingManager：spawn_monitor 并发，尊重 parallel / serial 声明
%%%
%%% 默认实现——现状行为零变化。串行/并发两条路径、屏障折叠、错误归一、
%%% 合成结果全部保留（均由 {@link beamai_agent_utils:execute_tools/5} 处理）。
%%%
%%% 并发退化规则（由 execute_tools 内部判定）：
%%% <ul>
%%%   <li><code>parallel=true</code> 且批内 >1 个 call 且无 serial 工具 → 并发</li>
%%%   <li>否则 → 串行</li>
%%% </ul>
%%%
%%% 选择此 manager = 选择「尊重 parallel opts」的执行策略。
%%% 如需强制全串行（忽略 parallel opts），用
%%% {@link beamai_sequential_tool_calling_manager}。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_concurrent_tool_calling_manager).

-behaviour(beamai_tool_calling_manager).

%% 构造
-export([new/0]).
%% Behaviour callback
-export([execute_tool_calls/4]).

%%====================================================================
%% 构造
%%====================================================================

%% @doc 构造并发 manager（无状态，Ref 为 atom 占位）。
-spec new() -> beamai_tool_calling_manager:manager().
new() ->
    {?MODULE, default}.

%%====================================================================
%% Behaviour callback
%%====================================================================

%% @doc 委托给 {@link beamai_agent_utils:execute_tools/5}。
%%
%% Unpack opts map 为 positional args，调 execute_tools，把返回的三元组
%% <code>&#123;ToolMsgs, CallRecords, NewContext&#125;</code> 包成 result map。
%% parallel opts 透传给 execute_tools（由其内部判定并发退化）。
-spec execute_tool_calls(term(), beamai_kernel:kernel(), [map()],
                         beamai_tool_calling_manager:execute_opts()) ->
    beamai_tool_calling_manager:execute_result().
execute_tool_calls(_Ref, Kernel, ToolCalls, Opts) ->
    Context = maps:get(context, Opts, beamai_context:new()),
    Parallel = maps:get(parallel, Opts, false),
    OnResult = maps:get(on_result, Opts, fun(_CR) -> ok end),
    {ToolMsgs, CallRecords, NewContext} =
        beamai_agent_utils:execute_tools(Kernel, ToolCalls, Context, Parallel, OnResult),
    #{messages => ToolMsgs, records => CallRecords, context => NewContext}.
