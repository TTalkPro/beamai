%%%-------------------------------------------------------------------
%%% @doc 串行 ToolCallingManager：永远按序执行，无并发
%%%
%%% 忽略 <code>parallel</code> opts，每个 tool_call 严格按序执行。
%%% 适合调试或严格副作用场景（如必须按序写文件 / 调外部非幂等 API）。
%%%
%%% 与 {@link beamai_concurrent_tool_calling_manager} 的唯一差异：
%%% <code>parallel</code> 永远传 <code>false</code>，无论 opts 怎么给。
%%% 底层仍走 {@link beamai_agent_utils:execute_tools/5}（串行路径），
%%% 屏障折叠、错误归一等语义完全一致。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_sequential_tool_calling_manager).

-behaviour(beamai_tool_calling_manager).

%% 构造
-export([new/0]).
%% Behaviour callback
-export([execute_tool_calls/4]).

%%====================================================================
%% 构造
%%====================================================================

%% @doc 构造串行 manager（无状态，Ref 为 atom 占位）。
-spec new() -> beamai_tool_calling_manager:manager().
new() ->
    {?MODULE, default}.

%%====================================================================
%% Behaviour callback
%%====================================================================

%% @doc 委托给 {@link beamai_agent_utils:execute_tools/5}，但 parallel 强制 false。
%%
%% 无论 opts 里 parallel 是 true 还是 false，都传 false 给 execute_tools。
%% 其余（context / on_result）正常透传。
-spec execute_tool_calls(term(), beamai_kernel:kernel(), [map()],
                         beamai_tool_calling_manager:execute_opts()) ->
    beamai_tool_calling_manager:execute_result().
execute_tool_calls(_Ref, Kernel, ToolCalls, Opts) ->
    Context = maps:get(context, Opts, beamai_context:new()),
    %% sequential manager 的核心：忽略 parallel opts，永远串行
    OnResult = maps:get(on_result, Opts, fun(_CR) -> ok end),
    {ToolMsgs, CallRecords, NewContext} =
        beamai_agent_utils:execute_tools(Kernel, ToolCalls, Context, false, OnResult),
    #{messages => ToolMsgs, records => CallRecords, context => NewContext}.
