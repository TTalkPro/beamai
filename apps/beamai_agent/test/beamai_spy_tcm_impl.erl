%%%-------------------------------------------------------------------
%%% @doc Spy ToolCallingManager 实现（供测试观测分派）
%%%
%%% 包一个真实 manager：记录每次 execute_tool_calls 的批次工具名到 ETS，
%%% 然后原样委托给内层 manager。行为零变化，只加观测。
%%%
%%% 用途：证明真实 provider 产生的 tool_calls 确实经 manager seam 分派，
%%% 而不是绕过 manager 直接走 execute_tools。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_spy_tcm_impl).
-behaviour(beamai_tool_calling_manager).

-export([wrap/2, execute_tool_calls/4]).
-export([dispatches/1]).

%% @doc 包住内层 manager，把分派批次记到 Tab（须为 public ETS 表）。
-spec wrap(beamai_tool_calling_manager:manager(), ets:tid()) ->
    beamai_tool_calling_manager:manager().
wrap(Inner, Tab) ->
    {?MODULE, #{inner => Inner, table => Tab}}.

%% @doc 取出所有分派记录，按发生序：每项为一批的工具名列表。
-spec dispatches(ets:tid()) -> [[binary()]].
dispatches(Tab) ->
    [Names || {{dispatch, _Seq}, Names} <- lists:sort(ets:tab2list(Tab))].

-spec execute_tool_calls(term(), beamai_kernel:kernel(), [map()],
                         beamai_tool_calling_manager:execute_opts()) ->
    beamai_tool_calling_manager:execute_result().
execute_tool_calls(#{inner := Inner, table := Tab}, Kernel, ToolCalls, Opts) ->
    Names = [maps:get(name, TC, undefined) || TC <- ToolCalls],
    ets:insert(Tab, {{dispatch, erlang:unique_integer([monotonic])}, Names}),
    beamai_tool_calling_manager:execute_tool_calls(Inner, Kernel, ToolCalls, Opts).
