%%%-------------------------------------------------------------------
%%% @doc Process-native Agent Tool Executor Step
%%%
%%% 实现 beamai_step_behaviour，作为 Process-native Agent 的工具执行环节。
%%% 接收 LLM step 发来的 tool_request 事件，执行所有 tool 调用，
%%% 将结果作为 tool_results 事件发回 LLM step。
%%%
%%% == 工作流程 ==
%%%   1. 接收 tool_request 输入（含 tool_calls 列表）
%%%   2. 使用 Context 中的 Kernel 逐个执行 tool
%%%   3. 发射 tool_results 事件（含所有 tool 执行结果）
%%%
%%% == 配置 ==
%%% ```
%%% #{
%%%     on_tool_call => fun((Name, Args) -> ok | {pause, Reason})
%%%         %% 可选：每个 tool 调用前的钩子
%%% }
%%% ```
%%%
%%% == 输入 ==
%%%   - tool_request: #{tool_calls => [map()], assistant_msg => map()}
%%%
%%% == 输出事件 ==
%%%   - tool_results: [#{tool_call_id => binary(), name => binary(),
%%%                      args => map(), result => binary()}]
%%%
%%% == HITL 支持 ==
%%% 当配置了 on_tool_call 钩子且返回 {pause, Reason} 时，
%%% step 返回 {pause, ...}，由 Process 暂停等待人工介入。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_agent_tool_step).

-behaviour(beamai_step_behaviour).

-export([init/1, can_activate/2, on_activate/3, on_resume/3]).

%%====================================================================
%% beamai_step_behaviour 回调
%%====================================================================

init(Config) ->
    OnToolCall = maps:get(on_tool_call, Config, undefined),
    {ok, #{on_tool_call => OnToolCall}}.

can_activate(Inputs, _State) ->
    maps:is_key(tool_request, Inputs).

on_activate(#{tool_request := #{tool_calls := ToolCalls}}, State, Context) ->
    #{on_tool_call := OnToolCall} = State,

    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, no_kernel_in_context};
        Kernel ->
            execute_all_tools(Kernel, ToolCalls, OnToolCall, [], State)
    end;
on_activate(#{tool_request := ToolCalls}, State, Context) when is_list(ToolCalls) ->
    %% 兼容直接传列表的情况
    #{on_tool_call := OnToolCall} = State,
    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, no_kernel_in_context};
        Kernel ->
            execute_all_tools(Kernel, ToolCalls, OnToolCall, [], State)
    end.

on_resume(ResumeData, State, Context) ->
    %% 恢复时，ResumeData 应包含剩余的 tool_calls
    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, no_kernel_in_context};
        Kernel ->
            RemainingCalls = maps:get(remaining_tool_calls, ResumeData, []),
            CompletedResults = maps:get(completed_results, ResumeData, []),
            #{on_tool_call := OnToolCall} = State,
            execute_all_tools(Kernel, RemainingCalls, OnToolCall, CompletedResults, State)
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 逐个执行 tool_calls 列表
%%
%% 遍历所有 tool_call，每个执行前先通过 on_tool_call 钩子检查。
%% 全部执行完毕后发射 tool_results 事件。
execute_all_tools(_Kernel, [], _OnToolCall, ResultsAcc, State) ->
    Results = lists:reverse(ResultsAcc),
    Event = beamai_process_event:new(tool_results, Results),
    {ok, #{events => [Event], state => State}};
execute_all_tools(Kernel, [TC | Rest], OnToolCall, ResultsAcc, State) ->
    {Id, Name, Args} = beamai_function:parse_tool_call(TC),
    case check_tool_hook(OnToolCall, Name, Args) of
        ok ->
            Result = execute_single_tool(Kernel, Id, Name, Args),
            execute_all_tools(Kernel, Rest, OnToolCall, [Result | ResultsAcc], State);
        {pause, Reason} ->
            PauseInfo = #{
                reason => Reason,
                interrupted_tool => #{name => Name, args => Args, tool_call_id => Id},
                completed_results => lists:reverse(ResultsAcc),
                remaining_tool_calls => [TC | Rest]
            },
            {pause, PauseInfo, State}
    end.

%% @private 执行单个 tool 并返回结果 map
%%
%% 通过 kernel:invoke_tool 执行，错误时返回编码后的错误信息。
execute_single_tool(Kernel, Id, Name, Args) ->
    ResultBinary = case beamai_kernel:invoke_tool(Kernel, Name, Args, beamai_context:new()) of
        {ok, Value, _Ctx} ->
            beamai_function:encode_result(Value);
        {error, Reason} ->
            beamai_function:encode_result(#{error => Reason})
    end,
    #{tool_call_id => Id, name => Name, args => Args, result => ResultBinary}.

%% @private 检查 on_tool_call 钩子
%%
%% 未配置钩子时直接放行；钩子返回 {pause, Reason} 时触发暂停。
check_tool_hook(undefined, _Name, _Args) -> ok;
check_tool_hook(Fun, Name, Args) when is_function(Fun, 2) ->
    try Fun(Name, Args) of
        {pause, Reason} -> {pause, Reason};
        _ -> ok
    catch
        _:_ -> ok
    end.
