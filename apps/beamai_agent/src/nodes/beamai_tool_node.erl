%%%-------------------------------------------------------------------
%%% @doc 工具执行节点模块
%%%
%%% 负责工具的执行逻辑，包括：
%%%   - 执行工具调用
%%%   - 处理工具结果
%%%   - 构建工具结果消息
%%%   - 触发工具相关回调
%%%
%%% 从 beamai_nodes.erl 拆分而来，专注于工具执行逻辑。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_node).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 节点创建 API
-export([create/1]).

%% 工具辅助函数
-export([build_handlers/1]).

%%====================================================================
%% 节点创建 API
%%====================================================================

%% @doc 创建工具执行节点
%%
%% 创建一个执行工具调用的图节点函数。
%% 从状态中获取 tool_calls，按顺序执行每个工具。
%%
%% @param ToolHandlers 工具处理器映射 #{ToolName => Handler}
%% @returns 节点函数 fun(State) -> {ok, NewState}
-spec create(#{binary() => function()}) -> fun((map()) -> {ok, map()}).
create(ToolHandlers) ->
    fun(State) ->
        %% 步骤 1：从状态获取数据
        ToolCalls = graph:get(State, tool_calls, []),
        Messages = graph:get(State, messages, []),
        Context = graph:get(State, context, #{}),

        %% 步骤 2：执行所有工具调用
        {Results, CtxUpdates} = execute_tool_calls(ToolCalls, ToolHandlers, Context, State),

        %% 步骤 3：构建工具结果消息
        ToolMessages = beamai_utils:build_tool_messages(ToolCalls, Results),

        %% 步骤 4：更新状态（批量更新）
        NewMsgs = Messages ++ ToolMessages,
        NewCtx = maps:merge(Context, CtxUpdates),

        BaseUpdates = [
            {messages, NewMsgs},
            {tool_results, Results},
            {tool_calls, []},
            {context, NewCtx}
        ],

        %% 步骤 5：同步 full_messages（如果存在）
        AllUpdates = beamai_utils:append_list_to_full_messages(
            BaseUpdates, ToolMessages, State),
        NewState = ?SET_STATE_MANY(State, AllUpdates),

        {ok, NewState}
    end.

%%====================================================================
%% 工具辅助函数
%%====================================================================

%% @doc 从工具定义列表构建处理器映射
%%
%% 将工具定义列表转换为 #{Name => Handler} 映射。
%% 忽略没有 name 或 handler 字段的工具定义。
%%
%% @param Tools 工具定义列表
%% @returns 处理器映射
-spec build_handlers([map()]) -> #{binary() => function()}.
build_handlers(Tools) ->
    lists:foldl(fun(#{name := N, handler := H}, Acc) ->
        Acc#{N => H};
    (_, Acc) ->
        Acc
    end, #{}, Tools).

%%====================================================================
%% 私有函数 - 工具调用执行
%%====================================================================

%% @private 执行工具调用列表
%%
%% 按顺序执行每个工具调用，收集结果和上下文更新。
%%
%% @param ToolCalls 工具调用列表
%% @param Handlers 处理器映射
%% @param Context 当前上下文
%% @param State 图状态（用于回调）
%% @returns {Results, ContextUpdates}
-spec execute_tool_calls([map()], #{binary() => function()}, map(), map()) ->
    {[{ok, binary()} | {error, term()}], map()}.
execute_tool_calls(ToolCalls, Handlers, Context, State) ->
    lists:foldl(fun(TC, {Results, CtxAcc}) ->
        {Result, NewCtx} = execute_single_tool(TC, Handlers, CtxAcc, State),
        {Results ++ [Result], maps:merge(CtxAcc, NewCtx)}
    end, {[], Context}, ToolCalls).

%% @private 执行单个工具
%%
%% 流程：
%%   1. 提取工具名称和参数
%%   2. 触发 on_tool_start 回调
%%   3. 查找处理器并执行
%%   4. 触发结果回调
%%
%% @param TC 工具调用
%% @param Handlers 处理器映射
%% @param Context 当前上下文
%% @param State 图状态
%% @returns {Result, ContextUpdates}
-spec execute_single_tool(map(), #{binary() => function()}, map(), map()) ->
    {{ok, binary()} | {error, term()}, map()}.
execute_single_tool(TC, Handlers, Context, State) ->
    %% 使用公共工具提取函数
    {Name, Args} = beamai_utils:extract_tool_info(TC),

    %% 触发 on_tool_start 回调
    ?INVOKE_CALLBACK_FROM_STATE(on_tool_start, [Name, Args], State),

    %% 执行工具
    execute_with_handler(Name, Args, maps:get(Name, Handlers, undefined), Context, State).

%% @private 执行工具（已获取处理器）
-spec execute_with_handler(binary(), map(), function() | undefined, map(), map()) ->
    {{ok, binary()} | {error, term()}, map()}.
execute_with_handler(Name, _Args, undefined, _Context, State) ->
    %% 工具不存在
    Error = {unknown_tool, Name},
    ?INVOKE_CALLBACK_FROM_STATE(on_tool_error, [Name, Error], State),
    {{error, Error}, #{}};
execute_with_handler(Name, Args, Handler, Context, State) ->
    %% 安全执行工具
    {Result, CtxUpdates} = safe_execute_tool(Handler, Args, Context),
    invoke_tool_result_callback(Name, Result, State),
    {Result, CtxUpdates}.

%% @private 触发工具结果回调
-spec invoke_tool_result_callback(binary(), {ok, binary()} | {error, term()}, map()) -> ok.
invoke_tool_result_callback(Name, {ok, Result}, State) ->
    ?INVOKE_CALLBACK_FROM_STATE(on_tool_end, [Name, Result], State);
invoke_tool_result_callback(Name, {error, Reason}, State) ->
    ?INVOKE_CALLBACK_FROM_STATE(on_tool_error, [Name, Reason], State).

%%====================================================================
%% 私有函数 - 安全执行
%%====================================================================

%% @private 安全执行工具
%%
%% 捕获工具执行中的异常，返回标准化结果。
%%
%% @param Handler 工具处理器函数
%% @param Args 工具参数
%% @param Context 执行上下文
%% @returns {Result, ContextUpdates}
-spec safe_execute_tool(function(), map(), map()) ->
    {{ok, binary()} | {error, term()}, map()}.
safe_execute_tool(Handler, Args, Context) ->
    try
        Result = call_handler(Handler, Args, Context),
        process_tool_result(Result)
    catch
        Class:Reason:_Stack ->
            {{error, {Class, Reason}}, #{}}
    end.

%% @private 根据 arity 调用处理器
%%
%% 支持两种处理器签名：
%%   - Handler(Args) -> Result
%%   - Handler(Args, Context) -> Result
%%
%% @param Handler 处理器函数
%% @param Args 工具参数
%% @param Context 执行上下文
%% @returns 处理器返回值
-spec call_handler(function(), map(), map()) -> term().
call_handler(Handler, Args, Context) ->
    case erlang:fun_info(Handler, arity) of
        {arity, 1} -> Handler(Args);
        {arity, 2} -> Handler(Args, Context);
        _ -> Handler(Args)
    end.

%% @private 处理工具执行结果
%%
%% 标准化各种返回格式：
%%   - {Result, CtxUpdates} -> {{ok, Binary}, CtxUpdates}
%%   - {ok, Result, CtxUpdates} -> {{ok, Binary}, CtxUpdates}
%%   - {ok, Result} -> {{ok, Binary}, #{}}
%%   - {error, Reason} -> {{error, Reason}, #{}}
%%   - Result -> {{ok, Binary}, #{}}
%%
%% @param Result 工具返回值
%% @returns 标准化的 {Result, ContextUpdates}
-spec process_tool_result(term()) -> {{ok, binary()} | {error, term()}, map()}.
process_tool_result({Result, CtxUpdates}) when is_map(CtxUpdates) ->
    {{ok, beamai_utils:to_binary(Result)}, CtxUpdates};
process_tool_result({ok, Result, CtxUpdates}) when is_map(CtxUpdates) ->
    {{ok, beamai_utils:to_binary(Result)}, CtxUpdates};
process_tool_result({ok, Result}) ->
    {{ok, beamai_utils:to_binary(Result)}, #{}};
process_tool_result({error, Reason}) ->
    {{error, Reason}, #{}};
process_tool_result(Result) ->
    {{ok, beamai_utils:to_binary(Result)}, #{}}.
