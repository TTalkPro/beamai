%%%-------------------------------------------------------------------
%%% @doc Deep Agent 工具执行模块
%%%
%%% 负责工具的执行逻辑，包括：
%%%   - 执行待处理的工具调用
%%%   - 查找工具处理器
%%%   - 安全执行并捕获异常
%%%   - 收集和格式化结果
%%%
%%% 从 beamai_deepagent_nodes.erl 拆分而来，专注于工具执行逻辑。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_tool_executor).

%% 导入工具模块
-import(beamai_deepagent_utils, [
    state_get/2, state_get/3, state_set/3,
    add_trace/3
]).

%% 导入消息处理模块
-import(beamai_deepagent_messages, [
    append_tool_results/2
]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 节点构造器
-export([create/1]).

%% 工具执行函数（供其他模块使用）
-export([execute_single_tool/3]).

%%====================================================================
%% 类型定义
%%====================================================================

-type node_fun() :: fun((graph_state:state()) -> {ok, graph_state:state()}).

%%====================================================================
%% 节点构造器
%%====================================================================

%% @doc 创建工具执行节点
%%
%% 功能：执行待处理的工具调用列表
%% 流程：获取待处理工具 -> 逐个执行 -> 收集结果 -> 更新消息
-spec create(map()) -> node_fun().
create(Config) ->
    fun(State) -> execute_tool_node(Config, State) end.

%%====================================================================
%% 工具执行 API
%%====================================================================

%% @doc 执行单个工具
-spec execute_single_tool(map(), map(), graph_state:state()) -> {map(), graph_state:state()}.
execute_single_tool(ToolCall, Config, State) ->
    ToolName = extract_tool_name(ToolCall),
    ToolId = extract_tool_id(ToolCall),
    Arguments = parse_tool_arguments(ToolCall),

    %% 检查工具是否需要审批
    case check_and_approve(ToolName, Arguments, Config) of
        approved ->
            do_execute_tool(ToolName, ToolId, Arguments, Config, State);
        {auto_approved, _Reason} ->
            do_execute_tool(ToolName, ToolId, Arguments, Config, State);
        {denied, Reason} ->
            {make_denied_result(ToolId, ToolName, Reason), State};
        {pending, RequestId} ->
            %% 异步模式：返回暂停状态
            {make_pending_result(ToolId, ToolName, RequestId), State}
    end.

%% @private 实际执行工具
-spec do_execute_tool(binary(), binary(), map(), map(), graph_state:state()) ->
    {map(), graph_state:state()}.
do_execute_tool(ToolName, ToolId, Arguments, Config, State) ->
    Tools = beamai_deepagent_llm_node:build_tool_specs(Config, State),
    case find_tool_handler(ToolName, Tools) of
        {ok, Handler} -> execute_handler(Handler, ToolId, ToolName, Arguments, State);
        not_found -> {make_error_result(ToolId, ToolName, tool_not_found), State}
    end.

%% @private 检查并请求审批
-spec check_and_approve(binary(), map(), map()) ->
    approved | {auto_approved, binary()} | {denied, binary()} | {pending, binary()}.
check_and_approve(ToolName, Args, Config) ->
    case beamai_deepagent_approval:check_approval(ToolName, Args, Config) of
        approved ->
            approved;
        {auto_approved, Reason} ->
            {auto_approved, Reason};
        {needs_approval, Message} ->
            request_tool_approval(ToolName, Args, Message, Config)
    end.

%% @private 请求工具执行审批
-spec request_tool_approval(binary(), map(), binary(), map()) ->
    approved | {denied, binary()} | {pending, binary()}.
request_tool_approval(ToolName, Args, Message, Config) ->
    HumanConfig = maps:get(human_in_loop, Config, #{}),
    Request = #{
        type => approval,
        tool => ToolName,
        args => Args,
        message => Message
    },
    case beamai_deepagent_interaction:request_human_input(Request, HumanConfig) of
        {ok, #{approved := true}} -> approved;
        {ok, #{approved := false}} -> {denied, <<"用户拒绝了此操作"/utf8>>};
        {pending, RequestId} -> {pending, RequestId};
        {error, Reason} -> {denied, format_approval_error(Reason)}
    end.

%% @private 格式化审批错误
-spec format_approval_error(term()) -> binary().
format_approval_error(timeout) ->
    <<"审批请求超时"/utf8>>;
format_approval_error(Reason) when is_binary(Reason) ->
    Reason;
format_approval_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%%====================================================================
%% 私有函数 - 工具节点执行
%%====================================================================

%% @private 工具节点执行逻辑
-spec execute_tool_node(map(), graph_state:state()) -> {ok, graph_state:state()}.
execute_tool_node(Config, State) ->
    PendingTools = state_get(State, pending_tools, []),
    execute_pending_tools(PendingTools, Config, State).

%% @private 执行待处理工具
-spec execute_pending_tools([map()], map(), graph_state:state()) -> {ok, graph_state:state()}.
execute_pending_tools([], _Config, State) ->
    {ok, State};
execute_pending_tools(ToolCalls, Config, State) ->
    {Results, NewState} = execute_all_tools(ToolCalls, Config, State),
    {ok, finalize_tool_execution(NewState, Results)}.

%% @private 执行所有工具并收集结果
-spec execute_all_tools([map()], map(), graph_state:state()) -> {[map()], graph_state:state()}.
execute_all_tools(ToolCalls, Config, State) ->
    Folder = fun(ToolCall, {Acc, S}) ->
        {Result, NewS} = execute_single_tool(ToolCall, Config, S),
        {[Result | Acc], NewS}
    end,
    lists:foldl(Folder, {[], State}, ToolCalls).

%% @private 完成工具执行后的状态更新
-spec finalize_tool_execution(graph_state:state(), [map()]) -> graph_state:state().
finalize_tool_execution(State, Results) ->
    State1 = state_set(State, tool_results, Results),
    State2 = append_tool_results(State1, Results),
    State3 = state_set(State2, pending_tools, []),
    add_trace(State3, tools_executed, Results).

%%====================================================================
%% 私有函数 - 工具信息提取
%%====================================================================

%% @private 提取工具名称（支持多种格式）
-spec extract_tool_name(map()) -> binary().
extract_tool_name(ToolCall) ->
    maps:get(name, ToolCall, maps:get(<<"name">>, ToolCall, <<>>)).

%% @private 提取工具 ID
-spec extract_tool_id(map()) -> binary().
extract_tool_id(ToolCall) ->
    maps:get(id, ToolCall, maps:get(<<"id">>, ToolCall, <<>>)).

%% @private 解析工具参数
-spec parse_tool_arguments(map()) -> map().
parse_tool_arguments(ToolCall) ->
    Args = maps:get(arguments, ToolCall, maps:get(<<"arguments">>, ToolCall, <<"{}">>)),
    beamai_deepagent_utils:parse_json(Args).

%%====================================================================
%% 私有函数 - 处理器执行
%%====================================================================

%% @private 执行处理器并捕获异常
-spec execute_handler(function(), binary(), binary(), map(), graph_state:state()) ->
    {map(), graph_state:state()}.
execute_handler(Handler, ToolId, ToolName, Args, State) ->
    try
        Result = Handler(Args, State),
        {make_success_result(ToolId, ToolName, Result), State}
    catch
        Class:Reason:Stack ->
            {make_error_result(ToolId, ToolName, {Class, Reason, Stack}), State}
    end.

%% @private 查找工具处理器
%%
%% 委托给 beamai_deepagent_tool_registry 查找处理器
-spec find_tool_handler(binary(), [map()]) -> {ok, function()} | not_found.
find_tool_handler(ToolName, Tools) ->
    beamai_deepagent_tool_registry:find_handler(ToolName, Tools).

%%====================================================================
%% 私有函数 - 结果构建
%%====================================================================

%% @private 创建成功结果
-spec make_success_result(binary(), binary(), term()) -> map().
make_success_result(ToolId, ToolName, Result) ->
    #{tool_call_id => ToolId, name => ToolName, success => true, result => Result}.

%% @private 创建错误结果
-spec make_error_result(binary(), binary(), term()) -> map().
make_error_result(ToolId, ToolName, Error) ->
    #{tool_call_id => ToolId, name => ToolName, success => false, error => Error}.

%% @private 创建拒绝结果
-spec make_denied_result(binary(), binary(), binary()) -> map().
make_denied_result(ToolId, ToolName, Reason) ->
    #{tool_call_id => ToolId, name => ToolName, success => false,
      denied => true, reason => Reason}.

%% @private 创建待审批结果
-spec make_pending_result(binary(), binary(), binary()) -> map().
make_pending_result(ToolId, ToolName, RequestId) ->
    #{tool_call_id => ToolId, name => ToolName,
      pending => true, request_id => RequestId}.
