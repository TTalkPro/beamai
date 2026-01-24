%%%-------------------------------------------------------------------
%%% @doc Agent 共享工具函数
%%%
%%% 提供 beamai_agent 和 beamai_process_agent 共用的基础工具函数：
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
    parse_tool_results_messages/1
]).

%%====================================================================
%% API
%%====================================================================

%% @doc 从 LLM 响应中提取文本内容
%%
%% 处理三种情况：
%%   - content 为 binary: 直接返回
%%   - content 为 null: 返回空二进制（tool_calls 响应时常见）
%%   - 其他情况: 返回空二进制
-spec extract_content(map()) -> binary().
extract_content(#{content := Content}) when is_binary(Content) -> Content;
extract_content(#{content := null}) -> <<>>;
extract_content(#{}) -> <<>>.

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

%% @doc 执行 tool calls 并收集结果
%%
%% 遍历 LLM 返回的所有 tool_call，逐个：
%%   1. 解析 tool_call 获取 ID、函数名、参数
%%   2. 调用 kernel:invoke_tool 执行函数
%%   3. 将执行结果编码为 JSON 字符串
%%   4. 构建 tool 角色消息和调用记录
%%
%% @param Kernel kernel 实例
%% @param ToolCalls LLM 返回的 tool_call 列表
%% @returns {ToolResultMsgs, CallRecords}
-spec execute_tools(beamai_kernel:kernel(), [map()]) -> {[map()], [map()]}.
execute_tools(Kernel, ToolCalls) ->
    lists:foldl(fun(TC, {ResultsAcc, CallsAcc}) ->
        {Id, Name, Args} = beamai_function:parse_tool_call(TC),
        Result = case beamai_kernel:invoke_tool(Kernel, Name, Args, beamai_context:new()) of
            {ok, Value, _Ctx} -> beamai_function:encode_result(Value);
            {error, Reason} -> beamai_function:encode_result(#{error => Reason})
        end,
        Msg = #{role => tool, tool_call_id => Id, content => Result},
        CallRecord = #{name => Name, args => Args, result => Result, tool_call_id => Id},
        {ResultsAcc ++ [Msg], CallsAcc ++ [CallRecord]}
    end, {[], []}, ToolCalls).

%% @doc 将 tool 执行结果列表转换为消息格式
%%
%% 用于 Process-native agent 将 tool_step 返回的结果
%% 转换为可追加到 messages 列表的 tool 角色消息。
%% 支持 result 键和 content 键两种格式，未匹配的直接透传。
-spec parse_tool_results_messages([map()]) -> [map()].
parse_tool_results_messages(Results) ->
    lists:map(fun(#{tool_call_id := Id, result := R}) ->
        #{role => tool, tool_call_id => Id, content => ensure_binary(R)};
    (#{tool_call_id := Id, content := C}) ->
        #{role => tool, tool_call_id => Id, content => ensure_binary(C)};
    (Other) ->
        Other
    end, Results).

%%====================================================================
%% Internal
%%====================================================================

ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) -> beamai_function:encode_result(V).
