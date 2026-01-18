%%%-------------------------------------------------------------------
%%% @doc Graph Deep Agent 消息处理模块
%%%
%%% 负责处理 LLM 交互中的消息：
%%% - 消息构建：创建各类型消息（助手、工具、反思）
%%% - 消息提取：从响应中提取工具调用和内容
%%% - 消息格式化：将结果转换为消息格式
%%%
%%% 设计原则：
%%% - 单一职责：只处理消息相关逻辑
%%% - 纯函数：无副作用
%%% - 高复用：供节点模块和其他模块使用
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_messages).

%% 导入工具模块
-import(beamai_deepagent_utils, [
    state_get/2, state_get/3, state_set/3
]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 消息追加
-export([
    append_assistant_message/2,
    append_tool_results/2,
    append_reflection_message/2,
    append_trace_context/2
]).

%% 内容提取
-export([
    extract_tool_calls/2
]).

%%====================================================================
%% 消息追加 API
%%====================================================================

%% @doc 追加助手消息到状态
%%
%% 从 LLM 响应中构建助手消息并追加到消息历史。
%% 包含内容和工具调用两部分。
-spec append_assistant_message(graph_state:state(), map()) -> graph_state:state().
append_assistant_message(State, Response) ->
    Messages = state_get(State, messages, []),
    Content = maps:get(content, Response, <<>>),
    ToolCalls = maps:get(tool_calls, Response, []),
    AssistantMsg = #{role => assistant, content => Content, tool_calls => ToolCalls},
    state_set(State, messages, Messages ++ [AssistantMsg]).

%% @doc 追加工具结果到消息历史
%%
%% 将工具执行结果列表转换为工具消息并追加。
-spec append_tool_results(graph_state:state(), [map()]) -> graph_state:state().
append_tool_results(State, Results) ->
    Messages = state_get(State, messages, []),
    ToolMsgs = [format_tool_result_message(R) || R <- Results],
    state_set(State, messages, Messages ++ ToolMsgs).

%% @doc 追加反思消息
%%
%% 将反思内容作为系统消息追加，用于调整 LLM 后续行为。
-spec append_reflection_message(graph_state:state(), binary()) -> graph_state:state().
append_reflection_message(State, Reflection) ->
    Messages = state_get(State, messages, []),
    ReflectionMsg = #{role => system, content => Reflection},
    state_set(State, messages, Messages ++ [ReflectionMsg]).

%% @doc 追加轨迹上下文到消息
%%
%% 如果有执行轨迹，将其格式化为系统消息追加。
%% 帮助 LLM 理解最近的执行历史。
-spec append_trace_context([map()], [map()]) -> [map()].
append_trace_context(Messages, []) ->
    Messages;
append_trace_context(Messages, RecentTrace) ->
    TraceText = beamai_deepagent_trace:format(RecentTrace),
    TraceMsg = #{role => system, content => <<"Recent execution trace:\n", TraceText/binary>>},
    Messages ++ [TraceMsg].

%%====================================================================
%% 内容提取 API
%%====================================================================

%% @doc 从响应中提取工具调用并更新状态
%%
%% 设置 pending_tools 字段，并根据是否有内容设置 final_response。
-spec extract_tool_calls(graph_state:state(), map()) -> graph_state:state().
extract_tool_calls(State, Response) ->
    ToolCalls = maps:get(tool_calls, Response, []),
    Content = maps:get(content, Response, <<>>),
    State1 = state_set(State, pending_tools, ToolCalls),
    maybe_set_final_response(State1, ToolCalls, Content).

%%====================================================================
%% 私有函数 - 消息格式化
%%====================================================================

%% @private 格式化单个工具结果为消息
%%
%% 转换工具执行结果为 LLM 可识别的工具消息格式。
-spec format_tool_result_message(map()) -> map().
format_tool_result_message(#{tool_call_id := Id} = Result) ->
    #{role => tool, tool_call_id => Id, content => format_tool_content(Result)}.

%% @private 格式化工具结果内容
%%
%% 根据成功/失败状态生成不同的内容格式：
%% - 成功：JSON 编码结果
%% - 失败：错误信息前缀
-spec format_tool_content(map()) -> binary().
format_tool_content(#{success := true, result := Result}) ->
    jsx:encode(Result);
format_tool_content(#{success := false, error := Error}) ->
    ErrorBin = iolist_to_binary(io_lib:format("~p", [Error])),
    <<"Error: ", ErrorBin/binary>>.

%%====================================================================
%% 私有函数
%%====================================================================

%% @private 条件设置最终响应
%%
%% 仅当没有工具调用且有内容时设置 final_response。
-spec maybe_set_final_response(graph_state:state(), [map()], binary()) -> graph_state:state().
maybe_set_final_response(State, [], Content) when Content =/= <<>> ->
    state_set(State, final_response, Content);
maybe_set_final_response(State, _ToolCalls, _Content) ->
    State.
