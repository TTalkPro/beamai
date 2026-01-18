%%%-------------------------------------------------------------------
%%% @doc Agent Simple 统一工具模块
%%%
%%% 合并原有模块，提供：
%%%   - 消息构建（原 beamai_message_utils）
%%%   - LLM 辅助（原 agent_llm_utils）
%%%   - 工具处理（原 beamai_tool_utils）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_utils).

-include_lib("beamai_core/include/beamai_common.hrl").

%%====================================================================
%% 消息构建 API（原 beamai_message_utils）
%%====================================================================
-export([
    build_assistant_message/2,     %% 构建 assistant 消息
    build_tool_messages/2          %% 构建工具结果消息列表
]).

%% full_messages 同步 API
-export([
    append_to_full_messages/3,     %% 追加单个消息到 full_messages
    append_list_to_full_messages/3 %% 追加消息列表到 full_messages
]).

%%====================================================================
%% LLM 辅助 API（原 agent_llm_utils）
%%====================================================================
-export([
    build_llm_opts/3,              %% 构建 LLM 调用选项
    wrap_retry_callback/2          %% 包装重试回调
]).

%% 回调触发 API
-export([
    invoke_text_callback/2,        %% 触发文本回调
    invoke_action_callback/4       %% 触发动作回调
]).

%%====================================================================
%% 工具处理 API（原 beamai_tool_utils）
%%====================================================================
%% 工具信息提取
-export([
    extract_tool_name/1,           %% 提取工具名称
    extract_tool_id/1,             %% 提取工具 ID
    extract_tool_info/1,           %% 提取完整信息 {Name, Args}
    extract_tool_args/1            %% 提取工具参数（已解析）
]).

%% 参数解析
-export([
    parse_args/1,                  %% 解析参数（向后兼容）
    parse_args_safe/1              %% 安全解析（带日志）
]).

%% 格式化
-export([
    format_tool_calls/1,           %% 格式化工具调用列表
    format_tool_result/1           %% 格式化工具结果
]).

%%====================================================================
%% 消息构建实现
%%====================================================================

%% @doc 构建 assistant 消息
%%
%% 根据是否有工具调用构建不同格式的消息：
%%   - 无工具调用：#{role => assistant, content => Content}
%%   - 有工具调用：#{role => assistant, content => Content, tool_calls => ToolCalls}
-spec build_assistant_message(binary() | null, [map()]) -> map().
build_assistant_message(Content, []) ->
    #{role => assistant, content => Content};
build_assistant_message(Content, ToolCalls) ->
    FormattedCalls = format_tool_calls(ToolCalls),
    #{role => assistant, content => Content, tool_calls => FormattedCalls}.

%% @doc 构建工具结果消息列表
%%
%% 将工具调用和结果配对，构建 tool 角色消息。
-spec build_tool_messages([map()], [term()]) -> [map()].
build_tool_messages(ToolCalls, Results) ->
    lists:zipwith(fun(TC, Result) ->
        Id = extract_tool_id(TC),
        Content = format_tool_result(Result),
        #{role => tool, tool_call_id => Id, content => Content}
    end, ToolCalls, Results).

%% @doc 追加单个消息到 full_messages（如果存在）
-spec append_to_full_messages(list(), map(), map()) -> list().
append_to_full_messages(Updates, NewMsg, State) ->
    case graph:get(State, full_messages, undefined) of
        undefined -> Updates;
        FullMessages ->
            NewFullMsgs = FullMessages ++ [NewMsg],
            Updates ++ [{full_messages, NewFullMsgs}]
    end.

%% @doc 追加消息列表到 full_messages（如果存在）
-spec append_list_to_full_messages(list(), [map()], map()) -> list().
append_list_to_full_messages(Updates, NewMsgs, State) ->
    case graph:get(State, full_messages, undefined) of
        undefined -> Updates;
        FullMessages ->
            NewFullMsgs = FullMessages ++ NewMsgs,
            Updates ++ [{full_messages, NewFullMsgs}]
    end.

%%====================================================================
%% LLM 辅助实现
%%====================================================================

%% @doc 构建 LLM 调用选项
%%
%% 从状态中提取回调配置，构建 LLM 客户端调用选项。
-spec build_llm_opts([map()], map(), map()) -> map().
build_llm_opts(Tools, LLMConfig, State) ->
    Callbacks = graph:get(State, callbacks, #{}),
    Meta = graph:get(State, callback_meta, #{}),
    OnRetry = wrap_retry_callback(maps:get(on_retry, Callbacks, undefined), Meta),

    BaseOpts = #{
        tools => Tools,
        on_retry => OnRetry
    },

    case maps:get(timeout, LLMConfig, undefined) of
        undefined -> BaseOpts;
        Timeout -> BaseOpts#{timeout => Timeout}
    end.

%% @doc 包装重试回调，自动添加元数据参数
-spec wrap_retry_callback(function() | undefined, map()) -> function() | undefined.
wrap_retry_callback(undefined, _Meta) ->
    undefined;
wrap_retry_callback(Handler, Meta) when is_function(Handler) ->
    fun(RetryState) -> Handler(RetryState, Meta) end.

%% @doc 触发文本回调
-spec invoke_text_callback(binary() | null, map()) -> ok.
invoke_text_callback(null, _State) -> ok;
invoke_text_callback(<<>>, _State) -> ok;
invoke_text_callback(Text, State) when is_binary(Text) ->
    ?INVOKE_CALLBACK_FROM_STATE(on_text, [Text], State);
invoke_text_callback(_, _) -> ok.

%% @doc 触发 Agent 动作/完成回调
-spec invoke_action_callback([map()], binary() | null, binary(), map()) -> ok.
invoke_action_callback([], Content, FinishReason, State) ->
    FinishOutput = #{
        type => final_response,
        content => Content,
        finish_reason => FinishReason
    },
    ?INVOKE_CALLBACK_FROM_STATE(on_agent_finish, [FinishOutput], State);
invoke_action_callback(ToolCalls, Content, _FinishReason, State) ->
    Action = #{
        type => tool_calls,
        tool_calls => ToolCalls,
        content => Content
    },
    ?INVOKE_CALLBACK_FROM_STATE(on_agent_action, [Action], State).

%%====================================================================
%% 工具信息提取实现
%%====================================================================

%% @doc 提取工具名称
-spec extract_tool_name(map()) -> binary().
extract_tool_name(#{function := #{name := Name}}) -> Name;
extract_tool_name(#{name := Name}) -> Name;
extract_tool_name(_) -> <<"unknown">>.

%% @doc 提取工具调用 ID
-spec extract_tool_id(map()) -> binary().
extract_tool_id(#{id := Id}) -> Id;
extract_tool_id(_) -> <<"unknown">>.

%% @doc 提取完整工具信息
-spec extract_tool_info(map()) -> {binary(), map()}.
extract_tool_info(#{function := #{name := Name, arguments := ArgsJson}}) ->
    {Name, parse_args_safe(ArgsJson)};
extract_tool_info(#{name := Name, arguments := ArgsJson}) ->
    {Name, parse_args_safe(ArgsJson)}.

%% @doc 提取工具参数（已解析）
-spec extract_tool_args(map()) -> map().
extract_tool_args(#{function := #{arguments := Args}}) when is_binary(Args) ->
    parse_args_safe(Args);
extract_tool_args(#{function := #{arguments := Args}}) when is_map(Args) ->
    Args;
extract_tool_args(#{arguments := Args}) when is_binary(Args) ->
    parse_args_safe(Args);
extract_tool_args(#{arguments := Args}) when is_map(Args) ->
    Args;
extract_tool_args(_) ->
    #{}.

%%====================================================================
%% 参数解析实现
%%====================================================================

%% @doc 解析工具参数（向后兼容，静默处理错误）
-spec parse_args(binary() | map()) -> map().
parse_args(Args) when is_map(Args) -> Args;
parse_args(ArgsJson) when is_binary(ArgsJson) ->
    try jsx:decode(ArgsJson, [return_maps])
    catch _:_ -> #{}
    end;
parse_args(_) -> #{}.

%% @doc 安全解析工具参数（带日志）
-spec parse_args_safe(binary() | map()) -> map().
parse_args_safe(Args) when is_map(Args) -> Args;
parse_args_safe(ArgsJson) when is_binary(ArgsJson) ->
    try jsx:decode(ArgsJson, [return_maps])
    catch
        Class:Reason ->
            logger:warning("工具参数解析失败: ~p:~p, 输入: ~s",
                          [Class, Reason, truncate_for_log(ArgsJson)]),
            #{}
    end;
parse_args_safe(Other) ->
    logger:warning("无效的工具参数类型: ~p", [Other]),
    #{}.

%%====================================================================
%% 格式化实现
%%====================================================================

%% @doc 格式化工具调用列表为消息格式
-spec format_tool_calls([map()]) -> [map()].
format_tool_calls(ToolCalls) ->
    [format_single_tool_call(TC) || TC <- ToolCalls].

%% @doc 格式化工具执行结果
-spec format_tool_result({ok, binary()} | {error, term()}) -> binary().
format_tool_result({ok, Content}) -> Content;
format_tool_result({error, Reason}) ->
    iolist_to_binary(io_lib:format("Error: ~p", [Reason])).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 格式化单个工具调用
-spec format_single_tool_call(map()) -> map().
format_single_tool_call(#{id := Id, name := Name, arguments := Args}) ->
    #{id => Id, type => function, function => #{name => Name, arguments => Args}};
format_single_tool_call(#{id := Id, function := #{name := Name, arguments := Args}}) ->
    #{id => Id, type => function, function => #{name => Name, arguments => Args}};
format_single_tool_call(TC) -> TC.

%% @private 截断过长的日志内容
-spec truncate_for_log(binary()) -> binary().
truncate_for_log(Bin) when byte_size(Bin) > 200 ->
    <<(binary:part(Bin, 0, 200))/binary, "...">>;
truncate_for_log(Bin) -> Bin.
