%%%-------------------------------------------------------------------
%%% @doc Deep Agent LLM 调用节点模块
%%%
%%% 负责与 LLM 的交互逻辑，包括：
%%%   - 构建 LLM 请求消息
%%%   - 调用 LLM API
%%%   - 处理 LLM 响应
%%%   - 提取工具调用
%%%
%%% 从 beamai_deepagent_nodes.erl 拆分而来，专注于 LLM 调用逻辑。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_llm_node).

%% 导入工具模块
-import(beamai_deepagent_utils, [
    state_get/2, state_get/3, state_set/3,
    add_trace/3, prepend_system_prompt/2
]).

%% 导入消息处理模块
-import(beamai_deepagent_messages, [
    append_assistant_message/2,
    append_trace_context/2,
    extract_tool_calls/2
]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 节点构造器
-export([create/1]).

%% 工具规格构建（供其他模块使用）
-export([build_tool_specs/2]).

%% 辅助函数（供测试使用）
-export([inject_trace_context/2]).

%%====================================================================
%% 类型定义
%%====================================================================

-type node_fun() :: fun((graph_state:state()) ->
    {ok, graph_state:state()} | {error, term()}).

%%====================================================================
%% 节点构造器
%%====================================================================

%% @doc 创建 LLM 调用节点
%%
%% 功能：调用大语言模型并处理响应
%% 流程：构建消息 -> 调用 LLM -> 提取工具调用 -> 更新状态
-spec create(map()) -> node_fun().
create(Config) ->
    fun(State) -> execute_llm_node(Config, State) end.

%%====================================================================
%% 工具规格构建
%%====================================================================

%% @doc 构建工具规格列表
%%
%% 根据配置和当前状态动态组合可用工具。
%% 使用 beamai_tool_registry 和 beamai_deepagent_tool_provider 获取内置工具，
%% 然后添加用户自定义工具。
-spec build_tool_specs(map(), graph_state:state()) -> [map()].
build_tool_specs(Config, State) ->
    Depth = state_get(State, depth, 0),
    FullConfig = Config#{depth => Depth},
    %% 获取内置工具
    BuiltinTools = beamai_tool_registry:from_config(#{
        providers => [{beamai_deepagent_tool_provider, FullConfig}]
    }),
    %% 添加用户自定义工具
    CustomTools = maps:get(tools, Config, []),
    BuiltinTools ++ CustomTools.

%% @doc 注入轨迹上下文到消息
%%
%% 将最近执行轨迹作为系统消息添加，帮助 LLM 理解上下文。
-spec inject_trace_context([map()], beamai_deepagent_trace:t()) -> [map()].
inject_trace_context(Messages, Trace) ->
    RecentTrace = beamai_deepagent_trace:get_recent(Trace, 5),
    append_trace_context(Messages, RecentTrace).

%%====================================================================
%% 私有函数 - LLM 节点执行
%%====================================================================

%% @private LLM 节点执行逻辑
-spec execute_llm_node(map(), graph_state:state()) ->
    {ok, graph_state:state()} | {error, term()}.
execute_llm_node(Config, State) ->
    %% 准备消息（可能包含上下文压缩）
    {Messages, State1} = prepare_messages(State, Config),
    Tools = build_tool_specs(Config, State1),
    LLMConfig = build_llm_config(Config),

    case call_llm(LLMConfig, Messages, Tools) of
        {ok, Response} -> {ok, process_llm_response(State1, Response)};
        {error, Reason} -> {error, {llm_error, Reason}}
    end.

%% @private 准备发送给 LLM 的消息（带上下文压缩）
-spec prepare_messages(graph_state:state(), map()) -> {[map()], graph_state:state()}.
prepare_messages(State, Config) ->
    Messages = state_get(State, messages, []),

    %% 使用 buffer 构建上下文（如果配置了）
    {ContextMsgs, NewState} = build_context_with_buffer(Config, Messages, State),

    SystemPrompt = state_get(NewState, system_prompt, beamai_deepagent_utils:default_system_prompt()),
    Trace = state_get(NewState, trace, beamai_deepagent_trace:new()),

    %% 注入轨迹上下文并添加系统提示
    AugmentedMsgs = inject_trace_context(ContextMsgs, Trace),
    {prepend_system_prompt(SystemPrompt, AugmentedMsgs), NewState}.

%% @private 使用 buffer 构建 LLM 上下文
-spec build_context_with_buffer(map(), [map()], graph_state:state()) ->
    {[map()], graph_state:state()}.
build_context_with_buffer(Config, Messages, State) ->
    case maps:get(buffer, Config, undefined) of
        undefined ->
            %% 未配置 buffer，使用全部消息
            {Messages, State};
        BufferOpts when is_map(BufferOpts) ->
            BufferConfig = beamai_conversation_buffer:new(BufferOpts),
            case beamai_conversation_buffer:build_context(BufferConfig, Messages) of
                {ok, Context} ->
                    ContextMsgs = maps:get(messages, Context, []),
                    %% 如果有摘要，作为 system 消息前缀
                    FinalMsgs = case maps:get(summary, Context, undefined) of
                        undefined -> ContextMsgs;
                        <<>> -> ContextMsgs;
                        Summary ->
                            SummaryMsg = #{role => <<"system">>, content => <<"[对话历史摘要]\n", Summary/binary>>},
                            [SummaryMsg | ContextMsgs]
                    end,
                    {FinalMsgs, State};
                {error, _Reason} ->
                    %% 构建失败，使用全部消息
                    {Messages, State}
            end
    end.

%% @private 构建 LLM 配置
-spec build_llm_config(map()) -> map().
build_llm_config(Config) ->
    LLMOpts = maps:get(llm, Config, #{}),
    %% 如果已经是有效的 llm_client 配置，直接使用
    case llm_client:is_valid_config(LLMOpts) of
        true -> LLMOpts;
        false ->
            Provider = maps:get(provider, LLMOpts, openai),
            llm_client:create(Provider, LLMOpts)
    end.

%% @private 调用 LLM
-spec call_llm(map(), [map()], [map()]) -> {ok, map()} | {error, term()}.
call_llm(LLMConfig, Messages, Tools) ->
    FormattedTools = format_tools_for_llm(Tools),
    llm_client:with_tools(LLMConfig, Messages, FormattedTools).

%% @private 处理 LLM 响应
-spec process_llm_response(graph_state:state(), map()) -> graph_state:state().
process_llm_response(State, Response) ->
    State1 = state_set(State, last_response, Response),
    State2 = append_assistant_message(State1, Response),
    State3 = add_trace(State2, llm_response, Response),
    extract_tool_calls(State3, Response).

%%====================================================================
%% 私有函数 - 工具格式化
%%====================================================================

%% @private 格式化工具为 LLM API 格式
-spec format_tools_for_llm([map()]) -> [map()].
format_tools_for_llm(Tools) ->
    [format_single_tool(T) || T <- Tools].

%% @private 格式化单个工具
-spec format_single_tool(map()) -> map().
format_single_tool(#{name := Name, description := Desc, parameters := Params}) ->
    #{type => function, function => #{name => Name, description => Desc, parameters => Params}};
format_single_tool(Tool) ->
    Tool.

