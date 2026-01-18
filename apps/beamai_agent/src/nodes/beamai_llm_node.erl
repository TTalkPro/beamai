%%%-------------------------------------------------------------------
%%% @doc LLM 调用节点模块
%%%
%%% 负责与 LLM 的交互，包括：
%%%   - 构建 LLM 请求
%%%   - 处理 LLM 响应
%%%   - 触发相关回调
%%%   - 构建 assistant 消息
%%%
%%% 从 beamai_nodes.erl 拆分而来，专注于 LLM 调用逻辑。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_node).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 节点创建 API
-export([create/1, create/2]).

%% 内部函数（供 beamai_nodes 委托调用）
-export([build_llm_opts/3]).
-export([process_llm_response/4]).
-export([build_assistant_message/2]).

%%====================================================================
%% 节点创建 API
%%====================================================================

%% @doc 创建 LLM 调用节点（使用默认配置键）
%%
%% 创建一个执行 LLM 调用的图节点函数。
%% 使用默认的状态键：tools, messages, system_prompt。
%%
%% @param LLMConfig LLM 配置（provider、model 等）
%% @returns 节点函数 fun(State) -> {ok, NewState} | {error, Reason}
-spec create(map()) -> fun((map()) -> {ok, map()} | {error, term()}).
create(LLMConfig) ->
    create(LLMConfig, #{}).

%% @doc 创建 LLM 调用节点（带选项）
%%
%% 选项说明：
%%   - tools_key: 工具列表在状态中的键 (默认 tools)
%%   - messages_key: 消息列表在状态中的键 (默认 messages)
%%   - system_prompt_key: 系统提示词在状态中的键 (默认 system_prompt)
%%
%% @param LLMConfig LLM 配置
%% @param Opts 节点选项
%% @returns 节点函数
-spec create(map(), map()) -> fun((map()) -> {ok, map()} | {error, term()}).
create(LLMConfig, Opts) ->
    %% 提取配置键
    ToolsKey = maps:get(tools_key, Opts, tools),
    MsgsKey = maps:get(messages_key, Opts, messages),
    PromptKey = maps:get(system_prompt_key, Opts, system_prompt),

    fun(State) ->
        %% 步骤 1：从状态获取数据
        Messages = graph:get(State, MsgsKey, []),
        SystemPrompt = graph:get(State, PromptKey, <<>>),
        Tools = graph:get(State, ToolsKey, []),

        %% 步骤 2：构建完整消息（带系统提示）
        AllMsgs = beamai_message:prepend_system(SystemPrompt, Messages),

        %% 步骤 3：触发 on_llm_start 回调
        ?INVOKE_CALLBACK_FROM_STATE(on_llm_start, [AllMsgs], State),

        %% 步骤 4：构建 LLM 调用选项
        LLMOpts = build_llm_opts(Tools, LLMConfig, State),

        %% 步骤 5：调用 LLM 并处理结果
        execute_llm_call(LLMConfig, AllMsgs, LLMOpts, Messages, State, MsgsKey)
    end.

%%====================================================================
%% 内部函数（也供外部模块使用）
%%====================================================================

%% @doc 构建 LLM 调用选项
%%
%% 委托给 agent_llm_utils 模块。
%%
%% @param Tools 工具定义列表
%% @param LLMConfig LLM 配置
%% @param State 图状态
%% @returns LLM 调用选项映射
-spec build_llm_opts([map()], map(), map()) -> map().
build_llm_opts(Tools, LLMConfig, State) ->
    beamai_agent_utils:build_llm_opts(Tools, LLMConfig, State).

%% @doc 处理 LLM 响应
%%
%% 解析响应，触发回调，更新状态。
%% 同时维护 messages 和 full_messages 的同步。
%%
%% @param Response LLM 响应
%% @param Messages 原始消息列表
%% @param State 图状态
%% @param MsgsKey 消息在状态中的键
%% @returns {ok, NewState}
-spec process_llm_response(map(), [map()], map(), atom()) -> {ok, map()}.
process_llm_response(Response, Messages, State, MsgsKey) ->
    %% 步骤 1：提取响应内容
    Content = maps:get(content, Response, null),
    ToolCalls = maps:get(tool_calls, Response, []),
    FinishReason = maps:get(finish_reason, Response, <<"stop">>),

    %% 步骤 2：触发文本回调（如果有内容）
    beamai_agent_utils:invoke_text_callback(Content, State),

    %% 步骤 3：触发动作/完成回调
    beamai_agent_utils:invoke_action_callback(ToolCalls, Content, FinishReason, State),

    %% 步骤 4：构建 assistant 消息
    AssistantMsg = build_assistant_message(Content, ToolCalls),

    %% 步骤 5：更新状态（批量更新）
    NewMsgs = Messages ++ [AssistantMsg],
    BaseUpdates = [
        {MsgsKey, NewMsgs},
        {last_response, Response},
        {last_content, Content},
        {tool_calls, ToolCalls},
        {finish_reason, FinishReason}
    ],

    %% 步骤 6：同步 full_messages（如果存在）
    AllUpdates = beamai_agent_utils:append_to_full_messages(BaseUpdates, AssistantMsg, State),
    NewState = ?SET_STATE_MANY(State, AllUpdates),

    {ok, NewState}.

%% @doc 构建 assistant 消息
%%
%% 委托给 beamai_message_utils 模块。
%%
%% @param Content 消息内容
%% @param ToolCalls 工具调用列表
%% @returns assistant 消息映射
-spec build_assistant_message(binary() | null, [map()]) -> map().
build_assistant_message(Content, ToolCalls) ->
    beamai_agent_utils:build_assistant_message(Content, ToolCalls).

%%====================================================================
%% 私有函数
%%====================================================================

%% @private 执行 LLM 调用
%%
%% 调用 LLM 客户端并根据结果触发回调、处理响应。
%%
%% @param LLMConfig LLM 配置
%% @param AllMsgs 完整消息列表
%% @param LLMOpts LLM 调用选项
%% @param Messages 原始消息列表（用于追加）
%% @param State 图状态
%% @param MsgsKey 消息键
%% @returns {ok, NewState}
-spec execute_llm_call(map(), [map()], map(), [map()], map(), atom()) ->
    {ok, map()}.
execute_llm_call(LLMConfig, AllMsgs, LLMOpts, Messages, State, MsgsKey) ->
    case llm_client:chat(LLMConfig, AllMsgs, LLMOpts) of
        {ok, Response} ->
            %% 成功：触发 on_llm_end 回调，处理响应
            ?INVOKE_CALLBACK_FROM_STATE(on_llm_end, [Response], State),
            process_llm_response(Response, Messages, State, MsgsKey);

        {error, Reason} ->
            %% 失败：触发 on_llm_error 回调，设置错误状态
            ?INVOKE_CALLBACK_FROM_STATE(on_llm_error, [Reason], State),
            ErrorState = ?SET_STATE_MANY(State, [
                {error, Reason},
                {status, error}
            ]),
            {ok, ErrorState}
    end.
