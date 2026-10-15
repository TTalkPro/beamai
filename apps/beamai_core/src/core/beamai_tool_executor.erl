%%%-------------------------------------------------------------------
%%% @doc 工具执行器（运行侧）
%%%
%%% **执行工具的是这里，不是 ChatClient**。ChatClient 只声明「有哪些工具」，
%%% 到了要跑的时候由本模块按名解析、经 around_tool 洋葱执行、把结果与写意图
%%% 交回调用方。
%%%
%%% 对照 Spring AI（docs/api/tools.html）：本模块是 `ToolCallingManager` 的
%%% **单次执行原语**——它的另外两件事在别处：
%%% - 批量/并发/串行调度、限额、错误合成 → beamai_tool_calling_manager（agent 层）
%%% - 循环（把结果拼回历史再问模型）→ beamai_agent_tool_loop 的循环 filter
%%%
%%% 之所以入参是 ChatClient 而不是裸注册表：执行要同时用到**工具表**与
%%% **around_tool filter 链**，二者都挂在 ChatClient 上——正如 Spring 的 manager
%%% 从 Prompt 携带的 ChatOptions 里取 toolCallbacks。
%%%
%%% Context 为只读运行环境（自动绑定当前 ChatClient 引用，供工具内部组合调用）；
%%% 工具写状态经返回值第三元 Writes 表达，本模块原样透出，由调用方（tool 批次）
%%% 折叠进 state。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_executor).

-export([invoke/4]).

%%====================================================================
%% API
%%====================================================================

%% @doc 执行一次工具调用（按名解析 → around_tool 洋葱 → 归一返回）
-spec invoke(beamai_chat_client:chat_client(), binary(), beamai_tool:args(),
             beamai_context:t()) ->
    {ok, term(), beamai_context:writes()} | {error, term()}.
invoke(ChatClient, ToolName, Args, Context0) ->
    case beamai_tool_registry:resolve(beamai_chat_client:tools(ChatClient), ToolName) of
        {ok, ToolSpec} ->
            Context = beamai_context:with_chat_client(Context0, ChatClient),
            run(beamai_chat_client:filters(ChatClient), ToolSpec, Args, Context);
        error ->
            {error, {tool_not_found, ToolName}}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 运行 tool filter 洋葱链（用 around_tool hook）
%%
%% Request `#{tool, args, context}` → Response `#{result, writes, context}`，
%% 最内层 terminal 为真正的工具执行。`writes` 为工具写意图（纯数据），透出给
%% 调用方折叠进 state；`context` 仅承载 filter 私有状态合并（框架用）。
run(Filters, ToolSpec, Args, Context) ->
    Req = #{tool => ToolSpec, args => Args, context => Context},
    case beamai_filter_chain:run(Filters, around_tool, terminal(), Req) of
        {ok, #{result := Value} = Resp} -> {ok, Value, maps:get(writes, Resp, #{})};
        {error, _} = Err -> Err
    end.

%% @private tool 链最内层：真正执行工具（出错时 throw，由链统一捕获）
%%
%% 归一工具返回：`{ok,V}` → 空 writes；`{ok,V,W}` → W 为写意图。
%% Context 只读透传（filter 私有状态由链在外层合并）。
terminal() ->
    fun(#{tool := ToolSpec, args := Args, context := Ctx}) ->
        case beamai_tool:invoke(ToolSpec, Args, Ctx) of
            {ok, Value} ->
                Value1 = enforce_result_policy(ToolSpec, Value),
                #{result => Value1, writes => #{}, context => Ctx};
            {ok, Value, Writes} ->
                Value1 = enforce_result_policy(ToolSpec, Value),
                #{result => Value1, writes => Writes, context => Ctx};
            {error, Reason} -> throw(Reason)
        end
    end.

%% @private 对真正回传模型的结果执行 redaction 与大小上限
%%
%% 两条策略都 opt-in：未配置的工具保持原有 term 返回，零迁移成本。
%% redaction 先执行，避免 oversized payload 中已在文本内的秘密影响判断；
%% 大小检查以最终将要回传的编码文本为准。
enforce_result_policy(ToolSpec, Value) ->
    Rules = maps:get(result_redaction, ToolSpec, []),
    MaxBytes = maps:get(max_result_bytes, ToolSpec, undefined),
    case Rules =:= [] andalso MaxBytes =:= undefined of
        true -> Value;
        false -> enforce_configured_result_policy(ToolSpec, Value)
    end.

enforce_configured_result_policy(ToolSpec, Value) ->
    Rules = maps:get(result_redaction, ToolSpec, []),
    case beamai_tool:redact_result(Value, Rules) of
        {ok, Redacted} ->
            case beamai_tool:result_size(
                   Redacted, maps:get(max_result_bytes, ToolSpec, undefined)) of
                ok -> Redacted;
                {error, tool_result_too_large, Size, Max} ->
                    throw(#{error_class => semantic,
                            reason => tool_result_too_large,
                            actual_bytes => Size,
                            max_bytes => Max,
                            stacktrace => []})
            end;
        {error, Reason} ->
            throw(#{error_class => semantic,
                    reason => invalid_result_redaction,
                    detail => Reason,
                    stacktrace => []})
    end.
