%%%-------------------------------------------------------------------
%%% @doc Agent 中断机制管理
%%%
%%% 统一处理三种中断触发方式：
%%%   A. 特殊 Tool 触发 — LLM 调用注册在 interrupt_tools 中的 tool
%%%   B. 回调触发 — on_tool_call callback 返回 {interrupt, Reason}
%%%   C. Tool 执行结果触发 — tool 执行返回 {interrupt, Reason, PartialResult}
%%%
%%% 主要职责：
%%%   - 检测 tool_calls 中是否包含中断 tool
%%%   - 构建 interrupt_state（保存 tool loop 完整上下文）
%%%   - 构建恢复消息列表（resume 时使用）
%%%   - 验证 resume 输入
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_interrupt).

-export([
    find_interrupt_tool/2,
    is_interrupt_tool/2,
    handle_interrupt/4,
    build_resume_messages/2,
    validate_resume_input/2,
    get_interrupt_tool_specs/1
]).

%%====================================================================
%% API
%%====================================================================

%% @doc 从 tool_calls 列表中查找中断 tool
%%
%% 遍历 tool_calls，检查是否有匹配 interrupt_tools 配置的调用。
%% 如果找到，返回中断 tool_call 和其余 tool_calls。
%%
%% @param ToolCalls LLM 返回的 tool_call 列表
%% @param AgentState agent 状态（含 interrupt_tools 配置）
%% @returns {yes, InterruptToolCall, OtherCalls} | no
-spec find_interrupt_tool([map()], map()) ->
    {yes, map(), [map()]} | no.
find_interrupt_tool(ToolCalls, #{interrupt_tools := InterruptTools}) ->
    InterruptNames = [maps:get(name, T, maps:get(<<"name">>, T, <<>>))
                      || T <- InterruptTools],
    find_interrupt_in_calls(ToolCalls, InterruptNames, []);
find_interrupt_tool(_ToolCalls, _AgentState) ->
    no.

%% @doc 检查单个 tool_call 是否为中断 tool
%%
%% @param ToolCall 单个 tool_call map
%% @param InterruptTools 中断 tool 定义列表
%% @returns boolean()
-spec is_interrupt_tool(map(), [map()]) -> boolean().
is_interrupt_tool(ToolCall, InterruptTools) ->
    TCName = get_tool_call_name(ToolCall),
    InterruptNames = [maps:get(name, T, maps:get(<<"name">>, T, <<>>))
                      || T <- InterruptTools],
    lists:member(TCName, InterruptNames).

%% @doc 处理中断：构建 interrupt_state
%%
%% 根据中断类型和上下文构建完整的 interrupt_state，
%% 保存 tool loop 当前的所有关键状态以便后续恢复。
%%
%% @param Type 中断类型 (tool_request | tool_result | callback)
%% @param Reason 中断原因
%% @param Context tool loop 上下文
%% @param AgentState agent 状态
%% @returns {interrupt_state(), agent_state()}
-spec handle_interrupt(atom(), term(), map(), map()) ->
    {map(), map()}.
handle_interrupt(Type, Reason, Context, AgentState) ->
    IntState = #{
        status => interrupted,
        reason => Reason,
        pending_messages => maps:get(pending_messages, Context, []),
        assistant_response => maps:get(assistant_response, Context, #{}),
        completed_tool_results => maps:get(completed_tool_results, Context, []),
        interrupted_tool_call => maps:get(interrupted_tool_call, Context, undefined),
        iteration => maps:get(iteration, Context, 0),
        tool_calls_made => maps:get(tool_calls_made, Context, []),
        interrupt_type => Type,
        created_at => erlang:system_time(millisecond)
    },
    UpdatedAgent = AgentState#{interrupt_state => IntState},
    {IntState, UpdatedAgent}.

%% @doc 构建恢复的消息列表
%%
%% 中断时保存的消息 + 人类输入作为 tool result，
%% 组装成可以直接传给 tool_loop 继续执行的消息列表。
%%
%% 消息结构:
%%   [原始消息...] ++ [assistant(with tool_calls)] ++
%%   [已完成的tool结果...] ++ [人类输入作为tool结果]
%%
%% @param IntState 中断状态
%% @param HumanInput 人类输入（binary 或 map）
%% @returns 恢复用的完整消息列表
-spec build_resume_messages(map(), term()) -> [map()].
build_resume_messages(IntState, HumanInput) ->
    #{
        pending_messages := PendingMsgs,
        assistant_response := AssistantResp,
        completed_tool_results := CompletedResults,
        interrupted_tool_call := InterruptedCall
    } = IntState,

    %% 构建人类输入作为 tool result
    HumanToolResult = #{
        role => tool,
        tool_call_id => get_tool_call_id(InterruptedCall),
        content => format_human_input(HumanInput)
    },

    PendingMsgs ++ [AssistantResp | CompletedResults] ++ [HumanToolResult].

%% @doc 验证 resume 输入是否匹配中断上下文
%%
%% 检查：
%%   - 中断状态存在且有效
%%   - 输入不为空
%%
%% @param IntState 中断状态
%% @param HumanInput 人类输入
%% @returns ok | {error, term()}
-spec validate_resume_input(map(), term()) -> ok | {error, term()}.
validate_resume_input(undefined, _HumanInput) ->
    {error, not_interrupted};
validate_resume_input(#{status := interrupted}, HumanInput) when
    HumanInput =:= undefined; HumanInput =:= <<>> ->
    {error, empty_input};
validate_resume_input(#{status := interrupted}, _HumanInput) ->
    ok;
validate_resume_input(_, _) ->
    {error, invalid_interrupt_state}.

%% @doc 获取 interrupt_tools 的 tool specs（用于发送给 LLM）
%%
%% 将 interrupt_tools 配置转换为标准的 OpenAI tool spec 格式，
%% 以便和 kernel 中的普通 tool specs 合并后发送给 LLM。
%%
%% @param AgentState agent 状态
%% @returns tool specs 列表
-spec get_interrupt_tool_specs(map()) -> [map()].
get_interrupt_tool_specs(#{interrupt_tools := InterruptTools}) when InterruptTools =/= [] ->
    [interrupt_tool_to_spec(T) || T <- InterruptTools];
get_interrupt_tool_specs(_) ->
    [].

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 在 tool_calls 列表中查找中断 tool
find_interrupt_in_calls([], _InterruptNames, _Acc) ->
    no;
find_interrupt_in_calls([TC | Rest], InterruptNames, Acc) ->
    TCName = get_tool_call_name(TC),
    case lists:member(TCName, InterruptNames) of
        true ->
            OtherCalls = lists:reverse(Acc) ++ Rest,
            {yes, TC, OtherCalls};
        false ->
            find_interrupt_in_calls(Rest, InterruptNames, [TC | Acc])
    end.

%% @private 从 tool_call 中提取函数名
get_tool_call_name(#{function := #{name := Name}}) -> Name;
get_tool_call_name(#{<<"function">> := #{<<"name">> := Name}}) -> Name;
get_tool_call_name(_) -> <<>>.

%% @private 从 tool_call 中提取 ID
get_tool_call_id(#{id := Id}) -> Id;
get_tool_call_id(#{<<"id">> := Id}) -> Id;
get_tool_call_id(undefined) -> <<"unknown">>;
get_tool_call_id(_) -> <<"unknown">>.

%% @private 格式化人类输入为 tool result content
format_human_input(Input) when is_binary(Input) ->
    Input;
format_human_input(Input) when is_map(Input) ->
    jsx:encode(Input);
format_human_input(Input) when is_list(Input) ->
    list_to_binary(Input);
format_human_input(Input) ->
    list_to_binary(io_lib:format("~p", [Input])).

%% @private 将 interrupt_tool 配置转换为 OpenAI tool spec
interrupt_tool_to_spec(#{name := Name} = Tool) ->
    #{
        type => function,
        function => #{
            name => Name,
            description => maps:get(description, Tool, <<>>),
            parameters => maps:get(parameters, Tool, #{type => object, properties => #{}})
        }
    };
interrupt_tool_to_spec(#{<<"name">> := Name} = Tool) ->
    #{
        type => function,
        function => #{
            name => Name,
            description => maps:get(<<"description">>, Tool, <<>>),
            parameters => maps:get(<<"parameters">>, Tool, #{type => object, properties => #{}})
        }
    }.
