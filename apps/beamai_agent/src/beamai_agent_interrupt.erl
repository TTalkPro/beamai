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
    resume_action/3,
    replace_results_by_id/2,
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
    Base = #{
        status => interrupted,
        reason => Reason,
        messages => maps:get(messages, Context, []),
        completed_tool_results => maps:get(completed_tool_results, Context, []),
        interrupted_tool_call => maps:get(interrupted_tool_call, Context, undefined),
        iteration => maps:get(iteration, Context, 0),
        tool_calls_made => maps:get(tool_calls_made, Context, []),
        %% 中断前累积的 state 槽（纯数据），resume 时恢复进 context
        saved_state => maps:get(state, Context, #{}),
        interrupt_type => Type,
        created_at => erlang:system_time(millisecond)
    },
    %% 环境类暂停（phase=env_retry）额外携带批次结果与失败调用（供 resume retry）
    IntState = case maps:get(phase, Context, approval) of
        env_retry ->
            Base#{phase => env_retry,
                  batch_messages => maps:get(batch_messages, Context, []),
                  failed_calls => maps:get(failed_calls, Context, [])};
        _ ->
            Base#{phase => approval}
    end,
    UpdatedAgent = AgentState#{interrupt_state => IntState},
    {IntState, UpdatedAgent}.

%% @doc 构建恢复用的消息（人类输入作为被中断 tool_call 的结果）
%%
%% Agent 自管编排下，中断时本轮已累积的完整 messages（含 assistant(tool_calls)
%% 与已完成工具结果）由 interrupt_state.messages 携带。因此 resume 只需补一条
%% **人类输入作为被中断 tool_call 的 tool result**，由调用方拼到该 messages 之后。
%%
%% @param IntState 中断状态
%% @param HumanInput 人类输入（binary 或 map）
%% @returns 单元素列表 [人类输入作为 tool 结果]
-spec build_resume_messages(map(), term()) -> [map()].
build_resume_messages(#{interrupted_tool_call := InterruptedCall}, HumanInput) ->
    [#{
        role => tool,
        tool_call_id => get_tool_call_id(InterruptedCall),
        content => format_human_input(HumanInput)
    }].

%% @doc 审批暂停下把 Decision + Payload 解析为一个动作（见 design §4）：
%%   `{execute, ToolCall}` — approved：执行被中断工具（Payload 有 args 则替换参数）；
%%   `{result, Message}`   — reply/拒绝/自由文本：直接作为被中断工具的结果消息。
%%
%% 决策关键词（其余一律回落"自由文本作结果"，兼容旧 resume(Agent, 文本)）：
%%   approved → 执行；reply → Payload.message 作结果（必填）；rejected → 拒绝语。
-spec resume_action(map(), term(), map()) ->
    {execute, map()} | {result, map()}.
resume_action(#{interrupted_tool_call := TC}, Decision, Payload) ->
    Id = get_tool_call_id(TC),
    case decision_kind(Decision) of
        approved ->
            {execute, apply_arg_override(TC, Id, Payload)};
        reply ->
            {result, tool_result_msg(Id, format_human_input(maps:get(message, Payload, <<>>)))};
        rejected ->
            {result, tool_result_msg(Id, reject_text(Payload))};
        {text, T} ->
            {result, tool_result_msg(Id, format_human_input(T))}
    end.

%% @doc 用重跑结果按 tool_call_id 替换原批次消息中对应的 tool 结果（环境 retry 用）
%%
%% 历史无重复 tool_result：同 id 的旧结果被新结果整条替换，其余原样保留。
-spec replace_results_by_id([map()], [map()]) -> [map()].
replace_results_by_id(BatchMsgs, NewMsgs) ->
    ById = maps:from_list([{maps:get(tool_call_id, M), M}
                           || M <- NewMsgs, maps:is_key(tool_call_id, M)]),
    [maps:get(maps:get(tool_call_id, Msg, undefined), ById, Msg) || Msg <- BatchMsgs].

%% @private 决策关键词归类
decision_kind(<<"approved">>) -> approved;
decision_kind(approved) -> approved;
decision_kind(<<"reply">>) -> reply;
decision_kind(reply) -> reply;
decision_kind(<<"rejected">>) -> rejected;
decision_kind(rejected) -> rejected;
decision_kind(<<"reject">>) -> rejected;
decision_kind(reject) -> rejected;
decision_kind(T) -> {text, T}.

%% @private 构建 tool 结果消息
tool_result_msg(Id, Content) ->
    #{role => tool, tool_call_id => Id, content => Content}.

%% @private 拒绝语（可带理由）
reject_text(#{message := R}) when is_binary(R), R =/= <<>> ->
    <<"已拒绝执行：", R/binary>>;
reject_text(_) ->
    <<"已拒绝执行">>.

%% @private approved 时若 Payload 带 args 则替换被中断工具参数（构建扁平执行 TC）
apply_arg_override(TC, Id, #{args := Args}) when is_map(Args) ->
    #{id => Id, name => get_tool_call_name(TC), arguments => Args};
apply_arg_override(TC, _Id, _Payload) ->
    TC.

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
%%
%% 支持 OpenAI 嵌套格式（function.name）与统一响应的扁平格式（name），
%% 与 beamai_tool:parse_tool_call/1 的格式覆盖保持一致。
%% 畸形 tool_call 返回 <<>>（不会匹配任何中断工具名），但留下日志痕迹，
%% 避免中断机制静默失效难以排查。
get_tool_call_name(#{function := #{name := Name}}) -> Name;
get_tool_call_name(#{<<"function">> := #{<<"name">> := Name}}) -> Name;
get_tool_call_name(#{name := Name}) -> Name;
get_tool_call_name(#{<<"name">> := Name}) -> Name;
get_tool_call_name(TC) ->
    logger:warning("beamai_agent_interrupt: cannot extract tool name from malformed tool_call: ~p", [TC]),
    <<>>.

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
