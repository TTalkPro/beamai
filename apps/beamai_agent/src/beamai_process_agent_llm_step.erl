%%%-------------------------------------------------------------------
%%% @doc Process-native Agent LLM Step
%%%
%%% 实现 beamai_step_behaviour，作为 Process-native Agent 的 LLM 调用环节。
%%% 与 beamai_agent 的内部 tool loop 不同，本模块将每次 LLM 调用
%%% 作为独立的 step 激活，tool 调用通过 Process 事件路由到
%%% beamai_process_agent_tool_step 执行。
%%%
%%% == 工作流程 ==
%%%   1. 接收 user_message 或 tool_results 输入
%%%   2. 构建消息列表，调用 LLM（通过 Context 中的 Kernel）
%%%   3. 若 LLM 返回 tool_calls: 发射 tool_request 事件
%%%   4. 若 LLM 返回文本: 发射 agent_done 事件
%%%
%%% == 配置 ==
%%% ```
%%% #{
%%%     system_prompt => binary(),          %% 系统提示词
%%%     max_tool_iterations => pos_integer(),%% 最大 tool 迭代次数（默认 10）
%%%     output_event => atom()              %% 完成事件名（默认 agent_done）
%%% }
%%% ```
%%%
%%% == 输入 ==
%%%   - user_message: binary() — 用户消息
%%%   - tool_results: [map()] — tool 执行结果（来自 tool_step）
%%%
%%% == 输出事件 ==
%%%   - tool_request: #{tool_calls => [...], assistant_msg => map()}
%%%   - agent_done: #{response => binary(), tool_calls_made => [...], turn_count => integer()}
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_agent_llm_step).

-behaviour(beamai_step_behaviour).

-export([init/1, can_activate/2, on_activate/3]).

%%====================================================================
%% beamai_step_behaviour 回调
%%====================================================================

init(Config) ->
    SystemPrompt = maps:get(system_prompt, Config, undefined),
    MaxIter = maps:get(max_tool_iterations, Config, 10),
    OutputEvent = maps:get(output_event, Config, agent_done),
    {ok, #{
        system_prompt => SystemPrompt,
        messages => [],
        iteration => 0,
        max_iterations => MaxIter,
        output_event => OutputEvent,
        tool_calls_made => [],
        all_tool_calls => [],
        turn_count => 0,
        last_response => <<>>
    }}.

can_activate(Inputs, _State) ->
    maps:is_key(user_message, Inputs) orelse maps:is_key(tool_results, Inputs).

%% @doc 迭代次数耗尽时直接返回错误
on_activate(_Inputs, #{iteration := Iter, max_iterations := MaxIter,
                       tool_calls_made := PrevToolCalls}, _Context)
  when Iter >= MaxIter ->
    {error, {max_tool_iterations, PrevToolCalls}};

%% @doc 正常激活：获取 Kernel 并执行 LLM 调用
on_activate(Inputs, State, Context) ->
    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, no_kernel_in_context};
        Kernel ->
            do_llm_call(Inputs, State, Kernel)
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 执行 LLM 调用并处理响应
%%
%% 构建消息列表，调用 LLM，根据响应类型分发处理。
do_llm_call(Inputs, State, Kernel) ->
    #{system_prompt := SysPrompt, messages := History} = State,
    {NewMessages, IsNewTurn} = build_messages(Inputs, History, SysPrompt),
    ChatOpts = beamai_agent_utils:build_chat_opts(Kernel, #{}),
    case beamai_kernel:invoke_chat(Kernel, NewMessages, ChatOpts) of
        {ok, #{tool_calls := TCs} = _Response, _Ctx} when is_list(TCs), TCs =/= [] ->
            handle_tool_response(TCs, NewMessages, State);
        {ok, Response, _Ctx} ->
            handle_text_response(Response, Inputs, State, NewMessages, History, IsNewTurn);
        {error, Reason} ->
            {error, {llm_call_failed, Reason}}
    end.

%% @private 处理 LLM 返回 tool_calls 的情况
%%
%% 记录 tool 调用信息，发射 tool_request 事件。
handle_tool_response(TCs, NewMessages, State) ->
    #{iteration := Iter, tool_calls_made := PrevToolCalls,
      all_tool_calls := AllPrevToolCalls} = State,
    AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
    UpdatedMessages = NewMessages ++ [AssistantMsg],
    NewToolCalls = lists:map(fun(TC) ->
        {_Id, Name, Args} = beamai_function:parse_tool_call(TC),
        #{name => Name, args => Args}
    end, TCs),
    NewState = State#{
        messages => UpdatedMessages,
        iteration => Iter + 1,
        tool_calls_made => PrevToolCalls ++ NewToolCalls,
        all_tool_calls => AllPrevToolCalls ++ NewToolCalls
    },
    EventData = #{tool_calls => TCs, assistant_msg => AssistantMsg},
    Event = beamai_process_event:new(tool_request, EventData),
    {ok, #{events => [Event], state => NewState}}.

%% @private 处理 LLM 返回文本响应的情况
%%
%% 构建最终消息历史，更新状态，发射完成事件。
handle_text_response(Response, Inputs, State, NewMessages, History, IsNewTurn) ->
    #{output_event := OutputEvent, tool_calls_made := PrevToolCalls,
      turn_count := TurnCount} = State,
    Content = beamai_agent_utils:extract_content(Response),
    AssistantMsg = #{role => assistant, content => Content},
    UserMsg = extract_user_msg(Inputs),
    FinalMessages = case IsNewTurn of
        true -> History ++ [UserMsg, AssistantMsg];
        false -> NewMessages ++ [AssistantMsg]
    end,
    NewTurnCount = TurnCount + 1,
    NewState = State#{
        messages => FinalMessages,
        iteration => 0,
        tool_calls_made => [],
        turn_count => NewTurnCount,
        last_response => Content
    },
    EventData = #{
        response => Content,
        tool_calls_made => PrevToolCalls,
        turn_count => NewTurnCount,
        finish_reason => maps:get(finish_reason, Response, <<>>)
    },
    Event = beamai_process_event:new(OutputEvent, EventData),
    {ok, #{events => [Event], state => NewState}}.

%% @private 根据输入类型构建消息列表
%%
%% 支持三种输入：user_message、tool_results、其他（尝试提取 binary）。
%% 返回 {消息列表, 是否新对话轮} 二元组。
build_messages(#{user_message := UserMsg}, History, SysPrompt) ->
    Sys = sys_messages(SysPrompt),
    UserMessage = #{role => user, content => UserMsg},
    {Sys ++ History ++ [UserMessage], true};
build_messages(#{tool_results := ToolResults}, History, SysPrompt) ->
    Sys = sys_messages(SysPrompt),
    ToolMsgs = beamai_agent_utils:parse_tool_results_messages(ToolResults),
    {Sys ++ History ++ ToolMsgs, false};
build_messages(Inputs, History, SysPrompt) ->
    case find_user_message(Inputs) of
        {ok, Msg} ->
            Sys = sys_messages(SysPrompt),
            UserMessage = #{role => user, content => Msg},
            {Sys ++ History ++ [UserMessage], true};
        error ->
            Sys = sys_messages(SysPrompt),
            {Sys ++ History, false}
    end.

%% @private 构建系统消息列表
sys_messages(undefined) -> [];
sys_messages(<<>>) -> [];
sys_messages(Prompt) -> [#{role => system, content => Prompt}].

%% @private 从输入中提取用户消息
extract_user_msg(#{user_message := Msg}) ->
    #{role => user, content => Msg};
extract_user_msg(_) ->
    #{role => user, content => <<>>}.

%% @private 从输入 map 的值中查找 binary 类型的用户消息
find_user_message(Inputs) ->
    find_binary(maps:values(Inputs)).

%% @private 在值列表中查找第一个 binary 或含 user_message 的 map
find_binary([]) -> error;
find_binary([V | _]) when is_binary(V) -> {ok, V};
find_binary([#{user_message := Msg} | _]) -> {ok, Msg};
find_binary([_ | Rest]) -> find_binary(Rest).
