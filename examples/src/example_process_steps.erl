%%%-------------------------------------------------------------------
%%% @doc Process Framework 可复用 Step 模块
%%%
%%% 提供多种通用 Step 实现，供 Process Framework 示例使用：
%%%   - llm_chat: 单轮 LLM 对话
%%%   - llm_multiturn: 多轮对话（接续上下文）
%%%   - llm_tool_call: 带工具调用的 LLM 对话
%%%   - transform: 数据变换
%%%   - hook: 前置/后置钩子
%%%   - log: 日志记录
%%%   - merge: 合并多个输入
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_process_steps).

-behaviour(beamai_step_behaviour).

-export([init/1, can_activate/2, on_activate/3]).

%%====================================================================
%% Step: llm_chat - 单轮 LLM 对话
%%====================================================================
%%
%% Config:
%%   type => llm_chat
%%   system_prompt => binary()      (可选)
%%   output_event => atom()         (输出事件名)
%%
%% Input: #{input => #{user_message => binary(), ...}}
%% Output: 事件包含 #{response => binary(), messages => [...]}

%%====================================================================
%% Step: llm_multiturn - 多轮 LLM 对话
%%====================================================================
%%
%% Config:
%%   type => llm_multiturn
%%   system_prompt => binary()      (可选)
%%   output_event => atom()
%%
%% 状态中维护 messages 历史
%% Input: #{input => #{user_message => binary()}}
%% Output: 事件包含 #{response => binary(), history => [...]}

%%====================================================================
%% Step: llm_tool_call - 带工具调用
%%====================================================================
%%
%% Config:
%%   type => llm_tool_call
%%   system_prompt => binary()
%%   output_event => atom()
%%   plugins => [{Name, [FuncDef]}]  (工具定义)
%%
%% Input: #{input => #{user_message => binary()}}
%% Output: 事件包含 #{response => binary(), tool_calls => [...]}

%%====================================================================
%% Step: transform - 数据变换
%%====================================================================
%%
%% Config:
%%   type => transform
%%   transform_fn => fun(InputData) -> OutputData
%%   output_event => atom()

%%====================================================================
%% Step: hook - 前置/后置钩子
%%====================================================================
%%
%% Config:
%%   type => hook
%%   phase => pre | post
%%   hook_fn => fun(Data, State) -> {ok, NewData} | {error, Reason}
%%   output_event => atom()

%%====================================================================
%% Step: log - 日志步骤
%%====================================================================
%%
%% Config:
%%   type => log
%%   label => binary()
%%   output_event => atom()

%%====================================================================
%% Step: merge - 合并多输入
%%====================================================================
%%
%% Config:
%%   type => merge
%%   output_event => atom()
%%   required_inputs => [atom()]

%%====================================================================
%% Callbacks
%%====================================================================

init(#{type := llm_chat} = Config) ->
    {ok, #{
        type => llm_chat,
        system_prompt => maps:get(system_prompt, Config, <<"You are a helpful assistant.">>),
        output_event => maps:get(output_event, Config, chat_response)
    }};

init(#{type := llm_multiturn} = Config) ->
    SystemPrompt = maps:get(system_prompt, Config, <<"You are a helpful assistant.">>),
    {ok, #{
        type => llm_multiturn,
        system_prompt => SystemPrompt,
        output_event => maps:get(output_event, Config, multiturn_response),
        messages => [#{role => system, content => SystemPrompt}]
    }};

init(#{type := llm_tool_call} = Config) ->
    {ok, #{
        type => llm_tool_call,
        system_prompt => maps:get(system_prompt, Config, <<"You are a helpful assistant.">>),
        output_event => maps:get(output_event, Config, tool_response),
        plugins => maps:get(plugins, Config, [])
    }};

init(#{type := transform} = Config) ->
    {ok, #{
        type => transform,
        transform_fn => maps:get(transform_fn, Config, fun(X) -> X end),
        output_event => maps:get(output_event, Config, transformed)
    }};

init(#{type := hook} = Config) ->
    {ok, #{
        type => hook,
        phase => maps:get(phase, Config, pre),
        hook_fn => maps:get(hook_fn, Config, fun(Data, _) -> {ok, Data} end),
        output_event => maps:get(output_event, Config, hooked)
    }};

init(#{type := log} = Config) ->
    {ok, #{
        type => log,
        label => maps:get(label, Config, <<"LOG">>),
        output_event => maps:get(output_event, Config, logged)
    }};

init(#{type := merge} = Config) ->
    {ok, #{
        type => merge,
        output_event => maps:get(output_event, Config, merged)
    }};

init(Config) ->
    {ok, #{type => passthrough, output_event => maps:get(output_event, Config, output)}}.

can_activate(_Inputs, _State) ->
    true.

%%====================================================================
%% on_activate - LLM Chat
%%====================================================================

on_activate(Inputs, #{type := llm_chat, system_prompt := SysPrompt,
                      output_event := OutputEvent} = State, Context) ->
    UserMessage = extract_user_message(Inputs),
    Messages = [
        #{role => system, content => SysPrompt},
        #{role => user, content => UserMessage}
    ],
    case do_chat(Context, Messages) of
        {ok, Response} ->
            Event = beamai_process_event:new(OutputEvent, #{
                response => Response,
                user_message => UserMessage,
                messages => Messages ++ [#{role => assistant, content => Response}]
            }),
            {ok, #{events => [Event], state => State}};
        {error, Reason} ->
            {error, {llm_chat_failed, Reason}}
    end;

%%====================================================================
%% on_activate - LLM Multiturn
%%====================================================================

on_activate(Inputs, #{type := llm_multiturn, output_event := OutputEvent,
                      messages := History} = State, Context) ->
    UserMessage = extract_user_message(Inputs),
    NewHistory = History ++ [#{role => user, content => UserMessage}],
    case do_chat(Context, NewHistory) of
        {ok, Response} ->
            FinalHistory = NewHistory ++ [#{role => assistant, content => Response}],
            Event = beamai_process_event:new(OutputEvent, #{
                response => Response,
                turn => length(FinalHistory) div 2,
                history => FinalHistory
            }),
            {ok, #{events => [Event], state => State#{messages => FinalHistory}}};
        {error, Reason} ->
            {error, {llm_multiturn_failed, Reason}}
    end;

%%====================================================================
%% on_activate - LLM Tool Call
%%====================================================================

on_activate(Inputs, #{type := llm_tool_call, system_prompt := SysPrompt,
                      output_event := OutputEvent, plugins := Plugins} = State, Context) ->
    UserMessage = extract_user_message(Inputs),
    Messages = [
        #{role => system, content => SysPrompt},
        #{role => user, content => UserMessage}
    ],
    case do_chat_with_tools(Context, Messages, Plugins) of
        {ok, Response, ToolCalls} ->
            Event = beamai_process_event:new(OutputEvent, #{
                response => Response,
                user_message => UserMessage,
                tool_calls => ToolCalls
            }),
            {ok, #{events => [Event], state => State}};
        {error, Reason} ->
            {error, {llm_tool_call_failed, Reason}}
    end;

%%====================================================================
%% on_activate - Transform
%%====================================================================

on_activate(Inputs, #{type := transform, transform_fn := Fn,
                      output_event := OutputEvent} = State, _Context) ->
    InputData = maps:get(input, Inputs, Inputs),
    Result = Fn(InputData),
    Event = beamai_process_event:new(OutputEvent, Result),
    {ok, #{events => [Event], state => State}};

%%====================================================================
%% on_activate - Hook
%%====================================================================

on_activate(Inputs, #{type := hook, phase := Phase, hook_fn := HookFn,
                      output_event := OutputEvent} = State, _Context) ->
    InputData = maps:get(input, Inputs, Inputs),
    io:format("  [Hook:~p] Processing data~n", [Phase]),
    case HookFn(InputData, State) of
        {ok, NewData} ->
            Event = beamai_process_event:new(OutputEvent, NewData),
            {ok, #{events => [Event], state => State}};
        {error, Reason} ->
            {error, {hook_failed, Phase, Reason}}
    end;

%%====================================================================
%% on_activate - Log
%%====================================================================

on_activate(Inputs, #{type := log, label := Label,
                      output_event := OutputEvent} = State, _Context) ->
    InputData = maps:get(input, Inputs, Inputs),
    io:format("  [~ts] ~p~n", [Label, InputData]),
    Event = beamai_process_event:new(OutputEvent, InputData),
    {ok, #{events => [Event], state => State}};

%%====================================================================
%% on_activate - Merge
%%====================================================================

on_activate(Inputs, #{type := merge, output_event := OutputEvent} = State, _Context) ->
    %% 将所有输入合并为单个 map
    Event = beamai_process_event:new(OutputEvent, Inputs),
    {ok, #{events => [Event], state => State}};

%%====================================================================
%% on_activate - Passthrough
%%====================================================================

on_activate(Inputs, #{type := passthrough, output_event := OutputEvent} = State, _Context) ->
    Event = beamai_process_event:new(OutputEvent, Inputs),
    {ok, #{events => [Event], state => State}}.

%%====================================================================
%% Internal
%%====================================================================

extract_user_message(#{input := #{user_message := Msg}}) -> Msg;
extract_user_message(#{input := Msg}) when is_binary(Msg) -> Msg;
extract_user_message(#{user_message := Msg}) -> Msg;
extract_user_message(Inputs) when is_map(Inputs) ->
    %% 尝试从任意 input key 中获取 user_message
    case maps:values(Inputs) of
        [#{user_message := Msg} | _] -> Msg;
        [Msg | _] when is_binary(Msg) -> Msg;
        _ -> <<"(no message)">>
    end.

do_chat(Context, Messages) ->
    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, no_kernel_in_context};
        Kernel ->
            case beamai_kernel:invoke_chat(Kernel, Messages, #{}) of
                {ok, #{content := Content}, _} ->
                    {ok, Content};
                {error, _} = Error ->
                    Error
            end
    end.

do_chat_with_tools(Context, Messages, Plugins) ->
    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, no_kernel_in_context};
        Kernel ->
            %% 将 plugins 注册到 kernel
            K1 = lists:foldl(
                fun({Name, Functions}, KAcc) ->
                    beamai_kernel:add_plugin(KAcc, Name, Functions)
                end,
                Kernel,
                Plugins
            ),
            case beamai_kernel:invoke(K1, Messages, #{}) of
                {ok, #{content := Content}, Ctx} ->
                    ToolCalls = extract_tool_calls(Ctx),
                    {ok, Content, ToolCalls};
                {error, _} = Error ->
                    Error
            end
    end.

extract_tool_calls(Context) ->
    Trace = beamai_context:get_trace(Context),
    [Entry || #{type := tool_call} = Entry <- Trace].
