%%%-------------------------------------------------------------------
%%% @doc Kernel 核心：工具管理、LLM 服务、过滤器、工具调用循环
%%%
%%% Kernel 是框架的中枢，负责：
%%% - 管理工具注册
%%% - 持有 LLM 服务配置
%%% - 执行前置/后置过滤器管道
%%% - 驱动工具调用循环（LLM ↔ Tool）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_kernel).

%% Build API
-export([new/0, new/1]).
-export([add_tool/2]).
-export([add_tools/2]).
-export([add_tool_module/2]).
-export([add_service/2]).
-export([add_filter/2]).

%% Invoke API
-export([invoke/3]).
-export([invoke_tool/4]).
-export([invoke_chat/3]).

%% Query API
-export([get_tool/2]).
-export([list_tools/1]).
-export([get_tools_by_tag/2]).
-export([get_tool_specs/1]).
-export([get_tool_schemas/1, get_tool_schemas/2]).
-export([get_service/1]).

%% Types
-export_type([kernel/0, kernel_settings/0, chat_opts/0]).

-type kernel() :: #{
    '__kernel__' := true,
    tools := #{binary() => beamai_tool:tool_spec()},
    llm_config := beamai_chat_completion:config() | undefined,
    filters := [beamai_filter:filter_spec()],
    settings := kernel_settings()
}.

-type kernel_settings() :: #{
    max_tool_iterations => pos_integer(),
    default_timeout => pos_integer(),
    atom() => term()
}.

-type chat_opts() :: #{
    tools => [map()],
    tool_choice => auto | none | required,
    max_tool_iterations => pos_integer(),
    context => beamai_context:t(),
    system_prompts => [map()],
    atom() => term()
}.

%%====================================================================
%% Build API
%%====================================================================

%% @doc 创建空 Kernel（默认配置）
-spec new() -> kernel().
new() ->
    new(#{}).

%% @doc 创建 Kernel（自定义配置）
%%
%% @param Settings 配置项（如 #{max_tool_iterations => 5}）
%% @returns Kernel 实例
-spec new(kernel_settings()) -> kernel().
new(Settings) ->
    #{
        '__kernel__' => true,
        tools => #{},
        llm_config => undefined,
        filters => [],
        settings => Settings
    }.

%% @doc 注册工具到 Kernel
%%
%% 工具以其名称为键存入 tools Map。重名工具会被覆盖。
%%
%% @param Kernel Kernel 实例
%% @param Tool 工具定义（需包含 name 字段）
%% @returns 更新后的 Kernel
-spec add_tool(kernel(), beamai_tool:tool_spec()) -> kernel().
add_tool(#{tools := Tools} = Kernel, #{name := Name} = Tool) ->
    Kernel#{tools => Tools#{Name => Tool}}.

%% @doc 批量注册工具到 Kernel
%%
%% @param Kernel Kernel 实例
%% @param ToolList 工具定义列表
%% @returns 更新后的 Kernel
-spec add_tools(kernel(), [beamai_tool:tool_spec()]) -> kernel().
add_tools(Kernel, ToolList) ->
    lists:foldl(fun(Tool, K) -> add_tool(K, Tool) end, Kernel, ToolList).

%% @doc 从模块自动加载并注册工具
%%
%% 模块需实现 beamai_tool_behaviour，至少实现 tools/0 回调。
%% 加载失败时抛出 {tool_module_load_failed, Module, Reason} 错误。
%%
%% @param Kernel Kernel 实例
%% @param Module 实现了工具回调的模块
%% @returns 更新后的 Kernel
-spec add_tool_module(kernel(), module()) -> kernel().
add_tool_module(Kernel, Module) ->
    case beamai_tool:from_module(Module) of
        {ok, Tools} ->
            %% 如果模块实现了 filters/0，也注册过滤器
            K1 = add_tools(Kernel, Tools),
            maybe_add_filters(K1, Module);
        {error, Reason} ->
            erlang:error({tool_module_load_failed, Module, Reason})
    end.

%% @private 如果模块实现了 filters/0，添加过滤器
maybe_add_filters(Kernel, Module) ->
    case erlang:function_exported(Module, filters, 0) of
        true ->
            Filters = Module:filters(),
            lists:foldl(fun add_filter/2, Kernel, Filters);
        false ->
            Kernel
    end.

%% @doc 设置 LLM 服务配置
%%
%% 配置通过 beamai_chat_completion:create/2 创建。
%% 设置后可使用 invoke_chat/3 和 invoke/3。
%%
%% @param Kernel Kernel 实例
%% @param LlmConfig LLM 配置 Map
%% @returns 更新后的 Kernel
-spec add_service(kernel(), beamai_chat_completion:config()) -> kernel().
add_service(Kernel, LlmConfig) ->
    Kernel#{llm_config => LlmConfig}.

%% @doc 注册过滤器到 Kernel
%%
%% 过滤器追加到现有列表末尾，执行时按 priority 排序。
%%
%% @param Kernel Kernel 实例
%% @param Filter 过滤器定义（通过 beamai_filter:new/3,4 创建）
%% @returns 更新后的 Kernel
-spec add_filter(kernel(), beamai_filter:filter_spec()) -> kernel().
add_filter(#{filters := Filters} = Kernel, Filter) ->
    Kernel#{filters => Filters ++ [Filter]}.

%%====================================================================
%% Invoke API
%%====================================================================

%% @doc 调用 Kernel 中注册的工具
%%
%% 执行流程：查找工具 → 前置过滤器 → 工具执行 → 后置过滤器。
%% 上下文会自动关联当前 Kernel 引用。
%%
%% @param Kernel Kernel 实例
%% @param ToolName 工具名称
%% @param Args 调用参数
%% @param Context 执行上下文
%% @returns {ok, 结果, 更新后上下文} | {error, 原因}
-spec invoke_tool(kernel(), binary(), beamai_tool:args(), beamai_context:t()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
invoke_tool(#{filters := Filters} = Kernel, ToolName, Args, Context0) ->
    case get_tool(Kernel, ToolName) of
        {ok, ToolSpec} ->
            Context = beamai_context:with_kernel(Context0, Kernel),
            run_invoke_pipeline(Filters, ToolSpec, Args, Context);
        error ->
            {error, {tool_not_found, ToolName}}
    end.

%% @doc 发送 Chat Completion 请求（不含工具调用循环）
%%
%% 执行流程：前置 Chat 过滤器 → LLM 调用 → 后置 Chat 过滤器。
%% Kernel 需先通过 add_service/2 配置 LLM。
%%
%% @param Kernel Kernel 实例
%% @param Messages 消息列表（[#{role => ..., content => ...}]）
%% @param Opts Chat 选项
%% @returns {ok, 响应 Map, 更新后上下文} | {error, 原因}
-spec invoke_chat(kernel(), [map()], chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
invoke_chat(Kernel, Messages, Opts) ->
    case get_service(Kernel) of
        {ok, LlmConfig} ->
            #{filters := Filters} = Kernel,
            Context = maps:get(context, Opts, beamai_context:new()),
            run_chat_pipeline(LlmConfig, Filters, Messages, Opts, Context);
        error ->
            {error, no_llm_service}
    end.

%% @doc 发送 Chat Completion 请求并驱动工具调用循环
%%
%% 自动将 Kernel 中所有注册工具转为 tool specs 传给 LLM。
%% LLM 返回 tool_calls 时自动执行对应工具，将结果拼入消息后再次请求 LLM，
%% 循环直到 LLM 返回文本响应或达到最大迭代次数。
%%
%% @param Kernel Kernel 实例（需注册工具和 LLM 服务）
%% @param Messages 新输入消息列表（与 context.messages 组合后发给 LLM）
%% @param Opts Chat 选项（可设置 system_prompts、max_tool_iterations、tool_choice）
%% @returns {ok, 最终响应 Map, 更新后上下文} | {error, 原因}
-spec invoke(kernel(), [map()], chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
invoke(Kernel, Messages, Opts) ->
    ToolSpecs = get_tool_specs(Kernel),
    ChatOpts = Opts#{tools => ToolSpecs, tool_choice => maps:get(tool_choice, Opts, auto)},
    Context0 = maps:get(context, Opts, beamai_context:new()),
    SystemPrompts = maps:get(system_prompts, Opts, []),
    %% 组合 context.messages（已有上下文）+ Messages（新输入）
    ExistingMsgs = beamai_context:get_messages(Context0),
    ConvMsgs = ExistingMsgs ++ Messages,
    %% 记录新输入消息到 messages 和 history（system_prompts 不记录）
    Context = track_new_messages(Context0, Messages),
    case get_service(Kernel) of
        {ok, LlmConfig} ->
            #{filters := Filters} = Kernel,
            MaxIter = maps:get(max_tool_iterations, Opts,
                maps:get(max_tool_iterations, maps:get(settings, Kernel, #{}), 10)),
            tool_calling_loop(Kernel, LlmConfig, Filters, ConvMsgs, ChatOpts, Context, SystemPrompts, MaxIter);
        error ->
            {error, no_llm_service}
    end.

%%====================================================================
%% Query API
%%====================================================================

%% @doc 按名称查找 Kernel 中注册的工具
%%
%% @param Kernel Kernel 实例
%% @param ToolName 工具名称
%% @returns {ok, 工具定义} | error
-spec get_tool(kernel(), binary()) -> {ok, beamai_tool:tool_spec()} | error.
get_tool(#{tools := Tools}, ToolName) ->
    maps:find(ToolName, Tools).

%% @doc 列出 Kernel 中所有注册的工具
-spec list_tools(kernel()) -> [beamai_tool:tool_spec()].
list_tools(#{tools := Tools}) ->
    maps:values(Tools).

%% @doc 按标签查找工具
%%
%% @param Kernel Kernel 实例
%% @param Tag 标签
%% @returns 匹配的工具列表
-spec get_tools_by_tag(kernel(), binary()) -> [beamai_tool:tool_spec()].
get_tools_by_tag(#{tools := Tools}, Tag) ->
    [T || T <- maps:values(Tools), beamai_tool:has_tag(T, Tag)].

%% @doc 获取所有工具的统一 tool spec 列表
%%
%% 返回包含 name、description、parameters 的中间格式。
-spec get_tool_specs(kernel()) -> [map()].
get_tool_specs(Kernel) ->
    Tools = list_tools(Kernel),
    [beamai_tool:to_tool_spec(T) || T <- Tools].

%% @doc 获取所有工具的 tool schema（默认 OpenAI 格式）
-spec get_tool_schemas(kernel()) -> [map()].
get_tool_schemas(Kernel) ->
    get_tool_schemas(Kernel, openai).

%% @doc 获取所有工具的 tool schema（指定提供商格式）
%%
%% @param Kernel Kernel 实例
%% @param Provider 提供商标识（openai | anthropic）
%% @returns tool schema 列表
-spec get_tool_schemas(kernel(), openai | anthropic | atom()) -> [map()].
get_tool_schemas(Kernel, Provider) ->
    Tools = list_tools(Kernel),
    [beamai_tool:to_tool_schema(T, Provider) || T <- Tools].

%% @doc 获取 Kernel 的 LLM 服务配置
%%
%% 未配置 LLM 时返回 error。
-spec get_service(kernel()) -> {ok, beamai_chat_completion:config()} | error.
get_service(#{llm_config := undefined}) -> error;
get_service(#{llm_config := Config}) -> {ok, Config}.

%%====================================================================
%% 内部函数 - 工具调用循环
%%====================================================================

%% @private 工具调用循环主体
%%
%% 每次迭代通过 run_chat_pipeline 调用 LLM，确保 pre_chat/post_chat 过滤器生效。
%% LLM 返回 tool_calls 时：解析调用 → 执行工具 → 拼接结果 → 再次请求 LLM。
%% 迭代次数耗尽返回 max_tool_iterations 错误。
%% LLM 返回纯文本响应时终止循环。
tool_calling_loop(_Kernel, _LlmConfig, _Filters, _Msgs, _Opts, _Context, _SysPrompts, 0) ->
    {error, max_tool_iterations};
tool_calling_loop(Kernel, LlmConfig, Filters, Msgs, Opts, Context, SysPrompts, N) ->
    %% 每次调用 LLM 时，将 system_prompts 拼在最前面
    LlmMsgs = SysPrompts ++ Msgs,
    case run_chat_pipeline(LlmConfig, Filters, LlmMsgs, Opts, Context) of
        {ok, Response, Ctx0} ->
            %% 使用 llm_response 访问器统一处理响应
            case llm_response:has_tool_calls(Response) of
                true ->
                    TCs = llm_response:tool_calls(Response),
                    AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
                    Ctx1 = track_message(Ctx0, AssistantMsg),
                    {ToolResults, Ctx2} = execute_tool_calls(Kernel, TCs, Ctx1),
                    NewMsgs = Msgs ++ [AssistantMsg | ToolResults],
                    tool_calling_loop(Kernel, LlmConfig, Filters, NewMsgs, Opts, Ctx2, SysPrompts, N - 1);
                false ->
                    Content = llm_response:content(Response),
                    case Content of
                        null ->
                            {ok, Response, Ctx0};
                        _ ->
                            FinalMsg = #{role => assistant, content => Content},
                            FinalCtx = track_message(Ctx0, FinalMsg),
                            {ok, Response, FinalCtx}
                    end
            end;
        {error, _} = Err ->
            Err
    end.

%% @private 批量执行 tool_calls 列表
%%
%% 逐个解析 tool_call 结构并调用对应工具，
%% 将结果编码为 tool 角色消息并累积返回。
execute_tool_calls(Kernel, ToolCalls, Context) ->
    lists:foldl(fun(TC, {ResultsAcc, CtxAcc}) ->
        {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
        {ResultContent, NewCtx} = case invoke_tool(Kernel, Name, Args, CtxAcc) of
            {ok, Value, UpdatedCtx} -> {beamai_tool:encode_result(Value), UpdatedCtx};
            {error, Reason} -> {beamai_tool:encode_result(#{error => Reason}), CtxAcc}
        end,
        Msg = #{role => tool, tool_call_id => Id, name => Name, content => ResultContent},
        Ctx2 = track_message(NewCtx, Msg),
        {ResultsAcc ++ [Msg], Ctx2}
    end, {[], Context}, ToolCalls).

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 同时追加消息到 messages 和 history
track_message(Context, Msg) ->
    Ctx1 = beamai_context:append_message(Context, Msg),
    beamai_context:add_history(Ctx1, Msg).

%% @private 追踪记录新输入消息到 messages 和 history
track_new_messages(Context, Messages) ->
    lists:foldl(fun(Msg, Ctx) ->
        track_message(Ctx, Msg)
    end, Context, Messages).

%% @private 执行调用管道：前置过滤 → 工具执行 → 后置过滤
run_invoke_pipeline(Filters, ToolSpec, Args, Context) ->
    case beamai_filter:apply_pre_filters(Filters, ToolSpec, Args, Context) of
        {ok, FilteredArgs, FilteredCtx} ->
            execute_and_post_filter(Filters, ToolSpec, FilteredArgs, FilteredCtx);
        {skip, Value} ->
            {ok, Value, Context};
        {error, _} = Err ->
            Err
    end.

%% @private 执行工具并应用后置过滤器
execute_and_post_filter(Filters, ToolSpec, Args, Context) ->
    case beamai_tool:invoke(ToolSpec, Args, Context) of
        {ok, Value} ->
            beamai_filter:apply_post_filters(Filters, ToolSpec, Value, Context);
        {ok, Value, NewCtx} ->
            beamai_filter:apply_post_filters(Filters, ToolSpec, Value, NewCtx);
        {error, _} = Err ->
            Err
    end.

%% @private 执行 Chat 管道：前置过滤 → LLM 调用 → 后置过滤
run_chat_pipeline(LlmConfig, Filters, Messages, Opts, Context) ->
    case beamai_filter:apply_pre_chat_filters(Filters, Messages, Context) of
        {ok, FilteredMsgs, FilteredCtx} ->
            execute_llm_and_post_filter(LlmConfig, Filters, FilteredMsgs, Opts, FilteredCtx);
        {error, _} = Err ->
            Err
    end.

%% @private 执行 LLM 请求并应用后置过滤器
execute_llm_and_post_filter(LlmConfig, Filters, Messages, Opts, Context) ->
    case beamai_chat_completion:chat(LlmConfig, Messages, Opts) of
        {ok, Response} ->
            case beamai_filter:apply_post_chat_filters(Filters, Response, Context) of
                {ok, FinalResp, FinalCtx} -> {ok, FinalResp, FinalCtx};
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.
