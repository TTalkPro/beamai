%%%-------------------------------------------------------------------
%%% @doc Kernel 核心：插件管理、LLM 服务、过滤器、工具调用循环
%%%
%%% Kernel 是框架的中枢，负责：
%%% - 管理插件及其函数注册
%%% - 持有 LLM 服务配置
%%% - 执行前置/后置过滤器管道
%%% - 驱动工具调用循环（LLM ↔ Function）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_kernel).

%% Build API
-export([new/0, new/1]).
-export([add_plugin/2, add_plugin/3]).
-export([add_plugin_from_module/2]).
-export([add_service/2]).
-export([add_filter/2]).

%% Invoke API
-export([invoke/3]).
-export([invoke_tool/4]).
-export([invoke_chat/3]).

%% Query API
-export([get_function/2]).
-export([list_functions/1]).
-export([get_tool_specs/1]).
-export([get_tool_schemas/1, get_tool_schemas/2]).
-export([get_service/1]).

%% Types
-export_type([kernel/0, kernel_settings/0, chat_opts/0]).

-type kernel() :: #{
    '__kernel__' := true,
    plugins := #{binary() => beamai_plugin:plugin()},
    llm_config := beamai_chat_completion:config() | undefined,
    filters := [beamai_filter:filter_def()],
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
        plugins => #{},
        llm_config => undefined,
        filters => [],
        settings => Settings
    }.

%% @doc 注册已构建的插件到 Kernel
%%
%% 插件以其名称为键存入 plugins Map。重名插件会被覆盖。
%%
%% @param Kernel Kernel 实例
%% @param Plugin 插件 Map（需包含 name 字段）
%% @returns 更新后的 Kernel
-spec add_plugin(kernel(), beamai_plugin:plugin()) -> kernel().
add_plugin(#{plugins := Plugins} = Kernel, #{name := Name} = Plugin) ->
    Kernel#{plugins => Plugins#{Name => Plugin}}.

%% @doc 快捷注册插件（从名称和函数列表自动构建）
%%
%% @param Kernel Kernel 实例
%% @param Name 插件名称
%% @param Functions 函数定义列表
%% @returns 更新后的 Kernel
-spec add_plugin(kernel(), binary(), [beamai_function:function_def()]) -> kernel().
add_plugin(Kernel, Name, Functions) ->
    Plugin = beamai_plugin:new(Name, Functions),
    add_plugin(Kernel, Plugin).

%% @doc 从模块自动加载并注册插件
%%
%% 模块需实现 plugin_info/0 和 functions/0 回调。
%% 加载失败时抛出 {plugin_load_failed, Module, Reason} 错误。
%%
%% @param Kernel Kernel 实例
%% @param Module 实现了插件回调的模块
%% @returns 更新后的 Kernel
-spec add_plugin_from_module(kernel(), module()) -> kernel().
add_plugin_from_module(Kernel, Module) ->
    case beamai_plugin:from_module(Module) of
        {ok, Plugin} -> add_plugin(Kernel, Plugin);
        {error, Reason} -> erlang:error({plugin_load_failed, Module, Reason})
    end.

%% @doc 设置 LLM 服务配置
%%
%% 配置通过 beamai_chat_completion:create/2 创建。
%% 设置后可使用 invoke_chat/3 和 invoke_chat_with_tools/3。
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
-spec add_filter(kernel(), beamai_filter:filter_def()) -> kernel().
add_filter(#{filters := Filters} = Kernel, Filter) ->
    Kernel#{filters => Filters ++ [Filter]}.

%%====================================================================
%% Invoke API
%%====================================================================

%% @doc 调用 Kernel 中注册的工具函数
%%
%% 执行流程：查找函数 → 前置过滤器 → 函数执行 → 后置过滤器。
%% 上下文会自动关联当前 Kernel 引用。
%% 函数名支持全限定格式 <<"plugin.func">> 或短名 <<"func">>。
%%
%% @param Kernel Kernel 实例
%% @param FuncName 函数名称
%% @param Args 调用参数
%% @param Context 执行上下文
%% @returns {ok, 结果, 更新后上下文} | {error, 原因}
-spec invoke_tool(kernel(), binary(), beamai_function:args(), beamai_context:t()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
invoke_tool(#{filters := Filters} = Kernel, FuncName, Args, Context0) ->
    case get_function(Kernel, FuncName) of
        {ok, FuncDef} ->
            Context = beamai_context:with_kernel(Context0, Kernel),
            run_invoke_pipeline(Filters, FuncDef, Args, Context);
        error ->
            {error, {function_not_found, FuncName}}
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
%% 自动将 Kernel 中所有注册函数转为 tool specs 传给 LLM。
%% LLM 返回 tool_calls 时自动执行对应函数，将结果拼入消息后再次请求 LLM，
%% 循环直到 LLM 返回文本响应或达到最大迭代次数。
%%
%% @param Kernel Kernel 实例（需注册函数和 LLM 服务）
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
            MaxIter = maps:get(max_tool_iterations, Opts,
                maps:get(max_tool_iterations, maps:get(settings, Kernel, #{}), 10)),
            tool_calling_loop(Kernel, LlmConfig, ConvMsgs, ChatOpts, Context, SystemPrompts, MaxIter);
        error ->
            {error, no_llm_service}
    end.

%%====================================================================
%% Query API
%%====================================================================

%% @doc 按名称查找 Kernel 中注册的函数
%%
%% 支持两种格式：
%% - <<"plugin.func">>：从指定插件查找
%% - <<"func">>：遍历所有插件查找
%%
%% @param Kernel Kernel 实例
%% @param FuncName 函数名称
%% @returns {ok, 函数定义} | error
-spec get_function(kernel(), binary()) -> {ok, beamai_function:function_def()} | error.
get_function(#{plugins := Plugins}, FuncName) ->
    case binary:split(FuncName, <<".">>) of
        [PluginName, LocalName] ->
            case maps:find(PluginName, Plugins) of
                {ok, Plugin} -> beamai_plugin:get_function(Plugin, LocalName);
                error -> error
            end;
        [_Name] ->
            find_in_all_plugins(Plugins, FuncName)
    end.

%% @doc 列出 Kernel 中所有注册的函数
%%
%% 遍历所有插件，汇总函数定义列表。
-spec list_functions(kernel()) -> [beamai_function:function_def()].
list_functions(#{plugins := Plugins}) ->
    maps:fold(fun(_Name, Plugin, Acc) ->
        Acc ++ beamai_plugin:list_functions(Plugin)
    end, [], Plugins).

%% @doc 获取所有函数的统一 tool spec 列表
%%
%% 返回包含 name、description、parameters 的中间格式。
-spec get_tool_specs(kernel()) -> [map()].
get_tool_specs(Kernel) ->
    Functions = list_functions(Kernel),
    [beamai_function:to_tool_spec(F) || F <- Functions].

%% @doc 获取所有函数的 tool schema（默认 OpenAI 格式）
-spec get_tool_schemas(kernel()) -> [map()].
get_tool_schemas(Kernel) ->
    get_tool_schemas(Kernel, openai).

%% @doc 获取所有函数的 tool schema（指定提供商格式）
%%
%% @param Kernel Kernel 实例
%% @param Provider 提供商标识（openai | anthropic）
%% @returns tool schema 列表
-spec get_tool_schemas(kernel(), openai | anthropic | atom()) -> [map()].
get_tool_schemas(Kernel, Provider) ->
    Functions = list_functions(Kernel),
    [beamai_function:to_tool_schema(F, Provider) || F <- Functions].

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
%% LLM 返回 tool_calls 时：解析调用 → 执行函数 → 拼接结果 → 再次请求 LLM。
%% 迭代次数耗尽返回 max_tool_iterations 错误。
%% LLM 返回纯文本响应时终止循环。
tool_calling_loop(_Kernel, _LlmConfig, _Msgs, _Opts, _Context, _SysPrompts, 0) ->
    {error, max_tool_iterations};
tool_calling_loop(Kernel, LlmConfig, Msgs, Opts, Context, SysPrompts, N) ->
    %% 每次调用 LLM 时，将 system_prompts 拼在最前面
    LlmMsgs = SysPrompts ++ Msgs,
    case beamai_chat_completion:chat(LlmConfig, LlmMsgs, Opts) of
        {ok, #{tool_calls := TCs} = _Response} when is_list(TCs), TCs =/= [] ->
            AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
            Ctx1 = track_message(Context, AssistantMsg),
            {ToolResults, Ctx2} = execute_tool_calls(Kernel, TCs, Ctx1),
            NewMsgs = Msgs ++ [AssistantMsg | ToolResults],
            tool_calling_loop(Kernel, LlmConfig, NewMsgs, Opts, Ctx2, SysPrompts, N - 1);
        {ok, #{content := Content} = Response} ->
            FinalMsg = #{role => assistant, content => Content},
            FinalCtx = track_message(Context, FinalMsg),
            {ok, Response, FinalCtx};
        {ok, Response} ->
            {ok, Response, Context};
        {error, _} = Err ->
            Err
    end.

%% @private 批量执行 tool_calls 列表
%%
%% 逐个解析 tool_call 结构并调用对应函数，
%% 将结果编码为 tool 角色消息并累积返回。
execute_tool_calls(Kernel, ToolCalls, Context) ->
    lists:foldl(fun(TC, {ResultsAcc, CtxAcc}) ->
        {Id, Name, Args} = beamai_function:parse_tool_call(TC),
        {ResultContent, NewCtx} = case invoke_tool(Kernel, Name, Args, CtxAcc) of
            {ok, Value, UpdatedCtx} -> {beamai_function:encode_result(Value), UpdatedCtx};
            {error, Reason} -> {beamai_function:encode_result(#{error => Reason}), CtxAcc}
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

%% @private 在所有插件中查找函数（短名查找）
%%
%% 当函数名不含 "." 时调用，依次在每个插件中查找。
%% 若多个插件包含同名函数，返回第一个找到的。
find_in_all_plugins(Plugins, FuncName) ->
    Results = maps:fold(fun(_PName, Plugin, Acc) ->
        case beamai_plugin:get_function(Plugin, FuncName) of
            {ok, F} -> [F | Acc];
            error -> Acc
        end
    end, [], Plugins),
    case Results of
        [Found | _] -> {ok, Found};
        [] -> error
    end.

%% @private 执行调用管道：前置过滤 → 函数执行 → 后置过滤
run_invoke_pipeline(Filters, FuncDef, Args, Context) ->
    case beamai_filter:apply_pre_filters(Filters, FuncDef, Args, Context) of
        {ok, FilteredArgs, FilteredCtx} ->
            execute_and_post_filter(Filters, FuncDef, FilteredArgs, FilteredCtx);
        {skip, Value} ->
            {ok, Value, Context};
        {error, _} = Err ->
            Err
    end.

%% @private 执行函数并应用后置过滤器
execute_and_post_filter(Filters, FuncDef, Args, Context) ->
    case beamai_function:invoke(FuncDef, Args, Context) of
        {ok, Value} ->
            beamai_filter:apply_post_filters(Filters, FuncDef, Value, Context);
        {ok, Value, NewCtx} ->
            beamai_filter:apply_post_filters(Filters, FuncDef, Value, NewCtx);
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
