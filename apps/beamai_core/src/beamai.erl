%%%-------------------------------------------------------------------
%%% @doc Facade 入口：所有外部调用的统一入口
%%%
%%% 提供简洁的顶层 API，涵盖：
%%% - 构建 Kernel（插件 + LLM 服务）
%%% - 调用函数和 Chat Completion
%%% - 工具调用循环（LLM + 函数执行）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai).

%% Kernel
-export([kernel/0, kernel/1]).

%% Plugin
-export([plugin/2, plugin/3]).
-export([add_plugin/2, add_plugin/3]).
-export([add_plugin_module/2]).

%% Function
-export([function/2, function/3]).

%% Service (LLM)
-export([add_llm/3, add_llm/2]).

%% Filter
-export([add_filter/2, add_filter/4]).

%% Invoke
-export([invoke/3, invoke/4]).
-export([chat/2, chat/3]).
-export([chat_with_tools/2, chat_with_tools/3]).

%% Prompt
-export([render/2]).

%% Query
-export([functions/1]).
-export([tools/1, tools/2]).

%% Context
-export([context/0, context/1]).

%%====================================================================
%% Kernel
%%====================================================================

%% @doc 创建空 Kernel（默认配置）
-spec kernel() -> beamai_kernel:kernel().
kernel() ->
    beamai_kernel:new().

%% @doc 创建 Kernel（自定义配置）
%%
%% @param Settings 配置项（如 #{max_tool_iterations => 5}）
-spec kernel(beamai_kernel:kernel_settings()) -> beamai_kernel:kernel().
kernel(Settings) ->
    beamai_kernel:new(Settings).

%%====================================================================
%% Plugin
%%====================================================================

%% @doc 创建插件（名称 + 函数列表）
-spec plugin(binary(), [beamai_function:function_def()]) -> beamai_plugin:plugin().
plugin(Name, Functions) ->
    beamai_plugin:new(Name, Functions).

%% @doc 创建插件（带额外选项，如 description）
-spec plugin(binary(), [beamai_function:function_def()], map()) -> beamai_plugin:plugin().
plugin(Name, Functions, Opts) ->
    beamai_plugin:new(Name, Functions, Opts).

%% @doc 注册已构建的插件到 Kernel
-spec add_plugin(beamai_kernel:kernel(), beamai_plugin:plugin()) -> beamai_kernel:kernel().
add_plugin(Kernel, Plugin) ->
    beamai_kernel:add_plugin(Kernel, Plugin).

%% @doc 快捷注册插件（从名称和函数列表自动构建并注册）
-spec add_plugin(beamai_kernel:kernel(), binary(), [beamai_function:function_def()]) -> beamai_kernel:kernel().
add_plugin(Kernel, Name, Functions) ->
    beamai_kernel:add_plugin(Kernel, Name, Functions).

%% @doc 从模块自动加载并注册插件
%%
%% 模块需实现 plugin_info/0 和 functions/0 回调。
-spec add_plugin_module(beamai_kernel:kernel(), module()) -> beamai_kernel:kernel().
add_plugin_module(Kernel, Module) ->
    beamai_kernel:add_plugin_from_module(Kernel, Module).

%%====================================================================
%% Function
%%====================================================================

%% @doc 创建函数定义（名称 + 处理器）
-spec function(binary(), beamai_function:handler()) -> beamai_function:function_def().
function(Name, Handler) ->
    beamai_function:new(Name, Handler).

%% @doc 创建函数定义（带额外选项，如 description、parameters）
-spec function(binary(), beamai_function:handler(), map()) -> beamai_function:function_def().
function(Name, Handler, Opts) ->
    beamai_function:new(Name, Handler, Opts).

%%====================================================================
%% Service (LLM)
%%====================================================================

%% @doc 通过提供商和选项添加 LLM 服务
%%
%% 自动调用 beamai_chat_completion:create/2 创建配置并注册。
%%
%% 示例:
%%   K1 = beamai:add_llm(K0, anthropic, #{
%%       model => <<"claude-sonnet-4-20250514">>,
%%       api_key => os:getenv("ANTHROPIC_API_KEY")
%%   })
-spec add_llm(beamai_kernel:kernel(), beamai_chat_completion:provider(), map()) -> beamai_kernel:kernel().
add_llm(Kernel, Provider, Opts) ->
    LlmConfig = beamai_chat_completion:create(Provider, Opts),
    beamai_kernel:add_service(Kernel, LlmConfig).

%% @doc 使用预构建的 LLM 配置添加服务
%%
%% 示例:
%%   LLM = beamai_chat_completion:create(openai, #{model => <<"gpt-4">>, api_key => Key}),
%%   K1 = beamai:add_llm(K0, LLM)
-spec add_llm(beamai_kernel:kernel(), beamai_chat_completion:config()) -> beamai_kernel:kernel().
add_llm(Kernel, LlmConfig) ->
    beamai_kernel:add_service(Kernel, LlmConfig).

%%====================================================================
%% Filter
%%====================================================================

%% @doc 注册已构建的过滤器到 Kernel
-spec add_filter(beamai_kernel:kernel(), beamai_filter:filter_def()) -> beamai_kernel:kernel().
add_filter(Kernel, Filter) ->
    beamai_kernel:add_filter(Kernel, Filter).

%% @doc 快捷创建并注册过滤器
%%
%% @param Kernel Kernel 实例
%% @param Name 过滤器名称
%% @param Type 过滤器类型（pre_invocation | post_invocation | pre_chat | post_chat）
%% @param Handler 过滤器处理函数
%% @returns 更新后的 Kernel
-spec add_filter(beamai_kernel:kernel(), binary(), beamai_filter:filter_type(),
                 fun((beamai_filter:filter_context()) -> beamai_filter:filter_result())) ->
    beamai_kernel:kernel().
add_filter(Kernel, Name, Type, Handler) ->
    Filter = beamai_filter:new(Name, Type, Handler),
    beamai_kernel:add_filter(Kernel, Filter).

%%====================================================================
%% Invoke
%%====================================================================

%% @doc 调用 Kernel 中注册的函数
%%
%% 函数名支持 <<"plugin.func">> 或 <<"func">> 格式。
-spec invoke(beamai_kernel:kernel(), binary(), beamai_function:args()) ->
    beamai_function:function_result().
invoke(Kernel, FuncName, Args) ->
    beamai_kernel:invoke(Kernel, FuncName, Args).

%% @doc 调用 Kernel 中注册的函数（带上下文）
-spec invoke(beamai_kernel:kernel(), binary(), beamai_function:args(), beamai_context:t()) ->
    beamai_function:function_result().
invoke(Kernel, FuncName, Args, Context) ->
    beamai_kernel:invoke(Kernel, FuncName, Args, Context).

%% @doc 发送 Chat Completion 请求（默认选项）
-spec chat(beamai_kernel:kernel(), [map()]) ->
    {ok, map()} | {error, term()}.
chat(Kernel, Messages) ->
    chat(Kernel, Messages, #{}).

%% @doc 发送 Chat Completion 请求（自定义选项）
%%
%% 执行前置/后置 Chat 过滤器管道。
-spec chat(beamai_kernel:kernel(), [map()], beamai_kernel:chat_opts()) ->
    {ok, map()} | {error, term()}.
chat(Kernel, Messages, Opts) ->
    beamai_kernel:invoke_chat(Kernel, Messages, Opts).

%% @doc 发送带工具调用循环的 Chat 请求（默认选项）
%%
%% 自动注册 Kernel 中所有函数为 tools，驱动 LLM ↔ Function 循环。
-spec chat_with_tools(beamai_kernel:kernel(), [map()]) ->
    {ok, map()} | {error, term()}.
chat_with_tools(Kernel, Messages) ->
    chat_with_tools(Kernel, Messages, #{}).

%% @doc 发送带工具调用循环的 Chat 请求（自定义选项）
-spec chat_with_tools(beamai_kernel:kernel(), [map()], beamai_kernel:chat_opts()) ->
    {ok, map()} | {error, term()}.
chat_with_tools(Kernel, Messages, Opts) ->
    beamai_kernel:invoke_chat_with_tools(Kernel, Messages, Opts).

%%====================================================================
%% Prompt
%%====================================================================

%% @doc 渲染提示词模板
%%
%% 将 {{variable}} 占位符替换为 Vars 中对应的值。
%%
%% @param Template 模板字符串
%% @param Vars 变量 Map
%% @returns {ok, 渲染后的二进制} | {error, 原因}
-spec render(binary(), map()) -> {ok, binary()} | {error, term()}.
render(Template, Vars) ->
    Prompt = beamai_prompt:new(Template),
    beamai_prompt:render(Prompt, Vars).

%%====================================================================
%% Query
%%====================================================================

%% @doc 列出 Kernel 中所有注册的函数定义
-spec functions(beamai_kernel:kernel()) -> [beamai_function:function_def()].
functions(Kernel) ->
    beamai_kernel:list_functions(Kernel).

%% @doc 获取所有函数的 tool schema（默认 OpenAI 格式）
-spec tools(beamai_kernel:kernel()) -> [map()].
tools(Kernel) ->
    beamai_kernel:get_tool_schemas(Kernel).

%% @doc 获取所有函数的 tool schema（指定提供商格式）
-spec tools(beamai_kernel:kernel(), openai | anthropic | atom()) -> [map()].
tools(Kernel, Provider) ->
    beamai_kernel:get_tool_schemas(Kernel, Provider).

%%====================================================================
%% Context
%%====================================================================

%% @doc 创建空执行上下文
-spec context() -> beamai_context:t().
context() ->
    beamai_context:new().

%% @doc 创建带初始变量的执行上下文
-spec context(map()) -> beamai_context:t().
context(Vars) ->
    beamai_context:new(Vars).
