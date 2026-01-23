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

-spec kernel() -> beamai_kernel:kernel().
kernel() ->
    beamai_kernel:new().

-spec kernel(beamai_kernel:kernel_settings()) -> beamai_kernel:kernel().
kernel(Settings) ->
    beamai_kernel:new(Settings).

%%====================================================================
%% Plugin
%%====================================================================

-spec plugin(binary(), [beamai_function:function_def()]) -> beamai_plugin:plugin().
plugin(Name, Functions) ->
    beamai_plugin:new(Name, Functions).

-spec plugin(binary(), [beamai_function:function_def()], map()) -> beamai_plugin:plugin().
plugin(Name, Functions, Opts) ->
    beamai_plugin:new(Name, Functions, Opts).

-spec add_plugin(beamai_kernel:kernel(), beamai_plugin:plugin()) -> beamai_kernel:kernel().
add_plugin(Kernel, Plugin) ->
    beamai_kernel:add_plugin(Kernel, Plugin).

-spec add_plugin(beamai_kernel:kernel(), binary(), [beamai_function:function_def()]) -> beamai_kernel:kernel().
add_plugin(Kernel, Name, Functions) ->
    beamai_kernel:add_plugin(Kernel, Name, Functions).

-spec add_plugin_module(beamai_kernel:kernel(), module()) -> beamai_kernel:kernel().
add_plugin_module(Kernel, Module) ->
    beamai_kernel:add_plugin_from_module(Kernel, Module).

%%====================================================================
%% Function
%%====================================================================

-spec function(binary(), beamai_function:handler()) -> beamai_function:function_def().
function(Name, Handler) ->
    beamai_function:new(Name, Handler).

-spec function(binary(), beamai_function:handler(), map()) -> beamai_function:function_def().
function(Name, Handler, Opts) ->
    beamai_function:new(Name, Handler, Opts).

%%====================================================================
%% Service (LLM)
%%====================================================================

%% @doc Add LLM service by provider and options.
%%
%% Example:
%%   K1 = beamai:add_llm(K0, anthropic, #{
%%       model => <<"claude-sonnet-4-20250514">>,
%%       api_key => os:getenv("ANTHROPIC_API_KEY")
%%   })
-spec add_llm(beamai_kernel:kernel(), beamai_chat_completion:provider(), map()) -> beamai_kernel:kernel().
add_llm(Kernel, Provider, Opts) ->
    LlmConfig = beamai_chat_completion:create(Provider, Opts),
    beamai_kernel:add_service(Kernel, LlmConfig).

%% @doc Add LLM service with a pre-built config.
%%
%% Example:
%%   LLM = beamai_chat_completion:create(openai, #{model => <<"gpt-4">>, api_key => Key}),
%%   K1 = beamai:add_llm(K0, LLM)
-spec add_llm(beamai_kernel:kernel(), beamai_chat_completion:config()) -> beamai_kernel:kernel().
add_llm(Kernel, LlmConfig) ->
    beamai_kernel:add_service(Kernel, LlmConfig).

%%====================================================================
%% Filter
%%====================================================================

-spec add_filter(beamai_kernel:kernel(), beamai_filter:filter_def()) -> beamai_kernel:kernel().
add_filter(Kernel, Filter) ->
    beamai_kernel:add_filter(Kernel, Filter).

-spec add_filter(beamai_kernel:kernel(), binary(), beamai_filter:filter_type(),
                 fun((beamai_filter:filter_context()) -> beamai_filter:filter_result())) ->
    beamai_kernel:kernel().
add_filter(Kernel, Name, Type, Handler) ->
    Filter = beamai_filter:new(Name, Type, Handler),
    beamai_kernel:add_filter(Kernel, Filter).

%%====================================================================
%% Invoke
%%====================================================================

-spec invoke(beamai_kernel:kernel(), binary(), beamai_function:args()) ->
    beamai_function:function_result().
invoke(Kernel, FuncName, Args) ->
    beamai_kernel:invoke(Kernel, FuncName, Args).

-spec invoke(beamai_kernel:kernel(), binary(), beamai_function:args(), beamai_context:t()) ->
    beamai_function:function_result().
invoke(Kernel, FuncName, Args, Context) ->
    beamai_kernel:invoke(Kernel, FuncName, Args, Context).

-spec chat(beamai_kernel:kernel(), [map()]) ->
    {ok, map()} | {error, term()}.
chat(Kernel, Messages) ->
    chat(Kernel, Messages, #{}).

-spec chat(beamai_kernel:kernel(), [map()], beamai_kernel:chat_opts()) ->
    {ok, map()} | {error, term()}.
chat(Kernel, Messages, Opts) ->
    beamai_kernel:invoke_chat(Kernel, Messages, Opts).

-spec chat_with_tools(beamai_kernel:kernel(), [map()]) ->
    {ok, map()} | {error, term()}.
chat_with_tools(Kernel, Messages) ->
    chat_with_tools(Kernel, Messages, #{}).

-spec chat_with_tools(beamai_kernel:kernel(), [map()], beamai_kernel:chat_opts()) ->
    {ok, map()} | {error, term()}.
chat_with_tools(Kernel, Messages, Opts) ->
    beamai_kernel:invoke_chat_with_tools(Kernel, Messages, Opts).

%%====================================================================
%% Prompt
%%====================================================================

-spec render(binary(), map()) -> {ok, binary()} | {error, term()}.
render(Template, Vars) ->
    Prompt = beamai_prompt:new(Template),
    beamai_prompt:render(Prompt, Vars).

%%====================================================================
%% Query
%%====================================================================

-spec functions(beamai_kernel:kernel()) -> [beamai_function:function_def()].
functions(Kernel) ->
    beamai_kernel:list_functions(Kernel).

-spec tools(beamai_kernel:kernel()) -> [map()].
tools(Kernel) ->
    beamai_kernel:get_tool_schemas(Kernel).

-spec tools(beamai_kernel:kernel(), openai | anthropic | atom()) -> [map()].
tools(Kernel, Provider) ->
    beamai_kernel:get_tool_schemas(Kernel, Provider).

%%====================================================================
%% Context
%%====================================================================

-spec context() -> beamai_context:t().
context() ->
    beamai_context:new().

-spec context(map()) -> beamai_context:t().
context(Vars) ->
    beamai_context:new(Vars).
