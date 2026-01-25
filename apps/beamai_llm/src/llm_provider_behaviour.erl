%%%-------------------------------------------------------------------
%%% @doc LLM Provider 行为定义模块
%%%
%%% 定义 LLM（大语言模型）Provider 的统一接口，支持多种 LLM 服务商。
%%% 所有 Provider 实现必须遵循此 Behaviour。
%%%
%%% == 支持的 Provider ==
%%%
%%%   - OpenAI (GPT-4, GPT-4o, GPT-3.5)
%%%   - Anthropic (Claude 3.5, Claude 3)
%%%   - Ollama (本地模型，如 Llama3, Qwen)
%%%   - Zhipu (GLM-4.7, GLM-4 系列)
%%%   - DeepSeek (deepseek-chat, deepseek-reasoner)
%%%   - Bailian/DashScope (通义千问系列)
%%%
%%% == 设计原则 ==
%%%
%%% 1. 统一接口：所有 Provider 使用相同的消息格式和调用方式
%%% 2. 配置灵活：支持自定义 base_url、endpoint，便于代理兼容
%%% 3. 功能发现：通过 supports_tools/streaming 声明能力
%%% 4. 错误统一：所有错误以 {error, term()} 格式返回
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 获取 Provider 模块
%%% Module = llm_provider_openai,
%%%
%%% %% 验证配置
%%% ok = Module:validate_config(Config),
%%%
%%% %% 发送聊天请求
%%% Request = #{messages => [#{role => user, content => <<"Hello">>}]},
%%% {ok, Response} = Module:chat(Config, Request),
%%%
%%% %% 流式聊天
%%% Callback = fun(Event) -> io:format("~p~n", [Event]) end,
%%% {ok, FinalResponse} = Module:stream_chat(Config, Request, Callback).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_provider_behaviour).

%% 类型导出
-export_type([
    provider/0,
    config/0,
    message/0,
    tool/0,
    tool_call/0,
    chat_request/0,
    chat_response/0,
    usage/0
]).

%%====================================================================
%% 类型定义
%%====================================================================

%% Provider 标识
-type provider() :: openai | anthropic | ollama | zhipu | {custom, module()}.

%% Provider 配置
-type config() :: #{
    provider := provider(),
    api_key := binary(),
    base_url => binary(),
    model => binary(),
    timeout => pos_integer(),
    max_tokens => pos_integer(),
    temperature => float(),
    extra => map()
}.

%% 消息格式（统一格式）
-type role() :: system | user | assistant | tool.
-type message() :: #{
    role := role(),
    content := binary() | null,
    name => binary(),
    tool_calls => [tool_call()],
    tool_call_id => binary()
}.

%% 工具定义
-type tool() :: #{
    name := binary(),
    description := binary(),
    parameters := map()
}.

%% 工具调用
-type tool_call() :: #{
    id := binary(),
    name := binary(),
    arguments := binary() | map()
}.

%% 聊天请求
-type chat_request() :: #{
    messages := [message()],
    tools => [tool()],
    tool_choice => auto | none | required | binary(),
    stream => boolean(),
    extra => map()
}.

%% 聊天响应
-type chat_response() :: #{
    id := binary(),
    model := binary(),
    content := binary() | null,
    tool_calls => [tool_call()],
    finish_reason := binary(),
    usage := usage()
}.

%% Token 使用统计
-type usage() :: #{
    prompt_tokens := non_neg_integer(),
    completion_tokens := non_neg_integer(),
    total_tokens := non_neg_integer()
}.

%%====================================================================
%% 回调函数定义
%%====================================================================

%% @doc 获取 Provider 显示名称
%%
%% 返回用于日志和界面显示的 Provider 名称。
%%
%% @returns Provider 名称（binary）
-callback name() -> binary().

%% @doc 获取默认配置
%%
%% 返回 Provider 的默认配置参数，如默认模型、超时时间等。
%% 用于合并用户配置时的缺省值。
%%
%% @returns 默认配置 map
-callback default_config() -> map().

%% @doc 验证配置有效性
%%
%% 检查配置是否包含必需参数（如 api_key）。
%% 应在发送请求前调用。
%%
%% @param Config Provider 配置
%% @returns ok | {error, Reason}
-callback validate_config(config()) -> ok | {error, term()}.

%% @doc 发送聊天请求（同步）
%%
%% 发送对话请求并等待完整响应。
%% 适用于简单查询或不需要流式输出的场景。
%%
%% @param Config Provider 配置
%% @param Request 聊天请求，包含 messages、tools 等
%% @returns {ok, Response} | {error, Reason}
-callback chat(config(), chat_request()) ->
    {ok, chat_response()} | {error, term()}.

%% @doc 发送流式聊天请求
%%
%% 发送对话请求，通过回调函数实时接收响应片段。
%% 适用于需要实时显示输出的场景。
%%
%% @param Config Provider 配置
%% @param Request 聊天请求
%% @param Callback 事件回调函数，接收每个响应片段
%% @returns {ok, FinalResponse} | {error, Reason}
-callback stream_chat(config(), chat_request(), fun((term()) -> ok)) ->
    {ok, chat_response()} | {error, term()}.

%% @doc 是否支持工具调用（Function Calling）
%%
%% 返回 Provider 是否支持工具/函数调用功能。
%% 调用方应据此决定是否在请求中包含工具定义。
%%
%% @returns true | false
-callback supports_tools() -> boolean().

%% @doc 是否支持流式输出
%%
%% 返回 Provider 是否支持流式（SSE）输出。
%% 若不支持，调用 stream_chat/3 将回退到同步模式。
%%
%% @returns true | false
-callback supports_streaming() -> boolean().

%%====================================================================
%% 可选回调
%%====================================================================

%% stream_chat/3 为可选回调
%% 部分 Provider 可能不支持流式输出
-optional_callbacks([
    stream_chat/3
]).
