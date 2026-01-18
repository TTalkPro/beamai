%%%-------------------------------------------------------------------
%%% @doc LLM Provider 行为定义
%%%
%%% 定义 LLM Provider 的统一接口，支持多种 LLM 服务商。
%%% 所有 Provider 实现必须遵循此 Behaviour。
%%%
%%% 支持的 Provider：
%%%   - OpenAI (GPT-4, GPT-3.5)
%%%   - Anthropic (Claude)
%%%   - Ollama (本地模型)
%%%   - Zhipu (GLM-4 系列)
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

%%  获取 Provider 名称
-callback name() -> binary().

%%  获取默认配置
-callback default_config() -> map().

%%  验证配置有效性
-callback validate_config(config()) -> ok | {error, term()}.

%%  发送聊天请求
-callback chat(config(), chat_request()) ->
    {ok, chat_response()} | {error, term()}.

%%  发送流式聊天请求
-callback stream_chat(config(), chat_request(), fun((term()) -> ok)) ->
    {ok, chat_response()} | {error, term()}.

%%  是否支持工具调用
-callback supports_tools() -> boolean().

%%  是否支持流式输出
-callback supports_streaming() -> boolean().

%%====================================================================
%% 可选回调
%%====================================================================

-optional_callbacks([
    stream_chat/3
]).
