%%%-------------------------------------------------------------------
%%% @doc LLM 行为接口定义
%%%
%%% 定义 LLM Chat Completion 的标准接口。
%%% 默认实现：beamai_chat_completion（位于 beamai_llm 应用）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_behaviour).

%% Types
-export_type([config/0, provider/0]).

%%====================================================================
%% Types
%%====================================================================

-type provider() :: openai | anthropic | ollama | zhipu | bailian | deepseek | mock | {custom, module()}.

-type config() :: #{
    provider := provider(),
    '__llm_config__' := true,
    atom() => term()
}.

%%====================================================================
%% 回调定义
%%====================================================================

%% @doc 创建指定提供商的 LLM 配置
%%
%% @param Provider 提供商标识
%% @param Opts 配置选项（如 model、api_key、base_url 等）
%% @returns LLM 配置 Map
-callback create(Provider :: provider(), Opts :: map()) -> config().

%% @doc 发送 Chat Completion 请求（默认选项）
%%
%% @param Config LLM 配置
%% @param Messages 消息列表（[#{role => ..., content => ...}]）
%% @returns {ok, 响应 Map} | {error, 原因}
-callback chat(Config :: config(), Messages :: [map()]) ->
    {ok, map()} | {error, term()}.

%% @doc 发送 Chat Completion 请求（自定义选项）
%%
%% @param Config LLM 配置
%% @param Messages 消息列表
%% @param Opts 请求选项（如 tools、tool_choice、temperature 等）
%% @returns {ok, 响应 Map} | {error, 原因}
-callback chat(Config :: config(), Messages :: [map()], Opts :: map()) ->
    {ok, map()} | {error, term()}.

%% @doc 发送流式 Chat 请求（可选实现）
%%
%% @param Config LLM 配置
%% @param Messages 消息列表
%% @param Callback 流式回调函数，接收每个数据块
%% @returns {ok, 累积结果} | {error, 原因}
-callback stream_chat(Config :: config(), Messages :: [map()], Callback :: fun()) ->
    {ok, map()} | {error, term()}.

-optional_callbacks([stream_chat/3]).
