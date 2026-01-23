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
%% Callbacks
%%====================================================================

%% @doc Create LLM config for a given provider
-callback create(Provider :: provider(), Opts :: map()) -> config().

%% @doc Send chat completion request
-callback chat(Config :: config(), Messages :: [map()]) ->
    {ok, map()} | {error, term()}.

%% @doc Send chat completion request with options
-callback chat(Config :: config(), Messages :: [map()], Opts :: map()) ->
    {ok, map()} | {error, term()}.

%% @doc Send streaming chat request
-callback stream_chat(Config :: config(), Messages :: [map()], Callback :: fun()) ->
    {ok, map()} | {error, term()}.

-optional_callbacks([stream_chat/3]).
