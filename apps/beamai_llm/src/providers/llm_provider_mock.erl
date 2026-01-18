%%%-------------------------------------------------------------------
%%% @doc Mock LLM Provider 实现
%%%
%%% 用于测试的 Mock Provider，不进行实际 API 调用。
%%% 返回预设的响应或简单的 echo 响应。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_provider_mock).
-behaviour(llm_provider_behaviour).

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"Mock">>.

default_config() ->
    #{
        model => <<"mock-model">>,
        timeout => 5000,
        max_tokens => 1024
    }.

validate_config(_Config) ->
    ok.

%% @doc Mock chat - 返回简单响应
chat(_Messages, _Config) ->
    {ok, #{
        content => <<"This is a mock response.">>,
        role => assistant,
        finish_reason => stop,
        usage => #{
            prompt_tokens => 10,
            completion_tokens => 5,
            total_tokens => 15
        }
    }}.

%% @doc Mock stream chat - 不支持流式
stream_chat(_Messages, _Config, _Callback) ->
    {error, not_supported}.

supports_tools() -> false.
supports_streaming() -> false.
