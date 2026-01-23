%%%-------------------------------------------------------------------
%%% @doc 流式对话示例
%%%
%%% 演示如何使用 beamai_chat_completion:stream_chat 进行流式对话，
%%% token 逐个输出到终端。
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_streaming:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_streaming).

-export([run/0, run/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 运行流式对话示例（使用 Zhipu GLM-4.6）
-spec run() -> ok.
run() ->
    LLMConfig = example_llm_config:zhipu(),
    run(LLMConfig).

%% @doc 运行流式对话示例（使用指定 LLM 配置）
%%
%% 使用 beamai_chat_completion:stream_chat 直接进行流式调用，
%% 每收到一个 token 立即打印到终端。
-spec run(beamai_chat_completion:config()) -> ok.
run(LLMConfig) ->
    io:format("=== BeamAI Streaming Chat Example ===~n~n"),

    Messages = [
        #{role => system, content => <<"你是一个有帮助的助手。"/utf8>>},
        #{role => user, content => <<"用三句话介绍 Erlang 的并发模型。"/utf8>>}
    ],
    io:format("User: 用三句话介绍 Erlang 的并发模型。~n~n"),
    io:format("Assistant: "),

    %% 流式回调：每收到一个 token 打印到终端
    Callback = fun(Event) ->
        Token = extract_token(Event),
        case Token of
            <<>> -> ok;
            _ -> io:format("~ts", [Token])
        end
    end,

    case beamai_chat_completion:stream_chat(LLMConfig, Messages, Callback) of
        {ok, _FinalResponse} ->
            io:format("~n~n[Stream completed]~n");
        {error, Reason} ->
            io:format("~nError: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从 SSE 事件中提取 token
%% 支持 OpenAI 和 Anthropic 两种流式格式
extract_token(#{<<"choices">> := [#{<<"delta">> := #{<<"content">> := Content}} | _]})
  when is_binary(Content) ->
    Content;
extract_token(#{<<"delta">> := #{<<"text">> := Text}}) when is_binary(Text) ->
    Text;
extract_token(#{<<"type">> := <<"content_block_delta">>,
                <<"delta">> := #{<<"text">> := Text}}) when is_binary(Text) ->
    Text;
extract_token(_) ->
    <<>>.
