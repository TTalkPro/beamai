%%%-------------------------------------------------------------------
%%% @doc ChatClient 对话示例
%%%
%%% 演示 beamai ChatClient 的多种对话模式：
%%%   - run/0: 单轮对话（使用预建配置）
%%%   - run_inline/0: 单轮对话（使用 beamai:add_chat_model/3 内联配置）
%%%   - multi_turn/0: 多轮对话
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_chat_client_chat:run().
%%% example_chat_client_chat:run_inline().
%%% example_chat_client_chat:multi_turn().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_chat_client_chat).

-export([run/0, run/1, run_inline/0, multi_turn/0, multi_turn/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 运行单轮对话示例（使用 GLM-4.7 via Anthropic provider）
-spec run() -> ok.
run() ->
    LLMConfig = example_llm_config:anthropic(),
    run(LLMConfig).

%% @doc 运行单轮对话示例（使用指定 LLM 配置）
-spec run(beamai_chat_model:config()) -> ok.
run(LLMConfig) ->
    io:format("=== BeamAI ChatClient Chat Example ===~n~n"),

    %% 1. 构建 ChatClient，添加 Chat Completion 服务
    K0 = beamai:chat_client(),
    K1 = beamai:add_chat_model(K0, LLMConfig),

    %% 2. 创建对话历史（消息列表）
    History0 = [
        #{role => system, content => <<"你是一个有帮助的助手。"/utf8>>}
    ],

    %% 3. 添加用户消息
    UserMsg = <<"你好，请介绍一下自己"/utf8>>,
    History1 = History0 ++ [#{role => user, content => UserMsg}],
    io:format("User: ~ts~n~n", [UserMsg]),

    %% 4. 调用 ChatClient 获取回复
    case beamai:chat(K1, History1) of
        {ok, #{content := Content}, _} ->
            io:format("Assistant: ~ts~n~n", [Content]),

            %% 5. 将回复加入历史
            _History2 = History1 ++ [#{role => assistant, content => Content}],
            io:format("(Chat history now has ~B messages)~n", [3]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%% @doc 使用 beamai:add_chat_model/3 内联构建 ChatClient（无需预建配置）
%%
%% 展示最简洁的方式：直接传入 provider 和选项。
-spec run_inline() -> ok.
run_inline() ->
    io:format("=== BeamAI Inline LLM Config Example ===~n~n"),

    %% 直接用 beamai:add_chat_model/3 传入 provider + 选项
    ApiKey = list_to_binary(os:getenv("ZHIPU_API_KEY")),
    ChatClient = beamai:add_chat_model(beamai:chat_client(), anthropic, #{
        api_key => ApiKey,
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
        model => <<"glm-4.7">>,
        max_tokens => 1024
    }),

    Messages = [
        #{role => system, content => <<"用一句话回答。"/utf8>>},
        #{role => user, content => <<"Erlang OTP 是什么？"/utf8>>}
    ],
    io:format("User: Erlang OTP 是什么？~n~n"),

    case beamai:chat(ChatClient, Messages) of
        {ok, #{content := Content}, _} ->
            io:format("Assistant: ~ts~n~n", [Content]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%% @doc 运行多轮对话示例（使用 GLM-4.7 via Anthropic provider）
-spec multi_turn() -> ok.
multi_turn() ->
    LLMConfig = example_llm_config:anthropic(),
    multi_turn(LLMConfig).

%% @doc 运行多轮对话示例
%%
%% 演示如何维护对话历史进行多轮交互。
-spec multi_turn(beamai_chat_model:config()) -> ok.
multi_turn(LLMConfig) ->
    io:format("=== BeamAI ChatClient Multi-Turn Chat ===~n~n"),

    %% 1. 构建 ChatClient
    K0 = beamai:chat_client(),
    K1 = beamai:add_chat_model(K0, LLMConfig),

    %% 2. 初始化对话历史
    History0 = [
        #{role => system, content => <<"你是一个有帮助的助手。请简洁回答。"/utf8>>}
    ],

    %% 3. 多轮对话
    Questions = [
        <<"Erlang 是什么？"/utf8>>,
        <<"它的主要优势是什么？"/utf8>>,
        <<"给我一个简单的代码示例"/utf8>>
    ],

    chat_loop(K1, History0, Questions, 1).

%%====================================================================
%% 内部函数
%%====================================================================

-spec chat_loop(beamai_chat_client:chat_client(), [map()], [binary()], pos_integer()) -> ok.
chat_loop(_ChatClient, _History, [], _Turn) ->
    io:format("~n=== Conversation ended ===~n"),
    ok;
chat_loop(ChatClient, History, [Question | Rest], Turn) ->
    io:format("--- Turn ~B ---~n", [Turn]),
    io:format("User: ~ts~n~n", [Question]),

    History1 = History ++ [#{role => user, content => Question}],

    case beamai:chat(ChatClient, History1) of
        {ok, #{content := Content}, _} ->
            io:format("Assistant: ~ts~n~n", [Content]),
            History2 = History1 ++ [#{role => assistant, content => Content}],
            chat_loop(ChatClient, History2, Rest, Turn + 1);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.
