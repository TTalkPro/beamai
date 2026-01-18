-module(example_llm_chat).
-export([simple_chat/0, chat_with_messages/0, multi_turn/0, chat_with_system_prompt/0]).

%% @doc 智谱 Anthropic 兼容 API 配置常量
-define(ZHIPU_ANTHROPIC_BASE_URL, <<"https://open.bigmodel.cn/api/anthropic">>).
-define(DEFAULT_MODEL, <<"glm-4.7">>).

%%====================================================================
%% 示例 1: 最简单的单轮对话
%%====================================================================

simple_chat() ->
    io:format("=== 示例 1: 最简单的单轮对话 ===~n~n"),

    %% 1. 创建配置（使用 Anthropic 兼容 API）
    ApiKey = list_to_binary(os:getenv("ZHIPU_API_KEY")),
    Config = llm_client:config(anthropic, #{
        api_key => ApiKey,
        base_url => ?ZHIPU_ANTHROPIC_BASE_URL,
        model => ?DEFAULT_MODEL,
        max_tokens => 2048
    }),

    %% 2. 发送消息
    Question = <<"你好，请介绍一下你自己。">>,
    io:format("提问: ~ts~n~n", [Question]),

    case llm_client:simple_chat(Config, Question) of
        {ok, Response} ->
            io:format("回答: ~ts~n~n", [Response]);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end.

%%====================================================================
%% 示例 2: 使用自定义消息列表
%%====================================================================

chat_with_messages() ->
    io:format("=== 示例 2: 使用自定义消息列表 ===~n~n"),

    ApiKey = list_to_binary(os:getenv("ZHIPU_API_KEY")),
    Config = llm_client:config(anthropic, #{
        api_key => ApiKey,
        base_url => ?ZHIPU_ANTHROPIC_BASE_URL,
        model => ?DEFAULT_MODEL,
        max_tokens => 2048,
        temperature => 0.8
    }),

    %% 构建消息列表
    Messages = [
        #{role => user, content => <<"什么是 Erlang？">>}
    ],

    io:format("消息列表: ~p~n~n", [Messages]),

    case llm_client:chat(Config, Messages) of
        {ok, #{content := Content}} ->
            io:format("回答: ~ts~n~n", [Content]);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end.

%%====================================================================
%% 示例 3: 带系统提示词的对话
%%====================================================================

chat_with_system_prompt() ->
    io:format("=== 示例 3: 带系统提示词的对话 ===~n~n"),

    ApiKey = list_to_binary(os:getenv("ZHIPU_API_KEY")),
    Config = llm_client:config(anthropic, #{
        api_key => ApiKey,
        base_url => ?ZHIPU_ANTHROPIC_BASE_URL,
        model => ?DEFAULT_MODEL,
        max_tokens => 2048
    }),

    %% 构建包含系统提示词的消息列表
    Messages = [
        #{role => system, content => <<"你是一个专业的技术顾问，请用简洁专业的语言回答问题。">>},
        #{role => user, content => <<"请解释什么是函数式编程？">>}
    ],

    io:format("系统提示词: 你是一个专业的技术顾问~n"),
    io:format("用户问题: 请解释什么是函数式编程？~n~n"),

    case llm_client:chat(Config, Messages) of
        {ok, #{content := Content}} ->
            io:format("回答: ~ts~n~n", [Content]);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end.

%%====================================================================
%% 示例 4: 多轮对话
%%====================================================================

multi_turn() ->
    io:format("=== 示例 4: 多轮对话 ===~n~n"),

    ApiKey = list_to_binary(os:getenv("ZHIPU_API_KEY")),
    Config = llm_client:config(anthropic, #{
        api_key => ApiKey,
        base_url => ?ZHIPU_ANTHROPIC_BASE_URL,
        model => ?DEFAULT_MODEL,
        max_tokens => 2048
    }),

    %% 第一轮
    io:format("=== 第一轮 ===~n"),
    Messages1 = [
        #{role => user, content => <<"我记得 Python 的创始人是谁？">>}
    ],

    case llm_client:chat(Config, Messages1) of
        {ok, #{content := Response1}} ->
            io:format("用户: 我记得 Python 的创始人是谁？~n"),
            io:format("助手: ~ts~n~n", [Response1]),

            %% 第二轮：继续对话，包含历史消息
            io:format("=== 第二轮 ===~n"),
            Messages2 = Messages1 ++ [
                #{role => assistant, content => Response1},
                #{role => user, content => <<"那他为什么创建 Python？">>}
            ],

            case llm_client:chat(Config, Messages2) of
                {ok, #{content := Response2}} ->
                    io:format("用户: 那他为什么创建 Python？~n"),
                    io:format("助手: ~ts~n~n", [Response2]);
                {error, Reason} ->
                    io:format("错误: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end.
