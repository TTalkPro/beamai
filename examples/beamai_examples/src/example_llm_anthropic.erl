%%%-------------------------------------------------------------------
%%% @doc 智谱 Anthropic 兼容 API 示例
%%%
%%% 演示如何使用智谱的 Anthropic 兼容 API 创建 Agent。
%%%
%%% 使用方法:
%%% ```
%%% %% 设置环境变量
%%% export ZHIPU_API_KEY=your-api-key
%%%
%%% %% 运行示例
%%% rebar3 shell
%%% zhipu_anthropic_example:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_llm_anthropic).

-export([run/0, run/1]).
-export([chat/1, chat/2]).
-export([calculator/0, calculator/1]).

%%====================================================================
%% 配置（使用 example_utils 获取公共配置）
%%====================================================================

%%====================================================================
%% 公共 API
%%====================================================================

%% @doc 运行示例（从环境变量获取 API Key）
-spec run() -> ok | {error, term()}.
run() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            io:format("Error: ZHIPU_API_KEY not set~n"),
            io:format("Please run: export ZHIPU_API_KEY=your-api-key~n"),
            {error, no_api_key};
        ApiKey ->
            run(list_to_binary(ApiKey))
    end.

%% @doc 运行示例（使用指定 API Key）
-spec run(binary()) -> ok.
run(ApiKey) ->
    io:format("~n=== Zhipu Anthropic Compatible API Example ===~n~n"),

    %% 1. 简单对话
    io:format("1. Simple Chat~n"),
    io:format("---~n"),
    chat_example(ApiKey),

    %% 2. 计算器 Agent
    io:format("~n2. Calculator Agent~n"),
    io:format("---~n"),
    calculator_example(ApiKey),

    io:format("~n=== Examples completed ===~n"),
    ok.

%%====================================================================
%% 简单对话
%%====================================================================

%% @doc 简单对话（从环境变量获取 API Key）
-spec chat(binary()) -> {ok, binary()} | {error, term()}.
chat(Message) ->
    case os:getenv("ZHIPU_API_KEY") of
        false -> {error, no_api_key};
        ApiKey -> chat(Message, list_to_binary(ApiKey))
    end.

%% @doc 简单对话（使用指定 API Key）
-spec chat(binary(), binary()) -> {ok, binary()} | {error, term()}.
chat(Message, ApiKey) ->
    Config = get_llm_config(ApiKey),
    Messages = [#{role => user, content => Message}],

    case llm_client:chat(Config, Messages) of
        {ok, #{content := Content}} ->
            {ok, Content};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
chat_example(ApiKey) ->
    Message = <<"你为我介绍下你是谁？能做什么？"/utf8>>,
    io:format("User: ~ts~n", [Message]),

    case chat(Message, ApiKey) of
        {ok, Response} ->
            io:format("Assistant: ~ts~n", [Response]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

%%====================================================================
%% 计算器 Agent
%%====================================================================

%% @doc 创建计算器 Agent（从环境变量获取 API Key）
-spec calculator() -> {ok, map()} | {error, term()}.
calculator() ->
    case os:getenv("ZHIPU_API_KEY") of
        false -> {error, no_api_key};
        ApiKey -> calculator(list_to_binary(ApiKey))
    end.

%% @doc 创建并运行计算器 Agent
-spec calculator(binary()) -> {ok, map()} | {error, term()}.
calculator(ApiKey) ->
    Tools = [
        #{
            name => <<"calculate">>,
            description => <<"Perform mathematical calculations. Supports +, -, *, / operations.">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"expression">> => #{
                        type => string,
                        description => <<"Mathematical expression to evaluate, e.g., '25 * 4 + 100'">>
                    }
                },
                required => [<<"expression">>]
            },
            handler => fun(Args) ->
                Expr = maps:get(<<"expression">>, Args, <<"0">>),
                evaluate_expression(Expr)
            end
        }
    ],

    %% 使用 run_once API（推荐的纯函数模式）
    Config = #{
        name => <<"Calculator Assistant">>,
        system_prompt => <<"You are a helpful calculator assistant. Use the calculate tool to perform mathematical operations. Always show your work.">>,
        tools => Tools,
        llm => get_llm_config(ApiKey)
    },

    beamai_agent:run_once(Config, <<"计算下 25 * 4 + 100 是多少"/utf8>>).

%% @private
calculator_example(ApiKey) ->
    case calculator(ApiKey) of
        {ok, Result} ->
            Response = maps:get(final_response, Result, <<"No response">>),
            io:format("Question: What is 25 * 4 + 100?~n"),
            io:format("Answer: ~ts~n", [Response]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 获取智谱 Anthropic 兼容 API 的 LLM 配置
-spec get_llm_config(binary()) -> map().
get_llm_config(ApiKey) ->
    example_utils:get_llm_config(ApiKey, #{}).

%% @doc 计算数学表达式
-spec evaluate_expression(binary()) -> map().
evaluate_expression(Expr) ->
    try
        %% 将表达式转换为 Erlang 可执行格式
        ExprStr = binary_to_list(<<Expr/binary, ".">>),
        {ok, Tokens, _} = erl_scan:string(ExprStr),
        {ok, Parsed} = erl_parse:parse_exprs(Tokens),
        {value, Value, _} = erl_eval:exprs(Parsed, []),
        #{result => Value, expression => Expr}
    catch
        _:Error ->
            #{error => iolist_to_binary(io_lib:format("~p", [Error])), expression => Expr}
    end.
