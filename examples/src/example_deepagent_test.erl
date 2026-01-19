%%%-------------------------------------------------------------------
%%% @doc Deep Agent 修改验证测试
%%%
%%% 验证 beamai_deepagent 的以下修改：
%%% 1. LLM 配置验证 - 必须使用 llm_client:create/2 创建
%%% 2. Middleware 支持 - 可以配置 middleware 链
%%%
%%% 使用方法:
%%% ```
%%% %% 设置环境变量
%%% export ZHIPU_API_KEY=your-api-key
%%%
%%% %% 运行测试
%%% rebar3 shell
%%% example_deepagent_test:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_deepagent_test).

-export([run/0]).
-export([test_llm_config_validation/0]).
-export([test_basic_deepagent/0]).
-export([test_deepagent_with_middleware/0]).

%%====================================================================
%% 运行所有测试
%%====================================================================

%% @doc 运行所有测试
run() ->
    io:format("~n=== Deep Agent 修改验证测试 ===~n~n"),

    %% 测试 1: LLM 配置验证
    io:format("【测试 1】LLM 配置验证~n"),
    test_llm_config_validation(),

    %% 测试 2: 基本 DeepAgent（无 Middleware）
    io:format("~n【测试 2】基本 DeepAgent（无 Middleware）~n"),
    test_basic_deepagent(),

    %% 测试 3: 带 Middleware 的 DeepAgent
    io:format("~n【测试 3】带 Middleware 的 DeepAgent~n"),
    test_deepagent_with_middleware(),

    io:format("~n=== 所有测试完成 ===~n"),
    ok.

%%====================================================================
%% 测试 1: LLM 配置验证
%%====================================================================

%% @doc 测试 LLM 配置验证
test_llm_config_validation() ->
    io:format("  1.1 测试无效的 LLM 配置（应返回错误）...~n"),

    %% 测试 1.1: 无 LLM 配置
    Config1 = beamai_deepagent:new(#{
        tools => []
    }),
    case beamai_deepagent:run(Config1, <<"test">>) of
        {error, {missing_llm_config, _}} ->
            io:format("      ✓ 无 LLM 配置时正确返回错误~n");
        Other1 ->
            io:format("      ✗ 预期 missing_llm_config 错误，实际: ~p~n", [Other1])
    end,

    %% 测试 1.2: 非法的 LLM 配置（未通过 llm_client:create/2 创建）
    io:format("  1.2 测试非法的 LLM 配置（未通过 llm_client:create/2）...~n"),
    Config2 = beamai_deepagent:new(#{
        llm => #{provider => anthropic, api_key => <<"fake-key">>}
    }),
    case beamai_deepagent:run(Config2, <<"test">>) of
        {error, {invalid_llm_config, _}} ->
            io:format("      ✓ 非法 LLM 配置时正确返回错误~n");
        Other2 ->
            io:format("      ✗ 预期 invalid_llm_config 错误，实际: ~p~n", [Other2])
    end,

    %% 测试 1.3: 有效的 LLM 配置（通过 llm_client:create/2 创建）
    io:format("  1.3 测试有效的 LLM 配置...~n"),
    case example_utils:get_llm_config() of
        {ok, LLM} ->
            case llm_client:is_valid_config(LLM) of
                true ->
                    io:format("      ✓ llm_client:create/2 创建的配置通过验证~n");
                false ->
                    io:format("      ✗ llm_client:create/2 创建的配置验证失败~n")
            end;
        {error, no_api_key} ->
            io:format("      ! 跳过（未设置 ZHIPU_API_KEY）~n")
    end,

    io:format("  LLM 配置验证测试完成~n"),
    ok.

%%====================================================================
%% 测试 2: 基本 DeepAgent
%%====================================================================

%% @doc 测试基本 DeepAgent（无 Middleware）
test_basic_deepagent() ->
    case example_utils:get_llm_config() of
        {ok, LLM} ->
            run_basic_deepagent_test(LLM);
        {error, no_api_key} ->
            io:format("  ! 跳过（未设置 ZHIPU_API_KEY）~n"),
            io:format("  请运行: export ZHIPU_API_KEY=your-api-key~n")
    end.

run_basic_deepagent_test(LLM) ->
    io:format("  使用 GLM-4.7 (Anthropic 兼容 API)~n"),

    %% 创建简单的计算器工具
    Tools = [create_calculator_tool()],

    %% 创建配置（无 Middleware）
    Config = beamai_deepagent:new(#{
        llm => LLM,
        tools => Tools,
        max_depth => 1,
        max_iterations => 5,
        planning_enabled => false,
        reflection_enabled => false
    }),

    io:format("  发送请求: 计算 25 * 4~n"),

    case beamai_deepagent:run(Config, <<"请帮我计算 25 乘以 4 等于多少"/utf8>>) of
        {ok, Result} ->
            Response = maps:get(response, Result, <<"无响应"/utf8>>),
            Iterations = maps:get(iterations, Result, 0),
            io:format("  ✓ 执行成功~n"),
            io:format("    迭代次数: ~p~n", [Iterations]),
            io:format("    响应: ~ts~n", [truncate(Response, 200)]);
        {error, Reason} ->
            io:format("  ✗ 执行失败: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% 测试 3: 带 Middleware 的 DeepAgent
%%====================================================================

%% @doc 测试带 Middleware 的 DeepAgent
test_deepagent_with_middleware() ->
    case example_utils:get_llm_config() of
        {ok, LLM} ->
            run_middleware_deepagent_test(LLM);
        {error, no_api_key} ->
            io:format("  ! 跳过（未设置 ZHIPU_API_KEY）~n"),
            io:format("  请运行: export ZHIPU_API_KEY=your-api-key~n")
    end.

run_middleware_deepagent_test(LLM) ->
    io:format("  使用 GLM-4.7 (Anthropic 兼容 API) + Middleware~n"),

    %% 创建简单的工具
    Tools = [create_calculator_tool()],

    %% 创建配置（带 Middleware）
    Config = beamai_deepagent:new(#{
        llm => LLM,
        tools => Tools,
        max_depth => 1,
        max_iterations => 10,
        planning_enabled => false,
        reflection_enabled => false,
        %% 添加 Middleware
        middlewares => [
            {middleware_call_limit, #{
                max_model_calls => 5,
                max_tool_calls => 10
            }}
        ]
    }),

    io:format("  已配置 Middleware: middleware_call_limit~n"),
    io:format("  发送请求: 计算 100 + 50~n"),

    case beamai_deepagent:run(Config, <<"请帮我计算 100 加 50 等于多少"/utf8>>) of
        {ok, Result} ->
            Response = maps:get(response, Result, <<"无响应"/utf8>>),
            Iterations = maps:get(iterations, Result, 0),
            io:format("  ✓ 执行成功（Middleware 正常工作）~n"),
            io:format("    迭代次数: ~p~n", [Iterations]),
            io:format("    响应: ~ts~n", [truncate(Response, 200)]);
        {error, {middleware_halt, Reason}} ->
            io:format("  ! Middleware 中止执行: ~p~n", [Reason]),
            io:format("    这是正常行为（如果达到调用限制）~n");
        {error, Reason} ->
            io:format("  ✗ 执行失败: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 创建计算器工具
create_calculator_tool() ->
    #{
        name => <<"calculate">>,
        description => <<"执行数学计算。支持加减乘除运算。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"expression">> => #{
                    type => string,
                    description => <<"数学表达式，如 '25 * 4' 或 '100 + 50'"/utf8>>
                }
            },
            required => [<<"expression">>]
        },
        handler => fun(Args, _State) ->
            Expression = maps:get(<<"expression">>, Args, <<"0">>),
            Result = evaluate_expression(Expression),
            io:format("    [工具调用] calculate(~s) = ~p~n", [Expression, Result]),
            #{result => Result, expression => Expression}
        end
    }.

%% @private 简单的表达式求值
evaluate_expression(Expr) when is_binary(Expr) ->
    evaluate_expression(binary_to_list(Expr));
evaluate_expression(Expr) when is_list(Expr) ->
    %% 简单解析：支持 "A op B" 格式
    case string:tokens(Expr, " ") of
        [A, "*", B] -> list_to_integer(A) * list_to_integer(B);
        [A, "+", B] -> list_to_integer(A) + list_to_integer(B);
        [A, "-", B] -> list_to_integer(A) - list_to_integer(B);
        [A, "/", B] -> list_to_integer(A) div list_to_integer(B);
        _ ->
            %% 尝试直接解析为数字
            try list_to_integer(Expr)
            catch _:_ -> 0
            end
    end.

%% @private 截断字符串
truncate(Bin, MaxLen) when is_binary(Bin), byte_size(Bin) > MaxLen ->
    <<Prefix:MaxLen/binary, _/binary>> = Bin,
    <<Prefix/binary, "...">>;
truncate(Bin, _MaxLen) ->
    Bin.
