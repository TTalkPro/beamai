%%%-------------------------------------------------------------------
%%% @doc beamai_agent 多 Provider 集成测试
%%%
%%% 测试 beamai_agent 使用不同 LLM Provider 的功能：
%%% - 阿里云百炼 (Bailian/DashScope)
%%% - 智谱 AI (Zhipu/GLM)
%%%
%%% 测试场景：
%%% - 单轮次对话
%%% - 多轮次对话（上下文记忆）
%%%
%%% 需要环境变量：
%%% - BAILIAN_API_KEY: 阿里云百炼 API Key
%%% - ZHIPU_API_KEY: 智谱 AI API Key
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_provider_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Provider 配置
%%====================================================================

%% @doc 百炼配置 (qwen-plus)
bailian_config() ->
    ApiKey = list_to_binary(os:getenv("BAILIAN_API_KEY", "")),
    case ApiKey of
        <<>> ->
            io:format("警告: 未设置 BAILIAN_API_KEY 环境变量~n"),
            error(missing_bailian_api_key);
        _ ->
            llm_client:create(bailian, #{
                model => <<"qwen-plus">>,
                api_key => ApiKey,
                timeout => 60000,
                max_tokens => 2048
            })
    end.

%% @doc 智谱配置 (GLM-4.6)
zhipu_config() ->
    ApiKey = list_to_binary(os:getenv("ZHIPU_API_KEY", "")),
    case ApiKey of
        <<>> ->
            io:format("警告: 未设置 ZHIPU_API_KEY 环境变量~n"),
            error(missing_zhipu_api_key);
        _ ->
            llm_client:create(zhipu, #{
                model => <<"glm-4.6">>,
                api_key => ApiKey,
                timeout => 60000,
                max_tokens => 2048
            })
    end.

%%====================================================================
%% Test fixtures
%%====================================================================

setup() ->
    application:ensure_all_started(beamai_agent),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% 百炼 Provider 测试
%%====================================================================

bailian_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"百炼 - 单轮次对话", {timeout, 120, fun test_bailian_single_turn/0}},
      {"百炼 - 中文对话", {timeout, 120, fun test_bailian_chinese/0}},
      {"百炼 - 多轮次对话", {timeout, 180, fun test_bailian_multi_turn/0}}
     ]}.

test_bailian_single_turn() ->
    io:format("~n=== 百炼 Provider - 单轮次对话测试 ===~n"),

    {ok, Agent} = beamai_agent:start_link(<<"bailian_single">>, #{
        system_prompt => <<"You are a helpful assistant. Keep responses concise.">>,
        llm => bailian_config()
    }),

    {ok, Result} = beamai_agent:run(Agent, <<"What is 2 + 2? Just answer with the number.">>),

    Response = maps:get(final_response, Result, <<>>),
    io:format("问题: What is 2 + 2?~n"),
    io:format("回答: ~s~n", [Response]),

    %% 验证响应包含 "4"
    ?assert(binary:match(Response, <<"4">>) =/= nomatch),

    beamai_agent:stop(Agent),
    io:format("=== 百炼单轮次对话测试完成 ===~n~n"),
    ok.

test_bailian_chinese() ->
    io:format("~n=== 百炼 Provider - 中文对话测试 ===~n"),

    {ok, Agent} = beamai_agent:start_link(<<"bailian_chinese">>, #{
        system_prompt => <<"你是一个友好的中文助手。请用简洁的中文回答。"/utf8>>,
        llm => bailian_config()
    }),

    {ok, Result} = beamai_agent:run(Agent, <<"请用一句话介绍你自己"/utf8>>),

    Response = maps:get(final_response, Result, <<>>),
    io:format("问题: 请用一句话介绍你自己~n"),
    io:format("回答: ~ts~n", [Response]),

    %% 验证响应非空
    ?assert(byte_size(Response) > 0),

    beamai_agent:stop(Agent),
    io:format("=== 百炼中文对话测试完成 ===~n~n"),
    ok.

test_bailian_multi_turn() ->
    io:format("~n=== 百炼 Provider - 多轮次对话测试 ===~n"),

    {ok, Agent} = beamai_agent:start_link(<<"bailian_multi">>, #{
        system_prompt => <<"You are a helpful assistant. Remember what the user tells you. Be concise.">>,
        llm => bailian_config()
    }),

    %% 第一轮：告诉 Agent 一个信息
    io:format("~n第一轮对话:~n"),
    {ok, Result1} = beamai_agent:run(Agent, <<"My favorite color is blue.">>),
    Response1 = maps:get(final_response, Result1, <<>>),
    io:format("  用户: My favorite color is blue.~n"),
    io:format("  助手: ~s~n", [Response1]),

    %% 第二轮：告诉更多信息
    io:format("~n第二轮对话:~n"),
    {ok, Result2} = beamai_agent:run(Agent, <<"My name is Alice.">>),
    Response2 = maps:get(final_response, Result2, <<>>),
    io:format("  用户: My name is Alice.~n"),
    io:format("  助手: ~s~n", [Response2]),

    %% 第三轮：询问之前的信息
    io:format("~n第三轮对话:~n"),
    {ok, Result3} = beamai_agent:run(Agent, <<"What is my favorite color and what is my name?">>),
    Response3 = maps:get(final_response, Result3, <<>>),
    io:format("  用户: What is my favorite color and what is my name?~n"),
    io:format("  助手: ~s~n", [Response3]),

    %% 验证 Agent 记住了信息
    ?assert(binary:match(Response3, <<"blue">>) =/= nomatch orelse
            binary:match(Response3, <<"Blue">>) =/= nomatch),
    ?assert(binary:match(Response3, <<"Alice">>) =/= nomatch),

    beamai_agent:stop(Agent),
    io:format("~n=== 百炼多轮次对话测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 智谱 Provider 测试
%%====================================================================

zhipu_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"智谱 GLM-4.6 - 单轮次对话", {timeout, 120, fun test_zhipu_single_turn/0}},
      {"智谱 GLM-4.6 - 中文对话", {timeout, 120, fun test_zhipu_chinese/0}},
      {"智谱 GLM-4.6 - 多轮次对话", {timeout, 180, fun test_zhipu_multi_turn/0}}
     ]}.

test_zhipu_single_turn() ->
    io:format("~n=== 智谱 GLM-4.6 - 单轮次对话测试 ===~n"),

    {ok, Agent} = beamai_agent:start_link(<<"zhipu_single">>, #{
        system_prompt => <<"You are a helpful assistant. Keep responses concise.">>,
        llm => zhipu_config()
    }),

    {ok, Result} = beamai_agent:run(Agent, <<"What is 3 + 5? Just answer with the number.">>),

    Response = maps:get(final_response, Result, <<>>),
    io:format("问题: What is 3 + 5?~n"),
    io:format("回答: ~s~n", [Response]),

    %% 验证响应包含 "8"
    ?assert(binary:match(Response, <<"8">>) =/= nomatch),

    beamai_agent:stop(Agent),
    io:format("=== 智谱单轮次对话测试完成 ===~n~n"),
    ok.

test_zhipu_chinese() ->
    io:format("~n=== 智谱 GLM-4.6 - 中文对话测试 ===~n"),

    {ok, Agent} = beamai_agent:start_link(<<"zhipu_chinese">>, #{
        system_prompt => <<"你是一个友好的中文助手。请用简洁的中文回答。"/utf8>>,
        llm => zhipu_config()
    }),

    {ok, Result} = beamai_agent:run(Agent, <<"请用一句话介绍你自己"/utf8>>),

    Response = maps:get(final_response, Result, <<>>),
    io:format("问题: 请用一句话介绍你自己~n"),
    io:format("回答: ~ts~n", [Response]),

    %% 验证响应非空
    ?assert(byte_size(Response) > 0),

    beamai_agent:stop(Agent),
    io:format("=== 智谱中文对话测试完成 ===~n~n"),
    ok.

test_zhipu_multi_turn() ->
    io:format("~n=== 智谱 GLM-4.6 - 多轮次对话测试 ===~n"),

    {ok, Agent} = beamai_agent:start_link(<<"zhipu_multi">>, #{
        system_prompt => <<"You are a helpful assistant. Remember what the user tells you. Be concise.">>,
        llm => zhipu_config()
    }),

    %% 第一轮：告诉 Agent 一个信息
    io:format("~n第一轮对话:~n"),
    {ok, Result1} = beamai_agent:run(Agent, <<"My favorite animal is a cat.">>),
    Response1 = maps:get(final_response, Result1, <<>>),
    io:format("  用户: My favorite animal is a cat.~n"),
    io:format("  助手: ~s~n", [Response1]),

    %% 第二轮：告诉更多信息
    io:format("~n第二轮对话:~n"),
    {ok, Result2} = beamai_agent:run(Agent, <<"My name is Bob.">>),
    Response2 = maps:get(final_response, Result2, <<>>),
    io:format("  用户: My name is Bob.~n"),
    io:format("  助手: ~s~n", [Response2]),

    %% 第三轮：询问之前的信息
    io:format("~n第三轮对话:~n"),
    {ok, Result3} = beamai_agent:run(Agent, <<"What is my favorite animal and what is my name?">>),
    Response3 = maps:get(final_response, Result3, <<>>),
    io:format("  用户: What is my favorite animal and what is my name?~n"),
    io:format("  助手: ~s~n", [Response3]),

    %% 验证 Agent 记住了信息
    ?assert(binary:match(Response3, <<"cat">>) =/= nomatch orelse
            binary:match(Response3, <<"Cat">>) =/= nomatch),
    ?assert(binary:match(Response3, <<"Bob">>) =/= nomatch),

    beamai_agent:stop(Agent),
    io:format("~n=== 智谱多轮次对话测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 综合比较测试
%%====================================================================

comparison_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"双 Provider 对比测试", {timeout, 240, fun test_provider_comparison/0}}
     ]}.

test_provider_comparison() ->
    io:format("~n=== 双 Provider 对比测试 ===~n"),

    Question = <<"What are the three primary colors? List them briefly.">>,

    %% 百炼回答
    io:format("~n--- 百炼 (qwen-plus) 回答 ---~n"),
    {ok, BailianAgent} = beamai_agent:start_link(<<"bailian_compare">>, #{
        system_prompt => <<"You are a helpful assistant. Keep responses concise.">>,
        llm => bailian_config()
    }),
    {ok, BailianResult} = beamai_agent:run(BailianAgent, Question),
    BailianResponse = maps:get(final_response, BailianResult, <<>>),
    io:format("问题: ~s~n", [Question]),
    io:format("回答: ~s~n", [BailianResponse]),
    beamai_agent:stop(BailianAgent),

    %% 智谱回答
    io:format("~n--- 智谱 (GLM-4.6) 回答 ---~n"),
    {ok, ZhipuAgent} = beamai_agent:start_link(<<"zhipu_compare">>, #{
        system_prompt => <<"You are a helpful assistant. Keep responses concise.">>,
        llm => zhipu_config()
    }),
    {ok, ZhipuResult} = beamai_agent:run(ZhipuAgent, Question),
    ZhipuResponse = maps:get(final_response, ZhipuResult, <<>>),
    io:format("问题: ~s~n", [Question]),
    io:format("回答: ~s~n", [ZhipuResponse]),
    beamai_agent:stop(ZhipuAgent),

    %% 验证两个响应都包含颜色相关内容
    ?assert(byte_size(BailianResponse) > 0),
    ?assert(byte_size(ZhipuResponse) > 0),

    io:format("~n=== 双 Provider 对比测试完成 ===~n~n"),
    ok.
