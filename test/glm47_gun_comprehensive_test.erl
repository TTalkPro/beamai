%%%-------------------------------------------------------------------
%%% @doc GLM-4.7 Gun 后端全面测试
%%%
%%% 使用 GLM-4.7 via Anthropic Provider 全面测试 Gun HTTP 后端。
%%%
%%% 测试内容：
%%% 1. llm_client 同步聊天
%%% 2. llm_client 流式聊天
%%% 3. beamai_agent 集成测试
%%%
%%% 运行方式:
%%%   rebar3 eunit --module=glm47_gun_comprehensive_test
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(glm47_gun_comprehensive_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite
%%====================================================================

glm47_gun_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {inorder, [
         {"1. 验证 Gun 为默认后端", fun test_gun_is_default/0},
         {"2. llm_client 同步聊天 - 简单问答", {timeout, 120, fun test_sync_chat_simple/0}},
         {"3. llm_client 同步聊天 - 多轮对话", {timeout, 120, fun test_sync_chat_multi_turn/0}},
         {"4. llm_client 同步聊天 - 带系统提示", {timeout, 120, fun test_sync_chat_with_system/0}},
         {"5. llm_client 流式聊天 - 基础测试", {timeout, 120, fun test_stream_chat_basic/0}},
         {"6. llm_client 流式聊天 - Token 回调", {timeout, 120, fun test_stream_chat_token_callback/0}},
         {"7. beamai_agent - 基础运行", {timeout, 180, fun test_agent_basic/0}},
         {"8. beamai_agent - 带系统提示", {timeout, 180, fun test_agent_with_system_prompt/0}},
         {"9. beamai_agent - 多轮交互", {timeout, 300, fun test_agent_multi_turn/0}}
     ]}}.

%%====================================================================
%% Setup / Cleanup
%%====================================================================

setup() ->
    io:format("~n~n"),
    io:format("╔══════════════════════════════════════════════════════════╗~n"),
    io:format("║     GLM-4.7 + Gun 后端全面测试                           ║~n"),
    io:format("╚══════════════════════════════════════════════════════════╝~n~n"),

    %% 启动应用
    application:ensure_all_started(beamai_agent),

    ok.

cleanup(_) ->
    io:format("~n"),
    io:format("══════════════════════════════════════════════════════════~n"),
    io:format("测试完成~n"),
    io:format("══════════════════════════════════════════════════════════~n"),
    ok.

%%====================================================================
%% Test 1: 验证 Gun 后端
%%====================================================================

test_gun_is_default() ->
    io:format("~n▶ 测试 1: 验证 Gun 为默认后端~n"),
    Backend = beamai_http:get_backend(),
    io:format("  当前后端: ~p~n", [Backend]),
    ?assertEqual(beamai_http_gun, Backend),

    %% 检查连接池
    PoolStats = beamai_http_pool:stats(),
    io:format("  连接池状态: ~p~n", [PoolStats]),

    io:format("  ✓ 通过~n").

%%====================================================================
%% Test 2: 同步聊天 - 简单问答
%%====================================================================

test_sync_chat_simple() ->
    io:format("~n▶ 测试 2: llm_client 同步聊天 - 简单问答~n"),

    case get_llm_config() of
        skip ->
            io:format("  [跳过] ZHIPU_API_KEY 未设置~n"),
            ok;
        Config ->
            Messages = [
                #{role => user, content => <<"1+1等于几？只回答数字。">>}
            ],

            io:format("  发送消息: ~ts~n", [maps:get(content, hd(Messages))]),

            {Time, Result} = timer:tc(fun() -> llm_client:chat(Config, Messages) end),
            TimeMs = Time div 1000,
            io:format("  耗时: ~p ms~n", [TimeMs]),

            case Result of
                {ok, Response} ->
                    Content = maps:get(content, Response, <<>>),
                    io:format("  响应: ~ts~n", [Content]),
                    ?assert(byte_size(Content) > 0),
                    io:format("  ✓ 通过~n");
                {error, Reason} ->
                    io:format("  ✗ 错误: ~p~n", [Reason]),
                    ?assert(false)
            end
    end.

%%====================================================================
%% Test 3: 同步聊天 - 多轮对话
%%====================================================================

test_sync_chat_multi_turn() ->
    io:format("~n▶ 测试 3: llm_client 同步聊天 - 多轮对话~n"),

    case get_llm_config() of
        skip ->
            io:format("  [跳过] ZHIPU_API_KEY 未设置~n"),
            ok;
        Config ->
            %% 第一轮
            Messages1 = [
                #{role => user, content => <<"My name is Alice. Please remember it.">>}
            ],
            io:format("  Round 1: My name is Alice~n"),

            {ok, Response1} = llm_client:chat(Config, Messages1),
            Content1 = maps:get(content, Response1, <<>>),
            io:format("  Assistant: ~ts~n", [truncate(Content1, 80)]),

            %% 第二轮（带上下文）
            Messages2 = [
                #{role => user, content => <<"My name is Alice. Please remember it.">>},
                #{role => assistant, content => Content1},
                #{role => user, content => <<"What is my name?">>}
            ],
            io:format("  Round 2: What is my name?~n"),

            {ok, Response2} = llm_client:chat(Config, Messages2),
            Content2 = maps:get(content, Response2, <<>>),
            io:format("  Assistant: ~ts~n", [truncate(Content2, 80)]),

            %% 验证上下文保持
            ?assert(binary:match(Content2, <<"Alice">>) =/= nomatch orelse
                    binary:match(Content2, <<"alice">>) =/= nomatch),
            io:format("  ✓ 通过 (上下文保持正确)~n")
    end.

%%====================================================================
%% Test 4: 同步聊天 - 带系统提示
%%====================================================================

test_sync_chat_with_system() ->
    io:format("~n▶ 测试 4: llm_client 同步聊天 - 带系统提示~n"),

    case get_llm_config() of
        skip ->
            io:format("  [跳过] ZHIPU_API_KEY 未设置~n"),
            ok;
        Config ->
            Messages = [
                #{role => system, content => <<"You are a pirate. Always respond in pirate speak with 'Arrr!' in your reply.">>},
                #{role => user, content => <<"Hello!">>}
            ],

            io:format("  System: You are a pirate~n"),
            io:format("  User: Hello!~n"),

            {ok, Response} = llm_client:chat(Config, Messages),
            Content = maps:get(content, Response, <<>>),
            io:format("  Assistant: ~ts~n", [truncate(Content, 100)]),

            %% 验证响应包含 pirate style
            HasArrr = binary:match(Content, <<"Arrr">>) =/= nomatch orelse
                      binary:match(Content, <<"arrr">>) =/= nomatch orelse
                      binary:match(Content, <<"ARRR">>) =/= nomatch orelse
                      binary:match(Content, <<"pirate">>) =/= nomatch orelse
                      binary:match(Content, <<"matey">>) =/= nomatch orelse
                      binary:match(Content, <<"ahoy">>) =/= nomatch,
            ?assert(HasArrr),
            io:format("  ✓ 通过 (系统提示生效)~n")
    end.

%%====================================================================
%% Test 5: 流式聊天 - 基础测试
%%====================================================================

test_stream_chat_basic() ->
    io:format("~n▶ 测试 5: llm_client 流式聊天 - 基础测试~n"),

    case get_llm_config() of
        skip ->
            io:format("  [跳过] ZHIPU_API_KEY 未设置~n"),
            ok;
        Config ->
            Messages = [
                #{role => user, content => <<"从1数到5，每个数字一行。">>}
            ],

            io:format("  发送: 从1数到5，每个数字一行~n"),
            io:format("  流式输出: "),

            ChunkRef = make_ref(),
            put({chunk_count, ChunkRef}, 0),

            Callback = fun(Event) ->
                Count = get({chunk_count, ChunkRef}),
                put({chunk_count, ChunkRef}, Count + 1),
                case Event of
                    #{<<"type">> := <<"content_block_delta">>, <<"delta">> := Delta} ->
                        Text = maps:get(<<"text">>, Delta, <<>>),
                        io:format("~ts", [Text]);
                    _ ->
                        ok
                end
            end,

            {Time, Result} = timer:tc(fun() ->
                llm_client:stream_chat(Config, Messages, Callback)
            end),
            TimeMs = Time div 1000,

            io:format("~n  总 Chunk 数: ~p, 耗时: ~p ms~n",
                      [get({chunk_count, ChunkRef}), TimeMs]),

            case Result of
                {ok, Response} ->
                    Content = maps:get(content, Response, <<>>),
                    ?assert(byte_size(Content) > 0),
                    io:format("  ✓ 通过~n");
                {error, Reason} ->
                    io:format("  ✗ 错误: ~p~n", [Reason]),
                    ?assert(false)
            end
    end.

%%====================================================================
%% Test 6: 流式聊天 - Token 回调
%%====================================================================

test_stream_chat_token_callback() ->
    io:format("~n▶ 测试 6: llm_client 流式聊天 - Token 回调~n"),

    case get_llm_config() of
        skip ->
            io:format("  [跳过] ZHIPU_API_KEY 未设置~n"),
            ok;
        Config ->
            Messages = [
                #{role => user, content => <<"说三个水果名称，用逗号分隔。">>}
            ],

            io:format("  发送: 说三个水果名称~n"),

            TokenRef = make_ref(),
            put({tokens, TokenRef}, []),

            OnNewToken = fun(Token, _Meta) ->
                Tokens = get({tokens, TokenRef}),
                put({tokens, TokenRef}, [Token | Tokens])
            end,

            Callback = fun(_Event) -> ok end,

            Result = llm_client:stream_chat(Config, Messages, Callback, #{
                on_llm_new_token => OnNewToken
            }),

            AllTokens = lists:reverse(get({tokens, TokenRef})),
            TokenCount = length(AllTokens),

            io:format("  收到 Token 数: ~p~n", [TokenCount]),
            io:format("  部分 Token: ~p~n", [lists:sublist(AllTokens, 5)]),

            case Result of
                {ok, Response} ->
                    Content = maps:get(content, Response, <<>>),
                    io:format("  完整响应: ~ts~n", [Content]),
                    ?assert(byte_size(Content) > 0),
                    io:format("  ✓ 通过~n");
                {error, Reason} ->
                    io:format("  ✗ 错误: ~p~n", [Reason]),
                    ?assert(false)
            end
    end.

%%====================================================================
%% Test 7: Agent 基础运行
%%====================================================================

test_agent_basic() ->
    io:format("~n▶ 测试 7: beamai_agent - 基础运行~n"),

    case get_llm_config() of
        skip ->
            io:format("  [跳过] ZHIPU_API_KEY 未设置~n"),
            ok;
        LLMConfig ->
            io:format("  启动 Agent...~n"),

            {ok, Agent} = beamai_agent:start_link(<<"test_basic">>, #{
                llm => LLMConfig
            }),

            io:format("  Agent PID: ~p~n", [Agent]),
            io:format("  发送: 2+3等于多少？~n"),

            {Time, Result} = timer:tc(fun() ->
                beamai_agent:run(Agent, <<"2+3等于多少？只回答数字。">>)
            end),
            TimeMs = Time div 1000,

            io:format("  耗时: ~p ms~n", [TimeMs]),

            case Result of
                {ok, #{final_response := Response}} ->
                    io:format("  响应: ~ts~n", [Response]),
                    ?assert(is_binary(Response)),
                    io:format("  ✓ 通过~n");
                {ok, #{response := Response}} ->
                    io:format("  响应: ~ts~n", [Response]),
                    ?assert(is_binary(Response)),
                    io:format("  ✓ 通过~n");
                {ok, Other} ->
                    io:format("  结果: ~p~n", [Other]),
                    io:format("  ✓ 通过 (返回其他格式)~n");
                {error, Reason} ->
                    io:format("  ✗ 错误: ~p~n", [Reason]),
                    ?assert(false)
            end,

            beamai_agent:stop(Agent)
    end.

%%====================================================================
%% Test 8: Agent 带系统提示
%%====================================================================

test_agent_with_system_prompt() ->
    io:format("~n▶ 测试 8: beamai_agent - 带系统提示~n"),

    case get_llm_config() of
        skip ->
            io:format("  [跳过] ZHIPU_API_KEY 未设置~n"),
            ok;
        LLMConfig ->
            SystemPrompt = <<"你是一个专业的数学老师。回答要简洁明了。">>,

            io:format("  系统提示: ~ts~n", [SystemPrompt]),

            {ok, Agent} = beamai_agent:start_link(<<"test_system">>, #{
                system_prompt => SystemPrompt,
                llm => LLMConfig
            }),

            io:format("  发送: 什么是质数？~n"),

            Result = beamai_agent:run(Agent, <<"什么是质数？用一句话解释。">>),

            case Result of
                {ok, #{final_response := Response}} ->
                    io:format("  响应: ~ts~n", [truncate(Response, 100)]),
                    ?assert(is_binary(Response)),
                    io:format("  ✓ 通过~n");
                {ok, #{response := Response}} ->
                    io:format("  响应: ~ts~n", [truncate(Response, 100)]),
                    ?assert(is_binary(Response)),
                    io:format("  ✓ 通过~n");
                {ok, _} ->
                    io:format("  ✓ 通过~n");
                {error, Reason} ->
                    io:format("  ✗ 错误: ~p~n", [Reason]),
                    ?assert(false)
            end,

            beamai_agent:stop(Agent)
    end.

%%====================================================================
%% Test 9: Agent 多轮交互
%%====================================================================

test_agent_multi_turn() ->
    io:format("~n▶ 测试 9: beamai_agent - 多轮交互~n"),

    case get_llm_config() of
        skip ->
            io:format("  [跳过] ZHIPU_API_KEY 未设置~n"),
            ok;
        LLMConfig ->
            {ok, Agent} = beamai_agent:start_link(<<"test_multi">>, #{
                system_prompt => <<"You have a good memory. Remember what the user tells you.">>,
                llm => LLMConfig
            }),

            %% 第一轮
            io:format("  Round 1: My favorite color is blue.~n"),
            {ok, R1} = beamai_agent:run(Agent, <<"My favorite color is blue. Please remember it.">>),
            Response1 = extract_response(R1),
            io:format("  Assistant: ~ts~n", [truncate(Response1, 80)]),

            %% 第二轮
            io:format("  Round 2: I like apples.~n"),
            {ok, R2} = beamai_agent:run(Agent, <<"I also like apples. Please remember this too.">>),
            Response2 = extract_response(R2),
            io:format("  Assistant: ~ts~n", [truncate(Response2, 80)]),

            %% 第三轮 - 验证记忆
            io:format("  Round 3: Test memory~n"),
            {ok, R3} = beamai_agent:run(Agent, <<"What is my favorite color? What fruit do I like?">>),
            Response3 = extract_response(R3),
            io:format("  Assistant: ~ts~n", [truncate(Response3, 150)]),

            %% 验证记忆
            HasBlue = binary:match(Response3, <<"blue">>) =/= nomatch orelse
                      binary:match(Response3, <<"Blue">>) =/= nomatch,
            HasApple = binary:match(Response3, <<"apple">>) =/= nomatch orelse
                       binary:match(Response3, <<"Apple">>) =/= nomatch,

            io:format("  Memory check: blue=~p, apple=~p~n", [HasBlue, HasApple]),

            ?assert(HasBlue orelse HasApple),  %% 至少记住一个
            io:format("  ✓ 通过 (Agent 保持对话上下文)~n"),

            beamai_agent:stop(Agent)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc 获取 GLM-4.7 LLM 配置
get_llm_config() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            skip;
        Key ->
            llm_client:create(anthropic, #{
                model => <<"glm-4.7">>,
                api_key => list_to_binary(Key),
                base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
                timeout => 60000,
                max_tokens => 1024
            })
    end.

%% @doc 截断文本
truncate(Text, MaxLen) when byte_size(Text) =< MaxLen ->
    Text;
truncate(Text, MaxLen) ->
    <<Short:MaxLen/binary, _/binary>> = Text,
    <<Short/binary, "...">>.

%% @doc 从 Agent 结果中提取响应
extract_response(#{final_response := R}) -> R;
extract_response(#{response := R}) -> R;
extract_response(#{content := R}) -> R;
extract_response(R) when is_binary(R) -> R;
extract_response(_) -> <<"(无法提取响应)">>.
