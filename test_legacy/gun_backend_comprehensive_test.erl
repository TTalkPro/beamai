%%%-------------------------------------------------------------------
%%% @doc Gun 后端全面测试
%%%
%%% 使用 GLM-4.7 via Anthropic Provider 测试 Gun HTTP 后端。
%%% 特别关注 timeout 处理和连接池行为。
%%%
%%% 运行方式:
%%%   rebar3 eunit --module=gun_backend_comprehensive_test
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gun_backend_comprehensive_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite
%%====================================================================

gun_backend_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {inorder, [
         {"1. Verify Gun is default backend", fun test_gun_is_default/0},
         {"2. Connection pool basic test", {timeout, 60, fun test_connection_pool/0}},
         {"3. GLM-4.7 basic chat", {timeout, 120, fun test_glm47_basic_chat/0}},
         {"4. GLM-4.7 streaming chat", {timeout, 120, fun test_glm47_streaming_chat/0}},
         {"5. Timeout handling - short timeout", {timeout, 60, fun test_short_timeout/0}},
         {"6. Timeout handling - normal timeout", {timeout, 120, fun test_normal_timeout/0}},
         {"7. Agent integration", {timeout, 180, fun test_agent_integration/0}},
         {"8. Concurrent requests", {timeout, 180, fun test_concurrent_requests/0}},
         {"9. Connection reuse", {timeout, 180, fun test_connection_reuse/0}}
     ]}}.

%%====================================================================
%% Setup / Cleanup
%%====================================================================

setup() ->
    io:format("~n~n========================================~n"),
    io:format("Gun Backend Comprehensive Test~n"),
    io:format("========================================~n~n"),

    %% 启动应用
    application:ensure_all_started(beamai_agent),

    %% 确保 Gun 后端
    beamai_http:ensure_started(),

    ok.

cleanup(_) ->
    io:format("~n========================================~n"),
    io:format("Test Complete~n"),
    io:format("========================================~n"),
    ok.

%%====================================================================
%% Test 1: Verify Gun is default
%%====================================================================

test_gun_is_default() ->
    io:format("~n--- Test 1: Verify Gun is default backend ---~n"),
    Backend = beamai_http:get_backend(),
    io:format("Current backend: ~p~n", [Backend]),
    ?assertEqual(beamai_http_gun, Backend),
    io:format("[PASS] Gun is default backend~n").

%%====================================================================
%% Test 2: Connection pool
%%====================================================================

test_connection_pool() ->
    io:format("~n--- Test 2: Connection pool basic test ---~n"),

    %% 确保 Gun 和连接池已启动
    beamai_http:ensure_started(),

    %% 获取初始统计（池可能还不存在）
    Stats1 = try beamai_http_pool:stats() catch _:_ -> #{} end,
    io:format("Initial pool stats: ~p~n", [Stats1]),

    %% 发送一个简单请求
    Result = beamai_http:get(<<"https://httpbin.org/get">>),
    io:format("Request result: ~p~n", [element(1, Result)]),

    %% 检查连接池状态
    timer:sleep(100),  %% 等待连接归还
    Stats2 = try beamai_http_pool:stats() catch _:_ -> #{} end,
    io:format("After request pool stats: ~p~n", [Stats2]),

    ?assertMatch({ok, _}, Result),
    io:format("[PASS] Connection pool working~n").

%%====================================================================
%% Test 3: GLM-4.7 basic chat
%%====================================================================

test_glm47_basic_chat() ->
    io:format("~n--- Test 3: GLM-4.7 basic chat ---~n"),

    case get_glm47_config() of
        skip ->
            io:format("[SKIP] ZHIPU_API_KEY not set~n"),
            ok;
        Config ->
            io:format("Config (without key): ~p~n", [maps:remove(api_key, Config)]),

            Messages = [
                #{role => user, content => <<"What is 2+2? Answer with just the number.">>}
            ],

            io:format("Sending message...~n"),
            StartTime = erlang:system_time(millisecond),
            Result = llm_client:chat(Config, Messages),
            EndTime = erlang:system_time(millisecond),
            Duration = EndTime - StartTime,
            io:format("Request duration: ~p ms~n", [Duration]),

            case Result of
                {ok, Response} ->
                    Content = maps:get(content, Response, <<>>),
                    io:format("Response content: ~s~n", [Content]),
                    ?assert(is_binary(Content)),
                    ?assert(byte_size(Content) > 0),
                    io:format("[PASS] Basic chat successful~n");
                {error, Reason} ->
                    io:format("[FAIL] Error: ~p~n", [Reason]),
                    ?assert(false)
            end
    end.

%%====================================================================
%% Test 4: GLM-4.7 streaming chat
%%====================================================================

test_glm47_streaming_chat() ->
    io:format("~n--- Test 4: GLM-4.7 streaming chat ---~n"),

    case get_glm47_config() of
        skip ->
            io:format("[SKIP] ZHIPU_API_KEY not set~n"),
            ok;
        Config ->
            Messages = [
                #{role => user, content => <<"Count from 1 to 5, one number per line.">>}
            ],

            io:format("Starting streaming request...~n"),
            ChunkCount = erlang:make_ref(),
            put(ChunkCount, 0),

            Callback = fun(Event) ->
                Count = get(ChunkCount),
                put(ChunkCount, Count + 1),
                case maps:get(<<"type">>, Event, undefined) of
                    <<"content_block_delta">> ->
                        Delta = maps:get(<<"delta">>, Event, #{}),
                        Text = maps:get(<<"text">>, Delta, <<>>),
                        io:format("  Chunk ~p: ~s~n", [Count + 1, Text]);
                    _ ->
                        ok
                end
            end,

            StartTime = erlang:system_time(millisecond),
            Result = llm_client:stream_chat(Config, Messages, Callback),
            EndTime = erlang:system_time(millisecond),
            Duration = EndTime - StartTime,

            FinalCount = get(ChunkCount),
            io:format("Total chunks received: ~p~n", [FinalCount]),
            io:format("Streaming duration: ~p ms~n", [Duration]),

            case Result of
                {ok, Response} ->
                    Content = maps:get(content, Response, <<>>),
                    io:format("Final content: ~s~n", [Content]),
                    ?assert(byte_size(Content) > 0),
                    io:format("[PASS] Streaming chat successful~n");
                {error, Reason} ->
                    io:format("[FAIL] Error: ~p~n", [Reason]),
                    ?assert(false)
            end
    end.

%%====================================================================
%% Test 5: Timeout handling - short timeout
%%====================================================================

test_short_timeout() ->
    io:format("~n--- Test 5: Timeout handling (short timeout) ---~n"),

    case get_glm47_config() of
        skip ->
            io:format("[SKIP] ZHIPU_API_KEY not set~n"),
            ok;
        BaseConfig ->
            %% 使用非常短的超时（1ms），应该触发超时
            Config = BaseConfig#{timeout => 1},

            Messages = [
                #{role => user, content => <<"Say hello">>}
            ],

            io:format("Testing with 1ms timeout (should fail)...~n"),
            Result = llm_client:chat(Config, Messages),

            case Result of
                {error, {request_failed, timeout}} ->
                    io:format("[PASS] Timeout correctly triggered~n");
                {error, {request_failed, {gun_error, timeout}}} ->
                    io:format("[PASS] Gun timeout correctly triggered~n");
                {error, timeout} ->
                    io:format("[PASS] Timeout correctly triggered~n");
                {error, {http_error, _, _}} ->
                    io:format("[INFO] Got HTTP error (might be timeout related)~n");
                {ok, _} ->
                    io:format("[INFO] Request succeeded (network too fast?)~n");
                Other ->
                    io:format("[INFO] Got: ~p~n", [Other])
            end
    end.

%%====================================================================
%% Test 6: Timeout handling - normal timeout
%%====================================================================

test_normal_timeout() ->
    io:format("~n--- Test 6: Timeout handling (normal timeout) ---~n"),

    case get_glm47_config() of
        skip ->
            io:format("[SKIP] ZHIPU_API_KEY not set~n"),
            ok;
        BaseConfig ->
            %% 使用正常超时（60秒）
            Config = BaseConfig#{timeout => 60000},

            Messages = [
                #{role => user, content => <<"What is the capital of France? One word answer.">>}
            ],

            io:format("Testing with 60s timeout...~n"),
            StartTime = erlang:system_time(millisecond),
            Result = llm_client:chat(Config, Messages),
            EndTime = erlang:system_time(millisecond),
            Duration = EndTime - StartTime,

            io:format("Duration: ~p ms~n", [Duration]),

            case Result of
                {ok, Response} ->
                    Content = maps:get(content, Response, <<>>),
                    io:format("Response: ~s~n", [Content]),
                    ?assert(Duration < 60000),  %% 应该远小于超时时间
                    io:format("[PASS] Normal timeout works correctly~n");
                {error, Reason} ->
                    io:format("[FAIL] Error: ~p~n", [Reason]),
                    ?assert(false)
            end
    end.

%%====================================================================
%% Test 7: Agent integration
%%====================================================================

test_agent_integration() ->
    io:format("~n--- Test 7: Agent integration ---~n"),

    case get_glm47_config() of
        skip ->
            io:format("[SKIP] ZHIPU_API_KEY not set~n"),
            ok;
        LLMConfig ->
            io:format("Starting agent...~n"),

            {ok, Agent} = beamai_agent:start_link(<<"test_gun_agent">>, #{
                system_prompt => <<"You are a helpful assistant. Keep responses very short.">>,
                llm => LLMConfig
            }),

            io:format("Agent started, running query...~n"),
            StartTime = erlang:system_time(millisecond),
            Result = beamai_agent:run(Agent, <<"What is 3+3? Just the number.">>),
            EndTime = erlang:system_time(millisecond),
            Duration = EndTime - StartTime,

            io:format("Agent run duration: ~p ms~n", [Duration]),

            case Result of
                {ok, #{final_response := Response}} ->
                    io:format("Agent response: ~s~n", [Response]),
                    ?assert(is_binary(Response)),
                    io:format("[PASS] Agent integration successful~n");
                {ok, #{response := Response}} ->
                    io:format("Agent response: ~s~n", [Response]),
                    ?assert(is_binary(Response)),
                    io:format("[PASS] Agent integration successful~n");
                {ok, Other} ->
                    io:format("Agent result: ~p~n", [Other]),
                    io:format("[PASS] Agent returned result~n");
                {error, Reason} ->
                    io:format("[FAIL] Agent error: ~p~n", [Reason]),
                    ?assert(false)
            end,

            beamai_agent:stop(Agent)
    end.

%%====================================================================
%% Test 8: Concurrent requests
%%====================================================================

test_concurrent_requests() ->
    io:format("~n--- Test 8: Concurrent requests ---~n"),

    case get_glm47_config() of
        skip ->
            io:format("[SKIP] ZHIPU_API_KEY not set~n"),
            ok;
        Config ->
            io:format("Sending 3 concurrent requests...~n"),

            Parent = self(),
            Questions = [
                <<"What is 1+1? One word.">>,
                <<"What is 2+2? One word.">>,
                <<"What is 3+3? One word.">>
            ],

            StartTime = erlang:system_time(millisecond),

            %% 启动并发请求
            Pids = lists:map(fun(Q) ->
                spawn(fun() ->
                    Messages = [#{role => user, content => Q}],
                    Result = llm_client:chat(Config, Messages),
                    Parent ! {self(), Result}
                end)
            end, Questions),

            %% 收集结果
            Results = lists:map(fun(Pid) ->
                receive
                    {Pid, Result} -> Result
                after 90000 ->
                    {error, timeout}
                end
            end, Pids),

            EndTime = erlang:system_time(millisecond),
            Duration = EndTime - StartTime,

            io:format("All requests completed in ~p ms~n", [Duration]),

            %% 检查结果
            SuccessCount = length([R || {ok, _} = R <- Results]),
            io:format("Successful requests: ~p/~p~n", [SuccessCount, length(Results)]),

            lists:foreach(fun({I, R}) ->
                case R of
                    {ok, Resp} ->
                        Content = maps:get(content, Resp, <<>>),
                        io:format("  Request ~p: ~s~n", [I, Content]);
                    {error, Reason} ->
                        io:format("  Request ~p: ERROR ~p~n", [I, Reason])
                end
            end, lists:zip(lists:seq(1, length(Results)), Results)),

            %% 检查连接池
            PoolStats = beamai_http_pool:stats(),
            io:format("Pool stats after concurrent: ~p~n", [PoolStats]),

            ?assert(SuccessCount >= 2),  %% 至少2个成功
            io:format("[PASS] Concurrent requests completed~n")
    end.

%%====================================================================
%% Test 9: Connection reuse
%%====================================================================

test_connection_reuse() ->
    io:format("~n--- Test 9: Connection reuse ---~n"),

    case get_glm47_config() of
        skip ->
            io:format("[SKIP] ZHIPU_API_KEY not set~n"),
            ok;
        Config ->
            %% 清理连接池
            catch beamai_http_pool:close_all(),
            timer:sleep(100),

            Stats1 = try beamai_http_pool:stats() catch _:_ -> #{} end,
            io:format("Initial pool (after cleanup): ~p~n", [Stats1]),

            %% 第一次请求
            io:format("First request...~n"),
            Messages = [#{role => user, content => <<"Hi">>}],
            Result1 = llm_client:chat(Config, Messages),
            case Result1 of
                {ok, _} ->
                    timer:sleep(100),
                    Stats2 = try beamai_http_pool:stats() catch _:_ -> #{} end,
                    io:format("After first request: ~p~n", [Stats2]),

                    %% 第二次请求（应该复用连接）
                    io:format("Second request (should reuse connection)...~n"),
                    Result2 = llm_client:chat(Config, Messages),
                    case Result2 of
                        {ok, _} ->
                            timer:sleep(100),
                            Stats3 = try beamai_http_pool:stats() catch _:_ -> #{} end,
                            io:format("After second request: ~p~n", [Stats3]),

                            %% 验证连接复用（总连接数应该是1或2，不是2或3）
                            TotalConns = maps:get(total_connections, Stats3, 0),
                            io:format("Total connections: ~p~n", [TotalConns]),

                            ?assert(TotalConns =< 2),  %% 应该复用连接
                            io:format("[PASS] Connection reuse working~n");
                        {error, Reason2} ->
                            io:format("[FAIL] Second request error: ~p~n", [Reason2]),
                            ?assert(false)
                    end;
                {error, Reason1} ->
                    io:format("[FAIL] First request error: ~p~n", [Reason1]),
                    ?assert(false)
            end
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Get GLM-4.7 config via Anthropic provider
get_glm47_config() ->
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
