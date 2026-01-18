%%%-------------------------------------------------------------------
%%% @doc beamai_agent Callback 系统实时测试
%%%
%%% 使用真实的 LLM API 测试所有 Callback 功能。
%%% 需要环境变量 ZHIPU_API_KEY。
%%%
%%% 配置：GLM-4.7 + Anthropic Provider
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_callbacks_live_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 配置
%%====================================================================

%% GLM-4.7 配置
glm_config() ->
    ApiKey = list_to_binary(os:getenv("ZHIPU_API_KEY", "")),
    case ApiKey of
        <<>> ->
            io:format("警告: 未设置 ZHIPU_API_KEY 环境变量~n"),
            error(missing_api_key);
        _ ->
            llm_client:create(anthropic, #{
                model => <<"glm-4.7">>,
                api_key => ApiKey,
                base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
                timeout => 60000,
                max_tokens => 2048
            })
    end.

%%====================================================================
%% Test fixtures
%%====================================================================

setup() ->
    application:ensure_all_started(beamai_runtime),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% 测试：基础 Callback 触发
%%====================================================================

basic_callbacks_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"on_llm_start 和 on_llm_end 触发", {timeout, 120, fun test_llm_callbacks/0}},
      {"on_chain_start 和 on_chain_end 触发", {timeout, 120, fun test_chain_callbacks/0}},
      {"on_agent_finish 触发", {timeout, 120, fun test_agent_finish/0}}
     ]}.

%%====================================================================
%% 测试：LLM 回调
%%====================================================================

test_llm_callbacks() ->
    Self = self(),
    Events = ets:new(events, [public, ordered_set]),

    %% 创建 Agent，设置回调
    {ok, Agent} = beamai_agent:start_link(<<"test_llm_cb">>, #{
        system_prompt => <<"You are a helpful assistant. Keep responses short.">>,
        llm => glm_config(),
        callbacks => #{
            on_llm_start => fun(Prompts, Meta) ->
                io:format("✓ on_llm_start 触发~n"),
                io:format("  Prompts: ~p~n", [Prompts]),
                io:format("  Meta: ~p~n", [Meta]),
                ets:insert(Events, {erlang:monotonic_time(), llm_start, Prompts}),
                Self ! {callback, llm_start}
            end,
            on_llm_end => fun(Response, Meta) ->
                io:format("✓ on_llm_end 触发~n"),
                io:format("  Response: ~p~n", [Response]),
                io:format("  Meta: ~p~n", [Meta]),
                ets:insert(Events, {erlang:monotonic_time(), llm_end, Response}),
                Self ! {callback, llm_end}
            end
        }
    }),

    %% 运行简单对话
    io:format("~n=== 开始测试 LLM Callbacks ===~n"),
    {ok, Result} = beamai_agent:run(Agent, <<"Say hello in one sentence.">>),
    io:format("~nAgent 响应: ~p~n", [Result]),

    %% 等待回调触发
    receive {callback, llm_start} -> ok after 5000 -> ?assert(false) end,
    receive {callback, llm_end} -> ok after 5000 -> ?assert(false) end,

    %% 验证事件顺序
    AllEvents = ets:tab2list(Events),
    io:format("~n记录的事件: ~p~n", [AllEvents]),
    ?assert(length(AllEvents) >= 2),

    %% 清理
    beamai_agent:stop(Agent),
    ets:delete(Events),
    io:format("~n=== LLM Callbacks 测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 测试：Chain 回调
%%====================================================================

test_chain_callbacks() ->
    Self = self(),

    {ok, Agent} = beamai_agent:start_link(<<"test_chain_cb">>, #{
        system_prompt => <<"You are a helpful assistant. Keep responses short.">>,
        llm => glm_config(),
        callbacks => #{
            on_chain_start => fun(Input, Meta) ->
                io:format("✓ on_chain_start 触发~n"),
                io:format("  Input: ~p~n", [Input]),
                io:format("  Meta: ~p~n", [Meta]),
                Self ! {callback, chain_start, Input}
            end,
            on_chain_end => fun(Output, Meta) ->
                io:format("✓ on_chain_end 触发~n"),
                io:format("  Output: ~p~n", [Output]),
                io:format("  Meta: ~p~n", [Meta]),
                Self ! {callback, chain_end, Output}
            end
        }
    }),

    io:format("~n=== 开始测试 Chain Callbacks ===~n"),
    TestInput = <<"What is 2+2? Answer with just the number.">>,
    {ok, _Result} = beamai_agent:run(Agent, TestInput),

    %% 验证 chain_start 收到正确输入
    receive
        {callback, chain_start, Input} ->
            ?assertEqual(TestInput, Input)
    after 5000 ->
        ?assert(false)
    end,

    %% 验证 chain_end 收到输出
    receive
        {callback, chain_end, Output} ->
            ?assert(is_map(Output)),
            io:format("~nChain 输出: ~p~n", [Output])
    after 5000 ->
        ?assert(false)
    end,

    beamai_agent:stop(Agent),
    io:format("~n=== Chain Callbacks 测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 测试：Agent Finish 回调
%%====================================================================

test_agent_finish() ->
    Self = self(),

    {ok, Agent} = beamai_agent:start_link(<<"test_finish_cb">>, #{
        system_prompt => <<"You are a helpful assistant. Keep responses short.">>,
        llm => glm_config(),
        callbacks => #{
            on_agent_finish => fun(Result, Meta) ->
                io:format("✓ on_agent_finish 触发~n"),
                io:format("  Result: ~p~n", [Result]),
                io:format("  Meta: ~p~n", [Meta]),
                Self ! {callback, agent_finish, Result}
            end
        }
    }),

    io:format("~n=== 开始测试 Agent Finish Callback ===~n"),
    {ok, Result} = beamai_agent:run(Agent, <<"Tell me a fun fact in one sentence.">>),

    %% 验证 agent_finish 被调用
    receive
        {callback, agent_finish, FinishResult} ->
            ?assert(is_map(FinishResult)),
            io:format("~nAgent Finish 结果: ~p~n", [FinishResult])
    after 5000 ->
        ?assert(false)
    end,

    beamai_agent:stop(Agent),
    io:format("~n=== Agent Finish Callback 测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 测试：多个 Callback 同时工作
%%====================================================================

multiple_callbacks_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"所有 Callback 类型同时触发", {timeout, 120, fun test_all_callbacks/0}}
     ]}.

test_all_callbacks() ->
    Self = self(),
    CallbackLog = ets:new(callback_log, [public, duplicate_bag]),

    {ok, Agent} = beamai_agent:start_link(<<"test_all_cb">>, #{
        system_prompt => <<"You are a helpful assistant. Keep responses very short.">>,
        llm => glm_config(),
        callbacks => #{
            on_llm_start => fun(_Prompts, _Meta) ->
                log_callback(CallbackLog, on_llm_start),
                Self ! {cb, on_llm_start}
            end,
            on_llm_end => fun(_Response, _Meta) ->
                log_callback(CallbackLog, on_llm_end),
                Self ! {cb, on_llm_end}
            end,
            on_chain_start => fun(_Input, _Meta) ->
                log_callback(CallbackLog, on_chain_start),
                Self ! {cb, on_chain_start}
            end,
            on_chain_end => fun(_Output, _Meta) ->
                log_callback(CallbackLog, on_chain_end),
                Self ! {cb, on_chain_end}
            end,
            on_agent_finish => fun(_Result, _Meta) ->
                log_callback(CallbackLog, on_agent_finish),
                Self ! {cb, on_agent_finish}
            end
        }
    }),

    io:format("~n=== 开始测试所有 Callbacks ===~n"),
    {ok, _Result} = beamai_agent:run(Agent, <<"What is 1+1? Just say the number.">>),

    %% 收集所有回调
    ExpectedCallbacks = [on_chain_start, on_llm_start, on_llm_end, on_agent_finish, on_chain_end],
    ReceivedCallbacks = collect_callbacks(ExpectedCallbacks, []),

    io:format("~n预期的 Callbacks: ~p~n", [ExpectedCallbacks]),
    io:format("收到的 Callbacks: ~p~n", [ReceivedCallbacks]),

    %% 验证所有回调都被触发
    lists:foreach(fun(Expected) ->
        ?assert(lists:member(Expected, ReceivedCallbacks)),
        io:format("✓ ~p 已触发~n", [Expected])
    end, ExpectedCallbacks),

    %% 显示日志统计
    AllLogs = ets:tab2list(CallbackLog),
    io:format("~n总共触发的 Callback 事件数: ~p~n", [length(AllLogs)]),

    beamai_agent:stop(Agent),
    ets:delete(CallbackLog),
    io:format("~n=== 所有 Callbacks 测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 测试：自定义事件
%%====================================================================

custom_event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"触发自定义事件", {timeout, 120, fun test_custom_event/0}}
     ]}.

test_custom_event() ->
    Self = self(),

    {ok, Agent} = beamai_agent:start_link(<<"test_custom">>, #{
        system_prompt => <<"You are a helpful assistant.">>,
        llm => glm_config(),
        callbacks => #{
            on_custom_event => fun(EventName, Data, Meta) ->
                io:format("✓ on_custom_event 触发~n"),
                io:format("  Event: ~p~n", [EventName]),
                io:format("  Data: ~p~n", [Data]),
                io:format("  Meta: ~p~n", [Meta]),
                Self ! {custom_event, EventName, Data}
            end
        }
    }),

    io:format("~n=== 开始测试自定义事件 ===~n"),

    %% 触发自定义事件
    TestData = #{message => <<"Hello">>, value => 42},
    beamai_agent:emit_custom_event(Agent, my_event, TestData),

    %% 验证事件被接收
    receive
        {custom_event, my_event, Data} ->
            ?assertEqual(TestData, Data),
            io:format("~n自定义事件数据验证通过~n")
    after 2000 ->
        ?assert(false)
    end,

    beamai_agent:stop(Agent),
    io:format("~n=== 自定义事件测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 辅助函数
%%====================================================================

log_callback(Ets, CallbackName) ->
    Timestamp = erlang:monotonic_time(),
    ets:insert(Ets, {Timestamp, CallbackName}),
    io:format("  [~p] ~p~n", [Timestamp, CallbackName]).

collect_callbacks([], Acc) ->
    lists:reverse(Acc);
collect_callbacks([Expected | Rest], Acc) ->
    receive
        {cb, Expected} ->
            collect_callbacks(Rest, [Expected | Acc])
    after 10000 ->
        io:format("超时等待 callback: ~p~n", [Expected]),
        collect_callbacks(Rest, Acc)
    end.
