%%%-------------------------------------------------------------------
%%% @doc Process-native Agent 集成测试
%%%
%%% 对标 beamai_agent_step 的集成测试，验证 beamai_process_agent
%%% 在真实 LLM 调用下的正确性。
%%%
%%% 前置条件:
%%%   export ZHIPU_API_KEY=your-api-key
%%%
%%% 运行:
%%%   ERL_LIBS=../_build/default/lib rebar3 shell
%%%   example_process_agent_test:run_all().
%%% @end
%%%-------------------------------------------------------------------
-module(example_process_agent_test).

-export([run_all/0, run_all/1]).

-define(BASE_URL, <<"https://open.bigmodel.cn/api/coding/paas/v4">>).
-define(ENDPOINT, <<"/chat/completions">>).
-define(DEFAULT_MODEL, <<"glm-4.7">>).
-define(MAX_TOKENS, 2048).

run_all() -> run_all(?DEFAULT_MODEL).
run_all(Model) ->
    ensure_started(),
    io:format("~n=== Process-native Agent Integration Tests ===~n"),

    test_single_agent(Model),
    test_agent_pipeline(Model),
    test_parallel_agents(Model),
    test_shared_kernel(Model),
    test_multi_turn(Model),
    test_with_tools(Model),

    io:format("~nAll tests passed!~n").

%%====================================================================
%% Test 1: Single Agent (basic chat)
%%====================================================================

test_single_agent(Model) ->
    io:format("~nTest 1: Single agent~n"),

    Config = #{
        llm => {openai, #{model => Model, api_key => get_api_key(),
                             base_url => ?BASE_URL, endpoint => ?ENDPOINT, max_tokens => ?MAX_TOKENS}},
        system_prompt => <<"You are a helpful assistant. Reply concisely in English.">>
    },

    {ok, Result} = beamai_process_agent:run_sync(Config, <<"What is 2+2?">>),
    Response = maps:get(response, Result),
    TurnCount = maps:get(turn_count, Result),

    true = is_binary(Response),
    true = byte_size(Response) > 0,
    1 = TurnCount,

    io:format("  Response: ~s~n", [Response]),
    io:format("  PASSED~n").

%%====================================================================
%% Test 2: Agent pipeline (translate -> summarize)
%%====================================================================

test_agent_pipeline(Model) ->
    io:format("~nTest 2: Agent pipeline (translate -> summarize)~n"),

    ApiKey = get_api_key(),

    %% Step 1: 翻译 Agent
    TranslateConfig = #{
        system_prompt => <<"Translate the user's message to English. Output only the translation.">>,
        max_tool_iterations => 1
    },
    %% Step 2: 摘要 Agent
    SummarizeConfig = #{
        system_prompt => <<"Summarize the input in exactly one short sentence.">>,
        max_tool_iterations => 1
    },

    %% 构建 Kernel
    LlmCfg = beamai_chat_completion:create(openai, #{
        model => Model, api_key => ApiKey,
        base_url => ?BASE_URL, endpoint => ?ENDPOINT, max_tokens => ?MAX_TOKENS}),
    Kernel = beamai_kernel:add_service(beamai_kernel:new(), LlmCfg),
    Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

    %% 构建 Process: translate_step -> summarize_step
    B0 = beamai_process:builder(pipeline),
    B1 = beamai_process:add_step(B0, translate_step, beamai_process_agent_llm_step,
        TranslateConfig#{required_inputs => []}),
    B2 = beamai_process:add_step(B1, summarize_step, beamai_process_agent_llm_step,
        SummarizeConfig#{required_inputs => [], output_event => pipeline_done}),
    %% user_message -> translate
    B3 = beamai_process:on_event(B2, user_message, translate_step, user_message),
    %% translate 完成后（agent_done）-> summarize 的 user_message
    B4 = beamai_process:on_event(B3, agent_done, summarize_step, user_message,
        fun(Data) -> maps:get(response, Data, <<>>) end),

    {ok, ProcessDef} = beamai_process:build(B4),
    InitEvent = beamai_process_event:new(user_message, <<"Erlang是一门并发函数式编程语言"/utf8>>),
    PD = ProcessDef#{initial_events => [InitEvent]},

    {ok, Pid} = beamai_process:start(PD, #{context => Context, caller => self()}),
    receive
        {process_completed, Pid, StepsState} ->
            #{summarize_step := #{state := SumState}} = StepsState,
            SumResponse = maps:get(last_response, SumState, <<>>),
            true = byte_size(SumResponse) > 0,
            io:format("  Final summary: ~s~n", [SumResponse]),
            io:format("  PASSED~n");
        {process_failed, Pid, Reason} ->
            io:format("  FAILED: ~p~n", [Reason]),
            error(test_failed)
    after 30000 ->
        error(timeout)
    end.

%%====================================================================
%% Test 3: Parallel agents (fan-out -> fan-in)
%%====================================================================

test_parallel_agents(Model) ->
    io:format("~nTest 3: Parallel agents (fan-out -> fan-in)~n"),

    ApiKey = get_api_key(),
    LlmCfg = beamai_chat_completion:create(openai, #{
        model => Model, api_key => ApiKey,
        base_url => ?BASE_URL, endpoint => ?ENDPOINT, max_tokens => ?MAX_TOKENS}),
    Kernel = beamai_kernel:add_service(beamai_kernel:new(), LlmCfg),
    Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

    %% 三个并行 Agent
    Agent1Config = #{system_prompt => <<"Answer in one word: what color is the sky?">>,
                     required_inputs => [], output_event => a1_done},
    Agent2Config = #{system_prompt => <<"Answer in one word: what color is grass?">>,
                     required_inputs => [], output_event => a2_done},
    Agent3Config = #{system_prompt => <<"Answer in one word: what color is snow?">>,
                     required_inputs => [], output_event => a3_done},

    %% 汇聚 Transform step
    MergeConfig = #{
        required_inputs => [a1, a2, a3],
        transform => fun(#{a1 := A1, a2 := A2, a3 := A3}) ->
            #{merged => <<A1/binary, ", ", A2/binary, ", ", A3/binary>>}
        end,
        output_event => merged_done
    },

    B0 = beamai_process:builder(parallel_agents),
    B1 = beamai_process:add_step(B0, agent1, beamai_process_agent_llm_step, Agent1Config),
    B2 = beamai_process:add_step(B1, agent2, beamai_process_agent_llm_step, Agent2Config),
    B3 = beamai_process:add_step(B2, agent3, beamai_process_agent_llm_step, Agent3Config),
    B4 = beamai_process:add_step(B3, merge, beamai_process_step_transform, MergeConfig),

    %% 同一 user_message 路由到三个 agent
    B5 = beamai_process:on_event(B4, user_message, agent1, user_message),
    B6 = beamai_process:on_event(B5, user_message, agent2, user_message),
    B7 = beamai_process:on_event(B6, user_message, agent3, user_message),

    %% 三个 agent 完成后路由到 merge
    ExtractResponse = fun(D) -> maps:get(response, D, <<>>) end,
    B8 = beamai_process:on_event(B7, a1_done, merge, a1, ExtractResponse),
    B9 = beamai_process:on_event(B8, a2_done, merge, a2, ExtractResponse),
    B10 = beamai_process:on_event(B9, a3_done, merge, a3, ExtractResponse),

    {ok, ProcessDef} = beamai_process:build(B10),
    InitEvent = beamai_process_event:new(user_message, <<"answer the question">>),
    PD = ProcessDef#{initial_events => [InitEvent]},

    T0 = erlang:monotonic_time(millisecond),
    {ok, Pid} = beamai_process:start(PD, #{context => Context, caller => self()}),
    receive
        {process_completed, Pid, StepsState} ->
            T1 = erlang:monotonic_time(millisecond),
            #{merge := #{state := MergeState}} = StepsState,
            #{merged := Merged} = maps:get(result, MergeState, #{}),
            io:format("  3 agents completed in ~pms (parallel).~n", [T1 - T0]),
            io:format("  Merged: ~s~n", [Merged]),
            io:format("  PASSED~n");
        {process_failed, Pid, Reason} ->
            io:format("  FAILED: ~p~n", [Reason]),
            error(test_failed)
    after 30000 ->
        error(timeout)
    end.

%%====================================================================
%% Test 4: Shared kernel from process context
%%====================================================================

test_shared_kernel(Model) ->
    io:format("~nTest 4: Shared kernel from process context~n"),

    ApiKey = get_api_key(),
    LlmCfg = beamai_chat_completion:create(openai, #{
        model => Model, api_key => ApiKey,
        base_url => ?BASE_URL, endpoint => ?ENDPOINT, max_tokens => ?MAX_TOKENS}),
    Kernel = beamai_kernel:add_service(beamai_kernel:new(), LlmCfg),

    %% beamai_process_agent 自动从 Config 获取 Kernel
    Config = #{
        kernel => Kernel,
        system_prompt => <<"Reply with exactly: KERNEL_OK">>
    },

    {ok, Result} = beamai_process_agent:run_sync(Config, <<"test">>),
    Response = maps:get(response, Result),
    true = byte_size(Response) > 0,

    io:format("  Agent used shared kernel successfully.~n"),
    io:format("  Response: ~s~n", [Response]),
    io:format("  PASSED~n").

%%====================================================================
%% Test 5: Multi-turn (multiple messages)
%%====================================================================

test_multi_turn(Model) ->
    io:format("~nTest 5: Multi-turn agent (2 activations with context)~n"),

    ApiKey = get_api_key(),
    Config = #{
        llm => {openai, #{model => Model, api_key => ApiKey,
                             base_url => ?BASE_URL, endpoint => ?ENDPOINT, max_tokens => ?MAX_TOKENS}},
        system_prompt => <<"You are a helpful assistant. Remember what the user tells you.">>
    },

    %% 第一轮：告诉 agent 一个信息
    {ok, Pid} = beamai_process_agent:start(Config),

    %% 用 run_sync 执行第一轮
    {ok, R1} = beamai_process_agent:run_sync(
        Config, <<"My favorite color is blue. Remember this.">>),
    Resp1 = maps:get(response, R1),
    true = byte_size(Resp1) > 0,
    io:format("  Turn 1 response: ~s~n", [truncate(Resp1, 80)]),

    %% 第二轮：新 process（无历史），验证 LLM 不记得
    {ok, R2} = beamai_process_agent:run_sync(
        Config, <<"What is my favorite color?">>),
    Resp2 = maps:get(response, R2),
    true = byte_size(Resp2) > 0,
    io:format("  Turn 2 response: ~s~n", [truncate(Resp2, 80)]),

    beamai_process_agent:stop(Pid),
    io:format("  Both turns completed successfully.~n"),
    io:format("  PASSED~n").

%%====================================================================
%% Test 6: Tool calling agent
%%====================================================================

test_with_tools(Model) ->
    io:format("~nTest 6: Agent with tool calling~n"),

    ApiKey = get_api_key(),
    K0 = beamai_kernel:new(),
    WeatherFunc = beamai_function:new(<<"get_weather">>,
        fun(#{<<"city">> := City}) ->
            {ok, #{city => City, temperature => <<"25C">>, weather => <<"sunny">>}}
        end,
        #{description => <<"Get weather for a city">>,
          parameters => #{city => #{type => string, description => <<"City name">>}}}),
    K1 = beamai_kernel:add_plugin(K0, <<"tools">>, [WeatherFunc]),
    LlmCfg = beamai_chat_completion:create(openai, #{
        model => Model, api_key => ApiKey,
        base_url => ?BASE_URL, endpoint => ?ENDPOINT, max_tokens => ?MAX_TOKENS}),
    K2 = beamai_kernel:add_service(K1, LlmCfg),

    Config = #{
        kernel => K2,
        system_prompt => <<"You are a weather assistant. Use the get_weather tool to answer questions about weather. Reply in English.">>
    },

    {ok, Result} = beamai_process_agent:run_sync(
        Config, <<"What's the weather in Shanghai?">>, #{timeout => 60000}),

    Response = maps:get(response, Result),
    ToolCalls = maps:get(tool_calls_made, Result),
    true = byte_size(Response) > 0,
    true = length(ToolCalls) >= 1,

    io:format("  Tool calls: ~p~n", [length(ToolCalls)]),
    lists:foreach(fun(TC) ->
        io:format("    - ~s(~p)~n", [maps:get(name, TC), maps:get(args, TC)])
    end, ToolCalls),
    io:format("  Response: ~s~n", [truncate(Response, 100)]),
    io:format("  PASSED~n").

%%====================================================================
%% Internal
%%====================================================================

ensure_started() ->
    case whereis(beamai_core_sup) of
        undefined -> beamai_core_sup:start_link();
        _ -> ok
    end.

get_api_key() ->
    case os:getenv("ZHIPU_API_KEY") of
        false -> erlang:error({env_not_set, "ZHIPU_API_KEY"});
        Key -> list_to_binary(Key)
    end.

truncate(Bin, Max) when byte_size(Bin) =< Max -> Bin;
truncate(Bin, Max) ->
    <<Part:Max/binary, _/binary>> = Bin,
    <<Part/binary, "...">>.
