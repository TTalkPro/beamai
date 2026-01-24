%%%-------------------------------------------------------------------
%%% @doc Process Framework 工具调用示例
%%%
%%% 演示 Process Framework 中如何在步骤里使用 LLM 工具调用：
%%%   1. 单步工具调用（天气查询）
%%%   2. 多步工具管线（查询 -> 分析 -> 建议）
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_process_tools:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_process_tools).

-export([run/0, run/1]).
-export([run_pipeline/0, run_pipeline/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 运行单步工具调用示例
-spec run() -> ok.
run() ->
    run(example_llm_config:anthropic()).

-spec run(beamai_chat_completion:config()) -> ok.
run(LLMConfig) ->
    io:format("~n=== Process Framework: 工具调用示例 ===~n~n"),

    ensure_started(),

    %% 构建带工具的 Kernel
    Kernel = build_weather_kernel(LLMConfig),
    Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

    %% 构建 Process: 单步工具调用
    P0 = beamai_process:builder(tool_call_demo),
    P1 = beamai_process:add_step(P0, weather_query, example_process_steps, #{
        type => llm_tool_call,
        system_prompt => <<"You are a weather assistant. You MUST use the get_current_weather tool to answer weather questions. Answer in Chinese."/utf8>>,
        output_event => weather_result,
        plugins => weather_plugins()
    }),
    P2 = beamai_process:on_event(P1, ask_weather, weather_query, input),
    P3 = beamai_process:set_initial_event(P2, ask_weather,
        #{user_message => <<"北京今天天气怎么样？"/utf8>>}),
    P4 = beamai_process:set_execution_mode(P3, sequential),

    io:format("用户: 北京今天天气怎么样？~n~n"),
    {ok, Def} = beamai_process:build(P4),
    case beamai_process:run_sync(Def, #{timeout => 60000, context => Context}) of
        {ok, _Result} ->
            io:format("~n工具调用完成!~n~n");
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,
    ok.

%% @doc 运行多步工具管线示例
%%
%% 管线: 查询天气 -> 分析数据 -> 生成建议
-spec run_pipeline() -> ok.
run_pipeline() ->
    run_pipeline(example_llm_config:anthropic()).

-spec run_pipeline(beamai_chat_completion:config()) -> ok.
run_pipeline(LLMConfig) ->
    io:format("~n=== Process Framework: 工具管线示例 ===~n~n"),
    io:format("管线: 查询天气 -> 分析数据 -> 生成出行建议~n~n"),

    ensure_started(),

    Kernel = build_weather_kernel(LLMConfig),
    Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

    %% Step 1: 查询多城市天气
    P0 = beamai_process:builder(weather_pipeline),
    P1 = beamai_process:add_step(P0, weather_fetcher, example_process_steps, #{
        type => llm_tool_call,
        system_prompt => <<"You are a weather data collector. Use the get_current_weather tool to get weather for ALL cities mentioned. Report the raw data in a structured format.">>,
        output_event => weather_data,
        plugins => weather_plugins()
    }),

    %% Step 2: 分析天气数据
    P2 = beamai_process:add_step(P1, analyzer, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"你是一个天气分析专家。根据输入的天气数据，分析各城市的天气状况差异，用中文回答。简洁明了，不超过3句话。"/utf8>>,
        output_event => analysis_done
    }),

    %% Step 3: 生成建议
    P3 = beamai_process:add_step(P2, advisor, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"你是一个出行建议助手。根据天气分析结果，给出简短的出行建议。用中文回答，不超过2句话。"/utf8>>,
        output_event => advice_done
    }),

    %% 绑定
    P4 = beamai_process:on_event(P3, start, weather_fetcher, input),
    P5 = beamai_process:on_event(P4, weather_data, analyzer, input,
        fun(#{response := Resp}) -> #{user_message => Resp} end),
    P6 = beamai_process:on_event(P5, analysis_done, advisor, input,
        fun(#{response := Resp}) -> #{user_message => Resp} end),

    %% 初始事件
    Query = <<"Compare the weather in Beijing, Shanghai and Tokyo">>,
    io:format("查询: ~ts~n~n", [Query]),
    P7 = beamai_process:set_initial_event(P6, start, #{user_message => Query}),
    P8 = beamai_process:set_execution_mode(P7, sequential),

    {ok, Def} = beamai_process:build(P8),
    case beamai_process:run_sync(Def, #{timeout => 120000, context => Context}) of
        {ok, _Result} ->
            io:format("~n管线完成!~n~n");
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% Internal
%%====================================================================

ensure_started() ->
    case whereis(beamai_core_sup) of
        undefined ->
            {ok, _} = beamai_core_sup:start_link(),
            ok;
        _ ->
            ok
    end.

build_weather_kernel(LLMConfig) ->
    K0 = beamai:kernel(),
    K1 = beamai:add_plugin(K0, <<"weather">>, [
        beamai:function(<<"get_current_weather">>,
            fun(Args) -> get_weather(Args) end,
            #{description => <<"Get the current weather for a given city">>,
              parameters => #{
                  city => #{type => string,
                           description => <<"City name, e.g. Beijing, Tokyo, New York">>,
                           required => true}
              }})
    ]),
    beamai:add_llm(K1, LLMConfig).

weather_plugins() ->
    [{<<"weather">>, [
        beamai:function(<<"get_current_weather">>,
            fun(Args) -> get_weather(Args) end,
            #{description => <<"Get the current weather for a given city">>,
              parameters => #{
                  city => #{type => string,
                           description => <<"City name, e.g. Beijing, Tokyo, New York">>,
                           required => true}
              }})
    ]}].

get_weather(Args) ->
    City = maps:get(city, Args, maps:get(<<"city">>, Args, <<"unknown">>)),
    io:format("  [Tool] get_current_weather(~ts)~n", [City]),
    Weather = mock_weather(City),
    {ok, Weather}.

mock_weather(<<"Beijing">>) ->
    #{city => <<"Beijing">>, temperature => 28, condition => <<"Sunny">>, humidity => 45};
mock_weather(<<"北京"/utf8>>) ->
    #{city => <<"Beijing">>, temperature => 28, condition => <<"Sunny">>, humidity => 45};
mock_weather(<<"Shanghai">>) ->
    #{city => <<"Shanghai">>, temperature => 32, condition => <<"Cloudy">>, humidity => 75};
mock_weather(<<"上海"/utf8>>) ->
    #{city => <<"Shanghai">>, temperature => 32, condition => <<"Cloudy">>, humidity => 75};
mock_weather(<<"Tokyo">>) ->
    #{city => <<"Tokyo">>, temperature => 26, condition => <<"Rainy">>, humidity => 80};
mock_weather(<<"东京"/utf8>>) ->
    #{city => <<"Tokyo">>, temperature => 26, condition => <<"Rainy">>, humidity => 80};
mock_weather(City) ->
    #{city => City, temperature => 20, condition => <<"Unknown">>, humidity => 50}.
