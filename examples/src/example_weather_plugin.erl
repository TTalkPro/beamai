%%%-------------------------------------------------------------------
%%% @doc Weather Plugin 示例
%%%
%%% 演示如何定义 Plugin、注册到 Kernel 并通过 LLM 自动调用工具。
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_weather_plugin:run().
%%% ```
%%%
%%% 等价于 C# Semantic Kernel:
%%% ```csharp
%%% var kernel = Kernel.CreateBuilder()
%%%     .AddChatCompletion(...)
%%%     .Build();
%%% kernel.Plugins.AddFromType<WeatherPlugin>();
%%% var result = await kernel.InvokePromptAsync(
%%%     "What's the weather in Beijing?",
%%%     new(new OpenAIPromptExecutionSettings { ToolCallBehavior = auto }));
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_weather_plugin).

-export([run/0, run/1]).
-export([invoke_only/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 运行完整示例：LLM + 工具调用（使用 GLM-4.7 Anthropic 兼容 API）
-spec run() -> ok.
run() ->
    LLMConfig = example_llm_config:anthropic(),
    run(LLMConfig).

-spec run(beamai_chat_completion:config()) -> ok.
run(LLMConfig) ->
    io:format("=== BeamAI Weather Plugin Example ===~n~n"),

    %% 1. 构建 Kernel
    K0 = beamai:kernel(),

    %% 2. 定义 Weather Plugin
    K1 = beamai:add_plugin(K0, <<"weather">>, [
        beamai:function(<<"get_current_weather">>,
            fun(Args) -> get_weather(Args) end,
            #{description => <<"Get the current weather for a given city">>,
              parameters => #{
                  city => #{type => string, description => <<"City name, e.g. Beijing, Tokyo, New York">>, required => true}
              }})
    ]),

    %% 3. 添加 LLM 服务
    K2 = beamai:add_llm(K1, LLMConfig),

    %% 4. 查看注册的工具
    Tools = beamai:tools(K2, anthropic),
    io:format("Registered tools: ~p~n~n", [Tools]),

    %% 5. 使用 chat_with_tools 让 LLM 自动调用工具
    Messages = [
        #{role => system, content => <<"You are a helpful weather assistant. You MUST use the get_current_weather tool to answer any weather questions. Never guess the weather.">>},
        #{role => user, content => <<"What's the weather like in Beijing today?">>}
    ],
    io:format("User: What's the weather like in Beijing today?~n~n"),

    case beamai:chat_with_tools(K2, Messages) of
        {ok, #{content := Content}, _} ->
            io:format("Assistant: ~ts~n~n", [Content]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,

    %% 6. 再试一个多城市查询
    io:format("--- Multi-city query ---~n~n"),
    Messages2 = [
        #{role => system, content => <<"You are a helpful weather assistant. You MUST use the get_current_weather tool to answer any weather questions. Never guess the weather.">>},
        #{role => user, content => <<"Compare the weather in Tokyo and Shanghai">>}
    ],
    io:format("User: Compare the weather in Tokyo and Shanghai~n~n"),

    case beamai:chat_with_tools(K2, Messages2) of
        {ok, #{content := Content2}, _} ->
            io:format("Assistant: ~ts~n~n", [Content2]);
        {error, Reason2} ->
            io:format("Error: ~p~n", [Reason2])
    end,
    ok.

%% @doc 仅演示直接调用（不需要 LLM）
-spec invoke_only() -> ok.
invoke_only() ->
    io:format("=== Direct Invoke Example ===~n~n"),

    K0 = beamai:kernel(),
    K1 = beamai:add_plugin(K0, <<"weather">>, [
        beamai:function(<<"get_current_weather">>,
            fun(Args) -> get_weather(Args) end,
            #{description => <<"Get the current weather for a given city">>,
              parameters => #{
                  city => #{type => string, description => <<"City name">>, required => true}
              }})
    ]),

    %% 直接调用函数
    {ok, Result, _} = beamai:invoke(K1, <<"weather.get_current_weather">>, #{city => <<"Beijing">>}),
    io:format("Direct invoke result: ~p~n", [Result]),
    ok.

%%====================================================================
%% 内部函数 - 模拟天气 API
%%====================================================================

-spec get_weather(map()) -> {ok, map()}.
get_weather(Args) ->
    City = maps:get(city, Args, maps:get(<<"city">>, Args, <<"unknown">>)),
    io:format("  [Tool Called] get_current_weather(~ts)~n", [City]),
    %% 模拟天气数据
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
mock_weather(<<"New York">>) ->
    #{city => <<"New York">>, temperature => 22, condition => <<"Partly Cloudy">>, humidity => 60};
mock_weather(City) ->
    #{city => City, temperature => 20, condition => <<"Unknown">>, humidity => 50}.
