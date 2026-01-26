%%%-------------------------------------------------------------------
%%% @doc 工具重构后的测试示例
%%%
%%% 使用 GLM-4.7（智谱 AI）验证新的 tool 架构
%%%
%%% 使用方法：
%%%   1. 设置环境变量 ZHIPU_API_KEY
%%%   2. 在 Erlang shell 中运行:
%%%      example_tool_refactored:run().
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_tool_refactored).

-export([run/0, run/1]).

%% 测试用工具处理器
-export([get_weather/1, get_time/1]).

%%====================================================================
%% 公开 API
%%====================================================================

%% @doc 使用默认配置运行示例
run() ->
    ApiKey = os:getenv("ZHIPU_API_KEY"),
    case ApiKey of
        false ->
            io:format("Error: ZHIPU_API_KEY environment variable not set~n"),
            {error, no_api_key};
        _ ->
            run(#{api_key => list_to_binary(ApiKey)})
    end.

%% @doc 使用自定义配置运行示例
run(Opts) ->
    io:format("~n========================================~n"),
    io:format("   Tool Refactored Architecture Test~n"),
    io:format("========================================~n~n"),

    %% 1. 创建 Kernel
    io:format("[1] Creating Kernel...~n"),
    K0 = beamai:kernel(),
    io:format("    Kernel created~n~n"),

    %% 2. 添加工具（使用新的 API）
    io:format("[2] Adding tools...~n"),

    %% 方式一：直接创建工具
    WeatherTool = beamai:tool(<<"get_weather">>, fun ?MODULE:get_weather/1, #{
        description => <<"Get current weather for a city">>,
        tag => <<"weather">>,
        parameters => #{
            <<"city">> => #{
                type => string,
                description => <<"City name, e.g. Beijing, Shanghai">>,
                required => true
            }
        }
    }),

    TimeTool = beamai:tool(<<"get_time">>, fun ?MODULE:get_time/1, #{
        description => <<"Get current time for a timezone">>,
        tag => <<"time">>,
        parameters => #{
            <<"timezone">> => #{
                type => string,
                description => <<"Timezone, e.g. Asia/Shanghai, UTC">>,
                required => true
            }
        }
    }),

    K1 = beamai:add_tools(K0, [WeatherTool, TimeTool]),
    io:format("    Added 2 tools: get_weather, get_time~n~n"),

    %% 3. 查看注册的工具
    io:format("[3] Registered tools:~n"),
    Tools = beamai:tools(K1, openai),
    lists:foreach(fun(T) ->
        #{<<"function">> := #{<<"name">> := Name, <<"description">> := Desc}} = T,
        io:format("    - ~s: ~s~n", [Name, Desc])
    end, Tools),
    io:format("~n"),

    %% 4. 按 tag 查询工具
    io:format("[4] Tools by tag 'weather':~n"),
    WeatherTools = beamai:tools_by_tag(K1, <<"weather">>),
    lists:foreach(fun(#{name := Name}) ->
        io:format("    - ~s~n", [Name])
    end, WeatherTools),
    io:format("~n"),

    %% 5. 添加 LLM 服务（使用 GLM-4.7，兼容 OpenAI API）
    io:format("[5] Adding LLM service (GLM-4.7 via OpenAI provider)...~n"),
    ApiKey = maps:get(api_key, Opts),
    BaseUrl = maps:get(base_url, Opts, <<"https://open.bigmodel.cn/api/paas/v4">>),
    Model = maps:get(model, Opts, <<"glm-4-flash">>),

    K2 = beamai:add_llm(K1, openai, #{
        api_key => ApiKey,
        base_url => BaseUrl,
        model => Model
    }),
    io:format("    LLM configured: ~s~n~n", [Model]),

    %% 6. 使用 chat_with_tools 进行对话
    io:format("[6] Testing chat_with_tools...~n"),
    Messages = [
        #{role => system, content => <<"You are a helpful assistant. Use the provided tools to answer questions. Always respond in Chinese.">>},
        #{role => user, content => <<"北京现在的天气怎么样？">>}
    ],

    io:format("    User: 北京现在的天气怎么样？~n"),
    case beamai:chat_with_tools(K2, Messages) of
        {ok, #{content := Content}, _Context} ->
            io:format("    Assistant: ~ts~n~n", [Content]),
            io:format("[SUCCESS] Tool calling loop completed!~n");
        {error, Reason} ->
            io:format("    [ERROR] ~p~n~n", [Reason])
    end,

    io:format("~n========================================~n"),
    io:format("   Test completed!~n"),
    io:format("========================================~n"),
    ok.

%%====================================================================
%% 工具处理器
%%====================================================================

%% @doc 模拟获取天气
get_weather(Args) ->
    City = maps:get(<<"city">>, Args, <<"Unknown">>),
    io:format("    [Tool] get_weather(~ts) called~n", [City]),

    %% 模拟天气数据
    Weather = case City of
        <<"Beijing">> -> #{city => City, temp => 15, condition => <<"Sunny">>, humidity => 40};
        <<"北京">> -> #{city => City, temp => 15, condition => <<"晴天">>, humidity => 40};
        <<"Shanghai">> -> #{city => City, temp => 20, condition => <<"Cloudy">>, humidity => 65};
        <<"上海">> -> #{city => City, temp => 20, condition => <<"多云">>, humidity => 65};
        _ -> #{city => City, temp => 18, condition => <<"Unknown">>, humidity => 50}
    end,
    {ok, Weather}.

%% @doc 模拟获取时间
get_time(Args) ->
    Timezone = maps:get(<<"timezone">>, Args, <<"UTC">>),
    io:format("    [Tool] get_time(~ts) called~n", [Timezone]),

    %% 返回当前时间
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    TimeStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S]),
    {ok, #{timezone => Timezone, time => iolist_to_binary(TimeStr)}}.
