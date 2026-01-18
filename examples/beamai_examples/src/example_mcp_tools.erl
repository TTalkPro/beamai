%%%-------------------------------------------------------------------
%%% @doc Agent 集成 MCP 工具完整示例
%%%
%%% 演示如何创建一个具有 MCP 工具能力的 beamai_agent。
%%%
%%% 运行方式:
%%%   rebar3 shell
%%%   > c("examples/beamai_with_mcp_tools.erl").
%%%   > beamai_with_mcp_tools:run().
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_mcp_tools).

-export([
    run/0,
    run_with_filesystem/0,
    run_with_local_tools/0
]).

-include_lib("apps/beamai_mcp/include/beamai_mcp.hrl").

%%====================================================================
%% 示例 1：Agent + 文件系统 MCP 工具
%%====================================================================

run_with_filesystem() ->
    io:format("~n=== Agent 集成文件系统 MCP 工具 ===~n~n"),

    %% 1. 启动 MCP 工具代理
    io:format("1. 启动 MCP 文件系统服务器代理...~n"),
    {ok, Proxy} = beamai_mcp_tool_proxy:start_link(#{
        transport => stdio,
        command => "npx",
        args => ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
    }),

    %% 2. 等待工具加载
    io:format("2. 等待工具加载...~n"),
    timer:sleep(3000),

    %% 3. 获取 MCP 工具
    io:format("3. 获取 MCP 工具列表...~n"),
    {ok, McpTools} = beamai_mcp_tool_proxy:get_tools(Proxy),
    io:format("   成功加载 ~p 个工具~n", [length(McpTools)]),

    %% 4. 配置 LLM（使用 GLM-4.7）
    LLMConfig = get_llm_config(),

    %% 5. 创建 Agent（带 MCP 工具）
    io:format("4. 创建 Agent（集成 MCP 工具）...~n"),
    {ok, Agent} = beamai_agent:start_link(<<"fs_agent">>, #{
        system_prompt => <<"You are a helpful assistant with file system access. "
                          "You can read, write, and list files in /tmp directory.">>,
        llm => LLMConfig,
        tools => McpTools,
        max_iterations => 10
    }),

    io:format("5. Agent 已就绪！~n~n"),

    %% 6. 测试对话
    io:format("=== 测试 Agent ===~n~n"),

    %% 创建测试文件
    TestFile = "/tmp/agent_test.txt",
    TestContent = <<"This is a test file created by agent.\nIt has multiple lines.\n">>,
    file:write_file(TestFile, TestContent),

    %% 测试 1：读取文件
    io:format("测试 1: 让 Agent 读取文件~n"),
    {ok, Result1} = beamai_agent:run(Agent,
        <<"Read the file /tmp/agent_test.txt and tell me its content.">>),
    io:format("Agent 响应: ~s~n~n", [maps:get(response, Result1, <<"no response">>)]),

    %% 测试 2：列出文件
    io:format("测试 2: 让 Agent 列出目录~n"),
    {ok, Result2} = beamai_agent:run(Agent,
        <<"List all .txt files in /tmp directory.">>),
    io:format("Agent 响应: ~s~n~n", [maps:get(response, Result2, <<"no response">>)]),

    %% 清理
    file:delete(TestFile),
    beamai_agent:stop(Agent),
    beamai_mcp_tool_proxy:stop(Proxy),

    io:format("✓ 示例完成~n"),
    ok.

%%====================================================================
%% 示例 2：Agent + 本地 MCP 工具
%%====================================================================

run_with_local_tools() ->
    io:format("~n=== Agent 集成本地 MCP 工具 ===~n~n"),

    %% 1. 定义本地 MCP 工具
    io:format("1. 定义本地 MCP 工具...~n"),

    %% 天气查询工具
    WeatherTool = beamai_mcp_types:make_tool(
        <<"get_weather">>,
        <<"Get weather information for a city">>,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"city">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"City name">>
                }
            },
            <<"required">> => [<<"city">>]
        },
        fun(#{<<"city">> := City}) ->
            %% 模拟天气数据
            Weather = mock_weather(City),
            {ok, [beamai_mcp_types:text_content(Weather)]}
        end
    ),

    %% 计算器工具
    CalculatorTool = beamai_mcp_types:make_tool(
        <<"calculate">>,
        <<"Perform mathematical calculations">>,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"operation">> => #{
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>]
                },
                <<"a">> => #{<<"type">> => <<"number">>},
                <<"b">> => #{<<"type">> => <<"number">>}
            }
        },
        fun(Args) ->
            Result = perform_calculation(Args),
            {ok, [beamai_mcp_types:text_content(Result)]}
        end
    ),

    %% 2. 转换为 beamai_agent 工具
    io:format("2. 转换为 beamai_agent 工具...~n"),
    AgentTools = beamai_mcp_adapter:mcp_tools_to_beamai_tools([WeatherTool, CalculatorTool]),

    %% 3. 配置 LLM
    LLMConfig = get_llm_config(),

    %% 4. 创建 Agent
    io:format("3. 创建 Agent...~n"),
    {ok, Agent} = beamai_agent:start_link(<<"tool_agent">>, #{
        system_prompt => <<"You are a helpful assistant with weather and calculation tools.">>,
        llm => LLMConfig,
        tools => AgentTools,
        max_iterations => 10
    }),

    io:format("4. Agent 已就绪！~n~n"),

    %% 5. 测试对话
    io:format("=== 测试 Agent ===~n~n"),

    %% 测试 1：天气查询
    io:format("测试 1: 天气查询~n"),
    {ok, Result1} = beamai_agent:run(Agent,
        <<"What's the weather like in Beijing?">>),
    io:format("Agent 响应: ~s~n~n", [maps:get(response, Result1, <<"no response">>)]),

    %% 测试 2：计算
    io:format("测试 2: 数学计算~n"),
    {ok, Result2} = beamai_agent:run(Agent,
        <<"Calculate 15 multiplied by 8">>),
    io:format("Agent 响应: ~s~n~n", [maps:get(response, Result2, <<"no response">>)]),

    %% 清理
    beamai_agent:stop(Agent),

    io:format("✓ 示例完成~n"),
    ok.

%%====================================================================
%% 主入口
%%====================================================================

run() ->
    io:format("~n╔════════════════════════════════════════════════╗~n"),
    io:format("║   Agent 集成 MCP 工具完整示例                 ║~n"),
    io:format("╚════════════════════════════════════════════════╝~n"),

    %% 启动应用
    io:format("~n启动必要的应用...~n"),
    ensure_apps_started(),

    %% 运行示例
    Examples = [
        {"本地 MCP 工具", fun run_with_local_tools/0},
        {"文件系统 MCP 工具", fun run_with_filesystem/0}
    ],

    lists:foreach(fun({Name, Fun}) ->
        io:format("~n~n"),
        io:format("════════════════════════════════════════════════~n"),
        io:format(" ~s~n", [Name]),
        io:format("════════════════════════════════════════════════~n"),
        try
            Fun()
        catch
            Class:Reason:Stack ->
                io:format("✗ 示例失败: ~p:~p~n", [Class, Reason]),
                io:format("堆栈: ~p~n", [Stack])
        end
    end, Examples),

    io:format("~n~n╔════════════════════════════════════════════════╗~n"),
    io:format("║   所有示例完成！                              ║~n"),
    io:format("╚════════════════════════════════════════════════╝~n~n"),

    ok.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 获取 LLM 配置
get_llm_config() ->
    case example_utils:get_llm_config() of
        {ok, Config} -> Config;
        {error, _} ->
            io:format("警告: 未设置 ZHIPU_API_KEY，使用 mock LLM~n"),
            #{provider => mock}
    end.

%% @private 启动必要的应用
ensure_apps_started() ->
    Apps = [crypto, asn1, public_key, ssl, jsx, hackney, uuid, esqlite],
    lists:foreach(fun(App) ->
        case application:ensure_all_started(App) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok;
            {error, Reason} ->
                io:format("警告: 启动 ~p 失败: ~p~n", [App, Reason])
        end
    end, Apps),

    %% 启动 agent 应用
    case application:ensure_all_started(beamai_runtime) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        _ -> ok
    end.

%% @private 模拟天气数据
mock_weather(<<"Beijing">>) ->
    <<"Weather in Beijing: Sunny, 25°C, Light breeze">>;
mock_weather(<<"Shanghai">>) ->
    <<"Weather in Shanghai: Cloudy, 28°C, Humid">>;
mock_weather(<<"New York">>) ->
    <<"Weather in New York: Rainy, 18°C, Windy">>;
mock_weather(City) ->
    <<"Weather in ", City/binary, ": Data not available">>.

%% @private 执行计算
perform_calculation(#{<<"operation">> := <<"add">>, <<"a">> := A, <<"b">> := B}) ->
    Result = A + B,
    format_number(Result);
perform_calculation(#{<<"operation">> := <<"subtract">>, <<"a">> := A, <<"b">> := B}) ->
    Result = A - B,
    format_number(Result);
perform_calculation(#{<<"operation">> := <<"multiply">>, <<"a">> := A, <<"b">> := B}) ->
    Result = A * B,
    format_number(Result);
perform_calculation(#{<<"operation">> := <<"divide">>, <<"a">> := A, <<"b">> := B}) when B =/= 0 ->
    Result = A / B,
    format_number(Result);
perform_calculation(#{<<"operation">> := <<"divide">>, <<"b">> := 0}) ->
    <<"Error: Division by zero">>;
perform_calculation(_) ->
    <<"Error: Invalid calculation parameters">>.

%% @private 格式化数字
format_number(N) when is_integer(N) ->
    integer_to_binary(N);
format_number(N) when is_float(N) ->
    float_to_binary(N, [{decimals, 2}]).
