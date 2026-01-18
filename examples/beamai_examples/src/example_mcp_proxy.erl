%%%-------------------------------------------------------------------
%%% @doc MCP Tool Proxy 使用示例
%%%
%%% 演示如何使用 beamai_mcp_tool_proxy 连接 MCP 服务器，
%%% 并将 MCP 工具集成到 beamai_agent 中。
%%%
%%% 运行方式:
%%%   rebar3 shell
%%%   > mcp_tool_proxy_example:run().
%%%   > mcp_tool_proxy_example:run_filesystem_example().
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_mcp_proxy).

-export([
    run/0,
    run_filesystem_example/0,
    run_local_tool_example/0,
    run_multiple_servers_example/0
]).

-include_lib("beamai_mcp/include/beamai_mcp.hrl").

%%====================================================================
%% 主入口
%%====================================================================

%% @doc 运行所有示例
-spec run() -> ok.
run() ->
    io:format("~n=== MCP Tool Proxy 示例 ===~n~n"),

    %% 运行示例
    run_all_examples(),

    ok.

%%====================================================================
%% 示例 1：连接文件系统 MCP 服务器
%%====================================================================

run_filesystem_example() ->
    io:format("~n=== 示例 1: 文件系统 MCP 服务器 ===~n~n"),

    %% 检查是否安装了 Node.js 和 npx
    case check_npx_available() of
        false ->
            io:format("跳过: 未找到 npx 命令（需要安装 Node.js）~n"),
            {error, npx_not_found};
        true ->
            run_filesystem_example_impl()
    end.

run_filesystem_example_impl() ->
    io:format("启动 MCP 文件系统服务器代理...~n"),

    %% 启动 MCP 工具代理
    {ok, Proxy} = beamai_mcp_tool_proxy:start_link(#{
        transport => stdio,
        command => "npx",
        args => ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
        client_info => #{
            <<"name">> => <<"erlang-agent">>,
            <<"version">> => <<"1.0.0">>
        }
    }),

    io:format("等待工具加载...~n"),
    timer:sleep(3000),

    %% 获取工具列表
    case beamai_mcp_tool_proxy:get_tools(Proxy) of
        {ok, Tools} ->
            io:format("成功加载 ~p 个工具:~n", [length(Tools)]),
            lists:foreach(fun(Tool) ->
                Name = maps:get(name, Tool),
                Desc = maps:get(description, Tool),
                io:format("  - ~s: ~s~n", [Name, Desc])
            end, Tools),

            %% 测试工具调用
            test_filesystem_tools(Tools),

            %% 停止代理
            beamai_mcp_tool_proxy:stop(Proxy),
            ok;
        {error, Reason} ->
            io:format("加载工具失败: ~p~n", [Reason]),
            beamai_mcp_tool_proxy:stop(Proxy),
            {error, Reason}
    end.

test_filesystem_tools(Tools) ->
    io:format("~n测试工具调用:~n"),

    %% 查找 read_file 工具
    case lists:search(fun(T) -> maps:get(name, T) =:= <<"read_file">> end, Tools) of
        {value, ReadFileTool} ->
            io:format("  测试 read_file 工具...~n"),
            Handler = maps:get(handler, ReadFileTool),

            %% 创建测试文件
            TestFile = "/tmp/mcp_test.txt",
            file:write_file(TestFile, <<"Hello from MCP!">>),

            %% 调用工具
            Result = Handler(#{<<"path">> => list_to_binary(TestFile)}),
            io:format("  结果: ~s~n", [Result]),

            %% 清理
            file:delete(TestFile);
        false ->
            io:format("  未找到 read_file 工具~n")
    end.

%%====================================================================
%% 示例 2：本地 MCP 工具转换
%%====================================================================

run_local_tool_example() ->
    io:format("~n=== 示例 2: 本地 MCP 工具转换 ===~n~n"),

    %% 定义本地 MCP 工具
    io:format("定义本地 MCP 工具...~n"),

    CalculatorTool = beamai_mcp_types:make_tool(
        <<"calculate">>,
        <<"Simple calculator">>,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"expression">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Math expression to evaluate">>
                }
            },
            <<"required">> => [<<"expression">>]
        },
        fun(#{<<"expression">> := Expr}) ->
            Result = calculate_expression(Expr),
            {ok, [beamai_mcp_types:text_content(Result)]}
        end
    ),

    EchoTool = beamai_mcp_types:make_tool(
        <<"echo">>,
        <<"Echo the input">>,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"text">> => #{<<"type">> => <<"string">>}
            }
        },
        fun(#{<<"text">> := Text}) ->
            {ok, [beamai_mcp_types:text_content(<<"Echo: ", Text/binary>>)]}
        end
    ),

    %% 转换为 beamai_agent 工具
    io:format("转换为 beamai_agent 工具...~n"),
    AgentTools = beamai_mcp_adapter:mcp_tools_to_beamai_tools([CalculatorTool, EchoTool]),

    io:format("成功转换 ~p 个工具~n", [length(AgentTools)]),

    %% 测试工具
    test_local_tools(AgentTools),

    ok.

test_local_tools(Tools) ->
    io:format("~n测试本地工具:~n"),

    %% 测试 calculator
    [CalcTool | _] = Tools,
    CalcHandler = maps:get(handler, CalcTool),
    CalcResult = CalcHandler(#{<<"expression">> => <<"2 + 2">>}),
    io:format("  calculate(2 + 2) = ~s~n", [CalcResult]),

    %% 测试 echo
    [_, EchoTool] = Tools,
    EchoHandler = maps:get(handler, EchoTool),
    EchoResult = EchoHandler(#{<<"text">> => <<"Hello">>}),
    io:format("  echo(Hello) = ~s~n", [EchoResult]),

    ok.

%%====================================================================
%% 示例 3：连接多个 MCP 服务器
%%====================================================================

run_multiple_servers_example() ->
    io:format("~n=== 示例 3: 连接多个 MCP 服务器 ===~n~n"),

    case check_npx_available() of
        false ->
            io:format("跳过: 未找到 npx 命令~n"),
            {error, npx_not_found};
        true ->
            run_multiple_servers_impl()
    end.

run_multiple_servers_impl() ->
    io:format("启动多个 MCP 服务器代理...~n"),

    %% 启动文件系统代理
    {ok, FsProxy} = beamai_mcp_tool_proxy:start_link(fs_proxy, #{
        transport => stdio,
        command => "npx",
        args => ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
    }),

    %% 这里可以启动更多服务器...
    %% {ok, GitProxy} = beamai_mcp_tool_proxy:start_link(git_proxy, #{...}),

    timer:sleep(3000),

    %% 获取所有工具
    {ok, FsTools} = beamai_mcp_tool_proxy:get_tools(FsProxy),
    %% {ok, GitTools} = beamai_mcp_tool_proxy:get_tools(GitProxy),

    AllTools = FsTools, %% ++ GitTools,

    io:format("总共加载了 ~p 个工具~n", [length(AllTools)]),

    %% 停止代理
    beamai_mcp_tool_proxy:stop(FsProxy),

    ok.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 检查 npx 是否可用
check_npx_available() ->
    case os:find_executable("npx") of
        false -> false;
        _ -> true
    end.

%% @private 简单计算器实现
calculate_expression(<<"2 + 2">>) -> <<"4">>;
calculate_expression(<<"10 * 5">>) -> <<"50">>;
calculate_expression(Expr) ->
    <<"Cannot calculate: ", Expr/binary>>.

%% @private 运行所有示例
run_all_examples() ->
    Examples = [
        {"本地工具转换", fun run_local_tool_example/0},
        {"文件系统服务器", fun run_filesystem_example/0},
        {"多服务器连接", fun run_multiple_servers_example/0}
    ],

    lists:foreach(fun({Name, Fun}) ->
        try
            Fun(),
            io:format("~n✓ ~s 示例完成~n", [Name])
        catch
            Class:Reason:Stack ->
                io:format("~n✗ ~s 示例失败: ~p:~p~n", [Name, Class, Reason]),
                io:format("堆栈: ~p~n", [Stack])
        end
    end, Examples),

    ok.
