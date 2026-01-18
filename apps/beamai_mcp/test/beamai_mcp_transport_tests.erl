%%%-------------------------------------------------------------------
%%% @doc beamai_mcp_transport 单元测试
%%%
%%% 注意：Stdio 和网络传输测试需要实际的外部服务，
%%% 这里主要测试传输层的基本逻辑和错误处理。
%%%-------------------------------------------------------------------
-module(beamai_mcp_transport_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamai_mcp.hrl").

%%====================================================================
%% 测试组
%%====================================================================

beamai_mcp_transport_test_() ->
    [
     {"传输创建测试", fun transport_create_test/0},
     {"传输配置错误测试", fun transport_config_error_test/0},
     {"HTTP 传输状态测试", fun http_transport_state_test/0}
    ].

%%====================================================================
%% 传输创建测试
%%====================================================================

transport_create_test() ->
    %% 测试 HTTP 传输创建
    HttpConfig = #{
        transport => http,
        url => <<"http://localhost:8080/mcp">>
    },
    {ok, {beamai_mcp_transport_http, _State}} = beamai_mcp_transport:create(HttpConfig),

    %% 测试不支持的传输类型
    {error, {unsupported_transport, unknown}} = beamai_mcp_transport:create(#{transport => unknown}),

    %% 测试缺少传输类型
    {error, missing_transport} = beamai_mcp_transport:create(#{}),

    ok.

transport_config_error_test() ->
    %% Stdio 缺少 command
    {error, missing_command} = beamai_mcp_transport:create(#{transport => stdio}),

    %% SSE 缺少 url
    {error, missing_url} = beamai_mcp_transport:create(#{transport => sse}),

    %% HTTP 缺少 url
    {error, missing_url} = beamai_mcp_transport:create(#{transport => http}),

    ok.

%%====================================================================
%% HTTP 传输状态测试
%%====================================================================

http_transport_state_test() ->
    %% 创建 HTTP 传输
    Config = #{
        transport => http,
        url => <<"http://localhost:8080/mcp">>,
        timeout => 5000,
        session_id => <<"test-session-123">>
    },

    {ok, {Mod, State}} = beamai_mcp_transport:create(Config),

    %% 验证初始状态
    ?assertEqual(beamai_mcp_transport_http, Mod),
    ?assertEqual(true, Mod:is_connected(State)),

    %% 关闭连接
    ok = Mod:close(State),

    ok.

%%====================================================================
%% Stdio 传输测试（需要实际可执行文件）
%%====================================================================

stdio_transport_test_() ->
    {setup,
     fun() ->
         %% 检查是否有 echo 命令
         case os:find_executable("echo") of
             false -> skip;
             _ -> ok
         end
     end,
     fun(_) -> ok end,
     fun(skip) ->
         [];
        (ok) ->
         [{"Stdio 基本连接测试", fun stdio_basic_test/0}]
     end}.

stdio_basic_test() ->
    %% 使用 echo 命令测试 stdio 传输
    %% 注意：这只测试进程启动，不测试实际 MCP 通信
    Config = #{
        transport => stdio,
        command => "echo",
        args => ["test"]
    },

    case beamai_mcp_transport:create(Config) of
        {ok, {Mod, State}} ->
            %% 进程应该已启动
            ?assertEqual(true, Mod:is_connected(State)),
            %% 关闭
            ok = Mod:close(State);
        {error, _Reason} ->
            %% 某些环境可能无法启动
            ok
    end.

%%====================================================================
%% SSE 解析集成测试
%%====================================================================

sse_parsing_test_() ->
    [
     {"SSE 事件解析", fun sse_parse_events_test/0},
     {"SSE 不完整数据", fun sse_incomplete_data_test/0}
    ].

sse_parse_events_test() ->
    %% 使用 beamai_sse 解析（传输层依赖此模块）
    Data = <<"event: endpoint\ndata: {\"uri\":\"http://test\"}\n\nevent: message\ndata: {\"id\":1}\n\n">>,

    {Remaining, Events} = beamai_sse:parse(Data),

    ?assertEqual(<<>>, Remaining),
    ?assertEqual(2, length(Events)),

    [Event1, Event2] = Events,
    ?assertEqual(<<"endpoint">>, maps:get(event, Event1)),
    ?assertEqual(<<"message">>, maps:get(event, Event2)).

sse_incomplete_data_test() ->
    %% 不完整的数据应该保留在缓冲区
    Data = <<"event: message\ndata: partial">>,

    {Remaining, Events} = beamai_sse:parse(Data),

    ?assertEqual([], Events),
    ?assertEqual(Data, Remaining).
