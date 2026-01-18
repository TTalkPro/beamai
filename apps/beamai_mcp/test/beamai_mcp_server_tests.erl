%%%-------------------------------------------------------------------
%%% @doc beamai_mcp_server 单元测试
%%%-------------------------------------------------------------------
-module(beamai_mcp_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamai_mcp.hrl").

%%====================================================================
%% 测试组
%%====================================================================

beamai_mcp_server_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_initialize/1,
      fun test_ping/1,
      fun test_tools_list/1,
      fun test_tools_call/1,
      fun test_tools_call_error/1,
      fun test_resources_list/1,
      fun test_resources_read/1,
      fun test_prompts_list/1,
      fun test_prompts_get/1,
      fun test_register_unregister/1,
      fun test_uninitialized_error/1
     ]}.

setup() ->
    %% 创建测试工具
    EchoTool = #mcp_tool{
        name = <<"echo">>,
        description = <<"Echo the input">>,
        input_schema = #{<<"type">> => <<"object">>, <<"properties">> => #{<<"text">> => #{<<"type">> => <<"string">>}}},
        handler = fun(#{<<"text">> := Text}) -> {ok, Text};
                     (_) -> {error, missing_text}
                  end
    },

    AddTool = #mcp_tool{
        name = <<"add">>,
        description = <<"Add two numbers">>,
        input_schema = #{<<"type">> => <<"object">>},
        handler = fun(#{<<"a">> := A, <<"b">> := B}) -> {ok, A + B};
                     (_) -> {error, invalid_params}
                  end
    },

    FailTool = #mcp_tool{
        name = <<"fail">>,
        description = <<"Always fails">>,
        input_schema = #{},
        handler = fun(_) -> error(intentional_crash) end
    },

    %% 创建测试资源
    TestResource = #mcp_resource{
        uri = <<"file:///test.txt">>,
        name = <<"test_file">>,
        description = <<"A test file">>,
        mime_type = <<"text/plain">>,
        handler = fun() -> {ok, <<"Hello, World!">>} end
    },

    JsonResource = #mcp_resource{
        uri = <<"data://config.json">>,
        name = <<"config">>,
        description = <<"Config data">>,
        mime_type = <<"application/json">>,
        handler = fun() -> {ok, #{<<"uri">> => <<"data://config.json">>, <<"text">> => <<"{\"key\":\"value\"}">>}} end
    },

    %% 创建测试提示
    GreetPrompt = #mcp_prompt{
        name = <<"greet">>,
        description = <<"A greeting prompt">>,
        arguments = [
            #mcp_prompt_arg{name = <<"name">>, description = <<"User name">>, required = true}
        ],
        handler = fun(#{<<"name">> := Name}) ->
            {ok, [#{<<"role">> => <<"user">>, <<"content">> => #{<<"type">> => <<"text">>, <<"text">> => <<"Hello, ", Name/binary, "!">>}}]}
        end
    },

    %% 启动服务器
    Config = #{
        tools => [EchoTool, AddTool, FailTool],
        resources => [TestResource, JsonResource],
        prompts => [GreetPrompt],
        server_info => #{<<"name">> => <<"test-server">>, <<"version">> => <<"1.0.0">>}
    },
    {ok, Pid} = beamai_mcp_server:start_link(Config),
    Pid.

cleanup(Pid) ->
    beamai_mcp_server:stop(Pid).

%%====================================================================
%% 初始化测试
%%====================================================================

test_initialize(Pid) ->
    {"初始化测试", fun() ->
        %% 构造初始化请求
        InitRequest = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"method">> => <<"initialize">>,
            <<"params">> => #{
                <<"protocolVersion">> => <<"2025-11-25">>,
                <<"capabilities">> => #{},
                <<"clientInfo">> => #{<<"name">> => <<"test-client">>, <<"version">> => <<"1.0">>}
            }
        },

        {ok, Response} = beamai_mcp_server:handle_request(Pid, InitRequest),
        {ok, Decoded} = beamai_mcp_jsonrpc:decode(Response),

        %% 验证响应
        ?assertEqual(1, maps:get(<<"id">>, Decoded)),
        Result = maps:get(<<"result">>, Decoded),
        ?assertEqual(<<"2025-11-25">>, maps:get(<<"protocolVersion">>, Result)),
        ?assert(maps:is_key(<<"capabilities">>, Result)),
        ?assert(maps:is_key(<<"serverInfo">>, Result)),

        ServerInfo = maps:get(<<"serverInfo">>, Result),
        ?assertEqual(<<"test-server">>, maps:get(<<"name">>, ServerInfo))
    end}.

test_ping(Pid) ->
    {"Ping 测试", fun() ->
        %% 先初始化
        initialize_server(Pid),

        %% 发送 ping
        PingRequest = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 2,
            <<"method">> => <<"ping">>,
            <<"params">> => #{}
        },

        {ok, Response} = beamai_mcp_server:handle_request(Pid, PingRequest),
        {ok, Decoded} = beamai_mcp_jsonrpc:decode(Response),

        ?assertEqual(2, maps:get(<<"id">>, Decoded)),
        ?assertEqual(#{}, maps:get(<<"result">>, Decoded))
    end}.

%%====================================================================
%% 工具测试
%%====================================================================

test_tools_list(Pid) ->
    {"工具列表测试", fun() ->
        initialize_server(Pid),

        Request = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 3,
            <<"method">> => <<"tools/list">>,
            <<"params">> => #{}
        },

        {ok, Response} = beamai_mcp_server:handle_request(Pid, Request),
        {ok, Decoded} = beamai_mcp_jsonrpc:decode(Response),

        Result = maps:get(<<"result">>, Decoded),
        Tools = maps:get(<<"tools">>, Result),
        ?assertEqual(3, length(Tools)),

        %% 验证工具名称
        ToolNames = [maps:get(<<"name">>, T) || T <- Tools],
        ?assert(lists:member(<<"echo">>, ToolNames)),
        ?assert(lists:member(<<"add">>, ToolNames)),
        ?assert(lists:member(<<"fail">>, ToolNames))
    end}.

test_tools_call(Pid) ->
    {"工具调用测试", fun() ->
        initialize_server(Pid),

        %% 调用 echo 工具
        EchoRequest = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 4,
            <<"method">> => <<"tools/call">>,
            <<"params">> => #{
                <<"name">> => <<"echo">>,
                <<"arguments">> => #{<<"text">> => <<"Hello!">>}
            }
        },

        {ok, EchoResponse} = beamai_mcp_server:handle_request(Pid, EchoRequest),
        {ok, EchoDecoded} = beamai_mcp_jsonrpc:decode(EchoResponse),

        EchoResult = maps:get(<<"result">>, EchoDecoded),
        ?assertEqual(false, maps:get(<<"isError">>, EchoResult)),
        Content = maps:get(<<"content">>, EchoResult),
        ?assertEqual(1, length(Content)),

        %% 调用 add 工具
        AddRequest = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 5,
            <<"method">> => <<"tools/call">>,
            <<"params">> => #{
                <<"name">> => <<"add">>,
                <<"arguments">> => #{<<"a">> => 10, <<"b">> => 20}
            }
        },

        {ok, AddResponse} = beamai_mcp_server:handle_request(Pid, AddRequest),
        {ok, AddDecoded} = beamai_mcp_jsonrpc:decode(AddResponse),

        AddResult = maps:get(<<"result">>, AddDecoded),
        ?assertEqual(false, maps:get(<<"isError">>, AddResult))
    end}.

test_tools_call_error(Pid) ->
    {"工具调用错误测试", fun() ->
        initialize_server(Pid),

        %% 调用不存在的工具
        NotFoundRequest = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 6,
            <<"method">> => <<"tools/call">>,
            <<"params">> => #{
                <<"name">> => <<"unknown">>,
                <<"arguments">> => #{}
            }
        },

        {ok, NotFoundResponse} = beamai_mcp_server:handle_request(Pid, NotFoundRequest),
        {ok, NotFoundDecoded} = beamai_mcp_jsonrpc:decode(NotFoundResponse),

        Error = maps:get(<<"error">>, NotFoundDecoded),
        ?assertEqual(?MCP_ERROR_TOOL_NOT_FOUND, maps:get(<<"code">>, Error)),

        %% 调用会崩溃的工具
        FailRequest = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 7,
            <<"method">> => <<"tools/call">>,
            <<"params">> => #{
                <<"name">> => <<"fail">>,
                <<"arguments">> => #{}
            }
        },

        {ok, FailResponse} = beamai_mcp_server:handle_request(Pid, FailRequest),
        {ok, FailDecoded} = beamai_mcp_jsonrpc:decode(FailResponse),

        FailResult = maps:get(<<"result">>, FailDecoded),
        ?assertEqual(true, maps:get(<<"isError">>, FailResult))
    end}.

%%====================================================================
%% 资源测试
%%====================================================================

test_resources_list(Pid) ->
    {"资源列表测试", fun() ->
        initialize_server(Pid),

        Request = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 8,
            <<"method">> => <<"resources/list">>,
            <<"params">> => #{}
        },

        {ok, Response} = beamai_mcp_server:handle_request(Pid, Request),
        {ok, Decoded} = beamai_mcp_jsonrpc:decode(Response),

        Result = maps:get(<<"result">>, Decoded),
        Resources = maps:get(<<"resources">>, Result),
        ?assertEqual(2, length(Resources))
    end}.

test_resources_read(Pid) ->
    {"资源读取测试", fun() ->
        initialize_server(Pid),

        %% 读取文本资源
        Request = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 9,
            <<"method">> => <<"resources/read">>,
            <<"params">> => #{<<"uri">> => <<"file:///test.txt">>}
        },

        {ok, Response} = beamai_mcp_server:handle_request(Pid, Request),
        {ok, Decoded} = beamai_mcp_jsonrpc:decode(Response),

        Result = maps:get(<<"result">>, Decoded),
        Contents = maps:get(<<"contents">>, Result),
        ?assertEqual(1, length(Contents)),

        [Content] = Contents,
        ?assertEqual(<<"Hello, World!">>, maps:get(<<"text">>, Content)),

        %% 读取不存在的资源
        NotFoundRequest = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 10,
            <<"method">> => <<"resources/read">>,
            <<"params">> => #{<<"uri">> => <<"file:///not/exist">>}
        },

        {ok, NotFoundResponse} = beamai_mcp_server:handle_request(Pid, NotFoundRequest),
        {ok, NotFoundDecoded} = beamai_mcp_jsonrpc:decode(NotFoundResponse),

        Error = maps:get(<<"error">>, NotFoundDecoded),
        ?assertEqual(?MCP_ERROR_RESOURCE_NOT_FOUND, maps:get(<<"code">>, Error))
    end}.

%%====================================================================
%% 提示测试
%%====================================================================

test_prompts_list(Pid) ->
    {"提示列表测试", fun() ->
        initialize_server(Pid),

        Request = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 11,
            <<"method">> => <<"prompts/list">>,
            <<"params">> => #{}
        },

        {ok, Response} = beamai_mcp_server:handle_request(Pid, Request),
        {ok, Decoded} = beamai_mcp_jsonrpc:decode(Response),

        Result = maps:get(<<"result">>, Decoded),
        Prompts = maps:get(<<"prompts">>, Result),
        ?assertEqual(1, length(Prompts)),

        [Prompt] = Prompts,
        ?assertEqual(<<"greet">>, maps:get(<<"name">>, Prompt))
    end}.

test_prompts_get(Pid) ->
    {"提示获取测试", fun() ->
        initialize_server(Pid),

        Request = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 12,
            <<"method">> => <<"prompts/get">>,
            <<"params">> => #{
                <<"name">> => <<"greet">>,
                <<"arguments">> => #{<<"name">> => <<"Alice">>}
            }
        },

        {ok, Response} = beamai_mcp_server:handle_request(Pid, Request),
        {ok, Decoded} = beamai_mcp_jsonrpc:decode(Response),

        Result = maps:get(<<"result">>, Decoded),
        Messages = maps:get(<<"messages">>, Result),
        ?assertEqual(1, length(Messages)),

        %% 获取不存在的提示
        NotFoundRequest = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 13,
            <<"method">> => <<"prompts/get">>,
            <<"params">> => #{
                <<"name">> => <<"unknown">>,
                <<"arguments">> => #{}
            }
        },

        {ok, NotFoundResponse} = beamai_mcp_server:handle_request(Pid, NotFoundRequest),
        {ok, NotFoundDecoded} = beamai_mcp_jsonrpc:decode(NotFoundResponse),

        Error = maps:get(<<"error">>, NotFoundDecoded),
        ?assertEqual(?MCP_ERROR_PROMPT_NOT_FOUND, maps:get(<<"code">>, Error))
    end}.

%%====================================================================
%% 动态注册测试
%%====================================================================

test_register_unregister(Pid) ->
    {"动态注册/注销测试", fun() ->
        initialize_server(Pid),

        %% 注册新工具
        NewTool = #mcp_tool{
            name = <<"new_tool">>,
            description = <<"A new tool">>,
            input_schema = #{},
            handler = fun(_) -> {ok, <<"new result">>} end
        },
        ok = beamai_mcp_server:register_tool(Pid, NewTool),

        %% 验证新工具存在
        ListRequest = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 14,
            <<"method">> => <<"tools/list">>,
            <<"params">> => #{}
        },

        {ok, ListResponse} = beamai_mcp_server:handle_request(Pid, ListRequest),
        {ok, ListDecoded} = beamai_mcp_jsonrpc:decode(ListResponse),
        Tools1 = maps:get(<<"tools">>, maps:get(<<"result">>, ListDecoded)),
        ?assertEqual(4, length(Tools1)),

        %% 注销工具
        ok = beamai_mcp_server:unregister_tool(Pid, <<"new_tool">>),

        {ok, ListResponse2} = beamai_mcp_server:handle_request(Pid, ListRequest),
        {ok, ListDecoded2} = beamai_mcp_jsonrpc:decode(ListResponse2),
        Tools2 = maps:get(<<"tools">>, maps:get(<<"result">>, ListDecoded2)),
        ?assertEqual(3, length(Tools2))
    end}.

%%====================================================================
%% 未初始化错误测试
%%====================================================================

test_uninitialized_error(Pid) ->
    {"未初始化错误测试", fun() ->
        %% 不初始化直接调用方法
        Request = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 15,
            <<"method">> => <<"tools/list">>,
            <<"params">> => #{}
        },

        {ok, Response} = beamai_mcp_server:handle_request(Pid, Request),
        {ok, Decoded} = beamai_mcp_jsonrpc:decode(Response),

        ?assert(maps:is_key(<<"error">>, Decoded)),
        Error = maps:get(<<"error">>, Decoded),
        ?assertEqual(?MCP_ERROR_INVALID_REQUEST, maps:get(<<"code">>, Error))
    end}.

%%====================================================================
%% 辅助函数
%%====================================================================

initialize_server(Pid) ->
    InitRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 0,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => <<"2025-11-25">>,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{<<"name">> => <<"test">>}
        }
    },
    {ok, _} = beamai_mcp_server:handle_request(Pid, InitRequest),
    ok.
