%%%-------------------------------------------------------------------
%%% @doc beamai_mcp_adapter 单元测试
%%%
%%% 测试 MCP 工具到 beamai_agent 工具的转换功能。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_adapter_test).

-include_lib("eunit/include/eunit.hrl").
-include("beamai_mcp.hrl").

%%====================================================================
%% 测试 Fixtures
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% 测试：基础转换
%%====================================================================

basic_conversion_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"convert MCP tool to agent tool", fun test_basic_conversion/0},
      {"convert batch MCP tools", fun test_batch_conversion/0}
     ]}.

test_basic_conversion() ->
    %% 创建测试 MCP 工具
    McpTool = #mcp_tool{
        name = <<"test_tool">>,
        description = <<"A test tool">>,
        input_schema = #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"arg1">> => #{<<"type">> => <<"string">>}
            }
        },
        handler = fun(#{<<"arg1">> := Val}) ->
            {ok, [#{<<"type">> => <<"text">>, <<"text">> => <<"Result: ", Val/binary>>}]}
        end
    },

    %% 转换
    AgentTool = beamai_mcp_adapter:mcp_tool_to_beamai_tool(McpTool),

    %% 验证字段
    ?assertEqual(<<"test_tool">>, maps:get(name, AgentTool)),
    ?assertEqual(<<"A test tool">>, maps:get(description, AgentTool)),
    ?assert(is_map(maps:get(parameters, AgentTool))),
    ?assert(is_function(maps:get(handler, AgentTool))),

    %% 测试 handler
    Handler = maps:get(handler, AgentTool),
    Result = Handler(#{<<"arg1">> => <<"value">>}),
    ?assertEqual(<<"Result: value">>, Result),

    ok.

test_batch_conversion() ->
    %% 创建多个 MCP 工具
    McpTools = [
        #mcp_tool{
            name = <<"tool1">>,
            description = <<"Tool 1">>,
            input_schema = #{},
            handler = fun(_) -> {ok, [beamai_mcp_types:text_content(<<"OK1">>)]} end
        },
        #mcp_tool{
            name = <<"tool2">>,
            description = <<"Tool 2">>,
            input_schema = #{},
            handler = fun(_) -> {ok, [beamai_mcp_types:text_content(<<"OK2">>)]} end
        }
    ],

    %% 批量转换
    AgentTools = beamai_mcp_adapter:mcp_tools_to_beamai_tools(McpTools),

    %% 验证
    ?assertEqual(2, length(AgentTools)),
    [Tool1, Tool2] = AgentTools,
    ?assertEqual(<<"tool1">>, maps:get(name, Tool1)),
    ?assertEqual(<<"tool2">>, maps:get(name, Tool2)),

    ok.

%%====================================================================
%% 测试：Handler 适配
%%====================================================================

handler_adaptation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"adapt successful handler", fun test_successful_handler/0},
      {"adapt error handler", fun test_error_handler/0},
      {"adapt handler with multiple contents", fun test_multiple_contents/0}
     ]}.

test_successful_handler() ->
    %% MCP handler 返回成功
    McpHandler = fun(_) ->
        {ok, [#{<<"type">> => <<"text">>, <<"text">> => <<"Success">>}]}
    end,

    %% 适配
    AgentHandler = beamai_mcp_adapter:adapt_handler(McpHandler),

    %% 调用
    Result = AgentHandler(#{}),
    ?assertEqual(<<"Success">>, Result),

    ok.

test_error_handler() ->
    %% MCP handler 返回错误
    McpHandler = fun(_) ->
        {error, #{<<"message">> => <<"Something went wrong">>}}
    end,

    %% 适配
    AgentHandler = beamai_mcp_adapter:adapt_handler(McpHandler),

    %% 调用
    Result = AgentHandler(#{}),
    ?assertEqual(<<"Error: Something went wrong">>, Result),

    ok.

test_multiple_contents() ->
    %% MCP handler 返回多个内容
    McpHandler = fun(_) ->
        {ok, [
            #{<<"type">> => <<"text">>, <<"text">> => <<"First">>},
            #{<<"type">> => <<"text">>, <<"text">> => <<"Second">>},
            #{<<"type">> => <<"image">>, <<"data">> => <<"base64...">>}  %% 应该被忽略
        ]}
    end,

    %% 适配
    AgentHandler = beamai_mcp_adapter:adapt_handler(McpHandler),

    %% 调用
    Result = AgentHandler(#{}),
    ?assertEqual(<<"First\nSecond">>, Result),

    ok.

%%====================================================================
%% 测试：内容提取
%%====================================================================

content_extraction_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"extract text from contents", fun test_extract_text/0},
      {"extract from empty contents", fun test_empty_contents/0},
      {"extract from mixed contents", fun test_mixed_contents/0}
     ]}.

test_extract_text() ->
    Contents = [
        #{<<"type">> => <<"text">>, <<"text">> => <<"Hello">>},
        #{<<"type">> => <<"text">>, <<"text">> => <<"World">>}
    ],

    Result = beamai_mcp_adapter:extract_text_from_contents(Contents),
    ?assertEqual(<<"Hello\nWorld">>, Result),

    ok.

test_empty_contents() ->
    Result = beamai_mcp_adapter:extract_text_from_contents([]),
    ?assertEqual(<<"No content returned">>, Result),

    ok.

test_mixed_contents() ->
    Contents = [
        #{<<"type">> => <<"text">>, <<"text">> => <<"Text content">>},
        #{<<"type">> => <<"image">>, <<"data">> => <<"...">>},
        #{<<"type">> => <<"resource">>, <<"uri">> => <<"file://...">>}
    ],

    Result = beamai_mcp_adapter:extract_text_from_contents(Contents),
    ?assertEqual(<<"Text content">>, Result),

    ok.

%%====================================================================
%% 测试：错误格式化
%%====================================================================

error_formatting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"format MCP error", fun test_format_mcp_error/0},
      {"format binary error", fun test_format_binary_error/0},
      {"format atom error", fun test_format_atom_error/0},
      {"format term error", fun test_format_term_error/0}
     ]}.

test_format_mcp_error() ->
    Error = #{
        <<"code">> => -32002,
        <<"message">> => <<"Resource not found">>
    },

    Result = beamai_mcp_adapter:format_error(Error),
    ?assertEqual(<<"Error [-32002]: Resource not found">>, Result),

    ok.

test_format_binary_error() ->
    Error = <<"Something failed">>,
    Result = beamai_mcp_adapter:format_error(Error),
    ?assertEqual(<<"Error: Something failed">>, Result),

    ok.

test_format_atom_error() ->
    Error = timeout,
    Result = beamai_mcp_adapter:format_error(Error),
    ?assertEqual(<<"Error: timeout">>, Result),

    ok.

test_format_term_error() ->
    Error = {error, some_reason},
    Result = beamai_mcp_adapter:format_error(Error),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"Error:">>) =/= nomatch),

    ok.
