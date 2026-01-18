%%%-------------------------------------------------------------------
%%% @doc beamai_mcp_types 单元测试
%%%-------------------------------------------------------------------
-module(beamai_mcp_types_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamai_mcp.hrl").

%%====================================================================
%% 测试组
%%====================================================================

beamai_mcp_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Tool 构造和转换测试", fun tool_construction_test/0},
      {"Resource 构造和转换测试", fun resource_construction_test/0},
      {"Resource Template 构造和转换测试", fun resource_template_construction_test/0},
      {"Prompt 构造和转换测试", fun prompt_construction_test/0},
      {"Prompt Arg 构造和转换测试", fun prompt_arg_construction_test/0},
      {"Server Capabilities 构造和转换测试", fun server_capabilities_test/0},
      {"Client Capabilities 构造和转换测试", fun client_capabilities_test/0},
      {"Content 构造测试", fun content_construction_test/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Tool 测试
%%====================================================================

tool_construction_test() ->
    %% 基本构造
    Handler = fun(_Args) -> {ok, <<"result">>} end,
    Tool = beamai_mcp_types:make_tool(
        <<"test_tool">>,
        <<"A test tool">>,
        #{<<"type">> => <<"object">>},
        Handler
    ),

    ?assertEqual(<<"test_tool">>, Tool#mcp_tool.name),
    ?assertEqual(<<"A test tool">>, Tool#mcp_tool.description),
    ?assertEqual(#{<<"type">> => <<"object">>}, Tool#mcp_tool.input_schema),
    ?assertEqual(undefined, Tool#mcp_tool.output_schema),

    %% 带输出 Schema 构造
    Tool2 = beamai_mcp_types:make_tool(
        <<"test_tool2">>,
        <<"Another test tool">>,
        #{<<"type">> => <<"object">>},
        #{<<"type">> => <<"string">>},
        Handler
    ),
    ?assertEqual(#{<<"type">> => <<"string">>}, Tool2#mcp_tool.output_schema),

    %% 转换为 map
    ToolMap = beamai_mcp_types:tool_to_map(Tool),
    ?assertEqual(<<"test_tool">>, maps:get(<<"name">>, ToolMap)),
    ?assertEqual(<<"A test tool">>, maps:get(<<"description">>, ToolMap)),
    ?assertEqual(#{<<"type">> => <<"object">>}, maps:get(<<"inputSchema">>, ToolMap)),
    ?assertEqual(false, maps:is_key(<<"outputSchema">>, ToolMap)),

    %% 带输出 Schema 的转换
    Tool2Map = beamai_mcp_types:tool_to_map(Tool2),
    ?assertEqual(#{<<"type">> => <<"string">>}, maps:get(<<"outputSchema">>, Tool2Map)).

%%====================================================================
%% Resource 测试
%%====================================================================

resource_construction_test() ->
    Handler = fun() -> {ok, <<"content">>} end,

    %% 简化构造
    Resource = beamai_mcp_types:make_resource(
        <<"file:///tmp/test.txt">>,
        <<"test_file">>,
        <<"A test file">>,
        Handler
    ),

    ?assertEqual(<<"file:///tmp/test.txt">>, Resource#mcp_resource.uri),
    ?assertEqual(<<"test_file">>, Resource#mcp_resource.name),
    ?assertEqual(<<"A test file">>, Resource#mcp_resource.description),

    %% 完整构造
    Resource2 = beamai_mcp_types:make_resource(
        <<"file:///tmp/test2.txt">>,
        <<"test_file2">>,
        <<"Test File 2">>,
        <<"A test file with all fields">>,
        <<"text/plain">>,
        Handler
    ),

    ?assertEqual(<<"Test File 2">>, Resource2#mcp_resource.title),
    ?assertEqual(<<"text/plain">>, Resource2#mcp_resource.mime_type),

    %% 转换为 map
    ResMap = beamai_mcp_types:resource_to_map(Resource2),
    ?assertEqual(<<"file:///tmp/test2.txt">>, maps:get(<<"uri">>, ResMap)),
    ?assertEqual(<<"test_file2">>, maps:get(<<"name">>, ResMap)),
    ?assertEqual(<<"Test File 2">>, maps:get(<<"title">>, ResMap)),
    ?assertEqual(<<"text/plain">>, maps:get(<<"mimeType">>, ResMap)).

%%====================================================================
%% Resource Template 测试
%%====================================================================

resource_template_construction_test() ->
    Template = beamai_mcp_types:make_resource_template(
        <<"file:///{path}">>,
        <<"file_template">>,
        <<"File access template">>
    ),

    ?assertEqual(<<"file:///{path}">>, Template#mcp_resource_template.uri_template),
    ?assertEqual(<<"file_template">>, Template#mcp_resource_template.name),
    ?assertEqual(<<"File access template">>, Template#mcp_resource_template.description),

    %% 转换为 map
    TplMap = beamai_mcp_types:resource_template_to_map(Template),
    ?assertEqual(<<"file:///{path}">>, maps:get(<<"uriTemplate">>, TplMap)),
    ?assertEqual(<<"file_template">>, maps:get(<<"name">>, TplMap)).

%%====================================================================
%% Prompt 测试
%%====================================================================

prompt_construction_test() ->
    Handler = fun(_Args) -> {ok, [#{<<"role">> => <<"user">>, <<"content">> => <<"Hello">>}]} end,

    %% 无参数构造
    Prompt = beamai_mcp_types:make_prompt(
        <<"greeting">>,
        <<"A greeting prompt">>,
        Handler
    ),

    ?assertEqual(<<"greeting">>, Prompt#mcp_prompt.name),
    ?assertEqual(<<"A greeting prompt">>, Prompt#mcp_prompt.description),
    ?assertEqual([], Prompt#mcp_prompt.arguments),

    %% 带参数构造
    Arg1 = beamai_mcp_types:make_prompt_arg(<<"name">>, <<"User name">>, true),
    Arg2 = beamai_mcp_types:make_prompt_arg(<<"style">>, <<"Greeting style">>),

    Prompt2 = beamai_mcp_types:make_prompt(
        <<"custom_greeting">>,
        <<"A customizable greeting">>,
        [Arg1, Arg2],
        Handler
    ),

    ?assertEqual(2, length(Prompt2#mcp_prompt.arguments)),

    %% 转换为 map
    PromptMap = beamai_mcp_types:prompt_to_map(Prompt2),
    ?assertEqual(<<"custom_greeting">>, maps:get(<<"name">>, PromptMap)),
    Args = maps:get(<<"arguments">>, PromptMap),
    ?assertEqual(2, length(Args)).

%%====================================================================
%% Prompt Arg 测试
%%====================================================================

prompt_arg_construction_test() ->
    %% 可选参数
    Arg1 = beamai_mcp_types:make_prompt_arg(<<"optional_arg">>, <<"An optional argument">>),
    ?assertEqual(<<"optional_arg">>, Arg1#mcp_prompt_arg.name),
    ?assertEqual(false, Arg1#mcp_prompt_arg.required),

    %% 必填参数
    Arg2 = beamai_mcp_types:make_prompt_arg(<<"required_arg">>, <<"A required argument">>, true),
    ?assertEqual(true, Arg2#mcp_prompt_arg.required),

    %% 转换为 map
    ArgMap = beamai_mcp_types:prompt_arg_to_map(Arg2),
    ?assertEqual(<<"required_arg">>, maps:get(<<"name">>, ArgMap)),
    ?assertEqual(true, maps:get(<<"required">>, ArgMap)).

%%====================================================================
%% Server Capabilities 测试
%%====================================================================

server_capabilities_test() ->
    %% 默认能力
    Caps = beamai_mcp_types:make_server_capabilities(),
    ?assertEqual(false, Caps#mcp_server_capabilities.tools),
    ?assertEqual(false, Caps#mcp_server_capabilities.resources),
    ?assertEqual(false, Caps#mcp_server_capabilities.prompts),
    ?assertEqual(false, Caps#mcp_server_capabilities.logging),

    %% 从 map 构造
    Caps2 = beamai_mcp_types:make_server_capabilities(#{
        tools => true,
        resources => #{subscribe => true},
        prompts => true,
        logging => true
    }),
    ?assertEqual(true, Caps2#mcp_server_capabilities.tools),
    ?assertEqual(#{subscribe => true}, Caps2#mcp_server_capabilities.resources),

    %% 转换为 map
    CapsMap = beamai_mcp_types:server_capabilities_to_map(Caps2),
    ?assertEqual(true, maps:is_key(<<"tools">>, CapsMap)),
    ?assertEqual(true, maps:is_key(<<"logging">>, CapsMap)).

%%====================================================================
%% Client Capabilities 测试
%%====================================================================

client_capabilities_test() ->
    %% 默认能力
    Caps = beamai_mcp_types:make_client_capabilities(),
    ?assertEqual(false, Caps#mcp_client_capabilities.roots),
    ?assertEqual(false, Caps#mcp_client_capabilities.sampling),

    %% 从 map 构造
    Caps2 = beamai_mcp_types:make_client_capabilities(#{
        roots => #{list_changed => true},
        sampling => true
    }),
    ?assertEqual(#{list_changed => true}, Caps2#mcp_client_capabilities.roots),
    ?assertEqual(true, Caps2#mcp_client_capabilities.sampling),

    %% 转换为 map
    CapsMap = beamai_mcp_types:client_capabilities_to_map(Caps2),
    ?assertEqual(true, maps:is_key(<<"roots">>, CapsMap)),
    ?assertEqual(true, maps:is_key(<<"sampling">>, CapsMap)).

%%====================================================================
%% Content 构造测试
%%====================================================================

content_construction_test() ->
    %% 文本内容
    TextContent = beamai_mcp_types:text_content(<<"Hello, World!">>),
    ?assertEqual(<<"text">>, maps:get(<<"type">>, TextContent)),
    ?assertEqual(<<"Hello, World!">>, maps:get(<<"text">>, TextContent)),

    %% 图片内容
    ImageContent = beamai_mcp_types:image_content(<<"base64data">>, <<"image/png">>),
    ?assertEqual(<<"image">>, maps:get(<<"type">>, ImageContent)),
    ?assertEqual(<<"base64data">>, maps:get(<<"data">>, ImageContent)),
    ?assertEqual(<<"image/png">>, maps:get(<<"mimeType">>, ImageContent)),

    %% 带注释的图片
    ImageContent2 = beamai_mcp_types:image_content(
        <<"base64data">>,
        <<"image/png">>,
        #{<<"alt">> => <<"An image">>}
    ),
    ?assertEqual(#{<<"alt">> => <<"An image">>}, maps:get(<<"annotations">>, ImageContent2)),

    %% 资源引用
    ResourceContent = beamai_mcp_types:resource_content(<<"file:///tmp/test.txt">>, <<"text/plain">>),
    ?assertEqual(<<"resource">>, maps:get(<<"type">>, ResourceContent)),
    Resource = maps:get(<<"resource">>, ResourceContent),
    ?assertEqual(<<"file:///tmp/test.txt">>, maps:get(<<"uri">>, Resource)).
