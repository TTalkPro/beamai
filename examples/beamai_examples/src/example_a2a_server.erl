%%%-------------------------------------------------------------------
%%% @doc A2A Server 完整示例
%%%
%%% 展示如何启动一个完整的 A2A HTTP Server，包括：
%%% - 创建 Agent 配置
%%% - 启动 A2A Server
%%% - 启动 Cowboy HTTP Server
%%% - 配置路由
%%%
%%% == 运行示例 ==
%%%
%%% ```
%%% $ rebar3 shell
%%% > a2a_server_example:start().
%%% > a2a_server_example:stop().
%%% ```
%%%
%%% == 测试 ==
%%%
%%% ```bash
%%% # 获取 Agent Card
%%% curl http://localhost:8080/.well-known/agent.json
%%%
%%% # 发送消息
%%% curl -X POST http://localhost:8080/a2a \
%%%   -H "Content-Type: application/json" \
%%%   -d '{
%%%     "jsonrpc": "2.0",
%%%     "id": 1,
%%%     "method": "message/send",
%%%     "params": {
%%%       "message": {
%%%         "role": "user",
%%%         "parts": [{"kind": "text", "text": "Hello"}]
%%%       }
%%%     }
%%%   }'
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_a2a_server).

-export([start/0, start/1, stop/0]).

%% 默认配置
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_AGENT_NAME, <<"example-agent">>).

%%====================================================================
%% API
%%====================================================================

%% @doc 使用默认配置启动服务器
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    start(#{}).

%% @doc 使用自定义配置启动服务器
%%
%% 配置选项：
%% - port: HTTP 端口（默认 8080）
%% - agent_name: Agent 名称
%% - agent_description: Agent 描述
%% - system_prompt: 系统提示词
%% - tools: 工具列表
%% - llm: LLM 配置
%%
-spec start(map()) -> {ok, pid()} | {error, term()}.
start(Opts) ->
    %% 1. 构建 Agent 配置
    AgentConfig = build_agent_config(Opts),

    %% 2. 启动 A2A Server
    {ok, A2AServer} = beamai_a2a_server:start_link(#{
        agent_config => AgentConfig
    }),

    %% 3. 配置 Cowboy 路由
    Port = maps:get(port, Opts, ?DEFAULT_PORT),
    Routes = build_routes(A2AServer),

    Dispatch = cowboy_router:compile([{'_', Routes}]),

    %% 4. 启动 Cowboy
    {ok, _} = cowboy:start_clear(a2a_http_listener, [
        {port, Port}
    ], #{
        env => #{dispatch => Dispatch}
    }),

    io:format("~n"),
    io:format("========================================~n"),
    io:format("  A2A Server started on port ~p~n", [Port]),
    io:format("========================================~n"),
    io:format("~n"),
    io:format("Endpoints:~n"),
    io:format("  GET  http://localhost:~p/.well-known/agent.json~n", [Port]),
    io:format("  POST http://localhost:~p/a2a~n", [Port]),
    io:format("  POST http://localhost:~p/a2a/stream~n", [Port]),
    io:format("~n"),
    io:format("Test with:~n"),
    io:format("  curl http://localhost:~p/.well-known/agent.json~n", [Port]),
    io:format("~n"),

    {ok, A2AServer}.

%% @doc 停止服务器
-spec stop() -> ok.
stop() ->
    cowboy:stop_listener(a2a_http_listener),
    io:format("A2A Server stopped~n"),
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 构建 Agent 配置
build_agent_config(Opts) ->
    %% 基础配置
    Name = maps:get(agent_name, Opts, ?DEFAULT_AGENT_NAME),
    Description = maps:get(agent_description, Opts,
        <<"A sample A2A agent for demonstration">>),
    Url = maps:get(url, Opts, <<"http://localhost:8080/a2a">>),

    %% 系统提示词
    SystemPrompt = maps:get(system_prompt, Opts,
        <<"You are a helpful assistant. Answer questions concisely.">>),

    %% 工具配置（示例工具）
    Tools = maps:get(tools, Opts, example_tools()),

    %% LLM 配置（默认使用智谱 GLM-4.7）
    DefaultLLM = #{
        provider => anthropic,
        model => <<"glm-4.7">>,
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
        api_key => list_to_binary(os:getenv("ZHIPU_API_KEY", "")),
        max_tokens => 2048
    },
    LLMConfig = maps:get(llm, Opts, DefaultLLM),

    #{
        name => Name,
        description => Description,
        url => Url,
        system_prompt => SystemPrompt,
        tools => Tools,
        llm => LLMConfig
    }.

%% @private 示例工具
example_tools() ->
    [
        #{
            name => <<"get_time">>,
            description => <<"Get the current time">>,
            parameters => #{
                type => <<"object">>,
                properties => #{},
                required => []
            }
        },
        #{
            name => <<"echo">>,
            description => <<"Echo back the input message">>,
            parameters => #{
                type => <<"object">>,
                properties => #{
                    <<"message">> => #{
                        type => <<"string">>,
                        description => <<"The message to echo">>
                    }
                },
                required => [<<"message">>]
            }
        }
    ].

%% @private 构建 Cowboy 路由
build_routes(A2AServer) ->
    HandlerState = #{a2a_server => A2AServer},

    [
        %% Agent Card（Well-Known URI）
        {"/.well-known/agent.json", a2a_cowboy_handler, HandlerState#{action => agent_card}},

        %% JSON-RPC 端点
        {"/a2a", a2a_cowboy_handler, HandlerState#{action => handle_request}},

        %% SSE 流式端点
        {"/a2a/stream", a2a_cowboy_handler, HandlerState#{action => stream}}
    ].

%%====================================================================
%% 使用示例
%%====================================================================

%% @doc 示例：自定义配置启动
%%
%% ```erlang
%% a2a_server_example:start(#{
%%     port => 9000,
%%     agent_name => <<"my-agent">>,
%%     agent_description => <<"My custom A2A agent">>,
%%     system_prompt => <<"You are a specialized assistant.">>,
%%     llm => #{
%%         provider => anthropic,
%%         model => <<"claude-3-haiku-20240307">>,
%%         api_key => os:getenv("ANTHROPIC_API_KEY")
%%     }
%% }).
%% ```
-spec example_custom_start() -> {ok, pid()}.
example_custom_start() ->
    start(#{
        port => 9000,
        agent_name => <<"inventory-agent">>,
        agent_description => <<"Inventory management agent">>,
        system_prompt => <<"You are an inventory management assistant.
            You can check stock levels and manage reservations.">>,
        tools => [
            #{
                name => <<"check_stock">>,
                description => <<"Check inventory stock for a product">>,
                parameters => #{
                    type => <<"object">>,
                    properties => #{
                        <<"product_id">> => #{
                            type => <<"string">>,
                            description => <<"The product ID to check">>
                        }
                    },
                    required => [<<"product_id">>]
                }
            }
        ]
    }).
