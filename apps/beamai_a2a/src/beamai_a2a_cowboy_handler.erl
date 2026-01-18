%%%-------------------------------------------------------------------
%%% @doc A2A Cowboy Handler 适配器
%%%
%%% 将 beamai_a2a_http_handler 适配到 Cowboy HTTP 服务器。
%%% 这是一个可选模块，用户可以选择使用或自行实现适配器。
%%%
%%% == 支持的端点 ==
%%%
%%% - POST /a2a : JSON-RPC 请求端点
%%% - GET /.well-known/agent.json : Agent Card 端点
%%% - OPTIONS /* : CORS 预检请求
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 配置 Agent
%%% AgentConfig = #{
%%%     name => <<"my-agent">>,
%%%     description => <<"A helpful agent">>,
%%%     %% ... 其他 Agent 配置
%%% },
%%%
%%% A2AConfig = #{
%%%     agent_config => AgentConfig
%%% },
%%%
%%% %% 配置 Cowboy 路由
%%% Dispatch = cowboy_router:compile([
%%%     {'_', [
%%%         %% JSON-RPC 端点
%%%         {"/a2a", beamai_a2a_cowboy_handler, A2AConfig},
%%%         %% Agent Card 端点
%%%         {"/.well-known/agent.json", beamai_a2a_cowboy_handler,
%%%             A2AConfig#{mode => agent_card}}
%%%     ]}
%%% ]),
%%%
%%% %% 启动 Cowboy
%%% {ok, _} = cowboy:start_clear(a2a_listener, [{port, 8080}],
%%%                               #{env => #{dispatch => Dispatch}}).
%%% ```
%%%
%%% == 认证和限流 ==
%%%
%%% 认证和限流通过 beamai_a2a_auth 和 beamai_a2a_rate_limit 模块配置。
%%% 在应用启动时配置这些模块：
%%%
%%% ```erlang
%%% %% 配置 API Key 认证
%%% beamai_a2a_auth:configure(#{
%%%     enabled => true,
%%%     keys => [
%%%         #{key => <<"sk-test-key">>, name => <<"Test Key">>}
%%%     ]
%%% }).
%%%
%%% %% 配置限流
%%% beamai_a2a_rate_limit:configure(#{
%%%     enabled => true,
%%%     default_limit => 100,
%%%     window_ms => 60000
%%% }).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_cowboy_handler).

-include_lib("beamai_core/include/beamai_common.hrl").

%%====================================================================
%% Cowboy Handler 回调
%%====================================================================

-export([init/2]).

%%====================================================================
%% Cowboy Handler 实现
%%====================================================================

%% @doc 初始化请求处理
%%
%% 根据请求方法和配置的 mode 分发到不同的处理逻辑。
init(Req, Config) ->
    Method = cowboy_req:method(Req),
    Mode = maps:get(mode, Config, jsonrpc),
    handle_by_mode(Method, Mode, Req, Config).

%% @private 根据模式处理请求
handle_by_mode(<<"POST">>, jsonrpc, Req, Config) ->
    handle_jsonrpc(Req, Config);
handle_by_mode(<<"GET">>, agent_card, Req, Config) ->
    handle_agent_card(Req, Config);
handle_by_mode(<<"OPTIONS">>, _, Req, Config) ->
    handle_options(Req, Config);
handle_by_mode(Method, Mode, Req, _Config) ->
    %% 方法不允许
    AllowedMethods = allowed_methods(Mode),
    Req2 = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>,
        <<"allow">> => AllowedMethods,
        <<"access-control-allow-origin">> => <<"*">>
    }, jsx:encode(#{
        <<"error">> => <<"Method not allowed">>,
        <<"method">> => Method,
        <<"allowed">> => AllowedMethods
    }), Req),
    {ok, Req2, undefined}.

%%====================================================================
%% JSON-RPC 端点处理
%%====================================================================

%% @private 处理 JSON-RPC POST 请求
handle_jsonrpc(Req, Config) ->
    %% 读取请求体
    {ok, Body, Req2} = cowboy_req:read_body(Req),

    %% 获取请求头
    Headers = maps:to_list(cowboy_req:headers(Req2)),

    %% 初始化 handler
    HandlerState = beamai_a2a_http_handler:init(Config),

    %% 处理请求
    case beamai_a2a_http_handler:handle_post(Body, Headers, HandlerState) of
        {ok, ResponseJson, RespHeaders, NewState} ->
            %% 成功响应
            Req3 = cowboy_req:reply(200, RespHeaders, ResponseJson, Req2),
            beamai_a2a_http_handler:close(NewState),
            {ok, Req3, undefined};

        {error, auth_error, ResponseJson, RespHeaders, NewState} ->
            %% 认证错误 - 401
            Req3 = cowboy_req:reply(401, RespHeaders, ResponseJson, Req2),
            beamai_a2a_http_handler:close(NewState),
            {ok, Req3, undefined};

        {error, rate_limited, ResponseJson, RespHeaders, NewState} ->
            %% 限流 - 429
            Req3 = cowboy_req:reply(429, RespHeaders, ResponseJson, Req2),
            beamai_a2a_http_handler:close(NewState),
            {ok, Req3, undefined};

        {error, parse_error, ResponseJson, RespHeaders, NewState} ->
            %% 解析错误 - 400
            Req3 = cowboy_req:reply(400, RespHeaders, ResponseJson, Req2),
            beamai_a2a_http_handler:close(NewState),
            {ok, Req3, undefined};

        {error, _, ResponseJson, RespHeaders, NewState} ->
            %% 其他错误 - 500
            Req3 = cowboy_req:reply(500, RespHeaders, ResponseJson, Req2),
            beamai_a2a_http_handler:close(NewState),
            {ok, Req3, undefined}
    end.

%%====================================================================
%% Agent Card 端点处理
%%====================================================================

%% @private 处理 Agent Card GET 请求
handle_agent_card(Req, Config) ->
    %% 初始化 handler（只为获取 Agent Card）
    HandlerState = beamai_a2a_http_handler:init(Config),

    %% 获取 Agent Card
    {ok, CardJson, RespHeaders} = beamai_a2a_http_handler:handle_agent_card(HandlerState),

    %% 响应
    Req2 = cowboy_req:reply(200, RespHeaders, CardJson, Req),
    beamai_a2a_http_handler:close(HandlerState),
    {ok, Req2, undefined}.

%%====================================================================
%% CORS 和 OPTIONS 处理
%%====================================================================

%% @private 处理 OPTIONS 请求（CORS 预检）
handle_options(Req, Config) ->
    Mode = maps:get(mode, Config, jsonrpc),
    Req2 = cowboy_req:reply(204, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => allowed_methods(Mode),
        <<"access-control-allow-headers">> => <<"content-type, authorization, x-api-key">>,
        <<"access-control-max-age">> => <<"86400">>
    }, <<>>, Req),
    {ok, Req2, undefined}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取允许的方法
-spec allowed_methods(atom()) -> binary().
allowed_methods(jsonrpc) -> <<"POST, OPTIONS">>;
allowed_methods(agent_card) -> <<"GET, OPTIONS">>;
allowed_methods(_) -> <<"POST, GET, OPTIONS">>.
