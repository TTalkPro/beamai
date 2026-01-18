%%%-------------------------------------------------------------------
%%% @doc MCP Cowboy Handler 适配器
%%%
%%% 将 beamai_mcp_handler 适配到 Cowboy HTTP 服务器。
%%% 这是一个可选模块，用户可以选择使用或自行实现适配器。
%%%
%%% == 支持的端点 ==
%%%
%%% - POST /mcp : Streamable HTTP 端点
%%% - GET /mcp/sse : SSE 流端点（可选）
%%% - POST /mcp/sse/message : SSE 模式消息端点（可选）
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 在 Cowboy 路由配置中
%%% McpConfig = #{
%%%     tools => [
%%%         beamai_mcp_types:make_tool(
%%%             <<"echo">>,
%%%             <<"Echo the input">>,
%%%             #{type => object, properties => #{text => #{type => string}}},
%%%             fun(#{<<"text">> := Text}) -> {ok, Text} end
%%%         )
%%%     ],
%%%     server_info => #{<<"name">> => <<"my-mcp-server">>, <<"version">> => <<"1.0">>}
%%% },
%%%
%%% Dispatch = cowboy_router:compile([
%%%     {'_', [
%%%         %% Streamable HTTP 端点
%%%         {"/mcp", beamai_mcp_cowboy_handler, McpConfig},
%%%         %% SSE 端点（可选）
%%%         {"/mcp/sse", beamai_mcp_cowboy_handler, McpConfig#{mode => sse_init}},
%%%         {"/mcp/sse/message", beamai_mcp_cowboy_handler, McpConfig#{mode => sse_message}}
%%%     ]}
%%% ]),
%%%
%%% {ok, _} = cowboy:start_clear(mcp_listener, [{port, 8080}],
%%%                               #{env => #{dispatch => Dispatch}}).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_cowboy_handler).

-include("beamai_mcp.hrl").

%%====================================================================
%% Cowboy Handler 回调
%%====================================================================

-export([init/2]).

%% 用于 SSE 流模式
-export([info/3]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(sse_state, {
    handler_state :: beamai_mcp_handler:handler_state(),
    config :: map()
}).

%%====================================================================
%% Cowboy Handler 实现
%%====================================================================

%% @doc 初始化请求处理
%%
%% 根据请求方法和配置的 mode 分发到不同的处理逻辑。
init(Req, Config) ->
    Method = cowboy_req:method(Req),
    Mode = maps:get(mode, Config, streamable_http),
    handle_by_mode(Method, Mode, Req, Config).

%% @private 根据模式处理请求
handle_by_mode(<<"POST">>, streamable_http, Req, Config) ->
    handle_streamable_http(Req, Config);
handle_by_mode(<<"POST">>, sse_message, Req, Config) ->
    handle_sse_message(Req, Config);
handle_by_mode(<<"GET">>, sse_init, Req, Config) ->
    handle_sse_init(Req, Config);
handle_by_mode(<<"OPTIONS">>, _, Req, Config) ->
    handle_options(Req, Config);
handle_by_mode(Method, Mode, Req, _Config) ->
    Req2 = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>,
        <<"allow">> => allowed_methods(Mode)
    }, jsx:encode(#{
        <<"error">> => <<"Method not allowed">>,
        <<"method">> => Method
    }), Req),
    {ok, Req2, undefined}.

%%====================================================================
%% Streamable HTTP 处理
%%====================================================================

%% @private 处理 Streamable HTTP POST 请求
handle_streamable_http(Req, Config) ->
    %% 读取请求体
    {ok, Body, Req2} = cowboy_req:read_body(Req),

    %% 获取请求头
    Headers = maps:to_list(cowboy_req:headers(Req2)),

    %% 初始化 handler
    HandlerState = beamai_mcp_handler:init(Config),

    %% 处理请求
    case beamai_mcp_handler:handle_post(Body, Headers, HandlerState) of
        {json, Response, NewState} ->
            %% JSON 响应
            SessionId = beamai_mcp_handler:get_session_id(NewState),
            RespHeaders = build_response_headers(json, SessionId),
            Req3 = cowboy_req:reply(200, RespHeaders, Response, Req2),
            beamai_mcp_handler:close(NewState),
            {ok, Req3, undefined};

        {sse, Response, NewState} ->
            %% SSE 响应
            SessionId = beamai_mcp_handler:get_session_id(NewState),
            RespHeaders = build_response_headers(sse, SessionId),
            Req3 = cowboy_req:stream_reply(200, RespHeaders, Req2),
            cowboy_req:stream_body(iolist_to_binary(Response), fin, Req3),
            beamai_mcp_handler:close(NewState),
            {ok, Req3, undefined};

        {no_content, _, NewState} ->
            Req3 = cowboy_req:reply(204, #{}, <<>>, Req2),
            beamai_mcp_handler:close(NewState),
            {ok, Req3, undefined}
    end.

%%====================================================================
%% SSE 模式处理
%%====================================================================

%% @private 处理 SSE 初始化（GET 请求）
handle_sse_init(Req, Config) ->
    Headers = maps:to_list(cowboy_req:headers(Req)),

    %% 初始化 handler
    HandlerState = beamai_mcp_handler:init(Config),

    case beamai_mcp_handler:handle_sse_init(Headers, HandlerState) of
        {ok, InitialData, NewState} ->
            %% 开始 SSE 流
            RespHeaders = #{
                <<"content-type">> => <<"text/event-stream">>,
                <<"cache-control">> => <<"no-cache">>,
                <<"connection">> => <<"keep-alive">>,
                <<"access-control-allow-origin">> => <<"*">>
            },
            Req2 = cowboy_req:stream_reply(200, RespHeaders, Req),

            %% 发送初始 endpoint 事件
            cowboy_req:stream_body(iolist_to_binary(InitialData), nofin, Req2),

            %% 进入流模式，等待后续事件
            State = #sse_state{
                handler_state = NewState,
                config = Config
            },
            {cowboy_loop, Req2, State};

        {error, Reason} ->
            Req2 = cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                <<"error">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }), Req),
            {ok, Req2, undefined}
    end.

%% @private 处理 SSE 模式下的消息 POST
handle_sse_message(Req, _Config) ->
    {ok, _Body, Req2} = cowboy_req:read_body(Req),
    _Headers = maps:to_list(cowboy_req:headers(Req2)),

    %% TODO: 需要找到对应的 SSE 会话
    %% 这里简化处理，直接返回 202 Accepted
    %% 实际实现需要会话管理

    Req3 = cowboy_req:reply(202, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{<<"status">> => <<"accepted">>}), Req2),
    {ok, Req3, undefined}.

%% @doc 处理 SSE 流中的消息
%%
%% 用于 cowboy_loop 模式
info({send_event, EventType, Data}, Req, #sse_state{handler_state = _HS} = State) ->
    %% 发送 SSE 事件
    EventData = beamai_mcp_handler:format_sse_response(EventType, Data),
    cowboy_req:stream_body(iolist_to_binary(EventData), nofin, Req),
    {ok, Req, State};

info(close, Req, #sse_state{handler_state = HS} = State) ->
    beamai_mcp_handler:close(HS),
    {stop, Req, State};

info(_Info, Req, State) ->
    {ok, Req, State}.

%%====================================================================
%% CORS 和 OPTIONS 处理
%%====================================================================

%% @private 处理 OPTIONS 请求（CORS 预检）
handle_options(Req, Config) ->
    Mode = maps:get(mode, Config, streamable_http),
    Req2 = cowboy_req:reply(204, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => allowed_methods(Mode),
        <<"access-control-allow-headers">> => <<"content-type, mcp-session-id, authorization">>,
        <<"access-control-max-age">> => <<"86400">>
    }, <<>>, Req),
    {ok, Req2, undefined}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 构建响应头
-spec build_response_headers(json | sse, binary() | undefined) -> map().
build_response_headers(json, SessionId) ->
    Base = #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    },
    add_session_header(Base, SessionId);
build_response_headers(sse, SessionId) ->
    Base = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>
    },
    add_session_header(Base, SessionId).

%% @private 添加会话 ID 头
-spec add_session_header(map(), binary() | undefined) -> map().
add_session_header(Headers, undefined) ->
    Headers;
add_session_header(Headers, SessionId) ->
    Headers#{<<"mcp-session-id">> => SessionId}.

%% @private 获取允许的方法
-spec allowed_methods(atom()) -> binary().
allowed_methods(streamable_http) -> <<"POST, OPTIONS">>;
allowed_methods(sse_init) -> <<"GET, OPTIONS">>;
allowed_methods(sse_message) -> <<"POST, OPTIONS">>;
allowed_methods(_) -> <<"POST, GET, OPTIONS">>.
