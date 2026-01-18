%%%-------------------------------------------------------------------
%%% @doc A2A HTTP 请求处理模块（框架无关）
%%%
%%% 提供 A2A 协议的 HTTP 请求处理逻辑，与具体 HTTP 框架解耦。
%%% 用户可以将此模块集成到任何 HTTP 服务器框架中。
%%%
%%% == 设计理念 ==
%%%
%%% 本模块只负责：
%%% - 解析和验证 HTTP 请求
%%% - 执行认证和限流检查
%%% - 调用 beamai_a2a_server 处理业务逻辑
%%% - 格式化响应并返回响应头
%%%
%%% 不负责：
%%% - HTTP 服务器启动/配置
%%% - 路由配置
%%% - TLS（由用户的 HTTP 框架处理）
%%%
%%% == 支持的端点 ==
%%%
%%% 1. POST /a2a - JSON-RPC 请求端点
%%% 2. GET /.well-known/agent.json - Agent Card 端点
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 初始化配置
%%% Config = beamai_a2a_http_handler:init(#{
%%%     agent_config => AgentConfig
%%% }).
%%%
%%% %% 处理 JSON-RPC POST 请求
%%% case beamai_a2a_http_handler:handle_post(RequestBody, Headers, Config) of
%%%     {ok, ResponseJson, RespHeaders, NewConfig} ->
%%%         send_response(200, RespHeaders, ResponseJson);
%%%     {error, auth_error, ResponseJson, RespHeaders, NewConfig} ->
%%%         send_response(401, RespHeaders, ResponseJson);
%%%     {error, rate_limited, ResponseJson, RespHeaders, NewConfig} ->
%%%         send_response(429, RespHeaders, ResponseJson)
%%% end.
%%%
%%% %% 获取 Agent Card
%%% {ok, CardJson, RespHeaders} = beamai_a2a_http_handler:handle_agent_card(Config).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_http_handler).

-include_lib("beamai_core/include/beamai_common.hrl").

%%====================================================================
%% API 导出
%%====================================================================

-export([
    init/1,
    handle_post/3,
    handle_agent_card/1,
    build_rate_limit_headers/1,
    close/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(handler_state, {
    %% A2A 服务器进程
    server_pid :: pid() | undefined,
    %% 配置
    config :: map()
}).

-type handler_state() :: #handler_state{}.

%% 响应类型：
%% - {ok, Body, Headers, State} - 成功响应 (200)
%% - {error, auth_error, Body, Headers, State} - 认证错误 (401)
%% - {error, rate_limited, Body, Headers, State} - 限流 (429)
%% - {error, parse_error, Body, Headers, State} - 解析错误 (400)
-type response() ::
    {ok, binary(), map(), handler_state()} |
    {error, atom(), binary(), map(), handler_state()}.

-export_type([handler_state/0, response/0]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化 Handler 状态
%%
%% @param Config 配置参数
%%   - agent_config: Agent 配置 map（必需）
%%   - name: 服务器注册名称（可选）
%% @returns handler_state()
-spec init(map()) -> handler_state().
init(Config) ->
    %% 启动 A2A 服务器进程
    {ok, ServerPid} = beamai_a2a_server:start_link(Config),
    #handler_state{
        server_pid = ServerPid,
        config = Config
    }.

%% @doc 处理 HTTP POST 请求
%%
%% 这是 A2A JSON-RPC 端点的主要入口。
%% 执行认证、限流检查后处理请求。
%%
%% @param Body 请求体（JSON）
%% @param Headers 请求头 [{Name, Value}]
%% @param State Handler 状态
%% @returns {ok, ResponseJson, RespHeaders, NewState} |
%%          {error, ErrorType, ResponseJson, RespHeaders, NewState}
-spec handle_post(binary(), [{binary(), binary()}], handler_state()) -> response().
handle_post(Body, Headers, #handler_state{server_pid = ServerPid} = State) ->
    %% 基础响应头
    BaseHeaders = #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    },

    %% 解码 JSON
    case beamai_a2a_jsonrpc:decode(Body) of
        {ok, Request} ->
            %% 使用带认证的处理
            case beamai_a2a_server:handle_request_with_auth(ServerPid, Request, Headers) of
                {ok, Response, RateLimitInfo} ->
                    %% 成功
                    RespHeaders = merge_headers(BaseHeaders, build_rate_limit_headers(RateLimitInfo)),
                    {ok, jsx:encode(Response, []), RespHeaders, State};

                {error, auth_error, ErrorResponse} ->
                    %% 认证错误
                    {error, auth_error, jsx:encode(ErrorResponse, []), BaseHeaders, State};

                {error, rate_limited, ErrorResponse} when is_map(ErrorResponse) ->
                    %% 限流 - ErrorResponse 包含限流信息
                    RateLimitInfo = extract_rate_limit_from_error(ErrorResponse),
                    RespHeaders = merge_headers(BaseHeaders, build_rate_limit_headers(RateLimitInfo)),
                    {error, rate_limited, jsx:encode(ErrorResponse, []), RespHeaders, State};

                {error, _Reason, RateLimitInfo} ->
                    %% 其他错误
                    RespHeaders = merge_headers(BaseHeaders, build_rate_limit_headers(RateLimitInfo)),
                    ErrorResp = beamai_a2a_jsonrpc:internal_error(maps:get(<<"id">>, Request, null)),
                    {ok, jsx:encode(ErrorResp, []), RespHeaders, State}
            end;

        {error, parse_error} ->
            %% JSON 解析错误
            ErrorResp = beamai_a2a_jsonrpc:parse_error(null),
            {error, parse_error, jsx:encode(ErrorResp, []), BaseHeaders, State};

        {error, _Reason} ->
            %% 无效请求
            ErrorResp = beamai_a2a_jsonrpc:invalid_request(null),
            {error, parse_error, jsx:encode(ErrorResp, []), BaseHeaders, State}
    end.

%% @doc 处理 Agent Card 请求
%%
%% 用于 GET /.well-known/agent.json 端点。
%%
%% @param State Handler 状态
%% @returns {ok, CardJson, RespHeaders}
-spec handle_agent_card(handler_state()) -> {ok, binary(), map()}.
handle_agent_card(#handler_state{server_pid = ServerPid}) ->
    {ok, Card} = beamai_a2a_server:get_agent_card(ServerPid),
    RespHeaders = #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>,
        <<"cache-control">> => <<"public, max-age=3600">>
    },
    {ok, jsx:encode(Card, []), RespHeaders}.

%% @doc 构建限流响应头
%%
%% 根据限流信息构建标准的 Rate Limit 响应头。
%%
%% @param RateLimitInfo 限流信息 map
%% @returns 响应头 map
-spec build_rate_limit_headers(map()) -> map().
build_rate_limit_headers(#{enabled := false}) ->
    #{};
build_rate_limit_headers(RateLimitInfo) ->
    Remaining = maps:get(remaining, RateLimitInfo, 0),
    Limit = maps:get(limit, RateLimitInfo, 0),
    ResetAt = maps:get(reset_at, RateLimitInfo, 0),
    RetryAfter = maps:get(retry_after, RateLimitInfo, 0),

    Base = #{
        <<"x-ratelimit-limit">> => integer_to_binary(Limit),
        <<"x-ratelimit-remaining">> => integer_to_binary(Remaining),
        <<"x-ratelimit-reset">> => integer_to_binary(ResetAt)
    },

    %% 如果被限流，添加 Retry-After 头
    case RetryAfter > 0 of
        true ->
            %% 转换为秒
            RetryAfterSecs = max(1, RetryAfter div 1000),
            Base#{<<"retry-after">> => integer_to_binary(RetryAfterSecs)};
        false ->
            Base
    end.

%% @doc 关闭 Handler
%%
%% 停止关联的 A2A 服务器进程。
-spec close(handler_state()) -> ok.
close(#handler_state{server_pid = undefined}) ->
    ok;
close(#handler_state{server_pid = ServerPid}) ->
    try
        beamai_a2a_server:stop(ServerPid)
    catch
        exit:noproc -> ok;
        exit:{noproc, _} -> ok
    end,
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 合并响应头
-spec merge_headers(map(), map()) -> map().
merge_headers(Base, Additional) ->
    maps:merge(Base, Additional).

%% @private 从错误响应中提取限流信息
-spec extract_rate_limit_from_error(map()) -> map().
extract_rate_limit_from_error(ErrorResponse) ->
    case maps:get(<<"error">>, ErrorResponse, undefined) of
        undefined ->
            #{};
        Error ->
            case maps:get(<<"data">>, Error, undefined) of
                undefined ->
                    #{};
                Data ->
                    #{
                        retry_after => maps:get(<<"retryAfter">>, Data, 0),
                        reset_at => maps:get(<<"resetAt">>, Data, 0),
                        limit => maps:get(<<"limit">>, Data, 0)
                    }
            end
    end.
