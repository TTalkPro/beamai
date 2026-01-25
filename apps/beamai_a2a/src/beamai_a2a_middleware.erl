%%%-------------------------------------------------------------------
%%% @doc A2A 认证和限流中间件
%%%
%%% 提供 HTTP 请求的认证和限流检查功能。
%%% 此模块从 beamai_a2a_server 中分离出来，专注于安全相关的中间件逻辑。
%%%
%%% == 主要功能 ==
%%%
%%% 1. 认证检查
%%%    - 验证 API Key（Bearer Token 或 X-API-Key）
%%%    - 支持禁用认证模式
%%%
%%% 2. 限流检查
%%%    - 基于 API Key 或 IP 地址的限流
%%%    - 支持禁用限流模式
%%%
%%% 3. 错误响应构建
%%%    - 标准化的认证错误响应
%%%    - 标准化的限流错误响应
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 检查认证和限流
%%% Headers = [{<<"authorization">>, <<"Bearer my-api-key">>}],
%%% case beamai_a2a_middleware:check_auth_and_rate_limit(Headers) of
%%%     {ok, AuthInfo, RateLimitInfo} ->
%%%         %% 认证和限流都通过
%%%         handle_request(...);
%%%     {error, auth_disabled, RateLimitInfo} ->
%%%         %% 认证已禁用，直接处理
%%%         handle_request(...);
%%%     {error, missing_api_key, _} ->
%%%         %% 缺少 API Key
%%%         return_auth_error(401);
%%%     {error, rate_limited, RateLimitInfo} ->
%%%         %% 限流
%%%         return_rate_limit_error(429, RateLimitInfo)
%%% end.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_middleware).

%% API 导出
-export([
    %% 认证和限流检查
    check_auth_and_rate_limit/1,
    check_rate_limit_only/1,

    %% 错误响应构建
    make_auth_error_response/2,
    make_rate_limit_error_response/2,

    %% HTTP 头部工具
    get_header/2,
    get_rate_limit_key/1,
    get_rate_limit_key_from_headers/1
]).

%%====================================================================
%% 认证和限流检查
%%====================================================================

%% @doc 检查认证和限流
%%
%% 执行顺序：
%% 1. 检查认证是否启用
%% 2. 如果启用，验证 API Key
%% 3. 检查限流
%%
%% @param Headers HTTP 请求头列表 [{Name, Value}]
%% @returns
%% - {ok, AuthInfo, RateLimitInfo} - 认证和限流都通过
%% - {error, auth_disabled, RateLimitInfo} - 认证已禁用
%% - {error, AuthReason, #{}} - 认证失败
%% - {error, rate_limited, RateLimitInfo} - 限流
-spec check_auth_and_rate_limit([{binary(), binary()}]) ->
    {ok, map(), map()} | {error, atom(), map()}.
check_auth_and_rate_limit(Headers) ->
    %% 步骤 1：检查认证
    case beamai_a2a_auth:is_enabled() of
        false ->
            %% 认证已禁用，只检查限流
            check_rate_limit_only(Headers);
        true ->
            case beamai_a2a_auth:authenticate(Headers) of
                {ok, AuthInfo} ->
                    %% 认证成功，检查限流
                    RateLimitKey = get_rate_limit_key(AuthInfo),
                    check_rate_limit(RateLimitKey, AuthInfo);
                {error, Reason} ->
                    {error, Reason, #{}}
            end
    end.

%% @doc 仅检查限流（认证已禁用时使用）
%%
%% 当认证禁用时，使用 IP 地址或默认 Key 进行限流。
%%
%% @param Headers HTTP 请求头列表
%% @returns {error, auth_disabled, RateLimitInfo} | {error, rate_limited, RateLimitInfo}
-spec check_rate_limit_only([{binary(), binary()}]) ->
    {error, atom(), map()}.
check_rate_limit_only(Headers) ->
    %% 使用 IP 或默认 Key 进行限流
    RateLimitKey = get_rate_limit_key_from_headers(Headers),
    case beamai_a2a_rate_limit:is_enabled() of
        false ->
            {error, auth_disabled, #{enabled => false}};
        true ->
            case beamai_a2a_rate_limit:check_and_consume(RateLimitKey) of
                {ok, RateLimitInfo} ->
                    {error, auth_disabled, RateLimitInfo};
                {error, rate_limited, RateLimitInfo} ->
                    {error, rate_limited, RateLimitInfo}
            end
    end.

%% @private 检查限流
check_rate_limit(RateLimitKey, AuthInfo) ->
    case beamai_a2a_rate_limit:is_enabled() of
        false ->
            {ok, AuthInfo, #{enabled => false}};
        true ->
            case beamai_a2a_rate_limit:check_and_consume(RateLimitKey) of
                {ok, RateLimitInfo} ->
                    {ok, AuthInfo, RateLimitInfo};
                {error, rate_limited, RateLimitInfo} ->
                    {error, rate_limited, RateLimitInfo}
            end
    end.

%%====================================================================
%% 限流 Key 获取
%%====================================================================

%% @doc 从认证信息获取限流 Key
%%
%% 优先使用 API Key，其次是匿名标识，最后是默认值。
%%
%% @param AuthInfo 认证信息 map
%% @returns 限流 Key 二进制字符串
-spec get_rate_limit_key(map()) -> binary().
get_rate_limit_key(#{key := ApiKey}) ->
    ApiKey;
get_rate_limit_key(#{anonymous := true}) ->
    <<"anonymous">>;
get_rate_limit_key(_) ->
    <<"default">>.

%% @doc 从请求头获取限流 Key
%%
%% 用于认证禁用时的限流。尝试获取客户端 IP 地址。
%%
%% @param Headers HTTP 请求头列表
%% @returns 限流 Key 二进制字符串
-spec get_rate_limit_key_from_headers([{binary(), binary()}]) -> binary().
get_rate_limit_key_from_headers(Headers) ->
    %% 尝试获取 X-Forwarded-For 或 X-Real-IP
    case get_header(Headers, <<"x-forwarded-for">>) of
        undefined ->
            case get_header(Headers, <<"x-real-ip">>) of
                undefined -> <<"anonymous">>;
                IP -> <<"ip:", IP/binary>>
            end;
        ForwardedFor ->
            %% 取第一个 IP（最左边是原始客户端）
            [FirstIP | _] = binary:split(ForwardedFor, <<",">>),
            <<"ip:", (string:trim(FirstIP))/binary>>
    end.

%%====================================================================
%% HTTP 头部工具
%%====================================================================

%% @doc 从请求头列表获取指定头部值
%%
%% 支持大小写不敏感的匹配。
%%
%% @param Headers HTTP 请求头列表 [{Name, Value}]
%% @param Name 要获取的头部名称（小写）
%% @returns 头部值或 undefined
-spec get_header([{binary(), binary()}], binary()) -> binary() | undefined.
get_header([], _Name) ->
    undefined;
get_header([{Name, Value} | _Rest], Name) ->
    Value;
get_header([{HeaderName, Value} | Rest], Name) ->
    %% 大小写不敏感匹配
    case string:lowercase(HeaderName) of
        LowerName when LowerName =:= Name -> Value;
        _ -> get_header(Rest, Name)
    end.

%%====================================================================
%% 错误响应构建
%%====================================================================

%% @doc 创建认证错误响应
%%
%% 根据错误原因创建标准的 JSON-RPC 错误响应。
%%
%% @param Id JSON-RPC 请求 ID
%% @param Reason 错误原因 atom
%% @returns JSON-RPC 错误响应 map
-spec make_auth_error_response(term(), atom()) -> map().
make_auth_error_response(Id, missing_api_key) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32010,
            <<"message">> => <<"Authentication required">>,
            <<"data">> => #{
                <<"reason">> => <<"missing_api_key">>,
                <<"hint">> => <<"Provide API key via Authorization header (Bearer <key>) or X-API-Key header">>
            }
        }
    };
make_auth_error_response(Id, invalid_api_key) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32011,
            <<"message">> => <<"Invalid API key">>,
            <<"data">> => #{
                <<"reason">> => <<"invalid_api_key">>
            }
        }
    };
make_auth_error_response(Id, key_expired) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32012,
            <<"message">> => <<"API key expired">>,
            <<"data">> => #{
                <<"reason">> => <<"key_expired">>
            }
        }
    };
make_auth_error_response(Id, insufficient_permissions) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32013,
            <<"message">> => <<"Insufficient permissions">>,
            <<"data">> => #{
                <<"reason">> => <<"insufficient_permissions">>
            }
        }
    };
make_auth_error_response(Id, Reason) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32010,
            <<"message">> => <<"Authentication failed">>,
            <<"data">> => #{
                <<"reason">> => format_error(Reason)
            }
        }
    }.

%% @doc 创建限流错误响应
%%
%% @param Id JSON-RPC 请求 ID
%% @param RateLimitInfo 限流信息 map（包含 retry_after, reset_at, limit）
%% @returns JSON-RPC 错误响应 map
-spec make_rate_limit_error_response(term(), map()) -> map().
make_rate_limit_error_response(Id, RateLimitInfo) ->
    RetryAfter = maps:get(retry_after, RateLimitInfo, 60000),
    ResetAt = maps:get(reset_at, RateLimitInfo, 0),
    Limit = maps:get(limit, RateLimitInfo, 0),
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32020,
            <<"message">> => <<"Rate limit exceeded">>,
            <<"data">> => #{
                <<"reason">> => <<"rate_limited">>,
                <<"retryAfter">> => RetryAfter,
                <<"resetAt">> => ResetAt,
                <<"limit">> => Limit
            }
        }
    }.

%%====================================================================
%% 内部函数（使用公共模块）
%%====================================================================

%% @private 格式化错误（委托给公共模块）
format_error(Reason) ->
    beamai_a2a_utils:format_error(Reason).
