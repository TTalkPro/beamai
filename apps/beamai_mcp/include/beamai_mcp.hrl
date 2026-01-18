%%%-------------------------------------------------------------------
%%% @doc MCP 头文件
%%%
%%% 定义 MCP 协议相关的宏、记录和类型。
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AGENT_MCP_HRL).
-define(AGENT_MCP_HRL, true).

%%====================================================================
%% 协议版本
%%====================================================================

-define(MCP_PROTOCOL_VERSION, <<"2025-11-25">>).
-define(MCP_SUPPORTED_VERSIONS, [<<"2025-11-25">>, <<"2025-03-26">>]).

%%====================================================================
%% 方法名
%%====================================================================

%% 生命周期
-define(MCP_METHOD_INITIALIZE, <<"initialize">>).
-define(MCP_METHOD_PING, <<"ping">>).

%% Tools
-define(MCP_METHOD_TOOLS_LIST, <<"tools/list">>).
-define(MCP_METHOD_TOOLS_CALL, <<"tools/call">>).

%% Resources
-define(MCP_METHOD_RESOURCES_LIST, <<"resources/list">>).
-define(MCP_METHOD_RESOURCES_READ, <<"resources/read">>).
-define(MCP_METHOD_RESOURCES_SUBSCRIBE, <<"resources/subscribe">>).
-define(MCP_METHOD_RESOURCES_UNSUBSCRIBE, <<"resources/unsubscribe">>).
-define(MCP_METHOD_RESOURCES_TEMPLATES_LIST, <<"resources/templates/list">>).

%% Prompts
-define(MCP_METHOD_PROMPTS_LIST, <<"prompts/list">>).
-define(MCP_METHOD_PROMPTS_GET, <<"prompts/get">>).

%% Logging
-define(MCP_METHOD_LOGGING_SET_LEVEL, <<"logging/setLevel">>).

%% Sampling (Client -> Server)
-define(MCP_METHOD_SAMPLING_CREATE_MESSAGE, <<"sampling/createMessage">>).

%%====================================================================
%% 通知
%%====================================================================

-define(MCP_NOTIFY_INITIALIZED, <<"notifications/initialized">>).
-define(MCP_NOTIFY_CANCELLED, <<"notifications/cancelled">>).
-define(MCP_NOTIFY_PROGRESS, <<"notifications/progress">>).
-define(MCP_NOTIFY_TOOLS_CHANGED, <<"notifications/tools/list_changed">>).
-define(MCP_NOTIFY_RESOURCES_CHANGED, <<"notifications/resources/list_changed">>).
-define(MCP_NOTIFY_RESOURCES_UPDATED, <<"notifications/resources/updated">>).
-define(MCP_NOTIFY_PROMPTS_CHANGED, <<"notifications/prompts/list_changed">>).
-define(MCP_NOTIFY_LOGGING_MESSAGE, <<"notifications/message">>).

%%====================================================================
%% 错误码
%%====================================================================

%% 标准 JSON-RPC 错误码
-define(MCP_ERROR_PARSE, -32700).
-define(MCP_ERROR_INVALID_REQUEST, -32600).
-define(MCP_ERROR_METHOD_NOT_FOUND, -32601).
-define(MCP_ERROR_INVALID_PARAMS, -32602).
-define(MCP_ERROR_INTERNAL, -32603).

%% MCP 特定错误码 (-32000 ~ -32099)
-define(MCP_ERROR_RESOURCE_NOT_FOUND, -32002).
-define(MCP_ERROR_TOOL_NOT_FOUND, -32003).
-define(MCP_ERROR_PROMPT_NOT_FOUND, -32004).

%%====================================================================
%% 超时配置
%%====================================================================

-define(MCP_DEFAULT_TIMEOUT, 30000).
-define(MCP_INIT_TIMEOUT, 10000).
-define(MCP_TOOL_CALL_TIMEOUT, 60000).

%%====================================================================
%% 记录定义
%%====================================================================

%% Tool 定义
-record(mcp_tool, {
    name :: binary(),
    description :: binary(),
    input_schema :: map(),
    output_schema = undefined :: map() | undefined,
    handler :: fun((map()) -> {ok, term()} | {error, term()})
}).

%% Resource 定义
-record(mcp_resource, {
    uri :: binary(),
    name :: binary(),
    title = undefined :: binary() | undefined,
    description = undefined :: binary() | undefined,
    mime_type = undefined :: binary() | undefined,
    handler :: fun(() -> {ok, binary() | map()} | {error, term()})
}).

%% Resource Template 定义
-record(mcp_resource_template, {
    uri_template :: binary(),
    name :: binary(),
    title = undefined :: binary() | undefined,
    description = undefined :: binary() | undefined,
    mime_type = undefined :: binary() | undefined
}).

%% Prompt 参数定义（必须在 mcp_prompt 之前定义）
-record(mcp_prompt_arg, {
    name :: binary(),
    description = undefined :: binary() | undefined,
    required = false :: boolean()
}).

%% Prompt 定义
-record(mcp_prompt, {
    name :: binary(),
    title = undefined :: binary() | undefined,
    description = undefined :: binary() | undefined,
    arguments = [] :: [#mcp_prompt_arg{}],
    handler :: fun((map()) -> {ok, [map()]} | {error, term()})
}).

%% 服务器能力
-record(mcp_server_capabilities, {
    tools = false :: boolean() | #{list_changed => boolean()},
    resources = false :: boolean() | #{subscribe => boolean(), list_changed => boolean()},
    prompts = false :: boolean() | #{list_changed => boolean()},
    logging = false :: boolean()
}).

%% 客户端能力
-record(mcp_client_capabilities, {
    roots = false :: boolean() | #{list_changed => boolean()},
    sampling = false :: boolean(),
    elicitation = false :: boolean()
}).

%% 会话状态
-record(mcp_session, {
    id :: binary(),
    client_info :: map(),
    client_capabilities :: #mcp_client_capabilities{} | map(),
    protocol_version :: binary(),
    created_at :: integer(),
    last_active :: integer(),
    subscriptions = [] :: [binary()]  %% 订阅的资源 URI
}).

-endif.
