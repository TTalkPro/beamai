%%%-------------------------------------------------------------------
%%% @doc MCP 类型定义模块
%%%
%%% 定义 MCP 协议相关的类型和转换函数。
%%%
%%% == MCP 协议概述 ==
%%%
%%% MCP 定义三种基本原语:
%%% - Tools: 模型可调用的函数（模型控制）
%%% - Resources: 应用提供的数据（应用控制）
%%% - Prompts: 用户可选择的提示模板（用户控制）
%%%
%%% == 类型导出 ==
%%%
%%% 本模块导出以下类型:
%%% - mcp_tool/0: 工具定义
%%% - mcp_resource/0: 资源定义
%%% - mcp_prompt/0: 提示定义
%%% - mcp_capabilities/0: 能力定义
%%% - content_type/0: 内容类型
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_types).

-include("beamai_mcp.hrl").

%%====================================================================
%% API 导出
%%====================================================================

%% 类型导出
-export_type([
    mcp_tool/0,
    mcp_resource/0,
    mcp_resource_template/0,
    mcp_prompt/0,
    mcp_prompt_arg/0,
    mcp_server_capabilities/0,
    mcp_client_capabilities/0,
    mcp_session/0,
    content_type/0,
    transport_type/0
]).

%% 转换函数
-export([
    tool_to_map/1,
    resource_to_map/1,
    resource_template_to_map/1,
    prompt_to_map/1,
    prompt_arg_to_map/1,
    server_capabilities_to_map/1,
    client_capabilities_to_map/1,
    session_to_map/1
]).

%% 构造函数
-export([
    make_tool/4,
    make_tool/5,
    make_resource/4,
    make_resource/6,
    make_resource_template/3,
    make_prompt/3,
    make_prompt/4,
    make_prompt_arg/2,
    make_prompt_arg/3,
    make_server_capabilities/0,
    make_server_capabilities/1,
    make_client_capabilities/0,
    make_client_capabilities/1
]).

%% 内容构造函数
-export([
    text_content/1,
    image_content/2,
    image_content/3,
    resource_content/2
]).

%%====================================================================
%% 类型定义
%%
%% MCP 协议定义了多种类型，这里使用 Erlang record 来表示。
%% 每种类型都有对应的构造函数和转换函数。
%%====================================================================

%% MCP 工具类型 - 模型可调用的函数
-type mcp_tool() :: #mcp_tool{}.

%% MCP 资源类型 - 应用暴露的数据
-type mcp_resource() :: #mcp_resource{}.

%% MCP 资源模板类型 - 动态资源的 URI 模板
-type mcp_resource_template() :: #mcp_resource_template{}.

%% MCP 提示类型 - 用户可选择的提示模板
-type mcp_prompt() :: #mcp_prompt{}.

%% MCP 提示参数类型
-type mcp_prompt_arg() :: #mcp_prompt_arg{}.

%% MCP 服务器能力类型
-type mcp_server_capabilities() :: #mcp_server_capabilities{}.

%% MCP 客户端能力类型
-type mcp_client_capabilities() :: #mcp_client_capabilities{}.

%% MCP 会话类型
-type mcp_session() :: #mcp_session{}.

%% 内容类型: text=文本, image=图片, resource=资源引用
-type content_type() :: text | image | resource.

%% 传输类型: stdio=标准IO, sse=服务器推送事件, http=HTTP
-type transport_type() :: stdio | sse | http.

%%====================================================================
%% 构造函数
%%
%% 提供创建 MCP 类型实例的便捷函数。
%% 使用这些函数而非直接构造 record，以确保字段正确初始化。
%%====================================================================

%% @doc 创建工具定义
%%
%% @param Name 工具名称
%% @param Description 工具描述
%% @param InputSchema 输入参数的 JSON Schema
%% @param Handler 处理函数
%% @returns mcp_tool()
-spec make_tool(binary(), binary(), map(), function()) -> mcp_tool().
make_tool(Name, Description, InputSchema, Handler) ->
    #mcp_tool{
        name = Name,
        description = Description,
        input_schema = InputSchema,
        handler = Handler
    }.

%% @doc 创建工具定义（带输出 Schema）
-spec make_tool(binary(), binary(), map(), map() | undefined, function()) -> mcp_tool().
make_tool(Name, Description, InputSchema, OutputSchema, Handler) ->
    #mcp_tool{
        name = Name,
        description = Description,
        input_schema = InputSchema,
        output_schema = OutputSchema,
        handler = Handler
    }.

%% @doc 创建资源定义（简化版）
%%
%% @param Uri 资源 URI
%% @param Name 资源名称
%% @param Description 资源描述
%% @param Handler 处理函数
%% @returns mcp_resource()
-spec make_resource(binary(), binary(), binary(), function()) -> mcp_resource().
make_resource(Uri, Name, Description, Handler) ->
    #mcp_resource{
        uri = Uri,
        name = Name,
        description = Description,
        handler = Handler
    }.

%% @doc 创建资源定义（完整版）
-spec make_resource(binary(), binary(), binary() | undefined, binary() | undefined,
                    binary() | undefined, function()) -> mcp_resource().
make_resource(Uri, Name, Title, Description, MimeType, Handler) ->
    #mcp_resource{
        uri = Uri,
        name = Name,
        title = Title,
        description = Description,
        mime_type = MimeType,
        handler = Handler
    }.

%% @doc 创建资源模板定义
-spec make_resource_template(binary(), binary(), binary() | undefined) -> mcp_resource_template().
make_resource_template(UriTemplate, Name, Description) ->
    #mcp_resource_template{
        uri_template = UriTemplate,
        name = Name,
        description = Description
    }.

%% @doc 创建提示定义（无参数）
-spec make_prompt(binary(), binary(), function()) -> mcp_prompt().
make_prompt(Name, Description, Handler) ->
    #mcp_prompt{
        name = Name,
        description = Description,
        arguments = [],
        handler = Handler
    }.

%% @doc 创建提示定义（带参数）
-spec make_prompt(binary(), binary(), [mcp_prompt_arg()], function()) -> mcp_prompt().
make_prompt(Name, Description, Arguments, Handler) ->
    #mcp_prompt{
        name = Name,
        description = Description,
        arguments = Arguments,
        handler = Handler
    }.

%% @doc 创建提示参数（可选）
-spec make_prompt_arg(binary(), binary()) -> mcp_prompt_arg().
make_prompt_arg(Name, Description) ->
    #mcp_prompt_arg{
        name = Name,
        description = Description,
        required = false
    }.

%% @doc 创建提示参数（指定必填）
-spec make_prompt_arg(binary(), binary(), boolean()) -> mcp_prompt_arg().
make_prompt_arg(Name, Description, Required) ->
    #mcp_prompt_arg{
        name = Name,
        description = Description,
        required = Required
    }.

%% @doc 创建默认服务器能力
-spec make_server_capabilities() -> mcp_server_capabilities().
make_server_capabilities() ->
    #mcp_server_capabilities{}.

%% @doc 创建服务器能力（从 map）
-spec make_server_capabilities(map()) -> mcp_server_capabilities().
make_server_capabilities(Map) ->
    #mcp_server_capabilities{
        tools = maps:get(tools, Map, false),
        resources = maps:get(resources, Map, false),
        prompts = maps:get(prompts, Map, false),
        logging = maps:get(logging, Map, false)
    }.

%% @doc 创建默认客户端能力
-spec make_client_capabilities() -> mcp_client_capabilities().
make_client_capabilities() ->
    #mcp_client_capabilities{}.

%% @doc 创建客户端能力（从 map）
-spec make_client_capabilities(map()) -> mcp_client_capabilities().
make_client_capabilities(Map) ->
    #mcp_client_capabilities{
        roots = maps:get(roots, Map, false),
        sampling = maps:get(sampling, Map, false),
        elicitation = maps:get(elicitation, Map, false)
    }.

%%====================================================================
%% 内容构造函数
%%
%% MCP 协议定义了三种内容类型：
%% - text: 纯文本内容
%% - image: Base64 编码的图片数据
%% - resource: 引用已有资源的内容
%%====================================================================

%% @doc 创建文本内容
%%
%% 这是最常用的内容类型，用于返回纯文本结果。
-spec text_content(binary()) -> map().
text_content(Text) ->
    #{
        <<"type">> => <<"text">>,
        <<"text">> => Text
    }.

%% @doc 创建图片内容
-spec image_content(binary(), binary()) -> map().
image_content(Data, MimeType) ->
    #{
        <<"type">> => <<"image">>,
        <<"data">> => Data,
        <<"mimeType">> => MimeType
    }.

%% @doc 创建图片内容（带注释）
-spec image_content(binary(), binary(), map()) -> map().
image_content(Data, MimeType, Annotations) ->
    #{
        <<"type">> => <<"image">>,
        <<"data">> => Data,
        <<"mimeType">> => MimeType,
        <<"annotations">> => Annotations
    }.

%% @doc 创建资源引用内容
-spec resource_content(binary(), binary()) -> map().
resource_content(Uri, MimeType) ->
    #{
        <<"type">> => <<"resource">>,
        <<"resource">> => #{
            <<"uri">> => Uri,
            <<"mimeType">> => MimeType
        }
    }.

%%====================================================================
%% 转换函数
%%
%% 将 Erlang record 转换为 MCP 协议的 JSON 格式（map）。
%% 这些函数用于构建 JSON-RPC 响应。
%%====================================================================

%% @doc 工具记录转为 map
%%
%% 转换后的 map 可直接用于 JSON 编码。
-spec tool_to_map(mcp_tool()) -> map().
tool_to_map(#mcp_tool{name = Name, description = Desc, input_schema = Schema,
                       output_schema = OutputSchema}) ->
    Base = #{
        <<"name">> => Name,
        <<"description">> => Desc,
        <<"inputSchema">> => Schema
    },
    case OutputSchema of
        undefined -> Base;
        _ -> Base#{<<"outputSchema">> => OutputSchema}
    end.

%% @doc 资源记录转为 map
-spec resource_to_map(mcp_resource()) -> map().
resource_to_map(#mcp_resource{uri = Uri, name = Name, title = Title,
                              description = Desc, mime_type = MimeType}) ->
    Base = #{
        <<"uri">> => Uri,
        <<"name">> => Name
    },
    add_optional_fields(Base, [
        {<<"title">>, Title},
        {<<"description">>, Desc},
        {<<"mimeType">>, MimeType}
    ]).

%% @doc 资源模板记录转为 map
-spec resource_template_to_map(mcp_resource_template()) -> map().
resource_template_to_map(#mcp_resource_template{uri_template = UriTemplate,
                                                 name = Name, title = Title,
                                                 description = Desc, mime_type = MimeType}) ->
    Base = #{
        <<"uriTemplate">> => UriTemplate,
        <<"name">> => Name
    },
    add_optional_fields(Base, [
        {<<"title">>, Title},
        {<<"description">>, Desc},
        {<<"mimeType">>, MimeType}
    ]).

%% @doc 提示记录转为 map
-spec prompt_to_map(mcp_prompt()) -> map().
prompt_to_map(#mcp_prompt{name = Name, title = Title, description = Desc,
                          arguments = Args}) ->
    Base = #{
        <<"name">> => Name
    },
    WithOptional = add_optional_fields(Base, [
        {<<"title">>, Title},
        {<<"description">>, Desc}
    ]),
    case Args of
        [] -> WithOptional;
        _ -> WithOptional#{<<"arguments">> => [prompt_arg_to_map(A) || A <- Args]}
    end.

%% @doc 提示参数记录转为 map
-spec prompt_arg_to_map(mcp_prompt_arg()) -> map().
prompt_arg_to_map(#mcp_prompt_arg{name = Name, description = Desc, required = Required}) ->
    Base = #{
        <<"name">> => Name,
        <<"required">> => Required
    },
    add_optional_fields(Base, [{<<"description">>, Desc}]).

%% @doc 服务器能力转为 map
-spec server_capabilities_to_map(mcp_server_capabilities()) -> map().
server_capabilities_to_map(#mcp_server_capabilities{tools = Tools, resources = Resources,
                                                      prompts = Prompts, logging = Logging}) ->
    Caps = #{},
    Caps1 = add_capability(Caps, <<"tools">>, Tools),
    Caps2 = add_capability(Caps1, <<"resources">>, Resources),
    Caps3 = add_capability(Caps2, <<"prompts">>, Prompts),
    add_capability(Caps3, <<"logging">>, Logging).

%% @doc 客户端能力转为 map
-spec client_capabilities_to_map(mcp_client_capabilities()) -> map().
client_capabilities_to_map(#mcp_client_capabilities{roots = Roots, sampling = Sampling,
                                                      elicitation = Elicitation}) ->
    Caps = #{},
    Caps1 = add_capability(Caps, <<"roots">>, Roots),
    Caps2 = add_capability(Caps1, <<"sampling">>, Sampling),
    add_capability(Caps2, <<"elicitation">>, Elicitation).

%% @doc 会话记录转为 map
-spec session_to_map(mcp_session()) -> map().
session_to_map(#mcp_session{id = Id, client_info = ClientInfo,
                             client_capabilities = ClientCaps,
                             protocol_version = Version,
                             created_at = CreatedAt, last_active = LastActive,
                             subscriptions = Subs}) ->
    #{
        <<"id">> => Id,
        <<"clientInfo">> => ClientInfo,
        <<"clientCapabilities">> => case ClientCaps of
            #mcp_client_capabilities{} -> client_capabilities_to_map(ClientCaps);
            Map when is_map(Map) -> Map
        end,
        <<"protocolVersion">> => Version,
        <<"createdAt">> => CreatedAt,
        <<"lastActive">> => LastActive,
        <<"subscriptions">> => Subs
    }.

%%====================================================================
%% 内部函数
%%
%% 辅助函数用于简化转换逻辑，处理可选字段和能力配置。
%%====================================================================

%% @private 添加可选字段到 map
%%
%% 跳过 undefined 值，只添加有效的可选字段。
-spec add_optional_fields(map(), [{binary(), term()}]) -> map().
add_optional_fields(Map, []) ->
    Map;
add_optional_fields(Map, [{_Key, undefined} | Rest]) ->
    add_optional_fields(Map, Rest);
add_optional_fields(Map, [{Key, Value} | Rest]) ->
    add_optional_fields(Map#{Key => Value}, Rest).

%% @private 添加能力到 map
%%
%% 根据能力值的类型决定如何添加：
%% - false: 不添加（能力未启用）
%% - true: 添加空 map（能力已启用，使用默认配置）
%% - map: 添加该 map（能力已启用，有自定义配置）
-spec add_capability(map(), binary(), boolean() | map()) -> map().
add_capability(Map, _Key, false) ->
    Map;
add_capability(Map, Key, true) ->
    Map#{Key => #{}};
add_capability(Map, Key, SubCaps) when is_map(SubCaps) ->
    Map#{Key => SubCaps}.
