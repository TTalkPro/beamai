%%%-------------------------------------------------------------------
%%% @doc MCP 服务器
%%%
%%% 使用 gen_server 实现 MCP 服务器会话管理。
%%% 每个服务器实例处理一个客户端连接。
%%%
%%% == 功能概述 ==
%%%
%%% - 处理客户端的初始化请求
%%% - 提供工具、资源、提示列表
%%% - 执行工具调用
%%% - 读取资源
%%% - 管理资源订阅
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 在 Cowboy handler 中启动服务器实例
%%% {ok, Pid} = beamai_mcp_server:start_link(#{
%%%     tools => [
%%%         beamai_mcp_types:make_tool(
%%%             <<"echo">>,
%%%             <<"Echo the input">>,
%%%             #{type => object, properties => #{text => #{type => string}}},
%%%             fun(#{<<"text">> := Text}) -> {ok, Text} end
%%%         )
%%%     ],
%%%     resources => [],
%%%     prompts => [],
%%%     server_info => #{name => <<"my-server">>, version => <<"1.0">>}
%%% }).
%%%
%%% %% 处理请求
%%% Response = beamai_mcp_server:handle_request(Pid, RequestJson).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_server).

-behaviour(gen_server).

-include("beamai_mcp.hrl").

%%====================================================================
%% API 导出
%%====================================================================

-export([
    start_link/1,
    stop/1,
    handle_request/2,
    handle_notification/2,
    register_tool/2,
    register_resource/2,
    register_prompt/2,
    unregister_tool/2,
    unregister_resource/2,
    unregister_prompt/2,
    get_session/1
]).

%%====================================================================
%% gen_server 回调
%%====================================================================

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(state, {
    %% 服务器配置
    server_info :: map(),
    server_capabilities :: #mcp_server_capabilities{},
    %% 会话信息
    session :: #mcp_session{} | undefined,
    initialized = false :: boolean(),
    %% 原语注册
    tools = #{} :: #{binary() => #mcp_tool{}},
    resources = #{} :: #{binary() => #mcp_resource{}},
    resource_templates = [] :: [#mcp_resource_template{}],
    prompts = #{} :: #{binary() => #mcp_prompt{}},
    %% 订阅管理
    subscriptions = #{} :: #{binary() => [pid()]}
}).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动 MCP 服务器实例
%%
%% @param Config 服务器配置
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc 停止服务器
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc 处理 JSON-RPC 请求
%%
%% @param Pid 服务器进程
%% @param Request JSON 请求（binary 或已解码的 map）
%% @returns {ok, Response} | {error, Reason}
-spec handle_request(pid(), binary() | map()) -> {ok, binary()} | {error, term()}.
handle_request(Pid, Request) when is_binary(Request) ->
    case beamai_mcp_jsonrpc:decode(Request) of
        {ok, Decoded} ->
            handle_request(Pid, Decoded);
        {error, _} = Error ->
            Error
    end;
handle_request(Pid, Request) when is_map(Request) ->
    gen_server:call(Pid, {handle_request, Request}).

%% @doc 处理 JSON-RPC 通知
-spec handle_notification(pid(), binary() | map()) -> ok.
handle_notification(Pid, Notification) when is_binary(Notification) ->
    case beamai_mcp_jsonrpc:decode(Notification) of
        {ok, Decoded} ->
            handle_notification(Pid, Decoded);
        {error, _} ->
            ok
    end;
handle_notification(Pid, Notification) when is_map(Notification) ->
    gen_server:cast(Pid, {handle_notification, Notification}).

%% @doc 注册工具
-spec register_tool(pid(), #mcp_tool{}) -> ok.
register_tool(Pid, Tool) ->
    gen_server:call(Pid, {register_tool, Tool}).

%% @doc 注册资源
-spec register_resource(pid(), #mcp_resource{}) -> ok.
register_resource(Pid, Resource) ->
    gen_server:call(Pid, {register_resource, Resource}).

%% @doc 注册提示
-spec register_prompt(pid(), #mcp_prompt{}) -> ok.
register_prompt(Pid, Prompt) ->
    gen_server:call(Pid, {register_prompt, Prompt}).

%% @doc 注销工具
-spec unregister_tool(pid(), binary()) -> ok.
unregister_tool(Pid, Name) ->
    gen_server:call(Pid, {unregister_tool, Name}).

%% @doc 注销资源
-spec unregister_resource(pid(), binary()) -> ok.
unregister_resource(Pid, Uri) ->
    gen_server:call(Pid, {unregister_resource, Uri}).

%% @doc 注销提示
-spec unregister_prompt(pid(), binary()) -> ok.
unregister_prompt(Pid, Name) ->
    gen_server:call(Pid, {unregister_prompt, Name}).

%% @doc 获取会话信息
-spec get_session(pid()) -> {ok, #mcp_session{}} | {error, not_initialized}.
get_session(Pid) ->
    gen_server:call(Pid, get_session).

%%====================================================================
%% gen_server 回调
%%====================================================================

init(Config) ->
    ServerInfo = maps:get(server_info, Config, #{
        <<"name">> => <<"erlang-mcp-server">>,
        <<"version">> => <<"0.1.0">>
    }),

    %% 初始化工具
    Tools = lists:foldl(fun(#mcp_tool{name = Name} = Tool, Acc) ->
        Acc#{Name => Tool}
    end, #{}, maps:get(tools, Config, [])),

    %% 初始化资源
    Resources = lists:foldl(fun(#mcp_resource{uri = Uri} = Res, Acc) ->
        Acc#{Uri => Res}
    end, #{}, maps:get(resources, Config, [])),

    %% 初始化提示
    Prompts = lists:foldl(fun(#mcp_prompt{name = Name} = Prompt, Acc) ->
        Acc#{Name => Prompt}
    end, #{}, maps:get(prompts, Config, [])),

    %% 计算能力
    Capabilities = #mcp_server_capabilities{
        tools = map_size(Tools) > 0,
        resources = map_size(Resources) > 0,
        prompts = map_size(Prompts) > 0,
        logging = maps:get(logging, Config, false)
    },

    State = #state{
        server_info = ServerInfo,
        server_capabilities = Capabilities,
        tools = Tools,
        resources = Resources,
        prompts = Prompts
    },
    {ok, State}.

handle_call({handle_request, Request}, _From, State) ->
    {Response, NewState} = do_handle_request(Request, State),
    {reply, {ok, Response}, NewState};

handle_call({register_tool, #mcp_tool{name = Name} = Tool}, _From,
            #state{tools = Tools} = State) ->
    NewTools = Tools#{Name => Tool},
    NewState = update_capabilities(State#state{tools = NewTools}),
    {reply, ok, NewState};

handle_call({register_resource, #mcp_resource{uri = Uri} = Resource}, _From,
            #state{resources = Resources} = State) ->
    NewResources = Resources#{Uri => Resource},
    NewState = update_capabilities(State#state{resources = NewResources}),
    {reply, ok, NewState};

handle_call({register_prompt, #mcp_prompt{name = Name} = Prompt}, _From,
            #state{prompts = Prompts} = State) ->
    NewPrompts = Prompts#{Name => Prompt},
    NewState = update_capabilities(State#state{prompts = NewPrompts}),
    {reply, ok, NewState};

handle_call({unregister_tool, Name}, _From, #state{tools = Tools} = State) ->
    NewTools = maps:remove(Name, Tools),
    NewState = update_capabilities(State#state{tools = NewTools}),
    {reply, ok, NewState};

handle_call({unregister_resource, Uri}, _From, #state{resources = Resources} = State) ->
    NewResources = maps:remove(Uri, Resources),
    NewState = update_capabilities(State#state{resources = NewResources}),
    {reply, ok, NewState};

handle_call({unregister_prompt, Name}, _From, #state{prompts = Prompts} = State) ->
    NewPrompts = maps:remove(Name, Prompts),
    NewState = update_capabilities(State#state{prompts = NewPrompts}),
    {reply, ok, NewState};

handle_call(get_session, _From, #state{session = undefined} = State) ->
    {reply, {error, not_initialized}, State};
handle_call(get_session, _From, #state{session = Session} = State) ->
    {reply, {ok, Session}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({handle_notification, Notification}, State) ->
    NewState = do_handle_notification(Notification, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数 - 请求处理
%%====================================================================

%% @private 处理请求
do_handle_request(#{<<"id">> := Id, <<"method">> := Method} = Req, State) ->
    Params = maps:get(<<"params">>, Req, #{}),
    case dispatch_method(Method, Params, Id, State) of
        {ok, Result, NewState} ->
            Response = beamai_mcp_jsonrpc:encode_response(Id, Result),
            {Response, NewState};
        {error, Code, Message, NewState} ->
            Response = beamai_mcp_jsonrpc:encode_error(Id, Code, Message),
            {Response, NewState};
        {error, Code, Message, Data, NewState} ->
            Response = beamai_mcp_jsonrpc:encode_error(Id, Code, Message, Data),
            {Response, NewState}
    end;
do_handle_request(#{<<"id">> := Id}, State) ->
    Response = beamai_mcp_jsonrpc:invalid_request(Id),
    {Response, State};
do_handle_request(_, State) ->
    Response = beamai_mcp_jsonrpc:invalid_request(null),
    {Response, State}.

%% @private 方法分发
dispatch_method(?MCP_METHOD_INITIALIZE, Params, _Id, State) ->
    handle_initialize(Params, State);
dispatch_method(?MCP_METHOD_PING, _Params, _Id, State) ->
    {ok, #{}, State};
dispatch_method(_Method, _Params, _Id, #state{initialized = false} = State) ->
    {error, ?MCP_ERROR_INVALID_REQUEST, <<"Server not initialized">>, State};
dispatch_method(?MCP_METHOD_TOOLS_LIST, Params, _Id, State) ->
    handle_tools_list(Params, State);
dispatch_method(?MCP_METHOD_TOOLS_CALL, Params, _Id, State) ->
    handle_tools_call(Params, State);
dispatch_method(?MCP_METHOD_RESOURCES_LIST, Params, _Id, State) ->
    handle_resources_list(Params, State);
dispatch_method(?MCP_METHOD_RESOURCES_READ, Params, _Id, State) ->
    handle_resources_read(Params, State);
dispatch_method(?MCP_METHOD_RESOURCES_SUBSCRIBE, Params, _Id, State) ->
    handle_resources_subscribe(Params, State);
dispatch_method(?MCP_METHOD_RESOURCES_UNSUBSCRIBE, Params, _Id, State) ->
    handle_resources_unsubscribe(Params, State);
dispatch_method(?MCP_METHOD_PROMPTS_LIST, Params, _Id, State) ->
    handle_prompts_list(Params, State);
dispatch_method(?MCP_METHOD_PROMPTS_GET, Params, _Id, State) ->
    handle_prompts_get(Params, State);
dispatch_method(Method, _Params, _Id, State) ->
    {error, ?MCP_ERROR_METHOD_NOT_FOUND, <<"Method not found">>,
     #{<<"method">> => Method}, State}.

%%====================================================================
%% 内部函数 - 生命周期方法
%%====================================================================

%% @private 处理初始化
handle_initialize(Params, #state{server_info = ServerInfo,
                                  server_capabilities = Caps} = State) ->
    ClientInfo = maps:get(<<"clientInfo">>, Params, #{}),
    ClientCaps = maps:get(<<"capabilities">>, Params, #{}),
    ProtocolVersion = maps:get(<<"protocolVersion">>, Params, ?MCP_PROTOCOL_VERSION),

    %% 检查协议版本
    case lists:member(ProtocolVersion, ?MCP_SUPPORTED_VERSIONS) of
        true ->
            SessionId = generate_session_id(),
            Session = #mcp_session{
                id = SessionId,
                client_info = ClientInfo,
                client_capabilities = ClientCaps,
                protocol_version = ProtocolVersion,
                created_at = erlang:system_time(millisecond),
                last_active = erlang:system_time(millisecond)
            },
            Result = #{
                <<"protocolVersion">> => ProtocolVersion,
                <<"capabilities">> => beamai_mcp_types:server_capabilities_to_map(Caps),
                <<"serverInfo">> => ServerInfo
            },
            NewState = State#state{
                session = Session,
                initialized = true
            },
            {ok, Result, NewState};
        false ->
            {error, ?MCP_ERROR_INVALID_REQUEST, <<"Unsupported protocol version">>,
             #{<<"supported">> => ?MCP_SUPPORTED_VERSIONS}, State}
    end.

%%====================================================================
%% 内部函数 - 工具方法
%%====================================================================

%% @private 列出工具
handle_tools_list(_Params, #state{tools = Tools} = State) ->
    ToolList = [beamai_mcp_types:tool_to_map(T) || T <- maps:values(Tools)],
    {ok, #{<<"tools">> => ToolList}, State}.

%% @private 调用工具
handle_tools_call(#{<<"name">> := Name} = Params, #state{tools = Tools} = State) ->
    case maps:get(Name, Tools, undefined) of
        undefined ->
            {error, ?MCP_ERROR_TOOL_NOT_FOUND, <<"Tool not found">>,
             #{<<"tool">> => Name}, State};
        #mcp_tool{handler = Handler} ->
            Arguments = maps:get(<<"arguments">>, Params, #{}),
            try Handler(Arguments) of
                {ok, Result} ->
                    ResultContent = format_tool_result(Result),
                    {ok, #{<<"content">> => ResultContent, <<"isError">> => false}, State};
                {error, Error} ->
                    ErrorContent = [beamai_mcp_types:text_content(
                        iolist_to_binary(io_lib:format("~p", [Error]))
                    )],
                    {ok, #{<<"content">> => ErrorContent, <<"isError">> => true}, State}
            catch
                _:Exception:_Stack ->
                    ExceptionContent = [beamai_mcp_types:text_content(
                        iolist_to_binary(io_lib:format("Tool execution failed: ~p", [Exception]))
                    )],
                    {ok, #{<<"content">> => ExceptionContent, <<"isError">> => true}, State}
            end
    end;
handle_tools_call(_Params, State) ->
    {error, ?MCP_ERROR_INVALID_PARAMS, <<"Missing tool name">>, State}.

%%====================================================================
%% 内部函数 - 资源方法
%%====================================================================

%% @private 列出资源
handle_resources_list(_Params, #state{resources = Resources} = State) ->
    ResourceList = [beamai_mcp_types:resource_to_map(R) || R <- maps:values(Resources)],
    {ok, #{<<"resources">> => ResourceList}, State}.

%% @private 读取资源
handle_resources_read(#{<<"uri">> := Uri}, #state{resources = Resources} = State) ->
    case maps:get(Uri, Resources, undefined) of
        undefined ->
            {error, ?MCP_ERROR_RESOURCE_NOT_FOUND, <<"Resource not found">>,
             #{<<"uri">> => Uri}, State};
        #mcp_resource{handler = Handler, mime_type = MimeType} ->
            try
                case Handler() of
                    {ok, Content} when is_binary(Content) ->
                        {ok, #{
                            <<"contents">> => [#{
                                <<"uri">> => Uri,
                                <<"mimeType">> => default_mime_type(MimeType),
                                <<"text">> => Content
                            }]
                        }, State};
                    {ok, Content} when is_map(Content) ->
                        {ok, #{<<"contents">> => [Content#{<<"uri">> => Uri}]}, State};
                    {error, Error} ->
                        {error, ?MCP_ERROR_INTERNAL, <<"Resource read failed">>,
                         #{<<"error">> => iolist_to_binary(io_lib:format("~p", [Error]))}, State}
                end
            catch
                _:Exception:_Stack ->
                    {error, ?MCP_ERROR_INTERNAL, <<"Resource read exception">>,
                     #{<<"error">> => iolist_to_binary(io_lib:format("~p", [Exception]))}, State}
            end
    end;
handle_resources_read(_Params, State) ->
    {error, ?MCP_ERROR_INVALID_PARAMS, <<"Missing resource URI">>, State}.

%% @private 订阅资源
handle_resources_subscribe(#{<<"uri">> := Uri}, #state{subscriptions = Subs} = State) ->
    %% TODO: 实际的订阅逻辑
    NewSubs = maps:update_with(Uri, fun(L) -> [self() | L] end, [self()], Subs),
    {ok, #{}, State#state{subscriptions = NewSubs}}.

%% @private 取消订阅
handle_resources_unsubscribe(#{<<"uri">> := Uri}, #state{subscriptions = Subs} = State) ->
    NewSubs = maps:update_with(Uri, fun(L) -> lists:delete(self(), L) end, [], Subs),
    {ok, #{}, State#state{subscriptions = NewSubs}}.

%%====================================================================
%% 内部函数 - 提示方法
%%====================================================================

%% @private 列出提示
handle_prompts_list(_Params, #state{prompts = Prompts} = State) ->
    PromptList = [beamai_mcp_types:prompt_to_map(P) || P <- maps:values(Prompts)],
    {ok, #{<<"prompts">> => PromptList}, State}.

%% @private 获取提示
handle_prompts_get(#{<<"name">> := Name} = Params, #state{prompts = Prompts} = State) ->
    case maps:get(Name, Prompts, undefined) of
        undefined ->
            {error, ?MCP_ERROR_PROMPT_NOT_FOUND, <<"Prompt not found">>,
             #{<<"prompt">> => Name}, State};
        #mcp_prompt{handler = Handler} ->
            Arguments = maps:get(<<"arguments">>, Params, #{}),
            try
                case Handler(Arguments) of
                    {ok, Messages} ->
                        {ok, #{<<"messages">> => Messages}, State};
                    {error, Error} ->
                        {error, ?MCP_ERROR_INTERNAL, <<"Prompt execution failed">>,
                         #{<<"error">> => iolist_to_binary(io_lib:format("~p", [Error]))}, State}
                end
            catch
                _:Exception:_Stack ->
                    {error, ?MCP_ERROR_INTERNAL, <<"Prompt exception">>,
                     #{<<"error">> => iolist_to_binary(io_lib:format("~p", [Exception]))}, State}
            end
    end;
handle_prompts_get(_Params, State) ->
    {error, ?MCP_ERROR_INVALID_PARAMS, <<"Missing prompt name">>, State}.

%%====================================================================
%% 内部函数 - 通知处理
%%====================================================================

%% @private 处理通知
do_handle_notification(#{<<"method">> := ?MCP_NOTIFY_INITIALIZED}, State) ->
    %% 客户端已完成初始化
    State;
do_handle_notification(#{<<"method">> := ?MCP_NOTIFY_CANCELLED, <<"params">> := _Params}, State) ->
    %% TODO: 取消进行中的请求
    State;
do_handle_notification(_Notification, State) ->
    State.

%%====================================================================
%% 内部函数 - 辅助函数
%%====================================================================

%% @private 生成会话 ID
generate_session_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFF),
    iolist_to_binary(io_lib:format("mcp-~.16b-~.4b", [Timestamp, Random])).

%% @private 更新能力
update_capabilities(#state{tools = Tools, resources = Resources, prompts = Prompts,
                           server_capabilities = Caps} = State) ->
    NewCaps = Caps#mcp_server_capabilities{
        tools = map_size(Tools) > 0,
        resources = map_size(Resources) > 0,
        prompts = map_size(Prompts) > 0
    },
    State#state{server_capabilities = NewCaps}.

%% @private 格式化工具结果
format_tool_result(Result) when is_binary(Result) ->
    [beamai_mcp_types:text_content(Result)];
format_tool_result(Result) when is_list(Result) ->
    Result;
format_tool_result(Result) when is_map(Result) ->
    [Result];
format_tool_result(Result) ->
    [beamai_mcp_types:text_content(
        iolist_to_binary(io_lib:format("~p", [Result]))
    )].

%% @private 默认 MIME 类型
default_mime_type(undefined) -> <<"text/plain">>;
default_mime_type(MimeType) -> MimeType.
