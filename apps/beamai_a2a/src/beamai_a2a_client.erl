%%%-------------------------------------------------------------------
%%% @doc A2A Client 模块
%%%
%%% 用于调用远程 A2A Agent 的客户端实现。
%%%
%%% == 功能 ==
%%%
%%% - Agent 发现：获取远程 Agent Card
%%% - 消息发送：通过 JSON-RPC 发送消息
%%% - 任务管理：查询和取消远程任务
%%% - 流式响应：支持 SSE 流式通信
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 发现远程 Agent
%%% {ok, Card} = beamai_a2a_client:discover("https://agent.example.com").
%%%
%%% %% 发送消息
%%% {ok, Task} = beamai_a2a_client:send_message(
%%%     "https://agent.example.com/a2a",
%%%     #{role => user, parts => [#{kind => text, text => <<"Hello">>}]}
%%% ).
%%%
%%% %% 查询任务状态
%%% {ok, Task} = beamai_a2a_client:get_task(
%%%     "https://agent.example.com/a2a",
%%%     <<"task-123">>
%%% ).
%%%
%%% %% 流式消息
%%% {ok, Result} = beamai_a2a_client:send_message_stream(
%%%     "https://agent.example.com/a2a/stream",
%%%     Message,
%%%     fun(Event) -> io:format("Event: ~p~n", [Event]) end
%%% ).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_client).

%% API 导出
-export([
    %% Agent 发现
    discover/1,
    discover/2,
    get_agent_card/1,
    get_agent_card/2,

    %% 消息发送
    send_message/2,
    send_message/3,
    send_message/4,

    %% 任务管理
    get_task/2,
    get_task/3,
    cancel_task/2,
    cancel_task/3,

    %% 流式请求
    send_message_stream/3,
    send_message_stream/4,

    %% 便捷函数
    create_text_message/1,
    create_text_message/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type base_url() :: binary() | string().
-type endpoint() :: binary() | string().
-type message() :: map().
-type task_id() :: binary().
-type context_id() :: binary() | undefined.
-type options() :: #{
    timeout => pos_integer(),
    headers => [{binary(), binary()}],
    api_key => binary() | string()
}.
-type stream_callback() :: fun((map()) -> ok | {error, term()}).

-export_type([base_url/0, endpoint/0, message/0, task_id/0, context_id/0, options/0]).

%%====================================================================
%% 默认配置
%%====================================================================

-define(CLIENT_DEFAULT_TIMEOUT, 30000).
-define(AGENT_CARD_PATH, "/.well-known/agent.json").

%%====================================================================
%% Agent 发现 API
%%====================================================================

%% @doc 发现远程 Agent（获取 Agent Card）
%%
%% 从远程 Agent 的 well-known URL 获取 Agent Card。
%%
%% @param BaseUrl Agent 基础 URL（例如 "https://agent.example.com"）
%% @returns {ok, AgentCard} | {error, Reason}
-spec discover(base_url()) -> {ok, map()} | {error, term()}.
discover(BaseUrl) ->
    discover(BaseUrl, #{}).

%% @doc 发现远程 Agent（带选项）
-spec discover(base_url(), options()) -> {ok, map()} | {error, term()}.
discover(BaseUrl, Opts) ->
    Url = build_agent_card_url(BaseUrl),
    get_agent_card(Url, Opts).

%% @doc 获取 Agent Card
%%
%% 从指定 URL 获取 Agent Card。
%%
%% @param Url Agent Card 完整 URL
%% @returns {ok, AgentCard} | {error, Reason}
-spec get_agent_card(endpoint()) -> {ok, map()} | {error, term()}.
get_agent_card(Url) ->
    get_agent_card(Url, #{}).

%% @doc 获取 Agent Card（带选项）
-spec get_agent_card(endpoint(), options()) -> {ok, map()} | {error, term()}.
get_agent_card(Url, Opts) ->
    HttpOpts = build_http_opts(Opts),
    case beamai_http:get(Url, #{}, HttpOpts) of
        {ok, Body} when is_map(Body) ->
            beamai_a2a_card:from_map(Body);
        {ok, Body} when is_binary(Body) ->
            beamai_a2a_card:from_json(Body);
        {error, Reason} ->
            {error, {discovery_failed, Reason}}
    end.

%%====================================================================
%% 消息发送 API
%%====================================================================

%% @doc 发送消息到远程 Agent
%%
%% 通过 JSON-RPC message/send 方法发送消息。
%%
%% @param Endpoint A2A 端点 URL
%% @param Message 消息内容
%% @returns {ok, Task} | {error, Reason}
-spec send_message(endpoint(), message()) -> {ok, map()} | {error, term()}.
send_message(Endpoint, Message) ->
    send_message(Endpoint, Message, undefined, #{}).

%% @doc 发送消息（带上下文 ID）
-spec send_message(endpoint(), message(), context_id()) -> {ok, map()} | {error, term()}.
send_message(Endpoint, Message, ContextId) ->
    send_message(Endpoint, Message, ContextId, #{}).

%% @doc 发送消息（带选项）
-spec send_message(endpoint(), message(), context_id(), options()) -> {ok, map()} | {error, term()}.
send_message(Endpoint, Message, ContextId, Opts) ->
    %% 构建 JSON-RPC 请求
    Params = build_message_params(Message, ContextId),
    Request = beamai_a2a_jsonrpc:encode_request(generate_request_id(), <<"message/send">>, Params),

    %% 发送请求
    case do_rpc_request(Endpoint, Request, Opts) of
        {ok, #{<<"result">> := Result}} ->
            {ok, Result};
        {ok, #{<<"error">> := Error}} ->
            {error, {rpc_error, Error}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 任务管理 API
%%====================================================================

%% @doc 获取远程任务状态
%%
%% 通过 JSON-RPC tasks/get 方法查询任务状态。
%%
%% @param Endpoint A2A 端点 URL
%% @param TaskId 任务 ID
%% @returns {ok, Task} | {error, Reason}
-spec get_task(endpoint(), task_id()) -> {ok, map()} | {error, term()}.
get_task(Endpoint, TaskId) ->
    get_task(Endpoint, TaskId, #{}).

%% @doc 获取任务状态（带选项）
-spec get_task(endpoint(), task_id(), options()) -> {ok, map()} | {error, term()}.
get_task(Endpoint, TaskId, Opts) ->
    Params = #{<<"taskId">> => TaskId},
    Request = beamai_a2a_jsonrpc:encode_request(generate_request_id(), <<"tasks/get">>, Params),

    case do_rpc_request(Endpoint, Request, Opts) of
        {ok, #{<<"result">> := Result}} ->
            {ok, Result};
        {ok, #{<<"error">> := Error}} ->
            {error, {rpc_error, Error}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 取消远程任务
%%
%% 通过 JSON-RPC tasks/cancel 方法取消任务。
%%
%% @param Endpoint A2A 端点 URL
%% @param TaskId 任务 ID
%% @returns {ok, Task} | {error, Reason}
-spec cancel_task(endpoint(), task_id()) -> {ok, map()} | {error, term()}.
cancel_task(Endpoint, TaskId) ->
    cancel_task(Endpoint, TaskId, #{}).

%% @doc 取消任务（带选项）
-spec cancel_task(endpoint(), task_id(), options()) -> {ok, map()} | {error, term()}.
cancel_task(Endpoint, TaskId, Opts) ->
    Params = #{<<"taskId">> => TaskId},
    Request = beamai_a2a_jsonrpc:encode_request(generate_request_id(), <<"tasks/cancel">>, Params),

    case do_rpc_request(Endpoint, Request, Opts) of
        {ok, #{<<"result">> := Result}} ->
            {ok, Result};
        {ok, #{<<"error">> := Error}} ->
            {error, {rpc_error, Error}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 流式请求 API
%%====================================================================

%% @doc 发送消息并流式接收响应
%%
%% 使用 SSE (Server-Sent Events) 流式接收响应。
%% 每收到一个事件，会调用回调函数。
%%
%% @param Endpoint 流式端点 URL（通常是 /a2a/stream）
%% @param Message 消息内容
%% @param Callback 事件回调函数
%% @returns {ok, FinalResult} | {error, Reason}
-spec send_message_stream(endpoint(), message(), stream_callback()) ->
    {ok, map()} | {error, term()}.
send_message_stream(Endpoint, Message, Callback) ->
    send_message_stream(Endpoint, Message, Callback, #{}).

%% @doc 流式发送消息（带选项）
-spec send_message_stream(endpoint(), message(), stream_callback(), options()) ->
    {ok, map()} | {error, term()}.
send_message_stream(Endpoint, Message, Callback, Opts) ->
    %% 构建请求
    Params = build_message_params(Message, undefined),
    Request = beamai_a2a_jsonrpc:encode_request(generate_request_id(), <<"message/stream">>, Params),

    %% 使用流式 HTTP 请求
    Headers = build_request_headers(Opts),
    HttpOpts = #{
        timeout => maps:get(timeout, Opts, ?CLIENT_DEFAULT_TIMEOUT),
        headers => Headers,
        init_acc => #{events => [], last_task => undefined}
    },

    %% SSE 事件处理器
    Handler = fun(Chunk, Acc) ->
        handle_sse_chunk(Chunk, Acc, Callback)
    end,

    case beamai_http:stream_request(post, Endpoint, [{<<"Content-Type">>, <<"application/json">>}],
                                   Request, HttpOpts, Handler) of
        {ok, #{last_task := Task}} when Task =/= undefined ->
            {ok, Task};
        {ok, _} ->
            {error, no_task_received};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 便捷函数
%%====================================================================

%% @doc 创建文本消息
%%
%% 便捷函数，用于创建简单的文本消息。
%%
%% @param Text 文本内容
%% @returns 消息 map
-spec create_text_message(binary() | string()) -> message().
create_text_message(Text) ->
    create_text_message(Text, user).

%% @doc 创建文本消息（指定角色）
-spec create_text_message(binary() | string(), user | agent) -> message().
create_text_message(Text, Role) ->
    TextBin = beamai_utils:to_binary(Text),
    #{
        <<"role">> => atom_to_binary(Role, utf8),
        <<"parts">> => [
            #{<<"kind">> => <<"text">>, <<"text">> => TextBin}
        ]
    }.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 构建 Agent Card URL
-spec build_agent_card_url(base_url()) -> binary().
build_agent_card_url(BaseUrl) ->
    Base = beamai_utils:to_binary(BaseUrl),
    %% 移除尾部斜杠
    CleanBase = case binary:last(Base) of
        $/ -> binary:part(Base, 0, byte_size(Base) - 1);
        _ -> Base
    end,
    <<CleanBase/binary, ?AGENT_CARD_PATH>>.

%% @private 构建消息参数
-spec build_message_params(message(), context_id()) -> map().
build_message_params(Message, undefined) ->
    #{<<"message">> => Message};
build_message_params(Message, ContextId) ->
    #{<<"message">> => Message, <<"contextId">> => ContextId}.

%% @private 构建 HTTP 选项
-spec build_http_opts(options()) -> map().
build_http_opts(Opts) ->
    HttpOpts = #{
        timeout => maps:get(timeout, Opts, ?CLIENT_DEFAULT_TIMEOUT)
    },
    case maps:get(headers, Opts, undefined) of
        undefined -> HttpOpts;
        Headers -> HttpOpts#{headers => Headers}
    end.

%% @private 构建请求头
-spec build_request_headers(options()) -> [{binary(), binary()}].
build_request_headers(Opts) ->
    BaseHeaders = [{<<"Accept">>, <<"text/event-stream">>}],
    case maps:get(api_key, Opts, undefined) of
        undefined ->
            BaseHeaders;
        ApiKey ->
            ApiKeyBin = beamai_utils:to_binary(ApiKey),
            [{<<"Authorization">>, <<"Bearer ", ApiKeyBin/binary>>} | BaseHeaders]
    end.

%% @private 执行 JSON-RPC 请求
-spec do_rpc_request(endpoint(), binary(), options()) -> {ok, map()} | {error, term()}.
do_rpc_request(Endpoint, RequestJson, Opts) ->
    Headers = build_request_headers(Opts),
    HttpOpts = #{
        timeout => maps:get(timeout, Opts, ?CLIENT_DEFAULT_TIMEOUT),
        headers => Headers
    },

    case beamai_http:post_json(Endpoint, jsx:decode(RequestJson, [return_maps]), HttpOpts) of
        {ok, Response} when is_map(Response) ->
            {ok, Response};
        {ok, Response} when is_binary(Response) ->
            case jsx:is_json(Response) of
                true -> {ok, jsx:decode(Response, [return_maps])};
                false -> {error, {invalid_response, Response}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% @private 生成请求 ID
-spec generate_request_id() -> binary().
generate_request_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFF),
    iolist_to_binary(io_lib:format("req-~.16b-~.4b", [Timestamp, Random])).

%% @private 处理 SSE 数据块
-spec handle_sse_chunk(binary(), map(), stream_callback()) ->
    {continue, map()} | {done, map()}.
handle_sse_chunk(Chunk, Acc, Callback) ->
    %% 解析 SSE 事件
    #{events := Events, last_task := LastTask} = Acc,
    case parse_sse_events(Chunk) of
        {ok, ParsedEvents} ->
            %% 处理每个事件
            {NewLastTask, Done} = process_sse_events(ParsedEvents, LastTask, Callback),
            NewAcc = Acc#{
                events => Events ++ ParsedEvents,
                last_task => NewLastTask
            },
            case Done of
                true -> {done, NewAcc};
                false -> {continue, NewAcc}
            end;
        {error, _} ->
            %% 忽略解析错误，继续接收
            {continue, Acc}
    end.

%% @private 解析 SSE 事件
-spec parse_sse_events(binary()) -> {ok, [map()]} | {error, term()}.
parse_sse_events(Chunk) ->
    %% SSE 格式: event: <type>\ndata: <json>\n\n
    Lines = binary:split(Chunk, <<"\n">>, [global]),
    parse_sse_lines(Lines, undefined, []).

%% @private 解析 SSE 行
parse_sse_lines([], _CurrentEvent, Acc) ->
    {ok, lists:reverse(Acc)};
parse_sse_lines([<<>> | Rest], CurrentEvent, Acc) ->
    %% 空行表示事件结束
    parse_sse_lines(Rest, CurrentEvent, Acc);
parse_sse_lines([<<"event: ", EventType/binary>> | Rest], _CurrentEvent, Acc) ->
    parse_sse_lines(Rest, #{type => EventType}, Acc);
parse_sse_lines([<<"data: ", Data/binary>> | Rest], CurrentEvent, Acc) ->
    case CurrentEvent of
        undefined ->
            %% 没有事件类型，创建默认事件
            Event = #{type => <<"message">>, data => parse_json_data(Data)},
            parse_sse_lines(Rest, undefined, [Event | Acc]);
        #{type := Type} ->
            Event = #{type => Type, data => parse_json_data(Data)},
            parse_sse_lines(Rest, undefined, [Event | Acc])
    end;
parse_sse_lines([_ | Rest], CurrentEvent, Acc) ->
    %% 忽略其他行
    parse_sse_lines(Rest, CurrentEvent, Acc).

%% @private 解析 JSON 数据
-spec parse_json_data(binary()) -> term().
parse_json_data(<<"[DONE]">>) ->
    done;
parse_json_data(Data) ->
    case jsx:is_json(Data) of
        true ->
            try jsx:decode(Data, [return_maps])
            catch _:_ -> Data
            end;
        false ->
            Data
    end.

%% @private 处理 SSE 事件
-spec process_sse_events([map()], map() | undefined, stream_callback()) ->
    {map() | undefined, boolean()}.
process_sse_events([], LastTask, _Callback) ->
    {LastTask, false};
process_sse_events([Event | Rest], LastTask, Callback) ->
    #{type := Type, data := Data} = Event,

    %% 调用回调
    _ = Callback(Event),

    %% 检查是否完成
    case {Type, Data} of
        {<<"done">>, _} ->
            NewTask = case Data of
                done -> LastTask;
                TaskData when is_map(TaskData) -> TaskData;
                _ -> LastTask
            end,
            {NewTask, true};
        {_, done} ->
            {LastTask, true};
        {<<"task">>, TaskData} when is_map(TaskData) ->
            process_sse_events(Rest, TaskData, Callback);
        {<<"taskStatusUpdate">>, TaskData} when is_map(TaskData) ->
            process_sse_events(Rest, TaskData, Callback);
        _ ->
            process_sse_events(Rest, LastTask, Callback)
    end.
