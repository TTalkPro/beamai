%%%-------------------------------------------------------------------
%%% @doc A2A Cowboy HTTP Handler 示例
%%%
%%% 这是一个 Cowboy 2.x handler 示例，展示如何将 beamai_a2a_server
%%% 集成到 Cowboy HTTP Server 中。
%%%
%%% == 功能 ==
%%%
%%% - GET  /.well-known/agent.json  → 返回 Agent Card
%%% - POST /a2a                     → 处理 JSON-RPC 请求
%%% - POST /a2a/stream              → 流式响应（SSE）
%%%
%%% == 使用方式 ==
%%%
%%% 1. 将此文件复制到你的项目中
%%% 2. 根据需要修改配置
%%% 3. 在 Cowboy 路由中添加路由规则
%%%
%%% == 依赖 ==
%%%
%%% - cowboy 2.x
%%% - beamai_a2a 应用
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_a2a_handler).

-export([init/2]).

%% 用于 SSE 流式响应
-export([info/3]).

%%====================================================================
%% Cowboy Handler 回调
%%====================================================================

%% @doc 初始化 handler
%%
%% Req 中应包含 action 参数：
%% - agent_card: 返回 Agent Card
%% - handle_request: 处理 JSON-RPC 请求
%% - stream: 流式响应
init(Req0, State) ->
    Action = maps:get(action, State, handle_request),
    Method = cowboy_req:method(Req0),

    case {Action, Method} of
        {agent_card, <<"GET">>} ->
            handle_agent_card(Req0, State);
        {handle_request, <<"POST">>} ->
            handle_json_rpc(Req0, State);
        {stream, <<"POST">>} ->
            handle_stream(Req0, State);
        _ ->
            reply_error(Req0, 405, <<"Method Not Allowed">>, State)
    end.

%%====================================================================
%% Agent Card 处理
%%====================================================================

%% @private 返回 Agent Card
handle_agent_card(Req0, State) ->
    %% 从 State 获取 A2A Server 引用
    Server = maps:get(a2a_server, State),

    case beamai_a2a_server:get_agent_card(Server) of
        {ok, Card} ->
            JsonBin = beamai_a2a_card:to_json(Card),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>,
                <<"cache-control">> => <<"max-age=3600">>
            }, JsonBin, Req0),
            {ok, Req, State};
        {error, Reason} ->
            reply_error(Req0, 500, format_error(Reason), State)
    end.

%%====================================================================
%% JSON-RPC 请求处理
%%====================================================================

%% @private 处理 JSON-RPC 请求
handle_json_rpc(Req0, State) ->
    %% 读取请求体
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    %% 验证 Content-Type
    ContentType = cowboy_req:header(<<"content-type">>, Req1, <<>>),
    case is_json_content_type(ContentType) of
        false ->
            reply_error(Req1, 415, <<"Unsupported Media Type">>, State);
        true ->
            %% 处理请求
            Server = maps:get(a2a_server, State),
            case beamai_a2a_server:handle_json(Server, Body) of
                {ok, ResponseJson} ->
                    Req = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, ResponseJson, Req1),
                    {ok, Req, State};
                {error, Reason} ->
                    %% 返回 JSON-RPC 错误
                    ErrorJson = beamai_a2a_jsonrpc:internal_error(null),
                    logger:error("A2A request error: ~p", [Reason]),
                    Req = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, ErrorJson, Req1),
                    {ok, Req, State}
            end
    end.

%%====================================================================
%% SSE 流式响应处理
%%====================================================================

%% @private 初始化 SSE 流
handle_stream(Req0, State) ->
    %% 读取请求体
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    %% 解析请求
    case beamai_a2a_jsonrpc:decode_request(Body) of
        {ok, {Id, Method, Params}} ->
            %% 检查是否为 stream 方法
            case Method of
                <<"message/stream">> ->
                    start_sse_stream(Req1, Id, Params, State);
                _ ->
                    %% 非 stream 方法，按普通请求处理
                    handle_json_rpc(Req0, State)
            end;
        {error, _} ->
            ErrorJson = beamai_a2a_jsonrpc:parse_error(null),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, ErrorJson, Req1),
            {ok, Req, State}
    end.

%% @private 启动 SSE 流
start_sse_stream(Req0, Id, Params, State) ->
    %% 设置 SSE 响应头
    Req = cowboy_req:stream_reply(200, #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>
    }, Req0),

    %% 创建任务并异步执行
    Server = maps:get(a2a_server, State),
    Message = maps:get(<<"message">>, Params, #{}),

    %% 发送初始事件
    send_sse_event(Req, <<"connected">>, #{<<"id">> => Id}),

    %% 创建任务
    case beamai_a2a_server:handle_request(Server, #{
        <<"method">> => <<"message/send">>,
        <<"id">> => Id,
        <<"params">> => Params
    }) of
        {ok, #{<<"result">> := Task}} ->
            %% 发送任务创建事件
            send_sse_event(Req, <<"task">>, Task),

            %% 启动轮询进程监控任务状态
            TaskId = maps:get(<<"id">>, Task),
            NewState = State#{
                sse_req => Req,
                task_id => TaskId,
                request_id => Id
            },
            %% 设置定时器轮询任务状态
            erlang:send_after(500, self(), poll_task),
            {cowboy_loop, Req, NewState};
        {error, Reason} ->
            send_sse_event(Req, <<"error">>, #{<<"error">> => format_error(Reason)}),
            cowboy_req:stream_body(<<"data: [DONE]\n\n">>, fin, Req),
            {ok, Req, State}
    end.

%% @doc 处理异步消息（用于 SSE 轮询）
info(poll_task, Req, State) ->
    Server = maps:get(a2a_server, State),
    TaskId = maps:get(task_id, State),
    RequestId = maps:get(request_id, State),

    %% 查询任务状态
    case beamai_a2a_server:handle_request(Server, #{
        <<"method">> => <<"tasks/get">>,
        <<"id">> => RequestId,
        <<"params">> => #{<<"taskId">> => TaskId}
    }) of
        {ok, #{<<"result">> := Task}} ->
            Status = maps:get(<<"status">>, Task, <<"working">>),

            %% 发送状态更新
            send_sse_event(Req, <<"taskStatusUpdate">>, Task),

            %% 检查是否完成
            case is_terminal_status(Status) of
                true ->
                    %% 发送完成事件
                    send_sse_event(Req, <<"done">>, Task),
                    cowboy_req:stream_body(<<"data: [DONE]\n\n">>, fin, Req),
                    {stop, Req, State};
                false ->
                    %% 继续轮询
                    erlang:send_after(500, self(), poll_task),
                    {ok, Req, State}
            end;
        {error, _} ->
            send_sse_event(Req, <<"error">>, #{<<"error">> => <<"Task query failed">>}),
            cowboy_req:stream_body(<<"data: [DONE]\n\n">>, fin, Req),
            {stop, Req, State}
    end;

info(_Msg, Req, State) ->
    {ok, Req, State}.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 检查 Content-Type 是否为 JSON
is_json_content_type(ContentType) ->
    case binary:match(ContentType, <<"application/json">>) of
        nomatch -> false;
        _ -> true
    end.

%% @private 发送 SSE 事件
send_sse_event(Req, Event, Data) ->
    JsonData = jsx:encode(Data, []),
    EventBin = iolist_to_binary([
        <<"event: ">>, Event, <<"\n">>,
        <<"data: ">>, JsonData, <<"\n\n">>
    ]),
    cowboy_req:stream_body(EventBin, nofin, Req).

%% @private 检查是否为终态
is_terminal_status(<<"completed">>) -> true;
is_terminal_status(<<"failed">>) -> true;
is_terminal_status(<<"canceled">>) -> true;
is_terminal_status(<<"rejected">>) -> true;
is_terminal_status(_) -> false.

%% @private 返回错误响应
reply_error(Req0, StatusCode, Message, State) ->
    Req = cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"text/plain">>
    }, Message, Req0),
    {ok, Req, State}.

%% @private 格式化错误
format_error(Reason) when is_binary(Reason) -> Reason;
format_error(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
format_error(Reason) -> iolist_to_binary(io_lib:format("~p", [Reason])).
