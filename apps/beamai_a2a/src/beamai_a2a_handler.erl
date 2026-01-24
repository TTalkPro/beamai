%%%-------------------------------------------------------------------
%%% @doc A2A JSON-RPC 方法处理器
%%%
%%% 处理 A2A 协议定义的所有 JSON-RPC 方法。
%%% 此模块从 beamai_a2a_server 中分离出来，专注于方法级别的业务逻辑。
%%%
%%% == 支持的方法 ==
%%%
%%% 1. 消息发送
%%%    - message/send: 发送消息，创建或继续任务
%%%
%%% 2. 任务管理
%%%    - tasks/get: 获取任务状态
%%%    - tasks/cancel: 取消任务
%%%
%%% 3. Push 通知配置
%%%    - tasks/pushNotificationConfig/set: 设置 webhook 配置
%%%    - tasks/pushNotificationConfig/get: 获取 webhook 配置
%%%    - tasks/pushNotificationConfig/delete: 删除 webhook 配置
%%%
%%% == 设计原则 ==
%%%
%%% 1. 方法处理器不持有状态，通过参数接收和返回状态
%%% 2. 返回统一的 {ok, Response, State} 或 {error, Reason, State} 格式
%%% 3. 所有错误都以 JSON-RPC 格式返回
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_handler).

%% API 导出
-export([
    %% 请求分发
    dispatch/3,

    %% 消息发送
    handle_message_send/3,

    %% 任务管理
    handle_tasks_get/3,
    handle_tasks_cancel/3,

    %% Push 通知配置
    handle_push_config_set/3,
    handle_push_config_get/3,
    handle_push_config_delete/3
]).

-include_lib("beamai_core/include/beamai_common.hrl").

%%====================================================================
%% 请求分发
%%====================================================================

%% @doc 分发 JSON-RPC 请求到对应的处理器
%%
%% @param Method 方法名（binary）
%% @param Params 参数 map
%% @param Context 请求上下文 #{id, tasks, contexts, agent_config}
%% @returns {ok, Response, NewContext} | {error, Reason, NewContext}
-spec dispatch(binary(), map(), map()) -> {ok, map(), map()} | {error, term(), map()}.
dispatch(Method, Params, Context) ->
    Id = maps:get(id, Context, null),
    case Method of
        <<"message/send">> ->
            handle_message_send(Id, Params, Context);
        <<"tasks/get">> ->
            handle_tasks_get(Id, Params, Context);
        <<"tasks/cancel">> ->
            handle_tasks_cancel(Id, Params, Context);
        <<"tasks/pushNotificationConfig/set">> ->
            handle_push_config_set(Id, Params, Context);
        <<"tasks/pushNotificationConfig/get">> ->
            handle_push_config_get(Id, Params, Context);
        <<"tasks/pushNotificationConfig/delete">> ->
            handle_push_config_delete(Id, Params, Context);
        _ ->
            %% 方法不存在
            Response = make_error_response(Id, -32601, <<"Method not found">>,
                                          #{<<"method">> => Method}),
            {ok, Response, Context}
    end.

%%====================================================================
%% message/send 处理
%%====================================================================

%% @doc 处理 message/send 方法
%%
%% 根据参数决定：
%% - 创建新任务（无 taskId 和 contextId）
%% - 在上下文中继续任务（有 contextId，无 taskId）
%% - 继续指定任务（有 taskId）
%%
%% @param Id JSON-RPC 请求 ID
%% @param Params 请求参数
%% @param Context 请求上下文
%% @returns {ok, Response, NewContext}
-spec handle_message_send(term(), map(), map()) -> {ok, map(), map()}.
handle_message_send(Id, Params, Context) ->
    Message = maps:get(<<"message">>, Params, #{}),
    TaskId = maps:get(<<"taskId">>, Params, undefined),
    ContextId = maps:get(<<"contextId">>, Params, undefined),

    case {TaskId, ContextId} of
        {undefined, undefined} ->
            %% 创建新任务（无上下文）
            create_and_execute_task(Id, Message, undefined, Context);
        {undefined, _} ->
            %% 有 contextId，检查是否存在待继续的任务
            case find_input_required_task(ContextId, Context) of
                {ok, ExistingTaskId, TaskPid} ->
                    %% 继续 input_required 状态的任务
                    continue_input_required_task(Id, ExistingTaskId, TaskPid, Message, Context);
                not_found ->
                    %% 在现有上下文中创建新任务
                    create_and_execute_task(Id, Message, ContextId, Context)
            end;
        {_, _} ->
            %% 有 taskId，继续现有任务
            continue_task(Id, TaskId, Message, Context)
    end.

%% @private 创建并执行新任务
create_and_execute_task(Id, Message, ContextId, Context) ->
    Tasks = maps:get(tasks, Context, #{}),
    Contexts = maps:get(contexts, Context, #{}),
    AgentConfig = maps:get(agent_config, Context, #{}),

    %% 创建 Task 进程
    TaskOpts = #{
        message => beamai_a2a_convert:normalize_message(Message),
        context_id => ContextId
    },
    {ok, TaskPid} = beamai_a2a_task:start_link(TaskOpts),

    %% 监控 Task 进程（由调用者负责）
    erlang:monitor(process, TaskPid),

    %% 获取 Task ID
    {ok, TaskId} = beamai_a2a_task:get_id(TaskPid),

    %% 更新状态映射
    NewTasks = maps:put(TaskId, TaskPid, Tasks),
    NewContexts = case ContextId of
        undefined -> Contexts;
        _ ->
            ExistingTasks = maps:get(ContextId, Contexts, []),
            maps:put(ContextId, [TaskId | ExistingTasks], Contexts)
    end,

    %% 异步执行任务
    spawn_link(fun() -> execute_task(TaskPid, AgentConfig) end),

    %% 立即返回 Task
    {ok, Task} = beamai_a2a_task:get(TaskPid),

    Response = make_success_response(Id, beamai_a2a_convert:task_to_json(Task)),
    NewContext = Context#{tasks => NewTasks, contexts => NewContexts},

    {ok, Response, NewContext}.

%% @private 继续现有任务
continue_task(Id, TaskId, Message, Context) ->
    Tasks = maps:get(tasks, Context, #{}),

    case maps:get(TaskId, Tasks, undefined) of
        undefined ->
            %% 任务不存在
            Response = make_error_response(Id, -32001, <<"Task not found">>,
                                          #{<<"taskId">> => TaskId}),
            {ok, Response, Context};
        TaskPid ->
            %% 检查任务状态是否允许继续
            {ok, CurrentTask} = beamai_a2a_task:get(TaskPid),
            CurrentStatus = maps:get(state, maps:get(status, CurrentTask, #{}), undefined),

            case CurrentStatus of
                input_required ->
                    continue_input_required_task(Id, TaskId, TaskPid, Message, Context);
                working ->
                    %% 任务正在执行，不能继续
                    Response = make_error_response(Id, -32004, <<"Task is still running">>,
                                                  #{<<"taskId">> => TaskId, <<"status">> => <<"working">>}),
                    {ok, Response, Context};
                Terminal when Terminal =:= completed; Terminal =:= failed; Terminal =:= canceled ->
                    %% 任务已终止，不能继续
                    Response = make_error_response(Id, -32003, <<"Task already terminated">>,
                                                  #{<<"taskId">> => TaskId, <<"status">> => atom_to_binary(Terminal, utf8)}),
                    {ok, Response, Context};
                _ ->
                    %% 其他状态（submitted），可以继续
                    do_continue_task(Id, TaskPid, Message, Context)
            end
    end.

%% @private 查找上下文中处于 input_required 状态的任务
find_input_required_task(ContextId, Context) ->
    Contexts = maps:get(contexts, Context, #{}),
    Tasks = maps:get(tasks, Context, #{}),

    case maps:get(ContextId, Contexts, []) of
        [] ->
            not_found;
        TaskIds ->
            %% 遍历上下文中的任务，找到 input_required 状态的
            find_input_required_in_list(TaskIds, Tasks)
    end.

%% @private 在任务列表中查找 input_required 状态的任务
find_input_required_in_list([], _Tasks) ->
    not_found;
find_input_required_in_list([TaskId | Rest], Tasks) ->
    case maps:get(TaskId, Tasks, undefined) of
        undefined ->
            find_input_required_in_list(Rest, Tasks);
        TaskPid ->
            {ok, Task} = beamai_a2a_task:get(TaskPid),
            Status = maps:get(state, maps:get(status, Task, #{}), undefined),
            case Status of
                input_required ->
                    {ok, TaskId, TaskPid};
                _ ->
                    find_input_required_in_list(Rest, Tasks)
            end
    end.

%% @private 继续 input_required 状态的任务
continue_input_required_task(Id, _TaskId, TaskPid, Message, Context) ->
    AgentConfig = maps:get(agent_config, Context, #{}),

    %% 添加用户消息
    ok = beamai_a2a_task:add_message(TaskPid, beamai_a2a_convert:normalize_message(Message)),

    %% 更新状态为 working
    ok = beamai_a2a_task:update_status(TaskPid, working),

    %% 异步继续执行
    spawn_link(fun() -> execute_task(TaskPid, AgentConfig) end),

    %% 返回更新后的 Task
    {ok, Task} = beamai_a2a_task:get(TaskPid),

    Response = make_success_response(Id, beamai_a2a_convert:task_to_json(Task)),
    {ok, Response, Context}.

%% @private 执行任务继续
do_continue_task(Id, TaskPid, Message, Context) ->
    AgentConfig = maps:get(agent_config, Context, #{}),

    %% 添加消息并继续执行
    ok = beamai_a2a_task:add_message(TaskPid, beamai_a2a_convert:normalize_message(Message)),

    %% 更新状态为 working
    ok = beamai_a2a_task:update_status(TaskPid, working),

    %% 异步继续执行
    spawn_link(fun() -> execute_task(TaskPid, AgentConfig) end),

    %% 返回更新后的 Task
    {ok, Task} = beamai_a2a_task:get(TaskPid),

    Response = make_success_response(Id, beamai_a2a_convert:task_to_json(Task)),
    {ok, Response, Context}.

%%====================================================================
%% tasks/get 处理
%%====================================================================

%% @doc 处理 tasks/get 方法
%%
%% @param Id JSON-RPC 请求 ID
%% @param Params 请求参数（需要 taskId）
%% @param Context 请求上下文
%% @returns {ok, Response, Context}
-spec handle_tasks_get(term(), map(), map()) -> {ok, map(), map()}.
handle_tasks_get(Id, Params, Context) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),
    Tasks = maps:get(tasks, Context, #{}),

    case TaskId of
        undefined ->
            Response = make_error_response(Id, -32602, <<"Missing required parameter: taskId">>, #{}),
            {ok, Response, Context};
        _ ->
            case maps:get(TaskId, Tasks, undefined) of
                undefined ->
                    Response = make_error_response(Id, -32001, <<"Task not found">>,
                                                  #{<<"taskId">> => TaskId}),
                    {ok, Response, Context};
                TaskPid ->
                    {ok, Task} = beamai_a2a_task:get(TaskPid),
                    Response = make_success_response(Id, beamai_a2a_convert:task_to_json(Task)),
                    {ok, Response, Context}
            end
    end.

%%====================================================================
%% tasks/cancel 处理
%%====================================================================

%% @doc 处理 tasks/cancel 方法
%%
%% @param Id JSON-RPC 请求 ID
%% @param Params 请求参数（需要 taskId）
%% @param Context 请求上下文
%% @returns {ok, Response, Context}
-spec handle_tasks_cancel(term(), map(), map()) -> {ok, map(), map()}.
handle_tasks_cancel(Id, Params, Context) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),
    Tasks = maps:get(tasks, Context, #{}),

    case TaskId of
        undefined ->
            Response = make_error_response(Id, -32602, <<"Missing required parameter: taskId">>, #{}),
            {ok, Response, Context};
        _ ->
            case maps:get(TaskId, Tasks, undefined) of
                undefined ->
                    Response = make_error_response(Id, -32001, <<"Task not found">>,
                                                  #{<<"taskId">> => TaskId}),
                    {ok, Response, Context};
                TaskPid ->
                    case beamai_a2a_task:cancel(TaskPid) of
                        ok ->
                            {ok, Task} = beamai_a2a_task:get(TaskPid),
                            Response = make_success_response(Id, beamai_a2a_convert:task_to_json(Task)),
                            {ok, Response, Context};
                        {error, Reason} ->
                            Response = make_error_response(Id, -32003, <<"Cannot cancel task">>,
                                                          #{<<"reason">> => format_error(Reason)}),
                            {ok, Response, Context}
                    end
            end
    end.

%%====================================================================
%% Push 通知配置处理
%%====================================================================

%% @doc 处理 tasks/pushNotificationConfig/set 方法
%%
%% @param Id JSON-RPC 请求 ID
%% @param Params 请求参数（需要 taskId 和 pushNotificationConfig）
%% @param Context 请求上下文
%% @returns {ok, Response, Context}
-spec handle_push_config_set(term(), map(), map()) -> {ok, map(), map()}.
handle_push_config_set(Id, Params, Context) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),
    WebhookConfig = maps:get(<<"pushNotificationConfig">>, Params, #{}),
    Tasks = maps:get(tasks, Context, #{}),

    case TaskId of
        undefined ->
            Response = make_error_response(Id, -32602, <<"Missing required parameter: taskId">>, #{}),
            {ok, Response, Context};
        _ ->
            %% 验证任务存在
            case maps:get(TaskId, Tasks, undefined) of
                undefined ->
                    Response = make_error_response(Id, -32001, <<"Task not found">>,
                                                  #{<<"taskId">> => TaskId}),
                    {ok, Response, Context};
                _ ->
                    %% 注册 webhook
                    Config = normalize_webhook_config(WebhookConfig),
                    case beamai_a2a_push:register(TaskId, Config) of
                        ok ->
                            Response = make_success_response(Id, #{
                                <<"taskId">> => TaskId,
                                <<"pushNotificationConfig">> => webhook_config_to_json(Config)
                            }),
                            {ok, Response, Context};
                        {error, Reason} ->
                            Response = make_error_response(Id, -32005, <<"Failed to set push notification config">>,
                                                          #{<<"reason">> => format_error(Reason)}),
                            {ok, Response, Context}
                    end
            end
    end.

%% @doc 处理 tasks/pushNotificationConfig/get 方法
%%
%% @param Id JSON-RPC 请求 ID
%% @param Params 请求参数（需要 taskId）
%% @param Context 请求上下文
%% @returns {ok, Response, Context}
-spec handle_push_config_get(term(), map(), map()) -> {ok, map(), map()}.
handle_push_config_get(Id, Params, Context) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),

    case TaskId of
        undefined ->
            Response = make_error_response(Id, -32602, <<"Missing required parameter: taskId">>, #{}),
            {ok, Response, Context};
        _ ->
            case beamai_a2a_push:get_config(TaskId) of
                {ok, Config} ->
                    Response = make_success_response(Id, #{
                        <<"taskId">> => TaskId,
                        <<"pushNotificationConfig">> => Config
                    }),
                    {ok, Response, Context};
                {error, not_found} ->
                    Response = make_error_response(Id, -32006, <<"Push notification config not found">>,
                                                  #{<<"taskId">> => TaskId}),
                    {ok, Response, Context}
            end
    end.

%% @doc 处理 tasks/pushNotificationConfig/delete 方法
%%
%% @param Id JSON-RPC 请求 ID
%% @param Params 请求参数（需要 taskId）
%% @param Context 请求上下文
%% @returns {ok, Response, Context}
-spec handle_push_config_delete(term(), map(), map()) -> {ok, map(), map()}.
handle_push_config_delete(Id, Params, Context) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),

    case TaskId of
        undefined ->
            Response = make_error_response(Id, -32602, <<"Missing required parameter: taskId">>, #{}),
            {ok, Response, Context};
        _ ->
            case beamai_a2a_push:unregister(TaskId) of
                ok ->
                    Response = make_success_response(Id, #{
                        <<"taskId">> => TaskId,
                        <<"deleted">> => true
                    }),
                    {ok, Response, Context};
                {error, not_found} ->
                    Response = make_error_response(Id, -32006, <<"Push notification config not found">>,
                                                  #{<<"taskId">> => TaskId}),
                    {ok, Response, Context}
            end
    end.

%%====================================================================
%% 任务执行
%%====================================================================

%% @private 执行任务
execute_task(TaskPid, AgentConfig) ->
    %% 更新状态为 working
    ok = beamai_a2a_task:update_status(TaskPid, working),

    %% 获取任务 ID 和发送 working 通知
    {ok, TaskId} = beamai_a2a_task:get_id(TaskPid),
    {ok, WorkingTask} = beamai_a2a_task:get(TaskPid),
    notify_task_update(TaskId, WorkingTask),

    %% 获取任务消息
    Messages = maps:get(messages, WorkingTask, []),

    %% 提取用户输入
    UserInput = extract_user_input(Messages),

    %% 使用 beamai_agent 执行
    case execute_with_agent(UserInput, AgentConfig) of
        {ok, Result} ->
            %% 创建 Artifact
            Artifact = #{
                name => <<"response">>,
                parts => [#{kind => text, text => Result}]
            },
            ok = beamai_a2a_task:add_artifact(TaskPid, Artifact),
            ok = beamai_a2a_task:update_status(TaskPid, completed),
            %% 发送 completed 通知
            {ok, CompletedTask} = beamai_a2a_task:get(TaskPid),
            notify_task_update(TaskId, CompletedTask);
        {error, Reason} ->
            ErrorMsg = #{
                role => agent,
                parts => [#{kind => text, text => format_error(Reason)}]
            },
            ok = beamai_a2a_task:update_status(TaskPid, {failed, ErrorMsg}),
            %% 发送 failed 通知
            {ok, FailedTask} = beamai_a2a_task:get(TaskPid),
            notify_task_update(TaskId, FailedTask)
    end.

%% @private 从消息列表提取用户输入
extract_user_input([]) -> <<>>;
extract_user_input(Messages) ->
    %% 获取最后一条用户消息
    UserMessages = [M || M <- Messages, maps:get(role, M, undefined) =:= user],
    case UserMessages of
        [] -> <<>>;
        _ ->
            LastMsg = lists:last(UserMessages),
            Parts = maps:get(parts, LastMsg, []),
            extract_text_from_parts(Parts)
    end.

%% @private 从 Parts 提取文本
extract_text_from_parts([]) -> <<>>;
extract_text_from_parts([#{kind := text, text := Text} | _]) -> Text;
extract_text_from_parts([_ | Rest]) -> extract_text_from_parts(Rest).

%% @private 使用 Agent 执行任务
execute_with_agent(Input, AgentConfig) ->
    case beamai_agent:new(AgentConfig) of
        {ok, Agent} ->
            case beamai_agent:run(Agent, Input) of
                {ok, #{content := Content}, _NewAgent} ->
                    {ok, Content};
                {ok, Result, _NewAgent} when is_map(Result) ->
                    case maps:get(content, Result, undefined) of
                        undefined -> {ok, jsx:encode(Result, [])};
                        Resp -> {ok, Resp}
                    end;
                {interrupt, _InterruptInfo, _NewAgent} ->
                    {ok, <<"Agent execution interrupted, awaiting input.">>};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {agent_create_failed, Reason}}
    end.

%% @private 发送任务状态通知
notify_task_update(TaskId, Task) ->
    %% 异步发送通知，不阻塞主流程
    spawn(fun() ->
        beamai_a2a_push:notify_async(TaskId, Task)
    end),
    ok.

%%====================================================================
%% 响应构建
%%====================================================================

%% @private 构建成功响应
make_success_response(Id, Result) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    }.

%% @private 构建错误响应
make_error_response(Id, Code, Message, Data) ->
    Error = case maps:size(Data) of
        0 -> #{<<"code">> => Code, <<"message">> => Message};
        _ -> #{<<"code">> => Code, <<"message">> => Message, <<"data">> => Data}
    end,
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => Error
    }.

%%====================================================================
%% 工具函数
%%====================================================================

%% @private 格式化错误
format_error(Reason) when is_binary(Reason) -> Reason;
format_error(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
format_error(Reason) -> iolist_to_binary(io_lib:format("~p", [Reason])).

%% @private 规范化 webhook 配置
normalize_webhook_config(Config) ->
    #{
        url => maps:get(<<"url">>, Config, undefined),
        token => maps:get(<<"token">>, Config, undefined),
        events => normalize_webhook_events(maps:get(<<"events">>, Config, all)),
        retry_count => maps:get(<<"retryCount">>, Config, 3)
    }.

%% @private 规范化 webhook 事件列表
normalize_webhook_events(all) -> all;
normalize_webhook_events(<<"all">>) -> all;
normalize_webhook_events(Events) when is_list(Events) ->
    [binary_to_atom(E, utf8) || E <- Events, is_binary(E)];
normalize_webhook_events(_) -> all.

%% @private 将 webhook 配置转换为 JSON
webhook_config_to_json(Config) ->
    Base = #{
        <<"url">> => maps:get(url, Config),
        <<"retryCount">> => maps:get(retry_count, Config, 3)
    },
    %% 隐藏 token
    WithToken = case maps:get(token, Config, undefined) of
        undefined -> Base;
        _ -> Base#{<<"token">> => <<"***">>}
    end,
    %% 添加事件
    Events = maps:get(events, Config, all),
    case Events of
        all -> WithToken#{<<"events">> => <<"all">>};
        _ -> WithToken#{<<"events">> => [atom_to_binary(E, utf8) || E <- Events]}
    end.
