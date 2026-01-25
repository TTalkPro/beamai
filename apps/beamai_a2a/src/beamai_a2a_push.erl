%%%-------------------------------------------------------------------
%%% @doc A2A Push Notifications 模块
%%%
%%% 实现 A2A 协议的 Push Notifications (Webhook) 功能。
%%% 当任务状态改变时，通过 HTTP POST 发送通知到已注册的 webhook URL。
%%%
%%% == 功能 ==
%%%
%%% - Webhook URL 注册和管理
%%% - 任务状态变更通知
%%% - 认证支持（Bearer Token）
%%% - 失败重试机制（指数退避）
%%% - 事件过滤（仅通知特定状态变化）
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 注册 webhook
%%% Config = #{
%%%     url => <<"https://example.com/webhook">>,
%%%     token => <<"secret-token">>,  %% 可选，用于 Bearer 认证
%%%     events => [completed, failed]  %% 可选，默认 all
%%% },
%%% ok = beamai_a2a_push:register(TaskId, Config).
%%%
%%% %% 发送通知（由任务状态更新时自动调用）
%%% ok = beamai_a2a_push:notify(TaskId, Task).
%%%
%%% %% 获取 webhook 配置
%%% {ok, Config} = beamai_a2a_push:get_config(TaskId).
%%%
%%% %% 取消注册
%%% ok = beamai_a2a_push:unregister(TaskId).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_push).

-behaviour(gen_server).

%% 避免与 erlang:get/1 冲突
-compile({no_auto_import,[get/1]}).

%% API 导出
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% Webhook 管理
    register/2,
    register/3,
    unregister/1,
    get_config/1,
    list_webhooks/0,
    list_webhooks/1,

    %% 通知发送
    notify/2,
    notify_async/2,

    %% 统计信息
    stats/0
]).

%% gen_server 回调
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 常量定义
-define(SERVER, ?MODULE).
-define(TABLE, beamai_a2a_push_registry).       %% Webhook 注册表
-define(RETRY_TABLE, beamai_a2a_push_retries).  %% 重试追踪表
-define(PUSH_RETRY_COUNT, 3).                  %% 默认重试次数
-define(PUSH_RETRY_DELAY, 1000).               %% 初始重试延迟（毫秒）
-define(PUSH_MAX_RETRY_DELAY, 30000).          %% 最大重试延迟（毫秒）
-define(PUSH_HTTP_TIMEOUT, 10000).             %% HTTP 请求超时（毫秒）

%% 服务器状态
-record(state, {
    config :: map(),  %% 服务配置
    stats :: map()    %% 统计信息
}).

%% Webhook 配置记录
-record(webhook, {
    task_id :: binary(),              %% 任务 ID
    url :: binary(),                  %% Webhook URL
    token :: binary() | undefined,    %% Bearer Token（可选）
    events :: [atom()] | all,         %% 订阅的事件列表
    retry_count :: non_neg_integer(), %% 最大重试次数
    created_at :: non_neg_integer()   %% 创建时间戳
}).

%%====================================================================
%% API - 服务生命周期
%%====================================================================

%% @doc 启动 Push Notifications 服务
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc 启动 Push Notifications 服务（带配置）
%%
%% @param Config 服务配置（预留）
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc 停止服务
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%%====================================================================
%% API - Webhook 管理
%%====================================================================

%% @doc 为任务注册 webhook
%%
%% @param TaskId 任务 ID
%% @param Config webhook 配置
%%   - url: webhook URL（必需）
%%   - token: Bearer token（可选）
%%   - events: 要通知的事件列表（可选，默认 all）
%%   - retry_count: 重试次数（可选，默认 3）
%% @returns ok | {error, Reason}
-spec register(binary(), map()) -> ok | {error, term()}.
register(TaskId, Config) ->
    register(TaskId, Config, #{}).

%% @doc 为任务注册 webhook（带额外选项）
-spec register(binary(), map(), map()) -> ok | {error, term()}.
register(TaskId, Config, _Opts) ->
    gen_server:call(?SERVER, {register, TaskId, Config}, ?DEFAULT_TIMEOUT).

%% @doc 取消任务的 webhook 注册
-spec unregister(binary()) -> ok | {error, term()}.
unregister(TaskId) ->
    gen_server:call(?SERVER, {unregister, TaskId}, ?DEFAULT_TIMEOUT).

%% @doc 获取任务的 webhook 配置
-spec get_config(binary()) -> {ok, map()} | {error, not_found}.
get_config(TaskId) ->
    gen_server:call(?SERVER, {get_config, TaskId}, ?DEFAULT_TIMEOUT).

%% @doc 列出所有 webhook
-spec list_webhooks() -> [map()].
list_webhooks() ->
    gen_server:call(?SERVER, list_webhooks, ?DEFAULT_TIMEOUT).

%% @doc 列出指定任务的 webhook
-spec list_webhooks(binary()) -> [map()].
list_webhooks(TaskId) ->
    gen_server:call(?SERVER, {list_webhooks, TaskId}, ?DEFAULT_TIMEOUT).

%%====================================================================
%% API - 通知发送
%%====================================================================

%% @doc 发送任务状态通知（同步）
%%
%% @param TaskId 任务 ID
%% @param Task 任务数据
%% @returns ok | {error, Reason}
-spec notify(binary(), map()) -> ok | {error, term()}.
notify(TaskId, Task) ->
    gen_server:call(?SERVER, {notify, TaskId, Task}, ?DEFAULT_TIMEOUT * 2).

%% @doc 发送任务状态通知（异步）
%%
%% 异步发送通知，不等待结果。适用于不需要确认的场景。
%%
%% @param TaskId 任务 ID
%% @param Task 任务数据
%% @returns ok
-spec notify_async(binary(), map()) -> ok.
notify_async(TaskId, Task) ->
    gen_server:cast(?SERVER, {notify, TaskId, Task}).

%%====================================================================
%% API - 统计信息
%%====================================================================

%% @doc 获取统计信息
%%
%% 返回包含以下字段的 map：
%% - notifications_sent: 成功发送的通知数
%% - notifications_failed: 失败的通知数
%% - retries: 重试次数
%% - webhooks_registered: 当前注册的 webhook 数
%% - table_size: 注册表大小
%% - retry_table_size: 重试表大小
-spec stats() -> map().
stats() ->
    gen_server:call(?SERVER, stats, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @private 初始化服务
init(Config) ->
    %% 创建 Webhook 注册表
    ets:new(?TABLE, [
        named_table,
        public,
        set,
        {keypos, #webhook.task_id},
        {read_concurrency, true}
    ]),

    %% 创建重试追踪表
    ets:new(?RETRY_TABLE, [
        named_table,
        public,
        set,
        {read_concurrency, true}
    ]),

    State = #state{
        config = Config,
        stats = #{
            notifications_sent => 0,
            notifications_failed => 0,
            retries => 0,
            webhooks_registered => 0
        }
    },

    {ok, State}.

%% @private 处理同步调用
handle_call({register, TaskId, Config}, _From, State) ->
    case do_register(TaskId, Config) of
        ok ->
            NewStats = maps:update_with(
                webhooks_registered,
                fun(V) -> V + 1 end,
                State#state.stats
            ),
            {reply, ok, State#state{stats = NewStats}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unregister, TaskId}, _From, State) ->
    case do_unregister(TaskId) of
        ok ->
            NewStats = maps:update_with(
                webhooks_registered,
                fun(V) -> max(0, V - 1) end,
                State#state.stats
            ),
            {reply, ok, State#state{stats = NewStats}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_config, TaskId}, _From, State) ->
    Result = do_get_config(TaskId),
    {reply, Result, State};

handle_call(list_webhooks, _From, State) ->
    Webhooks = do_list_webhooks(),
    {reply, Webhooks, State};

handle_call({list_webhooks, TaskId}, _From, State) ->
    Webhooks = do_list_webhooks(TaskId),
    {reply, Webhooks, State};

handle_call({notify, TaskId, Task}, _From, State) ->
    {Result, NewStats} = do_notify(TaskId, Task, State#state.stats),
    {reply, Result, State#state{stats = NewStats}};

handle_call(stats, _From, State) ->
    BaseStats = State#state.stats,
    FullStats = BaseStats#{
        table_size => ets:info(?TABLE, size),
        retry_table_size => ets:info(?RETRY_TABLE, size)
    },
    {reply, FullStats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private 处理异步消息
handle_cast({notify, TaskId, Task}, State) ->
    {_Result, NewStats} = do_notify(TaskId, Task, State#state.stats),
    {noreply, State#state{stats = NewStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private 处理系统消息
handle_info({retry, TaskId, Task, Attempt}, State) ->
    %% 处理重试请求
    {_Result, NewStats} = do_retry_notify(TaskId, Task, Attempt, State#state.stats),
    {noreply, State#state{stats = NewStats}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private 终止回调
terminate(_Reason, _State) ->
    ets:delete(?TABLE),
    ets:delete(?RETRY_TABLE),
    ok.

%% @private 代码升级
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数 - Webhook 管理
%%====================================================================

%% @private 注册 webhook
do_register(TaskId, Config) ->
    case maps:get(url, Config, undefined) of
        undefined ->
            {error, missing_url};
        Url ->
            Webhook = #webhook{
                task_id = TaskId,
                url = normalize_url(Url),
                token = maps:get(token, Config, undefined),
                events = normalize_events(maps:get(events, Config, all)),
                retry_count = maps:get(retry_count, Config, ?PUSH_RETRY_COUNT),
                created_at = erlang:system_time(millisecond)
            },
            ets:insert(?TABLE, Webhook),
            ok
    end.

%% @private 取消注册
do_unregister(TaskId) ->
    case ets:lookup(?TABLE, TaskId) of
        [] ->
            {error, not_found};
        _ ->
            ets:delete(?TABLE, TaskId),
            ets:delete(?RETRY_TABLE, TaskId),
            ok
    end.

%% @private 获取配置
do_get_config(TaskId) ->
    case ets:lookup(?TABLE, TaskId) of
        [] ->
            {error, not_found};
        [Webhook] ->
            {ok, webhook_to_map(Webhook)}
    end.

%% @private 列出所有 webhook
do_list_webhooks() ->
    ets:foldl(
        fun(Webhook, Acc) ->
            [webhook_to_map(Webhook) | Acc]
        end,
        [],
        ?TABLE
    ).

%% @private 列出指定任务的 webhook
do_list_webhooks(TaskId) ->
    case ets:lookup(?TABLE, TaskId) of
        [] -> [];
        [Webhook] -> [webhook_to_map(Webhook)]
    end.

%%====================================================================
%% 内部函数 - 通知发送
%%====================================================================

%% @private 发送通知
do_notify(TaskId, Task, Stats) ->
    case ets:lookup(?TABLE, TaskId) of
        [] ->
            %% 没有注册的 webhook，直接返回
            {ok, Stats};
        [Webhook] ->
            %% 检查事件过滤
            TaskState = get_task_state(Task),
            case should_notify(TaskState, Webhook#webhook.events) of
                true ->
                    send_notification(TaskId, Task, Webhook, Stats);
                false ->
                    {ok, Stats}
            end
    end.

%% @private 重试发送通知
do_retry_notify(TaskId, Task, Attempt, Stats) ->
    case ets:lookup(?TABLE, TaskId) of
        [] ->
            {ok, Stats};
        [Webhook] ->
            case Attempt > Webhook#webhook.retry_count of
                true ->
                    %% 超过重试次数，标记失败
                    NewStats = maps:update_with(
                        notifications_failed,
                        fun(V) -> V + 1 end,
                        Stats
                    ),
                    ets:delete(?RETRY_TABLE, TaskId),
                    {{error, max_retries_exceeded}, NewStats};
                false ->
                    %% 继续重试
                    RetryStats = maps:update_with(retries, fun(V) -> V + 1 end, Stats),
                    send_notification(TaskId, Task, Webhook, RetryStats)
            end
    end.

%% @private 发送 HTTP 通知
send_notification(TaskId, Task, Webhook, Stats) ->
    Url = Webhook#webhook.url,
    Payload = build_payload(TaskId, Task),
    Headers = build_headers(Webhook#webhook.token),

    case do_http_post(Url, Headers, Payload) of
        {ok, _Response} ->
            %% 成功
            NewStats = maps:update_with(
                notifications_sent,
                fun(V) -> V + 1 end,
                Stats
            ),
            ets:delete(?RETRY_TABLE, TaskId),
            {ok, NewStats};
        {error, Reason} ->
            %% 失败，安排重试
            Attempt = get_retry_attempt(TaskId),
            case Attempt < Webhook#webhook.retry_count of
                true ->
                    schedule_retry(TaskId, Task, Attempt + 1),
                    {{error, {retry_scheduled, Reason}}, Stats};
                false ->
                    NewStats = maps:update_with(
                        notifications_failed,
                        fun(V) -> V + 1 end,
                        Stats
                    ),
                    ets:delete(?RETRY_TABLE, TaskId),
                    {{error, Reason}, NewStats}
            end
    end.

%% @private 构建请求 payload
%%
%% 使用 beamai_a2a_convert 模块将 Task 转换为 JSON 格式
build_payload(TaskId, Task) ->
    Event = #{
        <<"type">> => <<"TaskStatusUpdateEvent">>,
        <<"taskId">> => TaskId,
        <<"task">> => beamai_a2a_convert:task_to_json(Task),
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    jsx:encode(Event, []).

%% @private 构建请求头
build_headers(undefined) ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"User-Agent">>, <<"A2A-Agent/1.0">>}
    ];
build_headers(Token) when is_binary(Token) ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Authorization">>, <<"Bearer ", Token/binary>>},
        {<<"User-Agent">>, <<"A2A-Agent/1.0">>}
    ].

%% @private 发送 HTTP POST 请求
do_http_post(Url, Headers, Body) ->
    %% 使用 beamai_http 模块
    case beamai_http:post(Url, Headers, Body, #{timeout => ?PUSH_HTTP_TIMEOUT}) of
        {ok, {{_, StatusCode, _}, _, ResponseBody}} when StatusCode >= 200, StatusCode < 300 ->
            {ok, ResponseBody};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数 - 重试机制
%%====================================================================

%% @private 获取重试次数
get_retry_attempt(TaskId) ->
    case ets:lookup(?RETRY_TABLE, TaskId) of
        [] -> 0;
        [{TaskId, Attempt}] -> Attempt
    end.

%% @private 安排重试
%%
%% 使用指数退避算法计算重试延迟
schedule_retry(TaskId, Task, Attempt) ->
    ets:insert(?RETRY_TABLE, {TaskId, Attempt}),
    Delay = calculate_retry_delay(Attempt),
    erlang:send_after(Delay, self(), {retry, TaskId, Task, Attempt}).

%% @private 计算重试延迟（指数退避）
%%
%% 延迟 = 基础延迟 * 2^(尝试次数-1)
%% 最大不超过 PUSH_MAX_RETRY_DELAY
calculate_retry_delay(Attempt) ->
    Delay = ?PUSH_RETRY_DELAY * (1 bsl (Attempt - 1)),
    min(Delay, ?PUSH_MAX_RETRY_DELAY).

%%====================================================================
%% 内部函数 - 事件过滤
%%====================================================================

%% @private 检查是否应该通知
should_notify(_State, all) -> true;
should_notify(State, Events) when is_list(Events) ->
    lists:member(State, Events).

%% @private 从任务获取状态
get_task_state(Task) ->
    Status = maps:get(status, Task, #{}),
    maps:get(state, Status, unknown).

%%====================================================================
%% 内部函数 - 数据转换
%%====================================================================

%% @private 规范化 URL
normalize_url(Url) when is_binary(Url) -> Url;
normalize_url(Url) when is_list(Url) -> list_to_binary(Url).

%% @private 规范化事件列表
%%
%% 使用 beamai_a2a_types 的安全转换函数，防止 atom 表耗尽攻击。
%% 无效事件会被过滤掉。
normalize_events(all) -> all;
normalize_events(Events) when is_list(Events) ->
    ValidEvents = [normalize_event(E) || E <- Events],
    %% 过滤掉 undefined（无效事件）
    [E || E <- ValidEvents, E =/= undefined];
normalize_events(_) -> all.

%% @private 规范化单个事件（安全版本）
%%
%% 使用白名单验证，防止恶意输入创建任意 atom。
normalize_event(E) when is_atom(E) ->
    %% 验证 atom 是否在白名单中
    case beamai_a2a_types:is_valid_push_event(E) of
        true -> E;
        false -> undefined
    end;
normalize_event(E) when is_binary(E) ->
    beamai_a2a_types:binary_to_push_event(E);
normalize_event(E) when is_list(E) ->
    %% 先转为 binary 再使用安全转换
    beamai_a2a_types:binary_to_push_event(list_to_binary(E)).

%% @private 将 webhook 记录转换为 map
%%
%% 注意：token 字段被隐藏为 "***"
webhook_to_map(#webhook{} = W) ->
    #{
        task_id => W#webhook.task_id,
        url => W#webhook.url,
        token => case W#webhook.token of
            undefined -> undefined;
            _ -> <<"***">>  %% 安全：隐藏实际 token
        end,
        events => W#webhook.events,
        retry_count => W#webhook.retry_count,
        created_at => W#webhook.created_at
    }.
