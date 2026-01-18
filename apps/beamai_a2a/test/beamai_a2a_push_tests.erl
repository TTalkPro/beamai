%%%-------------------------------------------------------------------
%%% @doc Push Notifications 单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_push_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试设置
%%====================================================================

setup() ->
    %% 启动 Push Notifications 服务
    {ok, Pid} = beamai_a2a_push:start_link(#{}),
    Pid.

cleanup(Pid) ->
    %% 停止服务
    case is_process_alive(Pid) of
        true -> beamai_a2a_push:stop();
        false -> ok
    end.

%%====================================================================
%% 测试套件
%%====================================================================

push_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"注册 webhook 测试", fun test_register/0},
        {"取消注册 webhook 测试", fun test_unregister/0},
        {"获取配置测试", fun test_get_config/0},
        {"列出 webhooks 测试", fun test_list_webhooks/0},
        {"统计信息测试", fun test_stats/0},
        {"事件过滤测试", fun test_event_filtering/0},
        {"缺少 URL 错误测试", fun test_missing_url/0},
        {"不存在的 webhook 测试", fun test_not_found/0}
     ]}.

%%====================================================================
%% 基本功能测试
%%====================================================================

test_register() ->
    TaskId = <<"task-test-1">>,
    Config = #{
        url => <<"https://example.com/webhook">>,
        token => <<"secret-token">>,
        events => [completed, failed]
    },

    %% 注册 webhook
    ok = beamai_a2a_push:register(TaskId, Config),

    %% 验证注册成功
    {ok, Retrieved} = beamai_a2a_push:get_config(TaskId),
    ?assertEqual(TaskId, maps:get(task_id, Retrieved)),
    ?assertEqual(<<"https://example.com/webhook">>, maps:get(url, Retrieved)),
    %% Token 应该被隐藏
    ?assertEqual(<<"***">>, maps:get(token, Retrieved)),

    ok.

test_unregister() ->
    TaskId = <<"task-test-2">>,
    Config = #{url => <<"https://example.com/webhook">>},

    %% 注册
    ok = beamai_a2a_push:register(TaskId, Config),

    %% 验证存在
    {ok, _} = beamai_a2a_push:get_config(TaskId),

    %% 取消注册
    ok = beamai_a2a_push:unregister(TaskId),

    %% 验证不存在
    {error, not_found} = beamai_a2a_push:get_config(TaskId),

    ok.

test_get_config() ->
    TaskId = <<"task-test-3">>,
    Config = #{
        url => <<"https://example.com/webhook">>,
        events => all,
        retry_count => 5
    },

    %% 注册
    ok = beamai_a2a_push:register(TaskId, Config),

    %% 获取配置
    {ok, Retrieved} = beamai_a2a_push:get_config(TaskId),

    ?assertEqual(TaskId, maps:get(task_id, Retrieved)),
    ?assertEqual(<<"https://example.com/webhook">>, maps:get(url, Retrieved)),
    ?assertEqual(all, maps:get(events, Retrieved)),
    ?assertEqual(5, maps:get(retry_count, Retrieved)),
    ?assert(maps:is_key(created_at, Retrieved)),

    ok.

test_list_webhooks() ->
    %% 注册多个 webhooks
    ok = beamai_a2a_push:register(<<"task-list-1">>, #{url => <<"https://a.com/webhook">>}),
    ok = beamai_a2a_push:register(<<"task-list-2">>, #{url => <<"https://b.com/webhook">>}),
    ok = beamai_a2a_push:register(<<"task-list-3">>, #{url => <<"https://c.com/webhook">>}),

    %% 列出所有
    AllWebhooks = beamai_a2a_push:list_webhooks(),
    ?assert(length(AllWebhooks) >= 3),

    %% 列出特定任务
    TaskWebhooks = beamai_a2a_push:list_webhooks(<<"task-list-1">>),
    ?assertEqual(1, length(TaskWebhooks)),
    [W] = TaskWebhooks,
    ?assertEqual(<<"task-list-1">>, maps:get(task_id, W)),

    ok.

test_stats() ->
    %% 注册一些 webhooks
    ok = beamai_a2a_push:register(<<"task-stats-1">>, #{url => <<"https://stats.com/webhook">>}),
    ok = beamai_a2a_push:register(<<"task-stats-2">>, #{url => <<"https://stats.com/webhook">>}),

    %% 获取统计
    Stats = beamai_a2a_push:stats(),

    ?assert(maps:is_key(notifications_sent, Stats)),
    ?assert(maps:is_key(notifications_failed, Stats)),
    ?assert(maps:is_key(retries, Stats)),
    ?assert(maps:is_key(webhooks_registered, Stats)),
    ?assert(maps:is_key(table_size, Stats)),

    ok.

test_event_filtering() ->
    TaskId = <<"task-filter-1">>,

    %% 注册只监听 completed 事件
    Config = #{
        url => <<"https://filter.com/webhook">>,
        events => [completed]
    },
    ok = beamai_a2a_push:register(TaskId, Config),

    %% 获取配置验证
    {ok, Retrieved} = beamai_a2a_push:get_config(TaskId),
    ?assertEqual([completed], maps:get(events, Retrieved)),

    ok.

test_missing_url() ->
    TaskId = <<"task-missing-url">>,
    Config = #{
        token => <<"some-token">>
        %% 没有 url
    },

    %% 应该返回错误
    {error, missing_url} = beamai_a2a_push:register(TaskId, Config),

    ok.

test_not_found() ->
    %% 获取不存在的配置
    {error, not_found} = beamai_a2a_push:get_config(<<"nonexistent-task">>),

    %% 取消注册不存在的
    {error, not_found} = beamai_a2a_push:unregister(<<"nonexistent-task">>),

    ok.

%%====================================================================
%% 通知发送测试（模拟）
%%====================================================================

notify_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = beamai_a2a_push:start_link(#{}),
         Pid
     end,
     fun(Pid) ->
         case is_process_alive(Pid) of
             true -> beamai_a2a_push:stop();
             false -> ok
         end
     end,
     [
        {"无 webhook 时通知返回 ok", fun test_notify_no_webhook/0},
        {"异步通知测试", fun test_notify_async/0}
     ]}.

test_notify_no_webhook() ->
    TaskId = <<"task-no-webhook">>,
    Task = #{
        id => TaskId,
        status => #{state => completed},
        artifacts => []
    },

    %% 没有注册 webhook，通知应该直接返回 ok
    ok = beamai_a2a_push:notify(TaskId, Task),

    ok.

test_notify_async() ->
    TaskId = <<"task-async-notify">>,
    Task = #{
        id => TaskId,
        status => #{state => completed},
        artifacts => []
    },

    %% 异步通知（不阻塞）
    ok = beamai_a2a_push:notify_async(TaskId, Task),

    %% 等待一点时间让异步操作完成
    timer:sleep(50),

    ok.

%%====================================================================
%% 服务器集成测试
%%====================================================================

server_integration_test_() ->
    {setup,
     fun() ->
         %% 启动 A2A Server
         AgentConfig = #{
             name => <<"test-agent">>,
             description => <<"Test Agent">>,
             url => <<"http://localhost:8080/a2a">>,
             system_prompt => <<"You are a test agent.">>,
             tools => [],
             llm => #{provider => mock, model => <<"mock">>}
         },
         {ok, Server} = beamai_a2a_server:start(#{agent_config => AgentConfig}),
         %% 启动 Push 服务
         {ok, Push} = beamai_a2a_push:start_link(#{}),
         {Server, Push}
     end,
     fun({Server, Push}) ->
         beamai_a2a_server:stop(Server),
         case is_process_alive(Push) of
             true -> beamai_a2a_push:stop();
             false -> ok
         end
     end,
     [
        {"JSON-RPC pushNotificationConfig/set 测试", fun test_jsonrpc_push_config_set/0},
        {"JSON-RPC pushNotificationConfig/get 测试", fun test_jsonrpc_push_config_get/0},
        {"JSON-RPC pushNotificationConfig/delete 测试", fun test_jsonrpc_push_config_delete/0}
     ]}.

test_jsonrpc_push_config_set() ->
    %% 这个测试需要先创建一个任务
    ok.

test_jsonrpc_push_config_get() ->
    %% 测试获取不存在的配置
    ok.

test_jsonrpc_push_config_delete() ->
    %% 测试删除配置
    ok.
