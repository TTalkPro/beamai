%%%-------------------------------------------------------------------
%%% @doc input_required 多轮对话单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_input_required_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试设置
%%====================================================================

setup() ->
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
    Server.

cleanup(Server) ->
    beamai_a2a_server:stop(Server).

%%====================================================================
%% 测试套件
%%====================================================================

input_required_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        fun test_task_status_message/1,
        fun test_request_input/1,
        fun test_continue_input_required_task/1,
        fun test_context_based_continuation/1,
        fun test_cannot_continue_completed_task/1,
        fun test_complete_task_api/1,
        fun test_fail_task_api/1
     ]}.

%%====================================================================
%% 任务状态消息测试
%%====================================================================

test_task_status_message(_Server) ->
    fun() ->
        %% 创建任务
        {ok, TaskPid} = beamai_a2a_task:start_link(#{
            message => #{role => user, parts => [#{kind => text, text => <<"Hello">>}]},
            context_id => <<"ctx-test-1">>
        }),

        %% 先设置为 working 状态（因为 submitted -> input_required 不是有效转换）
        ok = beamai_a2a_task:update_status(TaskPid, working),

        %% 设置为 input_required 状态（带消息）
        QuestionMsg = #{
            role => agent,
            parts => [#{kind => text, text => <<"What is your name?">>}]
        },
        ok = beamai_a2a_task:update_status(TaskPid, {input_required, QuestionMsg}),

        %% 获取任务状态
        {ok, Task} = beamai_a2a_task:get(TaskPid),
        Status = maps:get(status, Task),

        %% 验证状态和消息
        ?assertEqual(input_required, maps:get(state, Status)),
        ?assertEqual(QuestionMsg, maps:get(message, Status)),

        beamai_a2a_task:stop(TaskPid),
        ok
    end.

%%====================================================================
%% request_input API 测试
%%====================================================================

test_request_input(_Server) ->
    fun() ->
        %% 创建任务
        {ok, TaskPid} = beamai_a2a_task:start_link(#{
            message => #{role => user, parts => [#{kind => text, text => <<"Hello">>}]}
        }),

        %% 更新为 working
        ok = beamai_a2a_task:update_status(TaskPid, working),

        %% 请求输入
        ok = beamai_a2a_server:request_input(TaskPid, <<"Please provide more details.">>, #{}),

        %% 验证状态
        {ok, Task} = beamai_a2a_task:get(TaskPid),
        Status = maps:get(status, Task),
        ?assertEqual(input_required, maps:get(state, Status)),

        %% 验证消息内容
        StatusMsg = maps:get(message, Status),
        ?assertEqual(agent, maps:get(role, StatusMsg)),
        Parts = maps:get(parts, StatusMsg),
        [Part] = Parts,
        ?assertEqual(text, maps:get(kind, Part)),
        ?assertEqual(<<"Please provide more details.">>, maps:get(text, Part)),

        beamai_a2a_task:stop(TaskPid),
        ok
    end.

%%====================================================================
%% 继续 input_required 任务测试
%%====================================================================

test_continue_input_required_task(_Server) ->
    fun() ->
        %% 创建任务
        {ok, TaskPid} = beamai_a2a_task:start_link(#{
            message => #{role => user, parts => [#{kind => text, text => <<"Hello">>}]},
            context_id => <<"ctx-continue-test">>
        }),
        {ok, _TaskId} = beamai_a2a_task:get_id(TaskPid),

        %% 设置为 input_required
        ok = beamai_a2a_task:update_status(TaskPid, working),
        ok = beamai_a2a_server:request_input(TaskPid, <<"What is your name?">>, #{}),

        %% 直接测试 Task 状态转换（模拟用户回复）
        ok = beamai_a2a_task:add_message(TaskPid, #{
            role => user,
            parts => [#{kind => text, text => <<"My name is Alice">>}]
        }),
        ok = beamai_a2a_task:update_status(TaskPid, working),

        %% 验证状态变为 working
        {ok, Task} = beamai_a2a_task:get(TaskPid),
        Status = maps:get(status, Task),
        ?assertEqual(working, maps:get(state, Status)),

        %% 验证消息历史
        Messages = maps:get(messages, Task),
        ?assertEqual(2, length(Messages)),

        beamai_a2a_task:stop(TaskPid),
        ok
    end.

%%====================================================================
%% 上下文续接测试
%%====================================================================

test_context_based_continuation(Server) ->
    fun() ->
        ContextId = <<"ctx-multi-turn">>,

        %% 发送第一条消息
        Request1 = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"method">> => <<"message/send">>,
            <<"params">> => #{
                <<"contextId">> => ContextId,
                <<"message">> => #{
                    <<"role">> => <<"user">>,
                    <<"parts">> => [#{<<"kind">> => <<"text">>, <<"text">> => <<"Start conversation">>}]
                }
            }
        },

        {ok, Response1} = beamai_a2a_server:handle_request(Server, Request1),

        %% 验证返回了任务
        ?assert(maps:is_key(<<"result">>, Response1)),
        Result1 = maps:get(<<"result">>, Response1),
        ?assertEqual(<<"task">>, maps:get(<<"kind">>, Result1)),
        ?assert(is_binary(maps:get(<<"id">>, Result1))),
        ?assertEqual(ContextId, maps:get(<<"contextId">>, Result1)),

        %% 等待任务完成（或超时）
        timer:sleep(100),

        ok
    end.

%%====================================================================
%% 终止状态任务测试
%%====================================================================

test_cannot_continue_completed_task(_Server) ->
    fun() ->
        %% 创建并完成任务
        {ok, TaskPid} = beamai_a2a_task:start_link(#{
            message => #{role => user, parts => [#{kind => text, text => <<"Test">>}]}
        }),

        ok = beamai_a2a_task:update_status(TaskPid, working),
        ok = beamai_a2a_task:update_status(TaskPid, completed),

        %% 验证无法再更新状态
        Result = beamai_a2a_task:update_status(TaskPid, working),
        ?assertEqual({error, {invalid_transition, completed, working}}, Result),

        beamai_a2a_task:stop(TaskPid),
        ok
    end.

%%====================================================================
%% complete_task API 测试
%%====================================================================

test_complete_task_api(_Server) ->
    fun() ->
        %% 创建任务
        {ok, TaskPid} = beamai_a2a_task:start_link(#{
            message => #{role => user, parts => [#{kind => text, text => <<"Test">>}]}
        }),

        ok = beamai_a2a_task:update_status(TaskPid, working),

        %% 使用 API 完成任务
        ok = beamai_a2a_server:complete_task(TaskPid, <<"Task completed successfully!">>, #{}),

        %% 验证状态
        {ok, Task} = beamai_a2a_task:get(TaskPid),
        Status = maps:get(status, Task),
        ?assertEqual(completed, maps:get(state, Status)),

        %% 验证 Artifact
        Artifacts = maps:get(artifacts, Task),
        ?assertEqual(1, length(Artifacts)),
        [Artifact] = Artifacts,
        ?assertEqual(<<"response">>, maps:get(name, Artifact)),

        beamai_a2a_task:stop(TaskPid),
        ok
    end.

%%====================================================================
%% fail_task API 测试
%%====================================================================

test_fail_task_api(_Server) ->
    fun() ->
        %% 创建任务
        {ok, TaskPid} = beamai_a2a_task:start_link(#{
            message => #{role => user, parts => [#{kind => text, text => <<"Test">>}]}
        }),

        ok = beamai_a2a_task:update_status(TaskPid, working),

        %% 使用 API 标记失败
        ok = beamai_a2a_server:fail_task(TaskPid, <<"Something went wrong">>, #{}),

        %% 验证状态
        {ok, Task} = beamai_a2a_task:get(TaskPid),
        Status = maps:get(status, Task),
        ?assertEqual(failed, maps:get(state, Status)),

        %% 验证错误消息
        StatusMsg = maps:get(message, Status),
        ?assertEqual(agent, maps:get(role, StatusMsg)),

        beamai_a2a_task:stop(TaskPid),
        ok
    end.

%%====================================================================
%% JSON 序列化测试
%%====================================================================

json_serialization_test_() ->
    [
        {"状态消息 JSON 序列化", fun test_status_message_json/0}
    ].

test_status_message_json() ->
    %% 创建带状态消息的任务
    {ok, TaskPid} = beamai_a2a_task:start_link(#{
        message => #{role => user, parts => [#{kind => text, text => <<"Hello">>}]},
        context_id => <<"ctx-json-test">>
    }),

    %% 设置 input_required 状态
    ok = beamai_a2a_task:update_status(TaskPid, working),
    QuestionMsg = #{
        role => agent,
        parts => [#{kind => text, text => <<"Please confirm">>}]
    },
    ok = beamai_a2a_task:update_status(TaskPid, {input_required, QuestionMsg}),

    %% 获取任务
    {ok, Task} = beamai_a2a_task:get(TaskPid),

    %% 状态应该包含消息
    Status = maps:get(status, Task),
    ?assertEqual(input_required, maps:get(state, Status)),
    ?assertEqual(QuestionMsg, maps:get(message, Status)),

    beamai_a2a_task:stop(TaskPid),
    ok.
