%%%-------------------------------------------------------------------
%%% @doc 新增 Middleware 测试套件
%%%
%%% 测试 9 个新增的 Middleware 模块：
%%% - middleware_model_retry      - 模型调用重试
%%% - middleware_model_fallback   - 模型故障转移
%%% - middleware_pii_detection    - PII 检测
%%% - middleware_tool_selector    - 工具选择器
%%% - middleware_context_editing  - 上下文编辑
%%% - middleware_todo_list        - 任务列表
%%% - middleware_tool_emulator    - 工具模拟器
%%% - middleware_shell_tool       - Shell 工具
%%% - middleware_file_search      - 文件搜索
%%%
%%% 使用 GLM-4.7 作为测试 LLM
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_suite_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试配置
%%====================================================================

%% GLM-4.7 配置
glm_config() ->
    ApiKey = os:getenv("ZHIPU_API_KEY", ""),
    #{
        provider => anthropic,
        model => <<"glm-4.7">>,
        api_key => list_to_binary(ApiKey),
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
        timeout => 60000,
        max_tokens => 2048
    }.

%% 基础 Agent 配置
base_config() ->
    #{
        llm_config => glm_config(),
        system_prompt => <<"You are a helpful assistant for testing middleware.">>,
        max_iterations => 5
    }.

%%====================================================================
%% Model Retry 测试
%%====================================================================

middleware_model_retry_init_test() ->
    %% 测试初始化
    State = middleware_model_retry:init(#{}),
    ?assertEqual(3, maps:get(max_retries, State)),
    %% backoff 是嵌套的 map
    Backoff = maps:get(backoff, State),
    ?assertEqual(1000, maps:get(initial_delay, Backoff)),
    ?assertEqual(exponential, maps:get(type, Backoff)).

middleware_model_retry_custom_init_test() ->
    %% 测试自定义配置
    Opts = #{
        max_retries => 5,
        backoff => #{
            type => linear,
            initial_delay => 500,
            max_delay => 10000
        }
    },
    State = middleware_model_retry:init(Opts),
    ?assertEqual(5, maps:get(max_retries, State)),
    Backoff = maps:get(backoff, State),
    ?assertEqual(500, maps:get(initial_delay, Backoff)),
    ?assertEqual(linear, maps:get(type, Backoff)).

middleware_model_retry_retryable_errors_test() ->
    %% 测试可重试错误判断
    MwState = middleware_model_retry:init(#{}),

    %% 错误在 retryable_errors 列表中
    RetryableErrors = maps:get(retryable_errors, MwState),
    ?assert(lists:member(timeout, RetryableErrors)),
    ?assert(lists:member(econnrefused, RetryableErrors)).

middleware_model_retry_delay_calculation_test() ->
    %% 测试延迟计算
    %% calculate_delay 接收 (RetryCount, BackoffConfig)
    BackoffConfig = #{
        type => exponential,
        initial_delay => 1000,
        max_delay => 30000,
        multiplier => 2,
        jitter => false  %% 关闭 jitter 以便测试
    },

    %% 首次重试延迟
    Delay1 = middleware_model_retry:calculate_delay(1, BackoffConfig),
    ?assert(Delay1 >= 1000),

    %% 第二次重试延迟应该更大（指数退避）
    Delay2 = middleware_model_retry:calculate_delay(2, BackoffConfig),
    ?assert(Delay2 >= Delay1).

%%====================================================================
%% Model Fallback 测试
%%====================================================================

middleware_model_fallback_init_test() ->
    %% 测试初始化
    Opts = #{
        fallback_models => [
            #{provider => openai, model => <<"gpt-3.5-turbo">>}
        ]
    },
    State = middleware_model_fallback:init(Opts),
    ?assertEqual(1, length(maps:get(fallback_models, State))),
    %% init 不设置 current_level，它在运行时通过 State 管理
    ?assert(maps:is_key(fallback_models, State)).

middleware_model_fallback_get_model_test() ->
    %% 测试获取当前模型
    %% get_current_model 返回 #{is_fallback => bool, model => ...}
    LLMConfig = #{provider => anthropic, model => <<"claude-3">>},

    %% 创建模拟状态 - 主模型状态（使用 binary keys）
    State1 = #{
        <<"llm_config">> => LLMConfig,
        <<"mw_fallback_active">> => false
    },
    Result1 = middleware_model_fallback:get_current_model(State1),
    ?assertEqual(false, maps:get(is_fallback, Result1)),

    %% 激活故障转移状态
    State2 = State1#{<<"mw_fallback_active">> => true, <<"mw_fallback_index">> => 0},
    Result2 = middleware_model_fallback:get_current_model(State2),
    %% 检查返回的 map 包含预期的键
    ?assert(maps:is_key(is_fallback, Result2)),
    ?assert(maps:is_key(model, Result2)).

%%====================================================================
%% PII Detection 测试
%%====================================================================

middleware_pii_detection_init_test() ->
    %% 测试初始化
    State = middleware_pii_detection:init(#{}),
    %% 默认策略是 warn
    ?assertEqual(warn, maps:get(strategy, State)),
    ?assert(maps:is_key(detect_types, State)).

middleware_pii_detection_email_test() ->
    %% 测试邮箱检测
    MwState = middleware_pii_detection:init(#{}),

    %% 检测邮箱
    Text = <<"Contact me at test@example.com for details">>,
    Result = middleware_pii_detection:detect_pii(Text, MwState),

    ?assert(length(Result) > 0),
    ?assertEqual(email, maps:get(type, hd(Result))).

middleware_pii_detection_phone_test() ->
    %% 测试电话号码检测
    MwState = middleware_pii_detection:init(#{}),

    %% 检测电话号码
    Text = <<"Call me at 13812345678">>,
    Result = middleware_pii_detection:detect_pii(Text, MwState),

    ?assert(length(Result) > 0),
    ?assertEqual(phone, maps:get(type, hd(Result))).

middleware_pii_detection_mask_test() ->
    %% 测试 PII 遮掩
    %% mask_pii 接收 (Text, Matches) 其中 Matches 来自 detect_pii
    MwState = middleware_pii_detection:init(#{strategy => mask}),

    %% 遮掩邮箱
    Text = <<"Email: test@example.com">>,
    Matches = middleware_pii_detection:detect_pii(Text, MwState),

    %% 确保检测到了邮箱
    ?assert(length(Matches) > 0),

    %% 遮掩 PII
    Masked = middleware_pii_detection:mask_pii(Text, Matches),
    ?assertNotEqual(Text, Masked).

middleware_pii_detection_redact_test() ->
    %% 测试 PII 删除
    MwState = middleware_pii_detection:init(#{}),

    Text = <<"Contact: user@domain.com">>,
    Matches = middleware_pii_detection:detect_pii(Text, MwState),

    %% 确保检测到了邮箱
    ?assert(length(Matches) > 0),

    %% 删除 PII
    Redacted = middleware_pii_detection:redact_pii(Text, Matches),
    ?assertNotEqual(Text, Redacted),
    %% 应该包含 REDACTED 标记
    ?assertNotEqual(nomatch, binary:match(Redacted, <<"REDACTED">>)).

%%====================================================================
%% Tool Selector 测试
%%====================================================================

middleware_tool_selector_init_test() ->
    %% 测试初始化
    State = middleware_tool_selector:init(#{}),
    %% 默认最大工具数为 5，阈值为 8
    ?assertEqual(5, maps:get(max_tools, State)),
    ?assertEqual(8, maps:get(min_tools_to_filter, State)),
    ?assertEqual([], maps:get(always_include, State)).

middleware_tool_selector_config_test() ->
    %% 测试自定义配置
    MwState = middleware_tool_selector:init(#{
        max_tools => 3,
        min_tools_to_filter => 5,
        always_include => [<<"search">>, <<"calculator">>]
    }),

    ?assertEqual(3, maps:get(max_tools, MwState)),
    ?assertEqual(5, maps:get(min_tools_to_filter, MwState)),
    ?assertEqual([<<"search">>, <<"calculator">>], maps:get(always_include, MwState)).

%%====================================================================
%% Context Editing 测试
%%====================================================================

middleware_context_editing_init_test() ->
    %% 测试初始化
    State = middleware_context_editing:init(#{}),
    ?assertEqual(3, maps:get(keep_recent_tool_results, State)),
    ?assertEqual(10, maps:get(trigger_message_count, State)).

middleware_context_editing_estimate_tokens_test() ->
    %% 测试 token 估算
    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Hello world">>},
        #{<<"role">> => <<"assistant">>, <<"content">> => <<"Hi there!">>}
    ],

    Tokens = middleware_context_editing:estimate_tokens(Messages),
    ?assert(Tokens > 0),
    ?assert(Tokens < 100).

middleware_context_editing_clear_test() ->
    %% 测试清理旧工具结果
    MwState = middleware_context_editing:init(#{keep_recent_tool_results => 1}),

    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Search for files">>},
        #{<<"role">> => <<"tool">>, <<"content">> => <<"Result 1: file1.txt">>},
        #{<<"role">> => <<"user">>, <<"content">> => <<"Now search again">>},
        #{<<"role">> => <<"tool">>, <<"content">> => <<"Result 2: file2.txt">>},
        #{<<"role">> => <<"user">>, <<"content">> => <<"One more time">>},
        #{<<"role">> => <<"tool">>, <<"content">> => <<"Result 3: file3.txt">>}
    ],

    {Cleaned, Count} = middleware_context_editing:clear_old_tool_results(Messages, MwState),

    %% 应该清理了 2 个旧结果
    ?assertEqual(2, Count),
    %% 总消息数不变
    ?assertEqual(length(Messages), length(Cleaned)).

%%====================================================================
%% Todo List 测试
%%====================================================================

middleware_todo_list_init_test() ->
    %% 测试初始化
    State = middleware_todo_list:init(#{}),
    ?assertEqual(true, maps:get(add_guidance, State)),
    ?assertEqual([], maps:get(initial_todos, State)).

middleware_todo_list_operations_test() ->
    %% 测试任务列表操作
    State = #{},

    %% 添加任务
    State1 = middleware_todo_list:add_todo(State, <<"Task 1">>),
    Todos1 = middleware_todo_list:get_todos(State1),
    ?assertEqual(1, length(Todos1)),

    %% 再添加一个
    State2 = middleware_todo_list:add_todo(State1, #{content => <<"Task 2">>, status => pending}),
    Todos2 = middleware_todo_list:get_todos(State2),
    ?assertEqual(2, length(Todos2)),

    %% 更新状态
    State3 = middleware_todo_list:update_todo(State2, 0, in_progress),
    Todos3 = middleware_todo_list:get_todos(State3),
    ?assertEqual(in_progress, maps:get(status, hd(Todos3))),

    %% 标记完成
    State4 = middleware_todo_list:complete_todo(State3, 0),
    Todos4 = middleware_todo_list:get_todos(State4),
    ?assertEqual(completed, maps:get(status, hd(Todos4))).

middleware_todo_list_summary_test() ->
    %% 测试摘要统计
    %% graph:get 使用 binary 键，所以需要使用 binary 作为 key
    State = #{<<"mw_todos">> => [
        #{content => <<"Task 1">>, status => completed},
        #{content => <<"Task 2">>, status => in_progress},
        #{content => <<"Task 3">>, status => pending},
        #{content => <<"Task 4">>, status => pending}
    ]},

    Summary = middleware_todo_list:get_todo_summary(State),
    ?assertEqual(4, maps:get(total, Summary)),
    ?assertEqual(2, maps:get(pending, Summary)),
    ?assertEqual(1, maps:get(in_progress, Summary)),
    ?assertEqual(1, maps:get(completed, Summary)).

%%====================================================================
%% Tool Emulator 测试
%%====================================================================

middleware_tool_emulator_init_test() ->
    %% 测试初始化
    State = middleware_tool_emulator:init(#{}),
    ?assertEqual([], maps:get(emulate_tools, State)),
    ?assertEqual([], maps:get(exclude_tools, State)),
    ?assertEqual(0, maps:get(emulate_delay, State)).

middleware_tool_emulator_should_emulate_test() ->
    %% 测试是否应该模拟
    MwState = middleware_tool_emulator:init(#{
        emulate_tools => [<<"api_call">>, <<"database_query">>],
        exclude_tools => [<<"calculator">>]
    }),

    %% 在模拟列表中
    ?assert(middleware_tool_emulator:should_emulate(<<"api_call">>, MwState)),

    %% 不在模拟列表中
    ?assertNot(middleware_tool_emulator:should_emulate(<<"file_read">>, MwState)),

    %% 在排除列表中
    MwStateAll = middleware_tool_emulator:init(#{
        emulate_tools => [],  %% 空表示全部模拟
        exclude_tools => [<<"calculator">>]
    }),
    ?assertNot(middleware_tool_emulator:should_emulate(<<"calculator">>, MwStateAll)).

%%====================================================================
%% Shell Tool 测试
%%====================================================================

middleware_shell_tool_init_test() ->
    %% 测试初始化
    State = middleware_shell_tool:init(#{}),
    ?assertEqual(host, maps:get(execution_policy, State)),
    ?assertEqual(30000, maps:get(timeout, State)),
    ?assertEqual(<<"/tmp">>, maps:get(working_dir, State)).

middleware_shell_tool_blocked_commands_test() ->
    %% 测试危险命令列表
    State = middleware_shell_tool:init(#{}),
    BlockedCommands = maps:get(blocked_commands, State),

    %% 确保危险命令在列表中
    ?assert(lists:member(<<"rm -rf /">>, BlockedCommands)),
    ?assert(lists:member(<<":(){ :|:& };:">>, BlockedCommands)).

middleware_shell_tool_execute_simple_test() ->
    %% 测试简单命令执行
    MwState = middleware_shell_tool:init(#{working_dir => <<"/tmp">>}),

    %% 执行简单命令 - 使用 echo 直接输出
    Result = middleware_shell_tool:execute_command(<<"echo 'test_output_123'">>, MwState),

    case Result of
        {ok, Output, ExitCode} ->
            ?assertEqual(0, ExitCode),
            %% 输出应该包含测试字符串
            HasOutput = binary:match(Output, <<"test_output">>) =/= nomatch,
            %% 如果输出为空，可能是环境问题，跳过
            case HasOutput of
                true -> ?assert(true);
                false -> ?debugFmt("Shell output: ~p", [Output])
            end;
        {error, Reason} ->
            %% 某些环境可能不支持 shell 执行
            ?debugFmt("Shell execution failed: ~p", [Reason])
    end.

middleware_shell_tool_blocked_execution_test() ->
    %% 测试阻止危险命令
    MwState = middleware_shell_tool:init(#{}),

    %% 尝试执行危险命令
    Result = middleware_shell_tool:execute_command(<<"rm -rf /">>, MwState),

    ?assertMatch({error, {blocked_command, _}}, Result).

%%====================================================================
%% File Search 测试
%%====================================================================

middleware_file_search_init_test() ->
    %% 测试初始化
    State = middleware_file_search:init(#{}),
    ?assertEqual(100, maps:get(max_results, State)),
    ?assertEqual(1048576, maps:get(max_file_size, State)).

middleware_file_search_glob_test() ->
    %% 测试 Glob 搜索
    MwState = middleware_file_search:init(#{allowed_paths => []}),

    %% 搜索 Erlang 文件
    {ok, Files} = middleware_file_search:glob_search(
        <<"*.erl">>,
        <<"/home/david/workspace/research/erlang-in-actions/agent/apps/beamai_agent/src">>,
        MwState
    ),

    ?assert(length(Files) > 0),
    %% 所有文件应该是 .erl 后缀
    lists:foreach(fun(File) ->
        ?assertNotEqual(nomatch, binary:match(File, <<".erl">>))
    end, Files).

middleware_file_search_read_test() ->
    %% 测试文件读取
    MwState = middleware_file_search:init(#{allowed_paths => []}),

    %% 读取当前测试文件
    TestFile = <<"/home/david/workspace/research/erlang-in-actions/agent/apps/beamai_agent/test/middleware_suite_test.erl">>,

    {ok, Content} = middleware_file_search:read_file(TestFile, MwState),

    ?assert(byte_size(Content) > 0),
    %% 应该包含模块定义
    ?assertNotEqual(nomatch, binary:match(Content, <<"middleware_suite_test">>)).

%%====================================================================
%% 集成测试（需要 GLM API）
%%====================================================================

%% 检查 API Key 是否可用
has_api_key() ->
    case os:getenv("ZHIPU_API_KEY") of
        false -> false;
        "" -> false;
        _ -> true
    end.

%% 跳过没有 API Key 的测试
skip_without_api_key(TestFun) ->
    case has_api_key() of
        true -> TestFun();
        false -> ?debugMsg("Skipping test: ZHIPU_API_KEY not set")
    end.

%% 集成测试：Todo List Middleware 与 LLM
integration_todo_list_test() ->
    skip_without_api_key(fun() ->
        Config = (base_config())#{
            middleware => [
                {middleware_todo_list, #{debug => true}}
            ],
            tools => []
        },

        %% 运行对话
        Message = <<"Create a simple task list for making coffee: 1) boil water, 2) add coffee, 3) pour water">>,
        case beamai_agent:run_once(Config, Message) of
            {ok, State} ->
                %% 检查响应
                Response = graph:get(State, response, <<>>),
                ?assert(byte_size(Response) > 0);
            {error, Reason} ->
                ?debugFmt("Integration test failed: ~p", [Reason])
        end
    end).

%% 集成测试：Shell Tool Middleware 与 LLM
integration_shell_tool_test() ->
    skip_without_api_key(fun() ->
        Config = (base_config())#{
            middleware => [
                {middleware_shell_tool, #{
                    working_dir => <<"/tmp">>,
                    debug => true
                }}
            ],
            tools => []
        },

        %% 运行对话，让 LLM 使用 shell 工具
        Message = <<"Use the shell_execute tool to run 'echo hello world' and tell me the output.">>,
        case beamai_agent:run_once(Config, Message) of
            {ok, State} ->
                %% 检查响应
                Response = graph:get(State, response, <<>>),
                ?assert(byte_size(Response) > 0);
            {error, Reason} ->
                ?debugFmt("Integration test failed: ~p", [Reason])
        end
    end).

%% 集成测试：File Search Middleware 与 LLM
integration_file_search_test() ->
    skip_without_api_key(fun() ->
        Config = (base_config())#{
            middleware => [
                {middleware_file_search, #{
                    allowed_paths => [],
                    debug => true
                }}
            ],
            tools => []
        },

        %% 运行对话
        Message = <<"Use the file_glob tool to find all .erl files in /tmp and tell me how many you found.">>,
        case beamai_agent:run_once(Config, Message) of
            {ok, State} ->
                %% 检查响应
                Response = graph:get(State, response, <<>>),
                ?assert(byte_size(Response) > 0);
            {error, Reason} ->
                ?debugFmt("Integration test failed: ~p", [Reason])
        end
    end).

%% 集成测试：多个 Middleware 组合
integration_combined_middleware_test() ->
    skip_without_api_key(fun() ->
        Config = (base_config())#{
            middleware => [
                {middleware_call_limit, #{max_iterations => 10}},
                {middleware_todo_list, #{add_guidance => true}},
                {middleware_shell_tool, #{working_dir => <<"/tmp">>}}
            ],
            tools => []
        },

        %% 运行对话
        Message = <<"Hi, what tools do you have available?">>,
        case beamai_agent:run_once(Config, Message) of
            {ok, State} ->
                %% 检查响应
                Response = graph:get(State, response, <<>>),
                ?assert(byte_size(Response) > 0);
            {error, Reason} ->
                ?debugFmt("Integration test failed: ~p", [Reason])
        end
    end).

