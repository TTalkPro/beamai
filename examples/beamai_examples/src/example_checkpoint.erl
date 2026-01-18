%%%-------------------------------------------------------------------
%%% @doc Agent Deep Checkpoint 使用示例
%%%
%%% 演示如何使用 beamai_deepagent 的 checkpoint 功能：
%%% 1. 启用存储和自动保存
%%% 2. 手动保存 checkpoint
%%% 3. 从 checkpoint 恢复
%%% 4. 使用 checkpoint 工具
%%%
%%% 运行方式：
%%% ```
%%% erlc -I ../../apps -o ../../apps beamai_deepagent_checkpoint_example.erl
%%% erl -pa ../../apps/*/ebin -s beamai_deepagent_checkpoint_example run -s init stop
%%% ```
%%% @end
%%%-------------------------------------------------------------------
-module(example_checkpoint).

-export([run/0]).

%%====================================================================
%% 示例入口
%%====================================================================

run() ->
    io:format("=== Agent Deep Checkpoint 功能示例 ===~n~n"),

    %% 配置 LLM（使用智谱 GLM-4.7 通过 Anthropic 兼容 API）
    ApiKey = case os:getenv("ZHIPU_API_KEY") of
        false -> <<"your-api-key">>;
        Key -> list_to_binary(Key)
    end,
    LLMConfig = #{
        provider => anthropic,
        api_key => ApiKey,
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
        model => <<"glm-4.7">>,
        max_tokens => 2048,
        timeout => 120000
    },

    %% 示例 1：启用自动 checkpoint
    example_auto_checkpoint(LLMConfig),

    %% 示例 2：手动保存 checkpoint
    example_manual_checkpoint(LLMConfig),

    %% 示例 3：从 checkpoint 恢复
    example_restore_checkpoint(LLMConfig),

    %% 示例 4：使用 checkpoint 工具
    example_checkpoint_tool(LLMConfig),

    io:format("~n=== 所有示例完成 ===~n").

%%====================================================================
%% 示例 1：启用自动 checkpoint
%%====================================================================

example_auto_checkpoint(LLMConfig) ->
    io:format("~n--- 示例 1：启用自动 Checkpoint ---~n"),

    Config = beamai_deepagent:new(#{
        llm => LLMConfig,
        enable_storage => true,      %% 启用存储
        auto_checkpoint => true,     %% 自动保存
        max_depth => 2
    }),

    Message1 = <<"研究 Erlang 的并发模型"/utf8>>,

    %% 第一次运行（会自动保存 checkpoint）
    io:format("运行第 1 个任务...~n"),
    {ok, Result1} = beamai_deepagent:run(Config, Message1),
    io:format("完成，response: ~ts~n", [maps:get(response, Result1)]),

    %% 第二次运行（会自动保存新的 checkpoint）
    io:format("~n运行第 2 个任务...~n"),
    Message2 = <<"研究 Erlang 的分布式特性"/utf8>>,
    {ok, Result2} = beamai_deepagent:run(Config, Message2),
    io:format("完成，response: ~ts~n", [maps:get(response, Result2)]),

    %% 列出所有 checkpoints
    io:format("~n列出所有 checkpoints:~n"),
    {ok, Checkpoints} = beamai_deepagent:list_checkpoints(Result2),
    lists:foreach(fun(Cp) ->
        CpId = maps:get(id, Cp),
        Timestamp = maps:get(timestamp, Cp),
        io:format("  - ~s (~p)~n", [CpId, Timestamp])
    end, Checkpoints).

%%====================================================================
%% 示例 2：手动保存 checkpoint
%%====================================================================

example_manual_checkpoint(LLMConfig) ->
    io:format("~n--- 示例 2：手动保存 Checkpoint ---~n"),

    Config = beamai_deepagent:new(#{
        llm => LLMConfig,
        enable_storage => true,
        max_depth => 2
    }),

    Message = <<"分析 Go 和 Erlang 的并发模型差异"/utf8>>,

    %% 运行任务
    io:format("运行任务...~n"),
    {ok, Result} = beamai_deepagent:run(Config, Message),
    io:format("完成，response: ~ts~n", [maps:get(response, Result)]),

    %% 手动保存 checkpoint
    io:format("~n保存 checkpoint...~n"),
    {ok, CpId} = beamai_deepagent:save_checkpoint(Result, #{
        tag => <<"analysis_complete">>,
        description => <<"完成 Go vs Erlang 并发模型分析"/utf8>>
    }),
    io:format("Checkpoint 已保存: ~s~n", [CpId]).

%%====================================================================
%% 示例 3：从 checkpoint 恢复
%%====================================================================

example_restore_checkpoint(LLMConfig) ->
    io:format("~n--- 示例 3：从 Checkpoint 恢复 ---~n"),

    Config = beamai_deepagent:new(#{
        llm => LLMConfig,
        enable_storage => true,
        max_depth => 2
    }),

    %% 第一次运行
    Message1 = <<"研究 Python 的异步编程"/utf8>>,
    io:format("第 1 次运行...~n"),
    {ok, Result1} = beamai_deepagent:run(Config, Message1),

    %% 保存 checkpoint
    {ok, CpId} = beamai_deepagent:save_checkpoint(Result1, #{tag => <<"research_start">>}),
    io:format("Checkpoint 已保存: ~s~n", [CpId]),

    %% 第二次运行（新任务）
    Message2 = <<"研究 JavaScript 的 Promise"/utf8>>,
    io:format("~n第 2 次运行...~n"),
    {ok, Result2} = beamai_deepagent:run(Config, Message2),

    %% 从 checkpoint 恢复并继续
    io:format("~n从 checkpoint 恢复...~n"),
    RestoreMessage = <<"继续 Python 异步编程研究"/utf8>>,
    {ok, Result3} = beamai_deepagent:restore_from_checkpoint(CpId, RestoreMessage),
    io:format("恢复后完成，response: ~ts~n", [maps:get(response, Result3)]).

%%====================================================================
%% 示例 4：使用 checkpoint 工具
%%====================================================================

example_checkpoint_tool(LLMConfig) ->
    io:format("~n--- 示例 4：使用 Checkpoint 工具 ---~n"),

    Config = beamai_deepagent:new(#{
        llm => LLMConfig,
        enable_storage => true,      %% 必须启用存储才能持久化
        max_depth => 2,
        system_prompt => <<"你是一个研究助手。在完成每个重要步骤后，使用 checkpoint 工具保存进度。"/utf8>>
    }),

    Message = <<"研究 Rust 的所有权系统，分 3 个步骤进行"/utf8>>,

    %% LLM 会在执行过程中调用 checkpoint 工具
    io:format("运行任务（LLM 会自动调用 checkpoint 工具）...~n"),
    {ok, Result} = beamai_deepagent:run(Config, Message),

    io:format("完成，response: ~ts~n", [maps:get(response, Result)]),

    %% 查看轨迹中是否有 checkpoint 调用
    Trace = beamai_deepagent:get_trace(Result),
    io:format("~n执行轨迹中的 checkpoint 调用:~n"),
    print_checkpoint_calls(Trace).

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 打印轨迹中的 checkpoint 调用
print_checkpoint_calls(Trace) ->
    lists:foreach(fun(Entry) ->
        case maps:get(type, Entry, undefined) of
            checkpoint ->
                Label = maps:get(label, Entry, <<"">>),
                Saved = maps:get(saved, Entry, false),
                io:format("  - Checkpoint: ~ts (saved: ~p)~n", [Label, Saved]);
            _ ->
                ok
        end
    end, Trace).
