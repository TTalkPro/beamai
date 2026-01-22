%%%-------------------------------------------------------------------
%%% @doc Agent Checkpoint 使用示例
%%%
%%% 演示如何使用 beamai_agent 的 checkpoint 功能：
%%% 1. 启用存储和自动保存
%%% 2. 手动保存 checkpoint
%%% 3. 从 checkpoint 恢复
%%% 4. 列出和管理 checkpoints
%%%
%%% 使用方法:
%%% ```
%%% %% 方式 1: 使用环境变量配置 API Key（推荐使用智谱 GLM-4.7）
%%% %% 设置环境变量: export ZHIPU_API_KEY=your-api-key
%%% example_checkpoint:run().
%%%
%%% %% 方式 2: 直接传入 LLM 配置
%%% LLMConfig = llm_client:create(anthropic, #{
%%%     api_key => <<"your-api-key">>,
%%%     base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
%%%     model => <<"glm-4.7">>
%%% }),
%%% example_checkpoint:run(LLMConfig).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_checkpoint).

-export([run/0, run/1,example_list_all_checkpoints/1]).

%%====================================================================
%% 示例入口
%%====================================================================

%% @doc 运行所有示例（使用环境变量配置）
-spec run() -> ok | {error, term()}.
run() ->
    case example_utils:get_llm_config() of
        {ok, LLMConfig} ->
            run(LLMConfig);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason]),
            io:format("请设置环境变量 ZHIPU_API_KEY（推荐），~n"),
            io:format("或者使用 run(LLMConfig) 直接传入配置。~n"),
            {error, Reason}
    end.

%% @doc 运行所有示例（使用指定的 LLM 配置）
-spec run(map()) -> ok.
run(LLMConfig) ->
    io:format("=== Agent Checkpoint 功能示例 ===~n~n"),

    %% 示例 1：启用自动 checkpoint
    example_auto_checkpoint(LLMConfig),

    %% 示例 2：手动保存 checkpoint
    example_manual_checkpoint(LLMConfig),

    %% 示例 3：从 checkpoint 恢复
    example_restore_checkpoint(LLMConfig),

    %% 示例 4：列出所有 checkpoints 详情
    example_list_all_checkpoints(LLMConfig),

    io:format("~n=== 所有示例完成 ===~n"),
    ok.

%%====================================================================
%% 示例 1：启用自动 checkpoint
%%====================================================================

example_auto_checkpoint(LLMConfig) ->
    io:format("~n--- 示例 1：启用自动 Checkpoint ---~n"),

    %% 1. 创建存储后端（ETS 内存存储）
    StoreName = checkpoint_example_store_1,
    {ok, _} = beamai_store_ets:start_link(StoreName, #{max_items => 1000}),

    %% 2. 创建 Memory 实例
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName}
    }),

    %% 3. 创建启用存储的 Agent
    {ok, Agent} = beamai_agent:start_link(<<"checkpoint_auto_test">>, #{
        system_prompt => <<"你是一个有帮助的助手。请简洁回答。"/utf8>>,
        llm => LLMConfig,
        storage => Memory  %% 传入 Memory 实例启用 checkpoint
    }),

    %% 第一次运行（会自动保存 checkpoint）
    io:format("运行第 1 个任务...~n"),
    case beamai_agent:run(Agent, <<"请用一句话介绍 Erlang"/utf8>>) of
        {ok, Result1} ->
            io:format("完成，response: ~ts~n", [maps:get(final_response, Result1, <<>>)]);
        {error, Reason1} ->
            io:format("错误: ~p~n", [Reason1])
    end,

    %% 第二次运行（会自动保存新的 checkpoint）
    io:format("~n运行第 2 个任务...~n"),
    case beamai_agent:run(Agent, <<"Erlang 的并发模型是什么？"/utf8>>) of
        {ok, Result2} ->
            io:format("完成，response: ~ts~n", [maps:get(final_response, Result2, <<>>)]);
        {error, Reason2} ->
            io:format("错误: ~p~n", [Reason2])
    end,

    %% 列出所有 checkpoints
    io:format("~n列出所有 checkpoints:~n"),
    case beamai_agent:list_checkpoints(Agent) of
        {ok, Checkpoints} ->
            lists:foreach(fun(Cp) ->
                CpId = maps:get(id, Cp, <<"unknown">>),
                io:format("  - ~s~n", [CpId])
            end, Checkpoints);
        {error, ListError} ->
            io:format("列出失败: ~p~n", [ListError])
    end,

    %% 清理
    beamai_agent:stop(Agent),
    beamai_store_ets:stop(StoreName).

%%====================================================================
%% 示例 2：手动保存 checkpoint
%%====================================================================

example_manual_checkpoint(LLMConfig) ->
    io:format("~n--- 示例 2：手动保存 Checkpoint ---~n"),

    %% 1. 创建存储后端
    StoreName = checkpoint_example_store_2,
    {ok, _} = beamai_store_ets:start_link(StoreName, #{max_items => 1000}),

    %% 2. 创建 Memory 实例
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName}
    }),

    %% 3. 创建 Agent
    {ok, Agent} = beamai_agent:start_link(<<"checkpoint_manual_test">>, #{
        system_prompt => <<"你是一个有帮助的助手。"/utf8>>,
        llm => LLMConfig,
        storage => Memory
    }),

    %% 运行任务
    io:format("运行任务...~n"),
    case beamai_agent:run(Agent, <<"什么是函数式编程？"/utf8>>) of
        {ok, Result} ->
            io:format("完成，response: ~ts~n", [maps:get(final_response, Result, <<>>)]),

            %% 手动保存 checkpoint（带元数据）
            io:format("~n手动保存 checkpoint...~n"),
            case beamai_agent:save_checkpoint(Agent, #{
                tag => <<"fp_intro">>,
                description => <<"函数式编程介绍完成"/utf8>>
            }) of
                {ok, CpId} ->
                    io:format("Checkpoint 已保存: ~s~n", [CpId]);
                {error, SaveError} ->
                    io:format("保存失败: ~p~n", [SaveError])
            end;
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,

    %% 清理
    beamai_agent:stop(Agent),
    beamai_store_ets:stop(StoreName).

%%====================================================================
%% 示例 3：从 checkpoint 恢复
%%====================================================================

example_restore_checkpoint(LLMConfig) ->
    io:format("~n--- 示例 3：从 Checkpoint 恢复 ---~n"),

    %% 1. 创建存储后端
    StoreName = checkpoint_example_store_3,
    {ok, _} = beamai_store_ets:start_link(StoreName, #{max_items => 1000}),

    %% 2. 创建 Memory 实例
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName}
    }),

    %% 3. 创建 Agent
    {ok, Agent} = beamai_agent:start_link(<<"checkpoint_restore_test">>, #{
        system_prompt => <<"你是一个有帮助的助手。记住用户告诉你的信息。"/utf8>>,
        llm => LLMConfig,
        storage => Memory
    }),

    %% 第一次运行：告诉 Agent 一些信息
    io:format("第 1 次运行（设置上下文）...~n"),
    case beamai_agent:run(Agent, <<"我的名字是 Alice，我喜欢编程。"/utf8>>) of
        {ok, _Result1} ->
            io:format("完成~n");
        {error, Reason1} ->
            io:format("错误: ~p~n", [Reason1])
    end,

    %% 保存 checkpoint
    io:format("~n保存 checkpoint...~n"),
    CpId = case beamai_agent:save_checkpoint(Agent, #{tag => <<"context_set">>}) of
        {ok, Id} ->
            io:format("Checkpoint 已保存: ~s~n", [Id]),
            Id;
        {error, SaveError} ->
            io:format("保存失败: ~p~n", [SaveError]),
            undefined
    end,

    %% 第二次运行：新的对话（不相关）
    io:format("~n第 2 次运行（新话题）...~n"),
    case beamai_agent:run(Agent, <<"今天天气怎么样？"/utf8>>) of
        {ok, Result2} ->
            io:format("完成，response: ~ts~n", [maps:get(final_response, Result2, <<>>)]);
        {error, Reason2} ->
            io:format("错误: ~p~n", [Reason2])
    end,

    %% 从 checkpoint 恢复
    case CpId of
        undefined ->
            io:format("~n跳过恢复（无有效 checkpoint）~n");
        _ ->
            io:format("~n从 checkpoint 恢复...~n"),
            case beamai_agent:restore_from_checkpoint(Agent, CpId) of
                ok ->
                    io:format("恢复成功~n"),

                    %% 验证上下文是否恢复
                    io:format("~n验证上下文...~n"),
                    case beamai_agent:run(Agent, <<"我的名字是什么？"/utf8>>) of
                        {ok, Result3} ->
                            io:format("回答: ~ts~n", [maps:get(final_response, Result3, <<>>)]);
                        {error, Reason3} ->
                            io:format("错误: ~p~n", [Reason3])
                    end;
                {error, RestoreError} ->
                    io:format("恢复失败: ~p~n", [RestoreError])
            end
    end,

    %% 清理
    beamai_agent:stop(Agent),
    beamai_store_ets:stop(StoreName).

%%====================================================================
%% 示例 4：列出所有 checkpoints 详情
%%====================================================================

example_list_all_checkpoints(LLMConfig) ->
    io:format("~n--- 示例 4：列出所有 Checkpoints 详情 ---~n"),

    %% 1. 创建存储后端
    StoreName = checkpoint_example_store_4,
    {ok, _} = beamai_store_ets:start_link(StoreName, #{max_items => 1000}),

    %% 2. 创建 Memory 实例
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName}
    }),

    %% 3. 创建 Agent
    {ok, Agent} = beamai_agent:start_link(<<"checkpoint_list_test">>, #{
        system_prompt => <<"你是一个有帮助的助手。"/utf8>>,
        llm => LLMConfig,
        storage => Memory
    }),

    %% 进行多轮对话并保存多个 checkpoints
    io:format("进行多轮对话...~n"),

    %% 第 1 轮
    _ = beamai_agent:run(Agent, <<"你好，我是测试用户"/utf8>>),
    {ok, CpId1} = beamai_agent:save_checkpoint(Agent, #{
        tag => <<"greeting">>,
        step => 1
    }),
    io:format("  保存 checkpoint 1: ~s~n", [CpId1]),

    %% 第 2 轮
    _ = beamai_agent:run(Agent, <<"什么是 OTP？"/utf8>>),
    {ok, CpId2} = beamai_agent:save_checkpoint(Agent, #{
        tag => <<"otp_question">>,
        step => 2
    }),
    io:format("  保存 checkpoint 2: ~s~n", [CpId2]),

    %% 第 3 轮
    _ = beamai_agent:run(Agent, <<"GenServer 是什么？"/utf8>>),
    {ok, CpId3} = beamai_agent:save_checkpoint(Agent, #{
        tag => <<"genserver_question">>,
        step => 3
    }),
    io:format("  保存 checkpoint 3: ~s~n", [CpId3]),

    %% 列出所有 checkpoints
    io:format("~n列出所有 checkpoints:~n"),
    io:format("~s~n", [string:copies("=", 70)]),

    case beamai_agent:list_checkpoints(Agent) of
        {ok, Checkpoints} ->
            lists:foreach(fun(Cp) ->
                print_checkpoint_details(Cp)
            end, Checkpoints),
            io:format("~n共 ~p 个 checkpoints~n", [length(Checkpoints)]);
        {error, ListError} ->
            io:format("列出失败: ~p~n", [ListError])
    end,

    %% 加载特定 checkpoint 查看详情
    io:format("~n加载 checkpoint 2 的完整数据:~n"),
    case beamai_agent:load_checkpoint(Agent, CpId2) of
        {ok, CpData} ->
            print_checkpoint_data(CpData);
        {error, LoadError} ->
            io:format("加载失败: ~p~n", [LoadError])
    end,

    %% 清理
    beamai_agent:stop(Agent),
    beamai_store_ets:stop(StoreName).

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 打印 checkpoint 摘要信息
print_checkpoint_details(Cp) ->
    Id = maps:get(id, Cp, <<"unknown">>),
    Data = maps:get(data, Cp, #{}),
    Metadata = maps:get(metadata, Cp, #{}),

    %% 从 data 中提取消息数量
    Messages = maps:get(messages, Data, []),
    MsgCount = length(Messages),

    %% 从 metadata 中提取信息
    %% 用户自定义元数据在 metadata.metadata 中
    UserMeta = maps:get(metadata, Metadata, #{}),
    Tag = maps:get(tag, UserMeta, maps:get(<<"tag">>, UserMeta, <<"-">>)),
    UserStep = maps:get(step, UserMeta, maps:get(<<"step">>, UserMeta, <<"-">>)),

    %% 系统元数据
    Iteration = maps:get(iteration, Metadata, 0),
    CheckpointType = maps:get(checkpoint_type, Metadata, undefined),

    io:format("~nCheckpoint: ~s~n", [Id]),
    io:format("  Type: ~p~n", [CheckpointType]),
    io:format("  Iteration: ~p~n", [Iteration]),
    io:format("  Tag: ~p~n", [Tag]),
    io:format("  User Step: ~p~n", [UserStep]),
    io:format("  Messages: ~p 条~n", [MsgCount]).

%% @private 打印 checkpoint 完整数据
print_checkpoint_data(CpData) ->
    io:format("~s~n", [string:copies("-", 50)]),

    %% 打印消息历史
    Messages = maps:get(messages, CpData, []),
    io:format("消息历史 (~p 条):~n", [length(Messages)]),
    lists:foreach(fun(Msg) ->
        Role = maps:get(role, Msg, unknown),
        Content = maps:get(content, Msg, <<>>),
        %% 截断过长的内容
        DisplayContent = case byte_size(Content) > 100 of
            true -> <<(binary:part(Content, 0, 100))/binary, "..."/utf8>>;
            false -> Content
        end,
        io:format("  [~p] ~ts~n", [Role, DisplayContent])
    end, Messages),

    %% 打印上下文
    Context = maps:get(context, CpData, #{}),
    case maps:size(Context) > 0 of
        true ->
            io:format("~n上下文:~n"),
            maps:foreach(fun(K, V) ->
                io:format("  ~p: ~p~n", [K, V])
            end, Context);
        false ->
            ok
    end,

    %% 打印 metadata
    Metadata = maps:get(metadata, CpData, #{}),
    case maps:size(Metadata) > 0 of
        true ->
            io:format("~n元数据:~n"),
            maps:foreach(fun(K, V) ->
                io:format("  ~p: ~p~n", [K, V])
            end, Metadata);
        false ->
            ok
    end,

    io:format("~s~n", [string:copies("-", 50)]).
