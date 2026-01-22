%%%-------------------------------------------------------------------
%%% @doc Agent Checkpoint 使用示例（新 API）
%%%
%%% 演示如何使用新的 checkpoint 架构：
%%% 1. thread_id 在 Memory 中管理（自动生成或手动指定）
%%% 2. Checkpoint 通过 on_checkpoint 回调自动保存
%%% 3. 通过 beamai_memory API 直接访问 checkpoints
%%%
%%% == API 变更说明 ==
%%%
%%% 旧 API（已移除）：
%%% - beamai_agent:save_checkpoint/1,2
%%% - beamai_agent:load_checkpoint/2
%%% - beamai_agent:list_checkpoints/1,2
%%% - beamai_agent:restore_from_checkpoint/2
%%%
%%% 新 API：
%%% - beamai_memory:get_thread_id/1 - 获取 Memory 的 thread_id
%%% - beamai_memory:list_checkpoints/2 - 列出 checkpoints
%%% - beamai_memory:load_checkpoint/2 - 加载 checkpoint
%%% - beamai_memory:load_latest_checkpoint/2 - 加载最新 checkpoint
%%%
%%% 使用方法:
%%% ```
%%% %% 方式 1: 使用环境变量配置 API Key
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

-export([run/0, run/1]).
-export([example_list_all_checkpoints/1]).

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
            io:format("请设置环境变量 ZHIPU_API_KEY，~n"),
            io:format("或者使用 run(LLMConfig) 直接传入配置。~n"),
            {error, Reason}
    end.

%% @doc 运行所有示例（使用指定的 LLM 配置）
-spec run(map()) -> ok.
run(LLMConfig) ->
    io:format("=== Agent Checkpoint 功能示例（新 API）===~n~n"),

    %% 示例 1：自动 checkpoint（通过回调）
    example_auto_checkpoint(LLMConfig),

    %% 示例 2：通过 Memory 访问 checkpoints
    example_access_checkpoints(LLMConfig),

    %% 示例 3：自定义 thread_id
    example_custom_thread_id(LLMConfig),

    %% 示例 4：列出所有 checkpoints 详情
    example_list_all_checkpoints(LLMConfig),

    io:format("~n=== 所有示例完成 ===~n"),
    ok.

%%====================================================================
%% 示例 1：自动 checkpoint（通过回调）
%%====================================================================

example_auto_checkpoint(LLMConfig) ->
    io:format("~n--- 示例 1：自动 Checkpoint（通过回调）---~n"),
    io:format("Checkpoint 现在在图执行时自动保存，无需手动调用。~n~n"),

    %% 1. 创建存储后端（ETS 内存存储）
    StoreName = checkpoint_example_store_1,
    {ok, _} = beamai_store_ets:start_link(StoreName, #{max_items => 1000}),

    %% 2. 创建 Memory 实例（thread_id 自动生成）
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName}
    }),

    %% 获取自动生成的 thread_id
    ThreadId = beamai_memory:get_thread_id(Memory),
    io:format("自动生成的 thread_id: ~s~n", [ThreadId]),

    %% 3. 创建 Agent（传入 Memory 启用自动 checkpoint）
    {ok, Agent} = beamai_agent:start_link(<<"checkpoint_auto_test">>, #{
        system_prompt => <<"你是一个有帮助的助手。请简洁回答。"/utf8>>,
        llm => LLMConfig,
        storage => Memory  %% 传入 Memory 实例启用自动 checkpoint
    }),

    %% 第一次运行（checkpoint 自动保存）
    io:format("~n运行第 1 个任务...~n"),
    case beamai_agent:run(Agent, <<"请用一句话介绍 Erlang"/utf8>>) of
        {ok, Result1} ->
            io:format("完成，response: ~ts~n", [maps:get(final_response, Result1, <<>>)]);
        {error, Reason1} ->
            io:format("错误: ~p~n", [Reason1])
    end,

    %% 第二次运行（checkpoint 自动保存）
    io:format("~n运行第 2 个任务...~n"),
    case beamai_agent:run(Agent, <<"Erlang 的并发模型是什么？"/utf8>>) of
        {ok, Result2} ->
            io:format("完成，response: ~ts~n", [maps:get(final_response, Result2, <<>>)]);
        {error, Reason2} ->
            io:format("错误: ~p~n", [Reason2])
    end,

    %% 通过 Memory 直接列出 checkpoints
    io:format("~n通过 Memory 列出所有 checkpoints:~n"),
    case beamai_memory:list_checkpoints(Memory, #{thread_id => ThreadId}) of
        {ok, Checkpoints} ->
            io:format("共 ~p 个 checkpoints~n", [length(Checkpoints)]),
            lists:foreach(fun({Cp, _Meta, _Parent}) ->
                CpId = get_checkpoint_field(Cp, id),
                io:format("  - ~s~n", [CpId])
            end, Checkpoints);
        {error, ListError} ->
            io:format("列出失败: ~p~n", [ListError])
    end,

    %% 清理
    beamai_agent:stop(Agent),
    beamai_store_ets:stop(StoreName).

%%====================================================================
%% 示例 2：通过 Memory 访问 checkpoints
%%====================================================================

example_access_checkpoints(LLMConfig) ->
    io:format("~n--- 示例 2：通过 Memory 访问 Checkpoints ---~n"),

    %% 1. 创建存储后端
    StoreName = checkpoint_example_store_2,
    {ok, _} = beamai_store_ets:start_link(StoreName, #{max_items => 1000}),

    %% 2. 创建 Memory 实例
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName}
    }),
    ThreadId = beamai_memory:get_thread_id(Memory),

    %% 3. 创建 Agent
    {ok, Agent} = beamai_agent:start_link(<<"checkpoint_access_test">>, #{
        system_prompt => <<"你是一个有帮助的助手。"/utf8>>,
        llm => LLMConfig,
        storage => Memory
    }),

    %% 进行多轮对话
    io:format("进行多轮对话...~n"),
    _ = beamai_agent:run(Agent, <<"你好，我是 Alice"/utf8>>),
    _ = beamai_agent:run(Agent, <<"什么是 OTP？"/utf8>>),
    _ = beamai_agent:run(Agent, <<"GenServer 是什么？"/utf8>>),

    %% 通过 Memory 访问 checkpoints
    io:format("~n通过 Memory 访问 checkpoints:~n"),

    %% 列出所有 checkpoints
    case beamai_memory:list_checkpoints(Memory, #{thread_id => ThreadId, limit => 10}) of
        {ok, Checkpoints} ->
            io:format("共 ~p 个 checkpoints:~n", [length(Checkpoints)]),
            lists:foreach(fun({Cp, Meta, _Parent}) ->
                print_checkpoint_info(Cp, Meta)
            end, Checkpoints);
        {error, ListError} ->
            io:format("列出失败: ~p~n", [ListError])
    end,

    %% 加载最新 checkpoint
    io:format("~n加载最新 checkpoint:~n"),
    case beamai_memory:load_latest_checkpoint(Memory, #{thread_id => ThreadId}) of
        {ok, LatestData} ->
            %% global_state 使用 binary 键
            Messages = get_data_value(LatestData, messages, []),
            io:format("  消息数量: ~p~n", [length(Messages)]);
        {error, LoadError} ->
            io:format("加载失败: ~p~n", [LoadError])
    end,

    %% 清理
    beamai_agent:stop(Agent),
    beamai_store_ets:stop(StoreName).

%%====================================================================
%% 示例 3：自定义 thread_id
%%====================================================================

example_custom_thread_id(LLMConfig) ->
    io:format("~n--- 示例 3：自定义 thread_id ---~n"),

    %% 1. 创建存储后端
    StoreName = checkpoint_example_store_3,
    {ok, _} = beamai_store_ets:start_link(StoreName, #{max_items => 1000}),

    %% 2. 创建 Memory 实例（指定自定义 thread_id）
    CustomThreadId = <<"my_custom_session_123">>,
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => CustomThreadId  %% 自定义 thread_id
    }),

    %% 验证 thread_id
    ThreadId = beamai_memory:get_thread_id(Memory),
    io:format("自定义 thread_id: ~s~n", [ThreadId]),
    io:format("thread_id 匹配: ~p~n", [ThreadId =:= CustomThreadId]),

    %% 3. 创建 Agent
    {ok, Agent} = beamai_agent:start_link(<<"checkpoint_custom_thread_test">>, #{
        system_prompt => <<"你是一个有帮助的助手。记住用户告诉你的信息。"/utf8>>,
        llm => LLMConfig,
        storage => Memory
    }),

    %% 运行任务
    io:format("~n运行任务...~n"),
    case beamai_agent:run(Agent, <<"我的名字是 Bob，我喜欢 Erlang。"/utf8>>) of
        {ok, Result} ->
            io:format("完成，response: ~ts~n", [maps:get(final_response, Result, <<>>)]);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,

    %% 使用自定义 thread_id 访问 checkpoints
    io:format("~n使用自定义 thread_id 访问 checkpoints:~n"),
    case beamai_memory:list_checkpoints(Memory, #{thread_id => CustomThreadId}) of
        {ok, Checkpoints} ->
            io:format("thread_id '~s' 下共 ~p 个 checkpoints~n",
                      [CustomThreadId, length(Checkpoints)]);
        {error, ListError} ->
            io:format("列出失败: ~p~n", [ListError])
    end,

    %% 清理
    beamai_agent:stop(Agent),
    beamai_store_ets:stop(StoreName).

%%====================================================================
%% 示例 4：列出所有 checkpoints 详情
%%====================================================================

%% @doc 列出 Agent 保存在内存中的所有 checkpoints
%%
%% 这个示例展示如何：
%% 1. 运行 Agent 多次，让其自动保存多个 checkpoints
%% 2. 通过 beamai_memory API 列出所有 checkpoints
%% 3. 查看每个 checkpoint 的详细信息（消息、元数据等）
%% 4. 加载特定 checkpoint 的完整数据
-spec example_list_all_checkpoints(map()) -> ok.
example_list_all_checkpoints(LLMConfig) ->
    io:format("~n--- 示例 4：列出所有 Checkpoints 详情 ---~n"),
    io:format("演示如何列出并查看 Agent 保存的所有 checkpoints~n~n"),

    %% 1. 创建存储后端
    StoreName = checkpoint_example_store_4,
    {ok, _} = beamai_store_ets:start_link(StoreName, #{max_items => 1000}),

    %% 2. 创建 Memory 实例（使用自定义 thread_id 便于识别）
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"list_checkpoints_demo">>
    }),
    ThreadId = beamai_memory:get_thread_id(Memory),
    io:format("thread_id: ~s~n", [ThreadId]),

    %% 3. 创建 Agent
    {ok, Agent} = beamai_agent:start_link(<<"checkpoint_list_demo">>, #{
        system_prompt => <<"你是一个有帮助的助手。"/utf8>>,
        llm => LLMConfig,
        storage => Memory
    }),

    %% 4. 进行多轮对话（每次运行都会自动保存 checkpoints）
    io:format("~n进行多轮对话以生成 checkpoints...~n"),

    io:format("  第 1 轮对话...~n"),
    _ = beamai_agent:run(Agent, <<"你好，请记住我叫 Alice"/utf8>>),

    io:format("  第 2 轮对话...~n"),
    _ = beamai_agent:run(Agent, <<"Erlang 是什么？"/utf8>>),

    io:format("  第 3 轮对话...~n"),
    _ = beamai_agent:run(Agent, <<"OTP 有什么作用？"/utf8>>),

    %% 5. 列出所有 checkpoints
    io:format("~n" ++ string:copies("=", 60) ++ "~n"),
    io:format("列出所有 checkpoints:~n"),
    io:format(string:copies("=", 60) ++ "~n"),

    case beamai_memory:list_checkpoints(Memory, #{thread_id => ThreadId}) of
        {ok, Checkpoints} ->
            io:format("~n共找到 ~p 个 checkpoints~n~n", [length(Checkpoints)]),

            %% 按 timestamp 排序（从旧到新）
            SortedCheckpoints = lists:sort(
                fun({CpA, _, _}, {CpB, _, _}) ->
                    get_checkpoint_field(CpA, timestamp) =< get_checkpoint_field(CpB, timestamp)
                end,
                Checkpoints
            ),

            %% 遍历并打印每个 checkpoint 的详情
            lists:foldl(fun({Cp, Meta, _Parent}, Index) ->
                print_checkpoint_details(Index, Cp, Meta),
                Index + 1
            end, 1, SortedCheckpoints),

            %% 6. 演示加载最后 2 个 checkpoint 的完整数据
            %% 按时间排序后，最后的 checkpoint 应该是 final 类型，包含 LLM 回复
            Last2 = lists:nthtail(max(0, length(SortedCheckpoints) - 2), SortedCheckpoints),
            lists:foreach(fun({Cp, _, _}) ->
                CpId = get_checkpoint_field(Cp, id),
                io:format("~n" ++ string:copies("-", 60) ++ "~n"),
                io:format("加载 checkpoint (~s) 的完整数据:~n", [CpId]),
                io:format(string:copies("-", 60) ++ "~n"),
                case beamai_memory:load_checkpoint_tuple(Memory, #{
                    thread_id => ThreadId,
                    checkpoint_id => CpId
                }) of
                    {ok, {LoadedCp, LoadedMeta, _}} ->
                        print_checkpoint_data(LoadedCp, LoadedMeta);
                    {error, LoadErr} ->
                        io:format("加载失败: ~p~n", [LoadErr])
                end
            end, Last2);

        {error, not_found} ->
            io:format("没有找到任何 checkpoints~n");

        {error, ListError} ->
            io:format("列出失败: ~p~n", [ListError])
    end,

    %% 清理
    beamai_agent:stop(Agent),
    beamai_store_ets:stop(StoreName),
    ok.

%%====================================================================
%% 辅助函数
%%====================================================================

%% 注意：不使用 record 定义，而是通过 map 访问 checkpoint 数据
%% checkpoint 元组格式: {Checkpoint, Metadata, ParentConfig}
%% Checkpoint 是一个 record，但我们通过 element/2 访问字段

%% @private 获取 checkpoint 字段
%% checkpoint record 字段顺序: {checkpoint, id, thread_id, parent_id, values, timestamp}
get_checkpoint_field(Cp, id) -> element(2, Cp);
get_checkpoint_field(Cp, thread_id) -> element(3, Cp);
get_checkpoint_field(Cp, parent_id) -> element(4, Cp);
get_checkpoint_field(Cp, values) -> element(5, Cp);
get_checkpoint_field(Cp, timestamp) -> element(6, Cp).

%% @private 获取 metadata 字段
%% checkpoint_metadata record 字段顺序:
%% {checkpoint_metadata, checkpoint_type, step, active_vertices, completed_vertices,
%%  run_id, agent_id, agent_name, iteration, metadata}
get_metadata_field(Meta, checkpoint_type) -> element(2, Meta);
get_metadata_field(Meta, step) -> element(3, Meta);
get_metadata_field(Meta, active_vertices) -> element(4, Meta);
get_metadata_field(Meta, completed_vertices) -> element(5, Meta);
get_metadata_field(Meta, run_id) -> element(6, Meta);
get_metadata_field(Meta, agent_id) -> element(7, Meta);
get_metadata_field(Meta, agent_name) -> element(8, Meta);
get_metadata_field(Meta, iteration) -> element(9, Meta);
get_metadata_field(Meta, metadata) -> element(10, Meta).

%% @private 打印 checkpoint 简要信息
print_checkpoint_info(Cp, Meta) ->
    CpId = get_checkpoint_field(Cp, id),
    Timestamp = get_checkpoint_field(Cp, timestamp),
    CheckpointType = get_metadata_field(Meta, checkpoint_type),
    Iteration = get_metadata_field(Meta, iteration),

    io:format("  Checkpoint: ~s~n", [CpId]),
    io:format("    Type: ~p~n", [CheckpointType]),
    io:format("    Iteration: ~p~n", [Iteration]),
    io:format("    Timestamp: ~p~n", [Timestamp]).

%% @private 打印 checkpoint 详细信息（用于示例 4）
print_checkpoint_details(Index, Cp, Meta) ->
    CpId = get_checkpoint_field(Cp, id),
    Timestamp = get_checkpoint_field(Cp, timestamp),
    Values = get_checkpoint_field(Cp, values),
    CheckpointType = get_metadata_field(Meta, checkpoint_type),
    Iteration = get_metadata_field(Meta, iteration),
    Superstep = get_metadata_field(Meta, step),
    ActiveVertices = get_metadata_field(Meta, active_vertices),
    CompletedVertices = get_metadata_field(Meta, completed_vertices),
    RunId = get_metadata_field(Meta, run_id),

    %% 计算消息数量
    Messages = maps:get(messages, Values, maps:get(<<"messages">>, Values, [])),
    MsgCount = length(Messages),

    io:format("[~p] Checkpoint: ~s~n", [Index, CpId]),
    io:format("    ID: ~s~n", [CpId]),
    io:format("    run_id: ~s~n", [format_run_id(RunId)]),
    io:format("    类型: ~p~n", [CheckpointType]),
    io:format("    迭代: ~p~n", [Iteration]),
    io:format("    超步: ~p~n", [Superstep]),
    io:format("    消息数: ~p~n", [MsgCount]),
    io:format("    活跃节点: ~ts~n", [format_vertices(ActiveVertices)]),
    io:format("    完成节点: ~ts~n", [format_vertices(CompletedVertices)]),
    io:format("    时间戳: ~p (~s)~n", [Timestamp, format_timestamp(Timestamp)]),
    io:format("~n").

%% @private 打印 checkpoint 完整数据
%% Cp: checkpoint record, Meta: checkpoint_metadata record
print_checkpoint_data(Cp, Meta) ->
    %% 从 checkpoint 获取 values (global_state)
    Values = get_checkpoint_field(Cp, values),

    %% 从 metadata 获取执行上下文信息
    io:format("~n执行上下文:~n"),
    CheckpointType = get_metadata_field(Meta, checkpoint_type),
    RunId = get_metadata_field(Meta, run_id),
    Iteration = get_metadata_field(Meta, iteration),
    Superstep = get_metadata_field(Meta, step),
    ActiveVertices = get_metadata_field(Meta, active_vertices),
    CompletedVertices = get_metadata_field(Meta, completed_vertices),

    io:format("  checkpoint_type: ~p~n", [CheckpointType]),
    io:format("  run_id: ~s~n", [format_run_id(RunId)]),
    io:format("  iteration: ~p~n", [Iteration]),
    io:format("  superstep: ~p~n", [Superstep]),
    io:format("  active_vertices: ~p~n", [ActiveVertices]),
    io:format("  completed_vertices: ~p~n", [CompletedVertices]),

    %% 从 values (global_state) 获取消息历史
    Messages = get_data_value(Values, messages, []),
    io:format("~n消息历史 (~p 条):~n", [length(Messages)]),
    lists:foreach(fun(Msg) ->
        Role = maps:get(role, Msg, maps:get(<<"role">>, Msg, unknown)),
        Content = maps:get(content, Msg, maps:get(<<"content">>, Msg, <<>>)),
        %% 截断过长的内容
        DisplayContent = truncate_content(Content, 80),
        io:format("  [~p] ~ts~n", [Role, DisplayContent])
    end, Messages),

    %% 打印 scratchpad（如果有）
    Scratchpad = get_data_value(Values, scratchpad, []),
    case Scratchpad of
        [] -> ok;
        _ ->
            io:format("~nScratchpad (~p 条):~n", [length(Scratchpad)]),
            lists:foreach(fun(Item) ->
                io:format("  ~p~n", [Item])
            end, lists:sublist(Scratchpad, 3))  %% 只显示前 3 条
    end,

    %% 打印 context（如果有）
    Context = get_data_value(Values, context, #{}),
    case maps:size(Context) > 0 of
        true ->
            io:format("~nContext:~n"),
            maps:foreach(fun(K, V) ->
                io:format("  ~p: ~p~n", [K, truncate_value(V)])
            end, Context);
        false ->
            ok
    end,

    io:format("~n").

%% @private 从数据中获取值，支持原子和二进制键
get_data_value(Data, Key, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:get(BinKey, Data, undefined) of
        undefined -> maps:get(Key, Data, Default);
        Value -> Value
    end.

%% @private 格式化 run_id
format_run_id(undefined) -> "N/A";
format_run_id(RunId) when is_binary(RunId) -> RunId;
format_run_id(RunId) -> io_lib:format("~p", [RunId]).

%% @private 截断内容
truncate_content(Content, MaxLen) when is_binary(Content) ->
    case byte_size(Content) > MaxLen of
        true -> <<(binary:part(Content, 0, MaxLen))/binary, "..."/utf8>>;
        false -> Content
    end;
truncate_content(Content, _MaxLen) ->
    Content.

%% @private 截断值（用于打印）
truncate_value(V) when is_binary(V), byte_size(V) > 50 ->
    <<(binary:part(V, 0, 50))/binary, "...">>;
truncate_value(V) ->
    V.

%% @private 格式化时间戳
format_timestamp(Timestamp) when is_integer(Timestamp) ->
    %% 转换毫秒时间戳为可读格式
    Seconds = Timestamp div 1000,
    {{Y, M, D}, {H, Min, S}} = calendar:gregorian_seconds_to_datetime(
        Seconds + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    ),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                  [Y, M, D, H, Min, S]);
format_timestamp(_) ->
    "unknown".

%% @private 格式化节点列表
format_vertices([]) ->
    <<"(无)"/utf8>>;
format_vertices(Vertices) when is_list(Vertices) ->
    %% 将节点列表格式化为可读字符串
    VertexNames = lists:map(fun
        (V) when is_atom(V) -> atom_to_list(V);
        (V) when is_binary(V) -> binary_to_list(V);
        (V) -> io_lib:format("~p", [V])
    end, Vertices),
    unicode:characters_to_binary(string:join(VertexNames, ", "));
format_vertices(Other) ->
    unicode:characters_to_binary(io_lib:format("~p", [Other])).
