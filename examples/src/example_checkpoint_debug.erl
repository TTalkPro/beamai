%%%-------------------------------------------------------------------
%%% @doc Checkpoint 调试模块
%%%
%%% 用于调试 checkpoint 保存流程，找出 LLM 消息丢失的原因
%%% @end
%%%-------------------------------------------------------------------
-module(example_checkpoint_debug).

-export([run/0, run/1]).

%%====================================================================
%% 调试入口
%%====================================================================

run() ->
    case example_utils:get_llm_config() of
        {ok, LLMConfig} ->
            run(LLMConfig);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason]),
            {error, Reason}
    end.

run(LLMConfig) ->
    io:format("=== Checkpoint 调试 ===~n~n"),

    %% 1. 创建存储后端
    StoreName = checkpoint_debug_store,
    {ok, _} = beamai_store_ets:start_link(StoreName, #{max_items => 1000}),

    %% 2. 创建 Memory 实例
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"debug_thread">>
    }),
    ThreadId = beamai_memory:get_thread_id(Memory),
    io:format("thread_id: ~s~n", [ThreadId]),

    %% 3. 创建带调试回调的 Agent
    %% 我们需要自定义 checkpoint 回调来查看实际保存的数据
    DebugCallback = create_debug_callback(Memory, ThreadId),

    {ok, Agent} = beamai_agent:start_link(<<"checkpoint_debug_agent">>, #{
        system_prompt => <<"你是一个有帮助的助手。请简短回答。"/utf8>>,
        llm => LLMConfig,
        storage => Memory
    }),

    %% 4. 运行一次对话
    io:format("~n--- 运行对话 ---~n"),
    Msg = <<"你好，我叫 Alice"/utf8>>,
    io:format("发送消息: ~ts~n", [Msg]),

    case beamai_agent:run(Agent, Msg) of
        {ok, Result} ->
            io:format("~n--- Agent 返回结果 ---~n"),
            io:format("status: ~p~n", [maps:get(status, Result, undefined)]),
            io:format("final_response: ~ts~n", [maps:get(final_response, Result, <<>>)]),

            %% 检查返回的 messages
            Messages = maps:get(messages, Result, []),
            io:format("返回的 messages 数量: ~p~n", [length(Messages)]),
            lists:foreach(fun(M) ->
                Role = maps:get(role, M, maps:get(<<"role">>, M, unknown)),
                Content = maps:get(content, M, maps:get(<<"content">>, M, <<>>)),
                io:format("  [~p] ~ts~n", [Role, truncate(Content, 80)])
            end, Messages);
        {error, Reason} ->
            io:format("Agent 运行失败: ~p~n", [Reason])
    end,

    %% 5. 检查保存的 checkpoints
    io:format("~n--- 检查保存的 Checkpoints ---~n"),
    case beamai_memory:list_checkpoints(Memory, #{thread_id => ThreadId}) of
        {ok, Checkpoints} ->
            io:format("共 ~p 个 checkpoints~n~n", [length(Checkpoints)]),

            %% 按 timestamp 排序
            Sorted = lists:sort(
                fun({CpA, _, _}, {CpB, _, _}) ->
                    element(6, CpA) =< element(6, CpB)
                end,
                Checkpoints
            ),

            lists:foreach(fun({Cp, Meta, _}) ->
                CpId = element(2, Cp),
                Values = element(5, Cp),
                Timestamp = element(6, Cp),
                CheckpointType = element(2, Meta),
                Superstep = element(3, Meta),

                io:format("Checkpoint: ~s~n", [CpId]),
                io:format("  type: ~p, superstep: ~p, timestamp: ~p~n",
                         [CheckpointType, Superstep, Timestamp]),

                %% 检查 messages
                Msgs = get_value(Values, messages, []),
                io:format("  messages (~p 条):~n", [length(Msgs)]),
                lists:foreach(fun(M) ->
                    Role = maps:get(role, M, maps:get(<<"role">>, M, unknown)),
                    Content = maps:get(content, M, maps:get(<<"content">>, M, <<>>)),
                    io:format("    [~p] ~ts~n", [Role, truncate(Content, 60)])
                end, Msgs),
                io:format("~n")
            end, Sorted);
        {error, Err} ->
            io:format("列出 checkpoints 失败: ~p~n", [Err])
    end,

    %% 清理
    beamai_agent:stop(Agent),
    beamai_store_ets:stop(StoreName),
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

create_debug_callback(Memory, ThreadId) ->
    fun(Info, CheckpointData) ->
        Type = maps:get(type, CheckpointData, maps:get(type, Info, step)),
        Superstep = maps:get(superstep, CheckpointData, 0),

        %% 提取 global_state
        PregelCheckpoint = maps:get(pregel_checkpoint, CheckpointData, #{}),
        GlobalState = maps:get(global_state, PregelCheckpoint, #{}),

        %% 也从 CheckpointData 直接获取 global_state 对比
        DirectGlobalState = maps:get(global_state, CheckpointData, #{}),

        io:format("~n[DEBUG CALLBACK] type=~p, superstep=~p~n", [Type, Superstep]),

        %% 检查两个 global_state 是否一致
        PregelMsgs = get_state_value(GlobalState, messages, []),
        DirectMsgs = get_state_value(DirectGlobalState, messages, []),

        io:format("  pregel_checkpoint.global_state.messages: ~p 条~n", [length(PregelMsgs)]),
        io:format("  checkpoint_data.global_state.messages: ~p 条~n", [length(DirectMsgs)]),

        %% 打印消息内容
        io:format("  Direct messages:~n"),
        lists:foreach(fun(M) ->
            Role = maps:get(role, M, maps:get(<<"role">>, M, unknown)),
            io:format("    [~p]~n", [Role])
        end, DirectMsgs),

        continue
    end.

get_value(Map, Key, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:get(BinKey, Map, undefined) of
        undefined -> maps:get(Key, Map, Default);
        V -> V
    end.

get_state_value(State, Key, Default) ->
    %% graph_state 使用 binary 键
    BinKey = atom_to_binary(Key, utf8),
    case maps:get(BinKey, State, undefined) of
        undefined -> maps:get(Key, State, Default);
        V -> V
    end.

truncate(Content, MaxLen) when is_binary(Content) ->
    case byte_size(Content) > MaxLen of
        true -> <<(binary:part(Content, 0, MaxLen))/binary, "..."/utf8>>;
        false -> Content
    end;
truncate(null, _) -> <<"(null)">>;
truncate(Other, _) -> io_lib:format("~p", [Other]).
