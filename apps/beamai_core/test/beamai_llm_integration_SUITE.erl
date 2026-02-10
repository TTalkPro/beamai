%%%-------------------------------------------------------------------
%%% @doc LLM Integration Test Suite
%%%
%%% 使用真实 LLM Provider 验证重构后的 graph/process/snapshot 功能。
%%% 通过 ZHIPU_ANTHROPIC_BASE_URL + ZHIPU_API_KEY 环境变量调用
%%% Zhipu 的 Anthropic 兼容 API。
%%%
%%% 运行方式:
%%%   ZHIPU_ANTHROPIC_BASE_URL="https://open.bigmodel.cn/api/anthropic" \
%%%   ZHIPU_API_KEY="<key>" \
%%%   rebar3 ct --suite=beamai_llm_integration_SUITE --verbose
%%%
%%% 未设置环境变量时所有测试将被跳过。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Graph flow tests
-export([
    test_graph_single_llm_node/1,
    test_graph_two_node_chain/1,
    test_graph_conditional_routing/1,
    test_graph_interrupt_resume/1
]).

%% Process flow tests
-export([
    test_process_single_llm_step/1,
    test_process_two_step_chain/1,
    test_process_pause_resume/1
]).

%% Snapshot restore tests
-export([
    test_process_snapshot_save_load/1,
    test_process_snapshot_time_travel/1,
    test_graph_snapshot_save_load/1,
    test_graph_snapshot_interrupt_restore/1
]).

%%====================================================================
%% CT callbacks
%%====================================================================

all() ->
    [{group, graph_flow},
     {group, process_flow},
     {group, snapshot_restore}].

groups() ->
    [{graph_flow, [sequence], [
        test_graph_single_llm_node,
        test_graph_two_node_chain,
        test_graph_conditional_routing,
        test_graph_interrupt_resume
    ]},
     {process_flow, [sequence], [
        test_process_single_llm_step,
        test_process_two_step_chain,
        test_process_pause_resume
    ]},
     {snapshot_restore, [sequence], [
        test_process_snapshot_save_load,
        test_process_snapshot_time_travel,
        test_graph_snapshot_save_load,
        test_graph_snapshot_interrupt_restore
    ]}].

init_per_suite(Config) ->
    BaseUrl = os:getenv("ZHIPU_ANTHROPIC_BASE_URL"),
    ApiKey = os:getenv("ZHIPU_API_KEY"),
    case {BaseUrl, ApiKey} of
        {false, _} ->
            {skip, "ZHIPU_ANTHROPIC_BASE_URL not set"};
        {_, false} ->
            {skip, "ZHIPU_API_KEY not set"};
        {Url, Key} ->
            %% 启动依赖应用
            {ok, _} = application:ensure_all_started(hackney),
            {ok, _} = application:ensure_all_started(beamai_core),
            {ok, _} = application:ensure_all_started(beamai_llm),
            {ok, _} = application:ensure_all_started(beamai_memory),

            %% 创建 LLM 配置（通过 Zhipu Anthropic 兼容 API）
            LlmConfig = beamai_chat_completion:create(anthropic, #{
                api_key => list_to_binary(Key),
                base_url => list_to_binary(Url),
                model => <<"glm-4-flash-250414">>,
                max_tokens => 256
            }),

            %% 启动 process supervisor（可能已被 application 启动）
            SupPid = case beamai_process_sup:start_link() of
                {ok, Pid} -> Pid;
                {error, {already_started, Pid}} -> Pid
            end,

            [{llm_config, LlmConfig}, {sup_pid, SupPid} | Config]
    end.

end_per_suite(Config) ->
    case ?config(sup_pid, Config) of
        undefined -> ok;
        Pid ->
            unlink(Pid),
            exit(Pid, shutdown)
    end,
    ok.

init_per_group(snapshot_restore, Config) ->
    %% 启动 ETS 存储后端（unlink 防止 CT 进程退出时杀掉 store）
    StoreName = llm_test_store,
    {ok, StorePid} = beamai_store_ets:start_link(StoreName, #{}),
    unlink(StorePid),
    Store = {beamai_store_ets, StoreName},
    StateStore = beamai_state_store:new(Store),
    ProcessSnapshotMgr = beamai_process_snapshot:new(StateStore),
    GraphSnapshotMgr = beamai_graph_snapshot:new(StateStore),
    [{store_name, StoreName},
     {state_store, StateStore},
     {process_snapshot_mgr, ProcessSnapshotMgr},
     {graph_snapshot_mgr, GraphSnapshotMgr} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(snapshot_restore, Config) ->
    StoreName = ?config(store_name, Config),
    beamai_store_ets:stop(StoreName),
    ok;
end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, 120}),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Graph flow tests
%%====================================================================

%% 单个 LLM 节点 → __end__
test_graph_single_llm_node(Config) ->
    LlmConfig = ?config(llm_config, Config),
    NodeFn = make_llm_node_fun(LlmConfig, prompt, response),

    {ok, Graph} = beamai_graph:build([
        {node, llm_node, NodeFn},
        {edge, llm_node, '__end__'},
        {entry, llm_node}
    ]),

    InitCtx = beamai_graph:context(#{prompt => <<"What is 1+1? Reply only the number.">>}),
    {ok, FinalCtx} = beamai_graph:run_sync(Graph, InitCtx),

    Response = beamai_context:get(FinalCtx, response),
    ?assert(is_binary(Response)),
    ?assert(byte_size(Response) > 0),
    ct:pal("Single LLM node response: ~s", [Response]).

%% LLM 生成 → 预处理 → LLM 润色 → __end__
test_graph_two_node_chain(Config) ->
    LlmConfig = ?config(llm_config, Config),

    GenerateFn = make_llm_node_fun(LlmConfig, prompt, draft),

    PrepareFn = fun(State, _) ->
        Draft = beamai_context:get(State, draft, <<>>),
        RefinePrompt = <<"Please improve the following text, make it more concise:\n\n",
                         Draft/binary>>,
        {ok, beamai_context:set(State, refine_prompt, RefinePrompt)}
    end,

    RefineFn = make_llm_node_fun(LlmConfig, refine_prompt, refined),

    {ok, Graph} = beamai_graph:build([
        {node, generate, GenerateFn},
        {edge, generate, prepare},
        {node, prepare, PrepareFn},
        {edge, prepare, refine},
        {node, refine, RefineFn},
        {edge, refine, '__end__'},
        {entry, generate}
    ]),

    InitCtx = beamai_graph:context(#{
        prompt => <<"Write a short paragraph about Erlang programming language.">>
    }),
    {ok, FinalCtx} = beamai_graph:run_sync(Graph, InitCtx),

    Draft = beamai_context:get(FinalCtx, draft),
    Refined = beamai_context:get(FinalCtx, refined),
    ?assert(is_binary(Draft)),
    ?assert(is_binary(Refined)),
    ?assert(byte_size(Draft) > 0),
    ?assert(byte_size(Refined) > 0),
    ct:pal("Draft: ~s~nRefined: ~s", [Draft, Refined]).

%% LLM 分析 → 条件路由 → positive/negative → __end__
test_graph_conditional_routing(Config) ->
    LlmConfig = ?config(llm_config, Config),

    AnalyzeFn = make_llm_node_fun(LlmConfig, prompt, analysis),

    RouterFun = fun(State) ->
        Analysis = beamai_context:get(State, analysis, <<>>),
        %% 检查 LLM 回答中是否包含数字 4
        case binary:match(Analysis, <<"4">>) of
            nomatch -> negative_path;
            _ -> positive_path
        end
    end,

    PositiveFn = fun(State, _) ->
        {ok, beamai_context:set(State, route, <<"positive">>)}
    end,

    NegativeFn = fun(State, _) ->
        {ok, beamai_context:set(State, route, <<"negative">>)}
    end,

    {ok, Graph} = beamai_graph:build([
        {node, analyze, AnalyzeFn},
        {conditional_edge, analyze, RouterFun},
        {node, positive_path, PositiveFn},
        {node, negative_path, NegativeFn},
        {edge, positive_path, '__end__'},
        {edge, negative_path, '__end__'},
        {entry, analyze}
    ]),

    InitCtx = beamai_graph:context(#{
        prompt => <<"What is 2+2? Reply only the number.">>
    }),
    {ok, FinalCtx} = beamai_graph:run_sync(Graph, InitCtx),

    Route = beamai_context:get(FinalCtx, route),
    Analysis = beamai_context:get(FinalCtx, analysis),
    ?assert(is_binary(Route)),
    ?assert(Route =:= <<"positive">> orelse Route =:= <<"negative">>),
    ct:pal("Analysis: ~s, Route: ~s", [Analysis, Route]).

%% LLM 调用 → 中断审核 → 恢复 → __end__
test_graph_interrupt_resume(Config) ->
    LlmConfig = ?config(llm_config, Config),

    ProcessFn = make_llm_node_fun(LlmConfig, prompt, llm_result),

    ReviewFn = make_llm_interrupt_node_fun(LlmConfig),

    {ok, Graph} = beamai_graph:build([
        {node, process, ProcessFn},
        {edge, process, review},
        {node, review, ReviewFn},
        {edge, review, '__end__'},
        {entry, process}
    ]),

    InitCtx = beamai_graph:context(#{
        prompt => <<"Say hello in exactly 3 words.">>
    }),

    %% 第一次执行：应该中断
    {interrupted, InterruptedVertices, Snapshot} =
        beamai_graph:run_sync(Graph, InitCtx),

    ?assert(length(InterruptedVertices) >= 1),
    VertexIds = [Id || {Id, _Reason} <- InterruptedVertices],
    ?assert(lists:member(review, VertexIds)),
    ?assert(is_map(Snapshot)),

    ct:pal("Interrupted at vertices: ~p", [VertexIds]),

    %% 第二次执行：从快照恢复
    {ok, FinalCtx} = beamai_graph:run_sync(Graph, InitCtx, #{
        snapshot => Snapshot,
        resume_data => #{review => #{approved => true}}
    }),

    LlmResult = beamai_context:get(FinalCtx, llm_result),
    Reviewed = beamai_context:get(FinalCtx, reviewed),
    ?assert(is_binary(LlmResult)),
    ?assertEqual(true, Reviewed),
    ct:pal("LLM result: ~s, Reviewed: ~p", [LlmResult, Reviewed]).

%%====================================================================
%% Process flow tests
%%====================================================================

%% 单个 llm_call 步骤
test_process_single_llm_step(Config) ->
    LlmConfig = ?config(llm_config, Config),

    P0 = beamai_process:builder(single_llm),
    P1 = beamai_process:add_step(P0, llm_step, llm_test_step, #{
        type => llm_call,
        llm_config => LlmConfig,
        output_event => llm_done
    }),
    P2 = beamai_process:on_event(P1, start, llm_step, input),
    P3 = beamai_process:set_initial_event(P2, start, #{
        prompt => <<"What is the capital of France? Reply in one word.">>
    }),
    P4 = beamai_process:set_execution_mode(P3, sequential),
    {ok, Def} = beamai_process:build(P4),

    {ok, Result} = beamai_process:run_sync(Def, #{timeout => 60000}),

    #{llm_step := StepState} = Result,
    ?assertEqual(1, maps:get(activation_count, StepState)),
    InnerState = maps:get(state, StepState),
    Response = maps:get(response, InnerState),
    ?assert(is_binary(Response)),
    ?assert(byte_size(Response) > 0),
    ct:pal("Single step response: ~s", [Response]).

%% llm_call → llm_transform 链
test_process_two_step_chain(Config) ->
    LlmConfig = ?config(llm_config, Config),

    P0 = beamai_process:builder(two_step_chain),
    P1 = beamai_process:add_step(P0, generator, llm_test_step, #{
        type => llm_call,
        llm_config => LlmConfig,
        output_event => generated
    }),
    P2 = beamai_process:add_step(P1, transformer, llm_test_step, #{
        type => llm_transform,
        llm_config => LlmConfig,
        instruction => <<"Translate the following text to Chinese:">>,
        output_event => transformed
    }),
    P3 = beamai_process:on_event(P2, start, generator, input),
    P4 = beamai_process:on_event(P3, generated, transformer, input),
    P5 = beamai_process:set_initial_event(P4, start, #{
        prompt => <<"Write one sentence about the Erlang programming language.">>
    }),
    P6 = beamai_process:set_execution_mode(P5, sequential),
    {ok, Def} = beamai_process:build(P6),

    {ok, Result} = beamai_process:run_sync(Def, #{timeout => 60000}),

    #{generator := GenState, transformer := TransState} = Result,
    ?assertEqual(1, maps:get(activation_count, GenState)),
    ?assertEqual(1, maps:get(activation_count, TransState)),

    GenResponse = maps:get(response, maps:get(state, GenState)),
    TransResponse = maps:get(response, maps:get(state, TransState)),
    ?assert(is_binary(GenResponse)),
    ?assert(is_binary(TransResponse)),
    ?assert(byte_size(GenResponse) > 0),
    ?assert(byte_size(TransResponse) > 0),
    ct:pal("Generated: ~s~nTranslated: ~s", [GenResponse, TransResponse]).

%% llm_pause 步骤 — HITL
test_process_pause_resume(Config) ->
    LlmConfig = ?config(llm_config, Config),

    P0 = beamai_process:builder(pause_resume),
    P1 = beamai_process:add_step(P0, review_step, llm_test_step, #{
        type => llm_pause,
        llm_config => LlmConfig,
        output_event => approved
    }),
    P2 = beamai_process:on_event(P1, start, review_step, input),
    P3 = beamai_process:set_initial_event(P2, start, #{
        prompt => <<"Analyze this request: Should we deploy to production?">>
    }),
    P4 = beamai_process:set_execution_mode(P3, sequential),
    {ok, Def} = beamai_process:build(P4),

    %% 首次运行应暂停
    {paused, {awaiting_review, LlmResponse}, Snapshot} =
        beamai_process:run_sync(Def, #{timeout => 60000}),

    ?assert(is_binary(LlmResponse)),
    ?assert(byte_size(LlmResponse) > 0),
    ?assert(is_map(Snapshot)),
    ct:pal("Paused with LLM analysis: ~s", [LlmResponse]),

    %% 恢复执行
    {ok, Result} = beamai_process:run_sync(Def, #{
        snapshot => Snapshot,
        resume_data => #{review_step => #{approved => true, reviewer => <<"test_user">>}}
    }),

    #{review_step := StepState} = Result,
    ?assert(maps:get(activation_count, StepState) >= 1),
    InnerState = maps:get(state, StepState),
    ?assertEqual(true, maps:get(resumed, InnerState)),
    ct:pal("Resumed successfully, approval: ~p", [maps:get(approval, InnerState)]).

%%====================================================================
%% Snapshot restore tests
%%====================================================================

%% 运行 process → 保存快照 → 加载 → 验证数据往返
test_process_snapshot_save_load(Config) ->
    LlmConfig = ?config(llm_config, Config),
    Mgr = ?config(process_snapshot_mgr, Config),

    %% 运行一个 process 获取结果
    P0 = beamai_process:builder(snap_save_load),
    P1 = beamai_process:add_step(P0, llm_step, llm_test_step, #{
        type => llm_call,
        llm_config => LlmConfig,
        output_event => done
    }),
    P2 = beamai_process:on_event(P1, start, llm_step, input),
    P3 = beamai_process:set_initial_event(P2, start, #{
        prompt => <<"Say hello.">>
    }),
    P4 = beamai_process:set_execution_mode(P3, sequential),
    {ok, Def} = beamai_process:build(P4),
    {ok, Result} = beamai_process:run_sync(Def, #{timeout => 60000}),

    %% 构造 process state map 用于快照
    ThreadId = <<"test-process-snap-", (integer_to_binary(erlang:system_time(microsecond)))/binary>>,
    ProcessState = #{
        process_spec => snap_save_load,
        fsm_state => completed,
        steps_state => Result,
        event_queue => [],
        paused_step => undefined,
        pause_reason => undefined
    },

    %% 保存快照
    {ok, SavedSn, Mgr1} = beamai_process_snapshot:save_from_state(
        Mgr, ThreadId, ProcessState, #{snapshot_type => completed}),

    SnapshotId = beamai_process_snapshot:get_id(SavedSn),
    ?assert(is_binary(SnapshotId)),

    %% 加载快照
    {ok, LoadedSn} = beamai_process_snapshot:load(Mgr1, SnapshotId),

    %% 验证关键字段
    ?assertEqual(SnapshotId, beamai_process_snapshot:get_id(LoadedSn)),
    ?assertEqual(false, beamai_process_snapshot:is_paused(LoadedSn)),

    StepsState = beamai_process_snapshot:get_steps_state(LoadedSn),
    ?assert(is_map(StepsState)),

    ct:pal("Snapshot saved and loaded: ~s", [SnapshotId]).

%% 保存 2 个快照 → go_back → 验证较早状态
test_process_snapshot_time_travel(Config) ->
    Mgr = ?config(process_snapshot_mgr, Config),

    ThreadId = <<"test-time-travel-", (integer_to_binary(erlang:system_time(microsecond)))/binary>>,

    %% 第一次 LLM 调用
    {ok, Response1} = call_llm(Config, <<"Say 'first'.">>),
    ?assert(is_binary(Response1)),

    State1 = #{
        process_spec => time_travel_test,
        fsm_state => running,
        steps_state => #{llm_step => #{
            state => #{response => Response1},
            collected_inputs => #{},
            activation_count => 1
        }},
        event_queue => [],
        paused_step => undefined,
        pause_reason => undefined
    },
    {ok, Sn1, Mgr1} = beamai_process_snapshot:save_from_state(
        Mgr, ThreadId, State1, #{snapshot_type => step_completed}),

    %% 第二次 LLM 调用
    {ok, Response2} = call_llm(Config, <<"Say 'second'.">>),
    ?assert(is_binary(Response2)),

    State2 = #{
        process_spec => time_travel_test,
        fsm_state => completed,
        steps_state => #{llm_step => #{
            state => #{response => Response2},
            collected_inputs => #{},
            activation_count => 2
        }},
        event_queue => [],
        paused_step => undefined,
        pause_reason => undefined
    },
    {ok, _Sn2, Mgr2} = beamai_process_snapshot:save_from_state(
        Mgr1, ThreadId, State2, #{snapshot_type => completed}),

    %% 回退 1 步
    {ok, BackSn, _Mgr3} = beamai_process_snapshot:go_back(Mgr2, ThreadId, 1),

    %% 验证回到了第一个快照的版本
    BackVersion = beamai_process_snapshot:entry_version(BackSn),
    Sn1Version = beamai_process_snapshot:entry_version(Sn1),
    ?assertEqual(Sn1Version, BackVersion),

    ct:pal("Time travel: went back from v~p to v~p",
           [beamai_process_snapshot:entry_version(_Sn2), BackVersion]).

%% 运行 graph → 保存快照 → 加载 → 验证
test_graph_snapshot_save_load(Config) ->
    LlmConfig = ?config(llm_config, Config),
    Mgr = ?config(graph_snapshot_mgr, Config),

    %% 运行一个简单的 graph
    NodeFn = make_llm_node_fun(LlmConfig, prompt, response),
    {ok, Graph} = beamai_graph:build([
        {node, llm_node, NodeFn},
        {edge, llm_node, '__end__'},
        {entry, llm_node}
    ]),

    InitCtx = beamai_graph:context(#{prompt => <<"Say hello.">>}),
    {ok, FinalCtx} = beamai_graph:run_sync(Graph, InitCtx),

    Response = beamai_context:get(FinalCtx, response),
    ?assert(is_binary(Response)),

    %% 构造 Pregel state 用于快照
    RunId = <<"test-graph-snap-", (integer_to_binary(erlang:system_time(microsecond)))/binary>>,
    PregelState = #{
        superstep => 1,
        iteration => 0,
        vertices => #{llm_node => #{
            value => beamai_context:get(FinalCtx, response),
            active => false,
            messages => [],
            halt_voted => false
        }},
        pending_activations => [],
        global_state => #{response => Response},
        active_vertices => [],
        completed_vertices => [llm_node],
        failed_vertices => [],
        interrupted_vertices => []
    },

    %% 保存快照
    {ok, SavedSn, Mgr1} = beamai_graph_snapshot:save_from_pregel(
        Mgr, RunId, PregelState, #{snapshot_type => final}),

    %% 加载快照
    SnId = beamai_graph_snapshot:entry_id(SavedSn),
    ?assert(is_binary(SnId)),
    {ok, LoadedSn} = beamai_graph_snapshot:load(Mgr1, SnId),

    %% 验证字段
    ?assertEqual(SnId, beamai_graph_snapshot:entry_id(LoadedSn)),
    ?assertEqual(1, beamai_graph_snapshot:get_superstep(LoadedSn)),

    Vertices = beamai_graph_snapshot:get_vertices(LoadedSn),
    ?assert(maps:is_key(llm_node, Vertices)),
    ?assertEqual([llm_node], maps:keys(Vertices)),

    ct:pal("Graph snapshot saved and loaded: ~s", [SnId]).

%% Graph 中断 → 快照 → 从快照恢复
test_graph_snapshot_interrupt_restore(Config) ->
    LlmConfig = ?config(llm_config, Config),
    Mgr = ?config(graph_snapshot_mgr, Config),

    ProcessFn = make_llm_node_fun(LlmConfig, prompt, llm_result),
    ReviewFn = make_llm_interrupt_node_fun(LlmConfig),

    {ok, Graph} = beamai_graph:build([
        {node, process, ProcessFn},
        {edge, process, review},
        {node, review, ReviewFn},
        {edge, review, '__end__'},
        {entry, process}
    ]),

    InitCtx = beamai_graph:context(#{prompt => <<"Count from 1 to 5.">>}),

    %% 第一次执行：中断
    {interrupted, InterruptedVertices, GraphSnapshot} =
        beamai_graph:run_sync(Graph, InitCtx),

    ?assert(length(InterruptedVertices) >= 1),

    %% 保存中断快照到存储
    RunId = <<"test-graph-interrupt-", (integer_to_binary(erlang:system_time(microsecond)))/binary>>,
    InterruptedState = maps:get('__graph_state__', GraphSnapshot, #{}),
    PregelState = #{
        superstep => maps:get(superstep, InterruptedState, 1),
        vertices => maps:get(vertices, InterruptedState, #{}),
        global_state => maps:get(global_state, InterruptedState, #{}),
        interrupted_vertices => [Id || {Id, _} <- InterruptedVertices],
        pending_activations => [],
        active_vertices => [],
        completed_vertices => [],
        failed_vertices => []
    },
    {ok, SavedSn, Mgr1} = beamai_graph_snapshot:save_from_pregel(
        Mgr, RunId, PregelState, #{snapshot_type => interrupted, resumable => true}),

    %% 验证快照已保存
    SnId = beamai_graph_snapshot:entry_id(SavedSn),
    {ok, _LoadedSn} = beamai_graph_snapshot:load(Mgr1, SnId),
    ?assert(beamai_graph_snapshot:is_resumable(SavedSn)),

    %% 从 run_sync 快照恢复（使用原始 snapshot，非存储的）
    {ok, FinalCtx} = beamai_graph:run_sync(Graph, InitCtx, #{
        snapshot => GraphSnapshot,
        resume_data => #{review => #{approved => true}}
    }),

    LlmResult = beamai_context:get(FinalCtx, llm_result),
    Reviewed = beamai_context:get(FinalCtx, reviewed),
    ?assert(is_binary(LlmResult)),
    ?assertEqual(true, Reviewed),

    ct:pal("Graph interrupt→snapshot→restore: snapshot=~s, result=~s",
           [SnId, LlmResult]).

%%====================================================================
%% Helper functions
%%====================================================================

%% @doc 创建 2-arity 的 LLM 图节点函数
%%
%% 从 context 中读取 PromptKey 的值作为 prompt 发送给 LLM，
%% 将 LLM 响应写入 context 的 ResponseKey。
make_llm_node_fun(LlmConfig, PromptKey, ResponseKey) ->
    fun(State, _Context) ->
        Prompt = beamai_context:get(State, PromptKey, <<"Hello">>),
        case call_llm_raw(LlmConfig, Prompt) of
            {ok, Response} ->
                {ok, beamai_context:set(State, ResponseKey, Response)};
            {error, Reason} ->
                error({llm_call_failed, Reason})
        end
    end.

%% @doc 创建 3-arity 的 LLM 中断节点函数
%%
%% 首次调用（ResumeData=undefined）：中断等待审核
%% 恢复调用（ResumeData!=undefined）：标记已审核
make_llm_interrupt_node_fun(_LlmConfig) ->
    fun
        (State, _Input, undefined) ->
            {interrupt, need_review, State};
        (State, _Input, _ResumeData) ->
            {ok, beamai_context:set(State, reviewed, true)}
    end.

%% @doc 便捷 LLM 调用 — 返回 binary 响应
call_llm(Config, Prompt) ->
    LlmConfig = ?config(llm_config, Config),
    call_llm_raw(LlmConfig, Prompt).

%% @doc 原始 LLM 调用
call_llm_raw(LlmConfig, Prompt) ->
    Messages = [#{role => user,
                  content => Prompt}],
    case beamai_chat_completion:chat(LlmConfig, Messages) of
        {ok, #{content := Content}} when is_binary(Content) ->
            {ok, Content};
        {ok, #{content := null}} ->
            {ok, <<>>};
        {error, Reason} ->
            {error, Reason}
    end.
