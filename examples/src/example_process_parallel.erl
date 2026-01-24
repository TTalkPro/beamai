%%%-------------------------------------------------------------------
%%% @doc Process Framework 并行执行和事件类型示例
%%%
%%% 演示：
%%%   1. 顺序执行 vs 并行执行对比
%%%   2. Fan-out: 一个事件触发多个步骤
%%%   3. Fan-in: 多个步骤结果汇聚
%%%   4. 不同类型的输入事件
%%%   5. 事件变换 (Transform)
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_process_parallel:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_process_parallel).

-export([run/0, run/1]).
-export([run_sequential/0, run_sequential/1]).
-export([run_events/0, run_events/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 运行并行执行示例（Fan-out + Fan-in）
-spec run() -> ok.
run() ->
    run(example_llm_config:anthropic()).

-spec run(beamai_chat_completion:config()) -> ok.
run(LLMConfig) ->
    io:format("~n=== Process Framework: 并行执行示例 ===~n~n"),
    io:format("架构: 用户问题 --fan-out--> [分析师A, 分析师B, 分析师C] --fan-in--> 汇总~n~n"),

    ensure_started(),

    Kernel = beamai:add_llm(beamai:kernel(), LLMConfig),
    Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

    P0 = beamai_process:builder(parallel_analysis),

    %% 三个并行分析步骤（不同角度）
    P1 = beamai_process:add_step(P0, analyst_tech, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"你是技术分析师。从技术可行性角度分析问题，用一句话回答。"/utf8>>,
        output_event => tech_done
    }),
    P2 = beamai_process:add_step(P1, analyst_biz, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"你是商业分析师。从商业价值角度分析问题，用一句话回答。"/utf8>>,
        output_event => biz_done
    }),
    P3 = beamai_process:add_step(P2, analyst_risk, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"你是风险分析师。从风险管控角度分析问题，用一句话回答。"/utf8>>,
        output_event => risk_done
    }),

    %% 汇总步骤（等待所有分析完成）
    P4 = beamai_process:add_step(P3, summarizer, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"你是决策顾问。综合技术、商业、风险三个角度的分析，给出一句话总结建议。"/utf8>>,
        output_event => final_result,
        required_inputs => [tech_in, biz_in, risk_in]
    }),

    %% Fan-out: 同一事件触发三个分析师
    P5 = beamai_process:on_event(P4, question, analyst_tech, input),
    P6 = beamai_process:on_event(P5, question, analyst_biz, input),
    P7 = beamai_process:on_event(P6, question, analyst_risk, input),

    %% Fan-in: 三个分析结果汇入汇总步骤
    P8 = beamai_process:on_event(P7, tech_done, summarizer, tech_in,
        fun(#{response := R}) -> #{user_message => <<"技术分析: ", R/binary>>} end),
    P9 = beamai_process:on_event(P8, biz_done, summarizer, biz_in,
        fun(#{response := R}) -> #{user_message => <<"商业分析: ", R/binary>>} end),
    P10 = beamai_process:on_event(P9, risk_done, summarizer, risk_in,
        fun(#{response := R}) -> #{user_message => <<"风险分析: ", R/binary>>} end),

    %% 初始事件
    Question = <<"用Erlang开发一个高并发即时通讯系统"/utf8>>,
    io:format("问题: ~ts~n~n", [Question]),
    P11 = beamai_process:set_initial_event(P10, question, #{user_message => Question}),

    %% 使用并行执行模式（fan-out 的三个步骤将并行执行）
    %% 注意: 并行模式需要 poolboy 已启动
    P12 = beamai_process:set_execution_mode(P11, concurrent),

    {ok, Def} = beamai_process:build(P12),
    io:format("开始并行执行...~n~n"),
    T0 = erlang:system_time(millisecond),
    case beamai_process:run_sync(Def, #{timeout => 120000, context => Context}) of
        {ok, _Result} ->
            T1 = erlang:system_time(millisecond),
            io:format("~n并行执行完成! 耗时: ~pms~n~n", [T1 - T0]);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,
    ok.

%% @doc 运行顺序执行示例（对比）
-spec run_sequential() -> ok.
run_sequential() ->
    run_sequential(example_llm_config:anthropic()).

-spec run_sequential(beamai_chat_completion:config()) -> ok.
run_sequential(LLMConfig) ->
    io:format("~n=== Process Framework: 顺序执行示例 ===~n~n"),
    io:format("架构: 相同的 fan-out/fan-in 结构，但使用顺序执行模式~n~n"),

    ensure_started(),

    Kernel = beamai:add_llm(beamai:kernel(), LLMConfig),
    Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

    P0 = beamai_process:builder(sequential_analysis),

    P1 = beamai_process:add_step(P0, analyst_tech, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"你是技术分析师。从技术可行性角度分析，用一句话回答。"/utf8>>,
        output_event => tech_done
    }),
    P2 = beamai_process:add_step(P1, analyst_biz, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"你是商业分析师。从商业价值角度分析，用一句话回答。"/utf8>>,
        output_event => biz_done
    }),

    %% 汇总（Fan-in）
    P3 = beamai_process:add_step(P2, summarizer, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"综合技术和商业两个角度，给出一句话建议。"/utf8>>,
        output_event => done,
        required_inputs => [tech_in, biz_in]
    }),

    P4 = beamai_process:on_event(P3, question, analyst_tech, input),
    P5 = beamai_process:on_event(P4, question, analyst_biz, input),
    P6 = beamai_process:on_event(P5, tech_done, summarizer, tech_in,
        fun(#{response := R}) -> #{user_message => <<"技术: ", R/binary>>} end),
    P7 = beamai_process:on_event(P6, biz_done, summarizer, biz_in,
        fun(#{response := R}) -> #{user_message => <<"商业: ", R/binary>>} end),

    Question = <<"用Erlang开发一个高并发即时通讯系统"/utf8>>,
    io:format("问题: ~ts~n~n", [Question]),
    P8 = beamai_process:set_initial_event(P7, question, #{user_message => Question}),
    P9 = beamai_process:set_execution_mode(P8, sequential),

    {ok, Def} = beamai_process:build(P9),
    io:format("开始顺序执行...~n~n"),
    T0 = erlang:system_time(millisecond),
    case beamai_process:run_sync(Def, #{timeout => 120000, context => Context}) of
        {ok, _Result} ->
            T1 = erlang:system_time(millisecond),
            io:format("~n顺序执行完成! 耗时: ~pms~n~n", [T1 - T0]);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,
    ok.

%% @doc 运行事件类型示例
%%
%% 演示不同类型的输入事件及其路由：
%% - 初始事件 (initial_event)
%% - 外部事件 (send_event)
%% - 变换事件 (transform binding)
%% - 多输入事件 (fan-in)
-spec run_events() -> ok.
run_events() ->
    run_events(example_llm_config:anthropic()).

-spec run_events(beamai_chat_completion:config()) -> ok.
run_events(LLMConfig) ->
    io:format("~n=== Process Framework: 事件类型示例 ===~n~n"),

    ensure_started(),

    Kernel = beamai:add_llm(beamai:kernel(), LLMConfig),
    Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

    %% 构建包含多种事件模式的 Process
    P0 = beamai_process:builder(event_types_demo),

    %% Step 1: 接收初始事件
    P1 = beamai_process:add_step(P0, receiver, example_process_steps, #{
        type => log,
        label => <<"INIT-EVENT">>,
        output_event => received
    }),

    %% Step 2: 变换步骤（演示 transform binding）
    P2 = beamai_process:add_step(P1, transformer, example_process_steps, #{
        type => transform,
        transform_fn => fun(#{user_message := Msg}) ->
            #{user_message => <<"[增强] ", Msg/binary>>}
        end,
        output_event => enhanced
    }),

    %% Step 3: LLM 处理
    P3 = beamai_process:add_step(P2, processor, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"用一句话回答问题。"/utf8>>,
        output_event => processed
    }),

    %% Step 4: 等待外部事件的步骤（需要两个输入）
    P4 = beamai_process:add_step(P3, combiner, example_process_steps, #{
        type => merge,
        output_event => combined,
        required_inputs => [llm_result, external_data]
    }),

    %% Step 5: 最终格式化
    P5 = beamai_process:add_step(P4, formatter, example_process_steps, #{
        type => transform,
        transform_fn => fun(Inputs) ->
            LlmResult = maps:get(llm_result, Inputs, #{}),
            ExtData = maps:get(external_data, Inputs, #{}),
            io:format("  最终合并:~n"),
            io:format("    LLM结果: ~p~n", [maps:get(response, LlmResult, <<"N/A">>)]),
            io:format("    外部数据: ~p~n", [ExtData]),
            #{final => true, llm => LlmResult, external => ExtData}
        end,
        output_event => final
    }),

    %% 绑定
    P6 = beamai_process:on_event(P5, start, receiver, input),
    P7 = beamai_process:on_event(P6, received, transformer, input),
    P8 = beamai_process:on_event(P7, enhanced, processor, input),
    %% Transform binding: 提取 LLM 响应
    P9 = beamai_process:on_event(P8, processed, combiner, llm_result),
    %% 外部事件绑定
    P10 = beamai_process:on_event(P9, external_input, combiner, external_data),
    P11 = beamai_process:on_event(P10, combined, formatter, input),

    %% 初始事件
    P12 = beamai_process:set_initial_event(P11, start,
        #{user_message => <<"什么是gen_server？"/utf8>>}),
    P13 = beamai_process:set_execution_mode(P12, sequential),

    {ok, Def} = beamai_process:build(P13),

    %% 启动 process（不等待完成，因为需要发送外部事件）
    io:format("启动 Process...~n"),
    io:format("  初始事件: start~n"),
    {ok, Pid} = beamai_process:start(Def, #{context => Context}),

    %% 等待 LLM 处理完成，然后发送外部事件
    timer:sleep(15000),
    io:format("~n发送外部事件: external_input~n"),
    ExternalEvent = beamai_process_event:new(external_input,
        #{source => <<"database">>, extra_info => <<"supplementary data">>}),
    beamai_process:send_event(Pid, ExternalEvent),

    %% 等待完成
    timer:sleep(5000),
    {ok, Status} = beamai_process:get_status(Pid),
    io:format("~n最终状态: ~p~n~n", [maps:get(state, Status)]),
    beamai_process:stop(Pid),
    ok.

%%====================================================================
%% Internal
%%====================================================================

ensure_started() ->
    case whereis(beamai_core_sup) of
        undefined ->
            {ok, _} = beamai_core_sup:start_link(),
            ok;
        _ ->
            ok
    end.
