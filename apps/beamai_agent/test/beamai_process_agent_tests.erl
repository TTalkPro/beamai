-module(beamai_process_agent_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% beamai_agent_utils tests
%%====================================================================

extract_content_binary_test() ->
    ?assertEqual(<<"hello">>, beamai_agent_utils:extract_content(#{content => <<"hello">>})).

extract_content_null_test() ->
    ?assertEqual(<<>>, beamai_agent_utils:extract_content(#{content => null})).

extract_content_missing_test() ->
    ?assertEqual(<<>>, beamai_agent_utils:extract_content(#{})).

build_chat_opts_no_tools_test() ->
    Kernel = beamai_kernel:new(),
    ?assertEqual(#{}, beamai_agent_utils:build_chat_opts(Kernel, #{})).

build_chat_opts_with_tools_test() ->
    K0 = beamai_kernel:new(),
    Func = beamai_function:new(<<"test_func">>, fun(_) -> {ok, <<"ok">>} end,
        #{description => <<"desc">>, parameters => #{}}),
    K1 = beamai_kernel:add_plugin(K0, <<"test_plugin">>, [Func]),
    Opts = beamai_agent_utils:build_chat_opts(K1, #{}),
    ?assert(maps:is_key(tools, Opts)),
    ?assertEqual(auto, maps:get(tool_choice, Opts)).

build_chat_opts_custom_chat_opts_test() ->
    Kernel = beamai_kernel:new(),
    Opts = beamai_agent_utils:build_chat_opts(Kernel, #{chat_opts => #{temperature => 0.5}}),
    ?assertEqual(#{temperature => 0.5}, Opts).

%%====================================================================
%% beamai_process_agent_llm_step tests
%%====================================================================

llm_step_init_defaults_test() ->
    {ok, State} = beamai_process_agent_llm_step:init(#{}),
    ?assertEqual(undefined, maps:get(system_prompt, State)),
    ?assertEqual(10, maps:get(max_iterations, State)),
    ?assertEqual(agent_done, maps:get(output_event, State)),
    ?assertEqual([], maps:get(messages, State)),
    ?assertEqual(0, maps:get(turn_count, State)).

llm_step_init_custom_config_test() ->
    Config = #{
        system_prompt => <<"test prompt">>,
        max_tool_iterations => 5,
        output_event => my_done
    },
    {ok, State} = beamai_process_agent_llm_step:init(Config),
    ?assertEqual(<<"test prompt">>, maps:get(system_prompt, State)),
    ?assertEqual(5, maps:get(max_iterations, State)),
    ?assertEqual(my_done, maps:get(output_event, State)).

llm_step_can_activate_user_message_test() ->
    ?assertEqual(true, beamai_process_agent_llm_step:can_activate(
        #{user_message => <<"hi">>}, #{})).

llm_step_can_activate_tool_results_test() ->
    ?assertEqual(true, beamai_process_agent_llm_step:can_activate(
        #{tool_results => []}, #{})).

llm_step_can_activate_rejects_empty_test() ->
    ?assertEqual(false, beamai_process_agent_llm_step:can_activate(
        #{other => <<"data">>}, #{})).

llm_step_no_kernel_error_test() ->
    {ok, State} = beamai_process_agent_llm_step:init(#{}),
    Inputs = #{user_message => <<"hello">>},
    Context = beamai_context:new(),
    ?assertMatch({error, no_kernel_in_context},
        beamai_process_agent_llm_step:on_activate(Inputs, State, Context)).

llm_step_max_iterations_error_test() ->
    {ok, State0} = beamai_process_agent_llm_step:init(#{max_tool_iterations => 2}),
    %% 模拟已达到最大迭代
    State = State0#{iteration => 2},
    Inputs = #{tool_results => []},
    Context = beamai_context:new(),
    ?assertMatch({error, {max_tool_iterations, _}},
        beamai_process_agent_llm_step:on_activate(Inputs, State, Context)).

%%====================================================================
%% beamai_process_agent_tool_step tests
%%====================================================================

tool_step_init_defaults_test() ->
    {ok, State} = beamai_process_agent_tool_step:init(#{}),
    ?assertEqual(undefined, maps:get(on_tool_call, State)).

tool_step_init_with_hook_test() ->
    Hook = fun(_Name, _Args) -> ok end,
    {ok, State} = beamai_process_agent_tool_step:init(#{on_tool_call => Hook}),
    ?assertEqual(Hook, maps:get(on_tool_call, State)).

tool_step_can_activate_test() ->
    ?assertEqual(true, beamai_process_agent_tool_step:can_activate(
        #{tool_request => #{tool_calls => []}}, #{})),
    ?assertEqual(false, beamai_process_agent_tool_step:can_activate(
        #{other => data}, #{})).

tool_step_no_kernel_error_test() ->
    {ok, State} = beamai_process_agent_tool_step:init(#{}),
    Inputs = #{tool_request => #{tool_calls => []}},
    Context = beamai_context:new(),
    ?assertMatch({error, no_kernel_in_context},
        beamai_process_agent_tool_step:on_activate(Inputs, State, Context)).

tool_step_executes_tools_test() ->
    %% 创建带 tool 的 kernel
    K0 = beamai_kernel:new(),
    Func = beamai_function:new(<<"echo">>,
        fun(#{<<"msg">> := Msg}) -> {ok, Msg} end,
        #{description => <<"echo input">>,
          parameters => #{msg => #{type => string}}}),
    K1 = beamai_kernel:add_plugin(K0, <<"test">>, [Func]),
    Context = beamai_context:with_kernel(beamai_context:new(), K1),

    {ok, State} = beamai_process_agent_tool_step:init(#{}),

    ToolCall = #{
        <<"id">> => <<"call_1">>,
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"echo">>,
            <<"arguments">> => <<"{\"msg\":\"hello\"}">>
        }
    },
    Inputs = #{tool_request => #{tool_calls => [ToolCall]}},

    {ok, #{events := [Event]}} =
        beamai_process_agent_tool_step:on_activate(Inputs, State, Context),
    %% 验证事件
    ?assertEqual(tool_results, maps:get(name, Event)),
    Data = maps:get(data, Event),
    ?assert(is_list(Data)),
    [Result | _] = Data,
    ?assertEqual(<<"call_1">>, maps:get(tool_call_id, Result)),
    ?assertEqual(<<"echo">>, maps:get(name, Result)).

tool_step_pause_on_hook_test() ->
    K0 = beamai_kernel:new(),
    Func = beamai_function:new(<<"danger">>,
        fun(_) -> {ok, <<"done">>} end,
        #{description => <<"dangerous op">>,
          parameters => #{}}),
    K1 = beamai_kernel:add_plugin(K0, <<"test">>, [Func]),
    Context = beamai_context:with_kernel(beamai_context:new(), K1),

    Hook = fun(<<"danger">>, _Args) -> {pause, needs_approval};
              (_, _) -> ok end,
    {ok, State} = beamai_process_agent_tool_step:init(#{on_tool_call => Hook}),

    ToolCall = #{
        <<"id">> => <<"call_2">>,
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"danger">>,
            <<"arguments">> => <<"{}">>
        }
    },
    Inputs = #{tool_request => #{tool_calls => [ToolCall]}},

    ?assertMatch({pause, #{reason := needs_approval}, _},
        beamai_process_agent_tool_step:on_activate(Inputs, State, Context)).

%%====================================================================
%% beamai_process_agent build tests
%%====================================================================

build_basic_config_test() ->
    Config = #{
        kernel => beamai_kernel:new(),
        system_prompt => <<"test">>
    },
    {ok, {ProcessDef, Context}} = beamai_process_agent:build(Config),
    ?assert(is_map(ProcessDef)),
    ?assertNotEqual(undefined, beamai_context:get_kernel(Context)),
    %% 验证有两个 step
    Steps = maps:get(steps, ProcessDef),
    ?assert(maps:is_key(llm_step, Steps)),
    ?assert(maps:is_key(tool_step, Steps)).

build_with_llm_config_test() ->
    Config = #{
        llm => {zhipu, #{model => <<"glm-4-flash">>}},
        system_prompt => <<"test">>
    },
    {ok, {_ProcessDef, Context}} = beamai_process_agent:build(Config),
    Kernel = beamai_context:get_kernel(Context),
    ?assertNotEqual(undefined, Kernel).

build_with_output_event_test() ->
    Config = #{
        kernel => beamai_kernel:new(),
        output_event => custom_done
    },
    {ok, {ProcessDef, _Context}} = beamai_process_agent:build(Config),
    #{steps := #{llm_step := #{config := LlmConfig}}} = ProcessDef,
    ?assertEqual(custom_done, maps:get(output_event, LlmConfig)).
