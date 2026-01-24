%%%-------------------------------------------------------------------
%%% @doc BeamAI Plugin System Tests
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Plugin Loading Tests
%%====================================================================

load_file_plugin_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_plugins:load(K0, beamai_plugin_file),
    Functions = beamai_kernel:list_functions(K1),
    Names = [maps:get(name, F) || F <- Functions],
    ?assert(lists:member(<<"file_read">>, Names)),
    ?assert(lists:member(<<"file_write">>, Names)),
    ?assert(lists:member(<<"file_glob">>, Names)),
    ?assert(lists:member(<<"file_grep">>, Names)),
    ?assert(lists:member(<<"file_list">>, Names)),
    ?assert(lists:member(<<"file_mkdir">>, Names)).

load_shell_plugin_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_plugins:load(K0, beamai_plugin_shell),
    Functions = beamai_kernel:list_functions(K1),
    Names = [maps:get(name, F) || F <- Functions],
    ?assert(lists:member(<<"shell_execute">>, Names)).

load_todo_plugin_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_plugins:load(K0, beamai_plugin_todo),
    Functions = beamai_kernel:list_functions(K1),
    Names = [maps:get(name, F) || F <- Functions],
    ?assert(lists:member(<<"write_todos">>, Names)),
    ?assert(lists:member(<<"read_todos">>, Names)).

load_human_plugin_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_plugins:load(K0, beamai_plugin_human),
    Functions = beamai_kernel:list_functions(K1),
    Names = [maps:get(name, F) || F <- Functions],
    ?assert(lists:member(<<"ask_human">>, Names)),
    ?assert(lists:member(<<"confirm_action">>, Names)).

load_all_plugins_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_plugins:load_all(K0, beamai_plugins:available()),
    Functions = beamai_kernel:list_functions(K1),
    ?assert(length(Functions) >= 10).

%%====================================================================
%% Plugin Behaviour Tests
%%====================================================================

plugin_info_test() ->
    Info = beamai_plugin_file:plugin_info(),
    ?assertEqual(<<"file">>, maps:get(name, Info)),
    ?assert(maps:is_key(description, Info)).

plugin_functions_test() ->
    Funcs = beamai_plugin_file:functions(),
    ?assert(is_list(Funcs)),
    ?assert(length(Funcs) > 0),
    [First | _] = Funcs,
    ?assert(maps:is_key(name, First)),
    ?assert(maps:is_key(handler, First)).

%%====================================================================
%% Middleware Tests
%%====================================================================

middleware_init_test() ->
    Chain = beamai_middleware_runner:init([
        {middleware_call_limit, #{max_model_calls => 5}}
    ]),
    ?assert(is_list(Chain)),
    ?assertEqual(1, length(Chain)),
    [#{module := middleware_call_limit, state := State}] = Chain,
    ?assertEqual(5, maps:get(max_model_calls, State)).

middleware_to_filters_test() ->
    Chain = beamai_middleware_runner:init([
        {middleware_call_limit, #{}}
    ]),
    Filters = beamai_middleware_runner:to_filters(Chain),
    ?assert(is_list(Filters)),
    %% Should have at least one filter (for hooks that call_limit implements)
    ?assert(length(Filters) >= 1),
    [#{name := Name, type := Type} | _] = Filters,
    ?assert(is_binary(Name)),
    ?assert(lists:member(Type, [pre_chat, post_chat, pre_invocation, post_invocation])).

with_middleware_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_plugins:with_middleware(K0, [
        {middleware_call_limit, #{max_model_calls => 10}}
    ]),
    #{filters := Filters} = K1,
    ?assert(length(Filters) >= 1).

middleware_presets_test() ->
    Default = beamai_middleware_presets:default(),
    ?assert(is_list(Default)),
    ?assert(length(Default) >= 1),

    Minimal = beamai_middleware_presets:minimal(),
    ?assert(length(Minimal) >= 1),

    Production = beamai_middleware_presets:production(),
    ?assert(length(Production) >= 2).

%%====================================================================
%% Middleware Hook Tests
%%====================================================================

call_limit_pre_chat_test() ->
    State = middleware_call_limit:init(#{max_model_calls => 2}),
    %% First call should pass
    ?assertEqual(ok, middleware_call_limit:pre_chat(#{}, State)),
    %% Exceed limit
    ExceededState = State#{model_call_count => 5},
    Result = middleware_call_limit:pre_chat(#{}, ExceededState),
    ?assertMatch({error, {limit_exceeded, _}}, Result).

call_limit_pre_invocation_test() ->
    State = middleware_call_limit:init(#{max_tool_calls => 3}),
    ?assertEqual(ok, middleware_call_limit:pre_invocation(#{}, State)),
    ExceededState = State#{tool_call_count => 5},
    Result = middleware_call_limit:pre_invocation(#{}, ExceededState),
    ?assertMatch({error, {limit_exceeded, _}}, Result).

human_approval_none_mode_test() ->
    State = middleware_human_approval:init(#{mode => none}),
    ?assertEqual(ok, middleware_human_approval:pre_invocation(#{}, State)).

human_approval_all_mode_test() ->
    State = middleware_human_approval:init(#{mode => all}),
    FilterCtx = #{function => #{name => <<"test">>}, args => #{}},
    Result = middleware_human_approval:pre_invocation(FilterCtx, State),
    ?assertMatch({skip, #{pending := approval}}, Result).

%%====================================================================
%% Tool DSL Tests
%%====================================================================

define_tool_simple_test() ->
    Tool = beamai_plugins:define_tool(<<"test">>, <<"A test tool">>,
        fun(_Args) -> {ok, <<"hello">>} end),
    ?assertEqual(<<"test">>, maps:get(name, Tool)),
    ?assertEqual(<<"A test tool">>, maps:get(description, Tool)).

define_tool_with_params_test() ->
    Tool = beamai_plugins:define_tool(<<"greet">>, <<"Greet someone">>,
        [{<<"name">>, string, <<"Person's name">>, required}],
        fun(#{<<"name">> := Name}) -> {ok, <<"Hello, ", Name/binary>>} end),
    Params = maps:get(parameters, Tool),
    Props = maps:get(properties, Params),
    ?assert(maps:is_key(<<"name">>, Props)).

%%====================================================================
%% Tool Security Tests
%%====================================================================

security_check_path_test() ->
    ?assertEqual(ok, beamai_tool_security:check_path(<<"/tmp/test.txt">>)),
    ?assertMatch({error, _}, beamai_tool_security:check_path(<<".git/config">>)),
    ?assertMatch({error, _}, beamai_tool_security:check_path(<<"/home/.env">>)).

security_check_command_test() ->
    ?assertEqual(ok, beamai_tool_security:check_command(<<"ls -la">>)),
    ?assertMatch({error, _}, beamai_tool_security:check_command(<<"rm -rf /">>)).

%%====================================================================
%% Integration Tests
%%====================================================================

full_pipeline_test() ->
    %% Build kernel with plugins and middleware
    K0 = beamai_kernel:new(),
    K1 = beamai_plugins:load(K0, beamai_plugin_file),
    K2 = beamai_plugins:load(K1, beamai_plugin_todo),
    K3 = beamai_plugins:with_middleware(K2, beamai_middleware_presets:minimal()),

    %% Verify functions are registered
    Functions = beamai_kernel:list_functions(K3),
    ?assert(length(Functions) >= 8),

    %% Verify filters are present
    #{filters := Filters} = K3,
    ?assert(length(Filters) >= 1),

    %% Invoke a function directly
    Result = beamai_kernel:invoke_tool(K3, <<"read_todos">>, #{}, beamai_context:new()),
    ?assertMatch({ok, _, _}, Result).

available_plugins_test() ->
    Available = beamai_plugins:available(),
    ?assert(length(Available) >= 4),
    ?assert(lists:member(beamai_plugin_file, Available)),
    ?assert(lists:member(beamai_plugin_shell, Available)).
