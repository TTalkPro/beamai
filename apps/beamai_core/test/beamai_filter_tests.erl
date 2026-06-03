%%%-------------------------------------------------------------------
%%% @doc Filter 洋葱链测试
%%%-------------------------------------------------------------------
-module(beamai_filter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHAT, {pre_chat, post_chat}).
-define(TOOL, {pre_tool, post_tool}).

%%====================================================================
%% 构造器 / 工具
%%====================================================================

new_shape_test() ->
    F = beamai_filter:new(<<"a">>, #{pre_chat => fun(R) -> R end}, 5),
    ?assertEqual(<<"a">>, maps:get(name, F)),
    ?assertEqual(5, maps:get(order, F)),
    ?assert(is_function(beamai_filter:hook(F, pre_chat), 1)),
    ?assertEqual(undefined, beamai_filter:hook(F, post_chat)).

new_default_order_test() ->
    F = beamai_filter:new(<<"a">>, #{}),
    ?assertEqual(0, maps:get(order, F)).

sort_test() ->
    F1 = beamai_filter:new(<<"a">>, #{}, 30),
    F2 = beamai_filter:new(<<"b">>, #{}, 10),
    F3 = beamai_filter:new(<<"c">>, #{}, 20),
    ?assertEqual([<<"b">>, <<"c">>, <<"a">>],
                 [maps:get(name, F) || F <- beamai_filter:sort([F1, F2, F3])]).

%%====================================================================
%% 洋葱顺序：外层 pre 先、外层 post 后（回程自动逆序）
%%====================================================================

onion_order_test() ->
    A = trace_filter(<<"A">>, 1, a_before, a_after),
    B = trace_filter(<<"B">>, 2, b_before, b_after),
    Terminal = fun(#{trace := T}) -> #{trace => T ++ [terminal]} end,
    {ok, #{trace := Final}} =
        beamai_filter_chain:run([B, A], ?CHAT, Terminal, #{trace => []}),  % 乱序传入靠 order 排
    ?assertEqual([a_before, b_before, terminal, b_after, a_after], Final).

empty_chain_runs_terminal_test() ->
    Terminal = fun(#{trace := T}) -> #{trace => T ++ [terminal]} end,
    {ok, #{trace := Final}} = beamai_filter_chain:run([], ?CHAT, Terminal, #{trace => []}),
    ?assertEqual([terminal], Final).

%%====================================================================
%% 链按 hook 选择：tool-only filter 不参与 chat 链
%%====================================================================

irrelevant_filter_skipped_test() ->
    ChatF = trace_filter(<<"chat">>, 1, c_before, c_after),
    ToolOnly = beamai_filter:new(<<"tool">>, #{
        pre_tool => fun(#{trace := T} = R) -> R#{trace => T ++ [t_before]} end
    }, 0),
    Terminal = fun(#{trace := T}) -> #{trace => T ++ [terminal]} end,
    {ok, #{trace := Final}} =
        beamai_filter_chain:run([ChatF, ToolOnly], ?CHAT, Terminal, #{trace => []}),
    %% tool-only filter 在 chat 链被跳过
    ?assertEqual([c_before, terminal, c_after], Final).

%%====================================================================
%% pre 短路（{halt, Response}）：跳过内层，仍执行本层 post
%%====================================================================

halt_short_circuits_test() ->
    Halter = beamai_filter:new(<<"halt">>, #{
        pre_chat => fun(#{trace := T}) -> {halt, #{trace => T ++ [halted]}} end,
        post_chat => fun(#{trace := T} = R) -> R#{trace => T ++ [h_after]} end
    }, 1),
    Terminal = fun(#{trace := T}) -> #{trace => T ++ [terminal]} end,
    {ok, #{trace := Final}} = beamai_filter_chain:run([Halter], ?CHAT, Terminal, #{trace => []}),
    ?assertEqual([halted, h_after], Final).

%%====================================================================
%% tool 链
%%====================================================================

tool_phase_test() ->
    F = beamai_filter:new(<<"t">>, #{
        pre_tool => fun(#{trace := T} = R) -> R#{trace => T ++ [t_before]} end,
        post_tool => fun(#{trace := T} = R) -> R#{trace => T ++ [t_after]} end
    }, 0),
    Terminal = fun(#{trace := T}) -> #{trace => T ++ [terminal]} end,
    {ok, #{trace := Final}} = beamai_filter_chain:run([F], ?TOOL, Terminal, #{trace => []}),
    ?assertEqual([t_before, terminal, t_after], Final).

%%====================================================================
%% terminal throw → {error, Reason}
%%====================================================================

terminal_error_test() ->
    F = trace_filter(<<"A">>, 1, a_before, a_after),
    Terminal = fun(_) -> throw(boom) end,
    ?assertEqual({error, boom},
                 beamai_filter_chain:run([F], ?CHAT, Terminal, #{trace => []})).

%%====================================================================
%% 辅助
%%====================================================================

trace_filter(Name, Order, BeforeTag, AfterTag) ->
    beamai_filter:new(Name, #{
        pre_chat => fun(#{trace := T} = R) -> R#{trace => T ++ [BeforeTag]} end,
        post_chat => fun(#{trace := T} = R) -> R#{trace => T ++ [AfterTag]} end
    }, Order).
