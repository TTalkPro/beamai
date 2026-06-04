%%%-------------------------------------------------------------------
%%% @doc Filter 洋葱链测试（around 模型）
%%%-------------------------------------------------------------------
-module(beamai_filter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHAT, around_chat).
-define(TOOL, around_tool).

%%====================================================================
%% 构造器 / 工具
%%====================================================================

new_shape_test() ->
    F = beamai_filter:new(<<"a">>, #{around_chat => fun(R, _F, N) -> N(R) end}, 5),
    ?assertEqual(<<"a">>, maps:get(name, F)),
    ?assertEqual(5, maps:get(order, F)),
    ?assertEqual(#{}, beamai_filter:init(F)),
    ?assert(is_function(beamai_filter:hook(F, around_chat), 3)),
    ?assertEqual(undefined, beamai_filter:hook(F, around_tool)).

new_default_order_test() ->
    F = beamai_filter:new(<<"a">>, #{}),
    ?assertEqual(0, maps:get(order, F)).

new_with_init_test() ->
    F = beamai_filter:new(<<"a">>, #{}, 0, #{seed => 42}),
    ?assertEqual(#{seed => 42}, beamai_filter:init(F)).

sort_test() ->
    F1 = beamai_filter:new(<<"a">>, #{}, 30),
    F2 = beamai_filter:new(<<"b">>, #{}, 10),
    F3 = beamai_filter:new(<<"c">>, #{}, 20),
    ?assertEqual([<<"b">>, <<"c">>, <<"a">>],
                 [maps:get(name, F) || F <- beamai_filter:sort([F1, F2, F3])]).

%%====================================================================
%% 洋葱顺序：外层前置先、外层后置后（回程自动逆序）
%%====================================================================

onion_order_test() ->
    A = trace_filter(<<"A">>, 1, a_before, a_after),
    B = trace_filter(<<"B">>, 2, b_before, b_after),
    {ok, #{trace := Final}} =
        beamai_filter_chain:run([B, A], ?CHAT, terminal(), req()),  % 乱序传入靠 order 排
    ?assertEqual([a_before, b_before, terminal, b_after, a_after], Final).

empty_chain_runs_terminal_test() ->
    {ok, #{trace := Final}} = beamai_filter_chain:run([], ?CHAT, terminal(), req()),
    ?assertEqual([terminal], Final).

%%====================================================================
%% 链按 hook 选择：tool-only filter 不参与 chat 链
%%====================================================================

irrelevant_filter_skipped_test() ->
    ChatF = trace_filter(<<"chat">>, 1, c_before, c_after),
    ToolOnly = beamai_filter:new(<<"tool">>, #{
        around_tool => fun(#{trace := T} = R, _F, N) -> N(R#{trace => T ++ [t_before]}) end
    }, 0),
    {ok, #{trace := Final}} =
        beamai_filter_chain:run([ChatF, ToolOnly], ?CHAT, terminal(), req()),
    %% tool-only filter 在 chat 链被跳过
    ?assertEqual([c_before, terminal, c_after], Final).

%%====================================================================
%% 短路：around 不调 Next，跳过内层；外层 filter 的后置仍执行
%%====================================================================

short_circuit_test() ->
    Outer = trace_filter(<<"outer">>, 1, o_before, o_after),
    Halter = beamai_filter:new(<<"halt">>, #{
        around_chat => fun(#{trace := T} = Req, _F, _Next) ->
            Req#{trace => T ++ [halted]}   %% 不调 Next：跳过内层与 terminal
        end
    }, 2),
    Inner = trace_filter(<<"inner">>, 3, i_before, i_after),
    {ok, #{trace := Final}} =
        beamai_filter_chain:run([Outer, Halter, Inner], ?CHAT, terminal(), req()),
    ?assertEqual([o_before, halted, o_after], Final).

%%====================================================================
%% tool 链
%%====================================================================

tool_phase_test() ->
    F = beamai_filter:new(<<"t">>, #{
        around_tool => fun(#{trace := T} = Req, _F, Next) ->
            #{trace := T2} = Resp = Next(Req#{trace => T ++ [t_before]}),
            Resp#{trace => T2 ++ [t_after]}
        end
    }, 0),
    {ok, #{trace := Final}} = beamai_filter_chain:run([F], ?TOOL, terminal(), req()),
    ?assertEqual([t_before, terminal, t_after], Final).

%%====================================================================
%% terminal throw → {error, Reason}
%%====================================================================

terminal_error_test() ->
    F = trace_filter(<<"A">>, 1, a_before, a_after),
    Terminal = fun(_) -> throw(boom) end,
    ?assertEqual({error, boom},
                 beamai_filter_chain:run([F], ?CHAT, Terminal, req())).

%%====================================================================
%% 私有上下文：按 filter 隔离
%%====================================================================

filter_state_isolation_test() ->
    %% 两个 filter 各自维护私有计数器（同名内部键 n），按名字隔离互不干扰
    A = counter_filter(<<"A">>),
    B = counter_filter(<<"B">>),
    {ok, #{context := Ctx}} = beamai_filter_chain:run([A, B], ?CHAT, terminal(), req()),
    ?assertEqual(#{n => 1}, beamai_context:filter_state(Ctx, <<"A">>, #{})),
    ?assertEqual(#{n => 1}, beamai_context:filter_state(Ctx, <<"B">>, #{})).

%%====================================================================
%% 私有上下文：随 context 透传，跨多次 run 存活
%%====================================================================

filter_state_persists_test() ->
    C = counter_filter(<<"C">>),
    {ok, #{context := Ctx1}} = beamai_filter_chain:run([C], ?CHAT, terminal(), req()),
    ?assertEqual(#{n => 1}, beamai_context:filter_state(Ctx1, <<"C">>, #{})),
    %% 第二次 run 复用上一次的 context，计数器累积
    {ok, #{context := Ctx2}} =
        beamai_filter_chain:run([C], ?CHAT, terminal(), #{trace => [], context => Ctx1}),
    ?assertEqual(#{n => 2}, beamai_context:filter_state(Ctx2, <<"C">>, #{})).

%%====================================================================
%% 私有上下文：init 初值在首次进入时种入
%%====================================================================

filter_state_init_test() ->
    F = beamai_filter:new(<<"I">>, #{
        around_chat => fun(Req, FCtx, Next) ->
            Resp = Next(Req),
            {Resp, FCtx#{seen => true}}
        end
    }, 0, #{seed => 42}),
    {ok, #{context := Ctx}} = beamai_filter_chain:run([F], ?CHAT, terminal(), req()),
    ?assertEqual(#{seed => 42, seen => true}, beamai_context:filter_state(Ctx, <<"I">>, #{})).

%%====================================================================
%% 辅助
%%====================================================================

%% 测试请求：trace 累积链 + 真实共享 context
req() ->
    #{trace => [], context => beamai_context:new()}.

%% 测试 terminal：保留请求（含 context），追加 terminal 标记
terminal() ->
    fun(#{trace := T} = Req) -> Req#{trace => T ++ [terminal]} end.

%% 前置追加 BeforeTag、后置追加 AfterTag 的 around_chat filter
trace_filter(Name, Order, BeforeTag, AfterTag) ->
    beamai_filter:new(Name, #{
        around_chat => fun(#{trace := T} = Req, _FCtx, Next) ->
            #{trace := T2} = Resp = Next(Req#{trace => T ++ [BeforeTag]}),
            Resp#{trace => T2 ++ [AfterTag]}
        end
    }, Order).

%% 私有上下文计数器 filter（每次调用 n+1，存于自身私有槽）
counter_filter(Name) ->
    beamai_filter:new(Name, #{
        around_chat => fun(Req, FCtx, Next) ->
            N = maps:get(n, FCtx, 0),
            Resp = Next(Req),
            {Resp, FCtx#{n => N + 1}}
        end
    }, 0).
