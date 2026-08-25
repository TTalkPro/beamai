%%%-------------------------------------------------------------------
%%% @doc chat / llm 两层洋葱的分层测试
%%%
%%% 锚点：around_chat 每轮进出一次、around_llm 每次真实请求进出一次、重试只在
%%% llm 层重入、缺省重试 filter 的注入与关闭、单次 opts 覆盖、流式路径不重试。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_kernel_llm_chain_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MSGS, [#{role => user, content => <<"hi">>}]).
-define(OK_RESP, #{content => <<"ok">>, finish_reason => <<"stop">>}).
-define(BOOM, {http_error, 503, <<"busy">>}).

%%====================================================================
%% 辅助
%%====================================================================

%% meck chat/3：前 Failures 次返回可重试错误，其后成功；每次调用留 trace
mock_chat(Failures) ->
    Self = self(),
    Attempts = counters:new(1, []),
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat,
        fun(_Config, _Messages, _Opts) ->
            N = counters:get(Attempts, 1),
            counters:add(Attempts, 1, 1),
            Self ! {trace, llm_call},
            case N < Failures of
                true -> {error, ?BOOM};
                false -> {ok, ?OK_RESP}
            end
        end).

%% meck stream_chat/4：总是失败（用于验证流式不重试）
mock_stream_failing() ->
    Self = self(),
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, _RawCb, _Opts) ->
            Self ! {trace, llm_stream},
            {error, ?BOOM}
        end).

%% 两链都留痕的 filter（Tag 用于区分层）
tracing_filter(Name, Tag) ->
    Self = self(),
    beamai_filter:new(Name, #{
        around_chat => fun(Req, _FCtx, Next) ->
            Self ! {trace, {chat_in, Tag}},
            Resp = Next(Req),
            Self ! {trace, {chat_out, Tag}},
            Resp
        end,
        around_llm => fun(Req, _FCtx, Next) ->
            Self ! {trace, {llm_in, Tag}},
            Resp = Next(Req),
            Self ! {trace, {llm_out, Tag}},
            Resp
        end
    }).

kernel_with(Settings, Filters) ->
    beamai_kernel:add_chat_model(beamai_kernel:new(Settings, Filters),
                              beamai_chat_model:create(mock, #{})).

invoke(K) -> invoke(K, #{}).
invoke(K, Opts) -> beamai_kernel:invoke_chat(K, ?MSGS, Opts).

fast_retry() -> #{llm_retry => #{retry_delay => 1}}.

drain() -> drain([]).
drain(Acc) ->
    receive {trace, T} -> drain([T | Acc])
    after 50 -> lists:reverse(Acc)
    end.

flush() -> receive {trace, _} -> flush() after 0 -> ok end.

count(X, L) -> length([E || E <- L, E =:= X]).

with_meck(Fun) ->
    flush(),
    try Fun()
    after meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% 缺省重试 filter（kernel 注入在 llm 链最内层）
%%====================================================================

default_retry_filter_injected_test() ->
    with_meck(fun() ->
        mock_chat(2),
        ?assertMatch({ok, _, _}, invoke(kernel_with(fast_retry(), []))),
        %% 两次失败 + 一次成功 = 三次真实请求
        ?assertEqual(3, count(llm_call, drain()))
    end).

llm_retry_false_disables_injection_test() ->
    with_meck(fun() ->
        mock_chat(1),
        ?assertEqual({error, ?BOOM}, invoke(kernel_with(#{llm_retry => false}, []))),
        ?assertEqual(1, count(llm_call, drain()))
    end).

%% 单次 chat opts 覆盖 filter 构造时的默认重试参数
chat_opts_override_retry_test() ->
    with_meck(fun() ->
        mock_chat(1),
        ?assertEqual({error, ?BOOM},
                     invoke(kernel_with(fast_retry(), []), #{max_retries => 0})),
        ?assertEqual(1, count(llm_call, drain()))
    end).

%%====================================================================
%% 分层语义：重试只在 llm 层重入
%%====================================================================

retry_does_not_reenter_chat_layer_test() ->
    with_meck(fun() ->
        mock_chat(2),
        K = kernel_with(fast_retry(), [tracing_filter(<<"probe">>, outer)]),
        ?assertMatch({ok, _, _}, invoke(K)),
        Trace = drain(),
        %% chat 层每轮恰好一次；probe 在缺省重试 filter 之外，故 llm 层也只一次
        ?assertEqual(1, count({chat_in, outer}, Trace)),
        ?assertEqual(1, count({chat_out, outer}, Trace)),
        ?assertEqual(1, count({llm_in, outer}, Trace)),
        %% 而真实请求发了三次
        ?assertEqual(3, count(llm_call, Trace))
    end).

%% 位于重试 filter **之内**的 llm filter 看得到每一次真实尝试
inner_llm_filter_sees_every_attempt_test() ->
    with_meck(fun() ->
        mock_chat(2),
        K = kernel_with(#{llm_retry => false},
                        [tracing_filter(<<"outer">>, outer),
                         beamai_llm_filters:retry_filter(#{retry_delay => 1}),
                         tracing_filter(<<"inner">>, inner)]),
        ?assertMatch({ok, _, _}, invoke(K)),
        Trace = drain(),
        ?assertEqual(1, count({llm_in, outer}, Trace)),
        ?assertEqual(3, count({llm_in, inner}, Trace)),
        %% chat 链不受影响：两个 filter 各进出一次
        ?assertEqual(1, count({chat_in, outer}, Trace)),
        ?assertEqual(1, count({chat_in, inner}, Trace))
    end).

%%====================================================================
%% 层序：llm 链嵌在 chat 链之内
%%====================================================================

llm_chain_nested_inside_chat_chain_test() ->
    with_meck(fun() ->
        mock_chat(0),
        K = kernel_with(#{llm_retry => false}, [tracing_filter(<<"probe">>, p)]),
        ?assertMatch({ok, _, _}, invoke(K)),
        ?assertEqual([{chat_in, p}, {llm_in, p}, llm_call, {llm_out, p}, {chat_out, p}],
                     drain())
    end).

%%====================================================================
%% 流式：llm 链照跑，但缺省重试 filter 不介入
%%====================================================================

stream_runs_llm_chain_without_retry_test() ->
    with_meck(fun() ->
        mock_stream_failing(),
        K = kernel_with(fast_retry(), [tracing_filter(<<"probe">>, p)]),
        Sink = fun(_Token, _Meta) -> ok end,
        ?assertEqual({error, ?BOOM},
                     beamai_kernel:invoke_chat_stream(K, ?MSGS, #{}, Sink)),
        Trace = drain(),
        %% llm 层在流式路径同样生效
        ?assertEqual(1, count({llm_in, p}, Trace)),
        %% 但只发了一次真实请求——token 已投递的流不能重跑
        ?assertEqual(1, count(llm_stream, Trace))
    end).
