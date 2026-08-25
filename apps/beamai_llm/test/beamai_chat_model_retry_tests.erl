%%%-------------------------------------------------------------------
%%% @doc beamai_chat_model 内建重试的行为与层次
%%%
%%% 重试在 provider 调用**内部**、整个 filter 栈**之下**：
%%% filter 看到的是「一次逻辑调用」，重试重入碰不到任何 filter。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_chat_model_retry_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MSGS, [#{role => user, content => <<"hi">>}]).

%%====================================================================
%% 辅助
%%====================================================================

%% FailN 次失败后成功；retry_delay 压到 1ms
config(FailN) -> config(FailN, #{}).
config(FailN, Extra) ->
    Ctr = counters:new(1, []),
    Config = beamai_chat_model:create({custom, beamai_flaky_provider},
                                      maps:merge(#{attempts => Ctr, fail_n => FailN,
                                                   retry_delay => 1}, Extra)),
    {Config, fun() -> counters:get(Ctr, 1) end}.

%%====================================================================
%% 重试判定
%%====================================================================

%% 瞬态错误（503）重试到成功
retries_transient_error_test() ->
    {C, Attempts} = config(2),
    ?assertMatch({ok, #{content := <<"ok">>}}, beamai_chat_model:chat(C, ?MSGS)),
    ?assertEqual(3, Attempts()).

%% 语义错误（400）不重试
no_retry_on_semantic_error_test() ->
    {C, Attempts} = config(1, #{error => {http_error, 400, <<"bad">>}}),
    ?assertEqual({error, {http_error, 400, <<"bad">>}}, beamai_chat_model:chat(C, ?MSGS)),
    ?assertEqual(1, Attempts()).

%% 重试耗尽后返回最后一次错误
gives_up_after_max_retries_test() ->
    {C, Attempts} = config(99, #{max_retries => 2}),
    ?assertMatch({error, {http_error, 503, _}}, beamai_chat_model:chat(C, ?MSGS)),
    ?assertEqual(3, Attempts()).   %% 首次 + 2 次重试

%%====================================================================
%% 三级取值：单次 Opts > provider Config > 框架默认
%%====================================================================

config_level_default_applies_test() ->
    {C, Attempts} = config(1, #{max_retries => 0}),
    ?assertMatch({error, _}, beamai_chat_model:chat(C, ?MSGS)),
    ?assertEqual(1, Attempts()).

opts_override_config_test() ->
    {C, Attempts} = config(2, #{max_retries => 0}),
    ?assertMatch({ok, _}, beamai_chat_model:chat(C, ?MSGS, #{max_retries => 3})),
    ?assertEqual(3, Attempts()).

opts_can_disable_test() ->
    {C, Attempts} = config(1),
    ?assertMatch({error, _}, beamai_chat_model:chat(C, ?MSGS, #{max_retries => 0})),
    ?assertEqual(1, Attempts()).

%% on_retry 回调是观测每次真实尝试的入口（filter 层看不到尝试）
on_retry_callback_test() ->
    Self = self(),
    {C, _} = config(2, #{on_retry => fun(S) -> Self ! {retry, maps:get(attempt, S)}, ok end}),
    ?assertMatch({ok, _}, beamai_chat_model:chat(C, ?MSGS)),
    ?assertEqual([1, 2], drain_retries([])).

drain_retries(Acc) ->
    receive {retry, N} -> drain_retries([N | Acc])
    after 50 -> lists:reverse(Acc)
    end.

%%====================================================================
%% 流式不重试（token 已投递，重跑会重复投递）
%%====================================================================

stream_does_not_retry_test() ->
    {C, Attempts} = config(99),
    ?assertMatch({error, _}, beamai_chat_model:stream_chat(C, ?MSGS, fun(_) -> ok end)),
    ?assertEqual(1, Attempts()).

%%====================================================================
%% 层次：重试在整个 filter 栈之下
%%====================================================================

%% chat filter 每轮只进出一次，哪怕底下重试了 3 次真实请求
retry_is_invisible_to_chat_filters_test() ->
    Self = self(),
    Probe = beamai_filter:new(<<"probe">>, #{
        around_chat => fun(Req, _F, Next) ->
            Self ! {chat_in}, R = Next(Req), Self ! {chat_out}, R
        end
    }),
    {C, Attempts} = config(2),
    K = beamai_chat_client:add_chat_model(beamai_chat_client:new(#{}, [Probe]), C),
    ?assertMatch({ok, _, _}, beamai_chat_client:invoke_chat(K, ?MSGS, #{})),
    ?assertEqual(3, Attempts()),          %% 真实请求 3 次
    ?assertEqual({1, 1}, count_chat()).   %% chat filter 只进出 1 次

count_chat() -> count_chat(0, 0).
count_chat(In, Out) ->
    receive
        {chat_in} -> count_chat(In + 1, Out);
        {chat_out} -> count_chat(In, Out + 1)
    after 50 -> {In, Out}
    end.
