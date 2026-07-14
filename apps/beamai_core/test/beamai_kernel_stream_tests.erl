%%%-------------------------------------------------------------------
%%% @doc token_transform 链 kernel 集成测试（invoke_chat_stream 组装点）
%%%
%%% 对照 clj token-stream-filter-design.md §6 锚点：变换生效、最终响应
%%% 不被变换、异常不 flush、退化路径、同步路径忽略、hold-release 两分支。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_kernel_stream_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

%% meck stream_chat：把 Tokens 依次喂给 on_llm_new_token，返回 Result
mock_stream(Tokens, Result) ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            TokenCb = maps:get(on_llm_new_token, Opts),
            [TokenCb(T, #{}) || T <- Tokens],
            Result
        end).

kernel_with(Filters) ->
    beamai_kernel:add_service(beamai_kernel:new(#{}, Filters),
                              beamai_chat_completion:create(mock, #{})).

collector() ->
    Self = self(),
    fun(Token, _Meta) -> Self ! {tok, Token}, ok end.

drain(Acc) ->
    receive {tok, T} -> drain([T | Acc])
    after 50 -> lists:reverse(Acc)
    end.

flush_mailbox() ->
    receive {tok, _} -> flush_mailbox() after 0 -> ok end.

run_stream(K, Sink) ->
    beamai_kernel:invoke_chat_stream(
        K, [#{role => user, content => <<"hi">>}], #{}, Sink).

%%====================================================================
%% 变换生效：redact filter 改写出站 token
%%====================================================================

redact_transforms_sink_test() ->
    flush_mailbox(),
    mock_stream([<<"key=abc123 ok">>, <<"plain">>],
                {ok, #{content => <<"key=abc123 ok plain">>,
                       finish_reason => <<"stop">>}}),
    try
        K = kernel_with([beamai_filters:token_redact_filter(
                             <<"key=\\w+">>, <<"[REDACTED]">>)]),
        {ok, Resp, _} = run_stream(K, collector()),
        ?assertEqual([<<"[REDACTED] ok">>, <<"plain">>], drain([])),
        %% 硬边界：最终归一化响应不被变换
        ?assertEqual(<<"key=abc123 ok plain">>, maps:get(content, Resp))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 组合顺序：先注册先见原始 token
%%====================================================================

composition_order_test() ->
    flush_mailbox(),
    mock_stream([<<"x">>], {ok, #{content => <<"x">>, finish_reason => <<"stop">>}}),
    Prefix = fun(Name, P) ->
        beamai_filter:new(Name, #{token_transform => #{
            step => fun(#{token := T} = TD, S) ->
                {[TD#{token => <<P/binary, T/binary>>}], S}
            end}})
    end,
    try
        K = kernel_with([Prefix(<<"a">>, <<"A">>), Prefix(<<"b">>, <<"B">>)]),
        {ok, _, _} = run_stream(K, collector()),
        %% A 先见原始 token → "Ax"，再经 B → "BAx"
        ?assertEqual([<<"BAx">>], drain([]))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% hold-release：通过 → 完流时按原序放行
%%====================================================================

hold_release_pass_test() ->
    flush_mailbox(),
    mock_stream([<<"he">>, <<"llo">>],
                {ok, #{content => <<"hello">>, finish_reason => <<"stop">>}}),
    try
        K = kernel_with([beamai_filters:hold_release_filter(
                             fun(<<"hello">>) -> ok end)]),
        {ok, Resp, _} = run_stream(K, collector()),
        %% 全部缓冲在完流时按原序放行
        ?assertEqual([<<"he">>, <<"llo">>], drain([])),
        ?assertEqual(<<"hello">>, maps:get(content, Resp))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% hold-release：不通过 → 只收到一个替换 token；最终响应仍是原文
%%====================================================================

hold_release_block_test() ->
    flush_mailbox(),
    mock_stream([<<"bad">>, <<" stuff">>],
                {ok, #{content => <<"bad stuff">>, finish_reason => <<"stop">>}}),
    try
        K = kernel_with([beamai_filters:hold_release_filter(
                             fun(_) -> {block, <<"[BLOCKED]">>} end)]),
        {ok, Resp, _} = run_stream(K, collector()),
        ?assertEqual([<<"[BLOCKED]">>], drain([])),
        %% 硬边界：token 链只改交付；返回的响应仍是完整原文
        ?assertEqual(<<"bad stuff">>, maps:get(content, Resp))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 异常不 flush：stream_chat 出错 → 缓冲丢弃，sink 什么都收不到
%%====================================================================

error_no_flush_test() ->
    flush_mailbox(),
    mock_stream([<<"partial">>], {error, boom}),
    try
        K = kernel_with([beamai_filters:hold_release_filter(fun(_) -> ok end)]),
        ?assertEqual({error, boom}, run_stream(K, collector())),
        %% hold_release 缓冲了 "partial"，错误路径不 flush → 不外泄
        ?assertEqual([], drain([]))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 退化路径：无 token_transform 时 sink 原样收到原始 token
%%====================================================================

no_token_transform_passthrough_test() ->
    flush_mailbox(),
    mock_stream([<<"a">>, <<"b">>],
                {ok, #{content => <<"ab">>, finish_reason => <<"stop">>}}),
    try
        %% 有 chat filter 但无 token_transform：token 路径不受影响
        ChatF = beamai_filter:new(<<"noop">>, #{
            around_chat => fun(Req, _F, Next) -> Next(Req) end}),
        K = kernel_with([ChatF]),
        {ok, _, _} = run_stream(K, collector()),
        ?assertEqual([<<"a">>, <<"b">>], drain([]))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 同步路径忽略 token_transform：invoke_chat 带 token_transform filter 照常工作
%%====================================================================

sync_ignores_token_transform_test() ->
    K = kernel_with([beamai_filters:token_redact_filter(<<"x">>, <<"y">>)]),
    {ok, Resp, _} = beamai_kernel:invoke_chat(
        K, [#{role => user, content => <<"hi">>}], #{}),
    ?assert(is_map(Resp)).
