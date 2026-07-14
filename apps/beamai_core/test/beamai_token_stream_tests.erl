%%%-------------------------------------------------------------------
%%% @doc token 流变换链测试（对照 clj token-stream-filter-design.md §6 锚点）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_token_stream_tests).

-include_lib("eunit/include/eunit.hrl").

%% 收集 sink：token 按序进邮箱
sink() ->
    Self = self(),
    fun(Token, Meta) -> Self ! {tok, Token, Meta}, ok end.

drain(Acc) ->
    receive {tok, T, _M} -> drain([T | Acc])
    after 50 -> lists:reverse(Acc)
    end.

flush_mailbox() ->
    receive {tok, _, _} -> flush_mailbox() after 0 -> ok end.

%% 无状态改写 xf：token 加前缀
prefix_xf(Prefix) ->
    #{step => fun(#{token := T} = TD, S) ->
        {[TD#{token => <<Prefix/binary, T/binary>>}], S}
    end}.

%% 缓冲 N 个批量放行的 xf（partition-all 等价：拼成一个 token 放行）
batch_xf(N) ->
    #{init => [],
      step => fun(TD, Buf) ->
          Buf1 = [TD | Buf],
          case length(Buf1) >= N of
              true ->
                  Joined = join(lists:reverse(Buf1)),
                  {[Joined], []};
              false ->
                  {[], Buf1}
          end
      end,
      flush => fun([]) -> [];
                  (Buf) -> [join(lists:reverse(Buf))]
               end}.

join([#{meta := M} | _] = TDs) ->
    #{token => iolist_to_binary([T || #{token := T} <- TDs]), meta => M}.

%%====================================================================
%% 恒等退化：无 xf 时原样直通
%%====================================================================

identity_passthrough_test() ->
    flush_mailbox(),
    {Cb, Flush} = beamai_token_stream:wrap([], sink()),
    Cb(<<"a">>, #{}), Cb(<<"b">>, #{}),
    ok = Flush(),
    ?assertEqual([<<"a">>, <<"b">>], drain([])).

%%====================================================================
%% 1→N 与 flush：7 个 token、批 3 放行 → 2 批 + completion 冲出尾巴
%%====================================================================

batch_and_flush_test() ->
    flush_mailbox(),
    {Cb, Flush} = beamai_token_stream:wrap([batch_xf(3)], sink()),
    [Cb(T, #{}) || T <- [<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>]],
    %% 正常完流前只放行了 2 批
    ?assertEqual([<<"123">>, <<"456">>], drain([])),
    ok = Flush(),
    %% completion 冲出尾巴
    ?assertEqual([<<"7">>], drain([])).

%%====================================================================
%% 不 flush：缓冲残留丢弃（异常路径调用方不调 Flush 即可）
%%====================================================================

no_flush_drops_buffer_test() ->
    flush_mailbox(),
    {Cb, _Flush} = beamai_token_stream:wrap([batch_xf(3)], sink()),
    Cb(<<"1">>, #{}), Cb(<<"2">>, #{}),
    %% 不调 Flush：sink 什么都收不到
    ?assertEqual([], drain([])).

%%====================================================================
%% 组合顺序：先注册先见原始 token
%%====================================================================

composition_order_test() ->
    flush_mailbox(),
    {Cb, Flush} = beamai_token_stream:wrap(
        [prefix_xf(<<"A">>), prefix_xf(<<"B">>)], sink()),
    Cb(<<"x">>, #{}),
    ok = Flush(),
    %% A 先见原始 token（A 在外）→ "Ax"，再经 B → "BAx"
    ?assertEqual([<<"BAx">>], drain([])).

%%====================================================================
%% flush 级联：外层残留经内层 step 传播后 sink，再轮到内层 flush
%%====================================================================

flush_cascade_through_downstream_test() ->
    flush_mailbox(),
    {Cb, Flush} = beamai_token_stream:wrap(
        [batch_xf(10), prefix_xf(<<"P">>)], sink()),   %% 外层缓冲、内层改写
    Cb(<<"a">>, #{}), Cb(<<"b">>, #{}),
    ?assertEqual([], drain([])),
    ok = Flush(),
    %% 外层 flush 出 "ab"，仍要经内层 prefix → "Pab"
    ?assertEqual([<<"Pab">>], drain([])).

%%====================================================================
%% meta 穿越缓冲保留（token_data 整体入缓冲）
%%====================================================================

meta_preserved_test() ->
    flush_mailbox(),
    Self = self(),
    Sink = fun(Token, Meta) -> Self ! {tok, Token, Meta}, ok end,
    {Cb, Flush} = beamai_token_stream:wrap([batch_xf(2)], Sink),
    Cb(<<"a">>, #{k => 1}), Cb(<<"b">>, #{k => 1}),
    ok = Flush(),
    receive {tok, <<"ab">>, Meta} -> ?assertEqual(#{k => 1}, Meta)
    after 1000 -> ?assert(false)
    end.

%%====================================================================
%% 吞掉（1→0）：step 返回空列表
%%====================================================================

swallow_test() ->
    flush_mailbox(),
    Drop = #{step => fun(#{token := T} = TD, S) ->
        case T of
            <<"secret">> -> {[], S};
            _ -> {[TD], S}
        end
    end},
    {Cb, Flush} = beamai_token_stream:wrap([Drop], sink()),
    Cb(<<"a">>, #{}), Cb(<<"secret">>, #{}), Cb(<<"b">>, #{}),
    ok = Flush(),
    ?assertEqual([<<"a">>, <<"b">>], drain([])).

%%====================================================================
%% Flush 幂等：重复调用不崩溃、不重复放行
%%====================================================================

flush_idempotent_test() ->
    flush_mailbox(),
    {Cb, Flush} = beamai_token_stream:wrap([batch_xf(3)], sink()),
    Cb(<<"1">>, #{}),
    ok = Flush(),
    ?assertEqual([<<"1">>], drain([])),
    ok = Flush(),
    ?assertEqual([], drain([])).
