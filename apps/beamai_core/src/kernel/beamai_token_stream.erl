%%%-------------------------------------------------------------------
%%% @doc Token 流变换链组装器（filter 第四钩子 token_transform）
%%%
%%% 把 filters 上收集的 token_transform 列表（注册顺序）折成一条变换链，包裹
%%% 最终 on-token sink：
%%%
%%%   provider 原始 token → token_transform 链（注册顺序，靠前者先见原始 token）
%%%                      → Sink(Token, Meta)
%%%
%%% 契约（对照 clj-agent token-stream-filter-design.md，transducer 的
%%% Erlang 等价）：
%%% - step 1→N：一个 token 进、0/1/N 个出（吞掉/改写/缓冲后批量放行）；
%%% - 状态显式穿线：每次 LLM 流 wrap/2 时按 init 现场初始化，工具循环
%%%   每轮各自新状态；
%%% - Flush = completion：流**正常**结束时调用，级联冲出各层缓冲残留
%%%   （外层残留先经内层 step 传播再 sink，然后内层自己 flush——精确
%%%   对齐 transducer completion 语义）；异常路径不调 Flush（缓冲丢弃，
%%%   半截答案不外泄）。
%%%
%%% 状态存调用进程 pdict（唯一 ref 键，Flush 后 erase）。前提（已核实）：
%%% token 回调由 provider 流式循环在**同一进程内串行**调用
%%% （gun/hackney receive 循环），Wrap 返回的回调与 Flush 必须在同一
%%% 进程使用。
%%%
%%% 无 token_transform 时零开销退化：Sink 原样直通，Flush 为空操作。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_token_stream).

-export([wrap/2]).

-type sink() :: fun((binary(), map()) -> ok).

%%====================================================================
%% API
%%====================================================================

%% @doc 把 token_transform 列表与 sink 组装为 {TokenCallback, Flush}
%%
%% Xfs 按注册顺序给出（靠前者最先见原始 token）。TokenCallback 兼容
%% on_llm_new_token 契约 fun(Token, Meta) -> ok；Flush 在流正常结束后
%% 调用一次（异常路径不得调用）。
-spec wrap([beamai_filter:token_transform()], sink()) ->
    {fun((binary(), map()) -> ok), fun(() -> ok)}.
wrap([], Sink) ->
    %% 零开销退化：原样直通
    {Sink, fun() -> ok end};
wrap(Xfs, Sink) when is_list(Xfs) ->
    Key = {?MODULE, make_ref()},
    put(Key, [{Xf, maps:get(init, Xf, undefined)} || Xf <- Xfs]),
    Callback = fun(Token, Meta) ->
        Chain0 = get(Key),
        {Emitted, Chain1} = step_chain(Chain0, [#{token => Token, meta => Meta}]),
        put(Key, Chain1),
        sink_all(Sink, Emitted)
    end,
    Flush = fun() ->
        Chain = erase(Key),
        flush_chain(Chain, Sink)
    end,
    {Callback, Flush}.

%%====================================================================
%% 内部
%%====================================================================

%% @private 把 token 列表依次穿过各层 step，返回 {最终出站列表, 新链}
step_chain([], Tokens) ->
    {Tokens, []};
step_chain([{Xf, State} | Rest], Tokens) ->
    #{step := Step} = Xf,
    {Emitted, NewState} =
        lists:foldl(fun(TD, {AccOut, S}) ->
            {Out, S1} = Step(TD, S),
            {AccOut ++ Out, S1}
        end, {[], State}, Tokens),
    {Final, NewRest} = step_chain(Rest, Emitted),
    {Final, [{Xf, NewState} | NewRest]}.

%% @private 级联 flush：逐层冲残留——本层残留经**下游** step 传播后 sink，
%% 再轮到下游自己 flush（transducer completion 的展开顺序）
flush_chain(undefined, _Sink) ->
    ok;   %% Flush 被重复调用（链已 erase）：幂等空操作
flush_chain([], _Sink) ->
    ok;
flush_chain([{Xf, State} | Rest], Sink) ->
    Residual = case maps:get(flush, Xf, undefined) of
        undefined -> [];
        FlushFn -> FlushFn(State)
    end,
    {Emitted, Rest1} = step_chain(Rest, Residual),
    sink_all(Sink, Emitted),
    flush_chain(Rest1, Sink).

%% @private 逐个送出站 token 给 sink
sink_all(_Sink, []) ->
    ok;
sink_all(Sink, [#{token := Token} = TD | Rest]) ->
    Sink(Token, maps:get(meta, TD, #{})),
    sink_all(Sink, Rest).
