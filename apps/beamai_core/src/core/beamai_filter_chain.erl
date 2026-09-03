%%%-------------------------------------------------------------------
%%% @doc Filter 洋葱链（around 模型）
%%%
%%% 把 filter 列表按某条链的 around hook 合成为嵌套调用，最内层是
%%% terminal（真正的 LLM 调用或工具执行）：
%%%
%%%   compose([A, B], Terminal)
%%%     = fun(Req) -> A_around( fun(Req) -> B_around(Terminal) end ) end
%%%
%%% 执行顺序为洋葱式：A 前置 → B 前置 → Terminal → B 后置 → A 后置。
%%% 某 filter 的 around 不调 Next 即短路（跳过内层，直接返回 Response）。
%%%
%%% 每个 filter 进入时，链从请求的共享 context 投影出该 filter 的**私有
%%% 上下文**（按名字隔离，缺省取 filter 的 init），作为 FCtx 传给 around；
%%% around 返回 {Response, NewFCtx} 时把 NewFCtx 合并回响应的 context，
%%% 仅返回 Response 时私有状态保持不变。私有状态随共享 context 透传，跨
%%% 工具循环各轮存活。
%%%
%%% **tuple 响应（turn 链）的回写**：turn 链的响应是工具循环结果 tuple，没有
%%% context 槽可合并。这类响应的 {Response, NewFCtx} 由链收集到进程内的写表，
%%% 在 run/4、run_with_context/4 收尾时统一合并进**请求的 context**并返回
%%% （见 run_with_context/4）。turn filter 每 turn 只进出一次，回写因此不是给
%%% 本 turn 用的——调用方（beamai_agent）拿到这份 context 后按会话保存，下一
%%% turn 建 context 时种回去，turn filter 的私有状态于是跨轮存活。
%%% 判据是「first 元素为 tuple」：合法的 turn 结果 tuple 首元素都是 atom
%%% （ok/interrupt/error），与 {Response, NewFCtx} 不会混淆。
%%%
%%% terminal 通过 throw 报错，run/4 用 try/catch 捕获，统一返回
%%% `{ok, Response} | {error, Reason}`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_filter_chain).

-export([run/4, run_with_context/4, compose/3]).

%% 进程内 filter 写表（仅承接 tuple 响应的回写；map 响应直接合并进响应 context）
-define(WRITES_KEY, '$beamai_filter_chain_writes').

-type request() :: beamai_filter:request().
-type response() :: beamai_filter:response().
-type terminal() :: fun((request()) -> response()).
-type phase() :: beamai_filter:hook_type().

%%====================================================================
%% API
%%====================================================================

%% @doc 运行某条链的 filter 洋葱
%%
%% Phase 指定该链用哪个 around hook：chat 链传 around_chat，step 链传
%% around_step，tool 链传 around_tool。只参与该链（含对应 around）的 filter
%% 进入洋葱，其余跳过。
%% 注册顺序即层序：列表靠前 = 外层（无排序）。
%% Terminal 产出最内层响应，出错时 throw。
%%
%% @returns {ok, Response} | {error, Reason}
-spec run([beamai_filter:filter()], phase(), terminal(), request()) ->
    {ok, response()} | {error, term()}.
run(Filters, Phase, Terminal, Request) ->
    case run_with_context(Filters, Phase, Terminal, Request) of
        {ok, Response, _Ctx} -> {ok, Response};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 同 run/4，另外返回**汇总了 tuple 响应回写**的 context
%%
%% Ctx = 请求的 context + 本次执行中各 filter 对 tuple 响应做的私有状态回写。
%% map 响应（chat/step/tool 链）的回写本就在响应自己的 context 里，这里的
%% Ctx 对它们没有额外信息；turn 链则只有这条路能把回写带出来。
%%
%% @returns {ok, Response, Context} | {error, Reason}
-spec run_with_context([beamai_filter:filter()], phase(), terminal(), request()) ->
    {ok, response(), beamai_context:t()} | {error, term()}.
run_with_context(Filters, Phase, Terminal, Request) ->
    Run = compose(Filters, Phase, Terminal),
    Saved = erlang:put(?WRITES_KEY, #{}),
    try
        Response = Run(Request),
        {ok, Response, merge_writes(Request, take_writes())}
    catch
        throw:Reason -> {error, Reason}
    after
        restore_writes(Saved)
    end.

%% @doc 把 filter 列表与 terminal 合成为单个洋葱函数
%%
%% 自行按 Phase 过滤（不含对应 around 的 filter 跳过），故可直接传整份 filters
%% 列表。合成结果**不捕获** throw：嵌套使用时（turn 链的 terminal 就是 step 链）
%% 由最外层的 run/4 统一捕获。
-spec compose([beamai_filter:filter()], phase(), terminal()) ->
    fun((request()) -> response()).
compose(Filters, Phase, Terminal) ->
    compose_relevant(relevant(Filters, Phase), Phase, Terminal).

%%====================================================================
%% 内部
%%====================================================================

%% @private 逐层折叠（列表已按 Phase 过滤）
compose_relevant([], _Phase, Terminal) ->
    Terminal;
compose_relevant([Filter | Rest], Phase, Terminal) ->
    Next = compose_relevant(Rest, Phase, Terminal),
    Around = beamai_filter:hook(Filter, Phase),
    Name = maps:get(name, Filter),
    Init = beamai_filter:init(Filter),
    fun(#{context := Ctx} = Req) ->
        FCtx = beamai_context:filter_state(Ctx, Name, Init),
        case Around(Req, FCtx, Next) of
            {#{context := RCtx} = Resp, NewFCtx} when is_map(NewFCtx) ->
                Resp#{context => beamai_context:set_filter_state(RCtx, Name, NewFCtx)};
            {Resp, NewFCtx} when is_tuple(Resp), is_map(NewFCtx) ->
                %% tuple 响应（turn 链）没有 context 槽：记进进程写表，由
                %% run_with_context/4 收尾时合并
                record_write(Name, NewFCtx),
                Resp;
            Resp ->
                Resp
        end
    end.

%% @private 仅保留对该链有对应 around hook 的 filter
relevant(Filters, Phase) ->
    [F || F <- Filters, beamai_filter:hook(F, Phase) =/= undefined].

%% @private 记一笔 tuple 响应的私有状态回写（无写表时说明不在 run 内，忽略）
record_write(Name, FCtx) ->
    case erlang:get(?WRITES_KEY) of
        Writes when is_map(Writes) -> erlang:put(?WRITES_KEY, Writes#{Name => FCtx});
        undefined -> ok
    end.

%% @private 取出本次执行累积的写表
take_writes() ->
    case erlang:get(?WRITES_KEY) of
        Writes when is_map(Writes) -> Writes;
        undefined -> #{}
    end.

%% @private 还原外层（嵌套 run）的写表
restore_writes(undefined) -> erlang:erase(?WRITES_KEY);
restore_writes(Saved) -> erlang:put(?WRITES_KEY, Saved).

%% @private 把写表合并进请求的 context
merge_writes(Request, Writes) when map_size(Writes) =:= 0 ->
    request_context(Request);
merge_writes(Request, Writes) ->
    maps:fold(fun(Name, FCtx, Ctx) ->
                  beamai_context:set_filter_state(Ctx, Name, FCtx)
              end, request_context(Request), Writes).

%% @private 请求的 context（缺席则新建，仅为容错——链本身要求请求带 context）
request_context(Request) ->
    maps:get(context, Request, beamai_context:new()).
