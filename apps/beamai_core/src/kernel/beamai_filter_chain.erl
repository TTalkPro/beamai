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
%%% terminal 通过 throw 报错，run/4 用 try/catch 捕获，统一返回
%%% `{ok, Response} | {error, Reason}`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_filter_chain).

-export([run/4, compose/3]).

-type request() :: beamai_filter:request().
-type response() :: beamai_filter:response().
-type terminal() :: fun((request()) -> response()).
-type phase() :: beamai_filter:hook_type().

%%====================================================================
%% API
%%====================================================================

%% @doc 运行某条链的 filter 洋葱
%%
%% Phase 指定该链用哪个 around hook：chat 链传 around_chat，tool 链传
%% around_tool。只参与该链（含对应 around）的 filter 进入洋葱，其余跳过。
%% 注册顺序即层序：列表靠前 = 外层（无排序）。
%% Terminal 产出最内层响应，出错时 throw。
%%
%% @returns {ok, Response} | {error, Reason}
-spec run([beamai_filter:filter()], phase(), terminal(), request()) ->
    {ok, response()} | {error, term()}.
run(Filters, Phase, Terminal, Request) ->
    Relevant = relevant(Filters, Phase),
    Run = compose(Relevant, Phase, Terminal),
    try
        {ok, Run(Request)}
    catch
        throw:Reason -> {error, Reason}
    end.

%% @doc 把 filter 列表与 terminal 合成为单个洋葱函数
-spec compose([beamai_filter:filter()], phase(), terminal()) ->
    fun((request()) -> response()).
compose([], _Phase, Terminal) ->
    Terminal;
compose([Filter | Rest], Phase, Terminal) ->
    Next = compose(Rest, Phase, Terminal),
    Around = beamai_filter:hook(Filter, Phase),
    Name = maps:get(name, Filter),
    Init = beamai_filter:init(Filter),
    fun(#{context := Ctx} = Req) ->
        FCtx = beamai_context:filter_state(Ctx, Name, Init),
        case Around(Req, FCtx, Next) of
            {#{context := RCtx} = Resp, NewFCtx} ->
                Resp#{context => beamai_context:set_filter_state(RCtx, Name, NewFCtx)};
            Resp ->
                Resp
        end
    end.

%%====================================================================
%% 内部
%%====================================================================

%% @private 仅保留对该链有对应 around hook 的 filter
relevant(Filters, Phase) ->
    [F || F <- Filters, beamai_filter:hook(F, Phase) =/= undefined].
