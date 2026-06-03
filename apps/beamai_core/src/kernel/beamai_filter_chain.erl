%%%-------------------------------------------------------------------
%%% @doc Filter 洋葱链
%%%
%%% 把 filter 列表按某条链的 (pre, post) hook 合成为嵌套调用，最内层是
%%% terminal（真正的 LLM 调用或工具执行）：
%%%
%%%   compose([A, B], Terminal)
%%%     = fun(Req) -> A_pre/post 包裹( fun(Req) -> B_pre/post 包裹(Terminal) end ) end
%%%
%%% 执行顺序为洋葱式：
%%%   A.pre → B.pre → Terminal → B.post → A.post
%%% 回程自动逆序；某 filter 的 pre 返回 {halt, Response} 即短路（跳过内层，
%%% 仍执行本层 post）。
%%%
%%% terminal 通过 throw 报错，run/4 用 try/catch 捕获，统一返回
%%% `{ok, Response} | {error, Reason}`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_filter_chain).

-export([run/4, compose/4]).

-type request() :: beamai_filter:request().
-type response() :: beamai_filter:response().
-type terminal() :: fun((request()) -> response()).
-type phase() :: {PreKey :: beamai_filter:hook_type(), PostKey :: beamai_filter:hook_type()}.

%%====================================================================
%% API
%%====================================================================

%% @doc 运行某条链的 filter 洋葱
%%
%% Phase 指定该链用哪一对 hook：chat 链传 {pre_chat, post_chat}，
%% tool 链传 {pre_tool, post_tool}。只参与该链（含至少一个相关 hook）的
%% filter 进入洋葱，其余跳过。Terminal 产出最内层响应，出错时 throw。
%%
%% @returns {ok, Response} | {error, Reason}
-spec run([beamai_filter:filter()], phase(), terminal(), request()) ->
    {ok, response()} | {error, term()}.
run(Filters, Phase, Terminal, Request) ->
    Relevant = relevant(beamai_filter:sort(Filters), Phase),
    Run = compose(Relevant, Phase, Terminal, identity),
    try
        {ok, Run(Request)}
    catch
        throw:Reason -> {error, Reason}
    end.

%% @doc 把 filter 列表与 terminal 合成为单个洋葱函数
%%
%% 第 4 个参数仅用于内部递归占位，外部调用传 identity。
-spec compose([beamai_filter:filter()], phase(), terminal(), term()) ->
    fun((request()) -> response()).
compose([], _Phase, Terminal, _) ->
    Terminal;
compose([Filter | Rest], {PreKey, PostKey} = Phase, Terminal, _) ->
    Next = compose(Rest, Phase, Terminal, identity),
    Pre = hook_or_identity(Filter, PreKey),
    Post = hook_or_identity(Filter, PostKey),
    fun(Req) ->
        case Pre(Req) of
            {halt, Resp} -> Post(Resp);
            Req1 -> Post(Next(Req1))
        end
    end.

%%====================================================================
%% 内部
%%====================================================================

%% @private 仅保留对该链有相关 hook 的 filter
relevant(Filters, {PreKey, PostKey}) ->
    [F || F <- Filters,
          beamai_filter:hook(F, PreKey) =/= undefined
              orelse beamai_filter:hook(F, PostKey) =/= undefined].

%% @private 取 hook，缺失则用恒等函数
hook_or_identity(Filter, HookType) ->
    case beamai_filter:hook(Filter, HookType) of
        undefined -> fun(X) -> X end;
        Fun -> Fun
    end.
